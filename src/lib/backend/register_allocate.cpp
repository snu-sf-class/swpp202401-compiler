#include "register_allocate.h"

#include "analysis.h"
#include "arch.h"
#include "const_map.h"

#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Casting.h"

#include <algorithm>
#include <array>
#include <cstdlib>
#include <map>
#include <queue>
#include <utility>
#include <vector>

namespace {
using namespace sc::backend;

constexpr unsigned int MAX_ARGUMENT = 16U;
constexpr unsigned int MAX_REGISTER = 32U;
constexpr unsigned int MAX_VECTOR_REGISTER = 16U;
constexpr unsigned int LOAD_COST = 30U;
constexpr unsigned int STORE_COST = 30U;
constexpr unsigned int VECTOR_LOAD_COST = 60U;
constexpr unsigned int VECTOR_STORE_COST = 60U;
constexpr unsigned int UNKNOWN_LOOP_CNT = 100U;

void PostOrderRegCollect(llvm::BasicBlock &BB,
                         std::vector<llvm::Instruction *> &insts,
                         std::set<llvm::BasicBlock *> &visit) {
  visit.insert(&BB);
  llvm::Instruction *term = BB.getTerminator();
  unsigned int num = term->getNumSuccessors();
  for (unsigned int i = 0U; i < num; i++) {
    llvm::BasicBlock *next = term->getSuccessor(i);
    if (!visit.count(next))
      PostOrderRegCollect(*next, insts, visit);
  }
  for (auto it = BB.rbegin(); it != BB.rend(); it++) {
    llvm::Instruction &I = *it;
    insts.emplace_back(&I);
  }
}

void makeInterferenceGraph(
    llvm::Function &F,
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph,
    std::map<llvm::Instruction *, int> &inst2num) {
  std::vector<llvm::Instruction *> insts; // post-order DFS-traversal
  std::set<llvm::BasicBlock *> visit;
  std::map<llvm::Instruction *, std::set<llvm::Instruction *>> in, out;

  PostOrderRegCollect(F.getEntryBlock(), insts, visit);
  for (auto &BB : F) {
    if (!llvm::isPotentiallyReachable(&F.getEntryBlock(), &BB)) {
      PostOrderRegCollect(BB, insts, visit);
    }
  }

  for (llvm::Instruction *I : insts) {
    in[I] = {};
    out[I] = {};
    for (llvm::Value *oper : I->operand_values()) {
      llvm::Instruction *inst = llvm::dyn_cast<llvm::Instruction>(oper);
      if (inst)
        in[I].insert(inst);
    }
  }
  bool updated = true;
  while (updated) {
    updated = false;
    for (llvm::Instruction *I : insts) {
      size_t before = out[I].size();
      if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I)) {
        llvm::Instruction *nonPhi = phi->getParent()->getFirstNonPHI();
        out[I].insert(in[nonPhi].begin(), in[nonPhi].end());
      } else if (llvm::Instruction *next = I->getNextNode()) {
        out[I].insert(in[next].begin(), in[next].end());
      } else {
        unsigned int num = I->getNumSuccessors();
        for (unsigned int i = 0U; i < num; i++) {
          llvm::BasicBlock *succ = I->getSuccessor(i);
          llvm::Instruction *nonPhi = succ->getFirstNonPHI();
          std::set<llvm::Instruction *> temp = in[nonPhi];
          for (llvm::PHINode &succPhi : succ->phis()) {
            temp.erase(&succPhi);
            llvm::Value *v = succPhi.getIncomingValueForBlock(I->getParent());
            if (v) {
              llvm::Instruction *mine = llvm::dyn_cast<llvm::Instruction>(v);
              assert(mine && "It should be an instruction.");
              temp.insert(mine);
            }
          }
          out[I].insert(temp.begin(), temp.end());
        }
      }
      size_t after = out[I].size();
      updated = updated || (before != after);
      before = in[I].size();
      if (out[I].count(I)) {
        out[I].erase(I);
        in[I].insert(out[I].begin(), out[I].end());
        out[I].insert(I);
      } else {
        in[I].insert(out[I].begin(), out[I].end());
      }
      after = in[I].size();
      updated = updated || (before != after);
    }
  }
  int cnt = 0;
  for (llvm::Instruction *I : insts)
    if (analysis::isReg(I)) {
      inter_graph[I] = {};
      inst2num[I] = cnt++;
    }
  for (llvm::Instruction *I : insts)
    if (analysis::isReg(I))
      for (llvm::Instruction *J : out[I])
        if (I != J && analysis::isReg(J)) {
          inter_graph[I].insert(J);
          inter_graph[J].insert(I);
        }
}

void coalesceMovInsts(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph) {
  std::vector<llvm::Instruction *> trash;

  for (auto &[I, S] : inter_graph) {
    llvm::Value *v = I;
    while (llvm::Instruction *inst = analysis::isMoveInst(v))
      v = inst->getOperand(0);
    llvm::Instruction *p = llvm::dyn_cast<llvm::Instruction>(v);
    assert(p && "p should be instruction.");
    if (p != I) {
      inter_graph[p].insert(S.begin(), S.end());
      for (llvm::Instruction *other : S)
        inter_graph[other].insert(p);
      trash.emplace_back(I);
    }
  }

  for (llvm::Instruction *I : trash)
    inter_graph.erase(I);
  for (auto &[I, S] : inter_graph) {
    for (llvm::Instruction *other : trash)
      S.erase(other);
    S.erase(I);
  }
}

class InvalidConstantError : public static_error::Error<InvalidConstantError> {
public:
  const char *what() const noexcept {
    return "Invalid constant error: this constant cannot be used";
  }
};

bool resolvePHIInterference(
    llvm::Module &M,
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph,
    llvm::IntegerType *Int64Ty, const_map::ConstMap *CM) {
  for (auto &[I, S] : inter_graph)
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I)) {
      std::vector<llvm::Instruction *> parent;
      std::set<llvm::Instruction *> inter = S;
      for (int i = 0; i < phi->getNumIncomingValues(); i++) {
        llvm::Value *v = phi->getIncomingValue(i);
        while (llvm::Instruction *next = analysis::isMoveInst(v))
          v = next->getOperand(0);
        llvm::Instruction *p = llvm::dyn_cast<llvm::Instruction>(v);
        assert(p && "It should be an instruction.");
        parent.push_back(p);
        inter.insert(inter_graph[p].cbegin(), inter_graph[p].cend());
      }
      size_t size = parent.size();
      for (int i = 0; i < size; i++)
        if (inter.count(parent[i])) {
          llvm::Instruction *next = parent[i]->getNextNode();
          while (next && analysis::isMoveInst(next))
            next = next->getNextNode();
          llvm::Instruction *t = phi->getIncomingBlock(i)->getTerminator();
          if (next == t)
            continue;
          llvm::Value *v = phi->getIncomingValue(i);
          llvm::Type *type = v->getType();
          if (type->isVectorTy()) {
            const auto cst_1 = CM->resolve_constant(
                I->getFunction(), llvm::dyn_cast<llvm::ConstantDataVector>(v),
                I);
            v = llvm::BinaryOperator::CreateMul(v, cst_1, "", t);
          } else {
            if (!type->isIntegerTy())
              v = llvm::CastInst::CreateBitOrPointerCast(v, Int64Ty, "", t);

            const auto cst_1 = CM->resolve_constant(
                I->getFunction(),
                llvm::dyn_cast<llvm::IntegerType>(v->getType()), 1, I);
            v = llvm::BinaryOperator::CreateMul(v, cst_1, "", t);
            if (!type->isIntegerTy())
              v = llvm::CastInst::CreateBitOrPointerCast(v, type, "", t);
          }
          phi->setIncomingValue(i, v);
          return false;
        }
    }
  return true;
}

void coalescePHINodes(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph) {
  std::vector<llvm::Instruction *> trash;

  for (auto &[I, S] : inter_graph)
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I))
      for (int i = 0; i < phi->getNumIncomingValues(); i++) {
        llvm::Value *v = phi->getIncomingValue(i);
        while (llvm::Instruction *next = analysis::isMoveInst(v))
          v = next->getOperand(0);
        llvm::Instruction *V = llvm::dyn_cast<llvm::Instruction>(v);
        assert(V && "operand of phi should be an instruction.");
        inter_graph[phi].insert(inter_graph[V].begin(), inter_graph[V].end());
        for (llvm::Instruction *other : inter_graph[V])
          inter_graph[other].insert(phi);
        trash.emplace_back(V);
      }
  for (llvm::Instruction *I : trash)
    inter_graph.erase(I);
  for (auto &[I, S] : inter_graph) {
    for (llvm::Instruction *other : trash)
      S.erase(other);
    S.erase(I);
  }
}

void PerfectEliminationOrdering(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>>
        inter_graph, // should be copied
    std::map<llvm::Instruction *, int> &inst2num,
    std::vector<llvm::Instruction *> &order) {
  std::map<llvm::Instruction *, int> C;
  std::queue<llvm::Instruction *> Q;
  std::set<llvm::Instruction *> visit;
  std::vector<llvm::Instruction *> nodes;
  for (auto &[I, S] : inter_graph) {
    size_t size = S.size();
    C[I] = size * (size - 1) / 2;
    std::vector<llvm::Instruction *> neighbor;
    for (llvm::Instruction *X : S)
      neighbor.push_back(X);
    for (int i = 0; i < size; i++)
      for (int j = i + 1; j < size; j++)
        C[I] -= inter_graph[neighbor[i]].count(neighbor[j]);
    nodes.push_back(I);
  }
  auto compare = [&](llvm::Instruction *a, llvm::Instruction *b) {
    return inst2num[a] < inst2num[b];
  };
  std::sort(nodes.begin(), nodes.end(), compare);
  while (order.size() < inter_graph.size()) {
    auto min = INT32_MAX;
    const auto min_c_inst =
        *std::min_element(nodes.cbegin(), nodes.cend(),
                          [visit, C, &min](const auto min_I, const auto I) {
                            if (!visit.count(I)) {
                              const auto c = C.at(I);
                              if (min >= c) {
                                min = c;
                                return false;
                              }
                            }
                            return true;
                          });

    if (!visit.count(min_c_inst)) {
      visit.insert(min_c_inst);
      Q.push(min_c_inst);
    }

    while (!Q.empty()) {
      llvm::Instruction *v = Q.front();
      order.push_back(v);
      Q.pop();

      size_t size = inter_graph[v].size();
      std::vector<llvm::Instruction *> adj_nodes;
      adj_nodes.reserve(size);
      std::transform(inter_graph[v].cbegin(), inter_graph[v].cend(),
                     std::back_inserter(adj_nodes),
                     [](const auto w) { return w; });
      std::sort(adj_nodes.begin(), adj_nodes.end(), compare);

      for (llvm::Instruction *w : adj_nodes) {
        inter_graph[w].erase(v);
        C[w] -= inter_graph[w].size() - (size - 1);
        if (!C[w] && !visit.count(w)) {
          visit.insert(w);
          Q.push(w);
        }
      }
    }
  }
  // TODO: SSA-form should have chordal interference, but some are shown to be
  // non-chordal... but why? note:
  // http://web.cs.ucla.edu/~palsberg/paper/aplas05.pdf
  assert(order.size() == inter_graph.size() &&
         "SSA-form should have chordal interference graph.");
}

std::pair<int, int> GreedyColoring(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph,
    std::vector<llvm::Instruction *> &order,
    std::map<llvm::Instruction *, int> &general_reg_color,
    std::map<llvm::Instruction *, int> &vector_reg_color) {
  std::set<int> general_color_used;
  std::set<int> vector_color_used;
  int general_num_colors = 0, vector_num_colors = 0;
  for (llvm::Instruction *I : order) {
    general_color_used.clear();
    vector_color_used.clear();
    for (llvm::Instruction *J : inter_graph[I]) {
      if (general_reg_color.count(J))
        general_color_used.insert(general_reg_color[J]);
      if (vector_reg_color.count(J))
        vector_color_used.insert(vector_reg_color[J]);
    }
    int i = 1;
    if (I->getType()->isIntegerTy() || I->getType()->isPointerTy()) {
      while (general_color_used.count(i))
        i++;
      general_reg_color[I] = i;
      general_num_colors = std::max(general_num_colors, i);
    } else if (I->getType()->isVectorTy()) {
      while (vector_color_used.count(i))
        i++;
      vector_reg_color[I] = i;
      vector_num_colors = std::max(vector_num_colors, i);
    }
  }
  return std::make_pair(general_num_colors, vector_num_colors);
}

void recursivelyInsertSymbols(
    symbol::SymbolMap *SM,
    std::map<llvm::Instruction *, int> &general_reg_color,
    std::map<llvm::Instruction *, int> &vector_reg_color, llvm::Value *V) {
  if (SM->getSymbol(V))
    return;
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    SM->addSymbol(C, symbol::Symbol::createConstantSymbol(C->getZExtValue()));
    return;
  }
  if (llvm::isa<llvm::ConstantPointerNull>(V)) {
    SM->addSymbol(V, symbol::Symbol::createConstantSymbol(NULL_PTR));
    return;
  }
  if (auto undef = llvm::dyn_cast<llvm::UndefValue>(V)) {
    const auto undef_ty = undef->getType();
    if (undef_ty->isVectorTy()) {
      SM->addSymbol(undef, symbol::Symbol::createVectorRegisterSymbol(1));
    } else {
      SM->addSymbol(undef, symbol::Symbol::createRegisterSymbol(1));
    }
    return;
  }
  llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(V);
  if (CI && CI->getCalledFunction()->getName().str() == "$decr_sp") {
    SM->addSymbol(CI, symbol::Symbol::createStackPtrSymbol());
    return;
  }
  llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(V);
  assert(I && "It should be an instruction.");
  if (general_reg_color.count(I)) {
    SM->addSymbol(I,
                  symbol::Symbol::createRegisterSymbol(general_reg_color[I]));
  }
  if (vector_reg_color.count(I)) {
    SM->addSymbol(
        I, symbol::Symbol::createVectorRegisterSymbol(vector_reg_color[I]));
  }
  for (llvm::Value *use : I->operand_values()) {
    recursivelyInsertSymbols(SM, general_reg_color, vector_reg_color, use);
  }
  if (llvm::Instruction *MV = analysis::isMoveInst(I)) {
    const symbol::Symbol *sym = SM->getSymbol(MV->getOperand(0));
    std::string &&name = sym->getName();
    switch (name.at(0)) {
    case 's':
      SM->addSymbol(I, symbol::Symbol::createStackPtrSymbol());
      break;
    case 'r':
      SM->addSymbol(I, symbol::Symbol::createRegisterSymbol(std::move(name)));
      break;
    case 'a':
      SM->addSymbol(I, symbol::Symbol::createArgumentSymbol(std::move(name)));
      break;
    case 'v':
      SM->addSymbol(
          I, symbol::Symbol::createVectorRegisterSymbol(std::move(name)));
      break;
    default:
      SM->addSymbol(I, symbol::Symbol::createConstantSymbol(std::move(name)));
      break;
    }
  }
}

void propagatePHINodeColors(std::map<llvm::Instruction *, int> &color) {
  std::vector<std::pair<llvm::Instruction *, int>> buff;

  for (auto &[I, c] : color)
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I))
      for (llvm::Use &use : phi->incoming_values()) {
        llvm::Value *v = use.get();
        while (llvm::Instruction *mv = analysis::isMoveInst(v))
          v = mv->getOperand(0);
        llvm::Instruction *i = llvm::dyn_cast<llvm::Instruction>(v);
        buff.emplace_back(i, c);
      }
  for (auto [I, c] : buff)
    color[I] = c;
}

void insertSymbols(symbol::SymbolMap *SM, llvm::Function &F,
                   std::map<llvm::Instruction *, int> &general_reg_color,
                   std::map<llvm::Instruction *, int> &vector_reg_color) {
  int i = 1;
  for (llvm::Argument &arg : F.args()) {
    assert(i <= MAX_ARGUMENT && "Too many arguments.");
    SM->addSymbol(&arg, symbol::Symbol::createArgumentSymbol(i++));
  }
  i = 1;
  for (llvm::BasicBlock &BB : F)
    SM->addSymbol(&BB, symbol::Symbol::createBasicBlockLabelSymbol(
                           BB.hasName() ? BB.getName().str()
                                        : "_default." + std::to_string(i++)));
  /* You should not put below inside BasicBlock loop, should be run separately.
   */
  for (llvm::BasicBlock &BB : F)
    for (llvm::Instruction &I : BB)
      recursivelyInsertSymbols(SM, general_reg_color, vector_reg_color, &I);
}

void insertLoadStore(std::vector<llvm::Instruction *> &insts,
                     llvm::CallInst *SP,
                     std::set<llvm::Instruction *> &not_spill) {
  std::vector<llvm::Instruction *> stores;
  std::vector<llvm::Use *> loads;
  const auto Int64Ty = llvm::IntegerType::getInt64Ty(SP->getContext());
  const auto PtrTy = llvm::PointerType::get(SP->getContext(), 0);

  llvm::FunctionCallee const_i64 =
      SP->getModule()->getOrInsertFunction("const_i64", Int64Ty, Int64Ty);

  for (llvm::Instruction *I : insts) {
    if (not_spill.count(I))
      continue;
    not_spill.insert(I);
    if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I))
      stores.emplace_back(I);
    for (llvm::Use &use : I->uses()) {
      llvm::Instruction *user =
          llvm::dyn_cast<llvm::Instruction>(use.getUser());
      assert(user && "It should be an instruction.");
      if (!analysis::isMoveInst(user) && !llvm::isa<llvm::PHINode>(user))
        loads.emplace_back(&use);
    }
  }

  uint64_t acc =
      llvm::dyn_cast<llvm::ConstantInt>(SP->getArgOperand(0))->getZExtValue();
  const auto spill_size = 8 * stores.size();
  SP->setArgOperand(0, llvm::ConstantInt::get(Int64Ty, acc + spill_size, true));

  for (llvm::Instruction *I : stores) {
    llvm::Instruction *V = I;
    llvm::Instruction *next = V->getNextNode();
    llvm::Instruction *ptr = SP;
    if (acc) {
      llvm::Value *args[] = {llvm::ConstantInt::get(Int64Ty, acc, true)};
      const auto cst = llvm::CallInst::Create(const_i64, args, "", next);
      ptr = llvm::BinaryOperator::CreateAdd(ptr, cst, "", next);
      not_spill.insert(cst);
      not_spill.insert(ptr);
    }
    ptr = llvm::CastInst::CreateBitOrPointerCast(ptr, PtrTy, "", next);
    not_spill.insert(ptr);
    llvm::Type *type = V->getType();
    if (type->isIntegerTy()) {
      if (!type->isIntegerTy(64)) {
        V = llvm::CastInst::CreateIntegerCast(V, Int64Ty, false, "", next);
        not_spill.insert(V);
      }
    } else {
      V = llvm::CastInst::CreateBitOrPointerCast(V, Int64Ty, "", next);
      not_spill.insert(V);
    }
    llvm::StoreInst *SI = new llvm::StoreInst(V, ptr, next);
    not_spill.insert(SI);
  }
  for (llvm::Use *use : loads) {
    llvm::Instruction *user = llvm::dyn_cast<llvm::Instruction>(use->getUser());
    assert(user && "It should be an instruction.");
    llvm::Instruction *ptr = SP;
    if (acc) {
      llvm::Value *args[] = {llvm::ConstantInt::get(Int64Ty, acc, true)};
      const auto cst = llvm::CallInst::Create(const_i64, args, "", user);
      ptr = llvm::BinaryOperator::CreateAdd(ptr, cst, "", user);
      not_spill.insert(cst);
      not_spill.insert(ptr);
    }
    ptr = llvm::CastInst::CreateBitOrPointerCast(ptr, PtrTy, "", user);
    not_spill.insert(ptr);
    llvm::LoadInst *LI = new llvm::LoadInst(Int64Ty, ptr, "", user);
    not_spill.insert(LI);
    llvm::Instruction *V = LI;
    llvm::Type *type = use->get()->getType();
    if (type->isIntegerTy()) {
      if (!type->isIntegerTy(64U)) {
        V = llvm::CastInst::CreateIntegerCast(V, type, false, "", user);
        not_spill.insert(V);
      }
    } else {
      V = llvm::CastInst::CreateBitOrPointerCast(V, type, "", user);
      not_spill.insert(V);
    }
    use->set(V);
  }
}

void insertVectorLoadStore(std::vector<llvm::Instruction *> &insts,
                           llvm::CallInst *SP,
                           std::set<llvm::Instruction *> &not_spill) {
  std::vector<llvm::Instruction *> stores;
  std::vector<llvm::Use *> loads;

  const auto Int64Ty = llvm::IntegerType::getInt64Ty(SP->getContext());
  const auto PtrTy = llvm::PointerType::get(SP->getContext(), 0);
  const auto I64x4Ty = llvm::VectorType::get(Int64Ty, 4, false);
  const auto I32x8Ty = llvm::VectorType::get(
      llvm::IntegerType::getInt32Ty(SP->getContext()), 8, false);

  llvm::FunctionCallee const_i64 =
      SP->getModule()->getOrInsertFunction("const_i64", Int64Ty, Int64Ty);

  for (llvm::Instruction *I : insts) {
    if (not_spill.count(I))
      continue;
    not_spill.insert(I);
    if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I))
      stores.push_back(I);
    for (llvm::Use &use : I->uses()) {
      llvm::Instruction *user =
          llvm::dyn_cast<llvm::Instruction>(use.getUser());
      assert(user && "It should be an instruction.");
      if (!analysis::isMoveInst(user) && !llvm::isa<llvm::PHINode>(user))
        loads.push_back(&use);
    }
  }

  uint64_t acc =
      llvm::dyn_cast<llvm::ConstantInt>(SP->getArgOperand(0))->getZExtValue();
  const auto spill_size = 32 * stores.size();
  SP->setArgOperand(0, llvm::ConstantInt::get(Int64Ty, acc + spill_size, true));

  for (llvm::Instruction *I : stores) {
    llvm::Instruction *V = I;
    llvm::Instruction *next = V->getNextNode();
    llvm::Instruction *ptr = SP;
    if (acc) {
      llvm::Value *args[] = {llvm::ConstantInt::get(Int64Ty, acc, true)};
      const auto cst = llvm::CallInst::Create(const_i64, args, "", next);
      ptr = llvm::BinaryOperator::CreateAdd(ptr, cst, "", next);
      not_spill.insert(cst);
      not_spill.insert(ptr);
    }
    ptr = llvm::CastInst::CreateBitOrPointerCast(ptr, PtrTy, "", next);
    not_spill.insert(ptr);
    llvm::Type *type = V->getType();
    if (type->isVectorTy()) {
      if (type == I32x8Ty || type == I64x4Ty) {
        llvm::StoreInst *SI = new llvm::StoreInst(V, ptr, next);
        not_spill.insert(SI);
        continue;
      }
    }
    // crash!
    std::unreachable();
  }
  for (llvm::Use *use : loads) {
    llvm::Instruction *user = llvm::dyn_cast<llvm::Instruction>(use->getUser());
    assert(user && "It should be an instruction.");
    llvm::Instruction *ptr = SP;
    if (acc) {
      llvm::Value *args[] = {llvm::ConstantInt::get(Int64Ty, acc, true)};
      const auto cst = llvm::CallInst::Create(const_i64, args, "", user);
      ptr = llvm::BinaryOperator::CreateAdd(ptr, cst, "", user);
      not_spill.insert(cst);
      not_spill.insert(ptr);
    }
    ptr = llvm::CastInst::CreateBitOrPointerCast(ptr, PtrTy, "", user);
    not_spill.insert(ptr);
    llvm::Type *type = use->get()->getType();
    llvm::LoadInst *LI = new llvm::LoadInst(type, ptr, "", user);
    not_spill.insert(LI);
    llvm::Instruction *V = LI;
    use->set(V);
  }
}

void spillColors(llvm::Function &F, int num_general_colors,
                 int num_vector_colors,
                 std::map<llvm::Instruction *, int> &general_reg_color,
                 std::map<llvm::Instruction *, int> &vector_reg_color,
                 llvm::FunctionCallee &decr_sp,
                 std::set<llvm::Instruction *> &not_spill) {
  llvm::DominatorTree DT(F);
  llvm::LoopInfo LI(DT);
  llvm::TargetLibraryInfoImpl TLIImpl;
  llvm::TargetLibraryInfo TLI(TLIImpl, &F);
  llvm::AssumptionCache AC(F);
  llvm::ScalarEvolution SCE(F, TLI, AC, DT, LI);
  const auto Int64Ty = llvm::IntegerType::getInt64Ty(F.getContext());

  std::vector<std::vector<llvm::Instruction *>> general_color2inst(
      num_general_colors + 1, std::vector<llvm::Instruction *>());
  std::vector<std::vector<llvm::Instruction *>> vector_color2inst(
      num_vector_colors + 1, std::vector<llvm::Instruction *>());
  int min_general_cost = 0x7fffffff, min_general_color = 0;
  int min_vector_cost = 0x7fffffff, min_vector_color = 0;

  for (llvm::BasicBlock &BB : F)
    for (llvm::Instruction &I : BB)
      if (analysis::isReg(&I)) {
        llvm::Value *V = &I;
        while (llvm::Instruction *inst = analysis::isMoveInst(V))
          V = inst->getOperand(0);
        llvm::Instruction *P = llvm::dyn_cast<llvm::Instruction>(V);
        if (P->getType()->isVectorTy()) {
          vector_color2inst[vector_reg_color[P]].push_back(&I);
        } else {
          general_color2inst[general_reg_color[P]].push_back(&I);
        }
      }

  for (int i = 1; i <= num_general_colors; i++) {
    int cost = 0;
    for (llvm::Instruction *I : general_color2inst[i]) {
      if (not_spill.count(I))
        continue;
      if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I)) {
        unsigned int cnt = 1U;
        if (llvm::Loop *L = LI.getLoopFor(I->getParent())) {
          cnt = SCE.getSmallConstantTripCount(L);
          if (!cnt)
            cnt = UNKNOWN_LOOP_CNT;
        }
        cost += STORE_COST * cnt;
      }
      for (llvm::User *user : I->users()) {
        llvm::Instruction *J = llvm::dyn_cast<llvm::Instruction>(user);
        assert(I);
        if (!analysis::isMoveInst(J) && !llvm::isa<llvm::PHINode>(J)) {
          unsigned int cnt = 1U;
          if (llvm::Loop *L = LI.getLoopFor(J->getParent())) {
            cnt = SCE.getSmallConstantTripCount(L);
            if (!cnt)
              cnt = UNKNOWN_LOOP_CNT;
          }
          cost += LOAD_COST * cnt;
        }
      }
    }
    if (cost && min_general_cost > cost) {
      min_general_cost = cost;
      min_general_color = i;
    }
  }

  for (int i = 1; i <= num_vector_colors; i++) {
    int cost = 0;
    for (llvm::Instruction *I : vector_color2inst[i]) {
      if (not_spill.count(I))
        continue;
      if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I)) {
        unsigned int cnt = 1U;
        if (llvm::Loop *L = LI.getLoopFor(I->getParent())) {
          cnt = SCE.getSmallConstantTripCount(L);
          if (!cnt)
            cnt = UNKNOWN_LOOP_CNT;
        }
        cost += VECTOR_STORE_COST * cnt;
      }
      for (llvm::User *user : I->users()) {
        llvm::Instruction *J = llvm::dyn_cast<llvm::Instruction>(user);
        assert(I);
        if (!analysis::isMoveInst(J) && !llvm::isa<llvm::PHINode>(J)) {
          unsigned int cnt = 1U;
          if (llvm::Loop *L = LI.getLoopFor(J->getParent())) {
            cnt = SCE.getSmallConstantTripCount(L);
            if (!cnt)
              cnt = UNKNOWN_LOOP_CNT;
          }
          cost += VECTOR_LOAD_COST * cnt;
        }
      }
    }
    if (cost && min_vector_cost > cost) {
      min_vector_cost = cost;
      min_vector_color = i;
    }
  }

  llvm::BasicBlock &EB = F.getEntryBlock();
  llvm::CallInst *SP = nullptr;
  for (llvm::Instruction &I : EB) {
    if (llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(&I))
      if (CI->getCalledFunction()->getName().equals("$decr_sp")) {
        SP = CI;
        break;
      }
  }
  if (!SP) {
    llvm::Instruction *FI = F.getEntryBlock().getFirstNonPHI();
    llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, 0UL, true);
    llvm::Value *Args[] = {Const};
    SP = llvm::CallInst::Create(decr_sp, llvm::ArrayRef<llvm::Value *>(Args),
                                "", FI);
    not_spill.insert(SP);
  }

  insertLoadStore(general_color2inst[min_general_color], SP, not_spill);
  insertVectorLoadStore(vector_color2inst[min_vector_color], SP, not_spill);
}
} // namespace

namespace sc::backend::reg_alloc {
RegisterAllocatePass::RegisterAllocatePass(symbol::SymbolMap &__SM,
                                           const_map::ConstMap &__CM)
    : SM(&__SM), CM(&__CM) {}

llvm::PreservedAnalyses
RegisterAllocatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  std::map<llvm::Instruction *, std::set<llvm::Instruction *>> inter_graph;
  // TODO: maps and sets should use compare fn based on inst2num
  std::map<llvm::Instruction *, int> inst2num;
  std::vector<llvm::Instruction *> order;
  std::map<llvm::Instruction *, int> general_reg_color;
  std::map<llvm::Instruction *, int> vector_reg_color;
  std::set<llvm::Instruction *> not_spill;
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  llvm::PointerType *Int64PtrTy = llvm::PointerType::get(M.getContext(), 0);
  llvm::FunctionCallee decr_sp =
      M.getOrInsertFunction("$decr_sp", Int64Ty, Int64Ty);

  for (llvm::Function &F : M)
    SM->addSymbol(&F,
                  symbol::Symbol::createFunctionNameSymbol(F.getName().str()));

  for (llvm::Function &F : M) {
    if (F.isDeclaration())
      continue;

    not_spill.clear();
    while (true) {
      while (true) {
        inter_graph.clear();
        inst2num.clear();
        makeInterferenceGraph(F, inter_graph, inst2num);
        coalesceMovInsts(inter_graph);
        if (resolvePHIInterference(M, inter_graph, Int64Ty, CM))
          break;
      }
      coalescePHINodes(inter_graph);
      order.clear();
      PerfectEliminationOrdering(inter_graph, inst2num, order);
      std::reverse(order.begin(), order.end());
      general_reg_color.clear();
      vector_reg_color.clear();
      const auto &[num_general_colors, num_vector_colors] = GreedyColoring(
          inter_graph, order, general_reg_color, vector_reg_color);
      propagatePHINodeColors(general_reg_color);
      propagatePHINodeColors(vector_reg_color);
      if (num_general_colors <= MAX_REGISTER &&
          num_vector_colors <= MAX_VECTOR_REGISTER)
        break;

      spillColors(F, num_general_colors, num_vector_colors, general_reg_color,
                  vector_reg_color, decr_sp, not_spill);
    }
    insertSymbols(SM, F, general_reg_color, vector_reg_color);
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::reg_alloc
