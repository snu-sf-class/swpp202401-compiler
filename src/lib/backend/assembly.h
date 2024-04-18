#ifndef SC_LIB_BACKEND_ASSEMBLY_H
#define SC_LIB_BACKEND_ASSEMBLY_H

#include "assembly/inst.h"
#include "assembly/int_t.h"
#include "assembly/register_t.h"
#include "assembly/width_t.h"

namespace sc::backend::assembly {
using AccessWidth = width_t::AccessWidth;
using BitWidth = width_t::BitWidth;
using VectorWidth = width_t::VectorWidth;
using IcmpCondition = inst::IcmpCondition;

using IntTy = int_t::IntTy;
using GeneralRegister = register_t::GeneralRegister;
using ArgumentRegister = register_t::ArgumentRegister;
using VectorRegister = register_t::VectorRegister;
using ScalarRegisterTy = inst::ScalarRegisterTy;
using ScalarValueTy = inst::ScalarValueTy;

using FunctionStartInst = inst::FunctionStartInst;
using FunctionEndInst = inst::FunctionEndInst;
using BasicBlockInst = inst::BasicBlockInst;
using CommentInst = inst::CommentInst;
using ReturnInst = inst::ReturnInst;
using JumpInst = inst::JumpInst;
using BranchInst = inst::BranchInst;
using SwitchInst = inst::SwitchInst;
using MallocInst = inst::MallocInst;
using FreeInst = inst::FreeInst;
using LoadInst = inst::LoadInst;
using StoreInst = inst::StoreInst;
using IntAddInst = inst::IntAddInst;
using IntSubInst = inst::IntSubInst;
using IntMulInst = inst::IntMulInst;
using IntUDivInst = inst::IntUDivInst;
using IntSDivInst = inst::IntSDivInst;
using IntURemInst = inst::IntURemInst;
using IntSRemInst = inst::IntSRemInst;
using IntAndInst = inst::IntAndInst;
using IntOrInst = inst::IntOrInst;
using IntXorInst = inst::IntXorInst;
using IntShlInst = inst::IntShlInst;
using IntLShrInst = inst::IntLShrInst;
using IntAShrInst = inst::IntAShrInst;
using IntCompInst = inst::IntCompInst;
using SelectInst = inst::SelectInst;
using CallInst = inst::CallInst;
using RecursiveCallInst = inst::RecursiveCallInst;
using AssertEqInst = inst::AssertEqInst;

using AsyncLoadInst = inst::AsyncLoadInst;
using IntSumInst = inst::IntSumInst;
using IntIncrInst = inst::IntIncrInst;
using IntDecrInst = inst::IntDecrInst;

using ConstantInst = inst::ConstantInst;

using VectorStoreInst = inst::VectorStoreInst;
using VectorLoadInst = inst::VectorLoadInst;

using VectorAddInst = inst::VectorAddInst;
using VectorSubInst = inst::VectorSubInst;

using VectorMulInst = inst::VectorMulInst;
using VectorUDivInst = inst::VectorUDivInst;
using VectorSDivInst = inst::VectorSDivInst;
using VectorURemInst = inst::VectorURemInst;
using VectorSRemInst = inst::VectorSRemInst;

using VectorAndInst = inst::VectorAndInst;
using VectorOrInst = inst::VectorOrInst;
using VectorXorInst = inst::VectorXorInst;

using VectorShlInst = inst::VectorShlInst;
using VectorLShrInst = inst::VectorLShrInst;
using VectorAShrInst = inst::VectorAShrInst;

using VectorIncrInst = inst::VectorIncrInst;
using VectorDecrInst = inst::VectorDecrInst;

using VectorCompInst = inst::VectorCompInst;

using VectorSelectInst = inst::VectorSelectInst;

using VectorParallelAddInst = inst::VectorParallelAddInst;
using VectorParallelSubInst = inst::VectorParallelSubInst;

using VectorParallelMulInst = inst::VectorParallelMulInst;
using VectorParallelUDivInst = inst::VectorParallelUDivInst;
using VectorParallelSDivInst = inst::VectorParallelSDivInst;
using VectorParallelURemInst = inst::VectorParallelURemInst;
using VectorParallelSRemInst = inst::VectorParallelSRemInst;

using VectorParallelAndInst = inst::VectorParallelAndInst;
using VectorParallelOrInst = inst::VectorParallelOrInst;
using VectorParallelXorInst = inst::VectorParallelXorInst;

using VectorParallelShlInst = inst::VectorParallelShlInst;
using VectorParallelLShrInst = inst::VectorParallelLShrInst;
using VectorParallelAShrInst = inst::VectorParallelAShrInst;

using VectorParallelCompInst = inst::VectorParallelCompInst;

using VectorParallelSelectInst = inst::VectorParallelSelectInst;

using VectorBroadcastInst = inst::VectorBroadcastInst;
using VectorExtractInst = inst::VectorExtractInst;
using VectorUpdateInst = inst::VectorUpdateInst;
} // namespace sc::backend::assembly
#endif // SC_LIB_BACKEND_ASSEMBLY_H
