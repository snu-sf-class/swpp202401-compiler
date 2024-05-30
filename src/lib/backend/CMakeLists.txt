cmake_minimum_required(VERSION 3.28.0)

add_library(SCBackendAnalysis OBJECT analysis.cpp)
target_include_directories(SCBackendAnalysis PRIVATE ${LLVM_INCLUDE_DIRS})

add_library(SCBackendEmitter OBJECT emitter.cpp)
target_include_directories(SCBackendEmitter PRIVATE ${LLVM_INCLUDE_DIRS})

add_library(SCBackendSymbol symbol.cpp)
llvm_map_components_to_libnames(backend_symbol_llvm_libs core support)
target_link_libraries(SCBackendSymbol PRIVATE
    SCBackendSymbolBase ${backend_symbol_llvm_libs})
target_include_directories(SCBackendSymbol PRIVATE ${LLVM_INCLUDE_DIRS})

function(add_backend_pass pass_name file_name)
    add_library(${pass_name} OBJECT ${file_name})
    target_include_directories(${pass_name} PRIVATE ${LLVM_INCLUDE_DIRS})
endfunction()

add_backend_pass(SCBackendConstExprElimPass const_expr_eliminate.cpp)
add_backend_pass(SCBackendConstMap const_map.cpp)
add_backend_pass(SCBackendConstSplitPass const_split.cpp)
add_backend_pass(SCBackendFreezeElimPass freeze_eliminate.cpp)
add_backend_pass(SCBackendGEPElimPass gep_eliminate.cpp)
add_backend_pass(SCBackendGVElimPass gv_eliminate.cpp)
add_backend_pass(SCBackendAllocaElimPass alloca_eliminate.cpp)
add_backend_pass(SCBackendGEPConstCombPass gep_const_combine.cpp)
add_backend_pass(SCBackendPHIPreprocessPass phi_preprocess.cpp)
add_backend_pass(SCBackendSextEliminatePass sext_eliminate.cpp)
add_backend_pass(SCBackendTruncAdjustPass trunc_adjust.cpp)
add_backend_pass(SCBackendRegAllocPass register_allocate.cpp)

add_subdirectory(assembly)
add_subdirectory(symbol)
