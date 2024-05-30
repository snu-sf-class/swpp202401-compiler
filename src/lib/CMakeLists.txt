cmake_minimum_required(VERSION 3.28.0)

add_library(SCParser parser.cpp)
target_include_directories(SCParser PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(parser_llvm_libs asmparser core support)
target_link_libraries(SCParser PRIVATE ${parser_llvm_libs})

add_library(SCPrintIR print_ir.cpp)
target_include_directories(SCPrintIR PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(printer_llvm_libs core support)
target_link_libraries(SCPrintIR ${printer_llvm_libs})

add_library(SCOpt opt.cpp)
target_include_directories(SCOpt PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(opt_llvm_libs analysis passes scalaropts transformutils)
target_link_libraries(SCOpt PRIVATE SCPrintIR OptPasses ${opt_llvm_libs})

add_library(SCBackend backend.cpp)
target_include_directories(SCBackend PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(backend_llvm_libs analysis scalaropts transformutils)
target_link_libraries(SCBackend PRIVATE
                        SCPrintIR SCBackendAssembly SCBackendAnalysis
                        SCBackendEmitter SCBackendSymbol  SCBackendFreezeElimPass
                        SCBackendConstExprElimPass SCBackendConstSplitPass SCBackendConstMap
                        SCBackendGEPElimPass SCBackendGVElimPass SCBackendAllocaElimPass
                        SCBackendGEPConstCombPass SCBackendPHIPreprocessPass
                        SCBackendSextEliminatePass SCBackendTruncAdjustPass
                        SCBackendRegAllocPass ${backend_llvm_libs})

add_subdirectory(backend)
add_subdirectory(opt)
