cmake_minimum_required(VERSION 3.28.0)

add_library(OptPasses INTERFACE)

llvm_map_components_to_libnames(pass_llvm_libs passes analysis core support)
function(add_opt_pass pass_name file_name)
    add_library(${pass_name} ${CMAKE_CURRENT_SOURCE_DIR}/${file_name})
    target_include_directories(${pass_name} PRIVATE ${LLVM_INCLUDE_DIRS})
    target_link_libraries(${pass_name} PRIVATE ${pass_llvm_libs})
    target_link_libraries(OptPasses INTERFACE ${pass_name})
endfunction()

# /================= ADD OPT PASSES BELOW ================/
