## How to add your optimization pass to the compiler

1. Implement new optimization pass in this directory
2. Include the header of your pass in [opt.cpp](../opt.cpp)
3. Add your pass using one of FPM, CGPM, or MPM.
4. In [CMakeLists.txt](../../../CMakeLists.txt), add the following in the
designated area where it says `ADD OPT PASSES BELOW`
> ```add_opt_pass(YourPassName your_pass_source_file.cpp)```
