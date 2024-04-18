# SWPP Compiler

This compiler converts LLVM IR to SWPP assembly.

NOTE: Use 'Use this template' function to create a private team repository.
Then, the rest of the teammates should fork from the team repository.
Watch out not to create another repos with this template!

Whenever there is a change in this repo, you can cherry-pick the new commits.
Relevant links:
[here](https://coderwall.com/p/sgpksw/git-cherry-pick-from-another-repository),
[here](https://stackoverflow.com/questions/5120038/is-it-possible-to-cherry-pick-a-commit-from-another-git-repository).

## How to compile

To compile this project, you'll need to clone & build LLVM 18.1.0 first.
You can either build LLVM on your own, or use the script we provided
in the main repo

After LLVM 18.1.0 is successfully built, please run:

```
cmake -GNinja -Bbuild
cmake --build build --target swpp-compiler
```

If CMake fails to locate the installed LLVM or you have multiple LLVMs
installed, you can instruct CMake to use the LLVM in specific location
by setting the `LLVM_ROOT` variable in the `CMakeLists.txt`.


## How to test

You should configure tests in the `CMakeLists.txt`

```
cd build
ctest
```


## How to run

Compile LLVM IR `input.ll` into an assembly `a.s` using this command:

```
// adding --verbose prints the IR after each optimization step
build/swpp-compiler input.ll a.s [--verbose]
```


## How to create documents

To build the document, you have to install Doxygen in your environment.
We recommend you use the package manager, but you may build it on your own

After Doxygen is successfully built, please run:

```
cmake -GNinja -Bbuild
cmake --build build --target docs
```

The documents will be created in the `docs` directory.  
You can read the document by opening `docs/html/index.html` with the
web browser.
