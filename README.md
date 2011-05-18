fpde
====

fpde is a set of structures (classes) to unify an exchange of pieces
of numerical software in our group.

Compilation
-----------

CMake is required to compile this library.

To compile fpde out of source first create and enter a directory to store build
data e.g.

    mkdir -p [fpde_build]
    cd [fpde_build]

Then run cmake to generate makefiles

    cmake [path_to_fpde_sources]

After generating makefiles you can compile fpde by typing

    make

Note: After each change in source files running `make` will result in
regeneration of the makefiles. After adding *new* files to fpde you
have to run `cmake [path_to_fpde_sources]` again to generate makefiles
regarding the new files.

### Selecting compiler

You can select a different fortran compiler (altough it has to have a
support for some features of f2003) by executing

    cd [fpde_build]
    rm -f CMakeCache.txt && FC=gfortran-4.6 cmake [path_to_fpde_sources]

In this case `gfortran-4.6` is chosen to be a compiler. Removing
`CmakeCache.txt` forces CMake to rebuild makefiles.

### Changing compiler flags

*This is going to be changed soon!*

To change flags edit `CMakeFile.txt.` in `[path_to_fpde_sources]` find
a case regarding compiler of your choice and edit the flags.
