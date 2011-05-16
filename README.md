fpde
====

fpde is a set of structures (classes) to unify an exchange of pieces
of numerical software in our group.

Compilation
-----------

CMake is required to compile this library.

To compile fpde out of source first create and enter a directory to store build
data e.g.

`mkdir -p ~/fpde_build`
`cd fpde_build`

Then run cmake to generate makefiles

`cmake [path_to_fpde_sources]`

After generating makefiles you can compile fpde by typing

`make`

Note: After each change in source files running make will result in
regeneration of the makefiles. After adding *new* files to fpde you have
to run cmake again to generate makefiles regarding the new files.
