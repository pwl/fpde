cmake_minimum_required(VERSION 2.8)

set( fpde_prefix "~" )
set( fpde_mod    "${fpde_prefix}/lib/fortran/Intel/fpde/mod" )
set( fpde_lib    "${fpde_prefix}/lib" )
set( fpde_cfg    "${fpde_prefix}/share/fpde" )
include_directories( ${fpde_mod} )
link_directories( ${fpde_lib} )

files(
  GLOB prog_list
  RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/src
  "*.f90" )

foreach(prog ${prog_list})
  add_executable( prog )

endforeach(prog)

target_link_libraries( program fpde )
