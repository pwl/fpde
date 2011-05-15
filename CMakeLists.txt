cmake_minimum_required(VERSION 2.8)
project( fpde Fortran )

####################

# variables used in out of source build
set( LIBRARY_OUTPUT_PATH ${CMAKE_CURRENT_BINARY_DIR}/lib )
set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake )
# .mod files output directory (fortran specific)
set( CMAKE_Fortran_MODULE_DIRECTORY ${LIBRARY_OUTPUT_PATH})
# add the above path to include path
include_directories( ${LIBRARY_OUTPUT_PATH} )

# create directory for test files
file(MAKE_DIRECTORY "bin/test")

####################

# available compilers
find_program(ifort "ifort")
find_program(gfortran "gfortran-4.6")

# default to intel
if( NOT CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER ${intel})
endif()

set(comp ${CMAKE_Fortran_COMPILER})

# intel flags
if( ${comp} STREQUAL ${ifort})
  set(flags "-ggdb -u -ftrapuv -warn all -gen-interfaces -traceback -stand f03")
endif()

# gfort flags
if( ${comp} STREQUAL ${gfortran})
  set(flags "-ggdb -Wall -pedantic -fimplicit-none -std=f2003 -Wno-unused -Wno-unused-dummy-argument")
endif()

# source files suffix
set(SUFFIX "f90")

# add a subdirectory with a test library
add_subdirectory( src )

# search for source files in ./test
file(GLOB bin_list RELATIVE ${PROJECT_SOURCE_DIR}/test ${PROJECT_SOURCE_DIR}/test/*.${SUFFIX})

# create targets for each file from ./test
foreach(bin ${bin_list})
  string(REPLACE ".${SUFFIX}" "" bin ${bin})

  add_executable(${bin} ${PROJECT_SOURCE_DIR}/test/${bin}.${SUFFIX})

  # set the target binary file location
  set_target_properties(${bin}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/test)

  # set the additional compile flags
  set_target_properties(${bin}
    PROPERTIES
    COMPILE_FLAGS
    ${flags})

  # link to the helper libraries
  target_link_libraries(${bin} fpde)
endforeach(bin)
