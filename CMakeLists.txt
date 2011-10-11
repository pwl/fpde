cmake_minimum_required(VERSION 2.8)
project( fpde Fortran )

####################

# variables used in out of source build
set( LIBRARY_OUTPUT_PATH ${CMAKE_CURRENT_BINARY_DIR}/lib )
set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake )
set( CMAKE_INSTALL_PREFIX "/usr/local/" )

# .mod files output directory (fortran specific)
set( CMAKE_Fortran_MODULE_DIRECTORY ${LIBRARY_OUTPUT_PATH}/mod)
# install ( CODE
#   "EXECUTE_PROCESS (COMMAND \"${CMAKE_COMMAND}\" -E copy_directory \"${CMAKE_Fortran_MODULE_DIRECTORY}/\" \"${CMAKE_INSTALL_PREFIX}/fortran\")"
# )
# add the above path to include path
include_directories( ${LIBRARY_OUTPUT_PATH} )

# configuration dir
set( confdir "${CMAKE_CURRENT_SOURCE_DIR}/config")

# create directory for test files
file(MAKE_DIRECTORY "bin/test")

# add a target to generate API documentation with Doxygen
find_package(Doxygen)
if(DOXYGEN_FOUND)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
  add_custom_target(doc
    ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation with Doxygen" VERBATIM
    )
endif(DOXYGEN_FOUND)

####################

# available compilers
find_program(ifort "ifort")
find_program(gfortran "gfortran")

set(comp ${CMAKE_Fortran_COMPILER})

# INTEL COMPILER
if( ${comp} STREQUAL ${ifort})
  set(flags "@${confdir}/ifort.cfg @${confdir}/ifort_precision.cfg")
  set(flags_f77 "@${confdir}/ifort_f77.cfg @${confdir}/ifort_precision.cfg")

  # MKL LINKING FLAGS
  # set(lflags "-L$(MKLROOT)/lib/ia32  -Wl,--start-group -lmatmul -lmkl_intel -lmkl_intel_thread -lmkl_core -Wl,--end-group -openmp -lpthread")
  # set(CMAKE_SHARED_LINKER_FLAGS ${lflags})
  # set(CMAKE_EXE_LINKER_FLAGS ${lflags})
  # END OF MKL LINKING FLAGS
  # set(comp_short "ifort")
endif()

# GFORTRAN COMPILER
if( ${comp} STREQUAL ${gfortran})
  set(flags "-ggdb -Wall -pedantic -Wno-unused -Wno-unused-dummy-argument  -fdefault-real-8 -fdefault-double-8")
  set(flags_f77 "${flags}")
endif()

# source files suffix
set(suffix "f90")
set(suffix_f77 "f")

# set(libs "-llapack -lblas")
# set(libs "-openmp")
# set(libs "-L$(MKLROOT)/lib/ia32 -lmkl_intel -lmkl_intel_thread -lmkl_core -openmp -lpthread -threads")
# set(libs "-L$MKLROOT/lib/ia32  -lmkl_blas95 -lmkl_lapack95 -lmkl_intel -lmkl_intel_thread -lmkl_core -openmp -lpthread")

# add a subdirectory with a test library
add_subdirectory( src )

# search for source files in ./test
file(GLOB bin_list RELATIVE ${PROJECT_SOURCE_DIR}/test ${PROJECT_SOURCE_DIR}/test/*.${suffix})

# create targets for each file from ./test
foreach(bin ${bin_list})
  string(REPLACE ".${suffix}" "" bin ${bin})

  add_executable(${bin} ${PROJECT_SOURCE_DIR}/test/${bin}.${suffix})

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
  target_link_libraries(${bin} fpde ${libs})
endforeach(bin)

# install the module files
install(
  DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  DESTINATION "lib/fortran/${CMAKE_Fortran_COMPILER_ID}/fpde" )

# install the scripts
file(GLOB scripts "${CMAKE_CURRENT_SOURCE_DIR}/scripts/*.[sp][hy]")
install(
  PROGRAMS ${scripts}
  DESTINATION bin
  )

# install the ifort config files
file(GLOB scripts "${CMAKE_CURRENT_SOURCE_DIR}/config/*")
install(
  FILES ${scripts}
  DESTINATION share/fpde
  )

# install the fpde.cmake file used to set all the libraries and
# include paths
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/misc/fpde.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/misc/fpde.cmake"
  @ONLY)

message("${CMAKE_CURRENT_BINARY_DIR}/misc/fpde.cmake")

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/misc/fpde.cmake"
  DESTINATION share/fpde
  )

export(PACKAGE fpde)
