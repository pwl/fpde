include_directories( . )

# Search for the source files recursively and add them to fpde_src
file( GLOB_RECURSE fpde_src "[^.]*.${SUFFIX}" )
file( GLOB_RECURSE fpde_src_f "[^.]*.f" )
set(flags "${flags} -stand f77")

# set the additional compile flags
set_source_files_properties("${fpde_src}"
  PROPERTIES
  COMPILE_FLAGS
  "${flags}")

set_source_files_properties("${fpde_src_f}"
  PROPERTIES
  COMPILE_FLAGS
  "${flags}")

# target_link_libraries(${fpde_src} fpde m ${GMP_LIBRARIES})

add_library(fpde SHARED ${fpde_src} ${fpde_src_f})
target_link_libraries(fpde blas lapack)
