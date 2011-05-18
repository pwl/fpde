include_directories( . )

# Search for the source files recursively and add them to fpde_src
file( GLOB_RECURSE fpde_src [^.]*.${SUFFIX} )

# set the additional compile flags
set_source_files_properties(${fpde_src}
  PROPERTIES
  COMPILE_FLAGS
  "${flags}")

# target_link_libraries(${fpde_src} fpde m ${GMP_LIBRARIES})

add_library(fpde SHARED ${fpde_src})
# target_link_libraries(fpde dlmalloc mpreal mpfr gmp)
