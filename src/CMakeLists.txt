xinclude_directories( . )

# Search for the source files recursively and add them to yapdes_src
file( GLOB_RECURSE yapdes_src [^.]*.${SUFFIX} )

# set the additional compile flags
set_source_files_properties(${yapdes_src}
  PROPERTIES
  COMPILE_FLAGS
  "${flags}")

# target_link_libraries(${yapdes_src} yapdes m ${GMP_LIBRARIES})

add_library(fpde SHARED ${yapdes_src})
# target_link_libraries(yapdes dlmalloc mpreal mpfr gmp)
