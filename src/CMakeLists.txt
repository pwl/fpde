include_directories( . )

# Search for the source files recursively and add them to fpde_src
file( GLOB_RECURSE fpde_src "[^.]*.${suffix}" )
# file( GLOB_RECURSE fpde_src_f77 "[^.]*.${suffix_f77}" )


# set the additional compile flags
set_source_files_properties(${fpde_src}
  PROPERTIES
  COMPILE_FLAGS
  "${flags}")

# set_source_files_properties(${fpde_src_f77}
#   PROPERTIES
#   COMPILE_FLAGS
#   "${flags_f77}")

# add_library(fpde SHARED ${fpde_src} ${fpde_src_f77})
add_library(fpde SHARED ${fpde_src})
target_link_libraries(fpde blas lapack)

install(
  TARGETS fpde
  DESTINATION lib
  EXPORT fpde.cmake)
