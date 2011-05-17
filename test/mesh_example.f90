program mesh_example_test
  ! first use the module with a desired mesh type
  use module_mesh_example

  ! declare a mesh of a given type
  type(mesh_example) :: m
  real, pointer :: v(:)

  ! you can than call the methods (type-bound procedures) with a
  ! uniform manner

  ! initialize mesh
  call m % fill_for_debug
  ! print a mesh
  call m % print_preview
  ! write and read data to and from vector v
  call m % to_vector(v)
  call m % from_vector(v)
  ! free when done with the mesh
  call m % free

end program mesh_example_test
