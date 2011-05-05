program mesh_example_test
  ! first use the module with a desired mesh type
  use module_mesh_example

  ! declare a mesh of a given type
  type(mesh_example) :: m
  real, allocatable :: v(:)
  allocate(v(3*2))

  ! you can than call the methods (type-bound procedures) with a
  ! uniform manner

  ! initialize mesh
  call m%FillForDebug
  ! print a mesh
  call m%Print
  ! fill vector v with values of the functions
  call m%ToVector(v)
  print *, v
  ! free when done with the mesh
  call m%Free

end program mesh_example_test
