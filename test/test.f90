program mesh_example_test
  ! first use the module with a desired mesh type
  use module_mesh_example

  ! declare a mesh of a given type
  type(mesh_example) :: m

  ! you can than call the methods (type-bound procedures) with a
  ! uniform manner

  ! initialize mesh
  call m%Init(200,2,2)
  ! print a mesh
  call m%Print
  ! cache derivatives
  call m%CacheDerivatives
  ! free when done with the mesh
  call m%Free
    
end program mesh_example_test
