program mesh_symmetric_finite_difference_3pt_test
  ! first use the module with a desired mesh type
  use module_mesh_symmetric_finite_difference_3pt

  ! declare a mesh of a given type
  type(mesh_symmetric_finite_difference_3pt) :: m
  real, allocatable :: v(:)
  real :: pi
  pi = acos(0.)

  ! you can than call the methods (type-bound procedures) with a
  ! uniform manner

  ! initialize mesh
  call m%FillForDebug
  ! print a mesh
  call m%PrintPreview
  ! write and read data to and from vector v
  call m%ToVector(v)
  v = sin(m%x/pi)
  call m%FromVector(v)
  call m%PrintPreview
  ! free when done with the mesh
  call m%Free

end program mesh_symmetric_finite_difference_3pt_test
