program mesh_sfd3pt_test
  ! first use the module with a desired mesh type
  use module_mesh_sfd3pt

  ! declare a mesh of a given type
  type(mesh_sfd3pt) :: m
  real, allocatable :: v(:)
  real :: pi
  pi = 2.*acos(0.)

  ! initialize mesh
  call m % init(1000000,1,30,0.,1.)
  ! print a mesh
  call m % print_preview
  ! write and read data to and from vector v
  call m % to_vector( v )
  v = m%x! sin( m%x * 10 * pi )
  call m % from_vector( v )
  call m % print_preview
  call m % calculate_derivatives( 30 )
  ! print *, m % df(:,1,1)
  ! print *, m % df(:,1,2)
  ! free when done with the mesh
  call m % free

end program mesh_sfd3pt_test
