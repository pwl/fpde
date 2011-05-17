program mesh_sfd3pt_test
  ! first use the module with a desired mesh type
  ! use omp_lib
  use module_mesh_sfd3pt

  ! declare a mesh of a given type
  type(mesh_sfd3pt) :: m
  integer :: rk = 10
  real, pointer :: v(:)
  real :: pi
  ! integer :: t
  pi = 2.*acos(0.)

  ! initialize mesh
  call m % init(100,2,rk,0.,1.)
  ! print a mesh
  ! call m % print_preview
  ! write and read data to and from vector v
  call m % to_vector( v )
  v = sin( m%x )
  call m % from_vector( v )
  ! call m % print_preview
  call m % calculate_derivatives( rk )
  print *, m % df(1:5,1,2)
  ! free when done with the mesh
  call m % free

end program mesh_sfd3pt_test
