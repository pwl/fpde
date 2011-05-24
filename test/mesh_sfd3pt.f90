program mesh_sfd3pt_test
  ! first use the module with a desired mesh type
  ! use omp_lib
  use class_mesh
  use class_mesh_sfd3pt
  use pretty_print

  ! declare a mesh of a given type
  type(mesh_sfd3pt) :: m
  integer :: rk = 10
  ! real, pointer :: v(:)
  real :: pi
  real :: xmin, xmax
  integer :: u
  ! integer :: t
  pi = 2.*acos(0.)

  xmin = 0.0
  xmax = 1.0

  u = get_unit()
  open(unit   = u,&
       file   = "test/mesh_sfd3pt.txt",&
       status = 'replace')

  ! initialize mesh
  call m % init(100,2,rk,xmin,xmax)
  ! print a mesh
  call m % info
  ! write and read data to and from vector v
  ! call m % to_vector( v )
  ! v = sin( m%x )
  ! call m % from_vector( v )
  ! call m % print_preview
  call m % calculate_derivatives( rk )
  ! print *, m % df(1:5,1,2)
  ! free when done with the mesh

  call m % print_by_index( (/ 1, 2 /), u )
  call m % free

  close(u)

end program mesh_sfd3pt_test
