program mesh_example_test
  ! first use the module with a desired mesh type
  use class_mesh
  use mesh_factory
  use pretty_print

  class(mesh), pointer :: m
  integer :: i,j,u

  m => mesh_new( "sfd3pt" )
  u = get_unit()
  open(unit=u,&
       file="test/mesh_example.txt",&
       status = 'replace')


  call m%init(5,2,2,0.,1.)

  m%f(:,1)=0.
  forall(i=1:5) m%f(i,2) = sin(i/5.)

  print *, m%df(:,2,2)
  call m%calculate_derivatives(2)
  print *, m%df(:,2,2)

  call m%print_by_index( (/1,2/), u )

  call m%info

end program mesh_example_test
