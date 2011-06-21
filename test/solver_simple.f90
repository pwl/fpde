program solver_simple_program

  use class_mesh
  use class_stepper

  use class_solver
  use class_solver_simple
  use class_solver_simple_data

  type(solver_simple) :: s
  type(solver_simple_data) :: data
  procedure(interface_rhs), pointer :: rhs
  real, pointer :: y(:)
  real, target :: z(3) = (/ 1,2,3 /)
  real :: pi
  integer :: nx = 10

  pi = acos(-1.)

  rhs => my_rhs

  data = solver_simple_data( &
       mesh_id = "sfd3pt",   &
       step_id = "rk4cs",    &
       nx      = nx,        &
       nf      = 2,        &
       x0      = 0.,         &
       x1      = 1.,         &
       t1      = 1.,         &
       h0      = 1.e-3,      &
       rhs     = rhs)
  data % rhs => my_rhs


  call s % init(data)

  call data % rhs(s)

  call s % info


  ! prepare initial data
  s % f(:,1) = sin( s % x * pi )
  s % y(1:nx) = sin( s % x * pi )
  s % y(nx+1:2*nx) = 0.
  ! print *, s % y
  call my_rhs(s)

  call s % solve

  ! print *, sum(s % f(:,1)-exp(-s%t1*pi**2)*sin( s % x * pi))

  call s % free

contains

  subroutine my_rhs( s )
    class(solver) :: s
    integer :: i

    call s % calculate_dfdx( 2 )

    ! print *, "solver_simple with my_rhs (diffusion equation test)"
    do i = 1, s % nx
       write(*, "(E14.5,E14.5,E14.5)") s%x(i), s % f(i,1), s % f(i,2)
    end do

    print *, ""
    print *, ""
    print *, ""

    s % dfdt(:,1) = s % f(:,2)
    s % dfdt(:,2) = s % dfdx(:,1,2)

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

  end subroutine my_rhs


end program solver_simple_program
