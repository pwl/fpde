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

  pi = acos(-1.)

  rhs => my_rhs

  data = solver_simple_data( &
       mesh_id = "sfd3pt",   &
       step_id = "rk4cs",    &
       nx      = 10,        &
       nf      = 2,        &
       x0      = 0.,         &
       x1      = 1.,         &
       t1      = 1.,         &
       h0      = 1.e-3,      &
       rhs     = rhs)

  call s % init(data)

  call s % info

  ! prepare initial data
  s % f(:,1) = sin( s % x * pi )
  print *, s % y

  ! call s % solve

  ! print *, sum(s % f(:,1)-exp(-s%t1*pi**2)*sin( s % x * pi))

  call s % free

contains

  subroutine my_rhs( s )
    class(solver) :: s
    integer :: i

    call s % calculate_dfdx( 2 )

    ! print *, "solver_simple with my_rhs (diffusion equation test)"
    do i = 1, s % nx
       write(*, "(E14.5,E14.5)") s%x(i), s % f(i,1)
    end do

    print *, ""
    print *, ""
    print *, ""

    s % dfdt(:,:) = s % dfdx(:,:,2)

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

  end subroutine my_rhs


end program solver_simple_program
