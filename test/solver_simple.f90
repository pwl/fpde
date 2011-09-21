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
  real :: pi
  integer :: nx = 101

  pi = acos(-1.)

  rhs => my_rhs

  ! @todo: default control type?
  data = solver_simple_data( &
       mesh_id = "sfd3pt",   &
       step_id = "rkm43",    &
       nx      = nx,        &
       nf      = 2,        &
       x0      = 0.,         &
       x1      = 1.,         &
       t1      = 1.,         &
       h0      = 1.e-2,      &
       rhs     = rhs)
  data % rhs => my_rhs


  call s % init(data)

  ! call data % rhs(s)

  ! call s % info


  ! @todo: implement a functional way to give initial data prepare
  ! initial data
  ! s % f(:,1) = sin( s % x * pi )
  ! s % f(:,2) = 0.
  s % y(1:(nx/2)) = sin( s % x * pi )
  s % y(nx+1:2*nx) = 0.
  s % y(nx) = 0.
  s % y(2*nx) = 0.
  ! print *, s % y(1:6)
  ! print *,""

  ! call my_rhs(s)
  ! call s%mesh%calculate_derivatives(2)
  ! print *, s%mesh%df(:,:,1)
  ! print *,""

  call s % solve

  ! print *, sum(s % f(:,1)-exp(-s%t1*pi**2)*sin( s % x * pi))

  call s % free

contains

  subroutine my_rhs( s )
    class(solver) :: s
    integer :: i

    call s % calculate_dfdx( 2 )

    s % dfdt(:,1) = s % f(:,2)
    s % dfdt(:,2) = s % dfdx(:,1,2)
    ! s % dfdt(:,1) = s % dfdx(:,1,2)

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

    ! @todo: write a wrapper to convert s % y to f % y
    ! print *, "solver_simple with my_rhs (diffusion equation test)"
    do i = 1, s % nx
       write(*, "(6E14.5)") &
            s%x(i), s%f(i,1), s%f(i,2),&
            s%dfdt(i,1), s%dfdt(i,2), s%dfdx(i,1,2)
       ! write(*, "(6E14.5)") &
       !      s%x(i), s%f(i,1), s%dfdt(i,1), s%dfdx(i,1,2)
       ! write(*, "(6E14.5)") s%x(i) s%f(i,1)
    end do

    print *, ""
    print *, ""
    print *, ""



  end subroutine my_rhs


end program solver_simple_program
