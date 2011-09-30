program solver_simple_program

  use class_mesh
  use class_ode_stepper

  use class_module
  use class_module_test
  use class_module_print_data
  use class_trigger
  use class_trigger_timed
  use class_trigger_once

  use class_solver_data
  use class_solver
  use class_solver_simple
  use class_solver_simple_data

  class(solver), pointer :: s
  type(solver_simple_data) :: data
  procedure(interface_rhs), pointer :: rhs
  real :: pi
  integer :: nx = 51

  data = solver_simple_data( &
       mesh_id = "afd5pt",   &
       stepper_id = "rk4cs",    &
       nx      = nx,         &
       nf      = 2,          &
       x0      = 0.,         &
       x1      = 1.,         &
       t0      = 0.,         &
       t1      = 10.,         &
       h0      = 1.e-4,      &
       rhs     = rhs)           !what does it mean?
  ! @todo any way to squeeze this into initialization expression?
  data % rhs => my_rhs

  s => data % generate_solver()

  ! call s % add(&
  !      module_print_data( file_name = "test/test.dat" ),&
  !      trigger_always(test_result=.false.),&
  !      trigger_timed(dt = .001))

  call s % add(                   &
       module_print_data(         &
       file_name = "data/test",   &
       extension = ".dat" ),      &
       trigger_timed( dt = .01 ) )

  ! @todo: implement a functional way to give initial data prepare
  ! initial data
  ! @todo: or put this to the solver_simple_data
  pi = acos(-1.)
  ! s % f(:,1) = 0.
  s % f(:,1) = sin( 2.* s % x * pi )**6
  s % f(:,2) = sin( s % x * pi) ** 6
  s % f(1,:) = 0.
  s % f(nx,:) = 0.

  ! solve the equation up to time t1
  call s % solve

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
    ! do i = 1, s % nx
    !    write(*, "(6E14.5)") &
    !         s%x(i), s%f(i,1), s%f(i,2),&
    !         s%dfdt(i,1), s%dfdt(i,2), s % t
    !    ! write(*, "(6E14.5)") &
    !    !      s%x(i), s%f(i,1), s%dfdt(i,1), s%dfdx(i,1,2)
    !    ! write(*, "(6E14.5)") s%x(i) s%f(i,1)
    ! end do

    ! print *, ""
    ! print *, ""
    ! print *, ""

  end subroutine my_rhs

end program solver_simple_program
