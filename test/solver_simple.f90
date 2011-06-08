program solver_simple_program

  use class_mesh
  use class_mesh_sfd3pt

  ! use class_marcher_dummy
  use class_stepper_rk4cs

  use class_solver
  use class_solver_simple

  ! select a mesh
  type(mesh_sfd3pt) :: m
  ! select a marcher
  type(ode_stepper_rk4cs) :: step
  ! select a solver
  type(solver_simple) :: s
  integer, parameter :: nx = 10
  ! give a maximal time
  real :: max_t, pi
  real, pointer :: param
  real, pointer :: f(:), dfdt(:)
  ! real, pointer :: dfdy

  allocate(f(nx))
  allocate(dfdt(nx))

  pi = acos(-1.)

  ! allocate parameters of the equation
  allocate(param)
  param = 4.

  max_t = 10.

  ! initialize mesh
  call m % init( nx, 1, 2, 0., 1. )

  ! initialize stepper
  call step % init( nx )

  ! initialize solver
  call s % init( m, max_t, my_rhs, step, param )
  call s % init( m, max_t, my_rhs, step, param )

  ! prepare initial data
  s % f(:,1) = s % x * (1. - s % x)

  call s % solve

  ! ! call rhs
  ! call s % rhs_for_marcher(0., f, dfdt, s )

  ! compare dfdf with analytic formula
  ! print *, "total error squared is: "
  ! print *, sqrt(sum((abs(s % dfdt(:,1) - (- pi**2 * sin( s % x * pi ))))**2)/nx)

  ! call s % solve

  call s % free

contains

  subroutine my_rhs( s, params )
    class(solver) :: s
    class(*) :: params
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
