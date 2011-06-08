program solver_simple_program

  use class_mesh
  use class_mesh_sfd3pt

  use class_marcher
  ! use class_marcher_dummy

  use class_solver
  use class_solver_simple

  ! select a mesh
  type(mesh_sfd3pt) :: m
  ! select a marcher
  type(marcher) :: march
  ! select a solver
  type(solver_simple) :: s
  integer, parameter :: nx = 1000
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

  ! initialize mesh
  call m % init( nx, 1, 2, 0., 1. )

  ! initialize marcher
  call march % init( nx )

  ! initialize solver
  call s % init( m, march, max_t, my_rhs, param )

  ! prepare initial data
  s % f(:,1) = sin( s % x * pi)

  ! call rhs
  call s % rhs_for_marcher(0., f, dfdt, s )

  ! compare dfdf with analytic formula
  print *, "total error squared is: "
  print *, sqrt(sum((abs(s % dfdt(:,1) - (- pi**2 * sin( s % x * pi ))))**2)/nx)

  ! call s % solve

  call s % free

contains

  subroutine my_rhs( s, params )
    class(solver) :: s
    class(*) :: params

    call s % calculate_dfdx( 2 )

    print *, "solver_simple with my_rhs (diffusion equation test)"

    s % dfdt(:,:) = s % dfdx(:,:,2)

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

  end subroutine my_rhs


end program solver_simple_program
