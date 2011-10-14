program test_solver_mmpde6

  use pretty_print

  use class_solver_data
  use class_solver
  use class_solver_standard
  use class_solver_mmpde6

  use class_module
  use class_module_print_data
  use class_module_solver_stop
  use class_trigger
  use class_trigger_timed
  use class_trigger_always
  use class_trigger_f_control
  use class_trigger_every_n_iter

  integer, parameter :: nx = 41
  integer :: i
  real :: pi, h, xi(nx), dxdt(nx)
  real, pointer :: x(:), m(:)
  type(solver_mmpde6) :: s

  s % t0 = 0.  !this is the initial physical time
  s % t1 = 1.e10 !this is the maximal computational time
  s % nx = nx
  s % nf = 1
  s % rk = 2
  s % rhs => my_rhs1
  s % x0 = 0.  !physical domain specifiers
  s % x1 = 1.
  ! functions ruling the mmpde6 method, user defined
  s % calculate_monitor => calculate_monitor
  s % initial => initial
  s % g => g
  s % epsilon => epsilon
  ! marcher parameters
  s % abs_error = 1.e-14
  s % rel_error = 1.e-14
  s % dt = 1.e-10  !this is used to initialize dtau, but after running
                   !the solver it is rewritten with dt := g*dtau

  h = (1.)/real(nx-1)

  call s % init

  call s % add(&
       module_print_data(file_name = "data/test"),&
       trigger_every_n_iter(dn = 100))

  call s % solve

contains

  subroutine initial( x, f, params )
    real, intent(in)   :: x(:)
    real, intent(out)   :: f(:,:)
    class(*), pointer  :: params

    pi = acos(-1.)
    f(:,1) = 100.*sin(pi*x) * x
    f(1,1) = 0.
    f(nx,1) = 0.
  end subroutine initial


  subroutine my_rhs1( s )
    class(solver) :: s
    integer :: i

    ! call s % calculate_dfdx( 2 )

    s % dfdt(:,1) = s % dfdx(:,1,2) + (s % f(:,1))**2
    ! s % dfdt(:,2) = s % dfdx(:,1,2)

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

  end subroutine my_rhs1

  real function g(s)
    class(solver_mmpde6) :: s
    real :: u_tx_0, u_x_0

    ! @todo physical2 % derivative is not working
    ! ! use the previously calculated value of u_x(x=0)
    ! u_x_0 = s % dfdx(1,1,1)
    ! ! calculate u_xt(x=0)
    ! u_tx_0 = s % physical2 % derivative( 1, 1, 1 )

    ! the value of g is custom suited to the problem
    g = 1./ maxval(abs(s%f(:,1))) ! (abs(u_x_0) + 1.)/(abs(u_tx_0) + 1.)

  end function g

  subroutine calculate_monitor(s)
    class(solver_mmpde6) :: s
    real, pointer ::  dfdx(:,:,:), f(:,:)
    real :: norm

    ! print *, "DEBUG: calculate_monitor"

    dfdx => s % dfdx
    f => s % f

    ! M(u) = |f_x| + sqrt(|f_xx|)
    ! s % monitor(:) = abs(dfdx(:,1,1)) + sqrt(abs(dfdx(:,1,2)))
    ! s % monitor(:) = sqrt(1+dfdx(:,1,1)**2)
    s % monitor(:) = f(:,1)
    norm = s % physical % integrate(s%monitor)
    s % monitor = s % monitor / norm + .1

  end subroutine calculate_monitor

  !>
  !!
  !! @param g
  !!
  !! @return
  !!
  ! this function was found to be giving best results, see Biernat and
  ! Bizon [2011]
  real function epsilon(s,g)
    class(solver_mmpde6) :: s
    real :: g
    ! epsilon = 100. * sqrt(g) + .05
    ! ! epsilon = 100. * sqrt(min(abs(g),1.e-4)) + .05
    epsilon = 6.*sqrt(g) + .05
    ! epsilon = .5

  end function epsilon

end program test_solver_mmpde6
