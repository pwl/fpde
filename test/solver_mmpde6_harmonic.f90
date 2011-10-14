program test_solver_mmpde6

  use pretty_print

  use class_solver_data
  use class_solver
  use class_solver_standard
  use class_solver_mmpde6

  use class_module
  use class_module_print_data
  use class_module_print_scalar_data
  use class_module_solver_stop
  use class_trigger
  use class_trigger_timed
  use class_trigger_always
  use class_trigger_f_control
  use class_trigger_every_n_iter
  use class_trigger_non_monotonic

  integer :: i, nx
  real :: pi, h
  real, pointer :: x(:), m(:)
  type(solver_mmpde6) :: s
  character(len=100) :: buffer
  real :: eps0
  pi = acos(-1.)
  print *, pi

  ! read eps0 as an argument
  call get_command_argument(1,buffer)
  read(buffer, *) eps0
  call get_command_argument(2,buffer)
  read(buffer, *) nx

  s % stepper_id = "rkpd54"
  s % t0 = 0.  !this is the initial physical time
  s % t1 = 1.e10 !this is the maximal computational time
  s % nx = nx
  s % nf = 1
  s % rk = 2
  s % rhs => my_rhs1
  s % x0 = 0.  !physical domain specifiers
  s % x1 = pi
  ! functions ruling the mmpde6 method, user defined
  s % calculate_monitor => calculate_monitor
  s % initial => initial
  s % g => g
  s % epsilon => epsilon
  ! marcher parameters
  s % abs_error = 1.e-25
  s % rel_error = 1.e-25
  s % dt = 1.e-20  !this is used to initialize dtau, but after running
                   !the solver it is rewritten with dt := g*dtau
  allocate( s % user_data_scalars( 4 ) )
  allocate( s % user_data_scalars_names( 4 ) )
  s % user_data_scalars_names(1) = "u_x_0"
  s % user_data_scalars_names(2) = "u_tx_0"
  s % user_data_scalars_names(3) = "x1"
  s % user_data_scalars_names(4) = "eps0"

  s % user_data_scalars(4) = eps0

  h = (1.)/real(nx-1)

  call s % init

  call s % add(&
       module_print_data(),&
       trigger_every_n_iter(dn = 100))

  call s % add(&
       module_print_scalar_data(),&
       trigger_every_n_iter(dn = 100))

  ! problems arise when
  ! call s % add(&
  !      module_solver_stop(),&
  !      trigger_every_n_iter(dn = 100),&
  !      trigger_f_control(max=pi/2.+.01, center=pi/2.))

  ! problems arise when
  call s % add(&
       module_solver_stop(),&
       trigger_every_n_iter(dn = 100),&
       trigger_non_monotonic( i = 1, increasing = .true.))

  call s % solve

contains

  subroutine initial( x, f, params )
    real, intent(in)   :: x(:)
    real, intent(out)   :: f(:,:)
    class(*), pointer  :: params

    f(:,1) = x + sin(x)

  end subroutine initial


  subroutine my_rhs1( s )
    class(solver) :: s
    integer :: i
    real, pointer :: x(:), f(:), df(:), d2f(:)
    real, parameter :: d = 7.
    x   => s%x
    f   => s%f(:,1)
    df  => s%dfdx(:,1,1)
    d2f => s%dfdx(:,1,2)

    s % dfdt(:,1) = d2f + (d-1.)*df/x - (d-1)/2.*sin(2.*f)/x**2

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

  end subroutine my_rhs1

  real function g(s)
    class(solver_mmpde6) :: s
    real :: u_tx_0, u_x_0

    ! @todo physical2 % derivative is not working
    ! use the previously calculated value of u_x(x=0)
    u_x_0 = s % physical % df(1,1,1)
    ! calculate u_xt(x=0)
    u_tx_0 = s % physical2 % derivative( 1, 1, 1 )
    s % user_data_scalars(1) = u_x_0
    s % user_data_scalars(2) = u_tx_0
    s % user_data_scalars(3) = s % x(2)

    ! the value of g is custom suited to the problem
    ! u_x_0 = abs(s%f(2,1)-s%f(1,1))/(s%x(2)-s%x(1))
    g = 0.5*(abs(u_x_0) + 1.)/(abs(u_tx_0) + 1.)
    ! g = u_x_0**-2
    ! g = 1.! 0.5*(abs(u_x_0))/(abs(u_tx_0))

  end function g

  subroutine calculate_monitor(s)
    class(solver_mmpde6) :: s
    ! real, pointer ::  dfdx(:,:,:), f(:,:)
    integer :: nx
    real :: norm
    real, pointer :: x(:), f(:), df(:), d2f(:), m(:), dx(:)
    nx = s % nx
    x   => s%x
    dx  => s%physical%dx
    ! f   => s%f(:,1)
    df  => s%dfdx(:,1,1)
    d2f => s%dfdx(:,1,2)
    m   => s%monitor

    ! print *, "DEBUG: calculate_monitor"

    ! M(u) = |f_x| + sqrt(|f_xx|)
    m = 5.*abs(df) + sqrt(abs(d2f))
    ! convolution:
    ! m(2:nx-1) = (m(1:nx-2) + m(3:nx) + 2.*m(2:nx-1))/4.

    ! weighted convolution:
    ! call s % physical % calculate_spacings
    ! m(2:nx-1) = (&
    !      m(1:nx-2)*dx(1:nx-2)&
    !      + m(3:nx)*dx(3:nx) &
    !      + 2.*m(2:nx-1)*dx(2:nx-1))/4.

    norm = s % physical % integrate(s%monitor)
    s % monitor = s % monitor / norm !+ .1

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
    real :: eps0
    eps0 = s % user_data_scalars(4)
    epsilon = 0.1*sqrt(g) + eps0
    ! epsilon = 5.e-2

  end function epsilon

end program test_solver_mmpde6
