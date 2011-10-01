program test_solver_mmpde6

  use pretty_print

  use class_solver_data
  use class_solver
  use class_solver_standard
  use class_solver_mmpde6

  use class_module
  use class_module_print_data
  use class_trigger
  use class_trigger_timed
  use class_trigger_always

  integer, parameter :: nx = 101
  integer :: i
  real :: pi, h, xi(nx), dxdt(nx)
  real, pointer :: x(:), m(:)
  type(solver_mmpde6) :: s

  s % t0 = 0.
  s % t1 = 1.
  s % nx = nx
  s % nf = 1
  s % rk = 2
  s % stepper_id = "rk4cs"
  s % rhs => my_rhs1
  s % dt = 1.e-5
  s % x0 = 0.
  s % x1 = 1.
  ! s % g => g
  ! s % calculate_monitor => calculate_monitor

  pi = acos(-1.)
  h = (1.)/real(nx-1)

  call s % init

  call s % add(&
       module_print_data(file_name = "data/test"),&
       trigger_always(test_result = .true.),&
       trigger_timed(dt=.01))

  ! initial point distribution suited for the initial data of the form
  ! f(x) = x*(1-x)
  xi = s % x
  s % x = 0.5*(2.414213562373095 - 1.*sqrt(5.82842712474619 - \
          7.656854249492381*xi))
  forall(i = 1 : nx/2)
     s % x(nx-(i-1)) = 1. - s % x(i)
  end forall


  s % f(:,1) = s%x*(1.-s%x)

  ! call s % info
  call s % calculate_dfdx(2)
  call s % calculate_dfdx(1)
  call s % calculate_monitor

  x => s % x
  m => s % monitor

  forall( i = 2 : nx - 1 ) &
       dxdt( i ) = 1.&
       * ( ( m(i+1) + m(i) ) * ( x(i+1) - x(i) ) &
       -   ( m(i) + m(i+1) ) * ( x(i) - x(i-1))) &
       /(2.*h**2)

  do i = 1, nx
     print *, x(i), dxdt(i), s % monitor(i)
  end do


  ! call s % solve

contains

  subroutine my_rhs1( s )
    class(solver) :: s
    integer :: i

    ! call s % calculate_dfdx( 2 )

    s % dfdt(:,1) = s % dfdx(:,1,2)
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
    g = 1. ! (abs(u_x_0) + 1.)/(abs(u_tx_0) + 1.)

  end function g

  subroutine calculate_monitor(s)
    class(solver_mmpde6) :: s
    real, pointer ::  dxdxi(:,:,:)

    dxdxi => s % physical % df

    ! M(u) = |f_x| + sqrt(|f_xx|)
    s % monitor(:) = abs(dxdxi(:,1,1)) + sqrt(abs(dxdxi(:,1,2)))

  end subroutine calculate_monitor



end program test_solver_mmpde6
