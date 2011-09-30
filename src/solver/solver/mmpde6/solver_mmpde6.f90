module class_solver_mmpde6

  use class_solver
  use class_mesh
  use class_ode_marcher
  use class_ode_stepper
  use class_ode_system
  use class_ode_step_control

  use mesh_factory

  use utils_greens

  private

  type, public, extends(solver) :: solver_mmpde6
     ! initialization parameters
     ! end of initialization parameters
     real, pointer :: tau
     ! physical mesh is used to calculate d/dx of f(:,:)
     class(mesh), pointer :: physical
     ! physical2 mesh is used to calculate d/dx of d/dt of f(:,:)
     class(mesh), pointer :: physical2
     ! all the data goes into various sectors of this vector
     real, contiguous, pointer :: y(:)
     real, contiguous, pointer :: monitor(:)
     ! greens function for -D^2 ( so that G is positive definite )
     real, contiguous, pointer :: greens(:,:)
     integer :: total_nf
     ! spacing of the computational mesh
     real :: h
     real, contiguous, pointer :: temporary(:)

     ! marcher data
     character(len=30) :: stepper_id = ""
     character(len=30) :: marcher_id = ""
     character(len=30) :: step_control_id = ""
     class(ode_stepper), pointer :: stepper
     class(*), pointer :: data
     class(ode_system), pointer :: system
     class(ode_marcher), pointer :: marcher
     class(ode_step_control), pointer :: step_control
   contains
     procedure :: set_pointers
     procedure :: g
     procedure :: calculate_monitor
  end type solver_mmpde6

contains

  subroutine init(s)
    class(solver_mmpde6) :: s
    integer :: nx,nf,rk,total_nf
    real :: xmin,xmax

    if( s % rk > 2) then
       print *, trim(s%name), " this sovler works only for 0 < rk < 2, setting rk = 2"
       s % rk = 2
    end if

    nx = s % nx
    nf = s % nf
    rk = s % rk
    xmin = s % x0
    xmax = s % x1
    s % h = (s%x1-s%x0)/real(nx-1)

    call s % solver % init

    s % physical => mesh_new("afd5pt")
    s % physical2 => mesh_new("afd5pt")
    ! s % computational => mesh_new("sfd3pt")

    ! physical mesh to hold function values and data points
    call s % physical % init( nx, nf, rk, xmin, xmax )

    ! physical2 mesh is used to calculate d/dx of d/dt of f used for
    ! the dilation transform described by g(). It is required to
    ! calculate only the first spatial derivative at most
    call s % physical2 % init( nx, nf, 1, xmin, xmax)

    ! allocate the memory used to contain all of the sovler data
    ! the last +1 is for the physical time
    allocate( s % y( nx * (nf + 1) + 1) )

    ! allocate the memory for computational time
    allocate( s % tau )

    ! deallocate the memory allocated by meshes
    deallocate( s%physical%f,  s%physical%x )
    deallocate( s%physical2%f, s%physical2%x )

    ! allocate memory for monitor(:)
    allocate( s % monitor( nx ) )

    ! allocate temporary table
    allocate( s % temporary( nx ) )

    ! allocate and calculate the greens function
    allocate( s % greens( nx, nx ) )
    call discrete_greens( s % greens, xmax - xmin )

    ! set the appropriate interface pointers to current values
    call s % set_pointers( y = s % y )

  end subroutine init

  subroutine set_pointers( s, t, y, dydt )
    class(solver_mmpde6) :: s
    real, target, optional, intent(in) :: y(:), dydt(:)
    real, target, optional, intent(in) :: t
    integer :: nx,total_nf,nf,rk
    nx = s % nx
    nf = s % nf
    rk = s % rk
    total_nf = nf + 2           !total_nf should always be nf+2

    if( present( t ) ) then
       s % tau => t
    end if

    if( present( y ) ) then
       ! f is assigned in the following way
       ! f(:, 1:nf) are, as expected functions introduced by user
       s % x( 1 : nx ) => y( nx * nf + 1 : nx * (nf + 1) )
       s % f( 1 : nx, 1 : nf ) => y( 1 : nx * nf )
       s % t => y( nx * total_nf + 1 )

       ! physical mesh differentiates over physical domain
       s % physical % x( 1 : nx ) => y( nx * nf+ 1 : nx * (nf + 1) )
       s % physical % f( 1 : nx, 1 : nf ) => y( 1 : nx * nf )

       ! we update the physical2 mesh domain (should be the same as
       ! the s % physical mesh domain)
       s % physical2 % x( 1 : nx ) => y( nx * nf + 1 : nx * (nf + 1) )

       ! we also update the interface poitners to the spatial
       ! derivatives
       s % dfdx(1:nx,1:nf,1:rk) => s % physical % df
    end if

    if( present( dydt ) ) then
       ! in rhs we want to set only the actual independent variables
       ! defined by the user, i.e. the incrementation of f(:,1:nx).
       s % dfdt( 1 : nx, 1 : nf ) => dydt( 1 : nx * nf)

       ! physical2 mesh differentiates the dfdt, i.e. dydt
       s % physical2 % f( 1 : nx, 1 : nf ) => dydt( 1 : nx * nf )
    end if

  end subroutine set_pointers

  subroutine calculate_dfdx( s, i )
    class(solver_mmpde6) :: s
    integer :: i
    integer :: j

    do j = 1, i
       call s % physical % calculate_derivatives( i )
    end do

    end subroutine calculate_dfdx


  ! dydt's real name should be dydtau, as we calculate here the
  ! temporal derivative over computational time
  subroutine rhs_for_marcher( t, y, dydt, s, status )
    real, intent(in) :: t       !computational time!
    real, target, intent(in) :: y(:)    !input data vector
    real, target, intent(out) :: dydt(:) !output data vector
    real, pointer    :: m(:), x(:), dxdt(:), dxdt_tmp(:), greens(:,:)
    real, pointer    :: dfdt(:,:), dfdx(:,:,:)
    class(solver_mmpde6) :: s
    integer, optional :: status
    integer :: nx, nf, i, j
    real :: g, epsilon, h

    nx = s % nx
    nf = s % nf
    h  = s % h

    ! initial pointer setup
    call s % set_pointers( t = t, y = y, dydt = dydt )

    ! temporary pointers, introduced for convenience
    m => s % monitor
    x => y( nx * nf + 1 : nx * (nf + 1) )
    dxdt => dydt( nx * nf + 1 : nx * (nf + 1) )
    dxdt_tmp => s % temporary
    greens => s % greens
    dfdt => s % dfdt
    dfdx => s % dfdx

    ! after setting pointers we calculate the required spatial
    ! derivatives

    call s % calculate_dfdx( s % rk )


    !!!!!!!!!!! calculate d/dt of f and store it in the appropriate
    !!!!!!!!!!! part of dydt

    ! calling rhs after set_pointers sets a part of dydt (see the
    ! definition of set_pointers)
    call s % rhs
    ! after calling rhs dydt( 1 : nx * (total_nf - 2)) is set


    !!!!!!!!!!! now calculate the Sundman transform g()

    ! calculate g right after calling s % rhs, order matters
    g = s % g()

    ! we also set the value of d/datu of t to a trivial 1.
    dydt( nx*nf + 1 ) = 1.


    !!!!!!!!!!! proceed to calculating dx/dt

    ! first calculate the values of the monitor function
    call s % calculate_monitor

    ! than use a symmetric discretization of (m*x_xi)_xi from [Budd and
    ! Williams 2009]
    ! the forall loop should run over all d/dt of x(2:nx-1) values
    ! according to pointer association in set_pointers
    forall( i = 2 : nx - 1 ) &
         dxdt( i ) = - 1./epsilon(g)  &
         * ( ( m(i+1) + m(i) ) * ( x(i+1) - x(i) ) &
         -   ( m(i) + m(i+1) ) * ( x(i) - x(i-1))) &
         /(2.*h**2)

    ! multiply the dxdt by the greens function
    dxdt_tmp = 0.

    forall( i = 1 : nx )
       dxdt_tmp(i) = dxdt_tmp(i) + sum(dxdt(:)*greens(i,:))
    end forall

    ! the boundary conditions for the mesh are (theese are imposed by
    ! greens function multiplication above, but we emphasize them
    ! here)
    dxdt(  1 ) = 0.
    dxdt( nx ) = 0.

    ! @todo add -x_t*f_x to the rhs
    forall( i = 1 : nf )
       dfdt(:,i) = dfdt(:,i) - dxdt(:) * dfdx(:,i,1)
    end forall


    ! now the whole dydt vector should be set up to the almost
    ! appropriate values, all is left is to multiply it by the
    ! dilation g()
    dydt = g * dydt


  end subroutine rhs_for_marcher


  real function g(s)
    class(solver_mmpde6) :: s
    real :: u_tx_0, u_x_0

    ! use the previously calculated value of u_x(x=0)
    u_x_0 = s % dfdx(1,1,1)
    ! calculate u_xt(x=0)
    u_tx_0 = s % physical2 % derivative( 1, 1, 1 )

    ! the value of g is custom suited to the problem
    g = (abs(u_x_0) + 1.)/(abs(u_tx_0) + 1.)

  end function g

  ! this function was found to be giving best results, see Biernat and
  ! Bizon [2011]
  real function epsilon(g)
    real :: g
    epsilon = 100. * sqrt(g) + .05

  end function epsilon


  subroutine calculate_monitor(s)
    class(solver_mmpde6) :: s
    real, pointer ::  dxdxi(:,:,:)

    dxdxi => s % physical % df

    ! M(u) = |u_r| + sqrt(|u_rr|)
    s % monitor(:) = abs(dxdxi(:,1,1)) + sqrt(abs(dxdxi(:,1,2)))

  end subroutine calculate_monitor


  subroutine solve( s )
    class(solver_mmpde6) :: s

    call s % start

    do while( s%t < s%t1 )
       ! do while( i < 3 )
       call s % marcher % apply( &
            s   = s % stepper,   &
            sys = s % system,    &
            t   = s % t,         &
            t1  = s % t1,        &
            h   = s % h,         &
            y   = s % y )
       ! @todo: neater error handling

       if ( s % marcher % status /= 1 ) then
          print *, "marcher error, status=",  s % marcher % status
          ! @todo change exit to an error report
          exit
       else

          ! increment the iteration number
          s % n_iter = s % n_iter + 1

          ! sync pointers first
          call s % set_pointers

          ! @todo: extra calculation, probably not needed
          call s % rhs

          call s % step
       endif
    end do

    call s % stop

  end subroutine solve


  ! @todo better free for solver_mmpde6
  subroutine free( s )
    class(solver_mmpde6) :: s

    ! the pointer in the argument below is moved around pretty much
    ! during execution of solve() and should be nullified in order not
    ! to point at some yet to be freed memory area
    call s % physical % free
    call s % stepper % free
    call s % marcher % free

    ! @bug not all of the arrays are freed
    deallocate( s % system, s % t, s % dfdt, s % y )
    ! deallocate( s % system, s % t, s % dfdt )

  end subroutine free





end module class_solver_mmpde6
