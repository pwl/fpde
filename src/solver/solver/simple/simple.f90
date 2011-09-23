! simple solver class with one mesh and one marcher

! The trick here is to synchronise three arrays, s % y, s % mesh % f
! and s % f. The idea is to make s % y the pointer to an allocated
! memory and make s % f the 2d view of it, synced after calculating
! rhs. To calculate rhs s % f and s % mesh % f are both temporary
! associated to y in solver_simple_rhs_for_marcher. Synchronization is
! done by a private function sync_to

module class_solver_simple

  use class_solver_simple_data
  use class_solver
  use class_ode_marcher
  use class_ode_stepper
  use class_ode_system
  use class_mesh

  use pretty_print

  private

  type, public, extends(solver) :: solver_simple
     real :: t1
     real :: h
     ! y(:) holds data used in mesh in a format compatible with
     ! rhs_for_marcher
     real, pointer :: y(:)
     class(mesh), pointer :: mesh
     class(ode_stepper), pointer :: step
     class(solver_simple_data), pointer :: data
     class(ode_system), pointer :: system
     class(ode_marcher), pointer :: marcher
     ! class(control), pointer :: control
   contains
     procedure :: init
     procedure :: info
     procedure :: free
     procedure :: solve
     procedure :: calculate_dfdx
     procedure :: pointwise_dfdx
     procedure :: sync_to
  end type solver_simple

  private sync_to

contains

  subroutine init(s, data)
    class(solver_simple) :: s
    class(solver_simple_data), target :: data
    integer :: i,j,nx,nf

    ! local variables
    nx = data % nx
    nf = data % nf

    s % name = "simple"

    ! initialize mesh
    call data % initialize_mesh( s % mesh )
    ! initialize step
    call data % initialize_step( s % step )
    ! initialize time
    call data % initialize_t( s % t )
    ! initialize ode_system
    call data % initialize_ode_system( s % system, s)
    ! initialize marcher
    call data % initialize_marcher( s % marcher )
    ! allocate memory for dfdt
    call data % initialize_dfdt( s % dfdt )
    ! initialize right hand side of equations
    call data % initialize_rhs( s % rhs, s % rhs_status )

    ! assign interface pointers
    s % x      => s % mesh % x
    s % f      => s % mesh % f
    s % dfdx   => s % mesh % df
    s % params => data % params
    s % data   => data

    s % nx     = nx
    s % nf     = nf
    s % rk     = data % rk
    s % t1     = data % t1
    s % h      = data % h0

    ! copy initial data from a mesh
    allocate( s % y( nf * nx ) )
    call s % sync_to( s % y )
    ! s % f( 1:nx, 1:nf ) => s % y

    ! @todo when pointer bounds(rank) remapping (test/array_test.f90)
    ! is implemented the following should work instead, then no
    ! copying will be required. (no, no, no, wrong, do it again)
    !
    ! s % y( 1 : s % nf * s % nx ) => s % mesh % f

  end subroutine init


  subroutine calculate_dfdx( s, i )
    class(solver_simple) :: s
    integer :: i

    call s % mesh % calculate_derivatives( i )

  end subroutine calculate_dfdx


  real function pointwise_dfdx( s, i, j, k )
    class(solver_simple) :: s
    integer :: i, j, k

    pointwise_dfdx = s % mesh % derivative( i,j,k )

  end function pointwise_dfdx

  subroutine sync_to( s, v )
    class(solver_simple) :: s
    real, pointer :: v(:)
    integer :: nx, nf
    nx = s % nx
    nf = s % nf

    s % f(1:nx, 1:nf) => v
    s % mesh % f(1:nx, 1:nf) => v

  end subroutine sync_to

  subroutine info( s )
    class(solver_simple) :: s


    print *, "solver: ", s % name

    if( associated(s % mesh) ) then
       print *, "mesh: " , trim(s % mesh % name)
    else
       print *, "mesh: NONE"
    end if

    if( associated(s % step) ) then
       print *, "step: ", trim(s % step % name )
    else
       print *, "step: NONE"
    end if

    if( associated(s % system) ) then
       print *, "system: aye!"
    else
       print *, "system: nay!"
    end if

    if( associated(s % rhs) ) then
       print *, "rhs: aye!"
    else
       print *, "rhs: nay!"
    end if

    if( associated(s % marcher) ) then
       print *, "marcher: aye!"
    else
       print *, "marcher: nay!"
    end if

    if( associated(s % data) ) then
       print *, "data:"
       call s % data % info
    else
       print *, "data: nay!"
    end if

  end subroutine info


  subroutine free( s )
    class(solver_simple) :: s

    nullify( s % mesh % f )
    call s % mesh % free
    call s % step % free
    call s % marcher % free

    deallocate( s % system, s % t, s % dfdt, s % y )
    ! deallocate( s % system, s % t, s % dfdt )

  end subroutine free

  subroutine solve( s )
    class(solver_simple) :: s
    ! integer :: i = 0
    do while( s%t < s%t1)
    ! do while( i < 3 )
       call s % marcher % apply(           &
            s   = s % step,                &
            sys = s % system,          &
            t   = s % t,                   &
            t1  = s % t1,               &
            h   = s % h, &
            y   = s % y )
       ! @todo: neater error handling
       if ( s % marcher % status /= 1 ) then
          print *, "marcher error, status=",  s % marcher % status
          exit
       endif

       call s % sync_to( s % y )
       ! @todo: extra calculation, probably not needed
       call s % rhs

    end do
  end subroutine solve

  ! this is a default wrapper for solver%rhs to work with marcher
  ! architecture. It should do the right thing for a simple solver,
  ! but should be rewritten in a more sophisticated solver
  ! implementation
  subroutine solver_simple_rhs_for_marcher( t, y, dydt, s, status )
    real, intent(in) :: t
    real, pointer, intent(in) :: y(:)
    real, pointer, intent(out) :: dydt(:)
    class(solver_simple) :: s
    integer, optional :: status
    integer :: nx, nf, i, j

    nx = s % nx
    nf = s % nf

    call s % sync_to( y )
    s % t = t
    ! s % y is a one dimensional view of s % f, so this assignment
    ! changes s % f
    ! s % y = y

    ! this is a neat way to write data to dydt, but
    s % dfdt(1:nx,1:nf) => dydt
    ! calculate rhs (with s % f obtained from y)
    call s % rhs

    if( present(status) ) then
       status = s % rhs_status
    end if

  end subroutine solver_simple_rhs_for_marcher



end module class_solver_simple

