! simple solver class with one mesh and one marcher

! The trick here is to synchronise three arrays, s % y, s % mesh % f
! and s % f. The idea is to make s % y the pointer to an allocated
! memory and make s % f the 2d view of it, synced after calculating
! rhs. To calculate rhs s % f and s % mesh % f are both temporary
! associated to y in solver_simple_rhs_for_marcher. Synchronization is
! done by a private function sync_f

module class_solver_simple

  ! use class_solver_simple_data
  use class_solver
  use class_ode_marcher
  use class_ode_stepper
  use class_ode_system
  use class_ode_step_control
  use class_mesh

  use pretty_print

  private

  type, public, extends(solver) :: solver_simple
     real :: h
     ! y(:) holds data used in mesh in a format compatible with
     ! rhs_for_marcher
     real, contiguous, pointer :: y(:), dydt(:)
     class(mesh), pointer :: mesh
     class(ode_stepper), pointer :: stepper
     class(*), pointer :: data
     class(ode_system), pointer :: system
     class(ode_marcher), pointer :: marcher
     class(ode_step_control), pointer :: step_control
   contains
     ! procedure :: init
     procedure :: info
     procedure :: free
     procedure :: solve
     procedure :: calculate_dfdx
     procedure :: pointwise_dfdx
     procedure :: sync_f
     procedure :: sync_dfdt
  end type solver_simple

  private sync_f, sync_dfdt
  public solver_simple_rhs_for_marcher

contains

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

  ! used to set the view of s and its subparts (i.e. mesh) to a vector
  ! v.
  subroutine sync_f( s, v )
    class(solver_simple) :: s
    real, pointer, contiguous :: v(:)
    integer :: nx, nf
    nx = s % nx
    nf = s % nf

    s % f(1:nx, 1:nf) => v
    s % mesh % f(1:nx, 1:nf) => v

  end subroutine sync_f

  ! same as above, but syncs dfdt
  subroutine sync_dfdt( s, v )
    class(solver_simple) :: s
    real, pointer, contiguous :: v(:)
    integer :: nx, nf
    nx = s % nx
    nf = s % nf

    s % dfdt(1:nx, 1:nf) => v

  end subroutine sync_dfdt

  subroutine info( s )
    class(solver_simple) :: s


    print *, "solver: ", s % name

    if( associated(s % mesh) ) then
       print *, "mesh: " , trim(s % mesh % name)
    else
       print *, "mesh: NONE"
    end if

    if( associated(s % stepper) ) then
       print *, "stepper: ", trim(s % stepper % name )
    else
       print *, "stepper: NONE"
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

    ! if( associated(s % data) ) then
    !    print *, "data:"
    !    call s % data % info
    ! else
    !    print *, "data: nay!"
    ! end if

  end subroutine info


  subroutine free( s )
    class(solver_simple) :: s

    ! the pointer in the argument below is moved around pretty much
    ! during execution of solve() and should be nullified in order not
    ! to point at some yet to be freed memory area
    nullify( s % mesh % f )
    call s % mesh % free
    call s % stepper % free
    call s % marcher % free

    deallocate( s % system, s % t, s % dfdt, s % y )
    ! deallocate( s % system, s % t, s % dfdt )

  end subroutine free

  subroutine solve( s )
    class(solver_simple) :: s
    ! integer :: i = 0

    call s % sync_f(s % y)
    call s % sync_dfdt(s % dydt)
    call s % rhs
    call s % start

    do while( s%t < s%t1)
       ! do while( i < 3 )
       call s % marcher % apply( &
            s   = s % stepper,   &
            c   = s % step_control, &
            sys = s % system,    &
            t   = s % t,         &
            t1  = s % t1,        &
            h   = s % dt,         &
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
          call s % sync_f( s % y )
          call s % sync_dfdt( s % dydt )

          ! @todo: extra calculation, probably not needed
          call s % rhs

          call s % step

          ! check the status after running triggers
          if( trim(s % status) == "stopped" .or. &
               trim(s % status) == "error" ) then
             return
          end if

       endif
    end do

    call s % stop

  end subroutine solve

  ! this is a default wrapper for solver%rhs to work with marcher
  ! architecture. It should do the right thing for a simple solver,
  ! but should be rewritten in a more sophisticated solver
  ! implementation
  subroutine solver_simple_rhs_for_marcher( t, y, dydt, s, status )
    real, intent(in) :: t
    real, pointer, contiguous, intent(in) :: y(:)
    real, pointer, contiguous, intent(out) :: dydt(:)
    class(solver_simple) :: s
    integer, optional :: status
    integer :: nx, nf, i, j

    nx = s % nx
    nf = s % nf

    call s % sync_f( y )
    call s % sync_dfdt( dydt )
    s % t = t

    ! calculate rhs (with s % f obtained from y)
    call s % rhs

    if( present(status) ) then
       status = s % rhs_status
    end if

  end subroutine solver_simple_rhs_for_marcher



end module class_solver_simple

