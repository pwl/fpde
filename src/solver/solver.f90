!>
!! @addtogroup solver
!! @{
!! @file   solver.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sun Sep 25 20:19:53 2011
!!
!! @brief @todo this solver concept needs cleaning up
!!
!!
!!
module class_solver

  use class_module
  use class_module_bundle
  use class_solver_data
  use class_trigger

  private


  type, public, extends(solver_data) :: solver
     class(module_bundle), pointer :: modules => null()
     procedure(interface_rhs), pointer :: rhs => null()
   contains
     procedure         :: info
     procedure         :: free
     procedure         :: solve
     procedure         :: init
     procedure         :: add => add_module
     procedure         :: step
     procedure         :: start
     procedure         :: stop
     procedure         :: calculate_dfdx
  end type solver

  abstract interface
     subroutine interface_rhs( s )
       import :: solver
       class(solver) :: s
     end subroutine interface_rhs
  end interface

 public interface_rhs

  ! interface to rhs should be publicly available to all child classes

contains

  subroutine init(s)
    class(solver), target :: s

    call s % solver_data % init

    allocate( s % modules )
    call s % modules % init


  end subroutine init

  subroutine add_module(s, m, t1, t2, t3)
    class(solver), target :: s
    class(module) :: m
    class(trigger), optional :: t1, t2, t3

    ! this has to be done prior to initialization as module%init() may
    ! need a solver_data structure
    m % solver_data => s % solver_data

    if( m % try_init() ) then

       ! module initialized succesfully, adding it to solver.
       call s % modules % add(m)

       if(present(t1)) then
          call m % add(t1)
       end if

       if(present(t2)) then
          call m % add(t2)
       end if

       if(present(t3)) then
          call m % add(t3)
       end if

    else
       ! @todo report error, module failed to initialize
       return
    end if

  end subroutine add_module

  subroutine step(s)
    class(solver) :: s
    if( associated( s % modules ) ) then
       call s % modules % step
    end if
  end subroutine step

  subroutine solve( s )
    class(solver) :: s

    print *, "solve not defined for ", trim(s % name)

  end subroutine solve

  subroutine info( s )
    class(solver) :: s
  end subroutine info

  subroutine free (s)
    class(solver) :: s
    call s % solver_data % free
  end subroutine free

  subroutine start(s)
    class(solver) :: s

    call s % solver_data % start

    if( associated( s % modules ) ) then
       call s % modules % start
       s % status = solver_started
    end if
  end subroutine start

  subroutine stop(s)
    class(solver) :: s
    if( associated( s % modules ) ) then
       call s % modules % stop
    end if
  end subroutine stop

  subroutine calculate_dfdx(s, i)
    class(solver) :: s
    integer :: i

    print *, "ERROR: solver: calculate_dfdx is not overloaded"

  end subroutine calculate_dfdx


end module class_solver
!> @}
