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

  private


  type, public, extends(solver_data) :: solver
     class(module_bundle), pointer :: modules
   contains
     procedure         :: info
     procedure         :: free
     procedure         :: solve
     procedure         :: init
     procedure         :: add => add_module
     procedure         :: run_modules
  end type solver

  ! interface to rhs should be publicly available to all child classes

contains

  subroutine init(s)
    class(solver) :: s
    allocate( s % modules )
    call s % modules % init
  end subroutine init

  subroutine add_module(s, m, t1, t2, t3)
    use class_trigger
    class(solver), target :: s
    class(module) :: m
    class(trigger), pointer, optional :: t1, t2, t3

    call s % modules % add(m)
    m % solver_data => s % solver_data

    if(present(t1)) then

    end if

  end subroutine add_module

  subroutine run_modules(s)
    class(solver) :: s
    call s % modules % step
  end subroutine run_modules

  subroutine solve( s )
    class(solver) :: s

    print *, "solve not defined for ", trim(s % name)

  end subroutine solve

  subroutine info( s )
    class(solver) :: s
  end subroutine info

  subroutine free (s)
    class(solver) :: s
  end subroutine free

end module class_solver
!> @}
