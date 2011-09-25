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

  use class_solver_data

  private


  type, public, extends(solver_data) :: solver
     ! class(module_bundle), pointer :: modules
   contains
     procedure         :: info => solver_info
     procedure         :: free => solver_free
     procedure         :: solve
  end type solver

  ! interface to rhs should be publicly available to all child classes

contains

  ! subroutine init(s)
  !   class(solver) :: s
  !   allocate( s % modules )
  !   call s % modules % init
  ! end subroutine init

  subroutine solve( s )
    class(solver) :: s

    print *, "solve not defined for ", trim(s % name)

  end subroutine solve

  subroutine solver_info( s )
    class(solver) :: s
  end subroutine solver_info

  subroutine solver_free (s)
    class(solver) :: s
  end subroutine solver_free

end module class_solver
!> @}
