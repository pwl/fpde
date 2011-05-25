module class_solver

  ! use class_marcher
  ! use class_mesh

  use pretty_print

  private

  type, public :: solver
     procedure(rhs), pointer :: rhs
   contains
     procedure :: solve
     procedure, non_overridable :: set_rhs
     ! procedure :: set_initial_values
     ! procedure :: set_boundary_conditions
     ! procedure :: set_maximal_time
  end type solver

  abstract interface

     ! draft of rhs interface, to be changed in future?
     subroutine rhs( s )
       import :: solver
       class(solver) :: s
     end subroutine rhs

  end interface

contains

  subroutine set_rhs( s, rhsin )
    class(solver) :: s
    procedure(rhs), pointer :: rhsin

    s % rhs => rhsin
  end subroutine set_rhs

  subroutine solve( s )
    class(solver) :: s

    stop "solve not overriden"

  end subroutine solve

end module class_solver
