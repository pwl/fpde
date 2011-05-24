module class_solver

  ! use class_marcher
  ! use class_mesh

  use pretty_print

  private

  type, public :: solver
     procedure(rhs), pointer :: rhs
   contains
     procedure    :: solve
  end type solver

  abstract interface

     subroutine rhs( s )
       import :: solver
       class(solver) :: s
     end subroutine rhs

  end interface

contains

  subroutine solve( s )
    class(solver) :: s
  end subroutine solve


end module class_solver
