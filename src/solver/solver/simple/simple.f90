module class_solver_simple

  use class_solver
  use class_marcher
  use class_mesh
  
  use pretty_print

  ! private

  type, public, extends(solver) :: solver_simple
     class(mesh), pointer    :: mesh
     class(mesh), pointer    :: mesh_rhs
     class(marcher), pointer :: marcher
     ! procedure(rhs_interface), pointer :: rhs
   contains
     procedure    :: init
     procedure    :: free
  end type solver_simple

  abstract interface
     
     subroutine rhs_interface( s )
       import :: solver
       class(solver) :: s
     end subroutine rhs_interface
     
  end interface

contains

  subroutine init(s, msh, march)
    class(solver_simple)  , intent(inout)      :: s
    class(mesh)    , intent(in), target :: msh
    class(marcher) , intent(in), target :: march
    class(mesh), pointer :: msh_rhs
    
    s % mesh    => msh
    s % marcher => march
    ! msh_rhs = msh

    ! call msh_rhs % init(10,2,2,0.,1.)
    
    ! allocate(s % mesh_rhs)
    ! s % mesh_rhs = s % mesh
    
  end subroutine init

  subroutine free(s)
    class(solver_simple), intent(inout) :: s
  end subroutine free

  ! subroutine attack(s)
    
  ! end subroutine attack

  
end module class_solver_simple
