module class_solver_simple

  use class_solver
  use class_marcher
  use class_mesh

  use pretty_print

  private

  type, public, extends(solver) :: solver_simple
     real :: max_t
     class(mesh), pointer    :: mesh
     class(mesh), pointer    :: mesh_rhs
     class(marcher), pointer :: marcher
     ! procedure(rhs_interface), pointer :: rhs
   contains
     procedure    :: init
     procedure    :: free
  end type solver_simple

contains

  subroutine init(s, msh, msh_rhs, march)
    class(solver_simple)  , intent(inout)      :: s
    class(mesh)    , intent(in), target :: msh, msh_rhs
    class(marcher) , intent(in), target :: march
    ! class(mesh), pointer :: msh_rhs

    s % mesh     => msh
    s % marcher  => march
    s % mesh_rhs => msh_rhs

  end subroutine init

  subroutine solve(s)
    class(solver_simple), intent(inout) :: s

    do while ( s % marcher % t < s % max_t )
       ! s % marcher % march
    end do


  end subroutine solve


  subroutine free(s)
    class(solver_simple), intent(inout) :: s
  end subroutine free

end module class_solver_simple
