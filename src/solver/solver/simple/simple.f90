module class_solver_simple

  use class_solver
  use class_marcher
  use class_mesh

  use pretty_print

  private

  type, public, extends(solver) :: solver_simple
     real :: max_t
     class(mesh), pointer    :: mesh
     class(marcher), pointer :: marcher
     real, allocatable    :: rhs_table(:,:)
     ! procedure(rhs_interface), pointer :: rhs
   contains
     procedure    :: init
     procedure    :: free
  end type solver_simple

contains

  subroutine init(s, msh, march, max_t)
    class(solver_simple)  , intent(inout)      :: s
    class(mesh)    , target :: msh
    class(marcher) , intent(in), target :: march
    real, intent(in) :: max_t

    allocate( s % rhs_table(msh % nx, msh % nf ) )

    s % mesh     => msh
    s % marcher  => march

  end subroutine init

  subroutine solve(s)
    class(solver_simple), intent(inout) :: s

    do while ( s % marcher % t < s % max_t )
       ! s % marcher % apply
    end do

  end subroutine solve

  subroutine free(s)
    class(solver_simple), intent(inout) :: s

    deallocate( s % rhs_table )
    call s % mesh % free
    call s % marcher % free
  end subroutine free

end module class_solver_simple
