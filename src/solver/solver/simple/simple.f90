! simple solver class with one mesh and one marcher

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
   contains
     procedure    :: init
     procedure    :: free
     procedure    :: solve
     procedure :: calculate_dfdx
     procedure :: pointwise_dfdx
  end type solver_simple

  ! @todo how to use interface from class_solver?
  abstract interface
     subroutine interface_rhs( s, params )
       import :: solver
       class(solver) :: s
       class(*) :: params
     end subroutine interface_rhs
  end interface

contains

  subroutine init(s, msh, march, max_t, rhs, params)
    class(solver_simple)  , intent(inout)      :: s
    class(mesh)    , target :: msh
    class(marcher) , intent(in), target :: march
    real, intent(in) :: max_t
    procedure(interface_rhs) :: rhs
    class(*), target :: params

    s % mesh     => msh
    s % marcher  => march
    s % params   => params
    s % name = "Simple solver"

    ! bind pointers to appropriate targets
    s % t     => march % t
    s % x     => msh % x
    s % f     => msh % f
    s % dfdx  => msh % df
    s % nx    = msh % nx
    s % nf    = msh % nf
    s % maxrk = msh % maxrk

    ! allocate space for a pointer to calculated rhs
    allocate( s % dfdt(msh % nx, msh % nf ) )

    ! set the rhs
    s % rhs => rhs

  end subroutine init

  subroutine solve(s)
    class(solver_simple), intent(inout) :: s

    call s % rhs( s % params )

    ! do while ( s % marcher % t < s % max_t )
    !    ! s % marcher % apply
    ! end do

  end subroutine solve


  subroutine free(s)
    class(solver_simple), intent(inout) :: s

    ! free components of the solver
    call s % mesh % free
    call s % marcher % free
    deallocate( s % dfdt )

  end subroutine free

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

end module class_solver_simple
