module class_mesh_example
  use class_mesh

  ! everything except the type should be private
  private

  type, public, extends( mesh ) :: mesh_example
     private
     ! private data goes here (if needed)
     real, allocatable :: test(:)
   contains
     ! overloaded procedures go here (if needed)
     procedure :: calculate_derivatives
     procedure :: init
     procedure :: free
  end type mesh_example

contains

  ! overloaded init
  subroutine init( m )
    class(mesh_example), intent(inout) :: m

    ! first init the parent class, this is necessary
    call m % mesh % init

    ! then init user data
    allocate( m % test( m % nx ) )

  end subroutine Init

  ! overloaded free
  subroutine free( m )
    class(mesh_example), intent(inout) :: m

    ! first free user defined data
    deallocate( m % test )

    ! then free parent class
    call m % mesh % free

  end subroutine free

  ! overloaded CalculateDerivatives
  subroutine calculate_derivatives( m, i )
    class(mesh_example), target,  intent(inout) :: m
    integer, intent(in) :: i

    ! overloading is simple!
    print *, "Overloaded calculate_derivatives!"

  end subroutine calculate_derivatives

end module class_mesh_example
