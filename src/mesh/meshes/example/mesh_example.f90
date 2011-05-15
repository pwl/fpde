module module_mesh_example
  use mesh_module

  ! everything except the type should be private
  private

  type, public, extends( mesh ) :: mesh_example
     private
     ! private data goes here (if needed)
     real, allocatable :: test(:)
   contains
     ! overloaded procedures go here (if needed)
     procedure :: cache_derivatives
     procedure :: init
     procedure :: free
  end type mesh_example

contains

  ! overloaded init
  subroutine init( m, nx, nf, maxrk, xmin, xmax )
    class(mesh_example), intent(inout) :: m
    integer :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax

    ! first init the parent class, this is necessary
    call m % mesh % init( nx, nf, maxrk, xmin, xmax )

    ! then init user data
    allocate( m % test( nx ) )

  end subroutine Init

  ! overloaded free
  subroutine free( m )
    class(mesh_example), intent(inout) :: m

    ! first free user defined data
    deallocate( m % test )

    ! then free parent class
    call m % mesh % free

  end subroutine free

  ! overloaded CacheDerivatives
  subroutine cache_derivatives( m, i )
    class(mesh_example), intent(inout) :: m
    integer :: i

    ! overloading is simple!
    print *, "Overloaded cache_derivatives!"

  end subroutine cache_derivatives

end module module_mesh_example
