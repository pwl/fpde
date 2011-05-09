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
     procedure :: CacheDerivatives
     procedure :: Init
     procedure :: Free
  end type mesh_example

contains

  ! overloaded Init
  subroutine Init( m, nx, nf, maxrk, xmin, xmax )
    class(mesh_example), intent(inout) :: m
    integer :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax

    ! first init the parent class, this is necessary
    call m % mesh % Init( nx, nf, maxrk, xmin, xmax )

    ! then init user data
    allocate( m % test( nx ) )

  end subroutine Init

  ! overloaded Free
  subroutine Free( m )
    class(mesh_example), intent(inout) :: m

    ! first free user defined data
    deallocate( m % test )

    ! then free parent class
    call m % mesh % Free
      
  end subroutine Free

  ! overloaded CacheDerivatives
  subroutine CacheDerivatives( m, i )
    class(mesh_example), intent(inout) :: m
    integer :: i

    ! overloading is simple!
    print *, "Overloaded CacheDerivatives!"
      
  end subroutine CacheDerivatives
  
end module module_mesh_example
