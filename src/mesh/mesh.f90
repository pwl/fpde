module mesh_module
  
  private
  
  ! general mesh class to be inherited by user-defined meshes
  type, public :: mesh
     private
     integer           :: dim, nx, nf, maxrk
     real, allocatable :: x(:)
     real, allocatable :: f(:,:)
     real, allocatable :: df(:,:,:)
   contains
     procedure :: Print
     procedure :: Init
     procedure :: Free
     procedure :: CacheDerivatives
  end type mesh

contains
  subroutine Init(m, nx, nf, maxrk)
    class(mesh), intent(inout) :: m
    integer :: nx,nf,maxrk

    m%dim = 1
    m%nx = nx
    m%nf = nf
    m%maxrk = maxrk

    allocate( m % x( nx ) )
    allocate( m % f( nf, nx ) )
    allocate( m % df( maxrk, nf, nx ) )
  end subroutine Init

  subroutine Free( m )
    class(mesh), intent(inout) :: m
    deallocate( m % x )
    deallocate( m % f )
    deallocate( m % df )
  end subroutine Free

  subroutine CacheDerivatives( m )
    class(mesh), intent(in) :: m
    print *, "module_mesh: CacheDerivatives not overloaded!"
      
  end subroutine CacheDerivatives
  
  
  subroutine Print( m )
    class(mesh), intent(in) :: m
    print *, "dim,nx,nf,maxrk = ", m%dim, m%nx, m%nf, m%maxrk
  end subroutine Print
  
end module mesh_module
