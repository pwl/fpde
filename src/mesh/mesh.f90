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
     procedure :: ToVector
     procedure :: FillForDebug
  end type mesh

contains
  subroutine Init(m, nx, nf, maxrk)
    class(mesh), intent(inout) :: m
    integer :: nx,nf,maxrk

    m % dim = 1
    m % nx = nx
    m % nf = nf
    m % maxrk = maxrk

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

  ! this is not the best way to convert to a vector velue because it
  ! copies data
  subroutine ToVector( m, v )
    class(mesh), intent(in) :: m
    real, intent(inout) :: v(:)

    v = reshape( m%f, shape(v) )

  end subroutine ToVector

  subroutine Print( m )
    class(mesh), intent(in) :: m
    integer :: offset = 5

    ! print
    print *, "dim,nx,nf,maxrk = ",&
         m % dim, m % nx, m % nf, m % maxrk

    ! print mesh points
    print *, "x = ", m % x(1:offset), " (...) ", m % x(m%nx - offset : m%nx : 1)

  end subroutine Print

  subroutine FillForDebug( m )
    class(mesh), intent(inout) :: m
    integer :: nx = 4, nf = 3, maxrk = 2
    integer :: i,j

    call m%init(nx, nf, maxrk)
    forall(i = 1:m%nx) m%x(i) = i
    forall(i = 1:m%nx, j = 1:m%nf) m%f(i,j) = i*100+j

  end subroutine FillForDebug

end module mesh_module
