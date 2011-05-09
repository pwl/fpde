module mesh_module

  private

  ! general mesh class to be inherited by user-defined meshes
  type, public :: mesh
     ! private
     integer              :: nx, nf, maxrk
     real, allocatable    :: x(:)
     real, allocatable    :: f(:,:)
     real, allocatable    :: df(:,:,:)
     real                 :: xmin, xmax
     logical, allocatable :: cached(:)     
   contains
     procedure :: PrintPreview
     procedure :: Init
     procedure :: Free
     procedure :: CacheDerivatives
     procedure :: ToVector
     procedure :: FromVector
     procedure :: FillForDebug
     procedure :: Derivative
     generic :: test1 => FillForDebug, FromVector
  end type mesh

contains
  subroutine Init(m, nx, nf, maxrk, xmin, xmax)
    class(mesh), intent(inout) :: m
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i

    m % nx = nx
    m % nf = nf
    m % maxrk = maxrk
    m % xmin = xmin
    m % xmax = xmax

    allocate( m % cached( maxrk ) )
    m % cached = .false.

    ! setup a uniform grid
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

  function Derivative( m, i, j, k )
    class(mesh), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: Derivative

    Derivative = 0
    
  end function Derivative
  
  subroutine CacheDerivatives( m, i )
    class(mesh), intent(inout) :: m
    integer :: i,j,k

    if( i > m  % maxrk .or. i < 1 ) then
       return
    else if( m % cached( i ) ) then
       return 
    end if
        
    forall(&
         j = 1 : m % nf,&
         k = 1 : m % nx )
       m % df( i, j, k) = m % Derivative( i, j, k )
    end forall

    m % cached( i ) = .true.
    
  end subroutine CacheDerivatives

  ! this is not the best way to convert to a vector velue because it
  ! copies data
  subroutine ToVector( m, v )
    class(mesh), intent(in) :: m
    real, allocatable, intent(inout) :: v(:)

    if( .not. allocated( v ) ) then
       allocate( v ( m % nx * m % nf ) )
    end if
    
    v = reshape( m % f, shape( v ) )

  end subroutine ToVector

  subroutine FromVector( m, v )
    class(mesh), intent(inout) :: m
    real, intent(in) :: v(:)

    m % f = reshape( v, shape( m % f ) )
    m % cached = .false.
    
  end subroutine FromVector
  
  subroutine PrintPreview( m )
    class(mesh), intent(in) :: m
    integer :: offset = 5, i

    ! print parameters
    print *, "nx,nf,maxrk = ",&
         m % nx, m % nf, m % maxrk

    ! print mesh points
    print *, "x = ", m % x(1:offset), " (...) ", m % x(m%nx - offset : m%nx)
    do i = 1, m%nf
       print *, "f_", i, " = ", m % f(i,1:offset),&
            " (...) ", m % f(i,m%nx - offset : m%nx)
    end do
    
  end subroutine PrintPreview

  subroutine FillForDebug( m )
    class(mesh), intent(inout) :: m
    integer :: nx = 10, nf = 3, maxrk = 2
    integer :: i,j

    call m%init(nx, nf, maxrk, 0., 1.)
    forall(i = 1:m%nx, j = 1:m%nf) m%f(i,j) = i*100+j

  end subroutine FillForDebug
  

end module mesh_module
