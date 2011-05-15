module mesh_module

  private

  ! general mesh class to be inherited by user-defined meshes
  type, public :: mesh
     ! private
     ! TODO: add something like
     ! string :: name
     integer              :: nx, nf, maxrk
     real, allocatable    :: x(:)
     real, allocatable    :: f(:,:)
     real, allocatable    :: df(:,:,:)
     logical, allocatable :: df_calculated(:)
   contains
     procedure :: print_preview
     procedure :: init
     procedure :: free
     procedure :: derivative
     procedure :: calculate_derivatives
     procedure :: to_vector
     procedure :: from_vector
     procedure :: fill_for_debug
  end type mesh

contains

  subroutine init(m, nx, nf, maxrk, xmin, xmax)
    class(mesh), intent(inout) :: m
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i

    m % nx = nx
    m % nf = nf
    m % maxrk = maxrk

    allocate( m % df_calculated( maxrk ) )
    m % df_calculated = .false.

    ! setup a uniform grid
    allocate( m % x( nx ) )
    allocate( m % f( nx, nf ) )
    allocate( m % df( nx, nf, maxrk ) )
  end subroutine init


  subroutine free( m )
    class(mesh), intent(inout) :: m

    deallocate( m % x )
    deallocate( m % f )
    deallocate( m % df )
    deallocate( m % df_calculated )
  end subroutine free


  ! calculates k-th derivative of j-th function at i-th point
  function derivative( m, i, j, k )
    class(mesh), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: derivative
    derivative = 0

    stop 'method "derivative" is not overloaded'

  end function derivative


  ! calculates i-th derivative of all functions at all points
  subroutine calculate_derivatives( m, i )
    class(mesh), target, intent(inout) :: m
    integer :: i

    stop 'method "calculate_derivatives" not overloaded'

  end subroutine calculate_derivatives


  ! this is not the best way to convert to a vector velue because it
  ! copies data
  subroutine to_vector( m, v )
    class(mesh), intent(in) :: m
    real, allocatable, intent(inout) :: v(:)

    if( .not. allocated( v ) ) then
       allocate( v ( m % nx * m % nf ) )
    end if

    v = reshape( m % f, shape( v ) )

  end subroutine to_vector

  ! analogue of to_vector
  subroutine from_vector( m, v )
    class(mesh), intent(inout) :: m
    real, intent(in) :: v(:)

    m % f = reshape( v, shape( m % f ) )
    m % df_calculated = .false.

  end subroutine from_vector

  ! used to print a readable output of a mesh, to be improved
  subroutine print_preview( m )
    class(mesh), intent(in) :: m
    integer :: offset = 5, i

    ! print parameters
    print *, ""
    print *, "nx,nf,maxrk = ",&
         m % nx, m % nf, m % maxrk

    ! print mesh points
    print *, "x = ", m % x(1:offset), " (...) ", m % x(m%nx - offset : m%nx)
    do i = 1, m%nf
       print *, "f_", i, " = ", m % f(1:offset,i),&
            " (...) ", m % f(m%nx - offset : m%nx,i)
    end do
    print *, ""

  end subroutine print_preview

  ! subroutine used to init a mesh for debugging purposes, to be
  ! removed in future
  subroutine fill_for_debug( m )
    class(mesh), intent(inout) :: m
    integer :: nx = 10, nf = 3, maxrk = 2
    integer :: i,j

    call m%init(nx, nf, maxrk, 0., 1.)
    forall(i = 1:m%nx, j = 1:m%nf) m%f(j,i) = i*100+j

  end subroutine fill_for_debug


end module mesh_module
