module class_mesh

  use pretty_print
  ! use omp_lib

  private

  ! general mesh class to be inherited by user-defined meshes
  type, public :: mesh
     ! private
     ! TODO: add something like
     character(len=300)    :: name
     integer               :: nx, nf, maxrk
     real, contiguous, pointer         :: x(:)
     real, contiguous, pointer         :: f(:,:)
     real, contiguous, pointer         :: df(:,:,:)
     real, contiguous, pointer         :: dx(:)
     logical, private, contiguous, pointer :: df_calculated(:)
   contains
     ! obligatory
     procedure :: derivative
     procedure :: calculate_derivatives

     ! optional
     procedure :: init
     procedure :: free
     procedure :: info
     procedure :: integrate

     ! use only, not to be overloaded
     procedure, non_overridable :: print_by_index
     ! generic   :: print => print_by_index
     procedure, non_overridable :: print_preview
     procedure, non_overridable :: check_derivatives
     procedure, non_overridable :: clear_derivatives

     ! questionable
     procedure :: to_vector
     procedure :: from_vector
     procedure :: fill_for_debug
     procedure :: calculate_spacings
  end type mesh

contains

  subroutine init(m, nx, nf, maxrk, xmin, xmax)
    class(mesh), intent(inout) :: m
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i


    m % nx = nx
    m % nf = nf
    m % maxrk = Maxrk
    allocate( m % df_calculated( maxrk ) )
    m % df_calculated = .false.

    ! setup a uniform grid
    allocate( m % x( nx ) )
    allocate( m % f( nx, nf ) )
    allocate( m % df( nx, nf, maxrk ) )
    ! allocate memory for mesh spacing
    allocate( m % dx(nx) )

  end subroutine init

  subroutine info(m)
  class(mesh), intent(inout) :: m
  print*,' *** General info ***'
  print*,'Mesh type: ',m % name
  print*,'Mesh size: ',m % nx
  print*,'Number of functions: ', m%nf
  end subroutine info


  subroutine free( m )
    class(mesh), intent(inout) :: m

    if( associated( m % x ) ) then
       deallocate( m % x )
    end if

    if( associated( m % f ) ) then
       deallocate( m % f )
    end if

    if( associated( m % df ) ) then
       deallocate( m % df )
    end if

    if( associated( m % df_calculated ) ) then
       deallocate( m % df_calculated )
    end if

    if( associated( m % dx ) ) then
       deallocate( m % dx )
    end if

  end subroutine free


  ! calculates i-th derivative of j-th function at k-th point
  function derivative( m, k, j, i ) result(d)
    class(mesh), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: d
    d = 0.

    stop 'method "derivative" is not overloaded'

  end function derivative


  ! calculates i-th derivative of all functions at all points
  subroutine calculate_derivatives( m, i )
    class(mesh), target, intent(inout) :: m
    integer, intent(in) :: i

    stop 'method "calculate_derivatives" not overloaded'

  end subroutine calculate_derivatives


  ! used to check weather a derivative should be calculated
  logical function check_derivatives( m, i )
    class(mesh), target, intent(inout) :: m
    integer :: i

    check_derivatives = .false.

    if( m % df_calculated( i ) ) then
       return
    else if( i > m % maxrk ) then
       stop "maxrk exceeded!"
       return
    else
       check_derivatives = .true.
       m % df_calculated( i ) = .true.
       return
    end if
  end function check_derivatives


  ! to be run after changes to m % f, after this df is considered to
  ! be not calculated
  subroutine clear_derivatives( m )
    class(mesh), target, intent(inout) :: m

    m % df_calculated = .false.

  end subroutine clear_derivatives


  ! this is not the best way to convert to a vector velue because it
  ! copies data
  subroutine to_vector( m, v )
    class(mesh), target, intent(in) :: m
    real, pointer, intent(inout) :: v(:)

    v(1 : m%nf * m%nx) => m % f

  end subroutine to_vector

  ! not needed anymore!
  subroutine from_vector( m, v )
    class(mesh), intent(inout) :: m
    real, intent(in) :: v(:)

    m % f = reshape( v, shape( m % f ) )
    call m % clear_derivatives()

  end subroutine from_vector


  ! to be implemented:
  ! from_function(m, fns)
  ! from_vector(m, v)

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
    forall(i = 1:m%nx, j = 1:m%nf) m%f(i,j) = i*100+j

  end subroutine fill_for_debug


  ! subroutine print_01( m, i )
  !   class(mesh), intent(inout) :: m
  !   integer :: i

  ! end subroutine print_01

  ! form is an optional argument
  subroutine print_by_index( m, f_select, file_desc, form )
    class(mesh), intent(inout) :: m
    integer, intent(in) :: f_select(:)
    integer, intent(in) :: file_desc
    character(len=*), optional :: form
    integer :: i,j

    ! strange write juggling
    do i = 1, size(m % x)
       ! write mesh point first
       write( file_desc, n_format(1,form), advance='no') m % x(i)

       ! write all selected functions
       do j = 1, size(f_select)
          write (file_desc, n_format(1,form), advance='no') &
               m % f(i,f_select(j))
       end do

       ! end with a newline
       write (file_desc,*)
    end do

  end subroutine print_by_index

  function integrate( m, f ) result(r)
    class(mesh) :: m
    real :: r
    real, intent(in) :: f(:)
    real, pointer :: dx(:), x(:)
    integer :: nx

    dx => m % dx


    call m % calculate_spacings
    ! integrate f wrt measure dx
    r = sum(f*dx)

  end function integrate

  subroutine calculate_spacings(m)
    class(mesh) :: m
    real, pointer :: dx(:), x(:)
    integer :: nx
    dx => m % dx
    x  => m % x
    nx = m % nx

    ! calculate mesh spacing
    dx(2:nx-1) = (x(3:nx) - x(1:nx-2))/2.
    dx(1) = (x(2)-x(1))/2.
    dx(nx) = (x(nx)-x(nx-1))/2.
  end subroutine calculate_spacings


end module class_mesh

