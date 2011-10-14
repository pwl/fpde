module class_mesh

  use pretty_print
  ! use omp_lib

  character(len=30), public, parameter ::&
       mesh_boundary_fixed = "fixed",&
       mesh_boundary_von_neumann = "von_neumann"

  private

  ! general mesh class to be inherited by user-defined meshes
  type, public :: mesh
     ! private
     ! inittialization parameters
     character(len=300)                    :: name = ""
     integer                               :: nx = 0, nf = 1, rk = 0
     real                                  :: x0 = 0., x1 = 0.
     character(len=30)                     :: boundary_left = "fixed"
     character(len=30)                     :: boundary_right = "fixed"
     integer, pointer                      :: info_file => null()
     ! end of initialization parameters
     character(len=30), pointer            :: &
          allowable_boundary_left(:) => null()
     character(len=30), pointer            :: &
          allowable_boundary_right(:) => null()
     real, contiguous, pointer             :: x(:) => null()
     real, contiguous, pointer             :: f(:,:) => null()
     real, contiguous, pointer             :: df(:,:,:) => null()
     real, contiguous, pointer             :: dx(:) => null()
     ! @todo obsolate, delete
     logical, private, contiguous, pointer :: df_calculated(:) => null()
   contains
     ! obligatory
     procedure                  :: derivative
     procedure                  :: calculate_derivatives

     ! optional
     procedure                  :: init
     procedure                  :: free
     procedure                  :: info
     procedure                  :: integrate
     procedure                  :: calculate_spacings

     ! use only, not to be overloaded
     procedure, non_overridable :: print_by_index
     ! generic                  :: print => print_by_index
     procedure, non_overridable :: print_preview
     procedure, non_overridable :: check_derivatives
     procedure, non_overridable :: clear_derivatives

     ! questionable
     procedure                  :: fill_for_debug
  end type mesh

contains

  subroutine init(m)
    class(mesh), intent(inout) :: m

    if( .not. associated(m % info_file) ) then
       allocate( m % info_file )
       m % info_file = stdout_file
    end if

    if( m % nx == 0 ) then
       print *, "ERROR: mesh: init: nx = 0"
    end if

    if( m % nf == 0 ) then
       print *, "ERROR: mesh: init: nf = 0"
    end if

    if( m % x1 <= m % x0 ) then
       print *, "ERROR: mesh: init: x1 <= x0"
    end if

    if( associated(m % allowable_boundary_left) .and. &
         associated(m % allowable_boundary_right) ) then
       if( .not. any(m % allowable_boundary_left == m % boundary_left) ) then
          print *, "ERROR: mesh: init: [left] boundary conditions do not match the mesh"
       end if
       if( .not. any(m % allowable_boundary_right == m % boundary_right) ) then
          print *, "ERROR: mesh: init: [right] boundary conditions do not match the mesh"
       end if
    else
       print *, "INFOR: mesh: init: allowable boundary conditions are not defined for this mesh"
    end if

    ! m % rk does not have to be >0, mesh can be used to
    ! integration (@todo or to interpolation in later versions). In
    ! the case rk = 0, the following allocation won't fail
    allocate( m % df_calculated( m % rk ) )

    allocate( m % x( m % nx ) )
    allocate( m % f( m % nx, m % nf ) )
    allocate( m % df( m % nx, m % nf, m % rk ) )
    ! allocate memory for mesh spacing
    allocate( m % dx( m % nx ) )

  end subroutine init

  subroutine info(m)
    class(mesh), intent(inout) :: m
    integer :: f
    f = m % info_file

    write(f,*)' *** General info ***'
    write(f,*)'Mesh name: ',m % name
    write(f,*)'N. of points: ',m % nx
    write(f,*)'Number of functions: ', m%nf
    write(f,*)'Rank: ', m % rk
    write(f,*)'Range: [', m % x0, ", ", m % x1, "]"
    write(f,*)'Boundary [left, right]: [ ',&
         trim(m % boundary_left), ", ", trim(m % boundary_right), ' ]'
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

    print *, "INFOR: mesh: derivative: subroutine not overloaded"

  end function derivative


  ! calculates i-th derivative of all functions at all points
  subroutine calculate_derivatives( m, i )
    class(mesh), target, intent(inout) :: m
    integer, intent(in) :: i

    print *, "INFOR: mesh: calculate_derivatives: subroutine not overloaded"

  end subroutine calculate_derivatives


  ! used to check weather a derivative should be calculated
  logical function check_derivatives( m, i )
    class(mesh), target, intent(inout) :: m
    integer :: i

    check_derivatives = .false.

    if( m % df_calculated( i ) ) then
       return
    else if( i > m % rk ) then
       stop "rk exceeded!"
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

  ! used to print a readable output of a mesh, to be improved
  subroutine print_preview( m )
    class(mesh), intent(in) :: m
    integer :: offset = 5, i

    ! print parameters
    print *, ""
    print *, "nx,nf,rk = ",&
         m % nx, m % nf, m % rk

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
    integer :: nx = 10, nf = 3, rk = 2
    integer :: i,j

    m % nx = nx
    m % nf = nf
    m % rk = rk
    m % x1 = 1.
    m % name = "mesh debug"
    call m % init

    call m%init
    forall(i = 1:m%nx, j = 1:m%nf) m%f(i,j) = i*100+j

  end subroutine fill_for_debug


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

