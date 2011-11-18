!>
!! @file   mesh.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Nov 18 12:57:09 2011
!!
!! @brief File contains a skeleton of the mesh class used to store a
!! discrete representation of a set of functions defined on grid
!! points.
!!
!! Programmer extending this class should provide approximations to
!! derivatives of the functions with respect to mesh, and possibly an
!! algorighm to calculate an integral of a givan function, see
!! \ref mesh\%integrate.
!!
module class_mesh

  use pretty_print

  private

  !> @todo write an additional interface to the calculate_derivatives
  !! etc. which does not use internal tables and takes input/output
  !! data as arguments

  !> skeleton mesh class to be extended by user-defined meshes
  type, public :: mesh
     !> mesh name
     character(len=300)        :: name = ""
     !> number of mesh points, determines the size of x(nx)
     integer                   :: nx = 0
     !> rk holds a maximal rank of derivative that can be calculated
     !! using this mesh
     integer                   :: rk = 0
     ! !> mesh should range from x0 to x1
     ! real                      :: x0 = 0.0, x1 = 0.0
     !> \f$x_i\f$
     real, pointer, contiguous :: x(:) => null()
     ! !> \f$\Delta x_i\f$
     real, pointer, contiguous :: dx(:) => null()

     !> file to which \ref info prints
     integer, pointer          :: info_file => null()

   contains
     ! obligatory
     procedure                  :: diff_point
     procedure                  :: diff_global

     ! optional
     procedure                  :: init
     procedure                  :: free
     ! procedure                  :: info
     procedure                  :: generate_x
     procedure                  :: integrate
     procedure                  :: calculate_spacings

  end type mesh

contains

  !> does allocate(mesh%x(mesh%nx)), provided that x(:) has not been
  !! already pointed to a location. In such case it sets nx based on
  !! x(:). It also _assumes_ that dx is _not_ allocated, and tries to
  !! allocate it to be of the same size as x(:).
  !!
  !! @param m
  !!
  !! @return
  !!
  subroutine init(m)
    class(mesh), intent(inout) :: m

    if( .not. associated( m % info_file ) ) then
       allocate( m % info_file )
       ! stdout_file is defined in module pretty_print to be the unit
       ! id of standard output
       m % info_file = stdout_file
    end if

    if( .not. associated(m%x) .and. m % nx > 0 ) then
       allocate( m % x ( m % nx ) )
       allocate( m % dx( m % nx ) )
       print *, "INFOR: mesh: init: generating from nx"
    else
       print *, "ERROR: mesh: init: neither, x(:) or scalar parameters were set correctly"
    end if

    if( m % rk < 0 ) then
       print *, "ERROR: mesh: init: rk < 0"
    end if

  end subroutine init

  !> Overloaded should generate contents of x(:), using an algorithm
  !! dependent on a perticular mesh implementation, e.g. uniformly or
  !! gauss-lobatto-like. As a bonus, it updates nx and dx(:) to be
  !! compatible with x(:). This function should be called _once_ per
  !! mesh. The points x(1) and x(nx) determine boundaries of the grid.
  !!
  !! @param m
  !!
  !! @return
  !!
  subroutine generate_x(m)
    class(mesh), intent(inout) :: m

    ! if m % x is associated, then update nx to size(x) and reallocate
    ! the table for dx
    if( associated( m % x ) .and. size(m%x) /= m%nx ) then
       ! update value of nx, in case x(:) has changed recentely
       m % nx = size( m%x )
       ! reallocate dx(:) to match the size of x(:)
       deallocate(m%dx)
       allocate(m%dx(m%nx))
       if( m % x(m%nx) < m % x(1) ) then
          print *, "WARNING: mesh: generate_x: x(:) runs backward"
       end if

    else
       print *, "ERROR: mesh: generate_x: x(:) is not associated"
    end if

  end subroutine generate_x


  !> Prints information on mesh parameters, should be called by
  !> inheriting structures
  !!
  !! @param m
  !!
  subroutine info(m)
    class(mesh), intent(inout) :: m
    integer :: f
    f = m % info_file

    ! @todo better info
    write(f,*)' *** General info ***'
    write(f,*)'Mesh name: ',m % name
    write(f,*)'N. of points: ',m % nx
    write(f,*)'Rank: ', m % rk
  end subroutine info

  !> Deallocate memory associated with mesh, checks if pointers are
  !! disassociated
  !!
  !! @param m
  !!
  !! @return
  !!
  subroutine free( m )
    class(mesh), intent(inout) :: m

    if( associated( m % x ) ) then
       deallocate( m % x )
    end if

    if( associated( m % dx ) ) then
       deallocate( m % dx )
    end if

  end subroutine free


  !> Skeleton function, after overloading should calculate \f$D_k f(x_i)\f$.
  !!
  !! @param m
  !! @param f function do differentiate
  !! @param k k-th derivative
  !! @param i i-th mesh point
  !!
  !! @return \f$D_k f_j(x_i)\f$
  !!
  function diff_point( m, f, k, i ) result(d)
    class(mesh), intent(inout) :: m
    integer, intent(in) :: i,k
    real, intent(in) :: f(:)
    real :: d

    ! assign some arbitrary value to avoid compilation warnings
    d = 0.

    ! @todo log error
    print *, "ERROR: mesh: diff_point: subroutine not overloaded"

  end function diff_point


  !> Skeleton subroutine, if overloaded should calculate
  !! \f$D_k f(x_i)\f$ for \f$k\f$ given as argument and save it to
  !! df
  !!
  !! @param m
  !! @param f function do differentiate
  !! @param df derivative is going to be saved here
  !! @param k rank of derivative to calculate
  !!
  subroutine diff_global( m, f, df, k )
    class(mesh), target, intent(inout) :: m
    integer, intent(in) :: k
    real, intent(in) :: f(:)
    real, intent(out) :: df(:)

    ! assign some arbitrary value to avoid compilation warnings
    df=0.

    ! @todo log error
    print *, "ERROR: mesh: internal_global_d: called with storage_active = .false."

  end subroutine diff_global

  !> Calculate integral of f (approximation of
  !! \f$\int_{x_i}^{x_j}f(x)dx\f$) using method of rectangles, works
  !! also for a nonuniform mesh. \f$(\Delta x_i)_{i=1,n}\f$ are calculated using
  !! \ref calculate_spacings.
  !!
  !! @param m
  !! @param f vector representing values of function to be integrated
  !! given at mesh points
  !!
  !! @return \f$\sum_{k=j}^{j}\Delta x_k f_k\f$
  !!
  function integrate( m, f, i, j ) result(r)
    class(mesh) :: m
    integer, intent(in) :: i,j
    real :: r
    real, intent(in) :: f(:)
    real, pointer :: dx(:), x(:)
    integer :: nx

    ! temporary pointer for brevity
    dx => m % dx

    ! use m%calculate_spacings to calculate \Delta x_i
    call m % calculate_spacings
    ! calculate sum \Delta x_i f_i
    r = sum(f(i:j)*dx(i:j))

  end function integrate

  !> calculates \f$(\Delta x_i)_{i=1,n}\f$ using the following formula
  !! \f$\Delta x_1 = \frac{x_2-x_1}{2}\f$, \f$\Delta x_n =
  !! \frac{x_n-x_{n-1}}{2}\f$, \f$\Delta x_i =
  !! \frac{x_{i+1}-x_{i-1}}{2}\f$ for the method of rectangles
  !!
  !! @param m
  !!
  !! @return
  !!
  subroutine calculate_spacings(m)
    class(mesh) :: m
    real, pointer :: dx(:), x(:)
    integer :: nx
    ! set local pointers and variables for brevity
    dx => m % dx
    x  => m % x
    nx = m % nx

    ! calculate mesh spacing
    dx(2:nx-1) = (x(3:nx) - x(1:nx-2))/2.
    dx(1) = (x(2)-x(1))/2.
    dx(nx) = (x(nx)-x(nx-1))/2.
  end subroutine calculate_spacings

end module class_mesh

