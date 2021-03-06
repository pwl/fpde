module class_mesh_afd5pt
  use class_mesh
  use utils_random_seed
  use pretty_print

  character(len=30), private, target :: &
       bd_left(2) = [mesh_boundary_fixed, mesh_boundary_von_neumann]
  character(len=30), private, target :: &
       bd_right(1) = [mesh_boundary_fixed]

  ! everything except the type should be private
  private

  type, public, extends( mesh ) :: mesh_afd5pt
   contains
     ! overloaded procedures go here (if needed)
     procedure :: init
     procedure :: derivative
     procedure :: calculate_derivatives
  end type mesh_afd5pt

contains

  subroutine init( m )
    class(mesh_afd5pt), intent(inout) :: m
    integer :: i
    real :: pi
    pi = acos(-1.)

    ! first set the allowable boundary conditions, so the init from
    ! type mesh can verifie the ones supplied by the user
    m % allowable_boundary_left => bd_left
    m % allowable_boundary_right => bd_right

    ! then, check if the left or right default boundaries have been
    ! set by the user, if not, set them to the vaues corresponding to
    ! this particular mesh
    if( m % boundary_left_default == mesh_boundary_default ) then
       m % boundary_left_default = mesh_boundary_fixed
    end if

    if( m % boundary_right_default == mesh_boundary_default ) then
       m % boundary_right_default = mesh_boundary_fixed
    end if

    ! initialize parent
    call m % mesh % init

    ! set the new name if empty
    call set_string_if_empty( m % name, "mesh_afd5pt" )

    ! setup a sample non uniform grid, @todo remove
    forall( i = 1: m % nx )
       m % x(i) = (m % x1 - m % x0)*(1.-cos(real(i-1)/real(m % nx-1)*pi))/2.+ m % x0
    end forall

  end subroutine init

  function derivative( m, k, j, i ) result(d)
    class(mesh_afd5pt), intent(inout) :: m
    integer, intent(in) :: k,j,i
    real :: d
    integer :: nx, nf
    real, pointer :: f(:,:), x(:)

    f => m % f
    x => m % x
    nx = m % nx
    nf = m % nf

    select case( m % boundary_left(j) )
    case( mesh_boundary_von_neumann )
       f(1,j) =\
       (-((f(2,j)*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5))**2)/((x(2) - \
       x(3))*(x(2) - x(4))*(x(2) - x(5)))) + (f(3,j)*(x(1) - x(2))**2*(x(1) \
       - x(4))*(x(1) - x(5))**2)/((x(1) - x(3))*(x(2) - x(3))*(x(3) - \
       x(4))*(x(3) - x(5))) - (f(4,j)*(x(1) - x(2))**2*(x(1) - x(3))*(x(1) - \
       x(5))**2)/((x(3) - x(4))*(-x(1) + x(4))*(-x(2) + x(4))*(x(4) - x(5))) \
       + (f(5,j)*(x(1) - x(2))**2*(x(1) - x(3))*(x(1) - x(4)))/((x(3) - \
       x(5))*(-x(2) + x(5))*(-x(4) + x(5))))/((x(1) - x(2))*(-1 + ((-1 + \
       ((-2*x(1) + x(2) + x(3))*(-x(1) + x(4)))/((x(1) - x(2))*(-x(1) + \
       x(3))))*(-x(1) + x(5)))/(-x(1) + x(4))))
    end select


    select case(i)
    case(1)
       ! first derivatives
       if( k == 1 ) then
          ! various method of calculating derivetives depending on the
          ! boundary conditions
          select case(m % boundary_left(j))

             ! "fixed" boundary conditions
          case(mesh_boundary_fixed)
             d=\
             ((f(2,j)*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5))**2)/((x(1) - \
             x(2))*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))) + (f(3,j)*(x(1) - \
             x(2))*(x(1) - x(4))*(x(1) - x(5))**2)/((x(1) - x(3))*(-x(2) + \
             x(3))*(x(3) - x(4))*(x(3) - x(5))) + (f(4,j)*(x(1) - x(2))*(x(1) - \
             x(3))*(x(1) - x(5))**2)/((x(1) - x(4))*(-x(2) + x(4))*(-x(3) + \
             x(4))*(x(4) - x(5))) + (f(5,j)*(x(1) - x(2))*(x(1) - x(3))*(x(1) - \
             x(4)))/((-x(2) + x(5))*(-x(3) + x(5))*(-x(4) + x(5))) + f(1,j)*(-1 + \
             ((-1 + ((-2*x(1) + x(2) + x(3))*(-x(1) + x(4)))/((x(1) - x(2))*(-x(1) \
             + x(3))))*(-x(1) + x(5)))/(-x(1) + x(4))))/(-x(1) + x(5))

             ! "von_neumann" boundary conditions
          case(mesh_boundary_von_neumann)
             ! function is symmetric, first derivative is set to 0.
             d = 0.

             ! undefined behavior brake the calculations
          case default
             d = sqrt(-1.)

          end select


       elseif( k == 2 ) then

          d=\
          (-((f(1,j)*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))**2)/((x(1) - \
          x(2))*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5)))) + (f(4,j)*(-x(1) + \
          x(2))*(x(2) - x(3))*(x(2) - x(5))**2)/((x(1) - x(4))*(-x(2) + \
          x(4))*(-x(3) + x(4))*(x(4) - x(5))) + (f(3,j)*(-x(1) + x(2))*(-x(2) + \
          x(4))*(x(2) - x(5))**2)/((-x(1) + x(3))*(-x(2) + x(3))*(-x(3) + \
          x(4))*(-x(3) + x(5))) + (f(5,j)*(x(1) - x(2))*(x(2) - x(3))*(x(2) - \
          x(4)))/((x(1) - x(5))*(-x(3) + x(5))*(-x(4) + x(5))) + f(2,j)*(-1 + \
          ((-1 - ((x(1) - 2*x(2) + x(3))*(x(2) - x(4)))/((x(1) - x(2))*(x(2) - \
          x(3))))*(-x(2) + x(5)))/(-x(2) + x(4))))/(-x(2) + x(5))


          ! the remaining right boundary conditions are defaulted to
          ! fixed boundary conditions without performing the checks
       elseif( k == nx-1 ) then
          d=\
          ((f(-4 + nx,j)*(x(-3 + nx) - x(-1 + nx))*(-x(-2 + nx) + x(-1 + \
          nx))*(x(-1 + nx) - x(nx))**2)/((x(-4 + nx) - x(-3 + nx))*(x(-4 + nx) \
          - x(-2 + nx))*(x(-4 + nx) - x(nx))) + (f(-3 + nx,j)*(x(-4 + nx) - \
          x(-1 + nx))**2*(x(-2 + nx) - x(-1 + nx))*(x(-1 + nx) - \
          x(nx))**2)/((x(-4 + nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 + nx))*(x(-3 \
          + nx) - x(-1 + nx))*(x(-3 + nx) - x(nx))) + (f(-2 + nx,j)*(x(-4 + nx) \
          - x(-1 + nx))**2*(x(-3 + nx) - x(-1 + nx))*(x(-1 + nx) - \
          x(nx))**2)/((x(-4 + nx) - x(-2 + nx))*(-x(-3 + nx) + x(-2 + \
          nx))*(x(-2 + nx) - x(-1 + nx))*(x(-2 + nx) - x(nx))) + (f(nx,j)*(x(-4 \
          + nx) - x(-1 + nx))**2*(-x(-3 + nx) + x(-1 + nx))*(-x(-2 + nx) + x(-1 \
          + nx)))/((x(-4 + nx) - x(nx))*(-x(-3 + nx) + x(nx))*(-x(-2 + nx) + \
          x(nx))) + (f(-1 + nx,j)*(x(-3 + nx)*(x(-1 + nx)*(3*x(-1 + nx) - \
          2*x(nx)) + x(-2 + nx)*(-2*x(-1 + nx) + x(nx))) + x(-4 + nx)*(x(-1 + \
          nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-2 + nx)*(-2*x(-1 + nx) + x(nx)) + \
          x(-3 + nx)*(x(-2 + nx) - 2*x(-1 + nx) + x(nx))) + x(-1 + nx)*(x(-2 + \
          nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-1 + nx)*(-4*x(-1 + nx) + \
          3*x(nx)))))/((x(-3 + nx) - x(-1 + nx))*(-x(-2 + nx) + x(-1 + \
          nx))))/((x(-4 + nx) - x(-1 + nx))*(-x(-1 + nx) + x(nx)))

       elseif( k == nx ) then
          d=\
          (-((f(-3 + nx,j)*(x(-4 + nx) - x(nx))**2*(x(-2 + nx) - x(nx))*(x(-1 + \
          nx) - x(nx)))/((x(-4 + nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 + \
          nx))*(x(-3 + nx) - x(-1 + nx)))) + (f(-4 + nx,j)*(x(-3 + nx) - \
          x(nx))**2*(x(-2 + nx) - x(nx))*(x(-1 + nx) - x(nx)))/((x(-4 + nx) - \
          x(-3 + nx))*(x(-4 + nx) - x(-2 + nx))*(x(-4 + nx) - x(-1 + nx))) + \
          (f(-1 + nx,j)*(x(-4 + nx) - x(nx))**2*(x(-3 + nx) - x(nx))**2*(-x(-2 \
          + nx) + x(nx)))/((x(-4 + nx) - x(-1 + nx))*(-x(-3 + nx) + x(-1 + \
          nx))*(-x(-2 + nx) + x(-1 + nx))*(x(-1 + nx) - x(nx))) + (f(-2 + \
          nx,j)*(x(-4 + nx) - x(nx))**2*(x(-3 + nx) - x(nx))**2*(-x(-1 + nx) + \
          x(nx)))/((x(-4 + nx) - x(-2 + nx))*(-x(-3 + nx) + x(-2 + nx))*(x(-2 + \
          nx) - x(-1 + nx))*(x(-2 + nx) - x(nx))) + (f(nx,j)*(x(nx)*((3*x(-1 + \
          nx) - 4*x(nx))*x(nx) + x(-2 + nx)*(-2*x(-1 + nx) + 3*x(nx))) + x(-3 + \
          nx)*(x(-2 + nx)*(x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + \
          3*x(nx))) + x(-4 + nx)*(x(-2 + nx)*(x(-1 + nx) - 2*x(nx)) + x(-3 + \
          nx)*(x(-2 + nx) + x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + \
          3*x(nx)))))/((-x(-2 + nx) + x(nx))*(-x(-1 + nx) + x(nx))))/((x(-3 + \
          nx) - x(nx))*(-x(-4 + nx) + x(nx)))

       else
          d= \
          (f(k,j)*(-1 + ((x(k)*(3*x(k) - 2*x(1 + k)) + x(-1 + k)*(-2*x(k) + x(1 \
          + k)) + x(-2 + k)*(x(-1 + k) - 2*x(k) + x(1 + k)))*(x(k) - x(2 + \
          k)))/((x(-2 + k) - x(k))*(-x(-1 + k) + x(k))*(x(k) - x(1 + k)))) + \
          (f(-2 + k,j)*(x(-1 + k) - x(k))*(x(k) - x(1 + k))*(x(k) - x(2 + \
          k))**2)/((x(-2 + k) - x(-1 + k))*(x(-2 + k) - x(k))*(x(-2 + k) - x(1 \
          + k))*(x(-2 + k) - x(2 + k))) - (f(-1 + k,j)*(x(-2 + k) - x(k))*(x(k) \
          - x(1 + k))*(x(k) - x(2 + k))**2)/((x(-2 + k) - x(-1 + k))*(x(-1 + k) \
          - x(k))*(x(-1 + k) - x(1 + k))*(x(-1 + k) - x(2 + k))) + (f(1 + \
          k,j)*(-x(-2 + k) + x(k))*(-x(-1 + k) + x(k))*(x(k) - x(2 + \
          k))**2)/((x(-2 + k) - x(1 + k))*(-x(-1 + k) + x(1 + k))*(-x(k) + x(1 \
          + k))*(x(1 + k) - x(2 + k))) + (f(2 + k,j)*(x(-2 + k) - x(k))*(-x(-1 \
          + k) + x(k))*(x(k) - x(1 + k)))/((x(-2 + k) - x(2 + k))*(-x(-1 + k) + \
          x(2 + k))*(-x(1 + k) + x(2 + k))))/(-x(k) + x(2 + k))
       end if

    case(2)
       ! second derivatives
       if( k == 1 ) then

             d= \
             (-2*f(5,j)*(3*x(1)**2 + x(3)*x(4) + x(2)*(x(3) + x(4)) - 2*x(1)*(x(2) \
             + x(3) + x(4))))/((x(1) - x(5))*(-x(2) + x(5))*(-x(3) + x(5))*(-x(4) \
             + x(5))) - (2*f(4,j)*(3*x(1)**2 + x(3)*x(5) + x(2)*(x(3) + x(5)) - \
             2*x(1)*(x(2) + x(3) + x(5))))/((x(1) - x(4))*(-x(2) + x(4))*(-x(3) + \
             x(4))*(x(4) - x(5))) - (2*f(3,j)*(3*x(1)**2 + x(4)*x(5) + x(2)*(x(4) \
             + x(5)) - 2*x(1)*(x(2) + x(4) + x(5))))/((x(1) - x(3))*(-x(2) + \
             x(3))*(x(3) - x(4))*(x(3) - x(5))) - (2*f(2,j)*(3*x(1)**2 + x(4)*x(5) \
             + x(3)*(x(4) + x(5)) - 2*x(1)*(x(3) + x(4) + x(5))))/((x(1) - \
             x(2))*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))) + \
             (2*f(1,j)*(6*x(1)**2 + x(3)*x(4) + x(3)*x(5) + x(4)*x(5) + x(2)*(x(3) \
             + x(4) + x(5)) - 3*x(1)*(x(2) + x(3) + x(4) + x(5))))/((x(1) - \
             x(2))*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5)))


          elseif( k == 2) then

             d= \
             2*(-((f(5,j)*(3*x(2)**2 + x(3)*x(4) - 2*x(2)*(x(3) + x(4)) + \
             x(1)*(-2*x(2) + x(3) + x(4))))/((x(1) - x(5))*(-x(2) + x(5))*(-x(3) + \
             x(5))*(-x(4) + x(5)))) + (f(4,j)*(-3*x(2)**2 + x(1)*(2*x(2) - x(3) - \
             x(5)) - x(3)*x(5) + 2*x(2)*(x(3) + x(5))))/((x(1) - x(4))*(-x(2) + \
             x(4))*(-x(3) + x(4))*(x(4) - x(5))) + (f(3,j)*(-3*x(2)**2 + \
             x(1)*(2*x(2) - x(4) - x(5)) - x(4)*x(5) + 2*x(2)*(x(4) + \
             x(5))))/((x(1) - x(3))*(-x(2) + x(3))*(x(3) - x(4))*(x(3) - x(5))) + \
             (f(1,j)*(3*x(2)**2 + x(4)*x(5) + x(3)*(x(4) + x(5)) - 2*x(2)*(x(3) + \
             x(4) + x(5))))/((x(1) - x(2))*(x(1) - x(3))*(x(1) - x(4))*(x(1) - \
             x(5))) + (f(2,j)*(-6*x(2)**2 - x(3)*x(4) + x(1)*(3*x(2) - x(3) - x(4) \
             - x(5)) - x(3)*x(5) - x(4)*x(5) + 3*x(2)*(x(3) + x(4) + \
             x(5))))/((x(1) - x(2))*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))))

       elseif( k == nx - 1) then
          d= \
          (-2*f(nx,j)*(x(-3 + nx)*(x(-2 + nx) - 2*x(-1 + nx)) + x(-4 + \
          nx)*(x(-3 + nx) + x(-2 + nx) - 2*x(-1 + nx)) + x(-1 + nx)*(-2*x(-2 + \
          nx) + 3*x(-1 + nx))))/((x(-4 + nx) - x(nx))*(-x(-3 + nx) + \
          x(nx))*(-x(-2 + nx) + x(nx))*(-x(-1 + nx) + x(nx))) - (2*f(-1 + \
          nx,j)*(-3*x(-2 + nx)*x(-1 + nx) + 6*x(-1 + nx)**2 + x(-2 + nx)*x(nx) \
          - 3*x(-1 + nx)*x(nx) + x(-3 + nx)*(x(-2 + nx) - 3*x(-1 + nx) + x(nx)) \
          + x(-4 + nx)*(x(-3 + nx) + x(-2 + nx) - 3*x(-1 + nx) + \
          x(nx))))/((x(-4 + nx) - x(-1 + nx))*(-x(-3 + nx) + x(-1 + nx))*(-x(-2 \
          + nx) + x(-1 + nx))*(x(-1 + nx) - x(nx))) - (2*f(-2 + nx,j)*(x(-1 + \
          nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-3 + nx)*(-2*x(-1 + nx) + x(nx)) + \
          x(-4 + nx)*(x(-3 + nx) - 2*x(-1 + nx) + x(nx))))/((x(-4 + nx) - x(-2 \
          + nx))*(-x(-3 + nx) + x(-2 + nx))*(x(-2 + nx) - x(-1 + nx))*(x(-2 + \
          nx) - x(nx))) - (2*f(-3 + nx,j)*(x(-1 + nx)*(3*x(-1 + nx) - 2*x(nx)) \
          + x(-2 + nx)*(-2*x(-1 + nx) + x(nx)) + x(-4 + nx)*(x(-2 + nx) - \
          2*x(-1 + nx) + x(nx))))/((x(-4 + nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 \
          + nx))*(x(-3 + nx) - x(-1 + nx))*(x(-3 + nx) - x(nx))) + (2*f(-4 + \
          nx,j)*(x(-1 + nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-2 + nx)*(-2*x(-1 + \
          nx) + x(nx)) + x(-3 + nx)*(x(-2 + nx) - 2*x(-1 + nx) + \
          x(nx))))/((x(-4 + nx) - x(-3 + nx))*(x(-4 + nx) - x(-2 + nx))*(x(-4 + \
          nx) - x(-1 + nx))*(x(-4 + nx) - x(nx)))

       elseif( k == nx ) then
          d= \
          (-2*f(nx,j)*(x(-2 + nx)*x(-1 + nx) + x(-3 + nx)*(x(-2 + nx) + x(-1 + \
          nx) - 3*x(nx)) + x(-4 + nx)*(x(-3 + nx) + x(-2 + nx) + x(-1 + nx) - \
          3*x(nx)) - 3*x(-2 + nx)*x(nx) - 3*x(-1 + nx)*x(nx) + \
          6*x(nx)**2))/((x(-4 + nx) - x(nx))*(-x(-3 + nx) + x(nx))*(-x(-2 + nx) \
          + x(nx))*(-x(-1 + nx) + x(nx))) - (2*f(-1 + nx,j)*(x(-3 + nx)*(x(-2 + \
          nx) - 2*x(nx)) + x(-4 + nx)*(x(-3 + nx) + x(-2 + nx) - 2*x(nx)) + \
          x(nx)*(-2*x(-2 + nx) + 3*x(nx))))/((x(-4 + nx) - x(-1 + nx))*(-x(-3 + \
          nx) + x(-1 + nx))*(-x(-2 + nx) + x(-1 + nx))*(x(-1 + nx) - x(nx))) - \
          (2*f(-2 + nx,j)*(x(-3 + nx)*(x(-1 + nx) - 2*x(nx)) + x(-4 + nx)*(x(-3 \
          + nx) + x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + \
          3*x(nx))))/((x(-4 + nx) - x(-2 + nx))*(-x(-3 + nx) + x(-2 + \
          nx))*(x(-2 + nx) - x(-1 + nx))*(x(-2 + nx) - x(nx))) - (2*f(-3 + \
          nx,j)*(x(-2 + nx)*(x(-1 + nx) - 2*x(nx)) + x(-4 + nx)*(x(-2 + nx) + \
          x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + 3*x(nx))))/((x(-4 + \
          nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 + nx))*(x(-3 + nx) - x(-1 + \
          nx))*(x(-3 + nx) - x(nx))) + (2*f(-4 + nx,j)*(x(-2 + nx)*(x(-1 + nx) \
          - 2*x(nx)) + x(-3 + nx)*(x(-2 + nx) + x(-1 + nx) - 2*x(nx)) + \
          x(nx)*(-2*x(-1 + nx) + 3*x(nx))))/((x(-4 + nx) - x(-3 + nx))*(x(-4 + \
          nx) - x(-2 + nx))*(x(-4 + nx) - x(-1 + nx))*(x(-4 + nx) - x(nx)))

       else
          d= \
          (-2*f(2 + k,j)*(x(k)*(3*x(k) - 2*x(1 + k)) + x(-1 + k)*(-2*x(k) + x(1 \
          + k)) + x(-2 + k)*(x(-1 + k) - 2*x(k) + x(1 + k))))/((x(-2 + k) - x(2 \
          + k))*(-x(-1 + k) + x(2 + k))*(-x(k) + x(2 + k))*(-x(1 + k) + x(2 + \
          k))) - (2*f(1 + k,j)*(x(k)*(3*x(k) - 2*x(2 + k)) + x(-1 + k)*(-2*x(k) \
          + x(2 + k)) + x(-2 + k)*(x(-1 + k) - 2*x(k) + x(2 + k))))/((x(-2 + k) \
          - x(1 + k))*(-x(-1 + k) + x(1 + k))*(-x(k) + x(1 + k))*(x(1 + k) - \
          x(2 + k))) + (2*f(-1 + k,j)*(-3*x(k)**2 + x(-2 + k)*(2*x(k) - x(1 + \
          k) - x(2 + k)) - x(1 + k)*x(2 + k) + 2*x(k)*(x(1 + k) + x(2 + \
          k))))/((x(-2 + k) - x(-1 + k))*(x(-1 + k) - x(k))*(x(-1 + k) - x(1 + \
          k))*(x(-1 + k) - x(2 + k))) - (2*f(-2 + k,j)*(-3*x(k)**2 + x(-1 + \
          k)*(2*x(k) - x(1 + k) - x(2 + k)) - x(1 + k)*x(2 + k) + 2*x(k)*(x(1 + \
          k) + x(2 + k))))/((x(-2 + k) - x(-1 + k))*(x(-2 + k) - x(k))*(x(-2 + \
          k) - x(1 + k))*(x(-2 + k) - x(2 + k))) - (2*f(k,j)*(6*x(k)**2 - \
          3*x(k)*x(1 + k) - 3*x(k)*x(2 + k) + x(1 + k)*x(2 + k) + x(-1 + \
          k)*(-3*x(k) + x(1 + k) + x(2 + k)) + x(-2 + k)*(x(-1 + k) - 3*x(k) + \
          x(1 + k) + x(2 + k))))/((x(-2 + k) - x(k))*(-x(-1 + k) + x(k))*(x(k) \
          - x(1 + k))*(x(k) - x(2 + k)))
       end if


    case default
       ! @todo error
       print *, trim(m%name), " cannot calculate derivatives higher than 2"
    end select


  end function derivative

  recursive subroutine calculate_derivatives( m, i )
    class(mesh_afd5pt), target, intent(inout) :: m
    integer, intent(in) :: i
    integer :: j,k,nx,nf
    real, pointer :: f(:,:), x(:)

    f => m % f
    x => m % x
    nx = m % nx
    nf = m % nf

    do j = 1, nf
       select case( m % boundary_left(j) )
       case( mesh_boundary_von_neumann )
          f(1,j) =\
          (-((f(2,j)*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5))**2)/((x(2) - \
          x(3))*(x(2) - x(4))*(x(2) - x(5)))) + (f(3,j)*(x(1) - x(2))**2*(x(1) \
          - x(4))*(x(1) - x(5))**2)/((x(1) - x(3))*(x(2) - x(3))*(x(3) - \
          x(4))*(x(3) - x(5))) - (f(4,j)*(x(1) - x(2))**2*(x(1) - x(3))*(x(1) - \
          x(5))**2)/((x(3) - x(4))*(-x(1) + x(4))*(-x(2) + x(4))*(x(4) - x(5))) \
          + (f(5,j)*(x(1) - x(2))**2*(x(1) - x(3))*(x(1) - x(4)))/((x(3) - \
          x(5))*(-x(2) + x(5))*(-x(4) + x(5))))/((x(1) - x(2))*(-1 + ((-1 + \
          ((-2*x(1) + x(2) + x(3))*(-x(1) + x(4)))/((x(1) - x(2))*(-x(1) + \
          x(3))))*(-x(1) + x(5)))/(-x(1) + x(4))))

       end select
    end do

    select case(i)

    case(1)
       ! first derivatives
       forall( j = 1 : m % nf, &
            k = 3 : m % nx - 2 )

          m%df(k,j,i) = \
          (f(k,j)*(-1 + ((x(k)*(3*x(k) - 2*x(1 + k)) + x(-1 + k)*(-2*x(k) + x(1 \
          + k)) + x(-2 + k)*(x(-1 + k) - 2*x(k) + x(1 + k)))*(x(k) - x(2 + \
          k)))/((x(-2 + k) - x(k))*(-x(-1 + k) + x(k))*(x(k) - x(1 + k)))) + \
          (f(-2 + k,j)*(x(-1 + k) - x(k))*(x(k) - x(1 + k))*(x(k) - x(2 + \
          k))**2)/((x(-2 + k) - x(-1 + k))*(x(-2 + k) - x(k))*(x(-2 + k) - x(1 \
          + k))*(x(-2 + k) - x(2 + k))) - (f(-1 + k,j)*(x(-2 + k) - x(k))*(x(k) \
          - x(1 + k))*(x(k) - x(2 + k))**2)/((x(-2 + k) - x(-1 + k))*(x(-1 + k) \
          - x(k))*(x(-1 + k) - x(1 + k))*(x(-1 + k) - x(2 + k))) + (f(1 + \
          k,j)*(-x(-2 + k) + x(k))*(-x(-1 + k) + x(k))*(x(k) - x(2 + \
          k))**2)/((x(-2 + k) - x(1 + k))*(-x(-1 + k) + x(1 + k))*(-x(k) + x(1 \
          + k))*(x(1 + k) - x(2 + k))) + (f(2 + k,j)*(x(-2 + k) - x(k))*(-x(-1 \
          + k) + x(k))*(x(k) - x(1 + k)))/((x(-2 + k) - x(2 + k))*(-x(-1 + k) + \
          x(2 + k))*(-x(1 + k) + x(2 + k))))/(-x(k) + x(2 + k))

       end forall

       forall( j = 1 : m % nf )

             m%df(1,   j,i)=\
             ((f(2,j)*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5))**2)/((x(1) - \
             x(2))*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))) + (f(3,j)*(x(1) - \
             x(2))*(x(1) - x(4))*(x(1) - x(5))**2)/((x(1) - x(3))*(-x(2) + \
             x(3))*(x(3) - x(4))*(x(3) - x(5))) + (f(4,j)*(x(1) - x(2))*(x(1) - \
             x(3))*(x(1) - x(5))**2)/((x(1) - x(4))*(-x(2) + x(4))*(-x(3) + \
             x(4))*(x(4) - x(5))) + (f(5,j)*(x(1) - x(2))*(x(1) - x(3))*(x(1) - \
             x(4)))/((-x(2) + x(5))*(-x(3) + x(5))*(-x(4) + x(5))) + f(1,j)*(-1 + \
             ((-1 + ((-2*x(1) + x(2) + x(3))*(-x(1) + x(4)))/((x(1) - x(2))*(-x(1) \
             + x(3))))*(-x(1) + x(5)))/(-x(1) + x(4))))/(-x(1) + x(5))

             m%df(2,   j,i)=\
             (-((f(1,j)*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))**2)/((x(1) - \
             x(2))*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5)))) + (f(4,j)*(-x(1) + \
             x(2))*(x(2) - x(3))*(x(2) - x(5))**2)/((x(1) - x(4))*(-x(2) + \
             x(4))*(-x(3) + x(4))*(x(4) - x(5))) + (f(3,j)*(-x(1) + x(2))*(-x(2) + \
             x(4))*(x(2) - x(5))**2)/((-x(1) + x(3))*(-x(2) + x(3))*(-x(3) + \
             x(4))*(-x(3) + x(5))) + (f(5,j)*(x(1) - x(2))*(x(2) - x(3))*(x(2) - \
             x(4)))/((x(1) - x(5))*(-x(3) + x(5))*(-x(4) + x(5))) + f(2,j)*(-1 + \
             ((-1 - ((x(1) - 2*x(2) + x(3))*(x(2) - x(4)))/((x(1) - x(2))*(x(2) - \
             x(3))))*(-x(2) + x(5)))/(-x(2) + x(4))))/(-x(2) + x(5))

       end forall

       ! the right boundary is left intact
       forall( j = 1 : m % nf )
          m%df(nx-1,j,i)=\
          ((f(-4 + nx,j)*(x(-3 + nx) - x(-1 + nx))*(-x(-2 + nx) + x(-1 + \
          nx))*(x(-1 + nx) - x(nx))**2)/((x(-4 + nx) - x(-3 + nx))*(x(-4 + nx) \
          - x(-2 + nx))*(x(-4 + nx) - x(nx))) + (f(-3 + nx,j)*(x(-4 + nx) - \
          x(-1 + nx))**2*(x(-2 + nx) - x(-1 + nx))*(x(-1 + nx) - \
          x(nx))**2)/((x(-4 + nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 + nx))*(x(-3 \
          + nx) - x(-1 + nx))*(x(-3 + nx) - x(nx))) + (f(-2 + nx,j)*(x(-4 + nx) \
          - x(-1 + nx))**2*(x(-3 + nx) - x(-1 + nx))*(x(-1 + nx) - \
          x(nx))**2)/((x(-4 + nx) - x(-2 + nx))*(-x(-3 + nx) + x(-2 + \
          nx))*(x(-2 + nx) - x(-1 + nx))*(x(-2 + nx) - x(nx))) + (f(nx,j)*(x(-4 \
          + nx) - x(-1 + nx))**2*(-x(-3 + nx) + x(-1 + nx))*(-x(-2 + nx) + x(-1 \
          + nx)))/((x(-4 + nx) - x(nx))*(-x(-3 + nx) + x(nx))*(-x(-2 + nx) + \
          x(nx))) + (f(-1 + nx,j)*(x(-3 + nx)*(x(-1 + nx)*(3*x(-1 + nx) - \
          2*x(nx)) + x(-2 + nx)*(-2*x(-1 + nx) + x(nx))) + x(-4 + nx)*(x(-1 + \
          nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-2 + nx)*(-2*x(-1 + nx) + x(nx)) + \
          x(-3 + nx)*(x(-2 + nx) - 2*x(-1 + nx) + x(nx))) + x(-1 + nx)*(x(-2 + \
          nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-1 + nx)*(-4*x(-1 + nx) + \
          3*x(nx)))))/((x(-3 + nx) - x(-1 + nx))*(-x(-2 + nx) + x(-1 + \
          nx))))/((x(-4 + nx) - x(-1 + nx))*(-x(-1 + nx) + x(nx)))

          m%df(nx  ,j,i)=\
          (-((f(-3 + nx,j)*(x(-4 + nx) - x(nx))**2*(x(-2 + nx) - x(nx))*(x(-1 + \
          nx) - x(nx)))/((x(-4 + nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 + \
          nx))*(x(-3 + nx) - x(-1 + nx)))) + (f(-4 + nx,j)*(x(-3 + nx) - \
          x(nx))**2*(x(-2 + nx) - x(nx))*(x(-1 + nx) - x(nx)))/((x(-4 + nx) - \
          x(-3 + nx))*(x(-4 + nx) - x(-2 + nx))*(x(-4 + nx) - x(-1 + nx))) + \
          (f(-1 + nx,j)*(x(-4 + nx) - x(nx))**2*(x(-3 + nx) - x(nx))**2*(-x(-2 \
          + nx) + x(nx)))/((x(-4 + nx) - x(-1 + nx))*(-x(-3 + nx) + x(-1 + \
          nx))*(-x(-2 + nx) + x(-1 + nx))*(x(-1 + nx) - x(nx))) + (f(-2 + \
          nx,j)*(x(-4 + nx) - x(nx))**2*(x(-3 + nx) - x(nx))**2*(-x(-1 + nx) + \
          x(nx)))/((x(-4 + nx) - x(-2 + nx))*(-x(-3 + nx) + x(-2 + nx))*(x(-2 + \
          nx) - x(-1 + nx))*(x(-2 + nx) - x(nx))) + (f(nx,j)*(x(nx)*((3*x(-1 + \
          nx) - 4*x(nx))*x(nx) + x(-2 + nx)*(-2*x(-1 + nx) + 3*x(nx))) + x(-3 + \
          nx)*(x(-2 + nx)*(x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + \
          3*x(nx))) + x(-4 + nx)*(x(-2 + nx)*(x(-1 + nx) - 2*x(nx)) + x(-3 + \
          nx)*(x(-2 + nx) + x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + \
          3*x(nx)))))/((-x(-2 + nx) + x(nx))*(-x(-1 + nx) + x(nx))))/((x(-3 + \
          nx) - x(nx))*(-x(-4 + nx) + x(nx)))
       end forall
    case(2)
       ! second derivatives
       forall( j = 1 : m % nf, &
            k = 3 : m % nx - 2 )

          m%df(k,j,i) = \
          (-2*f(2 + k,j)*(x(k)*(3*x(k) - 2*x(1 + k)) + x(-1 + k)*(-2*x(k) + x(1 \
          + k)) + x(-2 + k)*(x(-1 + k) - 2*x(k) + x(1 + k))))/((x(-2 + k) - x(2 \
          + k))*(-x(-1 + k) + x(2 + k))*(-x(k) + x(2 + k))*(-x(1 + k) + x(2 + \
          k))) - (2*f(1 + k,j)*(x(k)*(3*x(k) - 2*x(2 + k)) + x(-1 + k)*(-2*x(k) \
          + x(2 + k)) + x(-2 + k)*(x(-1 + k) - 2*x(k) + x(2 + k))))/((x(-2 + k) \
          - x(1 + k))*(-x(-1 + k) + x(1 + k))*(-x(k) + x(1 + k))*(x(1 + k) - \
          x(2 + k))) + (2*f(-1 + k,j)*(-3*x(k)**2 + x(-2 + k)*(2*x(k) - x(1 + \
          k) - x(2 + k)) - x(1 + k)*x(2 + k) + 2*x(k)*(x(1 + k) + x(2 + \
          k))))/((x(-2 + k) - x(-1 + k))*(x(-1 + k) - x(k))*(x(-1 + k) - x(1 + \
          k))*(x(-1 + k) - x(2 + k))) - (2*f(-2 + k,j)*(-3*x(k)**2 + x(-1 + \
          k)*(2*x(k) - x(1 + k) - x(2 + k)) - x(1 + k)*x(2 + k) + 2*x(k)*(x(1 + \
          k) + x(2 + k))))/((x(-2 + k) - x(-1 + k))*(x(-2 + k) - x(k))*(x(-2 + \
          k) - x(1 + k))*(x(-2 + k) - x(2 + k))) - (2*f(k,j)*(6*x(k)**2 - \
          3*x(k)*x(1 + k) - 3*x(k)*x(2 + k) + x(1 + k)*x(2 + k) + x(-1 + \
          k)*(-3*x(k) + x(1 + k) + x(2 + k)) + x(-2 + k)*(x(-1 + k) - 3*x(k) + \
          x(1 + k) + x(2 + k))))/((x(-2 + k) - x(k))*(-x(-1 + k) + x(k))*(x(k) \
          - x(1 + k))*(x(k) - x(2 + k)))

       end forall

       forall( j = 1 : m % nf )

             m%df(1,   j,i) = \
             (-2*f(5,j)*(3*x(1)**2 + x(3)*x(4) + x(2)*(x(3) + x(4)) - 2*x(1)*(x(2) \
             + x(3) + x(4))))/((x(1) - x(5))*(-x(2) + x(5))*(-x(3) + x(5))*(-x(4) \
             + x(5))) - (2*f(4,j)*(3*x(1)**2 + x(3)*x(5) + x(2)*(x(3) + x(5)) - \
             2*x(1)*(x(2) + x(3) + x(5))))/((x(1) - x(4))*(-x(2) + x(4))*(-x(3) + \
             x(4))*(x(4) - x(5))) - (2*f(3,j)*(3*x(1)**2 + x(4)*x(5) + x(2)*(x(4) \
             + x(5)) - 2*x(1)*(x(2) + x(4) + x(5))))/((x(1) - x(3))*(-x(2) + \
             x(3))*(x(3) - x(4))*(x(3) - x(5))) - (2*f(2,j)*(3*x(1)**2 + x(4)*x(5) \
             + x(3)*(x(4) + x(5)) - 2*x(1)*(x(3) + x(4) + x(5))))/((x(1) - \
             x(2))*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))) + \
             (2*f(1,j)*(6*x(1)**2 + x(3)*x(4) + x(3)*x(5) + x(4)*x(5) + x(2)*(x(3) \
             + x(4) + x(5)) - 3*x(1)*(x(2) + x(3) + x(4) + x(5))))/((x(1) - \
             x(2))*(x(1) - x(3))*(x(1) - x(4))*(x(1) - x(5)))

             m%df(2,   j,i) = \
             2*(-((f(5,j)*(3*x(2)**2 + x(3)*x(4) - 2*x(2)*(x(3) + x(4)) + \
             x(1)*(-2*x(2) + x(3) + x(4))))/((x(1) - x(5))*(-x(2) + x(5))*(-x(3) + \
             x(5))*(-x(4) + x(5)))) + (f(4,j)*(-3*x(2)**2 + x(1)*(2*x(2) - x(3) - \
             x(5)) - x(3)*x(5) + 2*x(2)*(x(3) + x(5))))/((x(1) - x(4))*(-x(2) + \
             x(4))*(-x(3) + x(4))*(x(4) - x(5))) + (f(3,j)*(-3*x(2)**2 + \
             x(1)*(2*x(2) - x(4) - x(5)) - x(4)*x(5) + 2*x(2)*(x(4) + \
             x(5))))/((x(1) - x(3))*(-x(2) + x(3))*(x(3) - x(4))*(x(3) - x(5))) + \
             (f(1,j)*(3*x(2)**2 + x(4)*x(5) + x(3)*(x(4) + x(5)) - 2*x(2)*(x(3) + \
             x(4) + x(5))))/((x(1) - x(2))*(x(1) - x(3))*(x(1) - x(4))*(x(1) - \
             x(5))) + (f(2,j)*(-6*x(2)**2 - x(3)*x(4) + x(1)*(3*x(2) - x(3) - x(4) \
             - x(5)) - x(3)*x(5) - x(4)*x(5) + 3*x(2)*(x(3) + x(4) + \
             x(5))))/((x(1) - x(2))*(x(2) - x(3))*(x(2) - x(4))*(x(2) - x(5))))
       end forall


       forall( j = 1 : m % nf )
             m%df(nx-1,j,i) = \
          (-2*f(nx,j)*(x(-3 + nx)*(x(-2 + nx) - 2*x(-1 + nx)) + x(-4 + \
          nx)*(x(-3 + nx) + x(-2 + nx) - 2*x(-1 + nx)) + x(-1 + nx)*(-2*x(-2 + \
          nx) + 3*x(-1 + nx))))/((x(-4 + nx) - x(nx))*(-x(-3 + nx) + \
          x(nx))*(-x(-2 + nx) + x(nx))*(-x(-1 + nx) + x(nx))) - (2*f(-1 + \
          nx,j)*(-3*x(-2 + nx)*x(-1 + nx) + 6*x(-1 + nx)**2 + x(-2 + nx)*x(nx) \
          - 3*x(-1 + nx)*x(nx) + x(-3 + nx)*(x(-2 + nx) - 3*x(-1 + nx) + x(nx)) \
          + x(-4 + nx)*(x(-3 + nx) + x(-2 + nx) - 3*x(-1 + nx) + \
          x(nx))))/((x(-4 + nx) - x(-1 + nx))*(-x(-3 + nx) + x(-1 + nx))*(-x(-2 \
          + nx) + x(-1 + nx))*(x(-1 + nx) - x(nx))) - (2*f(-2 + nx,j)*(x(-1 + \
          nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-3 + nx)*(-2*x(-1 + nx) + x(nx)) + \
          x(-4 + nx)*(x(-3 + nx) - 2*x(-1 + nx) + x(nx))))/((x(-4 + nx) - x(-2 \
          + nx))*(-x(-3 + nx) + x(-2 + nx))*(x(-2 + nx) - x(-1 + nx))*(x(-2 + \
          nx) - x(nx))) - (2*f(-3 + nx,j)*(x(-1 + nx)*(3*x(-1 + nx) - 2*x(nx)) \
          + x(-2 + nx)*(-2*x(-1 + nx) + x(nx)) + x(-4 + nx)*(x(-2 + nx) - \
          2*x(-1 + nx) + x(nx))))/((x(-4 + nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 \
          + nx))*(x(-3 + nx) - x(-1 + nx))*(x(-3 + nx) - x(nx))) + (2*f(-4 + \
          nx,j)*(x(-1 + nx)*(3*x(-1 + nx) - 2*x(nx)) + x(-2 + nx)*(-2*x(-1 + \
          nx) + x(nx)) + x(-3 + nx)*(x(-2 + nx) - 2*x(-1 + nx) + \
          x(nx))))/((x(-4 + nx) - x(-3 + nx))*(x(-4 + nx) - x(-2 + nx))*(x(-4 + \
          nx) - x(-1 + nx))*(x(-4 + nx) - x(nx)))

          m%df(nx  ,j,i) = \
          (-2*f(nx,j)*(x(-2 + nx)*x(-1 + nx) + x(-3 + nx)*(x(-2 + nx) + x(-1 + \
          nx) - 3*x(nx)) + x(-4 + nx)*(x(-3 + nx) + x(-2 + nx) + x(-1 + nx) - \
          3*x(nx)) - 3*x(-2 + nx)*x(nx) - 3*x(-1 + nx)*x(nx) + \
          6*x(nx)**2))/((x(-4 + nx) - x(nx))*(-x(-3 + nx) + x(nx))*(-x(-2 + nx) \
          + x(nx))*(-x(-1 + nx) + x(nx))) - (2*f(-1 + nx,j)*(x(-3 + nx)*(x(-2 + \
          nx) - 2*x(nx)) + x(-4 + nx)*(x(-3 + nx) + x(-2 + nx) - 2*x(nx)) + \
          x(nx)*(-2*x(-2 + nx) + 3*x(nx))))/((x(-4 + nx) - x(-1 + nx))*(-x(-3 + \
          nx) + x(-1 + nx))*(-x(-2 + nx) + x(-1 + nx))*(x(-1 + nx) - x(nx))) - \
          (2*f(-2 + nx,j)*(x(-3 + nx)*(x(-1 + nx) - 2*x(nx)) + x(-4 + nx)*(x(-3 \
          + nx) + x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + \
          3*x(nx))))/((x(-4 + nx) - x(-2 + nx))*(-x(-3 + nx) + x(-2 + \
          nx))*(x(-2 + nx) - x(-1 + nx))*(x(-2 + nx) - x(nx))) - (2*f(-3 + \
          nx,j)*(x(-2 + nx)*(x(-1 + nx) - 2*x(nx)) + x(-4 + nx)*(x(-2 + nx) + \
          x(-1 + nx) - 2*x(nx)) + x(nx)*(-2*x(-1 + nx) + 3*x(nx))))/((x(-4 + \
          nx) - x(-3 + nx))*(x(-3 + nx) - x(-2 + nx))*(x(-3 + nx) - x(-1 + \
          nx))*(x(-3 + nx) - x(nx))) + (2*f(-4 + nx,j)*(x(-2 + nx)*(x(-1 + nx) \
          - 2*x(nx)) + x(-3 + nx)*(x(-2 + nx) + x(-1 + nx) - 2*x(nx)) + \
          x(nx)*(-2*x(-1 + nx) + 3*x(nx))))/((x(-4 + nx) - x(-3 + nx))*(x(-4 + \
          nx) - x(-2 + nx))*(x(-4 + nx) - x(-1 + nx))*(x(-4 + nx) - x(nx)))
       end forall
    case default
       ! @todo error
       print *, trim(m%name), " cannot calculate derivatives higher than 2"
    end select


  end subroutine calculate_derivatives


end module class_mesh_afd5pt
