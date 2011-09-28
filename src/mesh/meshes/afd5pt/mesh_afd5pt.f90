module class_mesh_afd5pt
  use class_mesh
  use utils_random_seed

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

  subroutine init(m, nx, nf, maxrk, xmin, xmax)
    class(mesh_afd5pt), intent(inout) :: m
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i,j
    real :: pi

    pi = acos(-1.)

    call m % mesh % init( nx, nf, maxrk, xmin, xmax)

    m % name = "mesh_afd5pt"

    ! setup a sample non uniform grid
    forall( i = 1:nx )&
         m % x(i) = (xmax-xmin)*(1.-cos(real(i-1)/real(nx-1)*pi))/2.+xmin

  end subroutine init

  function derivative( m, i, j, k )
    class(mesh_afd5pt), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: derivative

    if( .not. m%check_derivatives(i) ) then
       return
    end if

    call m % calculate_derivatives( k )
    derivative = m % df( i, j, k )

  end function derivative

  recursive subroutine calculate_derivatives( m, i )
    class(mesh_afd5pt), target, intent(inout) :: m
    integer, intent(in) :: i
    integer :: j,k,nx
    real, pointer :: f(:,:), x(:)

    f => m % f
    x => m % x
    nx = m % nx

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