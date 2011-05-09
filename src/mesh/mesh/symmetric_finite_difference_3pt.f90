module module_mesh_symmetric_finite_difference_3pt
  use mesh_module
  
  ! everything except the type should be private
  private
  
  type, public, extends( mesh ) :: mesh_symmetric_finite_difference_3pt
     real :: h
   contains
     ! overloaded procedures go here (if needed)
     procedure :: Derivative 
  end type mesh_symmetric_finite_difference_3pt

contains

  subroutine Init(m, nx, nf, maxrk, xmin, xmax)
    class(mesh_symmetric_finite_difference_3pt), intent(inout) :: m
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i

    call m % mesh % Init( nx, nf, maxrk, xmin, xmax)

    m % h = (xmax-xmin)/(nx-1)
    print *, "3pt"
    
    ! setup a uniform grid
    forall( i = 1:nx )&
         m % x(i) = xmin + (i-1) * m % h
  
  end subroutine Init
    
  function Derivative( m, i, j, k )
    class(mesh_symmetric_finite_difference_3pt), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: Derivative

    call m % CacheDerivatives( i )
    Derivative = m % df( i, j, k )
            
  end function Derivative

  subroutine CacheDerivatives( m, i )
    class(mesh_symmetric_finite_difference_3pt), target, intent(inout) :: m
    integer :: i,j,k
    real, pointer :: f(:,:)

    f => m % f

    forall( j = 1 : m % nf, &
            k = 2 : m % nx - 1 )
       m%df(i,j,k)=(f(i,j+1)-f(i,j-1))/m%h/2.
    end forall

    forall( j = 1 : m % nf )
       m%df(i,j,1)=(f(i,2)-f(i,1))/m%h
       m%df(i,j,m%nx)=(f(i,m%nx)-f(i,m%nx-1))/m%h
    end forall
    
  end subroutine CacheDerivatives
  
    
end module module_mesh_symmetric_finite_difference_3pt
