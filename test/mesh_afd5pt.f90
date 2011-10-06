program test_afd5pt
  use class_mesh_afd5pt

  type(mesh_afd5pt) :: m
  integer, parameter :: nx = 50
  integer :: i
  real :: x(nx), f(nx), df(nx,1,2)

  x=[(real(i-1)/real(nx-1),i=1,nx)]
  ! x = x**2                      !non-uniform mesh

  f(:) = sin(x)

  call m % init(nx,1,2,0.,1.)

  m % x = x
  m % f(:,1) = f

  call m % info
  call m % calculate_derivatives(1)
  call m % calculate_derivatives(2)

  do i = 1, nx
     df(i,1,1) = m % derivative(i,1,1)
     df(i,1,2) = m % derivative(i,1,2)
  end do

  print *,"Two numbers below should be zero"
  do i = 1, 2
     print *, m % integrate( df(:,1,i) - m % df(:,1,i) )
  end do

  print *, abs(df(:,1,1)-cos(x))

  print *, "L1 norm of |df-cos(x)|"
  print *,m % integrate( abs(df(:,1,1) - cos(x)) )
  print *,maxloc(abs(df(:,1,1) - cos(x)) )
  print *, "L1 norm of |d2f+sin(x)|"
  print *,m % integrate( abs(df(:,1,2) + sin(x)) )
  print *,maxloc(abs(df(:,1,1) - cos(x)) )
  print *, "L2 norm of df-cos(x)"
  print *,m % integrate( (df(:,1,1) - cos(x))**2 )
  print *, "L2 norm of d2f+sin(x)"
  print *,m % integrate( (df(:,1,2) + sin(x))**2 )

  call m % free


end program test_afd5pt
