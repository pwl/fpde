program test_afd5pt
  use class_mesh
  use class_mesh_afd5pt

  type(mesh_afd5pt) :: m
  integer, parameter :: nx = 101
  integer :: i
  real :: x(nx), f(nx), df(nx,1,2)
  character(len=30), target :: fixed(1) = [mesh_boundary_fixed]
  character(len=30), target :: von_neumann(1) = [mesh_boundary_von_neumann]

  x=[(real(i-1)/real(nx-1),i=1,nx)]
  ! x = x**2                      !non-uniform mesh

  f(:) = sin(x)

  m = mesh_afd5pt(nx = nx, rk = 2, x1 = 1.)
  call m % init

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

  ! check the von_neumann boundary conditions
  m % boundary_left => von_neumann
  m % f(:,1) = cos(x)

  call m % calculate_derivatives(1)
  call m % calculate_derivatives(2)

  print *, abs(df(:,1,1)-cos(x))

  print *, "L1 norm of |df+sin(x)|"
  print *,m % integrate( abs(df(:,1,1) + sin(x)) )
  print *,maxloc(abs(df(:,1,1) + sin(x)) )
  print *, "L1 norm of |d2f+cos(x)|"
  print *,m % integrate( abs(df(:,1,2) + cos(x)) )
  print *,maxloc(abs(df(:,1,1) - cos(x)) )
  print *, "L2 norm of df+sin(x)"
  print *,m % integrate( (df(:,1,1) + sin(x))**2 )
  print *, "L2 norm of d2f+cos(x)"
  print *,m % integrate( (df(:,1,2) + cos(x))**2 )

  ! step function check
  m % boundary_left => fixed
  m % f(1:nx/2,1) = 0.
  m % f(nx/2+1:nx,1) = 1.

  do i = 1, nx
     df(i,1,1) = m % derivative(i,1,1)
     df(i,1,2) = m % derivative(i,1,2)
  end do


  print *, abs(df(:,1,1))

  print *, "L1 norm of |df|"
  print *,m % integrate( abs(df(:,1,1)) )
  print *,maxloc(abs(df(:,1,1)))
  print *, "L1 norm of |d2f|"
  print *,m % integrate( abs(df(:,1,2)) )
  print *,maxloc(abs(df(:,1,1)) )
  print *, "L2 norm of df"
  print *,m % integrate( (df(:,1,1))**2 )
  print *, "L2 norm of d2f"
  print *,m % integrate( (df(:,1,2))**2 )



  call m % free


end program test_afd5pt
