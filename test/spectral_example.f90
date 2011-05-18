program mesh_spectral_test
  ! first use the module with a desired mesh type
  use module_mesh_spectral
  integer :: i
  ! declare a mesh of a given type
  type(mesh_spectral) :: m
  ! 128 points, two functions, first derivative, mesh from 0.0 to 1.0
  call m % init(128,2,1,0.0,1.0)
  ! have FFT + capability of boundary neuman, keep coeffs in table coeffs
  call m % tweak(2,.true.)
  
  call m % info
  
  m % f(:,1) = sin(m % x)
  
  call m % calculate_derivatives(1)
 print *, ' x dsin/dx cos(x) delta( dsin/dx - cos(x)) / cos(x)'
  do i=1,128
   print *, m%x(i) , m%df(i,1,1), cos(m%x(i)),(m%df(i,1,1)- cos(m%x(i)))/cos(m%x(i))
  end do 
 print *, 'coeffs'
  do i=1,2*128-2
   print *, i,m%coeffs(i,1)
  end do 
 
  ! free when done with the mesh
  call m % free

end program mesh_spectral_test
