program test_inverse_green
  use utils_greens

  integer, parameter :: nx = 30
  real :: array(nx,nx)
  real :: L = 3.
  integer :: i,j

  call discrete_greens(array,L)

  do i = 1, nx
     do j = 1, nx
        write(*,"(i3,i3,f20.10)") i, j, array(i,j)
     end do
     print *, ""
  end do

end program test_inverse_green
