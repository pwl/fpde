program array_test
  integer, parameter :: n = 10
  real               :: array(n,n)
  

  call f(n,array)

end program array_test

subroutine f(n,a)
  integer :: n
  real :: a(n*n)

  print *, a

end subroutine f
