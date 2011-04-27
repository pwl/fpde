module rhs_mod

  use derivative_mod

  interface

     subroutine swap(a,b)
       real,dimension(:) :: a, b
       real,dimension(size(a)) :: c
     end subroutine swap

     subroutine rhs(f,df,tau)
       real,intent(in) :: f(:),tau
       real,intent(out) :: df(size(f))
     end subroutine rhs

  end interface

contains

end module rhs_mod

subroutine rhs(f,df,tau)
  ! parameters
  real,intent(in) :: f(:),tau
  real,intent(out) :: df(size(f))

  ! user defined variables
  integer                       :: n   ! number of mesh points
  real,dimension((size(f)-1)/2) :: u,x,du ! function and mesh points
  real                          :: t   ! time

  n = (size(f)-1)/2
  t = f(1)
  x = f(2:n)
  u = f(n+1:)

  call d1(u,x,du)
  ! df(n+1:) = du * tau
  df = 0.

end subroutine rhs

subroutine swap(a,b)
  real,dimension(:) :: a, b
  real,dimension(size(a)) :: c

  c = a
  a = b
  b = c
end subroutine swap
