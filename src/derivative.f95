module derivative_mod

  interface
     subroutine d1(f,x,df)
       real,intent(in) :: f(:)
       real,intent(in) :: x(size(f))
       real,intent(out) :: df(size(f))
     end subroutine d1
  end interface

end module derivative_mod

subroutine d1(f,x,df)
  ! interface
  real,intent(in) :: f(:)
  real,intent(in) :: x(size(f))
  real,intent(out) :: df(size(f))

  ! additional parameters
  integer :: n,i
  n = size(f)

  forall( i = 2 : n-1 )
     df(i) = (f(i+1)-f(i-1))/(x(i+1)-x(i-1))
  end forall

  df(1) = 0
  df(n) = 0
end subroutine d1
