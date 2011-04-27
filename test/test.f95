program hello
  use rhs_mod

  integer, parameter :: n = 60000
  integer            :: i
  real               :: x(n) = (/((i-1.)/(n-1.),i=1,n)/)
  real               :: f(n), df(n)

  f=sin(x)

  ! print *, f
  ! print *, x
  ! print *, df

  ! call rhs(f,df,1.)
  ! call swap(f,df)
  call d1(f,x,df)

  print *, sum(df-cos(x))/(n-1.)

end program hello
