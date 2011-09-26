program constructor_test

  type :: a
     real :: x = -1.,y = -2.
  end type a

  type :: b
     integer, pointer :: z
  end type b

  type(a) :: a1
  type(a), pointer :: a2

  a2 = a(x=1.)
  call zz( a() )


contains
  subroutine zz(aa)
    type(a) :: aa
  end subroutine zz

end program constructor_test
