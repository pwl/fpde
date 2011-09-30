module class_a

  type :: a
     procedure(f_interface), pointer :: f => null()
     real :: a = 0.
  end type a

  abstract interface
     subroutine f_interface( arg )
       import :: a
       class(a) :: arg
     end subroutine f_interface
  end interface

end module class_a

program test

  use class_a

  type(a) :: aa
  aa = a( f = null() )

end program test

