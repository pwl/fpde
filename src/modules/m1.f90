module points

  type :: point
     real :: x, y
  end type point

  type, extends(point) :: color_point
     integer :: color
  end type color_point

  type(color_point) :: p1

contains

  subroutine test()

    p1 = color_point(point=point(1.,2.), color = 1)

  end subroutine test

end module points
