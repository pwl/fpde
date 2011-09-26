module class_a
  type :: a
     integer :: aa = 1
  end type a
end module class_a

module class_b
  use class_a
  type, extends(a) :: b
     integer :: bb = 2
  end type b
end module class_b

module class_c
  use class_b
  type, extends(b) :: c
     integer :: cc = 3
  end type c
end module class_c

program inheritance_test
  use class_b

  type(b) :: cb
  cb = b()
end program inheritance_test
