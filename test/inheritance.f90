module class_a
  type :: a
     real, pointer :: t => null()
  end type a
end module class_a

module class_aa
  use class_a
  type, extends(a) :: aa
  end type aa
end module class_aa

module class_aaa
  use class_aa
  type, extends(b) :: aaa
  end type aaa
end module class_aaa


module class_c
  use class_a
  type :: c
     class(a), pointer :: ap
  end type c
end module class_c

program inheritance_test
  use class_a
  use class_aa
  use class_c


contains
  subroutine test(b,c)

  end subroutine test

end program inheritance_test
