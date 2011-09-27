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
  type, extends(aa) :: aaa
  end type aaa
end module class_aaa


module class_b
  use class_a
  type :: b
     class(a), pointer :: ap
  end type b
end module class_b

module class_bb
  use class_b
  type, extends(b) :: bb
  end type bb
end module class_bb


program inheritance_test
  use class_aaa
  use class_bb

  class(aa), pointer :: aac
  type(bb) :: bbc


  allocate(aaa :: aac)
  allocate(aac % t)

  aac % t = 999.
  print *, aac % t

  aac % a = a()

  print *, aac % t

contains
  subroutine test(aac,bc)
    use class_aa
    use class_b
    class(aa), target :: aac
    class(b) :: bc

    bc % ap => aac % a

  end subroutine test

end program inheritance_test
