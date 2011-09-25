module module_heterogenic_list

  type :: beba
   contains
     procedure :: info
  end type beba

  type, extends(beba) :: buba
   contains
     procedure :: info => info_buba
  end type buba

  type, extends(beba) :: baba
   contains
     procedure :: info => info_baba
  end type baba

  contains
  subroutine info(this)
    class(beba) :: this
    print *, "beba"
  end subroutine info

  subroutine info_buba(this)
    class(buba) :: this
    print *, "buba"
  end subroutine info_buba

  subroutine info_baba(this)
    class(baba) :: this
    print *, "baba"
  end subroutine info_baba

end module module_heterogenic_list

program heterogenic_list
  use class_list
  use module_heterogenic_list

  type(list), pointer :: l
  type(list), pointer :: lp
  class(*), pointer :: bu, ba, be
  class(*), pointer :: taken

  allocate( l )

  allocate(buba :: bu)
  allocate(baba :: ba)
  allocate(beba :: be)

  print *, l % length()
  call l % add( bu )
  print *, l % length()
  call l % add( ba )
  print *, l % length()
  call l % add( be )
  print *, l % length()
  call l % map(interpret_as_beba)

  taken => l % take(1)
  print *, associated(taken)
  taken => l % take(2)
  taken => l % take(3)

contains

  subroutine interpret_as_beba(this)
    class(beba) :: this
    call this % info
  end subroutine interpret_as_beba

end program heterogenic_list


