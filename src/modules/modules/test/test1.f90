module class_module_test1
  use class_module

  private

  type, public, extends(module) :: module_test1
   contains
     procedure :: start
     ! procedure :: stop
     ! procedure :: step
  end type module_test1

  public module_test1_init

contains
  subroutine start(this)
    class(module_test1) :: this
    this % name = "test1_started"
  end subroutine start

  function module_test1_init() result(m)
    type(module_test1), pointer :: m
    allocate(m)
    m % name = "test1"
  end function module_test1_init

end module class_module_test1
