module class_module_test2
  use class_module

  private

  type, public, extends(module) :: module_test2
   contains
     procedure :: start
     ! procedure :: stop
     ! procedure :: step
  end type module_test2

  public module_test2_init

contains
  subroutine start(this)
    class(module_test2) :: this
    this % name = "test2_started"
  end subroutine start

  function module_test2_init() result(m)
    type(module_test2), pointer :: m
    allocate(m)
    m % name = "test2"
  end function module_test2_init

end module class_module_test2
