module class_module_test1
  use class_module

  private

  type, public, extends(module) :: module_test1
   contains
     procedure :: start
     ! procedure :: stop
     procedure :: step
  end type module_test1

  public module_test1_init

contains
  function start(this) result(r)
    class(module_test1) :: this
    logical :: r
    r = .true.
    this % name = "test1_started"
  end function start

  function step(this) result(r)
    class(module_test1) :: this
    logical :: r
    r = .true.
    print *, trim(this % name), " % step()"
  end function step

  function module_test1_init() result(m)
    type(module_test1), pointer :: m
    allocate(m)
    call m % init

    m % name = "test1"
  end function module_test1_init

end module class_module_test1
