module class_module_test2
  use class_module

  private

  type, public, extends(module) :: module_test2
   contains
     procedure :: start
     ! procedure :: stop
     procedure :: step
  end type module_test2

  public module_test2_init

contains
  function start(this) result(r)
    class(module_test2) :: this
    logical :: r
    r = .true.
    this % name = "test2_started"
  end function start

  function step(this) result(r)
    class(module_test2) :: this
    logical :: r
    r = .true.
    print *, trim(this % name), " % step()"
  end function step

  function module_test2_init(name) result(m)
    type(module_test2), pointer :: m
    character(*), optional :: name
    allocate(m)
    call m % init

    if(present(name)) then
       m % name = name
    else
       m % name = "test2"
    end if

  end function module_test2_init

end module class_module_test2
