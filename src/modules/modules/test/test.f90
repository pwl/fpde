module class_module_test
  use class_module

  private

  type, public, extends(module) :: module_test
   contains
     procedure :: start
     ! procedure :: stop
     procedure :: step
  end type module_test

contains
  function start(this) result(r)
    class(module_test) :: this
    logical :: r
    r = .true.
  end function start

  function step(this) result(r)
    class(module_test) :: this
    logical :: r
    r = .true.
    print *, "calling ", trim(this % name), " % step()"
  end function step

  function init(m) result(r)
    class(module_test) :: m
    logical :: r
    r = m % module % init()
    ! no further initialization needed, this is only a test module

  end function init


end module class_module_test
