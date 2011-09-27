module class_module_test
  use class_module

  private

  type, public, extends(module) :: module_test
   contains
     procedure :: start
     procedure :: step
     procedure :: init
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
    print *, "   calling ", trim(this % name), " % step()"
    print *, "   t = ", this % solver_data % t
  end function step

  function init(this) result(r)
    class(module_test) :: this
    logical :: r
    r = this % module % init()

    if(r) then
       if( .not. this % named() ) then
          this % name = "module_test"
       end if
    end if

    ! no further initialization needed, this is only a test module
  end function init


end module class_module_test
