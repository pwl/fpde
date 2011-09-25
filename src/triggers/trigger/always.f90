module class_trigger_always

  use class_trigger

  private

  type, public, extends(trigger) :: trigger_always
     logical :: test_result
   contains
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: test
     ! procedure :: free
  end type trigger_always

  public :: trigger_always_init

contains

  function trigger_always_init(test_result) result(t)
    class(trigger_always), pointer :: t
    logical, optional :: test_result
    allocate(t)
    call t % init

    if( present(test_result) ) then
       t % test_result = test_result
    else
       ! test result defaults to .true.
       t % test_result = .true.
    end if

    t % name = "trigger_always"
  end function trigger_always_init

  function start(t) result(r)
    class(trigger_always) :: t
    logical :: r
    r = .true.
  end function start

  function stop(t) result(r)
    class(trigger_always) :: t
    logical :: r
    r = .true.
  end function stop

  function test(t) result(r)
    class(trigger_always) :: t
    logical :: r
    r = t % test_result
  end function test

  subroutine info(t)
    class(trigger_always) :: t
    print *, "I:trigger%test_result=", t%test_result
  end subroutine info


end module class_trigger_always

