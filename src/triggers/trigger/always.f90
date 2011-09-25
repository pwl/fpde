module class_trigger_always

  use class_trigger

  private

  type, public, extends(trigger) :: trigger_always
     character(len=30) :: buba
   contains
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: test
     ! procedure :: free
  end type trigger_always

  public :: trigger_always_init

contains

  function trigger_always_init(buba) result(t)
    class(trigger_always), pointer :: t
    character(*), optional :: buba
    allocate(t)
    call t % init

    if( present(buba) ) then
       t % buba = buba
    else
       t % buba = "no buba!"
    end if

    t % name = "trigger_always"
  end function trigger_always_init

  function start(t) result(r)
    class(trigger_always) :: t
    logical :: r
    ! print *, "started!!!"
    r = .true.
  end function start

  function stop(t) result(r)
    class(trigger_always) :: t
    logical :: r
    ! print *, "stopped!!!"
    r = .true.
  end function stop

  function test(t) result(r)
    class(trigger_always) :: t
    logical :: r
    r = .true.
  end function test

  subroutine info(t)
    class(trigger_always) :: t
    ! print *, "calling info for trigger_always"
    print *, "I:trigger%buba=", t%buba
  end subroutine info


end module class_trigger_always

