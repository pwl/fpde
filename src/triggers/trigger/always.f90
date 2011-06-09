module class_trigger_always

  use class_trigger

  private

  type, public, extends(trigger) :: trigger_always
     character(len=300) :: buba
   contains
     procedure :: info
     ! procedure :: start
     ! procedure :: stop
     ! procedure :: test
     ! procedure :: free
  end type trigger_always

  public :: class_trigger_always_init

contains

  function class_trigger_always_init() result(t)
    type(trigger_always), pointer :: t
    allocate(t)

    t % buba = "buba"
  end function class_trigger_always_init

  subroutine info(t)
    class(trigger_always) :: t
    print *, "haha"
    print *, trim(t%buba)
  end subroutine info


end module class_trigger_always

