module class_trigger_bundle
  use class_list
  use class_trigger
  use class_solver

  private

  type, public :: trigger_bundle
     class(list), pointer :: triggers
   contains
     procedure :: add
     procedure :: info
     procedure :: init
     procedure :: start
     procedure :: stop
     ! procedure :: test
  end type trigger_bundle

contains

  subroutine init(this)
    class(trigger_bundle) :: this
    allocate(this%triggers)
  end subroutine init

  subroutine add(this, t)
    class(trigger_bundle) :: this
    class(trigger), pointer :: t
    class(*), pointer :: dummy
    dummy => t
    call this % triggers % add(dummy)
  end subroutine add

  subroutine info(this)
    class(trigger_bundle) :: this
    call this % triggers % map(trigger_info)
  end subroutine info

  subroutine start(this)
    class(trigger_bundle) :: this
    call this % triggers % map(trigger_start)
  end subroutine start

  subroutine stop(this)
    class(trigger_bundle) :: this
    call this % triggers % map(trigger_stop)
  end subroutine stop

end module class_trigger_bundle
