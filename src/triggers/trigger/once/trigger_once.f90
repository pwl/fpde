module class_trigger_once

  use class_trigger

  private

  type, public, extends(trigger) :: trigger_once
     ! initialization data goes here
     integer   :: n_times = 1
     ! end of initialization data
   contains
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: test
     procedure :: init
  end type trigger_once

contains

  function init(this) result(r)
    class(trigger_once) :: this
    logical :: r

    ! call common initialization for triggers
    if( this % trigger % init() ) then
       ! we proceede with trigger-specific initialization. For this
       ! particular trigger there is no data to initialize other than
       ! test_result and name, so we return true
       this % name = "trigger_once"
       r = .true.
    else
       ! if init has failed we return false
       r = .false.
    end if

  end function init

  function start(t) result(r)
    class(trigger_once) :: t
    logical :: r
    r = .true.
  end function start

  function stop(t) result(r)
    class(trigger_once) :: t
    logical :: r
    r = .true.
  end function stop

  function test(t) result(r)
    class(trigger_once), target :: t
    logical :: r
    if( t % n_times > 0 ) then
       t % n_times = t % n_times - 1
       r = .true.
    else
       r = .false.
    end if

  end function test

  subroutine info(t)
    class(trigger_once) :: t
    ! print *, "I:trigger%test_result=", t%test_result
  end subroutine info

end module class_trigger_once
