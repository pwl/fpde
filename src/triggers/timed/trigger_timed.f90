module class_trigger_timed

  use class_solver_data
  use class_trigger

  private

  type, public, extends(trigger) :: trigger_timed
     ! initialization parameters
     real :: dt = 1.
     ! end of initialization parameters
     real :: last_run = 0.
   contains
     ! procedure :: info
     procedure :: test
     procedure :: init
     procedure :: start
  end type trigger_timed

contains

  function init(this) result(r)
    class(trigger_timed) :: this
    logical :: r
    r = this % trigger % init()

    if( r ) then
       this % name = "trigger_timed"
    end if

  end function init

  function start(t) result(r)
    class(trigger_timed) :: t
    logical :: r
    r = .true.

    ! set last_run to starting time of the solver
    t % last_run = t % solver_data % t
  end function start

  function test(t) result(r)
    class(trigger_timed), target :: t
    logical :: r
    real, pointer :: dt, l, st
    dt => t % dt
    l => t % last_run
    st => t % solver_data % t
    r = .false.

    if ( st > l + dt ) then
       l = st
       r = .true.
    end if

  end function test

end module class_trigger_timed
