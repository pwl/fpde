module class_trigger

  use class_solver_data

  private

  character(len=30), public, parameter :: &
       trigger_started = "started",     &
       trigger_stopped = "stopped",     &
       trigger_error = "error"

  type, public :: trigger
     type(solver_data), pointer :: solver_data => null()
     character(len=30)           :: name = ""
     character(len=30)           :: state = trigger_stopped
     logical :: initialized = .false.
   contains
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: test
     procedure :: free
     procedure :: init
     procedure :: try_init
     procedure :: named
  end type trigger

  ! public interface to this module
  public &
       trigger_info, &
       trigger_start,&
       trigger_stop, &
       trigger_test

  private info, start, stop, test, free, init, try_init

contains

  ! @todo if reporting that an error occured to a trigger do so only
  ! once, don't make any further reports on that trigger. Maybe add
  ! another state trigger_error_reported?
  !
  ! public interface used to change state of the trigger

  recursive subroutine trigger_start(t)
    class(trigger) :: t

    select case(t % state)
    case( trigger_stopped )
       if( t % start() ) then
          ! trigger started succesfully
          t % state = trigger_started
       else
          t % state = trigger_error
          ! call start again to report an error
          call trigger_start(t)
       end if
    case( trigger_started )
       ! trigger has already started, ignore this event
    case( trigger_error )
       ! @todo report error
    case default
       t % state = trigger_error
       call trigger_start(t)
    end select

  end subroutine trigger_start

  ! @todo inconsistency, trigger%stop is a function and trigger_stop
  ! is a subroutine
  recursive subroutine trigger_stop(t)
    class(trigger) :: t

    select case(t % state)
    case( trigger_stopped )
       ! do nothing
    case( trigger_started )
       if( t % stop() ) then
          ! everything went alright
          t % state = trigger_stopped
       else
          ! there was an error
          t % state = trigger_error
          ! run the subroutine again, status changed
          call trigger_stop(t)
       end if
    case( trigger_error )
       ! trigger is broken, doing nothing
       !> @todo report error
    case default
       ! unknown event, error
       t % state = trigger_error
       call trigger_stop(t)
    end select

  end subroutine trigger_stop

  !> calls trigger%test
  !!
  !! @param t
  !!
  !! @return
  !!
  recursive function trigger_test(t) result (r)
    class(trigger) :: t
    logical :: r

    ! return false by default
    r = .false.

    select case(t % state)
    case( trigger_stopped )
       ! do nothing
       return
    case( trigger_started )
       ! test if trigger should be activated
       r = t % test()
       return
    case( trigger_error )
       ! do nothing
       return
    case default
       t % state = trigger_error
       ! check the test result for the error case
       r = trigger_test(t)
       return
    end select

  end function trigger_test

  !> Calls the trigger%free subroutine if overloaded, also frees the
  !> subparts of tirgger class (if there are any). If trigger is
  !> running it tries to stop it before calling free
  !!
  !! @param t
  !!
  !! @return
  !!
  recursive subroutine trigger_free(t)
    class(trigger) :: t

    select case (t % state)
    case(trigger_stopped)
       call t % free
    case(trigger_started)
       call trigger_stop(t)
       call trigger_free(t)
    case(trigger_error)
       ! trigger is broken, do nothing
    case default
       ! unknown state, change state to error and try again
       t % state = trigger_error
       call trigger_free(t)
    end select

  end subroutine trigger_free


  subroutine trigger_info(t)
    class(trigger) :: t

    ! @todo print some general information, than call a
    ! trigger-specific info function
    ! e.g.
    print *, "I:trigger % name = ", t % name
    print *, "I:trigger % state =", t % state

    select case(t % state)
    case(trigger_started, trigger_stopped)
       call t % info
    case(trigger_error)
       ! do nothing
    case default
       ! do nothing
    end select

  end subroutine trigger_info

  ! end of the public interface

  ! this are the skeletons of the functions to be overloaded by
  ! inheriting class. If not overloaded they should do nothing
  function start(t) result(r)
    class(trigger) :: t
    logical :: r

    ! by default trigger starts succesfully if start() is not
    ! overloaded
    r = .true.

  end function start

  function stop(t) result(r)
    class(trigger) :: t
    logical :: r

    ! by default trigger stops succesfully if stop() is not
    ! overloaded
    r = .true.

  end function stop

  function test(t) result(r)
    class(trigger), target :: t
    logical :: r

    ! trigger does not run by default
    r = .false.

  end function test

  subroutine info(t)
    class(trigger) :: t
    print *, t % name
  end subroutine info

  !> Initializes a trigger.
  !!
  function init(this) result(r)
    class(trigger) :: this
    logical :: r
    ! nothing to init in a trigger
    r = .true.
  end function init

  !> returns tries to initialize a class instance
  !!
  !! @param this
  !!
  !! @return
  !!
  function try_init(this) result(r)
    class(trigger) :: this
    logical :: r
    r = .false.

    if( this % initialized ) then
       r = .true.
       ! if module is not in uninitialized state we leave it alone
    else if( this % init() ) then
       this % initialized = .true.
       ! m % init() is not evaluated if one of the previous conditions
       ! holds. Also calling m % init() sets m % initialized to .true.
       r = .true.
    else
       ! @todo error: module failed to initialize
       r = .false.
       print *, "E: trigger ", trim(this % name), " failed to initialize"
    end if
  end function try_init

  ! end of skeletons

  ! theese are functions used to maintain the trigger class
  !> frees the contents of the trigger class
  !!
  !! @param t
  !!
  !! @return
  !!
  subroutine free(t)
    class(trigger) :: t
  end subroutine free

  function named( this ) result(r)
    class(trigger) :: this
    logical :: r

    r = ( trim(this % name) /= "" )

  end function named




end module class_trigger
