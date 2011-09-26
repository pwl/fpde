module class_module
  use class_initializable
  use class_solver_data
  use class_trigger
  use class_trigger_bundle

  private

  character(len=30), public, parameter :: &
       module_started = "started",&
       module_stopped = "stopped",&
       module_error   = "error"

  type, public, extends(initializable) :: module
     character(len=30) :: state = module_stopped
     class(solver_data), pointer :: solver_data => null()
     type(trigger_bundle), pointer :: triggers => null()
     character(len=30) :: name => ""
     ! character(len=30) :: state => module_stopped
     integer :: steps_made = 0
   contains
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: step
     procedure :: init
     procedure :: free
     procedure :: add => add_trigger
  end type module

  public&
       module_start,&
       module_stop,&
       module_step,&
       module_info

contains

  recursive subroutine module_start(m)
    class(module) :: m

    select case(m % state)
    case( module_stopped )
       if( m % start() ) then
          call m % triggers % start
          m % state = module_started
       else
          m % state = module_error
          call module_start(m)
       end if
    case( module_started )
       ! @todo produce a warning?
    case( module_error )
       ! @todo produce an error
    case default
       m % state = module_error
       call module_start(m)
    end select

  end subroutine module_start

  recursive subroutine module_stop(m)
    class(module) :: m

    select case(m % state)
    case( module_stopped )
       ! @todo produce a warning?
    case( module_started )
       if( m % stop() ) then
          m % state = module_stopped
          call m % triggers % stop
       else
          ! @todo report error
          m % state = module_error
          call module_stop(m)
       end if
    case( module_error )
       ! @todo produce an error
    case default
       m % state = module_error
       call module_stop(m)
    end select

  end subroutine module_stop

  recursive subroutine module_step(m)
    class(module) :: m

    select case(m % state)
    case( module_stopped )
       ! ignore, @todo maybe produce a warning
    case( module_started )
       if( m % triggers % test() ) then
          if( m % step() ) then
             m % steps_made = m % steps_made + 1
          else
             m % state = module_error
             ! @todo error! step failed to execute
          end if
       else
          ! ignore, triggers haven't been executed
       end if
    case( module_error )
       ! ignore
    case default
       m % state = module_error
       call module_step(m)
    end select

  end subroutine module_step

  recursive subroutine module_info(m)
    class(module) :: m

    print *, "I:module % name  = ", m % name
    print *, "I:module % state = ", m % state
    print *, "I: # of module % triggers = ", m % triggers % n_triggers

    select case(m % state)
    case( module_stopped, module_started )
       call m % info
       call m % triggers % info
    case( module_error )
       print *, "info error"
       ! do nothing
    case default
       print *, "info default"
       ! do nothing
    end select

  end subroutine module_info


  ! internal procedures to be overloaded in extensions of module
  function start( this ) result(r)
    class(module) :: this
    logical :: r
    r = .true.
  end function start

  function stop( this ) result(r)
    class(module) :: this
    logical :: r
    r = .true.
  end function stop

  function step( this ) result(r)
    class(module):: this
    logical :: r
    r = .true.
  end function step

  subroutine info( this )
    class(module):: this
  end subroutine info

  subroutine free( this )
    class(module):: this
  end subroutine free

  function init( this ) result(r)
    class(module):: this
    logical :: r

    allocate(this % triggers)
    call this % triggers % init
    this % state = module_stopped

    r = .true.
  end function init

  ! a wrapper function to try_init
  function try_init(this) result(r)
    class(module) :: this
    logical :: r

    r = this % initializable % try_init()

    if( r ) then
       return
    else
       print *, "E: module ", trim(this % name), " failed to initialize"
    end if

  end function try_init

  ! internal procedures, not to be overloaded

  subroutine add_trigger( this, tr )
    class(module) :: this
    class(trigger) :: tr

    if( this % try_init() ) then
       call this % triggers % add( tr )
       tr % solver_data => this % solver_data
    else
       ! @todo report an error
       this % state = module_error
    end if

  end subroutine add_trigger

  ! end of internal procedures

end module class_module

