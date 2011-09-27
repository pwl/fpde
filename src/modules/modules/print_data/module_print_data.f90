module class_module_print_data

  use system_functions
  use class_module

  private

  type, public, extends(module) :: module_print_data
     ! initialization parameters
     character(len=1000) :: file_name = ""
     ! end of initialization parameters
     integer :: file_handle = -1
   contains
     procedure :: init
     procedure :: start
     procedure :: stop
     procedure :: step
  end type module_print_data

contains

  function start(this) result(r)
    class(module_print_data) :: this
    logical :: r

    call new_directory(this % file_name)

    open( newunit = this % file_handle,&
          file    = this % file_name,  &
          status  = 'unknown')

    r = .true.
  end function start

  function step(this) result(r)
    class(module_print_data) :: this
    logical :: r
    r = .true.

    write( this % file_handle, *) this % file_handle

  end function step

  function stop(this) result(r)
    class(module_print_data) :: this
    logical :: r

    close( this % file_handle )

    r = .true.
  end function stop

  function init(this) result(r)
    class(module_print_data) :: this
    logical :: r
    character(len=1000) :: test
    r = this % module % init()

    if(r) then

       if( .not. this % named() ) then
          this % name = "module_print_data"
       end if

       if( trim(this%file_name) == "" ) then
       !    ! should generate file name automatically as follows
       !    ! [date]/[solver%name]/[module_print_data%name + some unique
       !    ! module%id]/
          write(this%file_name, *) &
               ! solver run time
               trim(this%solver_data%time_started), "/", &
               ! solver name
               trim(this%solver_data%name), "/",&
               ! module name
               trim(this%name), "/", &
               ! actual file name
               "data"
       end if

    end if

  end function init



end module class_module_print_data
