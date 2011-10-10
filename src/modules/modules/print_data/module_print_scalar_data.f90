module class_module_print_scalar_data

  use class_solver_data
  use system_functions
  use class_module

  private

  type, public, extends(module) :: module_print_scalar_data
     ! initialization parameters
     character(len=1000) :: file_name = ""
     character(len=10)   :: extension = ".dat"
     ! end of initialization parameters
     integer :: file_handle = 0
   contains
     procedure :: init
     procedure :: start
     procedure :: stop
     procedure :: step
  end type module_print_scalar_data

contains

  function start(this) result(r)
    class(module_print_scalar_data) :: this
    logical :: r
    class(solver_data), pointer :: s
    character(len=30), pointer :: dsn(:), udsn(:)
    integer :: file, lds, luds, i, iostat
    r = .true.

    s => this % solver_data
    dsn => s % data_scalars_names
    udsn => s % user_data_scalars_names

    ! if no file_name has been specified, one is generated
    if( trim(this%file_name) == "" ) then
       ! should generate file name automatically as follows
       ! [date]/[solver%name]/[module_print_scalar_data%name + some
       ! unique module%id]/
       write(this%file_name, *) &
            ! solver run time
            trim(this%solver_data%time_started), "/", &
            "modules/",&
            ! module name
            trim(this%name), "/", &
            ! actual file name
            "data"
    end if

    call new_directory(this % file_name)


    ! open the file and erase its contents
    open(newunit = file,&
         file    = this % file_name,&
         form    = 'formatted',&
         action  = 'write',&
         recl    = 10000,&
         iostat  = iostat, &
         status  = 'replace')

    if( iostat /= 0  ) then
       ! @todo report error
       print *, "ERROR: module_print_scalar_data: start: ioostat /= 0"
       r = .false.
       return
    end if

    if( file == 0) then
       ! @todo report error
       print *, "ERROR: module_print_scalar_data: start: file_handle == 0"
       r = .false.
       return
    end if


    call s % set_data_lengths(lds=lds,luds=luds)

    ! write the initial line
    write(file, *) "# n_iter   t   dt   ", &
         ! then print the solver data names and user data names
         (dsn(i), i=1,lds), (udsn(i), i=1,luds)

    close(file)

    ! @todo write a header

  end function start

  function step(this) result(r)
    class(module_print_scalar_data) :: this
    class(solver_data), pointer :: s
    logical :: r
    integer :: file, iostat
    real, pointer :: ds(:), uds(:)
    integer :: lds, luds, i

    r = .true.

    s => this % solver_data

    ds => s % data_scalars
    uds => s % user_data_scalars

    open(newunit  = file,&
         file     = this % file_name,&
         form     = 'formatted',&
         action   = 'write',&
         position = 'append',&
         recl     = 10000,&
         iostat   = iostat, &
         status   = 'old')

    if( iostat /= 0  ) then
       ! @todo report error
       print *, "ERROR: module_print_scalar_data: step: ioostat /= 0"
       r = .false.
       return
    end if

    if( file == 0) then
       ! @todo report error
       print *, "ERROR: module_print_scalar_data: step: file_handle == 0"
       r = .false.
       return
    end if

    call s % set_data_lengths(lds=lds,luds=luds)

    ! write the actual data
    write(file,*) s%n_iter, s%t, s%dt,&
         (ds(i), i=1, lds), (uds(i), i=1, luds)


    close(file)

  end function step

  function stop(this) result(r)
    class(module_print_scalar_data) :: this
    logical :: r

    r = this % step()
  end function stop

  function init(this) result(r)
    class(module_print_scalar_data) :: this
    logical :: r
    character(len=1000) :: test
    r = this % module % init()

    if(r) then

       if( .not. this % named() ) then
          this % name = "module_print_scalar_data"
       end if

    end if

  end function init



end module class_module_print_scalar_data
