module class_module_print_data

  use class_solver_data
  use system_functions
  use class_module

  private

  type, public, extends(module) :: module_print_data
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
  end type module_print_data

contains

  function start(this) result(r)
    class(module_print_data) :: this
    logical :: r

    r = .true.


       ! if no file_name has been specified, one is generated
       if( trim(this%file_name) == "" ) then
          ! should generate file name automatically as follows
          ! [date]/[solver%name]/[module_print_data%name + some unique
          ! module%id]/
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

    ! print data at the start of the run
    r = this % step()

    ! @todo write a header

  end function start

  function step(this) result(r)
    class(module_print_data) :: this
    logical :: r
    integer :: nx, nf, i,j
    real, pointer :: x(:), f(:,:), dfdt(:,:), data_block(:,:)
    real, pointer :: data_scalars(:)
    integer :: file_handle
    integer :: iostat
    character(len=1000) :: file_name
    character(len=30)   :: name
    character(len=30), pointer :: data_scalars_names(:)
    class(solver_data), pointer :: s

    r = .true.

    s => this % solver_data

    x => s % x
    f => s % f
    dfdt => s % dfdt
    data_block => s % data_block
    data_scalars => s % data_scalars
    data_scalars_names => s % data_scalars_names

    nx = s % nx
    nf = s % nf

    ! set a new file name
    write(file_name, '(a,a,i20.20,a)')       &
         trim(this % file_name),             &
         "_n=", s % n_iter, &
         trim(this % extension)

    ! @todo write a timestamp
    open(newunit = file_handle,&
         file    = file_name,  &
         form    = 'formatted', &
         action  = 'write', &
         ! access = 'direct', &
         recl    = 10000, &
         iostat = iostat, &
         status  = 'replace')

    if( iostat /= 0  ) then
       ! @todo report error
       print *, "error in module_print_data ioostat /= 0"
       r = .false.
       return
    end if

    if( file_handle == 0) then
       ! @todo report error
       print *, "error in module_print_data file_handle == 0"
       r = .false.
       return
    end if


    ! @todo: write constant solver_data parameters to some file
    write( file_handle, * )&
         "#  solver_name = ", s % name
    write( file_handle, * )&
         "#  t = ", s % t
    write( file_handle, * )&
         "# dt = ", s % dt
    write( file_handle, * )&
         "# nx = ", s % nx
    write( file_handle, * )&
         "# nf = ", s % nf
    write( file_handle, * )&
         "# x0 = ", s % x0
    write( file_handle, * )&
         "# x1 = ", s % x1
    write( file_handle, * )&
         "# n_iter = ", s % n_iter

    call print_scalars( file_handle, &
         data_scalars, &
         data_scalars_names )

    call print_scalars( file_handle,&
         s % user_data_scalars,&
         s % user_data_scalars_names )


    if( associated( s % data_block ) ) then
       do i = 1, nx
          write( file_handle, *)&
               x(i),&
               (f(i,j), j=1,nf),&
               (dfdt(i,j), j=1,nf), &
               (data_block(i,j), j=1,size(data_block,2))
       end do
    else
       do i = 1, nx
          write( file_handle, *)&
               x(i),&
               (f(i,j),    j=1,nf),&
               (dfdt(i,j), j=1,nf)
       end do
    end if

    write( file_handle, *) (new_line('a'), i=1,3)

    close( file_handle )

  end function step

  function stop(this) result(r)
    class(module_print_data) :: this
    logical :: r

    r = this % step()
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

    end if

  end function init


  subroutine print_scalars( file_handle, data_scalars, data_names )
    integer :: file_handle
    real, pointer :: data_scalars(:)
    character(len=30), pointer :: data_names(:)
    character(len=30) :: name
    integer :: ls, ln, i

    if( .not. associated( data_scalars )) then
       return
    else
       ls = size(data_scalars)
    end if

    if( associated( data_names) ) then
       ln = size(data_names)
    else
       ln = 0
    end if

    do i = 1, ls
       if( i <= ln ) then
          name = data_names(i)
       else
          write(name, "(a,i1)") "x", i - ln + 1
       end if

       write( file_handle, * )&
            "# ", name, &
            " = ", data_scalars(i)
    end do

  end subroutine print_scalars


end module class_module_print_data
