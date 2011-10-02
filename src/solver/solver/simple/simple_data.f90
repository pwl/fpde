! @todo: reduce the number of functions
module class_solver_simple_data

  use class_solver_data
  use class_solver
  use class_solver_simple
  use class_mesh
  use class_ode_stepper
  use class_ode_marcher
  use class_ode_system
  use class_ode_step_control

  use mesh_factory
  use stepper_factory
  use solver_factory
  use control_factory

  private

  type, public :: solver_simple_data
     character(len=30) :: mesh_id = "sfd3pt"
     character(len=30) :: stepper_id = "rk4cs"
     character(len=30) :: step_control_id = "standard"
     character(len=30) :: solver_id = "simple"
     integer :: nf=1, nx, rk=2
     real :: x0, x1
     real :: h0=1.e-3, t0=0., t1=1.e10
     real :: abs_error = 1.e-10, rel_error = 1.e-10
     class(*), pointer :: params => null()
     procedure(interface_rhs), pointer, nopass :: rhs
   contains
     procedure :: initialize_mesh
     procedure :: initialize_marcher
     procedure :: initialize_stepper
     procedure :: initialize_step_control
     procedure :: initialize_t
     procedure :: initialize_ode_system
     procedure :: initialize_rhs
     procedure :: info
     procedure :: generate_solver
  end type solver_simple_data

contains

  function generate_solver ( this ) result(s)
    class(solver_simple_data), intent(in), target :: this
    class(solver_simple), pointer :: s
    integer :: i,j,nx,nf

    allocate( s )
    call s % init

    ! local variables
    nx = this % nx
    nf = this % nf

    ! @todo add iterator
    s % name = "simple"

    ! initialize mesh
    call this % initialize_mesh( s % mesh )
    ! initialize stepper
    call this % initialize_stepper( s % stepper )
    ! initialize step control
    call this % initialize_step_control( s % step_control )
    ! initialize time
    call this % initialize_t( s % t )
    ! initialize ode_system
    call this % initialize_ode_system( s % system, s)
    ! initialize marcher
    call this % initialize_marcher( s % marcher )
    ! initialize right hand side of equations
    call this % initialize_rhs( s % rhs, s % rhs_status )

    ! assign interface pointers
    s % x      => s % mesh % x
    s % dfdx   => s % mesh % df
    s % params => this % params
    s % data   => this

    s % nx     = nx
    s % nf     = nf
    s % rk     = this % rk
    s % t1     = this % t1
    s % dt      = this % h0

    ! @todo is this needed? may be for the reinitialization purposes,
    ! i.e. to reset the solver without referring to data explicitely?
    s % data => this

    ! this is where the actual data is being held, the view of s % f
    ! often points here (but not always) @todo be precise!
    allocate( s % y( nf * nx ) )
    allocate( s % dydt( nf * nx ) )

    ! we shall sync the solver in order for the user to set the
    ! initial data using the s % f view
    call s % sync_f( s % y )

    ! the view below is outdated
    ! @todo when pointer bounds(rank) remapping (test/array_test.f90)
    ! is implemented the following should work instead, then no
    ! copying will be required. (no, no, no, wrong, do it again)
    !
    ! s % y( 1 : s % nf * s % nx ) => s % mesh % f

    ! @todo add initial data for solver_simple_data here

    end function generate_solver


  subroutine initialize_mesh(data, m)
    class(solver_simple_data), intent(in) :: data
    class(mesh), pointer :: m

    m => mesh_new( data % mesh_id )

    if( .not. associated( m ) ) then
       print *, data % mesh_id, "is not a valid mesh_id"
       ! @todo report error instead of stopping
       stop
    end if

    call m % init(                 &
         data % nx, data % nf, data % rk, &
         data % x0, data % x1 )

    deallocate( m % f )

  end subroutine initialize_mesh

  subroutine initialize_stepper( data, s )
    class(solver_simple_data), intent(in) :: data
    class(ode_stepper), pointer :: s

    s => stepper_new( data % stepper_id )
    if( .not. associated( s )) then
       print *, data % stepper_id, "is not a valid stepper_id"
       ! @todo report error
       return
    end if

    call s % init( data % nx * data % nf )

  end subroutine initialize_stepper

  subroutine initialize_step_control( data, c )
    class(solver_simple_data), intent(in) :: data
    class(ode_step_control), pointer :: c

    c => control_new( data % step_control_id )

    if( .not. associated( c ) ) then
       print *, "ERROR: ", trim(data % step_control_id),&
            ": is not a valid stepper_id"
       return
    end if

    call c % init( &
         eps_abs = data % abs_error,&
         eps_rel = data % rel_error,&
         a_y = 1.0, a_dydt = 1.0 )

  end subroutine initialize_step_control


  subroutine initialize_t( data, t )
    class(solver_simple_data), intent(in) :: data
    real, pointer :: t

    allocate(t)
    t = data % t0

  end subroutine initialize_t

  subroutine initialize_ode_system( data, system, params )
    class(solver_simple_data), intent(in) :: data
    class(ode_system), pointer :: system
    class(*) :: params

    allocate( system )
    call ode_system_init(&
         sys = system,&
         fun = solver_simple_rhs_for_marcher,&
         dim = data % nx * data % nf,&
         params = params )

  end subroutine initialize_ode_system

  subroutine initialize_rhs( data, rhs, rhs_status )
    class(solver_simple_data), intent(in) :: data
    procedure(interface_rhs), pointer :: rhs
    integer :: rhs_status

    if ( associated(data % rhs) ) then
       rhs => data % rhs
       rhs_status = 1
    else
       ! @todo error!
       print *, "no rhs defined"
       ! no rhs defined!
       rhs_status = -1
    end if


  end subroutine initialize_rhs

  subroutine initialize_marcher( data, m)
    class(solver_simple_data), intent(in) :: data
    class(ode_marcher), pointer :: m

    allocate( m )
    call m % init( data % nf * data % nx )

  end subroutine initialize_marcher

  subroutine info( data )
    class(solver_simple_data) :: data

    print *, "mesh_id: ", trim(data % mesh_id)
    print *, "stepper_id: ", trim(data % stepper_id)
    print *, "step_control_id: ", trim(data % step_control_id)
    print *, "t0 = ", data % t0
    print *, "t1 = ", data % t1
    print *, "h0 = ", data % h0
    print *, "x0 = ", data % x0
    print *, "x1 = ", data % x1
    print *, "nx = ", data % nx
    print *, "nf = ", data % nf
    print *, "rk = ", data % rk

    ! print *, data
  end subroutine info


end module class_solver_simple_data
