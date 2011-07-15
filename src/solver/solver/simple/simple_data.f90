module class_solver_simple_data

  use class_mesh
  use class_ode_stepper
  use class_ode_marcher
  use mesh_factory
  use stepper_factory
  use class_ode_system
  use class_solver

  private

  type, public :: solver_simple_data
     character(len=30) :: mesh_id = "sfd3pt"
     character(len=30) :: step_id = "rk4cs"
     character(len=30) :: control_id = ""
     integer :: nf=1, nx, rk=2
     real :: x0, x1
     real :: h0=1.e-3, t0=0., t1=1.e10
     real :: abs_error = 1.e-8, rel_error = 1.e-8
     class(*), pointer :: params => null()
     procedure(interface_rhs), pointer, nopass :: rhs
   contains
     procedure :: initialize_mesh
     procedure :: initialize_marcher
     procedure :: initialize_step
     procedure :: initialize_t
     procedure :: initialize_ode_system
     procedure :: initialize_dfdt
     procedure :: info
  end type solver_simple_data

contains

  subroutine initialize_mesh(data, m)
    class(solver_simple_data), intent(in) :: data
    class(mesh), pointer :: m

    m => mesh_new( data % mesh_id )

    if( .not. associated( m ) ) then
       print *, data % mesh_id, "is not a valid mesh_id"
       stop
    end if

    call m % init(                 &
         data % nx, data % nf, data % rk, &
         data % x0, data % x1 )
  end subroutine initialize_mesh

  subroutine initialize_step( data, s )
    class(solver_simple_data), intent(in) :: data
    class(ode_stepper), pointer :: s

    s => stepper_new( data % step_id )
    if( .not. associated( s )) then
       print *, data % step_id, "is not a valid step_id"
       stop
    end if

    call s % init( data % nx * data % nf )

  end subroutine initialize_step

  subroutine initialize_t( data, t )
    class(solver_simple_data), intent(in) :: data
    real, pointer :: t

    allocate(t)
    t = data % t0

  end subroutine initialize_t

  subroutine initialize_ode_system( data, system, rhs, params )
    class(solver_simple_data), intent(in) :: data
    class(ode_system), pointer :: system
    procedure(fun_interface) :: rhs
    class(*) :: params

    allocate( system )
    call ode_system_init(&
         sys = system,&
         fun = rhs,&
         dim = data % nx * data % nf,&
         params = params )

  end subroutine initialize_ode_system

  subroutine initialize_marcher( data, m)
    class(solver_simple_data), intent(in) :: data
    class(ode_marcher), pointer :: m

    allocate( m )
    call m % init( data % nf * data % nx )

  end subroutine initialize_marcher

  subroutine initialize_dfdt( data, dfdt )
    class(solver_simple_data) :: data
    real, allocatable :: dfdt(:,:)

    allocate( dfdt( data % nx, data % nf ) )

  end subroutine initialize_dfdt

  subroutine info( data )
    class(solver_simple_data) :: data

    print *, "mesh_id: ", trim(data % mesh_id)
    print *, "step_id: ", trim(data % step_id)
    print *, "control_id: ", trim(data % control_id)
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
