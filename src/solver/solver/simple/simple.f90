! simple solver class with one mesh and one marcher

module class_solver_simple

  use class_solver
  use class_marcher
  use ode_system_module
  use class_stepper
  use class_mesh

  use pretty_print

  private

  type, public, extends(solver) :: solver_simple
     ! internal variables of the solver_simple
     real :: max_t
     real, pointer :: y(:)
     class(mesh), pointer    :: mesh
     class(marcher), pointer :: marcher
     class(ode_stepper_type), pointer :: step
     class(ode_system), pointer :: ode_system
   contains
     ! overloaded functions
     procedure :: init
     procedure :: free
     procedure :: solve
     procedure :: calculate_dfdx
     procedure :: pointwise_dfdx
  end type solver_simple

contains

  subroutine init(s, msh, march, max_t, rhs, step, params)
    class(solver_simple), intent(inout) :: s
    class(mesh), target                 :: msh
    class(marcher), intent(in), target  :: march
    real, intent(in)                    :: max_t
    procedure(interface_rhs)            :: rhs
    class(*), target                    :: params
    class(ode_stepper_type), target                :: step

    s % mesh   => msh
    s % params => params
    s % name   = "Simple solver"

    ! bind pointers to appropriate targets
    s % t     = 0.              ! initial time
    s % x     => msh % x
    s % f     => msh % f
    s % dfdx  => msh % df
    s % nx    = msh % nx
    s % nf    = msh % nf
    s % maxrk = msh % maxrk
    s % y( 1 : msh%nx * msh%nf ) => msh % f

    ! ode_system
    ! call ode_system_construct( &
    !      sys = s % ode_system,         &
    !      fun = rhs_for_marcher,    &
    !      jac = null(),                 &
    !      dim = s % nx * s % nf,        &
    !      params = s % params )

    ! stepper
    s % step => step

    ! marcher
    s % marcher  => march

    ! allocate space for a pointer to calculated rhs
    allocate( s % dfdt(msh % nx, msh % nf ) )

    ! set the rhs
    s % rhs => rhs

  end subroutine init

  subroutine solve(s)
    class(solver_simple), intent(inout) :: s

    call s % rhs( s % params )

    ! do while ( s % t < s % max_t )
       ! call s % marcher % apply(           &
       !      s   = s % step,                &
       !      sys = s % ode_system,          &
       !      t   = s % t,                   &
       !      t1  = s % max_t,               &
       !      h   = s % marcher % last_step, &
       !      y   = s % y )
    ! end do

  end subroutine solve


  subroutine free(s)
    class(solver_simple), intent(inout) :: s

    ! free components of the solver
    call s % mesh % free
    call s % marcher % free
    deallocate( s % dfdt )

  end subroutine free

  subroutine calculate_dfdx( s, i )
    class(solver_simple) :: s
    integer :: i

    call s % mesh % calculate_derivatives( i )

  end subroutine calculate_dfdx

  real function pointwise_dfdx( s, i, j, k )
    class(solver_simple) :: s
    integer :: i, j, k

    pointwise_dfdx = s % mesh % derivative( i,j,k )

  end function pointwise_dfdx

end module class_solver_simple
