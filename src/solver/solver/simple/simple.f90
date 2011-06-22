! simple solver class with one mesh and one marcher

module class_solver_simple

  use class_solver_simple_data
  use class_solver
  use class_marcher
  use ode_system_module
  use class_stepper
  use class_mesh

  use pretty_print

  private

  type, public, extends(solver) :: solver_simple
     real :: t1
     real :: h
     ! y(:) holds data used in mesh in a format compatible with
     ! rhs_for_marcher
     real, pointer :: y(:)
     class(mesh), pointer :: mesh
     class(ode_stepper_type), pointer :: step
     class(solver_simple_data), pointer :: data
     class(ode_system), pointer :: system
     class(marcher), pointer :: marcher
     ! class(control), pointer :: control
   contains
     procedure :: init
     procedure :: info
     procedure :: free
     procedure :: solve
     procedure :: calculate_dfdx
     procedure :: pointwise_dfdx
  end type solver_simple

contains

  subroutine init(s, data)
    class(solver_simple) :: s
    class(solver_simple_data), target :: data
    integer :: i,j

    s % name = "simple"

    ! initialize mesh
    call data % initialize_mesh( s % mesh )
    ! initialize step
    call data % initialize_step( s % step )
    ! initialize time
    call data % initialize_t( s % t )
    ! initialize ode_system
    call data % initialize_ode_system( s % system, s)
    ! initialize marcher
    call data % initialize_marcher( s % marcher )
    ! allocate memory for dfdt
    call data % initialize_dfdt( s % dfdt )
    ! initialize right hand side of equations
    call data % initialize_rhs( s % rhs )

    ! assign interface pointers
    s % x      => s % mesh % x
    s % f      => s % mesh % f
    s % dfdx   => s % mesh % df
    s % params => data % params
    s % data   => data

    s % nx     = data % nx
    s % nf     = data % nf
    s % rk     = data % rk
    s % t1     = data % t1
    s % h      = data % h0

    ! copy initial data from a mesh
    allocate( s%y( s%nf * s%nx ) )
    s % y = reshape( s % mesh % f, [ s%nf*s%nx ] )

    ! @todo when pointer bounds(rank) remapping (test/array_test.f90)
    ! is implemented the following should work instead, then no
    ! copying will be required
    !
    ! s % y( 1 : s % nf * s % nx ) => s % mesh % f

  end subroutine init


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


  subroutine info( s )
    class(solver_simple) :: s


    print *, "solver: ", s % name

    if( associated(s % mesh) ) then
       print *, "mesh: " , trim(s % mesh % name)
    else
       print *, "mesh: NONE"
    end if

    if( associated(s % step) ) then
       print *, "step: ", trim(s % step % name )
    else
       print *, "step: NONE"
    end if

    if( associated(s % system) ) then
       print *, "system: aye!"
    else
       print *, "system: nay!"
    end if

    if( associated(s % rhs) ) then
       print *, "rhs: aye!"
    else
       print *, "rhs: nay!"
    end if

    if( associated(s % marcher) ) then
       print *, "marcher: aye!"
    else
       print *, "marcher: nay!"
    end if

    if( associated(s % data) ) then
       print *, "data:"
       call s % data % info
    else
       print *, "data: nay!"
    end if

  end subroutine info


  subroutine free( s )
    class(solver_simple) :: s

    call s % mesh % free
    call s % step % free
    call s % marcher % free

    deallocate( s % system, s % t, s % dfdt, s % y )

  end subroutine free

  subroutine solve( s )
    class(solver_simple) :: s
    do while( s%t < s%t1)
       call s % marcher % apply(           &
            s   = s % step,                &
            sys = s % system,          &
            t   = s % t,                   &
            t1  = s % t1,               &
            h   = s % h, &
            y   = s % y )
    end do
  end subroutine solve

end module class_solver_simple

