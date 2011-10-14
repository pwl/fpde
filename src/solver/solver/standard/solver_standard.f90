!>
!! @file   solver_standard.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Sep 30 15:54:26 2011
!!
!! @brief Standard solver containing one marcher used in the method of
!! lines
!!
!!
!!
module class_solver_standard

  use class_solver_data
  use class_solver

  use class_ode_marcher
  use class_ode_stepper
  use class_ode_system
  use class_ode_step_control

  use stepper_factory
  use control_factory

  private

  type, public, extends(solver) :: solver_standard

     ! initialization data
     character(len=30) :: stepper_id = "rkpd54"
     character(len=30) :: step_control_id = "standard"
     real :: abs_error = 1.e-10
     real :: rel_error = 1.e-10
     ! real :: dt = 0.
     ! end of initialization data

     ! initialization data used by the programmer of extended type
     ! this is the length of the vector y(:), should be set only by
     ! classes inheriting from solver_standard
     integer :: ny = 0
     ! @todo rhs_for_marcher should be deleted from solver_data and
     ! considered as depreciatede
     procedure(fun_interface), pointer, nopass :: rhs_marcher => null()
     ! pointer used to initialize the system
     class(solver), pointer           :: param_solver => null()
     ! end of initialization data for extended type

     ! general vector containing all the data which is passed to
     ! marcher
     real, contiguous, pointer :: y(:) => null()
     ! temporary storage for dydt
     real, contiguous, pointer :: dydt(:) => null()

     ! marcher data
     class(ode_stepper), pointer      :: stepper      => null()
     class(ode_system), pointer       :: system       => null()
     class(ode_marcher), pointer      :: marcher      => null()
     class(ode_step_control), pointer :: step_control => null()


   contains
     procedure :: init
     procedure :: free
     procedure :: info
     procedure :: set_rhs_marcher
     procedure :: calculate_dfdx
  end type solver_standard

contains

  subroutine init( s )
    class(solver_standard), target :: s
    integer :: ny

    call s % solver % init

    ny = s % ny

    if( ny == 0 ) then
       ! @todo report error
       print *, "ERROR: solver_standard: ny = 0"
    end if


    ! allocate the memory used to contain all of the sovler data
    allocate( s % y( ny ) )

    ! allocate the memory used to contain all the time derivatives
    allocate( s % dydt( ny ) )

    ! initialize marchers
    allocate( s % marcher )
    call s % marcher % init( ny )

    ! create a stepper from stepper_id
    s % stepper => stepper_new( s % stepper_id )
    if( .not. associated( s % stepper )) then
       print *, "ERROR: ", trim(s % stepper_id),&
            "is not a valid stepper_id"
    else
       call s % stepper % init( ny )
    end if

    ! @todo create step_control from step_control_id
    s % step_control => control_new( s % step_control_id )
    if( .not. associated( s % step_control ) ) then
       print *, "ERROR: ", trim(s % step_control_id),&
            "is not a valid step_control_id"
    else
       call s % step_control % init( &
            eps_abs = s % abs_error, &
            eps_rel = s % rel_error, &
            a_y = 1.0, a_dydt = 1.0 )
    end if


    ! setup the ode_system
    allocate( s % system )
    call ode_system_init(          &
         sys    = s % system,      &
         fun    = s % rhs_marcher, &
         dim    = ny,              &
         params = s % param_solver )

  end subroutine init

  subroutine info( s )
    class(solver_standard) :: s
    integer :: f
    f = s % info_file

    call s % solver % info

    ! write(f,*) "#------------------------"
    ! write(f,*) "#--- SOLVER_STANDARD --- "
    ! write(f,*) "#------------------------"
    write(f,*) "# stepper_id      =    ", trim(s % stepper_id)
    write(f,*) "# ready: ",               associated(s % stepper)
    write(f,*) "# step_control_id =    ", trim(s % step_control_id)
    write(f,*) "# ready: ",               associated(s % step_control)
    write(f,*) "# abs_error =    ", s % abs_error
    write(f,*) "# rel_error =    ", s % rel_error
    write(f,*) "# ny              =    ", s % ny
    write(f,*) "# associated(y(:)): ", associated( s%y )

  end subroutine info

  ! @todo free the stepper, marcher and step_control
  subroutine free( s )
    class(solver_standard) :: s

    deallocate( s % y )

  end subroutine free

  subroutine calculate_dfdx(s, i)
    class(solver_standard) :: s
    integer :: i

    print *, "solver_standard: mam cie"

  end subroutine calculate_dfdx


  subroutine set_rhs_marcher( s, rhs )
    class(solver_standard) :: s
    procedure(fun_interface), pointer, intent(in) :: rhs

    if( associated( rhs ) ) then
       s % rhs_marcher => rhs
    else
       ! @todo report error
       print *, "ERROR: SOLVER_STANDARD: no rhs given to set_rhs_marcher()"
    end if

  end subroutine set_rhs_marcher



end module class_solver_standard
