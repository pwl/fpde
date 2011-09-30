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

  private

  type, public, extends(solver) :: solver_standard

     ! initialization data
     character(len=30) :: stepper_id = ""
     character(len=30) :: step_control_id = ""
     real :: dt = 0.
     ! this is the length of the vector y(:), should be set only by
     ! classes inheriting from solver_standard
     integer :: ny = 0
     ! @todo rhs_for_marcher should be deleted from solver_data and
     ! considered as depreciatede
     procedure(fun_interface), pointer, nopass :: rhs_marcher => null()
     ! end of initialization data

     ! general vector containing all the data which is passed to
     ! marcher
     real, contiguous, pointer :: y(:) => null()

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

  end type solver_standard

contains

  subroutine init( s )
    class(solver_standard) :: s
    integer :: ny

    ny = s % ny

    if( ny == 0 ) then
       ! @todo report error
       print *, "ERROR: solver_standard: ny = 0"
    end if


    ! allocate the memory used to contain all of the sovler data
    allocate( s % y( ny ) )

    ! initialize marchers
    allocate( s % marcher )
    call s % marcher % init( ny )

    ! create a stepper from stepper_id
    s % stepper => stepper_new( s % stepper_id )
    if( .not. associated( s % stepper )) then
       print *, s % stepper_id, "is not a valid stepper_id"
    end if

    ! @todo create step_control from step_control_id

    ! setup the ode_system
    allocate( s % system )
    call ode_system_init(          &
         sys    = s % system,      &
         fun    = s % rhs_marcher, &
         dim    = ny,              &
         params = s % params )

  end subroutine init

  subroutine info( s )
    class(solver_standard) :: s

    call s % solver % info

    print *, "========================"
    print *, "--- SOLVER_STANDARD --- "
    print *, "========================"
    print *, "stepper_id:       ", trim(s % stepper_id)
    print *, "step_control_id:  ", trim(s % step_control_id)
    print *, "ny:               ", s % ny
    print *, "associated(y(:)): ", associated( s%y )

  end subroutine info

  ! @todo free the stepper, marcher and step_control
  subroutine free( s )
    class(solver_standard) :: s

    deallocate( s % y )

  end subroutine free

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