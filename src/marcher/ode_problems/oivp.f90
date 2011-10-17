module class_ode_iv_problem
   
   use class_ode_system

   private
   type, public :: ode_iv_problem
      type(ode_system)   :: sys
      character(len=32)  :: name
      character(len=256) :: desc
      integer            :: dim
      real, pointer      :: y0(:)  ! initial data
      real, pointer      :: sol_end(:) ! analytic or high precision solution at tend
      real               :: tend
      logical            :: has_analytical_solution
      class(*), pointer  :: params
   contains
      procedure :: init
      procedure :: info
      procedure :: free
   end type ode_iv_problem
contains 
   subroutine init( oivp )
      class(ode_iv_problem), intent(inout) :: oivp
   end subroutine init

   subroutine rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status
   end subroutine rhs

   subroutine jac( t, y, dfdy, dfdt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dfdy(:)
      real, pointer, intent(out) :: dfdt(:)
      class(*) :: params
      integer, optional :: status
   end subroutine jac

   subroutine info( oivp )
      class(ode_iv_problem), intent(inout) :: oivp
      print *, 'ODE initial value problem: ', trim(oivp % name)
      print *, 'dim:                       ', oivp % dim
      print *, 'y0:                        ', oivp % y0
      print *, 'has_analytical_solution:   ', oivp % has_analytical_solution
      print *, 'description:               ', trim(oivp % desc)
   end subroutine info

   subroutine sol( t, dim, y, params )
      real, intent(in) :: t
      integer, intent(in) :: dim
      real, pointer, intent(out) :: y(:)
      class(*) :: params
   end subroutine sol

   subroutine free( oivp )
      class(ode_iv_problem), intent(inout) :: oivp
      deallocate ( oivp % y0 )
      deallocate ( oivp % sol_end )
   end subroutine free

end module class_ode_iv_problem
