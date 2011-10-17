module class_stiff1_oivp

   use class_ode_system
   use class_ode_iv_problem
   
   private
   type, public, extends( ode_iv_problem ) :: stiff1_oivp
   contains
      procedure :: init
   end type stiff1_oivp
contains
   subroutine init( oivp )
      class(stiff1_oivp), intent(inout) :: oivp
      ! local variables
      integer, parameter :: n = 1
      ! dimension of the problem
      oivp % dim = n
      ! short description
      oivp % name = 'STIFF1'
      oivp % desc = 'Simple, one dimensional stiff ODE'
      oivp % has_analytical_solution = .true.
      ! initial values
      allocate ( oivp % y0( n ) )
      allocate ( oivp % sol_end( n ) )
      oivp % y0(1) = 1.0

      oivp % tend = 5.0
      call sol( oivp % tend, oivp % dim, oivp % sol_end, 0 )
      ! ode_system initialization
      call ode_system_init( sys = oivp % sys, fun = rhs, dim = n, params = 0 )

   end subroutine init

   subroutine rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status

      ! fixed equation parameters
      real, parameter :: a = 15.0

      dydt(1)  = - a*y(1)
   end subroutine rhs

   subroutine sol( t, dim, y, params )
      real, intent(in) :: t
      integer, intent(in) :: dim
      real, pointer, intent(out) :: y(:)
      class(*) :: params
      ! fixed equation parameters
      real, parameter :: a = 15.0

      y(1) = exp(-a*t)
   end subroutine sol


end module class_stiff1_oivp
