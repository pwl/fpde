module class_stiff2_oivp

   use class_ode_system
   use class_ode_iv_problem
   
   private
   type, public, extends( ode_iv_problem ) :: stiff2_oivp
   contains
      procedure :: init
   end type stiff2_oivp
contains
   subroutine init( oivp )
      class(stiff2_oivp), intent(inout) :: oivp
      ! local variables
      integer, parameter :: n = 1
      real, parameter :: eps = 1.0e-3
      ! dimension of the problem
      oivp % dim = n
      ! short description
      oivp % name = 'STIFF2'
      oivp % desc = 'Simple, one dimensional stiff ODE - combustion model'
      oivp % has_analytical_solution = .false.
      ! initial values
      allocate ( oivp % y0( n ) )
      allocate ( oivp % sol_end( n ) )
      oivp % y0(1) = eps

      oivp % tend = 2.0/eps

      ! ode_system initialization
      call ode_system_init( sys = oivp % sys, fun = rhs, dim = n, params = 0 )

   end subroutine init

   subroutine rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status

      dydt(1)  = y(1)*y(1)*(1.0-y(1))
   end subroutine rhs

end module class_stiff2_oivp
