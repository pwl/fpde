module class_stiff3_oivp

   use class_ode_system
   use class_ode_iv_problem
   
   private
   type, public, extends( ode_iv_problem ) :: stiff3_oivp
   contains
      procedure :: init
   end type stiff3_oivp
contains
   subroutine init( oivp )
      class(stiff3_oivp), intent(inout) :: oivp
      ! local variables
      integer, parameter :: n = 2
      real, parameter :: eps = 0.003
      ! dimension of the problem
      oivp % dim = n
      ! short description
      oivp % name = 'STIFF3'
      oivp % desc = 'Van der Pol equation with eps=0.003'
      oivp % has_analytical_solution = .false.
      ! initial values
      allocate ( oivp % y0( n ) )
      allocate ( oivp % sol_end( n ) )
      oivp % y0(1) = 2.0
      oivp % y0(2) = 0.0

      oivp % tend = 2.5

      ! ode_system initialization
      call ode_system_init( sys = oivp % sys, fun = rhs, dim = n, params = 0 )

   end subroutine init

   subroutine rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dydt(:)
      class(*)  :: params
      integer, optional :: status

      real, parameter :: eps = 0.003

      dydt(1)  = y(2)
      dydt(2)  = ( (1.0-y(1)**2)*y(2) - y(1) )/eps
   end subroutine rhs

end module class_stiff3_oivp
