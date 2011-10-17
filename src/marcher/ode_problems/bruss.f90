module class_bruss_oivp

   use class_ode_system
   use class_ode_iv_problem
   
   private
   type, public, extends( ode_iv_problem ) :: bruss_oivp
   contains
      procedure :: init
   end type bruss_oivp
contains
   subroutine init( oivp )
      class(bruss_oivp), intent(inout) :: oivp
      ! local variables
      integer, parameter :: n = 2
      ! dimension of the problem
      oivp % dim = n
      ! short description
      oivp % name = 'BRUSSELATOR'
      oivp % desc = '-'
      ! initial values
      allocate ( oivp % y0( n ) )
      allocate ( oivp % sol_end( n ) )
      oivp % y0(1) = 1.5
      oivp % y0(2) = 3.0
      ! ode_system initialization
      call ode_system_init( sys = oivp % sys, fun = rhs, dim = n, params = 0 )

   end subroutine init

   subroutine rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status

      dydt(1)  = 1.0 + y(1)*y(1)*y(2) - 4.0*y(1)
      dydt(2)  = 3.0*y(1) - y(1)*y(1)*y(2)
   end subroutine rhs

end module class_bruss_oivp
