module class_aren_oivp

   use class_ode_system
   use class_ode_iv_problem
   
   private
   type, public, extends( ode_iv_problem ) :: aren_oivp
   contains
      procedure :: init
   end type aren_oivp
contains
   subroutine init( oivp )
      class(aren_oivp), intent(inout) :: oivp
      ! local variables
      integer, parameter :: n = 4
      ! dimension of the problem
      oivp % dim = n
      ! short description
      oivp % name = 'AREN'
      oivp % desc = 'Arenstorf orbits - the rescticted three body problem'
      ! initial values
      allocate ( oivp % y0( n ) )
      allocate ( oivp % sol_end( n ) )
      oivp % y0(1) = 0.994
      oivp % y0(2) = 0.0
      oivp % y0(3) = 0.0
      oivp % y0(4) = -2.00158510637908252240537862224

      oivp % tend = 17.0652165601579625588917206249
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
      ! masses
      real, parameter :: mu = 0.012277471, mu1 = 1.0 - mu
      real :: D1, D2
      
      D1 = ( (y(1)+mu)**2 + y(2)**2 )**(3.0/2.0)
      D2 = ( (y(1)-mu1)**2 + y(2)**2 )**(3.0/2.0)
      
      dydt(1)  = y(3)
      dydt(2)  = y(4)
      dydt(3)  = y(1) + 2.0*y(4) - mu1*(y(1)+mu)/D1 - mu*(y(1)-mu1)/D2
      dydt(4)  = y(2) - 2.0*y(3) - mu1*y(2)/D1 - mu*y(2)/D2
   end subroutine rhs

end module class_aren_oivp
