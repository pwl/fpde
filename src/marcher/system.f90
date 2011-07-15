! General ode_system class
module class_ode_system

   public :: fun_interface, jac_interface
   
   !! interfaces
   abstract interface
      subroutine fun_interface( t, y, dydt, params, status )
         real, intent(in) :: t
         real, intent(in) :: y(:)
         real, intent(out) :: dydt(:)
         class(*) :: params
         integer, optional :: status
      end subroutine fun_interface

      subroutine jac_interface( t, y, dfdy, dfdt, params, status )
         real, intent(in) :: t
         real, intent(in) :: y(:)
         real, intent(out) :: dfdy(:)
         real, intent(out) :: dfdt(:)
         class(*) :: params
         integer, optional :: status
      end subroutine jac_interface
   end interface


   type, public :: ode_system
      procedure(fun_interface), pointer, nopass :: fun
      procedure(jac_interface), pointer, nopass :: jac
      integer :: dim
      class(*), pointer :: params
      integer :: status
   end type ode_system
   

contains

   !! konstruktor
   subroutine ode_system_init( sys, fun, jac, dim, params )
      procedure(fun_interface) :: fun
      procedure(jac_interface), optional :: jac
      integer :: dim
      class (*),target :: params

      type(ode_system) :: sys
      sys % fun => fun
      if ( present( jac ) ) then
         sys % jac => jac
      else
         sys % jac => null()
      end if
      sys % dim = dim
      sys % params => params
      sys % status = 1 ! @todo domyslnie inicjalizowany status 1
      ! tymczasowa kolwencja
      ! status = 1 brak bledu
      ! status = 0 wystapily bledy
   end subroutine ode_system_init

end module class_ode_system
