! General ode_system class
module class_ode_system

   public :: fun_interface, jac_interface

   !! interfaces
   abstract interface
      subroutine fun_interface( t, y, dydt, params, status )
         real, intent(in) :: t
         real, pointer, intent(in) :: y(:)
         real, pointer, intent(out) :: dydt(:)
         class(*) :: params
         integer, optional :: status
      end subroutine fun_interface

      subroutine jac_interface( t, y, dfdy, dfdt, params, status )
         real, intent(in) :: t
         real, pointer, intent(in) :: y(:)
         real, pointer, intent(out) :: dfdy(:)
         real, pointer, intent(out) :: dfdt(:)
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
      procedure(fun_interface), pointer, intent(in) :: fun
      procedure(jac_interface), pointer, intent(in), optional :: jac
      integer :: dim
      class (*), target :: params

      type(ode_system) :: sys

      if( associated( fun ) ) then
         sys % fun => fun
      else
         ! @todo report error!
         print *, "nie ma funkcji w ode_system_init"
      end if

      if ( present( jac ) .and. associated( jac )) then
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
