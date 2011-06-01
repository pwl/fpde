!modul ode_system z parametrem class(*)
module ode_system_module

   public :: fun_interface, jac_interface
   
   !! interfejs
   abstract interface
      subroutine fun_interface( t, y, dydt, params )
         real, intent(in) :: t
         real, intent(in) :: y(:)
         real, intent(out) :: dydt(:)
         class(*) :: params
      end subroutine fun_interface

      subroutine jac_interface( t, y, dfdy, dfdt, params )
         real, intent(in) :: t
         real, intent(in) :: y(:)
         real, intent(out) :: dfdy(:)
         real, intent(out) :: dfdt(:)
         class(*) :: params
      end subroutine jac_interface
   end interface


   type, public :: ode_system
      procedure(fun_interface), pointer, nopass :: fun
      procedure(jac_interface), pointer, nopass :: jac
      integer :: dim
      class(*), pointer :: params
   end type ode_system
   

contains

   !! konstruktor
   subroutine ode_system_construct( sys, fun, jac, dim, params )
      ! interface
      !    subroutine fun( t, y, dydt, params )
      !       real, intent(in) :: t
      !       real, intent(in) :: y(:)
      !       real, intent(out) :: dydt(:)
      !       class(*) :: params
      !    end subroutine fun

      !    subroutine jac( t, y, dfdy, dfdt, params )
      !       real, intent(in) :: t
      !       real, intent(in) :: y(:)
      !       real, intent(out) :: dfdy(:)
      !       real, intent(out) :: dfdt(:)
      !       class(*) :: params
      !    end subroutine jac
      ! end interface
      procedure(fun_interface) :: fun
      procedure(jac_interface) :: jac
      integer :: dim
      class (*),target :: params

      type(ode_system) :: sys
      sys % fun => fun
      sys % jac => jac
      sys % dim = dim
      sys % params => params
   end subroutine ode_system_construct

end module ode_system_module
