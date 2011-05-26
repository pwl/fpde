!! nie kompilujacy sie modul z parametrem class(*)
! module ode_system_module_params
!    implicit none 
   
!    !! interfejs
!    abstract interface
!       subroutine fun_interface( t, y, dydt, params )
!          real, intent(in) :: t
!          real, intent(in) :: y(:)
!          real, intent(out) :: dydt(:)
!          class(*) :: params
!       end subroutine fun_interface

!       subroutine jac_interface( t, y, dfdy, dfdt, params )
!          real, intent(in) :: t
!          real, intent(in) :: y(:)
!          real, intent(out) :: dfdy(:)
!          real, intent(out) :: dfdt(:)
!          class(*) :: params
!       end subroutine jac_interface
!    end interface


!    type :: ode_system
!       procedure(fun_interface), pointer, nopass :: fun
!       procedure(jac_interface), pointer, nopass :: jac
!       integer :: dim
!       class(*) :: params
!    end type ode_system
   

! contains

!    !! konstruktor
!    subroutine ode_system_construct( sys, fun, jac, dim, params )
!       interface
!          subroutine fun( t, y, dydt, params )
!             real, intent(in) :: t
!             real, intent(in) :: y(:)
!             real, intent(out) :: dydt(:)
!             class(*) :: params
!          end subroutine fun

!          subroutine jac( t, y, dfdy, dfdt, params )
!             real, intent(in) :: t
!             real, intent(in) :: y(:)
!             real, intent(out) :: dfdy(:)
!             real, intent(out) :: dfdt(:)
!             class(*) :: params
!          end subroutine jac
!       end interface
!       integer :: dim

!       type(ode_system) :: sys
!       sys % fun => fun
!       sys % jac => jac
!       sys % dim = dim
!       sys % params = params
!    end subroutine ode_system_construct

! end module ode_system_module_params

  

module ode_system_module
   implicit none
   
   !! interfejs
   abstract interface
      subroutine fun_interface( t, y, dydt )
         real, intent(in) :: t
         real, intent(in) :: y(:)
         real, intent(out) :: dydt(:)
      end subroutine fun_interface

      subroutine jac_interface( t, y, dfdy, dfdt )
         real, intent(in) :: t
         real, intent(in) :: y(:)
         real, intent(out) :: dfdy(:)
         real, intent(out) :: dfdt(:)
      end subroutine jac_interface
   end interface


   type :: ode_system
      procedure(fun_interface), pointer, nopass :: fun
      procedure(jac_interface), pointer, nopass :: jac
      integer :: dim
      !class(*) :: params
   end type ode_system
   

contains

   !! konstruktor
   subroutine ode_system_construct( sys, fun, jac, dim )
      interface
         subroutine fun( t, y, dydt )
            real, intent(in) :: t
            real, intent(in) :: y(:)
            real, intent(out) :: dydt(:)
         end subroutine fun

         subroutine jac( t, y, dfdy, dfdt )
            real, intent(in) :: t
            real, intent(in) :: y(:)
            real, intent(out) :: dfdy(:)
            real, intent(out) :: dfdt(:)
         end subroutine jac
      end interface
      integer :: dim

      type(ode_system) :: sys
      sys % fun => fun
      sys % jac => jac
      sys % dim = dim
   end subroutine ode_system_construct

end module ode_system_module




program ode_system_test

  ! uzywamy modulu zdefiniowanego wyzej
  use ode_system_module

  integer :: dim = 1
  real :: t=0.0
  real :: y(1)=(/2.0/)
  real :: dydt(1)=(/3.0/)
  real :: dfdy(1), dfdt(1)

  type(ode_system) :: myode

  call ode_system_construct (myode, myfun, myjac, dim)

  print *, 'przed wywolaniem prawych stron dydt=', dydt

  call myode % fun(t,y,dydt)

  print *, 'po wywolaniu prawych stron dydt=', dydt

contains
   ! deklaruje prawa strone rownan oraz jakobian
   subroutine myfun( t, y, dydt )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(out) :: dydt(:)
      dydt(1)=2.0*y(1)
   end subroutine myfun

   subroutine myjac( t, y, dfdy, dfdt )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(out) :: dfdy(:)
      real, intent(out) :: dfdt(:)
      dfdy(1)=t
      dfdt(1)=t
   end subroutine myjac

end program ode_system_test
