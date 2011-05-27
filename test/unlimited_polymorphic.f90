!nie kompilujacy sie modul z parametrem class(*)
module ode_system_module_params
   implicit none 
   
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


   type :: ode_system
      procedure(fun_interface), pointer, nopass :: fun
      procedure(jac_interface), pointer, nopass :: jac
      integer :: dim
      class(*), pointer :: params
   end type ode_system
   

contains

   !! konstruktor
   subroutine ode_system_construct2( sys, fun, jac, dim, params )
      interface
         subroutine fun( t, y, dydt, params )
            real, intent(in) :: t
            real, intent(in) :: y(:)
            real, intent(out) :: dydt(:)
            class(*) :: params
         end subroutine fun

         subroutine jac( t, y, dfdy, dfdt, params )
            real, intent(in) :: t
            real, intent(in) :: y(:)
            real, intent(out) :: dfdy(:)
            real, intent(out) :: dfdt(:)
            class(*) :: params
         end subroutine jac
      end interface
      integer :: dim
      class (*),target :: params

      type(ode_system) :: sys
      sys % fun => fun
      sys % jac => jac
      sys % dim = dim
      sys % params => params
   end subroutine ode_system_construct2

end module ode_system_module_params

  

! module ode_system_module
!    implicit none
   
!    !! interfejs
!    abstract interface
!       subroutine fun_interface( t, y, dydt )
!          real, intent(in) :: t
!          real, intent(in) :: y(:)
!          real, intent(out) :: dydt(:)
!       end subroutine fun_interface

!       subroutine jac_interface( t, y, dfdy, dfdt )
!          real, intent(in) :: t
!          real, intent(in) :: y(:)
!          real, intent(out) :: dfdy(:)
!          real, intent(out) :: dfdt(:)
!       end subroutine jac_interface
!    end interface


!    type :: ode_system
!       procedure(fun_interface), pointer, nopass :: fun
!       procedure(jac_interface), pointer, nopass :: jac
!       integer :: dim
!       !class(*) :: params
!    end type ode_system
   

! contains

!    !! konstruktor
!    subroutine ode_system_construct( sys, fun, jac, dim )
!       interface
!          subroutine fun( t, y, dydt )
!             real, intent(in) :: t
!             real, intent(in) :: y(:)
!             real, intent(out) :: dydt(:)
!          end subroutine fun

!          subroutine jac( t, y, dfdy, dfdt )
!             real, intent(in) :: t
!             real, intent(in) :: y(:)
!             real, intent(out) :: dfdy(:)
!             real, intent(out) :: dfdt(:)
!          end subroutine jac
!       end interface
!       integer :: dim

!       type(ode_system) :: sys
!       sys % fun => fun
!       sys % jac => jac
!       sys % dim = dim
!    end subroutine ode_system_construct

! end module ode_system_module




program ode_system_test

  ! uzywamy modulu zdefiniowanego wyzej
  use ode_system_module_params

  integer :: dim = 1
  real :: t=0.0
  real :: y(1)=(/2.0/)
  real :: dydt(1)=(/3.0/)
  real :: dfdy(1), dfdt(1)

  type(ode_system) :: myode

  ! jakis typ - odpowiednik naszych parametrow
  type :: point
     real :: x,y
     integer :: direction
  end type point

  ! deklarujemy zmienna typu point
  type(point) :: punkt
  ! inicjalizujemy ja
  punkt % x = 0.
  punkt % y = 1.
  punkt % direction = -1


  call ode_system_construct2 (myode, myfun, myjac, dim, punkt)

  print *, 'przed wywolaniem prawych stron dydt=', dydt

  call myode % fun(t,y,dydt,punkt)

  print *, 'po wywolaniu prawych stron dydt=', dydt

contains
   ! deklaruje prawa strone rownan oraz jakobian
   subroutine myfun( t, y, dydt, p )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(out) :: dydt(:)
      class(point) :: p
      dydt(1)=2.0*y(1)
      print *, 'skladowe', p%x, p%y
      print *, 'kierunek', p%direction
   end subroutine myfun
   
   subroutine myjac( t, y, dfdy, dfdt, p )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(out) :: dfdy(:)
      real, intent(out) :: dfdt(:)
      class(point) :: p
      dfdy(1)=t
      dfdt(1)=t
   end subroutine myjac

end program ode_system_test
