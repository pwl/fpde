! Testujemy moduly/klasy:
! ode_system, class_stepper, class_marcher
! 
! Rozwiazujemy rownanie drugiego rzedu
! y''[t] == -a y[t] + b,
! z warunkami poczatkowymi
! y[0] == 1, y'[0] == 0
! oraz dowolnymi prametrami rzeczywistymi a,b
! (a>0). Rozwiazanie analityczne spelniajace
! powyzsze warunki poczatkowe ma postac:
! y[t]=(b + a Cos[Sqrt[a] t] - b Cos[Sqrt[a] t])/a

program ode_system_test

   ! Dolaczamy niezbedne moduly/klasy
   use ode_system_module
   ! use class_stepper
   use class_marcher
   use class_stepper_rk4cs
   
   integer :: dim = 2
   real :: t=0.0, t1=10.0
   real :: h=0.01
   ! real :: y(2)=(/1.0,0.0/)
   real, allocatable :: y(:)
   
   real :: a=1.1, b=0.1

   ! real :: dydt(1)=(/3.0/)
   ! real :: dfdy(1), dfdt(1)

   type(ode_system) :: myode

   type(ode_stepper_rk4cs) :: mystepper

   type(marcher) :: mymarcher

   ! parametry rownania
   type :: paramsab
      real :: a,b
   end type paramsab

   ! deklarujemy parametry typu myparams
   type(paramsab) :: myparams
   ! inicjalizujemy
   myparams % a = a
   myparams % b = b

   allocate( y( 2 ) )
   y(1) = 1.0
   y(2) = 0.0

   ! inicjalizujemy stepper
   call mystepper % init( dim )

   ! inicjalizujemy marcher
   call mymarcher % init( dim )
   
   ! konstruujemy/inicjalizujemy ode_system
   ! call ode_system_construct( myode, myfun, dim, myparams )
   call ode_system_construct( sys=myode, fun=myfun, dim=dim, params=myparams )
   
   do while (t<t1)
      ! wolamy marcher % apply
      call mymarcher % apply( mystepper, myode, t, t1, h, y )
      ! print *, y
      print *, ""
      print *, y(1)-(b + a*cos(sqrt(a)*t)- b*cos(sqrt(a)*t))/a
      print *, y(2)-((-a + b)*sin(sqrt(a)*t))/sqrt(a)
      print *, ""
   end do
   
   ! zwalniamy stepper
   call mystepper % free ()

   ! zwalniamy marcher
   call mymarcher % free ()

contains
   ! Deklaruje prawa strone rownan
   subroutine myfun( t, y, dydt, params, status )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(inout) :: dydt(:)
      class(paramsab) :: params
      integer, optional :: status

      dydt(1) = y(2)
      dydt(2) = -y(1)*params%a + params%b

      if ( present( status ) ) then
         status = 1
      end if
   end subroutine myfun
   
   ! oraz jakobian
   subroutine myjac( t, y, dfdy, dfdt, params )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(inout) :: dfdy(:)
      real, intent(inout) :: dfdt(:)
      class(paramsab) :: params
   end subroutine myjac

end program ode_system_test
