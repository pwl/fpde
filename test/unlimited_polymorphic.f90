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

! program test_epsilon
!    real, parameter :: eps=epsilon(0.0)
!    real :: y(3)=(/1.0,eps,2.0/)
!    ! eps = epsilon(eps)
!    print *, eps
!    print *, abs(-1.2)
   
!    print *, minval(y)

! end program test_epsilon

program ode_system_test

   ! Dolaczamy niezbedne moduly/klasy
   use class_ode_system

   use class_ode_marcher
   use class_ode_stepper_rk4cs
   use class_ode_stepper_rkf45
   use class_ode_step_control_standard

   integer :: dim = 2
   real :: t=0.0, t1=10.0
   real :: h=0.1
   real :: y(2)=(/1.0,0.0/)

   real :: a=1.1, b=0.1

   type(ode_system) :: myode
   type(ode_stepper_rkf45) :: mystepper
   type(ode_marcher) :: mymarcher
   type(ode_step_control_standard) :: mycontrol

   ! parametry rownania
   type :: paramsab
      real :: a,b
   end type paramsab

   ! deklarujemy parametry typu myparams
   type(paramsab) :: myparams
   ! inicjalizujemy parametry
   myparams % a = a
   myparams % b = b

   ! inicjalizujemy stepper
   call mystepper % init( dim )

   ! inicjalizujemy marcher
   call mymarcher % init( dim )

   ! inicjalizujemy controler 
   call mycontrol % init ( eps_abs=1.0e-10, eps_rel=1.0e-10, a_y=0.5, a_dydt=0.5 ) 

   ! konstruujemy/inicjalizujemy ode_system
   ! call ode_system_construct( myode, myfun, dim, myparams )
   call ode_system_init( sys=myode, fun=myfun, dim=dim, params=myparams )

   do while (t<t1)
      ! Wolamy marcher % apply
      call mymarcher % apply( s=mystepper, c=mycontrol, sys=myode, t=t, t1=t1, h=h, y=y )
      
      ! Sprawdzamy starus marchera, jezeli jest rozny od 1
      ! wychodzimy z petli
      if ( mymarcher % status /= 1 ) then
         exit
      end if

      ! Drukuje roznice pomiedzy rozwiazaniem znalezionym
      ! numerycznie a znanym analitycznnie
      print *, t, h
      print *, y(1)-(b + a*cos(sqrt(a)*t)- b*cos(sqrt(a)*t))/a
      print *, y(2)-((-a + b)*sin(sqrt(a)*t))/sqrt(a)
      print *, ""

   end do

   ! Wyswietlamy informacje machera
   print *, ""
   print *, "marcher info"
   print *, "status: ", mymarcher % status
   print *, "dim: ", mystepper % dim
   print *, "count: ", mymarcher % count
   print *, "failed steps: ", mymarcher % failed_steps
   print *, "last_step: ", mymarcher % last_step
   print *, ""

   ! Wyswietlamy informacje steppera
   print *, ""
   print *, "stepper info"
   print *, "status: ", mystepper % status
   print *, "name: ", mystepper % name
   print *, "order: ", mystepper % method_order
   print *, "dim: ", mystepper % dim
   print *, "uses dydt in: ", mystepper % can_use_dydt_in
   print *, "gives dydt_out: ", mystepper % gives_exact_dydt_out
   print *, "estimates error: ", mystepper % gives_estimated_yerr
   print *, ""
   
   ! Wyswietlamy informacje kontrolera
   print *, ""
   print *, "step control info"
   print *, "status: ", mycontrol % status
   print *, "name: ", mycontrol % name

   ! zwalniamy stepper
   call mystepper % free ()

   ! zwalniamy controler
   call mycontrol % free ()

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

end program ode_system_test
