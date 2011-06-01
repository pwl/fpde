
program ode_system_test

  ! uzywamy modulu
  use ode_system_module

  integer :: dim = 1
  real :: t=0.0
  real :: y(1)=(/2.0/)
  real :: dydt(1)=(/3.0/)
  real :: dfdy(1), dfdt(1)

  type(ode_system) :: myode

  ! jakis typ - odpowiednik naszych parametrow
  type :: line
     real :: x,y
     integer :: direction
  end type line

  ! deklarujemy zmienna typu point
  type(line) :: myline
  ! inicjalizujemy ja
  myline % x = 0.
  myline % y = 1.
  myline % direction = -1

<<<<<<< HEAD
  ! wywolujemy test_up_sub od zdefiniowanej przez nas funkcji rhs i
  ! konkretnego juz typu point. z definicji ponizej p bedzie
  ! interpretowane wewnatrz rhs jako point a wewnatrz test_up_sub jako
  ! class(*)
  call test_up_sub( rhs1, p )

  ! podajemy inna funkcje definiujaca prawa strone, tym razem
  ! ignorujaca p
  call test_up_sub( rhs2, p )

  ! w tym przypadku cos nie dziala
  call test_up_sub( rhs3, 1.)

contains

  ! definicja rhs - zamiast class(*) podajemy interesujacy nas
  ! typ class(point)
  subroutine rhs1( p )
    class(point) :: p
=======

  call ode_system_construct (myode, myfun, myjac, dim, myline)
>>>>>>> e7891d70c1d08f4d49503459fc4d1106daf93cd6

  print *, 'przed wywolaniem prawych stron dydt=', dydt

<<<<<<< HEAD
  end subroutine rhs1
=======
  call myode % fun(t,y,dydt,myline)
>>>>>>> e7891d70c1d08f4d49503459fc4d1106daf93cd6

  print *, 'po wywolaniu prawych stron dydt=', dydt

contains
   ! deklaruje prawa strone rownan oraz jakobian
   subroutine myfun( t, y, dydt, params )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(out) :: dydt(:)
      class(line) :: params
      dydt(1)=2.0*y(1)
      print *, 'skladowe', params%x, params%y
      print *, 'kierunek', params%direction
   end subroutine myfun
   
   subroutine myjac( t, y, dfdy, dfdt, params )
      real, intent(in) :: t
      real, intent(in) :: y(:)
      real, intent(out) :: dfdy(:)
      real, intent(out) :: dfdt(:)
      class(line) :: params
      dfdy(1)=t
      dfdt(1)=t
   end subroutine myjac

end program ode_system_test
