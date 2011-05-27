module unlimited_polymorphic_module

  ! interfejs dla funkcji przyjmujacej class(*) czyli odpowiednik void *
  abstract interface
     subroutine test_up_interface( a )
       class(*) :: a
     end subroutine test_up_interface
  end interface

contains

  ! funkcja, ktora przyjmuje jako argumenty funkcje typu
  ! test_up_interface i wywoluje ja od argumentu typu class(*)
  ! (analogicznie trzeba zaimplementowac marcher
  subroutine test_up_sub( f, a )
    procedure( test_up_interface ) :: f
    class(*) :: a

    call f(a)

  end subroutine test_up_sub

end module unlimited_polymorphic_module

! czesc napisana przez uzytkownika
program unlimited_polymorphic

  ! uzywamy modulu zdefiniowanego wyzej
  use unlimited_polymorphic_module

  ! jakis typ - odpowiednik naszych parametrow
  type :: point
     real :: x,y
  end type point

  ! deklarujemy zmienna typu point
  type(point) :: p
  ! inicjalizujemy ja
  p % x = 0.
  p % y = 1.

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

    ! robimy cos, juz z tym konkretnym typem
    print *, "wewnatrz rhs"
    print *, "p to class(point), jego skladowe to: ",  p%x, p%y

  end subroutine rhs1

  subroutine rhs2( p )
    class(*) :: p

    ! z ogolnym typem nie mozna nic zrobic
    print *, "wewnatrz rhs2"
    print *,"typ to class(*), nie mozna z nim nic zrobic"

  end subroutine rhs2

  ! ta funkcja reinterpretuje p jako wskaznik do zmiennej typu real
  subroutine rhs3( p )
    real, pointer :: p

    print *, "wewnatrz rhs3"
    print *, "teraz p jest rzeczywiste: ", p

  end subroutine rhs3



end program unlimited_polymorphic
