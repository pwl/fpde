module class_marcher_dummy

  use class_marcher

  private

  type, public, extends(marcher) ::  marcher_dummy
     contains
     procedure init
     procedure free
  end type marcher_dummy

contains

  subroutine init( m, n )
    class(marcher_dummy), intent(inout) :: m
    integer :: n
  end subroutine init

  subroutine free( m )
    class(marcher_dummy), intent(inout) :: m

  end subroutine free

end module class_marcher_dummy
