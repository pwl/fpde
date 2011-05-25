module class_marcher

  private

  type, public ::  marcher
     integer :: fn
     real :: t, dt
     real :: absolute_error_bound, relative_error_bound
     real, allocatable :: fout(:)
     real, allocatable :: df(:)
     ! class(stepper), pointer :: stepper
   contains
     procedure init
     procedure free
     procedure apply
  end type marcher

contains
  subroutine init( m, n )
    class(marcher), intent(inout) :: m
    integer :: n

  end subroutine init

  subroutine free( m )
    class(marcher), intent(inout) :: m

  end subroutine free

  subroutine apply( m, dt )
    class(marcher), intent(inout) :: m
    real, intent(inout) :: dt

  end subroutine apply

end module class_marcher
