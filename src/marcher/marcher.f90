module class_marcher

  private

  type, public ::  marcher
     integer :: fn
     real :: t, dt
     real, allocatable :: fout(:)
     real, allocatable :: df(:)
   contains
     procedure init
     ! procedure free
  end type marcher

contains
  subroutine init( m, n )
    class(marcher), intent(inout) :: m
    integer :: n
  end subroutine init

  subroutine free( m )
    class(marcher), intent(inout) :: m
    
  end subroutine free
  

end module class_marcher
