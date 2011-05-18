module module_marcher

  type marcher
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
    class(marcher) :: m
    integer :: n



  end subroutine init

end module module_marcher
