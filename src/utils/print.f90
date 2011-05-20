module pretty_print

  character(len=*), parameter :: real_format = "3(f10.3)"

contains

  subroutine  pretty_print_matrix(matrix, rows)
    real :: matrix(:,:)
    integer :: rows(:)
    integer :: i
    
    
    do i = 1, size(matrix,1) 
       print *, matrix(i,rows)
    end do
    
  end subroutine pretty_print_matrix

  character function format_for_n_reals(n)

end module pretty_print
