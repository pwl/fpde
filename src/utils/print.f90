module pretty_print

  character(len=*), parameter :: real_format = "f"

contains

  character(len=30) function format_for_n_reals(n)
    integer :: n,i
    format_for_n_reals = ' '

    write (format_for_n_reals, '(a,i0,a,a)'), '(', n, real_format, ')'

  end function format_for_n_reals
  

end module pretty_print
