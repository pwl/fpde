module utils_greens

contains

  ! calculates the discrete greens function for -D^2
  subroutine discrete_greens(array, L)
    real, intent(out) :: array(:,:)
    real, intent(in)  :: L
    integer :: nx, i, j
    real :: h
    nx = size(array,1)
    h = 1. / real(nx-1)

    forall(j = 1 : nx)
       forall(i = 1 : j )
          array(i,j) = 2.*L*( h*(i-1) )*(1. - h*(j-1))
       end forall
       forall(i = j+1 : nx)
          array(i,j) = 2.*L*( h*(j-1) )*(1. - h*(i-1))
       end forall
    end forall

  end subroutine discrete_greens


end module utils_greens
