module utils_inverse_greens

contains

  subroutine discrete_igf(array)
    real, intent(out) :: array(:,:)
    integer :: nx, i, j
    real :: h
    nx = size(array,1)
    h = 1. / real(nx-1)

    forall(j = 1 : nx)
       forall(i = 1 : j )
          array(i,j) = ( h*(i-1) )*(1. - h*(j-1))
       end forall
       forall(i = j+1 : nx)
          array(i,j) = ( h*(j-1) )*(1. - h*(i-1))
       end forall
    end forall

  end subroutine discrete_igf


end module utils_inverse_greens
