module utils_inverse_greens

contains

  subroutine discrete_igf(array, L)
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

  end subroutine discrete_igf


end module utils_inverse_greens
