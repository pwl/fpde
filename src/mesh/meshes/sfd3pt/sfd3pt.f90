module class_mesh_sfd3pt
  use class_mesh
  use pretty_print

  ! everything except the type should be private
  private

  type, public, extends( mesh ) :: mesh_sfd3pt
     real :: h = 0.
   contains
     ! overloaded procedures go here (if needed)
     procedure :: init
     procedure :: derivative
     procedure :: calculate_derivatives
  end type mesh_sfd3pt

contains

  subroutine init( m )
    class(mesh_sfd3pt), intent(inout) :: m
    integer :: i,j

    call m % mesh % init

    call set_string_if_empty( m % name, "sfd3pt")

    m % h = (m % x1 - m % x0)/(m % nx - 1)

    ! setup a uniform grid
    forall( i = 1 : m % nx )
       m % x(i) = m % x1 + (i-1) * m % h
    end forall

  end subroutine init

  function derivative( m, k, j, i ) result(d)
    class(mesh_sfd3pt), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: d

    if( .not. m%check_derivatives(k) ) then
       return
    end if

    call m % calculate_derivatives( k )
    d = m % df( k, j, i )

  end function derivative

  recursive subroutine calculate_derivatives( m, i )
    class(mesh_sfd3pt), target, intent(inout) :: m
    integer, intent(in) :: i
    integer :: j,k
    real, pointer :: f(:,:)


    if( i > 1) then
       call m % calculate_derivatives(i-1)
       f => m % df( :, : , i-1)
    else
       f => m % f
    end if

    forall( j = 1 : m % nf, &
            k = 2 : m % nx - 1 )
       m%df(k,j,i)=(f(k+1,j)-f(k-1,j))/m%h/2.
    end forall

    forall( j = 1 : m % nf )
       m%df(1,   j,i)=(f(2,   j)-f(1,     j))/m%h
       m%df(m%nx,j,i)=(f(m%nx,j)-f(m%nx-1,j))/m%h
    end forall

  end subroutine calculate_derivatives


end module class_mesh_sfd3pt
