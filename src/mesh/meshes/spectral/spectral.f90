!
!
! Generic mesh with spectral spatial derivatives
!
!                  Marek Lipert 2011
!
!  Body of most subroutines located in utils/derivs.f90 and utils/utils.f90
!  Uses DFFTPACK located in utils/dfftpack  
!
! ************************************************
!
!                     BEWARE ! ! !
!
!  This grid has nonstandard order, as x(1) = xmax, x(nx) = xmin
!
  






MODULE module_mesh_spectral
use mesh_module

PRIVATE

TYPE, public, extends( mesh ) :: mesh_spectral
! user accessible
  
  real :: dx                                       ! smallest distance between the nodes
  DOUBLE COMPLEX,ALLOCATABLE :: coeffs(:,:)        ! Czebyszev coefficients (might be needed) 
  
! internal

  DOUBLE COMPLEX,ALLOCATABLE :: work_FFT(:)        ! scratch for FFT
  real, ALLOCATABLE :: work_matrix(:,:)            ! scratch matrix for neumann IVP and/or matrix computations 
  real, ALLOCATABLE :: xlob(:)                     ! generic GL grid.  
  real :: factor                                   ! output from generic deriv functions is multiplied by this factor
  integer :: stat = 0                              ! 0 = uninitialized 1 = FFT, 2 = FFT + neumann, 3 = matrix + neumann 
  logical :: coeff_cmpt                            ! true - coeffs are stored, false - coeffs are not stored
  
  CONTAINS
  
  ! standard implementation
  procedure :: init
  procedure :: calculate_derivatives
  procedure :: derivative
  procedure :: free
  procedure :: info
  
  ! extensions
  procedure :: tweak
  !                  tweak(stat,coeffs) - tweaks the default mesh behaviour. It is not necessary to run it.
  !                                        
  !                  
  !                  coeffs : .true. or .false. : tells the mesh wheather to store/not store (and allocate) 
  !                                               Czebyshev coefficients in table 'coeffs'
  !                                                                                        default: .false.
  !                  stat   : Choose the method and capabilites of the mesh
  !                              1 - FFT computation ( O(n ln n) ), neumann BV impossible
  !                              2 - FFT computation ( O(n ln n) ), neumann BV possible
  !                              3 - MATRIX multiplication computation ( O(n*n)), neumann BV possible
  !                                                                                        default: 1
  
  procedure :: neumann
  !                   neumann(pos,val,fun) - sets neumann condition by setting the value of f(pos,fun) so that df(pos,fun) = val
  !                                          O(n)
  
  
END TYPE mesh_spectral

 SUBROUTINE info(s)
 class(mesh_spectral), intent(inout) :: s
 if(s%stat.eq.0) stop 'mesh_spectral: Initialize/tweak the mesh first!'
 
 call s % mesh % info
 
 print *,'Left boundary at point x(nx) with value: ',s % x (s%nx)
 print *,'Right boundary at point x(1) with value: ',s % x (1)
 print *,'Smallest distance on grid (near boundaries): ', s % dx
 print *,'Capable of computing only first derivative. '
 if(s%stat .eq. 1.or.s%stat.eq.2) print*,'Derivatives being computed using FFT O(n ln n)'
 if(s%stat .eq. 3) print *,'Derivatives being computed using MATRIX MULTIPLICATION O(n*n) '
 if(s%stat .gt. 1) print*,'Capable of applying Von Neumann boundary conditions'
 if(s%coeff_cmpt) print*,'Coefficients of Czebyshev expansion are being stored in coeffs(nx,nf)'
 
 END SUBROUTINE info

 SUBROUTINE tweak(s, stat, coeff)
    class(mesh_spectral), intent(inout) :: s
    integer, intent(in) :: stat
    logical, intent(in) :: coeff
    if(s%stat.eq.0) stop 'mesh_spectral: Initialize the mesh first'
    
    s % stat = stat
    s % coeff_cmpt = coeff
    
    
    if(allocated(s%work_FFT)) deallocate(s%work_FFT)
    
    if(stat.gt.1) then ! we need work matrix
            allocate(s%work_matrix(s%nx,s%nx))
            call d1spectral_matrix_init(s%nx,s%xlob,s%work_matrix)
    end if
    if(stat.lt.2) then 
              allocate(s%work_FFT(8*s%nx+12))
              call d1spectral_fft_init(s%nx,s%work_FFT)
    end if
    if (coeff) allocate(coeffs(s%nx,s%nf))
    
 END SUBROUTINE tweak

 
 SUBROUTINE init(s, nx, nf, maxrk, xmin, xmax)
    class(mesh_spectral), intent(inout) :: s
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i,j
    
    
    call s % mesh % init( nx, nf, maxrk, xmin, xmax)
    s % stat = 1
    s % coeff_cmpt = .false.
    s % name = 'Gauss-Lobatto'     
    allocate(s%xlob(nx))
    call generuj_GL(nx,s%xlob)
    call generuj_GL_przesuniete(nx,s%x,xmin,xmax)
    allocate(s%work_FFT(8*s%nx+12))
    call d1spectral_fft_init(s%nx,s%work_FFT)
    
    s%dx = s%x(1)-s%x(2) ! metoda 1
    s%factor = 2.0d0/(s%x(1)-s%x(n))
  END SUBROUTINE init
  
  SUBROUTINE free(s)
    class(mesh_spectral), intent(inout) :: s
    
    if(allocated(s%xlob)) deallocate(s%xlob)
    if(allocated(s%work_FFT)) deallocate(s%work_FFT)

    if(allocated(s%coeffs)) deallocate(s%coeffs)
    if(allocated(s%work_matrix)) deallocate(s%work_matrix)
    
    call s % mesh % free
    
  END SUBROUTINE free     
     
  FUNCTION derivative( m, i, j, k )
    class(mesh_spectral), intent(inout) :: m
    integer, intent(in) :: i,j,k
    real :: derivative
    if(  m%check_derivatives(k) ) then
       derivative = m % df( i, j, k )
       return 
    end if

    call m % calculate_derivatives( k )
    derivative = m % df( i, j, k )

  END FUNCTION derivative

  ! calculates i-th derivative of all functions at all points
  SUBROUTINE calculate_derivatives( s, i )
    class(mesh_spectral), target, intent(inout) :: s
    double complex :: cff(s%nx)
    integer :: i
   if (s % stat .eq. 0) stop 'mesh_spectral: Initialize the mesh first'    
   if (i .gt. 1) stop 'mesh_spectral: Only first derivative availible so far...'
   
   if (s%coeff_cmpt) then
    if (s%stat .lt. 3) then
     do i=1,s % nf
      call d1spectral_fft(s % nx,s % xlob,s % f(:,i) , s % df(:,i),s % work_FFT, cff)
      s%df(:,i) = s%df(:,i)*s % factor
     end do
    else
     do i=1,s % nf
      call d1spectral_matrix(s % nx,s % f(:,i) , s % df(:,i),s % work_matrix, cff)
      s%df(:,i) = s%df(:,i)*s % factor
     end do
   end if
  else 
    if (s%stat .lt. 3) then
     do i=1,s % nf
      call d1spectral_fft(s % nx,s % xlob,s % f(:,i) , s % df(:,i),s % work_FFT, s % coeffs(:,i))
      s%df(:,i) = s%df(:,i)*s % factor
     end do
    else
     do i=1,s % nf
      call d1spectral_matrix(s % nx,s % f(:,i) , s % df(:,i),s % work_matrix, s % coeffs(:,i))
      s%df(:,i) = s%df(:,i)*s % factor
     end do
   end if
  end if 
  END SUBROUTINE calculate_derivatives


  SUBROUTINE neumann(s,pos,val,fun)
    class(mesh_spectral), target, intent(inout) :: s
    integer,intent(in) :: pos,fun
    real, intent(in) :: val
    call d1spectral_neuman(s%nx,pos,s%f(pos,fun),val,s%work_matrix)
  END SUBROUTINE neumann

END MODULE module_mesh_spectral