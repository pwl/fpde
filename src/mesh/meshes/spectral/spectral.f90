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

CONTAINS 

 SUBROUTINE info(m)
 class(mesh_spectral), intent(inout) :: m
 if(m%stat.eq.0) stop 'mesh_spectral: Initialize/tweak the mesh first!'
 
 call m % mesh % info
 
 print *,'Left boundary at point x(nx) with value: ',m % x (m%nx)
 print *,'Right boundary at point x(1) with value: ',m % x (1)
 print *,'Smallest distance on grid (near boundaries): ', m % dx
 print *,'Capable of computing only first derivative. '
 if(m%stat .eq. 1.or.m%stat.eq.2) print*,'Derivatives being computed using FFT O(n ln n)'
 if(m%stat .eq. 3) print *,'Derivatives being computed using MATRIX MULTIPLICATION O(n*n) '
 if(m%stat .gt. 1) print*,'Capable of applying Von Neumann boundary conditions'
 if(m%coeff_cmpt) print*,'Coefficients of Czebyshev expansion are being stored in coeffs(nx,nf)'
 
 END SUBROUTINE info

 SUBROUTINE tweak(m, stat, coeff)
    class(mesh_spectral), intent(inout) :: m
    integer, intent(in) :: stat
    logical, intent(in) :: coeff
    if(m%stat.eq.0) stop 'mesh_spectral: Initialize the mesh first'
    
    m % stat = stat
    m % coeff_cmpt = coeff
    
    
    if(allocated(m%work_FFT)) deallocate(m%work_FFT)
    
    if(stat.gt.1) then ! we need work matrix
            allocate(m%work_matrix(m%nx,m%nx))
            call d1spectral_matrix_init(m%nx,m%xlob,m%work_matrix)
    end if
    if(stat.lt.3) then 
              allocate(m%work_FFT(8*m%nx+12))
              call d1spectral_fft_init(m%nx,m%work_FFT)
    end if
    if (coeff) allocate(m%coeffs(2*m%nx-1,m%nf))
    
 END SUBROUTINE tweak

 
 SUBROUTINE init(m, nx, nf, maxrk, xmin, xmax)
    class(mesh_spectral), intent(inout) :: m
    integer, intent(in) :: nx,nf,maxrk
    real, intent(in) :: xmin, xmax
    integer :: i,j
    
    
    call m % mesh % init( nx, nf, maxrk, xmin, xmax)
    m % stat = 1
    m % coeff_cmpt = .false.
    m % name = 'Gauss-Lobatto'     
    allocate(m%xlob(nx))
    call generuj_GL(nx,m%xlob)
    call generuj_GL_przesuniete(nx,m%x,xmin,xmax)
    allocate(m%work_FFT(8*m%nx+12))
    call d1spectral_fft_init(m%nx,m%work_FFT)
    
    m%dx = m%x(1)-m%x(2) ! metoda 1
    m%factor = 2.0/(m%x(1)-m%x(m%nx))
  END SUBROUTINE init
  
  SUBROUTINE free(m)
    class(mesh_spectral), intent(inout) :: m
    
    if(allocated(m%xlob)) deallocate(m%xlob)
    if(allocated(m%work_FFT)) deallocate(m%work_FFT)

    if(allocated(m%coeffs)) deallocate(m%coeffs)
    if(allocated(m%work_matrix)) deallocate(m%work_matrix)
    
    call m % mesh % free
    
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
  SUBROUTINE calculate_derivatives( m, i )
    class(mesh_spectral), target, intent(inout) :: m
    double complex :: cff(2*m%nx-1)
    integer,intent(in) :: i
    integer :: j
   if (m % stat .eq. 0) stop 'mesh_spectral: Initialize the mesh first'    
   if (i .gt. 1) stop 'mesh_spectral: Only first derivative availible so far...'
   
   if (.not. m%coeff_cmpt) then
    if (m%stat .lt. 3) then
     do j=1,m % nf
      call d1spectral_fft(m % nx,m % xlob,m % f(:,j) , m % df(:,j,1),m % work_FFT, cff)
      m%df(:,j,1) = m%df(:,j,1)*m % factor
     end do
    else
     do j=1,m % nf
      call d1spectral_matrix(m % nx,m % f(:,j) , m % df(:,j,1),m % work_matrix, cff)
      m%df(:,j,1) = m%df(:,j,1)*m % factor
     end do
   end if
  else 
    if (m%stat .lt. 3) then
     do j=1,m % nf
      call d1spectral_fft(m % nx,m % xlob,m % f(:,j) , m % df(:,j,1), m % work_FFT, m % coeffs(1,j))
    
      m%df(:,j,1) = m%df(:,j,1)*m % factor
     end do
    else
     do j=1,m % nf
      call d1spectral_matrix(m % nx,m % f(:,j) , m % df(:,j,1),m % work_matrix, m % coeffs(1,j))
      m%df(:,j,1) = m%df(:,j,1)*m % factor
     end do
   end if
  end if 
  if(m%coeff_cmpt) m%coeffs = m%coeffs/(2*m%nx-2) ! normalizacja
  
  END SUBROUTINE calculate_derivatives


  SUBROUTINE neumann(m,pos,val,fun)
    class(mesh_spectral), target, intent(inout) :: m
    integer,intent(in) :: pos,fun
    real, intent(in) :: val
    call d1spectral_neuman(m%nx,pos,m%f(pos,fun),val,m%work_matrix)
  END SUBROUTINE neumann

END MODULE module_mesh_spectral