! ************************************************************************
!
!                
! 
!         Metody wyliczania pochodnych przestrzennych dla pde
!
!
!                           Marek Lipert 2011
!
! ************************************************************************






!*********************************************************************
!     Wyliczenie pierwszej pochodnej metoda 5-punktowa centrowana FDA
!     O(h^4) na rownomiernie rozlozonej siatce. Przy brzegach pochodna liczona 
!            jednostronnie schematem O(h^5) (szesciopunktowym)
!
!     N - ilosc punktow na siatce
!    dx - stala siatki
!     u - tablica wartosci funkcji ktorej pochodna liczymy
!    du - tablica w ktorej ma byc przechowany wynik
!
!*********************************************************************
      subroutine d1cent5s(N,dx,u,du)
      implicit none
      integer, intent(in) :: N
      double precision,intent(in)  :: dx
      double precision, dimension(N), intent(in) :: u
      double precision, dimension(N), intent(out) :: du

      double precision, parameter :: a1=-1.0d0/5.0d0
      double precision, parameter :: a2=5.0d0/4.0d0
      double precision, parameter :: a3=-10.0d0/3.0d0
      double precision, parameter :: a4=5.0d0
      double precision, parameter :: a5=-5.0d0
      double precision, parameter :: a6=137.0d0/60.0d0
      
      double precision, parameter :: b1=1.0d0/12.0d0
      double precision, parameter :: b2=-2.0d0/3.0d0
      double precision, parameter :: b3=0.0d0
      double precision, parameter :: b4=2.0d0/3.0d0
      double precision, parameter :: b5=-1.0d0/12.0d0

      double precision, parameter :: c1=1.0d0/20.0d0
      double precision, parameter :: c2=-1.0d0/3.0d0
      double precision, parameter :: c3=1.0d0
      double precision, parameter :: c4=-2.0d0
      double precision, parameter :: c5=13.0d0/12.0d0
      double precision, parameter :: c6=1.0d0/5.0d0

      du(1)=-a6*u(1)-a5*u(2)-a4*u(3)-a3*u(4)-a2*u(5)-a1*u(6)
      du(2)=-c6*u(1)-c5*u(2)-c4*u(3)-c3*u(4)-c2*u(5)-c1*u(6)
      
      du(3:N-2) = b1*u(1:N-4)+b5*u(5:N) + & 
             b2*u(2:N-3)+b4*u(4:N-1) +b3*u(3:N-2) 

      du(N-1) = c1*u(N-5)+c2*u(N-4)+c3*u(N-3)+& 
           c4*u(N-2)+c5*u(N-1)+c6*u(N)
           
      du(N)=a1*u(N-5)+a2*u(N-4)+a3*u(N-3)+& 
           a4*u(N-2)+a5*u(N-1)+a6*u(N)
      du=du/(dx)
      
    end subroutine d1cent5s


!*********************************************************************
!     Wyliczenie pierwszej pochodnej metoda 3-punktowa centrowana FDA
!     O(h^2) na rownomiernie rozlozonej siatce. Na brzegach liczona
!            schematem O(h^3) (czteropunktowym)
!
!     N - ilosc punktow na siatce
!    dx - stala siatki
!     u - tablica wartosci funkcji ktorej pochodna liczymy
!    du - tablica w ktorej ma byc przechowany wynik
!

!*********************************************************************
      subroutine d1cent3s(N,dx,u,du)
      implicit none
      integer, intent(in) :: N
      double precision,intent(in)  :: dx
      double precision, dimension(N), intent(in) :: u
      double precision, dimension(N), intent(out) :: du

      double precision, parameter :: b1=1.0d0/12.0d0
      double precision, parameter :: b2=-2.0d0/3.0d0
      double precision, parameter :: b3=0.0d0
      double precision, parameter :: b4=2.0d0/3.0d0
      double precision, parameter :: b5=-1.0d0/12.0d0

      du(1)=(-11.0d0*u(1))/6.0d0 + 3.0d0*u(2) - (3.0d0*u(3))/2.0d0 + u(4)/3.0d0
      du(2:N-1) = (u(3:N)-u(1:N-2))/2.0d0
      du(N)=-u(-3 + N)/3.0d0 + (3.0d0*u(-2 + N))/2.0d0 - 3.0d0*u(-1 + N) + (11.0d0*u(N))/6.0d0           
      du=du/(dx)

    end subroutine d1cent3s


!*********************************************************************
!     Wyliczenie pierwszej pochodnej metoda 5-punktowa centrowana FDA
!     O(h^4) na rownomiernie rozlozonej siatce. Przy brzegach pochodna liczona 
!            jednostronnie schematem O(h^4) (pieciopunktowym)
!
!     N - ilosc punktow na siatce
!    dx - stala siatki
!     u - tablica wartosci funkcji ktorej pochodna liczymy
!    du - tablica w ktorej ma byc przechowany wynik
!
!*********************************************************************

      subroutine d1cent5(N,dx,u,du)
      implicit none
      integer, intent(in) :: N
      double precision,intent(in)  :: dx
      double precision, dimension(N), intent(in) :: u
      double precision, dimension(N), intent(out) :: du

      double precision, parameter :: b1=1.0d0/12.0d0
      double precision, parameter :: b2=-2.0d0/3.0d0
      double precision, parameter :: b3=0.0d0
      double precision, parameter :: b4=2.0d0/3.0d0
      double precision, parameter :: b5=-1.0d0/12.0d0

      du(1)=(-25.0d0*u(1))/12.0d0 + 4.0d0*u(2) - 3.0d0*u(3) + (4.0d0*u(4))/3.0d0 - u(5)/4.0d0
      du(2)=-u(1)/4.0d0 - (5.0d0*u(2))/6.0d0 + (3.0d0*u(3))/2.0d0 - u(4)/2.0d0 + u(5)/12.0d0
      
      du(3:N-2) = b1*u(1:N-4)+b5*u(5:N) + & 
             b2*u(2:N-3)+b4*u(4:N-1) +b3*u(3:N-2) 

      du(N-1) = -u(-4 + N)/12.0d0 + u(-3 + N)/2.0d0 - (3.0d0*u(-2 + N))/2.0d0 + (5.0d0*u(N-1))/6.0d0 & 
         + u( N)/4.0d0
           
      du(N)= u(-4 + N)/4.0d0 - (4.0d0*u(-3 + N))/3.0d0 + 3.0d0*u(-2 + N) - 4.0d0*u(-1 + N)& 
       + (25.0d0*u(N))/12.0d0
           
      du=du/(dx)
      
    end subroutine d1cent5


!*********************************************************************
!     Wyliczenie pierwszej pochodnej metoda 3-punktowa centrowana FDA
!     O(h^2) na rownomiernie rozlozonej siatce. Na brzegach liczona
!            schematem O(h^2) (trzypunktowym)
!
!     N - ilosc punktow na siatce
!    dx - stala siatki
!     u - tablica wartosci funkcji ktorej pochodna liczymy
!    du - tablica w ktorej ma byc przechowany wynik
!

!*********************************************************************
      subroutine d1cent3(N,dx,u,du)
      implicit none
      integer, intent(in) :: N
      double precision,intent(in)  :: dx
      double precision, dimension(N), intent(in) :: u
      double precision, dimension(N), intent(out) :: du

      du(1)=(-3.0d0*u(1))/2.0d0 + 2.0d0*u(2) - u(3)/2.0d0
      du(2:N-1) = (u(3:N)-u(1:N-2))/2.0d0
      du(N)=u(-2 + N)/2.0d0 - 2.0d0*u(-1 + N) + (3.0d0*u(N))/2.0d0

      du=du/dx
    end subroutine d1cent3



! *****************************************************************************************
!
!    Inicjalizacja siatke GAUSS-LOBATTO dla wyliczen spektralnych 
!    
!    generuj_GL daje:
!                xlob(1) = 1.0d0
!                xlob(N) = -1.0d0
!
!    wobec czego siatka jest uporzadkowana odwrotnie
!
!    generuj_GL_przesuniete daje
!                x(1) = r
!                x(N) = l
!
!    ale wymaga, aby r>l wiec tez porzadek jest odwrocony
!
! *****************************************************************************************


subroutine generuj_GL_przesuniete(N,x,l,r)
implicit none
integer, intent(in) :: N
double precision, intent(out),dimension(N) :: x
double precision, intent(in) :: l,r
double precision ,parameter :: pi = 3.141592653589793238462643383279d0
double precision :: xlob
integer :: i
do i=1,N
  xlob = dcos((i-1)*pi/(N-1))
  x(i)=((xlob+1.0d0)/2.0d0)*(r-l)+l
end do
end subroutine generuj_GL_przesuniete


subroutine generuj_GL(N,xlob)
implicit none
integer, intent(in) :: N
double precision, intent(out),dimension(N) :: xlob
integer :: i
double precision ,parameter :: pi = 3.141592653589793238462643383279d0
do i=1,N
  xlob(i) = dcos((i-1)*pi/(N-1))
end do
end subroutine generuj_GL



! *****************************************************************************************
!
!    Inicjalizacja siatke rownomierna 
!    
!    generuj_STD daje:
!                x(1) = l
!                x(N) = r
!
!     wymaga, aby r>l 
!
! *****************************************************************************************



subroutine generuj_STD(N,x,l,r)
implicit none
integer, intent(in) :: N
double precision, intent(out),dimension(N) :: x
double precision, intent(in) :: l,r
integer :: i
double precision :: h
h = (r-l)/(N-1)
do i=1,N
x(i) = l+(i-1)*h
end do
end subroutine generuj_STD



! *****************************************************************************************
!
!    Inicjalizacja tablicy pomocniczej work dla d1spectral_fft (8*N+12 double complex)
!
! *****************************************************************************************
subroutine d1spectral_fft_init(N,work)
  implicit none
  integer, intent(in) :: N
  double complex, dimension(8*N+12), intent(out) :: work
   ! FFT init
 ! print *,'koko'
 ! print*, work(509)
  call zffti(2*N-2,work)
 ! print*,'bobo'
end subroutine d1spectral_fft_init


! *****************************************************************************************
! wyliczenie pochodnej za pomoca transformaty fouriera 
!
!    Wszystkie tablice zawieraja double precision jako podstawowy typ chyba ze jest napisane inaczej w komentarzu.
!
! N       --- ilosc punktow (i jednoczesnie istotnych wspolczynnikow) 
! y       --- tablica zawierajaca funkcje    
! x       --- punkty kolokacji gauss-lobato
! yx      --- wynikowy wektor z pochodnymi
! coeffs  --- wynikowy wektor typu double complex o rozmiarze 2*N-1
! work    --- tablica pomocnicza uzyskana z d1spectral_fft_init, rozmiar 8*N+12 double complex

! Uwaga - aby ta metoda byla wiarygodna, punkty kolokacji musza byc punktami gauss-lobato : x(i) = dcos(pi*(i-1)/(N-1))
!         co oczywiscie pociaga x(N) = -1, x(1) = 1 , x e <-1,1>
 
 

subroutine d1spectral_fft(N,x,y,yx,work,coeffs)
   implicit none
   integer, intent(in) :: N ! ilosc punktow siatki
   integer  :: NFFT
   double precision,dimension(N),intent(in) ::   x,y
   double precision,dimension(N),intent(out) ::  yx
   double complex, dimension(2*N-1),intent(inout) :: coeffs  ! wspolczynniki fouriera
   double complex, dimension(2*N-1) :: tempfft ! tymczasowy wektor complexow na wejsciu do fft
   double complex, dimension(4*(2*N-1)+16),intent(inout) :: work
   integer iret,i,j
   double complex :: toko,mnoznik
   double precision  :: a,b,s
   double precision ,parameter :: pi = 3.141592653589793238462643383279d0
   NFFT = 2*N-1
   ! FFT init
 ! call zffti(NFFT-1,work)
!   print*, 'w' 
   do i = 1,N
!      print *,i
      coeffs(i) = dcmplx(y(i),0.0d0)
   end do
      ! wielomiany czebyszewa Tn = dcos(n t) maja t od 0 do pi, my potrzebujemy od 0 do 2 pi, musimy sobie rozmnozyc kozystajac z tego, ze cos (pi-x) = cos (pi+x)
!    print *,'dalej'  
   do i=N+1,NFFT
      coeffs(i) = dcmplx(y(2*N-i),0.0d0)
   end do
  ! transformata fouriera FFT
  ! print *, 'przed'
   call zfftf(NFFT-1,coeffs,work)
  ! print *, 'po'  

! NFFT musi byc nieparzyste i zawsze bedzie!!!!
! Najpierw trzeba obsluzyc niewygodny przypadek - na granicach przedzialu jest "0/0" i pochodne funkcji bazowych trzeba wyliczyc z de l'Hospitala
    yx(1)=0.0d0
    yx(N)=0.0d0
    do i=1,N-1
      yx(N)=yx(N)+i*i*(-1.0d0)**(i+1)*2.0d0*dble(coeffs(i+1))
      yx(1)=yx(1)+i*i*2.0d0*dble(coeffs(i+1))
    end do
    yx(1) = yx(1)/dble(NFFT-1)
    yx(n) = yx(n)/dble(NFFT-1)

! Koniec wyliczania na krancach dziedziny
! Zmodyfikowanie wspolczynnikow aby wyliczyc pochodna
    tempfft = coeffs
   do j=1,(NFFT-1)/2   ! pochodna
     tempfft(j) = tempfft(j)* (0.0d0,1.0d0)*dble(j-1)
   end do
   do j=NFFT-1,(NFFT-1)/2+2,-1 !  pochodna dla aliasow
     tempfft(j) = tempfft(j)*(0.0d0,1.0d0)*dble(j - (NFFT-1) - 1)
   end do
   tempfft((NFFT-1)/2+1) = tempfft((NFFT-1)/2+1)*(0.0d0,1.0d0)*(dble(NFFT-1)/2.0d0)
   call zfftb(NFFT-1,tempfft,work)
   do i=2,N-1
     yx(i) =  -1.0d0/dsin((i-1)*pi/(N-1))/dble(NFFT-1)*tempfft(i) ! normalizacja + transformacja pochodnej
   end do
   
end subroutine d1spectral_fft








! **************************************************************************************************************************************
! Inicjalizacja metody macierzowej wyliczania pochodnych spektralnych
!
! N       --- ilosc punktow (i jednoczesnie istotnych wspolczynnikow) 
! x       --- punkty kolokacji gauss-lobato
!
! Uwaga - aby ta metoda byla 100% wiarygodna, punkty kolokacji musza byc punktami gauss-lobato : x(i) = dcos(pi*(i-1)/(N-1))
!         co oczywiscie pociaga x(N) = -1, x(1) = 1 , x e <-1,1>


subroutine d1spectral_matrix_init(N,x,work)
   implicit none
   integer, intent(in) :: N ! ilosc punktow siatki
   double precision,dimension(N),intent(in) ::   x
   double precision,dimension(N,N),intent(out) ::   work
   double precision, dimension(N,N) :: phi,phix
   double precision,dimension(N*N) :: kk
   integer :: i,j,k,INFO1,INFO2
   integer,dimension(N) :: IPIV

   do i=1,N ! przygotowanie macierzy pochodnych i macierzy wartosci
     call basis(X(i),N,PHI(1,i),PHIX(1,i),work(1,i))
   end do
   call DGETRF(N,N,PHI,N,IPIV,INFO1)
   call DGETRI(N,PHI,N,IPIV,kk,N*N,INFO2)
   if (INFO1.ne.0.or.INFO2.ne.0) then
     print *, 'grlib: Blad poszukiwania macierzy odwrotnej do macierzy wartosci (lapack: DGETRF, DGETRI): ',INFO1,INFO2
     call exit(1)
     end if
   ! phi trzyma teraz phi^(-1)
   call DGEMM('N','N',N,N,N,1.0d0,phi,N,phix,N,0.0d0,work,N)
   work = TRANSPOSE(work)
   ! w work mamy teraz poszukiwana macierz pochodnej
end subroutine d1spectral_matrix_init





! **************************************************************************************************************************************
! wyliczenie pochodnej za pomoca mnozenia macierzowego 
! N       --- ilosc punktow (i jednoczesnie istotnych wspolczynnikow) 
! y       --- tablica zawierajaca funkcje
! yx      --- wynikowy wektory z pochodnymi 
! work    --- macierz (N,N) pomocnicza, uzyskujemy ja przez d1spectral_matrix_init


subroutine d1spectral_matrix(N,y,yx,work)
   implicit none
   integer, intent(in) :: N ! ilosc punktow siatki
   double precision,dimension(N),intent(in) ::   y
   double precision,dimension(N),intent(out) ::  yx
   double precision,dimension(N,N),intent(in) ::   work
   double precision ,parameter :: pi = 3.141592653589793238462643383279d0
   call DGEMM('N','N',N,1,N,1.0d0,work,N,y,N,0.0d0,yx,N)
end subroutine d1spectral_matrix


! **************************************************************************************************************************************
! wyliczenie wartosci funkcji nieznanej z warunku neumanna
!
! N       --- ilosc punktow (i jednoczesnie istotnych wspolczynnikow) 
! y       --- tablica zawierajaca funkcje w ktorej y(pos) moze miec dowolna wartosc
! ypos    --- wyliczona wartosc y(pos) 
! pos     --- numer punktu w ktorym zadajemy warunek neumanna  
! work    --- macierz (N,N) pomocnicza, uzyskujemy ja przez d1spectral_matrix_init
! val     --- zadana wartosc pochodnej

subroutine d1spectral_neuman(N,y,pos,ypos,val,work)
   implicit none
   integer, intent(in) :: N ! ilosc punktow siatki
   integer, intent(in) :: pos
   double precision, intent(in) :: val
   double precision,dimension(N),intent(in) ::   y
   double precision,intent(out) ::  ypos
   double precision,dimension(N,N),intent(in) ::   work
   double precision ,parameter :: pi = 3.141592653589793238462643383279d0
   integer :: i,j,k
   double precision :: temp1,temp2
   temp2 = work(pos,pos)
   temp1 = 0.0d0
   do i=1,n
    if(i.ne.pos) temp1 = temp1 + work(pos,i)*y(i)
   end do
   ypos = (val-temp1)/temp2
end subroutine d1spectral_neuman




! **************************************************************************************************************************************
! 
!   Ponizsza funkcja oblicza wartosci funkcji bazowych w punkcie x c [-1,1]
!
! **************************************************************************************************************************************

subroutine basis(X,NBASIS,PHI,PHIX,PHIXX)
implicit none
integer,intent(IN) :: NBASIS
double precision, intent(OUT), dimension(NBASIS) :: PHI,PHIX,PHIXX
double precision,intent(IN) :: X
integer :: I,N
double precision :: T,C,S,TN,TNT,TNTT,TNx,TNXX
double precision,parameter :: pi = 3.141592653589793238462643383279d0
!if(abs(x).eq.1.0) then
!!print *,'hahahahaha'
!end if

! After this call, the arrays PHI, PHIX, and PHIXX contain the values of the basis functions (and their first two derivatives, respectively, at X. 
if (ABS(X).LT.1.0) THEN
	![block below is executed only on the interior. This IF branches to a later block to evaluate functions at x = .1.] [T is the argument of the trig. functions]
   T = dacos(X)
   C =  dcos(T) 
   S = dsin(T)
   
   do  I=1,NBASIS 
       N = I-1 
       TN = dcos(N*T) !                        [Trig. form of TN (x)]
       TNT = - N * dsin(N*T)  !    [Derivative of TN with respect to t]
       TNTT=-N*N*TN !                         [Second t-derivative of TN ]
	!Convert t-derivatives into x-derivatives 
       TNX = - TNT / S				![x-derivative of N-th Chebyshev polynomial.]
       TNXX= TNTT/(S*S) - TNT*C/(S*S*S)   	![Second x-derivative] Final	step:	convert	TN s	into	the	basis	functions	.N s.
                                	        !We subtract 1 from the even degree T N .s, x from the odd degree and 1 from the first derivative of the odd polynomials only
        if (MOD(N,2).EQ.0) THEN 
              PHI(I) = TN 
              PHIX(I) = TNX
        ELSE
	    PHI(I) = TN  
	    PHIX(I)= TNX 
        ENDIF 
       PHIXX(I) = TNXX
   end do
else
   do I=1,NBASIS

       N=I-1
       if(MOD(N,2).EQ.0) then
         PHI(I)=1.0d0
         PHIX(I)=SIGN(1.0d0,X)*N*N
       else
         PHIX(I)=N*N
         PHI(I)=1.0d0*SIGN(1.0d0,X)
       endif
       PHIXX(I) =  (SIGN(1.0d0,X))**N * N*N * (N*N-1.)/3.
   end do
end if
return
end subroutine basis

! **************************************************************************************************************************************
!
!  Ponizsza funkcja oblicza wartosci funkcji bazowych w punkcie x c [-1,1], przy czym sa to kombinacje liniowe ktore maja 
!
!                           leftright = -1 ==>   T(1)=0
!                           leftright =  1 ==>   T(-1) = 0 
!
! **************************************************************************************************************************************

subroutine basis_zero(X,NBASIS,PHI,PHIX,PHIXX,leftright)
implicit none
integer,intent(IN) :: NBASIS,leftright
double precision, intent(OUT), dimension(NBASIS) :: PHI,PHIX,PHIXX
double precision,intent(IN) :: X
integer :: I,N
double precision :: T,C,S,TN,TNT,TNTT,TNx,TNXX
double precision,parameter :: pi = 3.141592653589793238462643383279d0

! After this call, the arrays PHI, PHIX, and PHIXX contain the values of the basis functions (and their first two derivatives, respectively, at X. 
if (ABS(X).LT.1.0) THEN
	![block below is executed only on the interior. This IF branches to a later block to evaluate functions at x = .1.] [T is the argument of the trig. functions]
   T = dacos(X)
   C =  dcos(T) 
   S = dsin(T)
   
   do  I=1,NBASIS 
       N = I 
       TN = dcos(N*T)      !                        [Trig. form of TN (x)]
       TNT = - N * dsin(N*T)  !    [Derivative of TN with respect to t]
       TNTT=-N*N*TN !                         [Second t-derivative of TN ]
	!Convert t-derivatives into x-derivatives 
       TNX = - TNT / S				![x-derivative of N-th Chebyshev polynomial.]
       TNXX= TNTT/(S*S) - TNT*C/(S*S*S)   	![Second x-derivative] Final	step:	convert	TN s	into	the	basis	functions	.N s.
                                	        !We subtract 1 from the even degree T N .s, x from the odd degree and 1 from the first derivative of the odd polynomials only
        if (MOD(N,2).EQ.0) THEN 
              PHI(I) = TN - 1.0d0
              PHIX(I) = TNX
        ELSE
	    PHI(I) = TN  - 1.0d0*(-leftright)
	    PHIX(I)= TNX 
        ENDIF 
       PHIXX(I) = TNXX
   end do
else
  ! print *,'blackout'
   do I=1,NBASIS
     !  print *,I,'hahaha'
       N=I
       if(MOD(N,2).EQ.0) then
         PHI(I)=0.0d0
         PHIX(I)=SIGN(1.0d0,X)*N*N
       else
!       print *,'else'
         PHIX(I)=N*N
         PHI(I)=1.0d0*SIGN(1.0d0,X)-1.0d0*(-leftright)
       endif
!       print *,'przed phixx'
       PHIXX(I) =  (SIGN(1.0d0,X))**N * N*N * (N*N-1.)/3.
!       print *,'po phixx'
   end do
 !  print *,'blackout2'
end if
return
end subroutine basis_zero

