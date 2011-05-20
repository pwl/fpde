! ************************************************************************
!
!                
! 
!         Funkcje pomocnicze i rozwiazywanie rownan zwyczajnych
!
!
!                           Marek Lipert 2011
!
! ************************************************************************


! umozliwia dodawanie ciagow znakow przy pomocy +

MODULE stringplus 

INTERFACE OPERATOR (+)

MODULE PROCEDURE concat


END INTERFACE

CONTAINS 
FUNCTION concat(cha, chb)
CHARACTER (LEN=*), INTENT(IN) :: cha, chb 
CHARACTER (LEN=LEN_TRIM(cha) +  LEN_TRIM(chb)) :: concat
concat = TRIM(cha) // TRIM(chb) 
END FUNCTION concat

END MODULE stringplus





subroutine wstrzyknij(x1,x2,y1,y2,N1,N2)
implicit none
integer, intent(in) :: N1,N2
double precision, intent(in),dimension(N1) :: x1,y1
double precision, intent(in),dimension(N2) :: x2
double precision, intent(out),dimension(N2) :: y2
double precision,dimension(N1) :: work
integer :: i

call spline(x1,y1,N1,0.0d0,1.0d30,work)

do i=1,N2
  call splint(x1,y1,work,N1,x2(i),y2(i))
end do
end subroutine wstrzyknij








! **********************************************
!  
!      Przykladowa funkcja ff dla rk4solve
!
!      subroutine aderiv(da,x,a,b1,b2,b3)
!      implicit none
!      double precision, intent(in) :: x,a,b1,b2,b3
!      double precision, intent(out) :: da
!
!      da = a*b1+b2*x-b3
!
!      end subroutine aderiv
!
! **********************************************



!************************************************************************
!     procedura numerycznego calkowania rownania 4-punktowa metoda R-K
!     pozyskano z kodu Tadeusza Chmaja oraz Numerical Recipes
!
!     bfun - procedura obliczania pochodnych
!     x(n) - zadany wektor zmiennej niezaleznych
!     y(n,nfun) - wyliczany wektor rozwiazania
!     y0(nfun) - zadana wartosc funkcji na brzegu
!     ekstra(n,nekstra) - zadane pomocnicze wektory 
!                         poprzez procedure ff
!     n   - ilosc punktow
!     dir - stala liczbowa okreslajaca sposob calkowania :
!          dir =  1   -  calkowanie od 1 do n, y0=y(1),
!          dir = -1   -  calkowanie od n do 1, y0=y(n)
!************************************************************************
      subroutine ODE_rk4(nfun,fun,n,x,y,y0,nekstra,ekstra,dir)
       implicit none
       external fun
       integer,intent(in) :: N,dir,nfun,nekstra
       integer :: i,ks,ke,j
       double precision, dimension(nfun),intent(in) :: y0
       double precision,intent(in),dimension(N) :: x
       double precision, intent(out), dimension(N,nfun) :: y
       double precision, intent(in), dimension(N,nekstra) :: ekstra
       double precision, dimension(N,nekstra) :: ekstra2
       double precision, dimension(nekstra) :: ekstra3
       double precision, dimension(nfun) :: dydx,yt,dyt,dym
       
       double precision :: h,hh,h6,xh,dx
      if (dir==1) then
       ks=1
       ke=N-1
       y(1,:)=y0
      else
       ks=N
       ke=2
       y(N,:)=y0
      endif
      dx = x(2)-x(1)
      h=dir*dx
      hh=dir*dx/2.0
      h6=dir*dx/6.0
      
      do i=1,nekstra
       call spline(x,ekstra(:,i),N,0.0d0,1.0d30,ekstra2(:,i))
      end do
      
      do i=ks,ke,dir
         xh=x(i)+hh
         do j=1,nekstra
           call splint(x,ekstra(:,j),ekstra2(:,j),N,xh,ekstra3(j))
         end do
         call fun(dydx,x(i),y(i,:),nekstra,ekstra(i,:))
         yt=y(i,:)+hh*dydx
         call fun(dyt,xh,yt,nekstra,ekstra3)
         yt=y(i,:)+hh*dyt
         call fun(dym,xh,yt,nekstra,ekstra3)
         yt=y(i,:) +h*dym
         dym=dyt+dym
         call fun(dyt,x(i+dir),yt,nekstra,ekstra(i+dir,:))
         y(i+dir,:)=y(i,:)+h6*(dydx+dyt+2.0d0*dym)
      end do
      
      end subroutine ODE_rk4

!************************************************************************
!     inicjalizacja splinow - procedura SPLINE z Numerical Recipies
!************************************************************************
      SUBROUTINE SPLINE(X,Y,EN,YP1,YPN,Y2)
      implicit none
      integer, intent(in) :: EN
      double precision, intent(in),dimension(EN) :: X,Y
      double precision, intent(in) :: YP1,YPN
      double precision, intent(inout), dimension(EN) :: Y2
      double precision, dimension(EN) :: UX
      double precision :: QN,UN,P,SIG
      integer :: I,K
      IF (YP1 > 0.99d30) THEN
       Y2(1)=0.0d0
       UX(1)=0.0d0
      ELSE
       Y2(1)=-0.50d0
       UX(1)=(3.0d0/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      END IF
      DO I=2,EN-1
       SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
       P=SIG*Y2(I-1)+2.0d0
       Y2(I)=(SIG-1.0d0)/P
       UX(I)=(6.0d0*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1)) & 
           /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*UX(I-1))/P
      end do
      IF (YPN > 0.99d30) THEN
        QN=0.0d0
        UN=0.0d0
      ELSE
       QN=0.5d0
       UN=(3.0d0/(X(EN)-X(EN-1)))*(YPN-(Y(EN)-Y(EN-1))/(X(EN)-X(EN-1)))
      END IF
      Y2(EN)=(UN-QN*UX(EN-1))/(QN*Y2(EN-1)+1.0d0)
      DO  K=EN-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+UX(K)
      END DO
      END SUBROUTINE SPLINE

!************************************************************************
!     wyliczenie wartosci funkcji metoda spline'ow - procedura SPLINT
!     z Numerical Recipies
!************************************************************************
      SUBROUTINE SPLINT(XA,YA,Y2A,EN,X,Y)
      implicit none
      integer, intent(in) :: EN
      double precision, intent(in), dimension(EN) :: XA,YA,Y2A
      double precision, intent(in) :: X
      double precision, intent(out) :: Y
      integer :: KLO,KHI,K
      double precision :: A,B,H
      KLO=1
      KHI=EN
      do while (KHI-KLO>1)
       K=(KHI+KLO)/2
       IF(XA(K) > X)THEN
         KHI=K
       ELSE
         KLO=K
       ENDIF
      end do
      H=XA(KHI)-XA(KLO)
      IF (H==0.0d0) then
            print *, 'grlib: Nieprawidlowy wektor XA na wejsciu SPLINT'
            call exit(1)
         end if
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+ & 
            ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.0d0
      END SUBROUTINE SPLINT




subroutine prosteA(wynik,x,k,l,nextra,extra)
implicit none
integer, intent(in) :: nextra
double precision, intent(in), dimension(nextra) :: extra ! funkcje pomocnicze
double precision, intent(in) :: x
integer, intent(in) :: k,l
double precision, intent(out) :: wynik

wynik = 1.0d0 ! czyli rozwiazaniem powinno byc e^x, bo jesli jest tylko jedna niewiadoma, to dla B=0 mamy
              ! u'(x) = 1.0d0 * u(x) + 0.0d0


!
! zwracamy A[k,l](x) takie, ze u'[k](x) = A[k,l](x) u[l](x) + B[k](x)
!
! gdzie u[l](x) oznacza wartosc l-tej niewiadomej funkcji w punkcie x 
! To jest najprostsza funkcja, ktora ignoruje k i l (zakladamy tylko jedna funkcje niewiadoma)
!

end subroutine prosteA

subroutine prosteB(wynik,x,k,nextra,extra)
implicit none
integer, intent(in) :: nextra
double precision, intent(in), dimension(nextra) :: extra ! funkcje pomocnicze
double precision, intent(in) :: x
integer, intent(in) :: k
double precision, intent(out) :: wynik

wynik = 0.0d0 ! czyli rozwiazaniem powinno byc G*e^x, bo jesli jest tylko jedna niewiadoma, to dla B=0 mamy
              ! u'(x) = 1.0d0 * u(x) + 0.0d0


!
! zwracamy B[k](x) takie, ze u'[k](x) = A[k,l](x) u[l](x) + B[k](x)
!
! gdzie u[l](x) oznacza wartosc l-tej niewiadomej funkcji w punkcie x 
! To jest najprostsza funkcja, ktora ignoruje k  (zakladamy tylko jedna funkcje niewiadoma)
!

end subroutine prosteB



!************************************************************************
!
!     rozwiazanie liniowego rownania zwyczajnego metoda spektralna
!
!************************************************************************

subroutine ODE_spektral_lin(NFUN,AFUN,BFUN,N,X,xlob,y,y0,NEXTRA,EXTRA,dir)
implicit none
external AFUN,BFUN
integer, intent(IN) :: NFUN,N,NEXTRA,dir
double precision, dimension(NFUN),intent(in) :: y0
double precision, dimension(NEXTRA,N), intent (in) :: EXTRA
double precision, dimension(N,NFUN), intent (out) :: y
double precision, dimension(N) :: X,xlob
double precision, dimension(N*NFUN,N*NFUN) :: glowna
double precision, dimension(N,N) :: phi,phix,phixx
double precision, dimension(N*NFUN) :: rhs
integer, dimension (N*NFUN) :: ipiv
integer :: info
double precision :: pomoc,factor
integer :: i,j,k,l
!call generuj_GL(N,xlob)
factor = dabs(2.0d0/(x(1)-x(N)))
do i=1,N ! przygotowanie macierzy pochodnych i macierzy wartosci
     call basis(xlob(i),N,PHI(1,i),PHIX(1,i),PHIXX(1,i))
end do

do k=1,NFUN
 do i=1,N
   call BFUN(rhs(i+(k-1)*N),X(i),k,NEXTRA,EXTRA(1,i))
 end do
end do
! warunek brzegowy

glowna = 0.0d0

do k = 1,NFUN
 do l = 1,NFUN
  do i = 1,N
   call AFUN(pomoc,X(i),k,l,NEXTRA,EXTRA(1,i))
   do j = 1,N     
     glowna( (k-1)*N+i,(l-1)*N+j ) =  - phi(j,i)*pomoc
     if(l.eq.k) then 
       glowna( (k-1)*N+i,(l-1)*N+j ) = glowna( (k-1)*N+i,(l-1)*N+j ) + phix(j,i)*factor
     end if 
   end do
  end do
 end do
end do


if(dir.eq.1) then
 do k=1,NFUN
  rhs(N+(k-1)*N) = y0(k)
  do i=1,N*NFUN
    glowna(N+(k-1)*N,i) = 0.0d0
  end do
  do i=1,N
    glowna(N+(k-1)*N,i+(k-1)*N) = phi(i,N)
  end do
 end do
else
 do k=1,NFUN
  rhs(1+(k-1)*N) = y0(k)
  do i=1,N*NFUN
    glowna(1+(k-1)*N,i) = 0.0d0
  end do
  do i=1,N
    glowna(1+(k-1)*N,i+(k-1)*N) = phi(i,1)
  end do
 end do
end if
!print *,'glowna'
!do i=1,N*NFUN
! print *,(glowna(i,j),j=1,n)
!end do
!print *,'boczna'
!do i=1,N*NFUN
! print *,rhs(i)
!end do

call dgesv(N*NFUN,1,glowna,N*NFUN,ipiv,rhs,N*NFUN,info)

if(info.ne.0) then
  print *,'grlib: Blad w dgesv(lapack) : ',info
  call exit(1)
end if
!print *,'wspolczynniki'
!do i=1,NFUN*N
!print *,rhs(i)

!end do

! tu mozna uzyc FFT aby 'wydusic' troche wydajnosci, ale z uwagi na dgesv jest to raczej nieoplacalne
do k = 1,NFUN
 do i = 1,N
  y(i,k) = 0.0d0
  do j = 1,N
   y(i,k) =  y(i,k) + phi(j,i)*rhs(j+(k-1)*N)
  end do
 end do
end do 

end SUBROUTINE ODE_spektral_lin

!************************************************************************
!
!     Rozwiazanie tak samo jako powyzej, ale przez rekombinacje bazy
!
!************************************************************************


subroutine ODE_spektral_lin_r(NFUN,AFUN,BFUN,N,X,xlob,y,y0,NEXTRA,EXTRA,dir)
implicit none
external AFUN,BFUN
integer, intent(IN) :: NFUN,N,NEXTRA,dir
double precision, dimension(NFUN),intent(in) :: y0
double precision, dimension(NEXTRA,N), intent (in) :: EXTRA
double precision, dimension(N,NFUN), intent (out) :: y
double precision, dimension(N) :: X,xlob
double precision, dimension((N-1)*NFUN,(N-1)*NFUN) :: glowna
double precision, dimension(N-1,N-1) :: phi,phix,phixx
double precision, dimension((N-1)*NFUN) :: rhs
integer, dimension ((N-1)*NFUN) :: ipiv
integer :: info
double precision :: pomoc,factor,pomocb
integer :: i,j,k,l,pos2
!call generuj_GL(N,xlob)
factor = dabs(2.0d0/(x(1)-x(N)))


if (dir.eq.1) then ! do przodu od x(N) = -1 --> x(1) = 1

 do i=1,N-1 ! przygotowanie macierzy pochodnych i macierzy wartosci
     call basis_zero(xlob(i),N-1,PHI(1,i),PHIX(1,i),PHIXX(1,i),1)
 end do

do k=1,NFUN
 do i=1,N-1
   call BFUN(pomocb,X(i),k,NEXTRA,EXTRA(1,i))   
   do l=1,NFUN
     call AFUN(pomoc,X(i),k,l,NEXTRA,EXTRA(1,i))
     pomocb = pomocb - xlob(i)*y0(l)*pomoc
   end do
   pomocb = pomocb + y0(k)*factor
   rhs(i+(k-1)*(N-1)) = pomocb
 end do
end do

else  ! do tylu : x(1) = 1 --> x(N) = -1

 do i=2,N ! przygotowanie macierzy pochodnych i macierzy wartosci
     call basis_zero(xlob(i),N-1,PHI(1,i-1),PHIX(1,i-1),PHIXX(1,i-1),-1)
 end do
do k=1,NFUN
 do i=2,N
   call BFUN(pomocb,X(i),k,NEXTRA,EXTRA(1,i))
   do l=1,NFUN
     call AFUN(pomoc,X(i),k,l,NEXTRA,EXTRA(1,i))
     pomocb = pomocb + xlob(i)*y0(l)*pomoc
   end do
   pomocb = pomocb - y0(k)*factor
   rhs(i-1+(k-1)*(N-1)) = pomocb
 end do
end do

end if

pos2 = 0
if(dir.eq.-1) then
 pos2 = 1
end if

glowna = 0.0d0

do k = 1,NFUN
 do l = 1,NFUN
  do i = 1,N-1
   call AFUN(pomoc,X(i+pos2),k,l,NEXTRA,EXTRA(1,i+pos2))
   do j = 1,N-1     
     glowna( (k-1)*(N-1)+i,(l-1)*(N-1)+j ) =  - phi(j,i)*pomoc
     if(l.eq.k) then 
       glowna( (k-1)*(N-1)+i,(l-1)*(N-1)+j ) = glowna( (k-1)*(N-1)+i,(l-1)*(N-1)+j ) + phix(j,i)*factor
     end if 
   end do
  end do
 end do
end do



call dgesv((N-1)*NFUN,1,glowna,(N-1)*NFUN,ipiv,rhs,(N-1)*NFUN,info)

if(info.ne.0) then
  print *,'grlib: Blad w dgesv(lapack) : ',info
  call exit(1)
end if
!print *,'wspolczynniki'
!do i=1,NFUN*N
!print *,rhs(i)

!end do
! tu mozna uzyc FFT aby 'wydusic' troche wydajnosci, ale z uwagi na obecnosc dgesv jest to raczej nieoplacalne

if (dir.eq.1) then
 do k = 1,NFUN
  do i = 1,N-1
   y(i,k) = 0.0d0
   do j = 1,N-1
    y(i,k) =  y(i,k) + phi(j,i)*rhs(j+(k-1)*(N-1))
   end do
  y(i,k) = y(i,k) - y0(k)*xlob(i)
  end do
 end do 
y(N,:) = y0
else
 do k = 1,NFUN
  do i = 2,N
   y(i,k) = 0.0d0
   do j = 2,N
    y(i,k) =  y(i,k) + phi(j-1,i-1)*rhs(j-1+(k-1)*(N-1))
   end do
   y(i,k) = y(i,k) + y0(k)*xlob(i)
  end do
 end do 
y(1,:) = y0
end if




end SUBROUTINE ODE_spektral_lin_r


!************************************************************************
!
!     Rozwiazanie tak samo jak powyzej, ale dopuszcza punkty w ktorych rozwiazanie jest niejednoznaczne (poprzez strzal)
!     Strzelamy do pierwszych nstrzal punktow stosujac wartosci z tablicy strzaly (nfun,nstrzal)
!
!************************************************************************


subroutine ODE_spektral_lin_r_strzal(NFUN,AFUN,BFUN,N,X,xlob,y,y0,NEXTRA,EXTRA,dir,nstrzal,strzaly)
implicit none
external AFUN,BFUN
integer, intent(IN) :: NFUN,N,NEXTRA,dir,nstrzal
double precision, dimension(NFUN),intent(in) :: y0
double precision, dimension(NEXTRA,N), intent (in) :: EXTRA
double precision, dimension(N,NFUN), intent (out) :: y
double precision, dimension(N),intent(in) :: X,xlob
double precision, dimension(nfun,nstrzal),intent(in) :: strzaly

double precision, dimension((N-1)*NFUN,(N-1)*NFUN) :: glowna
double precision, dimension(N-1,N-1) :: phi,phix,phixx
double precision, dimension((N-1)*NFUN) :: rhs
integer, dimension ((N-1)*NFUN) :: ipiv
integer :: info
double precision :: pomoc,factor,pomocb
integer :: i,j,k,l,pos2,ppp
!call generuj_GL(N,xlob)
factor = dabs(2.0d0/(x(1)-x(N)))


if (dir.eq.1) then ! do przodu od x(N) = -1 --> x(1) = 1

 do i=1,N-1 ! przygotowanie macierzy pochodnych i macierzy wartosci
     call basis_zero(xlob(i),N-1,PHI(1,i),PHIX(1,i),PHIXX(1,i),1)
 end do

do k=1,NFUN
 do i=1,N-1
   call BFUN(pomocb,X(i),k,NEXTRA,EXTRA(1,i))   
   do l=1,NFUN
     call AFUN(pomoc,X(i),k,l,NEXTRA,EXTRA(1,i))
     pomocb = pomocb - xlob(i)*y0(l)*pomoc
   end do
   pomocb = pomocb + y0(k)*factor
   rhs(i+(k-1)*(N-1)) = pomocb
 end do
end do

else  ! do tylu : x(1) = 1 --> x(N) = -1

 do i=2,N ! przygotowanie macierzy pochodnych i macierzy wartosci
     call basis_zero(xlob(i),N-1,PHI(1,i-1),PHIX(1,i-1),PHIXX(1,i-1),-1)
 end do
do k=1,NFUN
 do i=2,N
   call BFUN(pomocb,X(i),k,NEXTRA,EXTRA(1,i))
   do l=1,NFUN
     call AFUN(pomoc,X(i),k,l,NEXTRA,EXTRA(1,i))
     pomocb = pomocb + xlob(i)*y0(l)*pomoc
   end do
   pomocb = pomocb - y0(k)*factor
   rhs(i-1+(k-1)*(N-1)) = pomocb
 end do
end do

end if

pos2 = 0
if(dir.eq.-1) then
 pos2 = 1
end if

glowna = 0.0d0

do k = 1,NFUN
 do l = 1,NFUN
  do i = 1,N-1
   call AFUN(pomoc,X(i+pos2),k,l,NEXTRA,EXTRA(1,i+pos2))
   do j = 1,N-1     
     glowna( (k-1)*(N-1)+i,(l-1)*(N-1)+j ) =  - phi(j,i)*pomoc
     if(l.eq.k) then 
       glowna( (k-1)*(N-1)+i,(l-1)*(N-1)+j ) = glowna( (k-1)*(N-1)+i,(l-1)*(N-1)+j ) + phix(j,i)*factor
     end if 
   end do
  end do
 end do
end do

if (nstrzal.gt.0) then

if (dir.eq.1) then

 do ppp=1,nstrzal
  do k=1,NFUN
   rhs(N-ppp+(k-1)*(N-1)) = strzaly(k,ppp) + y0(k)*xlob(N-ppp)
   do i=1,(N-1)*NFUN
     glowna(N-ppp+(k-1)*(N-1),i) = 0.0d0
   end do
   do i=1,N-1
    glowna(N-ppp+(k-1)*(N-1),i+(k-1)*(N-1)) = phi(i,N-ppp)
   end do
  end do
 end do
else

 do ppp=1,nstrzal
   do k=1,NFUN
   rhs(ppp+(k-1)*(N-1)) = strzaly(k,ppp) - y0(k)*xlob(ppp+1)
   do i=1,(N-1)*NFUN
     glowna(ppp+(k-1)*(N-1),i) = 0.0d0
   end do
   do i=1,N-1
     glowna(ppp+(k-1)*(N-1),i+(k-1)*(N-1)) = phi(i,ppp)
   end do
  end do
 end do 
 
end if ! dir
 
end if ! strzaly





call dgesv((N-1)*NFUN,1,glowna,(N-1)*NFUN,ipiv,rhs,(N-1)*NFUN,info)

if(info.ne.0) then
  print *,'grlib: Blad w dgesv(lapack) : ',info
  call exit(1)
end if
!print *,'wspolczynniki'
!do i=1,NFUN*N
!print *,rhs(i)

!end do
! tu mozna uzyc FFT aby 'wydusic' troche wydajnosci, ale z uwagi na obecnosc dgesv jest to raczej nieoplacalne

if (dir.eq.1) then
 do k = 1,NFUN
  do i = 1,N-1
   y(i,k) = 0.0d0
   do j = 1,N-1
    y(i,k) =  y(i,k) + phi(j,i)*rhs(j+(k-1)*(N-1))
   end do
  y(i,k) = y(i,k) - y0(k)*xlob(i)
  end do
 end do 
y(N,:) = y0
else
 do k = 1,NFUN
  do i = 2,N
   y(i,k) = 0.0d0
   do j = 2,N
    y(i,k) =  y(i,k) + phi(j-1,i-1)*rhs(j-1+(k-1)*(N-1))
   end do
   y(i,k) = y(i,k) + y0(k)*xlob(i)
  end do
 end do 
y(1,:) = y0
end if




end SUBROUTINE ODE_spektral_lin_r_strzal




! *******************************************************************************************
!
!        	 wylicza norme roznicy dwoch funkcji (na tej samej siatce)
!
!
! *******************************************************************************************
subroutine norma(wynik,n,x,a,b)
implicit none
integer, intent(in) :: n
double precision, intent(in),dimension(n) :: x,a,b
double precision, intent(out) :: wynik
integer :: i
double precision :: miara
wynik = 0.0d0
do i=1,n-1
    miara = dabs(x(i)-x(i+1))
    wynik = wynik+miara*(a(i)-b(i))**2
end do
wynik = wynik + dabs(x(n-1)-x(n))*(a(n)-b(n))**2
wynik = dsqrt(wynik)
end subroutine norma
