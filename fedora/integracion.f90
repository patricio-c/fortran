function func(x)
real*8::func,x
func=sin(x)
end 
program integracion
implicit none
external func
!REGLA TRAPECIO compuesta, necesito una func, un punto inicial 'a', uno final 'b', la cantidad de puntos 'n' incluyendo a y b
!se necesita un vector x(i)
REAL*8::h,a,b,func,U,suma,SUMA1,SUMA2,UU,UUU,UUUU,integral,con1,con2,con3,summa
INTEGER*4::i,n,k,nn,mm,j 	
REAL*8,ALLOCATABLE,DIMENSION(:)::P,xx,AC,BD,w,hh
REAL*8,ALLOCATABLE,DIMENSION(:,:)::r
a=0d1
b= acos(-1.0)
n=5

h=abs(b-a)/(n-1)
suma=0d1
allocate(P(n))
do i=1,(n-2)
 P(i)=a+h*i
 suma=suma+func(P(i))
end do

U=h*(func(a)+func(b))/2+h*suma
print*, U, 'U'
!regla de simsomp de 3 puntos,

if (n==3) then
do i=1,(n-2)
 P(i)=a+h*i
 suma=suma+func(P(i))
end do
UU=(h/3)*(func(a)+4*func(P(1))+func(b))
print*, UU, '2U'
else if (n>3) then
!regla de simpson 3/8
UUU=((3*h)/8)*(func(a)+func(b)+ 3*(func(a+abs(b-a)/3)+func(a+2*abs(b-a)/3)))
!REGLA DE SIMSOPS COMPUESTA
SUMA1=0d1
SUMA2=0d1
do i=1,INT(n/2)
if (i==INT(n/2)) then
SUMA1=SUMA1+func(P(i*2-1))
else
SUMA2=SUMA2+func(P(i*2))
SUMA1=SUMA1+func(P(i*2-1))
end if
end do

UUUU=(h/3)*(func(a)+func(b)+4*SUMA1+2*SUMA2)
print*, UUU , '3U'
else 
print*, 'se usa la del trapecio'
end if
!REGLA DE NEWTON CONTES 2DO ORDEN
 !determinacion de los w(k) en un intervalo b,a con tres puntos
 allocate(xx(3),AC(2),BD(2),w(3))
 
 xx(0)=0d1
 xx(1)=5d0
 xx(2)=1d1
 do k = 0,2
 
 do i = 0,2
 if (i.ne.k) then
 	if (i>k) then
 	AC(i) = (xx(k)-xx(i))
 	BD(i)=(xx(i))/(xx(k)-xx(i)) 
 	ELSE
 	AC(i-1) = (xx(k)-xx(i))
 	BD(i-1)=(xx(i))/(xx(k)-xx(i))
 	end if
 else
 end if
 end do
 
 
 con1=AC(1)*AC(2)
 con2=(BD(2)/AC(1))+(BD(1)/AC(2))
 con3=BD(1)*BD(2)
  
 W(k)=((b-a)**3)/(3*con1)-(((b-a)**2)/2)*con2+(b-a)*con3
 
 end do
 
 integral=0
 do i=0,2
 integral=w(i)*func(xx(i))+integral
 end do
 print*, integral,'los w son',w(0),w(1),w(2)
 
 !------------------------------------------------
 !integracion romberg
 !obtenes una matriz R(nn,mm) donde necesito una funcion que se pueda derivar muhco, un valor inicial, uno final y la cantidad de nn y mm del romberg que quiero.
 nn=6
 mm=6
 allocate(r(nn,mm),hh(nn)) 
 r(0,0)=(1./2.)*(b-a)*(func(a)+func(b))
 print*, 'a',(1./2.)*(b-a)*(func(a)-func(b)),b,a
 do j=1,6
 do k=1,6
 hh(k)=(b-a)/(2**k)
 summa=0
 do i=1,(2**(k-1))
 summa=summa+func(a+(2*i-1)*hh(k))
 end do
 r(k,0)=(1./2.)*r(k-1,0)+hh(k)*summa
 r(k,j)=(1./((4**j)-1))*((4**j)*(r(k,j-1))-r(k-1,j-1))
 end do
 end do
 print*, 'r'
 do i=1,nn
 print*,r(:,i)
 end do
 print*, 'el r(6,6) es ', r(6,6)
end program
