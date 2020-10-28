program integrales

contains
!-------------------------------------------------
subroutine intrect(func,a,b,y)
!devuelve la y de una aproximacion por un rectangulo
implicit none
real*8::func,a,b,y
y=(func(a)+func(b))/2.
return
end subroutine
!-------------------------------------------------
subroutine inttrap(func,a,b,n,y)
!cantidad de puntos que uno quiere es n teniendo en cuenta a y b
implicit none
real*8::func,a,b,h,y,suma
REAL*8,ALLOCATABLE,DIMENSION(:)::P
integer*4::n,i
h=abs(b-a)/(n-1)
suma=0d1
allocate(P(n))
do i=1,(n-2)
 P(i)=a+h*i
 suma=suma+func(P(i))
end do
y=h*(func(a)+func(b))/2+h*suma
return
end subroutine
!-------------------------------------------------
subroutine intsimps13(func,a,b)
!regla simpson de 3 puntos
implicit none
real*8::func,a,b,h,y
h=abs(b-a)/2
end do
y=(func(a+h)/3)*(func(a)+4*func(a+h)+func(b))
return
end subroutine
!-------------------------------------------------
subroutine intsimpscomp13(func,a,b,n,y)
!dado n puntos (contando extremos) obtengo el resultado de la integral 'y'
implicit none
real*8::func,a,b,h,suma,SUMA1,SUMA2,y
REAL*8,ALLOCATABLE,DIMENSION(:)::P
integer*4::n,i
h=abs(b-a)/(n-1)
suma=0d1
allocate(P(n))
do i=1,(n-2)
P(i)=a+h*i
suma=suma+func(P(i))
end do
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
y=(h/3)*(func(a)+func(b)+4*SUMA1+2*SUMA2)
return
end subroutine
!-------------------------------------------------
subroutine intsimp38(func,a,b,y)
implicit none
real*8::func,a,b,y
integer*4::
h=abs(b-a)/(n-1)
y=((3*(abs(b-a)/3))/8)*(func(a)+func(b)+ 3*(func(a+abs(b-a)/3)+func(a+2*abs(b-a)/3)))
return
end subroutine
!-------------------------------------------------
subroutine intgausscuadr2do(func,a,b,n,y)
!tendra n+1 puntos, devuelve un valor y
implicit none
real*8::func,a,b,y
REAL*8,ALLOCATABLE,DIMENSION(:)::x,xx,w
integer*4::n,i
allocate(x(n+1),xx(n+1),w(n+1))
if (n==0) then
x(1)=0.
w(1)=2.
else if (n==1) then
x(1)=-(1/3)**(1/2)
x(2)=(1/3)**(1/2)
w(1)=1.
w(2)=1.
else if (n==2) then
x(1)=-(3/5)**(1/2)
x(2)=0.
x(3)=(3/5)**(1/2)
w(1)=5/9.
w(2)=8./9
w(3)=5/9.
end if
!cambio de intervalo
do i=1,n+1
xx(i)=((b-a)/2)*x(i)+((b+a)/2)
end do
y=0
do i=1,n+1
y=y+w(i)*func(xx(i))
end do
y=((b-a)/2)*y
end subroutine
!-------------------------------------------------
subroutine intromberg(func,a,b,n,m,y)
!esta subrutina se usa para una funcion que se pueda derivar mucho usando el metodo de romberg para una matriz(n,m) y devuelve el valor y=r(n,m)
implicit none
real*8::func,a,b,suma,y
REAL*8,ALLOCATABLE,DIMENSION(:)::h
REAL*8,ALLOCATABLE,DIMENSION(:,:)::r
integer*4::n,m,j,k,i
!integracion romberg
 !obtenes una matriz R(nn,mm) donde necesito una funcion que se pueda derivar muhco, un valor inicial, uno final y la cantidad de nn y mm del romberg que quiero.
 allocate(r(n,m),h(n)) 
 r(0,0)=(1./2.)*(b-a)*(func(a)+func(b))
 do j=1,m
 do k=1,n
 h(k)=(b-a)/(2**k)
 suma=0
 do i=1,(2**(k-1))
 suma=suma+func(a+(2*i-1)*h(k))
 end do
 r(k,0)=(1./2.)*r(k-1,0)+h(k)*suma
 r(k,j)=(1./((4**j)-1))*((4**j)*(r(k,j-1))-r(k-1,j-1))
 end do
 end do
 do i=1,m
 print*,r(:,i)
 end do
 y=r(n,m)
end subroutine
!-------------------------------------------------
subroutine intnewcont(func,a,b,n,xo,INTT)!falta como integrar w(k)
!esta subrutina se usa para una funcion usando el metodo de newton contes de orden n
implicit none
real*8::func,a,b,h,ll,INTT
REAL*8,ALLOCATABLE,DIMENSION(:)::x,l,w
integer*4::n,i,k
allocate(x(n+1),l(n+1),w(n+1))
h=(b-a)/n
do i=0,n
x(i)=a+h*i
end do

do k=0,n
ll=1
do i=0,n
if (i.ne.k) then
l(k)=((xo-x(i))/(x(k)-x(i)))*ll
end if
end do
end do

do i=0,n
!call alguna subrutina, la func van a ser los l(i)
!w(k)=y
end do
INTT=0
do k=0,n
INTT=w(k)*func(x(k))+INTT
end do
return
end subroutine
!-------------------------------------------------

end program

