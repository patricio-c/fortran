program ecuaciones diferenciales
!NO FUNCIONAN LAS SUBRUTINAS POR COMO SE LLAMAA A LA FUNCION, TAMPOCO LO DE REAL*4 Y ESO PERO SI FUNCIONAN TODOS LOS CODIGOS
!-------------------------------------------------
contains
!-------------------------------------------------
subroutine eulerexp(func,xo,yo,a,b,n,y)!funciona el codigo pero no com subrutina
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve el valor de la funcion y en esos puntos.
implicit none
external func
REAL,ALLOCATABLE,DIMENSION(:)::y,x
integer::n,i
real::func,xo,yo,a,b,h
open (10, FILE='intfunEE.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(x(n+1),y(n+1))
do i=0,n
x(i)=xo+h*i
end do
y(0)=yo+func(xo,yo)*h
do i=0,n
y(i+1)=y(i)+func(x(i),y(i))*h
write(10,*) i,x(i),y(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
subroutine eulerimp(func,xo,yo,a,b,n,y)!funciona el codigo pero no com subrutina
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::y,x
integer::n,i
real::func,xo,yo,a,b,h,YY
open (10, FILE='intfunEI.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(x(n+1),y(n+1))
do i=0,n
x(i)=xo+h*i
end do
YY=yo+func(xo,yo)*h
y(0)=yo+func(xo,yo)*h
do i=0,n
YY=y(i)+func(x(i+1),YY)
y(i+1)=y(i)+func(x(i+1),YY)*h
write(10,*) i,x(i),y(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
subroutine midpexp(func,xo,yo,a,b,n,y)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL*8,ALLOCATABLE,DIMENSION(:)::y,x
integer*4::n,i
real*8::func,xo,yo,a,b,h,YY
open (10, FILE='intfunME.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(x(n+1),y(n+1))
do i=0,n
x(i)=xo+h*i
end do
y(0)=yo
do i=0,n
YY=y(i)+func(x(i)+(h/2),y(i))*(h/2)
y(i+1)=y(i)+func(x(i)+(h/2),YY)*h
write(10,*) i,x(i),y(i)
end do
close(10)
returnend subroutine
!-------------------------------------------------
subroutine midpimp(func,xo,yo,a,b,n,y)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::y,x
integer::n,i
real::func,xo,yo,a,b,h,YY
open (10, FILE='intfunMI.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(x(n+1),y(n+1))
do i=0,n
x(i)=xo+h*i
end do
y(0)=yo
YY=yo
do i=0,n
YY=y(i)+func(x(i)+(h/2),YY)*(h/2)
y(i+1)=y(i)+func(x(i)+(h/2),YY)*h
write(10,*) i,x(i),y(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
subroutine ruegekutta2to(func,xo,yo,a,b,n,y)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::y,x
integer::n,i
real::func,xo,yo,a,b,h,YY
open (10, FILE='intfunRK2TO.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(x(n+1),y(n+1))
do i=0,n
x(i)=xo+h*i
end do
y(0)=yo
do i=0,n
YY=y(i)+func(x(i),y(i))*h
y(i+1)=y(i)+(func(x(i),y(i))+func(x(i)+h,YY))*(h/2)
write(10,*) i,x(i),y(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
subroutine ruegekutta4to(func,xo,yo,a,b,n,y)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::y,x
integer::n,i
real::func,xo,yo,a,b,h,YY1,YY2,YY3,YY4

open (10, FILE='intfunRK4TO.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(x(n+1),y(n+1))
do i=0,n
x(i)=xo+h*i
end do
y(0)=yo
do i=0,n
YY1=func(x(i),y(i))
YY2=y(i)+func(x(i),YY1)*(h/2)
YY3=y(i)+func(x(i),YY2)*(h/2)
YY4=y(i)+func(x(i)+(h/2),YY3)*h
y(i+1)=y(i)+(h/6)*func(x(i),y(i))+(h/3)*func(x(i)+(h/2),YY2)+(h/3)*func(x(i)+(h/2),YY3)+(h/6)*func(x(i),YY4)
write(10,*) i,x(i),y(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
subroutine leapfrogDKD(func,xo,vo,a,b,n,x,v)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::x,v
integer::n,i
real::func,xo,vo,a,b,h,VNmid
open (10, FILE='intfuncDKD.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(v(n+1),x(n+1))
x(0)=xo
v(0)=vo
do i=0,n
VNmid=v(i)+func(x(i))*(h/2.)
x(i+1)=x(i)+VNmid*h
v(i+1)=VNmid+func(x(i+1))*(h/2)
write(10,*) i,x(i),v(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
subroutine leapfrogDKD(func,xo,vo,a,b,n,x,v)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::x,v
integer::n,i
real::func,xo,vo,a,b,h,XNmid
open (10, FILE='intfuncKDK.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(v(n+1),x(n+1))
x(0)=xo
v(0)=vo
do i=0,n
XNmid=x(i)+v(i)*(h/2.)
v(i+1)=v(i)+func(XNmid)*h
x(i+1)=XNmid+v(i+1)*(h/2)
write(10,*) i,x(i),v(i)
end do
close(10)
return
end subroutine
!-------------------------------------------------
end

