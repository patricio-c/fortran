function gunc(x)
real*8::x,gunc
gunc=exp(x)
end

program aa
implicit none
external gunc
real*8::a,b,gunc,y
integer*4::n
n=2
a=0.
b=1.
call intgausscuadr2do(gunc,a,b,n,y)

print*, y

end program

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
