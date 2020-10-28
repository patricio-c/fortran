function func(x,y)
real*8::x,func,y
func=-y
end

program ecuaciones
implicit none
external func
REAL*8,ALLOCATABLE,DIMENSION(:)::y,t
integer*4::n,i,M
real*8::func,xo,yo,a,b,K1,K2,DT
M=5
a=0.
b=2.6
xo=0
yo=1
DT=0.2
allocate(t(M+1),y(M))
do i=0,M
t(i)=xo+DT*i
PRINT*, t(i)
end do
y(1)=yo
do n=1,M
K1=func(y(n),t(n))
K2=func(y(n)+(K1)*DT,t(n+1))
y(n+1)=((K2+K1)/2.)*DT
end do
print*, y(:)
end  

