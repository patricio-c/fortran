function func(x,tt)
real::g,l,x,tt
g=9.80665
l=1.0
func=-(g/l)*sin(x)
end 

program uno
implicit none
REAL,ALLOCATABLE,DIMENSION(:)::x,v,t
integer::n,i
real::func,xo,vo,a,b,h,VNmid,to


a=0.
b=10.
to=0.
vo=0.
xo=0.523599
n=1000

open (10, FILE='intfuncDKD.dat', STATUS='UNKNOWN')
h=(b-a)/float(n)
allocate(v(n+1),x(n+1),t(n+1))
do i=0,n
t(i)=to+h*i
end do
x(0)=xo
v(0)=vo
do i=0,n
VNmid=v(i)+func(x(i),t(i))*(h/2.)
x(i+1)=x(i)+VNmid*h
v(i+1)=VNmid+func(x(i+1),t(i))*(h/2)
write(10,*) i,x(i),v(i),t(i)
end do
close(10)
return
end program





