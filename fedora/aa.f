program uno
open (10, FILE='Ejer5A.dat', STATUS='UNKNOWN')
write(10,*) i,x1,x2,xm, abs(G)
close(10)
end program
subroutine eulerexp(func,xo,yo,a,b,n,y)
!dado la confincion inicial xo,yo, en el intervalo (a,b) se eligen n puntos y devuelve la funcion en esos puntos.
implicit none
REAL*8,ALLOCATABLE,DIMENSION(:)::y,x
integer*4::n,i
real*8::func,xo,yo,a,b,h
h=(b-a)/n
allocate(x(n+1),y(n))
do i=0,n
x(i)=xo+h*i
end do
y(1)=yo+func(xo,yo)*h
do i=1,n
y(i+1)=y(i)+func(x(i),y(i))*h
end do
return
end subroutine
