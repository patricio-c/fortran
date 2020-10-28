program N-cuerpos
real::dE,E,Er,dist,G,vel,epsi
real,allocatable,dimension(:)::m,r,v,a
!real,allocatable,dimension(:,:)::a
integer::i,j

!valores de constante( G constante universal, n numero de particulas)
G=
n=
epsi=
dt=

allocate(a(n),v(i),m(i),r(i))

!calculo de la matriz aceleracion
!duda si poner m(i) o m(j) cual es la que no es de la particula??
a=0.0d1
do  i=1,n
if (i.ne.j) then
do  j=1,n
a(i)=-((G*m(j))/((abs(r(i)-r(j))**2)+epsi))


!energia
dist=0
vel=0
do  i=1,n
vel=m(i)*(v(i)**2)+vel
if (i.ne.j) then
do  j=i,n
dist=dist+((abs(r(i)-r(j)))/(m(i)*m(j)*G))
end do
end if
end do
E=(1/2.0)*(vel)-(1/dist)

!error relativo
Er=dE/E
write(10,*) Er 
