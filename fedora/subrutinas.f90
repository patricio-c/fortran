program subrutinas

!call solveLU(L,U,n,B,sol) !solucinador del sistema LUX=B.
!call matriz(mat,fila,colum) !ingresar una matriz nxm
!call matcof(A,n,C,inv) !matriz de cofactores
!call matdet(A,n,det)  !determinante
!call  matlu(A,n,m,L,U) !descomposicion L.U, ,
!call solcof(B,A,n,D) !solucion mediante el metodo de cofactores para la inversa
!call kramer(A,n,B,X) ! solucion mediante el metodo de kramer de un sistema lineal de ecu


!-----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine  matlu(A,n,m,L,U)

! esta subrutina come una matriz A con n filas y m columnas y devuelve una matriz L y una matriz U

implicit none
real,allocatable,dimension(:,:)::A,L,U
integer:: k,h,j,n,m
	
allocate(L(n,m),U(n,m))
	
U=A
do h= 1,m
 do j= 1,n
! j filas, h columnas
if (j>h) then
   do k=1,m
  	U(j,k)=A(j,k)-(A(j,h)/A(h,h))*A(h,k)
   end do
   L(j,h)=(A(j,h)*(1/A(h,h)))
else if (j==h) then
 L(j,h)=1
else 
  L(j,h)=0
end if
 end do 
A=U
end do	
return
end subroutine
!--------------------------------------------------------------

subroutine matdet(A,n,det)
! esta matriz calcula determinante de una matriz A nxn
 
implicit none
real,allocatable,dimension(:,:)::A,L,U
integer:: i,n
real::det


if (n==1) then
 det=A(1,1)
else 
call matlu(A,n,n,L,U)
det=1
do i=1,N
det=U(i,i)*det
end do
end if
return
end subroutine

!--------------------------------------------------------------

subroutine matcof(A,n,C,inv)
! esta subrutina come una matriz A con n filas y m columnas y devuelve una matriz C de cofactores

implicit none
real,allocatable,dimension(:,:)::A,C,L,U,C2,inv
integer:: k,h,j,n,i,z
real::det
allocate(C(n,n),C2(n-1,n-1))

!h=f,k=c,i=f,j=c
do h=1,n
do k=1,n

do i=1,n
do j=1,n

!ME ARMO UNA MATRIZ SIN ESA FILA Y COLUMNA Y LE HAGO EL DETERMINANTE
if (j/=k.and.i/=h) then

   if (i<h.and.j<k) then
      C2(i,j)=A(i,j)
   else if (i<h.and.j>k) then
      C2(i,j-1)=A(i,j)
   else if (i>h.and.j<k) then
      C2(i-1,j)=A(i,j)
   else if (i>h.and.j>k) then
      C2(i-1,j-1)=A(i,j)
      else
   end if
  
end if
 
end do
end do
  call matdet(C2,(n-1),det)
  C(h,k)=det*((-1)**(h+k)) 
end do
 
end do

call matdet(A,n,det)

allocate(inv(n,n))

do i=1,n
do j=1,n
 inv(i,j)=C(j,i)/det
end do
end do

deallocate(C2)
return

end subroutine
!--------------------------------------------------------------
subroutine matriz(mat,fila,colum)
!esta subrutina te genera una matriz mat de fila y columna ingresada
real,allocatable,dimension(:,:)::mat
integer:: fila,colum,fila2,colum2,i

Write(*,*) "Ingrese las dimenciones de la matriz"
Write(*,*) "Filas"
read(*,*) fila
Write(*,*) "Columnas"
read(*,*) colum

allocate(mat(fila,colum))

write(*,*) "Ingrese los elementos de la matriz"
fila2=1
colum2=1

do while(fila2<=fila)
 do while(fila2<=fila.and.colum2<=colum)
 Write(*,*)"Ingrese el elemento de la matriz",fila2,",",colum2
 read(*,*)mat(fila2,colum2)
 colum2=colum2+1
end do
fila2=fila2+1
colum2=1
enddo
Write(*,*)"La matriz ingresada fue :"
do i=1,fila,1
write(*,*) mat(i,:)
end do 
return
end subroutine

!---------------------------------------------
subroutine solveLU(L,U,n,B,solu)
! esta subrutina come una matriz L y U de la descomposicion LU, un vector B
! que es el resultado, donde todas tiene n filas y devuelve el vector solucion


implicit none
real,allocatable,dimension(:,:)::L,U
real,allocatable,dimension(:)::solu,B,sol2
integer:: j,i,n,k
real::suma,suma2

allocate(sol2(n),solu(n))
sol2(:)=1.0d0
solu(:)=1.0d0

do i=1,n

if (i==1) then
 sol2(1)=B(1)/L(1,1)
else
suma=0
do k=1,(i-1)
suma=L(i,k)*sol2(k)+suma
end do 
sol2(i)=(1./(L(i,i)))*(B(i)-suma)
end if

end do

solu(n)=sol2(n)/U(n,n)

do i=1,(n-1)
suma2=0
do k=(n-i+1),n
suma2=U(n-i,k)*solu(k)+suma2
end do
solu(n-i)=(1./(U(n-i,n-i)))*(sol2(n-i)-suma2)
end do

deallocate (sol2)
return
end subroutine

!---------------------------------------------------
subroutine solcof(B,A,n,D)

implicit none
real,allocatable,dimension(:,:)::A,inv,C
real,allocatable,dimension(:)::D,B
integer:: n

call matcof(A,n,C,inv)

allocate(D(n))

D=matmul(inv,B)

return
end subroutine
!----------------------------------------
subroutine kramer(A,n,B,X)
implicit none
real, allocatable, dimension(:,:)::A,G,F
real, allocatable, dimension(:)::B,X
integer:: n,k,j,i
real:: det,deter

call matdet(A,n,det)

allocate(X(n))

do k=1,n
allocate(F(n,n))

do i=1,n
do j=1,n

if (k==j) then
F(i,j)=B(i)
else
F(i,j)=A(i,j)
end if
end do
end do
deter=0
call matdet(F,n,deter)
X(k)=deter/det
deallocate(F)

end do
return
end subroutine

!-----------------------
end program
