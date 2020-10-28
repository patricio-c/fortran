	program lu

	implicit none
	real,allocatable,dimension(:,:)::A,L,U
	integer:: k,h,j,n,m,i
	
	call matriz(A,n,m)
	
	allocate(L(n,m),U(n,m))
	
! esta A se reduce la diagonal inferior y queda la matriz U y la matriz L
!primero hago que U sea igual a A
	U=A
	do h= 1,m
	
	   do j= 1,n
!aca dice la parte triangular inferior o mejor dicho cuando tengo las filas mas grande que las columnas
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

!despues reemplazo A por la nueva U y listo

	A=U
	end do	

!--------------------------------------------

	Write(*,*)"La matriz L :"

	do i=1,n,1

	write(*,*) L(i,:)
	end do 
	
		Write(*,*)"La U :"

	do i=1,n,1

	write(*,*) U(i,:)
	end do 	

	A=matmul(L,U)

		Write(*,*)"La A :"

	do i=1,n,1

	write(*,*) A(i,:)
	end do 	
	

	  deallocate(A,L)	
!---------------------------------------------------------
	
	contains
	
!---------------------------------------------------------	
	subroutine matriz(mat,fila,colum)
	
	

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


	end subroutine

!---------------------------------------------


 	end program
