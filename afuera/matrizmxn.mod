         module uno
 !      -------------------------------------------------------------
        contains
  !     -------------------------------------------------------------
        subroutine mate(mat)
        implicit none

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
		enddo

		fila2=fila2+1
		colum2=1

      enddo

      Write(*,*)"La matriz ingresada fue :"

      do i=1,fila,1

      write(*,*) mat(i,:)
      end do
      end subroutine
       end module
!     -----------------------------------------

  !    deallocate(mat)
   !     pause



