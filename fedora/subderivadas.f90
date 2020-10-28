PROGRAM DERIVADAS
! CALL derivada(n,x,h,func,A) esta subrutina calcula con una presicion 'n', el valor 'A' de la derivada en un punto 'x' con un paso de 'h' de una funcion 'func'(definida como external antes del programa. todo con presicion doble

!-----------------------------------------------------------------------
	contains
! ----------------------------------------------------------------------       
      subroutine derivada(n,x,h,func,A)
      !esta subrutina calcula con una presicion 'n', el valor 'A' de la derivada en un punto 'x' con un paso de 'h' de una funcion 'func'(definida como external antes del programa. todo con presicion doble
      implicit none
      real*8::h,x,A,func
      REAL*8,ALLOCATABLE,DIMENSION(:,:)::D
      integer*4::i,j,n

      allocate(D(n,n))
      D(:,:)=0.0d1
      !CADA fila es un D(fila ejemplo 1) con la primer cordenada 2H la segunda H la tercera H/2. la segunda fila es un D2 con primer elemento H segundo H/2 Y ASI      
      do j=0,n-1
      do i=1,n-j
      if (j==0) then
      if (i==1) then
      D(j+1,i)=((func(x+h)-func(x-h))/(2*h))
      else
      D(j+1,i)=((func(x+(h/(2**(i-1))))-func(x-(h/(2**(i-1)))))/(2*(h/(2**(i-1)))))
      end if
      else
      D(j+1,i)=(((2**(2*(j))))*D(j,i+1)-D(j,i))/((2**(2*(j)))-1)
      end if
      end do
      end do
      A=D(n,1)
      deallocate(D)     
      return 
      end subroutine
END


