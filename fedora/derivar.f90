
      function func(x)
        func= x*exp(x)
      end
 
      program  aa
      
!      subroutine prder(x,func,n,h,D)
      !primera derivada de orden n en el error, en el intervalo h
      
      implicit none
      external func
      real::h,func,x
      REAL,ALLOCATABLE,DIMENSION(:,:)::D
      integer::i,j,n
       x=2
       n=4
       h=0.004
      allocate(D(n,n))
      D(:,:)=0.0d1
      !CADA fila es un D(fila ejemplo 1) con la primer cordenada 2H la segunda H la tercera H/2. la segunda fila es un D2 con primer elemento H segundo H/2 Y ASI
      
      do j=0,n-1
      do i=1,n-j

      if (j==0) then
      if (i==1) then
      D(j+1,i)=((func(x+2*h)-func(x-2*h))/(2*2*h))
      else
      D(j+1,i)=((func(x+2*(h/(2*(i-1))))-func(x-2*(h/(2*(i-1)))))/(2*2*(h/(2*(i-1)))))
      end if
      else
      D(j+1,i)=(((2**(2*(j))))*D(j,i+1)-D(j,i))/((2**(2*(j)))-1)


      end if
      end do

      end do

      
      print*, D(n,1)
      
      print*, (exp(2.)*3)
      return 
      end
      
      
      
!     REAL FUNCTION AVRAGE()
!     REAL X,Y,Z,SUM
!     SUM = X + Y + Z
!     AVRAGE = SUM /3.0
!     RETURN
!     END

