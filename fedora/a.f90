
      function func(x)
      real*8::x,func
        func= x*exp(x)
      end
 
      program  aa
      
!      subroutine prder(x,func,n,h,D)
      !primera derivada de orden n en el error, en el intervalo h
      
      implicit none
      external func
      real*8::h,x
      real*8::func
      REAL*8,ALLOCATABLE,DIMENSION(:,:)::D
      integer*4::i,j,n
       x=2.
       n=4
       h=4d-1
      allocate(D(n,n))
      D(:,:)=0.0d1
      !CADA fila es un D(fila ejemplo 1) con la primer cordenada 2H la segunda H la tercera H/2. la segunda fila es un D2 con primer elemento H segundo H/2 Y ASI
      
      do j=0,n-1
      do i=1,n-j

      if (j==0) then
      if (i==1) then
      D(j+1,i)=((func(x+h)-func(x-h))/(2*h))
      print*, j+1,i, D(j+1,i), 'a'
      else
      D(j+1,i)=((func(x+(h/(2**(i-1))))-func(x-(h/(2**(i-1)))))/(2*(h/(2**(i-1)))))
      print*, j+1,i, D(j+1,i), 'b'
      end if
      else
      D(j+1,i)=(((2**(2*(j))))*D(j,i+1)-D(j,i))/((2**(2*(j)))-1)
      print*, j+1,i, D(j+1,i), 'c'
 

      end if
      end do

      end do
      
      print*, ' '
      print*,'el valor de h usado es', h
      print*, ' '
      print*,'el valor de x usado es', x
      print*, ' '
      print*, 'de la funcion ', ' f(x) = exp(x)*x'
      print*, ' '
      
      
      print*, 'la matriz D es '

      do i=1,n
      print*, D(:,i)
      end do
      print*, ' '
      
      print*, 'el valor final con presicion',n, 'es', D(n,1)
      
      print*, 'el valor de la derivada analitica es ', (exp(2.)*3)
      
      return 
      end


