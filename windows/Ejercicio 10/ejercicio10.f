!       function func(x)
!        func=sin(x)
!        end

      
        Program ejercicio 10
        
       implicit none

        external fibonacci
        external result
        integer::n,i,m,j
        real::x,C,fibonacci,result
        real,allocatable,dimension(:)::L, B, A

        open(10, FILE='ejercicio10.dat', STATUS='UNKNOWN')
        
        call  inter(B,N)
        allocate (L(N))
        print*, result(5)
        
        do j= 1,M
           do i= 1,N
              if (i/=j) then

                 C(x)=C(x)*(x-B(i))/(B(j)-B(i))
                 
              end if
           end do
        L(j)=C(x)
        end do



!      ----------------------------------------------------------------
   !    subrutinas
!      ----------------------------------------------------------------

       contains

!      ----------------------------------------------------------------

        subroutine  inter(B,N)

        real,allocatable,dimension(:)::B
        real::k,f
        integer:: N
        write(*,*) 'introduzca el numero de puntos que desea'
        read(*,*) N
        write(*,*) 'introduzca el numero menor del intervalo'
        read(*,*) k
        write(*,*) 'introduzca el numero mayor del intervalo'
        read(*,*) f

 !     presentacion de los datos

        write(20,*) ' '
        write(20,*) 'Notacion:'
        write(20,*) ' '
        write(20,*) "     Ye,Xe = valor de Y/X experimental"
        write(20,*) "     Ya = valor de Y ajustado"
        write(20,*) ' '
        write(20,*) "En el intervalo",'(',k,',',f,')'
        write(20,*) "usando ",N,' Puntos'

        allocate(B(N))

        do i = 1, N
           B(i)= ran ( )
           B(i)=k+B(i)*(f-k)

        end do

        end subroutine

!      ----------------------------------------------------------------

        end

!      ----------------------------------------------------------------
        recursive function fibonacci(i) result(fibo)
         integer, intent(in) :: i
         integer :: fibo

           if (i <= 1) then
               fibo = 1
                 else
               fibo = fibonacci(i-1) + fibonacci(i-2)
           end if

           end function fibonacci

 !       REAL FUNCTION HORNER(A,N,X)
 !       INTEGER I,N
 !       REAL    A(0:N),X

 !       HORNER = A(N)
 !       do 10 I = N-1,0,-1
 !       HORNER = A(I) + HORNER*X
 !       END
