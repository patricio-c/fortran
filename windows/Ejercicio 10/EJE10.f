       function func(x)
       func=sin(x)
       end

       Program ejercicio 10b
        

        implicit none
        
        external func
        real:: A,C,func
        integer:: i,j,k,V,N
        real,allocatable,dimension(:)::L, B, D, P

        open(20, FILE='ejercicio10.dat', STATUS='UNKNOWN')
        
        call inter(B,N,D,V)

        allocate(P(V),L(N))



 !     tengo un vector que es un conjunto de los puntos x

        do k=1,V   !V es el tama¤o de la cantidad de puntos que quiero interpolar (los x que yo quiera

         do i=1,N    !N es el tama¤o de los puntos que yo conozoco
           A=1
           C=1
           do j=1,N
              if (i /= j) then

                 A=(B(i)-B(j))*A
                 C=(D(k)-B(j))*C

              end if

          end do
              L(i)=(C/A) ! aca estan en cada entrada un L valuado en algun punto
              P(k)=L(i)*func(B(i))+p(k) ! El polinomio va a tener tantas entradas como valores se desean calcular y de grado

          end do

        end do   !me quedo con un vector P que tienen los valores interpolados para cada x en cada entrada



        do k=1,V
           write(20,*) k , D(k),P(k), func(D(k))
        end do

       deallocate(B)
       deallocate(L)
       deallocate(P)
       deallocate(D)

       
!      ----------------------------------------------------------------
   !    subrutinas
!      ----------------------------------------------------------------

       contains

!      ----------------------------------------------------------------

        subroutine  inter(B,N,D,V)

        real,allocatable,dimension(:)::B,D
        real::k,f,k2,f2,ran
        integer:: N,V
        write(*,*) 'introduzca'
        write(*,*) 'el orden de polinomio que desea'
        read(*,*) N
        write(*,*)'el numero de puntos con el que quiere interpolar'
        read(*,*) V
        write(*,*) 'numero real menor del intervalo'
        read(*,*) k
        write(*,*) 'introduzca el numero mayor del intervalo'
        read(*,*) f

        
  !     presentacion de los datos

         N=N+1
        allocate(B(N),D(V))

        do i = 1, N
           call random_number(ran)
           B(i)=k+ran*(f-k)
           print*, B(i)
        end do
          pause
        do i = 1, V
           call random_number(ran)
           D(i)=k+ran*(f-k)
        end do

        end subroutine

!      ----------------------------------------------------------------

        end

!      ----------------------------------------------------------------

