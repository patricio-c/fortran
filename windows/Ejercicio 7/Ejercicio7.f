!       problema 7
!      Los halos de materia oscura tienen una densidad volumetrica de
!      masa dada por la el perfil NFW cuya expresi�n es
!      P(r)=Po(Rs/R)*(1+R/Rs)**(-2) donde r es el radio esferico, Po es
!      la densidad volum�trica central y la longitud de escala.
!      Integrar analiticamente esta expresion para encontrar la masa
!      interior contenida dentro de un radio arbitrario r. resolver
!      disco ecuacion para encontrar el radio Rh que contiene la mitad
!      de la masa total utilizando los 3 m�todos del problema 3).

 !     Funcion integrada
 !     P(r)=4*pi*P0*(Rs**3)*[log(abs(rs+r))+rs/(rs+r)-log(rs)-1]

!      defino funciones

      function func(R,A)
        real::A
        func=((log(R+1)-A/2-(R/(R+1)))*(1/A))

      end

      function gunc(R,A)
        real::A
        gunc= (((R/((R+1)**2)))*(1/A))
       end

!       inicio de programa

       program  ejerc 7

        implicit none

!      llamo a funciones externas
       external func
       external gunc

!      defino variables

       integer :: i
       real :: x1, x2, x3, xm, G, func, gunc, xneg, xpos, pres, Rs, Rm
       real :: A,B

 !      primer saludo en la pantalla

       print *,'hola, antes de empezar, la funcion que se intenta'
       print *,'solucionar es Cte=log(R+1)-(R/(R+1)),antes de continuar'
       print *,'grafique la funcion y observe un intervalo que contenga'
       print *, 'un cero, se elige un radio maximo tq 0<R<Rmax luego:'
       print *, '                                                      '

!       abro archivo donde se guardaran los datos

        open (10, FILE='Ejer7.dat', STATUS='UNKNOWN')

 !       defino el radio maximo
7         write(*,*) "introduzca radio maximo: (si no sabe ponga 0)"
         read(*,*) Rm
             if (Rm > 0) then

                A=(log(Rm+1)-(Rm/(Rm+1)))
                B=1/A
                write(10,*) "Radio maximo elegido = ", Rm
                print*, 'Grafique la funcion ln(x+1)-x/(x+1) '
                print*, 'y multipliquela por ', B
        print*, 'luego restele 0.5 y vea el x positivo y el negativo'
             else if ( Rm == 0) then

                Rm=200
                A=(log(Rm+1)-(Rm/(Rm+1)))
                B=1/A
                write(10,*) "Radio maximo elegido = ", Rm
                print*, 'Grafique la funcion ln(x+1)-x/(x+1) '
                print*, 'y multipliquela por ', B
                print*, '   '
         print*, 'luego restele 0.5 y vea el x positivo y el negativo'

             else
             write (*,*) "error, porfavor introduzca radio positivo"
             goto 7
         end if

!      introduzco valores manualmente

        call valIni(xpos, xneg, pres,Rs)

!       metodo de Biseccion
   !   info para guardar en el doc

        write(10,*) "Metodo Biseccion"
        write(10,*) "N         x1          x2        Xm       presic�on"

 !     defino las dos variables de entrada

        x1=xneg
        x2=xpos
        G =(xneg+xpos)/2

!      comienza el primer programa

        do while ( abs(G) > pres)

           i = i+1
           xm =(x1+x2)/2

 !      escribo los valores nuevos

           write(10,*) i,x1,x2,xm, abs(G)

 !      reemplazo de valores xneg o xpos y la presicion alcanzada

           call remplazo(func(xm,A))
           G = (x2-x1)

 !      si falla

       if (i == 100) then
            write (10,*) "el metodo fallo"
            GOTO 21
             else
       end if


       end do

21     continue

!      Trasformacion de Radio si Rh es distinto de cero
       call longitud(Rs,xm)

 !     ----------------------------------------------------------------
 !      metodo newton
 !     ----------------------------------------------------------------

 !     reseteo de valores

        call reset()

 !      escribo la primera linea del doc

        write(10,*) "                                                  "
        write(10,*) "Metodo Newton"
        write(10,*) "N  val inicial   val final       presic�on"

 !      comienza el segundo programa

        do while ( abs(G) > pres)
           i = i+1
           x2= xm - func(xm,A)/gunc(xm,A)

    !    escribo doc

         write(10,*) i,xm,x2,abs(G)

   !   defino la nueva presicion

           G=(x2-xm)

  !     cambio las variables

           xm=x2

 !       si falla
       if (i == 50) then
            write (10,*) "el metodo fallo"
            GOTO 22
             else
       end if

       end do

22     continue

!      Trasformacion de Radio si Rh es distinto de cero

       call longitud(Rs,xm)

   !   ----------------------------------------------------------------
  !     metodo de la secante
  !    ----------------------------------------------------------------

   !    reseto las variables

        call reset()

  !    escribo la primer linea del doc

        write(10,*) "                                                  "
        write(10,*) "Metodo Secante"
        write(10,*) "N  val iniciales   val final       presic�on"

!      comienza el tercer programa

        do while ( abs(G) > pres)

           i = i+1
           x3= x2 - func(x2,A)*((x2-x1)/(func(x2,A)-func(x1,A)))

    !    escribo doc

           write(10,*) i,x1,x2,x3,abs(G)

   !   defino la nueva presicion

           G=(x2-x1)

  !     cambio las variables

           x1=x2
           x2=x3

 !       si falla

       if (i == 50) then
            write (10,*) "el metodo fallo"
            exit
             else
       end if

       end do




!      Trasformacion de Radio si Rh es distinto de cero

       call longitud(Rs,x1)

 !     cierro el archivo

         close(10)

!      ----------------------------------------------------------------
   !    subrutinas
!      ----------------------------------------------------------------

       contains

!      ----------------------------------------------------------------

       subroutine valIni (t,y,p,q)
            real :: t, y, p, q
            write(*,*) "valor x inicial talque la funcion sea positiva"
            read(*,*) t
            write(*,*) "valor x inicial talque la funcion sea negativa"
            read(*,*) y
            write(*,*) "presicion deseada (ej: 0.0001)"
            read(*,*) p
            write(*,*) "valor de longitud de escala (si no lo sabe"
            write(*,*) "coloque 0)"
            read(*,*) q
            i=0
       end subroutine

!      ----------------------------------------------------------------

       subroutine remplazo(c)
         implicit none
         real :: c
             if (c > 0) then
                x2 = xm
             else if ( c < 0) then
                x1 = xm
             else
             write (*,*) "es la solucion", xm
         end if
        end subroutine

!      ----------------------------------------------------------------

       subroutine reset()
            i= 0
            G= (xpos-xneg)/xpos
            x1=xneg
            x2=xpos
            xm=(x1+x2)/2
        end subroutine

!      ----------------------------------------------------------------

       subroutine longitud(Rs,xm)
       implicit none
       real:: Rs,xm
       if (Rs==0) then
          write(10,*) "Rm/Rs es: " , xm
          else
          xm= xm*Rs
          write(10,*) "Rm: " , xm
       end if
       end subroutine

!      ----------------------------------------------------------------
        end
!      ----------------------------------------------------------------


