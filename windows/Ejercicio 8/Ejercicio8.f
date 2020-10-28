


!      defino funciones

      function func(R)
        func=((r-log(r+1)*(r+1))/((r+1)*r**2)+1/(r+1)**2)
      end

      function gunc(x)
      gunc=((2*x**3*log(x+1)-5*x**3+6*log(x+1)*x**2-5*x**2+6*x*log(x+1)
     &-2*x+2*log(x+1))/(((x+1)**3)*x**3))

       end

!       inicio de programa

       program  ejerc 8

        implicit none

!      llamo a funciones externas
       external func
       external gunc

!      defino variables

        integer :: i
        real :: x1, x2, x3, xm, G, func, gunc, xneg, xpos, pres, Rs

 !      primer saludo en la pantalla

       print *,'hola, antes de empezar, la funcion que se intenta'
       print *,'solucionar es encontrar el maximo de V**2=GM(r)/r,'
       print *,'para ello se calcula su derivada y se ve cuando da cero'
       print*, 'grafique la funcion y observe un intervalo que contenga'
       print *, 'ese cero, luego:'
       print *, '                                                      '

!      introduzco valores manualmente

        call valIni(xpos, xneg, pres,Rs)

!       abro archivo donde se guardaran los datos

        open (10, FILE='Ejer7.dat', STATUS='UNKNOWN')

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

           call remplazo(func(xm))
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
           x2= xm - func(xm)/gunc(xm)

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
           x3= x2 - func(x2)*((x2-x1)/(func(x2)-func(x1)))

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
            exit !GOTO 23
             else
       end if

       end do

!23    continue


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
!      -----------------------------------------------------------------
        end
!      ----------------------------------------------------------------

