!       Problema 3

!       Resolver la ecuaci�n x = Cos(x) por los metodos de biseccion,
!       Newton-Raphson y secante. Comparar la convergencia de cada uno
!       de estos metodos con el numero de iteraciones necesarias para
!       alcanzar una precision de 6 decimales.

!      defino funciones

        function func(x)
        func= cos(x)- x
        end


        function gunc(x)
        gunc= -(sin(x)+1)
        end

!       inicio de programa

       program  ejerc 3
       
        implicit none
       
!      llamo a funciones externas

       external func
       external gunc
       
!      defino variables

        integer :: i
        real :: x1, x2, x3, xm, G, func, gunc, xneg, xpos, pres

 !      primer saludo en la pantalla
 
       print *,'hola, antes de empezar, la funcion que se intenta'
       print *,'solucionar es x=cos(x), antes de continuar'
       print *,'grafique la funcion y observe un intervalo que contenga'
       print *, 'un cero, luego:'
       print *, '                                                      '
       
!      introduzco valores manualmente

        call valIni(xpos, xneg, pres)
        
!       abro archivo donde se guardaran los datos

        open (10, FILE='Ejer3.dat', STATUS='UNKNOWN')

!       metodo de Biseccion
   !   info para guardar en el doc
   
        write(10,*) "Metodo Biseccion"
        write(10,*) "N         x1          x2        Xm       presic�on"

 !     defino las dos variables de entrada
 
        x1=xneg
        x2=xpos
        G=(xneg+xpos)/2

 !      comienza el primer programa
 
        do while ( abs(G) > pres)

           i = i+1
           xm =(x1 + x2)/2
        
 !      escribo los valores nuevos

           write(10,*) i,x1,x2,xm, abs(G)

 !      reemplazo de valores xneg o xpos y la presicion alcanzada

           call remplazo(func(xm))
           G=(x2-x1)

 !       si falla
       if (i == 100) then
            write (10,*) "el metodo fallo"
            GOTO 22
             else
       end if

       end do

22     continue
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

         write(10,*) i,xm,x2,G

   !   defino la nueva presicion

           G=(x2-xm)
           

  !     cambio las variables

           xm=x2
           
   !       si falla
   
       if (i == 50) then
            write (10,*) "el metodo fallo"
            GOTO 23
             else
       end if

        end do
        
23     continue

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

           write(10,*) i,x1,x2,x3,G

   !   defino la nueva presicion

           G=(x2-x1)

  !     cambio las variables

           x1=x2
           x2=x3
           
 !       si falla
 
       if (i == 50) then
            write (10,*) "el metodo fallo"
            GOTO 24
             else
       end if

       end do

24     continue
        
 !     cierro el archivo
 
         close(10)

!      ----------------------------------------------------------------
   !    subrutinas
!      ----------------------------------------------------------------

       contains
       
!      ----------------------------------------------------------------

       subroutine valIni (t,y,p)
            real :: t, y, p
            write(*,*) "valor x inicial talque la funcion sea positiva"
            read(*,*) t
            write(*,*) "valor x inicial talque la funcion sea negativa"
            read(*,*) y
            write(*,*) "presicion deseada (ej: 0.0001)"
            read(*,*) p
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
             write (*,*) "es la solucion", func(xm)
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
        end
!      ----------------------------------------------------------------


