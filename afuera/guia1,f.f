
   !      include 'ejercicio3.f'
   !      integer::N
    !    character*3::R

!     ------------------------------------------------------------------
                              !
        print*,'Bienvenido a la resolucion de los ejercicios de la '
        print*,'guia uno, a continuacion puede introducir los numeros '
        print*,'3,4,5,6,7,8,9,10,11,12 para poder ver los programas '
        print*,'de cada ejercicio'
        print*,'  '
!     ------------------------------------------------------------------

15      continue
        print*,' '
        print*, 'introduce el numero de ejercicio para acceder al'
        print*, 'programa'

        read*,N

!     ------------------------------------------------------------------

        if (N==3) then
          include 'ejercicio3.f'
!        else if (N==4) then
!          include 'ejercicio4.f'
!        else if (N==5) then
!          include 'ejercicio5.f'
!        else if (N==6) then
!          include 'ejercicio6.f'
!        else if (N==7) then
!          include 'ejercicio7.f'
!        else if (N==8) then
!          include 'ejercicio8.f'
!        else if (N==9) then
!          include 'ejercicio9.f'
!        else if (N==10) then
!          include 'ejercicio10.f'
!        else if (N==11) then
!          include 'ejercicio11.f'
!        else if (N==12) then
!          include 'ejercicio12.f'
!        else if (N==13) then
!          include 'ejercicio13.f'
       else
!          include 'ejercicio14.f'
       end if

!     ------------------------------------------------------------------
        print*,' '
        print*, 'desea ver otro programa?(si/no)'
        read*, R
        if (R=='si') then
          goto 15
        else
         goto 20
        end if
        
20     continue

