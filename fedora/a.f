	program principal
	real::a							!-------------------
        real,allocatable,dimension(:)::B
	
      print*, 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa        
     + hola'
      call VAL(a) 
       
	allocate (B(5))
	B(:)=1.0d0
	print*,B(:)
      if (3==a) then
     	print*, 'como hola'
      else if (4==a) then
      	print*, 'si'
      else 
      	print*, 'no'
      end if
      
      contains
      subroutine VAL(R)
      real::R
      write(*,*) 'r'
      read(*,*) R
	end subroutine
	end program
	
	

