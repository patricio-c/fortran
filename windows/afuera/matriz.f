
    !   include 'matrizmxn.f'
       program matrices
       include 'matrizmxn'
       real:: hus(5,5)
       real, dimension(3,3)::JZ,A,B
       INTEGER::MATRIZ2(2,2),Jazz(4,3)
       integer,dimension(2,3)::jz2,c,d
       REAL:: F
      integer:: i

       
       A(1,:)=1
       A(2,:)=2
       A(3,:)=3
       
       B(1,:)=1
       B(2,:)=2
       B(3,:)=3

       B=A*B
    !   read(*,*)matriz
       pause
!       write(*,*)matriz(1,:)
 !      write(*,*)matriz(2,:)
  !     write(*,*)matriz(3,:)
       pause
       read(*,*) F
       end program
       
       
