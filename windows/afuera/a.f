        program random
        INTEGER*4 i, n
        real r(10)

        do n = 1, 1000
         r(n) = ran ( )
        end do

        write (*,*) r
        pause
        end

