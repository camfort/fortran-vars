      program overriding_kind_defs
        integer a*2
        integer a*4       ! kind = 4

        character b*5
        character b*10    ! kind = 10

        integer*2 c*4     ! kind = 4

        character*5 d*10  ! kind = 10

        character*(*) e*5 ! kind = 5
      end
