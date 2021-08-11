      subroutine foo
      common /buf/ B, C(3)
      integer B, C(3)
      integer before1, before2, after1, after2
      B = 1
      C(2) = 2
      before1 =  B    ! Top as common variables are skipped
      before2 =  C(2) ! Top
      call bar ()
      after1 = B      ! Top
      after2 = C(2)   ! Top
      stop
      end

      subroutine bar
      common /buf/ B, C(3)
      integer B, C(3)
      B = 42
      C(2) = 42
      stop
      end

