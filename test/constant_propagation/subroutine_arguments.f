c     check the value of subroutine arguments
      subroutine foo
      integer a, b, c, d
      PARAMETER ( d = 1 )
      a = 1
      b = 2
      call bar (a, b)     ! 1, 2
      call bar (a, b)     ! Bot, Bot
      a = 3
      b = 4
      call bar (1 + 2, a + b) ! 3, 7

      call bar (0, d) ! 0, 1

      return
      end
