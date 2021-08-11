c     check the value of function arguments
      subroutine foo
      integer a, b, c
      a = 1
      b = 2
      c = bar (a, b)     ! 1, 2
      d = bar (a, b)     ! Bot, Bot
      e = bar (1 + 2, c) ! 3, Bot
      return
      end

      integer function bar(x, y)
      integer x, y
      integer bar
      bar = x + y
      return
      end
