      program arithif
      integer n, a, b, c

c     check points
      integer a1, a2, a3

      n = 0
      a1 = b  ! Top
      if (n) 10, 20, 30
 10   b = 10
      c = 123
      goto 50
 20   b = 20
      c = 123
      goto 50
 30   b = 30
      c = 123
 50   a2 = b   ! Bot
      a3 = c   ! Const (Int 123)
      end
