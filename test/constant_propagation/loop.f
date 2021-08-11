      subroutine loop
      integer i, j, k

c     check points
      integer a1, a2, a3

      i = 1
      j = 123
 10   if (i .gt. 10) goto 40
      k = 456
      i = i + 1
      goto 10

 40   a1 = i     ! Bot
      a2 = j     ! Const (Int 123)
      a3 = k     ! Const (Int 456)
      end


      subroutine doloop
      integer i, j, k
      integer b(10)

c     check points
      integer b1, b2, b3, b4
      j = 123
      do 99  i = 2, 10
        b(i) = i
        k = 456
 99   continue

      b1 = b(1)   ! Bot
      b2 = b(2)   ! Bot
      b3 = j      ! Const (Int 123)
      b4 = k      ! Const (Int 456)
      end
