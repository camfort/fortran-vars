c     test for function call
      subroutine unary
      integer a, b
      integer myfunc
      equivalence (a, b)

c     check points
      integer a1, a2, a3, a4, a5

      b = 2
      a1 = b            ! Const (Int 2)
      a2 = b            ! Const (Int 2)

      a3 = myfunc(b)    ! Bot

      a4 = b            ! Bot
      a5 = a            ! Bot
      end

      subroutine binary
      integer a, b
c     check points
      integer b1, b2, b3, b4, b5
      integer myfunc2

      a = 3
      b = 4

      b1 = a               ! Const (Int 3)
      b2 = b               ! Const (Int 4)

      b3 = myfunc2(a, b)   ! Bot

      b4 = a               ! bot
      b5 = b               ! Bot
      end

      subroutine array
      integer a(2)
c     check points
      integer e1, e2, e3, e4, e5
      a(1) = 1
      a(2) = 2

      e1 = a(1)            ! Const (Int 1)
      e2 = a(2)            ! Const (Int 2)

      e3 = myfunc(a)       ! Bot

      e4 = a(1)            ! Bot
      e5 = a(2)            ! Bot
      end

c     It appears 'myfunc' definition is needed here for fortran-src
c     to correctly parse the function call 'a3 = myfunc(b)' in program
c     unit 'unary'. Without the function definition the function call
c     is parsed as array indexing.

      integer function myfunc(x)
      integer x
      integer myfunc
c     check points
      integer c1, c2
      myfunc = x * x
      c1 = x               ! Bot
      c2 = myfunc          ! Bot
      return
      end

      integer function myfunc2(x, y)
      integer x, y
c     check points
      integer d1, d2, d3
      integer myfunc2
      myfunc2 = x * y
      d1 = x               ! Bot
      d2 = y               ! Bot
      d3 = myfunc2         ! Bot
      return
      end
