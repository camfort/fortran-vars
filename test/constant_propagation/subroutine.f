c     test for subroutine call
      subroutine unary
      integer a, b
      equivalence (a, b)

c     check points
      integer a1, a2, a3, a4
      a = 1

      a1 = a  ! Const (Int 1)
      a2 = b  ! Const (Int 1)

      call sub1(a)

      a3 = a  ! Bot
      a4 = b  ! Bot
      end


      subroutine binary
      integer a, b

c     check points
      integer b1, b2, b3, b4

      a = 1
      b = 2
      b1 = a  ! Const (Int 1)
      b2 = b  ! Const (Int 2)

      call sub2(a, b)

      b3 = a  ! Bot
      b4 = b  ! Bot
      end


      subroutine array
      integer a(2)

c     check points
      integer c1, c2, c3, c4
      a(1) = 1
      a(2) = 2

      c1 = a(1)  ! Const (Int 1)
      c2 = a(2)  ! Const (Int 2)

      call sub3(a)

      c3 = a(1)  ! Bot
      c4 = a(2)  ! Bot
      end
