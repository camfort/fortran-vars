      subroutine array

      integer a(3)
      integer equiv
      integer unknown

      equivalence (a(3), equiv)

c     check points
      integer a1, a2, a3 , a4 , a5, a6, a7, a8, a9, a10, a11

      a(1) = 1
      a(2) = 2
      if (a(3) .gt. 100) then
          a(2) =  2
          unknown = 1
      else
          a(2) = -2
          unknown = 3
      endif
      a1 = a(1)         ! Const (Int 1)
      a2 = a(2)         ! Bot
      a3 = a(3)         ! Top

c     equivalence check
      a(3) = 123
      a4 = equiv        ! Const(Int 123)

c     unknown index on rhs
      a5 = a(unknown)   ! Bot
c     unknown index on lhs
      a(unknown) = 100  ! this will set whole array to Bot
      a6 = a(1)         ! Bot
      a7 = a(2)         ! Bot
      a8 = a(3)         ! Bot
      a9 = equiv        ! Bot

c     update array after processing unknown index on the lhs

      a(1) = 456
      a10  = a(1)       ! Const (Int 456))
      a11 =  a(2)       ! Bot

      end
