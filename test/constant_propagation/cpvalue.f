      subroutine cpvalue

      integer func
      integer top
      integer bot
      bot = func()  ! make it BOT

c     check points
      integer c1, c2, c3, c4,c5, c6, c7, c8, c9

      c1 = top + top ! TOP
      c2 = top + 1   ! Top
      c3 = 1 + top   ! Top
      c4 = bot + top ! BOT
      c5 = top + bot ! BOT
      c6 = 1 + 2     ! Const Int 3
      c7 = 1 + bot   ! BOT
      c8 = bot + 1   ! BOT
      c9 = bot + bot ! BOT
      stop
      end

      integer function func()
      stop
      end
