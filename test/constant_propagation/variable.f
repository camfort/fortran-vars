      subroutine branch
      integer a, b, c, d
      equivalence (a, d)
      integer checka, checkb, checkc, checkd
      a = 1
      b = 2
      if (c .gt. 100) then
          b = a
      else
          b = -1 * a
      endif
      checka = a
      checkb = b
      checkc = c

      a = 123
      checkd = d
      end
