      subroutine shortcircuit
c     uninitialzied variables
      logical l
      integer i
      real    r

c     check points
      logical logical1, logical2, logical3, logical4
      integer int1, int2
      real real1, real2, real3, real4, real5, real6

c    logical short circuiting
      logical1 =   l1 .AND. .FALSE.
      logical2 =   .FALSE. .AND. l1
      logical3 =   .TRUE. .OR. l1
      logical4 =   l1 .OR. .TRUE.

c    multiplication short circuiting
      int1 = 0 * i
      int2 = i * 0
      real1 = r * 0.0
      real2 = 0.0 * r

c    exponentiation short circuiting
      real3 = 1 ** i
      real4 = 1.0 ** r
      real5 = r ** 0
      real6 = r ** 0.0

      end

