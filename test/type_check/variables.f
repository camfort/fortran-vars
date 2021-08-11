      subroutine variables

      byte         b

      character     c
      character*7   c1
      character     c2*7
      character*(*) c3
      character     c4*(*)

      parameter (c3 = '1234')
      parameter (c4 = '12345678')


      integer      i
      integer*2    i2
      integer*4    i4
      integer*8    i8
C     ns denotes non-standard
      integer      i2ns*2
      integer      i4ns*4
      integer      i8ns*8

      logical      l
      logical*1    l1
      logical*2    l2
      logical*4    l4
      logical*8    l8
      logical      l2ns*2
      logical      l4ns*4
      logical      l8ns*8

      real         r
      real*4       r4
      real*8       r8
      real         r4ns*4
      real         r8ns*8
      double precision  dp

      complex      comp
      complex*8    comp8
      complex*16   comp16
      double complex dcomp

c      integer    i2_arr*2(3,4), i8_arr(3,4)*8

C     test types of the RHS expressions
      b = b

      c = c
      c1 = c1
      c2 = c2
      c3 = c3
      c4 = c4

      i =i
      i2 = i2
      i4 = i4
      i8 = i8
      i2ns  = i2ns
      i4ns = i4ns
      i8ns = i8ns

      l = 1
      l1 = l1
      l2 = l2
      l4 = l4
      l8 = l8
      l2ns = l2ns
      l4ns = l4ns
      l8ns = l8ns

      r = r
      r4 = r4
      r8 = r8
      r4ns = r4ns
      r8ns = r8ns
      dp = dp

      comp = comp
      comp8 = comp8
      comp16 = comp16
      dcomp = dcomp

      stop
      end
