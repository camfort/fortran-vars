      subroutine foo

      byte         b

      character     c 
      character*7   c1 
      character     c2*7
      character*(*) c3
      character     c4*(*)

      parameter (c3 = 'Hello World')
      parameter (c4 = 'Hello right back at you')


      integer      i
      integer*2    i2
      integer*4    i4
      integer*8    i8

      logical      l
      logical*1    l1
      logical*2    l2
      logical*4    l4
      logical*8    l8

      real         r
      real*4       r4
      real*8       r8

      DOUBLE PRECISION  dp

      complex      comp
      complex*8    comp8
      complex*16   comp16

      DOUBLE COMPLEX dcomp

      integer    i2_ns*2
      integer    i4_ns*4
      integer    i8_ns*8
      real       r2_ns*2, r8_ns*8
      logical    l2_ns*2, l8_ns*8


      integer    i2_arr*2(3,4), i8_arr(3,4)*8

      return
      end

