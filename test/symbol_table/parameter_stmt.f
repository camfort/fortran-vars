      program parameter_stmt
      character*7   c1
      character*(*)  c2
      parameter (c1 = "Hi", c2 = "Hello World")

      integer       i
      integer*2     i2
      integer*4     i4
      integer*8     i8
      parameter(i=2, i2=2, i4=2, i8=2)

      real          r
      real*2        r2
      real*4        r4
      real*8        r8
      parameter(r=2, r2=2, r4=4, r8=8)

      end
