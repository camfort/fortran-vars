      subroutine foo
c     single dimension
      real a(10)
      real b(-3:5)
      parameter(num=10)
      real c(num * 4 + 5)

c     multi-dimensions
      real a2(5,5)
      real a3(5,5,5)
      real a4(5,5,5,5)
      real a5(5,5,5,5,5)
      real a6(5,5,5,5,5,5)
      real a7(5,5,5,5,5,5,5)

c     dimension statement
      real d
      dimension d(10)

      integer m
      parameter (i = 10, j = 20)
      dimension m(i,j)

c     string array
      character*7 reqname(64)
      character   test(3,4)*7

c     integer array
      integer     itest1*8(3,4)
      integer     itest2(3,4)*8

c     dimension declaration within COMMON - ExpSubscript in AST
      integer arr_before_range,
     +        arr_before_multi
      common /comm1/ arr_before_range(8:10),
     +               arr_before_multi(12, 14:16),
     +               arr_after_range(22:24),
     +               arr_after_multi(26, 28:30)
      integer arr_after_range,
     +        arr_after_multi

c     dimension declaration within COMMON - ExpFunctionCall in AST
      integer*2 arr_before_standard_kind
      integer arr_before_simple,
     +        arr_before_nonstandard_kind*2
      common /comm2/ arr_before_standard_kind(2),
     +               arr_before_simple(4),
     +               arr_before_nonstandard_kind(6),
     +               arr_after_standard_kind(18),
     +               arr_after_simple(20),
     +               arr_after_nonstandard_kind(2)
      integer*2 arr_after_standard_kind
      integer arr_after_simple,
     +        arr_after_nonstandard_kind*2

      return
      end


