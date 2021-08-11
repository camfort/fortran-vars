      subroutine expression

      byte         b

      character     c
      character*7   c7

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
      double precision  dp

      complex      comp
      complex*8    comp8
      complex*16   comp16
      double complex dcomp

C     test types of the RHS expressions

C     Unary Expressions
      u1 = +i
      u2 = -i
      u3 = +r
      u4 = -r
      u5 = .NOT. l
      u6 = .NOT. .TRUE.

C     Arithmetic Expressions - Type Promotion
C     different kind of integers
      a1  = i + i2   ! int 4
      a2  = i - i4   ! int 4
      a3  = i * i8   ! int 8
      a4  = i8 / i2  ! int 8
      a5  = i8 ** i4 ! int 8

C     integer and logical
      a6  = i + l    ! int 4
      a7  = i2 + l   ! int 4
      a8  = i + l1   ! int 4
      a9  = l8 + i   ! int 8

C     integer and real
      a10 = i + r    ! real 4
      a11 = r + r8   ! real 8
      a12 = i + r8   ! real 8
      a13 = r + dp   ! real 8
      a14 = dp + r   ! real 8
      a15 = l + r8   ! real 8
      a16 = dp - i   ! real 8
      a17 = r + i8   ! real 4, even integer is 8 bytes

c     complex with integer and real
      a18 = comp + i      ! comp 8
      a19 = comp - r      ! comp 8
      a20 = comp16 * i    ! comp 16
      a21 = dcomp / comp  ! comp 16

C     String Concatenation
      s1 = c // c7   ! char 8
      s2 = c7 // c   ! char 8

C     Relational Expression
      re1 = i .LT. i8  ! logical 4
      re2 = r .GT. i8  ! logical 4
      re3 = r .EQ. r4  ! logical 4
      re4 = i .NE. i4  ! logical 4
      re5 = r8 .LE. r4 ! logical 4
      re6 = r .GE. i   ! logical 4

C     Logical Expression
      le1 = l .AND. l2    ! logical 4
      le2 = i8 .OR. l     ! logical 8
      le3 = l1 .XOR. l2   ! logical 2
      le4 = l1 .EQV. l2   ! logical 2
      le5 = l  .NEQV. l2  ! logical 4

C     More expressions
      e1 = 42 * i8 - i4             ! int 8
      e2 = 42 + len(c // c7)        ! int 4
      e3 = (1, 2) + (i8 - 12.34)    ! complex 8
      e4 = i .GT. 42 .AND. i .LE. r ! Logical 4
      e5 = int(r) + z'1f' * 2       ! Int 4

      end

