      subroutine intrinsictest

      integer i, ii
      integer*8 i8, ii8
      real r
      real*8 r8, rr8
      character*8 c
      complex*16 z

C     Integer intrinsics
      b1 = iabs(i)       ! Int 4
      b2 = len(c)        ! Int 4

C     Real intrinsics
      b3 = sqrt(r)       ! Real 4
      b4 = sin(r)        ! Real 4

C     Character intrinsics
      b5 = char(i)       ! char 1

C     Generic intrinsics
      b6 = abs(i)        ! Int 4
      b7 = abs(r)        ! Real 4
      b8 = min(i8, ii8)  ! Int 8
      b9 = min(r8, rr8)  ! Real 8

C     INT, INT2
      b10 = int(i8)     ! Int 4
      b11 = int(i, 2)   ! Int 2
      b12 = int(i, 4)   ! Int 4
      b13 = int(i, 8)   ! Int 8
      b14 = int(int2(i), 8) ! Int 8

      b15 = int2(i)     ! Int 2
      b16 = int2(i8)    ! Int 2
      b17 = int2(int(i8))  ! Int2

C     IAND, IOR, ISHFT
      b18 = iand(i, ii)     ! Int 4
      b19 = iand(i, 'ff'x)  ! Int 4
      b20 = iand(i, i8)     ! Int 8

      b21 = ior(i, ii)      ! Int 4
      b22 = ior('ff'x, i)   ! Int 4
      b23 = ior(i8, i)      ! Int 8

      b24 = ishft(i, 10)    ! Int 4
      b25 = ishft(i8, 10)   ! Int 8
      b26 = ishft(int2(i), 10)   ! Int 2

C     Misc
      b27 = dfloat(i)           ! Real 8
      b28 = loc(r)              ! Int 4
      b29 = loc(i8)             ! Int 4
      b30 = and(i,'0000ffff'x)  ! Int 4
      b31 = ieor(i,'0000ffff'x) ! Int 4
      b32 = sizeof(i8)          ! Int 4
      b33 = rshift(i, 10)       ! Int 4
      b34 = rshift(i8, 10)      ! Int 8
      b35 = iachar("AB")        ! Int 4
      b36 = ibits (i, 28, 4)    ! Int 4
      b37 = ibits (i8, 28, 4)   ! Int 8
      b38 = btest(2, 1)         ! Logical 4
      b39 = imag(z)             ! Real 8
      b40 = lshift(i8, 10)      ! Int 8
      end
