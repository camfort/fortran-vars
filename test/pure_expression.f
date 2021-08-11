      subroutine pure

c     check point variable is used as the LHS of an assignment statement, whose
c     RHS is being tested.
      integer i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11
      logical b1, b2, b3
      integer var, arr(10), arr2(4,5)
      integer func
      logical funcb
      real r1

      ! cosntants, variable reference, mathematical expressions are pure
      i1 = 123         ! pure
      i2 = var         ! pure

      i3 = 123 + var   ! pure
      i4 = 123 ** 2    ! pure
      b1 = var .GT. 1 .AND. var .LE. 100 ! pure
      b2 = .NOT. .True.

      ! Arrays
      i4 = arr(1)      ! pure
      i5 = arr2(var, 4) ! pure

      ! External functions are by default not pure
      i6 = func(1,3)     ! not pure
      i7 = arr(func(3,4)) ! not pure
      i8 = arr2(func(3,4), 5) ! not pure
      i9 = func(3,4) + 123 ! not pure
      b3 = .NOT. funcb()  ! not pure

      ! Intrinsic functions are pure
      r1 = sqrt(100)                    ! pure
      i10 = 123 + max (1, var, arr(3))  ! pure
      i11 = max (1, func(1,2))          ! not pure

      stop
      end
