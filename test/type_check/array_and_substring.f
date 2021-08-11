      subroutine array(c,d,N)
      integer a(10)       ! local
      integer b(-5:5, 5)  ! local
      integer N
      integer c(N)        ! adjustable
      integer d(10, *)    ! assumed-size

C     test the types of RHS expressions
      arr1 = a(1)
      arr2 = b(-1, 2)
      arr3 = c(1)
      arr4 = d(1, 1)
      arr5 = b(1)

c     test ranges
      i1   = a(3:5)
      i2   = a(1:1)
      i3   = c(3:)

c     test erroneous expressions where we have too many indices
      err1 = a(2, 3)
      err2 = b(2, 3, 4)
      err3 = d(2, 3, 4)
c     Can't deal with array
      err4 = d(2, 2:5)
      end

      subroutine string(a, b, c, n)
      character*(*) a
      character*(*) b(3)
      character*(*) c(n)

      character*10 d
      character*10 e(4, 5)
      integer n

C     test the types of RHS expressions
      s1 = a
      s2 = b(1)
      s3 = c(1)

c     substring
      s4 = d(:5)
      s5 = d(5:)
      s6 = d(:)
      s7 = e(1,2)(1:3)

      s8 = d(:n)
      s9 = d(1:n)
      s10 = d(n:)
      end
