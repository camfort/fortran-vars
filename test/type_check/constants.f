      subroutine constants

C     Integer Constant
C     4 byte integer
      i1 = 0
      i2 = -10
      i3 = +199
      i4 = 2147483647
      i5 = -2147483648

C     Logical Constant
      l1 = .TRUE.
      l2 = .FALSE.

C     Real Constant
      r1 = -32.
      r2 = -32.18
      r3 = 1.6E-9
      r4 = 7E3
      r5 = 1.6E12

C     Double Precision Real Constant
      r6 = 1.6D-9
      r7 = 7D3

C     Complex
      c1 = ( 9.01, .603 )
      c2 = ( +1.0, -2.0 )
      c3 = ( +1.0, -2 )
      c4 = ( 1, 2 )

C     Double Complex
      c5 = ( 9.01D6, .603 )
      c6 = ( +1.0, -2.0D0 )
      c7 = ( 1D0, 2 )

C     Character Constant
      s1 = '12345678'

C     BOZ Constant
      b1 = B'0011111'
      b2 = O'37'
      b3 = X'1f'
      b4 = Z'1f'

C     Hollerith Constant
      h1 = 2Hok
      h2 = 8HDEADBEEF

      end
