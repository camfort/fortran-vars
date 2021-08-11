      subroutine array

      character*10 strScalar

      character s1, s4

      strScalar = 'ABCDEFGHI'
      strScalar(4:5) = '45'
      s1 = strScalar(1:1)
      s4 = strScalar(4:4)

      end
