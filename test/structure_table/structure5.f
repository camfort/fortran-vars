      subroutine sub()
        character*10 c(10)
        c(2)(1:1) = 'ab'
        print *, c
      end subroutine sub

      subroutine sub2
        logical*2 xyz(5, 5, 5)
        xyz(2, 3, 4) = 5
      end subroutine sub2