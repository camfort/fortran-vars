      subroutine subscripts(a)
        character*10 a
        character*10 b
        character*10 c
        character*10 d

        character*5 bottom
        character*5 top
        character*10 const1
        character*5 const2
        character*5 const3

        bottom = a(1:5)
        top = b(1:5)
        c = "&&&&&&&&&&"
        d = "0123456789"

        const1 = d
        const2 = d(1:5)
        const3 = d(6:10)
      end
