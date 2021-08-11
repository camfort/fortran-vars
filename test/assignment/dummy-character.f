      subroutine f(c)
        character*(*) c
        c = "ABCDEFGHI"
        print *, c
      end subroutine f

      program main
        character*6 c
        call f(c)
      end program main