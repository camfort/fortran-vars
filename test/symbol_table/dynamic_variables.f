      subroutine f1(n)
        integer n
        character*(n) c
      end

      subroutine f2(n)
        integer n
        integer arr(n)
      end

      subroutine f3(n, m)
        integer n, m
        character*(n) arr(m)
      end

      subroutine f4(n)
        integer n, m
        parameter(m=5)
        character*(n) arr(m)
      end

      subroutine f5(m)
        integer n, m
        parameter(n=5)
        character*(n) arr(m)
      end

      subroutine f6(arr, n, m)
        integer n, m
        character*(n) arr(m)
      end