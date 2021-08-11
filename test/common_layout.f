      subroutine suba
      common /cname_a/ char_a, int_b, char_c
      character char_a
      integer int_b
      character char_c
      integer not_here
      return
      end

      subroutine subb
      common /cname_b/ char_array_a
      common /cname_b/ int_b
      character*7 char_array_a(65)
      integer int_b
      integer int_array_c(10)
      equivalence (int_b, int_array_c)
      return
      end

      subroutine subc
      integer int_a, int_b, int_c, int_d
      common /cname_c/ int_a, int_c, int_b
      common /cname_cc/ int_d
      return
      end
