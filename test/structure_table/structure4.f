      subroutine sub1()
        structure / str_inner /
          integer*1 inner_arr(1:5)
        end structure
        record / str_inner / struct
        struct.inner_arr(2) = 123
        print *, struct
      end subroutine sub1

      subroutine sub2()
        structure / str_inner /
          integer*2 inner_arr(1:5)
        end structure
        record / str_inner / struct(1:5)
        struct(1).inner_arr(2) = 123
      end subroutine sub2

      subroutine sub3()
        structure / str_inner /
          integer*1 inner_arr(1:3)
        end structure
        structure / str_outer /
          record / str_inner / outer_arr(1:5)
        end structure
        record / str_outer / struct
        struct.outer_arr(1).inner_arr = 123
      end subroutine sub3
