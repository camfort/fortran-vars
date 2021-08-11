      program main
        structure /foo/
          integer foo_bar
        end structure
        record /foo/ my_foo
        record /foo/ my_foo_2
        integer qux
        common my_foo, my_foo_2, qux
      end program main