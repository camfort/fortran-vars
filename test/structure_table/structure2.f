      program main
        structure /foo/
          integer*8 bar
        end structure
        structure /baz/
          record /foo/ qux
          character*10 quux
        end structure
      end