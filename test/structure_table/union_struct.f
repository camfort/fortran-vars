      program main
        structure /foo/
          union
            map
              integer*4 bar
            end map
            map
              integer*2 baz
              integer*8 qux
            end map
          end union
        end structure
      end