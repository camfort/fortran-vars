      program main
        structure /foo/
          integer*2 bar
        end structure
        structure /baz/
          character*13 qux
        end structure
        structure /quux/
          union
            map
              record /foo/ quuz
            end map
            map
              record /baz/ corge
            end map
          end union
        end structure
        record /quux/ test
        test.corge.qux = "deadbeef"
      end