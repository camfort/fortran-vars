      program main
        structure /foo/
          integer*8 bar
        end structure
        structure /baz/
          record /foo/ qux
          character*10 quux
        end structure
        structure /quuz/
          record /baz/ corge
          integer*2 foobar
        end structure
        structure /grault/
          record /quuz/ garply
        end structure

        record /grault/ test
        test.garply.corge.quux = "abcde"
      end