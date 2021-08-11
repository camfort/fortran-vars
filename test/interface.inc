        interface sespit_get_psetdt
          function sespit_get_psetdt()
            integer*2 sespit_get_psetdt(3)
          end function
        end interface

        interface
          function sespit_get_psetdt2_e(index)
            integer*2 sespit_get_psetdt2_e
            integer*4 index
          end function
        end interface

        interface
          subroutine sespit_set_trdnum(value)
            integer*2 value
          end subroutine
        end interface