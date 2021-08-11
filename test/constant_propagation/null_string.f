      program null_string
       character*5 a, b
       character c, d

       a = '12345'

       a(3:3) = ''

       b = a

       if (b(3:3) .eq. ' ') then
        c = 'A'
       endif

       if (b(3:3) .eq. '') then
        d = 'B'
       endif

       print *, c
       print *, d
      end
