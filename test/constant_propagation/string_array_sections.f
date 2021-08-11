      program string_array_sections
       character*2 a(3, 3)
       character b

       a(2, 1:2) = 'AB'

       b = a(2, 1)(1:1)
      end
