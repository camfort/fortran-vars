      program multi_subscript
       character*10 s(5)

       character p
       character*3 q

       p = s(4)(3:3)
       q = s(2)(1:3)

       print *, q
      end
