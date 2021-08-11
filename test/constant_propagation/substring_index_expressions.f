      subroutine substring_index_expressions(i)
       character*5 a
       character*2 b
       character c
       character d
       character*5 e
       integer j

       a = '12345'

       j = 3

       b = a(3 + 1 : 3 + 2)

       c = a(1 + j : 1 + j)

       d = a(i : i)

       a(1 + i : 2 + i) = 'AB'

       e = a
      end
