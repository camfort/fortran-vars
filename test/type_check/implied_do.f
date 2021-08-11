      program main
        integer*2 is(5, 5)
        data (is(i, i), i=1, 5)/5*1/
        data (is(i, 1), i=1,3)/3*1/
        data (is(i, i), i=1,3)/3*1/
        data ((is(i, j), i=1,3),j=1,3)/9*1/
        data (is(i, 1), i=1,3,2)/2*1/
        data ((is(i,j), i=1,3,1), j=1,3,2)/6*1/
        data (is(i, 2), i=3,5)/3*1/
        data (is(i, 1), i=4,2,-2)/2*1/
        data (is(i, 1), i=5,-2,-4)/2*5/
        print *, is
      end program main