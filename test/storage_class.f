      subroutine foo

c     default unspecified
      integer a

c     static variables - save follows declaration 
      integer b1
      save b1

c     static variables - save precedes declaration 
      save b2
      real b2 
      
c     automatic variables - automtic follows declaration 
      integer c1 
      automatic c1 

c     automatic variables - automtic precedes declaration 
      automatic c2 
      real c2 

c     common
      common /X/ d1, d2
      integer d1
      real    d2

      return
      end



          
