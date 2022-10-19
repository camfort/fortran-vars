      subroutine foo
c     only parameter statement
      parameter (a = 10)

c     parameter statement first, declaration later
      parameter (b = 10)
      integer b

c     declaration first, parameter statement later
c     2022-09-02 raehik: upgraded to REAL(8) to avoid 4->8 conversion losing
c     accuracy, since ExpVal uses Double=>REAL(8))
      real*8 r 
      parameter (r = 3.14D0)

c     integer arithmetics with variable
      integer c 
      parameter (c = 10)
      parameter (d1 = c + 2) 
      parameter (d2 = c - 2) 
      parameter (d3 = c * 2) 
      parameter (d4 = c / 3) 
      parameter (d5 = -c ) 
      parameter (d6 = +c ) 

c     float arithmetics with variable
c     2022-09-02 raehik: upgraded to REAL(8) to avoid 4->8 conversion losing
c     accuracy, since ExpVal uses Double=>REAL(8))
      real*8 pi 
      parameter (pi = 3.14D0)
      parameter (e1 = pi + pi) 
      parameter (e2 = pi - pi) 
      parameter (e3 = pi * 2.0D0) 
      parameter (e4 = pi / 2.0D0) 
      parameter (e5 = -pi) 
      parameter (e6 = +pi) 

c     double precision constant
      REAL*8 eps
      parameter (eps = 1.0D-12)


c     logical expressions : 
c     TODO: .AND. .OR.
      parameter (f = .TRUE.)
      parameter (f1 = .NOT. f)

c     TODO: relational expressions 

c     string expression 
      CHARACTER*(*) RCSID
      parameter (RCSID = 'DEADBEEF')

c     intrinsics
      parameter (d7 = ior(1,8))

      parameter (d8 = max(50,60))
      parameter (d9 = max(40,50,60,70))
      parameter (e7 = max(1.2D0,1.1D0))
      parameter (e8 = max(1.2,2))

      character*1 c1
      parameter (c1 =  char(65)) !'A'
      character*2 eol
      parameter( eol = char(13)//char(10) )  !"\r\n"

      integer*4 i1, i2, i3, i4, i5, i6, i7, i8, i9
      parameter (i1  = not(1))      ! -2
      parameter (i2  = int(42))     ! 42
      parameter (i3  = int(-42))    ! -42
      parameter (i4  = int(0.1))    ! 0
      parameter (i5  = int(1.1))    ! 1
      parameter (i6  = int(1.9))    ! 1
      parameter (i7  = int(-1.1))   ! -1
      parameter (i8  = int(-1.9))   ! -1
      parameter (i9  = int(z'10', 4)) ! 16

      return 
      end
