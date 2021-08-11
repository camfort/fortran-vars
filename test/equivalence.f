      subroutine foo

c     equivalence between variable and single dimensional array
      integer len
      parameter (len = 10)

      integer var
      integer array(20)
      equivalence(var, array(len + 5))


c     equivalence for substring

c     equivalence among multiple entities
c      character a*4, b*4, c(2)*3
c      equivalence (A, C(1)), (B,C(2))

c     equivalence causes merging of memory blocks
c     size are equal, but offsets are different
      real b(10)
      real c(10)
      equivalence (b(10), c(5))

c     offsets are equal, but sizes are different
      integer d(10)
      integer e(5)
      equivalence (d, e)

c     three way equivalence
      character aa*4, bb*4, cc(2)*3
      equivalence (aa, cc(1)), (bb, cc(2))

c     three way equivalence
      integer*2    sndbuff(16)
      integer*4    sndbbid1, sndbbid2
      equivalence  (sndbuff(2),sndbbid1)
      equivalence  (sndbuff(4),sndbbid2)

c    equvalence list
      real r1
      real r2
      real r3

      integer i1
      integer i2
      integer i3
      equivalence (r1, r2, r3), (i1, i2, i3)

c     equivalence for multidimensional array
      real onedim(10)
      real twodim(10, 10)
      real threedim(10,10,10)
      real fourdim(10,10,10,10)
      real fivedim(10,10,10,10,10)
      real sixdim(10,10,10,10,10,10)
      real sevendim(10,10,10,10,10,10,10)

      equivalence (threedim(2,2,2), twodim(2,2))
      equivalence (sevendim(2,2,2,2,2,2,2), fivedim)

c     substring
      character inbuf*80
      character exchange_code*1
      character price*6
      equivalence(inbuf(10:),exchange_code)
      equivalence(inbuf(23:28),price)

      CHARACTER control_switches(20)*8
      CHARACTER*1 tick
      equivalence (tick, control_switches(1)(8:8))

c     repeated equivalences
      integer*4 p6buf(4)
      integer*2 p6buf2(8)
      integer*4 p6uuid

      equivalence (p6buf(1),p6buf2(1))
      equivalence (p6buf(1),p6uuid)
      equivalence (p6buf2(1),p6uuid)
      equivalence (p6buf(1),p6buf2(1))
      equivalence (p6buf(1),p6buf2(1),p6uuid)

      return
      end
