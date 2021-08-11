      subroutine suba
      common /reqnamecomn/ reqname_a, rcode_a
      character*7 reqname_a(64)
      integer rcode_a
      return
      end

      subroutine subb
      common /reqnamecomn/ reqname_b
      common /reqnamecomn/ rcode_b
      character*7 reqname_b(64)
      integer rcode_b
      integer ext(10)
      equivalence (rcode_b, ext)
      return
      end

      subroutine subc
      integer a, common_block_name, b
      common /common_block_name/ a, common_block_name, b
      return
      end

      subroutine subd
      common /common_block_1/ a(10), b(30,2), c
      integer a*8, b*4, c*2
      integer d*8, e*4, f*2
      common /common_block_2/ d(-3:5), e(2:5,3:7), f
      return
      end
