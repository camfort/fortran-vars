      subroutine sub(stscalar1, starr1, dynscalar1, dynarr1, dynarr2,
     + dynarr3, dynarr4, *)
      integer stscalar1
      integer starr1(5)

      character*(*) dynscalar1
      integer dynarr1(*)
      integer dynarr2(stscalar1)
      integer dynarr3(3,*)
      integer dynarr4(3,stscalar1)

      integer dynarr5(stscalar1 + 1)

      stop
      end
