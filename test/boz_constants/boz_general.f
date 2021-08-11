      program main

c     BOZ Constants: suffix notation
c     (no padding/truncation, no overflows, all resolves to Int 1)

      integer*2 boz_suffix_binary
      parameter (boz_suffix_binary = '0000000000000001'b)

      integer*2 boz_suffix_octal
      parameter (boz_suffix_octal = '000001'o)

      integer*2 boz_suffix_hex_x
      parameter (boz_suffix_hex_x = '0001'x)

      integer*2 boz_suffix_hex_z
      parameter (boz_suffix_hex_z = '0001'z)

c     BOZ Constants: prefix notation
c     (no padding/truncation, no overflows, all resolves to Int 1)

      integer*2 boz_prefix_binary
      parameter (boz_prefix_binary = b'0000000000000001')

      integer*2 boz_prefix_octal
      parameter (boz_prefix_octal = o'000001')

      integer*2 boz_prefix_hex_x
      parameter (boz_prefix_hex_x = x'0001')

      integer*2 boz_prefix_hex_z
      parameter (boz_prefix_hex_z = z'0001')

c     BOZ Constants: types

      integer boz_integer
      parameter (boz_integer = '01'x)   ! resolves to Int 1

      return
      end
