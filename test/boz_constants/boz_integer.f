      program main
        integer*2 i2_1
        parameter (i2_1 = '1'x)
        ! pads to '0001'x and resolves to Int 1

        integer*2 i2_2
        parameter (i2_2 = '10'x)
        ! pads to '0010'x and resolves to Int 16

        integer*2 i2_3
        parameter (i2_3 = '100'x)
        ! pads to '0100'x and resolves to Int 256

        integer*2 i2_4
        parameter (i2_4 = '1000'x)
        ! doesn't pad and resolves to Int 4096

        integer*2 i2_5
        parameter (i2_5 = '7fff'x)
        ! doesn't pad and resolves to Int 32767
        ! the largest number with a positive sign
        !   (most significant bit = 0) for kind 2
        ! in other words, the highest number INTEGER*2 can take

        integer*2 i2_6
        parameter (i2_6 = '8000'x)
        ! doesn't pad and resolves to Int -32768
        ! the largest number with a negative sign
        !   (most significant bit = 1) for kind 2
        ! in other words, the lowest number INTEGER*2 can take

        integer*2 i2_7
        parameter (i2_7 = '8001'x)
        ! doesn't pad and resolves to Int -32767

        integer*2 i2_8
        parameter (i2_8 = '10000'x)
        ! truncates to '0000'x and resolves to Int 0

        integer*2 i2_9
        parameter (i2_9 = '10010'x)
        ! trunactes to '0010'x and resolves to Int 16

        integer*2 i2_10
        parameter (i2_10 = '00010'x)
        ! truncates to '0010'x and resolves to Int 16
      end
