        logical function f1()
        f1 = .TRUE.
        end

        logical*2 function f2()
        f2 = .TRUE.
        end

        character*5 function f3()
        f3 = 'abcde'
        end

        function f4()
        logical f4
        f4 = .TRUE.
        end

        integer*4 function f5()
        integer*4 f5
        f5 = 1234
        end

        character*6 function f6()
        character*6 f6
        f6 = 'abcdef'
        end

        character*(*) function f7()
        f7 = 'AB'
        end
