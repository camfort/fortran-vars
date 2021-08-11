      subroutine eq1()

      INTEGER*4 VAR1, VAR2, VAR3
      COMMON /COMMON1/ VAR1, VAR2, VAR3

      INTEGER*4 VAREQ(1)
      EQUIVALENCE(VAREQ,VAR1)

      VAR2 = 2
      VAR3 = 3
      end
      
      subroutine eq2()

      INTEGER*4 VAR1, VAR2, VAR3
      COMMON /COMMON2/ VAR1, VAR2, VAR3

      INTEGER*4 VAREQ(2)
      EQUIVALENCE(VAREQ,VAR1)

      VAR2 = 2
      VAR3 = 3
      end
      
      subroutine eq3()

      INTEGER*4 VAR1, VAR2, VAR3
      COMMON /COMMON3/ VAR1, VAR2, VAR3

      INTEGER*4 VAREQ(3)
      EQUIVALENCE(VAREQ,VAR1)

      VAR2 = 2
      VAR3 = 3
      end
      
      subroutine eq4()

      INTEGER*4 VAR1, VAR2, VAR3
      COMMON /COMMON4/ VAR1, VAR2, VAR3

      INTEGER*4 VAREQ(2)
      EQUIVALENCE(VAREQ,VAR2)

      VAR2 = 2
      VAR3 = 3
      end
      
      subroutine eq5()

      INTEGER*4 VAR1, VAR2, VAR3
      COMMON /COMMON5/ VAR1, VAR2, VAR3

      INTEGER*4 VAREQ(3)
      EQUIVALENCE(VAREQ,VAR2)

      VAR2 = 2
      VAR3 = 3
      end
      
