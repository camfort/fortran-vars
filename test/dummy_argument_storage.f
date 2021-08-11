      subroutine state(TEMP)
      REAL C,F,TEMP
      C(F) = 5.0*(F - 32.0)/9.0
      WRITE(*,*)'Enter the temperature in Fahrenheit'
      WRITE(*,*)'Fahrenheit = ',TEMP,' and Celsius = ',C(TEMP)

      stop
      end
