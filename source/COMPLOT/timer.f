C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


      SUBROUTINE ENDIT
C=======================================================================
C
C     Version 2019-1 (June 2019)
C
C     PRINT EXECUTION TIME AND TERMINATE = NORMAL FINISH
C
C=======================================================================
      CALL TIMER
      STOP
      ENTRY ENDERROR
C=======================================================================
C
C     ENTRY POINT TO STOP ON ERROR.
C
C=======================================================================
      CALL TIMEERR
      STOP
      END
      SUBROUTINE TIMER
C=======================================================================
C
C     TOTAL EXECUTION TIME
C
C     WARNING - ALL TIMES ARE IN SINGLE PRECISION.
C               DO NOT ADD ANY DOUBLE OR REAL*8 STATEMENTS.
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)  ! note - REAL*4 not 8
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*8 CODENAME
      COMMON/NAMECODE/CODENAME
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DATA TSTART/0.0/
      DATA IPASS/0/
C-----DEFINE CURRENT TIME
      CALL TIMEIT(TNOW)
C-----ON FIRST PASS DEFINE STARTING TIME
      IF(IPASS.EQ.0) TSTART=TNOW
      IPASS=IPASS+1
C-----PRINT EVERY PASS EXCEPT FIRST ONE
      IF(IPASS.LE.1) RETURN
      WRITE(OUTP,10) CODENAME,TNOW-TSTART
      WRITE(OUTP,20)
C-----OUTPUT TO SCREEN ONLY IF ENDF/B OUTPUT IS PRODUCED
      IF(OTAPE.GT.0) THEN
      WRITE(*   ,10) CODENAME,TNOW-TSTART
      WRITE(*   ,20)
      ENDIF
   10 FORMAT(1X,79('=')/1X,A8,' Total Execution Time',F20.2,' Seconds')
   20 FORMAT(1X,79('='))
      RETURN
C=======================================================================
C
C     ENTRY TIME TO RETURN ELAPSED TIME
C
C     WARNING - TIMER MUST BE CALLED FIRST TO DEFINE TSTART
C
C=======================================================================
      ENTRY TIMER1(SECONDS)
      CALL TIMEIT(TNOW)
      SECONDS = TNOW - TSTART
      RETURN
C=======================================================================
C
C     ENTRY TIME TO RETURN TIME FOR EACH MAT.
C     IDENTICAL TO TIMER, BUT MAT, NOT CODENAME.
C
C=======================================================================
      ENTRY TIMEMAT
      CALL TIMEIT(TNOW)
      WRITE(OUTP,30) TNOW-TSTART
      WRITE(OUTP,20)
      IF(OTAPE.GT.0) THEN
      WRITE(*   ,30) TNOW-TSTART
      WRITE(*   ,20)
      ENDIF
   30 FORMAT(1X,79('=')/1X,'     MAT',' Total Execution Time',F20.2,
     1 ' Seconds')
      RETURN
C=======================================================================
C
C     ENTRY TIME TO RETURN TIME FOR ERROR STOP.
C     IDENTICAL TO TIMER, BUT ERROR, NOT CODENAME.
C
C=======================================================================
      ENTRY TIMEERR
      CALL TIMEIT(TNOW)
      WRITE(OUTP,40) TNOW-TSTART
      WRITE(OUTP,20)
      IF(OTAPE.GT.0) THEN
      WRITE(*   ,40) TNOW-TSTART
      WRITE(*   ,20)
      ENDIF
   40 FORMAT(1X,79('=')/1X,'   ERROR',' Total Execution Time',F20.2,
     1 ' Seconds')
      RETURN
      END
