C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


      SUBROUTINE TIMEIT(SECONDS)
C=======================================================================
C
C     IBM-PC - TIME SINCE START OF PROBLEM (BASE TIME) IN SECONDS
C     UNIX     This works on ALL of these.
C     LINUX
C     MAC
C
C     WARNING - ALL TIMES ARE IN SINGLE PRECISION -
C               DO NOT ADD DOUBLE PRECISION OR REAL*8 STATEMENTS.
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z) ! note REAL*4, not 8
      SAVE
      DIMENSION TARRAY(2)
      SECONDS=ETIME(TARRAY)
      SECONDS=(TARRAY(1)+TARRAY(2))
      RETURN
      END
