C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     SIGMA1 COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----STORAGE FOR SAVE ENERGY POINTS DURING ITERATION
      PARAMETER (MAXSAVE = 12000)
      COMMON/SAVECOM/ESAVE(MAXSAVE),XCSAVE(MAXSAVE)
C-----2017/3/7 - INCREASED PAGE SIZE TO 1,200,000 FROM 600,000
C-----           Actual allocation is by MAXPAGT3 = 3 PAGES
C
c     WARNING - if NAXPAGE Changes, change all of the folowing
C
      PARAMETER (MAXPAGE  = 1200000)  !             = Size of 1 page
      PARAMETER (MAXPAGT3 = 3600000)  ! 3*MAXPAGE   = Size of 3 pages
c----- 3,600,000 X 8 bytes X 6 arrays = 172,800,000 bytes
      PARAMETER (MAXPAGP1 = 1200001)  ! MAXPAGE+1   = Start second page
      PARAMETER (MAXPAGT2 = 2400001)  ! 2*MAXPAGE+1 = Start third page
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----2017/4/6 - Changed to fixed 100 = PREPRO Standard
      COMMON/NBTINT/NBT(100),INT(100)
C-----STORAGE FOR TABULATED COMPLEMENTARY ERROR FUNCTION
      COMMON/ERFCCOM/ERFCTAB(-1:10002),ERFCX(-1:10002),ERFC1(0:10000),
     1 ERFC2(0:10000),ERFC3(0:10000),ERFC4(0:10000),
     1 F0(0:10000),F1(0:10000),F2(0:10000),F3(0:10000)
C-----3 PAGES OF COLD
      COMMON ECOLD(MAXPAGT3),YCOLD(MAXPAGT3),EHOT (MAXPAGT3),
     1      XCCOLD(MAXPAGT3),DCOLD(MAXPAGT3),XCHOT(MAXPAGT3)
C-----1 PAGE OF HOT = equivalenced - no additional memory
      DIMENSION EHOT1(MAXPAGE),EHOT2 (MAXPAGE),EHOT3 (MAXPAGE),
     1         XCHOT1(MAXPAGE),XCHOT2(MAXPAGE),XCHOT3(MAXPAGE)
c-----2005/06/02 - corrected error in EHOT3 equivalence
c-----             it said (2*MAXPAGE+2) it should be (2*MAXPAGE+1)
C                                     X                          X
c-----Corrected below
      EQUIVALENCE (EHOT(1)        ,EHOT1(1)),  ! First page
     1            (EHOT(MAXPAGP1) ,EHOT2(1)),  ! Second page
     1            (EHOT(MAXPAGT2) ,EHOT3(1)),  ! third page
     1            (XCHOT(1)       ,XCHOT1(1)), !    "
     2            (XCHOT(MAXPAGP1),XCHOT2(1)), !    "
     3            (XCHOT(MAXPAGT2),XCHOT3(1))  !    "
