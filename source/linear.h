C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     LINEAR COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/3/7 - INCREASED PAGE SIZE to 3,000,000 from   600,000
C-----2023/2/2 - DECREASED PAGE SIZE to   120,000 from 3,000,000
c     PARAMETER (MAXPAGE =  3000000)
      PARAMETER (MAXPAGE =   120000)
c----- 3,000,000 X 8 bytes X 6 arrays = 144,000,000 bytes
c-----   120,000 X 8 bytes X 6 arrays =   5,760,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
c-----2019/1/5 - Added Standard Dimension 100
      COMMON/NBTINT/NBT(100),INT(100)
C-----BLANK COMMON - INPUT IS 1 PAGE, OUTPUT IS 2 PAGES
      COMMON XIN(MAXPAGE),YIN(MAXPAGE),XOUT(2*MAXPAGE),YOUT(2*MAXPAGE)
c-----Equivalence arrays - no additional memory allocation
      DIMENSION XPAGE1(MAXPAGE),YPAGE1(MAXPAGE),XPAGE2(MAXPAGE),
     1 YPAGE2(MAXPAGE)
      EQUIVALENCE (XOUT(1),XPAGE1(1)),(YOUT(1),YPAGE1(1)),
     1 (XOUT(MAXPAGE+1),XPAGE2(1)),(YOUT(MAXPAGE+1),YPAGE2(1))
