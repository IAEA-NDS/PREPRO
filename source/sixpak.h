C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     SIXPAK COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/3/7 - INCREASED TO 600,000 FROM 500,000
      PARAMETER (MAXPOINT = 600000)
C-----2020/2/19 - Added MT=5 Cross Section to create MT=3 Cross Sections
C-----            for Charged Particles.
      PARAMETER (MAXMT5   =  10000)
c-----2020/2/19 - Added standard interpolation law common for MF/MT=3/5
      COMMON/NBTINT/NBTMT5(100),INTMT5(100)
C-----600,000 X 8 bytes X 37 arrays = 177,600,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON SPECTA(MAXPOINT,21),X1(MAXPOINT),Y1(MAXPOINT),
     1 X3(MAXPOINT),Y3(MAXPOINT),X4(MAXPOINT),Y4(MAXPOINT),
     1 X5(MAXPOINT),Y5(MAXPOINT),YY3LST(MAXPOINT),YY3(MAXPOINT),
     2 XX5(MAXPOINT),YY5(MAXPOINT),Y3LAST(MAXPOINT),XLIST(3*MAXPOINT),
     3 EMT5(MAXMT5),CSMT5(MAXMT5),PARTMT5(MAXMT5)
