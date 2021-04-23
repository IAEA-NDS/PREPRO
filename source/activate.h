C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     ACTIVATE COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
      PARAMETER (MAXMT   =      999)   ! MT can only be 1 to 999
c-----2017/03/06 - increased from 3,000,000 to 6,000,000 (multiple of 3)
      PARAMETER (MAXPOINT = 6000000)   ! Data points in memory
c----- 6,000,000 X 8 bytes X 2 arrays = 96,000,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON/MTTABG/MTTAB3(MAXMT),MTTAB9(MAXMT),MTTAB10(MAXMT)
      COMMON X9(MAXPOINT),Y9(MAXPOINT),N9
