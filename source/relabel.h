C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     RELABEL COMMON
C
C=======================================================================
C
C     PARAMERTERS
C
C-----------------------------------------------------------------------
C-----Maximum Label numbers in any routine
      PARAMETER (MAXLABEL = 100000)
C----- 100,000 X 8 bytes X 11 arrays = 8,800,000 bytes
C
C     STORAGE
C
C-----------------------------------------------------------------------
      CHARACTER*1 OLDTAB,NEWTAB
      COMMON OLDTAB(5,MAXLABEL),NEWTAB(5,MAXLABEL),IMUSED(MAXLABEL)
