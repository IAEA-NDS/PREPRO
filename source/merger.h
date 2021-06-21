C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     MERGER COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----Maximum number of ENDF Formatted Data files
      PARAMETER (MAXTAPE = 1000)
c----- 1,000 X 8 bytes X 15 arrays = 96,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      CHARACTER*72 FILEOUT,FILEIN
      COMMON/UNITS/NTAPE(MAXTAPE)
      COMMON/WISHES/IGETHOW,MYWISH,IWISH(3,2,MAXTAPE),MATGOT(MAXTAPE),
     1 MTGOT(MAXTAPE),ICARDGOT(MAXTAPE),MATNOW(MAXTAPE),NOWISH(MAXTAPE)
      COMMON/FILELIST/FILEOUT,FILEIN(MAXTAPE)
      COMMON/ZACOM/ZATAB(MAXTAPE),IZATAB(MAXTAPE)
