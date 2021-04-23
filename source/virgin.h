C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     VIRGIN COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
c-----2017/3/7 - INCREASED PAGE SIZE FROM 600,000 TO 3,000,000
c-----2018/1/22- DECREASED PAGE SIZE FROM 3,000,000 TO 1,500,000
c-----2023/3/7 - Deceeased page size from 1,500,000 to 120,000
c     PARAMETER (MAXPOINT = 1500000)
      PARAMETER (MAXPOINT =  120000)
c-----1,500,000 X 8 bytes X 8 arrays = 96,000,000
c-----  120,000 X 8 bytes X 8 arrays =  7,680,000
c-----2017/3/7 - Increased max. groups fom 6,000 to 30,000
      PARAMETER (MAXGROUP =   30000)
c-----30,000 X 8 bytes X 14 arrays = 3,360,000
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
c-----2019/1/3 - Added Fixed Dimention = 100 = PREPRO Standard
      COMMON/NBTINT/NBT(100),INT(100)
c-----Blank COMMON
      COMMON XTAB(MAXPOINT,3),YTAB(MAXPOINT,3),ETRAN(MAXPOINT),
     1 STRAN(MAXPOINT),FLUX(MAXGROUP),REACT(MAXGROUP),
     1 WTTP(MAXGROUP),WTTI(MAXGROUP),WTRP(MAXGROUP),WRTP(MAXGROUP),
     2 WRTI(MAXGROUP),WRRP(MAXGROUP),DENTAB(MAXGROUP),THICKI(MAXGROUP),
     3 FLUXINT(MAXGROUP),REACTINT(MAXGROUP),AVXCINT(MAXGROUP),
     4 EGROUP(MAXGROUP+1)
c-----Equivalenced arrays to handle 1 PAGE at a time
      DIMENSION XTAB1(MAXPOINT),XTAB2(MAXPOINT),XTAB3(MAXPOINT),
     1          YTAB1(MAXPOINT),YTAB2(MAXPOINT),YTAB3(MAXPOINT)
      EQUIVALENCE (XTAB(1,1),XTAB1(1)),(XTAB(1,2),XTAB2(1)),
     1            (XTAB(1,3),XTAB3(1)),(YTAB(1,1),YTAB1(1)),
     2            (YTAB(1,2),YTAB2(1)),(YTAB(1,3),YTAB3(1))
