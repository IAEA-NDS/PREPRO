C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     FIXUP COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
c-----2017/03/07 - INCREASED TO 3,000,000 FROM 600,000
c-----2017/05/25 - DECREASED TO 2,700,000: 3,000,000 too big for LINUX
c-----2018/01/20 - DECREASED TO 1,800,000: Better for many computers
c-----2023/01/30 - DECREASED TO   120,000: bigger arrays = more I/O time
      PARAMETER (MAXPOINT =  120000)
c-----   120,000 X 8 bytes X 8 arrays =   7,680,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
c-----2019/1/5 - Added NBT and INT dimension 100 = PREPRO Standard
      COMMON/NBTINT/NBT(100),INT(100)
c-----Blank COMMON
      COMMON XA(MAXPOINT),XB(MAXPOINT),XC(MAXPOINT),XCORE(MAXPOINT),
     1       YA(MAXPOINT),YB(MAXPOINT),YC(MAXPOINT),YCORE(MAXPOINT)
