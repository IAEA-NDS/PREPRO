C=======================================================================
C
C     DICTIN COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
c-----2017/3/7 - Increased fom 500,000 to 1,000,000
c-----2023/2/5 - Decreased from 1,000,000 to 120,000
c     PARAMETER (MAXTAB = 1000000)
      PARAMETER (MAXTAB =  120000)
c----- 1,000,000 X 8 bytes X 4 arrays = 32,000,000 bytes
c-----   120,000 X 8 bytes X 4 arrays =  3,840,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON MATTAB(MAXTAB),MFTAB(MAXTAB),MTTAB(MAXTAB),NCARD(MAXTAB)
