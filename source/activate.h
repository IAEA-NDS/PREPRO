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
c-----2023/2/5 - decreased from 6,000,000 to 120,000
c     PARAMETER (MAXPOINT = 6000000)   ! Data points in memory
      PARAMETER (MAXPOINT =  120000)   ! Data points in memory
c----- 6,000,000 X 8 bytes X 2 arrays = 96,000,000 bytes
c-----   120,000 X 8 bytes X 2 arrays =  1,920,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON/MTTABG/MTTAB3(MAXMT),MTTAB9(MAXMT),MTTAB10(MAXMT)
      COMMON X9(MAXPOINT),Y9(MAXPOINT),N9
