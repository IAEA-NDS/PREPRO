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
