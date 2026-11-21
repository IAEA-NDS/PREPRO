C=======================================================================
C
C     SPECTRA COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/3/7 - INCREASED PAGE SIZE 3,000,000 FROM 600,000
c-----2023/3/7 - Decreased page size from 3,000,000 to 120,000
c     PARAMETER (MAXPAGE =  3000000)
      PARAMETER (MAXPAGE =   120000)
C----- 3,000,000 X 8 bytes X 6 arrays = 144,000,000
C-----   120,000 X 8 bytes X 6 arrays =   5,760,000
C-----2019/6/27 - points to be added
      PARAMETER (MAXADD = 10000)
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----2019/6/27 - points to be added
      COMMON/COMADDED/EADD(MAXADD),YADD(MAXADD)
C-----INPUT IS 1 PAGE, OUTPUT IS 2 PAGES
      COMMON XIN(MAXPAGE),YIN(MAXPAGE),XOUT(2*MAXPAGE),YOUT(2*MAXPAGE)
C-----Equivalenced arrays = no added memory allocation
      DIMENSION XPAGE1(MAXPAGE),YPAGE1(MAXPAGE),XPAGE2(MAXPAGE),
     1 YPAGE2(MAXPAGE)
      EQUIVALENCE (XOUT(1),XPAGE1(1)),(YOUT(1),YPAGE1(1)),
     1 (XOUT(MAXPAGE+1),XPAGE2(1)),(YOUT(MAXPAGE+1),YPAGE2(1))
