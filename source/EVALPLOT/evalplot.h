C=======================================================================
C
C     EVALPLOT COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
      PARAMETER (MAXMAT   =     1000) ! MAT/MF/MT maximum
      PARAMETER (MAXGET   =      100) ! Requests
C-----2017/02/17 - INCREASED PAGE SIZE TO 7,500,000, to handle new
C-----             R-M (LRF=7) detailed angular distributions.
c----- 7,500,000 X 8 bytes X 2 arrays = 120,000,000 bytes
c-----   120,000 X 8 bytes X 2 arrays =   1,920,000 bytes
C-----                                          7,500,000/100=75,000
c-----MAXLEGE is equivalenced = no additional memory.
c     note:                7,500,000 = 75,000 X 100
      PARAMETER (MAXPOINT =  7500000) ! data point in memory
      PARAMETER (MAXLEGE  =    75000) ! MUST be MAXPOINT /MAXLEGC
      PARAMETER (MAXLEGC  =      100) ! max. order coefficent = 100
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----Requests
      COMMON/FILTAB/QEDTAB(MAXMAT),XYEVAL(2,2,MAXMAT),YLOTAB(MAXMAT),
     1 MTDEX1(MAXMAT),MTDEX2(MAXMAT),MTTAB(MAXMAT),MTSIZE
      COMMON/LINEUP/IMNEXT(MAXMAT),IMSAVE(MAXMAT),NEXT(MAXMAT),
     1 LPLOT,LPLOT1,LPLOT2
      COMMON/MATZA/EMAX(MAXGET),EMIN(MAXGET),MATMIN(MAXGET),
     1 MFMIN(MAXGET),MTMIN(MAXGET),MATMAX(MAXGET),MFMAX(MAXGET),
     1 MTMAX(MAXGET),NTYPE(MAXGET),MYZOOM(MAXGET),MODGET,NMATZA,IMATZA
      COMMON/ZOOMIES/EMINNOW,EMAXNOW
      CHARACTER*1 MTBOX
      COMMON/SIDESC/MTBOX(40,MAXMAT)
      COMMON/SIDESI/IMTBOX(MAXMAT)
      COMMON/TERPCOM/NBT(100),INT(100)
C-----TABULATED DATA
      COMMON XTAB(MAXPOINT),YTAB(MAXPOINT)
c-----Equivalence arrays = no additional memory
      DIMENSION XYTAB(2*MAXPOINT)
      EQUIVALENCE (XTAB(1),XYTAB(1))
C-----MF=4 LEGENDRE COEFFICINTS
c     PARAMETER (MAXPOINT =  7500000) ! data point in memory
c     PARAMETER (MAXLEGE  =    75000) ! MUST be MAXPOINT /MAXLEGC
c     PARAMETER (MAXLEGC  =      100) ! max. order coefficent = 100
      DIMENSION XTAB4(MAXLEGE,MAXLEGC),YTAB4(MAXLEGE,MAXLEGC)
      EQUIVALENCE (XTAB(1),XTAB4(1,1)),(YTAB(1),YTAB4(1,1))
