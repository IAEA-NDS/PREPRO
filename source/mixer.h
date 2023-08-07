C=======================================================================
C
C     MIXER COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/3/7 INCREASED PAGE SIZE TO 1,200,000 FROM 240,000
c-----2023/2/7 - Decreased page size from 1,2000,000 to 120,000
c     PARAMETER (MAXPOINT  = 1200000)
      PARAMETER (MAXPOINT  =  120000)
c----- 1,200,000 X 8 bytes X 22 arrays = 211,200,000
c-----   120,000 X 8 bytes X 22 arrays =  21,120,000
      PARAMETER (MAXPART   = 10)
      PARAMETER (MAXPARTP1 = 11)   ! MAXPART+1
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----PARAMETERS FOR EACH PART OF COMPOSITE
      COMMON/GETEM/AWRE(MAXPART),DENSE(MAXPART),TEMPK(MAXPART),
     1 QVALUE1(MAXPART),QVALUE2(MAXPART),ATOM10(MAXPART),
     2 FRACTA(MAXPART),FRACTG(MAXPART),IZAGET(MAXPART),MATGET(MAXPART),
     3 MTGET(MAXPART),IVERSE(MAXPART),NMAT
      COMMON/INPAGE/IXYLOW(MAXPARTP1),IXYHI(MAXPARTP1),ISCR(MAXPARTP1),
     1 NPAGE,NPAGEM
      COMMON/REALLY/NPTAB(MAXPARTP1),IPTAB(MAXPARTP1)
      COMMON/INDICE/IUSE(MAXPART),IUSEN
C-----NEXT POINTS FROM EACH TAPE
      COMMON/COMBOCOM/XA(MAXPART),YA(MAXPART),YB(MAXPART)
C-----INPUT INTERPOLATION LAW - Now fixed dimension = 100
      COMMON/NBTINT/NBT(100),INT(100)
C-----INPUT AND OUTPUT TABLE OF POINTS
      COMMON XTAB(MAXPOINT,MAXPARTP1),YTAB(MAXPOINT,MAXPARTP1)
