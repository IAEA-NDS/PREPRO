C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     ENDFIO.F includes routines to read and write ALL of the types
C     of ENDF records, e.g., CONT, TAB1, TAB2, LIST, etc.
C
C     The objective is to remove ALL direct reads and writes from
C     the ENDF Pre-Processing Codes (PREPRO) codes.
C
C     Note, that I (Red Cullen) have used these routines for so long
C     that some of the comments and variables still refer to CARDS -
C     as in way back when everything was based on 80 column computer
C     CARDS. I have purposely left this terminology in place, as a
C     reminder of how far we have come.
C
C     Version 2017-1 (June 2017)
C     ==========================
C     *Treat End-of-File as TEND file during CONTI read.
C     *Added INNEXT to maintain INTEGER*8 logic for 9 to 10 digit outout
C     *Added POINTIV and LISTIV - added V = Variable input unit - to
C      read tables of PREPRO Input Parameters (rather than ENDF data).
C
C     Version 2018-1 (Jan. 2018)
C     ==========================
C     *Replaced INCLUDE
C     *Added INT11 = integer to 11A1 characters
C     *10 digit output only for,
C        1) MF=2 resonance parameter energy
C        2) MF=3 neutron cross aection energy
C     *ZAHOL added Z = 111 to 118.
C
C     Version 2019-1 (June 2019)
C     ==========================
C     *Additional Interpolation Law Tests
C     *Standardized ALL PREPRO codes to use up to 100 Interpolation,
C      i.e., NBT(100),INT(100)
C     *Added MAXIE to check consistency of Maximum Tabulated Energy
C      fot ALL data in EACH evaluation.
C     *Added ZANAME to include ZAzzzaaa in file names.
C
C=======================================================================
C
C     OWNED, MAINTAINED AND DISTRIBUTED BY
C     ------------------------------------
C     THE NUCLEAR DATA SECTION
C     INTERNATIONAL ATOMIC ENERGY AGENCY
C     P.O. BOX 100
C     A-1400, VIENNA, AUSTRIA
C     EUROPE
C
C     ORIGINALLY WRITTEN BY
C     ------------------------------------
C     Dermott E. Cullen
C
C     PRESENT CONTACT INFORMATION
C     ---------------------------
C     Dermott E. Cullen
C     1466 Hudson Way
C     Livermore, CA 94550
C     U.S.A.
C     Telephone  925-443-1911
C     E. Mail    RedCullen1@Comcast.net
C     Website    RedCullen1.net/HOMEPAGE.NEW
C
C=======================================================================
      SUBROUTINE CARDIO(C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     READ AND WRITE ENDF/B CARD.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      CALL CARDO(C1,C2,L1,L2,N1,N2)
      RETURN
      END
      SUBROUTINE CONTI
C=======================================================================
C
C     READ ONE ENDF/B CONTROL LINE.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 FIELD2
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION FIELD2(11,2)
C-----READ FLOATING POINT FIELDS AS CHARACTERS
      READ(ITAPE,20,END=10) FIELD2,L1H,L2H,N1H,N2H,MATH,MFH,MTH
C-----TRANSLATE FROM CHARACTERS TO FLOATING POINT.
      CALL IN9(C1H,FIELD2(1,1))
      CALL IN9(C2H,FIELD2(1,2))
C-----ELIMINATE -0
      IF(IABS(L1H).LE.0) L1H=0
      IF(IABS(L2H).LE.0) L2H=0
      IF(IABS(N1H).LE.0) N1H=0
      IF(IABS(N2H).LE.0) N2H=0
      RETURN
c-----------------------------------------------------------------------
c
c     ERROR - treat EOF as TEND line
c
c-----------------------------------------------------------------------
   10 MATH = -1
      MFH  =  0
      MTH  =  0
      RETURN
   20 FORMAT(22A1,4I11,I4,I2,I3)
      END
      SUBROUTINE CONTO
C=======================================================================
C
C     WRITE ONE ENDF/B CONTROL RECORD.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD2
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION FIELD2(11,2)
C-----LAST MAT NUMBER - USED TO RESET SEQUENCE NUMBER
      DATA LASTMAT/-100000/
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----IF SEND, FEND OR MEND OUTPUT IN STANDARD FORM.
      IF(MTH.GT.0) GO TO 10
      CALL OUTS(MATH,MFH)
      RETURN
C-----CONVERT FLOATING POINT NUMBERS TO STANDARD OUTPUT FORM.
   10 CALL OUT9(C1H,FIELD2(1,1))
      CALL OUT9(C2H,FIELD2(1,2))
C-----ELIMINATE -0
      IF(IABS(L1H).LE.0) L1H=0
      IF(IABS(L2H).LE.0) L2H=0
      IF(IABS(N1H).LE.0) N1H=0
      IF(IABS(N2H).LE.0) N2H=0
C-----IF NEW MAT RESET SEQUENCE NUMBER
      IF(MATH.eq.LASTMAT) go to 20
      LASTMAT=MATH
      NOSEQ=1
C-----OUTPUT LINE IMAGE.
   20 IF(NOSEQ.LE.0) NOSEQ=1
      WRITE(OTAPE,30) FIELD2,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   30 FORMAT(22A1,4I11,I4,I2,I3,I5)
      END
      SUBROUTINE CARDI(C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     READ ENDF/B LINE.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 FIELD2
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/LEADER/C1X,C2X,L1X,L2X,N1X,N2X,MAT,MF,MT
      DIMENSION FIELD2(11,2)
      READ(ITAPE,10) FIELD2,L1,L2,N1,N2,MAT,MF,MT
      CALL IN9(C1,FIELD2(1,1))
      CALL IN9(C2,FIELD2(1,2))
C-----ELIMINATE -0
      IF(IABS(L1).LE.0) L1=0
      IF(IABS(L2).LE.0) L2=0
      IF(IABS(N1).LE.0) N1=0
      IF(IABS(N2).LE.0) N2=0
      RETURN
   10 FORMAT(22A1,4I11,I4,I2,I3)
      END
      SUBROUTINE CARDO(C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     WRITE ENDF/B LINE.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD2
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION FIELD2(11,2)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      IF(MTH.GT.0) GO TO 10
      CALL OUTS(MATH,MFH)
      RETURN
C-----CONVERT FLOATING POINT NUMBERS TO STANDARD OUTPUT FORM.
   10 CALL OUT9(C1,FIELD2(1,1))
      CALL OUT9(C2,FIELD2(1,2))
C-----ELIMINATE -0
      IF(IABS(L1).LE.0) L1=0
      IF(IABS(L2).LE.0) L2=0
      IF(IABS(N1).LE.0) N1=0
      IF(IABS(N2).LE.0) N2=0
C-----OUTPUT LINE.
      IF(NOSEQ.LE.0) NOSEQ=1
      WRITE(OTAPE,20) FIELD2,L1,L2,N1,N2,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   20 FORMAT(22A1,4I11,I4,I2,I3,I5)
      END
      SUBROUTINE TERPTEST(NBT,INT,N1,N2,MYPATH)
C=======================================================================
C
C     TEST TAB1 INTERPOLATION LAW.
C
c     Insure Laws (INT) are based on MYPATH,
c     0) 1 to 6, 11 to 15, 21 to 25 = Corresponding Points
c     1) 1 to 6 = General
c     2) 1 or 2 = Restricted
c     3) 2      = Only Linear
c
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1X,C2X,L1X,L2X,N1X,N2X,MAT,MF,MT
      DIMENSION NBT(N1),INT(N1)
c-----2019/1/5 - MAXTERP = Interpolation Law Dimension for ALL PREPRO
      data MAXTERP/100/
c-----------------------------------------------------------------------
c
c     Insure Interpolation Law is defined (N1 > 0) and (N1 <= MAXTERP)
c
c-----------------------------------------------------------------------
      if(N1.le.      0) go to 20
      if(N1.gt.MAXTERP) go to 40
c-----------------------------------------------------------------------
c
c     Insure 2 or More Tabulated Points
c
c-----------------------------------------------------------------------
      if(N2.lt.2) go to 60
c-----------------------------------------------------------------------
c
c     Insure point count (N2) and Interpolation Law agree.
c
c-----------------------------------------------------------------------
      if(N2.ne.NBT(N1)) go to 80
c-----------------------------------------------------------------------
c
c     Insure Laws (INT) are,
c     0) 1 to 6, 11 to 15, 21 to 25 = Corresponding Points
c     1) 1 to 6 = General
c     2) 1 or 2 = Restricted
c     3) 2      = Only Linear
c
c-----------------------------------------------------------------------
      NBTLAST = 0
      do 10 k=1,N1
      if(MYPATH.eq.0) then
      if(INT(k).ge. 1.and.INT(k).le. 6) go to 10
      if(INT(k).ge.11.and.INT(k).le.15) go to 10
      if(INT(k).ge.21.and.INT(k).le.25) go to 10
      go to 100
      endif
      if(MYPATH.eq.1) then
      if(INT(k).lt.1.or.INT(k).gt.6) go to 120  ! 1 to 6 = only WARNING
      endif
      if(MYPATH.eq.2) then
      if(INT(k).lt.1.or.INT(k).gt.2) go to 140  ! 1 or 2
      endif
      if(MYPATH.eq.3) then
      if(INT(k).ne.2               ) go to 160  ! 1
      endif
      if(NBT(k).le.NBTLAST) go to 190
      NBTLAST = NBT(k)
   10 continue
c-----------------------------------------------------------------------
c
c     Interpolation Law passes ALL tests.
c
c-----------------------------------------------------------------------
      return
c-----------------------------------------------------------------------
c
c     Interpolation Law (INT) Failed test.
c
c-----------------------------------------------------------------------
c-----N1 <=0 = No Interpolation Law Defined
   20 write(OUTP,30) MAT,MF,MT,N1,N2
      write(*   ,30) MAT,MF,MT,N1,N2
   30 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N1/N2=',I5,I3,I4,2I10/
     2 ' No Interpolation Law - N1 MUST be Positive')
      go to 210
c-----N1 > MAXTERP = Interpolation Law Memory Exceeded
   40 write(OUTP,50) MAT,MF,MT,N1,N2,MAXTERP
      write(*   ,50) MAT,MF,MT,N1,N2,MAXTERP
   50 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N1/N2=',I5,I3,I4,2I10/
     2 ' Interpolation Law N1 Exceeds Dimension',I5)
      go to 210
c-----N2 MUST be 2 or MORE
   60 write(OUTP,70) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,70) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
   70 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Point Count (N2) MUST be 2 or More'/
     3 '      NBT  INT'/(I9,I5))
      go to 210
c-----N2 does not agree with Interpolation Law
   80 write(OUTP,90) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,90) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
   90 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Point Count (N2) and Interpolation Law do not Agree'/
     3 '      NBT  INT'/(I9,I5))
      go to 210
c-----0) 1 to 6, 11 to 15, 21 to 25 = Corresponding Points
  100 write(OUTP,110) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,110) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
  110 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Interpolation Law (INT) MUST be 1 to 6, 11 to 15, or 21 to 25'/
     3 '      NBT  INT'/(I9,I5))
      go to 210
c-----------------------------------------------------------------------
c
c     1) 1 to 6 = General = only WARNING
c
c-----------------------------------------------------------------------
  120 write(OUTP,130) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,130) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
  130 FORMAT(1x,9('WARNING='),'WARNING'/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Interpolation Law (INT) Expected 1 to 6'/
     3 '      NBT  INT'/(I9,I5))
      RETURN
c-----------------------------------------------------------------------
c
c     2) 1 or 2 = Restricted
c
c-----------------------------------------------------------------------
  140 write(OUTP,150) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,150) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
  150 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Interpolation Law (INT) MUST be 1 or 2'/
     3 '      NBT  INT'/(I9,I5))
      write(OUTP,180)
      write(*   ,180)
      go to 210
c-----3) 2      = Only Linear
  160 write(OUTP,170) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,170) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
  170 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Interpolation Law (INT) MUST be 2'/
     3 '      NBT  INT'/(I9,I5))
      write(OUTP,180)
      write(*   ,180)
  180 FORMAT(' You MUST Linearize this data before using this code.'/
     1       ' We recommend you use PREPRO/LINEAR to Linearize'/
     2       ' Cross Sections.')
      go to 210
c-----NBT not in Ascending order
  190 write(OUTP,200) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
      write(*   ,200) MAT,MF,MT,N2,(NBT(k),INT(k),k=1,N1)
  200 FORMAT(1x,9('ERROR==='),'ERROR=='/
     1 ' MAT/MF/MT/N2=',I5,I3,I4,I10/
     2 ' Interpolation Boundary (NBT) MUST be in Ascending Order'/
     3 '      NBT  INT'/(I9,I5))
      go to 210
c-----------------------------------------------------------------------
c
c     Terminate Execution
c
c-----------------------------------------------------------------------
  210 write(OUTP,220)
      write(*   ,220)
  220 FORMAT('  Execution Terminated')
      CALL ENDERROR
      return
      end
      SUBROUTINE TERPI(NBT,INT,N1)
C=======================================================================
C
C     READ TAB1 INTERPOLATION LAW.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION NBT(N1),INT(N1)
      READ(ITAPE,10) (NBT(I),INT(I),I=1,N1)
      RETURN
   10 FORMAT(6I11)
      END
      SUBROUTINE TERPO(NBT,INT,N1)
C=======================================================================
C
C     WRITE TAB1 INTERPOLATION LAW.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION NBT(N1),INT(N1)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----LOOP OVER RANGES - UP TO 3 PER LINE
      DO 40 I1=1,N1,3
      I2=I1+2
      IF(I2.GT.N1) I2=N1
C-----OUTPUT LINE
      IOUT=(I2-I1)+1
      GO TO (10,20,30),IOUT
   10 WRITE(OTAPE,50) NBT(I1),INT(I1),MATH,MFH,MTH,NOSEQ
      GO TO 40
   20 WRITE(OTAPE,60) (NBT(II),INT(II),II=I1,I2),MATH,MFH,MTH,NOSEQ
      GO TO 40
   30 WRITE(OTAPE,70) (NBT(II),INT(II),II=I1,I2),MATH,MFH,MTH,NOSEQ
   40 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   50 FORMAT(2I11,44X,I4,I2,I3,I5)
   60 FORMAT(4I11,22X,I4,I2,I3,I5)
   70 FORMAT(6I11    ,I4,I2,I3,I5)
      END
      SUBROUTINE POINTI(X,Y,IXY)
C=======================================================================
C
C     READ A PAGE OF DATA POINTS AND INSURE THAT THE ENERGIES ARE IN
C     ASCENDING ORDER. IF ENERGIES ARE NOT, TERMINATE EXECUTION.
C
C     WARNING - BEFORE STARTING TO READ EACH TABLE OF POINTS,
C               ELAST MUST BE INITIALIZED = 0, TO ALLOW ENERGY
C               ORDER TEST.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LASTE/ELAST
      DIMENSION X(IXY),Y(IXY),FIELD6(11,6)
      DATA OKDIFF/1.0D-09/
C-----SET UP LOOP OVER LINES.
      DO 20 I=1,IXY,3
      II=I+2
      IF(II.GT.IXY) II=IXY
      IN=2*(II-I)+2
C-----READ ENERGY AS HOLLERITH AND CROSS SECTION AS FLOATING POINT.
      READ(ITAPE,50) FIELD6
      J=I
C-----CONVERT ENERGY TO FLOATING POINT.
      DO 10 K=1,IN,2
      CALL IN9(X(J),FIELD6(1,K))
      CALL IN9(Y(J),FIELD6(1,K+1))
   10 J=J+1
   20 CONTINUE
C-----CHECK ENERGY ORDER.
      DO 40 I=1,IXY
      IF(X(I).GE.ELAST) GO TO 40
C-----ALLOW FOR SMALL DIFFERENCES (ABOUT THE SAME TO 9 DIGITS).
      IF(DABS(ELAST-X(I)).LE.OKDIFF*ELAST) GO TO 30
      CALL OUT9(ELAST,FIELD6(1,1))
      CALL OUT9(X(I) ,FIELD6(1,2))
      WRITE(OUTP,60) MATH,MFH,MTH,
     1 I-1,(FIELD6(M,1),M=1,11),
     2 I  ,(FIELD6(M,2),M=1,11)
C-----WHEN SMALL DIFFERENCES OCCUR INSURE THAT ENERGIES ARE NOT IN
C-----DESCENDING ORDER.
   30 X(I)=ELAST
   40 ELAST=X(I)
      RETURN
   50 FORMAT(66A1)
   60 FORMAT(2X,78('-')/I5,I3,I4/
     1 ' Energies Not in Ascending Energy Order'/
     2 '  Index      Energy'/
     3 I7,1X,11A1      /I7,1X,11A1      /
     4 19X,' Execution Terminated.'/2X,78('-'))
      END
      SUBROUTINE POINTIV(IUNIT,X,Y,IXY)
C=======================================================================
C
C     POINTI + V = VARIABLE INPUT UNIT (IUNIT).
C
C     WARNING - NO X Order Test (as in POINTI).
C
C     Used to READ PREPRO Input Parameters.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 FIELD6
      DIMENSION X(IXY),Y(IXY),FIELD6(11,6)
C-----SET UP LOOP OVER LINES.
      DO 20 I=1,IXY,3
      II=I+2
      IF(II.GT.IXY) II=IXY
      IN=2*(II-I)+2
C-----READ ENERGY AS HOLLERITH AND CROSS SECTION AS FLOATING POINT.
      READ(IUNIT,30) FIELD6
      J=I
C-----CONVERT ENERGY TO FLOATING POINT.
      DO 10 K=1,IN,2
      CALL IN9(X(J),FIELD6(1,K))
      CALL IN9(Y(J),FIELD6(1,K+1))
   10 J=J+1
   20 CONTINUE
      RETURN
   30 FORMAT(66A1)
      END
      SUBROUTINE POINTO(X,Y,IXY)
C=======================================================================
C
C     WRITE IXY DATA POINTS. FORMAT OF ENERGIES WILL VARY TO ALLOW
C     MAXIMUM PRECISION OF BETWEEN 6 AND 9 DIGITS ACCURACY.
C
C     CROSS SECTIONS WILL ALWAYS BE OUTPUT E11.4 FORMAT.
C
C     PHOTON DATA WILL ALWAYS BE IN E11.4 FORMAT.
C
C     ENERGIES WILL BE OUTPUT IN EITHER STANDARD E11.4 FORMAT OR A
C     VARIABLE F FORMAT (VARIABLE FROM F11.8 TO F11.0) TO GIVE THE
C     MAXIMUM NUMBER OF DIGITS OF ACCURACY. AS OUTPUT BY THIS ROUTINE
C     STANDARD FORM E11.4 FORMAT GIVES 6 DIGITS OF ACCURACY. THE
C     VARIABLE FORM F FORMAT WILL GIVE 6 TO 9 DIGITS ACCURACY. AS
C     LONG AS THE EXPONENT OF AN ENERGY IN E11.4 FORMAT IS BETWEEN -3
C     AND +8, MORE DIGITS WILL BE INCLUDED IF THE NUMBER IS OUTPUT IN
C     VARIABLE F FORMAT. IN PARTICULAR A FULL 9 DIGITS WILL BE OUTPUT
C     FOR ALL ENERGIES BETWEEN 1 EV AND 100 MEV. BETWEEN 1 MILLI-EV
C     AND 1 EV THE NUMBER OF DIGITS WILL VARY FROM 6 TO 8.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/FLAGS/MINUS3,IMPLUS
      DIMENSION X(IXY),Y(IXY),FIELD6(11,6)
      DATA ZEROD/0.0D+00/
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS
      IF(IXY.LE.0) RETURN
C-----SET UP LOOP OVER LINES (UP TO 3 POINTS PER LINE).
      DO 50 I1=1,IXY,3
      I2=I1+2
      IF(I2.GT.IXY) I2=IXY
c-----------------------------------------------------------------------
C
C     OUTPUT ONE LINE WITH ENERGY IN F OR E FORMAT AND CROSS SECTION
C     IN E FORMAT.
C
c-----------------------------------------------------------------------
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 II=I1,I2
C-----Negative Imaginary Anomalous Scattering is O.K.
      IF(MFH.ne.27.or.MTH.ne.506) then
C-----COUNT IF CROSS SECTION IS NEGATIVE.
      IF(Y(II).LT.ZEROD) MINUS3=MINUS3+1
      ENDIF
C-----SET FLAG IF POSITIVE.
      IF(Y(II).GT.ZEROD) IMPLUS=1
      K=K+1
c-----------------------------------------------------------------------
c
c     2017/10/30 - OUT10 only for MF=3 neutron cross section energy
c
c-----------------------------------------------------------------------
      if(MFH.eq.3) then
c-----2013/1/12 - changed ENERGY to OUT10 from OUT9.
      CALL OUT10(X(II),FIELD6(1,K))    ! MF=3
      else
      CALL OUT9G(X(II),FIELD6(1,K))    ! All others
      endif
      K=K+1
C-----CHANGED CROSS SECTION TO 9 DIGIT OUTPUT
   10 CALL OUT9G(Y(II),FIELD6(1,K))
C-----OUTPUT ONE LINE.
      IOUT=(I2-I1)+1
      GO TO (20,30,40),IOUT
   20 WRITE(OTAPE,60) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 50
   30 WRITE(OTAPE,70) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 50
   40 WRITE(OTAPE,80) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   50 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   60 FORMAT(22A1,44X,I4,I2,I3,I5)
   70 FORMAT(44A1,22X,I4,I2,I3,I5)
   80 FORMAT(66A1    ,I4,I2,I3,I5)
      END
      SUBROUTINE POINTO9(X,Y,IXY)
C=======================================================================
C
C     Identical to POINTO, but uses OUT9G rather than OUT10 =
C     Used by GROUPIE - 10 digits never needed for  multi-group.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/FLAGS/MINUS3,IMPLUS
      DIMENSION X(IXY),Y(IXY),FIELD6(11,6)
      DATA ZEROD/0.0D+00/
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS
      IF(IXY.LE.0) RETURN
C-----SET UP LOOP OVER LINES (UP TO 3 POINTS PER LINE).
      DO 50 I1=1,IXY,3
      I2=I1+2
      IF(I2.GT.IXY) I2=IXY
c-----------------------------------------------------------------------
C
C     OUTPUT ONE LINE WITH ENERGY IN F OR E FORMAT AND CROSS SECTION
C     IN E FORMAT.
C
c-----------------------------------------------------------------------
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 II=I1,I2
C-----Negative Imaginary Anomalous Scattering is O.K.
      IF(MFH.ne.27.or.MTH.ne.506) then
C-----COUNT IF CROSS SECTION IS NEGATIVE.
      IF(Y(II).LT.ZEROD) MINUS3=MINUS3+1
      ENDIF
C-----SET FLAG IF POSITIVE.
      IF(Y(II).GT.ZEROD) IMPLUS=1
      K=K+1
c-----2013/1/12 - changed ENERGY to OUT10 from OUT9.
c-----2014/3/27 - added POINTO9 using OUT9 - this is the only
c-----            difference between POINTO and POINTO9.
c-----2015/7/30 - Changed OUT9 to OIUT9G.
      CALL OUT9G(X(II),FIELD6(1,K))
      K=K+1
C-----CHANGED CROSS SECTION TO 9 DIGIT OUTPUT
   10 CALL OUT9G(Y(II),FIELD6(1,K))
C-----OUTPUT ONE LINE.
      IOUT=(I2-I1)+1
      GO TO (20,30,40),IOUT
   20 WRITE(OTAPE,60) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 50
   30 WRITE(OTAPE,70) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 50
   40 WRITE(OTAPE,80) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   50 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   60 FORMAT(22A1,44X,I4,I2,I3,I5)
   70 FORMAT(44A1,22X,I4,I2,I3,I5)
   80 FORMAT(66A1    ,I4,I2,I3,I5)
      END
      SUBROUTINE LISTIO(X,IX)
C=======================================================================
C
C     READ AND WRITE LIST DATA.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      DIMENSION X(IX)
      CALL LISTI(X,IX)
      CALL LISTO(X,IX)
      RETURN
      END
      SUBROUTINE LISTIO9(X,IX)
C=======================================================================
C
C     READ AND WRITE LIST DATA.
C
C     SAME AS LISTIO, WITHOUT OUT10
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      DIMENSION X(IX)
      CALL LISTI(X,IX)
      CALL LISTO9(X,IX)
      RETURN
      END
      SUBROUTINE LISTSKIP(IX)
C=======================================================================
C
C     SKIP LIST DATA.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 DUMMY
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DATA DUMMY/' '/
      DO 10 I=1,IX,6
   10 READ(ITAPE,20) DUMMY
C-----USE DUMMY TO PREVENT COMPILER WARNING
      IF(DUMMY.NE.' ') I=1
      RETURN
   20 FORMAT(A1)
      END
      SUBROUTINE LISTI(X,IX)
C=======================================================================
C
C     READ LIST DATA.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION X(IX),FIELD6(11,6)
c-----------------------------------------------------------------------
C
C     READ LIST RECORD PARAMETERS
C
c-----------------------------------------------------------------------
C-----SET UP LOOP OVER CARDS.
      DO 20 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----READ AS CHARACTERS
      READ(ITAPE,30) FIELD6
C-----CONVERT FROM CHARACTERS TO FLOATING POINT
      K=0
      DO 10 L=I1,I2
      K=K+1
   10 CALL IN9(X(L),FIELD6(1,K))
   20 CONTINUE
      RETURN
   30 FORMAT(66A1)
      END
      SUBROUTINE LISTIV(IUNIT,X,IX)
C=======================================================================
C
C     READ LIST DATA LISTI + V = With Variable Input unit (IUNIT)
C
C     Used to Read PREPRO Input Parameters.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 FIELD6
      DIMENSION X(IX),FIELD6(11,6)
c-----------------------------------------------------------------------
C
C     READ LIST RECORD PARAMETERS
C
c-----------------------------------------------------------------------
C-----SET UP LOOP OVER CARDS.
      DO 20 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----READ AS CHARACTERS
      READ(IUNIT,30) FIELD6
C-----CONVERT FROM CHARACTERS TO FLOATING POINT
      K=0
      DO 10 L=I1,I2
      K=K+1
   10 CALL IN9(X(L),FIELD6(1,K))
   20 CONTINUE
      RETURN
   30 FORMAT(66A1)
      END
      SUBROUTINE LISTO(X,IX)
C=======================================================================
C
C     WRITE LIST DATA
C
C     2013/1/12 - changed to OUT10 from OUT9.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION X(IX),FIELD6(11,6)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS TO OUTPUT
      IF(IX.LE.0) RETURN
C-----SET UP LOOP OVER CARDS.
      DO 80 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 L=I1,I2
      K=K+1
c-----2017/10/30 - Only for MF=2 resonance parameter energy
      if(MFH.eq.2.and.K.eq.1) then
c-----2013/1/12 - changed to OUT10 from OUT9.
      CALL OUT10(X(L),FIELD6(1,K))
      else
      CALL OUT9 (X(L),FIELD6(1,K))
      endif
   10 CONTINUE
C-----OUTPUT ONE LINE.
      GO TO (20,30,40,50,60,70),K
   20 WRITE(OTAPE,90)   (FIELD6(M,1),M=1,11),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   30 WRITE(OTAPE,100) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   40 WRITE(OTAPE,110) ((FIELD6(M,II),M=1,11),II=1,3),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   50 WRITE(OTAPE,120) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   60 WRITE(OTAPE,130) ((FIELD6(M,II),M=1,11),II=1,5),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   70 WRITE(OTAPE,140) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   80 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   90 FORMAT(11A1,55X,I4,I2,I3,I5)
  100 FORMAT(22A1,44X,I4,I2,I3,I5)
  110 FORMAT(33A1,33X,I4,I2,I3,I5)
  120 FORMAT(44A1,22X,I4,I2,I3,I5)
  130 FORMAT(55A1,11X,I4,I2,I3,I5)
  140 FORMAT(66A1    ,I4,I2,I3,I5)
      END
      SUBROUTINE LISTO9(X,IX)
C=======================================================================
C
C     WRITE LIST DATA
C
C     SAME AS LISTO, BUT USE OUT9G NOT OUT10.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION X(IX),FIELD6(11,6)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS TO OUTPUT
      IF(IX.LE.0) RETURN
C-----SET UP LOOP OVER CARDS.
      DO 80 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 L=I1,I2
      K=K+1
c-----USE OUT9G - NOT OUT10.
   10 CALL OUT9G(X(L),FIELD6(1,K))
C-----OUTPUT ONE LINE.
      GO TO (20,30,40,50,60,70),K
   20 WRITE(OTAPE,90)   (FIELD6(M,1),M=1,11),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   30 WRITE(OTAPE,100) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   40 WRITE(OTAPE,110) ((FIELD6(M,II),M=1,11),II=1,3),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   50 WRITE(OTAPE,120) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   60 WRITE(OTAPE,130) ((FIELD6(M,II),M=1,11),II=1,5),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   70 WRITE(OTAPE,140) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   80 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   90 FORMAT(11A1,55X,I4,I2,I3,I5)
  100 FORMAT(22A1,44X,I4,I2,I3,I5)
  110 FORMAT(33A1,33X,I4,I2,I3,I5)
  120 FORMAT(44A1,22X,I4,I2,I3,I5)
  130 FORMAT(55A1,11X,I4,I2,I3,I5)
  140 FORMAT(66A1    ,I4,I2,I3,I5)
      END
      SUBROUTINE LINEIN
C=======================================================================
C
C     READ A LINE, INCLUDING MAT, MF, MT
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*4 CARD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
      READ(ITAPE,10) CARD,MATH,MFH,MTH
      RETURN
   10 FORMAT(16A4,A2,I4,I2,I3)
      END
      SUBROUTINE LINEOUT
C=======================================================================
C
C     WRITE A LINE
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*4 CARD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----USE STANDARD FORM FOR END LINES
      IF(MTH.GT.0) GO TO 10
      CALL OUTS(MATH,MFH)
      RETURN
   10 WRITE(OTAPE,20) CARD,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   20 FORMAT(16A4,A2,I4,I2,I3,I5)
      END
      SUBROUTINE COPYT
C=======================================================================
C
C     COPY TO TEND, MEND, FEND OR SEND RECORDS.
C     ENTRY POINTS ARE,
C     COPYT = COPY TO TEND RECORD
C     COPYM = COPY TO MEND RECORD
C     COPYF = COPY TO FEND RECORD
C     COPYS = COPY TO SEND RECORD
C     COPYL = COPY TAPE LABEL
C     COPY1 = COPY ONE LINE
C
C     COPY TO TEND RECORD
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      CHARACTER*4 CARD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
      COMMON/COPI/MFIELD(3)
   10 READ(ITAPE,150) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 30
      IF(MFIELD(3).GT.0) GO TO 20
      CALL OUTS(MFIELD(1),MFIELD(2))
      GO TO 30
   20 WRITE(OTAPE,150) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MAT < 0
   30 IF(MFIELD(1).ge.0) go to 10
      RETURN
C=======================================================================
C
C     COPY TO MEND RECORD
C
C=======================================================================
      ENTRY COPYM
   40 READ(ITAPE,150) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 60
      IF(MFIELD(3).GT.0) GO TO 50
      CALL OUTS(MFIELD(1),MFIELD(2))
      GO TO 60
   50 WRITE(OTAPE,150) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MAT <= 0
   60 IF(MFIELD(1).gt.0) go to 40
      RETURN
C=======================================================================
C
C     COPY TO FEND RECORD
C
C=======================================================================
      ENTRY COPYF
   70 READ(ITAPE,150) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 90
      IF(MFIELD(3).GT.0) GO TO 80
      CALL OUTS(MFIELD(1),MFIELD(2))
      GO TO 90
   80 WRITE(OTAPE,150) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MF <= 0
   90 IF(MFIELD(2).gt.0) go to 70
      RETURN
C=======================================================================
C
C     COPY TO SEND RECORD
C
C=======================================================================
      ENTRY COPYS
  100 READ(ITAPE,150) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 120
      IF(MFIELD(3).GT.0) GO TO 110
      CALL OUTS(MFIELD(1),MFIELD(2))
      GO TO 120
  110 WRITE(OTAPE,150) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MT <= 0
  120 IF(MFIELD(3).gt.0) go to 100
      RETURN
C=======================================================================
C
C     COPY TAPE LABEL
C
C=======================================================================
      ENTRY COPYL
      READ(ITAPE,150) CARD,MFIELD
      IF(OTAPE.LE.0) RETURN
C-----MF/MT/NOSEQ = 0 ON TEND LINE
      NOSEQ=0
C-----USE STANDARD TAPE NUMBER IF INPUT = 0
      IF(MFIELD(1).EQ.0) MFIELD(1)=7000      ! 2014/4/20 6000 to 7000
      MFIELD(2)=0
      MFIELD(3)=0
      WRITE(OTAPE,150) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
C=======================================================================
C
C     COPY ONE LINE
C
C=======================================================================
      ENTRY COPY1
      READ(ITAPE,150) CARD,MFIELD
      IF(OTAPE.LE.0) RETURN
      IF(MFIELD(3).GT.0) GO TO 130
      CALL OUTS(MFIELD(1),MFIELD(2))
      GO TO 140
  130 WRITE(OTAPE,150) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
  140 RETURN
  150 FORMAT(16A4,A2,I4,I2,I3,I5)
      END
      SUBROUTINE OUTT
C=======================================================================
C
C     OUTPUT TEND, MEND, FEND OR SEND RECORDS.
C     ENTRY POINTS ARE,
C     OUTT = OUTPUT TEND RECORD
C     OUTM = OUTPUT MEND RECORD
C     OUTF = OUTPUT FEND RECORD
C     OUTS = OUTPUT SEND RECORD
C
C     OUTPUT TEND RECORD
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,10)
      RETURN
C=======================================================================
C
C     OUTPUT MEND RECORD
C
C=======================================================================
      ENTRY OUTM
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,20)
C-----RESET NOSEQ AFTER MEND OUTPUT
      NOSEQ=1
      RETURN
C=======================================================================
C
C     OUTPUT FEND RECORD
C
C=======================================================================
      ENTRY OUTF(MATOUT)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,30) MATOUT
      NOSEQ=1
      RETURN
C=======================================================================
C
C     OUTPUT SEND RECORD
C
C=======================================================================
      ENTRY OUTS(MATOUT,MFOUT)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOSEQ = 0 ON FEND/MEND/TEND LINE
      IF(MFOUT.LE.0) then
      WRITE(OTAPE,30) MATOUT
      else
      WRITE(OTAPE,40) MATOUT,MFOUT  ! NOSEQ = 99999
      endif
      NOSEQ=1
      RETURN
   10 FORMAT(66X,'  -1 0  0    0')
   20 FORMAT(66X,'   0 0  0    0')
   30 FORMAT(66X,I4, ' 0  0    0')
   40 FORMAT(66X,I4,I2,'  099999')
      END
      SUBROUTINE SKIPT
C=======================================================================
C
C     SKIP TO TEND, MEND, FEND OR SEND RECORDS.
C     ENTRY POINTS ARE,
C     SKIPT = SKIP TO TEND RECORD
C     SKIPM = SKIP TO MEND RECORD
C     SKIPF = SKIP TO FEND RECORD
C     SKIPS = SKIP TO SEND RECORD
C     SKIPL = SKIP TAPE LABEL
C     SKIP1 = SKIP ONE LINE
C
C     SKIP TO TEND RECORD
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/COPI/MFIELD(3)
   10 READ(ITAPE,50) MFIELD
C-----NEED MAT < 0
      IF(MFIELD(1).ge.0) go to 10
      RETURN
C=======================================================================
C
C     SKIP TO MEND RECORD
C
C=======================================================================
      ENTRY SKIPM
   20 READ(ITAPE,50) MFIELD
C-----NEED MAT <= 0
      IF(MFIELD(1).gt.0) go to 20
      RETURN
C=======================================================================
C
C     SKIP TO FEND RECORD
C
C=======================================================================
      ENTRY SKIPF
   30 READ(ITAPE,50) MFIELD
C-----NEED MF <= 0
      IF(MFIELD(2).gt.0) go to 30
      RETURN
C=======================================================================
C
C     SKIP TO SEND RECORD
C
C=======================================================================
      ENTRY SKIPS
   40 READ(ITAPE,50) MFIELD
C-----NEED MT <= 0
      IF(MFIELD(3).gt.0) go to 40
      RETURN
C=======================================================================
C
C     SKIP TAPE LABEL
C
C=======================================================================
      ENTRY SKIPL
      READ(ITAPE,50) MFIELD
      RETURN
C=======================================================================
C
C     SKIP ONE LINE
C
C=======================================================================
      ENTRY SKIP1
      READ(ITAPE,50) MFIELD
      RETURN
   50 FORMAT(66X,I4,I2,I3,I5)
      END
      SUBROUTINE HOLLYI(LINE66)
C=======================================================================
C
C     READ A LINE OF 66 CHARACTERS
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 LINE66
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      READ(ITAPE,10) LINE66
      RETURN
   10 FORMAT(66A1,I4,I2,I3,I5)
      END
      SUBROUTINE HOLLYO(LINE66)
C=======================================================================
C
C     WRITE A LINE OF 66 CHARACTERS
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 LINE66
      INTEGER*4 OUTP,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION LINE66(66)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,10) LINE66,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   10 FORMAT(66A1,I4,I2,I3,I5)
      END
      FUNCTION NXTSEQ(NNSEQ)
C=======================================================================
C
C     DEFINE NEXT SEQUENCE NUMBER FOR ENDF/B OUTPUT. ALLOW FOR
C     MORE THAN 100000 LINES PER EVALUATION BY RESETTING NUMBER
C     TO 1 EVERY TIME 100000 IS REACHED.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      NN=NNSEQ+1
      IF(NN.EQ.100000) NN=1
      NXTSEQ=NN
      RETURN
      END
      SUBROUTINE ZAHOL(ZA,ZABCD)
C=======================================================================
C
C     GIVEN ANY ZA (1000*Z+A) THIS ROUTINE WILL DEFINE A 10 CHARACTER
C     EQUIVALENT IN THE FORM,
C
C     2010/07/04 - ADDED Z = 104 THROUGH 110.
C     2018/02/01 - ADDED Z = 111 THROUGH 118.
C
C     CHARACTER POSITION (1 THROUGH 10)
C              1
C     1234567890
C
C     ZZZ-SS-AAA
C
C     ZZZ  - CHARACTER REPRESENTATION OF Z
C     SS   - CHEMICAL SYMBOL FOR ELEMENT
C     AAA  - CHARACTER REPRESENTATION FOR A OR NAT, IF A = 0
C
C     Z IS RIGHT ADJUSTED TO END IN CHARACTER 3
C     A IS LEFT ADJUSTED TO START IN CHARACTER 8
C
C     EXAMPLE, ZA = 6012 IS RETURNED AS,
C
C     CHARACTER POSITION (1 THROUGH 10)
C              1
C     1234567890
C
C       6-C -12
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      INTEGER*4 ZA,Z,A
      CHARACTER*1 DUM1,DUM2,ZABCD,ZATAB,DIGITS,NEUTRON,FISSPRO,PHOTON
      DIMENSION ZATAB(2,118),DUM1(2,54),DUM2(2,64),ZABCD(10),
     1 DIGITS(10),NEUTRON(10),FISSPRO(10),PHOTON(10)
      EQUIVALENCE (ZATAB(1,1),DUM1(1,1)),(ZATAB(1,55),DUM2(1,1))
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DATA DUM1/
     1 'H',' ','H','e','L','i','B','e','B',' ','C',' ',
     2 'N',' ','O',' ','F',' ','N','e','N','a','M','g',
     3 'A','l','S','i','P',' ','S',' ','C','l','A','r',
     4 'K',' ','C','a','S','c','T','i','V',' ','C','r',
     5 'M','n','F','e','C','o','N','i','C','u','Z','n',
     6 'G','a','G','e','A','s','S','e','B','r','K','r',
     7 'R','b','S','r','Y',' ','Z','r','N','b','M','o',
     8 'T','c','R','u','R','h','P','d','A','g','C','d',
     9 'I','n','S','n','S','b','T','e','I',' ','X','e'/
      DATA DUM2/
     1 'C','s','B','a','L','a','C','e','P','r','N','d',
     2 'P','m','S','m','E','u','G','d','T','b','D','y',
     3 'H','o','E','r','T','m','Y','b','L','u','H','f',
     4 'T','a','W',' ','R','e','O','s','I','r','P','t',
     5 'A','u','H','g','T','l','P','b','B','i','P','o',
     6 'A','t','R','n','F','r','R','a','A','c','T','h',
     7 'P','a','U',' ','N','p','P','u','A','m','C','m',
     8 'B','k','C','f','E','s','F','m','M','d','N','o',
c----- 2018/2/1 Corrected Z=108 = Hs
     9 'L','r','R','f','D','b','S','g','B','h','H','s',
c----- 2018/2/1 Added Z = 111 to 118
c       109     110     111     112     113     114
     A 'M','t','D','s','R','g','C','n','N','h','F','l',
c       115     116     117     118
     B 'M','c','L','v','T','s','O','g'/
      DATA NEUTRON/' ','N','e','u','t','r','o','n',' ',' '/
      DATA PHOTON /' ','P','h','o','t','o','n',' ',' ',' '/
      DATA FISSPRO/'F','i','s','s','.','P','r','o','d','.'/
c-----------------------------------------------------------------------
C
C     SPECIAL TREATMENT FOR ENDL NEUTRON, FISSION PRODUCTS
C     AND PHOTON
C
c-----------------------------------------------------------------------
C-----NEUTRON?
      IF(ZA.NE.1) GO TO 20
      DO 10 I=1,10
   10 ZABCD(I)=NEUTRON(I)
      RETURN
C-----FISSION PRODUCT?
   20 IF(ZA.NE.99120.AND.ZA.NE.99125) GO TO 40
      DO 30 I=1,10
   30 ZABCD(I)=FISSPRO(I)
      RETURN
C-----PHOTON?
   40 IF(ZA.EQ.0) THEN
      DO 50 I=1,10
   50 ZABCD(I)=PHOTON(I)
      RETURN
      ENDIF
c-----------------------------------------------------------------------
C
C     NORMAL TREATMENT
C
c-----------------------------------------------------------------------
C-----BLANK OUT ZABCD TO START.
      DO 60 I=1,10
   60 ZABCD(I)=' '
C-----DEFINE Z AND A SEPARATELY.
      Z=ZA/1000
      A=ZA-1000*Z
C-----DEFINE SYMBOL FOR ELEMENT.
      ZABCD(4)='-'
      ZABCD(7)='-'
      IF(Z.GT.0.AND.Z.LE.118) GO TO 70
      ZABCD(5)='?'
      ZABCD(6)='?'
      IF(Z.LT.0.OR.Z.GT.999) GO TO 100
      GO TO 80
   70 ZABCD(5)=ZATAB(1,Z)
      ZABCD(6)=ZATAB(2,Z)
C-----DEFINE Z LAST DIGIT TO FIRST.
   80 II=3
      DO 90 I=1,3
      NEXTZ=Z/10
      KZ=Z-10*NEXTZ
      ZABCD(II)=DIGITS(KZ+1)
      Z=NEXTZ
      IF(Z.LE.0) GO TO 100
   90 II=II-1
  100 IF(A.GT.0) GO TO 110
C-----NATURAL ISOTOPIC MIXTURE.
      ZABCD(8) ='N'
      ZABCD(9) ='a'
      ZABCD(10)='t'
      GO TO 140
C-----DEFINE A FIRST DIGIT TO LAST.
  110 IDIV=100
      IMON=0
      II=7
      DO 130 I=1,3
      IA=A/IDIV
      IF(IA.EQ.0.AND.IMON.EQ.0) GO TO 120
      IMON=1
      II=II+1
      ZABCD(II)=DIGITS(IA+1)
  120 A=A-IDIV*IA
  130 IDIV=IDIV/10
  140 RETURN
      END
      REAL*8 FUNCTION TERPIT(X,X1,X2,Y1,Y2,INTERP)
C=======================================================================
C
C     INTERPOLATION ACCORDING TO ENDF/B LAWS 1 THROUGH 6.
C
C     INTERPOLATE BETWEEN (X1,Y1) AND (X2,Y2) TO DEFINE
C     Y AT X.
C
C     WARNING - THIS ROUTINE DOES NOT CHECK THE CONSISTENCY
C     BETWEEN DATA AND THE ENDF/B INTERPOLATION LAW - THEREFORE
C     TO AVOID ERRORS DURING EXECUTION THE USER MUST CHECK
C     CONSISTENCY BEFORE CALLING THIS ROUTINE.
C
C     CONSISTENCY = INTERPOLATION LAW = 1 THROUGH 6
C                 = NO DISCONTINUITIES, X1 = X2
C                 = ONLY POSITIVE VALUES IF LOG INTERPOLATION
C
C     06/02/10 - ADDED CONSISTENCY CHECKS FOR ALL PARAMETERS THAT
C                ARE NON-POSTITIVE AND REQUIRE TAKING THEIR LOG -
C                IN ALL SUCH CASE THIS ROUTINE SWITCHES TO LINEAR
C                (INTERP=2) INTERPOLATION.
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      DATA ONED /1.0D+00/
      DATA ZEROD/0.0D+00/
c-----------------------------------------------------------------------
C
C     FOR X1 = X2 OR Y1 = Y2 USE Y1.
C
c-----------------------------------------------------------------------
      IF(X1.EQ.X2.OR.Y1.EQ.Y2) GO TO 10
c-----------------------------------------------------------------------
C
C     SELECT INTERPOLATION METHOD.
C
C     IN ALL CASES THE RESULT (Y) IS THE WEIGHTED SUM OF
C     CONTRIBUTIONS FROM THE 2 ENDS OF THE INTERVAL.
C
c-----------------------------------------------------------------------
      GO TO (10,20,30,40,50,60),INTERP
C-----1) HISTOGRAM - OR X1=X2 OR Y1=Y2 DEFINE Y = Y1
   10 TERPIT=Y1
      RETURN
C-----2) LIN X VS. LIN Y.
   20 WT2=(X-X1)/(X2-X1)
      WT1=ONED-WT2
      TERPIT=WT2*Y2+WT1*Y1
      RETURN
C-----3) LOG X VS. LIN Y.
   30 IF(X.LE.ZEROD.OR.X1.LE.ZEROD.OR.X2.LE.ZEROD) GO TO 20
      WT2=DLOG(X/X1)/DLOG(X2/X1)
      WT1=ONED-WT2
      TERPIT=WT2*Y2+WT1*Y1
      RETURN
C-----4) LIN X VS. LOG Y.
   40 IF(Y1.LE.ZEROD.OR.Y2.LE.ZEROD) GO TO 20
      WT2=(X-X1)/(X2-X1)
      WT1=ONED-WT2
      TERPIT=DEXP(WT2*DLOG(Y2)+WT1*DLOG(Y1))
      RETURN
C-----5) LOG X VS. LOG Y.
   50 IF(X.LE.ZEROD.OR.X1.LE.ZEROD.OR.X2.LE.ZEROD) GO TO 20
      IF(Y1.LE.ZEROD.OR.Y2.LE.ZEROD) GO TO 20
      WT2=DLOG(X/X1)/DLOG(X2/X1)
      WT1=ONED-WT2
      TERPIT=DEXP(WT2*DLOG(Y2)+WT1*DLOG(Y1))
      RETURN
C-----6) CHARGED PARTICLE THRESHOLDS...WARNING = THIS ASSUMES T = 0.0.
C-----06/02/09 - ORIGINAL DID NOT INCLUDE (A/E) TERM.
C
C     SIG = (A/E)*EXP[-B/SQRT(E - T)]
C     E*SIG = A*EXP[-B/SQRT(E-T)]
C     LOG(E*SIG) = LOG(A) - B/SQRT(E-T)
C     LOG(E*SIG) = WT2*LOG(X2*Y2) + WT1*LOG(X1*Y1)
C
C-----06/02/09 = USE LINEAR NEAR X OR Y <= 0
   60 IF(X.LE.ZEROD.OR.X1.LE.ZEROD.OR.X2.LE.ZEROD) GO TO 20
      IF(Y1.LE.ZEROD.OR.Y2.LE.ZEROD) GO TO 20
C-----OTHERWISE WEIGHT FOR E*SIG IS,
C-----WT2 = (1/SQRT(E)-1/SQRT(E1))/(1/SQRT(E2)-1/SQRT(E1))
      WT2=(ONED/DSQRT( X) - ONED/DSQRT(X1))/
     1    (ONED/DSQRT(X2) - ONED/DSQRT(X1))
      WT1=ONED-WT2
C-----LOG(E*SIG) = WT2*LOG(X2*Y2) + WT1*LOG(X1*Y1)
      TERPIT=DEXP(WT2*DLOG(X2*Y2)+WT1*DLOG(X1*Y1))/X
      RETURN
      END
      SUBROUTINE INCORE9(ZIN)
C=======================================================================
C
C     PURPOSE
C     =======
C     ROUND NUMBER TO FROM 5 TO 9 DIGITS OF ACCURACY.
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C                  No test for 4 or more digit exponent
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused
C                  problems on several different types of computers.
C
C     ARGUMENTS
C     =========
C     ZIN      = NUMBER OF BE ROUNDED (INPUT/OUTPUT)
C
C     METHOD
C     ======
C     COLUMNS            12345678901     ACCURACY
C     -------------------------------------------
C     0 TO 10^-9          1.2345E-12     5 DIGITS
C     10^-9 TO 10^-4      1.23456E-8     6 DIGITS
C     10^-4 TO 10^-3      .000123456     6 DIGITS
C     10^-3 TO 10^-2      .001234567     7 DIGITS
C     10^-2 TO 10^-1      .012345678     8 DIGITS
C     10^-1 TO 1          .123456789     9 DIGITS
C     1 TO 10^9           12345.6789     9 DIGITS
C     10^9 TO 10^10       1.23456E+9     6 DIGITS
C     10^10 >             1.2345E+12     5 DIGITS
C
C=======================================================================
c----- 2017/10/14 replaced INCLUDE
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      REAL*8 IN
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DIMENSION TENS(-99:99),ROUNDER(-99:99)
C-----ON FIRST CALL INITIALIZE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
      INMAN8 = 100000000
      INMAN9 = 1000000000
      TENS(0)=1.0D+00
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DO 10 I=1,99
      TENS( I)=TENS(I-1)*10.0D+00
      TENS(-I)=TENS(1-I)/10.0D+00
   10 ROUNDER(I)  = 5.001D-05
      DO 20 I=-99,-10
   20 ROUNDER(I)  = 5.001D-05
      DO 30 I=-9,-4
   30 ROUNDER(I)  = 5.001D-06
      ROUNDER(-3) = 5.001D-07
      ROUNDER(-2) = 5.001D-08
      ROUNDER(-1) = 5.001D-09
      ROUNDER( 0) = 5.001D-09
      DO 40 I=1,8
   40 ROUNDER(I)  = 5.001D-09
      ROUNDER(9)  = 5.001D-06
c-----------------------------------------------------------------------
C
C     NO ROUNDING NECESSARY FOR ZERO - RETURN
C     OTHERWISE DEFINE SIGN AND ABSOLUTE VALUE.
C
c-----------------------------------------------------------------------
   50 IF(ZIN.eq.0.0D+0) go to 160 ! no rounding if = 0
      IF(ZIN.gt.0.0D+0) go to 60
C-----NEGATIVE.
      ZSIGN=-1.0D+00
      Z=-ZIN
      GO TO 70
C-----POSITIVE.
   60 ZSIGN=1.0D+00
      Z=ZIN
c-----------------------------------------------------------------------
C
C     DEFINE EXPONENT AND NORMALIZED MANTISSA
C
c-----------------------------------------------------------------------
   70 IEXP=DLOG10(Z)
      IF(Z.LT.1.0D+00) IEXP = IEXP - 1
      IF(iabs(IEXP).gt.99) go to 160    ! no 2 digit exponent
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)
      IF(ZN.eq.1.0D+00) go to 160 ! no rounding powers of 10
      IF(ZN.gt.1.0D+00) go to 80
      IEXP=IEXP-1                 ! < 1
      ZN=10.0D+00*ZN
      IF(iabs(IEXP).gt.99) go to 160    ! no 2 digit exponent
      Z = ZN*TENS(IEXP)
      GO TO 90
   80 IF(ZN.eq.10.0D+00) go to 160 ! no rounding powers of 10
      IF(ZN.lt.10.0D+00) go to 90
      IEXP=IEXP+1                ! > 10
      ZN=ZN/10.0D+00
      IF(iabs(IEXP).gt.99) go to 160    ! no 2 digit exponent
      Z = ZN*TENS(IEXP)
c-----------------------------------------------------------------------
C
C     ZN IS NOW IN NORMAL FORM 1.23456789...
C
C-----------------------------------------------------------------------
C
C     TEST FOR SPECIAL RANGES = VERY LOW PROBABILITY
C
C-----------------------------------------------------------------------
   90 IF(Z.GE.1.0D+00) GO TO 110
C
C     IF EXTREMELY LOW ENERGY RANGE < 10^-10 USE 5 DIGITS
C
      IF(Z.LT.1.0D-09) GO TO 120
      IF(Z.GE.1.0D-04) GO TO 100
C-----10^-10 TO 10^-4 = 6 DIGITS
      IN = ZN*TENS(5)
      KEXP = IEXP-5
      GO TO 140
C-----10^-4 TO 1: 6 TO 9 DIGITS
  100 II = 9 + IEXP
      IF(iabs(II).gt.99) go to 160    ! no 2 digit exponent
      IN = ZN*TENS(II)
      KEXP = IEXP-II
      GO TO 140
C-----------------------------------------------------------------------
C
C     HIGH ENERGY RANGE CHECK > 10^9
C
C-----------------------------------------------------------------------
  110 IF(Z.LT.1.0D+09) GO TO 130
      IF(Z.GE.1.0D+10) GO TO 120
C-----------------------------------------------------------------------
C
C     10^9 TO 10^10 = 6 DIGITS
C
C-----------------------------------------------------------------------
      IN = ZN*TENS(5)
      KEXP = IEXP-5
      GO TO 140
C-----------------------------------------------------------------------
C
C     EXTREME LOW AND HIGH ENERGY RANGE - USE 5 DIGITS
C
C-----------------------------------------------------------------------
  120 IN = ZN*TENS(4)
      KEXP = IEXP-4
      GO TO 140
C-----------------------------------------------------------------------
C
C     NORMAL RANGE - 1 TO < 10^10 - USE 9 DIGITS = HIGH PROBABILITY
C
C-----------------------------------------------------------------------
  130 IN = ZN*TENS(8)
      KEXP = IEXP-8
C-----------------------------------------------------------------------
C
C     IN IS NOW IN 9 DIGIT FORM 123456789
C     IF 10 DIGIT, DUE TO ROUNDING - DECREASE BY 10 AND INCREASE IEXP
C
C-----------------------------------------------------------------------
c-----2014/4/14 - changed to INMAN9, instead of integer strings.
  140 IF(IN.lt.INMAN9) go to 150
      IN  = INMAN8
      IEXP = IEXP + 1
C-----------------------------------------------------------------------
C
C     FLOAT 9 DIGIT AND RESTORE EXPONENT
C
C-----------------------------------------------------------------------
  150 Z   = IN
      IF(iabs(KEXP).gt.99) go to 160    ! no 2 digit exponent
      ZIN = ZSIGN*Z*TENS(KEXP)
      RETURN
C-----------------------------------------------------------------------
C
C     NO ROUNDING NECESSARY FOR 0
C
C-----------------------------------------------------------------------
  160 RETURN
      END
      SUBROUTINE OUT9(ZIN,FIELD)
C=======================================================================
C
C     PURPOSE
C     =======
C     FORMAT NUMBER FOR OUTPUT TO INCLUDE AS MANY DIGITS OF
C     ACCURACY AS POSSIBLE.
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C                  No test for 4 or more digit exponent
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused
C                  problems on several different types of computers.
C     10/25/2014 - Changed from D to E exponential form to improve
C                  compatibility between computer languages
C
C     040923 - CHANGED ZLOW FROM 1.0D-4 TO 1.0D-3
C              NEAR 1.0D-3 PRECISION GOES FROM 999,999 TO 1,000,000
C              FOR SMOOTHEST TRANSITION FROM 6 TO 7 DIGITS
C
C     ARGUMENTS
C     =========
C     Z        = FLOATING POINT NUMBER OF BE OUTPUT (INPUT)
C     FIELD    = 11A1 CHARACTERS TO OUTPUT          (OUTPUT)
C
C     METHOD
C     ======
C     COLUMNS            12345678901     ACCURACY
C     -------------------------------------------
C     0 TO 10^-9          1.2345E-12     5 DIGITS
C     10^-9 TO 10^-3      1.23456E-8     6 DIGITS
C     10^-3 TO 10^-2      .001234567     7 DIGITS
C     10^-2 TO 10^-1      .012345678     8 DIGITS
C     10^-1 TO 1          .123456789     9 DIGITS
C     1 TO 10^9           12345.6789     9 DIGITS
C     10^9 TO 10^10       1.23456E+9     6 DIGITS
C     10^10 >             1.2345E+12     5 DIGITS
C
C     OUTPUT WILL BE IN 11 COLUMN FORMAT
C
C     WARNING - THIS IS NOT A GENERAL ROUNDING ROUTINE WHICH WILL WORK
C               FOR ROUNDING TO ANY NUMBER OF DIGITS - IT WILL ONLY
C               WORK PROPERLY FOR THE RANGES INDICATED ABOVE, FOR
C               11 COLUMN OUTPUT.
C
C=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
c-----2017/5/3 - Add INNEXT = must be INTEGER*8
c-----INTEGER*8 for long integers
      integer*8 INMANT,INMAN9,INMAN8,INMAN7,INMAN6,INMAN5,INMAN4,INNEXT
c-----2017/5/3 - Add INNEXT = must be INTEGER*8
      CHARACTER*1 FIELD,DIGITS,ZEROH
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DIMENSION DIGITS(0:9),FIELD(11),ZEROH(11),
     1 TENS(-99:99),ROUNDER(-99:99)
      DATA DIGITS/
     1 '0','1','2','3','4','5','6','7','8','9'/
C-----RETURN FOR = 0
      DATA ZEROH/
     1 ' ','0','.','0',' ',' ',' ',' ',' ',' ',' '/
C-----LOWER TRANSITION POINT FROM 7 TO 9 DIGIT OUTPUT
      DATA ZLOW/1.0D-03/
C-----UPPER TRANSITION POINT FROM 9 TO 7 DIGIT OUTPUT
      DATA ZHIGH/1.0D+09/
      DATA TENTH/1.0D-01/
c-----------------------------------------------------------------------
c
c     ON FIRST CALL INITIALIZE POWERS OF 10
c
c-----------------------------------------------------------------------
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
c-----INTEGER*8 for long integers
      INMAN4  = 10000
      INMAN5  = 10*INMAN4
      INMAN6  = 10*INMAN5
      INMAN7  = 10*INMAN6
      INMAN8  = 10*INMAN7
      INMAN9  = 10*INMAN8
      TENS(0)=1.0D+00
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DO 10 I=1,99
      TENS( I)=TENS(I-1)*10.0D+00
      TENS(-I)=TENS(1-I)/10.0D+00
   10 ROUNDER(I)  = 5.001D-05
      DO 20 I=-99,-10
   20 ROUNDER(I)  = 5.001D-05
      DO 30 I=-9,-4
   30 ROUNDER(I)  = 5.001D-06
      ROUNDER(-3) = 5.001D-07
      ROUNDER(-2) = 5.001D-08
      ROUNDER(-1) = 5.001D-09
      ROUNDER( 0) = 5.001D-09
      DO 40 I=1,8
   40 ROUNDER(I)  = 5.001D-09
      ROUNDER(9)  = 5.001D-06
c-----------------------------------------------------------------------
C
C     IMMEDIATELY RETURN 0.00000+00.
C
c-----------------------------------------------------------------------
C-----02/14/04 - ADDED, JUST IN CASE
   50 IF(DABS(ZIN).le.0.0D+0) GO TO 60
      IF(DABS(ZIN).lt.1.0D-99) GO TO 60    ! Only 2 digit exponents
      IF(DABS(ZIN).gt.1.0D+99) GO TO 60
c----- ZIN = 0 handled above
      IF(ZIN.lt.0.0d+0) go to 80
      go to 90
c-----Return 0
   60 DO 70 I=1,11
   70 FIELD(I)=ZEROH(I)
      RETURN
c-----------------------------------------------------------------------
C
C     DEFINE SIGN OF MANTISSA AND ABSOLUTE MANTISSA
C
c-----------------------------------------------------------------------
C-----NEGATIVE.
   80 FIELD(1)='-'
      Z=-ZIN
      GO TO 100
C-----POSITIVE.
   90 FIELD(1)=' '
      Z=ZIN
c-----------------------------------------------------------------------
c
c     DEFINE EXPONENT AND NORMALIZED MANTISSA
c
c-----------------------------------------------------------------------
  100 IEXP=DLOG10(Z)
      IF(Z.LT.1.0D+00) IEXP = IEXP - 1
c-----11/22/2013 - Decide here on F or E Output
c-----11/26/2013 - Restrict to 2 digit exponent - may change by 1 or 2
      if(IABS(IEXP).gt.97) go to 60
      if(ZIN.lt.0.0D+0) then
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)       ! Standard
      else
      if(Z.LE.ZLOW.OR.Z.GE.ZHIGH) then       ! F or E Format?
      ZN=Z*TENS(-IEXP) + TENTH*ROUNDER(IEXP) ! Extra Digit
      else
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)       ! Standard
      endif
      endif
      IF(ZN.eq.1.0D+00) go to 120            ! Test rounding underflow
      IF(ZN.gt.1.0D+00) go to 110            ! Test rounding underflow
      IEXP=IEXP-1         ! MUST be < 1
      GO TO 120
  110 IF(ZN.lt.10.0D+00) go to 120           ! Test rounding overflow
      IEXP=IEXP+1         ! MUST be >= 10
      ZN=ZN/10.0D+00
  120 Z = ZN*TENS(IEXP)
c-----------------------------------------------------------------------
C
C     SELECT F OR E FORMAT
C
c-----------------------------------------------------------------------
      IF(Z.LE.ZLOW.OR.Z.GE.ZHIGH) GO TO 150
c-----------------------------------------------------------------------
C
C     F FORMAT
C
C     12345678901
C      X.XXXXXXXX = 9 DIGITS
C      .001234567
C      123456789.
C
c-----------------------------------------------------------------------
C-----DEFINE 6 TO 9 DIGIT MANTISSA WITH ROUNDING
      IPOWER=8-IEXP
      IF(IEXP.LT.0) IPOWER=8
      INMANT=Z*TENS(IPOWER)
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      if(INMANT.lt.INMAN9) go to 130
      INMANT=INMAN8
      IEXP=IEXP+1
C-----DECIMAL POINT.
  130 IDOT=3+IEXP
      IF(IDOT.LE.2) THEN
C----- IF < 1, MOVE DECIMAL POINT TO COLUMN 2 AND ADD A DIGIT
      IDOT=2
      INMANT=Z*10.0D+00*TENS(IPOWER)
      ENDIF
      FIELD(IDOT)='.'
C-----MANTISSA - LAST DIGIT TO FIRST.
      II=11
      DO 140 I=2,11
      IF(II.EQ.IDOT) GO TO 140
      INNEXT=INMANT/10
      I3=INMANT-10*INNEXT
      FIELD(II)=DIGITS(I3)
      INMANT=INNEXT
  140 II=II-1
      RETURN
c-----------------------------------------------------------------------
C
C     E FORMAT
C
C     12345678901
C      X.XXXXE+NN = 5 DIGITS
C      X.XXXXXE+N = 6 DIGITS
C
C     11/22/2013 - If not negative, use first column
C     12345678901
C     X.XXXXXE+NN = 6 DIGITS
C     X.XXXXXXE+N = 7 DIGITS
c
c-----------------------------------------------------------------------
C-----Negative?
  150 IF(ZIN.lt.0.0d+0) go to 170
C-----------------------------------------------------------------------
c
c     POsitive. Use first column - Decimal point is always in column 2
c
C-----------------------------------------------------------------------
      FIELD(2)='.'
      KDOT    = 2
      ISTART  = 1
      IF(IABS(IEXP).GE.10) GO TO 160
      ID=8                               ! 1 Digit exponent
      INMANT=(1.0D+06)*ZN
      IF(INMANT.lt.INMAN7) go to 190
      INMANT=INMAN6
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 190
  160 ID=7                               ! 2 Digit exponent
      INMANT=(1.0D+05)*ZN
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(INMANT.lt.INMAN6) go to 190
      INMANT=INMAN5
      IEXP=IEXP+1
C-----------------------------------------------------------------------
c
c     Negative Number - Cannot use first column
c
C-----------------------------------------------------------------------
C-----DECIMAL POINT IS ALWAYS IN COLUMN 3
  170 FIELD(3)='.'
      KDOT    = 3
      ISTART  = 2
      IF(IABS(IEXP).GE.10) GO TO 180
      ID=8                                 ! 1 Digit Exponent
      INMANT=(1.0D+05)*ZN
      IF(INMANT.lt.INMAN6) go to 190
      INMANT=INMAN5
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 190
  180 ID=7                                 ! 2 Digit Exponent
      INMANT=(1.0D+04)*ZN
      IF(INMANT.lt.INMAN5) go to 190
      INMANT=INMAN4
      IEXP=IEXP+1
C-----------------------------------------------------------------------
C
C     DEFINE MANTISSA
C
C-----------------------------------------------------------------------
  190 IEXPS=ID+1
      II=ID
      DO 200 I=ISTART,ID
      IF(II.EQ.KDOT) GO TO 200
      INNEXT=INMANT/10
      I3=INMANT-10*INNEXT
      FIELD(II)=DIGITS(I3)
      INMANT=INNEXT
  200 II=II-1
C-----------------------------------------------------------------------
C
C     E
C
C-----------------------------------------------------------------------
      FIELD(IEXPS) = 'E'
      IEXPS = IEXPS + 1
C-----------------------------------------------------------------------
C
C     SIGN OF EXPONENT
C
C-----------------------------------------------------------------------
      IF(IEXP.ge.0) go to 210
      IEXP=-IEXP
      FIELD(IEXPS)='-'
      GO TO 220
  210 FIELD(IEXPS)='+'
C-----------------------------------------------------------------------
C
C     EXPONENT
C
C-----------------------------------------------------------------------
  220 IF(IEXP.lt.10) go to 230
      KEXP=IEXP/10
      FIELD(10)=DIGITS(KEXP)
      IEXP=MOD(IEXP,10)
  230 FIELD(11)=DIGITS(IEXP)
C-----------------------------------------------------------------------
c
c     If using column 1 but mantissa ends in 0, move mantissa right.
c
C-----------------------------------------------------------------------
      if(KDOT.eq.2.and.FIELD(ID).eq.'0') then
      do k=ID-1,1,-1
      FIELD(k+1)=FIELD(k)
      enddo
      FIELD(1) = ' '
      endif
      RETURN
      END
      SUBROUTINE OUT9G(ZIN,FIELD)
C=======================================================================
C
C     PURPOSE
C     =======
C     20915/7/28 - same as OUT9, but column 1 is always '-' or blank.
C                  Used by GROUPIE to simplify listings.
C
C     FORMAT NUMBER FOR OUTPUT TO INCLUDE AS MANY DIGITS OF
C     ACCURACY AS POSSIBLE.
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C                  No test for 4 or more digit exponent
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused
C                  problems on several different types of computers.
C     10/25/2014 - Changed from D to E exponential form to improve
C                  compatibility between computer languages
C
C     040923 - CHANGED ZLOW FROM 1.0D-4 TO 1.0D-3
C              NEAR 1.0D-3 PRECISION GOES FROM 999,999 TO 1,000,000
C              FOR SMOOTHEST TRANSITION FROM 6 TO 7 DIGITS
C
C     ARGUMENTS
C     =========
C     Z        = FLOATING POINT NUMBER OF BE OUTPUT (INPUT)
C     FIELD    = 11A1 CHARACTERS TO OUTPUT          (OUTPUT)
C
C     METHOD
C     ======
C     COLUMNS            12345678901     ACCURACY
C     -------------------------------------------
C     0 TO 10^-9          1.2345E-12     5 DIGITS
C     10^-9 TO 10^-3      1.23456E-8     6 DIGITS
C     10^-3 TO 10^-2      .001234567     7 DIGITS
C     10^-2 TO 10^-1      .012345678     8 DIGITS
C     10^-1 TO 1          .123456789     9 DIGITS
C     1 TO 10^9           12345.6789     9 DIGITS
C     10^9 TO 10^10       1.23456E+9     6 DIGITS
C     10^10 >             1.2345E+12     5 DIGITS
C
C     OUTPUT WILL BE IN 11 COLUMN FORMAT
C
C     WARNING - THIS IS NOT A GENERAL ROUNDING ROUTINE WHICH WILL WORK
C               FOR ROUNDING TO ANY NUMBER OF DIGITS - IT WILL ONLY
C               WORK PROPERLY FOR THE RANGES INDICATED ABOVE, FOR
C               11 COLUMN OUTPUT.
C
C=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
c-----INTEGER*8 for long integers
c-----2017/5/3 - Add INNEXT = must be INTEGER*8
      integer*8 INMANT,INMAN9,INMAN8,INMAN7,INMAN6,INMAN5,INMAN4,INNEXT
c-----2017/5/3 - Add INNEXT = must be INTEGER*8
      CHARACTER*1 FIELD,DIGITS,ZEROH
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DIMENSION DIGITS(0:9),FIELD(11),ZEROH(11),
     1 TENS(-99:99),ROUNDER(-99:99)
      DATA DIGITS/
     1 '0','1','2','3','4','5','6','7','8','9'/
C-----RETURN FOR = 0
      DATA ZEROH/
     1 ' ','0','.','0',' ',' ',' ',' ',' ',' ',' '/
C-----LOWER TRANSITION POINT FROM 7 TO 9 DIGIT OUTPUT
      DATA ZLOW/1.0D-03/
C-----UPPER TRANSITION POINT FROM 9 TO 7 DIGIT OUTPUT
      DATA ZHIGH/1.0D+09/
c-----------------------------------------------------------------------
c
c     ON FIRST CALL INITIALIZE POWERS OF 10
c
c-----------------------------------------------------------------------
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
c-----INTEGER*8 for long integers
      INMAN4  = 10000
      INMAN5  = 10*INMAN4
      INMAN6  = 10*INMAN5
      INMAN7  = 10*INMAN6
      INMAN8  = 10*INMAN7
      INMAN9  = 10*INMAN8
      TENS(0)=1.0D+00
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DO 10 I=1,99
      TENS( I)=TENS(I-1)*10.0D+00
      TENS(-I)=TENS(1-I)/10.0D+00
   10 ROUNDER(I)  = 5.001D-05
      DO 20 I=-99,-10
   20 ROUNDER(I)  = 5.001D-05
      DO 30 I=-9,-4
   30 ROUNDER(I)  = 5.001D-06
      ROUNDER(-3) = 5.001D-07
      ROUNDER(-2) = 5.001D-08
      ROUNDER(-1) = 5.001D-09
      ROUNDER( 0) = 5.001D-09
      DO 40 I=1,8
   40 ROUNDER(I)  = 5.001D-09
      ROUNDER(9)  = 5.001D-06
c-----------------------------------------------------------------------
C
C     IMMEDIATELY RETURN 0.00000+00.
C
c-----------------------------------------------------------------------
C-----02/14/04 - ADDED, JUST IN CASE
   50 IF(DABS(ZIN).le.0.0D+0) GO TO 60
      IF(DABS(ZIN).lt.1.0D-99) GO TO 60    ! Only 2 digit exponents
      IF(DABS(ZIN).gt.1.0D+99) GO TO 60
c----- ZIN = 0 handled above
      IF(ZIN.lt.0.0d+0) go to 80
      go to 90
c-----Return 0
   60 DO 70 I=1,11
   70 FIELD(I)=ZEROH(I)
      RETURN
c-----------------------------------------------------------------------
C
C     DEFINE SIGN OF MANTISSA AND ABSOLUTE MANTISSA
C
c-----------------------------------------------------------------------
C-----NEGATIVE.
   80 FIELD(1)='-'
      Z=-ZIN
      GO TO 100
C-----POSITIVE.
   90 FIELD(1)=' '
      Z=ZIN
c-----------------------------------------------------------------------
c
c     DEFINE EXPONENT AND NORMALIZED MANTISSA
c
c-----------------------------------------------------------------------
  100 IEXP=DLOG10(Z)
      IF(Z.LT.1.0D+00) IEXP = IEXP - 1
c-----11/22/2013 - Decide here on F or E Output
c-----11/26/2013 - Restrict to 2 digit exponent - may change by 1 or 2
      if(IABS(IEXP).gt.97) go to 60
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)       ! Standard
      IF(ZN.eq.1.0D+00) go to 120            ! Test rounding underflow
      IF(ZN.gt.1.0D+00) go to 110            ! Test rounding underflow
      IEXP=IEXP-1         ! MUST be < 1
      GO TO 120
  110 IF(ZN.lt.10.0D+00) go to 120           ! Test rounding overflow
      IEXP=IEXP+1         ! MUST be >= 10
      ZN=ZN/10.0D+00
  120 Z = ZN*TENS(IEXP)
c-----------------------------------------------------------------------
C
C     SELECT F OR E FORMAT
C
c-----------------------------------------------------------------------
      IF(Z.LE.ZLOW.OR.Z.GE.ZHIGH) GO TO 150
c-----------------------------------------------------------------------
C
C     F FORMAT
C
C     12345678901
C      X.XXXXXXXX = 9 DIGITS
C      .001234567
C      123456789.
C
c-----------------------------------------------------------------------
C-----DEFINE 6 TO 9 DIGIT MANTISSA WITH ROUNDING
      IPOWER=8-IEXP
      IF(IEXP.LT.0) IPOWER=8
      INMANT=Z*TENS(IPOWER)
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      if(INMANT.lt.INMAN9) go to 130
      INMANT=INMAN8
      IEXP=IEXP+1
C-----DECIMAL POINT.
  130 IDOT=3+IEXP
      IF(IDOT.LE.2) THEN
C----- IF < 1, MOVE DECIMAL POINT TO COLUMN 2 AND ADD A DIGIT
      IDOT=2
      INMANT=Z*10.0D+00*TENS(IPOWER)
      ENDIF
      FIELD(IDOT)='.'
C-----MANTISSA - LAST DIGIT TO FIRST.
      II=11
      DO 140 I=2,11
      IF(II.EQ.IDOT) GO TO 140
      INNEXT=INMANT/10
      I3=INMANT-10*INNEXT
      FIELD(II)=DIGITS(I3)
      INMANT=INNEXT
  140 II=II-1
      RETURN
c-----------------------------------------------------------------------
C
C     E FORMAT
C
C     12345678901
C      X.XXXXE+NN = 5 DIGITS
C      X.XXXXXE+N = 6 DIGITS
C
C     11/22/2013 - If not negative, use first column
C     12345678901
C     X.XXXXXE+NN = 6 DIGITS
C     X.XXXXXXE+N = 7 DIGITS
c
c-----------------------------------------------------------------------
c
c     Do not cannot use first column
c
c-----------------------------------------------------------------------
C-----DECIMAL POINT IS ALWAYS IN COLUMN 3
  150 FIELD(3)='.'
      KDOT    = 3
      ISTART  = 2
      IF(IABS(IEXP).GE.10) GO TO 160
      ID=8                                 ! 1 Digit Exponent
      INMANT=(1.0D+05)*ZN
      IF(INMANT.lt.INMAN6) go to 170
      INMANT=INMAN5
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 170
  160 ID=7                                 ! 2 Digit Exponent
      INMANT=(1.0D+04)*ZN
      IF(INMANT.lt.INMAN5) go to 170
      INMANT=INMAN4
      IEXP=IEXP+1
c-----------------------------------------------------------------------
C
C     DEFINE MANTISSA
C
c-----------------------------------------------------------------------
  170 IEXPS=ID+1
      II=ID
      DO 180 I=ISTART,ID
      IF(II.EQ.KDOT) GO TO 180
      INNEXT=INMANT/10
      I3=INMANT-10*INNEXT
      FIELD(II)=DIGITS(I3)
      INMANT=INNEXT
  180 II=II-1
c-----------------------------------------------------------------------
C
C     E
C
c-----------------------------------------------------------------------
      FIELD(IEXPS) = 'E'
      IEXPS = IEXPS + 1
c-----------------------------------------------------------------------
C
C     SIGN OF EXPONENT
C
c-----------------------------------------------------------------------
      IF(IEXP.ge.0) go to 190
      IEXP=-IEXP
      FIELD(IEXPS)='-'
      GO TO 200
  190 FIELD(IEXPS)='+'
c-----------------------------------------------------------------------
C
C     EXPONENT
C
c-----------------------------------------------------------------------
  200 IF(IEXP.lt.10) go to 210
      KEXP=IEXP/10
      FIELD(10)=DIGITS(KEXP)
      IEXP=MOD(IEXP,10)
  210 FIELD(11)=DIGITS(IEXP)
      RETURN
      END
      SUBROUTINE OUT10(ZIN,FIELD)
C=======================================================================
C
C     PURPOSE
C     =======
C     FORMAT NUMBER FOR OUTPUT TO INCLUDE AS MANY DIGITS OF
C     ACCURACY AS POSSIBLE.
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C                  No test for 4 or more digit exponent
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused
C                  problems on several different types of computers.
C     10/25/2014 - Changed from D to E exponential form to improve
C                  compatibility between computer languages
C
C     040923 - CHANGED ZLOW FROM 1.0D-4 TO 1.0D-3
C              NEAR 1.0D-3 PRECISION GOES FROM 999,999 TO 1,000,000
C              FOR SMOOTHEST TRANSITION FROM 6 TO 7 DIGITS
C
C     ARGUMENTS
C     =========
C     Z        = FLOATING POINT NUMBER OF BE OUTPUT (INPUT)
C     FIELD    = 11A1 CHARACTERS TO OUTPUT          (OUTPUT)
C
C     METHOD
C     ======
C     COLUMNS            12345678901     ACCURACY
C     -------------------------------------------
C     0 TO 10^-9         1.23456E-12     6 DIGITS
C     10^-9 TO 10^-3     1.234567E-8     7 DIGITS
C     10^-3 TO 10^-2     .0012345678     8 DIGITS
C     10^-2 TO 10^-1     .0123456789     9 DIGITS
C     10^-1 TO 1         .1234567891    10 DIGITS
C     1 TO 10^9          12345.67891    10 DIGITS
C     10^9 TO 10^10      1.234567E+9     7 DIGITS
C     10^10 >            1.23456E+12     6 DIGITS
C
C     OUTPUT WILL BE IN 11 COLUMN FORMAT
C
C     WARNING - THIS IS NOT A GENERAL ROUNDING ROUTINE WHICH WILL WORK
C               FOR ROUNDING TO ANY NUMBER OF DIGITS - IT WILL ONLY
C               WORK PROPERLY FOR THE RANGES INDICATED ABOVE, FOR
C               11 COLUMN OUTPUT.
C
C=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
c-----INTEGER*8 for long integers
c-----2017/5/3 - Add INNEXT = must be INTEGER*8
      integer*8 INMANT,INMAN10,INMAN9,INMAN7,INMAN6,INMAN5,INMAN4,INNEXT
c-----2017/5/3 - Add INNEXT = must be INTEGER*8
      CHARACTER*1 FIELD,DIGITS,ZEROH
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DIMENSION DIGITS(0:9),FIELD(11),ZEROH(11),
     1 TENS(-99:99),ROUNDER(-99:99)
      DATA DIGITS/
     1 '0','1','2','3','4','5','6','7','8','9'/
C-----RETURN FOR = 0
      DATA ZEROH/
     1 ' ','0','.','0',' ',' ',' ',' ',' ',' ',' '/
C-----LOWER TRANSITION POINT FROM 7 TO 9 DIGIT OUTPUT
      DATA ZLOW/1.0D-03/
C-----UPPER TRANSITION POINT FROM 9 TO 7 DIGIT OUTPUT
      DATA ZHIGH/1.0D+09/
      DATA TENTH/1.0D-01/
c-----------------------------------------------------------------------
c
c     ON FIRST CALL INITIALIZE POWERS OF 10
c
c-----------------------------------------------------------------------
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
c-----INTEGER*8 for long integers
      INMAN4  = 10000
      INMAN5  = 10*INMAN4
      INMAN6  = 10*INMAN5
      INMAN7  = 10*INMAN6
      INMAN9  = 100*INMAN7
      INMAN10 = 10*INMAN9  ! WARNING - this exceeds 32 bit integer
      TENS(0)=1.0D+00
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DO 10 I=1,99
      TENS( I)=TENS(I-1)*10.0D+00
      TENS(-I)=TENS(1-I)/10.0D+00
   10 ROUNDER(I)  = 5.001D-05
      DO 20 I=-99,-10
   20 ROUNDER(I)  = 5.001D-05
      DO 30 I=-9,-4
   30 ROUNDER(I)  = 5.001D-06
      ROUNDER(-3) = 5.001D-07
      ROUNDER(-2) = 5.001D-08
      ROUNDER(-1) = 5.001D-09
      ROUNDER( 0) = 5.001D-09
      DO 40 I=1,8
   40 ROUNDER(I)  = 5.001D-09
      ROUNDER(9)  = 5.001D-06
c-----------------------------------------------------------------------
C
C     IMMEDIATELY RETURN 0.00000+00.
C
c-----------------------------------------------------------------------
C-----02/14/04 - ADDED, JUST IN CASE
   50 IF(DABS(ZIN).le.0.0D+0) GO TO 60
      IF(DABS(ZIN).lt.1.0D-99) GO TO 60    ! Only 2 digit expoents
      IF(DABS(ZIN).gt.1.0D+99) GO TO 60
c----- ZIN = 0 handled above
      IF(ZIN.lt.0.0d+0) go to 80
      go to 90
   60 DO 70 I=1,11
   70 FIELD(I)=ZEROH(I)
      RETURN
c-----------------------------------------------------------------------
C
C     DEFINE SIGN OF MANTISSA AND ABSOLUTE MANTISSA
C
c-----------------------------------------------------------------------
c-----< 0 = use OUT9
   80 CALL OUT9(ZIN,FIELD)
      RETURN
C-----POSITIVE.
   90 FIELD(1)=' '
      Z=ZIN
c-----------------------------------------------------------------------
c
c     DEFINE EXPONENT AND NORMALIZED MANTISSA
c
c-----------------------------------------------------------------------
      IEXP=DLOG10(Z)
c-----11/26/2013 - Restrict to 2 digit exponent - may change by 1 or 2
      if(IABS(IEXP).gt.97) go to 60
      IF(Z.LT.1.0D+00) IEXP = IEXP - 1
c-----11/22/2013 - Decide here on F or E Output
      if(ZIN.lt.0.0D+0) then
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)       ! Standard
      else
      ZN=Z*TENS(-IEXP) + TENTH*ROUNDER(IEXP) ! Extra Digit
      endif
      IF(ZN.eq.1.0D+00) go to 110
      IF(ZN.gt.1.0D+00) go to 100
      IEXP=IEXP-1               ! MUST be < 1
      go to 110
  100 IF(ZN.lt.10.0D+00) go to 110
      IEXP=IEXP+1               ! MUST be >= 10
      ZN=ZN/10.0D+00
  110 Z = ZN*TENS(IEXP)
c-----------------------------------------------------------------------
C
C     SELECT F OR E FORMAT
C
c-----------------------------------------------------------------------
      IF(Z.LE.ZLOW.OR.Z.GE.ZHIGH) GO TO 140
c-----------------------------------------------------------------------
C
C     F FORMAT
C
C     12345678901
C     X.XXXXXXXXX = 10 DIGITS
C     .0012345678
C     1234567891.
C
c-----------------------------------------------------------------------
C-----DEFINE 7 TO 10 DIGIT MANTISSA WITH ROUNDING
      IPOWER=9-IEXP
      IF(IEXP.LT.0) IPOWER=9
      INMANT=Z*TENS(IPOWER)
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(INMANT.lt.INMAN10) go to 120
      INMANT=INMAN9
      IEXP=IEXP+1
C-----DECIMAL POINT.
  120 IDOT=2+IEXP
      IF(IDOT.LE.1) THEN
C----- IF < 1, MOVE DECIMAL POINT TO COLUMN 1 AND ADD A DIGIT
      IDOT=1
      INMANT=Z*10.0D+00*TENS(IPOWER)
      ENDIF
      FIELD(IDOT)='.'
C-----MANTISSA - LAST DIGIT TO FIRST.
      II=11
      DO 130 I=1,11
      IF(II.EQ.IDOT) GO TO 130
      INNEXT=INMANT/10
      I3=INMANT-10*INNEXT
      FIELD(II)=DIGITS(I3)
      INMANT=INNEXT
  130 II=II-1
c-----------------------------------------------------------------------
c
c     If 10 digit ends in 0, shift right = leave column 1 blank
c
c-----------------------------------------------------------------------
      if(FIELD(1).ne.'-'.and.FIELD(11).eq.'0') then
      do k=10,1,-1
      FIELD(k+1)=FIELD(k)
      enddo
      FIELD(1) = ' '
      endif
      RETURN
c-----------------------------------------------------------------------
C
C     E FORMAT
C
C     12345678901
C      X.XXXXE+NN = 5 DIGITS
C      X.XXXXXE+N = 6 DIGITS
c
c     11/22/2013 - If first column is blank use it.
C
C     12345678901
C     X.XXXXXE+NN = 6 DIGITS
C     X.XXXXXXE+N = 7 DIGITS
C
C==============================================================
C-----Negative?
  140 IF(ZIN.lt.0.0d+0) go to 160
c-----------------------------------------------------------------------
c
c     POsitive. Use first column - Decimal point is always in column 2
c
c-----------------------------------------------------------------------
      FIELD(2)='.'
      KDOT    = 2
      ISTART  = 1
      IF(IABS(IEXP).GE.10) GO TO 150
      ID=8                               ! 1 Digit exponent
      INMANT=(1.0D+06)*ZN
      IF(INMANT.lt.INMAN7) go to 180
      INMANT=INMAN6
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 180
  150 ID=7                               ! 2 Digit exponent
      INMANT=(1.0D+05)*ZN
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(INMANT.lt.INMAN6) go to 180
      INMANT=INMAN5
      IEXP=IEXP+1
c-----------------------------------------------------------------------
c
c     Negative Number - Cannot use first column
c
c-----------------------------------------------------------------------
C-----DECIMAL POINT IS ALWAYS IN COLUMN 3
  160 FIELD(3)='.'
      KDOT    = 3
      ISTART  = 2
      IF(IABS(IEXP).GE.10) GO TO 170
      ID=8                                 ! 1 Digit Exponent
      INMANT=(1.0D+05)*ZN
      IF(INMANT.lt.INMAN6) go to 180
      INMANT=INMAN5
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 180
  170 ID=7                                 ! 2 Digit Exponent
      INMANT=(1.0D+04)*ZN
      IF(INMANT.lt.INMAN5) go to 180
      INMANT=INMAN4
      IEXP=IEXP+1
c-----------------------------------------------------------------------
C
C     DEFINE MANTISSA
C
c-----------------------------------------------------------------------
  180 IEXPS=ID+1
      II=ID
      DO 190 I=ISTART,ID
      IF(II.EQ.KDOT) GO TO 190
      INNEXT=INMANT/10
      I3=INMANT-10*INNEXT
      FIELD(II)=DIGITS(I3)
      INMANT=INNEXT
  190 II=II-1
c-----------------------------------------------------------------------
C
C     E
C
c-----------------------------------------------------------------------
      FIELD(IEXPS) = 'E'
      IEXPS = IEXPS + 1
c-----------------------------------------------------------------------
C
C     SIGN OF EXPONENT
C
c-----------------------------------------------------------------------
      IF(IEXP.ge.0) go to 200
      IEXP=-IEXP
      FIELD(IEXPS)='-'
      GO TO 210
  200 FIELD(IEXPS)='+'
c-----------------------------------------------------------------------
C
C     EXPONENT
C
c-----------------------------------------------------------------------
  210 IF(IEXP.lt.10) go to 220
      KEXP=IEXP/10
      FIELD(10)=DIGITS(KEXP)
      IEXP=MOD(IEXP,10)
  220 FIELD(11)=DIGITS(IEXP)
c-----------------------------------------------------------------------
c
c     If using column 1 but mantissa ends in 0, move mantissa right.
c
c-----------------------------------------------------------------------
      if(KDOT.eq.2.and.FIELD(ID).eq.'0') then
      do k=ID-1,1,-1
      FIELD(k+1)=FIELD(k)
      enddo
      FIELD(1) = ' '
      endif
      RETURN
      END
      SUBROUTINE IN9(E,FIELD)
C=======================================================================
C
C     PURPOSE
C     =======
C     CONVERT FROM HOLLERITH TO FLOATING POINT.
C     12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENT (I Thank Viktor Zerkin)
C                  More than 3 digit exponent = ERROR - return 0.0
C     06/12/2013 - BACK tO 2 digit exponent - 3 digits caused
C                  problems on several different types of computers.
C
C     ARGUMENTS
C     =========
C     E       = FLOATING POINT NUMBER (OUTPUT)
C     FIELD   = 11A1 CHARACTER STRING (INPUT)
C
C     METHOD
C     ======
C     FIELD IS A STRING OF 11 CHARACTERS.
C     IT IS CONVERTED INTO A FLOATING POINR NUMBER (E)
C
C=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      CHARACTER*1 MESS,DIGIT,FIELD,IFIELD
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENTS
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DIMENSION FIELD(11),TENS(-99:99),DIGIT(0:9),MESS(11),XDIG(0:9)
      DATA MESS/' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9'/
      DATA XDIG/0.0D+00,1.0D+00,2.0D+00,3.0D+00,4.0D+00,
     1          5.0D+00,6.0D+00,7.0D+00,8.0D+00,9.0D+00/
C-----ON FIRST CALL DEFINE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 20
      IPASS=1
      TENS(0)=1.0D+00
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENTS
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      DO 10 I=1,99
      TENS( I)=TENS(I-1)*10.0D+00
   10 TENS(-I)=TENS(1-I)/10.0D+00
c-----------------------------------------------------------------------
C
C     TRANSLATE MANTISSA.
C
c-----------------------------------------------------------------------
C-----SKIP LEADING BLANK CHARACTERS.
   20 DO 30 I=1,11
      IF(FIELD(I).NE.' ') GO TO 40
   30 CONTINUE
C-----FIELD IS COMPLETELY BLANK. RETURN ZERO.
      E=0.0D+00
      RETURN
C-----INITIALIZE SIGN TO PLUS AND THEN CHECK FOR LEADING MINUS SIGN.
   40 SIGN=1.0D+00
C-----06/02/09 - ADDED TEST FOR LEADING + SIGN.
      IF(FIELD(I).NE.'-') GO TO 50
      SIGN=-1.0D+00
      I=I+1
c-----added leading +
      GO TO 60
   50 IF(FIELD(I).EQ.'+') I = I + 1
C-----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT.
   60 X9IN=0.0d0
      IPT=-20
      IMZERO=0
C-----SCAN REMAINDER OF MANTISSA.
      DO 120 J=I,11
      IFIELD=FIELD(J)
C-----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).
      DO 70 K=0,9
      IF(IFIELD.EQ.DIGIT(K)) GO TO 90
   70 CONTINUE
      IF(IFIELD.NE.'.') GO TO 80
      IPT=0
      GO TO 120
C-----SCAN FOR BLANK (WHICH ENDS MANTISSA).
   80 IF(IFIELD.EQ.' ') GO TO 130
C-----SCAN FOR e, E, d, D, - OR + (WHICH BEGINS EXPONENT).
      IF(IFIELD.EQ.'e'.OR.IFIELD.EQ.'E') GO TO 160
      IF(IFIELD.EQ.'d'.OR.IFIELD.EQ.'D') GO TO 160
      IF(IFIELD.EQ.'-') GO TO 190
      IF(IFIELD.EQ.'+') GO TO 170
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 240
C-----DIGIT FOUND. SAVE TRAILING ZEROES AFTER DECIMAL POINT.
   90 IF(IPT.LT.0) GO TO 110
      IF(K.NE.0) GO TO 100
C-----SAVE TRAILING ZEROES.
      IMZERO=IMZERO+1
      GO TO 120
  100 IF(IMZERO.LE.0) GO TO 110
C-----INSERT ZEROES BEFORE NEXT NUMBER.
      X9IN=10.0D+00*X9IN
      IPT=IPT+1
      IMZERO=IMZERO-1
      GO TO 100
C-----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT
C-----OFFSET.
  110 X9IN=10.0D+00*X9IN+XDIG(K)
      IPT=IPT+1
  120 CONTINUE
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
      GO TO 150
C-----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR
C-----EXPONENT.
  130 I=J+1
      IF(I.GT.11) GO TO 150
      DO 140 J=I,11
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.' ') GO TO 140
      IF(IFIELD.EQ.'e'.OR.IFIELD.EQ.'E') GO TO 160
      IF(IFIELD.EQ.'d'.OR.IFIELD.EQ.'D') GO TO 160
      IF(IFIELD.EQ.'-') GO TO 190
      IF(IFIELD.EQ.'+') GO TO 170
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 240
  140 CONTINUE
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
  150 E=X9IN
      IF(IPT.GT.0) E=E/TENS(IPT)
      E=SIGN*E
      RETURN
c-----------------------------------------------------------------------
C
C     TRANSLATE EXPONENT.
C
c-----------------------------------------------------------------------
C-----BEGINNING OF EXPONENT FOUND (E OR D). CHECK FOR FOLLOWING - OR +.
  160 J=J+1
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.'-') GO TO 190
      IF(IFIELD.NE.'+') GO TO 180
C----- + FOUND. INITIALIZE EXPONENT SIGN.
  170 J=J+1
  180 ISIGN=1
      GO TO 200
C----- - FOUND. INITIALIZE EXPONENT SIGN.
  190 J=J+1
      ISIGN=-1
C-----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.
  200 IEXP=0
      DO 230 I=J,11
      IFIELD=FIELD(I)
      IF(IFIELD.EQ.' ') GO TO 230
      DO 210 K=0,9
      IF(IFIELD.EQ.DIGIT(K)) GO TO 220
  210 CONTINUE
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 240
C-----DIGIT FOUND. INCREMENT EXPONENT.
C-----OFFSET.
  220 IEXP=10*IEXP+K
  230 CONTINUE
C-----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING
C-----POINT.
      E=X9IN
      IEXP=ISIGN*IEXP
      IF(IPT.GT.0) IEXP=IEXP-IPT
C-----12/18/2012 - EXTENDED FOR 3 DIGIT EXPONENTS
C-----06/12/2013 - BACK tO 2 digit exponent - 3 digits caused problems
      IF(iabs(IEXP).GT.99) GO TO 240
      E=SIGN*E*TENS(IEXP)
      RETURN
c-----------------------------------------------------------------------
C
C     ERROR CONDITIONS.
C
c-----------------------------------------------------------------------
C-----ILLEGAL CHARACTER.
  240 MESS(J)='*'
      write(*,250) FIELD,MESS
  250 FORMAT(1X,11A1/1X,11A1/' ERROR in Input Data...Translated as 0.')
      E=0.0D+00
      MESS(J)=' '
      RETURN
      END
      subroutine INT11(iout,field)
c=======================================================================
c
c     Integer output, right adjusted for 11 columns
c    ----------------------------------------------
c
C     arguments
C     =========
C     iout    = fixed point number (input)
C     field   = 11A1 character string (output)
C
c=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      character*1 field(11),digits(0:9)
      data digits/'0','1','2','3','4','5','6','7','8','9'/
c-----initialize blank
      do i=1,11
      field(i) = ' '
      enddo
c-----return = 0
      if(iabs(iout).le.0) then
      field(11) = '0'
      go to 20
      endif
c-----translate last to first digit
      ii = iabs(iout)
      do id=11,1,-1
      kk = mod(ii,10)
      field(id) = digits(kk)
      ii = ii/10
      if(ii.le.0) go to 10
      enddo
      id = 2
c-----if < 0 add minus sign
   10 if(iout.lt.0) then
      id = id - 1
      field(id) = '-'
      endif
   20 return
      end
      subroutine maxie0
c=======================================================================
c
c     Check for same upper energy limits of ALL data in any evaluation
c     ----------------------------------------------------------------
c     Entry points
c     ------------
c     maxie0 = set maxie = 0
c     maxie1 = increment maxie and save mat,mf,mt
c     maxie2 = save (x,y) upper energy limit
c     maxie3 = check and print WARNING for any inconsistency
c     maxie4 = print final, detailed  WARNING for any inconsistency
c
c     itty/jtty = 0 = only print to file messages
c               = 1 = also print on-line messages
c
c=======================================================================
      implicit real*8 (a-h,o-z)
      save
      character*1 field11(11,2)
      common/maxiecom/xmaxie(999),ymaxie(999),matmaxie(999),
     1 mfmaxie(999),mtmaxie(999)
      common/emaxcom/xmaximum
      data maxie /0/         ! # of MTs
      data mypass/0/         ! Initialize not called
      data mygoof/0/         ! ERROR flag for final WARNING
      data xminimum /0.0d0/  ! Lowest for current MAT
      data xsmallest/0.0d0/  ! Lowest for entire run with multiple MATS
c-----------------------------------------------------------------------
c
c     maxio0 = set maxie = 0
c
c-----------------------------------------------------------------------
      mypass = 1
      maxie  = 0
      xmaximum = 0.0d0
      xminimum = 0.0d0
      return
      entry maxie1(matm,mfm,mtm)
c=======================================================================
c
c     maxin1 = increment maxie and save mat,mf,mt
c
c=======================================================================
      if(mypass.le.0) then   ! Protect against NO CALLS
      mypass = 1
      maxie  = 0
      xmaximum = 0.0d0
      xminimum = 0.0d0
      endif
      if(maxie.lt.999) maxie = maxie + 1 ! Do not exceed table sze
      matmaxie(maxie) = matm
      mfmaxie (maxie) = mfm
      mtmaxie (maxie) = mtm
      xmaxie  (maxie) = 0.0d0
      ymaxie  (maxie) = 0.0d0
      return
      entry maxie2(xm,ym)
c=======================================================================
c
c     maxin2 = save (x,y) upper energy limit
c
c=======================================================================
c-----2019/6/30 - Ignore xm = 0 values
      if(xm.le.0.0d0) return
      if(mypass.le.0) then   ! Protect against NO CALLS
      mypass = 1
      maxie  = 1
      xmaximum = 0.0d0
      xminimum = 0.0d0
      matmaxie(maxie) = 0
      mfmaxie (maxie) = 0
      mtmaxie (maxie) = 0
      endif
      xmaxie(maxie) = xm
      ymaxie(maxie) = ym
c-----------------------------------------------------------------------
c
c     Add exception list here = data we know may not extend to maximum.
c     To ignore just set value = 0.0
c
c-----------------------------------------------------------------------
c-----ignore form factors and scattering functions
      if(mfmaxie(maxie).eq.27) ymaxie(maxie) = 0.0d0
      return
      entry maxie3(itty)
c=======================================================================
c
c     maxin3 = check and print WARNING for any inconsistency
c
c=======================================================================
      if(mypass.le.0) return
c-----define xmaximum = Maximum tabulated Energy
c-----       xminimum   = Minimum with positive cross section
      if(maxie.le.0) return
      xmaximum = 0.0d0
      xminimum = 0.0d0
      do imt=1,maxie
      if(xmaxie(imt).gt.xmaximum) xmaximum = xmaxie(imt)
      if(ymaxie(imt).gt.0.0d0) then
      if(xminimum.le.0.0d0) then
      xminimum = xmaxie(imt)
      else
      if(xmaxie(imt).lt.xminimum) xminimum = xmaxie(imt)
      endif
      endif
      enddo
c-----If ALL MT ends in Y = 0, use highest tabulated E
      if(xminimum.le.0.0d0) then
      do imt=1,maxie
      if(xmaxie(imt).gt.xminimum) xminimum = xmaxie(imt)
      enddo
      endif
c-----------------------------------------------------------------------
c
c     Nothing if xminimum or xmaximum is still not set
c
c-----------------------------------------------------------------------
      if(xminimum.le.0.0d0) return
      if(xmaximum.le.0.0d0) return
c-----------------------------------------------------------------------
c
c     Print Summary
c
c-----------------------------------------------------------------------
      call out9(xmaximum,field11(1,2))
                    write(3,10) (field11(i,2),i=1,11)
      if(itty.gt.0) write(*,10) (field11(i,2),i=1,11)
   10 format(1x,78('-')/' All Cross Sections Read. Maximum Tabulated',
     1 ' Energy = ',11A1,' eV'/1x,78('-'))
c-----------------------------------------------------------------------
c
c     WARNING if ALL data is not consistent
c
c-----------------------------------------------------------------------
      if(xminimum.ge.xmaximum) return
      mygoof = 1
                    write(3,20)
      if(itty.gt.0) write(*,20)
   20 format(' WARNING - The Following MTs Stop below the Maximum',
     1 ' Tabulated Energy'/1x,78('-')/'  MAT MF  MT  This MT   ',
     2 '  Maximum E'/1x,78('-'))
      do imt=1,maxie
      if(ymaxie(imt).gt.0.0d0) then
      if(xmaxie(imt).lt.xmaximum) then
      call out9(xmaxie(imt),field11(1,1))
       write(3,30) matmaxie(imt),mfmaxie(imt),mtmaxie(imt),field11
      if(itty.gt.0)
     1 write(*,30) matmaxie(imt),mfmaxie(imt),mtmaxie(imt),field11
   30 format(i5,i3,i4,1x,11a1,1x,11a1)
      endif
      endif
      enddo
      call out9(xminimum,field11(1,1))
                    write(3,40) field11
      if(itty.gt.0) write(*,40) field11
   40 format(1x,78('-')/' WARNING - Data between',11a1,' and ',11a1,
     1 ' are NOT Reliable'/1x,78('-'))
c-----Cumulative low E for ALL MATs
      if(xsmallest.le.0.0d0) then
      xsmallest = xminimum
      else
      if(xminimum.lt.xsmallest) xsmallest = xminimum
      endif
      return
      entry maxie4(jtty)
c=======================================================================
c
c     maxin4 = print final, detailed WARNING for any inconsistency
c
c=======================================================================
c
c     No WARNING if routine was never called or no Inconsistency.
c
c-----------------------------------------------------------------------
      if(mypass.le.0) return        ! Never called
      if(mygoof.le.0) return        ! No Inconsistency
      if(xsmallest.le.0.0d0) return ! No smallest define
c-----------------------------------------------------------------------
c
c     Otherwise WARNING.
c
c-----------------------------------------------------------------------
      call out9(xsmallest,field11(1,1))
                    write(3,50) (field11(i,1),i=1,11)
      if(jtty.gt.0) write(*,50) (field11(i,1),i=1,11)
   50 format(1x,78('-')/
     1 ' WARNING - The ENDF Output is NOT Relialble above',11a1,' eV,'/
     1 '           because it is NOT Consistently Defined for ALL MTs.'/
     1 '           It is STRONGLY recommended that you NOT use this'/
     2 '           data above this energy. If you need data up to'/
     3 '           energies higher than this PLEASE correct the data'/
     4 '           and only then try re-running this code.'/
     5 1x,78('-'))
      return
      end
      subroutine ZANAME(IZA,NAME1)
c=======================================================================
c
c     Define ZA in character form = zAzzzaaa (8 characters)
c
c=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE,OTAPE2
      CHARACTER*1  NAME1,digits
      dimension NAME1(20),digits(0:9)
      data digits/'0','1','2','3','4','5','6','7','8','9'/
c-----------------------------------------------------------------------
c
c     Define ACE ENDF Filename = ZAzzzaaa.URR.ENDF
c
c-----------------------------------------------------------------------
      NAME1(1) = 'Z'
      NAME1(2) = 'A'
      kkza = IZA
      do kk=8,3,-1
      idigit = mod(kkza,10)
      NAME1(kk) = digits(idigit)
      kkza = kkza/10
      enddo
      return
      end
      SUBROUTINE HIGHEND(X,Y,NBT,INT,N1,N2)
c=======================================================================
c
c     2019/6/30 - Insure Spectra ends with Y = 0
c
c     Histogram for n GROUPS = (E1,Y1),(E2,Y2)....(En,Yn)(En+1,0)
c     ERROR if LAST is (En+1,Y?) - set Y? = 0
c
c=======================================================================
      INCLUDE 'implicit.h'
      DIMENSION X(*),Y(*),NBT(*),INT(*)
c-----Nothing to do if last Y = 0
      IF(Y(N2).eq.0.0d0) RETURN
      IF(INT(N1).ne.1) go to 10
c-----------------------------------------------------------------------
c
c     Histogram - Insure last Y = 0
c
c-----------------------------------------------------------------------
      if(X(N2).ne.X(N2-1)) then          ! 2 points for group?
      Y(N2) = 0.0d0                      ! yes, Set end of step Y = 0
      ELSE
      N2       = N2 - 1                  ! no, delete repeated point
      Y(N2)    = 0.0d0
      NBT(N1)  = N2
      ENDIF
      RETURN
c-----------------------------------------------------------------------
c
c     Non-Histogram - Repeat Last energy with Y = 0
c
c-----------------------------------------------------------------------
   10 N2       = N2 + 1
      X(N2)    = X(N2-1)
      Y(N2)    = 0.0d0
      NBT(N1)  = N2
      RETURN
      END
