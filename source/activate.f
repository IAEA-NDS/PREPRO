
C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


C=======================================================================
C
C     PROGRAM ACTIVATE
C     ================
C     VERS. 2000-1 (APRIL 2000)   *INITIAL VERSION.
C     VERS. 2002-1 (MAY 2002)     *OPTIONAL INPUT PARAMETERS
C     VERS. 2004-1 (JAN. 2004)    *CORRECTED ERROR - FIRST RECORD AFTER
C                                  MF=10 WAS MISSING.
C                                 *ADDED INCLUDE TO DEFINE COMMON
C                                 *INCREASED MAX. POINTS FROM 100,000
C                                  TO 1,000,000.
C     VERS. 2007-1 (JAN. 2007)    *CHECKED AGAINST ALL ENDF/B-VII
C     VERS. 2007-2 (DEC. 2007)    *72 CHARACTER FILE NAMES.
C     VERS. 2010-1 (Apr. 2010)    *General update based on user feedback
C     VERS. 2012-1 (Aug. 2012)    *Added CODENAME
C                                 *Added ERROR stop
C                                 *32 and 64 bit Compatible
C     VERS. 2015-1 (Jan. 2015)    *Corrected ERROR for missing or extra
C                                  SEND and MEND lines.
C                                 *Changed MF=8 pointer from MF=9 to 10.
C                                 *INCREASED MAX. POINTS to 3,000,000.
C                                 *Added Consistency checks, e.g.,
C                                  Any MT in MF=9 requires data in MF=3.
C                                 *Extended OUT9 - OUT10 is not used.
C                                 *Only processes ONE ENDF Tape - this
C                                  restriction is necessary to insure
C                                  compatibility with ALL PREPRO codes.
C                                 *Changed to current ENDF sequence
C                                  number convention, e.g., reset number
C                                  for each section (MAT/MF/MT).
C                                 *Replaced ALL 3 way IF statements.
C     VERS. 2017-1 (May  2017)    *Increased MAX. POINTS to 6,000,000.
C                                 *Do not create MF=10 for any MT that
C                                  already has MF=10 data = copy MF=10
C                                  data in its original form.
C                                 *Message for every MF=7 output,
C                                  whether created or copied from input.
C     VERS. 2018-1 (Jan. 2018)    *Updated based on user feedback.
C                                 *Added on-line output for ALL ENDERROR
C     VERS. 2020-1 (Mar. 2020)    *Additional Interpolation Law Tesrs
C                                 *Checked consistency of Maximum
C                                  tabulated energy for MF=3 and 9 data
C                                  to be compbined - print WARNING if
C                                  inconsistent.
C                                 *Added Target Isomer State
C     VERS. 2021-1 (Jan. 2021)    *Updated for FORTRAN 2018
C
C     Acknowledgement 2015
C     --------------------
C     Currently almost all improvements to this code are based upon
C     feedback from code users who report problems. This feedback
C     benefits ALL users of this code, and ALL users are encouraged
C     to report problems.
C
C     Improvements on the 2015 version of this code based on user
C     feedback, including IMPORTANT feedback from Andrej Trkov, up
C     to and including Feb. 2015.
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
C
C     AUTHORS MESSAGE
C     ---------------
C     THE REPORT DESCRIBED ABOVE IS THE LATEST PUBLISHED DOCUMENTATION
C     FOR THIS PROGRAM. HOWEVER, THE COMMENTS BELOW SHOULD BE CONSIDERED
C     THE LATEST DOCUMENTATION INCLUDING ALL RECENT IMPROVEMENTS. PLEASE
C     READ ALL OF THESE COMMENTS BEFORE IMPLEMENTATION.
C
C     AT THE PRESENT TIME WE ARE ATTEMPTING TO DEVELOP A SET OF COMPUTER
C     INDEPENDENT PROGRAMS THAT CAN EASILY BE IMPLEMENTED ON ANY ONE
C     OF A WIDE VARIETY OF COMPUTERS. IN ORDER TO ASSIST IN THIS PROJECT
C     IT WOULD BE APPECIATED IF YOU WOULD NOTIFY THE AUTHOR OF ANY
C     COMPILER DIAGNOSTICS, OPERATING PROBLEMS OR SUGGESTIONS ON HOW TO
C     IMPROVE THIS PROGRAM. HOPEFULLY, IN THIS WAY FUTURE VERSIONS OF
C     THIS PROGRAM WILL BE COMPLETELY COMPATIBLE FOR USE ON YOUR
C     COMPUTER.
C
C     PURPOSE
C     -------
C     THIS PROGRAM IS DESIGNED TO CREATE FILE 10 ACTIVATION CROSS
C     SECTIONS BY COMBINING FILE 3 CROSS SECTIONS AND FILE 9 MULTIPLIERS
C
C     IN THE FOLLOWING DISCUSSION FOR SIMPLICITY THE ENDF TERMINOLOGY
C     ---ENDF TAPE---WILL BE USED. IN FACT THE ACTUAL MEDIUM MAY BE
C     TAPE, CARDS, DISK OR ANY OTHER MEDIUM.
C
C     ASSUMPTIONS
C     -----------
C     IT IS ASSUMED THAT THE FILE 3 AND 9 DATA HAVE BEEN LINEARIZED
C     BEFORE THIS CODE IS USED - FILE 3 AND 9 DATA CAN BE LINEARIZED
C     USING PROGRAM LINEAR.
C
C     IT IS ASSUMED THAT THE FILE 9 MULTIPLIERS ARE FAIRLY SMOOTH VERSUS
C     ENERGY, AND THAT THE ACTIVATION CROSS SECTIONS FOR FILE 10 CAN BE
C     DEFINED AT EXACTLY THE SAME ENERGIES AS THE FILE 3 CROSS SECTIONS,
C     AND THAT THESE NEED MERELY BE MULTIPLIED BY THE FILE 9 TO DEFINE
C     THE FILE 10 ACTIVATION CROSS SECTIONS.
C
C     ENDF FORMAT
C     -----------
C     THIS PROGRAM ONLY USES THE ENDF BCD OR CARD IMAGE FORMAT (AS
C     OPPOSED TO THE BINARY FORMAT) AND CAN HANDLE DATA IN ANY VERSION
C     OF THE ENDF FORMAT (I.E., ENDF-1, 2, 3, 4, 5 OR 6 FORMAT).
C
C     IT IS ASSUMED THAT THE DATA IS CORRECTLY CODED IN THE ENDF
C     FORMAT AND NO ERROR CHECKING IS PERFORMED. IN PARTICULAR IT IS
C     ASSUMED THAT THE MAT, MF AND MT ON EACH LINE IS CORRECT. SEQUENCE
C     NUMBERS (COLUMNS 76-80) ARE IGNORED ON INPUT, BUT WILL BE
C     CORRECTLY OUTPUT ON ALL LINES. THE FORMAT OF SECTION MF=1, MT=451
C     AND ALL SECTIONS OF MF=3 MUST BE CORRECT. THE PROGRAM COPIES ALL
C     OTHER SECTION OF DATA AS HOLLERITH AND AS SUCH IS INSENSITIVE TO
C     THE CORRECTNESS OR INCORRECTNESS OF ALL OTHER SECTIONS.
C
C     OUTPUT FORMAT
C     -------------
C     ALL ENERGIES WILL BE OUTPUT IN F (INSTEAD OF E) FORMAT IN ORDER
C     TO ALLOW ENERGIES TO BE WRITTEN WITH UP TO 9 DIGITS OF ACCURACY.
C     COMPARISON OF THE NORMAL ENDF CONVENTION OF 6 DIGITS TO THE 9
C     DIGIT OUTPUT FROM THIS PROGRAM DEMONSTRATED THAT FAILURE TO USE
C     THE 9 DIGIT OUTPUT CAN LEAD TO LARGE ERRORS IN THE DATA DUE TO
C     TRUNCATION OF ENERGIES TO 6 DIGITS DURING OUTPUT.
C
C     CONTENTS OF OUTPUT
C     ------------------
C     ENTIRE EVALUATIONS ARE OUTPUT, NOT JUST THE PROCESSED DATA, E.G.,
C     ANGULAR AND ENERGY DISTRIBUTIONS ARE ALSO INCLUDED.
C
C     DOCUMENTATION
C     -------------
C     THE FACT THAT THIS PROGRAM HAS OPERATED ON THE DATA IS DOCUMENTED
C     BY THE ADDITION OF 3 COMMENT LINES AT THE END OF EACH HOLLERITH
C     SECTION IN THE FORM
C
C     ***************** PROGRAM ACTIVATE (2021-1) ****************
C     FILE 10 ACTIVATION CROSS SECTIONS HAVE BEEN DEFINED BY COMBINING
C     FILE 3 CROSS SECTIONS AND FILE 9 MULTIPLIERS. FILE 9 DELETED.
C
C     THE ORDER OF SIMILAR COMMENTS (FROM RECENT, SIGMA1 AND GROUPIE)
C     REPRESENTS A COMPLETE HISTORY OF ALL OPERATIONS PERFORMED ON
C     THE DATA BY THESE PROGRAMS.
C
C     THESE COMMENT LINES ARE ONLY ADDED TO EXISTING HOLLERITH SECTIONS,
C     I.E., THIS PROGRAM WILL NOT CREATE A HOLLERITH SECTION. THE FORMAT
C     OF THE HOLLERITH SECTION IN ENDF-5 DIFFERS FROM THE THAT OF
C     EARLIER VERSIONS OF ENDF. BY READING AN EXISTING MF=1, MT=451
C     IT IS POSSIBLE FOR THIS PROGRAM TO DETERMINE WHICH VERSION OF
C     THE ENDF FORMAT THE DATA IS IN. WITHOUT HAVING A SECTION OF
C     MF=1, MT=451 PRESENT IT IS IMPOSSIBLE FOR THIS PROGRAM TO
C     DETERMINE WHICH VERSION OF THE ENDF FORMAT THE DATA IS IN, AND
C     AS SUCH IT IS IMPOSSIBLE FOR THE PROGRAM TO DETERMINE WHAT FORMAT
C     SHOULD BE USED TO CREATE A HOLLERITH SECTION.
C
C     REACTION INDEX
C     --------------
C     THIS PROGRAM DOES NOT USE THE REACTION INDEX WHICH IS GIVEN IN
C     SECTION MF=1, MT=451 OF EACH EVALUATION.
C
C     THIS PROGRAM DOES NOT UPDATE THE REACTION INDEX IN MF=1, MT=451.
C     THIS CONVENTION HAS BEEN ADOPTED BECAUSE MOST USERS DO NOT
C     REQUIRE A CORRECT REACTION INDEX FOR THEIR APPLICATIONS AND IT WAS
C     NOT CONSIDERED WORTHWHILE TO INCLUDE THE OVERHEAD OF CONSTRUCTING
C     A CORRECT REACTION INDEX IN THIS PROGRAM. HOWEVER, IF YOU REQUIRE
C     A REACTION INDEX FOR YOUR APPLICATIONS, AFTER RUNNING THIS PROGRAM
C     YOU MAY USE PROGRAM DICTIN TO CREATE A CORRECT REACTION INDEX.
C
C     SECTION SIZE
C     ------------
C     SECTIONS OF MF=9 MULTIPLIERS ARE LIMITED TO A MAXIMUM OF 3,000,000
C     ENERGY POINTS.
C
C     THERE IS NO LIMIT ON THE NUMBER OF ENERGY POINTS IN MF=3 AND 10
C     TABLES = THIS DATA IS READ AS CHARACTERS, ONE LINE AT A TIME.
C
C     SELECTION OF DATA
C     -----------------
C     THE PROGRAM PROCESSES ALL ENDF DATA ON ONE ENDF TAPE.
C
C     2015 - IT NOW ONLY DOES ONE ENDF TAPE.
C
C     PROGRAM OPERATION
C     -----------------
C     PASS #1
C     -------
C     THE ENTIRE MAT IS COPIED TO A SCRATCH FILE IN THE ENDF ASCII
C     FORMAT AND WHILE COPYING IT TO SCRATCH MF=3, 9, AND 10 ARE ALSO
C     COPIED TO SEPERATE SCRATCH FILES, I.E., THERE ARE A TOTAL OF 4
C     SCRATCH FILES - SEE THEIR DEFINITIONS BELOW.
C
C     PASS #2
C     -------
C     IF NO MF=9 MULTIPLIERS ARE FOUND DURING PASS #1, THE ENTIRE MAT
C     IS COPIED FROM SCRATCH TO THE OUTPUT FILE, WITHOUT ANY CHECKS.
C
C     IF MF=9 MULTIPLIERS ARE FOUND THEY ARE USED WITH MF=3 CROSS
C     SECTIONS TO CREATE MF=10 ACTIVATION CROSS SECTIONS.
C
C     FOR ANY SECTION OF MF=10 DATA FOR WHICH NO MF=9 MULTIPLIERS ARE
C     FOUND, THE ORIGINAL MF=10 IS OUTPUT.
C
C     FOR CONSISTENCY ALL MF=9 MULTIPLIERS ARE DELETED, I.E., THEY ARE
C     NOT INCLUDED IN THE OUTPUT.
C
C     KEEP EVALUATED DATA POINTS
C     --------------------------
C     THE FILE 10 OUTPUT WILL BE AT EXACTLY THE SAME ENERGY POINTS AS
C     THE FILE 3 CROSS SECTIONS USED TO DEFINE THE FILE 10 ACTIVATION
C     CROSS SECTIONS.
C
C     INPUT FILES
C     -----------
C     UNIT  DESCRIPTION
C     ----  -----------
C        2  INPUT LINES (BCD - 80 CHARACTERS/RECORD)
C       10  ORIGINAL ENDF DATA (BCD - 80 CHARACTERS/RECORD)
C
C     OUTPUT FILES
C     ------------
C     UNIT  DESCRIPTION
C     ----  -----------
C        3  OUTPUT REPORT (BCD - 120 CHARACTERS/RECORD)
C       11  FINAL ENDF DATA (BCD - 80 CHARACTERS/RECORD)
C
C     SCRATCH FILES
C     -------------
C     UNIT  DESCRIPTION
C     ----  -----------
C       12  SCRATCH FILE FOR ALL MAT (BCD - 80 CHARACTERS/RECORD)
C       14  SCRATCH FILE FOR MF=3 DATA (BCD - 80 CHARACTERS/RECORD)
C       15  SCRATCH FILE FOR MF=9 DATA (BCD - 80 CHARACTERS/RECORD)
C       16  SCRATCH FILE FOR MF=10 DATA (BCD - 80 CHARACTERS/RECORD)
C
C     OPTIONAL STANDARD FILE NAMES (SEE SUBROUTINE FILEIO)
C     ----------------------------------------------------
C     UNIT  FILE NAME
C     ----  ----------
C       2   ACTIVATE.INP
C       3   ACTIVATE.LST
C      10   ENDFB.IN
C      11   ENDFB.OUT
C      12   (SCRATCH)
C      14   (SCRATCH)
C      15   (SCRATCH)
C
C     INPUT PARAMETERS
C     ----------------
C
C     LINE  COLS.  DESCRIPTION
C     ----  -----  -----------
C        1   1-72  ENDF INPUT DATA FILENAME
C                  (STANDARD OPTION = ENDFB.IN)
C        2   1-72  ENDF OUTPUT DATA FILENAME
C                  (STANDARD OPTION = ENDFB.OUT)
C
C     ONE PAIR OF INPUT LINES MAY BE USED, TO PROCESS ANY ENDF TAPE.
C
C     2015 - NOW ONLY DOES ONE ENDF TAPE.
C
C     EXAMPLE INPUT NO. 1
C     -------------------
C     PROCESS ENDF TAPE NAMED ACTIVATE.IN AND NAME THE OUTPUT FILE
C     ACTIVATE.OUT.
C
C     IN THIS CASE THE FOLLOWING 2 INPUT LINES ARE REQUIRED
C
C ACTIVATE.IN
C ACTIVATE.OUT
C
C     EXAMPLE INPUT NO. 2
C     -------------------
C     SAME AS THE ABOVE CASE, EXCEPT THAT IN THIS CASE THE ORIGINAL
C     TAPE IS IN A DIRECTORY NAMED \ENDFB6\ORIGINAL, AND THE
C     RESULTS WILL BE WRITTEN INTO A DIRECTORY NAMED \ENDFB6\ACTIVATE.
C
C     IN THIS CASE THE FOLLOWING 6 INPUT LINES ARE REQUIRED
C
C \ENDFB6\ORIGINAL\ACTIVATE.IN
C \ENDFB6\ACTIVATE\ACTIVATE.OUT
C
C     EXAMPLE INPUT NO. 3
C     -------------------
C     IF THERE IS NO ACTIVATE.INP FILE, OR THE FILENAMES ARE BLANK
C     THIS CODE WILL USE THE DEFAULT NAMES,
C
C ENDFB.IN
C ENDFB.OUT
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'activate.h'
      INTEGER OUTP,OTAPE
      CHARACTER*1 ZABCD
      CHARACTER*1 LINE66
      CHARACTER*4 CARD
C-----08/08/2012 DEFINE CODE NAME
      CHARACTER*8 CODENAME
      COMMON/NAMECODE/CODENAME
      COMMON/BCDZA/ZABCD(12)
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
      COMMON/COPI/MFIELD(3)
      COMMON/COUNTS/IZANOW,MATNOW,N2MATI,N2MATO,N2TOTI,N2TOTO
      COMMON/LISOCOM/LISO
      COMMON/LSTCOM/LSTIZA,LSTMAT,LSTMF
      COMMON/CLINE66/LINE66(66)
c-----2014/2/20 - if MF=8, change flag MF=9 to MF=10
      character*1 mt9(11),mt10(11)
      data mt9 /' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','9'/
      data mt10/' ',' ',' ',' ',' ',' ',' ',' ',' ','1','0'/
C-----08/08/2012 DEFINE CODE NAME
      CODENAME = 'ACTIVATE'
C-----INITIALIZE TIMER
      CALL TIMER
C-----------------------------------------------------------------------
C
C     DEFINE ALL I/O UNITS AND OPTIONALLY DEFINE FILE NAMES.
C
C-----------------------------------------------------------------------
      CALL FILEIO
C-----SAVE ENDF INPUT FILE NUMBER
      ISAVE = ITAPE
C-----WRITE OUTPUT TITLE.
      WRITE(OUTP,250)
      WRITE(*   ,250)
C-----------------------------------------------------------------------
C
C     READ INPUT PARAMETERS = filenames of ENDF input and output data.
C
C-----------------------------------------------------------------------
      CALL READIN
C-----------------------------------------------------------------------
C
C     COPY TAPE LABEL AND INITIALIZE CUMULATIVE COUNTS.
C
C-----------------------------------------------------------------------
C-----COPY TAPE LABEL RECORD TO LINEARIZED FILE.
      CALL COPYL
C-----LIST TAPE LABEL.
      WRITE(OUTP,230) CARD,MFIELD(1)
      WRITE(*   ,230) CARD,MFIELD(1)
C-----INITIALIZE TOTAL POINTS READ AND WRITTEN.
      N2TOTI=0
      N2TOTO=0
C-----INITIALIZE LAST MAT READ.
      MATNOW = 0
      LSTMAT = -9999
      LSTMF  = -99
      IMALL10= 0     ! Flag to indicate MF=10 data already exists
      LISO    = 0
C-----------------------------------------------------------------------
C
C     LOOP OVER MATS
C     ==============
C     PASS #1: COPY NEXT MAT TO SCRATCH, AND DETERMINE IF THERE ARE ANY
C     MF = 9 MULTIPLIERS.
C
C-----------------------------------------------------------------------
C-----INITIALIZE MT TABLE AND MF=9 SECTION AND POINT COUNTS.
   10 IMFILE9  = 0
      N2MATI   = 0
      N2MATO   = 0
      LISO     = 0
      DO K=1,MAXMT
      MTTAB3(K)  = 0
      MTTAB9(K)  = 0
      MTTAB10(K) = 0
      ENDDO
C-----COPY ENTIRE MAT.
      REWIND ISCR1
      ITAPE = ISAVE
      LISIN = 0   ! MF/MT line counter
      LISO  = 0   ! Target isomer flag
C-----------------------------------------------------------------------
c
c     Next line is start of next MAT or TEND.
c
C-----------------------------------------------------------------------
      READ(ITAPE,210) LINE66,MATH,MFH,MTH
      IF(MATH.LT.0) GO TO 190                   ! TEND if MATH < 0
c-----translate ZA and save MAT
      MATNOW = MATH
      LISO   = 0
      LISIN  = 0
      call IN9(ZAX,LINE66(1))
      IZANOW = ZAX
      MATNOW = MATH
c-----2020/3/21 - Added LISO = Target State  Number
      CALL ZAHOLM(IZANOW,LISO,ZABCD)
c-----2019/3/9 - Initialize MT table for next MAT
      CALL MAXIE0
C-----------------------------------------------------------------------
C
C     Not TEND - assume next MAT - COPY TO SCRATCH.
C
C-----------------------------------------------------------------------
C
C     KEEP TRACK OF MT # FROM MF=3, 9, AND 10.
C     Switch MF=8 pointers from MF=9 to MF=10.
C
C-----------------------------------------------------------------------
   20 IF(MTH.LE.0) GO TO 30
C-----MF=3?
      IF(MFH.EQ.3) THEN
      MTTAB3(MTH) = 1
      ENDIF
C-----MF=9?
      IF(MFH.EQ.9) THEN
      IMFILE9 = IMFILE9 + 1
      MTTAB9(MTH) = 1
      ENDIF
C-----MF=10?
      IF(MFH.EQ.10) THEN
      MTTAB10(MTH) = 1
      ENDIF
C-----MF=8? Change flag from MF=9 to MF=10.
      IF(MFH.EQ.8) THEN
c-----If L1H = 9, Replace by L1H = 10
      do i=1,11
      if(LINE66(i+22).ne.mt9(i)) go to 30
      enddo
      do i=1,11
      LINE66(i+22) = mt10(i)
      enddo
      ENDIF
   30 WRITE(ISCR1,210) LINE66,MATH,MFH,MTH
c-----------------------------------------------------------------------
c
c     Define LISO target isomer flag
c
c     WARNING - This only works for ENDFB-6 format - where
C               LISO, target isomer flag is L2 on second line
C               of MF/MT=1/451.
C
c-----------------------------------------------------------------------
      IF(MTH.eq.451) LISIN = LISIN + 1
      IF(LISIN.eq.2) then
      CALL IN9(XLISO,LINE66(34))
      LISO = XLISO
c-----2020/3/21 - Added LISO = Target State
      CALL ZAHOLM(IZANOW,LISO,ZABCD)
      ENDIF
c
c     Read next line
c
      READ(ITAPE,210) LINE66,MATH,MFH,MTH
      IF(MATH.GT.0) GO TO 20
c-----MEND = Entire MAT has been copied to scratch.
      WRITE(ISCR1,210) LINE66,MATH,MFH,MTH      ! Write MEND
      WRITE(ISCR1,220)                          ! Add TEND
      ENDFILE ISCR1
      REWIND ISCR1
C-----------------------------------------------------------------------
C
C     PASS #2: THE ENTIRE MAT IS NOW ON SCRATCH
C
C-----------------------------------------------------------------------
C
C     IGNORE (COPY) ANY MT THAT ALREADY HAS MF=10 DATA
C
C-----------------------------------------------------------------------
      IMFILE9 = 0
      MFPRINT = 10
      IMHERE  = 0
      do k=1,MAXMT
      if(MTTAB10(K).ne.0) then
      write(OUTP,40) ZABCD,MATNOW,MFPRINT,K
      write(*   ,40) ZABCD,MATNOW,MFPRINT,K
   40 format(1x,12A1,I5,I4,I4,
     1 ' WARNING - MF=10 data already exists - will copy MT.')
      MTTAB9(k) = 0
      MTTAB3(k) = 0
      IMHERE    = 1
      IMALL10   = 1
      else
      if(MTTAB9(k).gt.0) IMFILE9 = IMFILE9 + 1
      endif
      enddo
      if(IMHERE .gt.0) then
      if(IMFILE9.gt.0) then
      write(outp,50)
      write(*   ,50)
   50 format(1x,78('-'))
      endif
      endif
C-----------------------------------------------------------------------
C
C     IF NO MF=9 DATA - NOTHING TO DO - COPY ENTIRE MAT TO OUTPUT.
C
C-----------------------------------------------------------------------
      IF(IMFILE9.GT.0) GO TO 70
C-----READ/WRITE FIRST LINE TO FORCE OUTPUT REPORT FOR PRECEDING
C-----MAT, IF ANY.
      ITAPE = ISCR1
      CALL CONTI
      NOSEQ = 1
      CALL CONTOG
      ITAPE = ISAVE
   60 READ(ISCR1,210,END=180) LINE66,MATH,MFH,MTH
      IF(MATH.LE.0) go to 170      ! MEND = Finish Preceeding MAT
      IF(MTH.LE.0) THEN
      CALL OUTS(MATH,MFH)
      ELSE
      WRITE(OTAPE,210) LINE66,MATH,MFH,MTH,NOSEQ
      NOSEQ = NXTSEQ(NOSEQ)
      LSTMAT = MATH
      LSTMF  = MFH
      ENDIF
      GO TO 60
C-----------------------------------------------------------------------
C
C     PROCESS ALL MF=9 DATA, AND MERGE WITH REMAINING MF=10 DATA, IF ANY
C
C-----------------------------------------------------------------------
   70 ITAPE = ISCR1
C
C     LOOP OVER SECTIONS READ FROM SCRATCH
C
C-----FIND NEXT REQUESTED SECTION.
   80 CALL NEXTMT
C-----CHECK FOR END OF MATERIAL
      IF(MATH.LE.0) GO TO 160
C-----FIND FILE 1, SECTION 451 AND ADD COMMENTS TO INDICATE THAT
C-----THIS MATERIAL HAS BEEN PROCESSED.
      IF(MFH.NE.1.OR.MTH.NE.451) GO TO 90
C-----ADD COMMENTS.
      CALL CONTOG
      CALL FILE1
      GO TO 80
C-----COPY ALL OF MF=3, 9, AND 10 TO SCRATCH.
   90 IF(MFH.eq.3 ) go to 100    ! MF=3
      IF(MFH.eq.9 ) go to 110    ! MF=9
      IF(MFH.eq.10) go to 120   ! MF=10
      IF(MFH.lt.10) go to 130   ! Before MF=10 copy section.
      go to 140                 ! After MF=10 processing finished.
C-----COPY ALL OF MF=3 TO OUTPUT AND SCRATCH.
  100 CALL FILE3
      GO TO 80
C-----COPY ALL OF MF=9 TO SCRATCH.
  110 CALL FILE9
      GO TO 80
C-----IF ANY MF=9 DATA, COPY ALL OF MF=10 TO SCRATCH.
  120 IF(IMFILE9.EQ.0) GO TO 130
      CALL FILE10
      GO TO 80
C-----COPY SECTION.
  130 CALL CONTOG
      CALL COPYS
      GO TO 80
C-----------------------------------------------------------------------
C
C     PAST MF=10 - CREATE MF=10 DATA AND THEN COPY TO END OF MAT.
C
C-----------------------------------------------------------------------
C-----SAVE HEADER PARAMETERS
  140 C1HSAVE = C1H
      C2HSAVE = C2H
      L1HSAVE = L1H
      L2HSAVE = L2H
      N1HSAVE = N1H
      N2HSAVE = N2H
      MATHSAV = MATH
      MFHSAVE = MFH
      MTHSAVE = MTH
C-----CREATE FILE 10
      CALL CREATE10
C-----OUTPUT HEAD RECORD FOR NEXT FILE (ALREADR READ)
      C1H = C1HSAVE
      C2H = C2HSAVE
      L1H = L1HSAVE
      L2H = L2HSAVE
      N1H = N1HSAVE
      N2H = N2HSAVE
      MATH= MATHSAV
      MFH = MFHSAVE
      MTH = MTHSAVE
      CALL CONTOG
C-----------------------------------------------------------------------
C
C     MF=10 Finished - COPY TO END OF MAT
C
C-----------------------------------------------------------------------
  150 CALL NEXTMT
      IF(MATH.LE.0) then
      CALL OUTF(LSTMAT) ! Add FEND
      go to 170
      endif
      CALL CONTOG
      CALL COPYS
      GO TO 150
C-----------------------------------------------------------------------
C
C     AT THE END OF THE MATERIAL - CREATE MF=10 DATA.
C
C-----------------------------------------------------------------------
  160 CALL CREATE10
C-----------------------------------------------------------------------
C
C     Finish Preceding MAT - and Try for another MAT.
C
C-----------------------------------------------------------------------
  170 CALL REPORT
      CALL OUTM
      LSTMAT = 0
      LSTMF  = 0
      GO TO 10               ! Try for next MAT
C-----------------------------------------------------------------------
C
C     Finish Preceding MAT - end of ENDF TAPE.
C
C-----------------------------------------------------------------------
  180 CALL REPORT
      CALL OUTM
      LSTMAT = 0
      LSTMF  = 0
C-----------------------------------------------------------------------
C
C     THE END OF ENDF TAPE - PRINT SUMMARY - ONLY DO ONE ENDF TAPE.
C
C-----------------------------------------------------------------------
C-----END FILE ENDF FORMAT OUTPUT FILE
  190 MATH = -1
      MFH  = 0
      MTH  = 0
      CALL CONTOG
C-----WRITE POINT TOTALS FOR TAPE.
      WRITE(OUTP,240) N2TOTI,N2TOTO
      WRITE(*   ,240) N2TOTI,N2TOTO
      CLOSE(ITAPE)
      CLOSE(OTAPE)
C-----------------------------------------------------------------------
C
C     Print WARNING is MF=10 data already exists
c
C-----------------------------------------------------------------------
      if(IMALL10.gt.0) then
      write(OUTP,200)
      write(*   ,200)
  200 format(
     1 ' WARNING - The purpose of this code is to create MF=10 data'/
     2 '           if it does not already exist - if it does exist'/
     3 '           it is copied without checking whether or not it is'/
     4 '           consistent with MF=9 data. Be WARNED that for the'/
     1 '           MF=10 created here MF=9 and MF=10 are consistent.'/
     2 '           But for MF=10 that already existed there is no'/
     3 '           check that MF=9 and MF=10 are consistent.'/
     4 1X,78('-'))
      endif
c-----2019/3/7 - Final WARNING if Maximum Tabulated Energy Inconsistent.
      CALL MAXIE4(1)
C-----08/08/2012 - ONLY DO ONE ENDF FILE
      CALL ENDIT ! This ends execution, but some compilers do not like
      STOP       ! mains that end in a call - so this dummy STOP is
C                ! included here only avoid compiler ERROR Messages.
  210 FORMAT(66A1,I4,I2,I3,I5)
  220 FORMAT(66X,'  -1 0  0    0')
  230 FORMAT(1X,78('-')/
     1 ' ENDF Tape Label'/1X,78('-')/1X,16A4,A2,I4/1X,78('-')/
     2 '   Material    MAT  MF  MT  ENDF    ZAP Final',6X,'Q-Value',
     3 ' Points Points'/
     4 '                            Format      State',6X,'     eV',
     5 '     In    Out'/
     4 1X,78('-'))
  240 FORMAT(1X,78('-')/45X,'Tape Totals',2I7/1X,78('-'))
  250 FORMAT(' ENDF Activation Cross Sections',
     1 ' (ACTIVATE 2021-1)'/1X,78('-'))
      END
      SUBROUTINE CREATE10
C=======================================================================
C
C     CREATE MF=10 ACTIVATION CROSS SECTIONS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'activate.h'
      INTEGER OUTP,OTAPE
      CHARACTER*1 LINE66
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
c-----2019/1/5 - Increased to 100 from 50 = PREPRO Standard
      COMMON/NBTINT/NBT(100),INT(100)
      COMMON/LASTE/ELAST
      COMMON/CLINE66/LINE66(66)
C-----POSITION ALL SCRATCH FILES.
      REWIND ISCR2
      REWIND ISCR3
      REWIND ISCR4
C-----------------------------------------------------------------------
C
C     LOOP OVER SECTIONS (MT) TO OUTPUT
C
C-----------------------------------------------------------------------
C-----DEFINE I/O UNITS TO READ MF=9 DATA AS INPUT.
      ISAVE = ITAPE
      ITAPE = ISCR3
      DO 90 IMT=1,MAXMT
C-----------------------------------------------------------------------
C
C     MF=9 DATA?
C
C-----------------------------------------------------------------------
      IF(MTTAB9(IMT).LE.0) GO TO 60
C-----------------------------------------------------------------------
C
C     MF=9 data requires MF=3.
C
C-----------------------------------------------------------------------
      if(MTTAB3(IMT).le.0) then
      write(OUTP,140) IMT
      write(*   ,140) IMT
      CALL ENDERROR
      endif
C-----------------------------------------------------------------------
C
C     COMBINE MF=3 AND 9 DATA - OUTPUT MF=10
C
C-----------------------------------------------------------------------
C-----LOCATE MF=9 DATA.
      ITAPE = ISCR3
   10 CALL CONTI
      IF(MTH.LE.0) GO TO 10
      IF(MTH.eq.IMT) go to 20
      IF(MTH.gt.IMT) go to 110
      CALL SKIPS
      GO TO 10
   20 MFH = 10
      CALL CONTOG
C-----------------------------------------------------------------------
C
C     LOOP OVER MF=9 SECTIONS - MF=9 HAS MULTIPLE SECTIONS.
C
C-----------------------------------------------------------------------
      NS = N1H
      DO 50 IS=1,NS
C-----FIRST, LOAD MF=9 MULTIPLIERS
      ITAPE = ISCR3
      CALL INTLAW
C-----INSURE MULTIPLIERS WILL FIT IN MEMORY.
      N9 = N2
      IF(N9.LT.2.OR.N9.GT.MAXPOINT) THEN
      WRITE(OUTP,180) N9,MAXPOINT
      WRITE(*   ,180) N9,MAXPOINT
      CALL ENDERROR
      ENDIF
C-----READ AND CHECK MULTIPLIERS
      ELAST = 0.0d0
      CALL POINTI(X9(1),Y9(1),N9)
c-----2019/3/9 - Save mat/mf/mt for multiplicity (MF = 9)
      CALL MAXIE1(MATH,  9,MTH)                ! Note: MF9, not MFH
c-----2010/3/9 - Save Maximum Energy and Value
      CALL MAXIE2(X9(N9),Y9(N9))
C-----SAVE FOR OUTPUT IN MF=10
      C19 = C1
      C29 = C2
      L19 = L1
      L29 = L2
C-----------------------------------------------------------------------
C
C     NEXT, LOCATE MF=3 DATA
C
C-----------------------------------------------------------------------
      ITAPE = ISCR2
      REWIND ISCR2
   30 CALL CONTI
      IF(MTH.LE.0) GO TO 30
      IF(MTH.eq.IMT) go to 40
      IF(MTH.gt.IMT) go to 100
      CALL SKIPS
      GO TO 30
C-----READ AND CHECK INTERPOLATION LAW
   40 CALL INTLAW
C-----OUTPUT MF=10 LEADER AND INTERPLATION LAW
      C1 = C19
      C2 = C29
      L1 = L19
      L2 = L29
      MFH = 10
      CALL CARDO(C1,C2,L1,L2,N1,N2)
      CALL TERPO(NBT,INT,N1)
C-----COMBINE MF=3 AND 9 DATA
      CALL FILEX
   50 CONTINUE
C-----ADD SEND LINE.
      CALL OUTS(MATH,MFH)
      GO TO 90
C-----------------------------------------------------------------------
C
C     MF=10 DATA?
C
C-----------------------------------------------------------------------
   60 IF(MTTAB10(IMT).LE.0) GO TO 90
c-----Add FEND and ENDF if needed.
      MFH = 10
      CALL SYNCEND
C-----------------------------------------------------------------------
C
C     OUTPUT ORIGINAL MF=10 DATA
C
C-----------------------------------------------------------------------
c-----Find same MT
   70 READ(ISCR4,130,END=120) LINE66,MATH,MFH,MTH
      IF(MTH.eq.IMT) go to 80
      IF(MTH.gt.IMT) go to 120
      CALL SKIPS
      GO TO 70
c-----Copy section to Output.
   80 IF(MTH.LE.0) THEN
      CALL OUTS(MATH,MFH)
      ELSE
      WRITE(OTAPE,130) LINE66,MATH,MFH,MTH,NOSEQ
      NOSEQ = NXTSEQ(NOSEQ)
      ENDIF
c-----Continue to MT=0 (SEND)
      IF(MTH.LE.0) GO TO 90
      READ(ISCR4,130,END=120) LINE66,MATH,MFH,MTH
      GO TO 80
c-----END OF MT loop.
   90 CONTINUE
C-----------------------------------------------------------------------
C
C     MF=10 COMPLETED - ADD FEND AND RESTORE ENDF INPUT NUMBER
C
C-----------------------------------------------------------------------
      ITAPE = ISAVE
      RETURN
C-----------------------------------------------------------------------
C
C     ERROR READING MF=3 DATA.
C
C-----------------------------------------------------------------------
  100 WRITE(OUTP,170) IMT
      WRITE(*   ,170) IMT
      CALL ENDERROR
C-----------------------------------------------------------------------
C
C     ERROR READING MF=9 DATA.
C
C-----------------------------------------------------------------------
  110 WRITE(OUTP,150) IMT
      WRITE(*   ,150) IMT
      CALL ENDERROR
C-----------------------------------------------------------------------
C
C     ERROR READING MF=10 DATA.
C
C-----------------------------------------------------------------------
  120 WRITE(OUTP,160) IMT
      WRITE(*   ,160) IMT
      CALL ENDERROR
  130 FORMAT(66A1,I4,I2,I3,I5)
  140 FORMAT(' ERROR - MT=',I5,' MF=9 Data Required MF=3 Data')
  150 FORMAT(' ERROR - Cannot Locate MF=  9, MT=',I5,' Data')
  160 FORMAT(' ERROR - Cannot Locate MF= 10, MT=',I5,' Data')
  170 FORMAT(' ERROR - Cannot Locate MF=  3, MT=',I5,' Data')
  180 FORMAT(' ERROR - N2=',I8,' (MUST be 2 to ',I8,')'/
     1       ' Execution Terminated')
      END
      SUBROUTINE INTLAW
C=======================================================================
C
C     CHECK INTERPOLATION LAW
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 ZABCD
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
c-----2019/1/5 - Increased to 100 from 50 = PREPRO Standard
      COMMON/NBTINT/NBT(100),INT(100)
      COMMON/BCDZA/ZABCD(12)
C-----------------------------------------------------------------------
C
C     READ AND CHECK SECTION LEADER LINE AND INTERPOLATION LAW. IF
C     INTERPOLATION LAW IS ILLEGAL (BAD INTERPOLATION TYPE OR IMPROPER
C     INTERPOLATION BOUNDARY) TERMINATE EXECUTION.
C
C-----------------------------------------------------------------------
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      CALL TERPI(NBT,INT,N1)
c-----2019/1/3 - Additional Interpolation Law Tests
      CALL TERPTEST(NBT,INT,N1,N2,3)
      RETURN
      END
      SUBROUTINE FILEX
C=======================================================================
C
C     OUTPUT ALL SECTIONS OF MF=10 DATA USING,
C     1) A COMBINATION OF MF=3 AND 9, OR
C     2) ORIGINAL MF=10 DATA
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'activate.h'
      INTEGER OUTP,OTAPE
      CHARACTER*1 ZABCD,FIELD
      CHARACTER*4 FMTHOL
      CHARACTER*1 LINE66
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/HOLFMT/FMTHOL
      COMMON/FIELDC/FIELD(11,6)
      COMMON/COUNTS/IZANOW,MATNOW,N2MATI,N2MATO,N2TOTI,N2TOTO
      COMMON/LISOCOM/LISO
      COMMON/BCDZA/ZABCD(12)
      COMMON/CLINE66/LINE66(66)
C-----INITIALIZE TO AVOID COMPILER WARNING.
      DATA MF3/0/
      IF(MF3.eq.0) MF3 = 3    ! Avoid Freud WARNING
C-----DEFINE MATERIAL
      KZA = C1H
c-----2020/3/21 - Added LISO = Target State
      CALL ZAHOLM(KZA,LISO,ZABCD)
C-----SET OUTPUT FOR MF=10
      MFH = 10
      MTIN = MTH
C-----ORIGINAL AND FINAL POINT COUNTS ARE THE SAME = MF=3 POINTS.
      N2IN = N2
      N2OUT = N2
c-----2019/3/9 - save mat/mf/mt for cross section (MF = 3)
      CALL MAXIE1(MATH,  3,MTH)    ! Note: MF3, not MTH
C-----------------------------------------------------------------------
C
C     COPY MF=3 DATA, RE-NORMALIZING CROSS SECTIONS BY MF=9 MULTIPLIERS
C
C-----------------------------------------------------------------------
      I9=1
C-----LOOP OVER MF=3 LINES, 3 POINTS PER LINE.
      DO 60 I1=1,N2IN,3
      I2 = I1+2
      IF(I2.GT.N2IN) I2=N2IN
      K2 = (I2-I1)+1
      READ(ISCR2,90,END=80) LINE66,MATH,MF3,MTH
      KK=1
C-----DEFINE MF=3 ENERGY AND CROSS SECTIONS FOR EACH ENERGY POINT
C-----AND RE-NORMALIZE CROSS SECTION.
      DO 50 K1=1,K2
      CALL IN9( ENOW,LINE66(KK))
      CALL IN9(XCNOW,LINE66(KK+11))
C-----FIND MF=9 MULTIPLIER AT SAME ENERGY AS MF=3 CROSS SECTION
      DO K9=I9,N9
      IF(ENOW.lt.X9(K9)) go to 20
      IF(ENOW.eq.X9(K9)) go to 30
      ENDDO
   10 WRITE(OUTP,110) ENOW,X9(1),X9(N9),ENOW
      WRITE(*   ,110) ENOW,X9(1),X9(N9),ENOW
      XMULT = 0.0d0
      K9 = 1
      GO TO 40
C-----INTERPOLATE MULTIPLIER.
   20 IF(K9.LE.1) GO TO 10
      XMULT = TERPIT(ENOW,X9(K9-1),X9(K9),Y9(K9-1),Y9(K9),2)
      GO TO 40
C-----USE TABULATED MULTIPLER.
   30 XMULT = Y9(K9)
C-----RE-NORMALIZE MF=3 CROSS SECTION AND PUT BACK ON OUTPUT LINE
   40 I9 = K9
      XCNOW = XCNOW*XMULT
      CALL OUT9(XCNOW,LINE66(KK+11))
      KK=KK+22
   50 CONTINUE
C-----OUTPUT LINE OF UP TO 3 ENERGY POINTS
      WRITE(OTAPE,90) LINE66,MATH,MFH,MTH,NOSEQ
      NOSEQ = NXTSEQ(NOSEQ)
   60 CONTINUE
c-----2019/3/9 - Maximum tabulated MF=3 energy and value
      CALL MAXIE2(ENOW,XCNOW)
C-----------------------------------------------------------------------
C
C     PRINT SUMMARY OF SECTION
C
C-----------------------------------------------------------------------
      CALL OUT9(C2,FIELD(1,1))
      WRITE(OUTP,70) ZABCD,MATH,MFH,MTIN,FMTHOL,L1,L2,
     1 (FIELD(K,1),K=1,11),N2IN,N2OUT
      WRITE(*   ,70) ZABCD,MATH,MFH,MTIN,FMTHOL,L1,L2,
     1 (FIELD(K,1),K=1,11),N2IN,N2OUT
   70 FORMAT(1X,12A1,I5,I4,I4,3X,A2,I8,I6,2X,11A1,2I7)
c-----------------------------------------------------------------------
c
c     NOTE: ZAP=0 is o.k. = photon production.
C           It follows all others = not in incresasing ZAP order.
c
c-----------------------------------------------------------------------
C-----INCREMENT CUMULATIVE COUNTS
      N2MATI = N2MATI + N2IN
      N2MATO = N2MATO + N2OUT
      N2TOTI = N2TOTI + N2IN
      N2TOTO = N2TOTO + N2OUT
      RETURN
C-----------------------------------------------------------------------
C
C     ERROR READING MF=3 DATA
C
C-----------------------------------------------------------------------
   80 WRITE(OUTP,100)
      WRITE(*   ,100)
      CALL ENDERROR
   90 FORMAT(66A1,I4,I2,I3,I5)
  100 FORMAT(' ERROR - END of ENDF Data while Reading MF=3 Data')
  110 FORMAT(' WARNING - MF3 E=',1PD12.5,' Not in Energy Range of'/
     1       '           MF9 E=',1p2d12.5,' eV Data.'/
     2       '           Will Assume MF9 Data = 0 at E=',1PD12.5)
      END
      SUBROUTINE FILE1
C=======================================================================
C
C     ADD COMMENTS AT THE END OF FILE 1, SECTION 451 TO INDICATE
C     THAT THIS MATERIAL HAS BEEN PROCESSED BY PROGRAM LINEAR AND
C     TO SPECIFY THE ALLOWABLE ERROR.
C
C     DEFINE FORMAT TO BE ENDF-4, 5 OR 6.
C
C     THE ENDF FORMAT CAN BE DETERMINED FROM THE SECOND LINE.
C     ENDF-4  = N1 > 0, N2 = 0,LINE COUNT (POSITIVE)
C     ENDF-5  = N1 = N2 = 0
C     ENDF-6  =      N2 = VERSION NUMBER (6 OR MORE)
C
C     The First line has already been read.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 PROGDOC1
      CHARACTER*4 FMTTAB,FMTHOL
      CHARACTER*66 PROGDOC
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/HOLFMT/FMTHOL
      COMMON/LISOCOM/LISO
      DIMENSION FMTTAB(3),PROGDOC(3),PROGDOC1(66,3)
      EQUIVALENCE (PROGDOC(1),PROGDOC1(1,1))
C-----ENDF FORMAT VERSION
      DATA IVERSE/6/
      DATA FMTTAB/'4   ','5   ','6   '/
C-----DOCUMENTATION TO ADD TO ENDF OUTPUT - EACH LINE IS 66
C-----CHARACTERS LONG - FIELDS 12345678901 ARE FILLED IN WITH
C-----11 CHARACTERS DURING EXECUTION.
C               1         2         3         4         5         6
C       12345678901234567890123456789012345678901234567890123456789012
C       3456
      DATA PROGDOC/
     1 ' *************** PROGRAM ACTIVATE (VERSION 2021-1) ***********',
     2 ' MF=10 Activation Cross Sections Defined by Combining MF=3    ',
     3 ' Cross Section and MF=9 Multiplier. MF=9 Deleted/MF=8 Updated.'/
C-----FILL IN REMAINDER OF FIRST LINE.
      PROGDOC1(63,1) = '*'
      PROGDOC1(64,1) = '*'
      PROGDOC1(65,1) = '*'
      PROGDOC1(66,1) = '*'
c
c     Read Second Line
c
C-----HEAD LINE OF SECTION HAS BEEN READ AND WRITTEN. READ NEXT LINE
C-----AND DETERMINE IF THIS IS THE ENDF-4, 5 OR 6 FORMAT.
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      IVERSE=4
      LISOX = L2         ! Save as potntial isomeric number
C-----CHECK FOR ENDF-4.
C-----IV N1 > 0, N2 = 0
      IF(N1.GT.0.AND.N2.EQ.0) GO TO 10
C-----NOT ENDF-4. READ THIRD LINE.
      N2X=N2
      CALL CARDO(C1,C2,L1,L2,N1,N2)
c
c     Read third Line
c
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      IVERSE=5
C-----CHECK FOR ENDF-5 FORMAT.
      IF(N2X.LE.0) GO TO 10
C-----ENDF-6 FORMAT. READ FOURTH LINE.
      CALL CARDO(C1,C2,L1,L2,N1,N2)
c
c     Read fourth Line
c
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      IVERSE=6
      LISO  = LISOX                 ! Isomer state number
C-----SET DERIVED MATERIAL FLAG.
      L1=1
C-----DEFINE ENDF FORMAT NUMBER.
   10 FMTHOL=FMTTAB(IVERSE-3)
C-----INCREASE COMMENT LINE COUNT AND COPY TO END OF HOLLERITH.
      N1OUT = N1+3
      CALL CARDO(C1,C2,L1,L2,N1OUT,N2)
      DO 20 N=1,N1
      CALL COPY1
   20 CONTINUE
C-----------------------------------------------------------------------
C
C     ADD COMMENTS TO DOCUMENT WHAT WAS DONE TO DATA.
C
C-----------------------------------------------------------------------
C-----OUTPUT PROGRAM NAME AND VERSION I.D.
      CALL HOLLYO(PROGDOC1(1,1))
      CALL HOLLYO(PROGDOC1(1,2))
      CALL HOLLYO(PROGDOC1(1,3))
C-----COPY TO END OF SECTION.
      CALL COPYS
      RETURN
      END
      SUBROUTINE FILE3
C=======================================================================
C
C     COPY ALL OF MF=3 TO OUTPUT FILE AND SCRATCH.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER OUTP,OTAPE
      CHARACTER*1 LINE66
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COUNTS/IZANOW,MATNOW,N2MATI,N2MATO,N2TOTI,N2TOTO
      COMMON/CLINE66/LINE66(66)
C-----------------------------------------------------------------------
C
C     INITIALIZE SCRATCH FILE
C
C-----------------------------------------------------------------------
      MATNOW = MATH
      REWIND ISCR2
      IO1 = OTAPE
      IO2 = ISCR2
C-----HEADER CARD HAS ALREADY BEEN READ - OUTPUT IT.
      NOSEQ = 1
      CALL CONTOG
C-----ALSO OUTPUT TO SCRATCH
      OTAPE = IO2
      CALL CONTO
      NOSEQ = 2     ! Reset NOSEQ due to above CONTO to Scratch.
      OTAPE = IO1
C-----COPY TO END OF MF=3
   10 READ(ISCR1,30) LINE66,MATH,MFH,MTH
      IF(MFH.LE.0) GO TO 20
      IF(MTH.LE.0) THEN
      CALL OUTS(MATH,MFH)
      ELSE
      WRITE(OTAPE,30) LINE66,MATH,MFH,MTH,NOSEQ
      NOSEQ = NXTSEQ(NOSEQ)
      ENDIF
      WRITE(ISCR2 ,30) LINE66,MATH,MFH,MTH
      GO TO 10
   20 ENDFILE ISCR2
      REWIND ISCR2
      RETURN
   30 FORMAT(66A1,I4,I2,I3,I5)
      END
      SUBROUTINE FILE9
C=======================================================================
C
C     COPY ALL OF MF=9 TO SCRATCH - NOT TO OUTPUT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER OUTP,OTAPE
      CHARACTER*1 LINE66
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/CLINE66/LINE66(66)
C-----------------------------------------------------------------------
C
C     INITIALIZE SCRATCH FILE
C
C-----------------------------------------------------------------------
C-----SAVE STARTING SEQUENCE NUMBER
      INSEQ = NOSEQ
      REWIND ISCR3
      IO1 = OTAPE
C-----HEADER CARD HAS ALREADY BEEN READ.
      OTAPE = ISCR3
      CALL CONTO
      OTAPE = IO1
C-----COPY TO END OF MF=9
   10 READ(ISCR1,20) LINE66,MATH,MFH,MTH
      WRITE(ISCR3 ,20) LINE66,MATH,MFH,MTH
      IF(MFH.GT.0) GO TO 10
      ENDFILE ISCR3
      REWIND ISCR3
C-----RESTORE STARTING SEQUENCE NUMBER
      NOSEQ = INSEQ
      RETURN
   20 FORMAT(66A1,I4,I2,I3,I5)
      END
      SUBROUTINE FILE10
C=======================================================================
C
C     COPY ALL OF MF=10 TO SCRATCH - NOT TO OUTPUT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER OUTP,OTAPE
      CHARACTER*1 LINE66
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/CLINE66/LINE66(66)
C-----------------------------------------------------------------------
C
C     INITIALIZE SCRATCH FILE
C
C-----------------------------------------------------------------------
C-----SAVE STARTING SEQUENCE NUMBER
      INSEQ = NOSEQ
      REWIND ISCR4
      IO1 = OTAPE
C-----HEADER CARD HAS ALREADY BEEN READ.
      OTAPE = ISCR4
      CALL CONTO
      OTAPE = IO1
C-----COPY TO END OF MF=10
   10 READ(ISCR1,20) LINE66,MATH,MFH,MTH
      WRITE(ISCR4,20) LINE66,MATH,MFH,MTH
      IF(MFH.GT.0) GO TO 10
      ENDFILE ISCR4
      REWIND ISCR4
C-----RESTORE STARTING SEQUENCE NUMBER
      NOSEQ = INSEQ
      RETURN
   20 FORMAT(66A1,I4,I2,I3,I5)
      END
      SUBROUTINE READIN
C=======================================================================
C
C     READ AND CHECK ALL INPUT PARAMETERS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER OUTP,OTAPE
      CHARACTER*72 NAMEIN,NAMEOUT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/NAMEX/NAMEIN,NAMEOUT
C-----------------------------------------------------------------------
C
C     READ FILENAMES - IF BLANK USE STANDARD FILENAMES
C
C-----------------------------------------------------------------------
C-----INPUT DATA - IF NO INPUT FILE, USE DEFAULT NAMES
      IF(ISTAT1.EQ.1) GO TO 20
      READ(INP,10,END=20,ERR=20) NAMEIN
   10 FORMAT(A72)
      IF(NAMEIN.EQ.' ') NAMEIN = 'ENDFB.IN'
C-----OUTPUT DATA.
      READ(INP,10,END=30,ERR=30) NAMEOUT
      IF(NAMEOUT.EQ.' ') NAMEOUT = 'ENDFB.OUT'
      GO TO 40
C-----------------------------------------------------------------------
C
C     USE DEFAULTS
C
C-----------------------------------------------------------------------
C-----USE DEFAULT FILENAMES
   20 NAMEIN  = 'ENDFB.IN'
   30 NAMEOUT = 'ENDFB.OUT'
      ISTAT1 = 1
C-----PRINT FINAL FILENAMES
   40 WRITE(OUTP,50) NAMEIN,NAMEOUT
      WRITE(*   ,50) NAMEIN,NAMEOUT
   50 FORMAT(
     1 ' ENDF Input and Output Data Filenames'/1X,78('-')/
     2 1X,A72/1X,A72)
C-----------------------------------------------------------------------
C
C     OPEN ENDF DATA FILES
C
C-----------------------------------------------------------------------
      CALL FILIO2
C-----------------------------------------------------------------------
C
C     TERMINATE IF ERROR OPENING ENDF DATAQ FILE
C
C-----------------------------------------------------------------------
      IF(ISTAT2.EQ.1) THEN
      WRITE(OUTP,60) NAMEIN
      WRITE(   *,60) NAMEIN
   60 FORMAT(//' ERROR - Opening ENDF data file'/1X,A72//)
      CALL ENDERROR
      ENDIF
      RETURN
      END
      SUBROUTINE NEXTMT
C=======================================================================
C
C     FIND NEXT REQUESTED MATERIAL BASED EITHER ON ZA OR MAT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 ZABCD
      CHARACTER*4 FMTHOL
      COMMON/COUNTS/IZANOW,MATNOW,N2MATI,N2MATO,N2TOTI,N2TOTO
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/HOLFMT/FMTHOL
      COMMON/LISOCOM/LISO
      COMMON/BCDZA/ZABCD(12)
C-----READ NEXT LINE AND CHECK FOR END OF ENDF TAPE.
   10 CALL CONTI
      IF(MTH.gt.0) go to 30
      IF(MATH.gt.0) go to 10
      IF(MATH.lt.0) go to 20
C-----------------------------------------------------------------------
C
C     END OF MAT
C
C-----------------------------------------------------------------------
      RETURN
C-----------------------------------------------------------------------
C
C     END OF DATA - OUTPUT SUMMARY OF PRECEDING MATERIAL, IF ANY.
C
C-----------------------------------------------------------------------
C-----RETURN NEGATIVE MATH AS END OF RUN INDICATOR.
   20 MATH  =-1
      MFH   =0
      MTH   =0
      NOSEQ =0
      N2MATI=0
      N2MATO=0
      RETURN
C-----------------------------------------------------------------------
C
C     THIS SECTION REQUESTED. IF NEW MF OR MAT ADD FEND AND MEND LINES
C     AS REQUIRED.
C
C-----------------------------------------------------------------------
   30 IF(MATNOW.EQ.MATH) GO TO 40
C-----INITIALIZE TO ENDF-6 FORMAT.
      FMTHOL='6   '
C-----SAVE CURRENT ZA, MAT.
   40 IZANOW = C1H
c-----2020/3/21 - Added LISO = Target State Number
      CALL ZAHOLM(IZANOW,LISO,ZABCD)
      MATNOW=MATH
      RETURN
      END
      SUBROUTINE CONTOG
C=======================================================================
C
C     WRITE ONE ENDF CONTROL RECORD.
C
C     FIRST ADD FEND AND MEND RECORDS IF NEEDED = IF MAT/MF CHANGE
C
C=======================================================================
      INCLUDE 'implicit.h'
C-----------------------------------------------------------------------
c
c     ADD FEND and MEND as needed.
c
C-----------------------------------------------------------------------
      CALL SYNCEND
C-----------------------------------------------------------------------
c
c     NORMAL OUTPUT LINE FOR NEXT SECTION.
c
C-----------------------------------------------------------------------
      CALL CONTO
      RETURN
      END
      SUBROUTINE SYNCEND
C=======================================================================
C
C     ADD FEND AND MEND RECORDS IF NEEDED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 ZABCD2
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COUNTS/IZANOW,MATNOW,N2MATI,N2MATO,N2TOTI,N2TOTO
      COMMON/LSTCOM/LSTIZA,LSTMAT,LSTMF
      COMMON/LSTCOMC/ZABCD2(12)
      COMMON/LISOCOM/LISO
      IF(OTAPE.LE.0) RETURN
C-----------------------------------------------------------------------
c
c     ADD FEND AND MEND LINES AS NEEDED.
c
C-----------------------------------------------------------------------
      IF(LSTMAT.LE.0) GO TO 10
C-----ADD FEND LINE IF NEW MAT OR MF.
      IF(MATH.EQ.LSTMAT.AND.MFH.EQ.LSTMF) GO TO 10
      CALL OUTF(LSTMAT)
C-----PRINT SUMMARY OF PRECEDING MAT.
      IF(MATH.EQ.LSTMAT) GO TO 10
C-----------------------------------------------------------------------
C
C     PRINT SUMMARY OF PRECEDING MAT.
C
C-----------------------------------------------------------------------
      CALL REPORT
      N2MATI=0
      N2MATO=0
      CALL OUTM
      NOSEQ=1
C-----------------------------------------------------------------------
c
c     SAVE LAST ZA/MAT/MF FOR OUTPUT AT END OF CURRENT MAT.
c
C-----------------------------------------------------------------------
   10 LSTIZA = C1H
      LSTMAT = MATH
      LSTMF  = MFH
c-----2020/3/21 - Added LISO = Target State Number
      CALL ZAHOLM(LSTIZA,LISO,ZABCD2)
      RETURN
      END
      SUBROUTINE REPORT
C=======================================================================
C
C     PRINT SUMMARY OF PRECEDING MAT - IF ANY.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 ZABCD2
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COUNTS/IZANOW,MATNOW,N2MATI,N2MATO,N2TOTI,N2TOTO
      COMMON/LSTCOM/LSTIZA,LSTMAT,LSTMF
      COMMON/LSTCOMC/ZABCD2(12)
C-----SPECIAL MESSAGE IF NO MF=9 DATA.
      IF(N2MATI.GT.0.OR.N2MATO.GT.0) GO TO 10
      WRITE(OUTP,40) ZABCD2,LSTMAT
      WRITE(*   ,40) ZABCD2,LSTMAT
      GO TO 20
C-----WRITE POINT TOTAL FOR LAST MAT AND ADD TO TAPE TOTALS.
   10 WRITE(OUTP,30) N2MATI,N2MATO
      WRITE(*   ,30) N2MATI,N2MATO
c-----2019/3/7 - Check Maximum Tabulated Energy
   20 CALL MAXIE3(1)
C-----PRINT TIME
      CALL TIMEMAT
      RETURN
   30 FORMAT(1X,78('-')/45X,' MAT Totals',2I7/1X,78('-'))
   40 FORMAT(1X,12A1,I5,8x,' WARNING - No MF=10 to create -',
     1 ' will copy MAT')
      END
      SUBROUTINE FILEIO
C=======================================================================
C
C     DEFINE ALL I/O UNITS AND OPTIONAL DEFINE FILE NAMES.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER OUTP,OTAPE
      CHARACTER*72 NAMEIN,NAMEOUT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/UNITS/ISCR1,ISCR2,ISCR3,ISCR4
      COMMON/NAMEX/NAMEIN,NAMEOUT
C-----DEFINE ALL UNIT NUMBERS.
      INP=2
      OUTP=3
      ITAPE=10
      OTAPE=11
      ISCR1=12
      ISCR2=14
      ISCR3=15
      ISCR4=16
C-----DEFINE ALL FILE NAMES.
      OPEN(OUTP,FILE='ACTIVATE.LST',STATUS='UNKNOWN')
      OPEN(ISCR1,STATUS='SCRATCH')
      OPEN(ISCR2,STATUS='SCRATCH')
      OPEN(ISCR3,STATUS='SCRATCH')
      OPEN(ISCR4,STATUS='SCRATCH')
      OPEN(INP,FILE='ACTIVATE.INP',STATUS='OLD',ERR=10)
      ISTAT1 = 0
      RETURN
   10 ISTAT1 = 1   ! NO INPUT PARAMETERS = USE STANDARD FILENAMES.
      RETURN
      ENTRY FILIO2
C=======================================================================
C
C     DEFINE ENDF DATA FILE NAMES.
C
C=======================================================================
      OPEN(ITAPE,FILE=NAMEIN ,STATUS='OLD',ERR=20)
      OPEN(OTAPE,FILE=NAMEOUT,STATUS='UNKNOWN')
      ISTAT2 = 0
      RETURN
   20 ISTAT2 = 1 ! NO ENDF INPUT DATA
      RETURN
      END
