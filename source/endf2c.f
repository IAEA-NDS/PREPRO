C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


c=======================================================================
c
c     Program ENDF2C
c     ==============
c     Convert ENDF Data to Standard FORTRAN, C and C++ Format.
c
c     Version 2014-1 Feb. 2014 * Initial version.
C             2014-2 Oct. 2014 * Changed from D to E exponential form
C                                to improve compatibility between
C                                computer languages.
C             2015-1 Jan. 2015 * General updates for release with
C                                PREPRO2015.
C                              * Changed ENDF data filenames from ENDF2C
C                                to ENDFB, to agree with PREPRO default
C                                definitions.
C                              * Added code name (to be compatible
C                                with PREPRO output), but NOT TIME (to
C                                keep this code as computer independent
C                                as possible).
C             2017-1 May  2017 * Updated based on user feedbsck
C             2018-1 Jan. 2018 * Added on-line output for ALL ENDERROR
C             2019-1 June 2019 * Added /UNITS/ to allow correct output
C                                at end = output either o.k. or error.
C             2020-1 Feb. 2020 * Identical to 2019-1.
C             2021-1 Jan. 2021 * Updated for FOTRAN 2018
C             2023-1 Feb. 2023 * Identify ENDF in and out filenames
C
c     Purpose
c     ==================================================================
c     This code is designed for,
c     1) ENDF Data in any ENDF format = ENDF-1 through ENDF-6.
c     2) On any type of computer = 32 or 64 bit system/compiler
c
c     This code tries to keep things as simple as possible
c     1) There are NO INPUT PARAMETERS.
c     2) It reads  an ENDF formatted file named ENDFB.IN
c     3) It writes an ENDF formatted file named ENDFB.OUT
c     4) It writes a report file named          ENDF2C.LST
c
c     Author's Message
c     ----------------
c     I consider insuring that ENDF data is in a standard, officially
c     approved format for FORTRAN, C and C++ is SO IMPORTANT this code
c     does only one thing - and only one thing - and it does it in the
c     simplest possible manner - efficiency is NOT a consideration -
c     ONLY accuracy and general utility of the ENDF data is considered.
c
c     Method
c     ------
c     Other codes that attempt to do the same thing - including codes
c     written be me decades ago - are very complicated, and therefore
c     ERROR PRONE because they try to deal with each and every variant
c     in which data can be coded in the ENDF format. Needless to say
c     this means that every time the ENDF formats and procedures change
c     these codes MUSE also be changed.
c
c     In contrast, ENDF2C uses my almost 50 years of experience dealing
c     with the ENDF format to realize that except for the comments at
c     the beginning for each evaluation (MF/MT=1/451), every line of
c     ENDF data is IDENTICAL - in every version of the ENDF format, from
c     the original ENDF to today's ENDF-6. So to translate ENDF data
c     into an official format I do not have to consider differences in
c     each section (MF/MT) of data.
c
c     Every line of ENDF is divided into 6 fields, each 11 columns wide.
c     Each of the 6 fields is either, blank, integer or floating point.
c     Floating point fields ALL include a decimal point (.). So that ALL
c     this code does is convert every floating point field to standard
c     format.
c
c     In order to insure that this PRESERVES the accuracy of the data
c     this is done by reading and writing each ENDF line as characters.
c     Blank and integer fields are copied exactly as read. ALL floating
c     point number that are read are converted internally from character
c     to floating point - they are then converted back into characters
c     in a standard, officially approved format, for output.
c
c     As a last step to insure the accuracy of results the characters
c     to be output are again converted from characters to floating
c     point, and the numerical value that is output is compared to the
c     numerical value originally read, and if there is ANY DIFFERENCE
c     the characters strings read and written are listed in the output:
c     the characters strings read and written as well as the difference
c     is listed in the output report (ENDF2C.LST) and on the screen.
c
c     Running Time
c     ------------
c     It takes only seconds to translate an ENDF formatted evaluation,
c     so running time need not be a consideration. Concentrate on
c     keeping it simple and reliable - that should be your focus.
c
c     Documentation
c     -------------
c     ALL of my codes that process ENDF data and change it in ANY WAY
c     document what they have done by adding comment lines at the end
c     of the comment section (MF/MT=1/451) of each evaluation. This
c     allows data users to determine the pedigree of the data they are
c     using, by reading these comments. This code documents what is has
c     done by adding the following 2 comment lines.
c
c ***************** Program ENDF2C (Version 2023-1) ***************
c      Convert ENDF Data to Standard FORTRAN, C and C++ Format
c
c     WARNING - This documentation is IMPORTANT to data users and it
c     should not be deleted.
c
c     Written by
c     ------------------------------------
c     Dermott E. Cullen
c     University of California (retired)
c-----Present Home Address----------------------------------------------
c     Dermott E. Cullen
c     1466 Hudson Way
c     Livermore, CA 94550
c     U.S.A.
c     Telephone  925-443-1911
c     E. Mail    RedCullen1@Comcast.net
c     Website    RedCullen1.net/HOMEPAGE.NEW
c
c=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      character*8 codename
c-----2023/2/5 - Added ENDF in and out filenames
      character*12 filein,fileout
      character*1 LINE66
      integer*4 OUTP,OTAPE
      common/endf1/LINE66(11,6)
      common/endf2/MAT,MF,MT,NOSEQ,IVERSE
      common/ENDFIO/INP,OUTP,ITAPE,OTAPE
      common/namecode/codename
c-----2023/2/5 - Added ENDF in and out filenames
      data filein /'ENDFB.IN    '/
      data fileout/'ENDFB.OUT   '/
c                   123456799012
c-----------------------------------------------------------------------
c
c     Define Code name for output report and initialize time
c
c-----------------------------------------------------------------------
      codename = 'ENDF2C  '
c-----2019/1/8 - not used unit numbers, defined for PREPRO compatible
      INP  = 2
      OUTP = 3
      ITAPE= 10
      OTAPE= 11
c-----Initialize timer.
      call TIMER
c-----------------------------------------------------------------------
c
c     Define I/O Files
c
c-----------------------------------------------------------------------
      open( 3,file='ENDF2C.LST')
c-----------------------------------------------------------------------
c
c     Identify Code.
c
c-----------------------------------------------------------------------
c-----2023/2/5 - added ENDF in and out filenames
      write(*,10) filein,fileout
      write(3,10) filein,fileout
   10 format(//
     3 ' Convert ENDF Data to Standard FORTRAN, C and C++ Format'/
     1 '             Program ENDF2C (Version 2023-1)'/
     1 1x,79('-')/' ENDF Input and OutOut Filenames'/1x,A12/1x,A12/
     7 1x,79('-')/' ENDF Tape Label (TPID)'/1x,79('-'))
c-----2023/3/5 - use ENDF in and out filenames
      open(10,file=filein)
      open(11,file=fileout)
c-----Initialize previous MAT read and ENDF format version #
      MATLAST = -99999
      IVERSE  = 0
      NOSEQ   = 0
c-----------------------------------------------------------------------
c
c     copy TPID - and identify it in output listing
c
c-----------------------------------------------------------------------
      read (10,20) LINE66,MAT,MF,MT
      write( *,20) LINE66,MAT,MF,MT,NOSEQ
      write( 3,20) LINE66,MAT,MF,MT,NOSEQ
   20 format(66A1,i4,i2,i3,i5)
c-----------------------------------------------------------------------
c
c     Check for expected TPID format = file may not have TPID.
c
c-----------------------------------------------------------------------
      if(MF.eq.0.and.MT.eq.0) then
      NOSEQ = 0
      write(11,20) LINE66,MAT,MF,MT,NOSEQ               ! Assume TPID
      write( *,30)
      write( 3,30)
   30 format(1x,79('-')/' Input       Output         Fractional Diff.',
     5 '    MAT MF  MT  Seq# Format'/
     6       1x,79('-'))
      else
      write( *,40)                                      ! NOT TPID
      write( 3,40)
   40 format(1x,79('-')/' WARNING - This DOES NOT look like a TPID.',
     1       ' Expect MF = MT = 0 on TPID.'/
     2       '           Will Assume file has NO TPID',
     3       ' and TRY to Translate this line.'/
     4 1x,79('-')/' Input       Output         Fractional Diff.',
     5 '    MAT MF  MT  Seq# Format'/
     6       1x,79('-'))
      go to 60                                            ! Translate
      endif
c-----------------------------------------------------------------------
c
c     Loop over ALL ENDF lines to TEND line
c
c-----------------------------------------------------------------------
   50 read (10,20,end=80) LINE66,MAT,MF,MT
c-----Initialize ENDF Format vesion # for each new MAT
   60 if(MAT.ne.MATLAST) then
      MATLAST = MAT
      IVERSE  = 0
      NOSEQ   = 0
      endif
c-----------------------------------------------------------------------
c
c     special treatment for SEND/FEND/MEND/TEND
c
c-----------------------------------------------------------------------
      if(MT.le.0) then               ! SEND/FEND/MEND/TEND?
      if(MF.gt.0) then
      NOSEQ = 99999                  ! SEND NOSEQ = 99999
      else
      NOSEQ = 0                      ! FEND/MEND/TEND NOSEQ = 0
      endif
      write(11,70) MAT,MF,MT,NOSEQ ! Output with blank fields
   70 format(66X,i4,i2,i3,i5)
      NOSEQ = 0                      ! Initialze before next line.
      if(MAT.lt.0) then
      go to 100                      ! TEND = End of ENDF data
      else
      go to 50                       ! Otherwise continue reading
      endif
      endif
c-----------------------------------------------------------------------
c
c     special treatment MF/MT=1/451 = Add Documentation
c
c-----------------------------------------------------------------------
      if(MF.eq.1.and.MT.eq.451) then
      call FILE1
      go to 50
      endif
c-----------------------------------------------------------------------
c
c     No other special cases - translate 6 data fields to standard form
c
c-----------------------------------------------------------------------
      NOSEQ = NXTSEQ(NOSEQ)
      call STANDARD(6)
c-----Output standard format
      write(11,20) LINE66,MAT,MF,MT,NOSEQ
      go to 50
c-----------------------------------------------------------------------
c
c     End of ENDF input
c
c-----------------------------------------------------------------------
c-----End of File BEFORE TEND
   80 write( *,90)
      write( 3,90)
   90 format(1x,79('-')/
     1 ' ERROR - ENDF file does not end with TEND Line.'/1x,79('-'))
      call ENDERROR
c-----TEND line read
  100 call ENDIT
      end
      Subroutine FILE1
C=======================================================================
c
c     Add documentation to MF/MT=1/451
c
c     ENDF-4 Format: 2-nd line N1 > 0, N2 = 0
c         -5       : 3-rd line N1 = 0, N2 = 0
c         -6       : 4-th line N1 > 0, N2 > 0
c
C=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      character*1 LINE66,LINE44(44),LINE11(11),MTDICT(44)
      common/endf1/LINE66(11,6)
      common/endf2/MAT,MF,MT,NOSEQ,IVERSE
      equivalence (LINE66(1,1),LINE44(1)),(LINE66(1,6),LINE11)
c-----Initialize ENDF format version
      IVERSE = 0
c-----------------------------------------------------------------------
c
c     first line already read - translate C1 and C2 and output
c
c-----------------------------------------------------------------------
      NOSEQ = NXTSEQ(NOSEQ)
      call STANDARD(2)
      write(11,10) LINE66,MAT,MF,MT,NOSEQ
c-----------------------------------------------------------------------
c
c     Loop over UP TO next 3 lines = fewer for earlier ENDF versions.
c
c-----------------------------------------------------------------------
      do 60 iline=1,3
      read (10,10) LINE66,MAT,MF,MT
   10 format(66A1,i4,i2,i3,i5)
c-----Convert C1 and C2 fields
      NOSEQ = NXTSEQ(NOSEQ)
      call STANDARD(2)
c-----Define ENDF format version based on N1 and N2
      call in9(XN1,LINE66(1,5))
      call in9(XN2,LINE66(1,6))
      N1IN = XN1
      N2IN = XN2
c-----Based on which card read
      go to (20,30,40),iline
   20 N1A = N1IN
      N2A = N2IN
      if(N1A.le.0.or.N2A.ne.0) go to 50
      IVERSE = 4                              ! IV Format
      NWD    = N1A
      go to 70
   30 N1B = N1IN
      if(N2A.gt.0) go to 50
      IVERSE = 5                              ! V  Format
      NWD    = N1B
      go to 70
   40 N1C = N1IN
      IVERSE = 6                              ! VI Format
      NWD    = N1C
      go to 70
   50 write(11,10) LINE66,MAT,MF,MT,NOSEQ   ! Copy line = no change.
   60 CONTINUE
c-----Output updated comment count
   70 NWDOUT = NWD + 2
      write(11,80) LINE44,NWDOUT,LINE11,MAT,MF,MT,NOSEQ ! New count
   80 format(44A1,i11,11A1,i4,i2,i3,i5)
c-----------------------------------------------------------------------
c
c     Copy comments = insure first 22 coilumns are blank.
c
c-----------------------------------------------------------------------
      do iwd = 1,NWD
      read (10,10) LINE66,MAT,MF,MT
      NOSEQ = NXTSEQ(NOSEQ)
      write(11,10) LINE66,MAT,MF,MT,NOSEQ
      enddo
c-----------------------------------------------------------------------
c
c     Add comments + new sequence numbers
c
c-----------------------------------------------------------------------
      NOSEQ = NXTSEQ(NOSEQ)
      write(11,90) MAT,MF,MT,NOSEQ
      NOSEQ = NXTSEQ(NOSEQ)
      write(11,100) MAT,MF,MT,NOSEQ
   90 format(
     1 ' ',13('*'),'**** Program ENDF2C (Version 2023-1) ***',
     2 12('*'),i4,i2,i3,i5)
  100 format(5(' '),
     1 'Convert ENDF Data to Standard FORTRAN, C and C++ Format',
     2 6(' '),i4,i2,i3,i5)
c-----------------------------------------------------------------------
c
c     Copy to SEND
c
c-----------------------------------------------------------------------
  110 read (10,120) MTDICT,MAT,MF,MT
  120 format(22x,44A1,i4,i2,i3,i5)
      if(MT.gt.0) then
      NOSEQ = NXTSEQ(NOSEQ)
      write(11,120) MTDICT,MAT,MF,MT,NOSEQ
      go to 110
      endif
      NOSEQ = 99999                     ! SEND line
      write(11,130) MAT,MF,MT,NOSEQ
  130 format(66x,i4,i2,i3,i5)
      NOSEQ = 0                         ! Initialize for next line
      return
      end
      Subroutine STANDARD(NFIELD)
C=======================================================================
c
c     Scan ALL other lines
c
c     NFIELD = # of fields to scan = 2 or 6
c
C=======================================================================
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      save
      character*1 LINE66
      character*1 LINESAVE(11)
      common/endf1/LINE66(11,6)
      common/endf2/MAT,MF,MT,NOSEQ,IVERSE
c-----------------------------------------------------------------------
c
c     Scan fields for . No other test is necessary
c
c-----------------------------------------------------------------------
      do 30 i = 1,NFIELD
      do k = 1,11
      if(LINE66(k,i).eq.'.') go to 10
      enddo
c-----no . = nothing to change
      go to 30
c-----save original form
   10 do kk=1,11
      LINESAVE(kk) = LINE66(kk,i)
      enddo
c-----translate
      call IN9 (zin,LINE66(1,i))
c-----Try for 10 digit output - switch to 9 if negative.
      call OUT10(zin,LINE66(1,i))
c-----------------------------------------------------------------------
c
c     Compare original and final = Print if significantly different
c
c-----------------------------------------------------------------------
      call IN9 (zix,LINE66(1,i))
      if(DABS(zin).le.0.0d+0) then   ! Avoid divding by zero.
      diff = (zin - zix)
      else
      diff = (zin - zix)/zin
      endif
      if(dabs(diff).gt.1.0d-10) then
      write(*,20) LINESAVE,(LINE66(kk,i),kk=1,11),diff,
     1 MAT,MF,MT,NOSEQ,IVERSE
      write(3,20) LINESAVE,(LINE66(kk,i),kk=1,11),diff,
     1 MAT,MF,MT,NOSEQ,IVERSE
   20 format(1x,11a1,1x,11a1,2x,1pd20.12,i5,i3,i4,i6,i5)
      endif
c-----end of field loop
   30 continue
      return
      end
