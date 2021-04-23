C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


      subroutine starplot
c***********************************************************************
c
c       written by
c       ==========
c       Jay McGowan and Red Cullen
c       LLNL
c       L-298
c       P.O. Box 808
c       Livermore, Ca 94550
c
c       Please report all comments or errors to the authors.
c
c       purpose
c       =======
c       1) calcomp like graphics interface for SUN workstations
c       2) interactive graphics using mouse for SUN workstation
c
c       this routine can be used,
c       1) black and white - not pd7 sparc workstation
c          color           - if pd7 workstation
c       2) inside windows
c          not in windows
c
c       the workstation and whether or not in windows is
c       automatically determined by this routine - so this
c       routine can be used without any modifications for
c       any of the above combinations
c
c       use
c       ===
c       for calcomp like output and no mouse interaction only
c       use plots, plot and pen.
c
c       in either case to create a plot on the screen,
c       1) initialize the screen by calling plots
c       2) define the color of vectors calling pen (black/white - ignore
c       3) draw vectors on screen calling plot
c       4) end plot by calling plot - see plot arguments below
c       5) after all plots are completed terminate plotting by
c          calling plot - see plot arguments below
c
c       screen area
c       ===========
c       the screen is defined to extend from,
c       x   = -1.0 to 14.0
c       y   = -1.0 to 11.0
c       you should scale all x,y arguments to call plot to this range.
c       alternatively you may change the limits in SetWindow below
c       which presently says,
c
c       call SetWindow(-1.0, 14.0, -1.0, 11.0)
c
c       calcomp like graphics interface
c       ===============================
c       entry points
c       ============
c       starplot       = initialize plotting
c       nextplot       = pause at end of plot - then clear screen
c       endplots       = terminate plotting
c
c       plot(x,y,ipen) = draw, move, end plot, end plotting
c            x,y       = absolute coordinates for draw or move
c                ipen  =   2 - draw
c                      =   3 - move
c
c       pen(icolor)    = select color inverting 256 color table
c           icolor     = index to color (0 to any number)
c
c       pen1(icolor)   = select color directly in icolor order
c           icolor     = index to color (0 to any number)
c
c       backgd(icolor) = define background color
c
c       interactive graphics using mouse
c       ================================
c       entry points
c       ============
c       mousey(iway,x,y,iway1,iway2) = turn on mouse
c                                      test for mouse or keyboard input
c                                      turn off mouse
c                                iway  = 0 - none
c                                      = 1 - left mouse key
c                                      = 2 - center mouse key
c                                      = 3 - right mouse key
c                                      = 4 - keyboard
c                                   x  =     x coordinate if mouse key
c                                      =     0.0 otherwise
c                                   y  =     y coordinate if mouse key
c                                      =     0.0 otherwise
c                                iway1 = lowest allowable value of iway
c                                iway2 = highest allowable value of iway
c
c       sleepy(isleep) = delay for (isleep) seconds
c
c      call xinitialize
c
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      INTEGER*4 itemp,job,mymouse,penmap,mycolor,lcolor
      CHARACTER*44 MYFONT
      CHARACTER*1 MYFONT1
      character*1 keyhit
      dimension penmap(0:255),ired(256),igreen(256),iblue(256),
     1                   xc(2),yc(2),
     2 MYFONT1(44),XSIZE(2),YSIZE(2)
      EQUIVALENCE (MYFONT,MYFONT1(1))
      data mymouse/0/
      data lcolor/-9999/
      DATA MYFONT/
     1 '-adobe-courier-bold-r-*-*-25-*-*-*-*-*-*-*  '/
c-----indicate plotter has not been initialized.
      data imon/0/
c-----define plotting surface
      data xmin/0.0/
      data xmax/12.5/
      data ymin/0.0/
      data ymax/10.0/
c-----maintain perspective
      data myview/1/
c-----initialize background color to IBM blue
      data myback/183/
c***********************************************************************
c
c     initialize window
c
c***********************************************************************
c
c     initialize window
c
      imon=1
      call xpinit(xmin,xmax,ymin,ymax,myview)
      lastpen=0
      resolve=0.01
c
c     define type of screen - black/white = 1, otherwise color
c
      call myscreen(mycolor)
C***** DEBUG
C-----ACTIVATE TO SIMULATE BLACK/WHITE.
C     mycolor=1
C***** DEBUG
c
c     if black and white set all colors 0, except 0 = 1.
c
      if(mycolor.ne.1) go to 20
      penmap(0)=1
      myback=penmap(0)
      do 10 i=1,255
      penmap(i)=0
   10 continue
      return
   20 call paltab(penmap,ired,igreen,iblue,1)
c-----set new rgb data
      call pallette(ired,igreen,iblue)
c-----clear screen
      call xpclr()
      call boxfill(0.0,12.5,0.0,10.0,myback,-1)
C
C     LOAD FONT
C
      MYFONT1(43)=CHAR(0)
      CALL SETFONT(MYFONT)
      return
c
      ENTRY NEWBACK
C=======================================================================
C
C     SWITCH BETWEEN BLACK/WHITE BACKGROUNDS
C
C=======================================================================
c-----this assume with start with black background
      if(myback.eq.183) then
c-----switch to white background
      myback = penmap(1)
      i2     = penmap(2)
      i3     = penmap(3)
      i4     = penmap(4)
      i5     = penmap(5)   ! switch light and dark blue
      i8     = penmap(8)
      i10    = penmap(10)
      penmap(2) = 1
      penmap(3) = 1
      penmap(4) = i3
      penmap(5) = i8
      penmap(8) = i10
      penmap(10)= i5
      else
c-----switch to black background
      myback    = 183
      penmap(2) = i2
      penmap(3) = i3
      penmap(4) = i4
      penmap(5) = i5
      penmap(8) = i8
      penmap(10)= i10
      endif
c-----in all cases, switch 0 and 1 = assumed black and white
      i1 = penmap(0)
      penmap(0) = penmap(1)
      penmap(1) = i1
      RETURN
      ENTRY INTERACT(MYACTION)
C=======================================================================
C
C     INDICATE PROGRAM IS IN INTERACTIVE MODE
C
C=======================================================================
      MYACTION=1
      RETURN
      ENTRY MYSIZE(XSIZE,YSIZE)
C=======================================================================
C
C     DEFINE X AND Y SIZE OF SCREEN.
C
C=======================================================================
      XSIZE(1)=XMIN
      XSIZE(2)=XMAX
      YSIZE(1)=0.01
      YSIZE(2)=YMAX
      RETURN
      entry newpal(index)
c======================================================================
c
c     new rgb data and pen map
c
c     index = 1 - standard
c           = 2 - rainbow
c
c======================================================================
c-----ignore if black/write.
      if(mycolor.eq.1) return
c-----nothing to do if requested pallette is already loaded.
      if(index.eq.lastdex) return
      lastdex=index
      call paltab(penmap,ired,igreen,iblue,index)
c-----set new rgb data
      call pallette(ired,igreen,iblue)
c-----clear screen
      call xpclr()
      call boxfill(0.0,12.5,0.0,10.0,myback,-1)
      return
c***********************************************************************
c
c     draw or move
c
c***********************************************************************
      entry plot(xin, yin, ipen)
c-----always output if different type of output from last time
      if(ipen.ne.lastpen) go to 60
c-----otherwise, filter to resolution of screen
      if(abs(xin-xlast).ge.resolve.or.
     1   abs(yin-ylast).ge.resolve) go to 70
      return
c-----save parameters for filtering
   60 lastpen=ipen
   70 xlast=xin
      ylast=yin
c-----draw
      if(ipen.ne.2) go to 80
      call xpdraw(xin,yin)
      return
c-----move
   80 if(ipen.ne.3) return
      call xpmove(xin,yin)
      return
c***********************************************************************
c
c     end of a plot - delay and then clear screen.
c
c***********************************************************************
      entry nextplot
c-----nothing to do if plotter not initialized.
      if(imon.le.0) return
c-----if mouse is not being used, delay at end of plot.
c     if(mymouse.le.0) call xmouse(kway,xd,yd,4,4)
c-----clear screen.
      call xpclr()
      call boxfill(0.0,12.5,0.0,10.0,myback,-1)
      lastpen=0
      return
c***********************************************************************
c
c     clear the screen - no delay.
c
c***********************************************************************
      entry newplot
c-----nothing to do if plotter not initialized.
      if(imon.le.0) return
      call xpclr()
      call boxfill(0.0,12.5,0.0,10.0,myback,-1)
      return
c***********************************************************************
c
c     end of plotting.
c
c***********************************************************************
      entry endplots
c-----nothing to do if plotter not initialized.
      if(imon.le.0) return
      call xpdone(itemp,job)
      return
c***********************************************************************
c
c     select pen directly
c
c***********************************************************************
      entry pen1(icolor)
c-----ignore if same as current color.
      if(icolor.eq.lcolor) return
      lcolor=icolor
      call xpen(icolor)
      return
c***********************************************************************
c
c     select pen using pen map to insert order.
c
c***********************************************************************
      entry pen(icolor)
      if(mycolor.ne.1) go to 90
c-----black/white.
      kcolor=1
      if(icolor.eq.0) kcolor=0
      go to 100
   90 kcolor=mod(icolor,256)
  100 kcolor=penmap(kcolor)
c-----ignore if same as current color.
      if(kcolor.eq.lcolor) return
      lcolor=kcolor
      call xpen(kcolor)
      return
c***********************************************************************
c
c     select background color
c
c***********************************************************************
      entry backgd(icolor)
c
c     nothing to do if this is already the background color
c
      ii=penmap(0)
      if(icolor.eq.ii) return
c
c     if black and white reverse entire map
c
      if(mycolor.ne.1) go to 120
      do 110 i=1,255
      penmap(i)=ii
  110 continue
      penmap(0)=1-ii
      myback=penmap(0)
      call backcolor(penmap(0))
      return
c
c     for color switch the background and selected color
c
  120 kcolor=mod(icolor,256)
      penmap(0)=penmap(kcolor)
      penmap(kcolor)=ii
      call backcolor(penmap(0))
      return
c***********************************************************************
c
c     service mouse
c
c***********************************************************************
      entry mousey(iway,x,y,iway1,iway2)
c-----indicate mousey has been called - no delay between frames
      mymouse=1
      x=0.0
      y=0.0
c-----test mouse or keyboard input.
  150 call xmouse(iway,x,y,iway1,iway2)
      if(iway.gt.0) go to 160
c-----no input - keep testing if iway1 > 0
      if(iway1.gt.0) go to 150
      return
  160 if(iway.lt.4) go to 170
c-----input - unless requested ignore keyboard input.
      if(iway2.lt.4) go to 150
c-----otherwise use input.
      return
c-----ignore mouse outside scrreen area.
  170 if(x.lt.xmin.or.x.gt.xmax) go to 150
      if(y.lt.ymin.or.y.gt.ymax) go to 150
c-----otherwise use input.
      return
c***********************************************************************
c
c     draw and fill a rectangular box
c
c***********************************************************************
      entry boxcolor(xc,yc,mfill,mborder)
c-----ignore for black/white screens.
C     if(mycolor.eq.1) return
      ifill=-1
      ibord=-1
      if(mfill.lt.0) go to 180
      lfill=mod(mfill,256)
      if(mfill.ge.256.and.lfill.le.0) lfill=1
      ifill=penmap(lfill)
C***** DEBUG
C-----SPECIAL TREATMENT FOR IBM BLUE
      if(mfill.eq.183) ifill=183
C***** DEBUG
  180 if(mborder.lt.0) go to 190
      lfill=mod(mborder,256)
      if(mborder.ge.256.and.lfill.le.0) lfill=1
      ibord=penmap(lfill)
  190 call boxfill(xc(1),xc(2),yc(1),yc(2),ifill,ibord)
      return
c***********************************************************************
c
c     draw a box - either filled or not
c
c***********************************************************************
      entry boxdraw(x1,x2,y1,y2,kfill,kborder)
      ifill=-1
      ibord=-1
      if(kfill.lt.0) go to 200
      lfill=mod(kfill,256)
      if(kfill.ge.256.and.lfill.le.0) lfill=1
      ifill=penmap(lfill)
C***** DEBUG
C-----SPECIAL TREATMENT FOR IBM BLUE
      if(kfill.eq.183) ifill=183
C***** DEBUG
  200 if(kborder.lt.0) go to 210
      lfill=mod(kborder,256)
      if(kborder.ge.256.and.lfill.le.0) lfill=1
      ibord=penmap(lfill)
  210 call boxfill(x1,x2,y1,y2,ifill,ibord)
      return
c***********************************************************************
c
c     draw a polygon - either filled or not
c
c***********************************************************************
      entry polydraw(xtab,ytab,ntab,kfill,kborder)
      ifill=-1
      ibord=-1
      if(kfill.lt.0) go to 220
      lfill=mod(kfill,256)
      if(kfill.ge.256.and.lfill.le.0) lfill=1
      ifill=penmap(lfill)
C***** DEBUG
C-----SPECIAL TREATMENT FOR IBM BLUE
      if(kfill.eq.183) ifill=183
C***** DEBUG
  220 if(kborder.lt.0) go to 230
      lfill=mod(kborder,256)
      if(kborder.ge.256.and.lfill.le.0) lfill=1
      ibord=penmap(lfill)
  230 call polyfill(xtab,ytab,ntab,ifill,ibord)
      return
c***********************************************************************
c
c      read character from keyboard
c
c***********************************************************************
      entry key1(keyhit,iend)
      call pxkbd(keyhit,iend)
      return
c***********************************************************************
c
c      read character code (integer) from keyboard
c
c***********************************************************************
      entry key2(keyint)
      call pxkbd2(keyint)
      return
c***********************************************************************
c
c     empty display buffer and then delay for (isleep) seconds
c
c***********************************************************************
      entry sleepy(isleep)
      call flushit
      call sleep(isleep)
      return
      end
      FUNCTION KEYCODE()
C=======================================================================
C
C     READ 1 CHARACTER CODE FROM KEYBOARD
C
C     FOR SPECIAL CHARACTERS DEFINE IBM EQUIVALENT.
C     IGNORE CHARACTERS THAT ARE NOT USED.
C
C=======================================================================
C-----READ CHARACTER.
   10 CALL PXKBD2(KEY)
C-----USE NORMAL CODES
      IF(KEY.LE.255) GO TO 20
C-----IGNORE FUNCTION KEYS 3-12
      IF(KEY.GE.65472.AND.KEY.LE.65481) GO TO 10
      IF(KEY.EQ.65470) KEY=1059       ! F1       = REPEAT FIND
      IF(KEY.EQ.65471) KEY=1060       ! F2       = REPEAT CHANGE
      IF(KEY.EQ.65425) KEY=1073       ! PF1      = PAGE UP
      IF(KEY.EQ.65426) KEY=1081       ! PF2      = PAGE DOWN
      IF(KEY.EQ.65290) KEY=1071       ! LINEFEED = HOME
      IF(KEY.EQ.65387) KEY=1079       ! BREAK    = END
      IF(KEY.EQ.65507) KEY=1082       ! OPTION   = INSERT
      IF(KEY.EQ.65535) KEY=1083       ! DELETE
      IF(KEY.EQ.65293) KEY=13         ! RETURN
      IF(KEY.EQ.65307) KEY=27         ! ESCAPE
      IF(KEY.EQ.65362) KEY=1072       ! UP ARROW
      IF(KEY.EQ.65364) KEY=1080       ! DOWN ARROW
      IF(KEY.EQ.65361) KEY=1075       ! LEFT ARROW
      IF(KEY.EQ.65363) KEY=1077       ! RIGHT ARROW
      IF(KEY.EQ.65289) KEY=9          ! TAB
C-----NEWER KEYBOARD
      IF(KEY.EQ.65379) KEY=1082       ! INSERT
      IF(KEY.EQ.65360) KEY=1071       ! HOME
      IF(KEY.EQ.65365) KEY=1073       ! PAGE UP
      IF(KEY.EQ.65366) KEY=1081       ! PAGE DOWN
      IF(KEY.EQ.65367) KEY=1079       ! END
C-----IGNORE REMAINING SPECIAL CHARACTERS.
      IF(KEY.GE.10000) GO TO 10
C-----RETURN CHARACTER CODE
      KEYCODE=KEY
      RETURN
   20 IF(KEY.EQ.0) KEY=32             ! NULL = BLANK (SHIFT+SPACE BAR)
C-----RETURN CHARACTER CODE
      KEYCODE=KEY
      RETURN
      END
      SUBROUTINE SHOWTEXT(NX,NY,MESS,NMESS)
C=======================================================================
C
C     DISPLAY TEXT
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1 MESS,CURSOR,IBLANK
      DIMENSION MESS(80),XSPOT(0:81),YSPOT(0:30),XBSPOT(2,0:81),
     1 YBSPOT(2,0:30)
C-----ON FIRST PASS DEFINE POSITION OF CHARACTERS
      DATA IPASS/0/
C-----COLORS FOR CURSOR ON COLOR SCREEN.
      DATA ICC1/4/
      DATA ICC2/0/
C
C     INITIALIZATION - PREFORMED THE FIRST TIME THIS ROUTINE IS CALLED
C
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
      HTX=12.5/80.0
      HTY=9.9/30.0
      XSPOT(0)=0.0
      XSPOT(1)=0.5*HTX
      DO 10 I=2,81
      XSPOT(I)=XSPOT(I-1)+HTX
   10 CONTINUE
      YSPOT(0)=10.0-0.8*HTY
      DO 20 I=1,30
      YSPOT(I)=YSPOT(I-1)-HTY
   20 CONTINUE
      TAD=0.1*HTX
      DO 30 I=0,81
      XBSPOT(1,I)=XSPOT(I)-TAD
      XBSPOT(2,I)=XBSPOT(1,I)+HTX+TAD
   30 CONTINUE
      DO 40 I=1,30
      YBSPOT(1,I)=YSPOT(I)-TAD
      YBSPOT(2,I)=YSPOT(I)+HTX+TAD
   40 CONTINUE
C-----CURSOR OVER BLANK
      IBLANK=CHAR(2)
C-----DEFINE WHETHER COLOR OR BLACK AND WHITE
      CALL MYSCREEN(MYCOLOR)
      IF(MYCOLOR.GT.1) GO TO 50
C-----COLORS FOR CURSOR ON BLACK/WHITE SCREEN
      ICC1=1
      ICC2=0
C
C     LOOP OVER CHARACTERS.
C
   50 IX=NX
      DO 60 I=1,NMESS
      CALL RTEXTX(1,XSPOT(IX),YSPOT(NY),MESS(I),1)
      IX=IX+1
   60 CONTINUE
      CALL FLUSHIT
      RETURN
C=======================================================================
C
C     DISPLAY CURSOR
C
C=======================================================================
      ENTRY CURSEON(NXC,NYC,CURSOR)
C-----BOX AROUND CURSOR.
      CALL BOXCOLOR(XBSPOT(1,NXC),YBSPOT(1,NYC),ICC1,-1)
C-----NOTHING ELSE TO DO IF CHARACTER IS BLANK.
      IF(CURSOR.EQ.IBLANK) RETURN
C-----OTHERWISE CHARACTER WITHIN BOX.
      CALL PEN(ICC2)
      CALL RTEXTX(0,XSPOT(NXC),YSPOT(NYC),CURSOR,1)
      RETURN
C=======================================================================
C
C     REMOVE CURSOR
C
C=======================================================================
      ENTRY CURSEOFF(NXC,NYC,CURSOR)
C-----BLANK BOX AROUND CURSOR.
      CALL BOXCOLOR(XBSPOT(1,NXC),YBSPOT(1,NYC),0,-1)
C-----NOTHING ELSE TO DO IF CHARACTER IS BLANK.
      IF(CURSOR.EQ.IBLANK) RETURN
C-----OTHERWISE DISPLAY CHARACTER.
      CALL RTEXTX(1,XSPOT(NXC),YSPOT(NYC),CURSOR,1)
      RETURN
      END
      SUBROUTINE PIXSET(XP,YP,IPEN)
C=======================================================================
C
C     SET PIXEL COLOR
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CALL PEN(IPEN)
      CALL XYPIXEL(XP,YP)
      RETURN
      END
      SUBROUTINE PRINTIT
C***********************************************************************
C
C     PRINT SCREEN
C
C***********************************************************************
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1  NEXT1
      CHARACTER*56 NEXTPAGE
      DIMENSION NEXT1(56)
      EQUIVALENCE (NEXTPAGE,NEXT1(1))
      DATA SPAGE/0.0/
      DATA NEXTPAGE/
     1 'import -rotate 90                         SCREEN.       '/
c    1 'XtoPS -rotate 90                          SCREEN.       '/
C    1 'XtoPS -rotate 90 -geometry 612x792+67+92  SCREEN.       '/
C***** DEBUG - THIS IS THE STANDARD ORIGINAL GEOMETRY
C     THE OFFSETS IN X AND Y ARE ADDED TO THE SIZE TO DEFINE THE
C     BOX 67 92 544 700 (X = 67-544, Y = 92-700)
C    1 'XtoPS -rotate 90 -geometry 477x608+67+92  SCREEN.       '/
C       12345678901234567890123456789012345678901234567890123456
C                1         2         3         4         5
C***** DEBUG - THIS IS THE STANDARD ORIGINAL GEOMETRY
C-----INCREMENT NAME OF OUTPUT FILE.
      SPAGE=SPAGE+1.0
      CALL NUMBR2(SPAGE,-1,NEXT1(50),NN)
C-----INSURE EVERYTHING IS DISPLAYED
      CALL FLUSHIT
C-----PRINT SCREEN INTO OUTPUT FILE.
      CALL SYSTEM(NEXTPAGE)
      RETURN
      END
      SUBROUTINE SHOWSCRN(NX,NY,MESS,NMESS)
C=======================================================================
C
C     DISPLAY TEXT ONLY ON SCREEN - NOT HARD COPY.
C     OTHERWISE JUST A COPY OF SHOWTEXT
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1 MESS,IBLANK
      DIMENSION MESS(80),XSPOT(0:81),YSPOT(0:30),XBSPOT(2,0:81),
     1 YBSPOT(2,0:30)
C-----ON FIRST PASS DEFINE POSITION OF CHARACTERS
      DATA IPASS/0/
C-----COLORS FOR CURSOR ON COLOR SCREEN.
      DATA ICC1/4/
      DATA ICC2/0/
C
C     INITIALIZATION - PREFORMED THE FIRST TIME THIS ROUTINE IS CALLED
C
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
      HTX=12.5/80.0
      HTY=9.9/30.0
      XSPOT(0)=0.0
      XSPOT(1)=0.5*HTX
      DO 10 I=2,81
      XSPOT(I)=XSPOT(I-1)+HTX
   10 CONTINUE
      YSPOT(0)=10.0-0.8*HTY
      DO 20 I=1,30
      YSPOT(I)=YSPOT(I-1)-HTY
   20 CONTINUE
      TAD=0.1*HTX
      DO 30 I=0,81
      XBSPOT(1,I)=XSPOT(I)-TAD
      XBSPOT(2,I)=XBSPOT(1,I)+HTX+TAD
   30 CONTINUE
      DO 40 I=1,30
      YBSPOT(1,I)=YSPOT(I)-TAD
      YBSPOT(2,I)=YSPOT(I)+HTX+TAD
   40 CONTINUE
C-----CURSOR OVER BLANK
      IBLANK=CHAR(2)
C-----DEFINE WHETHER COLOR OR BLACK AND WHITE
      CALL MYSCREEN(MYCOLOR)
      IF(MYCOLOR.GT.1) GO TO 50
C-----COLORS FOR CURSOR ON BLACK/WHITE SCREEN
      ICC1=1
      ICC2=0
C
C     LOOP OVER CHARACTERS.
C
   50 IX=NX
      DO 60 I=1,NMESS
      CALL RTEXTX(1,XSPOT(IX),YSPOT(NY),MESS(I),1)
      IX=IX+1
   60 CONTINUE
      CALL FLUSHIT
      RETURN
      END
      SUBROUTINE PALTAB(PENMAP,RTAB,GTAB,BTAB,NPAL)
C=======================================================================
C
C     Version 2021-1 (Jan. 2021)
c     ==================================================================
C     DEFINE PALLETTE OF 256 RGB COLORS
C     NPAL  = 1 = MAXIMUM CONTRAST PALLETTE
C          <> 1 = RAINBOW PALLETTE
C
C=======================================================================
      INTEGER R1,G1,B1,R2,G2,B2,PAL1,PAL2,RTAB,GTAB,BTAB,PENMAP
      DIMENSION R1(256),G1(256),B1(256),R2(256),G2(256),B2(256),
     1 RTAB(256),GTAB(256),BTAB(256),PAL1(256),PAL2(256),PENMAP(256)
C-----MAXIMUM CONTRAST PALLETTE
      DATA PAL1/
     1   0, 15, 71, 23, 87,135,231, 39,167,119,215,247,103, 55,151,199,
     2  11,188, 76, 26, 90,138,236, 42,170,124,220,252,108, 60,156,204,
     3  10, 36, 67, 21, 83,131,227, 37,163,115,211,243, 99, 51,147,195,
     4   9,185, 56, 27, 89,137,234, 35,172,122,218,250,106,162,157,205,
     5   8, 92, 58, 25, 91,141,238, 54,136,125,221,253,109, 45,154,206,
     6  12,177, 72, 29, 88,142,237, 53,134,126,222,254,110, 44,153,202,
     7   6, 97, 74, 24,100,140,232, 41,164,123,203, 30, 65,244,155,111,
     8   7,113, 73, 28,101,171,233, 38,149,121,210, 31, 69, 17,152,127,
     9   4, 95, 70, 22,102,133,229, 40,187,120,213, 13, 66, 52,168,143,
     A 183,145, 75, 19, 84,139,217, 34,189,117,201,255, 80, 62,150,158,
     1  79,161, 77, 18,104,173,228, 48,190,116,197,249, 96, 49,169, 93,
     2 208,196, 47, 20,105,174,216, 16,207,114,212,239, 81,  3,184,191,
     3 160,193, 59,224,107,175,235, 64,165,159,226,245, 82, 61,182,144,
     4   5,209, 57,240, 86,166,214, 32,146,112,200,246, 68, 46,181,223,
     5   1,225, 78,241, 85,148,230, 33,130,118,198,248,129, 14,180,192,
     6   2,176, 43,242, 98,132,219, 50,186, 94,194,251,128, 63,179,178/
      DATA R1/
     1  16, 32, 48, 64, 80, 96,112,128,144,160,176,192,208,224,240,255,
     2  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     3  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     4  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     5  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     6  44, 56, 68, 80, 92,104,116,128,142,156,170,184,198,212,226,240,
     7   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     8   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     9   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     A   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     1   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     2   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     3  44, 56, 68, 80, 92,104,116,128,142,156,170,184,198,212,226,240,
     4  66, 84,102,120,138,156,174,192,199,206,213,220,227,234,241,248,
     5  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     6  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255/
      DATA G1/
     1  16, 32, 48, 64, 80, 96,112,128,144,160,176,192,208,224,240,255,
     2   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     3  44, 56, 68, 80, 92,104,116,128,142,156,170,184,198,212,226,240,
     4  66, 84,102,120,138,156,174,192,199,206,213,220,227,234,241,248,
     5  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     6  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     7  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     8  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     9  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     A  66, 84,102,120,138,156,174,192,199,206,213,220,227,234,241,248,
     1  44, 56, 68, 80, 92,104,116,128,142,156,170,184,198,212,226,240,
     2   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     3   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     4   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     5   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     6   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224/
      DATA B1/
     1  16, 32, 48, 64, 80, 96,112,128,144,160,176,192,208,224,240,255,
     2   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     3   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     4   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     5   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     6   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     7   0,  0,  0,  0,  0,  0,  0,  0, 28, 56, 84,112,140,168,196,224,
     8  55, 70, 85,100,115,130,145,160,170,181,191,202,212,223,233,244,
     9  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     A  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     1  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     2  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     3  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     4  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     5  88,112,136,160,184,208,232,255,255,255,255,255,255,255,255,255,
     6  22, 28, 34, 40, 46, 52, 58, 64, 85,106,127,148,169,190,211,232/
C-----RAINBOW PALLETTE
      DATA PAL2/
     1   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
     2  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
     3  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
     4  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
     5  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
     6  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
     7  96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111,
     8 112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,
     9 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
     A 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
     1 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
     2 176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
     3 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
     4 208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
     5 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
     6 240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255/
      DATA R2/
     1   0,255,240,224,208,192,176,160,144,128,112, 96, 80, 64, 48, 32,
     2 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     3 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     4 255,255,255,255,255,255,255,255,252,246,240,234,228,222,216,210,
     5 204,198,192,186,180,174,168,162,156,150,144,138,132,126,120,114,
     6 108,102, 96, 90, 84, 78, 72, 66, 60, 54, 48, 42, 36, 30, 24, 18,
     7   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     8   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     9   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     A   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     1   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     2  18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96,102,108,
     3 114,120,126,132,138,144,150,156,162,168,174,180,186,192,198,204,
     4 210,216,222,228,234,240,246,252,255,255,255,255,255,255,255,255,
     5 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     6 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255/
      DATA G2/
     1   0,255,240,224,208,192,176,160,144,128,112, 96, 80, 64, 48, 32,
     2  18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96,102,108,
     3 114,120,126,132,138,144,150,156,162,168,174,180,186,192,198,204,
     4 210,216,222,228,234,240,246,252,255,255,255,255,255,255,255,255,
     5 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     6 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     7 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     8 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     9 255,255,255,255,255,255,255,255,252,246,240,234,228,222,216,210,
     A 204,198,192,186,180,174,168,162,156,150,144,138,132,126,120,114,
     1 108,102, 96, 90, 84, 78, 72, 66, 60, 54, 48, 42, 36, 30, 24, 18,
     2   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     3   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     4   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     5   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     6   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
      DATA B2/
     1   0,255,240,224,208,192,176,160,144,128,112, 96, 80, 64, 48, 32,
     2   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     3   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     4   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     5   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     6   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     7  18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96,102,108,
     8 114,120,126,132,138,144,150,156,162,168,174,180,186,192,198,204,
     9 210,216,222,228,234,240,246,252,255,255,255,255,255,255,255,255,
     A 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     1 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     2 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     3 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     4 255,255,255,255,255,255,255,255,252,246,240,234,228,222,216,210,
     5 204,198,192,186,180,174,168,162,156,150,144,138,132,126,120,114,
     6 108,102, 96, 90, 84, 78, 72, 66, 60, 54, 48, 42, 36, 30, 24, 18/
      IF(NPAL.NE.1) GO TO 20
C-----MAXIMUM CONTRAST PALLETTE
      DO 10 I=1,256
      RTAB(I)=R1(I)
      GTAB(I)=G1(I)
      BTAB(I)=B1(I)
      PENMAP(I)=PAL1(I)
   10 CONTINUE
      RETURN
C-----RAINBOW PALLETTE
   20 IF(NPAL.NE.2) GO TO 40
      DO 30 I=1,256
      RTAB(I)=R2(I)
      GTAB(I)=G2(I)
      BTAB(I)=B2(I)
      PENMAP(I)=PAL2(I)
   30 CONTINUE
      RETURN
C-----SHADING PALLETTE
   40 IF(NPAL.NE.3) GO TO 90
C-----BASIC 16 COLORS
      DO 50 I=1,16
      K=PAL1(I)+1
      RTAB(I)=R1(K)
      GTAB(I)=G1(K)
      BTAB(I)=B1(K)
      PENMAP(I)=I-1
   50 CONTINUE
      DO 60 I=17,256
      PENMAP(I)=I-1
   60 CONTINUE
      RGBMULT=1.0
      DRGBMULT=1.0/22.0
      DO 80 I1=17,256,14
      I2=I1+13
      IF(I2.GT.256) I2=256
      KK=3
      RGBMULT=RGBMULT-DRGBMULT
      DO 70 I=I1,I2
      RTAB(I)=RGBMULT*RTAB(KK)
      GTAB(I)=RGBMULT*GTAB(KK)
      BTAB(I)=RGBMULT*BTAB(KK)
      KK=KK+1
   70 CONTINUE
   80 CONTINUE
   90 RETURN
      END
