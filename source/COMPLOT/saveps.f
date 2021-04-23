C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


      SUBROUTINE STARPLOT
C=======================================================================
C
C     2014 - Andrej Trkov Additions
C            MONEF = 0 =   1 plot   per file (old convention)
C                  = 1 = ALL plots in 1 file (new convention)
C
C     plotps  = uses MONEF = 0
C     plotps1 = uses MONEF = 1
C
C-----------------------------------------------------------------------
C     HARDSAVE
C     --------
C     HARD COPY VERSION OF CALCOMP LIKE GRAPHICS INTERFACE
C
C     THIS ROUTINE IS DESIGNED TO CREATE POSTSCRIPT PLOT FILES.
C
C     THE ONLY ENTRY POINTS ARE,
C
C     (1) STARPLOH - INITIALIZE PLOTTER
C     (2) NEXTPLOH - END OF A PLOT
C     (3) PLOTH    - MOVE OR DRAW
C
C     THIS VERSION DOES NOT OUTPUT TO PRINTER.
C     IT CREATES A SERIES OF FILES NAMED,
C     PLOT0001.ps
C     PLOT0002.ps
C     PLOT0003.ps
C     ETC.
C     THAT ARE LEFT ON DISK, FOR YOUR USE.
C
C     FOR OUTPUT DIRECTLY TO PRINTER USE HARDSEND.
C
C=======================================================================
      INCLUDE 'implicit4.h'
      CHARACTER*1  POST1
      CHARACTER*12 POSTSCRT
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
      COMMON/PLOTSTAR2/POSTSCRT
C* Trkov start
      COMMON/PLOTSTAR3/IPAGE,MONEF
C* Trkov end
      DIMENSION POST1(12),XSIZE(2),YSIZE(2)
      EQUIVALENCE (POSTSCRT,POST1(1))
C
C     DEFINE STANDARD PLOT SCALING.
C     WARNING - THESE LIMITS WILL MAP A PLOT FROM THE LOWER
C     PORTION OF THE SCREEN (OPTIONS AT TOP), TO A FULL PAGE.
C
C-----THIS ALLOWS FOR TOP BOXES TO BE IGNORED
      DATA XMIN/-0.4947/
      DATA XMAX/13.9947/
      DATA YMIN/-0.3/
      DATA YMAX/10.3/
C
C     ONLY INITIALIZE ONCE
C
      DATA MYPASS/0/
      IF(MYPASS.NE.0) RETURN
      MYPASS   = 1
C
C     INITIALIZE MYPLOT,
C     = 0 - TO SKIP THE FIRST PLOT (PROGRAM I.D.)
C     = 1 - TO PRINT ALL PLOTS
C
      MYPLOT   = 1
C-----INCH TO RASTER SCALING
      DX       = 738.0/(XMAX-XMIN)
      DY       = 540.0/(YMAX-YMIN)
C-----OUTPUT UNIT NUMBER
      IDEV     = 16
C-----DEFINE INITIAL POSTSCRIPT FILENAME
      POSTSCRT = 'PLOT0001.ps '
C-----LINE THICKNESS
      THICKNES = 0.6
C-----FILTERING PARAMETERS
      LASTPEN  = 0
      RESOLVE  = 0.003
C-----INITIALIZE STROKE COUNT
      MYCOUNT  = 0
      MYDRAW   = 0
      MYSTROKE = 0
C* Trkov start
C-----INITIALIZE PAGE COUNT AND ONE-FILE FLAG
      IPAGE=0
C-----Parameter MONEF is passed through COMMON/PLOTSTAR3/ and should be
C-----initialised in the calling program
C-----(0=separate files/1=single PostScript file)
      MONEF=0
C* Trkov end
      RETURN
      ENTRY PLOT    (X,Y,IPEN)
      ENTRY PLOTREAL(X,Y,IPEN)
C=======================================================================
C
C     PLOT ENTRY POINT
C     ================
C     X      = X COORDINATE (INCHES)
C     Y      = Y COORDINATE (INCHES)
C     IPEN   =   2 - DRAW
C            =   3 - MOVE
C            = OTHERWISE - IGNORE
C
C=======================================================================
C-----IGNORE STROKES FOR FIRST PLOT (PROGRAM I.D.)
      IF(MYPLOT.LE.0) RETURN
C-----IGNORE ALL EXCEPT DRAW AND MOVE
      IF(IPEN.NE.2.AND.IPEN.NE.3) RETURN
C
C     DRAW OR MOVE. OPEN FILE AND OUTPUT STANDARD HEADER INFORMATION
C     BEFORE FIRST STROKE OF EACH PLOT.
C
      IF(MYSTROKE.LE.0) CALL OPENPLOT
C-----SET IF DRAW
      IF(IPEN.EQ.2) MYDRAW=1
C-----ALWAYS USE IF NOT THE SAME TYPE OUTPUT AS PRECEEDING
      IF(IPEN.NE.LASTPEN) GO TO 20
C-----OTHERWISE FILTER TO RESOLUTION OF PLOTTER
      IF(ABS(X-XLAST).GE.RESOLVE.OR.
     1   ABS(Y-YLAST).GE.RESOLVE) GO TO 30
      RETURN
C-----SAVE COORDINATES FOR FILTERING
   20 LASTPEN=IPEN
   30 XLAST=X
      YLAST=Y
C-----SCALE X AND Y FOR OUTPUT.
      XX=DX*(X-XMIN)
      YY=DY*(Y-YMIN)
C-----TEST FOR DRAW.
      IF(IPEN.EQ.2) WRITE(IDEV,40) XX,YY
   40 FORMAT(2F6.1,' lineto')
C-----TEST FOR MOVE.
      IF(IPEN.EQ.3) WRITE(IDEV,50) XX,YY
   50 FORMAT(2F6.1,' moveto')
C-----COUNT STROKES IN CURRENT SET AND DRAW SET EVERY 100 STROKES.
      MYCOUNT=MYCOUNT+1
      IF(MYCOUNT.GE.100) THEN
      MYCOUNT=0
      WRITE(IDEV,60)
   60 FORMAT('stroke'/'newpath')
      WRITE(IDEV,50) XX,YY
      ENDIF
      RETURN
      ENTRY NEXTPLOT
C=======================================================================
C
C     END OF PLOT.
C
C=======================================================================
C-----NOTHING TO DO IF NEVER INITIALIZED OR NO DRAWS
      IF(MYPASS.LE.0.OR.MYDRAW.LE.0) RETURN
C-----RE-INITIALIZE STROKE COUNT.
      MYDRAW=0
      MYSTROKE=0
C-----IF ANY STROKES LEFT IN CURRENT SET DRAW THEM.
      IF(MYCOUNT.NE.0) WRITE(IDEV,70)
   70 FORMAT('stroke')
C-----FINISH PLOT FILE AND CLOSE.
      WRITE(IDEV,80)
   80 FORMAT('showpage')
C* Trkov start (do not close the file until all plots are drawn
c     CLOSE(IDEV)
      WRITE(*,82) '============== SENDING PLOT',IPAGE,' ============== '
   82 FORMAT(A,I4,A)
      IF(MONEF.EQ.0) THEN
        CLOSE(IDEV)
        IPAGE=0
      ENDIF
C*Trkov end
C
C     INCREMENT POSTSCRIPT FILENAME
C
      CALL NEXTNAME(POST1)
      RETURN
      ENTRY INTERACT(MYACTION)
C=======================================================================
C
C     INDICATE NOT IN INTERACTIVE MODE
C
C=======================================================================
      MYACTION=0
      RETURN
      ENTRY MYSIZE(XSIZE,YSIZE)
C=======================================================================
C
C     DEFINE LOCAL PLOTTER SIZEVE MODE
C
C=======================================================================
      XSIZE(1)=0.0
      XSIZE(2)=13.0
      YSIZE(1)=0.0
      YSIZE(2)=10.5
      RETURN
      END
      SUBROUTINE NEXTNAME(POST1)
C=======================================================================
C
C     INCREMENT FILENAME - INITIALLY POST1.001
C
C=======================================================================
      INCLUDE 'implicit4.h'
      CHARACTER*1 POST1,DIGITS
      DIMENSION POST1(12),DIGITS(0:9)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DO 30 I=8,5,-1
      DO 10 K=0,9
      IF(POST1(I).EQ.DIGITS(K)) GO TO 20
   10 CONTINUE
      RETURN
   20 IF(K.LT.9) GO TO 40
   30 POST1(I)='0'
      RETURN
   40 POST1(I)=DIGITS(K+1)
      RETURN
      END
      SUBROUTINE OPENPLOT
C=======================================================================
C
C     OPEN FILE FOR PLOTTING STROKES.
C
C=======================================================================
      INCLUDE 'implicit4.h'
      CHARACTER*1  POST1
      CHARACTER*12 POSTSCRT
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
      COMMON/PLOTSTAR2/POSTSCRT
C* Trkov start
      COMMON/PLOTSTAR3/IPAGE,MONEF
C* Trkov end
      DIMENSION POST1(12)
      EQUIVALENCE (POSTSCRT,POST1(1))
C* Trkov Start
C     OPEN(IDEV,FILE=POSTSCRT,STATUS='UNKNOWN')
C     WRITE(IDEV,10) MYPLOT,MYPLOT,THICKNES
C  10 FORMAT(
C    1 '%!PS-Adobe-2.0'/
C    2 '%%Creator: STARPLOT'/
C    3 '585 27 translate'/
C    4 '%%Page:',2I4/
C    5 '90 rotate'/
C    6 'newpath'/
C    7 F4.1,' setlinewidth'/
C    8 ' 1 setlinecap'/
C    9 ' 1 setlinejoin')
      IF(IPAGE.EQ.0 .OR. MONEF.EQ.0) THEN
        OPEN(IDEV,FILE=POSTSCRT,STATUS='UNKNOWN')
        WRITE(IDEV,10) '%!PS-Adobe-2.0'
        WRITE(IDEV,10) '%%Creator: STARPLOT'
      ENDIF
      IPAGE=IPAGE+1
      WRITE(IDEV,12) '%%Page:',IPAGE,IPAGE
      WRITE(IDEV,10) '585 27 translate'
      WRITE(IDEV,10) '90 rotate'
      WRITE(IDEV,10) 'newpath'
      WRITE(IDEV,14) THICKNES,' setlinewidth'
      WRITE(IDEV,10) ' 1 setlinecap'
      WRITE(IDEV,10) ' 1 setlinejoin'
   10 FORMAT(A)
   12 FORMAT(A,2I4)
   14 FORMAT(F4.1,A)
C* Trkov end
C-----INITIALIZE STROKE COUNT IN CURRENT SET OF STROKES.
      MYCOUNT  = 0
      LASTPEN  = 0
      MYSTROKE = 1    ! = 1 INDICATES FILE HAS BEEN NITIALIZED
      RETURN
      END
      SUBROUTINE PEN(IPEN)
C=======================================================================
C
C     CHANGE POSTSCRIPT COLOR.
C
C=======================================================================
      INCLUDE 'implicit4.h'
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
C**** COLOUR - (Patch provided by B.Zefran, tuned by A.Trkov, Aug-2002)
C----          extended and generalized by Red Cullen (November 2002)
      PARAMETER (MXCOL= 12)
      REAL*4 COLORS(3,0:MXCOL)
      DATA COLORS/
     1         0.,0.,0.,    ! 0 = black
     2         0.,0.,0.,    ! 1 = black
     3         0.,0.,0.,    ! 2 = black
     4         0.,0.,0.,    ! 3 = black
     5         1.,0.,0.,    ! 4 = red
     1         .0,.0,.8,    ! 10= dark blue
     a         .0,.8,.0,    ! 9 = dark green
     7         1.,0.,1.,    ! 6 = purple
     8         1.,.5,0.,    ! 7 = orange
     9         .8,.0,.0,    ! 8 = brown
     6         0.,0.,1.,    ! 5 = blue
     2         .8,.0,.8,    ! 11= dark purple
     3         .8,.4,0./    ! 12= dark orange
C
C     IF NOT YET USED, INITIALIZE PLOTTER
C
      IF(MYSTROKE.LE.0) CALL OPENPLOT
C-----CYCLE THROUGH COLORS
      N = ABS(IPEN)
C-----BEYOND MXCOL CYCLE FROM 3 TO MXCOL
      IF(N.GT.MXCOL) N = MOD(N-3,MXCOL-2) + 3
      WRITE(IDEV,10)
   10 FORMAT('stroke'/'newpath')
      WRITE(IDEV,20) COLORS(1,N),COLORS(2,N),COLORS(3,N)
   20 FORMAT(3F7.3,' setrgbcolor')
C-----IF NECESSARY, INCREMENT COUNT OF STROKES ON CURRENT PLOT.
      WRITE(IDEV,30)
   30 FORMAT(2F6.1,' moveto')
      RETURN
      END
      SUBROUTINE ENDPLOTS
C=======================================================================
C
C     END OF ALL PLOTTING - CALL NEXTPLOT, JUST IN CASE LAST PLOT
C     WAS NOT FINISHED BEFORE TERMINATING.
C
C=======================================================================
C* Trkov start
C     CALL NEXTPLOT
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
      COMMON/PLOTSTAR3/IPAGE,MONEF
      IF(MONEF.EQ.0) THEN
        CALL NEXTPLOT
      ELSE
        WRITE(IDEV,10) '%%Trailer'
        WRITE(IDEV,12) '%%Pages:',IPAGE
        WRITE(IDEV,10) '%%EOF'
        CLOSE(UNIT=IDEV)
      ENDIF
   10 FORMAT(A)
   12 FORMAT(A,I4)
C* Trkov end
      RETURN
      END
      SUBROUTINE BOXCOLOR(XT,YT,IC,IB)
C=======================================================================
C
C     FILL A REACTANGLE WITH COLOR - HERE ONLY BORDER IS DRAWN.
C
C=======================================================================
      INCLUDE 'implicit4.h'
      DIMENSION XT(2),YT(2)
C-----CANNOT FILL - USE IC TO AVOID COMPILER WARNING.
      IF(IC.LT.0) GO TO 10
C-----IF REQUESTED DRAW BORDER
   10 IF(IB.LT.0) RETURN
      CALL PLOTREAL(XT(1),YT(1),3)
      CALL PLOTREAL(XT(1),YT(2),2)
      CALL PLOTREAL(XT(2),YT(2),2)
      CALL PLOTREAL(XT(2),YT(1),2)
      CALL PLOTREAL(XT(1),YT(1),2)
      RETURN
      END
C=======================================================================
C
C     DUMMY ROUTINES
C
C=======================================================================
      SUBROUTINE NEWBACK
c
      INCLUDE 'implicit4.h'
      RETURN
      END
      SUBROUTINE MOUSEY(IWAY,X,Y,IWAY1,IWAY2)
c
      INCLUDE 'implicit4.h'
      IWAY=4
      X=0.0
      Y=0.0
      RETURN
      END
      SUBROUTINE PRINTIT
c
      INCLUDE 'implicit4.h'
      RETURN
      END
      SUBROUTINE SHOWTEXT(I,J,A,K)
c
      INCLUDE 'implicit4.h'
      CHARACTER*1 A
      DIMENSION A(K)
      RETURN
      END
