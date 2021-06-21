C This file is part of PREPRO.
C
C    Author: Dermott (Red) Cullen
C Copyright: (C) International Atomic Energy Agency
C
C PREPRO is free software; you can redistribute it and/or modify it
C under the terms of the MIT License; see LICENSE file for more details.


      subroutine starplot
c=======================================================================
c
c       Version 2021-1 (Jan. 2021)
c       ================================================================
c       Screen0.f
c       =========
c       This is the original microsft.f used by evalplot and complot
c       before trying to combine microsft.f and starplot.f into one
c       common routine - this didn't work.
c
c=======================================================================
c
c       Graphics interface for IBM-PC running unders Windows/NT
c       using MICROSOFT Powerstation 4.0 FORTRAN Graphics.
c
c       Use with Standard (MWs), NOT Quick Win (MW), for one FULL
C       screen Window.
c       ----------------------------------------------------------------
c       Interactive or Non-Interactive
c       ------------------------------
c       The STARPLOT interface can be used for interactive and/or
c       non-interactive (Postscript output) application codes. This
c       is an interactive code.
c
c       For     interactive codes INTERACT will return 1 (as here)
c       For non-interactive codes INTERACT will return 0.
c
c       Graphics Interface
c       ----------------------------------------------------------------
c       initialize, next plot, terminate, scaling, interaction
c       ----------------------------------------------------------------
c    *  starplot   = initialize plotting
c    *  nextplot   = blank entire screen for next plot
c    *  nuscreen   = blank lower area for next plot, leave upper options
c    *  endplots   = terminate plotting
c    *  mysize     = define (x,y) inch limits for users
c    *  interact   = indicate application is interactive
c       ----------------------------------------------------------------
c       color      - MUST be used by all routines prior to graphics
c       ----------------------------------------------------------------
c    *  pen        = set color for ALL following output
c    *  newpal     = change 256 color pallette
c       ----------------------------------------------------------------
c       graphic output
c       ----------------------------------------------------------------
c       vectors and points
c       ------------------
c    *  plotreal   = move/draw on screen and Postscript output line
c    *  plot       = move/draw on screen                output line
c    *  pixset     = set pixel color
c       -----
c       areas
c       -----
c    *  boxcolor   = fill a reactangle with color (x(2),y(2))
c    *  boxdraw    = fill a reactangle with color (x1,x2,y1,y2)
c    *  polyfill   = fill a polygon with color
c       ----------------------------------------------------------------
c       character input and output
c       ----------------------------------------------------------------
c    *  keycode    = read character code from keyboard
c    *  showtext   = display text in 80 by 30 layout
c    *  redtext    = initialize to display characters
c    *  curseon    = display cursor
c    *  curseoff   = remove cursor
c       ----------------------------------------------------------------
c       mouse input
c       ----------------------------------------------------------------
c    *  mousey     = mouse (x,y) coordinate input
c       ----------------------------------------------------------------
c       screen capture
c       ----------------------------------------------------------------
c    *  printit    = save entire screen to file as bitmap
c       ----------------------------------------------------------------
c       dummies    - for compatibility with other systems
c       ----------------------------------------------------------------
c    *  flushit    = flush everything to screen (UNIX only, dummy here)
c
c=======================================================================
c
c       Initialize window
c
c=======================================================================
      use dflib
      use dfwin
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
c-------window information
      type (qwinfo) winfo
c-------define limits of top of screen for nuscreen to leave option
c-------boxes at top of screen - yblank is used to blank lower portion
c-------of the screen
      real*4 xtopper,ytopper
      common/topxy/xtopper(2),ytopper(2)
c-------window hook parameters
      integer*4 hookhandle, primarythreadid
      common/redhooks/ hookhandle, primarythreadid
c-------scaling and flags
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
      integer*4 myback
      common/backmy/myback
      integer*4 penmap
      common/mappen/penmap(0:255)
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
      integer*4 status,myaction,kway
      integer*2 oldcolor
      real*4 xwidth,ywidth,xn,yn
      integer*4 i1,i2,i3,i4,i5,i8,i10
c-------xinches, yinches = xsize, ysize returned when mysize is called
c-------define size of plotting area in inches (no common initialized)
      real*4 xinches(2),yinches(2),yblank(2)
c-----------------------------------------------------------------------
c
c       callback initialization
c
c-----------------------------------------------------------------------
c-------set up name for our callback routine
      external KeyBoardProc
      !DEC$ ATTRIBUTES ALIAS: '_KeyBoardProc@12' :: KeyBoardProc
 
      myback = 183
c-------set hook for keyboard events
      hookhandle = SetWindowsHookEX(WH_KEYBOARD,
     &   loc(KeyBoardProc), 0, primarythreadid)
      if(hookhandle .eq. 0) then
      print *,'ERROR: could not set hook'
      stop
      endif
c-----------------------------------------------------------------------
c
c       Graphics routine to initialize FULL screen Window when
c       Standard Graphics is used (MWs), NOT Quick Win (MW).
c
c-----------------------------------------------------------------------
c-------define max. allowed window dimensions
      status = getwsizeqq(qwin$framewindow,qwin$sizemax,winfo)
c-------set window to max. dimensions
      winfo.type = QWIN$MAX
      status = setwsizeqq(qwin$framewindow,winfo)
c-------focus on main window
      status = focusqq(qwin$framewindow)
c-------set to exit without prompt
      status = setexitqq(qwin$exitnopersist)
c-------remove cursor
      status = displaycursor($gcursoroff)
c-------pass all characters to window
c     status = passdirkeysqq(.true.)
c-----------------------------------------------------------------------
c
c       Red's graphics
c       ==============
c       initialize parameters and define scaling from raster to inches
c       and inches to raster. Leave (x,y) room so initial image is
c       totally on screen.
c
c-----------------------------------------------------------------------
c-------define size of plotting area in inches (no common initialized)
      xsize(1) = 0.0
      xsize(2) = 13.5
      ysize(1) = 0.0
      ysize(2) = 10.0
c-------define (x,y) raster limits of Window to insure everything
c-------is initially on screen.
      xlow   = winfo.x + 2
      xwidth = winfo.w - 22
      xhigh  = xlow + xwidth
      ylow   = winfo.y + 2
      ywidth = winfo.h - 20
      yhigh  = ylow + ywidth
c-------define (x,y) pixel to inch and inch to pixel transformations
      dxin2p = xwidth/(xsize(2)-xsize(1))
      dyin2p = ywidth/(ysize(2)-ysize(1))
      dxp2in = 1.0/dxin2p
      dyp2in = 1.0/dyin2p
c-------define inch to pixel and pixel to inch bases
      xinch = xlow     - dxin2p*xsize(1)
      yinch = yhigh    + dyin2p*ysize(1)
      xpixel  = xsize(1) - dxp2in*xlow
      ypixel  = ysize(1) + dyp2in*yhigh
c-------initialize last color and pallette
      lastcolor  = -1
      lastpenmap = -1
c-------initialize mouse not yet used
      mouseon = 0
c-------start with maximum contrast pallette
      call newpal(1)
c-------initialize to display text
      call redtext
      return
      entry nextplot
c=======================================================================
c
c       next plot - blank entire screen.
c
c=======================================================================
c-------delay if mouse is not being used
      if(mouseon.eq.0) then
      call mousey(kway,xn,yn,1,4)
      mouseon = 0
      endif
c-------clear the screen
      call boxcolor(xsize,ysize,183,-1)
      return
      entry nuscreen
c=======================================================================
c
c       next plot - blank lower portion of screen.
c                   leave upper portion (option boxes) untouched
c
c=======================================================================
c-------delay if mouse is not being used
      if(mouseon.eq.0) then
      call mousey(kway,xn,yn,1,4)
      mouseon = 0
      endif
C-------clear lower portion of screen
      yblank(1) = ysize(1)
      yblank(2) = ytopper(1)
      call boxcolor(xsize,yblank,183,-1)
      return
      entry newback
c=======================================================================
c
c     new background color
c
c=======================================================================
      if(myback.eq.183) then
c-----switch to white background
      myback = penmap(1)
      i2     = penmap(2)
      i3     = penmap(3)
      i4     = penmap(4)
      i5     = penmap(5)   ! switch light and dark blue
      i8     = penmap(8)
      i10    = penmap(10)  ! switch dark purple and light blue
      penmap(2) = 1
      penmap(3) = 1
      penmap(4) = i3
      penmap(5) = i8
      penmap(8) = i10
      penmap(10)= i5
      else
c-----switch to black background
      myback = 183
      penmap(2) = i2
      penmap(3) = i3
      penmap(4) = i4
      penmap(5) = i5
      penmap(8) = i8
      penmap(10)= i10
      endif
c-----in all cases switch 0 to 1 = assume black and white
      i1 = penmap(0)
      penmap(0) = penmap(1)
      penmap(1) = i1
      return
      entry endplots
c=======================================================================
c
c       terminate plotting
c
c=======================================================================
      return
      entry mysize(xinches,yinches)
c=======================================================================
c
c       define size of window in inches
c
c=======================================================================
      xinches(1) = 0.0
      xinches(2) = 13.5
      yinches(1) = 0.0
      yinches(2) = 10.0
      return
      entry interact(myaction)
c=======================================================================
c
c       indicate code is interactive
c
c=======================================================================
      myaction = 1
      return
      end
      subroutine pen(icolor)
c=======================================================================
c
c       define color  - 256 colors (0 to 255)
c       0 - is always black
c       1 - is always white
c       for icolor > 255, ignore black and white and use the other
c       254 colors in rotation from 2 to 255.
c       183 = ALWAYS IBM Blue
c
c=======================================================================
      use dflib
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
      integer*4 myback
      common/backmy/myback
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
      integer*2 newcolor,oldcolor
      integer*4 icolor,mcolor,index,loop,status
C-------color map and RGB pallette
      integer*4 penmap,ired,igreen,iblue,irgb
      common/mappen/penmap(0:255)
      DIMENSION ired(0:255),igreen(0:255),iblue(0:255),
     1  irgb(0:255)
c-----------------------------------------------------------------------
c
c       nothing to do if same as last color
c
c-----------------------------------------------------------------------
      if(icolor.eq.lastcolor) return
      lastcolor = icolor
c-------up to 256 colors (0 to 255)
c-------0 and 1 are always black and white
c-------after 255 use colors in rotation from 2 to 255
      mcolor = icolor
      if(mcolor.gt.255) mcolor = 2 + mod(mcolor-256,254)
c-------permute colors into penmap order
      newcolor = penmap(mcolor)
c-------if icolor = 183, use IBM blue directly
      if(icolor.eq.183) newcolor=myback
c-------define both graphics and text color
      oldcolor = setcolor(newcolor)
      oldcolor = settextcolor(newcolor)
      return
      entry newpal(index)
c======================================================================
c
c     new rgb data and pen map
c
c     index = 1 - standard
c           = 2 - rainbow
c
c======================================================================
c-------nothing to do if pallette is already loaded
      if(index.eq.lastpenmap) return
      lastpenmap = index
c-------define penmap and RGB colors
      call paltab(penmap,ired,igreen,iblue,index)
c-------note - colors are defined in BGR, not RGB order.
      do loop = 0,255
      irgb(loop) = (256*iblue(loop)+igreen(loop))*256+ired(loop)
      enddo
      status = remapallpalettergb(irgb)
c-------set background color to blue
      status = setbkcolor(myback)
c-------clear the screen
      call boxcolor(xsize,ysize,183,-1)
      return
      end
      subroutine plotreal(xp,yp,ipen)
c=======================================================================
c
c       draw or move
c
c       ipen = 2 = draw
c            = 3 = move
c            = otherwise, nothing
c
c=======================================================================
      use dflib
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
c-------(x,y) coordinates
      type (xycoord) xynow
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
      integer*4 ipen,ipix,status
      integer*2 ixp,iyp
      real*4 xp,yp,xpix,ypix
c-----------------------------------------------------------------------
c
c       HARDCOPY?
c
c-----------------------------------------------------------------------
c-------translate x,y to plot coordinates
      ixp = xinch + dxin2p*xp
      iyp = yinch - dyin2p*yp
c-------draw or move
      if(ipen.eq.2) status = LINETO(ixp,iyp)
      if(ipen.eq.3) call MOVETO(ixp,iyp,xynow)
      return
      entry plot(xp,yp,ipen)
c======================================================================
c
c       same as plotreal - no postscript output
c
c======================================================================
c-------translate x,y to plot coordinates
      ixp = xinch + dxin2p*xp
      iyp = yinch - dyin2p*yp
c-------draw or move
      if(ipen.eq.2) status = LINETO(ixp,iyp)
      if(ipen.eq.3) call MOVETO(ixp,iyp,xynow)
      return
      entry pixset(xpix,ypix,ipix)
c=======================================================================
c
c       set pixel to color
c
c=======================================================================
c-------translate x,y to plot coordinates
      ixp = xinch + dxin2p*xpix
      iyp = yinch - dyin2p*ypix
c-------set color
      call pen(ipix)
c-------set pixel
      status = setpixel(ixp,iyp)
      return
      end
      subroutine boxcolor(xbox,ybox,ifill,ibord)
c=======================================================================
c
c       fill a rectangle with color - 2 coordinate arguments
c
c=======================================================================
      use dflib
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
c-------(x,y) coordinates
      type (xycoord) poly(5000)
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
c-------xbox,  ybox  = reactangle used by boxcolor
      integer*2 ixp1,ixp2,iyp1,iyp2
      integer*4 idum,ifill,ibord,ip,npoly,ifpoly,ibpoly
      real*4 xbox,ybox
      dimension xbox(2),ybox(2)
      integer*2  status,mpoly
      real*4 xpoly,ypoly,xbox1,xbox2,ybox1,ybox2
      dimension xpoly(npoly),ypoly(npoly)
c-----------------------------------------------------------------------
c
c       translate x,y to plot coordinates
c
c-----------------------------------------------------------------------
      ixp1 = xinch + dxin2p*xbox(1)
      iyp1 = yinch - dyin2p*ybox(1)
      ixp2 = xinch + dxin2p*xbox(2)
      iyp2 = yinch - dyin2p*ybox(2)
c-------ignore if box has no interior
      if(ixp1.eq.ixp2) return
      if(iyp1.eq.iyp2) return
c-------insure coordinates are in ascending order
      if(ixp1.gt.ixp2) then
      idum = ixp1
      ixp1 = ixp2
      ixp2 = idum
      endif
      if(iyp1.gt.iyp2) then
      idum = iyp1
      iyp1 = iyp2
      iyp2 = idum
      endif
c-------fill?
      if(ifill.ge.0) then
      call pen(ifill)
      status = rectangle($gfillinterior,ixp1,iyp1,
     1                                    ixp2,iyp2)
      endif
c-------border?
      if(ibord.ge.0) then
      call pen(ibord)
      call plotreal(xbox(1),ybox(1),3)
      call plotreal(xbox(1),ybox(2),2)
      call plotreal(xbox(2),ybox(2),2)
      call plotreal(xbox(2),ybox(1),2)
      call plotreal(xbox(1),ybox(1),2)
      endif
      return
      entry boxdraw(xbox1,xbox2,ybox1,ybox2,ifill,ibord)
c=======================================================================
c
c       fill a rectangle with color - 4 coordinate arguments
c
c=======================================================================
c-------translate x,y to plot coordinates
      ixp1 = xinch + dxin2p*xbox1
      iyp1 = yinch - dyin2p*ybox1
      ixp2 = xinch + dxin2p*xbox2
      iyp2 = yinch - dyin2p*ybox2
c-------ignore if box has no interior
      if(ixp1.eq.ixp2) return
      if(iyp1.eq.iyp2) return
c-------insure coordinates are in ascending order
      if(ixp1.gt.ixp2) then
      idum = ixp1
      ixp1 = ixp2
      ixp2 = idum
      endif
      if(iyp1.gt.iyp2) then
      idum = iyp1
      iyp1 = iyp2
      iyp2 = idum
      endif
c-------fill?
      if(ifill.ge.0) then
      call pen(ifill)
      status = rectangle($gfillinterior,ixp1,iyp1,
     1                                    ixp2,iyp2)
      endif
c-------border?
      if(ibord.ge.0) then
      call pen(ibord)
      call plotreal(xbox1,ybox1,3)
      call plotreal(xbox1,ybox2,2)
      call plotreal(xbox2,ybox2,2)
      call plotreal(xbox2,ybox1,2)
      call plotreal(xbox1,ybox1,2)
      endif
      return
      entry polyfill(npoly,xpoly,ypoly,ifpoly,ibpoly)
c=======================================================================
c
c       fill a polygon with color
c
c=======================================================================
c-----------------------------------------------------------------------
c
c       a polygon needs at least 3 points and less than 5,000
c
c-----------------------------------------------------------------------
      if(npoly.lt.3.or.npoly.ge.5000) return
c-----------------------------------------------------------------------
c
c       define polygon coordinates in raster units
c
c-----------------------------------------------------------------------
      do ip=1,npoly
      poly(ip).xcoord = xinch + dxin2p*xpoly(ip)
      poly(ip).ycoord = yinch - dyin2p*ypoly(ip)
      enddo
c-------close polygon
      poly(npoly+1).xcoord = xinch + dxin2p*xpoly(1)
      poly(npoly+1).ycoord = yinch - dyin2p*ypoly(1)
      mpoly = npoly + 1
c-----------------------------------------------------------------------
c
c       fill?
c
c-----------------------------------------------------------------------
      if(ifpoly.ge.0) then
      call pen(ifpoly)
      status = polygon($gfillinterior,poly,mpoly)
      endif
c-----------------------------------------------------------------------
c
c       border?
c
c-----------------------------------------------------------------------
      if(ibpoly.ge.0) then
      call pen(ibpoly)
      status = polygon($gborder      ,poly,mpoly)
      endif
      return
      end
      subroutine showtext(ixt,iyt,chart,nchart)
c=======================================================================
c
c       display text
c
c=======================================================================
      use dflib
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
      real*4 dx,dy,dx2
      integer*2 oldcolor
      integer*4 nowcolor,ixt,iyt,kxt,kyt,ipass,loop,nchart,nxc,nyc
      character*1 chart,chart1,cursor,imblank
      real*4 xgrid,ygrid,xbox,ybox,xspot,yspot,tad
      dimension chart(nchart),xgrid(0:80),ygrid(0:30),
     1  xbox(2),ybox(2),xspot(2,0:81),yspot(2,0:30)
c-----------------------------------------------------------------------
c
c       ignore illegal character positions
c
c-----------------------------------------------------------------------
      if(iyt.lt.0.or.iyt.gt.29) return
      if(ixt.lt.1.or.(ixt+nchart).gt.81) return
c-----------------------------------------------------------------------
c
c       blank before write
c
c-----------------------------------------------------------------------
c-------save current color
      nowcolor = lastcolor
      call boxdraw(xspot(1,ixt),xspot(2,ixt+nchart-1),
     1  yspot(1,iyt),yspot(2,iyt),0,-1)
c-------reset color
      call pen(nowcolor)
c-----------------------------------------------------------------------
c
c       plot characters 1 at a time by position index
c
c-----------------------------------------------------------------------
      kxt = ixt
      kyt = iyt
      do loop = 1, nchart
      chart1 = chart(loop)
      call plot(xgrid(kxt),ygrid(kyt),3)
      call outgtext(chart1)
      kxt = kxt + 1
      enddo
      return
      entry redtext
c=======================================================================
c
c       initialize to display text
c
c=======================================================================
      imblank = char(2)
      dx=xsize(2)/80.0
      dy=ysize(2)/30.0
      tad = 0.1*dx
c-------position of characters
      xgrid(0) =-0.5*dx
      xgrid(1) = 0.5*dx
      do loop = 2,80
      xgrid(loop) = xgrid(loop-1) + dx
      enddo
      ygrid(0) = ysize(2) - 0.1*dy
      do loop = 1,30
      ygrid(loop) = ygrid(loop-1) - dy
      enddo
c-------position of blanks around characters
      dx2 = 0.5*dx
      do loop = 0,80
      xspot(1,loop) = xgrid(loop) - tad
      xspot(2,loop) = xspot(1,loop) + dx + tad
      enddo
      do loop = 0,30
      yspot(1,loop) = ygrid(loop) + tad
      yspot(2,loop) = yspot(1,loop) - dy - tad
      enddo
c-------initialize fonts
      oldcolor = initializefonts()
c-------over 1000
      if(xhigh.ge.950.0) then
      oldcolor = setfont('t''Courier''h24w10b')
      else
c-------800
      if(xhigh.ge.750.0) then
      oldcolor = setfont('t''Courier''h20w8b')
      else
c-------640
      oldcolor = setfont('t''Courier''h16w6b')
      endif
      endif
      return
      entry curseon(nxc,nyc,cursor)
c=======================================================================
c
c       display cursor
c
c=======================================================================
c-------box around cursor
      call boxdraw(xspot(1,nxc),xspot(2,nxc),
     1  yspot(1,nyc),yspot(2,nyc),1,-1)
c-------nothing to do if cursor is blank
      if(cursor.eq.imblank) return
      call plot(xgrid(nxc),ygrid(nyc),3)
      call pen(0)
      call outgtext(cursor)
      return
      entry curseoff(nxc,nyc,cursor)
c=======================================================================
c
c       display cursor
c
c=======================================================================
c-------box around cursor
      call boxdraw(xspot(1,nxc),xspot(2,nxc),
     1  yspot(1,nyc),yspot(2,nyc),0,-1)
c-------nothing to do if cursor is blank
      if(cursor.eq.imblank) return
      call plot(xgrid(nxc),ygrid(nyc),3)
      call pen(1)
      call outgtext(cursor)
      return
      end
      subroutine mousey(imouse,xmouse,ymouse,imouse1,imouse2)
c=======================================================================
c
c       mouse handler
c
c       imouse   = mouse key pressed (here always 1)
c       xmouse   = x coordinate
c       ymouse   = y coordinate
c       imouse1  = minimum imouse (left, center, right)
c       imouse2  = maximum imouse (left, center, right)
c
c       imouse1 and imouse2 are not used here.
c
c=======================================================================
      use dflib
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
      integer*4 status,keystate,imouse,imouse1,imouse2
      integer*4 mrastx,mrasty
      real*4 xmouse,ymouse
c-----------------------------------------------------------------------
c
c       use imouse1 and imouse2 to prevent compiler warning
c
c-----------------------------------------------------------------------
      if(imouse1.eq.-7777.or.imouse2.eq.-7777) return
c-------indicate mouse is being used = no delay between plots
      mouseon = 1
c-------display cursor
      status = displaycursor($gcursoron)
c-------wait for mouse button to be pressed
      status = waitonmouseevent(mouse$lbuttondown.or.mouse$rbuttondown
     1   ,keystate,mrastx,mrasty)
c-------wait for mouse button to be released
      status = waitonmouseevent(mouse$lbuttonup  .or.mouse$rbuttonup
     1   ,keystate,mrastx,mrasty)
c-------remove cursor
      status = displaycursor($gcursoroff)
c-------translate from pixels to inches
      xmouse  = xpixel  + dxp2in*mrastx
      ymouse  = ypixel  - dyp2in*mrasty
      imouse = 1
      return
      end
      subroutine printit
c=======================================================================
c
c       print screen
c
c=======================================================================
      use dflib
      implicit none
      save
c-----------------------------------------------------------------------
c
c       common
c
c-----------------------------------------------------------------------
      real*4 xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize,ysize,xinch,yinch,xpixel,ypixel
      integer*4 mouseon,lastcolor,lastpenmap
      common/redgraph/xlow,xhigh,ylow,yhigh,dxin2p,dyin2p,
     1  dxp2in,dyp2in,xsize(2),ysize(2),xinch,yinch,xpixel,ypixel,
     2  mouseon,lastcolor,lastpenmap
c-----------------------------------------------------------------------
c
c       local
c
c-----------------------------------------------------------------------
      integer*2 ixl2,iyl2,ixr2,iyr2
      integer*4 ixl4,iyl4,ixr4,iyr4,isize,status
      character*16 filnam
      character*1  filnam1(16)
      equivalence (filnam,filnam1(1))
c-----------------------------------------------------------------------
c
c       zero terminated file name
c
c-----------------------------------------------------------------------
      filnam='scrn0001.bmp    '
      filnam1(13) = char(0)
      filnam1(14) = char(0)
      filnam1(15) = char(0)
      filnam1(16) = char(0)
c-------integer*2 for imagesize
      ixl2 = xlow   !  20 using 640 x 480 with 20 pixel border
      iyl2 = ylow   !  20
      ixr2 = xhigh  ! 620
      iyr2 = yhigh  ! 460
      isize = imagesize(ixl2,iyl2,ixr2,iyr2)
c-------integer*4 for saveimage
      ixl4 = xlow   !  20 using 640 x 480 with 20 pixel border
      iyl4 = ylow   !  20
      ixr4 = xhigh  ! 620
      iyr4 = yhigh  ! 460
      status=saveimage  (filnam,ixl4,iyl4,ixr4,iyr4)
      write(3,3000) status,isize,ixl4,iyl4,ixr4,iyr4
 3000   format(' PRINTIT=',6I10)
      return
      end
c=======================================================================
c
c       dummy routines - only used in UNIX version of STARPLOT interface
c
c=======================================================================
      subroutine flushit
      return
      end
      function keycode()
c=======================================================================
c
c     read 1 character from the keyboard
c
c=======================================================================
      use dflib
      use dfwin
      implicit none
      save
      character*1 key1,key2
      integer*4 status,keycode,ikey1,ikey2,ikey3
 
      integer*4, parameter :: VK_INSERT_ALIAS = 16#85  !F22
      integer*4, parameter :: VK_DELETE_ALIAS = 16#86  !F23
      integer*4, parameter :: VK_CANCEL_ALIAS = 16#87  !F24
 
      integer*4 hookhandle, primarythreadid
      common/redhooks/ hookhandle, primarythreadid
c------------------callback initialization
c--------set up name for our callback routine
      external KeyBoardProc
      !DEC$ ATTRIBUTES ALIAS: '_KeyBoardProc@12' :: KeyBoardProc
 
 
   10 key1 = getcharqq()
      select case (ichar(key1))
c
c     insert or delete - control-C not implemented
c
      case (VK_INSERT_ALIAS)
            keycode = 1082
            return
 
      case (VK_DELETE_ALIAS)
            keycode = 1083
            return
      case default
         !Do nothing
      endselect
c
c     function keys
c
      ikey1 = ichar(key1)
      if(ikey1.eq.0) then ! function key?
      key2  = getcharqq()
      ikey2 = ichar(key2)
      ikey3 = 0
      if(ikey2.eq.59) ikey3 = 1059 ! F1
      if(ikey2.eq.60) ikey3 = 1060 ! F2
      if(ikey3.le.0) go to 10
      keycode = ikey3
      return
      endif
c
c     direction keys
c
      if(ikey1.eq.224) then ! direction key?
      key2  = getcharqq()
      ikey2 = ichar(key2)
      ikey3 = 0
      if(ikey2.eq.73) ikey3 = 1073 ! Page Up
      if(ikey2.eq.81) ikey3 = 1081 ! Page Down
      if(ikey2.eq.71) ikey3 = 1071 ! Home
      if(ikey2.eq.79) ikey3 = 1079 ! End
      if(ikey2.eq.72) ikey3 = 1072 ! Up Arrow
      if(ikey2.eq.80) ikey3 = 1080 ! Down Arrow
      if(ikey2.eq.75) ikey3 = 1075 ! Left Arrow
      if(ikey2.eq.77) ikey3 = 1077 ! Right Arrow
      if(ikey3.le.0) go to 10
      keycode = ikey3
      return
      endif
c
c     otherwise, normal key
c
      if(ikey1.eq. 8) go to 10 ! Skip Backspace
      keycode = ikey1
      return
      end
 
      integer*4 function KeyBoardProc(nCode, wParam, lParam)
 !DEC$ ATTRIBUTES STDCALL, ALIAS: '_KeyBoardProc@12' :: KeyBoardProc
      use dflib
      use dfwin
      integer*4 nCode           ! hook code
      integer*4 wParam          ! virtual-key code
      integer*4 lParam          ! keystroke message information
 
      integer*4 i
 
      integer*4 hookhandle, primarythreadid
      common/redhooks/ hookhandle, primarythreadid
 
      integer*4, parameter :: VK_INSERT_ALIAS = 16#85  !F22
      integer*4, parameter :: VK_DELETE_ALIAS = 16#86  !F23
      integer*4, parameter :: VK_CANCEL_ALIAS = 16#87  !F24
 
      ! check ok to process and key-up from bit31
      if (nCode .ge. 0 .and. iand(lParam,16#80000000) .ne. 0) then
      ! may process this
 
      select case (wParam)
 
      case (VK_INSERT)
      i = SendMessage(GetHWndQQ(0), WM_CHAR, VK_INSERT_ALIAS, lParam )
      KeyBoardProc = 1 ! non zero to stop further processing
      return
 
      case (VK_DELETE)
      i = SendMessage(GetHWndQQ(0), WM_CHAR, VK_DELETE_ALIAS, lParam )
      KeyBoardProc = 1 ! non zero to stop further processing
      return
 
      case (16#00000043) ! do we have a c
 
      if(iand(GetKeyState(VK_CONTROL),16#00008000).ne.0) then
 
        !note: to stop quick win from intercepting this
        !      and killing the program, the "exit" item
        !      must be removed from the file menu.
      i = SendMessage(GetHWndQQ(0), WM_CHAR, VK_CANCEL_ALIAS, lParam )
      KeyBoardProc = 1 ! non zero to stop further processing
      return
       endif
 
       case DEFAULT
       ! do nothing
      end select ! end switch
      endif
 
      !!KeyboardProc = CallNextHookEx(hookhandle, nCode, wParam, loc(lPa
      KeyboardProc = 0
 
      return
      end function KeyBoardProc
 
      logical*4 function initialsettings()
      use dfwin
      integer*4 hookhandle, primarythreadid
      common/redhooks/ hookhandle, primarythreadid
      !we must use this tricky way to get the thread id
      !of the primary thread that handles the windows
      !messages.
      primarythreadid = GetCurrentThreadId()
      initialsettings = .true.
      end
