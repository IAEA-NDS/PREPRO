/*
 This file is part of PREPRO.

    Author: Dermott (Red) Cullen
 Copyright: (C) International Atomic Energy Agency

 PREPRO is free software; you can redistribute it and/or modify it
 under the terms of the MIT License; see LICENSE file for more details.
*/


/*  *********************************************************************************
    *
    *  C X-Windows  graphics interface
    *
    * by Red cullen - November 1992
    *
    * The author thanks Randy Smith for adding the patch
    * for DEC-Alpha computers (October 1994)
    *
    * Modified to run on the DEC alpha. Changed data type for most variables from long
    * (8 bytes) to int (4 bytes) to agree with data types on the SUNs and the HPs.
    * Exception is "keycode" currently declared as unsigned int.
    * Black background is set with the macro BlackPixel and white background is set
    * with the macro WhitePixel (rather than 0 and 1, respectively).
    * Calls to XSetForeground and XSetWindowBackground use the structure
    * colors[colorid].pixel (unsigned long) rather than the colorid directly for
    * correct datatype.
    * The DEC alpha window manager does not make room for the border when defining
    * its position. Modified hint.x, hint.y, and ixpix, iypix to allow for the border.
    * Roger J. Dejus, XFD/APS, March 1996.
    *
    ********************************************************************************* */

/*  *********************************************************************************
    *  black/white versus color screens
    *  ================================
    *  The FORTRAN interface plots.screen.f will use a call to myscreen to
    *  determine the type of screen = 1 - black/white, otherwise color.
    *
    *  For a color screen the entire 256 colors will be used.
    *
    *  For black/white plots.screen.f will set the color map for 0 for color
    *  0 and 1 for all other colors
    *
    *  Plots.screen.f can reverse the foreground and background by a call
    *  to newback - this will in then call backcolor (in this routine).
    *  Newback will reverse the color map - backcolor will change the
    *  foreground.
    *
    ********************************************************************************* */
#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#ifdef __alpha
#include <sys/proc.h>
#include <sys/sysinfo.h>
#endif

char *display_name = NULL;      /* Name of display to use.  Since the value
                                   is NULL, the program will use the value
                                   of the DISPLAY environmental variable. */

Display  *display;              /* our display; i.e., server & screen */
int       screen;
Status    status;
Window    window;
GC        gc;                   /* structure for Graphic Contents */
XGCValues values;
Colormap colormap;              /* physical color map */
XColor colors[256];             /* R, G. B values for color map */
int lastpen          = -1;      /* current color index */

XPoint pointtab[3001];          /* points to fill polygon */
XSizeHints      hint;           /* default window info */

XFontStruct *fontinfo;          /* structure for font */

KeySym        keysym;           /* keysym ID??? for reading characters */

FILE *rgb;                      /* file of red/green/blue color codes */
int r, g, b;                    /* variables read from file */
char colorname[50];

XEvent  event;                  /* structure for event information */

XSetWindowAttributes xswa;      /* structure to change window attributes */
unsigned long valuemask;

int kndex;                      /* index for loops */
unsigned int keycode;           /* keycode for reading keyboard */
int depth            =  1;      /* type of screen - initialize to black/white */
unsigned int myline  =  0;      /* line width - initialize = 0 for speed */
unsigned int foreground;        /* foreground and background (black/white) */
unsigned int background;

int lx1, lx2, ly1, ly2;         /* pixel values for move and draw */
int kx1, kx2, ky1, ky2;         /* pixel values for filling rectangles */
int lpix;                       /* pixel width of rectangles */
unsigned int dx, dy;            /* pixel increments for filling rectangles */

float xscreen,yscreen;          /* size of display in millimeters */
float ixpix,iypix;              /* size of display in pixels */
float xwind,ywind;              /* size of window in millimeters */
float ixwind,iywind;            /* size of window in pixels */
float dxi;                      /* x conversion from inches to pixels */
float dyi;                      /* y conversion from inches to pixels */
float dxp;                      /* x conversion from pixels to inches */
float dyp;                      /* y conversion from pixels to inches */
float xlength,ylength;          /* x and y inch dimensions */
float xbase,ybase;              /* x and y bases for conversion inches to pixels */

float xhit,yhit;                /* coordinates returned from mouse */
int len;                        /* length of input string */

/*  *********************************************************************************
    *
    * INITIALIZE ALL WINDOW PARAMETERS.
    *
    * xmin, xmax, ymin, ymax define the outer most limits of the plotting surface
    *                        in whatever units system uses (e.g., inches, mm, etc.)
    *
    * myview  = 0 - open entire screen without maintaining x,y perspective
    *         > 0 - open entire screen, but maintain x,y perspective
    *
    ********************************************************************************* */

void xpinit(xmin,xmax,ymin,ymax,myview)
float *xmin,*xmax,*ymin,*ymax;
int *myview;
{
#ifdef __alpha
/* Variable declarations for turning off printing of alignment errors. */
  int  buf[2], val, arg, error;
#endif

#ifdef __alpha
  /* Code to turn off printing of alignment error warnings. */
  /* See Ultrix to DEC OSF/1 migration guide: 7.9 Differences in
   * Standard Interfaces
   */

  buf[0]  =  SSIN_UACPROC;
  buf[1]  =  UAC_NOPRINT;
  error  =  setsysinfo(SSI_NVPAIRS,  buf,  1,  0,  0);
  if (error < 0) perror("setsysinfo");
#endif
/*
	Try to open display - terminate if not successful.
*/
	if ((display = XOpenDisplay(display_name)) == NULL)

        {        (void) fprintf(stderr,
			"REDPLOT(xpinit):  Could not open display '%s'!!!\n",
			XDisplayName(display_name));
		exit(-1); }

	screen  = DefaultScreen(display);
/*
	Scale either to use the entire screen or, if requested, maintain prespective.
*/
	xlength = *xmax - *xmin;                     /* x and y lengths in inches */
	ylength = *ymax - *ymin;

	ixpix    = DisplayWidth (display,screen);    /* define number of x, y pixels */
	iypix    = DisplayHeight(display,screen);
        xscreen  = DisplayWidthMM (display,screen);  /* define x, y size in millimeters */
        yscreen  = DisplayHeightMM(display,screen);

#ifdef __alpha
	ixpix    = ixpix - 12.0;                     /* leave room for border at left */
	iypix    = iypix - 35.0;                     /* leave room for border & window i.d. at top */
#else
	iypix    = iypix - 20.0;                     /* leave room for window i.d. at top */
#endif

	xwind = xscreen;                             /* initialize to scale to entire screen */
	ywind = yscreen;

	if(*myview > 0)                              /* if requested maintain perspective */
	{
          if(xlength/ylength > xwind/ywind) { ywind = xscreen*ylength/xlength; }
	  else                              { xwind = yscreen*xlength/ylength; }
	}

	ixwind = ixpix*xwind/xscreen;             /* x and y pixels in the window */
	iywind = iypix*ywind/yscreen;
	dxi = ixwind/xlength;                     /* conversion factors for inches to pixels */
	dyi = iywind/ylength;
	dxp = 1.0/dxi;                            /* conversion factors for pixels to inches */
	dyp = 1.0/dyi;
	xbase = (*xmin)*dxi-0.5*(ixpix - ixwind); /* bases for inches to pixel conversion */
	ybase = (*ymax)*dyi+0.5*(iypix - iywind); /* to center results in window */
/*
	Open entire screen as window
*/

#ifdef __alpha
	hint.x =  12;                        	  /* window position */
	hint.y =  35;
#else
	hint.x =  0;                              /* window position */
	hint.y =  0;
#endif

	hint.width  = (int)ixpix;                 /* window size */
	hint.height = (int)iypix;

	hint.flags = PPosition | PSize;

/*
	DIFFERENT TREATMENT FOR BLACK/WHITE VS. COLOR DISPLAYS
	depth = 1 = BLACK/WHITE
	      > 1 = COLOR (ASSUME 8)
                                                           */
	depth = DisplayPlanes(display,screen);
/*
	DEBUG - activate to simulate black/white */
/*      depth = 1; */
/*
	DEBUG - activate to simulate black/white */

	if (depth > 1)
/*
	COLOR
	Define color map
                                                           */
{
	colormap = DefaultColormap(display,screen);
	colormap = XCopyColormapAndFree(display, colormap);

         kndex = 0;
         while (kndex < 256)
	{ colors[kndex].pixel = kndex;
          colors[kndex].red   = 0;
          colors[kndex].green = 0;
          colors[kndex].blue  = 256*255;
	  kndex = kndex + 1; }

         XQueryColors(display,colormap,colors,256);

}
/*
	BLACK/WHITE OR COLOR.
                                                           */
	window = XCreateWindow(display,
			       RootWindow(display,screen),
			       hint.x,     hint.y,
			       hint.width, hint.height,
			       1,              /* border width */
			       CopyFromParent, /* depth */
			       InputOutput,    /* class */
			       CopyFromParent, /* visual */
         		       0,              /* valuemask */
         		       &xswa);         /* status    */

	if (depth == 1)
/*
	BLACK AND WHITE
        Start with white background and black foreground
                                                           */
{
	background = WhitePixel(display,screen) ;     /* white background */
	foreground = BlackPixel(display,screen) ;     /* black foreground */

	XSetWindowBackground(display,window,background);
}
/*
	COLOR
        Start with black background and white foreground
	Install color map
                                                           */
	else
{
	background = BlackPixel(display,screen);     /* black background */
	foreground = WhitePixel(display,screen);     /* white foreground */

	XSetWindowColormap(display,window,colormap);

	XSetWindowBackground(display,window,background);
}
/*
	BLACK/WHITE OR COLOR.
                                                           */
	XSetStandardProperties(display,window,             /* start with standard properties */
			       "REDPLOT",
			       "REDPLOT",
			       None,
			       0,0,
			       &hint);

	xswa.backing_store = WhenMapped;                   /* change properties */

	valuemask = CWBackingStore;

	XChangeWindowAttributes(display,window,
				valuemask,&xswa);

	values.plane_mask = AllPlanes;
	values.foreground = foreground;
	values.background = background;

	gc = XCreateGC(display,window,                     /* create Graphic Contents */
	 	       (GCPlaneMask |
			GCForeground |
			GCBackground ),
		       &values);

	XSetLineAttributes(display,gc,                     /* set line width */
	          myline,LineSolid,CapNotLast,JoinMiter);

        XSetIconName(display,window,"REDPLOT");            /* name display */

	XSelectInput(display,window,                       /* sense keyboard or mouse input */
                     ExposureMask |
		     KeyPressMask |
	 	     ButtonPressMask );

	XDefineCursor(display,window,                      /* define cursor */
	         XCreateFontCursor(display,XC_left_ptr));

	XMapWindow(display,window);                        /* display the window */
/*
	We're all set to go - Let's see if we can get this puppy up and flying

	Some window managers do not automatically open the window.
	Let's stay here resizing the window until we get an Expose event
	indicating that the window has been opened and is being shown
*/
	XMoveResizeWindow(display,window,hint.x,hint.y,hint.width,hint.height);

	while (1)                /* there's no way out of here until an Expose occurs */
	{
		XNextEvent(display,&event);
		if (event.type == Expose)
		{

			/* some window managers want the move earlier,
			   some want it here...  It doesn't seem to hurt
			   to call it twice... */

			XMoveResizeWindow(display,window,
				hint.x,hint.y,hint.width,hint.height);

			return;
		}
	}
}

/*  *********************************************************************************
    *
    * M O V E
    *
    ********************************************************************************* */

void xpmove(x,y)
float *x,*y;
{
 	  lx1 = dxi*(*x) - xbase;   /* save pixel coordinates as start for next draw */
	  ly1 = ybase - dyi*(*y);
}

/*  *********************************************************************************
    *
    * D R A W
    *
    ********************************************************************************* */

void xpdraw(x,y)
float *x,*y;
{
	lx2 = dxi*(*x) - xbase;
	ly2 = ybase - dyi*(*y);
/*
	ignore if these coordinates are the same as the preceding ones
	(this filters graphics to the resolution of the screen)       */

	if (lx1 != lx2 || ly1 != ly2)
	{ XDrawLine(display,window,gc,lx1,ly1,lx2,ly2);
	  lx1 = lx2;
	  ly1 = ly2; }
}

/*  *********************************************************************************
    *
    * DISPLAY 1 PIXEL
    *
    ********************************************************************************* */

void xypixel(x,y)
float *x,*y;
{
	kx1 = dxi*(*x) - xbase;
	ky1 = ybase - dyi*(*y);
	XDrawPoint(display,window,gc,kx1,ky1);
}

/*  *********************************************************************************
    *
    * END OF JOB
    *
    ********************************************************************************* */

void xpdone(temp,job)
int *temp,*job;

{       XClearWindow(display,window);
	XFreeGC(display,gc);
	XDestroyWindow(display,window);
	XCloseDisplay(display);
	return; }

/*  *********************************************************************************
    *
    * READ 1 CHARACTER CODE (INTEGER) FROM KEYBOARD - STAY HERE UNTIL 1 CHARACTER IS INPUT.
    *
    ********************************************************************************* */

void pxkbd2(keyint)
int keyint[];

{ XFlush(display);                             /* flush all output to display */

  len = 0;
	while (len == 0)
        { XNextEvent(display,&event);          /* next event must now be input */

		switch (event.type)
		{ case KeyPress:
                        keycode    = event.xkey.keycode;
			kndex      = event.xkey.state;
			keyint[0]  = XKeycodeToKeysym(display,keycode,kndex);
                        len        = 1;
		}
	}
}

/*  *********************************************************************************
    *
    * READ 1 CHARACTER FROM KEYBOARD - STAY HERE UNTIL 1 CHARACTER IS INPUT.
    *
    ********************************************************************************* */

void pxkbd(key, iend)
int  *iend;
char key[];

{ XFlush(display);                             /* flush all output to display */

  len = 0;
	while (len == 0)
        { XNextEvent(display,&event);          /* next event must now be input */

		switch (event.type)
		{ case KeyPress:
                        len = XLookupString (&event.xkey, key,
                                              sizeof key,&keysym,NULL);
                        if (key[0] == '\015' || key[0] == '\012') { *iend = 1; }
                        else                                      { *iend = 0; }
		}
	}
}

/*  *********************************************************************************
    *
    * TEST FOR MOUSE + KEYBOARD INPUT - RETURN WHETHER OR NOT INPUT
    *
    ********************************************************************************* */

void xmouse(iway,x,y,iway1,iway2)
float *x,*y;
int *iway,*iway1,*iway2;

{	XFlush(display);                     /* flush all output to display */

	kndex = QLength(display);            /* now the only queued events can be input */
	if(*iway1 == 0 & kndex == 0)         /* if none and iway1 = 0 immediately return */
	{ *iway = 0; }
	else                                 /* else see if event is mouse or keyboard */
	{
             XNextEvent(display,&event);
             switch (event.type)
             {
                    case ButtonPress: {
	                XButtonEvent * button_event = (XButtonEvent * ) & event;
                        if (button_event->button == Button1)
                                          *iway=1;
                        if (button_event->button == Button2)
                                          *iway=2;
                        if (button_event->button == Button3)
                                          *iway=3;

                                          xhit = (float)button_event->x + xbase;
                                          yhit = (float)button_event->y - ybase;

                                          *x =  dxp*xhit;
                                          *y = -dyp*yhit;
					  break;
                    case KeyPress:
                        *iway=4;
                        break;
                    default:                 /* not mouse or keyboard */
         	        *iway=0;
		        break; }
              }
	}
}

/*  *********************************************************************************
    *
    * CLEAR WINDOW
    *
    ********************************************************************************* */

void xpclr()

	{ XFlush(display);
	  XClearWindow(display,window); }

/*  *********************************************************************************
    *
    * DEFINE COLOR - Select black and white or color dissplay
    *
    ********************************************************************************* */

void xpen(ipen)
int *ipen;
{
    if(*ipen != lastpen)     /* nothing to do unless color changes */
    {
      XSetForeground(display,gc,colors[*ipen].pixel);    /* color */
      lastpen = *ipen;
    }
}
/*  *********************************************************************************
    *
    * DEFINE TYPE OF SCREEN = BLACK/WHITE OR COLOR
    *
    * If only 1 plane screen is black/white - otherwise it is color
    * depth is defined during initialization = must initialize before
    * calling this routine.
    *
    ********************************************************************************* */

void myscreen(mytype)
int *mytype;

	{ *mytype = depth; }

/*  *********************************************************************************
    *
    * DEFINE NEW BACKGROUND COLOR
    *
    ********************************************************************************* */

void backcolor(imback)
int *imback;

{
	if(*imback >= 0 & *imback < 256)
	   {XSetWindowBackground(display,window,colors[*imback].pixel);}
}
/*  *********************************************************************************
    *
    * DEFINE NEW PALLETTE OF COLORS
    *
    * redtab, greentab, bluetab should be 0 to 255
    *
    ********************************************************************************* */

void pallette(redtab,greentab,bluetab)
int redtab[],greentab[],bluetab[];
{
      colormap = DefaultColormap(display,screen);
      colormap = XCopyColormapAndFree(display, colormap);

      kndex = 0;
      while (kndex < 256)
       { colors[kndex].red   = 256*redtab[kndex];
         colors[kndex].green = 256*greentab[kndex];
         colors[kndex].blue  = 256*bluetab[kndex];
         XAllocColor(display,colormap,&colors[kndex]);
         kndex = kndex + 1; }

	XSetWindowColormap(display,window,colormap);
}

/*  *********************************************************************************
    *
    * DRAW RECTANGLE IPIX PIXELS WIDE ABOUT EITHER SIDE OF (X, Y)
    *
    * COLOR MUST BE DEFINED BEFORE CALLING THIS ROUTINE
    *
    ********************************************************************************* */

void pixfill(x,y,ipix)

float *x,*y;
int *ipix;

{
        lpix = *ipix;

	kx1 = dxi*(*x) - xbase;
	ky1 = ybase - dyi*(*y);

	if (lpix == 0) {XDrawPoint(display,window,gc,kx1,ky1);}

        else

    {   kx2 = kx1 + lpix;
        kx1 = kx1 - lpix;
	ky2 = ky1 + lpix;
        ky1 = ky1 - lpix;

        pointtab[0].x = kx1; pointtab[0].y = ky1;
        pointtab[1].x = kx1; pointtab[1].y = ky2;
        pointtab[2].x = kx2; pointtab[2].y = ky2;
        pointtab[3].x = kx2; pointtab[3].y = ky1;
        pointtab[4].x = kx1; pointtab[4].y = ky1;
        kndex = 5;

        XFillPolygon(display,window,gc,pointtab,kndex,Complex,CoordModeOrigin);
    }
}
/*  *********************************************************************************
    *
    * DRAW RECTANGLE
    *
    * fill   < 0 - do not fill
    *        = 0 to 255 - color to use to fill
    *
    * border < 0        - no border
    *        = 0 to 255 - color of border
    *
    ********************************************************************************* */

void boxfill(x1,x2,y1,y2,fill,border)

float *x1,*y1,*x2,*y2;
int *fill,*border;

{
	kx1 = dxi*(*x1) - xbase;
	kx2 = dxi*(*x2) - xbase;
	ky1 = ybase - dyi*(*y1);
	ky2 = ybase - dyi*(*y2);

	    pointtab[0].x = kx1; pointtab[0].y = ky1;
	    pointtab[1].x = kx1; pointtab[1].y = ky2;
	    pointtab[2].x = kx2; pointtab[2].y = ky2;
	    pointtab[3].x = kx2; pointtab[3].y = ky1;
	    pointtab[4].x = kx1; pointtab[4].y = ky1;
	    kndex = 5;
/* Fill? */
	    if(*fill >= 0 & *fill < 256)
            {XSetForeground(display,gc,colors[*fill].pixel);
	     XFillPolygon(display,window,gc,pointtab,kndex,Complex,CoordModeOrigin);
             XSetForeground(display,gc,colors[lastpen].pixel); }

/* Border? */
	    if(*border >= 0 & *border < 256)
            {XSetForeground(display,gc,colors[*border].pixel);
             XDrawLines(display,window,gc,pointtab,kndex,CoordModeOrigin);
             XSetForeground(display,gc,colors[lastpen].pixel); }
}

/*  *********************************************************************************
    *
    * DRAW POLYGON
    *
    * fill   < 0 - do not fill
    *        = 0 to 255 - color to use to fill
    *
    * border < 0        - no border
    *        = 0 to 255 - color of border
    *
    * The polygon need not be closed
    * This routine will connect the first and last points to close the polygon
    *
    ********************************************************************************* */

void polyfill(xtab,ytab,ntab,fill,border)

float xtab[],ytab[];
int *ntab,*fill,*border;

{  if(*ntab > 0 & *ntab < 3001)
     {
	   kndex = 0;
	   while (kndex < *ntab)
	  { kx1 = dxi*(xtab[kndex]) - xbase;
	    ky1 = ybase - dyi*(ytab[kndex]);
	    pointtab[kndex].x = kx1;
	    pointtab[kndex].y = ky1;
            kndex = kndex + 1;
	  }
	    pointtab[kndex].x = pointtab[0].x;
	    pointtab[kndex].y = pointtab[0].y;
	    kndex = kndex + 1;
/* Fill? */
	    if(*fill >= 0 & *fill < 256)
            {XSetForeground(display,gc,colors[*fill].pixel);
	     XFillPolygon(display,window,gc,pointtab,kndex,Complex,CoordModeOrigin);
            XSetForeground(display,gc,colors[lastpen].pixel); }

/* Border? */
	    if(*border >= 0 & *border < 256)
            {XSetForeground(display,gc,colors[*border].pixel);
	     XDrawLines(display,window,gc,pointtab,kndex,CoordModeOrigin);
             XSetForeground(display,gc,colors[lastpen].pixel); }
     }
}

/*  *********************************************************************************
    *
    * FLUSH OUTPUT BUFFER.
    *
    ********************************************************************************* */

void flushit()

	{ XFlush(display); }
/*  *********************************************************************************
    *
    * DEFINE LINE WIDTH.
    *
    ********************************************************************************* */

void widlin(mywidth)
int *mywidth;

{       myline = *mywidth;
	if(myline > 0 & myline < 4)
	{ XSetLineAttributes(display,gc,
	                     myline,LineSolid,CapNotLast,JoinMiter); }
}

/*  *********************************************************************************
    *
    * DEFINE FONT.
    *
    ********************************************************************************* */
void setfont(name)
char *name;
{
/*
	fontinfo = XLoadQueryFont(display,name);
	XSetFont(display,gc,fontinfo->fid);
*/
}

/*  *********************************************************************************
    *
    * DISPLAY TEXT
    *
    ********************************************************************************* */
void rtextx(f,x,y,string,len)
float *x,*y;
int *f,*len;
char *string;
{
	int lenx;
	lenx = *len;
	lx1 = dxi*(*x) - xbase;   /* save pixel coordinates as start for next draw */
	ly1 = ybase - dyi*(*y);
	if(*f == 0)
	{XDrawString(display,window,gc,lx1,ly1,string,lenx);}
	else
	{XDrawImageString(display,window,gc,lx1,ly1,string,lenx);}
}
