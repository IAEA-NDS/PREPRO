C=======================================================================
C
C     PROGRAM COMPLOT
C     ===============
C     VERSION 83-1 (FEBRUARY, 1983)
C     VERSION 83-2 (MAY, 1983)
C     VERSION 83-3 (DECEMBER, 1983) *MAJOR MODIFICATION.
C                                   *ADDED SELECTION OF PLOTS BY MAT OR
C                                    ZA/MT/ENERGY RANGE (EV).
C                                   *ADDED VARIABLE AXIS UNITS (PROGRAM
C                                    CONTROLLED..X=MILLI-EV, EV, KEV,
C                                    MEV...Y=MILLI-BARNS, BARNS).
C     VERSION 84-1 (APRIL, 1984)    *ADDED SELECTION BY REACTION/ENERGY
C                                    RANGE.
C                                   *ADDED IDENTIFY DATA POINTS OPTION
C                                    (SMALL BOX DRAWN AROUND EACH CROSS
C                                     SECTION AND RATIO POINT).
C                                   *IMPROVED NON-IBM GRAPHICS INTERFACE
C                                    (ALL CHARACTER POSITIONING NOW
C                                     BASED ON CHARACTER, NOT RASTER,
C                                     SIZE).
C     VERSION 85-1 (APRIL, 1985)    *SPECIAL I/O ROUTINES TO GUARANTEE
C                                    ACCURACY OF ENERGY.
C                                   *DOUBLE PRECISION TREATMENT OF
C                                    ENERGY (REQUIRED FOR NARROW
C                                    RESONANCES).
C                                   *ADDED (ZA,MT) EQUIVALENCE OPTION.
C                                   *ADDED SMALL PLOT OPTION.
C     VERSION 85-2 (AUGUST, 1985)   *FORTRAN-77/H VERSION
C     VERSION 86-1 (JANUARY, 1986)  *ENERGY DEPENDENT SCATTERING RADIUS
C     VERSION 86-2 (DECEMBER, 1986) *DOUBLE PRECISION PLOT SCALING
C                                    (REQUIRED FOR NARROW ENERGY RANGES)
C     VERSION 88-1 (JULY 1988)      *MAJOR REVISION TO MAKE CODE EASILY
C                                    INTERFACEABLE TO ALMOST ANY PLOTTER
C                                   *WARNING..INPUT PARAMETERS FROM BEEN
C                                    CHANGED (SEE, DESCRIPTION BELOW)
C                                   *COMPUTER INDEPENDENT SOFTWARE
C                                    CHARACTERS.
C                                   *COLOR PLOTS.
C                                   *MT NUMBER DEFINITIONS FROM DATA
C                                    FILE READ BY PROGRAM
C                                   *FORTRAN-77 REQUIRED (FORTRAN-H NO
C                                    SUPPORTED BY THIS PROGRAM).
C                                   *OPTION...INTERNALLY DEFINE ALL I/O
C                                    FILE NAMES (SEE, SUBROUTINE FILEIO
C                                    FOR DETAILS).
C                                   *IMPROVED BASED ON USER COMMENTS.
C     VERSION 88-2 (OCTOBER 1988)   *IMPROVED BASED ON USER COMMENTS.
C                                   *ADDED LIVERMORE CIVIC COMPILER
C                                    CONVENTIONS.
C                                   *UPDATED TO USE NEW PROGRAM CONVERT
C                                    KEYWORDS.
C     VERSION 89-1 (JANUARY 1989)   *PSYCHOANALYZED BY PROGRAM FREUD TO
C                                    INSURE PROGRAM WILL NOT DO ANYTHING
C                                    CRAZY.
C                                   *FORTRAN-77/FORTRAN-H COMPATIBLE
C                                   *SPECIAL ENDF/B MATERIAL DEFINITIONS
C                                    (ZA.LT.1000) FROM DATA FILE READ
C                                    BY PROGRAM.
C     VERSION 89-2 (MARCH 1989)     *ADDED ENDF/B-V AND VI MT
C                                    DEFINITIONS. PROGRAM WILL DETERMINE
C                                    ENDF/B FORMAT BASED ON MF=1,
C                                    MT=451 AND USE AS PPROPRIATE MT
C                                    DEFINITIONS. IF NO MF=1, MT=451
C                                    PROGRAM WILL USE ENDF/B-VI
C                                    MT DEFINITIONS.
C     VERSION 90-1 (AUGUST 1990)    *A NEW PROGRAM
C                                   *ADDED INTERACTIVE MOUSE INPUT
C                                   *ADDED 3 CHARACTER FONTS
C                                   *ADDED PHOTON DATA, MF=23 AND 27
C                                   *ADDED FORTRAN SAVE OPTION.
C                                   *ADDED MAXIMUM RATIO RANGE WHEN
C                                    PLOTTING RATIOS.
C                                   *ADDED GRID TYPES
C                                   *ADDED VARIABLE LINE THICKNESS
C                                   *WARNING...INPUT PARAMETER FORMAT
C                                    HAS BEEN CHANGED...SEE DESCRIPTION
C                                    BELOW.
C     VERSION 92-1 (JANUARY 1992)   *ADDED INCIDENT CHARGED PARTICLES
C                                    (IDENTIFIED IN PLOT TITLES)
C                                   *ADDED COMPLETELY COMPATIBLE I/O
C                                    FOR READING FLOATING POINT NUMBERS.
C     VERSION 92-2 (MAY 1992)       *CORRECTED DESCRIPTION OF INPUT
C                                    PARAMETERS AND EXAMPLE PROBLEMS.
C                                   *ADDED VARIABLE CHARACTER SIZE INPUT
C     VERSION 93-1 (MARCH 1993)     *UPDATE FOR ON SCREEN GRAPHIC
C                                    OUTPUT USING THE LAHEY COMPILER
C                                   *ADDED NU-BAR (TOTAL, DELAYED,
C                                    PROMPT).
C     VERSION 94-1 (JANUARY 1994)   *VARIABLE ENDF/B DATA FILENAMES
C                                    TO ALLOW ACCESS TO FILE STRUCTURES
C                                    (WARNING - INPUT PARAMETER FORMAT
C                                    HAS BEEN CHANGED)
C                                   *CLOSE ALL FILES BEFORE TERMINATING
C                                    (SEE, SUBROUTINE ENDIT)
C     VERSION 95-1 (MARCH 1995)     *CORRECTED CROSS SECTION
C                                    MULTIPLIER FOR EQUIVALENCES
C                                   *CORRECTED RATIO SCALING, FOR
C                                    MAXIMUM RATIO LESS THAN 1.0
C     VERSION 96-1 (JANUARY 1996)   *COMPLETE RE-WRITE
C                                   *IMPROVED COMPUTER INDEPENDENCE
C                                   *ALL DOUBLE PRECISION
C                                   *UNIFORM TREATMENT OF ENDF/B I/O
C                                   *IMPROVED OUTPUT PRECISION
C                                   *DEFINED SCRATCH FILE NAMES
C                                   *INCREASED PAGE SIZE FROM 24000
C                                    TO 48000 POINTS
C     VERSION 97-1 (APRIL 1997)     *INCREASED PAGE SIZE FROM 48000
C                                    TO 480000 POINTS
C     VERSION 99-1 (MARCH 1999)   *CORRECTED CHARACTER TO FLOATING
C                                  POINT READ FOR MORE DIGITS
C                                 *UPDATED TEST FOR ENDF/B FORMAT
C                                  VERSION BASED ON RECENT FORMAT CHANGE
C                                 *GENERAL IMPROVEMENTS BASED ON
C                                  USER FEEDBACK
C     VERS. 2000-1 (FEBRUARY 2000)*GENERAL IMPROVEMENTS BASED ON
C                                  USER FEEDBACK
C     VERS. 2002-1 (MAY 2002)     *INPUT PARAMETERS OPTIONAL
C                                 *CONTROL MINIMUM RATIO RANGE BY INPUT
C                                 *OPTIONAL BLACK OR WHITE BACKGROUND
C     VERS. 2004-1 (SEPT. 2004)   *ADDED INCLUDE FOR COMMON
C                                 *INCREASED PAGE SIZE FROM 480000
C                                  TO 600000 POINTS
C                                 *ADDED NEW REICH-MOORE TO FILE2 TO
C                                  ALLOW IDENTIFICATION OF RESOLVED AND
C                                  ANY FOLLOWING UNRESOLVED RESONANCE
C                                  REGIONS.
C     VERS. 2007-1 (JAN.  2007)   *CHECKED AGAINST ALL ENDF/B-VII.
C                                 *INCREASED MAXLOAD TO 600,000 FROM
C                                  12,000
C     VERS. 2009-1 (JAN.  2009)   *IGNORED DIFFERENCES NEAR RESONANCE
C                                  REGION BOUNDARIES (RESOLVED AND
C                                  UNRESOLVED).
C     VERS. 2010-1 (July  2010)   *Allow comparison plot even if there
C                                  is no difference (just see data).
C                                 *ONLY plot linearly interpoolable data
C                                 *Include threshold energy points to
C                                  show cross sections, but NOT ratios
C                                  near threshold.
C     VERS. 2011-1 (Jan.  2011)   *Increased MT.DAT from 200 to 1,000
C                                  entries, to accommodate new MTs.
C     VERS. 2012-1 (Aug.  2012)   *Increased incident particle list to
C                                  include photon (ZA = 0).
C                                 *Added CODENAME
C                                 *32 and 64 bit Compatible
C                                 *Added ERROR stop
C     VERS. 2013-1 (Nov. 2013)    *ONLY use min/max ratios to decide
C                                  whether or not to plot - non-positive
C                                  cross sections are no longer used.
C                                 *Limited per-cent differences to fit
C                                  output format = -9999 to +9999 %.
C                                 *OUT9 replaced NORMX
C     VERS. 2015-1 (Jan. 2015)    *Added MF=10 Radionuclide Production
C                                  which requires longer plot titles.
C                                 *Restricted character size multiplier
C                                  to 0.5 to 1.5 to accommodate longer
C                                  plot titles.
C                                 *Replaced ALL 3 way if statements.
C     VERS. 2015-2 (Mar. 2015)    *Corrected tables for X and Y axis
C                                  labels = see change search for 2015-2
C     VERS. 2015-3 (Oct. 2015)    *Allow multiple LRF=7 regions plus
C                                  unreslved region - earlier assumed
C                                  LRF=7 never used unrsesolved.
C     VERS. 2017-1 (May  2017)    *For MF=2 use MT=151 to define
C                                  Unresolved Resonance Region (URR).
C                                  Ignore NJOY MT=152 and 153.
C                                 *All floating input parameters changed
C                                  to character input + IN9 conversion.
C                                 *Added MF=4 Legendre Coefficient
C                                  Comparison: f1 through f6
C     Vers. 2018-1 (Jan. 2018)    *Doubled in core storage to 1,200,000.
C                                 *Replaced Q MeV by MT= at top of plots
C                                  (Q value in ENDF is now only defined
C                                   in MF=3, making it difficult for all
C                                   other MF now treated by this code)
C                                 *Initial Linear X scaling for MF=1
C                                  (nu-bar) and MF=4 (Legendre) =
C                                  this can be turned OFF by ZOOM
C                                 + Unless energy range is requested =
C                                  allows MF=1 and 4 default Linear X
C                                  scaling to be turned off by input
C                                  parameters, i.e., by COMHARD
C                                 *Zoom lower energy limit restricted
C                                  1.0d-5 eV - to lower zoom of linear
C                                  energy plots (otherwise cannot find
C                                  actual lower limit on plot).
C                                 *Added NRO = energy dependent scatter
C                                  radius to reading FILE2 parameters
C                                  to define unresolved energy range.
C                                 *Corrected energy dependent scatter
C                                  for all resonance types (see, above
C                                  remarks).
C     Vers. 2019-1 (June 2019)    *Additional Interpolation Law Tests
C                                 *Checked Maximum Tabulated Energy to
C                                  insure it is the same for all MTs -
C                                  if not, print WARNING messages.
C
C     2015-2 Acknowledgment
C     =====================
C     I thank Chuck Whitmer (TerraPower,WA) for reporting the errors
C     that led to the 2015-2 Improvements in this code.
C
C     I thank Jean-Christophe Sublet (UKAEA) for contributing MAC
C     executables and Bojan Zefran (IJS, Slovenia) for contributing
C     LINUX (32 or 63 bit) executables. And most of all I must thank
C     Andrej Trkov (NDS, IAEA) for overseeing the entire PREPRO project
C     at IAEA, Vienna. This was a truly International team who worked
C     together to produce PREPRO 2015-2.
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
C     AUTHORS MESSAGE
C     ---------------
C     THE COMMENTS BELOW SHOULD BE CONSIDERED THE LATEST DOCUMENTATION
C     ALL RECENT IMPROVEMENTS. PLEASE READ ALL OF THESE COMMENTS BEFORE,
C     PARTICULARLY THE COMMENTS CONCERNING MACHINE DEPENDENT CODING.
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
C     COMPARE ENDF/B FORMATTED DATA FROM TWO SEPARATE INPUT TAPES.
C     REACTIONS ARE CONSIDERED TO BE COMPARABLE IF THEY HAVE THE SAME
C     (ZA,MF,MT). RESULTS ARE PRESENTED IN GRAPHICAL FORM.
C
C     IN THE FOLLOWING FOR SIMPLICITY THE ENDF/B TERMINOLOGY--ENDF/B
C     TAPE--WILL BE USED. IN FACT THE ACTUAL MEDIUM MAY BE TAPE, CARDS,
C     DISK OR ANY OTHER MEDIUM.
C
C     ON WHAT COMPUTERS WILL THE PROGRAM RUN
C     ------------------------------------------------------------------
C     THE PROGRAM HAS BEEN IMPLEMENTED ON A VARIETY OF COMPUTERS FROM
C     CRAY AND IBM MAINFRAME TO SUN WORKSTATIONS TO AN IBM-AT PC. THE
C     PROGRAM IS SMALL ENOUGH TO RUN ON VIRTUALLY ANY COMPUTER.
C
C     THE PROGRAM USES A SIMPLE CALCOMP LIKE GRAPHICS INTERFACE
C     (DESCRIBED BELOW) AND ALLOWS THE USER SPECIFY THE PHYSICAL SIZE
C     OF THE PLOTTER BEING USED, BY INPUT PARAMETERS. USING THESE
C     CONVENTIONS THIS PROGRAM CAN BE EASILY INTERFACED TO VIRTUALLY
C     ANY PLOTTER.
C
C     FOR SPECIAL CONSIDERATIONS SEE THE SECTIONS BELOW ON,
C     (1) COMPUTER DEPENDENT CODING
C     (2) PLOTTER/GRAPHICS TERMINAL INTERFACE
C
C     GRAPHICS INTERFACE
C     ------------------------------------------------------------------
C     THIS PROGRAM USES A SIMPLE CALCOMP LIKE GRAPHICS INTERFACE WHICH
C     REQUIRES ONLY 3 SUBROUTINES...PLOTS, PLOT AND PEN (DESCRIBED IN
C     DETAIL BELOW). ALL CHARACTERS AND SYMBOLS ARE DRAWN USING TABLES
C     OF PEN STROKES (SUPPLIED WITH THIS PROGRAM). USING THIS METHOD
C     THE PROGRAM SHOULD BE SIMPLE TO INTERFACE TO VIRTUALLY ANY PLOTTER
C     OR GRAPHICS TERMINAL AND THE APPEARANCE AND LAYOUT OF THE PLOTS
C     SHOULD BE INDEPENDENT OF WHICH PLOTTER IS USED.
C
C     2015 PLOTTER DIMENSIONS
C     ==================================================================
C     PLOTTER DIMENSIONS ARE IN INCHES - NOT CM, MM, OR CUBITS.
C     THIS IS DONE FOR HISTORICAL REASONS AND HOPEFULLY THIS WILL
C     NOT INCONVENIENCE ANYONE - IN PRACTICE I HAVE USED EXACTLY THE
C     SAME DIMENSION = X = 0 to 12.5 and Y = 0 to 10 FOR DECADES
C     TO PRODUCE BOTH ON-SCREEN AND HARDCOPY POSTSCRIPT PLOTS.
C
c     I STRONGLY SUGGEST THAT YOU NOT CHANGE THESE DIMENSIONS UNLESS
C     YOU MUST = BASED ON THE PLOT SIZE YOU OBTAIN WHEN YOU FIRST RUN
C     THIS CODE.
C
C     PROGRAM IDENTIFICATION
C     ----------------------
C     AS DISTRIBUTED THE FIRST FRAME OF PLOTTED OUTPUT WILL DOCUMENT
C     THE PROGRAM NAME, VERSION AND INSTALLATION. THIS INFORMATION IS
C     STORED AS DATA IN THE ARRAY VERSES NEAR THE BEGINNING OF
C     SUBROUTINE FRAME1. IF YOU WISH TO CUSTOMIZE THE OUTPUT TO IDENTIFY
C     YOUR INSTALLATION CHANGE THE LAST TWO LINES OF THE ARRAY (VERSES).
C
C     ENDF/B FORMAT
C     -------------
C     THIS PROGRAM ONLY USES THE ENDF/B BCD OR CARD IMAGE FORMAT (AS
C     OPPOSED TO THE BINARY FORMAT) AND CAN HANDLE DATA IN ANY VERSION
C     OF THE ENDF/B FORMAT (I.E., ENDF/B-I, II,III, IV, V OR VI FORMAT).
C
C     BOTH SETS OF EVALUATED DATA MUST BE IN THE ENDF/B FORMAT. ONLY
C     SECTIONS OF FILE 2 (RESONANCE PARAMETERS) AND FILES 3, 23 AND 27
C     (TABULATED DATA) WILL BE READ AND ALL OTHER SECTIONS WILL BE
C     SKIPPED. IN FILE 2 THE ONLY IMPORTANT INFORMATION IS THE ENERGY
C     LIMITS OF THE RESOLVED AND UNRESOLVED RESONANCE REGION WHICH IS
C     LOCATED IN THE SAME FIELDS IN ALL VERSIONS OF THE ENDF/B FORMAT.
C     SIMILARLY THE FORMAT OF FILES 3, 23 AND 27 IS THE SAME IN ALL
C     VERSIONS OF ENDF/B. THEREFORE THIS PROGRAM CAN BE USED WITH DATA
C     IN ANY ENDF/B FORMAT (I.E. ENDF/B-I, II, III, IV, V OR VI).
C
C     CROSS SECTION INTERPOLATION
C     ---------------------------
C     CROSS SECTIONS MUST BE IN EITHER HISTOGRAM (I.E., INTERPOLATION
C     LAW 1) OR LINEARLY INTERPOLABLE (I.E. INTERPOLATION LAW 2) FORM.
C     IF THEY ARE NOT A WARNING MESSAGE WILL BE PRINTED AND EXECUTION
C     WILL BE TERMINATED. SEE INSTRUCTIONS BELOW ON HOW TO CONVERT
C     DATA TO HISTOGRAM OR LINEARLY INTERPOLABLE FORM.
C
C     REACTION INDEX
C     --------------
C     THIS PROGRAM DOES NOT USE THE REACTION INDEX WHICH IS GIVEN IN
C     SECTION MF=1, MT=451 OF EACH EVALUATION.
C
C     SECTION SIZE
C     ------------
C     SINCE THIS PROGRAM USES A LOGICAL PAGING SYSTEM THERE IS NO LIMIT
C     TO THE NUMBER OF POINTS IN ANY SECTION, E.G., THE TOTAL CROSS
C     SECTION MAY BE REPRESENTED BY 200,000 DATA POINTS.
C
C     DATA SELECTION
C     --------------
C     THE USER MAY SPECIFYING THE DATA TO BE COMPARED BY INPUTTING UP
C     TO 100 MAT/MT/ENERGY OR ZA/MT/ENERGY RANGES. IF THE UPPER LIMIT
C     OF THE MAT OR ZA RANGE IS LESS THAN THE LOWER LIMIT IT WILL BE SET
C     EQUAL TO THE LOWER LIMIT (I.E. THIS INDICATE ONLY COMPARE ONE
C     MAT OR ZA). IF THE UPPER LIMIT IS STILL ZERO IT WILL BE SET TO
C     9999 (NO LIMIT). IF THE UPPER MF OR MT LIMIT IS ZERO IT WILL BE
C     SET TO 99 OR 999, RESPECTIVELY (NO LIMIT). IF THE UPPER ENERGY
C     LIMIT IS ZERO IT WILL BE SET TO A LARGE NUMBER (NO LIMIT).
C
C     THE LIST OF RANGES MUST BE TERMINATED BY A BLANK LINE (I.E. ZERO
C     LOWER AND UPPER MAT/MF/MT OR ZA/MF/MT LIMITS).
C
C     IF THE FIRST RANGE LINE IS BLANK THIS LINE WILL TERMINATE THE
C     LIST OF REQUESTS (I.E. A SECOND BLANK LINE NEED NOT BE INPUT)
C     AND ALL PHYSICALLY COMPARABLE DATA WILL BE PLOTTED.
C
C     WHICH REACTIONS WILL BE PLOTTED
C     -------------------------------
C     THOSE REACTIONS WITH THE SAME (ZA, MF, MT) WILL BE COMPARED, BUT
C     ONLY THOSE DATA WHICH DIFFER BY A USER SPECIFIED ALLOWABLE
C     DIFFERENCE WILL BE PLOTTED. IN ORDER TO FORCE ALL COMPARABLE
C     REACTIONS TO BE PLOTTED THE USER NEED ONLY SPECIFY AN ALLOWABLE
C     DIFFERENCE OF ZERO.
C
C     EQUIVALENT REACTIONS
C     --------------------
C     IN ORDER TO COMPARE REACTIONS WHICH HAVE DIFFERENT ZA, MF OR MT
C     THE USER IS ALLOWED TO SPECIFY AN EQUIVALENCE LIST OF UP TO
C     100 (ZA,MF,MT) COMBINATIONS ON THE MASTER FILE WHICH ARE TO BE
C     EQUATED TO DIFFERENT (ZA,MF,MT) ON THE SECOND FILE. THIS OPTION
C     MAY BE USED TO COMPARE SIMILAR REACTIONS FROM DIFFERENT MATERIALS
C     (E.G. IRON AND NICKEL INELASTIC SCATTERING) OR DIFFERENT REACTIONS
C     FROM THE SAME OR DIFFERENT MATERIALS (E.G. U-235 CAPTURE AND
C     FISSION - IN WHICH CASE THE RATIO WILL BE THE CAPTURE TO FISSION
C     RATIO) OR THE SAME REACTION IN DIFFERENT VERSIONS OF THE ENDF/B
C     FORMAT WHICH MAY BE ASSIGNED DIFFERENT MT NUMBERS, E.G., THE
C     PHOTOELECTRIC CROSS SECTION IS MT=602 IN ENDF/B-V AND EARLIER
C     VERSIONS OF ENDF/B, BUT IS MT=522 IN ENDF/B-VI.
C
C     IN THESE EQUIVALENCE LISTS A ZERO FIELD IMPLIES ALL. FOR EXAMPLE,
C     TO EQUATE MT=522 FROM ONE FILE TO MT=602 ON THE OTHER, FOR ALL
C     MATERIALS, ONE NEED ONLY SPECIFY ZA=0, MF=23, MT=522 EQUIVALENT
C     TO ZA=0, MF=23 AND MT=602.
C
C     PLOT FORMATS
C     ------------
C     THE TWO CROSS SECTIONS ARE CONSIDERED TO BE A STANDARD (THE FIRST
C     CROSS SECTION) AND A CROSS SECTION TO BE COMPARED TO THE STANDARD
C     (THE SECOND CROSS SECTION). THE OUTPUT FROM THIS PROGRAM IS A
C     SERIES OF PLOTS. EACH PLOT WILL CONTAIN THE STANDARD CROSS SECTION
C     AND IN ADDITION THE USER MAY SPECIFY THAT EACH PLOT ALSO CONTAIN
C     THE SECOND CROSS SECTION AND/OR THE RATIO OF THE SECOND CROSS
C     SECTION TO THE FIRST CROSS SECTION.
C
C     THE USER MAY SELECT ONE OF THE FOLLOWING FIVE PLOT FORMATS (THE
C     NUMBER PRECEDING THE OPTION IS THE VALUE OF THE PLOT MODE SELECTOR
C     THAT THE USER SHOULD SPECIFY AS INPUT ON THE FIRST LINE).
C
C     (0) THE STANDARD CROSS SECTION (I.E. FIRST EVALUATION) AND THE
C         RATIO OF THE SECOND EVALUATION TO THE FIRST EVALUATION. THE
C         DATA WILL BE PRESENETED AS TWO SUB-PLOTS PER PLOT WITH THE
C         STANDARD CROSS SECTION IN THE UPPER HALF OF THE PLOT AND THE
C         RATIO IN THE LOWER HALF OF THE PLOT.
C
C     (1) THE STANDARD CROSS SECTION (I.E. FIRST EVALUATION) AND THE
C         SECOND EVALUATION. THE DATA WILL BE PRESENTED AS TWO SUB-PLOTS
C         PER PLOT WITH THE STANDARD CROSS SECTION ON THE UPPER HALF
C         OF THE PLOT AND THE SECOND CROSS SECTION IN THE LOWER HALF OF
C         THE PLOT.
C
C     (2) THE STANDARD CROSS SECTION (I.E. FIRST EVALUATION) AND THE
C         SECOND EVALUATION. THE DATA WILL BE PRESENTED AS ONE PLOT
C         CONTAINING BOTH THE STANDARD AND SECOND CROSS SECTION. THE
C         STANDARD CROSS SECTION WILL BE PRESENTED AS A SOLID LINE AND
C         THE SECOND CROSS SECTION WILL BE PRESENTED AS A DASHED LINE.
C
C     (3) THE STANDARD CROSS SECTION, SECOND CROSS SECTION AND RATIO OF
C         THE SECOND CROSS SECTION TO THE FIRST CROSS SECTION. THE DATA
C         WILL BE PRESENTED AS THREE SUB-PLOTS PER PLOT WITH THE
C         STANDARD CROSS SECTION IN THE UPPER THIRD OF THE PLOT, THE
C         SECOND CROSS SECTION IN THE MIDDLE THIRD AND THE RATIO OF THE
C         TWO IN THE LOWER THIRD OF THE PLOT (RECOMMENDED OPTION).
C
C     (4) THE STANDARD CROSS SECTION, SECOND CROSS SECTION AND RATIO OF
C         THE SECOND CROSS SECTION TO THE FIRST CROSS SECTION. THE DATA
C         WILL BE PRESENTED AS TWO SUB-PLOTS PER PLOT WITH THE STANDARD
C         AND SECOND CROSS SECTION ON THE SAME SUB-PLOT IN THE UPPER
C         TWO THIRDS OF THE PLOT AND THE RATIO OF THE TWO IN THE LOWER
C         THIRD OF THE PLOT. THE STANDARD CROSS SECTION WILL BE
C         PRESENTED AS A SOLID LINE AND THE SECOND CROSS SECTION WILL BE
C         PRESENTED AS A DASHED LINE.
C
C     ADDITIONAL PLOT FEATURES
C     ------------------------
C     IN ADDITION TO THE CROSS SECTIONS AND/OR RATIO THE FOLLOWING
C     INFORMATIONS WILL BE INCLUDED ON EACH PLOT.
C
C     (1) AN IDENTIFICATION FOR EACH SET OF CROSS SECTIONS (UP TO 30
C         CHARACTERS FOR EACH SET).
C
C     (2) THE MAXIMUM NEGATIVE AND POSITIVE PER-CENT DIFFERENCE BETWEEN
C         THE TWO CROSS SECTIONS.
C
C     (3) ARROWS INDICATING THE ENERGY AT WHICH THE MAXIMUM DIFFERENCES
C         (MINIMUM AND MAXIMUM RATIO) OCCUR.
C
C     (4) THE ENERGY LIMITS OF THE RESOLVED AND UNRESOLVED RESONANCE
C         REGION (IF THEY FALL WITHIN THE ENERGY LIMITS OF THE PLOT).
C
C     RATIO DATA
C     ----------
C     IF RATIO OUTPUT IS REQUESTED THE RATIO WILL BE DEFINED AT EACH
C     ENERGY THAT APPEARS IN EITHER EVALUATION. BETWEEN THESE ENERGIES
C     THE RATIO WILL BE PLOTTED ASSUMING LINEAR DEPENDENCE BETWEEN
C     TABULATED VALUES. FOR HISTOGRAM OR LINEARLY INTERPOLABLE CROSS
C     SECTIONS THIS REPRESENTATION WILL POINT OUT ALL EXTREMA OF THE
C     RATIO, BUT NOT NECESSARILY THE ENERGY DEPENDENCE BETWEEN TABULATED
C     VALUES.
C
C     IF THE EVALUATED DATA IS NOT IN EITHER HISTOGRAM OR LINRARLY
C     INTERPOLABLE FORM THE RATIO MAY NOT EVEN FIND ALL EXTREMA. FOR
C     EXAMPLE, IF ONE EVALUATION IS LINEARLY INTERPOLABLE AND THE
C     OTHER NON-LINEAR, BUT BOTH AGREE AT ALL TABULATED ENERGIES THE
C     RATIO WILL APPEAR TO BE EQUAL TO UNITY AT ALL ENERGIES, BUT IN
C     FACT THE CROSS SECTION BETWEEN TABULATED ENERGIES MAY BE QUITE
C     DIFFERENT USING LINEAR VS. NON-LINEAR INTERPOLATION. FOR THIS
C     REASON ONLY LINEARLY INTERPOLABLE OR HISTOGRAM DATA IS ALLOWED
C     AS INPUT TO THIS PROGRAM.
C
C     LINEAR INTERPOLABLE
C     -------------------
C     ALL CROSS SECTIONS MAY BE CONVERTED TO LINEARLY INTERPOLABLE FORM
C     BE USING PROGRAM LINEAR (UCRL-50400, VOL. 17, PART A).
C
C     HISTOGRAM
C     ---------
C     ALL LINEARLY INTERPOLABLE CROSS SECTION MAY BE CONVERTED TO
C     HISTOGRAM (I.E. MULTIGROUP) FORM BY USING PROGRAM GROUPIE
C     (UCRL-50400, VOL. 17, PART D).
C
C     INPUT UNITS
C     -----------
C     UNIT  DESCRIPTION
C     ----  -----------
C       2   INPUT LINE
C       9   MT DEFINITIONS.
C      10   FIRST ENDF/B FORMATTED EVALUATION (STANDARD).
C      11   SECOND ENDF/B FORMATTED EVALUATION.
C      17   SOFTWARE CHARACTERS.
C      18   SOFTWARE SYMBOLS AND LINE TYPES
C
C     OUTPUT UNITS
C     ------------
C     UNIT  DESCRIPTION
C     ----  -----------
C       3   NORMAL OUTPUT REPORT.
C      16   PLOTTER UNIT
C
C     SCRATCH UNITS
C     -------------
C     UNIT  DESCRIPTION
C     ----  -----------
C      12   SCRATCH UNIT FOR FIRST EVALUATION
C      13   SCRATCH UNIT FOR SECOND EVALUATION
C      14   SCRATCH UNIT FOR RATIO (ONLY USED IF RATIOS REQUESTED).
C
C     OPTIONAL STANDARD FILE NAMES (SEE SUBROUTINE FILIO1 AND FILIO2)
C     ---------------------------------------------------------------
C     UNIT  FILE NAME
C     ----  ----------
C       2   COMPLOT.INP
C       3   COMPLOT.LST
C       9   MT.DAT
C      10   ENDFB.IN1    (OR AS READ FROM INPUT)
C      11   ENDFB.IN2    (OR AS READ FROM INPUT)
C    12-14  (SCRATCH)
C      15   PLOT.CHR
C      16   (PLOTTER UNIT...USUALLY A DUMMY)
C
C     INPUT PARAMETERS
C     ------------------------------------------------------------------
C     LINE  COLUMNS  FORMAT  DESCRIPTION
C     ----  -------  ------  -----------
C       1     1-11    E11.4  LOWER X LIMIT OF PLOTTER
C            12-22    E11.4  UPPER X LIMIT OF PLOTTER
C            23-33    E11.4  LOWER Y LIMIT OF PLOTTER
C            34-44    E11.4  UPPER Y LIMIT OF PLOTTER
C            45-55     I11   NUMBER OF PLOTS PER FRAME IN X DIRECTION
C            56-66     I11   NUMBER OF PLOTS PER FRAME IN Y DIRECTION
C            67-70     F4.1  CHARACTER SIZE MULTIPLIER
C                            = 0 TO 1    - NORMAL CHARACTER SIZE
C                            = OTHERWISE - CHARACTERS SCALED BY THIS
C                                          FACTOR
C
C                            PLOT ORIENTATION IS BASED ON THE UPPER X
C                            LIMIT
C                            = .GT.0 - X HORIZONTAL/Y VERTICAL
C                            = .LT.0 - Y HORIZONTAL/X VERTICAL
C                            AFTER TESTING THE UPPER X LIMIT WILL BE
C                            SET TO ITS ABSOLUTE VALUE.
C       2     1-72     A72   FILENAME FOR FIRST ENDF/B DATA FILE
C                            (LEAVE BLANK FOR ENDFB.IN1)
C       3     1-72     A72   FILENAME FOR SECOND ENDF/B DATA FILE
C                            (LEAVE BLANK FOR ENDFB.IN2)
C       4     1-11     I11   RETRIEVAL MODE (0=MAT, 1=ZA)
C            12-22     I11   GRID (SPEED) OPTION.
C                            = 0 - TICK MARKS ON BORDER
C                            = 1 - SOLID AT COARSE INTERVALS
C                            = 2 - DASHED AT COARSE INTERVALS
C                            = 3 - SOLID AT COARSE AND FINE INTERVALS
C                            = 4 - DASHED AT COARSE AND FINE INTERVALS
C                            = 5 - SOLID COARSE/DASHED FINE INTERVALS
C            23-33     I11   SHOULD BORDER BE PLOTTED AROUND EACH PLOT
C                            = 0 - NO
C                            = 1 - YES
C            34-44     I11   LINE THICKNESS
C                            = 0 TO  5 - LINES AND CHARACTERS
C                            =-1 TO -5 - ONLY LINES
C            45-55     I11   OUTPUT MODE
C                            =-1 - ONLY COMPARISON LISTING. NO PLOTS.
C                            = 0 - CROSS SECTION OVER RATIO.
C                            = 1 - CROSS SECTION OVER CROSS SECTION.
C                            = 2 - TWO CROSS SECTIONS ON SAME PLOT.
C                            = 3 - CROSS SECTION OVER CROSS SECTION OVER
C                                  RATIO.
C                            = 4 - TWO CROSS SECTIONS ON SAME PLOT OVER
C                                  RATIO.
C            56-66     I11   STARTING PLOT NUMBER
C                            = 0     - DO NOT NUMBER PLOTS
C                            = .GT.0 - NUMBER PLOTS IN LOWER LEFT HAND
C                                      CORNER STARTING WITH INPUT NUMBER
C            67-70     I41   BACKGROUND COLOR
C                            = 0         = BLACK
C                            = OTHERWISE = WHITE
C        5    1-11    E11.4  ALLOWABLE FRACTIONAL DIFFERENCE. USED WHEN
C                            PLOTTING RATIOS. ANY REACTION WHERE THE
C                            TWO EVALUATIONS DIFFER BY MORE THAN THE
C                            ALLOWABLE DIFFERENCE WILL BE PLOTTED. IF
C                            ZERO IS INPUT THE STANDARD ALLOWABLE
C                            DIFFERENCE OF 0.001 (0.1 PER-CENT) WILL BE
C                            USED.
C            12-22    E11.4  MAXIMUM ALLOWABLE RATIO. IF RATIOS ARE
C                            PLOTTED THEY WILL BE IN THE RANGE RATMAX
C                            TO 1/RATMAX. IF 0.0 IS INPUT THERE WILL
C                            BE NO LIMIT ON THE RANGE OF THE RATIOS.
C                            THIS OPTION MAY BE USED TO IGNORE LARGE
C                            DIFFERENCES OVER VERY NARROW ENERGY RANGES
C                            (WHICH MAY BE UNIMPORTANT) AND ALLOW ONE
C                            TO SEE IMPORTANT, BUT SMALLER DIFFERENCES,
C                            OVER EXTENDED ENERGY RANGES.
C       6     1-40     40A1  IDENTIFICATION FOR UPPER EVALUATIONS
C       7     1-40     40A1  IDENTIFICATION FOR LOWER EVALUATIONS
C                            (IDENTIFICATIONS SHOULD BE LEFT ADJUSTED
C                            TO START IN COLUMN 1).
C     8-N     1- 6      I6   LOWER MAT OR ZA LIMIT (SEE SELECTION MODE,
C                            INPUT LINE 1, COLUMNS 1-11).
C             7- 8      I2   LOWER MF LIMIT
C             9-11      I3   LOWER MT LIMIT
C            12-22    E11.4  LOWER ENERGY LIMIT
C            23-28      I6   UPPER MAT OR ZA LIMIT (SEE SELECTION MODE,
C                            INPUT LINE 1, COLUMNS 1-11).
C            29-30      I2   UPPER MF LIMIT
C            31-33      I3   UPPER MT LIMIT
C            34-44    E11.4  UPPER ENERGY LIMIT
C            45-55     I11   IDENTIFY EVALUATED DATA POINTS OPTION.
C                            = 0 - DO NOT IDENTIFY DATA POINTS.
C                            = 1 - IDENTIFY DATA POINTS (BY DRAWING A
C                                  SMALL BOX AROUND EACH POINT).
C            56-66     I11   INTERACTIVE INPUT FLAG
C                            = 0 - NO INTERACTIVE INPUT ALLOWED
C                            = 1 - INTERACTIVE INPUT ALLOWED
C                           *SETTING THIS OPTION =1 WILL TURN ON THE
C                            MOUSE AFTER EACH PLOT AND ALLOW YOU TO
C                            INTERACTIVELY SPECIFY PLOT LIMITS.
C                           *IF YOU DO NOT WISH TO INTERACT WITH A PLOT
C                            OR IF YOU HAVE NO INTERACTIVE CAPABILITY
C                            THIS OPTION SHOULD BE SET = 0.
C
C                           *WARNING...DATA POINTS IDENTIFIED OPTION IS
C                            NOT RECOMMENDED FOR PLOTS CONTAINING MANY
C                            (I.E. THOUSANDS) OF DATA POINTS SINCE IT
C                            WILL MERELY INCREASE THE RUNNING TIME OF
C                            THE PROGRAM AND STILL NOT ALLOW ONE TO
C                            ACCURATELY SEE DATA POINTS.
C
C                           *UP TO 100 MAT OR ZA RANGES ARE ALLOWED.
C                            THE LIST IS TERMINATED BY A BLANK LINE.
C                            IF THE UPPER LIMIT IS LESS THAN THE LOWER
C                            LIMIT IT WILL BE SET EQUAL TO THE LOWER
C                            LIMIT. IF THE FIRST RANGE LINE IS BLANK
C                            ALL DATA WILL BE RETRIEVED. IF THE UPPER
C                            MT LIMIT IS ZERO IT WILL BE SET EQUAL TO
C                            999 (NO LIMIT). IF THE UPPER ENERGY LIMIT
C                            IS ZERO IT WILL BE INTREPRETED TO MEAN NO
C                            LIMIT. IF THE FIRST RANGE LINE SPECIFIES
C                            ZERO LOWER AND UPPER MAT OR ZA RANGE IT
C                            WILL TERMINATE THE LIST BE RANGE LINES
C                            (A SECOND BLANK LINE NEED NOT BE INPUT)
C                            AND THE ENTIRE RANGE OF MATS WILL BE
C                            COMPARED FOR THE SPECIFIED MT AND ENERGY
C                            RANGES.
C
C   N+1-M                    EQUIVALENCES
C             1- 6      I6   MASTER ZA.
C             7- 8      I2   MASTER MF.
C             9-11      I3   MASTER MT.
C            12-17      I6   EQUIVALENT ZA FROM SECOND FILE.
C            18-19      I2   EQUIVALENT MF FROM SECOND FILE.
C            20-22      I3   EQUIVALENT MT FROM SECOND FILE.
C            23-33    E11.4  MULTIPLICATION FACTOR. ANY EQUATED ZA,MF,
C                            MT DATA WILL BE MULTIPLIED BY THIS FACTOR.
C                           *THIS OPTION MAY BE USED TO RE-NORMALIZE
C                            THE SECOND CROSS SECTION OR IF COMPARING
C                            ONE CONSTITUENT OF A MIXTURE TO THE MIXED
C                            CROSS SECTION THIS MAY BE USED TO CONVERT
C                            THE SECOND CROSS SECTION TO BARNS PER MIXED
C                            ATOM BY USING A MULTIPLICATION FACTOR WHICH
C                            IS EQUAL TO THE NUMBER OF ATOMS OF THE ONE
C                            CONSTITUENT PER ATOM OF THE MIXTURE.
C                            = 0.0 - ON INPUT WILL BE INTERPRETED AS 1.0
C                            (WITH THIS CONVENTION THE USER NEED ONLY
C                            INPUT MULTIPLICATION FACTORS IF THEY ARE
C                            NOT 1.0).
C                           *UP TO 100 MAT OR ZA EQUIVALENCES ARE
C                            ALLOWED.
C                           *THE LIST IS TERMINATED BY A BLANK LINE.
C                           *A ZERO INPUT FIELD IMPLIES ALL. TO EQUATE
C                            A GIVEN MT NUMBER TO ANOTHER MT NUMBER YOU
C                            NEED MERELY SPECIFY ZA=0 ON INPUT.
C                           *NOTE, IN ALL CASES THE TITLE AT TOP OF PLOT
C                            WILL ONLY INDENTIFY MASTER (ZA,MF,MT). THE
C                            USER INPUT TITLES MUST BE USED TO IDENTIFY
C                            THE SECOND REACTION (SEE, EXAMPLE INPUT 4
C                            BELOW).
C
C     EXAMPLE DEFINITION OF PLOTTER
C     -----------------------------
C     2015 - WARNING - THE FOLLOWING DESCRIPTION IS OUT-OF-DATE.
C     TODAY THE DIMENSIONS OF THE PLOTTER ARE IN INCHES.
C
C     THE FIRST INPUT LINE DEFINES THE DIMENSIONS OF THE PLOTTER BEING
C     USED IN ANY UNITS (INCHES, CENTIMETERS, MILLIMETERS, ANYTHING)
C     WHICH APPLY TO THE PLOTTER. IN ADDITION THE FIRST LINE DEFINES
C     HOW MANY PLOTS SHOULD APPEAR ON EACH FRAME. THE PLOTTING AREA
C     DEFINED ON THE FIRST INPUT LINE MAY BE SUBDIVIDED INTO ANY NUMBER
C     OF PLOTS IN THE X AND Y DIRECTION. FOR EXAMPLE, TO PRODUCE A
C     SERIES OF FRAMES EACH CONTAINING 3 PLOTS IN THE X DIRECTION AND
C     2 PLOTS IN THE Y DIRECTION (6 PLOTS PER FRAME) COLUMN 45-55 OF
C     THE FIRST INPUT LINE SHOULD BE 3 AND COLUMNS 56-66 SHOULD BE 2.
C
C     IF THE LOCAL PLOTTER USES DIMENSIONS OF INCHES IN ORDER TO OBTAIN
C     10 X 10 INCH FRAMES WITH 3 X 2 PLOTS PER FRAME THE FIRST INPUT
C     LINE SHOULD BE,
C
C       0.0       10.0      0.0     10.0              3          2
C
C     IF THE LOCAL PLOTTER USES DIMENSION OF MILLIMETERS THE SAME
C     PHYSICAL SIZE PLOT MAY BE OBTAINED IF THE FIRST INPUT LINE IS,
C
C       0.0      254.0      0.0    254.0              3          2
C
C     FOR SIMPLICITY THE FOLLOWING EXAMPLE INPUTS WILL NOT DISCUSS THE
C     PHYSICAL DIMENSIONS OF THE PLOTTER AND THE FIRST INPUT LINE WILL
C     IN ALL CASES INDICATE 10 X 10 INCH PLOTS WITH ONLY 1 PLOT PER
C     FRAME.
C
C     IN THE FOLLOWING EXAMPLES IN ALL CASES THESE OPTIONS WILL BE USED,
C     1) DASHED GRID       - COLUMNS 12-22 OF SECOND INPUT LINE =  1
C     2) NO BORDER         - COLUMNS 23-33 OF SECOND INPUT LINE =  0
C     3) LINE THICKNESS    - COLUMNS 34-44 OF SECOND INPUT LINE = -2
C     4) OUTPUT MODE       - COLUMNS 45-55 OF SECOND INPUT LINE =  3
C     5) FIRST PLOT NUMBER - COLUMNS 56-66 OF SECOND INPUT LINE =  1
C
C     EXAMPLE INPUT 1
C     ---------------
C     RETRIEVE MATS 1023, 1056 AND 1065 THROUGH 1072, MT = 1 AND 2
C     (TOTAL AND ELASTIC) FROM THE FIRST INPUT FILE AND COMPARE TO
C     ANY SECTION FROM THE SECOND FILE THAT HAS THE SAME ZA/MF/MT. ONLY
C     COMPARE DATA OVER THE ENERGY RANGE 0.1 EV TO 1 KEV. IDENTIFY
C     THE TWO SETS OF DATA AS ENDF/B-V AND ENDF/B-IV, RESPECTIVELY.
C     ONLY PLOT THOSE REACTIONS WHICH DIFFER AT ONE OR MORE ENERGIES
C     BY MORE THAN 1 PER-CENT (NOTE, 1 PER-CENT = 0.01 AS INPUT
C     FRACTION). NO EQUIVALENT REACTIONS ARE SPECIFIED. FILERNAMES
C     ARE STANDARD (THSE CAN EITHER BE EXPLICITLY INCLUDED, OR SIMPLY
C     LEFT BLANK).
C
C     THE FOLLOWING 12 INPUT LINES ARE REQUIRED.
C
C        0.0       10.0      0.0     10.0              3          2
C  ENDFB.IN1
C  ENDFB.IN2
C          0          1        0       -2              3          1
C       0.01        0.0
C  ENDF/B-V DATA (STANDARD)
C  ENDF/B-IV DATA
C  1023 3  1 0.1            3  2 1000.0                0
C  1056 3  1 0.1            3  2 1000.0                0
C  1065 3  1 0.1       1072 3  2 1000.0                0
C                                       (TERMINATES REQUEST LIST)
C                                       (TERMINATES EQUIVALENCE LIST)
C
C     EXAMPLE INPUT 2
C     ---------------
C     TO USE ALL OF THE SAME OPTIONS AS SPECIFIED IN EXAMPLE INPUT 1,
C     EXCEPT TO RETRIEVE U-235, U-238 AND PU-239 THROUGH PU-242 THE
C     FOLLOWING 12 INPUT LINES ARE REQUIRED.
C
C        0.0       10.0      0.0     10.0              3          2
C  ENDFB.IN1
C  ENDFB.IN2
C          1          1        0       -2              3          1
C       0.01        0.0
C  ENDF/B-V DATA (STANDARD)
C  ENDF/B-IV DATA
C 92235 3  1 0.1            3  2 1000.0                0
C 92238 3  1 0.1            3  2 1000.0                0
C 94239 3  1 0.1      94242 3  2 1000.0                0
C                                       (TERMINATES REQUEST LIST)
C                                       (TERMINATES EQUIVALENCE LIST)
C     EXAMPLE INPUT 3
C     ---------------
C     TO USE ALL OF THE SAME OPTIONS AS SPECIFIED IN EXAMPLE INPUT 1,
C     EXCEPT TO RETRIEVE AND COMPARE ALL MATS THE FOLLOWING 10 INPUT
C     LINES ARE REQUIRED.
C
C       0.0       10.0      0.0     10.0              3          2
C  ENDFB.IN1
C  ENDFB.IN2
C         0          1        0       -2              3          1
C      0.01        0.0
C ENDF/B-V DATA (STANDARD)
C ENDF/B-IV DATA
C     1 1  1 0.0       999999999 0.0                   0
C                                       (TERMINATES REQUEST LIST)
C                                       (TERMINATES EQUIVALENCE LIST)
C                                        NOTE, ZERO LOWER AND UPPER
C                                        MAT LIMITS INDICATES NO LIMIT.
C
C     EXAMPLE INPUT 4
C     ---------------
C     RETRIEVE U-235 AND EQUATE THE FISSION CROSS SECTION (MT=18) ON
C     THE MASTER FILE TO CAPTURE (MT=102) ON THE SECOND FILE. PLOT
C     THE CAPTURE, FISSION AND CAPTURE TO FISSION RATIO OVER THE ENERGY
C     RANGE 0.0253 EV TO 1 KEV. THE FOLLOWING 11 INPUT LINES ARE
C     REQUIRED.
C
C        0.0       10.0      0.0     10.0              3          2
C  ENDFB.IN1
C  ENDFB.IN2
C          1          1        0       -2              3          1
C       0.01        0.0
C  FISSION
C  CAPTURE
C 92235 3 18 0.0253   92235 3 18 1000.0                0
C                                       (TERMINATES REQUEST LIST)
C 92235 3 18 92235 3102                 (MULTIPLICATION OF 1.0 INFERRED)
C                                       (TERMINATES EQUIVALENCE LIST)
C
C     EXAMPLE INPUT 5
C     ---------------
C     IN DIFFERENT VERSIONS OF THE ENDF/B FORMAT DIFFERENT MT NUMBERS
C     ARE ASSIGNED TO THE SAME REACTION. FOR EXAMPLE, IN ENDF/B-V AND
C     EARLIER VERSIONS OF ENDF/B THE PHOTOELECTRIC CROSS SECTION IS
C     MT=602, WHILE IN ENDF/B-VI IT IS MT=522. IN ORDER TO COMPARE
C     ASSUMING THAT THE MASTER IS ENDF/B-VI AND THE OTHER ENDF/B FILE
C     IS ENDF/B-V (OR EARLIER) YOU MAY EQUATE MT=522 TO 602.
C
C     WHEN COMPARING PHOTOELECTRIC CROSS SECTIONS WE EXPECT THERE TO BE
C     LARGE DIFFERENCES NEAR EDGES, SINCE IT IS UNLIKELY THAT TWO
C     INDEPENDENT EVALUATIONS USE EXACTLY THE SAME EDGE ENERGIES. FROM
C     A PRACTICAL VIEWPOINT THESE DIFFERENCES ARE NOT IMPORTANT IF THEY
C     ONLY OCCUR OVER NARROW ENERGY RANGES NEAR ENERGIES. HOWEVER THESE
C     LARGE DIFFERENCES MAY MAKE IT DIFFICULT TO SEE DIFFERENCES OVER
C     OTHER ENERGY RANGES, WHICH MAY BE IMPORTANT. IN ORDER TO BE ABLE
C     TO SEE IMPORTANT DIFFERENCES IN THE FOLLOWING COMPARISON WE WILL
C     CONSTRAIN THE PLOTTED RATIO TO THE RANGE ABOUT 0.9 TO 1.1 IN
C     ORDER TO BE ABLE TO SEE DIFFERENCES OF UP TO 10 PER-CENT. WE WILL
C     DO THIS BY SPECIFYING A MAXIMUM RATIO OF 1.1, WHICH WILL IN TURN
C     DEFINE A MINIMUM RATIO OF 1/1.1, OR ABOUT 0.9.
C
C     IN ORDER TO COMPARE THE PHOTOELECTRIC CROSS SECTION FOR ALL
C     MATERIALS THE FOLLOWING 11 INPUT LINES ARE REQUIRED.
C
C       0.0       10.0      0.0     10.0              3          2
C  ENDFB.IN1
C  ENDFB.IN2
C         0          1        0       -2              3          1
C      0.01        1.1
C ENDF/B-VI
C ENDF/B-V
C    023522           999923522                       0
C                                       (TERMINATES REQUEST LIST)
C    023522     023602                  (MULTIPLICATION OF 1.0 INFERRED)
C                                       (TERMINATES EQUIVALENCE LIST)
C
C     EXAMPLE INPUT 6
C     ---------------
C     THE SAME EXAMPLE AS ABOVE, EXCEPT THAT DIFFERENT FILENAMES WILL
C     BE USED TO READ THE DATA FROM A FILE TREE STRUCTURE. THE FOLLOWING
C     11 INPUT LINES ARE REQUIRED.
C
C       0.0       10.0      0.0     10.0              3          2
C  /Evaluated/ENDFB6/PHOTON.IN
C  /Evaluated/ENDFB5/PHOTON.IN
C         0          1        0       -2              3          1
C      0.01        1.1
C ENDF/B-VI
C ENDF/B-V
C    023522           999923522                       0
C                                       (TERMINATES REQUEST LIST)
C    023522     023602                  (MULTIPLICATION OF 1.0 INFERRED)
C                                       (TERMINATES EQUIVALENCE LIST)
C
C     EXAMPLE INPUT 7
C     ---------------
C     THE OUTPUT FOR ALL OF THE ABOVE EXAMPLES ARE ORIENTED WITH X
C     HORIZONTAL AND Y VERTICAL. TO CHANGE THE ORIENTATION OF THE PLOTS
C     YOU NEED MERELY SPECIFY A NEGATIVE UPPER X LIMIT OF THE SIZE OF
C     THE PLOTS ON THE FIRST INPUT LINE.
C
C     THE FOLLOWING EXAMPLE IS EXACTLY THE SAME AS THE ABOVE EXAMPLE,
C     EXCEPT THAT THE ORIENTATION OF THE PLOTS HAS BEEN CHANGED. THE
C     FOLLOWING 11 INPUT LINES ARE REQUIRED.
C
C       0.0      -10.0      0.0     10.0              3          2
C  /Evaluated/ENDFB6/PHOTON.IN
C  /Evaluated/ENDFB5/PHOTON.IN
C         0          1        0       -2              3          1
C      0.01        1.1
C ENDF/B-VI
C ENDF/B-V
C    023522           999923522                       0
C                                       (TERMINATES REQUEST LIST)
C    023522     023602                  (MULTIPLICATION OF 1.0 INFERRED)
C                                       (TERMINATES EQUIVALENCE LIST)
C
C===== PLOTTER/GRAPHICS TERMINAL INTERFACE =============================
C
C      NON-INTERACTIVE
C     ------------------------------------------------------------------
C      THIS PROGRAM USES A SIMPLE CALCOMP LIKE INTERFACE INVOLVING
C      ONLY 5 SUBROUTINES,
C
C      STARPLOT     - INITIALIZE PLOTTER
C      NEXTPLOT     - CLEAR SCREEN FOR NEXT PLOT
C      ENDPLOTS     - TERMINATE PLOTTING
C
C      PLOT(X,Y,IPEN)        - DRAW OR MOVE FROM LAST LOCATION TO (X,Y),
C                              END OF CURRENT PLOT OR END OF PLOTTING.
C            IPEN =   2 - DRAW
C                 =   3 - MOVE
C
C      PEN(IPEN)             - SELECT COLOR.
C          IPEN- COLOR = 1 TO N (N = ANY POSITIVE INTEGER)
C
C      BOXCOLOR(X,Y,IFILL,IBORDER) - FILL A RECTANGLE WITH COLOR
C               X,Y                = DEFINE THE CORNERS OF THE BOX
C                   IFILL          = COLOR TO FILL BOX WITH
C                         IBORDER  = COLOR OF BORDER OF BOX
C
C      INTERACTIVE
C     ------------------------------------------------------------------
C      THIS PROGRAM INCLUDES AN INTERACTIVE INTERFACE FOR USE WITH A
C      MOUSE. THE INTERFACE INVOLVES 2 SUBROUTINE,
C
C      INTERACT(MYACTION)             - WHETHER OR NOT INTERACTION
C               MYACTION              = 0 - NO (RETURNED BY INTERACT)
C                                     = 1 - YES (RETURNED BY INTERACT)
C
C      MOUSEY(IWAY,XI,YI,IWAY1,IWAY2) - READ POSITION OF MOUSE
C                             IWAY    = 0 - NO INPUT
C                                     = 1 - LEFT BUTTON
C                                     = 2 - MIDDLE BUTTON
C                                     = 3 - RIGHT BUTTON
C                                     = 4 - KEYBOARD INPUT
C                             XI      = X POSITION IN LOCAL UNITS
C                             YI      = Y POSITION IN LOCAL UNITS
C                             IWAY1   = MINIMUM ALLOWABLE IWAY
C                             IWAY2   = MAXIMUM ALLOWABLE IWAY
C
C     AS USED BY THIS PROGRAM IWAY1   = 1
C                             IWAY2   = 4
C     KEYBOARD INPUT (IWAY=4) MEANS NO ZOOMED PLOT REQUESTED.
C     MOUSE INPUT (IWAY=1 TO 3) MEANS A ZOOMED PLOT IS REQUESTED.
C     MOUSEY WILL BE CALLED ONCE TO SEE IF A ZOOMED PLOT IS REQUESTED.
C     IF IT IS XI WILL BE USED TO DEFINE ONE X (E.G., ENERGY) LIMIT OF
C     THE ZOOMED PLOT. MOUSEY WILL THEN BE CALLED A SECOND TIME TO
C     DEFINE A SECOND XI TO DEFINE THE OTHER X LIMIT OF THE ZOOMED
C     PLOT.
C
C     IF YOU DO NOT WANT INTERACTION YOU SHOULD INCLUDE THE FOLLOWING
C     SUBROUTINES IN YOUR GRAPHIC INTERFACE,
C
C     SUBROUTINE INTERACT(MYACTION)
C     MYACTION=0
C     RETURN
C     END
C     SUBROUTINE MOUSEY(IWAY,XI,YI,IWAY1,IWAY2)
C     IWAY=4
C     XI=0.0
C     YI=0.0
C     RETURN
C     END
C
C     ALTERNATIVE INTERACTIVE
C     ------------------------------------------------------------------
C     IF YOU DO NOT HAVE A MOUSE BUT WOULD STILL LIKE TO INTERACTIVE
C     INPUT YOU CAN REPLACE SUBROUTINE ACTION IN THIS PROGRAM.
C
C     AS DISTRIBUTED SUBROUTINE ACTION USES A MOUSE TO DEFINE LOWER
C     AND UPPER ENERGY (OR X) LIMITS WHICH ARE USED TO PRODUCE THE
C     NEXT PLOT. A CALL TO ACTION IS OF THE FORM,
C
C     CALL ACTION(KACTV,XACT1,XACT2)
C
C                 KACTV   = 0 - NO INTERACTIVE INPUT
C                         = 1 - INTERACTIVE INPUT
C                 XACT1   = LOWER ENERGY LIMIT
C                 XACT2   = UPPER ENERGY LIMIT
C
C     IF THERE IS NO INTERACTIVE INPUT THE PROGRAM WILL PROCEED TO THE
C     NEXT PLOT REQUESTED BY NON-INTERACTIVE INPUT.
C
C     IF THERE IS INTERACTIVE INPUT THE PROGRAM WILL USE XACT1 AND
C     XACT2 TO DEFINE THE ENERGY LIMITS OF THE NEXT PLOT USING THE
C     SAME DATA AS APPEARED ON THE LAST PLOT. AS WITH NON-INTERACTIVE
C     INPUT, IF YOU SELECT AN ENERGY RANGE WHERE THE MAXIMUM DIFFERENCE
C     IS LESS THAN THAT SPECIFIED BY INPUT NO PLOT WILL BE PRODUCED
C     AND THE CODE WILL PROCEED TO THE NEXT PLOT REQUESTED BY
C     NON-INTERACTIVE INPUT.
C
C     YOU CAN REPLACE SUBROUTINE ACTION FOLLOWING THE ABOVE CONVENTIONS
C     TO ALLOW INTERACTION VIA DIRECT READ OF X LIMITS, LIGHTPEN OR
C     WHATEVER FACILITIES YOU HAVE AVAILABLE.
C
C     INTERFACING
C     ------------------------------------------------------------------
C     IN ORDER TO INTERFACE THIS PROGRAM FOR USE ON ANY PLOTTER WHICH
C     DOES NOT USE THE ABOVE CONVENTIONS IT IS MERELY NECESSARY FOR THE
C     THE USER TO WRITE 5 SUBROUTINES DESCRIBED ABOVE AND TO THEN CALL
C     THE LOCAL EQUIVALENT ROUTINES.
C
C     COLOR PLOTS
C     ------------------------------------------------------------------
C     TO SELECT PLOTTING COLORS SUBROUTINE PEN (DESCRIBED ABOVE) IS USED
C     TO SELECT ONE OF THE AVAILABLE COLORS. WHEN RUNNING ON A MAINFRAME
C     USING AN IBM GRAPHICS TERMINAL OR ON AN IBM-PC USING A HEWLETT-
C     PACKARD PLOTTER THE GRAPHICS INTERFACE (DESCRIBED ABOVE) WILL
C     PRODUCE COLOR PLOTS.
C
C     BLACK AND WHITE PLOTS
C     ------------------------------------------------------------------
C     WHEN PRODUCING BLACK AND WHITE HARDCOPY ON A MAINFRAME THE USER
C     SHOULD ADD A DUMMY SUBROUTINE PEN TO THE END OF THE PROGRAM TO
C     IGNORE ATTEMPTS TO CHANGE COLOR. ADD THE FOLLOWING SUBROUTINE,
C
C     SUBROUTINE PEN(IPEN)
C     RETURN
C     END
C
C     CHARACTER SET
C     ------------------------------------------------------------------
C     THIS PROGRAM USES COMPUTER AND PLOTTER DEVICE INDEPENDENT SOFTWARE
C     CHARACTERS. THIS PROGRAM COMES WITH A FILE THAT DEFINES THE PEN
C     STROKES REQUIRED TO DRAW ALL CHARACTERS ON AN IBM KEYBOARD (UPPER
C     AND LOWER CASE CHARACTERS, NUMBERS, ETC.) PLUS AN ALTERNATE SET OF
C     ALL UPPER AND LOWER CASE GREEK CHARACTERS AND ADDITIONAL SPECIAL
C     SYMBOLS.
C
C     THE SOFTWARE CHARACTER TABLE CONTAINS X AND Y AND PEN POSITIONS TO
C     DRAW EACH CHARACTER. IF YOU WISH TO DRAW ANY ADDITIONAL CHARACTERS
C     OR TO MODIFY THE FONT OF THE EXISTING CHARACTERS YOU NEED ONLY
C     MODIFY THIS TABLE.
C
C     CONTROL CHARACTERS
C     ------------------------------------------------------------------
C     IN THE SOFTWARE CHARACTER TABLE ALL CHARACTERS TO BE PLOTTED WILL
C     HAVE PEN POSITION = 2 (DRAW) OR = 3 (MOVE). IN ADDITION THE TABLE
C     CURRENTLY CONTAINS 4 CONTROL CHARACTERS,
C
C     PEN POSITION = 0
C     ----------------
C     SHIFT THE NEXT PRINTED CHARACTER BY X AND Y. 3 CONTROL CHARACTERS
C     ARE PRESENTLY INCLUDED IN THE SOFTWARE CHARACTER TABLE TO ALLOW
C     SHIFTING.
C
C     {   = SHIFT UP (FOR SUPERSCRIPTS..............X= 0.0, Y= 0.5)
C     }   = SHIFT DOWN (FOR SUBSCRIPTS..............X= 0.0, Y=-0.5)
C     \   = SHIFT LEFT 1 CHARACTER (FOR BACKSPACE...X=-1.0, Y= 0.0)
C
C     PEN POSITION =-1
C     ----------------
C     SELECT THE NEXT PRINTED CHARACTER FROM THE ALTERNATE CHARACTER
C     SET. AT PRESENT THIS CONTROL CHARACTER IS,
C
C     ]   = SWITCH TO ALTERNATE CHARACTER SET
C
C     THESE 4 CONTROL CHARACTERS ARE ONLY DEFINED BY THE VALUE OF THE
C     PEN POSITION IN THE SOFTWARE CHARACTER TABLE (I.E., THEY ARE NOT
C     HARD WIRED INTO THIS PROGRAM). AS SUCH BY MODIFYING THE SOFTWARE
C     CHARACTER TABLE THE USER HAS THE OPTION OF DEFINING ANY CONTROL
C     CHARACTERS TO MEET SPECIFIC NEEDS.
C
C     THESE CHARACTERS MAY BE USED IN CHARACTER STRINGS TO PRODUCE
C     SPECIAL EFFECTS. FOR EXAMPLE, TO PLOT SUBSCRIPT 5, B, SUPERSCRIPT
C     10 USE THE STRING,
C
C     }5B{1{0
C
C     TO PLOT B, SUBSCRIPT 5 AND SUPERSCRIPT 10 WITH THE 5 DIRECTLY
C     BELOW THE 1 OF THE 10 WE CAN USE THE BACKSPACE CHARACTER TO
C     POSITION THE 1 DIRECTLY ABOVE THE 5 USING THE STRING,
C
C     B}5\{1{0
C
C     TO PLOT UPPER CASE GREEK GAMMA FOLLOWED BY THE WORD TOTAL (I.E.,
C     RESONANCE TOTAL WIDTH) USE THE STRING.
C
C     ]G TOTAL
C
C     NOTE, WHEN THESE CONTROL CHARACTERS ARE USED THEY ONLY EFFECT THE
C     NEXT 1 PRINTED CHARACTER (SEE, ABOVE EXAMPLE OF PLOTTING SUPER-
C     SCRIPT 10 WHERE THE SHIFT UP CONTROL CHARACTER WAS USED BEFORE THE
C     1 AND THEN AGAIN BEFORE THE 0 AND THE BACKSPACE AND SHIFT UP
C     CONTROL CHARACTERS WERE USED IN COMBINATION).
C
C     IF THESE 4 CONTROL CHARACTERS ARE NOT AVAILABLE ON YOUR COMPUTER
C     YOU CAN MODIFY THE SOFTWARE CHARACTER TABLE TO USE ANY OTHER 4
C     CHARACTERS THAT YOU DO NOT NORMALLY USE IN CHARACTER STRINGS (FOR
C     DETAILS SEE THE SOFTWARE CHARACTER TABLE).
C
C     STANDARD/ALTERNATE CHARACTER SETS
C     ------------------------------------------------------------------
C     THE SOFTWARE CHARACTER TABLE CONTAINS 2 SETS OF CHARACTERS WHICH
C     ARE A STANDARD SET (ALL CHARACTERS ON AN IBM KEYBOARD) AND AN
C     ALTERNATE SET (UPPER AND LOWER CASE GREEK CHARACTERS AND SPECIAL
C     CHARACTERS). TO DRAW A CHARACTER FROM THE ALTERNATE CHARACTER SET
C     PUT A RIGHT BRACKET CHARACTER (]) BEFORE A CHARACTER (SEE THE
C     ABOVE EXAMPLE AND THE SOFTWARE CHARACTER TABLE FOR DETAILS). THIS
C     CONTROL CHARACTER WILL ONLY EFFECT THE NEXT 1 PLOTTED CHARACTER.
C
C     SUB AND SUPER SCRIPTS
C     ------------------------------------------------------------------
C     TO DRAW SUBSCRIPT PRECEED A CHARACTER BY }. TO DRAW SUPERSCRIPT
C     PRECEED A CHARACTER BY { (SEE THE ABOVE EXAMPLE AND THE SOFTWARE
C     CHARACTER TABLE FOR DETAILS). THESE CONTROL CHARACTER WILL ONLY
C     EFFECT THE NEXT 1 PLOTTED CHARACTER.
C
C     BACKSPACING
C     ------------------------------------------------------------------
C     TO BACKSPACE ONE CHARACTER PRECEED A CHARACTER BY \ (SEE, THE
C     ABOVE EXAMPLE AND THE SOFTWARE CHARACTER TABLE FOR DETAILS). THIS
C     CONTROL CHARACTER WILL PERFORM A TRUE BACKSPACE AND WILL EFFECT
C     ALL FOLLOWING CHARACTERS IN THE SAME CHARACTER STRING.
C
C     PLOT DIMENSIONS
C     ---------------
C     ARE DEFINED BY USER INPUT. INTERNALLY THE PROGRAM WILL CREATE A
C     PLOT IN APPROXIMATELY A4 OR 8-1/2 BY 11 INCH FORMAT. DURING
C     OUTPUT THE PLOT IS TRANSFORMED TO THE UNITS (INCHES, CENTIMETERS,
C     MILLIMETERS, WHATEVER) OF THE PLOTTER BEING USED AND OUTPUT.
C
C===== PLOTTER/GRAPHICS TERMINAL INTERFACE =============================
C=======================================================================
      INCLUDE 'implicit.h'
C-----08/08/2012 DEFINE CODE NAME
      CHARACTER*8 CODENAME
      COMMON/NAMECODE/CODENAME
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 ZABCD
      CHARACTER*4 LABEL,BLANK,STAR,MARK1,MARK2
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
C-----2017/4/2 - Add IZOOM to control DEFAULT X axis scaling
      COMMON/ZOOMCOM/IZOOM
      COMMON/GPP7/XZOOM(2),YZOOM(2,5),IUSE1(60),IUSE2(60)
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      COMMON/WHERE1/IZA1,MAT1,MF1,MT1
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
C-----11/22/2013 - Substituted OUT9 output for NORMX
      CHARACTER*1 FIELD11
      COMMON/NORMCC/FIELD11(11,6)
c-----2014/7/22 - LEADER needed to define ZAP and LFS
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      COMMON/MINMAX/XMIN0,XMAX0,YMIN,YMAX,YMINPC,YMAXPC
      COMMON/WHATZA/IZA
      COMMON/ZEROES/EZERO,MYZERO
      COMMON/THRES/THRES3(3)
      COMMON/VERSON/IVERSE
      COMMON/MELINR/IMLINR
      COMMON/PAGE1/NPAGE1
      COMMON/PARTIN/INPART
      COMMON/COPC/LABEL(17)
      COMMON/COPI/NLABEL(3)
      COMMON/HARDON/IMHARD
c-----2017/3/21 - Added for MF=4 data
      COMMON/SETLABCM/LTT,LI,LCT,ILEG(2),NLEG(2),NPTLEG(2),NLEGB(2)
C-----2014/7/21 - ADDED MF=10 PARAMETERS
      COMMON/MF10COM/IZAP1,LFS1,NS101,JS101,
     1               IZAP2,LFS2,NS102,JS102
      INCLUDE 'complot.h'
      DIMENSION ZABCD(10)
      EQUIVALENCE (KTAPE(1),KTAPE1),(KTAPE(2),KTAPE2)
      DATA BLANK/' '/
      DATA STAR/'*'/
C-----INITIALIZE LAST MAT NUMBER FOUND.
      DATA MATLST/-99999/
C-----08/08/2012 DEFINE CODE NAME
      CODENAME = 'COMPLOT '
C-----INITIALIZE TIMER
      CALL TIMER
c-----------------------------------------------------------------------
C
C     DEFINE ALL I/O UNITS AND OPTIONALLY DEFINE FILENAMES.
C
c-----------------------------------------------------------------------
      CALL FILIO1
c-----------------------------------------------------------------------
C
C     INITIALIZE GENERAL PLOTTING PACKAGE (GPP) PARAMETERS AND THEN
C     SELECT OPTIONS.
C
c-----------------------------------------------------------------------
      CALL SET0
C-----INITIALIZE TO NO HARD COPY
      IMHARD=0
C-----DEFINE PROGRAM FOR GPP TO BE COMPLOT.
      MYPROG=1
C-----TURN ON GPP OPTION TO SHOW ENERGY LIMITS OF RESONANCE REGIONS.
      NREST=1
C-----TURN ON GPP OPTION TO SHOW POSITION OF MINIMUM AND MAXIMUM RATIOS.
      NRATIO=1
C-----TURN ON GPP OPTION TO SHOW UNITY AND BAND OF ALLOWABLE
C-----UNCERTAINTIES ON RATIO PLOT.
      NUNITY=1
C-----TURN ON GPP OPTION TO ALLOW INTERACTIVE INPUT.
      NACTV=1
C-----INITIALIZE COUNT OF THE NUMBER OF SECTION READ FROM SECOND TAPE.
      MTSIZE=0
      ISPOT=0
      JSPOT=0
      IZATAB(1)=0
      MFTAB(1)=0
      MTTAB(1)=0
C-----READ, CHECK AND LISTING ALL INPUT PARAMETERS.
      CALL READIN(DIFFOK)
      DIFMIN=1.0d0-DIFFOK
      DIFMAX=1.0d0+DIFFOK
C-----INITIALIZE PARAMETERS.
      MPASS=0
      MATCH=0
      IZA2=-999999
      MF2=-9999
      MT2=-9999
c-----2014/7/21 - Initialize MF=10 parameters
      NS101 = 0
      JS101 = 0
      NS102 = 0
      JS102 = 0
c-----2017/3/20 - Initialize MF=4 parameters
      LTT       = 0
      LI        = 0
      LCT       = 0
      do k=1,2
      ILEG  (k) = 0
      NLEG  (k) = 0
      NPTLEG(k) = 0
      enddo
      MAX3      = MAXLOAD/3
      NLEGB (1) = MAX3          ! Split storage between 3 sets of data
      NLEGB (2) = MAX3+MAX3     ! Constant throughout code
c-----------------------------------------------------------------------
C
C     INITIALIZE GRAPHICS AND IDENTIFY CODE
C
c-----------------------------------------------------------------------
      CALL GPPLOT(IMACTV,-1)
c-----------------------------------------------------------------------
C
C     FIND NEXT SECTION OF CROSS SECTIONS ON FIRST TAPE.
C
c-----------------------------------------------------------------------
C-----READ AND LIST TAPE LABEL ON FIRST TAPE.
      CALL COPY1L(KTAPE1)
      WRITE(OUTP,620) LABEL,NLABEL(1)
      GO TO 20
C-----------------------------------------------------------------------
C
C     Search for next section MAT/MF/MT) always starts here.
C
C-----------------------------------------------------------------------
c-----2014/7/21 - Initialize MF=10 parameters
   10 NS101 = 0
      JS101 = 0
      NS102 = 0
      JS102 = 0
c-----2017/3/20 - Initialize MF=4 parameters
      LTT       = 0
      LI        = 0
      LCT       = 0
      do k=1,2
      ILEG  (k) = 0
      NLEG  (k) = 0
      NPTLEG(k) = 0
      enddo
      IF(MATH.LT.0) GO TO 30
   20 CALL NXTMAT(KTAPE1)
      IF(MATH.GT.0) GO TO 60
C-----END OF DATA. PRINT COUNT OF NUMBER OF PLOTS GENERATED.
   30 PCDIFF=100.0d0*DIFFOK
      NPAGE=IABS(NPAGE-NPAGE1)
      WRITE(OUTP,590) NPAGE,PCDIFF
c-----Check MT list for any preceding MAT
      if(MATLST.gt.0) then
      CALL MAXIE3(0)
      endif
C-----------------------------------------------------------------------
C
C     IF NO PLOTS, NOTIFY USERS.
C
C-----------------------------------------------------------------------
      IF(NPAGE.LE.0) THEN
C-----ON SCREEN GRAPHICS MESSAGE
      IF(IMHARD.LE.0) CALL NOPLOTS
C-----ON SCREEN TEXT MESSAGE
      IF(IMHARD.GT.0) WRITE(*,40)
   40 FORMAT(//' Comparison of Evaluated Data (COMPLOT 2019-1)'/
     1 1X,97('-')/
     2 ' All Data Agreed within input Allowable Uncertainty.'/
     3 ' So NO Plots were Generated. See the output file'/
     4 ' COMPLOT.LST for Details of the Comparisons.')
      ELSE
C-----ON SCREEN TEXT MESSAGE
      IF(IMHARD.GT.0) WRITE(*,50)
   50 FORMAT(//' Comparison of Evaluated Data (COMPLOT 2019-1)'/
     1 1X,97('-')/
     2 ' See output file COMPLOT.LST for Details of the Comparisons.')
      ENDIF
c-----Print final WARNING if data not tabulated to same Maximum Energy
      CALL MAXIE4(0)
C-----End of Run - Normal
      CALL ENDIT
C-----------------------------------------------------------------------
C
C     NEXT MAT FOUND
C
C-----------------------------------------------------------------------
C-----INITIALIZE RESONANCE REGION LIMITS AND ENDF/B FORMAT VERSION
C-----WHEN NEW MAT IS FOUND.
   60 IF(MATH.EQ.MATLST) GO TO 70
c-----If any preceding MAT check MT Maximum Energy
      if(MATLST.gt.0) then
      CALL MAXIE3(0)
      endif
c-----Initialize MT table for new MAT
      CALL MAXIE0
      MATLST=MATH
      RLIMIT(1,1)=0.0d0
      RLIMIT(2,1)=0.0d0
      RLIMIT(1,2)=0.0d0
      RLIMIT(2,2)=0.0d0
      IVERSE=6
C-----3/22/2012 - INITIALIZE INCIDENT PARTICLE TO NEUTRON.
      INPART=1
C-----LOCATE FILE 1, 2 OR 3.
   70 IF(MFH.NE.1) GO TO 90
C-----DEFINE ENDF/B FORMAT (ENDF/B-V OR VI).
      IF(MTH.NE.451) GO TO 80
      CALL FILE1
      GO TO 130
C-----FOR NU-BAR (TOTAL, DELAYED, PROMPT) USE DATA IF TABULATED.
   80 IF(MTH.NE.452.AND.MTH.NE.455.AND.MTH.NE.456) GO TO 130
      IF(L2H.eq.2) go to 140 ! Can only plot tabulated
      go to 130
   90 IF(MFH.NE.2) GO TO 100
C-----2016/11/25 - Added to insure only resonance parameters are read.
      IF(MTH.NE.151) GO TO 110
C-----DEFINE RESONANCE REGION ENERGY LIMITS.
      CALL FILE2(RLIMIT(1,1),RLIMIT(2,1),RLIMIT(1,2),RLIMIT(2,2))
      GO TO 110
  100 IF(MFH.EQ. 3) GO TO 140
      IF(MFH.EQ.10) GO TO 140
      IF(MFH.EQ.23) GO TO 140
      IF(MFH.EQ.27) GO TO 140
C-----------------------------------------------------------------------
C
C     2017/3/21 - Added for MF=4 data = very different format.
C
C-----------------------------------------------------------------------
      IF(MFH.ne. 4) go to 110
      call CARDIL(KTAPE1,C1,C2,L1,L2,N1,N2)
      LTT = L2H                              ! Legendre or Tabulated
      LI  = L1                               ! Isotropic or not
      LCT = L2                               ! CM or Lab
      if(LTT.ne.1.and.LTT.ne.3) go to 110    ! Only Legendre
      if(LI.ne.0) go to 110                  ! Only NOT Isotropic
      do k=1,2
      ILEG  (k) = 0
      NLEG  (k) = 0
      NPTLEG(k) = 0
      enddo
      go to 140
C-----SKIP SECTION
  110 CALL SKIPSL(KTAPE1)
      GO TO 10
C-----NO COMPARABLE SECTION ON SECOND FILE. PRINT MESSAGE AND SKIP
C-----CURRENT SECTION FROM FIRST FILE.
  120 CALL ZAHOL(IZA1,ZABCD)
      WRITE(OUTP,600) ZABCD,MAT1,MF1,MT1
C-----SKIP SECTION.
  130 CALL SKIPSL(KTAPE1)
      GO TO 20
C-----SAVE ZA, MAT AND MT.
  140 IZA1=C1H
      MAT1=MATH
      MF1=MFH
      MT1=MTH
C-----COMPARE MT TO REQUEST LIST.
      DO 170 IMATZA=1,NMATZA
      IF(MODGET.NE.0) GO TO 150
      IF(MAT1.LT.MATMIN(IMATZA).OR.MAT1.GT.MATMAX(IMATZA)) GO TO 170
      GO TO 160
  150 IF(IZA1.LT.MATMIN(IMATZA).OR.IZA1.GT.MATMAX(IMATZA)) GO TO 170
  160 IF(MF1.LT.MFMIN(IMATZA).OR.MF1.GT.MFMAX(IMATZA)) GO TO 170
      IF(MT1.LT.MTMIN(IMATZA).OR.MT1.GT.MTMAX(IMATZA)) GO TO 170
C-----CURRENT MT HAS BEEN REQUESTED. DEFINE WHEATHER OR NOT TO IDENTIFY
C-----DATA POINTS.
      NPOINT=0
      IF(IIPONT(IMATZA).GT.0) NPOINT=1
C----- 2017/4/2 - set ZOOM option based on input request
      IZOOM = MYZOOM(IMATZA)
C-----SET INTERACTIVE INPUT FLAG.
      NACTV=MACTV(IMATZA)
      GO TO 180
  170 CONTINUE
C-----MT NOT REQUESTED. SKIP SECTION.
      IMATZA = NMATZA
      CALL SKIPSL(KTAPE1)
      GO TO 20
C-----------------------------------------------------------------------
C
C     SEARCH FOR EQUIVALENT (ZA,MT).
C
C-----------------------------------------------------------------------
  180 IZAQ=IZA1
      MFQ=MF1
      MTQ=MT1
      TIMUSE=1.0d0
      IF(IQUATE.LE.0) GO TO 200
      DO 190 I=1,IQUATE
      IZAX=IZAQ1(I)
      IMFX=MFQ1(I)
      IMTX=MTQ1(I)
      IF(IZAX.LE.0) IZAX=IZA1
      IF(IMFX.LE.0) IMFX=MF1
      IF(IMTX.LE.0) IMTX=MT1
      IF(IZA1.NE.IZAX) GO TO 190
      IF(MF1.NE.IMFX) GO TO 190
      IF(MT1.NE.IMTX) GO TO 190
      IZAQ=IZAQ2(I)
      MFQ=MFQ2(I)
      MTQ=MTQ2(I)
      IF(IZAQ.LE.0) IZAQ=IZA1
      IF(MFQ.LE.0) MFQ=MF1
      IF(MTQ.LE.0) MTQ=MT1
      TIMUSE=TIMES(I)
      GO TO 200
  190 CONTINUE
C-----------------------------------------------------------------------
C
C     MT REQUESTED. AFTER FIRST PASS THROUGH SECOND TAPE SEE IF
C     (ZA,MT) IS ON SECOND TAPE WITHOUT SEARCHING.
C
C-----------------------------------------------------------------------
  200 IF(MPASS.LE.1) GO TO 220
      IF(MTSIZE.LE.0) GO TO 120
      DO 210 JSPOT=1,MTSIZE
      IF(IZAQ.EQ.IZATAB(JSPOT).AND.
     1    MFQ.EQ.MFTAB (JSPOT).AND.
     2    MTQ.EQ.MTTAB (JSPOT)) GO TO 220
  210 CONTINUE
      GO TO 120
C-----INITIALIZE NON-LINEAR INTERPOLATION FLAG.
  220 IMLINR=0
C-----LOAD FIRST SECTION INTO PAGING SYSTEM.
      IF(MF1.EQ.10) THEN
C-----DEFINE THE NUMBER OF SECTIONS IN FILE1 AND INITIALIZE COUNT
      NS101=N1H
      JS101=0
      ENDIF
      CALL FILE3(1)
      IF(MF1.EQ.10) THEN
C-----IDENTIFY THE PRODUCT AND ITS FINAL STATE IN FILE1
C-----INCREMENT THE COUNTER OF MF10 SECTIONS
      JS101=JS101+1
      IZAP1=L1
      LFS1 =L2
      ENDIF
      MATCH=MPASS
C-----------------------------------------------------------------------
C
C     SEARCH FOR CORRESPONDING SECTION ON SECOND TAPE.
C
C-----------------------------------------------------------------------
C-----IF REQUIRED SECTION PRECEEDS PRESENT POSITION OF SECOND TAPE
C-----REWIND SECOND TAPE.
      IF(MPASS.GT.1.AND.JSPOT.LT.ISPOT) GO TO 230
C-----POSITION TAPE BEFORE READING SECOND TAPE THE FIRST TIME.
      IF(MPASS.GT.0) GO TO 240
  230 MPASS=MPASS+1
      ISPOT=0
      IF(MPASS.GT.1) REWIND KTAPE2
      CALL COPY1L(KTAPE2)
      IF(MPASS.LE.1) WRITE(OUTP,630) LABEL,NLABEL(1)
  240 CALL CONTIL(KTAPE2)
      IF(MTH.gt.0) go to 260
      IF(MATH.ge.0) go to 240
C-----END OF TAPE. ONLY SEARCH ENTIRE TAPE ONCE. IF NO MATCH FOUND
C-----SKIP SECTION ON FIRST TAPE.
  250 IF(MPASS.le.MATCH) go to 230
      MPASS=MPASS+1
      REWIND KTAPE2
      CALL SKIP1L(KTAPE2)
      GO TO 120
  260 IZA2=C1H
      MF2=MFH
      MT2=MTH
      ISPOT=ISPOT+1
      IF(MTSIZE.le.0) go to 270
      if(IZA2.gt.IZATAB(MTSIZE).or.
     1    MF2.gt. MFTAB(MTSIZE).or.
     2    MT2.gt. MTTAB(MTSIZE)) go to 270
      go to 280
  270 MTSIZE=MTSIZE+1
      IZATAB(MTSIZE)=IZA2
      MFTAB(MTSIZE)=MF2
      MTTAB(MTSIZE)=MT2
C-----FOR NU-BAR USE ONLY IF TABULATED.
  280 IF(MFH.NE.1) GO TO 290
      IF(MTH.NE.452.AND.MTH.NE.455.AND.MTH.NE.456) GO TO 320
      IF(L2H.eq.2) go to 310  ! Can only plot tabulated
      go to 320
  290 IF(MFH.EQ. 3) GO TO 310
      IF(MFH.EQ.10) GO TO 310
      IF(MFH.EQ.23) GO TO 310
      IF(MFH.EQ.27) GO TO 310
C-----------------------------------------------------------------------
C
C     2017/3/20 - Added for MF=4 data
C
C-----------------------------------------------------------------------
      IF(MFH.eq. 4) then
      call CARDIL(KTAPE2,C1,C2,L1,L2,N1,N2)
      if(L2H.ne.1.and.L2H.ne.3) go to 300    ! Only Legendre
      if(L1.ne.0) go to 300                  ! Only NOT Isotropic
      if(L2.ne.LCT) go to 300                ! Same system LAB or CM
      go to 310
      endif
C-----SKIP SECTION
  300 CALL SKIPSL(KTAPE2)
      IF(MATH.lt.0) go to 250
      go to 240
C-----Poosible Match
  310 IF(MF2.EQ.10) then
      NS102=N1H
      JS102=0
      ENDIF
      IF(IZA2.EQ.IZAQ.AND.MF2.EQ.MFQ.AND.MT2.EQ.MTQ) GO TO 330
C-----SKIP SECTION.
  320 CALL SKIPSL(KTAPE2)
      IF(MATH.lt.0) go to 250
      go to 240
C-----LOAD SECTION INTO PAGING SYSTEM.
  330 CALL FILE3(2)
C-----------------------------------------------------------------------
C
C     Same ZA, MF, MT - No longer need to test ZA, MF, MT.
C     Stay here until all sub-sections for MF=10 have been pricessed.
C
C     Make sure to skip to end of BOTH MF=10 sections when finished.
C
C-----------------------------------------------------------------------
      IF(MF.ne.10) go to 390
      JS102=JS102+1
      IZAP2=L1
      LFS2 =L2
c-----------------------------------------------------------------------
C
C     Loop to here over ZAP/LFS sections - Skip whichever is less
C
c-----------------------------------------------------------------------
  340 IF(IZAP1.lt.IZAP2) go to 350
      IF(IZAP1.gt.IZAP2) go to 370
      IF(LFS1 .eq.LFS2 ) go to 390
      IF(LFS1 .gt.LFS2 ) go to 370
c-----------------------------------------------------------------------
C
C     First is less
C
c-----------------------------------------------------------------------
C-----Exception is IZAP1 = 0 sorted at end = try skipping #2
  350 IF(JS101.eq.NS101.and.IZAP1.eq.0) go to 380
C-----First File is less = current section = No Comparison
  360 CALL ZAHOL(IZA1,ZABCD)
      WRITE(OUTP,610) ZABCD,MAT1,MF1,MT1,IZAP1,LFS1
C-----Skip #1
      IF(JS101.ge.NS101) go to 580  ! end of First file
      CALL FILE3(1)
      JS101=JS101+1
      IZAP1=L1
      LFS1 =L2
      go to 340
c-----------------------------------------------------------------------
C
C     Second is less
C
c-----------------------------------------------------------------------
C-----Exception is IZAP2 = 0 sorted at end = try skipping #1
  370 IF(JS102.eq.NS102.and.IZAP2.eq.0) go to 360
C-----Second File is less - skip or end - at end treat remaining first.
  380 IF(JS102.ge.NS102) go to 570  ! end of Second file
      CALL FILE3(2)
      JS102=JS102+1
      IZAP2=L1
      LFS2 =L2
      go to 340
C-----------------------------------------------------------------------
C
C     Comparable section found = PLOT.
C
C     This section may generate multiple plots - up to one per
C     input request (NMATZA).
C
C-----------------------------------------------------------------------
C-----IGNOR SMALL DIFFERENCES IN THRESHOLD.
  390 IF(DABS(THRES3(1)-THRES3(2)).LE.0.0001d0*THRES3(1))
     1 THRES3(2)=THRES3(1)
C-----DEFINE HIGHEST THRESHOLD AS STARTING POINT.
      THRES3(3)=THRES3(1)
      IF(THRES3(2).GT.THRES3(3)) THRES3(3)=THRES3(2)
c-----------------------------------------------------------------------
C
C     FOR NU-BAR USE LINEAR ENERGY SCALING - OTHERWISE USE AUOTMATIC.
C
c-----------------------------------------------------------------------
      NXPLAN=0
c-----2017/4/2 - Allow input to override DEFAULT X axis scaiing
c-----           basded on input parameters
      if(IZOOM.le.0) then
c-----Make MF=1 nu-bar, Linear X scale
      IF(MFH.EQ.1) NXPLAN=1
c-----2017/3/23 - Make MF=4 Linear X scale
      IF(MFH.EQ.4) NXPLAN=1
      endif
c-----------------------------------------------------------------------
C
C     CORRESPONDING SECTIONS FOUND. SET UP LOOP OVER REQUESTS TO DEFINE
C     REQUESTED ENERGY RANGES.
C
c-----------------------------------------------------------------------
C-----IDENTIFY MATERIAL.
      CALL ZAHOL(IZA1,ZABCD)
C-----DEFINE RATIOS.
      IUSE1(1)=MTDEX1(1)
      IUSE2(1)=MTDEX2(1)
      IUSE1(2)=MTDEX1(2)
      IUSE2(2)=MTDEX2(2)
      CALL RATIOS
C-----DEFINE ALL PLOT LABEL INFORMATION.
C-----2017/3/20 - Added LCT, LTT, ILEG for MF=4 Data
C-----2017/3/23 - Dropped Q1 (Use MT=### instead)
      CALL SETLAB(IZA1,MAT1,MF1,MT1,   LCT,LTT,ILEG(1))
c-----------------------------------------------------------------------
C
C     COMPARE TO REQUEST LIST TO DEFINE REQUESTED ENERGY RANGE.
C
c-----------------------------------------------------------------------
      DO 560 IMATZA=1,NMATZA
      IF(MODGET.NE.0) GO TO 400
      IF(MAT1.LT.MATMIN(IMATZA).OR.MAT1.GT.MATMAX(IMATZA)) GO TO 560
      GO TO 410
  400 IF(IZA1.LT.MATMIN(IMATZA).OR.IZA1.GT.MATMAX(IMATZA)) GO TO 560
  410 IF(MF1.LT.MFMIN(IMATZA).OR.MF1.GT.MFMAX(IMATZA)) GO TO 560
      IF(MT1.LT.MTMIN(IMATZA).OR.MT1.GT.MTMAX(IMATZA)) GO TO 560
c-----------------------------------------------------------------------
C
C     CURRENT MT HAS BEEN REQUESTED. DEFINE X AND Y RANGES WITHIN
C     REQUESTED ENERGY RANGE.
C
c-----------------------------------------------------------------------
C
C     DEFINE X RANGE DIRECTLY.
C
c-----------------------------------------------------------------------
C-----MAXIMUM X RANGE FOR BOTH SETS.
      XZOOM(1)=XYLIMS(1,1,1)
      XZOOM(2)=XYLIMS(2,1,1)
      IF(XYLIMS(1,1,2).LT.XZOOM(1)) XZOOM(1)=XYLIMS(1,1,2)
      IF(XYLIMS(2,1,2).GT.XZOOM(2)) XZOOM(2)=XYLIMS(2,1,2)
C-----NOT TO EXCEED REQUESTED X RANGE.
      IF(XZOOM(1).LT.ERMIN(IMATZA)) XZOOM(1)=ERMIN(IMATZA)
      IF(XZOOM(2).GT.ERMAX(IMATZA)) XZOOM(2)=ERMAX(IMATZA)
c-----------------------------------------------------------------------
C
C     DEFINE Y RANGES.
C
c-----------------------------------------------------------------------
  420 CALL ZOOMER
C-----DEFINE WHEATHER OR NOT TO IDENTIFY DATA POINTS.
      NPOINT=0
      IF(IIPONT(IMATZA).GT.0) NPOINT=1
C-----SET INTERACTIVE INPUT FLAG.
      NACTV=MACTV(IMATZA)
C-----DEFINE ENERGY RANGE FOR OUTPUT.
      ES=XZOOM(1)
      CALL OUT9(ES,FIELD11(1,1))
      ES=XZOOM(2)
      CALL OUT9(ES,FIELD11(1,2))
c-----------------------------------------------------------------------
c
C     IF NO CROSS SECTION IN REQUESTED ENERGY RANGE SKIP.
c
c-----------------------------------------------------------------------
      IF(IUSE2(1).GT.0.AND.IUSE2(2).GT.0) GO TO 480
      if(MF1.ne.4) go to 440
      WRITE(OUTP,430) ZABCD,MAT1,MF1,MT1,ILEG(1),
     1 ((FIELD11(kk,I),kk=1,11),I=1,2)
  430 FORMAT(1X,10A1,I5,I3,I4,   5x,'f',i1,4x,     11A1,11A1,
     1 ' No Data in Range')
      go to 560
  440 IF(MF1.ne.10) go to 460
      WRITE(OUTP,450) ZABCD,MAT1,MF1,MT1,IZAP1,LFS1,
     1 ((FIELD11(kk,I),kk=1,11),I=1,2)
  450 FORMAT(1X,10A1,I5,I3,I4,   I7,I4,            11A1,11A1,
     1 ' No Data in Range')
      go to 560
  460 WRITE(OUTP,470) ZABCD,MAT1,MF1,MT1,
     1 ((FIELD11(kk,I),kk=1,11),I=1,2)
  470 FORMAT(1X,10A1,I5,I3,I4,   11X  ,            11A1,11A1,
     1 ' No Data in Range')
      GO TO 560
c-----------------------------------------------------------------------
C
C     OUTPUT SUMMARY.
C
c-----------------------------------------------------------------------
C-----SET MARKERS TO INDICATE WHICH DIFFERENCES EXCEED ALLOWABLE VALUES.
  480 MARK1=BLANK
      IF(YMIN.LT.DIFMIN) MARK1=STAR
      MARK2=BLANK
      IF(YMAX.GT.DIFMAX) MARK2=STAR
C-----DEFINE THE ENERGIES WHERE MIN AND MAX RATIOS OCCUR.
      CALL OUT9(XMIN,FIELD11(1,3))
      CALL OUT9(XMAX,FIELD11(1,4))
      if(MF1.ne.4) go to 500
      WRITE(OUTP,490) ZABCD,MAT1,MF1,MT1,ILEG(1),
     1 ((FIELD11(kk,I),kk=1,11),I=1,2),
     2  (FIELD11(kk,3),kk=1,11),YMINPC,MARK1,
     3  (FIELD11(kk,4),kk=1,11),YMAXPC,MARK2
  490 FORMAT(1X,10A1,I5,I3,I4,   5X,'f',I1,4X,   11A1,11A1,11A1,F9.3,
     1 A1,11A1,F9.3,A1)
      go to 540
  500 IF(MF1.ne.10) go to 520
c-----Add ZAP and LFS for MF=10
      WRITE(OUTP,510) ZABCD,MAT1,MF1,MT1,IZAP1,LFS1,
     1 ((FIELD11(kk,I),kk=1,11),I=1,2),
     2  (FIELD11(kk,3),kk=1,11),YMINPC,MARK1,
     3  (FIELD11(kk,4),kk=1,11),YMAXPC,MARK2
  510 FORMAT(1X,10A1,I5,I3,I4,   I7,I4,          11A1,11A1,11A1,F9.3,
     1 A1,11A1,F9.3,A1)
      go to 540
  520 WRITE(OUTP,530) ZABCD,MAT1,MF1,MT1,
     1 ((FIELD11(kk,I),kk=1,11),I=1,2),
     2  (FIELD11(kk,3),kk=1,11),YMINPC,MARK1,
     3  (FIELD11(kk,4),kk=1,11),YMAXPC,MARK2
  530 FORMAT(1X,10A1,I5,I3,I4,   11X            ,11A1,11A1,11A1,F9.3,
     1 A1,11A1,F9.3,A1)
c-----------------------------------------------------------------------
c
C     PRINT WARNING IF MASTER CROSS SECTION IS ZERO.
c
c-----------------------------------------------------------------------
  540 IF(MYZERO.EQ.0) GO TO 550
      CALL OUT9(EZERO,FIELD11(1,1))
      WRITE(OUTP,640) MYZERO,(FIELD11(kk,1),kk=1,11)
C-----2013/3/21 - ONLY PRINT MESSAGE ONCE.
      MYZERO = 0
c-----------------------------------------------------------------------
C
C     NO PLOT IF EVALUATIONS AGREE TO WITHIN ALLOWABLE DIFFERENCE
C     AND DATA1 IS NOT ZERO WHERE DATA2 IS NON-ZERO.
C
c-----------------------------------------------------------------------
c-----2013/3/21 - removed MYZERO as criteria.
  550 IF(YMIN.GE.DIFMIN.AND.YMAX.LE.DIFMAX) GO TO 560
C-----IF REQUESTED PLOT DATA.
      IF(MODPLT.LT.0) GO TO 560
C-----IDENTIFY MIN AND MAX DIFFEENCES IN PLOT LABELS.
      CALL GOOFIE
C-----PLOT RESULTS.
      CALL GPPLOT(IMACTV,0)
C-----IF INTERACTIVE PLOT IS REQUESTED DO IT.
      IF(IMACTV.NE.0) GO TO 420
  560 CONTINUE
      IMATZA = NMATZA
C-----PRINT WARNING IF DATA IS NOT ALL LINEARLY INTERPOLABLE.
      IF(IMLINR.NE.0) WRITE(OUTP,650)
C-----PROCESSING OF SECTION COMPLETE. BRANCH TO FIND NEXT COMPARABLE
C-----SECTION.
C-----------------------------------------------------------------------
C
C     Continue to end of MF=4, one Legendre coefficient at a time
C
C-----------------------------------------------------------------------
      if(ILEG(1).lt.NLEG(1).and.
     1   ILEG(2).lt.NLEG(2)) then
      CALL FILE3(1)
      CALL FILE3(2)
      go to 390
      endif
C-----------------------------------------------------------------------
C
C     Finished if not MF=10 - Continue MF=10 to end
C
C-----------------------------------------------------------------------
      IF(MF1.ne.10) go to 580
      IF(JS101.ge.NS101) go to 580  ! End of First File?
c-----load next sub-section from First.
      CALL FILE3(1)
      JS101=JS101+1
      IZAP1=L1
      LFS1 =L2
      IF(JS102.ge.NS102) go to 570  ! End of Second File?
c-----load next sub-section from Second.
      CALL FILE3(2)
      JS102=JS102+1
      IZAP2=L1
      LFS2 =L2
      go to 340  ! Compare ZAP/LFS
C-----End of Second File - remaining first No Comparable Data
  570 WRITE(OUTP,610) ZABCD,MAT1,MF1,MT1,IZAP1,LFS1
      IF(JS101.ge.NS101) go to 580
      CALL FILE3(1)
      JS101=JS101+1
      IZAP1=L1
      LFS1 =L2
      go to 570
c-----------------------------------------------------------------------
C
C     End of section - insure both files are at SEND
c
c-----------------------------------------------------------------------
  580 CALL SKIPSL(KTAPE1)
      CALL SKIPSL(KTAPE2)
      go to 10            ! Search for next requested section
  590 FORMAT(1X,97('-')/I9,' Plots Generated'/1X,97('-')/
     1 ' * - Indicates Difference Exceeds',F9.4,' per-cent'/1X,97('-')/
     2 ' per-cent Difference         = 100 * ((Data1-Data2)/Data1)'/
     3 '                               at Each Energy Point'/
     4 ' Maximum per-cent Difference = Largest per-cent Difference at'/
     6 '                               ANY One or More Energy Points'/
     7 1X,97('-'))
  600 FORMAT(1X,10A1,I5,I3,I4,11X,  ' No Comparable Data')
  610 FORMAT(1X,10A1,I5,I3,I4,I7,I4,' No Comparable Data')
  620 FORMAT(1X,97('-')/' ENDF/B Tape Labels'/1X,97('-')/
     1 ' Data1=',16A4,A2,I4)
  630 FORMAT(' Data2=',16A4,A2,I4/1X,97('-')/
     1 ' Material    MAT MF  MT    ZAP LFS Energy Range',
     1 '           Maximum per-cent Differences'/
     3 '                           or      Minimum    Maximum',
     4 '    Maximum Negative     Maximum Positive'/
     5 '                           f#      eV         eV     ',
     6 '    eV         per-cent  eV         per-cent'/
     7 1X,97('-'))
  640 FORMAT(1X,97('-')/
     1 ' WARNING - For Above Comparison Could NOT Define Ratio at ALL',
     2 ' Energies.'/
     3 '           Data1 is Zero at ',I6,' Energies where Data2 is NOT',
     4 ' Zero.'/
     5 '           First Occurrence at ',11A1,' eV.'/
     6 1X,97('-'))
  650 FORMAT(1X,97('-')/
     1 ' WARNING - The Above Data is NOT All Histogram or Linearly',
     2 ' Interpolable.'/
     2 '           For Plot ALL Data has been Treated as Linearly',
     3 ' Interpolable.'/
     3 '           It is Recommended that You Linearize ALL Data',
     3 ' Using Program'/
     4 '           LINEAR and then Again Compare this Data.'/
     4 1X,97('-'))
      END
      SUBROUTINE READIN(DIFFOK)
C=======================================================================
C
C     READ AND CHECK ALL INPUT PARAMETERS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      REAL*4 XYSS
c***** 2017/4/6 - added characters to read input
      CHARACTER*1 DIVIDE,FIELD1,FIELD2,FIELD3,FIELD4,FIELD5,EVALID
      CHARACTER*4 YES,NO,MESS1,KBORD,KPOINT,KACTV
      CHARACTER*16 GRIDTYPE
      CHARACTER*72 NAMEIN1,NAMEIN2
      CHARACTER*84 EVALID84,DATA1,DATA2,BLANK84
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/IDEVAL/EVALID(84,3)
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1
      COMMON/RATOK/RATMIN,RATMAX
      COMMON/MINDY/DYMIN
C-----11/22/2013 - Substituted OUT9 output for NORMX
      CHARACTER*1 FIELD11
      COMMON/NORMCC/FIELD11(11,6)
      COMMON/PAGE1/NPAGE1
      COMMON/MULTHT/HTMULT
      COMMON/NAMEX/NAMEIN1,NAMEIN2
      COMMON/PALCOM/MYPAL
      INCLUDE 'complot.h'
c***** 2017/4/6 - added characters to read input
      DIMENSION MESS1(2),FIELD1(11),FIELD2(11),FIELD3(11),FIELD4(11),
     1 FIELD5(11),XYSS(2,2),GRIDTYPE(6),EVALID84(3)
      EQUIVALENCE (EVALID84(1),EVALID(1,1))
      DATA YES/' Yes'/
      DATA NO/'  No'/
C-----Default ENDF Data IDs
      DATA DATA1  /'Data 1'/
      DATA DATA2  /'Data 2'/
      DATA BLANK84/'      '/
C-----DEFINE DEFAULT UPPER ENERGY LIMIT.
      DATA EBIG/1.0d+12/
C-----DEFINE STANDARD OPTION FOR ALLOWABLE DIFFERENCE (CURRENTLY 0.5
C-----PER-CENT).
      DATA DIFUSE/1.0d-3/
C-----DEFINE LABELS TO INDICATE RETRIEVAL BY ZA OR MAT.
      DATA MESS1/' MAT','  ZA'/
      DATA DIVIDE/'/'/
      DATA GRIDTYPE/
     1 'Tick Marks      ',
     2 'Solid (Coarse)  ',
     3 'Dashed (Coarse) ',
     4 'Solid (Fine)    ',
     5 'Dashed (Fine)   ',
     5 'Solid/Dashed    '/
C-----2017/5/6 - Initialize character fields for input
      do i=1,11
      FIELD1(i) = ' '
      FIELD2(i) = ' '
      FIELD3(i) = ' '
      FIELD4(i) = ' '
      FIELD5(i) = ' '
      enddo
C-----PRINT HEADING FOR OUTPUT LISTING.
      WRITE(OUTP,370)
c-----------------------------------------------------------------------
C
C     DEFINE SIZE OF PLOTTER.
C
c-----------------------------------------------------------------------
C-----ONLY READ INPUT IF OPENED O.K.
      IF(ISTAT1.EQ.1) GO TO 20
c***** 2017/5/6 - changed all floating to character.
      READ(INP,10,END=20,ERR=20) FIELD1,FIELD2,FIELD3,FIELD4,
     1 NSMALX,NSMALY,(FIELD5(i),i=8,11)
   10 FORMAT(44A1,2I11,4A1)
      CALL IN9(XYPLOT(1,1),FIELD1)
      CALL IN9(XYPLOT(2,1),FIELD2)
      CALL IN9(XYPLOT(1,2),FIELD3)
      CALL IN9(XYPLOT(2,2),FIELD4)
      CALL IN9(HTMULT     ,FIELD5)
c***** 2017/5/6 - changed all floating to character.
      GO TO 30
C-----DEFINE DEFAULT VALUES
   20 ISTAT1 = 1
      XYPLOT(1,1) =  0.0d0
      XYPLOT(2,1) = 12.5d0
      XYPLOT(1,2) =  0.0d0
      XYPLOT(2,2) = 10.0d0
      NSMALX = 1
      NSMALY = 1
      HTMULT = 1.5d0
c-----------------------------------------------------------------------
C
C     DEFINE LOCAL PLOTTER SIZE.
C
c-----------------------------------------------------------------------
c-----2017/8/31 - Use "local" size only if not defined
   30 if(XYPLOT(2,1).le.0.0d0.or.
     1   XYPLOT(2,2).le.0.0d0) then
      CALL MYSIZE(XYSS(1,1),XYSS(1,2))
      DO 40 I=1,2
      XYPLOT(I,1)=XYSS(I,1)
   40 XYPLOT(I,2)=XYSS(I,2)
      endif
c-----2017/8/31 - Use "local" size only if not defined
c-----------------------------------------------------------------------
C
C     DEFINE WHETHER OR NOT THE PROGRAM IS RUNNING INTERACTIVELY
C
c-----------------------------------------------------------------------
      CALL INTERACT(MYACTION)
C-----DEFINE PLOT ORIENTATION.
      NFLIP=0
      IF(XYPLOT(2,1).LT.0.0d0) NFLIP=1
      XYPLOT(2,1)=DABS(XYPLOT(2,1))
C-----IF PLOT SIZE IS NOT GIVEN USE STANDARD SIZE.
      DO 50 I=1,2
      DO 50 J=1,2
   50 IF(XYPLOT(I,J).EQ.0.0d0) XYPLOT(I,J)=XYEDGE(I,J)
      IF(NSMALX.LT.1) NSMALX=1
      IF(NSMALY.LT.1) NSMALY=1
      WRITE(OUTP,380) XYPLOT,NSMALX,NSMALY
c-----2014/7/20 - Restricted character size multiplier to 0.5 to 1.5
c-----Otherwise MF=10 titles do not fit on plot.
c-----2019/4/09 - Changed to Default = 1.5 and Range      0.5 to 2.5
      IF(HTMULT.LE.0.0d0) HTMULT=1.5d0  ! Default 1.5
      IF(HTMULT.LT.0.5d0) HTMULT=0.5d0  ! Minimum
      IF(HTMULT.GT.2.5d0) HTMULT=2.5d0  ! Maximum
      WRITE(OUTP,450) HTMULT
      IF(NFLIP.EQ.0) WRITE(OUTP,400)
      IF(NFLIP.NE.0) WRITE(OUTP,410)
c-----------------------------------------------------------------------
C
C     READ FILENAMES - USE STANDARD IF BLANK.
C
c-----------------------------------------------------------------------
      IF(ISTAT1.EQ.1) GO TO 70
      READ(INP,60,END=70,ERR=70) NAMEIN1
      IF(NAMEIN1.EQ.' ') NAMEIN1 = 'ENDFB.IN1'
      READ(INP,60,END=80,ERR=80) NAMEIN2
   60 FORMAT(A72)
      IF(NAMEIN2.EQ.' ') NAMEIN2 = 'ENDFB.IN2'
      GO TO 90
C-----USE DEFAULT IF NO INPUT
   70 NAMEIN1 = 'ENDFB.IN1'
   80 NAMEIN2 = 'ENDFB.IN2'
      ISTAT1 = 1
C-----PRINT FINAL FILENAMES
   90 WRITE(OUTP,100) NAMEIN1,NAMEIN2
  100 FORMAT(' Data Filenames'/
     1 1X,97('-')/' Data1=',A72/' Data2=',A72/1X,97('-'))
C-----OPTIONALLY OPEN ENDF/B DATA FILES.
      CALL FILIO2
c-----------------------------------------------------------------------
C
C     READ, TEST AND LIST
C     (1) MAT OR ZA SELECTOR
C     (2) GRID/SPEED OPTION.
C     (3) BORDER ON PLOT
C     (4) LINE THICKNESS
C     (5) PLOT MODPLT (-1 TO 4).
C
c-----------------------------------------------------------------------
      IF(ISTAT1.EQ.1) GO TO 120
      READ(INP,110,END=120,ERR=120)
     1 MODGET,NGRID,NBORD,NTHICK,MODPLT,NPAGE,MYPAL
  110 FORMAT(6I11,I4)
      GO TO 130
C-----DEFINE DEFAULT VALUES
  120 ISTAT1 = 1
      MODGET = 0
      NGRID  = 0
      NBORD  = 0
      NTHICK = 0
      MODPLT = 4
      NPAGE  = 0
      MYPAL  = 0
C-----CHECK INPUT VALUES
  130 IF(MODGET.LE.0) MODGET=0
      IF(MODGET.GT.0) MODGET=1
      IF(NGRID.LE.0.OR.NGRID.GT.5) NGRID=0
      IF(NBORD.LT.0.OR.NBORD.GT.2) NBORD=0
      KBORD=NO
      IF(NBORD.NE.0) KBORD=YES
      IF(IABS(NTHICK).GT.5) NTHICK=0
      KTHICK=IABS(NTHICK)
      IF(MODPLT.LT.-1.OR.MODPLT.GT.4) MODPLT=-1
      WRITE(OUTP,390) MESS1(MODGET+1),GRIDTYPE(NGRID+1),
     1 KBORD,KTHICK
      IF(MODPLT.EQ.-1) WRITE(OUTP,490)
      IF(MODPLT.EQ.0) WRITE(OUTP,500)
      IF(MODPLT.EQ.1) WRITE(OUTP,510)
      IF(MODPLT.EQ.2) WRITE(OUTP,520)
      IF(MODPLT.EQ.3) WRITE(OUTP,530)
      IF(MODPLT.EQ.4) WRITE(OUTP,540)
      IF(NPAGE.LT.0) NPAGE=0
      WRITE(OUTP,550) NPAGE
      IF(MYPAL.EQ.0) THEN
      WRITE(OUTP,560)
      ELSE
      WRITE(OUTP,570)
      ENDIF
      NPAGE1=NPAGE
c-----------------------------------------------------------------------
C
C     READ, TEST AND LIST
C     (1) ALLOWABLE UNCERTAINTY
C     (2) MAXIMUM RANGE OF RATIOS.
C
c-----------------------------------------------------------------------
      IF(ISTAT1.EQ.1) GO TO 150
c***** 2017/5/6 - changed all floating to character.
      READ(INP,140,END=150,ERR=150) FIELD1,FIELD2
  140 FORMAT(22A1)
      CALL IN9(DIFFOK,FIELD1)
      CALL IN9(RATMAX,FIELD2)
c***** 2017/5/6 - changed all floating to character.
      GO TO 160
C-----DEFINE DEFAULT VALUES
  150 ISTAT1 = 1
      DIFFOK = DIFUSE
      RATMAX = 1.0d0
C-----CHECK INPUT VALUES (10/3/15 - Now allow 0)
  160 IF(DIFFOK.EQ.0.0d0) DIFFOK=DIFUSE
      PCDIFF=100.0d0*DIFFOK
      WRITE(OUTP,420) DIFFOK,PCDIFF
c-----------------------------------------------------------------------
C
C     DEFINE MINIMUM RANGE FOR RATIOS
C
c-----------------------------------------------------------------------
      DYMIN  = 0.01d0
      IF(RATMAX.LE.0.0d0.OR.RATMAX.EQ.1.0d0) THEN
      RATMAX = 1.0d0
      RATMIN = 1.0d0
      WRITE(OUTP,430)
      ELSE
      IF(RATMAX.LT.1.0d0) RATMAX = 1.0d0/RATMAX
      DDD = RATMAX - 1.0d0
      IF(DDD.LT.1.0d-5) DDD = 1.0d-5
      RATMAX = 1.0d0 + DDD
      RATMIN = 1.0d0/RATMAX
      IF(DDD.LT.DYMIN) DYMIN = DDD
      PCRAT=100.0d0*(RATMAX-1.0d0)
      WRITE(OUTP,440) RATMAX,PCRAT
      ENDIF
c-----------------------------------------------------------------------
C
C     READ AND DEFINE LENGTH OF THE TWO EVALUATION IDENTIFICATIONS.
C
c-----------------------------------------------------------------------
      EVALID84(1) = DATA1
      EVALID84(2) = DATA2
      EVALID84(3) = BLANK84
      IF(ISTAT1.EQ.1) GO TO 180
      READ(INP,170,END=180,ERR=180) (EVALID(J,1),J=1,40)
      READ(INP,170,END=190,ERR=190) (EVALID(J,2),J=1,40)
  170 FORMAT(40A1)
      GO TO 200
  180 EVALID84(1) = 'Data 1'
  190 EVALID84(2) = 'Data 2'
      ISTAT1 = 1
  200 WRITE(OUTP,590) ((EVALID(J,I),J=1,40),I=1,2)
c-----------------------------------------------------------------------
C
C     DEFINE IDENTIFICATION FOR RATIO...THE TWO IDENTIFICATIONS
C     SEPARATED BY /
C
c-----------------------------------------------------------------------
      CALL LONGX(EVALID(1,1),IT1,40)
      CALL LONGX(EVALID(1,2),IT2,40)
      DO 210 J=1,IT2
  210 EVALID(J,3)=EVALID(J,2)
      IT3=IT2+1
      EVALID(IT3,3)=DIVIDE
      DO 220 J=1,IT1
      IT3=IT3+1
  220 EVALID(IT3,3)=EVALID(J,1)
C-----------------------------------------------------------------------
C
C     READ SELECTION RANGES (EITHER MAT OR ZA AND MT AND ENERGY - EV).
C     IF MINIMUM AND MAXIMUM OF FIRST REQUEST ARE BOTH ZERO SET LIMITS
C     TO RETRIEVE ALL DATA. IF MAXIMUM OF REQUEST IS LESS THAN MINIMUM
C     SET MAXIMUM EQUAL TO MINIMUM (I.E., ONLY RETRIEVE THE MAT OR ZA
C     INDICATED BY THE LOWER LIMIT).
C
C-----------------------------------------------------------------------
      IF(MODGET.EQ.0) WRITE(OUTP,460)
      IF(MODGET.EQ.1) WRITE(OUTP,470)
      IF(ISTAT1.EQ.1) GO TO 240
      READ(INP,230,END=240,ERR=240) MATMIN(1),MFMIN(1),MTMIN(1),FIELD1,
     1 MATMAX(1),MFMAX(1),MTMAX(1),FIELD2,IIPONT(1)
  230 FORMAT(I6,I2,I3,11A1,I6,I2,I3,11A1,2I11)
      GO TO 250
C-----DEFINE DEFAULT VALUES
  240 ISTAT1    = 1
      MATMIN(1) = 0
      MFMIN(1)  = 0
      MTMIN(1)  = 0
      MATMAX(1) = 9999
      MFMAX(1)  = 99
      MTMAX(1)  = 999
      ERMIN(1)  = 0.0d0
      ERMAX(1)  = EBIG
      IIPONT(1) = 0
      MYZOOM(1) = 0
      GO TO 260
  250 CALL IN9(ERMIN(1),FIELD1)
      CALL IN9(ERMAX(1),FIELD2)
      MYZOOM(1) = 0
      if(ERMIN(1).gt.0.0d0.or.ERMAX(1).gt.0.0d0) MYZOOM(1) = 1
  260 MACTV(1)=MYACTION
      IF(MFMIN(1).LE.0) MFMIN(1)=0
      IF(MFMAX(1).LE.0) MFMAX(1)=99
      IF(MTMIN(1).LE.0) MTMIN(1)=0
      IF(MTMAX(1).LE.0) MTMAX(1)=999
      IF(ERMIN(1).LE.0.0d0) ERMIN(1)=0.0d0
      IF(ERMAX(1).LE.ERMIN(1)) ERMAX(1)=EBIG
      IIPONT(1)=IABS(IIPONT(1))
      IF(IIPONT(1).NE.0) IIPONT(1)=1
      IF(MACTV(1).NE.0) MACTV(1)=1
      NPOINT=IIPONT(1)
      KPOINT=YES
      IF(IIPONT(1).EQ.0) KPOINT=NO
      KACTV=YES
      IF(MACTV(1).EQ.0) KACTV=NO
      ES=ERMIN(1)
      CALL OUT9(ES,FIELD11(1,1))
      ES=ERMAX(1)
      CALL OUT9(ES,FIELD11(1,2))
      IF(MATMIN(1).GT.0.OR.MATMAX(1).GT.0) GO TO 270
      MATMIN(1)=0
      MATMAX(1)=9999
      MODGET=0
      WRITE(OUTP,480) MATMIN(1),MFMIN(1),MTMIN(1),
     1 (FIELD11(kk,1),kk=1,11),
     1 MATMAX(1),MFMAX(1),MTMAX(1),(FIELD11(kk,2),kk=1,11),
     2 KPOINT,KACTV
      NMATZA=2
      GO TO 320
  270 IF(MATMAX(1).LT.MATMIN(1)) MATMAX(1)=MATMIN(1)
      WRITE(OUTP,480) MATMIN(1),MFMIN(1),MTMIN(1),
     1 (FIELD11(kk,1),kk=1,11),
     1 MATMAX(1),MFMAX(1),MTMAX(1),(FIELD11(kk,2),kk=1,11),
     2 KPOINT,KACTV
      DO 310 NMATZA=2,101
      IF(ISTAT1.EQ.1) GO TO 280
      READ(INP,230,END=280,ERR=280)
     1 MATMIN(NMATZA),MFMIN(NMATZA),MTMIN(NMATZA),FIELD1,
     2 MATMAX(NMATZA),MFMAX(NMATZA),MTMAX(NMATZA),FIELD2,IIPONT(NMATZA)
      GO TO 290
C-----DEFINE DEFAULT VALUES
  280 ISTAT1          = 1
      MATMIN(NMATZA)  = 0
      MFMIN (NMATZA)  = 0
      MTMIN (NMATZA)  = 0
      MATMAX(NMATZA)  = 0
      MFMAX (NMATZA)  = 0
      MTMAX (NMATZA)  = 0
      ERMIN (NMATZA)  = 0.0d0
      ERMAX (NMATZA)  = EBIG
      IIPONT(NMATZA)  = 0
      MYZOOM(NMATZA)  = 0
      GO TO 300
  290 CALL IN9(ERMIN(NMATZA),FIELD1)
      CALL IN9(ERMAX(NMATZA),FIELD2)
      MYZOOM(NMATZA) = 0
      if(ERMIN(NMATZA).gt.0.0d0.or.ERMAX(NMATZA).gt.0.0d0)
     1 MYZOOM(NMATZA) = 1
  300 MACTV(NMATZA)=MYACTION
      IF(MATMIN(NMATZA).LE.0.AND.MATMAX(NMATZA).LE.0) GO TO 320
      IF(MATMAX(NMATZA).LT.MATMIN(NMATZA)) MATMAX(NMATZA)=MATMIN(NMATZA)
      IF(MFMIN(NMATZA).LE.0) MFMIN(NMATZA)=0
      IF(MFMAX(NMATZA).LE.0) MFMAX(NMATZA)=99
      IF(MTMIN(NMATZA).LE.0) MTMIN(NMATZA)=0
      IF(MTMAX(NMATZA).LE.0) MTMAX(NMATZA)=999
      IF(ERMIN(NMATZA).LE.0.0d0) ERMIN(NMATZA)=0.0d0
      IF(ERMAX(NMATZA).LE.ERMIN(NMATZA)) ERMAX(NMATZA)=EBIG
      IIPONT(NMATZA)=IABS(IIPONT(NMATZA))
      IF(IIPONT(NMATZA).NE.0) IIPONT(NMATZA)=1
      IF(MACTV(NMATZA).NE.0) MACTV(NMATZA)=1
      ES=ERMIN(NMATZA)
      CALL OUT9(ES,FIELD11(1,1))
      ES=ERMAX(NMATZA)
      CALL OUT9(ES,FIELD11(1,2))
      KPOINT=YES
      IF(IIPONT(NMATZA).EQ.0) KPOINT=NO
      KACTV=YES
      IF(MACTV(1).EQ.0) KACTV=NO
      WRITE(OUTP,480) MATMIN(NMATZA),MFMIN(NMATZA),MTMIN(NMATZA),
     1 (FIELD11(kk,1),kk=1,11),MATMAX(NMATZA),MFMAX(NMATZA),
     2 MTMAX(NMATZA),(FIELD11(kk,2),kk=1,11),KPOINT,KACTV
  310 CONTINUE
C-----OVER 100 MAT OR ZA RANGES. PRINT ERROR MESSAGE AND TERMINATE.
      WRITE(OUTP,580)
      CALL ENDERROR
C-----END OF REQUEST LIST REACHED. DEFINE NUMBER OF REQUESTS.
  320 NMATZA=NMATZA-1
C-----------------------------------------------------------------------
C
C     READ (ZA,MT) EQUIVALENCE LIST.
C
C-----------------------------------------------------------------------
      IF(ISTAT1.EQ.1) THEN
      IQUATE = 1
      GO TO 360
      ENDIF
      DO 340 IQUATE=1,101
c***** 2017/5/6 - changed all floating to character.
      READ(INP,330,END=350,ERR=350)
     1 IZAQ1(IQUATE),MFQ1(IQUATE),MTQ1(IQUATE),
     2 IZAQ2(IQUATE),MFQ2(IQUATE),MTQ2(IQUATE),FIELD1
  330 FORMAT(I6,I2,I3,I6,I2,I3,11A1)
      CALL IN9(TIMES(IQUATE),FIELD1)
c***** 2017/5/6 - changed all floating to character.
      IF(IZAQ1(IQUATE).LE.0.AND.
     1 MFQ1(IQUATE).LE.0.AND.MTQ1(IQUATE).LE.0.AND.
     1 IZAQ2(IQUATE).LE.0.AND.
     1 MFQ2(IQUATE).LE.0.AND.MTQ2(IQUATE).LE.0) GO TO 360
      IF(IQUATE.EQ.1) WRITE(OUTP,600)
      IF(DABS(TIMES(IQUATE)).LE.0.0d0) TIMES(IQUATE)=1.0d0
      CALL OUT9(TIMES(IQUATE),FIELD11(1,1))
  340 WRITE(OUTP,610) IZAQ1(IQUATE),MFQ1(IQUATE),MTQ1(IQUATE),
     1 IZAQ2(IQUATE),MFQ2(IQUATE),MTQ2(IQUATE),(FIELD11(kk,1),kk=1,11)
C-----OVER 100 EQUIVALENCES. PRINT ERROR MESSAGE AND TERMINATE.
      WRITE(OUTP,620)
      CALL ENDERROR
C-----END OF REQUEST LIST REACHED. DEFINE NUMBER OF REQUESTS.
  350 ISTAT1 = 1
  360 IQUATE=IQUATE-1
      IF(IQUATE.LE.0) WRITE(OUTP,630)
      RETURN
  370 FORMAT(' Comparison of Evaluated Data (COMPLOT 2019-1)'/
     1 1X,97('-'))
  380 FORMAT(
     1 ' Description of Plotter and Frame Layout'/1X,97('-')/
     1 ' Plotter X Dimensions----------',F11.4,' to ',F11.4,' Inches'/
     2 ' Plotter Y Dimensions----------',F11.4,' to ',F11.4,' Inches'/
     3 ' Plots per Frame (X by Y)------',I11,' by ',I11)
  390 FORMAT(' Retrieval Criteria------------',7X,A4/
     1       ' Grid Type---------------------',A16/
     1       ' Border on Plots---------------',7X,A4/
     3       ' Line Thickness----------------',I11)
  400 FORMAT(' Plot Orientation-------------- X Horizontal/Y Vertical'/
     1 1X,97('-'))
  410 FORMAT(' Plot Orientation-------------- Y Horizontal/X Vertical'/
     1 1X,97('-'))
  420 FORMAT(' Maximum Difference for Plots--',F11.4,' (',F9.4,' %)')
  430 FORMAT(' Maximum Ratio on Plots--------','   No Limit')
  440 FORMAT(' Maximum Ratio on Plots--------',F11.4,' (',F9.4,
     1 ' per-cent)')
  450 FORMAT(
     1 ' Character Size Multiplier-----',F11.2)
  460 FORMAT(' Request Ranges'/1X,97('-')/
     1 '          Mimimum                     Maximum              ',
     2 '  Identify   Interact'/
     3 '     MAT MF  MT    Energy-eV      MAT MF  MT    Energy-eV  ',
     4 '  POINTS'/1X,97('-'))
  470 FORMAT(' Request Ranges'/1X,97('-')/
     1 '          Mimimum                     Maximum              ',
     2 '  Identify   Interact'/
     3 '      ZA MF  MT    Energy-eV       ZA MF  MT    Energy-eV  ',
     4 '  POINTS'/1X,97('-'))
  480 FORMAT(I8,I3,I4,2X,11A1,3X,I6,I3,I4,2X,11A1,5X,A4,6X,A4)
  490 FORMAT(' Output Mode------------------- Comparison Listing.',
     1 ' No Plots.')
  500 FORMAT(' Plot Mode--------------------- Cross Section Over',
     1 ' Ratio')
  510 FORMAT(' Plot Mode--------------------- Cross Section Over',
     1 ' Cross Section')
  520 FORMAT(' Plot Mode--------------------- Cross Section Plus',
     1 ' Cross Section')
  530 FORMAT(' Plot Mode--------------------- Cross Section Over',
     1 ' Cross Section Over Ratio')
  540 FORMAT(' Plot Mode--------------------- Cross Section Plus',
     1 ' Cross Section Over Ratio')
  550 FORMAT(' Starting Plot Number----------',I11)
  560 FORMAT(' On Screen Background Color----','Black'/
     1 1X,97('-'))
  570 FORMAT(' On Screen Background Color----','White'/
     1 1X,97('-'))
  580 FORMAT(' Over 100 Ranges-----Execution Terminated')
  590 FORMAT(1X,97('-')/' Data Identifications'/
     1 1X,97('-')/' Data1=',40A1/' Data2=',40A1/1X,97('-'))
  600 FORMAT(1X,97('-')/' Equivalence List'/1X,97('-')/
     1 ' Master  ZA MF  MT   Second  ZA MF  MT  Multiplier'/
     2 1X,97('-'))
  610 FORMAT(I11,I3,I4,I13,I3,I4,1X,11A1)
  620 FORMAT(' Over 100 Equivalences-----Execution Terminated')
  630 FORMAT(1X,97('-')/' No Equivalences')
      END
      SUBROUTINE SETMAP
C=======================================================================
C
C     DEFINE WHICH CURVES APPEAR IN EACH SUBPLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      DATA IPASS/0/
c-----------------------------------------------------------------------
C
C     ASSIGNMENT IS THE SAME FOR ALL PLOTS.
C
c-----------------------------------------------------------------------
      IF(IPASS.GT.0) RETURN
      IPASS=1
C-----ASSIGN CURVES AND SETS OF POINTS ACCRDING TO THE MODE.
      MODP1=MODPLT+1
      GO TO (10,20,30,40,50),MODP1
C-----CROSS SECTION OVER RATIO (CROSS SECTION TOP 2/3).
   10 IMCUR(1)=1
      MYCUR(1,1)=1
      IMCUR(2)=1
      MYCUR(1,2)=3
      GO TO 60
C-----CROSS SECTION OVER CROSS SECTION (EACH 1/2)
   20 IMCUR(1)=1
      MYCUR(1,1)=1
      IMCUR(2)=1
      MYCUR(1,2)=2
      GO TO 60
C-----BOTH CROSS SECTIONS ON SAME PLOTTING AREA.
   30 IMCUR(1)=2
      MYCUR(1,1)=1
      MYCUR(2,1)=2
      GO TO 60
C-----CROSS SECTION OVER CROSS SECTION OVER RATIO (EACH 1/3)
   40 IMCUR(1)=1
      MYCUR(1,1)=1
      IMCUR(2)=1
      MYCUR(1,2)=2
      IMCUR(3)=1
      MYCUR(1,3)=3
      GO TO 60
C-----CROSS SECTION + CROSS SECTION OVER RATIO (CROSS SECTIONS TOP 1/3)
   50 IMCUR(1)=2
      MYCUR(1,1)=1
      MYCUR(2,1)=2
      IMCUR(2)=1
      MYCUR(1,2)=3
   60 RETURN
      END
      SUBROUTINE NXTMAT(KTAPE)
C=======================================================================
C
C     FIND NEXT REQUESTED MATERIAL BASED EITHER ON ZA OR MAT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
C-----2017/4/2 - Add IZOOM to control DEFAULT X axis scaling
      COMMON/ZOOMCOM/IZOOM
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/WHATZA/IZA
      INCLUDE 'complot.h'
      DIMENSION IZAMIN(101),IZAMAX(101)
      EQUIVALENCE (MATMIN(1),IZAMIN(1)),(MATMAX(1),IZAMAX(1))
C-----READ NEXT LINE AND CHECK FOR END OF ENDF/B TAPE.
   10 CALL CONTIL(KTAPE)
      IF(MTH.gt.0) go to 20
      IF(MATH.lt.0) go to 60
      go to 10
C-----DEFINE FIXED POINT ZA.
   20 IZA=C1H
C-----COMPARE MAT OR ZA TO SELECTION CRITERIA.
      LOW=0
      DO 50 IMATZA=1,NMATZA
      IF(MODGET.NE.0) GO TO 30
      IF(MATH.eq.MATMIN(IMATZA)) go to 70
      IF(MATH.lt.MATMIN(IMATZA)) go to 40
      IF(MATH.le.MATMAX(IMATZA)) go to 70
      go to 50
   30 IF(IZA.eq.IZAMIN(IMATZA)) go to 70
      IF(IZA.lt.IZAMIN(IMATZA)) go to 40
      IF(IZA.le.IZAMAX(IMATZA)) go to 70
      go to 50
   40 LOW=1
   50 CONTINUE
      IMATZA = NMATZA
C-----THIS MATERIAL HAS NOT BEEN REQUESTED. IF BEYOND RANGE OF ALL
C-----REQUESTS RUN IF COMPLETED. IF NOT SKIP TO NEXT MATERIAL.
      IF(LOW.LE.0) GO TO 60
C-----SKIP TO MATERIAL END (MEND) LINE.
      CALL SKIPML(KTAPE)
      GO TO 10
C-----END OF RUN. RETURN NEGATIVE MATH AS INDICATOR.
   60 MATH=-1
      MFH=0
      MTH=0
      RETURN
C-----MATERIAL REQUESTED. DEFINE WHEATHER OR NOT TO IDENTIFY DATA POINTS
   70 NPOINT=0
      IF(IIPONT(IMATZA).GT.0) NPOINT=1
C-----2017/4/2 - Set ZOOM flag based on input parameters
      IZOOM = MYZOOM(IMATZA)
C-----SET INTERACTIVE INPUT FLAG.
      NACTV=MACTV(IMATZA)
      RETURN
      END
      SUBROUTINE FILE1
C=======================================================================
C
C     DEFINE FORMAT TO BE ENDF/B-IV, V OR VI.
C
C     THE ENDF/B FORMAT CAN BE DETERMINED FROM THE SECOND LINE.
C     ENDF/B-IV = N1 - LINE COUNT (POSITIVE)
C     ENDF/B-V  = N1 = N2 = 0
C     ENDF/B-VI = N1 = 0, N2 = VERSION NUMBER (6 OR MORE)
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/VERSON/IVERSE
      COMMON/PARTIN/INPART
      EQUIVALENCE (KTAPE(1),KTAPE1)
C-----3/22/2012 - INITIALIZE INCIDENT PARTICLE TO NEUTRON.
      INPART=1
c-----------------------------------------------------------------------
C
C     DETERMINE ENDF/B FORMAT VERSION.
C
c-----------------------------------------------------------------------
      CALL CARDIL(KTAPE1,C1,C2,L1,L2,N1,N2)
      IVERSE=4
C-----CHECK FOR ENDF/B-IV.
      IF(N1.GT.0) GO TO 10
C-----NOT ENDF/B-IV. READ THIRD LINE.
      N2X=N2
      CALL CARDIL(KTAPE1,C1,C2,L1,L2,N1,N2)
      IVERSE=5
C-----CHECK FOR ENDF/B-V FORMAT.
      IF(N2X.LE.0) GO TO 10
C-----ENDF/B-VI.
      IVERSE=6
C-----DEFINE INCIDENT PARTICLE.
      INPART=N1/10
      IF(INPART.le.0) INPART=0   ! default to photon
   10 RETURN
      END
      SUBROUTINE FILE2(RES1,RES2,URES1,URES2)
C=======================================================================
C
C     READ RESONANCE PARAMETERS IN ORDER TO DEFINE THE ENERGY RANGE
C     OF THE RESOLVED AND UNRESOLVED RESONANCE REGIONS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      EQUIVALENCE (KTAPE(1),KTAPE1)
C-----INITIALIZE ALL ENERGIES TO ZERO.
      RES1 =0.0d0
      RES2 =0.0d0
      URES1=0.0d0
      URES2=0.0d0
C-----HEAD RECORD ALREADY READ. DEFINE NUMBER OF ISOTOPES.
      NIS=N1H
C-----SET UP DO LOOP OVER ALL ISOTOPES
      DO 220 IS=1,NIS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,LFW,NER,N2H)
C-----DO LOOP OVER ALL ENERGY RANGES
      DO 210 JER=1,NER
      CALL CARDIL(KTAPE1,EL,EH,LRU,LRF,N1H,N2H)
c-----------------------------------------------------------------------
C
C     2017/5/16 - Added Energy dependent scattering radius
C
c-----------------------------------------------------------------------
      NRO = N1H
      if(NRO.eq.1) then
c-----Energy dependent scattering radius - skip Tab1 record
      CALL CARDIL(KTAPE1,C1,C2,L1,L2,N1,N2)
      do i = 1,N1,3          ! Interpolation law (NBT,INT) pairs
      CALL SKIP1L(KTAPE1)
      enddo
      do i = 1,N2,3          ! Scattering Radius (X,Y) pars
      CALL SKIP1L(KTAPE1)
      enddo
      endif
C-----DEFINE LRU FOR INTERNAL USE AS ORIGINAL LRU (BEFORE RECENT).
      IF(LRU.GT.3) LRU=LRU-3
C-----Select resolved or unresolved.
      IF(LRU.eq.1) go to 10    ! Resolved
      IF(LRU.gt.1) go to 30    ! Unresolved
C-----NO RESONANCE PARAMETERS.
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,N1H,N2H)
      GO TO 210
C-----------------------------------------------------------------------
C
C     RESOLVED
C
C-----------------------------------------------------------------------
C-----RESOLVED REGION. SKIP IF NOT IN LEGAL PARAMETER TYPE.
   10 IF(LRF.GE.1.AND.LRF.LE.4) GO TO 20
      IF(LRF.EQ.7) GO TO 20               ! New Reich-Moore
C-----ILLEGAL - IGNORE REMAINDER OF FILE 2
      GO TO 230
   20 IF(RES1.LE.0.0d0.OR.(RES1.GT.EL)) RES1=EL
      IF(RES2.LE.0.0d0.OR.(RES2.LT.EH)) RES2=EH
      GO TO 40
C-----------------------------------------------------------------------
C
C     UNRESOLVED
C
C-----------------------------------------------------------------------
C-----UNRESOLVED PARAMETERS. SKIP IF NOT LEGAL PARAMETER TYPE.
   30 IF(LRF.LT.1.OR.LRF.GT.2) GO TO 230
      IF(URES1.LE.0.0d0.OR.(URES1.GT.EL)) URES1=EL
      IF(URES2.LE.0.0d0.OR.(URES2.LT.EH)) URES2=EH
C-----------------------------------------------------------------------
C
C     LRU= 1 - RESOLVED
C     LRF= 1 - SLBW
C        = 2 - MLBW
C        = 3 - ADLER-ADLER
C        = 4 - REICH-MOORE
C        = 7 - NEW REICH-MOORE
C
C     LRU= 2 - UNRESOLVED
C     LRF= 1 - ENERGY INDEPENDENT WIDTHS (EXCEPT POSSIBLY FISSION)
C        = 2 - ENERGY DEPENDENT WIDTHS
C
C-----------------------------------------------------------------------
   40 IF(LRU.NE.1) GO TO 50    ! Resolved?
      IF(LRF.EQ.1.OR.          ! Single Level Breit-Wigner
     1   LRF.EQ.2.OR.          ! Multi-Level  Breit-Wigner
     2   LRF.EQ.3) GO TO  60   ! Reich-Moore
      IF(LRF.EQ.4) GO TO 80   ! Adler-Adler
      IF(LRF.EQ.7) GO TO 110   ! Reich-Moore Generalized
C-----ILLEGAL - IGNORE REMAINDER OF FILE 2
      GO TO 230
   50 IF(LRU.NE.2) GO TO 230   ! Unresolved?
      IF(LRF.EQ.1) GO TO 130   ! Energy Independent Widths
      IF(LRF.EQ.2) GO TO 190   ! Energy   Dependent Widths
C-----ILLEGAL - IGNORE REMAINDER OF FILE 2
      GO TO 230
C-----------------------------------------------------------------------
C
C     BREIT-WIGNER (SINGLE OR MULTI-LEVEL) OR REICH-MOORE FORMALISM
C
C-----------------------------------------------------------------------
C-----READ NEXT LINE.
   60 CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NLS,N2H)
C-----LOOP OVER ALL L STATES
      DO 70 ILS=1,NLS
C-----READ NEXT LINE.
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,N1H,NRS)
C-----COPY RESONANCE PARAMETERS.
      DO 70 IRS=1,NRS
   70 CALL SKIP1L(KTAPE1)
      GO TO 210
C-----------------------------------------------------------------------
C
C     ADLER-ADLER FORMALISM
C
C-----------------------------------------------------------------------
C-----READ NEXT LINE.
   80 CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NLS,N2H)
C-----READ BACKGROUND CORRECTIONS.
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NX6,N2H)
C-----COPY BACKGROUND CORRECTION CONSTANTS.
      DO 90 I=1,NX6,6
   90 CALL SKIP1L(KTAPE1)
C-----LOOP OVER L STATES
      DO 100 I=1,NLS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NJS,N2H)
C-----LOOP OVER J STATES
      DO 100 J=1,NJS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,N1H,NLJ)
C-----COPY ALL RESONANCE DATA
      DO 100 K=1,NLJ
      CALL SKIP1L(KTAPE1)
  100 CALL SKIP1L(KTAPE1)
      GO TO 210
C-----------------------------------------------------------------------
C
C     NEW REICH-MOORE FORMALISM
C
C-----------------------------------------------------------------------
C-----DEFINE NUMBER OF J STATES
  110 CALL CARDIL(KTAPE1,C1,C2,L1,L2,NJS,N2)
C-----DEFINE NUMBER OF PARTICLE-PAIRS
      CALL CARDIL(KTAPE1,C1,C2,L1,L2,NPP12,N2)
C-----SKIP PARTICLE-PAIR DATA
      DO N=1,NPP12,6
      CALL SKIP1L(KTAPE1)
      ENDDO
C-----LOOP OVER J STATES
      DO 120 IJ=1,NJS
C-----J, PARITY AND NUMBER OF CHANNELS
      CALL CARDIL(KTAPE1,C1,C2,L1,L2,NCH6,N2)
C-----SKIP CHANNEL DATA
      DO N=1,NCH6,6
      CALL SKIP1L(KTAPE1)
      ENDDO
C-----DEFINE NUMBER OF RESONANCES
      CALL CARDIL(KTAPE1,C1,C2,L1,L2,N1,NRS)
C-----SKIP RESONANCE PARAMETERS
      DO N=1,NRS
      CALL SKIP1L(KTAPE1)
      ENDDO
  120 CONTINUE
      GO TO 210  ! continue
C-----------------------------------------------------------------------
C
C     UNRESOLVED WITH ENERGY INDEPENDENT WIDTHS.
C
C-----------------------------------------------------------------------
C-----TEST IF FISSION WIDTHS GIVEN
  130 IF(LFW.gt.0) go to 150
C-----FISSION WIDTHS NOT GIVEN
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NLS,N2H)
C-----LOOP OVER ALL L-STATES
      DO 140 ILS=1,NLS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,N1H,NJS)
      DO 140 N=1,NJS
  140 CALL SKIP1L(KTAPE1)
      GO TO 210
C-----FISSION WIDTHS GIVEN (LFW=1)
  150 CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NE,NLS)
C-----COPY FISSION WIDTH ENERGY POINTS
      DO 160 I=1,NE,6
  160 CALL SKIP1L(KTAPE1)
C-----LOOP OVER L-STATES
      DO 180 I=1,NLS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NJS,N2H)
C-----LOOP OVER J STATES
      DO 180 J=1,NJS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NEP6,N2H)
      DO 170 K=1,NEP6,6
  170 CALL SKIP1L(KTAPE1)
  180 CONTINUE
      GO TO 210
C-----------------------------------------------------------------------
C
C     UNRESOLVED WITH ENERGY DEPENDENT WIDTHS.
C
C-----------------------------------------------------------------------
C-----READ NEXT LINE.
  190 CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NLS,N2H)
C-----DO LOOP OVER L-STATES
      DO 200 I=1,NLS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NJS,N2H)
      DO 200 J=1,NJS
      CALL CARDIL(KTAPE1,C1H,C2H,L1H,L2H,NE6P6,N2H)
C-----COPY NUMBER OF DEGREES OF FREEDOM AND PARAMETERS.
      DO 200 K=1,NE6P6,6
  200 CALL SKIP1L(KTAPE1)
  210 CONTINUE
  220 CONTINUE
  230 RETURN
      END
      SUBROUTINE FILE3(LCURVE)
C=======================================================================
C
C     READ A TAB1 RECORD FROM THE ENDF/B FORMAT FILE AND LOAD INTO THE
C     PAGING SYSTEM.
C     DETERMINE ALL SCALING INFORMATION FOR PLOT.
C
C     LCURVE= 1 - READ FIRST SECTION.
C           = 2 - READ SECOND SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE,ONSCR
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/WHERE1/IZA1,MAT1,MF1,MT1
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
C-----2017/3/20 - Added for MF=4 Data
      COMMON/SETLABCM/LTT,LI,LCT,ILEG(2),NLEG(2),NPTLEG(2),NLEGB(2)
      COMMON/OLDTAB/INXBAS
      COMMON/WHATZA/IZA
      COMMON/LASTE/ELAST
      COMMON/THRES/THRES3(3)
      COMMON/MELINR/IMLINR
      COMMON/VERSON/IVERSE
      INCLUDE 'complot.h'
C-----2017/4/5 - Increase NBT and INT from 50 to 100 = PREPRO Standard
      DIMENSION INT(100),NBT(100)
C-----2017/3/20 - Added for MF=4 Data
      DIMENSION DUMMYLEGS(1000)    ! array for ALL Legendre coefficients
      DATA Y1    /0.0d0/
      DATA ETHRES/1.0d0/
      DATA WAVE  /1.239852d4/
      DATA XLAST /0.0d0/
      DATA X1    /0.0d0/
C-----DEFINE INPUT AND SCRATCH UNITS.
      NTAPE=KTAPE(LCURVE)
      NSCR=ISCR(LCURVE)
C-----INITIALIZE LAST ENERGY FOR ENERGY ORDER TEST.
      ELAST=0.0d0
C-----INITIALIZE THRESHOLD ENERGY.
      THRES3(LCURVE)=0.0d0
C-----------------------------------------------------------------------
C
C     FOR MF=4 Legendre - skip if ALL data is already in core
C
C-----------------------------------------------------------------------
      IF(MF1.eq.4) then
      IF(ILEG(LCURVE).gt.0) then
      N2 = NPTLEG(LCURVE)
      go to 30             ! Cannot jump into block at 72
      endif
      endif
C-----------------------------------------------------------------------
C
C     FOR DELAYED NU-BAR SKIP LIST RECORD OF DECAY CONSTANTS.
C
C-----------------------------------------------------------------------
      IF(MF1.NE.1.OR.MT1.NE.455) GO TO 20
      CALL CARDIL(NTAPE,C1,C2,L1,L2,N1,N2)
      DO 10 I=1,N1,6
   10 CALL SKIP1L(NTAPE)
C-----------------------------------------------------------------------
C
C     READ HEAD LINE AND INTERPOLATION LAW. TERMINATE IF ANY
C     ILLEGAL DATA FOUND ON HEAD LINE OR IF AN UNACCEPTABLE
C     INTERPOLATION LAW IS PRESENT (DATA MUST BE EITHER LINEARLY
C     OF HISTOGRAM INTERPOLABLE).
C
C-----------------------------------------------------------------------
C-----READ SECTION HEAD LINE AND INTERPOLATION LAW.
   20 CALL CARDIL(NTAPE,C1,C2,L1,L2,N1,N2)
      CALL TERPIL(NTAPE,NBT,INT,N1,N2)
C-----------------------------------------------------------------------
C
C     Ready to go.
C
C-----------------------------------------------------------------------
C-----INITIALIZE PAGING SYSTEM IN CORE AND ON SCRATCH INDICES.
   30 INCORE=0
      ONSCR=0
C-----IF NEEDED INITIALIZE INDEX TO OTHER CROSS SECTION.
      IM1=MTDEX1(1)
      IF(LCURVE.EQ.2) CALL XYCURV(X1,Y1,IM1,1)
C-----2017/3/21 - ALL MF=4 Legendre data MUST fit in core
      if(MF1.eq.4) then
      N1     = 1                   ! treat MF=4 as linear
      INT(1) = 2
      NBT(1) = N2
      if(N2 .gt.MAXLOAD/3) then       ! MUST fit in 1/3 core
      N2      = MAXLOAD/3
      NBT(N1) = N2
      endif
      endif
C-----INITIALIZE FLAG TO INDICATE POSITIVE CROSS SECTION NOT YET FOUND.
      KMPLUS=0
C-----INITIALIZE INTERPOLATION RANGE.
      IRANGE=1
      NBTNOW=NBT(1)
      INTNOW=INT(1)
      INTLST=0
C-----------------------------------------------------------------------
C
C     SET UP LOOP TO READ PAGES OF DATA.
C
C-----------------------------------------------------------------------
      LOOP2=0
      DO 280 LOOP1=1,N2,MAXLOAD
      INXBAS=LOOP1-1
      LOOP2=LOOP2+MAXLOAD
      IF(LOOP2.GT.N2) LOOP2=N2
      LOOP3=(LOOP2-LOOP1)+1
      IF(MF1.ne.4) go to 50
c-----------------------------------------------------------------------
c
c     For MF=4 Legendre Coefficients
C
c-----------------------------------------------------------------------
      if(ILEG(LCURVE).gt.0) go to 40    ! Skip if data already read
c-----------------------------------------------------------------------
c
c     First pass read all data and define counters
c
c-----------------------------------------------------------------------
      MAXF = MAXLOAD/LOOP3/3            ! Max. that will fit in 1/3 core
      if(MAXF.gt.6) MAXF = 6            ! Not more than 6
c-----initialize storage
      kbase = NLEGB(LCURVE)
      do k=1,LOOP3*MAXF
      kk = k + kbase
      XLOAD(kk) = 0.0d0
      YLOAD(kk) = 0.0d0
      enddo
      MAXN1L = 0                              ! count max. coefficients
c-----------------------------------------------------------------------
c
c     Read LIST records one at a time
c     and load data into upper core
c
c-----------------------------------------------------------------------
      do k=1,LOOP3
      call CARDIL(NTAPE,C1,C2,L1,L2,N1L,N2)
      do i=1,MAXF                             ! Insure shifted = 0
      DUMMYLEGS(i) = 0.0d0
      enddo
      call LISTIL(NTAPE,DUMMYLEGS(1),N1L)
      if(N1L.gt.MAXN1L) MAXN1L = N1L
      kbase = NLEGB(LCURVE)
      do i=1,MAXF
      kk = k + kbase
      XLOAD(kk) = C2
      YLOAD(kk) = DUMMYLEGS(i)
      kbase = kbase + LOOP3
      enddo
      enddo
c-----Set indices for following passes
      if(MAXN1L.gt.MAXF) MAXN1L = MAXF
      ILEG  (LCURVE) = 0               ! Advanced to 1 below
      NLEG  (LCURVE) = MAXN1L
      NPTLEG(LCURVE) = LOOP3
c-----------------------------------------------------------------------
c
c     Data is in UPPER code - shift downward - ALL cases
c
c-----------------------------------------------------------------------
c-----Advance counter for this pass
   40 ILEG(LCURVE) = ILEG(LCURVE) + 1
      kbase = LOOP3*(ILEG(LCURVE)-1) + NLEGB(LCURVE)
      do k = 1,LOOP3
      kk = k + kbase
      XLOAD(k) = XLOAD(kk)
      YLOAD(k) = YLOAD(kk)
      enddo
      go to 60
c-----------------------------------------------------------------------
c
c     NOT MF = 4 = Load all data
c
c-----------------------------------------------------------------------
   50 CALL POINTIL(NTAPE,XLOAD,YLOAD,LOOP3)
c-----Save MAT/MF/MT
      CALL MAXIE1(MATH,MFH,MTH)
c-----Save Maximum energy  and value
      CALL MAXIE2(XLOAD(LOOP3),YLOAD(LOOP3))
c-----------------------------------------------------------------------
c
c     Data is in place
c
c-----------------------------------------------------------------------
C-----FOR REAL ANOMALOUS SCATTERING DEFINE ABSOLUTE VALUES.
   60 IF(MF.NE.27.OR.MT.NE.506) GO TO 80
      DO 70 I=1,LOOP3
   70 YLOAD(I)=DABS(YLOAD(I))
      GO TO 100
C-----FOR FORM FACTORS CONVERT 1/WAVELENGTH TO EV.
   80 IF(MF.NE.27) GO TO 100
      IF(MT.NE.502.AND.MT.NE.504) GO TO 100
      DO 90 I=1,LOOP3
   90 XLOAD(I)=WAVE*XLOAD(I)
C-----IF THIS IS SECOND SET OF DATA MULTIPLY CROSS SECTION BY
C-----MULTIPLICATION FACTOR.
  100 IF(LCURVE.NE.2) GO TO 120
      DO 110 I=1,LOOP3
  110 YLOAD(I)=TIMUSE*YLOAD(I)
C-----SAVE FIRST DATA POINT.
  120 IF(LOOP1.NE.1) GO TO 130
      XSTART=XLOAD(1)
C-----DETERMINE X AND Y SCALING INFORMATION.
  130 DO 270 J=1,LOOP3
C-----DEFINE INTERPOLATION LAW.
      INDEX=J+INXBAS
      IF(INDEX.LE.NBTNOW) GO TO 140
      IF(IRANGE.GE.N1) GO TO 140
      IRANGE=IRANGE+1
      NBTNOW=NBT(IRANGE)
      INTNOW=NBT(IRANGE)
C-----DEFINE NEXT POINT.
  140 XNOW=XLOAD(J)
      YNOW=YLOAD(J)
C-----FOR SCALING IGNORE NON-POSITIVE X VALUES (ONLY APPLIES TO FORM
C-----FACTORS AND SCATTERING FUNCTIONS IN MF=27).
      IF(XNOW.LE.0.0d0) GO TO 260
C-----IF THIS IS SECOND CROSS SECTION WITH HISTOGRAM REPRESENTATION
C-----ELIMINATE MICRO-DIFFERENCES IN ENERGIES BY SETTING SECOND ENERGIES
C-----THAT ARE CLOSE TO THE FIRST ENERGY EQUAL TO THE FIRST ENERGY.
      IF(LCURVE.NE.2.OR.INTNOW.NE.1) GO TO 170
  150 CONTINUE
      IF(DABS(XNOW-X1).LE.0.0001d0*X1) GO TO 160
      IF(XNOW.lt.X1) go to 170
      IF(XNOW.eq.X1) go to 160
      IF(IM1.GE.MTDEX2(1)) GO TO 170
      IM1=IM1+1
      CALL XYCURV(X1,Y1,IM1,1)
      GO TO 150
  160 XNOW=X1
C-----HAS AT LEAST ONE DATA POINT WITH POSITIVE CROSS SECTION BEEN
C-----FOUND YET.
  170 IF(KMPLUS.GT.0) GO TO 210
c-----------------------------------------------------------------------
C
C     THIS SECTION IS DESIGNED TO SKIP ALL DATA POINTS BELOW THRESHOLD
C     AND IF THE CROSS SECTION STARTS AT AN ENERGY ABOVE 1 EV WITH A
C     NON-ZERO CROSS SECTION, TO INSERT A POINT AT THE THRESHOLD WITH
C     ZERO CROSS SECTION. INITIALIZE X AND Y LIMITS OF PLOT TO X AND Y
C     VALUES OF LAST PRECEDING POINT.
C
c-----------------------------------------------------------------------
C-----IF 2 OF FEWER POINTS ARE LEFT USE THEM, REGARDLESS OF VALUES.
      IF(INDEX.GE.(N2-1)) GO TO 180
C-----SKIP POINTS UNTIL ONE WITH POSITIVE X AND Y IS FOUND.
      IF(XNOW.LE.0.0d0) GO TO 260
c-----Allow negative Y to start = need not = 0
      IF(YNOW.NE.0.0d0) GO TO 180
C-----IF NEXT POINT IS POSITIVE, START AT CURRENT POINT TO INCLUDE
C-----THRESHOLD
      IF(J.GE.LOOP3) GO TO 260
      YNEXT = YLOAD(J+1)
      IF(YNEXT.LE.0.0d0) GO TO 260
  180 KMPLUS=INDEX
C-----IS X AND Y OF FIRST POINT POSITIVE.
      IF(INDEX.LE.1.OR.XLAST.LE.0.0d0) GO TO 190
C-----FIRST POINT IS NOT POSITIVE. SAVE LAST ZERO CROSS SECTION POINT
C-----TO INITIALIZE (IF INTERPOLATION LAW OF LAST POINT WAS HISTOGRAM
C-----SAVE CURRENT ENERGY INSTEAD OF LAST ENERGY).
      IF(INTLST.EQ.1) XLAST=XNOW
      GO TO 200
C-----FIRST POINT IS POSITIVE. DEFINE LAST POINT TO BE CURRENT POINT.
  190 XLAST=XNOW
      YLAST=YNOW
C-----INITIALIZE LOWER X AND LOWER AND UPPER Y LIMITS TO LAST POINT.
  200 XYLIMS(1,1,LCURVE)=XLAST
      XYLIMS(1,2,LCURVE)=YLAST
      XYLIMS(2,2,LCURVE)=YLAST
C-----FOR SCALING IGNOR THRESHOLD CROSS SECTION.
      IF(YLAST.LT.YNOW) XYLIMS(2,2,LCURVE)=YNOW
C-----SAVE THRESHOLD ENERGY.
      THRES3(LCURVE)=XLAST
      IF(MF.EQ.27.OR.THRES3(LCURVE).LE.ETHRES) THRES3(LCURVE) = 0.0d0
c-----------------------------------------------------------------------
C
C     BEGINNING OF MEANINGFUL DATA HAS BEEN FOUND. IF INTERPOLATION LAW
C     OF LAST ENERGY INTERVAL WAS HISTOGRAM INSERT AN ENERGY POINT AT
C     THE UPPER ENERGY LIMIT OF THE HISTOGRAM. SAVE CURRENT DATA POINT
C     AND UPDATE Y RANGE LIMITS.
C
c-----------------------------------------------------------------------
C-----IF HISTOGRAM INTERPOLATION INSERT POINT AT UPPER ENERGY LIMIT OF
C-----LAST ENERGY INTERVAL (EXCEPT AT FIRST ENERGY POINT).
  210 IF(INTLST.NE.1.OR.INDEX.LE.KMPLUS) GO TO 230
      INCORE=INCORE+1
      IF(INCORE.LE.MAXPOINT) GO TO 220
C-----POSITION SCRATCH FILE BEFORE FIRST WRITE.
      IF(ONSCR.EQ.0.AND.IMUSED(LCURVE).GT.0) REWIND NSCR
      IF(LCURVE.EQ.1) WRITE(NSCR) XPAGE1,YPAGE1
      IF(LCURVE.EQ.2) WRITE(NSCR) XPAGE2,YPAGE2
      ONSCR=ONSCR+MAXPOINT
      INCORE=1
  220 XTAB(INCORE,LCURVE)=XNOW
      YTAB(INCORE,LCURVE)=YLAST
C-----SAVE CURRENT POINT, UNLESS THIS IS LAST POINT OF TABLE AND IT
C-----IS THE UPPER ENERGY OF THE LAST HISTOGRAM.
  230 IF(INDEX.EQ.N2.AND.INTNOW.EQ.1) GO TO 280
      INCORE=INCORE+1
      IF(INCORE.LE.MAXPOINT) GO TO 240
C-----POSITION SCRATCH FILE BEFORE FIRST WRITE.
      IF(ONSCR.EQ.0.AND.IMUSED(LCURVE).GT.0) REWIND NSCR
      IF(LCURVE.EQ.1) WRITE(NSCR) XPAGE1,YPAGE1
      IF(LCURVE.EQ.2) WRITE(NSCR) XPAGE2,YPAGE2
      ONSCR=ONSCR+MAXPOINT
      INCORE=1
  240 XTAB(INCORE,LCURVE)=XNOW
      YTAB(INCORE,LCURVE)=YNOW
C-----2017/3/20 - Allow < 0 for MF=4.
      IF(MF.ne.4.and.YNOW.LE.0.0d0) GO TO 260
C-----UPDATE Y RANGES (X RANGE IS ALWAYS FIRST TO LAST X VALUES).
C-----IGNORE NON-POSITIVE Y RANGE.
      IF(XYLIMS(1,2,LCURVE).lt.YNOW) go to 250
      IF(XYLIMS(1,2,LCURVE).eq.YNOW) go to 260
      XYLIMS(1,2,LCURVE)=YNOW
      GO TO 260
  250 IF(XYLIMS(2,2,LCURVE).ge.YNOW) go to 260
      XYLIMS(2,2,LCURVE)=YNOW
C-----SAVE LAST X AND Y COORDINATES AND INTERPOLATION LAW.
  260 XLAST=XNOW
      YLAST=YNOW
      INTLST=INTNOW
  270 CONTINUE
  280 CONTINUE
C-----IF ALL DATA IS ZERO. FOR FIRST SET OF DATA USE ENTIRE ENERGY RANGE
C-----WITH ZERO CROSS SECTION. FOR SECOND SET OF DATA USE ENERGY RANGE
C-----OF FIRST SET.
      IF(INCORE.GT.0.OR.ONSCR.GT.0) GO TO 300
C-----SET POINT INDICES TO INDICATE ONLY TWO POINTS.
      INCORE=2
      MTDEX1(LCURVE)=1
      MTDEX2(LCURVE)=2
C-----DEFINE UPPER AND LOWER Y LIMITS TO BE ZERO.
      XYLIMS(1,2,LCURVE)=0.0d0
      XYLIMS(2,2,LCURVE)=0.0d0
C-----DEFINE TWO Y VALUES TO BE ZERO.
      YTAB(1,LCURVE)=0.0d0
      YTAB(2,LCURVE)=0.0d0
      IF(LCURVE.NE.1) GO TO 290
C-----FOR FIRST CROSS SECTION DEFINE ENERGY LIMITS TO BE FIRST AND LAST
C-----TABULATED ENERGIES.
      XYLIMS(1,1,1)=XSTART
      XYLIMS(2,1,1)=XNOW
      XTAB(1,1)=XSTART
      XTAB(2,1)=XNOW
      GO TO 310
C-----FOR SECOND CROSS SECTION DEFINE ENERGY LIMITS TO BE THE SAME AS
C-----FOR THE FIRST CROSS SECTION.
  290 XYLIMS(1,1,LCURVE)=XYLIMS(1,1,1)
      XYLIMS(2,1,LCURVE)=XYLIMS(2,1,1)
      XTAB(1,LCURVE)=XYLIMS(1,1,1)
      XTAB(2,LCURVE)=XYLIMS(2,1,1)
      IF(LCURVE.NE.2) GO TO 310
C-----2017/3/20 - No limit for MF=4 Data.
      IF(MF1.ne.4) then
C-----DO NOT ALLOW UPPER Y LIMIT TO BE MORE THAN 10 TIMES STANDARD.
      IF(XYLIMS(2,2,LCURVE).GT.10.0d0*XYLIMS(2,2,1))
     1 XYLIMS(2,2,LCURVE)=10.0d0*XYLIMS(2,2,1)
C-----DO NOT ALLOW LOWER Y LIMIT TO BE LESS THAN 1/10 TIMES STANDARD.
      IF(XYLIMS(1,2,LCURVE).LT.0.1d0*XYLIMS(1,2,1))
     1 XYLIMS(1,2,LCURVE)=0.1d0*XYLIMS(1,2,1)
      endif
      GO TO 310
C-----DEFINE UPPER X LIMIT AND PAGING SYSTEM INDICES.
  300 XYLIMS(2,1,LCURVE)=XNOW
      MTDEX1(LCURVE)=1
      MTDEX2(LCURVE)=INCORE+ONSCR
  310 IXYLOW(LCURVE)=0
C-----IF DATA WILL NOT FIT IN CORE UNLOAD IT TO SCRATCH.
      IF(ONSCR.GT.0) GO TO 320
C-----DATA IS ALL IN CORE DEFINE INDICES TO IT.
      IXYHI(LCURVE)=INCORE
      GO TO 330
C-----SET UP SCRATCH FILE TO READ.
  320 IF(LCURVE.EQ.1) WRITE(NSCR) XPAGE1,YPAGE1
      IF(LCURVE.EQ.2) WRITE(NSCR) XPAGE2,YPAGE2
      END FILE NSCR
      REWIND NSCR
      IMUSED(LCURVE)=1
C-----LOAD FIRST PAGE OF DATA FROM SCRATCH.
      IXYHI(LCURVE)=MAXPOINT
      IF(LCURVE.EQ.1) READ(NSCR) XPAGE1,YPAGE1
      IF(LCURVE.EQ.2) READ(NSCR) XPAGE2,YPAGE2
  330 CONTINUE
      RETURN
      END
      SUBROUTINE CONTIL(MTAPE)
C=======================================================================
C
C     READ CONTROL RECORD.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      ITAPE=MTAPE
      CALL CONTI
      RETURN
      END
      SUBROUTINE CARDIL(MTAPE,C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     READ LEADER RECORD.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      ITAPE=MTAPE
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      RETURN
      END
      SUBROUTINE LISTIL(MTAPE,XLIST,NXLIST)
C=======================================================================
C
C     READ LEADER RECORD.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION XLIST(*)
      ITAPE=MTAPE
      CALL LISTI(XLIST,NXLIST)
      RETURN
      END
      SUBROUTINE COPY1L(MTAPE)
C=======================================================================
C
C     COPY TAPE LABEL RECORD.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      ITAPE=MTAPE
      CALL COPYL
      RETURN
      END
      SUBROUTINE SKIP1L(MTAPE)
C=======================================================================
C
C     SKIP A LINE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      ITAPE=MTAPE
      CALL SKIP1
      RETURN
      END
      SUBROUTINE SKIPSL(MTAPE)
C=======================================================================
C
C     SKIP A SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      ITAPE=MTAPE
      CALL SKIPS
      RETURN
      END
      SUBROUTINE SKIPML(MTAPE)
C=======================================================================
C
C     SKIP A MATERIAL
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      ITAPE=MTAPE
      CALL SKIPM
      RETURN
      END
      SUBROUTINE TERPIL(MTAPE,NBT,INT,N1,N2)
C=======================================================================
C
C     READ INTERPOLATION LAW
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION NBT(N1),INT(N1)
      ITAPE=MTAPE
      CALL TERPI(NBT,INT,N1)
      CALL TERPTEST(NBT,INT,N1,N2,2)  ! INT = 1 or 2 o.k.
      RETURN
      END
      SUBROUTINE POINTIL(MTAPE,X,Y,IXY)
C=======================================================================
C
C     READ TABLE OF (X,Y) POINTS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      DIMENSION X(IXY),Y(IXY)
      ITAPE=MTAPE
      CALL POINTI(X,Y,IXY)
      RETURN
      END
      SUBROUTINE ZOOMER
C=======================================================================
C
C     IF RESTRICTED ENERGY RANGE IS REQUESTED RE-DEFINE Y LIMITS
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP7/XZOOM(2),YZOOM(2,5),IUSE1(60),IUSE2(60)
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/MINMAX/XMIN0,XMAX0,YMIN,YMAX,YMINPC,YMAXPC
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      COMMON/RATOK/RATMIN,RATMAX
      COMMON/WHERE1/IZA1,MAT1,MF1,MT1
c-----------------------------------------------------------------------
C
C     SELECT Y RANGES
C
c-----------------------------------------------------------------------
C-----SET UP LOOP OVER CROSS SECTIONS AND RATIO.
      DO 120 ISET=1,3
C-----IS ZOOM OF ENERGY RANGE REQUIRED.
      IF(XYLIMS(1,1,ISET).LT.XZOOM(1).OR.
     1   XYLIMS(2,1,ISET).GT.XZOOM(2)) GO TO 10
C-----NO ZOOM. DEFINE ZOOMED POINT INDICES AND X AND Y LIMITS FOR
C-----ENTIRE CROSS SECTION SET.
      IUSE1(ISET)=MTDEX1(ISET)
      IUSE2(ISET)=MTDEX2(ISET)
      YZOOM(1,ISET)=XYLIMS(1,2,ISET)
      YZOOM(2,ISET)=XYLIMS(2,2,ISET)
C-----FOR RATIO DEFINE WHERE MIN AND MAX DIFFERENCE OCCUR.
      XMIN=XMIN0
      XMAX=XMAX0
      GO TO 120
C-----ZOOM IS REQUIRED. FIND FIRST DATA POINT IN ENERGY RANGE.
   10 IM1=MTDEX1(ISET)
      IM2=MTDEX2(ISET)
      DO 20 I=IM1,IM2
      CALL XYCURV(X,Y,I,ISET)
      IF(X.GE.XZOOM(2)) GO TO 30
      IF(X.eq.XZOOM(1)) go to 50
      IF(X.gt.XZOOM(1)) go to 40
      XLAST=X
      YLAST=Y
   20 CONTINUE
C-----NO POINTS IN ENERGY RANGE.
   30 IUSE1(ISET)=0
      IUSE2(ISET)=0
      GO TO 120
C-----INTERPOLATE TO DEFINE STARTING X AND Y POINT.
   40 IF(I.EQ.IM1) GO TO 50
      Y=TERPY(XZOOM(1),X,XLAST,Y,YLAST,2)
      X=XZOOM(1)
C-----INITIALIZE LOWER POINT INDEX TO PRECEEDING POINT.
      IUSE1(ISET)=I-1
      GO TO 60
C-----INITIALIZE LOWER POINT INDEX TO CURRENT POINT.
   50 IUSE1(ISET)=I
C-----INITIALIZE LOWER AND UPPER Y LIMITS.
   60 YLOW=Y
      YHIGH=Y
C-----DEFINE LAST DATA POINT.
      XLAST=X
      YLAST=Y
c-----------------------------------------------------------------------
C
C     FOR RATIO KEEP TRACK OF X AND Y OF MIN AND MAX.
C
c-----------------------------------------------------------------------
      XMIN=X
      XMAX=X
C-----DEFINE Y LIMITS IN THE ENERGY RANGE UP TO UPPER ENERGY LIMIT.
      DO 100 J=I,IM2
      CALL XYCURV(X,Y,J,ISET)
      IF(X.le.XZOOM(2)) go to 70
      Y=TERPY(XZOOM(2),X,XLAST,Y,YLAST,2)
      X=XZOOM(2)
   70 IF(Y.GE.YLOW) GO TO 80
      YLOW=Y
      IF(ISET.EQ.3) XMIN=X
   80 IF(Y.LE.YHIGH) GO TO 90
      YHIGH=Y
      IF(ISET.EQ.3) XMAX=X
   90 IF(X.GE.XZOOM(2)) GO TO 110
      XLAST=X
  100 YLAST=Y
      J=IM2
C-----DEFINE UPPER POINT INDEX AND LOWER AND UPPER Y LIMITS.
  110 IUSE2(ISET)=J
C-----ONLY ALLOW SAME OR SMALLER Y RANGE THAN ENTIRE TABLE (E.G.
C-----IGNOR THRESHOLDS).
      YZOOM(1,ISET)=XYLIMS(1,2,ISET)
      YZOOM(2,ISET)=XYLIMS(2,2,ISET)
      IF(YLOW .GT.YZOOM(1,ISET)) YZOOM(1,ISET)=YLOW
      IF(YHIGH.LT.YZOOM(2,ISET)) YZOOM(2,ISET)=YHIGH
  120 CONTINUE
C-----IF BOTH CROSS SECTIONS WILL BE PLOTTED SET CROSS SECTION RANGES
C-----EQUAL.
      IF(MODPLT.EQ.0) GO TO 130
      IF(YZOOM(1,1).GT.YZOOM(1,2)) YZOOM(1,1)=YZOOM(1,2)
      IF(YZOOM(2,1).LT.YZOOM(2,2)) YZOOM(2,1)=YZOOM(2,2)
      YZOOM(1,2)=YZOOM(1,1)
      YZOOM(2,2)=YZOOM(2,1)
C-----DEFINE ABSOLUTE AND PER-CENT DIFFERENCES.
  130 YMIN=YZOOM(1,3)
      YMAX=YZOOM(2,3)
      YMINPC=100.0d0*(YMIN-1.0d0)
      YMAXPC=100.0d0*(YMAX-1.0d0)
C-----2013/9/21 - LIMIT RANGES TO FIT OUTPUT FORMAT
      IF(YMINPC.LT.-9999.0d0) YMINPC=-9999.0d0
      IF(YMINPC.GT. 9999.0d0) YMINPC= 9999.0d0
      IF(YMAXPC.LT.-9999.0d0) YMAXPC=-9999.0d0
      IF(YMAXPC.GT. 9999.0d0) YMAXPC= 9999.0d0
C-----IF NECESSARY RESTRICT RANGE OF RATIO.
      IF(RATMIN.GE.RATMAX) GO TO 140
C-----2017/3/20 - Allow negative for MF=4
      IF(MF1.eq.4) then
      if(YZOOM(1,3).lt.0.0d0.and.YZOOM(1,3).lt.-RATMAX)  ! MF=4
     1 YZOOM(1,3) = -RATMAX
      IF(YZOOM(2,3).GT. RATMAX) YZOOM(2,3)= RATMAX
      else
      IF(YZOOM(1,3).LT.RATMIN) YZOOM(1,3)=RATMIN         ! MF<>4
      IF(YZOOM(2,3).GT.RATMAX) YZOOM(2,3)=RATMAX
      endif
  140 RETURN
      END
      SUBROUTINE RATIOS
C=======================================================================
C
C     THIS ROUTINE IS DESIGNED TO COMPUTE THE RATIO OF TWO CROSS
C     SECTIONS AND TO SAVE THE RESULT IN THE PAGING SYSTEM.
C
C     EACH CROSS SECTION IS CONSIDERED SEPARATELY. FOR EACH AN ENERGY
C     INTERVAL IS DEFINED FROM THE LAST POINT AT WHICH A RATIO WAS
C     CALCULATED (EB,YB), UP TO THE NEXT TABULATED ENERGY (EA,YA).
C     AFTER INITIALIZATION (TO DEFINE THE FIRST POINT) THE LOWER ENERGY
C     EA IS SELECTED AND CROSS SECTIONS ARE INTERPOLATED TO THIS ENERGY
C     TO DEFINE A NEW LOWER ENERGY LIMIT EB AT WHICH TO CALCULATE
C     RATIOS. THIS CYCLE IS CONTINUED AS LONG AS ANY TABULATED ENERGY
C     POINTS REMAIN IN EITHER CROSS SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP7/XZOOM(2),YZOOM(2,5),IUSE1(60),IUSE2(60)
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      COMMON/RATDAT/XA(2),YA(2),XB,YB(2),N3CORE,N3SCR
      COMMON/ZEROES/EZERO,MYZERO
      DIMENSION IPTAB(2),XB2(2)
C-----INITIALIZE COUNT OF POINTS WHERE CROSS SECTIONS OF DATA1 IS
C-----ZERO AND DATA2 IS NOT (CANNOT DEFINE RATIO AT THESE PINTS).
      MYZERO=0
      EZERO=0.0d0
C-----INITIALIZE COUNT OF THE NUMBER OF RATIO POINTS IN CORE AND ON
C-----SCRATCH.
      N3CORE=0
      N3SCR=0
c-----------------------------------------------------------------------
C
C     DEFINE ENERGIES AND CROSS SECTIONS OVER FIRST INTERVAL WITHIN
C     REQUESTED ENERGY INTERVAL.
C
C     DEFINE LOWEST ENERGY WHERE BOTH ARE NON-ZERO.
C
c-----------------------------------------------------------------------
      DO 30 ISET=1,2
      IM1=IUSE1(ISET)
      IM2=IUSE2(ISET)
      DO 10 II=IM1,IM2
      CALL XYCURV(XA(ISET),YA(ISET),II,ISET)
      IF(YA(ISET).ne.0.0d0) go to 20
   10 CONTINUE
C-----NO POINTS IN REQUESTED ENERGY RANGE.
      RETURN
c-----START AT COMMON (HIGHER) POSITIVE CROSS SECTION POINT.
   20 IF(ISET.EQ.1) USEMIN=XA(ISET)
      IF(XA(ISET).GT.USEMIN) USEMIN=XA(ISET)
   30 CONTINUE
c-----------------------------------------------------------------------
C
C     DEFINE HIGHEST COMMON ENERGY.
C
c-----------------------------------------------------------------------
      USEMAX=XYLIMS(2,1,1)
      IF(XYLIMS(2,1,2).LT.USEMAX) USEMAX=XYLIMS(2,1,2)
c-----------------------------------------------------------------------
C
C     INITIALIZE LIMITS OF RATIO PLOT.
C
c-----------------------------------------------------------------------
      XYLIMS(1,1,3)=USEMIN
      XYLIMS(2,1,3)=USEMAX
      XYLIMS(1,2,3)=1.0d0
      XYLIMS(2,2,3)=1.0d0
C-----NOTHING TO DO IF NO OVERLAP.
      IF(USEMIN.GE.USEMAX) RETURN
c-----------------------------------------------------------------------
C
C     DEFINE BOTH CROSS SECTIONS AT LOWER ENERGY LIMIT.
C
c-----------------------------------------------------------------------
      DO 70 ISET=1,2
      IM1=IUSE1(ISET)
      IM2=IUSE2(ISET)
      CALL XYCURV(XA(ISET),YA(ISET),IM1,ISET)
      IM1=IM1+1
      DO 40 II=IM1,IM2
      XB2(ISET)=XA(ISET)
      YB(ISET)=YA(ISET)
      CALL XYCURV(XA(ISET),YA(ISET),II,ISET)
      IF(XA(ISET).gt.USEMIN) go to 50
   40 CONTINUE
C-----ALL DATA POINTS BELOW REQUESTED LOWER ENERGY LIMIT.
      RETURN
C-----INTERVAL DEFINED. TRUNCATE TO REQUESTED ENERGY RANGE.
   50 IF(XB2(ISET).GE.USEMIN) GO TO 60
      YB(ISET)=TERPY(USEMIN,XA(ISET),XB2(ISET),YA(ISET),YB(ISET),2)
      XB2(ISET)=USEMIN
   60 IF(XA(ISET).LE.USEMAX) GO TO 70
      YA(ISET)=TERPY(USEMAX,XA(ISET),XB2(ISET),YA(ISET),YB(ISET),2)
      XA(ISET)=USEMAX
C-----SAVE INDEX TO FIRST POINT WHICH MUST BE CONSIDERED.
   70 IPTAB(ISET)=II
c-----------------------------------------------------------------------
C
C     DEFINE RATIO AT FIRST TABULATED POINT (XB).
C
c-----------------------------------------------------------------------
      XB=XB2(1)
      CALL OVEREZ
c-----------------------------------------------------------------------
C
C     DEFINE BEGINNING OF NEXT INTERVAL TO BE THE SAME AS THE END OF
C     THE LAST INTERVAL. SELECT LONGEST ENERGY INTERVAL OVER WHICH BOTH
C     CROSS SECTIONS ARE LINEARLY INTERPOLABLE (THIS INTERVAL EXTENDS
C     FROM XB TO THE MINIMUM OF XA(1) AND XA(2)).
C
c-----------------------------------------------------------------------
C-----SAVE LAST ENERGY.
   80 XBB=XB
C-----DEFINE NEXT LOWEST ENERGY WITHIN REQUESTED ENERGY RANGE.
      XB=XA(1)
      IF(XA(2).LT.XB) XB=XA(2)
      IF(USEMAX.LT.XB) XB=USEMAX
C-----DEFINE CROSS SECTION FOR EACH CROSS SECTION AT NEXT LOWEST ENERGY.
      DO 120 ISET=1,2
      IF(XA(ISET).gt.XB) go to 110
C-----NEXT CALCULATION WILL BE AT THE ENERGY POINT XB. DEFINE CROSS
C-----SECTION AT THIS ENERGY POINT AND SINCE ENERGY INTERVAL HAS BEEN
C-----COMPLETELY USED, SELECT NEXT INTERVAL. EXTEND CROSS SECTION BEYOND
C-----ITS TABULATED ENERGY RANGE AS CONSTANT.
   90 YB(ISET)=YA(ISET)
      IPTAB(ISET)=IPTAB(ISET)+1
      IF(IPTAB(ISET).LE.IUSE2(ISET)) GO TO 100
      XA(ISET)=2.0d+20
      GO TO 120
  100 CALL XYCURV(XA(ISET),YA(ISET),IPTAB(ISET),ISET)
C-----SKIP POINT IF IT IS IDENTICAL TO LAST POINT.
      IF(XA(ISET).EQ.XB.AND.YA(ISET).EQ.YB(ISET)) GO TO 90
      GO TO 120
C-----NEXT CALCULATION WILL ONLY BE OVER A PORTION OF THE ENERGY
C-----INTERVAL. INTERPOLATE BETWEEN XBB AND XA TO DEFINE VALUE AT XB.
  110 YB(ISET)=TERPY(XB,XA(ISET),XBB,YA(ISET),YB(ISET),2)
  120 CONTINUE
C-----DEFINE RATIO AT LAST POINT (XB).
      CALL OVEREZ
C-----KEEP PROCEEDING TO NEXT POINT UNTIL NO MORE POINTS REMAIN IN ONE
C-----SET OR MAXIMUM REQUESTED ENERGY RANGE REACHED.
      IF(IPTAB(1).GT.IUSE2(1)) GO TO 130
      IF(IPTAB(2).GT.IUSE2(2)) GO TO 130
      IF(XB.GE.USEMAX) GO TO 130
      GO TO 80
C-----SET UP RATIO TO BE READ.
  130 CALL SETRAT
      RETURN
      END
      SUBROUTINE OVEREZ
C=======================================================================
C
C     THIS ROUTINE IS DESIGNED TO COMPUTE THE RATIO OF TWO CROSS
C     SECTIONS AND TO SAVE THE RESULT IN THE PAGING SYSTEM.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      COMMON/RATDAT/XA(2),YA(2),XB,YB(2),N3CORE,N3SCR
      COMMON/ZEROES/EZERO,MYZERO
      INCLUDE 'complot.h'
      EQUIVALENCE (ISCR(3),ISCR3)
      DATA KPASS/0/
C-----INITIALIZE COUNTER ON FIRST POINT.
      IF(N3CORE.EQ.0.AND.N3SCR.EQ.0) KPASS=0
C-----IF NO ROOM IN PAGING SYSTEM MOVE DATA TO SCRATCH FILE.
      IF(N3CORE.LT.MAXPOINT) GO TO 10
      IF(N3SCR.EQ.0.AND.IMUSED(3).GT.0) REWIND ISCR3
      WRITE(ISCR3) XPAGE3,YPAGE3
      N3SCR=N3SCR+MAXPOINT
      N3CORE=0
   10 N3CORE=N3CORE+1
C-----DEFINE ENERGY.
      XTAB(N3CORE,3)=XB
C-----DEFINE RATIO AT CURRENT ENERGY POINT.
      IF(YB(1).ne.0.0d0) go to 50
C-----DATA1 IS ZERO. IF DATA2 IS ALSO ZERO THIS IS O.K. OTHERWISE
C-----DEFINE RATIO TO BE LARGE POSITIVE OR NEGATIVE NUMBER.
      IF(YB(2).lt.0.0d0) go to 20
      IF(YB(2).gt.0.0d0) go to 30
      YTAB(N3CORE,3)=1.0d0
      GO TO 60
   20 YTAB(N3CORE,3)=-1000.0d0
      GO TO 40
   30 YTAB(N3CORE,3)= 1000.0d0
C-----COUNT NUMBER OF POINTS WHERE DATA1 IS ZERO AND DATA2 IS NOT.
C-----SAVE FIRST ENERGY WHERE THIS OCCURS.
   40 IF(MYZERO.EQ.0) EZERO=XB
      MYZERO=MYZERO+1
      GO TO 100
C-----DEFINE RATIO.
   50 YTAB(N3CORE,3)=YB(2)/YB(1)
c-----------------------------------------------------------------------
C
C     KEEP TRACK OF X AND Y RANGE TO DEFINE RATIO LIMITS.
C
c-----------------------------------------------------------------------
   60 IF(KPASS.gt.0) go to 70
      KPASS=1
      XYLIMS(1,1,3)=XB
      XYLIMS(2,1,3)=XB
      XYLIMS(1,2,3)=YTAB(N3CORE,3)
      XYLIMS(2,2,3)=XYLIMS(1,2,3)
      XMIN=XB
      XMAX=XB
      GO TO 100
   70 IF(YTAB(N3CORE,3).GE.XYLIMS(1,2,3)) GO TO 80
      XYLIMS(1,2,3)=YTAB(N3CORE,3)
      XMIN=XB
   80 IF(YTAB(N3CORE,3).LE.XYLIMS(2,2,3)) GO TO 90
      XYLIMS(2,2,3)=YTAB(N3CORE,3)
      XMAX=XB
   90 XYLIMS(2,1,3)=XB
C-----IF RATIO WILL NOT BE PLOTTED KEEP RESETTING RATIO POINT INDEX TO
C-----PREVENT COPYING RATIO TO SCRATCH.
  100 IF(MODPLT.EQ.-1.OR.MODPLT.EQ.1.OR.MODPLT.EQ.2) N3CORE=1
      RETURN
      END
      SUBROUTINE SETRAT
C=======================================================================
C
C     SET UP RATIO TO READ (IF RATIOS WILL BE PLOTTED).
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP7/XZOOM(2),YZOOM(2,5),IUSE1(60),IUSE2(60)
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      COMMON/RATDAT/XA(2),YA(2),XB,YB(2),N3CORE,N3SCR
      COMMON/MINMAX/XMIN0,XMAX0,YMIN,YMAX,YMINPC,YMAXPC
      INCLUDE 'complot.h'
      EQUIVALENCE (ISCR(3),ISCR3)
C-----SAVE ORIGINAL POSITION OF MIN AND MAX.
      XMIN0=XMIN
      XMAX0=XMAX
C-----NOTHING TO DO IF RATIO WILL NOT BE PLOTTED.
      IF(MODPLT.EQ.-1.OR.MODPLT.EQ.1.OR.MODPLT.EQ.2) GO TO 20
C-----DEFINE NUMBER OF DATA POINTS IN RATIO.
      IUSE1(3)=1
      IUSE2(3)=N3CORE+N3SCR
      MTDEX1(3)=IUSE1(3)
      MTDEX2(3)=IUSE2(3)
      IXYLOW(3)=0
C-----IF DATA IS ON SCRATCH UNLOAD LAST PAGE, END FILE AND LOAD FIRST
C-----PAGE.
      IF(N3SCR.LE.0) GO TO 10
      WRITE(ISCR3) XPAGE3,YPAGE3
      END FILE ISCR3
      REWIND ISCR3
      IMUSED(3)=1
      READ(ISCR3) XPAGE3,YPAGE3
      IXYHI(3)=MAXPOINT
      RETURN
C-----ALL DATA CORE RESIDENT. DEFINE INDEX TO LAST POINT IN CORE.
   10 IXYHI(3)=N3CORE
   20 RETURN
      END
      SUBROUTINE XYCURV(X,Y,I,INDEX)
C=======================================================================
C
C     DEFINE X AND Y VALUES FROM PAGING SYSTEM.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      COMMON/PAGLIM/XYLIMS(2,2,10),MTDEX1(10),MTDEX2(10)
      INCLUDE 'complot.h'
      EQUIVALENCE (ISCR(3),ISCR3)
C-----INSURE POINT INDEX IS IN LEGAL RANGE.
      IF(I.GE.MTDEX1(INDEX).AND.I.LE.MTDEX2(INDEX)) GO TO 10
      WRITE(OUTP,70) I,MTDEX1(INDEX),MTDEX2(INDEX),INDEX
C-----TERMINATE.
      CALL ENDERROR
C-----IF DATA IS NOT IN CORE LOAD CORRECT PAGE.
   10 NSCR=ISCR(INDEX)
      IF(I.GT.IXYLOW(INDEX)) GO TO 20
      REWIND NSCR
      IXYHI(INDEX)=0
   20 IF(I.LE.IXYHI(INDEX)) GO TO 60
      IF(INDEX.eq.2) go to 30
      IF(INDEX.gt.2) go to 40
      READ(NSCR) XPAGE1,YPAGE1
      GO TO 50
   30 READ(NSCR) XPAGE2,YPAGE2
      GO TO 50
   40 READ(NSCR) XPAGE3,YPAGE3
   50 IXYLOW(INDEX)=IXYHI(INDEX)
      IXYHI(INDEX)=IXYHI(INDEX)+MAXPOINT
      GO TO 20
C-----DEFINE REQUIRED POINT.
   60 ICORE=I-IXYLOW(INDEX)
      X=XTAB(ICORE,INDEX)
      Y=YTAB(ICORE,INDEX)
      RETURN
   70 FORMAT(' XYCURV Index Error...I/MTDEX1/MTDEX2/INDEX=',4I8/
     1 21H Execution Terminated)
      END
      SUBROUTINE XYPONT(X,DXM,DXP,Y,DYM,DYP,IPT,ISET)
C=======================================================================
C
C     DUMMY ROUTINE TO PLOT EXPERIMENTAL DATA POINTS (TO SATIFY
C     GGPLOT PLOTTING INTERFACE).
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP0/NPRINT,NPLOT,NCHR,NSYM
      X  =0.0d0
      DXM=0.0d0
      DXP=0.0d0
      Y  =0.0d0
      DYM=0.0d0
      DYP=0.0d0
      IPT=1
      ISET=1
      IF(NPRINT.GT.0) WRITE(NPRINT,10)
      RETURN
   10 FORMAT(' WARNING - Illegal Call to SUBROUTINE XYPONT')
      END
      SUBROUTINE MTHOL(MT,MTBCD,IMTBCD)
C=======================================================================
C
C     DEFINE HOLLERITH EQUIVALENT OF REACTION (MT).
C
C     DEFINITION OF TABLES
C
C     MTTABA = CHARACTER EQUIVALENT OF EACH MT NUMBER
C              UP TO 40 CHARACTERS PER MT - LEFT ADJUSTED
C     MTTABB = (1) NUMBER OF CHARACTERS IN EACH CHARACTER EQUIVALENT
C              (2) LOWER MT LIMIT....ENDF/B-VI
C              (3) UPPER MT LIMIT
C              (4) LOWER MT LIMIT....ENDF/B-V
C              (5) UPPER MT LIMIT
C
C     THE CHARACTER EQUIVALENT IS RETURNED IN MTBCD
C
C     MTBCD  = CHARACTER EQUIVALENT OF MT
C              UP TO 40 CHARACTERS - LEFT ADJUSTED
C     IMTBCD = NUMBER OF CHARACTERS IN CHARACTER EQUIVALENT OF MT
C
C     WARNING...THIS ROUTINE WILL LOAD THE DEFINITION OF ALL MT
C               NUMBERS THE FIRST TIME THAT THIS ROUTINE IS CALLED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 MTTABA,MTBCD,GOOF
c-----01/18/11 - increased dimension from 200 to 1,000
      COMMON/MTDAT1/MTTABA(40,1000)
      COMMON/MTDAT2/MTTABB(5,1000),MTLONG,MTTAPE
      COMMON/VERSON/IVERSE
      DIMENSION MTBCD(40),GOOF(15)
      DATA IPASS/0/
      DATA IGOOF/15/
      DATA GOOF/
     1 '*','*','*','U','n','d','e','f','i','n','e','d','*','*','*'/
C-----READ THE DEFINITION OF ALL MT NUMBERS THE FIRST TIME THAT THIS
C-----ROUTINE IS CALLED.
      IF(IPASS.NE.0) GO TO 10
      CALL MTREAD
      IPASS=1
c-----------------------------------------------------------------------
C
C     USE EITHER ENDF/B-V OR VI DEFINITIONS.
C
c-----------------------------------------------------------------------
   10 IF(IVERSE.EQ.6) GO TO 30
C-----ENDF/B-V
      DO 20 M=1,MTLONG
      IF(MTTABB(4,M).LE.0) GO TO 20
      IF(MT.lt.MTTABB(4,M)) go to 20
      IF(MT.eq.MTTABB(4,M)) go to 70
      IF(MT.le.MTTABB(5,M)) go to 70
   20 CONTINUE
      GO TO 50
C-----ENDF/B-VI
   30 DO 40 M=1,MTLONG
      IF(MTTABB(2,M).LE.0) GO TO 40
      IF(MT.lt.MTTABB(2,M)) go to 40
      IF(MT.eq.MTTABB(2,M)) go to 70
      IF(MT.le.MTTABB(3,M)) go to 70
   40 CONTINUE
C-----MT NUMBER NOT DEFINED.
   50 DO 60 I=1,IGOOF
   60 MTBCD(I)=GOOF(I)
      IMTBCD=IGOOF
      RETURN
C-----DEFINE MT DEFINITION.
   70 DO 80 L=1,40
   80 MTBCD(L)=MTTABA(L,M)
      IMTBCD=MTTABB(1,M)
c-----------------------------------------------------------------------
C
C     IF NOT NEUTRON INCIDENT TRY TO DEFINE INCIDENT PARTICLE.
C
c-----------------------------------------------------------------------
      CALL WHATIN(MTBCD,IMTBCD)
      RETURN
      END
      SUBROUTINE WHATIN(MTBCD,IMTBCD)
C=======================================================================
C
C     IF NOT NEUTRON INCIDENT TRY TO DEFINE INCIDENT PARTICLE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 MTBCD,BLANK,IMN,SHORTP,LONGP
      COMMON/PARTIN/INPART
      DIMENSION MTBCD(40),IMN(3),SHORTP(6,0:5),LONGP(9,0:5),NOTN(0:5)
      DATA IMN/'(','n',','/
C-----DEFINE CHARGED PARTICLES.
      DATA NOTN/   0,1001,1002,1003,2003,2004/
      DATA BLANK/' '/
      DATA SHORTP/
     1 '(',']','g',',',' ',' ',
     1 '(','p',',',' ',' ',' ',
     1 '(','d',',',' ',' ',' ',
     1 '(','t',',',' ',' ',' ',
     1 '(','H','e','-','3',',',
     1 '(',']','a',',',' ',' '/
      DATA LONGP/
     1 'P','h','o','t','o','n',' ',' ',' ',
     1 'P','r','o','t','o','n',' ',' ',' ',
     1 'D','e','u','t','e','r','o','n',' ',
     1 'T','r','i','t','o','n',' ',' ',' ',
     1 'H','e','-','3',' ',' ',' ',' ',' ',
     1 ']','a',' ',' ',' ',' ',' ',' ',' '/
C-----3/22/2012 - NOTHING TO DO IF INCIDENT PARTICLE IS NEUTRON.
      IF(INPART.EQ.1) RETURN
      DO 10 KPART=0,5
      IF(INPART.EQ.NOTN(KPART)) GO TO 20
   10 CONTINUE
C-----CANNOT DEFINE INCIDENT PARTICLE.
      GO TO 150
C-----SKIP LEADING BLANKS IN TITLE.
   20 DO 30 NOTB=1,IMTBCD
      IF(MTBCD(NOTB).NE.BLANK) GO TO 40
   30 CONTINUE
      GO TO 150
C-----IF TITLE STARTS WITH BLANKS ASSUME IT IS FOLLOWED BY MEV BLANK -
C-----SPACE OVER THESE 4 CHARACTERS.
   40 IF(NOTB.GT.1) NOTB=NOTB+4
C-----DOES TITLES START WITH (N,
      K=NOTB-1
      DO 50 L=1,3
      K=K+1
      IF(MTBCD(K).NE.IMN(L)) GO TO 100
   50 CONTINUE
C-----REPLACE (N,
      DO 60 KK=1,6
      IF(SHORTP(KK,KPART).EQ.BLANK) GO TO 70
   60 CONTINUE
      KK=7
   70 KK=KK-1
      LEFT=KK-3
      DO 80 M=IMTBCD,NOTB,-1
   80 MTBCD(M+LEFT)=MTBCD(M)
      K=K-3
      DO 90 M=1,KK
      K=K+1
   90 MTBCD(K)=SHORTP(M,KPART)
      IMTBCD=IMTBCD+LEFT
      GO TO 150
C-----USE LONG FORM.
  100 DO 110 KK=1,9
      IF(LONGP(KK,KPART).EQ.BLANK) GO TO 120
  110 CONTINUE
      KK=9
  120 LEFT=KK
      DO 130 M=IMTBCD,NOTB,-1
  130 MTBCD(M+LEFT)=MTBCD(M)
      K=NOTB-1
      DO 140 M=1,KK
      K=K+1
  140 MTBCD(K)=LONGP(M,KPART)
      IMTBCD=IMTBCD+LEFT
  150 RETURN
      END
      SUBROUTINE MTREAD
C=======================================================================
C
C     READ TABLE OF MT DEFINITIONS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 MTTABA
c-----01/18/11 - increased dimension from 200 to 1,000
      COMMON/MTDAT1/MTTABA(40,1000)
      COMMON/MTDAT2/MTTABB(5,1000),MTLONG,MTTAPE
C-----SET UP LOOP TO READ MT DEFINITIONS (NOW UP TO 1,000).
      DO 30 I=1,1000
      READ(MTTAPE,50) (MTTABB(J,I),J=2,5),(MTTABA(J,I),J=1,40)
C-----TERMINATE ON ALL MT LIMITS = ZERO.
      DO 10 J=2,5
      IF(MTTABB(J,I).GT.0) GO TO 20
   10 CONTINUE
      GO TO 40
C-----DEFINE LENGTH OF MT DEFINITION.
   20 CALL LONGX(MTTABA(1,I),MTTABB(1,I),40)
   30 CONTINUE
      I=201
   40 MTLONG=I-1
      RETURN
   50 FORMAT(4I4,1X,40A1)
      END
      SUBROUTINE MTSET(MT,MTBCD)
c-----------------------------------------------------------------------
c
c     2017/3/23 - Replaced Q MeV by MT=### (6 Characters)
c
c-----------------------------------------------------------------------
      INCLUDE 'implicit.h'
      CHARACTER*1 MTBCD   ,DIGITS     ,MTEQ   ,BLANK1
      DIMENSION   MTBCD(*),DIGITS(0:9),MTEQ(3)
      DATA MTEQ/'M','T','='/
      DATA BLANK1/' '/
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
C-----Add MT= in first 3 locations
      MTBCD(1) = MTEQ(1)
      MTBCD(2) = MTEQ(2)
      MTBCD(3) = MTEQ(3)
C-----Add MT ### in next 3 locations
      mm = MT
      kk = 100
      ib = 0
      do k=1,3
      ii = mm/kk
      if(ii.gt.0) ib = 1                  ! skip leading 0 (leave blank)
      if(ib.gt.0) then
      MTBCD(k+3) = DIGITS(ii)
      else
      MTBCD(k+3) = BLANK1
      endif
      mm = mm - kk*ii
      kk = kk/10
      enddo
      return
      end
      SUBROUTINE SETLAB(IZA,MAT,MF,MT,  LCT,LTTIN,ILEG)
c-----2017/3/20 - Dropped Q and added ILEG (Legendre order)
c     SUBROUTINE SETLAB(IZA,MAT,MF,MT,Q,LCT,LTTIN,ILEG)
C=======================================================================
C
C     DEFINE ALL HEADING AND AXIS INFORMATION FOR THE CURRENT PLOT
C     AND LOAD INTO GGP HEADER AND FOOTER ARRAYS.
C
C     ARGUMENTS
C     ---------
C     IZA     = ENDF/B ZA NUMBER (1000*Z +A, A=0 FOR NATURAL).
C     MAT     = ENDF/B MAT NUMBER.
C     MF      = ENDF/B MF NUMBER.
C     MT      = ENDF/B MT NUMBER.
C     LCT     = ANGULAR DISTRIBUTION SYSTEM
C             = 1 - LAB
C             = 2 - CENTER OF MASS (CM)
C     LTTIN   = ANGULAR DISTRIBUTION REPRESENTATION
C             = 1 - LEGENDRE COEFFICIENTS
C             = 2 - TABULATED PROBABILITIES VERSUS COSINE
C             = 3 - BOTH - WARNING: treated as = 1 inside this routine
C     ILEG    = Legendre Coefficient index, e.g., f 1, 2, 3,...
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 MATID,EUNIT,BLANK,NOLINR,TITLES,EVALID,
     1 XLABEL,YLABEL,XUNITS,THSHOL,
     2 TIT1L,TIT1M,TIT1R,TIT2L,TIT2M,TIT2R,FOOTL,FOOTM,FOOTR,
     3 YUNITS,ERROR,EV,BARNS,MTBOX(40)
      CHARACTER*40 XLAB(12),YLAB(12),XLABEL40,YLABEL40
      CHARACTER*80 YTIT(12),TIT2M80
      CHARACTER*1 DIGITS(0:9)
      COMMON/GPP4/XLABEL(40),XUNITS(40),YLABEL(40),YUNITS(40)
      COMMON/GPP5/TIT1L(80),TIT1M(80),TIT1R(80),TIT2L(80),TIT2M(80),
     1 TIT2R(80),FOOTL(80),FOOTM(80),FOOTR(80)
      COMMON/GPP6/TITLES(84,30)
      COMMON/IDEVAL/EVALID(84,3)
      COMMON/THRES/THRES3(3)
      COMMON/MELINR/IMLINR
c-----2014/7/22 - added for MT=10 Data.
c-----2015-2: Increased MFMTX and MFMTY from 11 to 12
      COMMON/MF10COM/IZAP1,LFS1,NS101,JS101,
     1               IZAP2,LFS2,NS102,JS102
      DIMENSION MATID(3),EUNIT(4,3),NOLINR(16),
     1 EV(2),BARNS(5),THSHOL(11),
     1 MFMTAB(4,2,15),MFMTY(12),ERROR(9),MFMTZ(15),MFMTX(12)
      EQUIVALENCE (XLABEL(1),XLABEL40),(YLABEL(1),YLABEL40),
     1 (TIT2M(1),TIT2M80)
C-----Legendre Coefficient f number
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
C-----DEFINE MESSAGE AND DATA UNITS FOR THRESHOLD.
      DATA THSHOL/' ','T','h','r','e','s','h','o','l','d',' '/
      DATA EUNIT/
     1 ' ','e','V',' ',
     2 ' ','k','e','V',
     3 ' ','M','e','V'/
C-----MF/MT/LCT/LTT RANGES.
c       MF   MT LCT LTT    MF   MT LCT LTT
      DATA MFMTAB/
     1   1,   1, 0, 0,      1, 999, 0, 0,
     2   3,   1, 0, 0,      3, 250, 0, 0,
     3   3, 251, 0, 0,      3, 253, 0, 0,
     4   3, 254, 0, 0,      3, 255, 0, 0,
     5   3, 256, 0, 0,      3, 999, 0, 0,
     6   4,   1, 1, 1,      4, 999, 1, 1,
     7   4,   1, 2, 1,      4, 999, 2, 1,
     8   4,   1, 1, 2,      4, 999, 1, 2,
     9   4,   1, 2, 2,      4, 999, 2, 2,
     A   5,   1, 0, 0,      5, 999, 0, 0,
     1  10,   1, 0, 0,     10, 999, 0, 0,  ! 2014/7/20 - Added MF=10
     2  23,   1, 0, 0,     23, 999, 0, 0,
     3  27, 502, 0, 0,     27, 502, 0, 0,
     4  27, 504, 0, 0,     27, 504, 0, 0,
     5  27, 505, 0, 0,     27, 506, 0, 0/
C-----INDEX TO LABEL TABLE BASED ON MF/MT/LCT/LTT RANGE SELECTED.
      DATA MFMTZ/1,2,1,11,2,6,7,8,9,10,12,2,3,4,5/ ! Add MF=10
c-----2015-2: MFMTX and MFMTY added 12-th value for MF=10
C-----WHICH X LABELS ARE IN UNITS OF EV.
      DATA MFMTX/1,1,1,1,1,1,1,0,0,1,1,1/
C-----WHICH Y LABELS ARE IN UNITS OF BARNS.
      DATA MFMTY/0,1,0,0,0,0,0,0,0,0,0,1/
C-----ERROR MESSAGE.
      DATA ERROR/'U','n','d','e','f','i','n','e','d'/
C-----X AND Y AXIS STANDARD UNITS.
      DATA EV/'e','V'/
      DATA BARNS/'b','a','r','n','s'/
C-----X AXIS LABELS
      DATA XLAB/
     1 'Incident Energy ',
     2 'Incident Energy ',
     3 'E*Sin(]q/2)     ',
     4 'E*Sin(]q/2)     ',
     5 'Incident Energy ',
     6 'Incident Energy ',
     7 'Incident Energy ',
     8 'Cosine (Lab)    ',
     9 'Cosine (CM)     ',
     A 'Secondary Energy',
     1 'Incident Energy ',
     2 'Incident Energy '/
c       1234567890123456
C-----Y AXIS LABELS.
      DATA YLAB/
     1 'Parameters                    ',
     2 'Cross Section                 ',
     3 'Form Factor                   ',
     4 'Scattering Function           ',
     5 'Scattering Factor             ',
     6 'f1 Legendre Coefficient (Lab) ',
     7 'f1 Legendre Coefficient (CM)  ',
     8 'Probability/Cosine            ',
     9 'Probability/Cosine            ',
     A 'Probability/eV                ',
     1 'Ratio                         ',
     2 'Cross Section                 '/
c       123456789012345678901234567890
      DATA YTIT/
     1 'Parameters                    ',
     2 'Cross Section                 ',
     3 'Form Factor                   ',
     4 'Scattering Function           ',
     5 'Scattering Factor             ',
     6 'f1 Legendre Coefficient (Lab) ',
     7 'f1 Legendre Coefficient (CM)  ',
     8 'Probability/Cosine            ',
     9 'Probability/Cosine            ',
     A 'Probability/eV                ',
     1 'Ratio                         ',
     2 'Radionuclide Production Cross Section'/
c       1234567890123456789012345678901234567
      DATA MATID/'M','A','T'/
      DATA NOLINR/'E','R','R','O','R',' ','N','o','t',' ',
     1 'L','i','n','e','a','r'/
      DATA BLANK/' '/
c-----------------------------------------------------------------------
c
c     Update Input Parameter Definitions
C
c-----------------------------------------------------------------------
c-----2017/3/20 - treat BOTH (LTT=3) as Legendre (LTT=1)
      LTTX = LTTIN
      if(LTTX.eq.3) LTTX = 1   ! Treat Legendre and BOTH as same here
c-----------------------------------------------------------------------
c
C     INITIALIZE ALL TITLES TO BLANK.
c
c-----------------------------------------------------------------------
      DO 10 I=1,80
      TIT1L(I)=BLANK
      TIT1M(I)=BLANK
      TIT1R(I)=BLANK
      TIT2L(I)=BLANK
      TIT2M(I)=BLANK
      TIT2R(I)=BLANK
      FOOTL(I)=BLANK
      FOOTM(I)=BLANK
   10 FOOTR(I)=BLANK
      DO 20 I=1,40
      XLABEL(I)=BLANK
      XUNITS(I)=BLANK
      YLABEL(I)=BLANK
   20 YUNITS(I)=BLANK
c-----------------------------------------------------------------------
C
C     DEFINE TITLES FOR EACH CURVE.
C
c-----------------------------------------------------------------------
      DO 30 I=1,3
      DO 30 J=1,84
   30 TITLES(J,I)=EVALID(J,I)
C-----IF NECESSARY ADD THRESHOLD ENERGY TO TITLE.
      DO 80 I=1,2
      IF(THRES3(I).LE.0.0d0) GO TO 80
C-----DEFINE LENGTH OF TITLE AND ADD THE WORD THRESHOLD.
      CALL LONGX(EVALID(1,I),IT1,40)
      DO 40 J=1,11
      IT1=IT1+1
   40 TITLES(IT1,I)=THSHOL(J)
      IT1=IT1+1
C-----SELECT UNITS OF EV, KEV OR MEV FOR OUTPUT.
      XX=THRES3(I)
      IUNIT=1
      IF(XX.LT.1000.0d0) GO TO 60
      IF(XX.GE.1000000.0d0) GO TO 50
      XX=XX/1000.0d0
      IUNIT=2
      GO TO 60
   50 XX=XX/1000000.0d0
      IUNIT=3
C-----ADD NUMBER.
   60 IXX=4
      IF(XX.GE.10.0d0) IXX=3
      IF(XX.GE.100.0d0) IXX=2
      CALL NUMBR2(XX,IXX,TITLES(IT1,I),IT2)
      IT1=IT1+5
C-----ADD UNITS.
      DO 70 J=1,4
      IT1=IT1+1
   70 TITLES(IT1,I)=EUNIT(J,IUNIT)
   80 CONTINUE
c-----------------------------------------------------------------------
C
C     SELECT LABELS BASED ON MF/MT/LCT/LTT RANGES.
C
c-----------------------------------------------------------------------
C----- 2015-2: Increased from 14 to 15
      DO 90 I=1,15
      IF(MF.LT.MFMTAB(1,1,I).OR.MT.  LT.MFMTAB(2,1,I).OR.
     1  LCT.LT.MFMTAB(3,1,I).OR.LTTX.LT.MFMTAB(4,1,I)) GO TO 90
      IF(MF.GT.MFMTAB(1,2,I).OR.MT.  GT.MFMTAB(2,2,I).OR.
     1  LCT.GT.MFMTAB(3,2,I).OR.LTTX.GT.MFMTAB(4,2,I)) GO TO 90
      GO TO 110
   90 CONTINUE
C-----NO MATCH. FILL IN ERROR MESSAGE.
      DO 100 I=1,9
      XLABEL(I)=ERROR(I)
      YLABEL(I)=ERROR(I)
  100 TIT2M(I)=ERROR(I)
      GO TO 140
C-----MATCH. FILL IN TITLES AND UNITS.
  110 INDEX=MFMTZ(I)
      XLABEL40 = XLAB(INDEX)
C-----X AXIS UNITS ARE EV OR BLANK.
      IF(MFMTX(INDEX).EQ.0) GO TO 120
      XUNITS(1)=EV(1)
      XUNITS(2)=EV(2)
c-----------------------------------------------------------------------
C
C     DATA TYPE IDENTIFICATION IN MIDDLE OF SECOND LINE.
C
c-----------------------------------------------------------------------
  120 YLABEL40 = YLAB(INDEX)
      TIT2M80  = YTIT(INDEX)
c-----2017/3/20 - Define Legendre Coefficient f number
      IF(YLABEL(1).eq.'f'.and.YLABEL(2).eq.'1') then
      YLABEL(2) = DIGITS(ILEG)
      TIT2M (2) = DIGITS(ILEG)
      ENDIF
C-----Y AXIS UNITS ARE BARNS OR BLANK.
      IF(MFMTY(INDEX).EQ.0) GO TO 140
      DO 130 I=1,5
  130 YUNITS(I)=BARNS(I)
c-----------------------------------------------------------------------
C
C     MAT NUMBER IN UPPER LEFT HAND CORNER OF PLOT.
C
c-----------------------------------------------------------------------
  140 DO 150 I=1,3
  150 TIT1L(I)=MATID(I)
      FMAT=MAT
      CALL NUMBR2(FMAT,-1,TIT1L(5),ITOPL)
c-----------------------------------------------------------------------
C
C     ISOTOPE IDENTIFICATION IN UPPER AND LOWER RIGHT HAND CORNER
C
c-----------------------------------------------------------------------
      CALL ZAHOL(IZA,TIT1R)
      DO 160 I=1,12
  160 FOOTR(I)=TIT1R(I)
c-----------------------------------------------------------------------
C
C     REACTION TYPE IDENTIFICATION IN MIDDLE OF FIRST LINE.
C
c-----------------------------------------------------------------------
      CALL MTHOL(MT,TIT1M,IT)
c-----2014/7/22 - For MF=10 add ZAP and LFS to title.
      IF(MF.eq.10) then
      CALL TIT10(IZAP1,LFS1,MTBOX,IMTBOX)
      TIT1M(IT+1) = ':'
      IT = IT + 1
      do kk1=1,IMTBOX
      IF(MTBOX(kk1).ne.' ') go to 170
      enddo
      go to 180
  170 do kk=kk1,IMTBOX
      IT = IT + 1
      TIT1M(IT) = MTBOX(kk)
      enddo
      endif
c-----------------------------------------------------------------------
c
c     2017/3/23 - Replaced Q MeV by MT=###
c
c-----------------------------------------------------------------------
C-----IF FIRST WORD OF TITLE IS BLANK PLOT MT=###
  180 IF(TIT1M(1).NE.BLANK) GO TO 190
      CALL MTSET(MT,TIT1M(4))
c-----shift message 3 left and insure last 3 are blank
      do k=1,77
      TIT1M(k) = TIT1M(k+3)
      enddo
      TIT1M(78) = ' '
      TIT1M(79) = ' '
      TIT1M(80) = ' '
C-----------------------------------------------------------------------
C
C     WARNING IF DATA IS NOT LINEAR-LINEAR INTERPOLABLE TO LEFT ON
C     SECOND LINE.
C
C-----------------------------------------------------------------------
  190 IF(IMLINR.EQ.0) GO TO 210
      DO 200 I=1,16
  200 TIT2L(I)=NOLINR(I)
  210 RETURN
      END
      SUBROUTINE GOOFIE
C=======================================================================
C
C     MAXIMUM POSITIVE AND NEGATIVE DIFFERENCES TO RIGHT SECOND LINE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 TOHOL,DIFFER,TIT1L,TIT1M,TIT1R,TIT2L,TIT2M,TIT2R,
     1 FOOTL,FOOTM,FOOTR
      COMMON/GPP5/TIT1L(80),TIT1M(80),TIT1R(80),TIT2L(80),TIT2M(80),
     1 TIT2R(80),FOOTL(80),FOOTM(80),FOOTR(80)
      COMMON/MINMAX/XMIN0,XMAX0,YMIN,YMAX,YMINPC,YMAXPC
      DIMENSION YPC(2),TOHOL(3),RANGEI(7),IRANGE(7),NRANGE(2)
      EQUIVALENCE (YPC(1),YMINPC)
      DATA TOHOL/' ','T','o'/
      DATA DIFFER/'%'/
      DATA RANGEI/1000.0d0,100.0d0,10.0d0,
     1               1.0d0,  0.1d0,0.01d0,0.001d0/
      DATA IRANGE/  0   , 1   , 2  , 3 , 3 , 3  ,  3  /
C-----LIMIT ERROR TO +/- 9999.0 PER-CENT.
      IF(YMINPC.LT.-9999.0d0) YMINPC=-9999.0d0
      IF(YMINPC.GT. 9999.0d0) YMINPC= 9999.0d0
      IF(YMAXPC.LT.-9999.0d0) YMAXPC=-9999.0d0
      IF(YMAXPC.GT. 9999.0d0) YMAXPC= 9999.0d0
C-----DEFINE NUMBER OF PLACES AFTER DECIMAL POINT.
      DO 20 LIMIT=1,2
      ABSYPC=DABS(YPC(LIMIT))
      DO 10 I=1,7
      IF(ABSYPC.GE.RANGEI(I)) GO TO 20
   10 CONTINUE
      YPC(LIMIT)=0.0d0
      I=7
   20 NRANGE(LIMIT)=IRANGE(I)
C-----LOWER LIMIT.
      CALL NUMBR2(YMINPC,NRANGE(1),TIT2R,IT)
C-----  TO  BETWEEN ERROR LIMITS.
      TIT2R(7)=TOHOL(1)
      TIT2R(8)=TOHOL(2)
      TIT2R(9)=TOHOL(3)
C-----UPPER LIMIT.
      CALL NUMBR2(YMAXPC,NRANGE(2),TIT2R(11),IT)
C-----PER-CENT SIGN.
      TIT2R(17)=DIFFER
      RETURN
      END
      SUBROUTINE SET0
C=======================================================================
C
C     INITIALIZE ALL GENERAL PLOTTING PACKAGE (GPP) PARAMETERS.
C     THIS ROUTINE SHOULD BE CALLED FIRST BY ANY PROGRAM WHICH
C     USES GPP. THE PROGRAM CAN THEN RESET PARAMETERS AS NEEDED.
C
C     USER PLOTTING INTERFACE
C     =======================
C     TO CREATE A PLOT,
C     1) INITIALIZE ALL GPP PARAMETERS.....................CALL SET0
C     2) LOAD YOUR DATA INTO CORE USING ANY STORAGE CONVENTION THAT
C        YOU WISH...SEE DECRIPTION BELOW
C     3) ASSIGN CURVES AND/OR SETS OF POINTS TO SUBPLOTS...CALL SETMAP
C     4) DEFINE THE LAYOUT OF THE PLOT.....................CALL SET1
C     5) DEFINE THE X AND Y LIMITS OF EACH SUBPLOT
C     6) SET ALL GPP PARAMETERS THAT YOU WISH TO USE, DESCRIBED BELOW
C     7) CREATE A PLOT.....................................CALL GPPLOT
C
C     TO CREATE MORE PLOTS WITH THE SAME GPP PARAMETERS REPEAT STEPS
C     2), 5) AND 6). IF YOU CHANGE GPP PARAMETERS REPEAT STEPS 1) THROUG
C     7).
C
C     RESERVED I/O UNIT NUMBERS
C     =========================
C     GPP USES THE FOLLOWING UNIT NUMBERS
C      6    NPRINT   OUTPUT UNIT FOR PRINTED MESSAGES
C     16    NPLOT    WRITE HARDCOPY PLOTS
C     17    NCHR     READ SOFTWARE CHARACTERS
C     18    NSYM     READ SOFTWARE SYMBOLS AND LINE TYPES
C     YOU MAY USE UNIT 6 FOR YOUR OUTPUT, BUT DO NOT USE 16, 17 OR 18.
C
C     USER DATA INTERFACE
C     ===================
C     YOU CAN USE ANY STORAGE CONVENTION THAT YOU WISH FOR CURVES
C     AND SETS OF POINTS. BUT YOU MUST INCLUDE 2 ROUTINES TO
C     ALLOW YOUR DATA TO BE USED BY GGP. THESE 2 ROUTINES ARE,
C
C     CURVES
C     ======
C     SUBROUTINE XYCURV(X,Y,IPT,ICURVE)
C     X      = X VALUE (DOUBLE PRECISION).....RETURNED
C     Y      = Y VALUE (SINGLE PRECISION).....RETURNED
C     IPT    = INDEX TO POINT.................INPUT
C     ICURVE = INDEX TO CURVE.................INPUT
C
C     A CALL TO THIS ROUTINE MUST RETURN (X,Y) VALUES FOR POINT
C     IPT OF CURVE ICURVE, E.G.,
C     CALL XYCURV(X,Y,4,3)
C     SHOULD RETURN THE 4-TH POINT OF THE 3-RD CURVE
C
C     SETS OF POINTS
C     ==============
C     SUBROUTINE XYPONT(X,DXM,DXP,Y,DYM,DYP,IPT,ISET)
C     X      = X VALUE (DOUBLE PRECISION)..........RETURNED
C     DXM    = LOWER X ERROR (DOUBLE PRECISION)....RETURNED
C     DXP    = UPPER X ERROR (DOUBLE PRECISION)....RETURNED
C     Y      = Y VALUE (SINGLE PRECISION)..........RETURNED
C     DYM    = LOWER Y ERROR (SINGLE PRECISION)....RETURNED
C     DYP    = UPPER Y ERROR (SINGLE PRECISION)....RETURNED
C     IPT    = INDEX TO A POINT....................INPUT
C     ISET   = INDEX TO SET OF POINTS..............INPUT
C
C     A CALL TO THIS ROUTINE MUST RETURN (X,DXM,DXP,Y,DYM,DYP) VALUES
C     FOR POINT IPT OF THE SET OF POINTS ISET, E.G.,
C     CALL XYPONT(X,DXM,DXP,Y,DYM,DYP,4,3)
C     SHOULD RETURN THE 4-TH POINT OF THE 3-RD SET OF POINTS
C
C     X AND Y ERRORS MUST BE IN THE SAME UNITS AS THE X AND Y DATA
C     AND ARE RELATIVE TO THE X AND Y VALUES. FOR EXAMPLE, IF X IS
C     6.0 WITH AN UNCERTAINTY (ERROR) OF -2 PER-CENT AND +4 PER-CENT
C     THE CORRECT VALUES TO RETURN FROM XYPONT ARE,
C     DXM = 0.12  (2 PER-CENT OF 6.0)
C     DXP = 0.24  (4 PER-CENT OF 6.0)
C     X AND Y ERRORS MAY BE RETURN FROM XYPONT AS POSITIVE OR NEGATIVE.
C     GPP WILL ALWAYS TREAT THEM AS POSITIVE VALUES RELATIVE TO X AND
C     Y, E.G., ERROR BAR WILL EXTEND FROM X-DABS(DXM) TO X+DABS(DXP).
C
C     FRAME LAYOUT
C     ============
C     EACH FRAME MAY BE DIVIDED INTO ANY NUMBER OF PLOTS IN THE X AND
C     Y DIRECTIONS...THE NUMBER OF PLOTS IN EACH DIRECTION IS DEFINED
C     BY NSMALX AND NSMALY (DEFINED BELOW).
C
C     IN ADDITION EACH PLOT CAN BE DIVIDED INTO 1 TO 5 SUBPLOTS - ALL
C     WITH THE SAME X (E.G., ENERGY) RANGE, BUT EACH SUBPLOT MAY HAVE
C     ITS OWN Y (E.G., CROSS SECTION) RANGE.
C
C     THE NUMBER OF SUBPLOTS IS DEFINED BY MODPLT AND NFRAME (DESCRIBED
C     BELOW).
C
C     THE DATA TO APPEAR ON EACH SUBPLOT IS DEFINED BY /GPP3/ (DESCRIBED
C     BELOW).
C
C     DEFINITION OF PARAMETERS
C     ========================
C     ALL GPP PARAMETERS ARE LOCATED IN LABELLED COMMON BLOCKS WITH
C     THE NAMES GGPN, N = 0, 1, 2, 3,......
C
C     THE FOLLOWING ARE THE ONLY GPP PARAMETERS THAT YOU CAN CHANGE.
C
C  0  NPRINT =  6 - OUTPUT UNIT FOR PRINTED MESSAGES
C     NPLOT  = 16 - OUTPUT UNIT FOR HARDCOPY PLOTS
C     NCHR   = 17 - INPUT UNIT FOR SOFTWARE CHARACTERS
C     NSYM   = 18 - INPUT UNIT FOR SYMBOLS AND LINE TYPES
C     ------------------------------------------------------------------
C  1  MYPROG = PROGRAM IDENTIFICATION
C            = 0 - USER DEFINED PROGRAM
C            = 1 - COMPLOT
C            = 2 - EVALPLOT
C            = 3 - PLOTTAB
C            = 4 - SMOOTHY
C            = 5 - PLOTC4
C     MODPLT = PLOTTING MODE
C            = 0 - CURVE 1 OVER RATIO TO ALL OTHER DATA
C            = 1 - CURVE 1 OVER ALL OTHER DATA
C            = 2 - ALL DATA ON SAME PLOT
C            = 3 - CURVE 1 OVER ALL OTHER DATA OVER RATIO TO ALL OTHER D
C            = 4 - ALL DATA OVER RATIO TO ALL OTHER DATA
C     XYPLOT = LOWER AND UPPER X AND Y LIMITS OF LOCAL PLOTTER
C     XYEDGE = LOWER AND UPPER X AND Y LIMITS OF STANDARD PLOTTER
C     NSMALX = NUMBER OF PLOTS PER FRAME IN X DIRECTION
C     NSMALY = NUMBER OF PLOTS PER FRAME IN Y DIRECTION
C     ------------------------------------------------------------------
C  2  NGRID  = GRID TYPE FOR PLOTS
C            = 0 - TICK MARKS ON BORDER
C            = 1 - SOLID COARSE GRID
C            = 2 - DASHED COARSE GRID
C            = 3 - SOLID COARSE AND FINE GRID
C            = 4 - DASHED COARSE AND FINE GRID
C            = 5 - SOLID COARSE AND DASHED FINE GRID
C     NPOINT = IDENTIFY POINTS ON CURVES
C            = 0 - NO
C            = 1 - YES
C     NTHICK = LINE THICKNESS
C            = 0 TO  5 - THICK LINES AND CHARACTERS (NOT GRID)
C            =-1 TO -5 - THICK LINES (NOT CHARACTER OR GRID)
C     NFLIP  = PLOT ORIENTATION
C            = 0 - X HORIZONTAL - Y VERTICAL
C            = 1 - Y HORIZONTAL - X VERTICAL
C     NBORD  = FLAG TO DRAW BORDER AROUND PLOT
C            = 0 - NO
C            = 1 - YES
C     NPAGE  = NUMBER OF NEXT PLOT - PLOTTED IN LOWER LEFT HAND CORNER
C            = 0              - DO NOT PLOT
C            = GREATER THAN 0 - PLOT AND INCREMENT
C     NREST  = FLAG TO SHOW ENERGY LIMITS OF RESONANCE REGIONS
C            = 0 - NO
C            = 1 - YES
C     NRATIO = FLAG TO SHOW POSITION OF MINIMUM AND MAXIMUM RATIOS
C            = 0 - NO
C            = 1 - YES
C              SEE XMIN AND XMAX BELOW ON HOW TO DEFINE MIN/MAX RATIOS
C     NUNITY = FLAG TO SHOW 1.0 AND ALLOWABLE UNCERTAINTY ON RATIO PLOT
C            = 0 - NO
C            = 1 - YES
C     NACTV  = FLAG FOR INTERACTIVE INPUT
C            = 0 - NO INTERACTIVE INPUT ALLOWED
C            = 1 - INTERACTIVE INPUT ALLOWED
C     NCOMP  = FLAG TO SHOW COMPOSITION MODE = NO FRAME ADVANCE AT END
C                                              OF PLOT
C            = 0 - NO
C            = 1 - YES
C     NROUND = LOWER AND UPPER X AND Y LIMIT ROUNDING FLAGS
C            = 0 - ROUND LIMIT - TO AVOID OVERLAP WITH GRID AT BORDER
C            = 1 - DO NOT ROUND
C     NERR   = X AND Y ERROR BAR FLAGS FOR SETS OF POINTS
C            = 0 - DO NOT PLOT ERROR BARS
C            = 1 - PLOT ERROR BARS
C     NOBOX  = FLAG TO INCLUDE LEGEND BOX ON EACH SUBPLOT OF A PLOT
C            = 0 - INCLUDE LEGEND BOX TO IDENTIFY DATA
C            = 1 - NO LEGEND BOX
C     NXPLAN = X AXIS PLOT SCALE (LINEAR OR LOG)
C            = 0 - CODE SELECTS LOG OR LINEAR SCALING
C            = 1 - LINEAR SCALING
C            = 2 - LOG SCALING
C     NYPLAN = Y AXIS PLOT SCALE (LINEAR OR LOG)
C            = 0 - CODE SELECTS LOG OR LINEAR SCALING
C            = 1 - LINEAR SCALING
C            = 2 - LOG SCALING
C     NXTERP = X AXIS INTERPOLATION BETWEEN TABULATED POINTS
C            = 1 - LINEAR INTERPOLATION
C            = 2 - LOG INTERPOLATION
C     NYTERP = Y AXIS INTERPOLATION BETWEEN TABULATED POINTS
C            = 1 - LINEAR INTERPOLATION
C            = 2 - LOG INTERPOLATION
C     NXLIMS = LOWER AND UPPER X FLAGS
C            = 0 - IGNOR LIMIT IN XZOOM/YZOOM - CALCULATE BASED ON DATA
C            = 1 - USE LIMIT IN XZOOM/YZOOM
C     NYLIMS = LOWER AND UPPER X FLAGS
C            = 0 - IGNOR LIMIT IN XZOOM/YZOOM - CALCULATE BASED ON DATA
C            = 1 - USE LIMIT IN XZOOM/YZOOM
C     MINSOK = ALLOW NEGATIVE Y RANGES
C            = 0 - NO - ONLY PLOT POSITIVE Y RANGES
C            = 1 - YES - ALLOW ANY Y RANGE
C     ------------------------------------------------------------------
C  3  MYCUR  = INDICES TO EACH CURVE TO APPEAR ON EACH SUBPLOT
C     MYPT   = INDICES TO EACH SET OF POINTS TO APPEAR ON EACH SUBPLOT
C     IMCUR  = NUMBER OF CURVES TO APPEAR ON EACH SUBPLOT
C     IMPT   = NUMBER OF SETS OF POINTS TO APPEAR ON EACH SUBPLOT
C     NFRAME = NUMBER OF SUBPLOTS PER PLOT
C     LFRAME = INDEX TO SUBPLOT CURRENTLY BEING PLOTTED
C     LFRAMR = INDEX TO SUBPLOT CONTAINING RATIO DATA
C     ------------------------------------------------------------------
C  4  XLABEL = X AXIS LABEL
C     XUNITS = X AXIS UNITS, E.G., MEV
C     YLABEL = Y AXIS LABEL
C     YUNITS = Y AXIS UNITS, E.G., BARNS
C     ------------------------------------------------------------------
C  5  TIT1L  = UPPER-LEFT    CORNER TITLE LINE - AT TOP OF PLOT
C     TIT1M  = UPPER-MIDDLE  CORNER TITLE LINE - AT TOP OF PLOT
C     TIT1R  = UPPER-RIGHT   CORNER TITLE LINE - AT TOP OF PLOT
C     TIT2L  = LOWER-LEFT    CORNER TITLE LINE - AT TOP OF PLOT
C     TIT2M  = LOWER-MIDDLE  CORNER TITLE LINE - AT TOP OF PLOT
C     TIT2R  = LOWER-RIGHT   CORNER TITLE LINE - AT TOP OF PLOT
C     FOOTL  = BOTTOM-LEFT   CORNER TITLE LINE - AT BOTTOM OF PLOT
C     FOOTM  = BOTTOM-MIDDLE CORNER TITLE LINE - AT BOTTOM OF PLOT
C     FOOTR  = BOTTOM-RIGHT  CORNER TITLE LINE - AT BOTTOM OF PLOT
C     ------------------------------------------------------------------
C  6  TITLES = TITLE FOR EACH CURVE OR SET OF POINTS
C     ------------------------------------------------------------------
C  7  XZOOM  = LOWER AND UPPER X LIMITS OF PLOT
C     YZOOM  = LOWER AND UPPER Y LIMITS OF EACH SUBPLOT
C     IUSE1  = LOWER INDEX TO POINTS FOR EACH CURVE OR SET OF POINTS
C     IUSE2  = UPPER INDEX TO POINTS FOR EACH CURVE OR SET OF POINTS
C     ------------------------------------------------------------------
C  8  XMIN   = ENERGY OF MINIMUM RATIO OVER CURRENT ENERGY RANGE
C     XMAX   = ENERGY OF MAXIMUM RATIO OVER CURRENT ENERGY RANGE
C     DIFMIN = MINIMUM DIFFERENCE (AT XMIN) - PER-CENT
C     DIFMAX = MAXIMUM DIFFERENCE (AT XMAX) - PER-CENT
C     RLIMIT = LOWER AND UPPER ENERGY LIMITS OF RESOLVED AND
C              UNRESOLVED RESONANCE REGIONS
C              RLIMIT(I,K) - I = 1 - LOWER   , 2 - UPPER
C                            K = 1 - RESOLVED, 2 - UNRESOLVED
C
C     THIS COMMON BLOCK IS ONLY USED IF A GPP OPTION IS TURNED ON
C     NRATIO - TO INDICATE POSITION AND MAGNITUDE OF RATIO DIFFERENCES
C     NREST  - TO INDICATE RESOLVED AND UNRESOLVED ENERGY LIMITS
C
C     IF YOU TURN ON THESE OPTIONS YOU MUST DEFINE THE REQUIRED
C     VARIABLES BEFORE MAKING A PLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 BLANK,XLABEL,XUNITS,YLABEL,YUNITS,TITLES,
     1 TIT1L,TIT1M,TIT1R,TIT2L,TIT2M,TIT2R,FOOTL,FOOTM,FOOTR
      COMMON/GPP0/NPRINT,NPLOT,NCHR,NSYM
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP4/XLABEL(40),XUNITS(40),YLABEL(40),YUNITS(40)
      COMMON/GPP5/TIT1L(80),TIT1M(80),TIT1R(80),TIT2L(80),TIT2M(80),
     1 TIT2R(80),FOOTL(80),FOOTM(80),FOOTR(80)
      COMMON/GPP6/TITLES(84,30)
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      DATA BLANK/' '/
C-----DEFINE ALL I/O UNITS.
      NPRINT=6
      NPLOT=16
      NCHR=17
      NSYM=18
C-----INITIALIZE TO USER DEFINE PROGRAM.
      MYPROG=0
C-----INITIALIZE TO STANDARD PLOT - ONE PLOT PER FRAME - ALL DATA ON
C-----THE SAME SUBPLOT.
      MODPLT=2
      XYPLOT(1,1)=0.0d0
      XYPLOT(2,1)=13.5d0
      XYPLOT(1,2)=0.0d0
      XYPLOT(2,2)=10.24d0
      NSMALX=1
      NSMALY=1
      NFRAME=1
      LFRAME=1
C-----NO RATIO PLOT.
      LFRAMR=0
C-----GRID WILL BE TICK MARKS ON BORDER.
      NGRID=0
      IMFINE(1)=0
      IMFINE(2)=0
C-----DO NOT IDENTIFY POINTS ON CURVES.
      NPOINT=0
C-----X HORIZONTAL/ Y VERTICAL, NO BORDER, NO PLOT NUMBER.
      NFLIP=0
      NBORD=0
      NPAGE=0
C-----DO NOT PLOT ENERGY LIMITS OF RESONANCE REGIONS.
      NREST=0
C-----DO NOT SHOW POSITION OF MINIMUM AND MAXIMUM RATIOS.
      NRATIO=0
C-----DO NOT SHOW 1.0 AND ALLOWABLE UNCERTAINTY.
      NUNITY=0
C-----TURN OFF INTERACTIVE INPUT MODE.
      NACTV=0
C-----TURN OFF COMPOSITION MODE
      NCOMP=0
C-----NO LINE THICKNESS
      NTHICK=0
      ITHICK=0
      THICK=0.00d0
      IMTHIN=0
      MTHICK(1)=0
      MTHICK(2)=0
      MTHICK(3)=0
C-----NO CURVES, NO PLOTS, LEGEND BOX ON ALL SUBPLOTS
      DO 20 I=1,5
      IMCUR(I)=0
      IMPT(I)=0
      NOBOX(I)=0
      DO 10 K=1,30
      MYCUR(K,I)=0
   10 MYPT(K,I)=0
   20 CONTINUE
C-----NO X OR Y ERROR BARS, ROUND X AND Y LIMITS.
      DO 30 I=1,2
      NERR(I)=0
      DO 30 J=1,2
      NROUND(I,J)=0
   30 CONTINUE
C-----PROGRAM SELECTS X/Y AXIS SCALE, INTERPOLATION IS LINEAR X AND Y.
      NXPLAN=0
      NXTERP=1
      DO 40 I=1,5
      NYPLAN(I)=0
   40 NYTERP(I)=1
C-----ALL LIMITS ARE REAL.
      NXLIMS(1)=1
      NXLIMS(2)=NXLIMS(1)
      DO 50 I=1,5
      NYLIMS(1,I)=NXLIMS(1)
   50 NYLIMS(2,I)=NYLIMS(1,I)
C-----BLANK ALL TEXT ARRAYS.
      DO 60 J=1,30
      DO 60 I=1,84
   60 TITLES(I,J)=BLANK
      DO 70 I=1,40
      XLABEL(I)=BLANK
      XUNITS(I)=BLANK
      YLABEL(I)=BLANK
   70 YUNITS(I)=BLANK
      DO 80 I=1,80
      TIT1L(I)=BLANK
      TIT1M(I)=BLANK
      TIT1R(I)=BLANK
      TIT2L(I)=BLANK
      TIT2M(I)=BLANK
      TIT2R(I)=BLANK
      FOOTL(I)=BLANK
      FOOTM(I)=BLANK
   80 FOOTR(I)=BLANK
      RETURN
      END
      SUBROUTINE GPPLOT(IMACTV,MYPATH)
C=======================================================================
C
C     GENERAL PLOTTING PACKAGE (GPP) PLOTTING INTERFACE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP7/XZOOM(2),YZOOM(2,5),IUSE1(60),IUSE2(60)
      COMMON/GPP26/XYDATA(2,2)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      COMMON/GPP30/STEPER(7,5),LINTYP
      DATA IPASS/0/
c-----------------------------------------------------------------------
C
C     INITIALIZE BEFORE FIRST PLOT.
C
c-----------------------------------------------------------------------
      IF(IPASS.NE.0) GO TO 10
      IPASS=1
C-----INITIALIZE VARIABLES.
      CALL SET1
C-----LOAD SOFTWARE SYMBOL TABLE.
      CALL CHRIN
C-----LOAD SYMBOLS FOR POINT DATA AND LINE TYPES.
      CALL SYMIN
C-----INITIALIZE PLOTTER.
      CALL FRAME1
C-----RETURN IF ONLY INITIALIZING
      IF(MYPATH.LT.0) RETURN
C-----DEFINE LAYOUT OF A PLOT.
   10 CALL SET2
C-----FILL AREA.
      CALL COLORBOX(XYGRID(1,1),XYGRID(1,2),0,-1)
C-----ASSIGN CURVES AND SETS OF POINTS TO EACH SUBPLOT.
      CALL SETMAP
c-----------------------------------------------------------------------
C
C     INITIALIZE INTERACTIVE PLOT REQUEST FLAG OFF.
C
c-----------------------------------------------------------------------
      IMACTV=0
c-----------------------------------------------------------------------
C
C     DEFINE POSITION AND SIZE OF LEGEND BOX FOR EACH SUBPLOT AND
C     PLOT ALL OF THEM.
C
c-----------------------------------------------------------------------
      CALL BOXPLT
c-----------------------------------------------------------------------
C
C     DRAW BORDER OF FRAME, MESSAGE BOX AND EACH SUB-PLOT.
C
c-----------------------------------------------------------------------
      CALL BORDER
c-----------------------------------------------------------------------
C
C     DEFINE X LIMITS OF PLOT IN INCHES (SAME FOR ALL SUBPLOTS).
C
c-----------------------------------------------------------------------
      XINCH1=XYINCH(1,1,1)
      XINCH2=XYINCH(2,1,1)
c-----------------------------------------------------------------------
C
C     DEFINE EVERYTHING FOR THE X SCALING ONLY ONCE (SAME FOR ALL
C     SUBPLOTS).
C
c-----------------------------------------------------------------------
C-----DEFINE X LIMITS OF PLOT IN DATA UNITS.
      XYDATA(1,1)=XZOOM(1)
      XYDATA(2,1)=XZOOM(2)
C-----CHECK X LIMITS AND SELECT LINEAR OR LOG SCALING.
      LFRAME=1
      CALL CHEKOV(1)
C-----EXTEND X LIMITS TO AVOID OVERLAP WITH GRID.
      CALL WINDXY(1)
C-----DEFINE X SCALING INFORMATION (SAME FOR ALL SUBPLOTS).
      CALL SCALXY(1)
C-----INDICATE RESOLVED AND UNRESOLVED REGIONS.
      CALL REGION
C-----ADD ARROWS TO INDICATE ENERGY OF MAXIMUM AND MINIMUM DIFFERENCES.
      CALL ARROWS
C-----DEFINE X AXIS LABEL.
      CALL LABLXY(1)
c-----------------------------------------------------------------------
C
C     PLOT IDENTIFICATION INFORMATION.
C
c-----------------------------------------------------------------------
      CALL LABLIT
c-----------------------------------------------------------------------
C
C     LOOP OVER SUB-PLOTS PER FRAME.
C
c-----------------------------------------------------------------------
      DO 50 LFRAME=1,NFRAME
C-----INITIALIZE LINE TYPE TO SOLID.
      LINTYP=1
c-----------------------------------------------------------------------
C
C     DEFINE Y LIMITS OF PLOT IN INCHES.
C
c-----------------------------------------------------------------------
      YINCH1=XYINCH(1,2,LFRAME)
      YINCH2=XYINCH(2,2,LFRAME)
c-----------------------------------------------------------------------
C
C     DEFINE EVERYTHING FOR THE Y SCALING FOR EACH SUBPLOT.
C
c-----------------------------------------------------------------------
C-----DEFINE Y LIMITS OF PLOT IN DATA UNITS.
      LCURVE=MYCUR(1,LFRAME)
      XYDATA(1,2)=YZOOM(1,LCURVE)
      XYDATA(2,2)=YZOOM(2,LCURVE)
C-----POSITION AND DRAW LEGEND BOX FOR SUBPLOT.
      CALL BOXSUB
C-----CHECK Y LIMITS AND SELECT LINEAR OR LOG SCALING.
      CALL CHEKOV(2)
C-----EXTEND Y LIMITS TO AVOID OVERLAP WITH GRID.
      CALL WINDXY(2)
C-----DEFINE Y SCALING INFORMATION.
      CALL SCALXY(2)
C-----DEFINE Y AXIS LABEL.
      CALL LABLXY(2)
C-----DEFINE INTERPOLATION OF DATA VS. PLANE OF PLOT.
      CALL TERPXY
c-----------------------------------------------------------------------
C
C     PLOT GRID.
C
c-----------------------------------------------------------------------
      CALL GRID
c-----------------------------------------------------------------------
C
C     PLOT CURVES.
C
c-----------------------------------------------------------------------
      KCURVE=IMCUR(LFRAME)
      IF(KCURVE.LE.0) GO TO 30
      DO 20 MCURVE=1,KCURVE
      LCURVE=MYCUR(MCURVE,LFRAME)
      IF(LCURVE.LE.0) GO TO 20
C-----INITIALIZE LINE TYPE TO SOLID.
      LINTYP=1
C-----IF BOTH CROSS SECTIONS ON SAME SUB-PLOT DEFINE LINE TYPE.
      IF(MCURVE.GT.1) LINTYP=MCURVE
      CALL PEN(LCURVE+2)
C-----IDENTIFY CURVE IN LEGEND BOX.
      CALL BOXLAB(LCURVE,1)
C-----PLOT CONTINUOUS CURVE.
      CALL LINER(IUSE1(LCURVE),IUSE2(LCURVE),LCURVE)
C-----END OF CURVE LOOP.
   20 CONTINUE
c-----------------------------------------------------------------------
C
C     PLOT SETS OF POINTS.
C
c-----------------------------------------------------------------------
   30 KSET=IMPT(LFRAME)
      IF(KSET.LE.0) GO TO 50
C-----SYMBOLS ARE ALWAYS SOLID.
      LINTYP=1
      DO 40 MSET=1,KSET
      LSET=MYPT(MSET,LFRAME)
      IF(LSET.LE.0) GO TO 40
      CALL PEN(LSET+2)
      CALL BOXLAB(LSET,2)
      CALL SETTER(IUSE1(LSET),IUSE2(LSET),LSET)
   40 CONTINUE
C-----END OF SUB-PLOT LOOP
   50 CONTINUE
c-----------------------------------------------------------------------
C
C     ADDITIONAL THINGS ON PLOT.
C
c-----------------------------------------------------------------------
      CONTINUE
      LFRAME=NFRAME
C-----IF PLOTTING RATIOS ADD CENTER LINE DASHED LINE AT RATIO OF UNITY.
      IF(LFRAMR.NE.0) CALL UNITY
c-----------------------------------------------------------------------
C
C     CHECK FOR INTERACTIVE INPUT, IF INTERACTION OPTION IS ON.
C
c-----------------------------------------------------------------------
      IMACTV=0
      IF(NACTV.LE.0) GO TO 60
      CALL ACTION(IMACTV,XACT1,XACT2)
      IF(IMACTV.LE.0) GO TO 60
C-----SET X LIMITS FOR INTERACTIVE PLOT REQUEST.
      XZOOM(1)=XACT1
      XZOOM(2)=XACT2
   60 IF(NCOMP.LE.0) CALL FRAMED(0)
      RETURN
      END
      SUBROUTINE TERPXY
C=======================================================================
C
C     DEFINE INTERPOLATION OF DATA VS. PLANE OF PLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
C-----INTERPOLATION OF DATA.
      NTDATA=2
      IF(NXTERP.EQ.2) NTDATA=3
      IF(NYTERP(LFRAME).EQ.2) NTDATA=NTDATA+2
C-----PLANE OF PLOT.
      NTPLAN=2
      IF(IXYWAY(1).EQ.2) NTPLAN=3
      IF(IXYWAY(2).EQ.2) NTPLAN=NTPLAN+2
C-----SET FLAG TO INDICATE IF INTERPOLATION OF DATA IS THE SAME AS THE
C-----PLANE OF THE PLOT = NO INTERPOLATION TO PLOT REQUIRED.
      IMTERP=1
      IF(NTDATA.EQ.NTPLAN) IMTERP=0
      RETURN
      END
      SUBROUTINE ACTION(IMACTV,XI1,XI2)
C=======================================================================
C
C     CHECK FOR MOUSE INPUT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 XI1S,YI1S,XI2S,YI2S
c-----2017/3/31 - Added for NXPLAN
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      COMMON/GPP30/STEPER(7,5),LINTYP
      DATA TEN/1.0d1/
C-----INITIALIZE ALL PARAMETERS TO NO MOUSE INPUT.
      IMACTV=0
      XI1S=0.0d0
      YI1S=0.0d0
      CALL MOUSEY(IWAY,XI1S,YI1S,1,4)
      XI1=XI1S
      YI1=YI1S
C-----RETURN IF KEYBOARD INPUT.
      IF(IWAY.EQ.4) GO TO 30
C-----TRANSLATE TO INTERNAL STANDARD UNITS.
      CALL VIEWXY(XI1,YI1)
C-----RETURN IF POSITION IS OFF THE PLOT.
      IF(XI1.LT.XYGRID(1,1).OR.XI1.GT.XYGRID(2,1)) GO TO 30
      IF(YI1.LT.XYGRID(1,2).OR.YI1.GT.XYGRID(2,2)) GO TO 30
C-----XI1 IS WITHIN THE X INCHES OF PLOT. USE IT AS FIRST HALF OF
C-----ZOOM COMMAND. READ SECOND HALF OF ZOOM COMMAND.
      LINTYP=1
      CALL DASHV(XI1)
      XI2S=0.0d0
      YI2S=0.0d0
      CALL MOUSEY(IWAY,XI2S,YI2S,1,4)
      XI2=XI2S
      YI2=YI2S
      IF(IWAY.EQ.4) GO TO 30
C-----TRANSLATE TO INTERNAL STANDARD UNITS.
      CALL VIEWXY(XI2,YI2)
C-----RETURN IF POSITION IS OFF THE PLOT.
      IF(XI2.LT.XYGRID(1,1).OR.XI2.GT.XYGRID(2,1)) GO TO 30
      IF(YI2.LT.XYGRID(1,2).OR.YI2.GT.XYGRID(2,2)) GO TO 30
      CALL DASHV(XI2)
C-----SORT INTO ASCENDING ORDER.
      IF(XI1.lt.XI2) go to 10
      IF(XI1.eq.XI2) go to 30       ! Ignore if no X range
      XI3=XI1
      XI1=XI2
      XI2=XI3
C-----INDICATE ZOOMED LIMITS DEFINED.
   10 IMACTV=1
C-----2017/3/31 - TURN OFF FORCED X LINEAR SCALING
      NXPLAN = 0
C-----CONVERT FROM INCHES TO DATA UNITS.
      DXINCH=XINCH2-XINCH1
      XI1=(XYPLAN(2,1)*(XI1-XINCH1)+XYPLAN(1,1)*(XINCH2-XI1))/DXINCH
      XI2=(XYPLAN(2,1)*(XI2-XINCH1)+XYPLAN(1,1)*(XINCH2-XI2))/DXINCH
      IF(IXYWAY(1).EQ.1) GO TO 20
c-----convert log to linear
      XI1=TEN**XI1
      XI2=TEN**XI2
c-----2017/3/31 - do not go below 1.0d-5 eV
   20 if(XI1.lt.1.0d-5) XI1 = 1.0d-5
   30 CONTINUE
      RETURN
      END
      SUBROUTINE VIEWXY(XIN,YIN)
C=======================================================================
C
C     TRANSLATE (XIN,YIN) FROM ABSOLUTE COORDINATES OF SCREEN, AS
C     SUPPLIED BY MOUSE, TO INTERNAL STANDARD UNITS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP31/XOFF,YOFF,XSCALE,YSCALE,XSIZE,YSIZE,XBASIS
      IF(NFLIP.NE.0) GO TO 10
C-----X HORIZONTAL/Y VERTICAL.
      XIN=(XIN-XOFF)/XSCALE
      YIN=(YIN-YOFF)/YSCALE
      GO TO 20
C-----Y HORIZONTAL/X VERTICAL.
   10 XYN=(XIN-YOFF)/YSCALE
      XIN=(YIN-XOFF)/XSCALE
      YIN=XYN
   20 RETURN
      END
      SUBROUTINE BORDER
C=======================================================================
C
C     DRAW BORDER OF FRAME, MESSAGE BOX AND EACH SUB-PLOT.
C
C     WARNING...THIS ASSUMES MESSAGE BOX IS AT TOP OF PLOT TOUCHING
C     THE FIRST SUB-PLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      CALL PEN(2)
C-----IF REQUESTED DRAW OUTLINE FOR ENTIRE PLOTTING AREA.
      IF(NBORD.LE.0) GO TO 10
      ITHICK=MTHICK(3)
      CALL PLOT4(XYEDGE(1,1),XYEDGE(1,2),3)
      CALL PLOT4(XYEDGE(1,1),XYEDGE(2,2),2)
      CALL PLOT4(XYEDGE(2,1),XYEDGE(2,2),2)
      CALL PLOT4(XYEDGE(2,1),XYEDGE(1,2),2)
      CALL PLOT4(XYEDGE(1,1),XYEDGE(1,2),2)
      ITHICK=MTHICK(2)
C-----TOP OF MESSAGE BOX (IF ANY).
   10 IF(XYINCH(2,2,1).GE.YL(7)) GO TO 20
      CALL PLOT4(XYINCH(1,1,1),YL(7),3)
      CALL PLOT4(XYINCH(2,1,1),YL(7),2)
C-----TOP OF EACH SUB-PLOT.
   20 DO 30 L=1,NFRAME
      CALL PLOT4(XYINCH(1,1,1),XYINCH(2,2,L),3)
   30 CALL PLOT4(XYINCH(2,1,1),XYINCH(2,2,L),2)
C-----BOTTOM OF LAST SUB-PLOT.
      CALL PLOT4(XYINCH(1,1,1),XYINCH(1,2,NFRAME),3)
      CALL PLOT4(XYINCH(2,1,1),XYINCH(1,2,NFRAME),2)
C-----LEFT AND RIGHT SIDE.
      CALL PLOT4(XYINCH(1,1,1),XYINCH(1,2,NFRAME),3)
      CALL PLOT4(XYINCH(1,1,1),YL(7),2)
      CALL PLOT4(XYINCH(2,1,1),XYINCH(1,2,NFRAME),3)
      CALL PLOT4(XYINCH(2,1,1),YL(7),2)
      RETURN
      END
      SUBROUTINE SET1
C=======================================================================
C
C     INITIALIZE GENERAL GRAPHICS PACKAGE (GGP) PARAMETERS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      COMMON/GPP30/STEPER(7,5),LINTYP
      COMMON/MULTHT/HTMULT
      DIMENSION STEPX(7,5)
C-----DEFINE FIVE DIFFERENT LINE TYPES.
      DATA STEPX/
     1 1.0,6*0.0,
     2 0.06,0.04,5*0.0,
     3 0.33,0.04,0.06,0.04,3*0.0,
     4 0.226666666,0.04,5*0.0,
     5 0.26,0.04,0.06,0.04,0.06,0.04,0.0/
C-----DEFINE RESOLUTION OF PLOTTING SURFACE.
      DXYMIN=0.01d0
C-----DEFINE FIVE DIFFERENT LINE TYPES.
      DO 10 I=1,5
      DO 10 J=1,7
   10 STEPER(J,I)=STEPX(J,I)
c-----------------------------------------------------------------------
C
C     DEFINE DIMENSIONS OF STANDARD PLOT.
C
c-----------------------------------------------------------------------
C-----DEFINE SIZE OF PLOTTING AREA.
      XYEDGE(1,1)=0.0d0
      XYEDGE(2,1)=13.5d0
      XYEDGE(1,2)=0.0d0
      XYEDGE(2,2)=10.24d0
C-----DEFINE GRID TYPE.
      KGRID=NGRID
      IF(KGRID.LT.0.OR.NGRID.GT.5) KGRID=0
      IMFINE(1)=0
      IMFINE(2)=0
      IF(KGRID.LE.2) IMFINE(1)=KGRID
      IF(KGRID.EQ.3.OR.KGRID.EQ.5) IMFINE(1)=1
      IF(KGRID.EQ.4) IMFINE(1)=2
      IF(KGRID.EQ.3) IMFINE(2)=1
      IF(KGRID.GT.3) IMFINE(2)=2
C-----DEFINE LINE THICKNESS.
      THICK=0.002d0
      ITHICK=NTHICK
      IMTHIN=0
      IF(ITHICK.GE.0) GO TO 20
      ITHICK=-ITHICK
      IMTHIN=1
   20 IF(ITHICK.GT.5) ITHICK=5
      MTHICK(1)=0
      MTHICK(2)=ITHICK
      MTHICK(3)=2*ITHICK
C-----INITIALIZE LEGEND BOX OFF THE PLOT.
      BIXMIN=-10.0d0
      BIXMAX=BIXMIN
      BIYMIN=BIXMIN
      BIYMAX=BIXMIN
C-----DEFINE HEIGHT OF CHARACTER IN INCHES.
      HT=0.10d0*HTMULT
C-----DEFINE WIDTH OF CHARACTER IN INCHES.
      WD=HT
C-----DEFINE MULTIPLES OF CHARACTER WIDTHS AND HEIGHTS FOR LATER
C-----POSITIONING CALCULATIONS.
      HTX1  =HT/1.4
      HTX2  =0.5d0*HTX1
      HT0P5 =0.5d0*HT
      HT1P75=1.75d0*HT
C-----DEFINE BASIC LENGTH OF TICK MARKS FOR GRID.
      TICK0 =2.0d0*DXYMIN
c-----------------------------------------------------------------------
C
C     DEFINE POSITION OF TITLE AND AXIS LABELS.
C
C     PLOTTING AREA HAS A BLANK BORDER OF 3 CHARACTERS.
C
C     SPACING FROM TOP.
C
c-----------------------------------------------------------------------
C-----DEFINE Y POSITION OF TWO TITLE LINES AT TOP OF PLOT.
      YL(1)=XYEDGE(2,2)-4.0d0*HT
      YL(2)=YL(1)-HT1P75
C-----DEFINE Y POSITION OF MESSAGE BOX AND LINES WITHIN MESSAGE BOX
C-----BELOW PLOT.
C----- 7 AND 12 - TOP AND BOTTOM OF BOX
C----- 8        - RESOLVED/UNRESOLVED MESSAGE
C----- 9        - RESOLVED/UNRESOLVED LINES
C-----10        - MIN/MAX MESSAGE
C-----11        - RATIO MESSAGE
      YL(7)=YL(2)-0.75d0*HT
C-----IF OPTIONS TO SHOW RESONANCE REGION AND MINIMUM AND MAXIMUM
C-----RATIOS ARE BOTH OFF DO NOT LEAVE ROOM FOR BOX AT TOP OF PLOT.
      IF(NREST.NE.0.OR.NRATIO.NE.0) GO TO 30
      YL(12)=YL(7)
      GO TO 40
   30 YL( 8)=YL(7)-1.5d0*HTX1
      YL( 9)=YL(8)-0.5d0*HTX1
      YL(10)=YL(9)-1.75d0*HTX1
C-----IF OPTION IS NOT ON TO SHOW LIMITS OF RESONANCE REGION DO NOT
C-----ALLOW SPACE FOR MESSAGE.
      IF(NREST.EQ.0) YL(10)=YL(7)-1.5d0*HTX1
      YL(11)=YL(10)-1.75d0*HTX1
      YL(12)=YL(11)-0.5d0*HTX1
C-----IF OPTION IS NOT ON TO SHOW MINIMUM AND MAXIMUM RATIO DO NOT
C-----ALLOW SPACE FOR MESSAGE.
      IF(NRATIO.EQ.0) YL(12)=YL(9)-HTX1
C-----TOP OF PLOTTING AREA.
   40 XYGRID(2,2)=YL(12)
c-----------------------------------------------------------------------
C
C     SPACING FROM BOTTOM.
C
c-----------------------------------------------------------------------
C-----DEFINE Y POSITION OF X AXIS LABEL.
      YL(5)=XYEDGE(1,2)+3.0d0*HT
C-----DEFINE Y POSITION OF X AXIS LOG 10.
      YL(4)=YL(5)+2.75*HT
C-----DEFINE Y POSITION OF X AXIS LINEAR SCALE AND LOG POWERS.
      YL(3)=YL(4)+HT
      XYGRID(1,2)=YL(3)+HT1P75
C-----DEFINE Y POSITION OF X AXIS FINE GRID DIGITS
      YL(6)=XYGRID(1,2)-1.5*HTX1
c-----------------------------------------------------------------------
C
C     SPACING FROM LEFT.
C
c-----------------------------------------------------------------------
C-----DEFINE X POSITION OF Y AXIS LABEL.
      XL(3)=XYEDGE(1,1)+4.0d0*WD
C-----DEFINE X POSITION OF Y AXIS SCALE.
      XL(1)=XL(3)
C-----DEFINE X POSITION OF Y AXIS 10** SCALE FACTOR.
      XL(2)=XL(3)+HT1P75
      XYGRID(1,1)=XL(2)+5.0d0*HT
C-----DEFINE X POSITION OF Y AXIS FINE GRID DIGITS
      XL(4)=XYGRID(1,1)-1.5*HTX1
c-----------------------------------------------------------------------
C
C     SPACING FROM RIGHT.
C
c-----------------------------------------------------------------------
      XYGRID(2,1)=XYEDGE(2,1)-3.0d0*HT
C-----INITIALIZE LINE TYPE TO SOLID LINE.
      LINTYP=1
C-----INITIALIZE FLAG TO ALLOW PLOTTING OF NEGATIVE DATA.
      MINSOK=1
c-----------------------------------------------------------------------
C
C     DEFINE SPACING TO PREVENT OVERLAP OF DATA AND GRID AT BORDER.
C
c-----------------------------------------------------------------------
C-----DEFINE STANDARD DISTANCE.
      DELTX1=14.0d0*DXYMIN
      DELTX2=DELTX1
      DELTY1=DELTX1
      DELTY2=DELTX1
C-----IF REQUESTED TURN OFF ROUNDING.
      IF(NROUND(1,1).GT.0) DELTX1=0.0d0
      IF(NROUND(2,1).GT.0) DELTX2=0.0d0
      IF(NROUND(1,2).GT.0) DELTY1=0.0d0
      IF(NROUND(2,2).GT.0) DELTY2=0.0d0
C-----DEFINE FOR EACH SUBPLOT.
      DO 50 L=1,5
      SPACER(1,1,L)=DELTX1
      SPACER(2,1,L)=DELTX2
      SPACER(1,2,L)=DELTY1
   50 SPACER(2,2,L)=DELTY2
      RETURN
      END
      SUBROUTINE SET2
C=======================================================================
C
C     DEFINE THE LAYOUT OF A PLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      DATA IPASS/0/
      DATA LSTPLT/0/
c-----------------------------------------------------------------------
C
C     LAYOUT IS THE SAME FOR ALL PLOTS UNLESS THE PLOTTING MODE
C     CHANGES.
C
c-----------------------------------------------------------------------
      IF(IPASS.LE.0) GO TO 10
      IF(LSTPLT.EQ.MODPLT) RETURN
   10 IPASS=1
      LSTPLT=MODPLT
c-----------------------------------------------------------------------
C
C     DEFINE PLOTTING AREAS EITHER AS A SERIES OF SUB-PLOTS ONE ABOVE
C     THE OTHER, ALL SUB-PLOTTING AREAS TO BE THE SAME OR A COMBINATION.
C
c-----------------------------------------------------------------------
      MODP1=MODPLT+1
      DGRID=(XYGRID(2,2)-XYGRID(1,2))
      GO TO (20,30,40,50,60),MODP1
C-----CURVE 1 OVER RATIO TO ALL OTHER DATA (CURVE 1 TOP 2/3).
   20 NFRAME=2
      LFRAMR=2
      XYINCH(2,2,1)=XYGRID(2,2)
      XYINCH(1,2,1)=XYINCH(2,2,1)-2.0d0*DGRID/3.0d0
      XYINCH(2,2,2)=XYINCH(1,2,1)
      XYINCH(1,2,2)=XYGRID(1,2)
      GO TO 70
C-----CURVE 1 OVER ALL OTHER DATA - NO RATIO (EACH 1/2)
   30 NFRAME=2
      LFRAMR=0
      XYINCH(2,2,1)=XYGRID(2,2)
      XYINCH(1,2,1)=XYINCH(2,2,1)-0.5d0*DGRID
      XYINCH(2,2,2)=XYINCH(1,2,1)
      XYINCH(1,2,2)=XYGRID(1,2)
      GO TO 70
C-----ALL DATA ON THE SAME PLOT - NO RATIO.
   40 NFRAME=1
      LFRAMR=0
      XYINCH(2,2,1)=XYGRID(2,2)
      XYINCH(1,2,1)=XYGRID(1,2)
      GO TO 70
C-----CURVE 1 OVER ALL OTHER DATA OVER RATIO TO ALL DATA (EACH 1/3)
   50 NFRAME=3
      LFRAMR=3
      DGRID=DGRID/3.0d0
      XYINCH(2,2,1)=XYGRID(2,2)
      XYINCH(1,2,1)=XYINCH(2,2,1)-DGRID
      XYINCH(2,2,2)=XYINCH(1,2,1)
      XYINCH(1,2,2)=XYINCH(2,2,2)-DGRID
      XYINCH(2,2,3)=XYINCH(1,2,2)
      XYINCH(1,2,3)=XYGRID(1,2)
      GO TO 70
C-----ALL DATA OVER RATIO TO ALL OTHER DATA (DATA TOP 1/3)
   60 NFRAME=2
      LFRAMR=2
      XYINCH(2,2,1)=XYGRID(2,2)
      XYINCH(1,2,1)=XYINCH(2,2,1)-2.0d0*DGRID/3.0d0
      XYINCH(2,2,2)=XYINCH(1,2,1)
      XYINCH(1,2,2)=XYGRID(1,2)
C-----IN ALL CASES X LIMITS ARE THE SAME.
   70 DO 80 L=1,NFRAME
      XYINCH(1,1,L)=XYGRID(1,1)
   80 XYINCH(2,1,L)=XYGRID(2,1)
      RETURN
      END
      SUBROUTINE GRID
C=======================================================================
C
C     DRAW GRID ALONG ALL FOUR AXIS OF A PLOT. GRID IN X OR Y DIRECTION
C     MAY BE LINEAR OR LOG. GRID MARK SPACING IS AUTOMATICALLY COMPUTED
C     TO ALLOW VISUAL DISCRIMINATION TO WITHIN ABOUT 1 PER-CENT.
C
C     ARGUMENTS
C     ---------
C     NFRAME   = NUMBER OF SUB-FRAMES PER FRAME.
C     LFRAME   = SUB-FRAME INDEX
C
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 DIGIT
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP21/TICKS(100),ITICK(100),KTICK
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      DIMENSION Q(2),P1(3,2),P2(3,2),DIGIT(9),XYLIM(2,2)
      EQUIVALENCE(XINCH1,XYLIM(1,1))
      DATA DIGIT/'1','2','3','4','5','6','7','8','9'/
      DATA ZLOW/1.0d-6/
c-----------------------------------------------------------------------
C
C     SET UP LOOP OVER X AND Y DIRECTIONS.
C
c-----------------------------------------------------------------------
      DO 150 LROT=1,2
C-----DEFINE SPACING BETWEEN AXIS LABELS.
      DZBASE=DXYBAS(LROT)
C-----DEFINE THE GRID TICK LENGTH.
      TICK1=TICK0
      ZINCH=(VIRTIN(2,LROT)-VIRTIN(1,LROT))/FLOAT(IDEC(LROT))
C-----DEFINE SPACING BETWEEN AXIS LABELS SO THAT NO MORE THAN 1 EVERY
C-----1/2 INCH.
      KSPACE=0.5d0/ZINCH
      ZSPACE=KSPACE
      IF(ZINCH*ZSPACE.LT.0.5d0) KSPACE=KSPACE+1
C-----DEFINE INDEX TO ORTHOGONAL DIRECTION.
      LL=3-LROT
      ZQ(LL)=XYLIM(1,LL)
C-----DEFINE FINE AND ULTRA-FINE GRID STRUCTURE.
      CALL TICKER(IXYWAY(LROT),KSPACE,ZINCH,LROT)
c-----------------------------------------------------------------------
C
C     SET UP LOOP OVER LOWER AND UPPER AXIS.
C
c-----------------------------------------------------------------------
      DO 140 LOOP=1,2
C-----SKIP UPPER X AXIS EXCEPT ON FIRST SUBPLOT OF FRAME.
      IF(LROT.EQ.1.AND.LOOP.EQ.2.AND.LFRAME.NE.1) GO TO 140
      ZBASE=XYBASE(LROT)
      IF(DABS(ZBASE).LT.ZLOW) ZBASE=0.0d0
      TICK2=2.0d0*TICK1
      TICK3=4.0d0*TICK1
C-----DEFINE BASE LINE AND THREE TICK MARK OFFSET POSITIONS.
      Q(LL)=ZQ(LL)
      P1(1,LL)=ZQ(LL)+TICK1
      P1(2,LL)=ZQ(LL)+TICK2
      P1(3,LL)=ZQ(LL)+TICK3
C-----FOR LOWER X AXIS OF ALL SUBPLOTS EXCEPT THE LAST ONE DRAW LOWER
C-----X TICK MARKS OF CURRENT SUBPLOT AND UPPER X TICK MARKS OF NEXT
C-----SUBPLOT AT THE SAME TIME.
      IF(LROT.NE.1.OR.LOOP.NE.1.OR.LFRAME.EQ.NFRAME) GO TO 10
      P2(1,LL)=ZQ(LL)-TICK1
      P2(2,LL)=ZQ(LL)-TICK2
      P2(3,LL)=ZQ(LL)-TICK3
      GO TO 20
   10 P2(1,LL)=ZQ(LL)
      P2(2,LL)=ZQ(LL)
      P2(3,LL)=ZQ(LL)
   20 ZQ(LROT)=VIRTIN(1,LROT)
      IDECP1=IDEC(LROT)+1
c-----------------------------------------------------------------------
C
C     LOOP OVER COARSE INTERVALS.
C
c-----------------------------------------------------------------------
      DO 120 M=1,IDECP1
C-----ADD COARSE GRID AND AXIS LABEL IF WITHIN RANGE OF PLOT.
      IF(ZQ(LROT).lt.XYLIM(1,LROT)) go to 60
      IF(ZQ(LROT).GT.XYLIM(2,LROT)) GO TO 130
C-----CHECK LABEL SPACING.
      NBASE=(DABS(ZBASE)+ZLOW)/DZBASE
      NBASE=MOD(NBASE,KSPACE)
C-----PLOT AXIS LABEL. ONLY AT LOWER X OR Y AXIS.
      IF(LOOP.NE.1) GO TO 30
C-----X AXIS LABEL ONLY ON LAST PLOT.
      IF(LROT.EQ.1.AND.LFRAME.NE.NFRAME) GO TO 30
      IF(NBASE.EQ.0) CALL AXIS(LROT,IXYWAY(LROT))
C-----SELECT GRID TYPE.
   30 IF(IMFINE(1).LE.0) GO TO 50
      IF(LOOP.NE.1) GO TO 50
C-----SOLID OR DASH.
      IF(NBASE.EQ.0) GO TO 40
      IF(IMFINE(1).EQ.1) CALL GRIDZ(ZQ(LROT),LROT,2)
      GO TO 50
   40 CALL GRIDZ(ZQ(LROT),LROT,IMFINE(1))
C-----TICK MARKS.
   50 P1(3,LROT)=ZQ(LROT)
      P2(3,LROT)=ZQ(LROT)
      CALL PLOTP(P2(3,1),P2(3,2),3)
      CALL PLOTP(P1(3,1),P1(3,2),2)
   60 ZBASE=ZBASE+DZBASE
c-----------------------------------------------------------------------
C
C     LOOP OVER FINE AND ULTRAFINE GRID.
C
c-----------------------------------------------------------------------
      DO 110 K=1,KTICK
C-----LAST POINT WILL BE DRAWN AT BEGINNING OF NEXT COARSE INTERVAL.
      IF(K.EQ.KTICK) GO TO 110
      Q(LROT)=ZQ(LROT)+TICKS(K)
C-----DRAW IF WITHIN RANGE OF PLOT.
      IF(Q(LROT).LE.XYLIM(1,LROT)) GO TO 110
      IF(Q(LROT).GT.XYLIM(2,LROT)) GO TO 130
C-----SELECT GRID TYPE.
      IF(ITICK(K).NE.0) GO TO 70
C-----ULTRAFINE GRID.
      P1(1,LROT)=Q(LROT)
      P2(1,LROT)=Q(LROT)
      CALL PLOTP(P2(1,1),P2(1,2),3)
      CALL PLOTP(P1(1,1),P1(1,2),2)
      GO TO 110
C-----FINE GRID. ADD FINE DIGITS TO LOG DECADES.
   70 IF(ITICK(K).LE.1) GO TO 90
C-----ONLY FROM LOWER X OR Y AXIS.
      IF(LOOP.NE.1) GO TO 90
      IF(LROT.EQ.1.AND.LFRAME.NE.NFRAME) GO TO 90
      KK=ITICK(K)
      IF(LROT.EQ.2) GO TO 80
C-----X AXIS.
      QSS = Q(1)-HTX2
      CALL SYMBL1(QSS,YL(6),HTX1,DIGIT(KK),0.0,1)
      GO TO 90
C-----Y AXIS. DO NOT PLOT ABOVE TOP OF PLOT.
   80 YTOP=Q(2)+HTX1
      IF(YTOP.GT.YINCH2) GO TO 90
      CALL SYMBL1(XL(4),Q(2),HTX1,DIGIT(KK),0.0,1)
C-----FINE GRID. SELECT GRID TYPE.
   90 IF(IMFINE(2).LE.0) GO TO 100
      IF(LOOP.EQ.1) CALL GRIDZ(Q(LROT),LROT,IMFINE(2))
C-----TICK MARKS.
  100 P1(2,LROT)=Q(LROT)
      P2(2,LROT)=Q(LROT)
      CALL PLOTP(P2(2,1),P2(2,2),3)
      CALL PLOTP(P1(2,1),P1(2,2),2)
C-----END OF FINE/ULTRAFINE LOOP.
  110 CONTINUE
C-----END OF COARSE LOOP. ADVANCE ONE DECADE.
  120 ZQ(LROT)=ZQ(LROT)+ZINCH
C-----SET BASE LINE AT UPPER AXIS FOR NEXT PASS.
  130 ZQ(LL)=XYLIM(2,LL)
C-----END OF AXIS LOOP. REVERSE DIRECTION OF TICK MARKS FOR UPPER AXIS.
  140 TICK1=-TICK1
C-----END OF X/Y LOOP.
  150 CONTINUE
      RETURN
      END
      SUBROUTINE TICKER(IWAY,KSPACE,ZINCH,LROT)
C=======================================================================
C
C     DEFINE TICK MARK SPACING FOR PLOT GRID.
C
C     A TABLE OF TICK MARKS WILL BE SET UP FOR THE X OR Y AXIS USING
C     EITHER LINEAR OR LOG SPACING. THE NUMBER OF TICK MARKS IS COMPUTED
C     TO ALLOW VISUAL DISCRIMINATION OF THE DATA TO WITHIN 1 PER-CENT
C     ACCURACY.
C
C     ARGUMENTS
C     ---------
C     INPUT
C     -----
C     IWAY    = TYPE OF GRID (1 = LINEAR, 2 = LOG)
C     KSPACE  = COARSE INTERVALS PER AXIS LABEL
C     ZINCH   = LENGTH OF COARSE INTERVAL IN INCHES
C
C     OUTPUT
C     ------
C     KTICK   = NUMBER OF TICK MARKS PER COARSE INTERVAL
C     TICKS   = POSITION OF TICKS PER COARSE INTERVAL IN INCHES
C     ITICK   = TICK FLAGS
C             = 0 - ULTRAFINE
C             = 1 - FINE
C             = GREATER THAN 1 - FINE PLUS FINE LOG DECADE DIGIT
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP21/TICKS(100),ITICK(100),KTICK
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      DIMENSION KOKLIN(5),KOLOG(4),KOLIN(4),RLGTAB(10),MTICK(4,3),
     1 ILOG1(9,4,3),RLOG1(9,4,3)
C-----DEFINE THE ALLOWED NUMBER OF ULTRA-FINE GRID DIVISIONS PER FINE
C-----GRID DIVISION.
      DATA KOLIN/1,2,5,10/
      DATA KOKLIN/1,2,5,10,20/
C-----ALLOWABLE NUMBER OF FINE GRID LINES PER LOG DECADE.
      DATA KOLOG/1,2,5,9/
C-----STANDARD FINE LOG SPACINGS.
      DATA RLGTAB/
     1  1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0,
     2  6.0d0, 7.0d0, 8.0d0, 9.0d0,10.0d0/
c-----------------------------------------------------------------------
C
C     SPECIAL TABLES FOR SMALL NUMBER OF FINE LINE PER DECADE.
C
c-----------------------------------------------------------------------
C-----TOTAL NUMBER OF GRID LINES PER DECADE FOR COMBINATIONS OF FINE
C-----AND ULTRAFINE GRID CHOICES.
      DATA MTICK/
     1  1, 2, 5, 9,
     2  2, 5, 9, 0,
     3  5, 9, 0, 0/
C-----FINE OR ULTRAFINE FLAGS.
      DATA ILOG1/
     1     1,   0,   0,   0,   0,   0,   0,   0,   0,
     2     0,   1,   0,   0,   0,   0,   0,   0,   0,
     3     0,   0,   0,   0,   1,   0,   0,   0,   0,
     4     0,   0,   0,   0,   0,   0,   0,   0,   1,
     1     5,   1,   0,   0,   0,   0,   0,   0,   0,
     2     0,   0,   0,   0,   1,   0,   0,   0,   0,
     3     0,   0,   0,   1,   0,   0,   0,   0,   1,
     4     0,   0,   0,   0,   0,   0,   0,   0,   0,
     1     2,   4,   6,   8,   1,   0,   0,   0,   0,
     2     2,   0,   4,   0,   6,   0,   8,   0,   1,
     3     0,   0,   0,   0,   0,   0,   0,   0,   0,
     4     0,   0,   0,   0,   0,   0,   0,   0,   0/
C-----COORDINATES FOR FINE AND ULTRAFINE.
      DATA RLOG1/
     1  10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     2   5.0,10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     3   2.0, 4.0, 6.0, 8.0,10.0, 0.0, 0.0, 0.0, 0.0,
     4   2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0,10.0,
     1   5.0,10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     2   2.0, 4.0, 6.0, 8.0,10.0, 0.0, 0.0, 0.0, 0.0,
     3   2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0,10.0,
     4   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     1   2.0, 4.0, 6.0, 8.0,10.0, 0.0, 0.0, 0.0, 0.0,
     2   2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0,10.0,
     3   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     4   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
C-----NUMBER OF LINES PER 1/3 INCH (FINE GRID).
      DENSE1=3.33d0*ZINCH
C-----NUMBER OF LINES PER 1/10 INCH (ULTRAFINE GRID).
      DENSE2=10.0d0*ZINCH
C-----SELECT LINEAR OR LOG SCALING.
      GO TO (10,80),IWAY
c-----------------------------------------------------------------------
C
C     LINEAR SCALING.
C
c-----------------------------------------------------------------------
C-----LINEAR SCALING. DEFINE FINE GRID NOT CLOSER THAN 1/3 INCH.
   10 DO 20 K=1,4
      KFINE=KOLIN(K)
      IF(DENSE1.LT.KFINE) GO TO 30
   20 CONTINUE
C-----DEFINE ULTRA-FINE GRID NOT CLOSER THAN 1/10 INCH.
   30 DO 40 K=1,5
      KULTRA=KOKLIN(K)
      KTICK=KULTRA*KFINE
      IF(KTICK.GT.100) GO TO 50
      IF(DENSE2.LT.KTICK) GO TO 50
   40 CONTINUE
      K=6
   50 IF(K.GT.1) K=K-1
      KULTRA=KOKLIN(K)
C-----LINEAR TABLE. POINTS EQUALLY SPACED.
      KTICK=KULTRA*KFINE
      DTAB=ZINCH/FLOAT(KTICK)
      ZTAB=0.0d0
      K=0
      DO 70 I=1,KFINE
      DO 60 J=1,KULTRA
      K=K+1
      ITICK(K)=0
      ZTAB=ZTAB+DTAB
   60 TICKS(K)=ZTAB
   70 ITICK(K)=1
      GO TO 250
c-----------------------------------------------------------------------
C
C     LOG SCALING
C
c-----------------------------------------------------------------------
C-----LOG SCALING. DEFINE FINE GRID NOT CLOSER THAN 1/3 INCH ON AVERAGE.
   80 DO 90 KTAB=1,4
      KFINE=KOLOG(KTAB)
      IF(DENSE1.LT.KFINE) GO TO 100
   90 CONTINUE
      KTAB=4
  100 IF(KTAB.EQ.4) GO TO 140
c-----------------------------------------------------------------------
C
C     SPECIAL TREATMENT FOR SMALL NUMBER OF LINES PER LOG DECADE.
C     SELECT TOTAL NUMBER OF GRID DIVISIONS.
C
c-----------------------------------------------------------------------
      DO 110 K=1,4
      KTICK=MTICK(K,KTAB)
      IF(KTICK.LE.0) GO TO 120
      IF(DENSE2.LT.KTICK) GO TO 120
  110 CONTINUE
      K=5
  120 IF(K.GT.1) K=K-1
      KTICK=MTICK(K,KTAB)
C-----LOAD INTO TABLE.
      DO 130 L=1,KTICK
      XX=RLOG1(L,K,KTAB)
      TICKS(L)=ZINCH*DLOG10(XX)
      ITICK(L)=ILOG1(L,K,KTAB)
      IF(ITICK(L).LE.0) GO TO 130
C-----TURN OFF FINE GRID IF ALL DECADES WILL NOT BE IDENTIFIED.
      IF(KSPACE.GT.1) ITICK(L)=0
  130 CONTINUE
      GO TO 190
c-----------------------------------------------------------------------
C
C     STANDARD LOG INTERVALS.
C
c-----------------------------------------------------------------------
C-----DEFINE ULTRAFINE GRID NOT CLOSER THAN 1/10 INCH ON AVERAGE.
  140 DO 150 K=1,5
      KULTRA=KOKLIN(K)
      KTICK=KULTRA*KFINE
      IF(KTICK.GT.100) GO TO 160
      IF(DENSE2.LT.KTICK) GO TO 160
  150 CONTINUE
      K=6
  160 IF(K.GT.1) K=K-1
      KULTRA=KOKLIN(K)
C-----LOG TABLE. POINTS SPACED LOGARITHMICALLY OVER ONE DECADE.
      KTICK=KULTRA*KFINE
      K=0
      XULTRA=KULTRA
      DO 180 I=1,KFINE
      XX=RLGTAB(I)
      DTAB=(RLGTAB(I+1)-XX)/XULTRA
      DO 170 J=1,KULTRA
      K=K+1
      XX=XX+DTAB
      ITICK(K)=0
  170 TICKS(K)=ZINCH*DLOG10(XX)
      ITICK(K)=I+1
C-----TURN OFF FINE GRID IF ALL DECADES WILL NOT BE IDENTIFIED.
      IF(KSPACE.GT.1) ITICK(K)=0
  180 CONTINUE
c-----------------------------------------------------------------------
C
C     TURN OFF FINE LOG TICK MARKS THAT ARE TOO CLOSE TOGETHER
C
C     DEFINE MINIMUM ALLOWABLE SPACING FOR FINE LOG DECADE DIGITS TO
C     AVOID CONFLICT WITH LOG DECADE AXIS LABELS.
C
c-----------------------------------------------------------------------
C-----DISTANCE FROM BOTTOM OF DECADE.
  190 HTD2=2.0*HT+HTX1
C-----ALLOW ANOTHER SPACE FOR X AXIS IF EXPONENTS OF 10 OR MORE.
      IF(LROT.NE.1) GO TO 200
      IF(XYPLAN(1,1).LE.-10.0d0.OR.XYPLAN(2,1).GE.10.0d0)
     1 HTD2=HTD2+HT
C-----DISTANCE FROM TOP OF DECADE.
  200 HTD3=HT+HTX1
C-----DISTANCE BETWEEN FINE DIGITS WITHIN DECADE.
      HTD1=2.0*HTX1
C-----TOP TO BOTTOM.
      HTDX=HTD3
      TLAST=TICKS(KTICK)
      DO 220 K=KTICK,1,-1
      IF(ITICK(K).LE.1) GO TO 220
      TNOW=TICKS(K)
      IF((TLAST-TNOW).LT.HTDX) GO TO 210
      TLAST=TNOW
      HTDX=HTD1
      GO TO 220
  210 ITICK(K)=1
  220 CONTINUE
C-----BOTTOM TO TOP.
      HTDX=HTD2
      TLAST=0.0d0
      DO 240 K=1,KTICK
      IF(ITICK(K).LE.1) GO TO 240
      TNOW=TICKS(K)
      IF((TNOW-TLAST).LT.HTDX) GO TO 230
      TLAST=TNOW
      HTDX=HTD1
      GO TO 240
  230 ITICK(K)=1
  240 CONTINUE
  250 RETURN
      END
      SUBROUTINE GRIDZ(Z,LROT,MYGRID)
C=======================================================================
C
C     DRAW SOLID OR DASHED GRID LINE THAT LIES OUTSIDE OF LEGEND BOX.
C     THIS ROUTINE WILL ONLY DRAW VERTICAL OR HORIZONTAL LINES.
C
C     Z      = CONSTANT X OR Y VALUE
C     LROT   = 1 - X AXIS - CONSTANT X FROM YINCH1 TO YINCH2
C            = 2 - Y AXIS - CONSTANT Y FROM XINCH1 TO XINCH2
C     MYGRID = 1 - SOLID
C            = 2 - DASHED
C
C     THE X AND Y INCH LIMITS OF THE PLOT ARE DEFINED BY GPP27
C     THE X AND Y INCH LIMITS OF THE BOX  ARE DEFINED BY GPP22
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      DIMENSION ZDASH(3)
      DATA ZDASH/0.0d0,0.0d0,0.0d0/
c-----------------------------------------------------------------------
C
C     DEFINE LENGTH OF TICK MARKS (FOR 5 PATTERNS PER INCH -
C     EACH PATTERN WILL BE 1 DASH, 2 BLANKS)
C
c-----------------------------------------------------------------------
      ZDASH(2)=1.0d0/15.0d0
      ZDASH(3)=2.0d0*ZDASH(2)
c-----------------------------------------------------------------------
C
C     SELECT X OR Y AXIS.
C
c-----------------------------------------------------------------------
      IF(LROT.NE.1) GO TO 110
c-----------------------------------------------------------------------
C
C     X AXIS - VERTICAL LINE FROM YINCH1 TO YINCH2.
C
c-----------------------------------------------------------------------
C-----START AT LOWER Y AXIS.
      CALL PLOTP(Z,YINCH1,3)
C-----CHECK FOR NO OVERLAP WITH LEGEND BOX.
      IF(Z.LE.BIXMIN.OR.Z.GT.BIXMAX) GO TO 70
c-----------------------------------------------------------------------
C
C     OVERLAP.
C
c-----------------------------------------------------------------------
      IF(MYGRID.EQ.2) GO TO 10
C-----SOLID EXCEPT IN BOX.
      CALL PLOTP(Z,BIYMIN,2)
      CALL PLOTP(Z,BIYMAX,3)
      CALL PLOTP(Z,YINCH2,2)
      GO TO 220
C-----DASHED EXCEPT IN BOX.
   10 YLAST=YINCH1
      KPEN=3
C-----UP TO LOWER LIMIT OF BOX.
   20 KPEN=5-KPEN
      YLAST=YLAST+ZDASH(KPEN)
      IF(YLAST.ge.BIYMIN) go to 30
      CALL PLOTP(Z,YLAST,KPEN)
      GO TO 20
C-----ACROSS BOX.
   30 CALL PLOTP(Z,BIYMIN,KPEN)
   40 KPEN=5-KPEN
      YLAST=YLAST+ZDASH(KPEN)
      IF(YLAST.lt.BIYMAX) go to 40
C-----TO UPPER Y AXIS.
      CALL PLOTP(Z,BIYMAX,3)
      IF(YLAST.GT.YINCH2) GO TO 60
      CALL PLOTP(Z,YLAST,KPEN)
   50 KPEN=5-KPEN
      YLAST=YLAST+ZDASH(KPEN)
      IF(YLAST.ge.YINCH2) go to 60
      CALL PLOTP(Z,YLAST,KPEN)
      GO TO 50
   60 CALL PLOTP(Z,YINCH2,KPEN)
      GO TO 220
c-----------------------------------------------------------------------
C
C     NO OVERLAP WITH LEGEND BOX.
C
c-----------------------------------------------------------------------
   70 IF(MYGRID.EQ.2) GO TO 80
C-----SOLID.
      CALL PLOTP(Z,YINCH2,2)
      GO TO 220
C-----DASHED.
   80 YLAST=YINCH1
      KPEN=3
   90 KPEN=5-KPEN
      YLAST=YLAST+ZDASH(KPEN)
      IF(YLAST.ge.YINCH2) go to 100
      CALL PLOTP(Z,YLAST,KPEN)
      GO TO 90
  100 CALL PLOTP(Z,YINCH2,KPEN)
      GO TO 220
c-----------------------------------------------------------------------
C
C     Y AXIS - HORIZONTAL LINE FROM XINCH1 TO XINCH2.
C
c-----------------------------------------------------------------------
C-----START AT LOWER X AXIS.
  110 CALL PLOTP(XINCH1,Z,3)
C-----CHECK FOR NO OVERLAP WITH LEGEND BOX.
      IF(Z.LE.BIYMIN.OR.Z.GT.BIYMAX) GO TO 180
c-----------------------------------------------------------------------
C
C     OVERLAP.
C
c-----------------------------------------------------------------------
      IF(MYGRID.EQ.2) GO TO 120
C-----SOLID EXCEPT IN BOX.
      CALL PLOTP(BIXMIN,Z,2)
      CALL PLOTP(BIXMAX,Z,3)
      CALL PLOTP(XINCH2,Z,2)
      GO TO 220
C-----DASHED EXCEPT IN BOX.
  120 XLAST=XINCH1
      KPEN=3
C-----UP TO LOWER LIMIT OF BOX.
  130 KPEN=5-KPEN
      XLAST=XLAST+ZDASH(KPEN)
      IF(XLAST.ge.BIXMIN) go to 140
      CALL PLOTP(XLAST,Z,KPEN)
      GO TO 130
C-----ACROSS BOX.
  140 CALL PLOTP(BIXMIN,Z,KPEN)
  150 KPEN=5-KPEN
      XLAST=XLAST+ZDASH(KPEN)
      IF(XLAST.lt.BIXMAX) go to 150
C-----TO UPPER Y AXIS.
      CALL PLOTP(BIXMAX,Z,3)
      IF(XLAST.GT.XINCH2) GO TO 170
      CALL PLOTP(XLAST,Z,KPEN)
  160 KPEN=5-KPEN
      XLAST=XLAST+ZDASH(KPEN)
      IF(XLAST.ge.XINCH2) go to 170
      CALL PLOTP(XLAST,Z,KPEN)
      GO TO 160
  170 CALL PLOTP(XINCH2,Z,KPEN)
      GO TO 220
c-----------------------------------------------------------------------
C
C     NO OVERLAP WITH LEGEND BOX.
C
c-----------------------------------------------------------------------
  180 IF(MYGRID.EQ.2) GO TO 190
C-----SOLID.
      CALL PLOTP(XINCH2,Z,2)
      GO TO 220
C-----DASHED.
  190 XLAST=XINCH1
      KPEN=3
  200 KPEN=5-KPEN
      XLAST=XLAST+ZDASH(KPEN)
      IF(XLAST.ge.XINCH2) go to 210
      CALL PLOTP(XLAST,Z,KPEN)
      GO TO 200
  210 CALL PLOTP(XINCH2,Z,KPEN)
  220 RETURN
      END
      SUBROUTINE CHEKOV(LROT)
C=======================================================================
C
C     CHECK X OR Y LIMITS AND SELECT LINEAR OR LOG SCALING.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP26/XYDATA(2,2)
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
C-----10/09/02 - DYMI IS NOW VARIABLE BASED ON INPUT - WAS 1 %
      COMMON/MINDY/DYMIN
      DATA TEN /1.0d1/
      DATA HALF/5.0d-1/
c-----------------------------------------------------------------------
C
C     CHECK FOR NO RANGE.
C
c-----------------------------------------------------------------------
      IF(XYDATA(1,LROT).NE.XYDATA(2,LROT)) GO TO 30
C-----NO RANGE. USE LINEAR SCALING.
      IXYWAY(LROT)=1
C-----IF CONSTANT ZERO SET LIMITS TO -1.0 AND +1.0.
      IF(XYDATA(1,LROT).NE.0.0d0) GO TO 10
      XYDATA(1,LROT)=-1.0d0
      XYDATA(2,LROT)= 1.0d0
      GO TO 50
C-----IF CONSTANT SET LIMITS TO 0 AND 1.2 TIMES THE CONSTANT.
   10 IF(XYDATA(1,LROT).GT.0.0d0) GO TO 20
      XYDATA(1,LROT)=1.2d0*XYDATA(1,LROT)
      XYDATA(2,LROT)=0.0
      GO TO 50
   20 XYDATA(1,LROT)=0.0
      XYDATA(2,LROT)=1.2d0*XYDATA(2,LROT)
      GO TO 50
c-----------------------------------------------------------------------
C
C     CHECK FOR MINIMUM 1 PER-CENT Y RANGE.
C
c-----------------------------------------------------------------------
C-----INITIALIZE TO LOG SCALE EXCEPT FOR THE FOLLOWING SITUATIONS.
   30 IXYWAY(LROT)=2
C-----INSURE Y RANGE IS AT LEAST 1 PER-CENT WIDE.
      IF(LROT.NE.2) GO TO 40
      AVY=HALF*(XYDATA(2,2)+XYDATA(1,2))
      DY=HALF*(XYDATA(2,2)-XYDATA(1,2))
      IF(DY.GE.DYMIN*AVY) GO TO 40
      DY=DYMIN*AVY
      XYDATA(1,2)=AVY-DY
      XYDATA(2,2)=AVY+DY
C-----IF SMALL RANGE USE LINEAR SCALING.
   40 IF(XYDATA(2,LROT).LT.TEN*XYDATA(1,LROT)) IXYWAY(LROT)=1
c-----------------------------------------------------------------------
C
C     OVERRIDE AUTOMATIC SCALING SELECTION IF REQUESTED.
C
c-----------------------------------------------------------------------
   50 IF(LROT.EQ.2) GO TO 60
      IF(NXPLAN.EQ.1.OR.NXPLAN.EQ.2) IXYWAY(1)=NXPLAN
      GO TO 70
   60 IF(NYPLAN(LFRAME).EQ.1.OR.NYPLAN(LFRAME).EQ.2)
     1 IXYWAY(2)=NYPLAN(LFRAME)
c-----------------------------------------------------------------------
C
C     CANNOT SELECT LOG SCALING IF LOWER LIMIT IS NOT POSITIVE.
C
c-----------------------------------------------------------------------
   70 IF(XYDATA(1,LROT).LE.0.0d0) IXYWAY(LROT)=1
      RETURN
      END
      SUBROUTINE WINDXY(LROT)
C=======================================================================
C
C     EXTEND X OR Y LIMIT OUTWARD TO AVOID OVERLAP WITH GRID.
C
C     LROT = 1 - X AXIS
C          = 2 - Y AXIS
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP26/XYDATA(2,2)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      DIMENSION XYLIM(2,2)
      EQUIVALENCE (XINCH1,XYLIM(1,1))
      DATA TEN/1.0d1/
C-----DEFINE LIMITS IN THE PLANE OF THE PLOT.
      XYPLAN(1,LROT)=XYDATA(1,LROT)
      XYPLAN(2,LROT)=XYDATA(2,LROT)
      IF(IXYWAY(LROT).EQ.1) GO TO 10
      XYPLAN(1,LROT)=DLOG10(XYPLAN(1,LROT))
      XYPLAN(2,LROT)=DLOG10(XYPLAN(2,LROT))
C-----EXTEND INCH LIMITS OF THE PLOT OUTWARD.
   10 BOTTOM=XYLIM(1,LROT)+SPACER(1,LROT,LFRAME)
      TOP=XYLIM(2,LROT)-SPACER(2,LROT,LFRAME)
C-----DEFINE SCALE FACTOR (INCHES/DATA) AND THEN EXTEND DATA LIMITS OF
C-----PLOT OUTWARD.
      DSCALE=(XYPLAN(2,LROT)-XYPLAN(1,LROT))/(TOP-BOTTOM)
C-----EXTEND LIMITS.
      XYPLAN(1,LROT)=XYPLAN(1,LROT)-DSCALE*SPACER(1,LROT,LFRAME)
      XYPLAN(2,LROT)=XYPLAN(2,LROT)+DSCALE*SPACER(2,LROT,LFRAME)
C-----IF NEGATIVE Y RANGE NOT REQUESTED DO NOT ALLOW IT.
      IF(LROT.NE.2.OR.MINSOK.GT.0) GO TO 20
      IF(IXYWAY(2).EQ.1.AND.XYPLAN(1,2).LT.0.0d0) XYPLAN(1,2)=0.0d0
C-----DEFINE LIMITS IN DATA UNITS.
   20 IF(IXYWAY(LROT).EQ.2) GO TO 30
      XYDATA(1,LROT)=XYPLAN(1,LROT)
      XYDATA(2,LROT)=XYPLAN(2,LROT)
      GO TO 40
   30 XYDATA(1,LROT)=TEN**XYPLAN(1,LROT)
      XYDATA(2,LROT)=TEN**XYPLAN(2,LROT)
C-----DEFINE RANGE OF THE PLOT IN PLANE OF THE PLOT (LINEAR OR LOG).
   40 DXYP(LROT)=XYPLAN(2,LROT)-XYPLAN(1,LROT)
      RETURN
      END
      SUBROUTINE LABLXY(LROT)
C=======================================================================
C
C     DEFINE X AND Y AXIS LABELS AND UNITS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 XUNITS,YUNITS,
     1 XLABEL,YLABEL,RATIO,TEN,PARENL,PARENR,XUNIT,YUNIT
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP4/XLABEL(40),XUNITS(40),YLABEL(40),YUNITS(40)
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      DIMENSION XUNIT(4,6),YUNIT(11,6),PARENL(1),PARENR(1),
     2 RATIO(5),TEN(2)
      DATA TEN/'1','0'/
      DATA PARENL/'('/
      DATA PARENR/')'/
C-----RATIO MESSAGE.
      DATA RATIO/'R','a','t','i','o'/
C-----DEFINE UNITS FOR X AXIS.
      DATA XUNIT/
     1 ']','m','e','V',
     2 'm','e','V',' ',
     3 'e','V',' ',' ',
     4 'K','e','V',' ',
     5 'M','e','V',' ',
     6 'G','e','V',' '/
C-----DEFINE UNITS FOR Y AXIS.
      DATA YUNIT/
     1 'm','i','c','r','o','-','b','a','r','n','s',
     1 'm','i','l','l','i','-','b','a','r','n','s',
     1 'b','a','r','n','s',' ',' ',' ',' ',' ',' ',
     1 'k','i','l','o','-','b','a','r','n','s',' ',
     1 'M','e','g','a','-','b','a','r','n','s',' ',
     1 'G','e','g','a','-','b','a','r','n','s',' '/
c-----------------------------------------------------------------------
C
C     DEFINE INDEX TO UNITS TABLE.
C
c-----------------------------------------------------------------------
      CALL PEN(2)
      IUNIT=3+ISCALE(LROT)/3
      IF(IUNIT.LT.1.OR.IUNIT.GT.6) IUNIT=0
C-----DO NOT USE SCALED UNITS IF NO SCALE FACTOR.
      IF(ISCALE(LROT).EQ.0) IUNIT=0
c-----------------------------------------------------------------------
C
C     ONLY ALLOW SCALED UNITS FOR
C     MYPROG = 1 - COMPLOT
C            = 2 - EVALPLOT
C
c-----------------------------------------------------------------------
      IF(MYPROG.LT.1.OR.MYPROG.GT.2) IUNIT=0
c-----------------------------------------------------------------------
C
C     SELECT X OR Y AXIS.
C
c-----------------------------------------------------------------------
      IF(LROT.EQ.2) GO TO 60
c-----------------------------------------------------------------------
C
C     X AXIS.
C
c-----------------------------------------------------------------------
C-----DEFINE LENGTH OF TITLE AND UNITS.
      CALL SPECAL(XLABEL,IL,ILP,40)
      CALL SPECAL(XUNITS,IU,IUP,40)
C-----CANNOT RESCALE IF NO UNITS.
      IF(IUP.EQ.0) IUNIT=0
C-----IF SCALED UNITS WILL BE USED DEFINE LENGTH.
      IF(IUNIT.NE.0) CALL SPECAL(XUNIT(1,IUNIT),IU,IUP,4)
C-----DEFINE TOTAL LENGTH OF MESSAGE.
C-----START WITH LABEL AND TITLE.
      IXBCD=ILP+IUP
      IF(IUNIT.NE.0) GO TO 10
C-----ADD SPACE FOR SCALE FACTOR.
      IF(ISCALE(1).NE.0) IXBCD=IXBCD+5
C-----IF UNITS OR SCALE FACTOR ADD SPACE FOR BLANK, ( AND ).
   10 IF(IUP.GT.0.OR.(IUNIT.EQ.0.AND.ISCALE(1).NE.0)) IXBCD=IXBCD+3
C-----PLOT LABEL.
      XP=0.5d0*((XINCH1+XINCH2)-FLOAT(IXBCD)*WD)
      CALL SYMBL1(XP,YL(5),HT,XLABEL(1),0.0,IL)
C-----NOTHING ELSE TO DO IF ONLY LABEL (NO UNITS OR SCALE FACTOR).
      IF(IUP.LE.0.AND.ISCALE(1).EQ.0) GO TO 140
      XP=XP+FLOAT(ILP+1)*WD
C-----FOLLOWED BY  (
      CALL SYMBL1(XP,YL(5),HT,PARENL(1),0.0,1)
      XP=XP+WD
C-----ONLY PLOT SCALE FACTOR IF SCALED UNITS ARE NOT USED AND
C-----SCALE FACTOR IS NOT ZERO.
      IF(IUNIT.GT.0.OR.ISCALE(1).EQ.0) GO TO 20
C-----FOLLOW BY 10**SCALE FACTOR
      CALL SYMBL1(XP,YL(5),HT,TEN(1),0.0,2)
      XP=XP+2.0*WD
      IF(ISCALE(1).LT.0) XP=XP-WD
      XX=ISCALE(1)
      YQQ = YL(5)+HT
      CALL NUMBR1(XP,YQQ,HT,XX,0.0,-1)
      XP=XP+2.0*WD
      IF(ISCALE(1).LT.0) XP=XP+WD
      IF(IABS(ISCALE(1)).LT.10) XP=XP-WD
      IF(IUP.GE.0) XP=XP+WD
C-----FOLLOW BY UNITS...SCALED OR ORIGINAL.
   20 IF(IUP.LE.0) GO TO 50
      IF(IUNIT.EQ.0) GO TO 30
      CALL SYMBL1(XP,YL(5),HT,XUNIT(1,IUNIT),0.0,IU)
      GO TO 40
   30 CALL SYMBL1(XP,YL(5),HT,XUNITS(1),0.0,IU)
C-----FOLLOWED BY ).
   40 XP=XP+FLOAT(IUP)*WD
   50 CALL SYMBL1(XP,YL(5),HT,PARENR(1),0.0,1)
      GO TO 140
c-----------------------------------------------------------------------
C
C     Y AXIS.
C
c-----------------------------------------------------------------------
   60 YMID=0.5d0*(YINCH1+YINCH2)
      IF(LFRAME.NE.LFRAMR) GO TO 70
c-----------------------------------------------------------------------
C
C     PLOT Y AXIS LABEL AS RATIO.
C
c-----------------------------------------------------------------------
      YP=YMID-2.5d0*WD
      CALL SYMBL1(XL(3),YP,HT,RATIO(1),90.0,5)
      GO TO 140
c-----------------------------------------------------------------------
C
C     IF TOP 2 SUB-PLOTS ARE BOTH CROSS SECTION SKIP SECOND TITLE.
C
c-----------------------------------------------------------------------
   70 IF(MODPLT.NE.1.AND.MODPLT.NE.3) GO TO 80
      IF(LFRAME.EQ.2) GO TO 140
C-----DEFINE LENGTH OF TITLE AND UNITS.
   80 CALL SPECAL(YLABEL,IL,ILP,40)
      CALL SPECAL(YUNITS,IU,IUP,40)
C-----CANNOT RESCALE IF NO UNITS.
      IF(IUP.EQ.0) IUNIT=0
C-----IF TOP 2 SUB-PLOTS ARE BOTH CROSS SECTION CENTER TITLE OVER BOTH.
      IF(MODPLT.EQ.1.OR.MODPLT.EQ.3) YMID=YINCH1
C-----IF SCALED UNITS WILL BE USED DEFINE LENGTH.
      IF(IUNIT.NE.0) CALL SPECAL(YUNIT(1,IUNIT),IU,IUP,11)
C-----DEFINE TOTAL LENGTH OF MESSAGE.
C-----START WITH LABEL AND TITLE.
      IYBCD=ILP+IUP
      IF(IUNIT.NE.0) GO TO 90
C-----ADD SPACE FOR SCALE FACTOR.
      IF(ISCALE(2).NE.0) IYBCD=IYBCD+5
C-----IF UNITS OR SCALE FACTOR ADD SPACE FOR BLANK, ( AND ).
   90 IF(IUP.GT.0.OR.(IUNIT.EQ.0.AND.ISCALE(2).NE.0)) IYBCD=IYBCD+3
C-----PLOT LABEL.
      YP=YMID-0.5d0*FLOAT(IYBCD)*WD
      CALL SYMBL1(XL(1),YP,HT,YLABEL(1),90.0,IL)
C-----NOTHING ELSE TO DO IF ONLY LABEL (NO UNITS OR SCALE FACTOR).
      IF(IUP.LE.0.AND.ISCALE(2).EQ.0) GO TO 140
      YP=YP+FLOAT(ILP+1)*WD
C-----FOLLOWED BY  (
      CALL SYMBL1(XL(1),YP,HT,PARENL(1),90.0,1)
      YP=YP+WD
C-----ONLY PLOT SCALE FACTOR IF SCALED UNITS ARE NOT USED AND
C-----SCALE FACTOR IS NOT ZERO.
      IF(IUNIT.GT.0.OR.ISCALE(2).EQ.0) GO TO 100
C-----FOLLOW BY 10**SCALE FACTOR
      CALL SYMBL1(XL(1),YP,HT,TEN(1),90.0,2)
      YP=YP+2.0d0*WD
      IF(ISCALE(2).LT.0) YP=YP-WD
      YP2=ISCALE(2)
      XQQ = XL(1)-HT
      CALL NUMBR1(XQQ,YP,HT,YP2,90.0,-1)
      YP=YP+2.0d0*WD
      IF(ISCALE(2).LT.0) YP=YP+WD
      IF(IABS(ISCALE(2)).LT.10) YP=YP-WD
      IF(IUP.GE.0) YP=YP+WD
C-----FOLLOW BY UNITS...SCALED OR ORIGINAL.
  100 IF(IUP.LE.0) GO TO 130
      IF(IUNIT.EQ.0) GO TO 110
      CALL SYMBL1(XL(1),YP,HT,YUNIT(1,IUNIT),90.0,IU)
      GO TO 120
  110 CALL SYMBL1(XL(1),YP,HT,YUNITS(1),90.0,IU)
C-----FOLLOWED BY ).
  120 YP=YP+FLOAT(IUP)*WD
  130 CALL SYMBL1(XL(1),YP,HT,PARENR(1),90.0,1)
  140 RETURN
      END
      SUBROUTINE SCALXY(LROT)
C=======================================================================
C
C     DEFINE SCALING PARAMETERS IN ORDER TO ADEQUATELY ANNOTATE THE
C     X OR Y AXIS OF PLOT.
C
C     IF THE SCALING IS LINEAR THE RANGE OF THE AXIS IS USED TO SELECT
C     A MAJOR SUB-DIVISION. THE AXIS WILL THEN BE LABELLED AT EACH
C     MULTIPLE OF THIS MAJOR SUB-DIVISION. THE NUMBER OF MAJOR
C     SUB-DIVISIONS WILL ALWAYS BE 5 TO 15, UNLESS THE PLOT COVERS
C     AN EXTREMELY NARROW RANGE. A VIRTUAL PLOT SIZE IS THEN DEFINED
C     WHICH IS AN EXACT MULTIPLE OF THE MAJOR SUB-DIVISION SIZE AND
C     WHICH COVERS A RANGE AT LEAST AS LARGE AS THE TRUE PLOTTING AREA.
C
C     IF THE SCALING IS LOG THE RANGE OF THE AXIS IS USED TO DEFINE AN
C     INTEGRAL NUMBER OF LOG DECADES THAT COVER A RANGE AT LEAST AS
C     LARGE AS THE TRUE PLOTTING AREA (THIS IS THE VIRTUAL PLOT SIZE).
C
C     WITHIN SUBROUTINE GRID THE VIRTUAL PLOT WILL BE LOGICALLY DRAWN.
C     HOWEVER, ONLY THAT PORTION WHICH APPEARS WITHIN THE TRUE PLOT AREA
C     WILL BE OUTPUT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      DIMENSION DELTA(27),IDUG(27),XYLIM(2,2)
      EQUIVALENCE (XINCH1,XYLIM(1,1))
C-----DEFINE NUMBER OF DECIMAL DIGITS NECESSARY TO ADEQUATELY
C-----REPRESENT MULTIPLES OF THE ABOVE MAJOR SUB-DIVISIONS.
      DATA IDUG/
     1 0,0,0,0,0,0,0,0,0,
     1 1,1,1,2,2,2,3,3,3,
     2 4,4,4,5,5,5,6,6,6/
C-----DEFINE MAJOR SUB-DIVISIONS IN NORMAL FORM.
      DATA ONE/1.0d0/
      DATA THOUSD/1.0d3/
      DATA DELTA/
     1 5.0d2 ,2.0d2 ,1.0d2 ,
     2 5.0d1 ,2.0d1 ,1.0d1 ,
     3 5.0d0 ,2.0d0 ,1.0d0 ,
     4 5.0d-1,2.0d-1,1.0d-1,
     5 5.0d-2,2.0d-2,1.0d-2,
     6 5.0d-3,2.0d-3,1.0d-3,
     7 5.0d-4,2.0d-4,1.0d-4,
     8 5.0d-5,2.0d-5,1.0d-5,
     9 5.0d-6,2.0d-6,1.0d-6/
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPD(TERPZ)=((TERPZ-XYPLAN(1,LROT))*XYLIM(2,LROT)+
     1 (XYPLAN(2,LROT)-TERPZ)*XYLIM(1,LROT))/DXYP(LROT)
C-----SELECT LINEAR OR LOG SCALING.
      IF(IXYWAY(LROT).EQ.2) GO TO 70
c-----------------------------------------------------------------------
C
C     LINEAR SCALING. SELECT LARGEST ABSOLUTE LIMIT OF PLOT AND PUT
C     INTO NORMAL FORM.
C
c-----------------------------------------------------------------------
      XYBIG=DABS(XYPLAN(2,LROT))
      ABSXY=DABS(XYPLAN(1,LROT))
      IF(ABSXY.GT.XYBIG) XYBIG=ABSXY
C-----ONLY ACCEPT POWERS OF 1000 AS SCALE FACTOR.
      KSCALE=0
      QEXP=ONE
      ZTOP=XYBIG
C-----ALLOW RATIO (Y AXIS) TO BE LESS THAN 1
      IF(LFRAME.EQ.LFRAMR.AND.LROT.EQ.2.AND.
     1 ZTOP.GE.1.0d-3) GO TO 20
      IF(ZTOP.GE.ONE) GO TO 20
   10 KSCALE=KSCALE-3
      QEXP=QEXP*THOUSD
      ZTOP=XYBIG*QEXP
      IF(ZTOP.LT.ONE) GO TO 10
      GO TO 40
   20 IF(ZTOP.LT.THOUSD) GO TO 40
   30 KSCALE=KSCALE+3
      QEXP=QEXP/THOUSD
      ZTOP=XYBIG*QEXP
      IF(ZTOP.GE.THOUSD) GO TO 30
   40 ISCALE(LROT)=KSCALE
C-----DETERMINE MAXIMUM INTERVAL SPACING THAT WILL YIELD THE REQUIRED
C-----NUMBER OF MAJOR SUB-DIVISIONS ON PLOT.
      ZTOP=XYPLAN(2,LROT)*QEXP
      ZBOT=XYPLAN(1,LROT)*QEXP
      NDEC=4
      DO 50 I=1,27
      ZZMAX=ZTOP/DELTA(I)
      ZZMIN=ZBOT/DELTA(I)
      IZZMAX=ZZMAX
      IZZMIN=ZZMIN
      IF(IZZMAX.LE.0.AND.IZZMAX.GT.ZZMAX) IZZMAX=IZZMAX-1
      IF(IZZMIN.GE.0.AND.IZZMIN.LT.ZZMIN) IZZMIN=IZZMIN+1
      IDEC(LROT)=IZZMAX-IZZMIN
      IF(I.EQ.11) NDEC=2
      IF(IDEC(LROT).GE.NDEC) GO TO 60
   50 CONTINUE
      I=27
C-----DEFINE ALL MAJOR DIVISION SCALING INFORMATION FOR THE PLOT
C-----INCLUDING SIZE OF EACH MAJOR SUB-DIVISION, VIRTUAL SIZE OF THE
C-----PLOT IN FULL MULTIPLES OF MAJOR SUB-DIVISION, SCALED STEP SIZE,
C-----SCALED LOWER STARTING POINT AND NUMBER OF DIGITS TO USE WITH
C-----AXIS ANNOTATION.
   60 ZZMAX=ZTOP/DELTA(I)
      ZZMIN=ZBOT/DELTA(I)
      IZZMAX=ZZMAX
      IZZMIN=ZZMIN
      IF(IZZMAX.GE.0.AND.IZZMAX.LT.ZZMAX) IZZMAX=IZZMAX+1
      IF(IZZMIN.LE.0.AND.IZZMIN.GT.ZZMIN) IZZMIN=IZZMIN-1
      IDEC(LROT)=IZZMAX-IZZMIN
C-----INSURE THAT THERE WILL BE AT LEAST ONE MAJOR SUB-DIVISION.
      IF(IDEC(LROT).LT.1) IDEC(LROT)=1
      DZ=DELTA(I)
      VIRTIN(2,LROT)=FLOAT(IZZMAX)*DZ/QEXP
      VIRTIN(1,LROT)=FLOAT(IZZMIN)*DZ/QEXP
      DXYBAS(LROT)=DZ
      XYBASE(LROT)=FLOAT(IZZMIN)*DZ
      IF(DABS(XYBASE(LROT)).EQ.0.0d0) XYBASE(LROT)=0.0d0
      IDIGIT(LROT)=IDUG(I)
      GO TO 80
c-----------------------------------------------------------------------
C
C     LOG SCALING. DETERMINE NUMBER OF FULL LOG DECADES AND DEFINE
C     VIRTUAL SIZE OF PLOT DEFINED BY FULL DECADES.
C
c-----------------------------------------------------------------------
   70 ZZMAX=XYPLAN(2,LROT)
      IZMAX=ZZMAX
      IF(ZZMAX.NE.IZMAX.AND.ZZMAX.GT.0.0d0) ZZMAX=ZZMAX+1.0d0
      ZZMIN=XYPLAN(1,LROT)
      IZMIN=ZZMIN
      IF(ZZMIN.NE.IZMIN.AND.ZZMIN.LT.0.0d0) ZZMIN=ZZMIN-1.0d0
      IDEC1=ZZMAX
      IDEC2=ZZMIN
      IDEC(LROT)=IDEC1-IDEC2
      VIRTIN(2,LROT)=IDEC1
      VIRTIN(1,LROT)=IDEC2
      DXYBAS(LROT)=1.0
      XYBASE(LROT)=VIRTIN(1,LROT)
      IDIGIT(LROT)=-1
      ISCALE(LROT)=0
c-----------------------------------------------------------------------
C
C     LINEAR OR LOG SCALING.
C
c-----------------------------------------------------------------------
C-----IF NO DECIMAL DIGITS IN AXIS LABEL REMOVE DECIMAL POINT.
   80 IF(IDIGIT(LROT).LE.0) IDIGIT(LROT)=-1
C-----DEFINE VIRTUAL SIZE OF PLOT IN PLANE OF THE PLOT (INCHES).
      VIRTIN(1,LROT)=TERPD(VIRTIN(1,LROT))
      VIRTIN(2,LROT)=TERPD(VIRTIN(2,LROT))
      RETURN
      END
      SUBROUTINE AXIS(LROT,IWAY)
C=======================================================================
C
C     PLOT SCALE DIVISION ON EITHER X OR Y AXIS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 TENBCD
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      DIMENSION TENBCD(2)
      DATA TENBCD/'1','0'/
      DATA ZLOW/1.0d-6/
      CALL PEN(2)
C-----CORRECT AXIS LABEL FOR POSSIBLE ROUND-OFF.
      IF(DABS(ZBASE).LT.ZLOW) ZBASE=0.0d0
C-----SELECT X OR Y AXIS SCALE.
      GO TO (10,40),LROT
c-----------------------------------------------------------------------
C
C     X AXIS. SELECT LINEAR OR LOG SCALING.
C
c-----------------------------------------------------------------------
   10 GO TO (20,30),IWAY
C-----LINEAR X. DEFINE NUMBER OF CHARACTERS THAT WILL BE PLOTTED.
   20 LDIG=IDIGIT(1)+2
      IF(ZBASE.LT.0.0d0) LDIG=LDIG+1
      NZBASE=1.00001d0*DABS(ZBASE)
      IF(NZBASE.GE.10) LDIG=LDIG+1
      IF(NZBASE.GE.100) LDIG=LDIG+1
      IF(NZBASE.GE.1000) LDIG=LDIG+1
C-----DEFINE OFFSET TO CENTER NUMBER BELOW AXIS TICK MARK.
      OFFSET=0.5d0*FLOAT(LDIG)*WD
C-----PLOT AXIS SCALE.
      ZZBASE=ZBASE
      ZQQ = ZQ(1)-OFFSET
      CALL NUMBR1(ZQQ,YL(3),HT,ZZBASE,0.0,IDIGIT(1))
      GO TO 70
C-----LOG X. PLOT 10 TO GO WITH EXPONENT.
   30 ZQQ = ZQ(1)-WD
      CALL SYMBL1(ZQQ,YL(4),HT,TENBCD(1),0.0,2)
C-----PLOT EXPONENT AS INTEGER.
      ZZBASE=ZBASE
      ZQ10=ZQ(1)+WD
C-----POSITION MINUS SIGN OVER THE 10.
      IF(ZZBASE.LT.0.0d0) ZQ10=ZQ10-HT
      CALL NUMBR1(ZQ10,YL(3),HT,ZZBASE,0.0,-1)
      GO TO 70
c-----------------------------------------------------------------------
C
C     Y AXIS. SELECT LINEAR OR LOG.
C
c-----------------------------------------------------------------------
C-----NOT ABOVE TOP OF PLOT.
   40 GO TO (50,60),IWAY
C-----LINEAR Y AXIS.
   50 ZZBASE=ZBASE
C-----DO NOT PLOT ABOVE TOP OF PLOT.
      YTOP=ZQ(2)+HT
      IF(YTOP.GT.YINCH2) GO TO 70
C-----RIGHT ADJUST.
      XX=3+IDIGIT(2)
      IF(ZZBASE.LT.0.0d0) XX=XX+1.0d0
      ABSBAS=DABS(ZZBASE)
      IF(ABSBAS.GE.10.0) XX=XX+1.0d0
      IF(ABSBAS.GE.100.0) XX=XX+1.0d0
      XX=XINCH1-XX*HT
      CALL NUMBR1(XX,ZQ(2),HT,ZZBASE,0.0,IDIGIT(2))
      GO TO 70
C-----LOG Y. DEFINE CENTER OF EXPONENT
   60 YLP=ZQ(2)+HT
C-----DO NOT PLOT ABOVE TOP OF PLOT.
      YTOP=YLP+HT
      IF(YTOP.GT.YINCH2) GO TO 70
C-----PLOT EXPONENT AS INTEGER.
      ZZBASE=ZBASE
C-----PLOT 10 TO GO WITH EXPONENT.
      CALL SYMBL1(XL(2),ZQ(2),HT,TENBCD(1),0.0,2)
      ZQ10=XL(2)+2.0d0*WD
C-----POSITION MINUS SIGN OVER THE 10.
      IF(ZZBASE.LT.0.0d0) ZQ10=ZQ10-HT
      CALL NUMBR1(ZQ10,YLP,HT,ZZBASE,0.0,-1)
   70 RETURN
      END
      SUBROUTINE LINER(NX1,NX2,LCURVE)
C=======================================================================
C
C     PLOT THAT PORTION OF A LINEARLY INTERPOLABLE FUNCTION THAT LIES
C     WITHIN THE RECTANGULAR PLOTTING AREA DEFINED BY XYDATA.
C
C     ARGUMENTS
C     ---------
C     NX1     = INDEX TO FIRST POINT IN PAGING SYSTEM TO PLOT.
C     NX2     = INDEX TO LAST POINT IN PAGING SYSTEM TO PLOT.
C     LCURVE  = INDEX TO DATA (1 TO 3)
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 XOFF2,YOFF1,YOFF2
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP26/XYDATA(2,2)
      DATA XOFF2/0/
      DATA YOFF1/0/
      DATA YOFF2/0/
c-----------------------------------------------------------------------
C
C     FIND FIRST ENERGY POINT ABOVE MINIMUM ENERGY OF PLOT.
C
c-----------------------------------------------------------------------
      DO 10 K=NX1,NX2
      CALL XYCURV(XD,Y2,K,LCURVE)
      X2=XD
      IF(X2.gt.XYDATA(1,1)) go to 20
      X1=X2
   10 Y1=Y2
      GO TO 230
C-----IF FIRST POINT IS ABOVE UPPER X LIMIT OF PLOT THERE IS NOTHING TO
C-----PLOT.
   20 IF(K.EQ.NX1.AND.X2.GE.XYDATA(2,1)) GO TO 230
c-----------------------------------------------------------------------
C
C     DEFINE CROSS SECTION AT LOWER ENERGY LIMIT OF PLOT, OR IF
C     THIS IS FIRST POINT REPEAT IT.
C
c-----------------------------------------------------------------------
      KPOINT=NPOINT
      XOFF2=0
      IF(K.LE.NX1) GO TO 40
      IF(X1.EQ.XYDATA(1,1)) GO TO 50
      IF(X2.le.X1) go to 30
      Y1=TERPY(XYDATA(1,1),X1,X2,Y1,Y2,NTDATA)
   30 X1=XYDATA(1,1)
      KPOINT=0
      GO TO 50
   40 IF(X2.GT.XYDATA(2,1)) GO TO 230
      X1=X2
      Y1=Y2
c-----------------------------------------------------------------------
C
C     IS CROSS SECTION OF LAST POINT BELOW, ON OR ABOVE CROSS SECTION
C     RANGE OF PLOT
C
c-----------------------------------------------------------------------
   50 IF(Y1.lt.XYDATA(1,2)) go to 70
      IF(Y1.eq.XYDATA(1,2)) go to 60
      IF(Y1.gt.XYDATA(2,2)) go to 80
C-----ON. SET FLAG AND MOVE BEAM TO STARTING POINT.
   60 YOFF1=0
      CALL DASHP(X1,Y1,3,KPOINT)
      GO TO 100
C-----BELOW
   70 YOFF1=1
      GO TO 100
C-----ABOVE
   80 YOFF1=2
      GO TO 100
c-----------------------------------------------------------------------
C
C     IF ANY POINTS REMAIN SELECT NEXT POINT. RE-DEFINE REAL POSITION
C     OF LAST POINT AND SAVE REAL POSITION OF CURRENT POINT.
C
c-----------------------------------------------------------------------
   90 IF(K.GE.NX2) GO TO 230
      X1=X2
      Y1=Y2
      YOFF1=YOFF2
      K=K+1
      CALL XYCURV(XD,Y2,K,LCURVE)
      X2=XD
c-----------------------------------------------------------------------
C
C     IF ENERGY ABOVE MAXIMUM ENERGY OF PLOT INTERPOLATE BACK TO TOP
C     OF PLOT
C
c-----------------------------------------------------------------------
  100 KPOINT=NPOINT
      IF(X2.lt.XYDATA(2,1)) go to 130
      IF(X2.eq.XYDATA(2,1)) go to 120
      IF(X2.le.X1) go to 110
      Y2=TERPY(XYDATA(2,1),X1,X2,Y1,Y2,NTDATA)
  110 X2=XYDATA(2,1)
      KPOINT=0
  120 XOFF2=2
c-----------------------------------------------------------------------
C
C      IS CROSS SECTION OF CURRENT POINT BELOW, ON OR ABOVE CROSS
C      SECTION RANGE OF PLOT.
C
c-----------------------------------------------------------------------
  130 IF(Y2.lt.XYDATA(1,2)) go to 150
      IF(Y2.eq.XYDATA(1,2)) go to 140
      IF(Y2.gt.XYDATA(2,2)) go to 160
C-----ON.
  140 YOFF2=0
      GO TO 180
C-----BELOW
  150 YOFF2=1
      GO TO 170
C-----ABOVE
  160 YOFF2=2
C-----CURRENT POINT IS NOT ON PLOT. IF LAST POINT IS ALSO OFF PLOT IN
C-----SAME DIRECTION, SKIP CURRENT POINT.
  170 IF(YOFF2.NE.YOFF1) GO TO 180
C-----IF ABOVE MAXIMUM ENERGY OF PLOT, PLOT IS FINISHED
      IF(XOFF2.le.0.0d0) go to 90
      go to 230
c-----------------------------------------------------------------------
C
C     IF CROSS SECTION OF LAST POINT WAS NOT IN RANGE OF PLOT
C     INTERPOLATE BACK TO PLOT BOUNDARY AND MOVE BEAM TO BOUNDARY
C
c-----------------------------------------------------------------------
  180 IF(YOFF1.EQ.0) GO TO 200
      IF(Y2.eq.Y1) go to 190
      YUSE=XYDATA(YOFF1,2)
      X1=TERPX(YUSE,Y1,Y2,X1,X2,NTDATA)
  190 IF(X1.GT.XYDATA(2,1)) GO TO 230
      Y1=XYDATA(YOFF1,2)
      CALL DASHP(X1,Y1,3,0)
c-----------------------------------------------------------------------
C
C     IF CROSS SECTION OF CURRENT POINT IS NOT IN RANGE OF PLOT
C     INTERPOLATE BACK TO PLOT BOUNDARY
C
c-----------------------------------------------------------------------
  200 KPOINT=NPOINT
      XP=X2
      YP=Y2
      IF(YOFF2.EQ.0) GO TO 220
      IF(Y2.eq.Y1) go to 210
      YUSE=XYDATA(YOFF2,2)
      XP=TERPX(YUSE,Y1,Y2,X1,X2,NTDATA)
  210 YP=XYDATA(YOFF2,2)
      KPOINT=0
c-----------------------------------------------------------------------
C
C     DRAW LINE TO NEXT POINT.
C
c-----------------------------------------------------------------------
  220 CALL DASHP(XP,YP,2,KPOINT)
C-----IF ABOVE MAXIMUM ENERGY OF PLOT, PLOT IS FINISHED.
      IF(XOFF2.LE.0) GO TO 90
  230 RETURN
      END
      SUBROUTINE OPENER(DWDX,DWDY)
C=======================================================================
C
C     FIND LARGEST OPEN AREA AND POSITION LEGEND BOX IN IT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      DIMENSION BOXTRY(2)
c-----------------------------------------------------------------------
C
C     FOR COMPLOT POSITION LEGEND BOX IN UPPER, RIGHT HAND CORNER.
C     FOR OTHER PROGRAMS FIND LARGEST OPEN EARA.
C
c-----------------------------------------------------------------------
      IF(MYPROG.NE.1) GO TO 10
      BOXLIM(1,1,LFRAME)=XYINCH(1,1,LFRAME)+WD
      BOXLIM(2,2,LFRAME)=XYINCH(2,2,LFRAME)-HT
      BOXLIM(2,1,LFRAME)=BOXLIM(1,1,LFRAME)+DWDX
      BOXLIM(1,2,LFRAME)=BOXLIM(2,2,LFRAME)-DWDY
      RETURN
c-----------------------------------------------------------------------
C
C     FIND LARGEST AREA TO LEFT, MIDDLE, RIGHT - AT TOP OR BOTTOM
C
c-----------------------------------------------------------------------
C-----LEFT, MIDDLE AND RIGHT
   10 DO 50 IX=1,3
      IF(IX.eq.2) go to 20
      IF(IX.gt.2) go to 30
C-----LEFT.
      BOXTRY(1)=XYINCH(1,1,LFRAME)+WD
      BOXTRY(2)=BOXTRY(1)+DWDX
      GO TO 40
C-----MIDDLE.
   20 BOXTRY(1)=0.5d0*(XYINCH(1,1,LFRAME)+XYINCH(1,1,LFRAME)-DWDX)
      BOXTRY(2)=0.5d0*(XYINCH(1,1,LFRAME)+XYINCH(1,1,LFRAME)+DWDX)
      GO TO 40
C-----RIGHT.
   30 BOXTRY(2)=XYINCH(2,1,LFRAME)-WD
      BOXTRY(1)=BOXTRY(2)-DWDX
C-----DEFINE IN DATA UNITS.
   40 CONTINUE
C-----DETERMINE THE Y RANGE OF EVERYTHING.
   50 CONTINUE
      RETURN
      END
      FUNCTION TERPY(X,X1,X2,Y1,Y2,ITERP)
C=======================================================================
C
C     INTERPOLATION IN X TO DEFINE Y.
C
C=======================================================================
      INCLUDE 'implicit.h'
      DATA ZERO/0.0d0/
C-----CHECK FOR DISCONTINUITY.
      IF(X1.NE.X2) GO TO 10
      TERPY=Y2
      RETURN
C-----CHECK FOR CONSTANT.
   10 IF(Y1.NE.Y2) GO TO 30
   20 TERPY=Y1
      RETURN
C-----SELECT INTERPOLATION LAW.
   30 GO TO (20,40,50,60,70),ITERP
C-----LIN X - LIN Y INTERPOLATION
   40 XX1=(X-X1)*Y2
      XX2=(X2-X)*Y1
      TERPY=(XX1+XX2)/(X2-X1)
      RETURN
C-----LOG X - LIN Y INTERPOLATION
   50 IF(X1.LE.ZERO.OR.X2.LE.ZERO.OR.X.LE.ZERO) GO TO 40
      XX1=DLOG(X/X1)*Y2
      XX2=DLOG(X2/X)*Y1
      TERPY=(XX1+XX2)/DLOG(X2/X1)
      RETURN
C-----LIN X - LOG Y INTERPOLATION.
   60 IF(Y1.LE.ZERO.OR.Y2.LE.ZERO) GO TO 40
      XX1=(X-X1)*DLOG(Y2)
      XX2=(X2-X)*DLOG(Y1)
      TERPY=DEXP((XX1+XX2)/(X2-X1))
      RETURN
C-----LOG X - LOG Y INTERPOLATION.
   70 IF(Y1.LE.ZERO.OR.Y2.LE.ZERO) GO TO 40
      XX1=DLOG(X/X1)*DLOG(Y2)
      XX2=DLOG(X2/X)*DLOG(Y1)
      TERPY=DEXP((XX1+XX2)/DLOG(X2/X1))
      RETURN
      END
      FUNCTION TERPX(Y,Y1,Y2,X1,X2,ITERP)
C=======================================================================
C
C     INTERPOLATION IN Y TO DEFINE X.
C
C=======================================================================
      INCLUDE 'implicit.h'
      DATA ZERO/0.0d0/
C-----CHECK FOR DISCONTINUITY.
      IF(Y1.NE.Y2) GO TO 10
      TERPX=X2
      RETURN
C-----CHECK FOR CONSTANT.
   10 IF(X1.NE.X2) GO TO 30
   20 TERPX=X1
      RETURN
C-----SELECT INTERPOLATION LAW.
   30 GO TO (20,40,50,60,70),ITERP
C-----LIN X - LIN Y INTERPOLATION
   40 YY1=(Y-Y1)*X2
      YY2=(Y2-Y)*X1
      TERPX=(YY1+YY2)/(Y2-Y1)
      RETURN
C-----LOG X - LIN Y INTERPOLATION
   50 IF(X1.LE.ZERO.OR.X2.LE.ZERO) GO TO 40
      YY1=(Y-Y1)*DLOG(X2)
      YY2=(Y2-Y)*DLOG(X1)
      TERPX=DEXP((YY1+YY2)/(Y2-Y1))
      RETURN
C-----LIN X - LOG Y INTERPOLATION.
   60 IF(Y1.LE.ZERO.OR.Y2.LE.ZERO.OR.Y.LE.ZERO) GO TO 40
      YY1=DLOG(Y/Y1)*X2
      YY2=DLOG(Y2/Y)*X1
      TERPX=(YY1+YY2)/DLOG(Y2/Y1)
      RETURN
C-----LOG X - LOG Y INTERPOLATION.
   70 IF(X1.LE.ZERO.OR.X2.LE.ZERO) GO TO 40
      YY1=DLOG(Y/Y1)*DLOG(X2)
      YY2=DLOG(Y2/Y)*DLOG(X1)
      TERPX=DEXP((YY1+YY2)/DLOG(Y2/Y1))
      RETURN
      END
      SUBROUTINE SETTER(NX1,NX2,LSET)
C=======================================================================
C
C     PLOT A SET OF POINTS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP26/XYDATA(2,2)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPD(TERPZ)=((TERPZ-XYPLAN(1,1))*XINCH2+
     1 (XYPLAN(2,1)-TERPZ)*XINCH1)/DXYP(1)
      TERPS(TERP1)=((TERP1-XYPLAN(1,2))*YINCH2+
     1 (XYPLAN(2,2)-TERP1)*YINCH1)/DXYP(2)
c-----------------------------------------------------------------------
C
C     FIND FIRST ENERGY POINT ABOVE MINIMUM ENERGY OF PLOT.
C
c-----------------------------------------------------------------------
      DO 30 K=NX1,NX2
      CALL XYPONT(XD,DXM,DXP,YS,DYM,DYP,K,LSET)
      IF(XD.LT.XYDATA(1,1)) GO TO 30
      IF(XD.GT.XYDATA(2,1)) GO TO 40
      IF(YS.LT.XYDATA(1,2).OR.YS.GT.XYDATA(2,2)) GO TO 30
C-----TRANSLATE TO THE PLANE OF THE PLOT (LINEAR OR LOG).
      IF(IXYWAY(1).NE.2) GO TO 10
      XD=DLOG10(XD)
      DXM=DLOG10(DXM)
      DXP=DLOG10(DXP)
   10 IF(IXYWAY(2).NE.2) GO TO 20
      YS=DLOG10(YS)
      DYM=DLOG10(DYM)
      DYP=DLOG10(DYP)
C-----TRANSLATE TO INCHES.
   20 XS=TERPD(XD)
      DXMS=TERPD(DXM)
      DXPS=TERPD(DXP)
      YS=TERPS(YS)
      DYM=TERPS(DYM)
      DYP=TERPS(DYP)
C-----PLOT SYMBOL AND ERROR BARS.
      CALL SYMREF(XS,DXMS,DXPS,YS,DYM,DYP,LSET)
   30 CONTINUE
   40 RETURN
      END
      SUBROUTINE ARROWS
C=======================================================================
C
C     IF RATIO IS PLOTTED INDICATE MAXIMUM AND MINIMUM RATIOS BY
C     PLOTTING AN ARROW AT THE UPPER AND LOWER LIMITS OF THE SUBPLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 MAXIE,MINNIE,RATIO
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      DIMENSION MAXIE(3),MINNIE(3),RATIO(5)
C-----DEFINE MESSAGES FOR MAX AND MIN RATIOS.
      DATA MAXIE/
     1 'M','a','x'/
      DATA MINNIE/
     1 'M','i','n'/
      DATA RATIO/
     1 'R','a','t','i','o'/
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPS(TERPZ)=((TERPZ-XYPLAN(1,1))*XINCH2+
     1 (XYPLAN(2,1)-TERPZ)*XINCH1)/DXYP(1)
c-----------------------------------------------------------------------
C
C     NO ARROWS UNLESS THEY ARE REQUESTED.
C
c-----------------------------------------------------------------------
      IF(NRATIO.LE.0) GO TO 150
c-----------------------------------------------------------------------
C
C     NO ARROWS IF NO MAX AND MIN.
C
c-----------------------------------------------------------------------
      IF(XMAX.LE.0.0d0.AND.XMIN.LE.0.0d0) GO TO 150
C-----DEFINE COLOR.
      CALL PEN(2)
C-----DEFINE SIZE OF ARROW HEAD.
      HEAD=HT0P5
C-----DEFINE SPACE REQUIRED FOR MESSAGES.
      XSPACE=6.0*HTX1
      XSMIN=7.0*HTX1
C-----DEFINE ENDS OF ARROW.
      YPMIN=YL(12)
      YPMAX=YL(7)
C-----DEFINE WHERE MESSAGES WILL BE.
      YP1=YL(10)
      YP2=YL(11)
c-----------------------------------------------------------------------
C
C     MAXIMUM ARROW.
C
c-----------------------------------------------------------------------
C-----NO ARROW IF ENERGY IS NOT POSITIVE.
      IF(XMAX.LE.0.0d0) GO TO 10
C-----DEFINE COORDINATES FOR INTERNAL USE.
      XPMAX=XMAX
C-----IF LOG SCALING TRANSFORM TO LOG.
      IF(IXYWAY(1).EQ.2) XPMAX=DLOG10(XPMAX)
      IF(XPMAX.LE.XYPLAN(1,1).OR.XPMAX.GE.XYPLAN(2,1)) GO TO 10
C-----TRANSFORM TO COORDINATES OF PLOT.
      XPMAX=TERPS(XPMAX)
C-----PLOT ARROW AT UPPER LIMIT.
      CALL PLOT4(XPMAX,YPMIN,3)
      CALL PLOT4(XPMAX,YPMAX,2)
      CALL PLOT4(XPMAX-HEAD,YPMAX-HEAD,2)
      CALL PLOT4(XPMAX,YPMAX,3)
      CALL PLOT4(XPMAX+HEAD,YPMAX-HEAD,2)
c-----------------------------------------------------------------------
C
C     MINIMUM ARROW.
C
c-----------------------------------------------------------------------
C-----NO ARROW IF ENERGY IS NOT POSITIVE.
   10 IF(XMIN.LE.0.0d0) GO TO 30
C-----DEFINE COORDINATES FOR INTERNAL USE.
      XPMIN=XMIN
C-----IF LOG SCALING TRANSFORM TO LOG.
      IF(IXYWAY(1).EQ.2) XPMIN=DLOG10(XPMIN)
      IF(XPMIN.LE.XYPLAN(1,1).OR.XPMIN.GE.XYPLAN(2,1)) GO TO 30
C-----TRANSFORM TO COORDINATES OF PLOT.
      XPMIN=TERPS(XPMIN)
C-----PLOT ARROW AT LOWER LIMIT.
      CALL PLOT4(XPMIN,YPMAX,3)
      CALL PLOT4(XPMIN,YPMIN,2)
      CALL PLOT4(XPMIN-HEAD,YPMIN+HEAD,2)
      CALL PLOT4(XPMIN,YPMIN,3)
      CALL PLOT4(XPMIN+HEAD,YPMIN+HEAD,2)
c-----------------------------------------------------------------------
C
C     DECIDE WHERE TO PRINT MAX AND MIN MESSAGES.
C
c-----------------------------------------------------------------------
      XPMINO=XINCH1-HTX1
      XPMAXO=XPMINO
      IF(XMAX.GT.0.0d0) GO TO 50
c-----------------------------------------------------------------------
C
C     ONLY MINIMUM RATIO. PLOT MESSAGE TOWARD MIDDLE OF FRAME.
C
c-----------------------------------------------------------------------
      XPAV=0.5d0*(XYGRID(1,1)+XYGRID(2,1))
      IF(XPMIN.GT.XPAV) GO TO 20
      XPMINO=XPMIN+HTX1
      GO TO 130
   20 XPMINO=XPMIN-XSPACE
      GO TO 130
c-----------------------------------------------------------------------
C
C     ONLY MAXIMUM RATIO. PLOT MESSAGE TOWARD MIDDLE OF FRAME.
C
c-----------------------------------------------------------------------
   30 IF(XMAX.LE.0.0d0) GO TO 130
      XPAV=0.5d0*(XYGRID(1,1)+XYGRID(2,1))
      IF(XPMAX.GT.XPAV) GO TO 40
      XPMAXO=XPMAX+HTX1
      GO TO 130
   40 XPMAXO=XPMAX-XSPACE
      GO TO 130
c-----------------------------------------------------------------------
C
C     BOTH MINIMUM AND MAXIMUM RATIOS.
C
c-----------------------------------------------------------------------
C-----DEFINE ASCENDING ORDER.
   50 IF(XPMIN.gt.XPMAX) go to 60
      IWAY=1
      XP1=XPMIN
      XP2=XPMAX
      GO TO 70
   60 IWAY=2
      XP1=XPMAX
      XP2=XPMIN
c-----------------------------------------------------------------------
C
C     IF ROOM BETWEEN THEM PLOT BOTH BETWEEN ARROWS
C
c-----------------------------------------------------------------------
   70 IF((XP2-XP1).LT.2.0*XSMIN) GO TO 90
      IF(IWAY.EQ.2) GO TO 80
C-----MIN IS TO THE LEFT OF MAX.
      XPMINO=XP1+HTX1
      XPMAXO=XP2-XSPACE
      GO TO 130
C-----MAX IS TO THE LEFT OF MIN.
   80 XPMAXO=XP1+HTX1
      XPMINO=XP2-XSPACE
      GO TO 130
c-----------------------------------------------------------------------
C
C     IF ROOM PLOT BOTH OUTSIDE ARROWS.
C
c-----------------------------------------------------------------------
   90 XPSTAR=XP1-XSPACE
      XPEND=XP2+XSPACE
      IF(XPSTAR.LE.XINCH1.OR.XPEND.GE.XINCH2) GO TO 110
      IF(IWAY.EQ.2) GO TO 100
C-----MIN IS TO THE LEFT OF MAX.
      XPMINO=XPSTAR
      XPMAXO=XP2+HTX1
      GO TO 130
C-----MAX IS TO THE LEFT OF MIN.
  100 XPMAXO=XPSTAR
      XPMINO=XP2+HTX1
      GO TO 130
c-----------------------------------------------------------------------
C
C     THEY ARE CLOSE TOGETHER AND NEAR ONE X BORDER. SELECT AT
C     LEAST ONE TO PLOT OUTSIDE.
C
c-----------------------------------------------------------------------
  110 IF(XPSTAR.LE.XINCH1) GO TO 120
C-----PLOT LOWER ONE OUTSIDE.
      IF(IWAY.EQ.1) XPMINO=XPSTAR
      IF(IWAY.EQ.2) XPMAXO=XPSTAR
C-----TRY TO PLOT UPPER INSIDE.
      IF((XP2-XP1).LE.XSMIN) GO TO 130
      IF((XP2-XP1).LE.XSMIN) GO TO 130
      IF(IWAY.EQ.1) XPMAXO=XP2-XSPACE
      IF(IWAY.EQ.2) XPMINO=XP2-XSPACE
      GO TO 130
  120 IF(XPEND.GE.XINCH2) GO TO 130
C-----PLOT UPPER ONE OUTSIDE.
      IF(IWAY.EQ.1) XPMAXO=XP2+HTX1
      IF(IWAY.EQ.2) XPMINO=XP2+HTX1
C-----TRY TO PLOT LOWER INSIDE.
      IF((XP2-XP1).LE.XSMIN) GO TO 130
      IF(IWAY.EQ.1) XPMINO=XP1+HTX1
      IF(IWAY.EQ.2) XPMAXO=XP1+HTX1
c-----------------------------------------------------------------------
C
C     PLOT LABELS THAT ARE ON PLOT.
C
c-----------------------------------------------------------------------
  130 XPEND=XPMINO+XSPACE
      IF(XPMINO.LE.XINCH1.OR.XPEND.GE.XINCH2) GO TO 140
      CALL SYMBL1(XPMINO,YP1,HTX1,MINNIE(1),0.0,3)
      CALL SYMBL1(XPMINO,YP2,HTX1,RATIO(1) ,0.0,5)
  140 XPEND=XPMAXO+XSPACE
      IF(XPMAXO.LE.XINCH1.OR.XPEND.GE.XINCH2) GO TO 150
      CALL SYMBL1(XPMAXO,YP1,HTX1,MAXIE(1),0.0,3)
      CALL SYMBL1(XPMAXO,YP2,HTX1,RATIO(1),0.0,5)
  150 RETURN
      END
      SUBROUTINE UNITY
C=======================================================================
C
C     IF RATIO IS PLOTTED PLOT CENTER LINE DASH AT A RATIO OF UNITY.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      COMMON/GPP30/STEPER(7,5),LINTYP
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPS(TERPZ)=((TERPZ-XYPLAN(1,2))*YINCH2+
     1 (XYPLAN(2,2)-TERPZ)*YINCH1)/DXYP(2)
c-----------------------------------------------------------------------
C
C     DO NOT PLOT UNLESS OPTION IS ON.
C
c-----------------------------------------------------------------------
      IF(NUNITY.LE.0) GO TO 80
C-----DEFINE COORDINATE OF UNITY AND ERROR BAND RATIOS.
      YP=1.0
      YPP=DIFMAX
      YPM=DIFMIN
C-----IF LOG SCALING TRANSFORM TO LOG.
      IF(IXYWAY(2).NE.2) GO TO 10
      YP=0.0d0
      YPP=DLOG10(YPP)
      YPM=DLOG10(YPM)
C-----TRANSFORM TO COORDINATES OF PLOT.
   10 YP=TERPS(YP)
      YPP=TERPS(YPP)
      YPM=TERPS(YPM)
C-----SET LINE TYPE TO CENTER LINE DASH.
      LINTYP=3
c-----------------------------------------------------------------------
C
C     PLOT DASH LINES IF THEY ARE ALL WITHIN PLOTTING AREA.
C
C     DRAW LINE EITHER FROM LOWER UP UPPER X LIMIT, OR UPPER X LIMIT OF
C     LEGEND BOX TO UPPER X LIMIT.
C
c-----------------------------------------------------------------------
      IF(YP.LT.YINCH1.OR.YP.GT.YINCH2) GO TO 30
      XII=XINCH1
      IF(YP.GT.BOXLIM(2,2,LFRAME).OR.
     1   YP.LT.BOXLIM(1,2,LFRAME)) GO TO 20
      XII=BOXLIM(2,1,LFRAME)
   20 CALL DASH(XII,YP,3)
      CALL DASH(XINCH2,YP,2)
   30 IF(YPP.LT.YINCH1.OR.YPP.GT.YINCH2) GO TO 50
      XII=XINCH1
      IF(YPP.GT.BOXLIM(2,2,LFRAME).OR.
     1   YPP.LT.BOXLIM(1,2,LFRAME)) GO TO 40
      XII=BOXLIM(2,1,LFRAME)
   40 CALL DASH(XII,YPP,3)
      CALL DASH(XINCH2,YPP,2)
   50 IF(YPM.LT.YINCH1.OR.YPM.GT.YINCH2) GO TO 70
      XII=XINCH1
      IF(YPM.GT.BOXLIM(2,2,LFRAME).OR.
     1   YPM.LT.BOXLIM(1,2,LFRAME)) GO TO 60
      XII=BOXLIM(2,1,LFRAME)
   60 CALL DASH(XII,YPM,3)
      CALL DASH(XINCH2,YPM,2)
C-----RE-SET LINE TYPE TO SOLID.
   70 LINTYP=1
   80 RETURN
      END
      SUBROUTINE REGION
C=======================================================================
C
C     INDICATE RESOLVED AND UNRESOLVED RESONANCE REGIONS, IF ANY.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 RTITLE
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP8/XMIN,XMAX,DIFMIN,DIFMAX,RLIMIT(2,2)
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP26/XYDATA(2,2)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      COMMON/GPP30/STEPER(7,5),LINTYP
      DIMENSION RTITLE(10,2),NTITLE(3,2)
      DATA RTITLE/
     1 'R','e','s','o','l','v','e','d',' ',' ',
     1 'U','n','r','e','s','o','l','v','e','d'/
      DATA NTITLE/8,3,1,
     1 10,5,1/
      DATA XP2A/0.0d0/
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPS(TERPZ)=((TERPZ-XYPLAN(1,1))*XINCH2+
     1 (XYPLAN(2,1)-TERPZ)*XINCH1)/DXYP(1)
c-----------------------------------------------------------------------
C
C     ONLY SHOW LIMITS IF OPTION IS ON.
C
c-----------------------------------------------------------------------
      IF(NREST.LE.0) GO TO 70
C-----DEFINE COLOR, LINE TYPE AND THICKNESS.
      CALL PEN(2)
      LINTYP=1
C-----DEFINE SIZE OF LINES AND ARROWS.
      HEAD=HT0P5
      YP2=YL(8)
      YP1=YL(9)
c-----------------------------------------------------------------------
C
C     SET UP LOOP OVER RESOLVED AND UNRESOLVED REGIONS.
C
c-----------------------------------------------------------------------
      DO 60 IREG=1,2
C-----DEFINE COORDINATES FOR INTERNAL USE.
      XP1=RLIMIT(1,IREG)
      XP2=RLIMIT(2,IREG)
C-----NO ARROW IF NO REGION.
      IF(XP2.LE.XP1) GO TO 60
C-----NO ARROW IF RANGE IS OUTSIDE RANGE OF PLOT.
      IF(XP1.GE.XYDATA(2,1).OR.XP2.LE.XYDATA(1,1)) GO TO 60
C-----RESTRICT LIMITS TO RANGE OF PLOT.
      IF(XP1.LT.XYDATA(1,1)) XP1=XYDATA(1,1)
      IF(XP2.GT.XYDATA(2,1)) XP2=XYDATA(2,1)
C-----IF LOG SCALING TRANSFORM TO LOG.
      IF(IXYWAY(1).EQ.2) XP1=DLOG10(XP1)
      IF(IXYWAY(1).EQ.2) XP2=DLOG10(XP2)
C-----TRANSFORM TO COORDINATES OF PLOT.
      XP1=TERPS(XP1)
      XP2=TERPS(XP2)
      IF(RLIMIT(1,IREG).LT.XYDATA(1,1)) GO TO 20
C-----PLOT VERTICAL LINE AT LOWER X LIMIT IF IT IS NOT THE SAME AS THE
C-----UPPER X LIMIT OF PRECEDING REGION.
      IF(IREG.EQ.2.AND.XP1.EQ.XP2A) GO TO 10
      LINTYP=3
      CALL DASHV(XP1)
      LINTYP=1
C-----PLOT ARROW HEAD AT LOWER X LIMIT IF LOWER LIMIT IS ON PLOT.
   10 CALL PLOT4(XP1+HEAD,YP1+HEAD,3)
      CALL PLOT4(XP1,YP1,2)
      CALL PLOT4(XP1+HEAD,YP1-HEAD,2)
C-----PLOT HORIZONTAL LINE FROM LOWER TO UPPER LIMIT.
   20 CALL PLOT4(XP1,YP1,3)
      CALL PLOT4(XP2,YP1,2)
C-----PLOT ARROW HEAD AT UPPER X LIMIT IF UPPER LIMIT IS ON PLOT
      IF(RLIMIT(2,IREG).GT.XYDATA(2,1)) GO TO 30
      CALL PLOT4(XP2-HEAD,YP1+HEAD,3)
      CALL PLOT4(XP2,YP1,2)
      CALL PLOT4(XP2-HEAD,YP1-HEAD,2)
C-----PLOT VERTICAL LINE AT UPPER X LIMIT.
      LINTYP=3
      CALL DASHV(XP2)
      LINTYP=1
C-----DEFINE WIDTH OF RESONANCE REGION IN UNITS OF CHARACTERS.
   30 NWIDTH=((XP2-XP1)-2*HEAD)/HTX1
C-----SELECT LENGTH OF TITLE TO PLOT.
      DO 40 I=1,3
      IF(NWIDTH.GE.NTITLE(I,IREG)) GO TO 50
   40 CONTINUE
C-----NO ROOM TO IDENTIFY RESONANCE REGION.
      GO TO 60
C-----IDENTIFY RESONANCE REGION.
   50 XP=0.5d0*(XP2+XP1-HTX1*FLOAT(NTITLE(I,IREG)))
      CALL SYMBL1(XP,YP2,HTX1,RTITLE(1,IREG),0.0,NTITLE(I,IREG))
   60 XP2A=XP2
   70 RETURN
      END
      SUBROUTINE BOXLAB(LTITLE,LTYPE)
C=======================================================================
C
C     IDENTIFY CURVE IN LEGEND BOX.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 TITLES
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP6/TITLES(84,30)
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP23/BOXREL(9)
c-----------------------------------------------------------------------
C
C     NO I.D. IF NO LEGEND BOX ON PLOT.
C
c-----------------------------------------------------------------------
      IF(NOBOX(LFRAME).NE.0.OR.BIXMIN.LE.0.0d0) GO TO 30
c-----------------------------------------------------------------------
C
C     SELECT CURVE OR SET OF POINTS.
C
c-----------------------------------------------------------------------
      IF(LTYPE.EQ.2) GO TO 10
c-----------------------------------------------------------------------
C
C     CURVE.
C
c-----------------------------------------------------------------------
C-----DEFINE LENGTH OF I.D.
      CALL SPECAL(TITLES(1,LTITLE),IT,ITP,84)
C-----PLOT LINE TYPE.
      YP=BOXREL(8)+HT0P5
      CALL DASH(BOXREL(2),YP,3)
      CALL DASH(BOXREL(3),YP,2)
      GO TO 20
c-----------------------------------------------------------------------
C
C     SET OF POINTS.
C
c-----------------------------------------------------------------------
C-----TEMPORARILY MOVE LEGEND BOX OFF PLOT.
   10 SAVMIN=BIXMIN
      SAVMAX=BIXMAX
      BIXMIN=-10.0d0
      BIXMAX=BIXMIN
C-----PLOT SYMBOL.
      XP=BOXREL(4)
      YP=BOXREL(8)+HT0P5
      CALL SYMREF(XP,XP,XP,YP,YP,YP,LTITLE)
C-----RESTORE BOX.
      BIXMIN=SAVMIN
      BIXMAX=SAVMAX
c-----------------------------------------------------------------------
C
C     PLOT I.D.
C
c-----------------------------------------------------------------------
C-----DEFINE LENGTH OF I.D.
   20 CALL SPECAL(TITLES(1,LTITLE),IT,ITP,84)
      CALL SYMBL1(BOXREL(5),BOXREL(8),HT,TITLES(1,LTITLE),0.0,IT)
C-----MOVE DOWN TO NEXT LINE IN BOX.
      BOXREL(8)=BOXREL(8)-HT1P75
   30 RETURN
      END
      SUBROUTINE LABLIT
C=======================================================================
C
C     PLOT ALL HEADER AND FOOTER INFORMATION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 TIT1L,TIT1M,TIT1R,TIT2L,TIT2M,TIT2R,FOOTL,FOOTM,FOOTR
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP5/TIT1L(80),TIT1M(80),TIT1R(80),TIT2L(80),TIT2M(80),
     1 TIT2R(80),FOOTL(80),FOOTM(80),FOOTR(80)
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      XMID=0.5d0*(XINCH1+XINCH2)
c-----------------------------------------------------------------------
C
C     TOP LINE.
C
c-----------------------------------------------------------------------
      CALL SPECAL(TIT1L,IT,ITP,80)
      IF(IT.LE.0) GO TO 10
      CALL SYMBL1(XINCH1,YL(1),HT,TIT1L(1),0.0,IT)
   10 CALL SPECAL(TIT1M,IT,ITP,80)
      IF(IT.LE.0) GO TO 20
      XP=XMID-0.5d0*FLOAT(ITP)*WD
      CALL SYMBL1(XP,YL(1),HT,TIT1M(1),0.0,IT)
   20 CALL SPECAL(TIT1R,IT,ITP,80)
      IF(IT.LE.0) GO TO 30
      XP=XINCH2-FLOAT(ITP)*WD
      CALL SYMBL1(XP,YL(1),HT,TIT1R(1),0.0,IT)
c-----------------------------------------------------------------------
C
C     NEXT LINE.
C
c-----------------------------------------------------------------------
   30 CALL SPECAL(TIT2L,IT,ITP,80)
      IF(IT.LE.0) GO TO 40
      CALL SYMBL1(XINCH1,YL(2),HT,TIT2L(1),0.0,IT)
   40 CALL SPECAL(TIT2M,IT,ITP,80)
      IF(IT.LE.0) GO TO 50
      XP=XMID-0.5d0*FLOAT(ITP)*WD
      CALL SYMBL1(XP,YL(2),HT,TIT2M(1),0.0,IT)
   50 CALL SPECAL(TIT2R,IT,ITP,80)
      IF(IT.LE.0) GO TO 60
      XP=XINCH2-FLOAT(ITP)*WD
      CALL SYMBL1(XP,YL(2),HT,TIT2R(1),0.0,IT)
c-----------------------------------------------------------------------
C
C     FOOTER LINE.
C
c-----------------------------------------------------------------------
   60 CALL SPECAL(FOOTL,IT,ITP,80)
      IF(IT.LE.0) GO TO 70
      CALL SYMBL1(XINCH1,YL(5),HT,FOOTL(1),0.0,IT)
   70 CALL SPECAL(FOOTM,IT,ITP,80)
      IF(IT.LE.0) GO TO 80
      XP=XMID-0.5d0*FLOAT(ITP)*WD
      CALL SYMBL1(XP,YL(5),HT,FOOTM(1),0.0,IT)
   80 CALL SPECAL(FOOTR,IT,ITP,80)
      IF(IT.LE.0) GO TO 90
      XP=XINCH2-FLOAT(ITP)*WD
      CALL SYMBL1(XP,YL(5),HT,FOOTR(1),0.0,IT)
c-----------------------------------------------------------------------
C
C     PAGE NUMBER IN LOWER LEFT HAND CORNER.
C
c-----------------------------------------------------------------------
   90 IF(NPAGE.LE.0) GO TO 100
      PAGE=NPAGE
      CALL NUMBR1(XINCH1,YL(5),HT,PAGE,0.0,-1)
      NPAGE=NPAGE+1
      GO TO 110
C-----COUNT PLOTS.
  100 NPAGE=NPAGE-1
  110 RETURN
      END
      SUBROUTINE FRAME1
C=======================================================================
C
C     INITIALIZE PLOTTER AND IDENTIFY PROGRAM.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 VERSES
      CHARACTER*32 VERSE1
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      COMMON/GPP31/XOFF,YOFF,XSCALE,YSCALE,XSIZE,YSIZE,XBASIS
      COMMON/GPP32/ISMALX,ISMALY
      COMMON/PALCOM/MYPAL
      DIMENSION VERSES(32,14),VERSE1(14),
     1 NVERSE(14),BOXZX(2),BOXZY(2)
      EQUIVALENCE (VERSES(1,1),VERSE1(1))
      DATA HTV/0.28d0/
C-----DEFINE IDENTIFICATION FOR PROGRAM AND VERSION BY,
C-----(1) KVERSE - NUMBER OF LINES IN I.D.
C-----(3) VERSES - CHARACTERS FOR I.D.
      DATA KVERSE/14/
C       12345678901234567890123456789032
      DATA VERSE1/
     1 'Program Complot                 ',
     2 '(Version 2019-1)                ',
     3 'by                              ',
     4 'Dermott E. Cullen               ',
     5 '(Present Contact Information)   ',
     6 'Dermott E. Cullen               ',
     7 '1466 Hudson Way                 ',
     8 'Livermore, CA 94550             ',
     9 'U.S.A.                          ',
     A 'Tele: 925-443-1911              ',
     1 'E.Mail:redcullen1@comcast.net   ',
     2 'Web:redcullen1.net/HOMEPAGE.NEW ',
     3 '                                ',
     4 'Press Mouse Button to Start     '/
C-----ONLY ALLOW PLOTTER TO BE INITIALIZED ONCE.
      DATA IPASS/0/
C-----DEFINE THE NUMBER OF CHARACTERS IN EACH LINE
      DO 10 K=1,KVERSE
   10 CALL LONGX(VERSES(1,K),NVERSE(K),32)
c-----------------------------------------------------------------------
C
C     IF PLOT IS RE-ORIENTED SWITCH X AND Y LOCAL PLOTTER LIMITS.
C
c-----------------------------------------------------------------------
      IF(NFLIP.LE.0) GO TO 30
      DO 20 I=1,2
      XYDUM=XYPLOT(I,1)
      XYPLOT(I,1)=XYPLOT(I,2)
   20 XYPLOT(I,2)=XYDUM
c-----------------------------------------------------------------------
C
C     DEFINE PLOT SIZE, SCALING FACTORS, OFFSET AND FRAME ADVANCE.
C
c-----------------------------------------------------------------------
   30 DX=XYPLOT(2,1)-XYPLOT(1,1)
      DY=XYPLOT(2,2)-XYPLOT(1,2)
      XSIZE=DX/FLOAT(NSMALX)
      IF(NFLIP.NE.0) XSIZE=-XSIZE
      YSIZE=DY/FLOAT(NSMALY)
      XSCALE=XSIZE/XYEDGE(2,1)
      YSCALE=YSIZE/XYEDGE(2,2)
      XBASIS=XYPLOT(1,1)
      IF(NFLIP.NE.0) XBASIS=XYPLOT(2,1)
      XOFF=XBASIS
      YOFF=XYPLOT(1,2)
      ISMALX=NSMALX
      ISMALY=NSMALY
c-----------------------------------------------------------------------
C
C     INITIALIZE PLOTTER
C
c-----------------------------------------------------------------------
      IF(IPASS.NE.0) GO TO 40
C-----INITIALIZE PLOTTER.
      CALL STARPLOT
C-----OPTIONAL WHITE BACKGROUND
      IF(MYPAL.NE.0) CALL NEWBACK
   40 IPASS=IPASS+1
c-----------------------------------------------------------------------
C
C     IDENTIFY PROGRAM AND INSTALLATION ONLY THE FIRST TIME THIS
C     ROUTINE IS CALLED.
C
c-----------------------------------------------------------------------
      IF(IPASS.NE.1) GO TO 80
C-----DEFINE COLOR.
      CALL PEN(2)
C-----IF REQUESTED DRAW OUTLINE FOR ENTIRE PLOTTING AREA.
      IF(NBORD.LE.0) GO TO 50
      ITHICK=MTHICK(3)
      CALL PLOT4(XYEDGE(1,1),XYEDGE(1,2),3)
      CALL PLOT4(XYEDGE(1,1),XYEDGE(2,2),2)
      CALL PLOT4(XYEDGE(2,1),XYEDGE(2,2),2)
      CALL PLOT4(XYEDGE(2,1),XYEDGE(1,2),2)
      CALL PLOT4(XYEDGE(1,1),XYEDGE(1,2),2)
   50 Y=0.5d0*(XYEDGE(2,2)+HTV*FLOAT(KVERSE)+0.75d0*HTV*FLOAT(KVERSE-1))
      XMID=0.5d0*XYEDGE(2,1)
C-----OUTLINE ID.
      LL=NVERSE(1)
      DO 60 L=1,KVERSE
      IF(NVERSE(L).GT.LL) LL=NVERSE(L)
   60 CONTINUE
      XLL=LL
      BOXZX(1)=XMID-HTV*(0.5d0*XLL+2.0d0)
      BOXZX(2)=XMID+HTV*(0.5d0*XLL+3.0d0)
      BOXZY(1)=Y-(1.75d0*FLOAT(KVERSE-1)+2.0d0)*HTV
      BOXZY(2)=Y+3.0d0*HTV
      CALL COLORBOX(BOXZX,BOXZY,0,2)
C-----PLOT ID.
      DO 70 L=1,KVERSE
      X=XMID-0.5d0*FLOAT(NVERSE(L))*HTV
      CALL SYMBL1(X,Y,HTV,VERSES(1,L),0.0,NVERSE(L))
   70 Y=Y-1.75d0*HTV
      ITHICK=MTHICK(2)
C-----ADVANCE TO NEXT FRAME AND DEFINE OFFSET FOR FIRST PLOT ON FRAME.
      CALL FRAMED(1)
   80 RETURN
      END
      SUBROUTINE NOPLOTS
C=======================================================================
C
C     NOTIFY USERS THAT THERE WERE NO PLOTS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 VERSES
      CHARACTER*32 VERSE1
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      COMMON/GPP31/XOFF,YOFF,XSCALE,YSCALE,XSIZE,YSIZE,XBASIS
      COMMON/GPP32/ISMALX,ISMALY
      DIMENSION VERSES(32,11),VERSE1(11),
     1 NVERSE(11),BOXZX(2),BOXZY(2)
      EQUIVALENCE (VERSES(1,1),VERSE1(1))
      DATA HTV/0.28d0/
C-----DEFINE IDENTIFICATION FOR PROGRAM AND VERSION BY,
C-----(1) KVERSE - NUMBER OF LINES IN I.D.
C-----(3) VERSES - CHARACTERS FOR I.D.
      DATA KVERSE/11/
C       12345678901234567890123456789032
      DATA VERSE1/
     1 '                                ',
     2 '                                ',
     3 'All Data Agreed within input    ',
     4 'Allowable Uncertainty.          ',
     5 'So NO plots were generated.     ',
     6 'See the output file COMPLOT.LST ',
     7 'for Details of the Comparisons. ',
     8 '                                ',
     9 '                                ',
     A 'Press Mouse Key to Terminate.   ',
     1 '                                '/
c-----------------------------------------------------------------------
C
C     PRINT MESSAGE.
C
c-----------------------------------------------------------------------
C-----DEFINE THE NUMBER OF CHARACTERS IN EACH LINE
      DO 10 K=1,KVERSE
   10 CALL LONGX(VERSES(1,K),NVERSE(K),32)
C-----DEFINE COLOR.
      CALL PEN(2)
      Y=0.5d0*(XYEDGE(2,2)+HTV*FLOAT(KVERSE)+0.75d0*HTV*FLOAT(KVERSE-1))
      XMID=0.5d0*XYEDGE(2,1)
C-----OUTLINE ID.
      LL=NVERSE(1)
      DO 20 L=1,KVERSE
      IF(NVERSE(L).GT.LL) LL=NVERSE(L)
   20 CONTINUE
      XLL=LL
      BOXZX(1)=XMID-HTV*(0.5d0*XLL+2.0d0)
      BOXZX(2)=XMID+HTV*(0.5d0*XLL+3.0d0)
      BOXZY(1)=Y-(1.75d0*FLOAT(KVERSE-1)+2.0d0)*HTV
      BOXZY(2)=Y+3.0d0*HTV
      CALL COLORBOX(BOXZX,BOXZY,0,2)
C-----PLOT ID.
      DO 30 L=1,KVERSE
      X=XMID-0.5d0*FLOAT(NVERSE(L))*HTV
      CALL SYMBL1(X,Y,HTV,VERSES(1,L),0.0,NVERSE(L))
   30 Y=Y-1.75d0*HTV
      ITHICK=MTHICK(2)
C-----ADVANCE TO NEXT FRAME AND DEFINE OFFSET FOR FIRST PLOT ON FRAME.
      CALL FRAMED(1)
      RETURN
      END
      SUBROUTINE FRAMED(MYDELAY)
C=======================================================================
C
C     ADVANCE TO NEXT PLOTTING AREA. THERE CAN BE ANY NUMBER OF PLOTS
C     IN X AND/OR Y DIRECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 XMOUSE,YMOUSE
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
      COMMON/GPP31/XOFF,YOFF,XSCALE,YSCALE,XSIZE,YSIZE,XBASIS
      COMMON/GPP32/ISMALX,ISMALY
C-----DELAY, IF REQUESTED.
      IF(MYDELAY.NE.0) CALL MOUSEY(KMOUSE,XMOUSE,YMOUSE,1,4)
C-----IF FRAME IS FULL ADVANCE TO NEXT FRAME.
      IF(ISMALX.LT.NSMALX.OR.ISMALY.LT.NSMALY) GO TO 10
      CALL NEXTPLOT
C-----INCREMENT INDEX IN X DIRECTION. RESET X POSITION WHEN X DIRECTION
C-----IS FULL.
   10 ISMALX=ISMALX+1
      XOFF=XOFF+XSIZE
      IF(ISMALX.LE.NSMALX) GO TO 20
C-----X DIRECTION IS FULL. RESET X INDEX AND POSITION. INCREMENT INDEX
C-----IN Y DIRECTION. WHEN Y DIRECTION IS FULL FRAME IS COMPLETE.
      ISMALX=1
      XOFF=XBASIS
      ISMALY=ISMALY+1
      YOFF=YOFF-YSIZE
      IF(ISMALY.LE.NSMALY) GO TO 20
      ISMALY=1
      YOFF=XYPLOT(2,2)-YSIZE
   20 RETURN
      END
      SUBROUTINE NUMBR1(XI,YI,HT,Z,ANG,IZ)
C=======================================================================
C
C     CONVERT FLOATING POINT NUMBER OF CHARACTER STRING AND PLOT WIDE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 ANG
      CHARACTER*1 DIGIT,MINUS,DOT,FIELD
      DIMENSION DIGIT(10),FIELD(16)
      DATA DIGIT/'1','2','3','4','5','6','7','8','9','0'/
      DATA MINUS/'-'/
      DATA DOT/'.'/
c-----------------------------------------------------------------------
C
C     ROUND-OFF NUMBER TO REQUIRED DIGITS, E.G. IF WRITING 54.321 WITH
C     2 DIGITS AFTER DECIMAL POINT DEFINE MR=5432 (ENTIRE STRING) AND
C     MR1=54 (DIGITS PRECEDING DECIMAL POINT).
C
c-----------------------------------------------------------------------
      AZ=DABS(Z)
      IIZ=IZ
      IF(IIZ.LE.0) IIZ=0
      Z10=10.0d0**IIZ
      MR=AZ*Z10+0.5d0
      IZ10=Z10
      MR1=MR/IZ10
c-----------------------------------------------------------------------
C
C     DETERMINE NUMBER OF DIGITS PRECEDING DECIMAL POINT.
C
c-----------------------------------------------------------------------
      M10=1
      DO 10 IDIG=1,12
      NR=MR1/M10
      IF(NR.LE.0) GO TO 20
   10 M10=10*M10
c-----------------------------------------------------------------------
C
C     NUMBER IS TOO BIG...NO PLOTTING.
C
c-----------------------------------------------------------------------
      RETURN
   20 IF(IDIG.EQ.1) GO TO 30
      IDIG=IDIG-1
      M10=M10/10
c-----------------------------------------------------------------------
C
C     ADD DIGITS AFTER DECIMAL POINT.
C
c-----------------------------------------------------------------------
   30 IDIG=IDIG+IIZ
      M10=M10*IZ10
c-----------------------------------------------------------------------
C
C     IF NUMBER IS ZERO, PLOT ZERO AND RETURN.
C
c-----------------------------------------------------------------------
      IF(IDIG.GT.0) GO TO 40
      IFIELD=1
      FIELD(1)=DIGIT(10)
      GO TO 80
c-----------------------------------------------------------------------
C
C     INITIALIZE CHARACTER COUNT.
C
c-----------------------------------------------------------------------
   40 IFIELD=0
c-----------------------------------------------------------------------
C
C     IF NUMBER IS NEGATIVE INSERT MINUS SIGN.
C
c-----------------------------------------------------------------------
      IF(Z.GE.0.0d0) GO TO 50
      FIELD(1)=MINUS
      IFIELD=1
c-----------------------------------------------------------------------
C
C     DEFINE POSITION OF DECIMAL POINT (IF ANY).
C
c-----------------------------------------------------------------------
   50 IDOT=IDIG-IZ
      IF(IZ.LT.0) IDOT=IDIG+2
c-----------------------------------------------------------------------
C
C     INSERT DIGITS AND DECIMAL POINT (IF ANY) IN STRING.
C
c-----------------------------------------------------------------------
      DO 70 I=1,IDIG
      NDIG=MR/M10
      KDIG=NDIG
      IF(KDIG.EQ.0) KDIG=10
      IFIELD=IFIELD+1
      FIELD(IFIELD)=DIGIT(KDIG)
c-----------------------------------------------------------------------
C
C     INSERT DECIMAL POINT AT APPROPRIATE POSITION.
C
c-----------------------------------------------------------------------
      IF(I.NE.IDOT) GO TO 60
      IFIELD=IFIELD+1
      FIELD(IFIELD)=DOT
   60 MR=MR-M10*NDIG
   70 M10=M10/10
c-----------------------------------------------------------------------
C
C     ENTIRE FIELD FORMATTED. PLOT IT.
C
c-----------------------------------------------------------------------
   80 CALL SYMBL1(XI,YI,HT,FIELD(1),ANG,IFIELD)
      RETURN
      END
      SUBROUTINE NUMBR2(Z,IZ,FIELD,IFIELD)
C=======================================================================
C
C     CONVERT FLOATING POINT NUMBER OF CHARACTER STRING AND RETURN IT.
C     Z     = FLOATING POINT NUMBER
C     IZ    = DIGITS AFTER DECIMAL POINT
C           = 0 - END WITH DECIMAL POINT
C           =-1 - NO DECIMAL POINT - OUTPUT LIKE INTEGER
C     FIELD = RETURNED CHARACTER STRING
C     IFIELD= NUMBER OF CHARACTERS RETURNED
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 DIGIT,MINUS,DOT,FIELD
      DIMENSION DIGIT(10),FIELD(16)
      DATA DIGIT/'1','2','3','4','5','6','7','8','9','0'/
      DATA MINUS/'-'/
      DATA DOT/'.'/
c-----------------------------------------------------------------------
C
C     ROUND-OFF NUMBER TO REQUIRED DIGITS, E.G. IF WRITING 54.321 WITH
C     2 DIGITS AFTER DECIMAL POINT DEFINE MR=5432 (ENTIRE STRING) AND
C     MR1=54 (DIGITS PRECEDING DECIMAL POINT).
C
c-----------------------------------------------------------------------
      AZ=DABS(Z)
      IIZ=IZ
      IF(IIZ.LE.0) IIZ=0
      Z10=10.0d0**IIZ
      MR=AZ*Z10+0.5d0
      IZ10=Z10
      MR1=MR/IZ10
c-----------------------------------------------------------------------
C
C     DETERMINE NUMBER OF DIGITS PRECEDING DECIMAL POINT.
C
c-----------------------------------------------------------------------
      M10=1
      DO 10 IDIG=1,12
      NR=MR1/M10
      IF(NR.LE.0) GO TO 20
   10 M10=10*M10
c-----------------------------------------------------------------------
C
C     NUMBER IS TOO BIG...NO PLOTTING.
C
c-----------------------------------------------------------------------
      RETURN
   20 IF(IDIG.EQ.1) GO TO 30
      IDIG=IDIG-1
      M10=M10/10
c-----------------------------------------------------------------------
C
C     ADD DIGITS AFTER DECIMAL POINT.
C
c-----------------------------------------------------------------------
   30 IDIG=IDIG+IIZ
      M10=M10*IZ10
c-----------------------------------------------------------------------
C
C     IF NUMBER IS ZERO, PLOT ZERO AND RETURN.
C
c-----------------------------------------------------------------------
      IF(IDIG.GT.0) GO TO 40
      IFIELD=1
      FIELD(1)=DIGIT(10)
      GO TO 80
c-----------------------------------------------------------------------
C
C     INITIALIZE CHARACTER COUNT.
C
c-----------------------------------------------------------------------
   40 IFIELD=0
c-----------------------------------------------------------------------
C
C     IF NUMBER IS NEGATIVE INSERT MINUS SIGN.
C
c-----------------------------------------------------------------------
      IF(Z.GE.0.0d0) GO TO 50
      FIELD(1)=MINUS
      IFIELD=1
c-----------------------------------------------------------------------
C
C     DEFINE POSITION OF DECIMAL POINT (IF ANY).
C
c-----------------------------------------------------------------------
   50 IDOT=IDIG-IZ
      IF(IZ.LT.0) IDOT=IDIG+2
c-----------------------------------------------------------------------
C
C     INSERT DIGITS AND DECIMAL POINT (IF ANY) IN STRING.
C
c-----------------------------------------------------------------------
      DO 70 I=1,IDIG
      NDIG=MR/M10
      KDIG=NDIG
      IF(KDIG.EQ.0) KDIG=10
      IFIELD=IFIELD+1
      FIELD(IFIELD)=DIGIT(KDIG)
c-----------------------------------------------------------------------
C
C     INSERT DECIMAL POINT AT APPROPRIATE POSITION.
C
c-----------------------------------------------------------------------
      IF(I.NE.IDOT) GO TO 60
      IFIELD=IFIELD+1
      FIELD(IFIELD)=DOT
   60 MR=MR-M10*NDIG
   70 M10=M10/10
c-----------------------------------------------------------------------
C
C     ENTIRE FIELD FORMATTED. RETURN IT.
C
c-----------------------------------------------------------------------
   80 RETURN
      END
      SUBROUTINE SYMBL1(X,Y,HTIN,MESS,ANG,NMESS)
C=======================================================================
C
C     DRAW THICK CHARACTERS DEFINED BY STROKES IN SYMBOL TABLE
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 ANG
      CHARACTER*1 MESS,CHRTAB,CHRTRL
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      COMMON/GPP40/CHRTAB(256),CHRTRL(256)
      COMMON/GPP41/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHAR,ICNTRL
      DIMENSION MESS(NMESS)
C-----INITIALIZE FLAG TO USE STANDARD CHARACTER SET.
      DATA IALTER/0/
C-----INITIALIZE X AND Y OFFSET (USED FOR SUB AND SUPER SCRIPTS).
      DATA XSET/0.0d0/
      DATA YSET/0.0d0/
C-----IF NO SOFTWARE CHARACTERS RETURN.
      IF(ICHAR.LE.0) RETURN
C-----SELECT LINE THICKNESS.
      IF(IMTHIN.LE.0) GO TO 10
      ITHICK=0
      GO TO 20
   10 ITHICK=MTHICK(2)
      IF(HTIN.LT.HT) ITHICK=MTHICK(1)
      IF(HTIN.GT.HT) ITHICK=MTHICK(3)
C-----INITIALIZE POSITION AND DEFINE INCREMENTS.
   20 X1=X
      Y1=Y
      NWAY=0
      IF(ABS(ANG).GT.1.0) NWAY=1
      IF(NWAY.NE.0) GO TO 30
      DX1=HTIN
      DY1=0.0d0
      GO TO 40
   30 DX1=0.0d0
      DY1=HTIN
C-----SET UP LOOP TO PLOT CHARACTERS ONE AT A TIME.
   40 DO 140 N=1,NMESS
C-----INITIALIZE COUNT OF THE NUMBER OF TIMES CHARACTER HAS BEEN FOUND
C-----(TO SELECT STANDARD OR ALTERNATE CHARACTER SET).
      NALTER=0
      DO 60 I=1,ICHAR
      IF(MESS(N).NE.CHRTAB(I)) GO TO 60
C-----ALWAYS USE CONTROL CHARACTERS REGARDLESS OF CHARACTER SET.
      I1=INDCHR(1,I)
      IF(ICHPEN(I1).LE.0) GO TO 70
C-----SELECT STANDARD OR ALTERNATE CHARACTER SET.
      IF(NALTER.NE.IALTER) GO TO 50
C-----CHARACTER FOUND.
      I2=INDCHR(2,I)
      GO TO 90
   50 NALTER=NALTER+1
   60 CONTINUE
C-----NOT SPECIAL CHARACTER...IGNORE.
      GO TO 140
C-----CONTROL CHARACTER...CHANGE CHARACTER SET OR RE-DEFINE OFFSET.
   70 IF(ICHPEN(I1).EQ.0) GO TO 80
C-----CHANGE CHARACTER SETS.
      IALTER=1-IALTER
      GO TO 140
C-----DEFINE OFFSET.
   80 XSET=XSET+XCHAR(I1)
      YSET=YSET+YCHAR(I1)
      GO TO 140
   90 IF(NWAY.NE.0) GO TO 110
C-----HORIZONTAL.
      DO 100 I=I1,I2
  100 CALL PLOT4(X1+HTIN*(XSET+XCHAR(I)),Y1+HTIN*(YSET+YCHAR(I)),
     1 ICHPEN(I))
C-----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSET
      X1=X1+XSET*HTIN
      GO TO 130
C-----VERTICAL.
  110 DO 120 I=I1,I2
  120 CALL PLOT4(X1-HTIN*(YSET+YCHAR(I)),Y1+HTIN*(XSET+XCHAR(I)),
     1 ICHPEN(I))
C-----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSET
      Y1=Y1+XSET*HTIN
C-----MOVE TO NEXT CHARACTER POSITION.
  130 X1=X1+DX1
      Y1=Y1+DY1
C-----TURN OFF ALTERNATE CHARACTER SET FLAG AND SET OFFSET TO ZERO.
      IALTER=0
      XSET=0.0d0
      YSET=0.0d0
  140 CONTINUE
      ITHICK=MTHICK(2)
      RETURN
      END
      SUBROUTINE DASHP(X2,Y2,IPEN,KPOINT)
C=======================================================================
C
C     STARTING FROM (X2,Y2) IN THE PLANE OF THE DATA TRANSFORM THE
C     COORDINATES TO THE PLANE OF THE PLOT AND PLOT ONE OF FIVE
C     POSSIBLE TYPES OF DASHED LINE FROM THE LAST POINT TO THE
C     COORDINATES DEFINED BY (X2,Y2). LINTYP DEFINES THE TYPE OF
C     DASHING. IF IT IS NOT A NUMBER BETWEEN 1 AND 5 A SOLID
C     LINE (TYPE 1) IS PLOTTED.
C
C     ASSUMING THE DATA IS LINEARLY INTERPOLABLE, IF THE PLOT IS NOT
C     LINEAR-LINEAR AND POINTS ARE TOO FAR APART INTERPOLATE DATA ONTO
C     THE PLOT IN SMALL STEPS TO OBTAIN A SMOOTH CURVE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP25/XYPLAN(2,2),NTDATA,NTPLAN,IMTERP
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP28/VIRTIN(2,2),DXYP(2),XYBASE(2),DXYBAS(2),IDEC(2),
     1 ISCALE(2),IDIGIT(2),IXYWAY(2)
      DATA Y1 /0.0d0/
      DATA YI1/0.0d0/
      DATA X1 /0.0d0/
      DATA XI1/0.0d0/
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPD(TERPZ)=((TERPZ-XYPLAN(1,1))*XINCH2+
     1 (XYPLAN(2,1)-TERPZ)*XINCH1)/DXYP(1)
      TERPS(TERP1)=((TERP1-XYPLAN(1,2))*YINCH2+
     1 (XYPLAN(2,2)-TERP1)*YINCH1)/DXYP(2)
C-----DEFINE COORDINATES FOR INTERNAL USE.
      XP=X2
      YP=Y2
C-----IF LOG SCALING TRANSFORM TO LOG AND THEN TRANSFORM TO COORDINATES
C-----OF THE PLOT.
      IF(IXYWAY(1).NE.2) GO TO 20
      IF(XP.GT.0.0d0) GO TO 10
      XP=XYPLAN(1,1)
      GO TO 20
   10 XP=DLOG10(XP)
   20 XI2=TERPD(XP)
      IF(IXYWAY(2).NE.2) GO TO 40
      IF(YP.GT.0.0d0) GO TO 30
      YP=XYPLAN(1,2)
      GO TO 40
   30 YP=DLOG10(YP)
   40 YI2=TERPS(YP)
C-----NO INTERPOLATION REQUIRED IF BEAM IS OFF OR PLOT IS LIN-LIN.
      IF(IPEN.NE.2.OR.IMTERP.EQ.0) GO TO 100
C-----COMPUTE INCH CHANGES IN X AND Y DIRECTION.
      DXI=XI2-XI1
      DYI=YI2-YI1
C-----NO INTERPOLATION IF X OR Y CHANGE IS LESS THAN ALLOWABLE
C-----SPACING.
      ABSDX=DABS(DXI)
      ABSDY=DABS(DYI)
      IF(ABSDX.LE.DXYMIN.OR.ABSDY.LE.DXYMIN) GO TO 100
C-----SELECT LARGEST INCH CHANGE. IF LESS THAN ALLOWABLE LENGTH
C-----NO INTERPOLATION REQUIRED.
      IF(ABSDX.LT.ABSDY) ABSDX=ABSDY
      IF(ABSDX.LE.DXYMIN) GO TO 100
C-----INTERPOLATION REQUIRED. DEFINE NUMBER AND SIZE OF STEP
      NSTEP=ABSDX/DXYMIN+1.0
      FSTEP=NSTEP
C-----DEFINE REQUIRED STEP CHANGE IN X.
      IF(IXYWAY(1).EQ.1) DSTEP=(X2-X1)/FSTEP
      IF(IXYWAY(1).EQ.2) DSTEP=DEXP(DLOG(X2/X1)/FSTEP)
c-----------------------------------------------------------------------
C
C     SPACE POINTS IN EQUAL INCH INCREMENTS ALONG X AXIS.
C
c-----------------------------------------------------------------------
C-----INITIALIZE COORDINATES.
      X0=X1
      Y0=Y1
C-----SET UP LOOP OVER STEPS.
      DO 90 LSTEP=1,NSTEP
C-----INSURE LAST STEP ENDS UP AT (X2,Y2).
      IF(LSTEP.NE.NSTEP) GO TO 50
      X0=X2
      Y0=Y2
      GO TO 80
C-----INCREMENT X IN EQUAL PLOT INCHES.
   50 IF(IXYWAY(1).EQ.2) GO TO 60
      X0=X0+DSTEP
      GO TO 70
   60 X0=X0*DSTEP
C-----DEFINE Y BY INTERPOLATION.
   70 Y0=TERPY(X0,X1,X2,Y1,Y2,NTDATA)
C-----IF LOG PLOT TRANSFORM COORDINATES.
   80 XP=X0
      YP=Y0
      IF(IXYWAY(1).EQ.2) XP=DLOG10(XP)
      IF(IXYWAY(2).EQ.2) YP=DLOG10(YP)
C-----TRANSFORM TO COORDINATES OF PLOT.
      XI2=TERPD(XP)
      YI2=TERPS(YP)
C-----MOVE BEAM TO NEXT POINT.
   90 CALL DASHX(XI2,YI2,IPEN)
      GO TO 110
C-----MOVE BEAM TO FINAL COORDINATES.
  100 CALL DASHX(XI2,YI2,IPEN)
C-----IF REQUESTED PLOT SMALL BOX AROUND DATA POINT.
  110 IF(KPOINT.LE.0) GO TO 130
C-----ONLY IDENTIFY POINT IF IT IS OUTSIDE THE LEGEND BOX.
      XI3=XI2-DXYMIN
      XI4=XI2+DXYMIN
      YI3=YI2-DXYMIN
      YI4=YI2+DXYMIN
      IF(XI4.LE.BIXMIN) GO TO 120
      IF(XI3.GE.BIXMAX) GO TO 120
      IF(YI4.LE.BIYMIN) GO TO 120
      IF(YI3.GE.BIYMAX) GO TO 120
      GO TO 130
  120 CALL DASH(XI3,YI3,3)
      CALL DASH(XI3,YI4,2)
      CALL DASH(XI4,YI4,2)
      CALL DASH(XI4,YI3,2)
      CALL DASH(XI3,YI3,2)
      CALL DASH(XI2,YI2,3)
C-----SAVE COORDINATES OF LAST POINT.
  130 X1=X2
      Y1=Y2
      XI1=XI2
      YI1=YI2
      RETURN
      END
      SUBROUTINE DASHX(XIN,YIN,IPEN)
C=======================================================================
C
C     DRAW THAT PORTION OF LINE FROM (XLAST,YLAST) TO (X,Y) WHICH IS
C     OUTSIDE THE LEGEND BOX.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      DATA XLAST/0.0d0/
      DATA YLAST/0.0d0/
C-----ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
      TERPL(X1,X2,X3,Y2,Y3)=(Y3*(X1-X2)+Y2*(X3-X1))/(X3-X2)
C-----DEFINE COORDINATES FOR INTERNAL USE.
      X=XIN
      Y=YIN
C-----IF NOT A DRAW PERFORM MOVE.
      IF(IPEN.NE.2) GO TO 120
c-----------------------------------------------------------------------
C
C     IF X IS DECREASING MOVE TO (X,Y), SWITCH (X,Y) AND (XLAST,YLAST),
C     DRAW STROKE IN OPPOSITE DIRECTION, MOVE TO (X,Y) AGAIN AT END.
C
c-----------------------------------------------------------------------
      ISWTCH=0
      IF(X.GE.XLAST) GO TO 10
      CALL DASH(X,Y,3)
      XT=X
      YT=Y
      X=XLAST
      Y=YLAST
      XLAST=XT
      YLAST=YT
      ISWTCH=1
c-----------------------------------------------------------------------
C
C     SEE IF ANY OVERLAP WITH LEGEND BOX.
C
c-----------------------------------------------------------------------
C-----X BELOW LOWER LIMIT OR XLAST ABOVE UPPER = NO OVERLAP.
   10 IF(X.LE.BIXMIN.OR.XLAST.GE.BIXMAX) GO TO 110
C-----Y AND YLAST BOTH ABOVE BOX = NO OVERLAP.
      IF(Y.GE.BIYMAX.AND.YLAST.GE.BIYMAX) GO TO 110
C-----Y AND YLAST BOTH BELOW BOX = NO OVERLAP.
      IF(Y.LE.BIYMIN.AND.YLAST.LE.BIYMIN) GO TO 110
c-----------------------------------------------------------------------
C
C     POSSIBLE OVERLAP.
C
c-----------------------------------------------------------------------
      IF(XLAST.GE.BIXMIN) GO TO 20
c-----------------------------------------------------------------------
C
C     DRAW AT LEAST TO LOWER X LIMIT OF BOX.
C
c-----------------------------------------------------------------------
      YLAST=TERPL(BIXMIN,X,XLAST,Y,YLAST)
      XLAST=BIXMIN
      CALL DASH(XLAST,YLAST,IPEN)
c-----------------------------------------------------------------------
C
C     XLAST IS AT LEAST AT LOWER X LIMIT OF BOX.
C     TEST ABOVE, WITHIN OR BELOW Y RANGE.
C
c-----------------------------------------------------------------------
   20 IF(YLAST.LE.BIYMAX) GO TO 30
c-----------------------------------------------------------------------
C
C     ABOVE
C
c-----------------------------------------------------------------------
C-----Y AND YLAST NOW BOTH ABOVE BOX = NO OVERLAP.
      IF(Y.GE.BIYMAX) GO TO 110
      XLAST=TERPL(BIYMAX,Y,YLAST,X,XLAST)
C-----IF TOP INTERCEPT IS BEYOND UPPER LIMIT OF BOX DRAW TO X.
      IF(XLAST.GE.BIXMAX) GO TO 110
C-----DRAW TO TOP INTERCEPT.
      YLAST=BIYMAX
      CALL DASH(XLAST,YLAST,IPEN)
      GO TO 40
   30 IF(YLAST.GE.BIYMIN) GO TO 40
c-----------------------------------------------------------------------
C
C     BELOW
C
c-----------------------------------------------------------------------
C-----Y AND YLAST NOW BOTH BELOW BOX = NO OVERLAP.
      IF(Y.LE.BIYMIN) GO TO 110
      XLAST=TERPL(BIYMIN,Y,YLAST,X,XLAST)
C-----IF BOTTOM INTERCEPT IS BEYOND UPPER LIMIT OF BOX DRAW TO X.
      IF(XLAST.GE.BIXMAX) GO TO 110
C-----DRAW TO BOTTOM INTERCEPT.
      YLAST=BIYMIN
      CALL DASH(XLAST,YLAST,IPEN)
c-----------------------------------------------------------------------
C
C     (XLAST,YLAST) WITHIN BOX. IF (X,Y) IS ALSO IN BOX MOVE TO IT.
C
c-----------------------------------------------------------------------
   40 IF(X.GT.BIXMAX) GO TO 50
C-----XLAST AND X WITHIN RANGE OF BOX. TEST FOR TOP/BOTTOM EMERGE.
      IF(Y.GT.BIYMAX) GO TO 70
      IF(Y.LT.BIYMIN) GO TO 60
C-----(XLAST,YLAST) AND (X,Y) BOTH WITHIN BOX. MOVE TO (X,Y).
      GO TO 120
C-----XLAST WITHIN AND X ABOVE RANGE OF BOX.
   50 IF(Y.GT.BIYMAX) GO TO 70
      IF(Y.LT.BIYMIN) GO TO 60
      GO TO 90
c-----------------------------------------------------------------------
C
C     (XLAST,YLAST) WITHIN BOX. CAN EMERGE THROUGH BOTTOM Y OR TOP X.
C
c-----------------------------------------------------------------------
   60 XLAST=TERPL(BIYMIN,Y,YLAST,X,XLAST)
C-----IF BOTTOM INTERCEPT IS BEYOND X TEST FOR EMERGE AT TOP X.
      IF(XLAST.GT.X) GO TO 80
c-----------------------------------------------------------------------
C
C     EMERGED FROM BOTTOM Y.
C
c-----------------------------------------------------------------------
      YLAST=BIYMIN
      GO TO 100
c-----------------------------------------------------------------------
C
C     (XLAST,YLAST) WITHIN BOX. CAN EMERGE THROUGH TOP Y OR TOP X.
C
c-----------------------------------------------------------------------
   70 XLAST=TERPL(BIYMAX,Y,YLAST,X,XLAST)
C-----IF TOP INTERCEPT IS BEYOND X TEST FOR EMERGE AT TOP X.
      IF(XLAST.GT.X) GO TO 80
c-----------------------------------------------------------------------
C
C     EMERGED FROM TOP Y.
C
c-----------------------------------------------------------------------
      YLAST=BIYMAX
      GO TO 100
   80 IF(X.LE.BIXMAX) GO TO 120
c-----------------------------------------------------------------------
C
C     EMERGED FROM TOP X.
C
c-----------------------------------------------------------------------
   90 YLAST=TERPL(BIXMAX,X,XLAST,Y,YLAST)
      XLAST=BIXMAX
c-----------------------------------------------------------------------
C
C     EMERGED FROM BOX.
C
c-----------------------------------------------------------------------
  100 CALL DASH(XLAST,YLAST,3)
c-----------------------------------------------------------------------
C
C     DRAW TO FINAL COORDINATES.
C
c-----------------------------------------------------------------------
  110 CALL DASH(X,Y,IPEN)
      GO TO 130
c-----------------------------------------------------------------------
C
C     MOVE TO FINAL LOCATION.
C
c-----------------------------------------------------------------------
  120 CALL DASH(X,Y,3)
c-----------------------------------------------------------------------
C
C     SAVE FINAL COORDINATES.
C
c-----------------------------------------------------------------------
  130 XLAST=XIN
      YLAST=YIN
c-----------------------------------------------------------------------
C
C     IF DIRECTION OF STROKE WAS REVERSED MOVE TO FINAL POSITION.
C
c-----------------------------------------------------------------------
      IF(ISWTCH.NE.0) CALL DASH(XIN,YIN,3)
      RETURN
      END
      SUBROUTINE DASHV(XIN)
C=======================================================================
C
C     DRAW A VERTICAL LINE AT XIN FROM THE TOP TO THE BOTTOM OF THE
C     PLOTTING AREA - ONLY THAT PORTION WHICH LIES OUTSIDE THE LEGEND
C     BOX.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
c-----------------------------------------------------------------------
C
C     START AT THE TOP OF THE PLOTTING AREA.
C
c-----------------------------------------------------------------------
      CALL DASH(XIN,XYGRID(2,2),3)
c-----------------------------------------------------------------------
C
C     LOOP OVER SUBPLOTS.
C
c-----------------------------------------------------------------------
      DO 20 L=1,NFRAME
c-----------------------------------------------------------------------
C
C     IF NOT WITHIN X RANGE OF LEGEND BOX DRAW LINE.
C
c-----------------------------------------------------------------------
      IF(XIN.LE.BOXLIM(1,1,L).OR.XIN.GE.BOXLIM(2,1,L)) GO TO 10
c-----------------------------------------------------------------------
C
C     DRAW DOWN TO TOP OF BOX AND MOVE TO BOTTOM OF BOX.
C
c-----------------------------------------------------------------------
      CALL DASH(XIN,BOXLIM(2,2,L),2)
      CALL DASH(XIN,BOXLIM(1,2,L),3)
c-----------------------------------------------------------------------
C
C     DRAW TO BOTTOM OF SUBPLOT.
C
c-----------------------------------------------------------------------
   10 CALL DASH(XIN,XYINCH(1,2,L),2)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE DASH(X,Y,IPEN)
C=======================================================================
C
C     PLOT ONE OF FIVE POSSIBLE TYPES OF DASHED LINE FROM THE LAST POINT
C     TO THE COORDINATES (X,Y). LINTYP DEFINES THE TYPE OF DASHING. IF
C     IT IS NOT A NUMBER BETWEEN 1 AND 5 A SOLID LINE (TYPE 1) IS DRAWN.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP30/STEPER(7,5),LINTYP
      DATA LPEN/3/
      DATA X2   /0.0d0/
      DATA Y2   /0.0d0/
      DATA STEP /0.1d0/
      DATA DLEFT/0.1d0/
      DATA KPEN/2/
C-----PLOT SOLID LINE IF ILLEGAL LINE TYPE SPECIFIED.
      IF(LINTYP.GT.1.AND.LINTYP.LE.5) GO TO 10
      CALL PLOT4(X,Y,IPEN)
      RETURN
C-----DEFINE PEN OPTION FOR INTERNAL USE.
   10 MPEN=IPEN
      IF(MPEN.eq.0) go to 20
      LPEN=MPEN
      GO TO 30
   20 MPEN=LPEN
C-----ADVANCE FRAME IF PEN OPTION NEGATIVE.
   30 IF(MPEN.eq.0) go to 50
      IF(MPEN.gt.0) go to 60
   40 CALL PLOT4(X,Y,MPEN)
   50 RETURN
C-----IF BEAM IS OFF INITIALIZE DASH PARAMETERS.
   60 IF(MPEN.lt.2) go to 50
      IF(MPEN.eq.2) go to 70
      X2=X
      Y2=Y
      ISTEP=1
      STEP=STEPER(1,LINTYP)
      DASHER=STEP
      KPEN=2
      GO TO 40
C-----CALCULATE DISTANCE TO NEXT POINT.
   70 D=DSQRT((X-X2)**2+(Y-Y2)**2)
      IF(D.LE.0.0d0) GO TO 110
C-----DEFINE DIRECTION COSINE AND SINE.
      COST=(Y-Y2)/D
      SINT=(X-X2)/D
C-----INITIALIZE DASH LENGTH AND DECREASE IF REMAINDER LESS THAN FULL
C-----DASH.
   80 DLEFT=STEP
      IF(DASHER.LT.DLEFT) DLEFT=DASHER
      IF(D.LT.DLEFT) DLEFT=D
C-----DEFINE NEW COORDINATE AND MOVE TO IT.
      X2=X2+DLEFT*SINT
      Y2=Y2+DLEFT*COST
      CALL PLOT4(X2,Y2,KPEN)
C-----SUBTRACT FROM LENGTH OF DASH.
      DASHER=DASHER-DLEFT
C-----IF END OF DASH FLIP BEAM AND RE-DEFINE LENGTH OF NEXT DASH.
      IF(DASHER.GT.0.0d0) GO TO 100
      KPEN=5-KPEN
      ISTEP=ISTEP+1
      IF(STEPER(ISTEP,LINTYP).GT.0.0d0) GO TO 90
      IF(ISTEP.LE.2) KPEN=2
      ISTEP=1
   90 STEP=STEPER(ISTEP,LINTYP)
      DASHER=STEP
C-----SUBTRACT LENGTH OF LINE PLOTTED AND DETERMINE IF ANY LEFT.
  100 D=D-DLEFT
      IF(D.GT.0.0d0) GO TO 80
C-----END OF LINE. DEFINE NEW POSITION.
  110 X2=X
      Y2=Y
      RETURN
      END
      SUBROUTINE PLOT4(X,Y,IPEN)
C=======================================================================
C
C     PLOT FROM LAST (X,Y) TO CURRENT (X,Y) EITHER NORMAL OR THICK LINE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP24/THICK,ITHICK,MTHICK(3),IMTHIN,IMFINE(2)
      DATA X2/0.0d0/
      DATA Y2/0.0d0/
C-----IF MOVE, NOT DRAW OR NORMAL LINE DO IT WITH 1 STROKE.
      IF(IPEN.NE.2.OR.ITHICK.LE.0) GO TO 30
      DXY=DSQRT((X-X2)**2+(Y-Y2)**2)
      IF(DXY.LE.0.0d0) GO TO 40
C-----DEFINE DIRECTION COSINE AND SINE.
      COST=(Y-Y2)/DXY
      SINT=(X-X2)/DXY
C-----DEFINE OFFSET FOR LINE THICKNESS.
      DXT=-THICK*COST
      DYT=THICK*SINT
C-----DRAW THICK LINE.
      CALL PLOTP(X,Y,2)
      DXT1=DXT
      DYT1=DYT
      DO 10 I=1,ITHICK
      CALL PLOTP(X2+DXT,Y2+DYT,3)
      CALL PLOTP(X+DXT,Y+DYT,2)
      DXT=DXT+DXT1
   10 DYT=DYT+DYT1
      DXT=DXT1
      DYT=DYT1
      DO 20 I=1,ITHICK
      CALL PLOTP(X2-DXT,Y2-DYT,3)
      CALL PLOTP(X-DXT,Y-DYT,2)
      DXT=DXT+DXT1
   20 DYT=DYT+DYT1
      CALL PLOTP(X,Y,3)
      GO TO 40
C-----DRAW NORMAL WIDTH LINE.
   30 CALL PLOTP(X,Y,IPEN)
   40 X2=X
      Y2=Y
      RETURN
      END
      SUBROUTINE PLOTP(XIN,YIN,IPEN)
C=======================================================================
C
C     TRANSLATE AND SCALE EACH STROKE TO THE PLANE OF THE PLOTTER AND
C     PLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 XP,YP
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP31/XOFF,YOFF,XSCALE,YSCALE,XSIZE,YSIZE,XBASIS
C-----PLOT WITH SELECTED ORIENTATION
      IF(NFLIP.NE.0) GO TO 10
C-----X HORIZONTAL/Y VERTICAL.
      XP=XSCALE*XIN+XOFF
      YP=YSCALE*YIN+YOFF
      CALL PLOT(XP,YP,IPEN)
      GO TO 20
C-----Y HORIZONTAL/X VERTICAL.
   10 XP=YSCALE*YIN+YOFF
      YP=XSCALE*XIN+XOFF
      CALL PLOT(XP,YP,IPEN)
   20 RETURN
      END
      SUBROUTINE COLORBOX(XB,YB,IFILL,IBORDER)
C=======================================================================
C
C     TRANSLATE AND SCALE EACH STROKE TO THE PLANE OF THE PLOTTER AND
C     FILL BOX.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 XP,YP
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP31/XOFF,YOFF,XSCALE,YSCALE,XSIZE,YSIZE,XBASIS
      DIMENSION XB(2),YB(2),XP(2),YP(2)
C-----PLOT WITH SELECTED ORIENTATION
      IF(NFLIP.NE.0) GO TO 10
C-----X HORIZONTAL/Y VERTICAL.
      XP(1)=XSCALE*XB(1)+XOFF
      YP(1)=YSCALE*YB(1)+YOFF
      XP(2)=XSCALE*XB(2)+XOFF
      YP(2)=YSCALE*YB(2)+YOFF
      CALL BOXCOLOR(XP,YP,IFILL,IBORDER)
      GO TO 20
C-----Y HORIZONTAL/X VERTICAL.
   10 XP(1)=YSCALE*YB(1)+YOFF
      YP(1)=XSCALE*XB(1)+XOFF
      XP(2)=YSCALE*YB(2)+YOFF
      YP(2)=XSCALE*XB(2)+XOFF
      CALL BOXCOLOR(XP,YP,IFILL,IBORDER)
   20 RETURN
      END
      SUBROUTINE LONGX(BCD,IBCD,NBCD)
C=======================================================================
C
C     DEFINE LENGTH OF MT DEFINITION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 BCD,BLANK
      DIMENSION BCD(NBCD)
      DATA BLANK/' '/
      IBCD=NBCD
      DO 10 I=1,NBCD
      IF(BCD(IBCD).NE.BLANK) GO TO 20
   10 IBCD=IBCD-1
   20 RETURN
      END
      SUBROUTINE SPECAL(BCD,IBCD,NBCD,KBCD)
C=======================================================================
C
C     DEFINE NUMBER OF NON-BLANK CHARACTERS AND PLOTTED CHARACTERS
C     (NON-BLANK MINUS CONTROL CHARACTERS).
C
C     BCD     = CHARACTER STRING
C     IBCD    = NUMBER OF NON-BLANK CHARACTERS
C     NBCD    = NUMBER OF PLOTTED CHARACTERS
C     KBCD    = TOTAL LENGTH OF CHARACTER STRING
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 BCD,BLANK,CHRTAB,CHRTRL
      COMMON/GPP40/CHRTAB(256),CHRTRL(256)
      COMMON/GPP41/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHAR,ICNTRL
      DIMENSION BCD(KBCD)
      DATA BLANK/' '/
C-----FIND LAST NON-BLANK CHARACTER.
      IBCD=KBCD
      DO 10 II=1,KBCD
      IF(BCD(IBCD).NE.BLANK) GO TO 20
   10 IBCD=IBCD-1
      IBCD=0
      NBCD=0
      GO TO 50
C-----COUNT SPECIAL NON-PRINTING CHARACTERS.
   20 NBCD=IBCD
      IF(ICNTRL.LE.0) GO TO 50
      DO 40 I=1,IBCD
      DO 30 J=1,ICNTRL
      IF(BCD(I).NE.CHRTRL(J)) GO TO 30
      NBCD=NBCD-1
      GO TO 40
   30 CONTINUE
   40 CONTINUE
   50 RETURN
      END
      SUBROUTINE BOXPLT
C=======================================================================
C
C     DEFINE X AND Y SIZE OF LEGEND BOX FOR EACH SUBPLOT.
C
C     X DIMENSIONS OF BOX INCLUDES
C     1) START OF BOX.
C     2) START OF LINE TYPE (WD)
C     3) END OF LINE TYPE   (7*WD)
C     4) MIDDLE OF SYMBOL   (3.5*WD)
C     5) START OF REFERENCE I.D. (8*WD)
C     6) END OF BOX (LENGTH OF I.D. + 9*WD)
C
C     Y DIMENSIONS OF BOX INCLUDES
C     7) TOP OF BOX
C     8) STARTING LINE FOR NEXT REFERENCE (1.75*HT)
C     9) BOTTOM OF BOX (1.75*HT PER I.D. + 0.75*HT)
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 TITLES
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP6/TITLES(84,30)
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP29/ZBASE,ZINCH,ZQ(2),XYGRID(2,2),XYINCH(2,2,5)
C-----INITIALIZE FLAG TO CALL PEN WHEN FIRST LEGEND BOX IS DRAWN.
      IPASS=0
c-----------------------------------------------------------------------
C
C     LOOP OVER SUBPLOTS.
C
c-----------------------------------------------------------------------
      DO 80 LFRAME=1,NFRAME
c-----------------------------------------------------------------------
C
C     SHOULD THERE BE A LEGEND BOX.
C
c-----------------------------------------------------------------------
      IF(NOBOX(LFRAME).NE.0) GO TO 70
c-----------------------------------------------------------------------
C
C     YES. DEFINE LONGEST MESSAGE IN TITLE BOX FOR CURRENT SUBPLOT.
C
c-----------------------------------------------------------------------
      MESBIG=0
C-----CURVES
      KCUR=IMCUR(LFRAME)
      IF(KCUR.LE.0) GO TO 20
      DO 10 I=1,KCUR
      LCUR=MYCUR(I,LFRAME)
      CALL SPECAL(TITLES(1,LCUR),IT,ITP,84)
      IF(ITP.GT.MESBIG) MESBIG=ITP
   10 CONTINUE
C-----SETS OF POINTS
   20 KPT=IMPT(LFRAME)
      IF(KPT.LE.0) GO TO 40
      DO 30 I=1,KPT
      LPT=MYPT(I,LFRAME)
      CALL SPECAL(TITLES(1,LPT),IT,ITP,84)
      IF(ITP.GT.MESBIG) MESBIG=ITP
   30 CONTINUE
c-----------------------------------------------------------------------
C
C     DEFINE OVERALL X AND Y DIMENSIONS OF LEGEND BOX.
C
c-----------------------------------------------------------------------
   40 DWDX=FLOAT(MESBIG+9)*WD
      DWDY=(1.75d0*FLOAT(KCUR+KPT)+0.75d0)*HT
c-----------------------------------------------------------------------
C
C     DEFINE ABSOLUTE POSITION OF LEGEND BOX...PROGRAM DEPENDENT.
C
c-----------------------------------------------------------------------
      IF(MYPROG.NE.1) GO TO 50
c-----------------------------------------------------------------------
C
C     COMPLOT - ALWAYS UPPER LEFT HAND CORNER.
C
c-----------------------------------------------------------------------
      BOXLIM(1,1,LFRAME)=XYINCH(1,1,LFRAME)+WD
      BOXLIM(2,2,LFRAME)=XYINCH(2,2,LFRAME)-HT
      BOXLIM(2,1,LFRAME)=BOXLIM(1,1,LFRAME)+DWDX
      BOXLIM(1,2,LFRAME)=BOXLIM(2,2,LFRAME)-DWDY
      GO TO 60
c-----------------------------------------------------------------------
C
C     ALL OTHER PROGRAMS FIND LARGEST OPEN AREA FOR LEGEND BOX.
C
c-----------------------------------------------------------------------
   50 CALL OPENER(DWDX,DWDY)
c-----------------------------------------------------------------------
C
C     DRAW LEGEND BOX.
C
c-----------------------------------------------------------------------
C-----SELECT PEN FOR FIRST LEGEND BOX DRAWN.
   60 IF(IPASS.EQ.0) CALL PEN(2)
      IPASS=1
      CALL PLOT4(BOXLIM(1,1,LFRAME),BOXLIM(1,2,LFRAME),3)
      CALL PLOT4(BOXLIM(1,1,LFRAME),BOXLIM(2,2,LFRAME),2)
      CALL PLOT4(BOXLIM(2,1,LFRAME),BOXLIM(2,2,LFRAME),2)
      CALL PLOT4(BOXLIM(2,1,LFRAME),BOXLIM(1,2,LFRAME),2)
      CALL PLOT4(BOXLIM(1,1,LFRAME),BOXLIM(1,2,LFRAME),2)
      GO TO 80
c-----------------------------------------------------------------------
C
C     POSITION LEGEND BOX OFF PLOT.
C
c-----------------------------------------------------------------------
   70 BOXLIM(1,1,LFRAME)=-10.0d0
      BOXLIM(2,1,LFRAME)=-10.0d0
      BOXLIM(1,2,LFRAME)=-10.0d0
      BOXLIM(2,2,LFRAME)=-10.0d0
c-----------------------------------------------------------------------
C
C     END OF SUBPLOT LOOP.
C
c-----------------------------------------------------------------------
   80 CONTINUE
      RETURN
      END
      SUBROUTINE BOXSUB
C=======================================================================
C
C     POSITION LEGEND BOX FOR SUBPLOT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP1/MYPROG,MODPLT,XYPLOT(2,2),XYEDGE(2,2),NSMALX,NSMALY
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP3/MYCUR(30,5),MYPT(30,5),IMCUR(5),IMPT(5),NFRAME,
     1 LFRAME,LFRAMR
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP22/BIXMIN,BIXMAX,BIYMIN,BIYMAX,BOXLIM(2,2,5)
      COMMON/GPP23/BOXREL(9)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
c-----------------------------------------------------------------------
C
C     ABSOLUTELY POSITION LEGEND BOX.
C
c-----------------------------------------------------------------------
      BIXMIN=BOXLIM(1,1,LFRAME)
      BIXMAX=BOXLIM(2,1,LFRAME)
      BIYMIN=BOXLIM(1,2,LFRAME)
      BIYMAX=BOXLIM(2,2,LFRAME)
c-----------------------------------------------------------------------
C
C     NOTHING ELSE TO DO IF NO LEGEND BOX ON PLOT.
C
c-----------------------------------------------------------------------
      IF(NOBOX(LFRAME).NE.0.OR.BIXMIN.LE.0.0) GO TO 10
      CALL COLORBOX(BOXLIM(1,1,LFRAME),BOXLIM(1,2,LFRAME),0,2)
c-----------------------------------------------------------------------
C
C     DEFINE POSITION OF,
C
C     X DIMENSIONS OF BOX INCLUDES
C     1) START OF BOX.
C     2) START OF LINE TYPE
C     3) END OF LINE TYPE
C     4) MIDDLE OF SYMBOL
C     5) START OF REFERENCE I.D.
C     6) END OF BOX
C
c-----------------------------------------------------------------------
      BOXREL(1)=BIXMIN
      BOXREL(2)=BOXREL(1)+WD
      BOXREL(3)=BOXREL(2)+6.0d0*WD
      BOXREL(4)=0.5d0*(BOXREL(2)+BOXREL(3))
      BOXREL(5)=BOXREL(3)+WD
      BOXREL(6)=BIXMAX
c-----------------------------------------------------------------------
C
C     Y DIMENSIONS OF BOX INCLUDES
C     7) TOP OF BOX
C     8) STARTING LINE FOR NEXT REFERENCE
C     9) BOTTOM OF BOX
C
c-----------------------------------------------------------------------
      BOXREL(7)=BIYMAX
      BOXREL(8)=BOXREL(7)-1.75*HT
      BOXREL(9)=BIYMIN
c-----------------------------------------------------------------------
C
C     FOR COMPLOT SET UPPER Y ROUNDING TO AVOID LEGEND BOX.
C
c-----------------------------------------------------------------------
      IF(MYPROG.EQ.1) SPACER(2,2,LFRAME)=(YINCH2-BIYMIN)+
     1 SPACER(1,2,LFRAME)
   10 RETURN
      END
      SUBROUTINE CHRIN
C=======================================================================
C
C     LOAD SOFTWARE CHARACTER TABLE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 CHRTAB,CHRTRL
      COMMON/GPP0/NPRINT,NPLOT,NCHR,NSYM
      COMMON/GPP40/CHRTAB(256),CHRTRL(256)
      COMMON/GPP41/XCHAR(5000),YCHAR(5000),ICHPEN(5000),INDCHR(2,256),
     1 ICHAR,ICNTRL
C-----DEFINE CORE SIZE.
      DATA ICORE/5000/
C-----ONLY ALLOW TABLE TO BE READ ONCE.
      DATA IPASS/0/
      IF(IPASS.NE.0) RETURN
      IPASS=1
C-----INITIALIZE CHARACTER INDEX AND COUNT OF CONTROL CHARACTERS.
      IHIGH=0
      ICNTRL=0
C-----SET UP LOOP TO READ UP TO 256 SPECIAL CHARACTERS.
      DO 30 ICHAR=1,256
C-----READ FIRST LINE TO DEFINE CHARACTER AND NUMBER OF STROKES.
      READ(NCHR,60,ERR=40,END=40) CHRTAB(ICHAR),ICOUNT
      LOWEST=IHIGH+1
      IHIGH=IHIGH+ICOUNT
C-----INSURE THERE IS AVAILABLE CORE.
      IF(IHIGH.GT.ICORE) GO TO 40
C-----READ STROKES (X, Y, PEN POSITION).
      DO 10 I=LOWEST,IHIGH
   10 READ(NCHR,70,ERR=40,END=40) XCHAR(I),YCHAR(I),ICHPEN(I)
C-----SAVE CONTROL CHARACTERS.
      IF(ICOUNT.NE.1) GO TO 20
      IF(ICHPEN(LOWEST).GT.0) GO TO 20
      ICNTRL=ICNTRL+1
      CHRTRL(ICNTRL)=CHRTAB(ICHAR)
C-----DEFINE INDICES TO SPECIAL CHARACTER STROKE TABLE.
   20 INDCHR(1,ICHAR)=LOWEST
   30 INDCHR(2,ICHAR)=IHIGH
      ICHAR=256
      GO TO 50
C-----END OF DATA OR ERROR.
   40 ICHAR=ICHAR-1
   50 RETURN
   60 FORMAT(1X,A1,I5)
   70 FORMAT(2F7.3,I5)
      END
      SUBROUTINE SYMREF(X1,X2,X3,Y1,Y2,Y3,ISYM)
C=======================================================================
C
C     PLOT SYMBOL AND ERROR BARS FOR DATA POINT
C     COORDINATES ARE IN INCHES IN THE PLANE OF THE PLOT
C     X2, X3 AND Y2, Y3 ARE ABSOLUTE, NOT RELATIVE TO X1 AND Y1.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP2/NGRID,NPOINT,NTHICK,NFLIP,NBORD,NPAGE,NREST,NRATIO,
     1 NUNITY,NACTV,NCOMP,NROUND(2,2),NERR(2),NOBOX(5),NXPLAN,NYPLAN(5),
     2 NXTERP,NYTERP(5),NXLIMS(2),NYLIMS(2,5),MINSOK
      COMMON/GPP20/XL(4),YL(12),HT,WD,HTX1,HTX2,HT0P5,HT1P75,TICK0,
     1 DXYMIN,SPACER(2,2,5)
      COMMON/GPP27/XINCH1,XINCH2,YINCH1,YINCH2
      COMMON/GPP42/XSYM(2000),YSYM(2000),ISYPEN(2000),INDSYM(2,60),
     1 SYMWTH(60),ISYMER
      DATA XSET/0.5d0/
      DATA YSET/0.5d0/
C-----FIRST CHECK FOR SPECIAL CHARACTERS.
      IF(ISYM.GT.ISYMER) GO TO 50
c-----------------------------------------------------------------------
C
C     DRAW SYMBOL CENTERED ON THE POINT (X1,X2).
C
c-----------------------------------------------------------------------
      I1=INDSYM(1,ISYM)
      I2=INDSYM(2,ISYM)
      DO 10 I=I1,I2
C-----DEFINE X AND Y AND TRUNCATE TO INTERIOR OF PLOT.
      XP=X1+HT*(XSYM(I)-XSET)
      YP=Y1+HT*(YSYM(I)-YSET)
      IF(XP.LT.XINCH1) XP=XINCH1
      IF(XP.GT.XINCH2) XP=XINCH2
      IF(YP.LT.YINCH1) YP=YINCH1
      IF(YP.GT.YINCH2) YP=YINCH2
   10 CALL DASHX(XP,YP,ISYPEN(I))
c-----------------------------------------------------------------------
C
C     DRAW ERROR BARS.
C
c-----------------------------------------------------------------------
      BOZ=HT/4.0
      HTY=HT/2.0
      HTX=HTY*SYMWTH(ISYM)
c-----------------------------------------------------------------------
C
C     X ERROR BARS.
C
c-----------------------------------------------------------------------
      IF(NERR(1).LE.0) GO TO 30
C-----DEFINE ENDS OF ERROR BAR AND TRUNCATE TO RANGE OF PLOT.
      Y1MBOZ=Y1-BOZ
      Y1PBOZ=Y1+BOZ
      IF(Y1MBOZ.LT.YINCH1) Y1MBOZ=YINCH1
      IF(Y1PBOZ.GT.YINCH2) Y1PBOZ=YINCH2
C-----X1 TO X2 (X-DX).
      XAI=X1-HTX
      IF(X2.LT.XINCH1) X2=XINCH1
      IF(X2.GT.XINCH2) X2=XINCH2
      IF(XAI.LE.X2) GO TO 20
      CALL DASHX(XAI,Y1,3)
      CALL DASHX(X2,Y1,2)
      CALL DASHX(X2,Y1MBOZ,3)
      CALL DASHX(X2,Y1PBOZ,2)
C-----X1 TO X3 (X+DX).
   20 XBI=X1+HTX
      IF(X3.LT.XINCH1) X3=XINCH1
      IF(X3.GT.XINCH2) X3=XINCH2
      IF(XBI.GE.X3) GO TO 30
      CALL DASHX(XBI,Y1,3)
      CALL DASHX(X3,Y1,2)
      CALL DASHX(X3,Y1MBOZ,3)
      CALL DASHX(X3,Y1PBOZ,2)
c-----------------------------------------------------------------------
C
C     Y ERROR BARS.
C
c-----------------------------------------------------------------------
   30 IF(NERR(2).LE.0) GO TO 50
C-----DEFINE ENDS OF ERROR BAR AND TRUNCATE TO RANGE OF PLOT.
      X1MBOZ=X1-BOZ
      X1PBOZ=X1+BOZ
      IF(X1MBOZ.LT.XINCH1) X1MBOZ=XINCH1
      IF(X1PBOZ.GT.XINCH2) X1PBOZ=XINCH2
C-----Y1 TO Y2 (Y-DY).
      YAI=Y1-HTY
      IF(Y2.LT.YINCH1) Y2=YINCH1
      IF(Y2.GT.YINCH2) Y2=YINCH2
      IF(YAI.LE.Y2) GO TO 40
      CALL DASHX(X1,YAI,3)
      CALL DASHX(X1,Y2,2)
      CALL DASHX(X1MBOZ,Y2,3)
      CALL DASHX(X1PBOZ,Y2,2)
C-----Y1 TO Y3 (Y+DY).
   40 YBI=Y1+HTY
      IF(Y3.LT.YINCH1) Y3=YINCH1
      IF(Y3.GT.YINCH2) Y3=YINCH2
      IF(YBI.GE.Y3) GO TO 50
      CALL DASHX(X1,YBI,3)
      CALL DASHX(X1,Y3,2)
      CALL DASHX(X1MBOZ,Y3,3)
      CALL DASHX(X1PBOZ,Y3,2)
   50 RETURN
      END
      SUBROUTINE SYMIN
C=======================================================================
C
C     LOAD SYMBOL FOR POINT DATA AND LINE TYPE TABLES.
C
C     THE FIRST 30 ARE SPECIAL SYMBOLS
C     THE NEXT 30 ARE LINE TYPES
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/GPP0/NPRINT,NPLOT,NCHR,NSYM
      COMMON/GPP42/XSYM(2000),YSYM(2000),ISYPEN(2000),INDSYM(2,60),
     1 SYMWTH(60),ISYMER
C-----DEFINE CORE SIZE.
      DATA ICORE/2000/
C-----ONLY ALLOW TABLE TO BE READ ONCE.
      DATA IPASS/0/
      IF(IPASS.NE.0) RETURN
      IPASS=1
C-----INITIALIZE CHARACTER INDEX.
      IHIGH=0
C-----SET UP LOOP TO READ UP TO 60 SYMBOLS AND LINE TYPES.
      DO 20 ISYMER=1,60
C-----READ FIRST LINE TO DEFINE CHARACTER AND NUMBER OF STROKES.
      READ(NSYM,50,ERR=30,END=30) ICOUNT,SYMWTH(ISYMER)
      LOWEST=IHIGH+1
      IHIGH=IHIGH+ICOUNT
C-----INSURE THERE IS AVAILABLE CORE.
      IF(IHIGH.GT.ICORE) GO TO 30
C-----READ STROKES (X, Y, PEN POSITION).
      DO 10 I=LOWEST,IHIGH
   10 READ(NSYM,60,ERR=30,END=30) XSYM(I),YSYM(I),ISYPEN(I)
C-----DEFINE INDICES TO SPECIAL CHARACTER STROKE TABLE.
      INDSYM(1,ISYMER)=LOWEST
      INDSYM(2,ISYMER)=IHIGH
   20 CONTINUE
      ISYMER=60
      GO TO 40
C-----END OF DATA OR ERROR.
   30 ISYMER=ISYMER-1
   40 RETURN
   50 FORMAT(2X,I5,F7.3)
   60 FORMAT(2F7.3,I5)
      END
      SUBROUTINE FILIO1
C=======================================================================
C
C     DEFINE ALL I/O UNITS AND OPTIONALLY DEFINE FILENAMES.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*72 NAMEIN1,NAMEIN2
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/GPP0/NPRINT,NPLOT,NCHR,NSYM
      COMMON/IOSTATUS/ISTAT1
      COMMON/INPAGE/IXYLOW(3),IXYHI(3),KTAPE(3),ISCR(3),IMUSED(3)
      COMMON/MTDAT2/MTTABB(5,1000),MTLONG,MTTAPE
      COMMON/NAMEX/NAMEIN1,NAMEIN2
C-----DEFINE ALL I/O UNITS.
C-----(NOTE...CHANGE IN INPUT UNIT NUMBER TO ALLOW UNIT 5 TO BE USED
C-----FOR KEYBOARD INTERACTION ON IBM-PC).
      INP=2
      OUTP=3
      ITAPE=0
      OTAPE=0
      MTTAPE=9
      KTAPE(1)=10
      KTAPE(2)=11
      ISCR(1)= 12
      ISCR(2)= 13
      ISCR(3)= 14
      NCHR   = 17
      NSYM   = 18
      DO 10 I=1,3
   10 IMUSED(I)=0
C-----DEFINE ALL FILE NAMES.
      OPEN(OUTP,FILE='COMPLOT.LST',STATUS='UNKNOWN')
      CALL SCRATCH1(ISCR(1),'COMPLOT.001 ')
      CALL SCRATCH1(ISCR(2),'COMPLOT.002 ')
      CALL SCRATCH1(ISCR(3),'COMPLOT.003 ')
      ISTAT2 = 0
      OPEN(MTTAPE,FILE='MT.DAT',STATUS='OLD',ERR=20)
      GO TO 40
   20 WRITE(OUTP,30)
   30 FORMAT(//' ERROR - Opening MT.DAT file')
      ISTAT2 = 1
C-----DEFINE FILE AND OPEN IT.
   40 OPEN(NCHR,FILE='PLOT.CHR',STATUS='OLD',ERR=50)
      GO TO 70
   50 WRITE(OUTP,60)
   60 FORMAT(//' ERROR - Opening PLOT.CHR file')
      ISTAT2 = 1
   70 OPEN(NSYM,FILE='PLOT.SYM',STATUS='OLD',ERR=80)
      GO TO 100
   80 WRITE(OUTP,90)
   90 FORMAT(//' ERROR - Opening PLOT.SYM file')
      ISTAT2 = 1
  100 OPEN(INP,FILE='COMPLOT.INP',STATUS='OLD',ERR=110)
      IF(ISTAT2.EQ.1) CALL ENDERROR
      RETURN
  110 ISTAT1 = 1
      IF(ISTAT2.EQ.1) CALL ENDERROR
      RETURN
      ENTRY FILIO2
C=======================================================================
C
C     ENTRY POINT TO DEFINE ENDF/B DATA FILES.
C
C=======================================================================
      ISTAT2 = 0
      OPEN(KTAPE(1),FILE=NAMEIN1,STATUS='OLD',ERR=120)
      GO TO 140
  120 WRITE(OUTP,130) NAMEIN1
  130 FORMAT(//' ERROR - Opening ENDF/B Data File'/1X,A72)
      ISTAT2 = 1
  140 OPEN(KTAPE(2),FILE=NAMEIN2,STATUS='OLD',ERR=150)
      IF(ISTAT2.EQ.1) CALL ENDERROR
      RETURN
  150 WRITE(OUTP,130) NAMEIN2
      ISTAT2 = 1
      CALL ENDERROR
      END
      SUBROUTINE TIT10(IZAP,LFS,MTBOX,IMTBOX)
C=======================================================================
C
C     DEFINE CHARACTER TITLE FOR THE LEGEND BOX MF=10
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 MTBOX,DIGITS(0:9)
      DIMENSION MTBOX(40)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
C-----------------------------------------------------------------------
C
C     CHARACTER EQUIVALENT OF ZAP
C
C-----------------------------------------------------------------------
C-----in initialize all blank
      DO I=1,40
      MTBOX(I) = ' '
      ENDDO
c-----define ZAP
      CALL ZAHOL(IZAP,MTBOX)
c-----define length
      DO IMTBOX=40,1,-1
      IF(MTBOX(IMTBOX).NE.' ') GO TO 10
      ENDDO
c-----length = 0 = ERROR - ZAP Undefined? Very unlikely.
      IMTBOX = 1
      RETURN
C-----------------------------------------------------------------------
C
C     ADD FINAL STATE = blank, g or m#
C
C-----------------------------------------------------------------------
   10 IF(LFS.ge.99) return    ! Assume LFS=99 means ALL
c----- "g" for ground state
      IF(LFS.LE.0) THEN
      IF(IZAP.lt.3000) return ! No state for neutron or light particles.
      IMTBOX = IMTBOX + 1
      MTBOX(IMTBOX) = 'g'
      RETURN
      ENDIF
c-----------------------------------------------------------------------
c
c     "m" followed by #
c
c-----------------------------------------------------------------------
c-----by here LFS = 1 to 98 = 1 or 2 digits
      IMTBOX = IMTBOX + 1
      MTBOX(IMTBOX) = 'm'
      I1 = LFS/10                ! First digit
      I2 = LFS - 10*I1           ! Second digit
      IF(I1.gt.0) THEN           ! First digit, if any (must be >0)
      IMTBOX = IMTBOX + 1
      MTBOX(IMTBOX) = DIGITS(I1)
      ENDIF
      IMTBOX = IMTBOX + 1
      MTBOX(IMTBOX) = DIGITS(I2) ! second digit, always
      RETURN
      END
