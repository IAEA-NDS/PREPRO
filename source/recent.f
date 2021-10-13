C=======================================================================
C
C     PROGRAM RECENT
C     ==============
C     VERSION 79-1 (OCTOBER 1979)  CDC-7600
C     VERSION 80-1 (MAY 1980)      IBM, CDC AND CRAY VERSION
C     VERSION 80-2 (DECEMBER 1980) IMPROVED TREATMENT OF UNRESOLVED
C                                  REGION TO COMPUTE ALL REACTIONS AT
C                                  THE SAME TIME.
C     VERSION 81-1 (MARCH 1981)  IMPROVED BASED ON USER COMMENTS.
C     VERSION 81-2 (AUGUST 1981) ADDED MONITOR MODE. ADDED SPEED OPTION
C                                TO BYPASS BACKWARDS THINNING IF FILE 3
C                                ALLOWABLE ERROR = 0.0 (NOTE THIS OPTION
C                                WILL RESULT IN ALL TABULATED POINTS
C                                FROM THE EVALUATION BEING KEPT IN THE
C                                OUTPUT FROM THIS PROGRAM).
C     VERSION 82-1 (JANUARY 1982) IMPROVED COMPUTER COMPATIBILITY.
C     VERSION 83-1 (JANUARY 1983)*MAJOR RE-DESIGN.
C                                *PAGE SIZES INCREASED.
C                                *ELIMINATED COMPUTER DEPENDENT CODING.
C                                *NEW, MORE COMPATIBLE I/O UNIT NUMBERS.
C                                *ADDED OPTION TO KEEP ALL RECONSTRUCTED
C                                 AND BACKGROUND ENERGY POINTS.
C                                *ADDED STANDARD ALLOWABLE ERROR OPTIONS
C                                 (CURRENTLY 0.1 PER-CENT RECONSTRUCTION
C                                 AND 0.0 PER-CENT THINNING).
C     VERSION 83-2 (OCTOBER 1983) IMPROVED BASED ON USER COMMENTS.
C     VERSION 84-1 (JANUARY 1984) IMPROVED INTERVAL HALFING CONVERGENCE.
C     VERSION 85-1 (APRIL 1985)  *A BRAND NEW PROGRAM WHICH COMPLETELY
C                                 SUPERCEDES ALL PREVIOUS VERSIONS OF
C                                 THIS PROGRAM.
C                                *UPDATED FOR ENDF/B-6 FORMATS.
C                                *ADDED GENERAL REICH-MOORE FORMALISM
C                                 (WITH TWO FISSION CHANNELS).
C                                *DECREASED RUNNING TIME.
C                                *SPECIAL I/O ROUTINES TO GUARANTEE
C                                 ACCURACY OF ENERGY.
C                                *DOUBLE PRECISION TREATMENT OF ENERGY
C                                 (REQUIRED FOR NARROW RESONANCES).
C     VERSION 85-2 (AUGUST 1985) *FORTRAN-77/H VERSION
C     VERSION 86-1 (JANUARY 1986)*ENERGY DEPENDENT SCATTERING RADIUS
C     VERSION 86-2 (JUNE 1986)   *IF FIRST CHANCE FISSION (MT=19)
C                                 BACKGROUND IS PRESENT ADD RESONANCE
C                                 CONTRIBUTION OF FISSION TO IT.
C     VERSION 86-3 (OCTOBER 1986)*MULTI-LEVEL OR REICH-MOORE..CORRECT
C                                 POTENTIAL SCATTERING CROSS SECTION FOR
C                                 MISSING AND/OR FICTICIOUS (L,J)
C                                 SEQUENCES.
C     VERSION 87-1 (JANUARY 1987)*IMPROVED COMBINING FILE 2+3
C     VERSION 87-2 (MARCH 1987)  *CORRECTED ADLER-ADLER CALCULATIONS.
C     VERSION 88-1 (JULY 1988)   *UPDATED REICH-MOORE ENDF/B-6 FORMAT
C                                 TO BE THE SAME AS REICH-MOORE FORMAT
C                                 IN EARLIER VERSIONS OF ENDF/B FORMAT.
C                                *CHECK FOR PRELIMINARY ENDF/B-6
C                                 REICH-MOORE FORMAT (NOW ABANDONED)
C                                 AND TERMINATE EXECUTION IF DATA IS
C                                 IN THIS FORMAT.
C                                *CALCULATE CHANNEL RADIUS OR SET IT
C                                 EQUAL TO THE SCATTERING RADIUS.
C                                *IMPLEMENTED HYBRID R-FUNCTION WITH THE
C                                 FOLLOWING RESTRICTIONS
C                                 - ONLY INELASTIC COMPETITION (NO
C                                   CHARGED PARTICLES)
C                                 - NO TABULATED FILE 2 BACKGROUND
C                                 - NO TABULATED OPTICAL MODEL PHASE
C                                   SHIFT
C                                *PROGRAM EXIT IF GENERAL R-MATRIX IN
C                                 THE EVALUATION (THIS FORMALISM WILL
C                                 BE IMPLEMENTED ONLY AFTER THE AUTHOR
C                                 RECEIVES REAL EVALUATIONS WHICH USE
C                                 THIS FORMALISM...UNTIL THEN IT IS
C                                 IMPOSSIBLE TO ADEQUATELY TEST THAT
C                                 THE CODING FOR THIS FORMALISM IS
C                                 CORRECT).
C                                *INCREASED MAXIMUM NUMBER OF RESONANCES
C                                 FROM 1002 TO 4008.
C                                *DOUBLE PRECISION RESONANCE REGION
C                                 LIMITS.
C                                *FILE 2 AND FILE 3 ENERGIES WHICH ARE
C                                 NEARLY EQUAL ARE TREATED AS EQUAL
C                                 (I.E., SAME TO ABOUT 9 DIGITS).
C                                *CHECK FILE 3 BACKGROUND CROSS SECTIONS
C                                 IN EDIT MODE.
C                                *OPTION...INTERNALLY DEFINE FILENAMES
C                                 (SEE SUBROUTINE FILEIO FOR DETAILS).
C     VERSION 89-1 (JANUARY 1989)*PSYCHOANALYZED BY PROGRAM FREUD TO
C                                 INSURE PROGRAM WILL NOT DO ANYTHING
C                                 CRAZY.
C                                *UPDATED TO USE NEW PROGRAM CONVERT
C                                 KEYWORDS.
C                                *CORRECTED MULTILEVEL, REICH-MOORE AND
C                                 HYBRID R-FUNCTION POTENTIAL SCATTER
C                                 TO ACCOUNT FOR REPEATED J-VALUES FOR
C                                 THE SAME TARGET SPIN AND L-VALUE.
C                                *ADDED LIVERMORE CIVIC COMPILER
C                                 CONVENTIONS.
C                                *UPDATED TO USE NEW ENDF/B-6
C                                 CONVENTION TO ALLOW UNRESOLVED
C                                 RESONANCE CONTRIBUTION TO ALREADY
C                                 BE INCLUDED IN THE FILE 3 CROSS
C                                 SECTIONS (INFINITELY DIULUTE
C                                 CONTRIBUTION).
C     VERSION 90-1 (JUNE 1990)   *UPDATED BASED ON USER COMMENTS
C                                *ADDED FORTRAN SAVE OPTION
C                                *NEW MORE CONSISTENT ENERGY OUTPUT
C                                 ROUTINE
C     VERSION 91-1 (JULY 1991)   *NEW UNIFORM TREATMENT OF ALL RESONANCE
C                                 FORMALISMS (SEE, COMMENTS BELOW)
C                                *NEW REICH-MOORE ALGORITHM
C                                *MORE EXTENSIVE ERROR CHECKING AND
C                                 ERROR MESSAGE EXPLANATIONS
C     VERSION 92-1 (JANUARY 1992)*MAJOR RESTRUCTING TO IMPROVE ACCURACY
C                                 AND COMPUTER INDEPENDENCE.
C                                *INCREASED ENERGY POINT PAGE SIZE FROM
C                                 1002 TO 4008.
C                                *NO MORE THAN 2 ENERGY POINTS WHERE
C                                 CROSS SECTION IS ZERO AT BEGINNING
C                                 OF A SECTION FOR EACH REACTION,E.G.,
C                                 THRESHOLD FISSION.
C                                *PROCESS ONLY A PORTION OF RESONANCE
C                                 REGION - SEE EXPLANATION BELOW
C                                *ALL ENERGIES INTERNALLY ROUNDED PRIOR
C                                 TO CALCULATIONS.
C                                *COMPLETELY CONSISTENT I/O AND ROUNDING
C                                 ROUTINES - TO MINIMIZE COMPUTER
C                                 DEPENDENCE.
C     VERSION 93-1 (MARCH 1993)  *UPDATED REICH-MOORE TREATMENT TO USE
C                                 L DEPENDENT SCATTERING RADIUS (APL)
C                                 RATHER THAN SCATTERING RADIUS (AP)
C                                 (SEE, ENDF/B-6 FORMATS AND
C                                  PROCEDURES MANUAL, PAGE 2.6)
C                                *INCREASED PAGE SIZE FROM 4008 TO
C                                 20040 DATA POINTS.
C                                *INCREASED MAXIMUM NUMBER OF RESONANCES
C                                 FROM 4008 TO 20040.
C     VERSION 94-1 (JANUARY 1994)*VARIABLE ENDF/B DATA FILENAMES
C                                 TO ALLOW ACCESS TO FILE STRUCTURES
C                                 (WARNING - INPUT PARAMETER FORMAT
C                                 HAS BEEN CHANGED).
C                                *CLOSE ALL FILES BEFORE TERMINATING
C                                 (SEE, SUBROUTINE ENDIT)
C     VERSION 94-2 (AUGUST 1994) *CORRECTED ADDL FOR ENERGY DEPENDENT
C                                 (TABULATED) SCATTERING RADIUS CASE.
C     VERSION 96-1 (JANUARY 1996) *COMPLETE RE-WRITE
C                                 *IMPROVED COMPUTER INDEPENDENCE
C                                 *ALL DOUBLE PRECISION
C                                 *ON SCREEN OUTPUT
C                                 *UNIFORM TREATMENT OF ENDF/B I/O
C                                 *IMPROVED OUTPUT PRECISION
C                                 *ALWAYS INCLUDE THERMAL VALUE
C                                 *DEFINED SCRATCH FILE NAMES
C     VERSION 97-1 (APRIL 1997)   *OPTIONAL MAKE NEGATIVE CROSS
C                                  SECTION = 0 FOR OUTPUT
C                                *INCREASED PAGE SIZE FROM 20040 TO
C                                 120000 DATA POINTS.
C                                *INCREASED MAXIMUM NUMBER OF RESONANCES
C                                 FROM 20040 TO 120000.
C     VERSION 99-1 (MARCH 1999)   *CORRECTED CHARACTER TO FLOATING
C                                  POINT READ FOR MORE DIGITS
C                                 *UPDATED TEST FOR ENDF/B FORMAT
C                                  VERSION BASED ON RECENT FORMAT CHANGE
C                                 *UPDATED CONSTANTS BASED ON CSEWG
C                                  SUBCOMMITTEE RECOMMENDATIONS
C                                 *GENERAL IMPROVEMENTS BASED ON
C                                  USER FEEDBACK
C     VERSION 99-2 (JUNE 1999)    *IMPLEMENTED NEW REICH-MOORE FORMALISM
C                                  TO ALLOW DEFINITION OF (L,J,S) FOR
C                                  EACH SEQUENCE.
C                                 *ASSUME ENDF/B-VI, NOT V, IF MISSING
C                                  MF=1, MT-451.
C     VERS. 2000-1 (FEBRUARY 2000)*GENERAL IMPROVEMENTS BASED ON
C                                  USER FEEDBACK
C     VERS. 2002-1 (MAY 2002)     *OPTIONAL INPUT PARAMETERS
C                  (SEPT. 2002)   *OUTPUT RESONANCE WITH 9 DIGITS
C                                 *TO BE C AND C++ COMPATIBLE OUTPUT
C     VERS. 2004-1 (JAN. 2004)    *ADDED INCLUDE 'recent.h'
C                                 *MADE ENDF/B-VII READY
C                                 *UPDATED FOR NEW REICH-MOORE LRF=7
C                                  PARAMETERS WITH COMPETITION
C                                 *ADDED COULOMB PENETRATION FACTORS FOR
C                                  LRF=7 COMPETITIVE CHANNELS.
C                                 *EXTENDED DEFINITIONS OF PENETRATION
C                                  FACTOR, LEVEL SHIFT FACTOR, AND
C                                  POTENTIAL SCATTERING PHASE SHIFT
C                                  ABOVE L = 5 TO INFINITY.
C                                 *ADDED QUICK CALCULATION - IF THE
C                                  INPUT ALLOWABLE ERROR IS 1.0 OR MORE
C                                  (100 % OR MORE) THERE IS NO ITERATION
C                                  TO CONVERGENCE - CROSS SECTION ARE
C                                  QUICKLY CALCULATED ONLY AT A FIXED
C                                  SET OF ENERGY POINTS, BASED ON THE
C                                  ENERGY AND WIDTH OF ALL RESONANCES.
C                                  THIS CAN BE USED TO QUICKLY "SEE"
C                                  NEW EVALUATIONS THAT MAY CONTAIN
C                                  ERRORS, THAT WOULD OTHERWISE CAUSE
C                                  THIS CODE TO RUN FOR AN EXCESSIVELY
C                                  LONG TIME.
C     VERS. 2005-1 (JUNE 2005)    *ADDED ENERGY DEPENDENT SCATTERING
C                                  RADIUS FOR ALL RESONANCE TYPES
C                                  (EARLIER ONLY BREIT-WIGNER ALLOWED).
C     VERS. 2007-1 (JAN. 2007)    *CHECKED AGAINST ALL ENDF/B-VII.
C                                 *DECOUPLED PAGE SIZE FROM MAX. # OF
C                                  RESONANCES.
C                                 *INCREASED PAGE SIZE FROM 120,000 TO
C                                  750,000 DATA POINTS.
C                                 *KEPT MAX. # OF RESONANCE AT 120,000.
C                                 *CORRECTED ALL BACKGROUND = 0 CASE
C     VERS. 2007-2 (OCT. 2007)    *NO MT=19 OUTPUT IF NO BACKGROUND,
C                                  REGARDLESS OF INPUT OPTION.
C                                 *72 CHARACTER FILE NAMES.
C     VERS. 2008-1 (FEB. 2008)    *CORRECTED NAPS ERROR - NOW DEFINE FOR
C                                  ALL TYPES OF PARAMETERS - EARLIER
C                                  ONLY DEFINED FOR B-W PARAMETERS.
C     VERS. 2008-2 (APRIL 2008)   *CORRECTED NRO/NAPS=1/1 - MUST
C                                  DEFINE RHOX2 AT EACH RESONANCE USING
C                                  SETRHO1 BEFORE ENERGY DEPENDENT
C                                  CALCULATION.
C                                 *ADDED PRECISION TO RESONANCE PROFILE
C                                  IN SUBROUTINE SUBINT
C     VERS. 2009-1 (JULY 2009)    *NEW REICH-MOORE COMPETITIVE WIDTHS -
C                                  IF CHARGED PARTICLE REACTION (MT=103
C                                  THROUGH 107) WILL ADD RESONANCE
C                                  CONTRIBUTION TO COMPETITIVE MT AND IF
C                                  PRESENT, THE GROUND LEVEL, MT = 600
C                                  THROUGH 800. IF COMPETITIVE CHANNEL
C                                  IS mt=4 (TOTAL N.N') IT WILL ALSO ADD
C                                  COMPETITIVE RESONANCE CONTRIBUTION TO
C                                  MT=50 (N,N' GROUND).
C                                 *NEW REICH-MOORE - SUM COMPETITIVE
C                                  WIDTHS IF ALL FOR THE SAME STATE (MT)
C     VERS. 2009-2 (AUG. 2009)    *RE-WRITE TO USE 12, RATHER THAN 6,
C                                  PAAMETERS PER RESONANCE.
C                                 *MAJOR RE-WRITE TO ACCOMODATE GENERAL
C                                  REICH-MOORE (LRF=7).
C                                 *COMPLETE RE-WRITE FOR ADLER-ADLER
C                                  AND HRF (N O LONGER USED IN ENDF/B)
C                                  TO USE 12 PARAMETERS PER RESNANCE.
C     VERS. 2010-1 (April 2010)   *ADDED SAMRML LOGIC TO HANDLE ALL
C                                  LRF=7 CASES.
C                                 *EXTENDED SAMRML LOGIC TO PROCESS ALL
C                                  EVALUATIONS = RESOLVED + UNRESOLVED +
C                                  TABULATED - SAMRML ONLY DOES ONE
C                                  SECTION OF RESOLVED LRF=7 DATA
C                                  WITHOUT TABULATED BACKGROUND.
C                                 *UPDATED ELASTIC POTENTIAL CALCULATION
C                                  FOR TOTAL (SLBW) AND CORRECTION FOR
C                                  MISSING SEQUENCES (MLBW, RM, HRF).
C                                 *ADDED HIDDEN (OPTIONAL) UNRESOLVED
C                                  COMPETITION LISTING (NOT ENDF/B).
C                                 *ADDED BOB MACFARLANE'S PROPOSAL - USE
C                                  LRX TO DEFINE COMPETITIVE L VALUE -
C                                  COMPETITIVE L = LRX - 1, IF LRX > 0.
C                                 *CHECKED FOR NEGATIVE WIDTHS.
C     VERS. 2012-1 (Nov.  2012)   *ADDED ENERGY DEPENDENT STEP SIZE
C                                  FOR STARTING GRID AROUND RESONANCES.
C                                 *Added CODENAME
C                                 *32 and 64 bit Compatible
C                                 *Added ERROR stops
C                                 *Check for no capture for Reich-Moore.
C     VERS. 2012-2 (Nov.  2012)   *Eliminated ERROR in NHIGH(0) index.
C     VERS. 2013-1 (Nov.  2013)   *Extended OUT9.
C     VERS. 2015-1 (Jan.  2015)   *Multiple LRF=7, General Reich-Moore
C                                  Resonance Regions.
C                                 *Added OUT10.
C                                 *Replaced ALL 3 way IF Statements.
C                                 *Replaced ALL LOGICAL by INTEGER.
C     VERS. 2016-1 (Jan.  2016)   *Do not Change LSSF during the
C                                  reconstrcution - for compatibility
C                                  with later URR treatment.
C                                 *Insured that all ERROR stops print
C                                  a message explaining why the code
C                                  stopped.
C                                 *Partial Energy Range Processing
C                                  no longer allowed - today's computers
C                                  are so fast that this option is now
C                                  out-of-date and no longer allowed.
C                                 *L-Value dependent fission = Earlier
C                                  was done only by entire isotope.
C                                 *Denser Starting Energy Grid.
C     VERS. 2017-1 (May   2017)   *Corrected ERROR in LRF=3 treatment.
C                                  This ERROR only existed in version
C                                  2016-1, which was never released to
C                                  the general public, so it will not
C                                  effect any results calculated by code
C                                  users.
C                                 *All floating input parameters changed
C                                  to character input + IN9 conversion.
C                                 *Added points to starting energy grid
C                                  to approximate the shape of each
C                                  resonance = based on comparisons of
C                                  0.01% to 0.1% results.
C                                 *Increased max. points to 1,200,000.
C                                 *LRF=7 Shift option no longer allowed
C                                  Set = 0, print WARNING and continue.
C                                 *Corrected COMMON/NAPRHO/NRO,NAPS
C                                  /NAPRHO/ mispelled - Freud found.
C     VERS. 2017-2 (Sept. 2017)   *Corrected Write statemnt at 5731.
C     VERS. 2018-1 (Nov.  2018)   *Added output for ALL ENDERROR
C     VERS. 2019-1 (June  2019)   *Terminate if MF/MT=1/451 Temperature
C                                  is NOT = 0 = Incompaible with the
C                                  0 Kelvin data output to MF=3 by this
C                                  code.
C                                 *Terminate if MF=3 Point Count and
C                                  Interpolation Law do not agree.
C                                 *Terminate if MF=3 Background
C                                  Interpolation is NOT Linear.
C                                 *Ignor background if zero at all
C                                  energies - previously merged.
C                                 *Output competitive data even if no
C                                  MF=3 background = previously skipped.
C                                 *Additional Interpolation Law Tests.
C                                 *Check Maximum Tabulated Energy to
C                                  insure it is the same for all MTs -
C                                  if not,print WARNING messages.
C                                 *Reduced Max. # of Resonance to
C                                  100,000 from 300,000, e.g., for
C                                  ENDF/B-VIII U235 and U238 have about
C                                  3,000 resonances each.
C     VERS. 2020-1 (Dec.  2020)   *Major re-write.
C                                 *Much more detailed starting grid
C                                 *Updated minimum/maximum convergence
C                                  procedures.
C                                 *Added Target Isomer State
C                                 *WARNING - print if resolved resonance
C                                  enegies do not extend to top of
C                                  resolved energy range.
C                                 *Stricter convergence for all except
c                                  total and elastic,e.g., narrower
C                                  capture resonances
C                                 *Increased minimum cross sections to
C                                  handle RML Charged Particles
C     VERS. 2021-1 (Mar.  2021)   *Valentin Sinitsa (GRUCON) supplied
C                                  Solution for LRF=7, SHIFT=1 problems.
C                                  Search for Valentin to find changes.
C                                 *Updated for FORTRAN 2018
C                                 *Changed FUNCTION   Xdot to
C                                          SUBROUTINE XdotQ with
C                                  arguments returned through COMMON.
C                                 *Mimimum Cross Section is no longer
C                                  an input option - set to 1.0d-30.
C                                 *Insure ALL nodes are INCORE10 to
C                                  prevent repeated energies in ENDF
C                                  format output.
C                                 *Corrected dummy arguments ,1) to ,*)
C                                  Thank Arjan
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
C     Acknowledgement (Version 2021-1)
C     ==================================================================
C     The author thanks Valentin Sinitsa (Kurchatov Institute) for
C     providing the FORTRAN coding to handle the Reich-Moore Limited
C     (RML) LRF=7, SHIFT=1, case. Valentin, I could not have completed
C     the RECENT 2021-1 release without your invaluable contribution -
C     I and all RECENT code users are indebted to you = SPASIBO.
C
C     Acknowledgement (Version 2004-1)
C     ==================================================================
C     The author thanks Nancy Larson, ORNL, for providing her SAMRML
c     code for comparison to RECENT output for Reich-Moore evaluations,
c     in particular to verify results for the new LFR=7 evaluations. I
c     also thank her for providing guidance to help me understand and
c     implement this new teatment for Reich-Moore parameters.
C
C     ACKNOWLEDGEMENT (VERSION 92-1)
C     ==================================================================
C     THE AUTHOR THANKS SOL PEARLSTEIN (BROOKHAVEN NATIONAL LAB) FOR
C     SIGNIFICANTLY CONTRIBUTING TOWARD IMPROVING THE ACCURACY AND
C     COMPUTER INDEPENDENCE OF THIS CODE - THANKS, SOL
C     ==================================================================
C
C     AUTHORS MESSAGE
C     ==================================================================
C     THE REPORT DESCRIBED ABOVE IS THE LATEST PUBLISHED DOCUMENTATION
C     FOR THIS PROGRAM. HOWEVER, THE COMMENTS BELOW SHOULD BE CONSIDERED
C     THE LATEST DOCUMENTATION INCLUDING ALL RECENT IMPROVEMENTS. PLEASE
C     READ ALL OF THESE COMMENTS BEFORE IMPLEMENTATION, PARTICULARLY
C     THE COMMENTS CONCERNING MACHINE DEPENDENT CODING.
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
C     ==================================================================
C     THIS PROGRAM IS DESIGNED TO RECONSTRUCT THE RESONANCE CONTRIBUTION
C     TO THE CROSS SECTION IN LINEARLY INTERPOLABLE FORM, ADD IN ANY
C     LINEARLY INTERPOLABLE BACKGROUND CROSS SECTION AND OUTPUT THE
C     RESULT IN THE ENDF/B FORMAT. THE CROSS SECTIONS OUTPUT BY THIS
C     PROGRAM WILL BE LINEARLY INTERPOLABLE OVER THE ENTIRE ENERGY RANGE
C
C     THE RESONANCE CONTRIBUTION IS CALCULATED FOR TOTAL (MT=1),
C     ELASTIC (MT=2), CAPTURE (MT=102) AND FISSION (MT=18), ADDED
C     TO THE BACKGROUND (IF ANY) AND OUTPUT. IN ADDITION, IF THERE
C     IS A FIRST CHANCE FISSION (MT=19) BACKGROUND PRESENT THE RESONANCE
C     CONTRIBUTION OF FISSION WILL BE ADDED TO THE BACKGROUND AND
C     OUTPUT. IF THERE IS NO FIRST CHANCE FISSION (MT=19) BACKGROUND
C     PRESENT THE PROGRAM WILL NOT OUTPUT MT=19.
C
C     IN THE FOLLOWING FOR SIMPLICITY THE ENDF/B TERMINOLOGY--ENDF/B
C     TAPE--WILL BE USED. IN FACT THE ACTUAL MEDIUM MAY BE TAPE, CARDS,
C     DISK OR ANY OTHER MEDIUM.
C
C     PROCESSING DATA IN THE ENDF/B-6 FORMAT
C     ==================================================================
C     IT HAS NOW BEEN CONFIRMED (PRIVATE COMMUNICATION, CHARLES DUNFORD,
C     APRIL, 1991) THAT THE PROPER PROCEDURE TO FOLLOW WHEN THERE ARE
C     MISSING OR DUPLICATE J VALUES IS TO IN ALL CASES ADD A SEQUENCE
C     WITH NO RESONANCES TO ACCOUNT FOR THE CONTRIBUTION OF THE SEQUENCE
C     TO THE POTENTIAL SCATTERING CROSS SECTION.
C
C     THIS IS THE PROCEDURE WHICH WAS FOLLOWED BY ALL VERSIONS OF RECENT
C     SINCE 86-3 AND WILL CONTINUE TO BE THE PROCEDURE.
C
C     INPUT ENDF/B FORMAT AND CONVENTIONS
C     ==================================================================
C     ENDF/B FORMAT
C     -------------
C     THIS PROGRAM ONLY USES THE ENDF/B BCD OR LINE IMAGE FORMAT (AS
C     OPPOSED TO THE BINARY FORMAT) AND CAN HANDLE DATA IN ANY VERSION
C     OF THE ENDF/B FORMAT (I.E., ENDF/B-1, 2, 3, 4, 5, 6 FORMAT).
C
C     IT IS ASSUMED THAT THE DATA IS CORRECTLY CODED IN THE ENDF/B
C     FORMAT AND NO ERROR CHECKING IS PERFORMED. IN PARTICULAR IT IS
C     ASSUMED THAT THE MAT, MF AND MT ON EACH LINE IS CORRECT. SEQUENCE
C     NUMBERS (COLUMNS 76-80) ARE IGNORED ON INPUT, BUT WILL BE
C     CORRECTLY OUTPUT ON ALL CARDS. THE FORMAT OF SECTION MF=1, MT=451
C     AND ALL SECTIONS OF MF=2 AND 3 MUST BE CORRECT. THE PROGRAM COPIES
C     ALL OTHER SECTION OF DATA AS HOLLERITH AND AS SUCH IS INSENSITIVE
C     TO THE CORRECTNESS OR INCORRECTNESS OF ALL OTHER SECTIONS.
C
C     ENDF/B FORMAT VERSION
C     ---------------------
C     THE FORMATS AND CONVENTIONS FOR READING AND INTERPRETING THE DATA
C     VARIES FROM ONE VERSION OF ENDF/B TO THE NEXT. HOWEVER, IF THE
C     HOLLERITH SECTION (MF=1, MT=451) IS PRESENT IT IS POSSIBLE FOR
C     THIS PROGRAM TO DISTINGUISH BETWEEN DATA IN THE ENDF/B-4, 5 AND
C     6 FORMATS AND TO USE THE APPROPRIATE CONVENTIONS FOR EACH
C     ENDF/B VERSION (SEE, SUBROUTINE FILE1 FOR A DESCRIPTION OF HOW
C     THIS IS DONE). IF THE HOLLERITH SECTION IS NOT PRESENT THE
C     PROGRAM WILL ASSUME THE DATA IS IN THE ENDF/B-6 FORMAT AND USE
C     ALL CONVENTIONS APPROPRIATE TO ENDF/B-V. USERS ARE ENCOURAGED TO
C     INSURE THAT THE HOLLERITH SECTION (MF=1, MT=451) IS PRESENT IN
C     ALL EVALUATIONS.
C
C     INPUT OF ENERGIES
C     -----------------
C     ALL ENERGIES ARE READ IN DOUBLE PRECISION (BY SPECIAL FORTRAN I/O
C     ROUTINES) AND ARE TREATED IN DOUBLE PRECISION IN ALL CALCULATIONS.
C
C     OUTPUT ENDF/B FORMAT AND CONVENTIONS
C     ==================================================================
C     CONTENTS OF OUTPUT
C     ------------------
C     ENTIRE EVALUATIONS ARE OUTPUT, NOT JUST THE RECONSTRUCTED FILE
C     3 CROSS SECTIONS, E.G. ANGULAR AND ENERGY DISTRIBUTIONS ARE
C     ALSO INCLUDED.
C
C     DOCUMENTATION
C     -------------
C     THE FACT THAT THIS PROGRAM HAS OPERATED ON THE DATA IS DOCUMENTED
C     BY THE ADDITION OF COMMENT CARDS AT THE END OF EACH HOLLERITH
C     SECTION IN THE FORM
C
C     ***************** RECENT (VERSION 2021-1) ***************
C     RESONANCE CONTRIBUTION RECONSTRUCTED TO WITHIN   0.100 PER-CENT
C     COMBINED DATA NOT THINNED (ALL RESONANCE + BACKGROUND DATA KEPT)
C
C     THE ORDER OF ALL SIMILAR COMMENTS (FROM LINEAR, SIGMA1 AND GROUPY)
C     REPRESENTS A COMPLETE HISTORY OF ALL OPERATIONS PERFORMED ON
C     THE DATA, INCLUDING WHICH VERSION OF EACH PROGRAM WAS USED.
C
C     THESE COMMENT CARDS ARE ONLY ADDED TO EXISTING HOLLERITH SECTIONS,
C     I.E., THIS PROGRAM WILL NOT CREATE A HOLLERITH SECTION. THE FORMAT
C     OF THE HOLLERITH SECTION IN ENDF/B-5 DIFFERS FROM THE THAT OF
C     EARLIER VERSIONS OF ENDF/B. BY READING AN EXISTING MF=1, MT=451
C     IT IS POSSIBLE FOR THIS PROGRAM TO DETERMINE WHICH VERSION OF
C     THE ENDF/B FORMAT THE DATA IS IN. WITHOUT HAVING A SECTION OF
C     MF=1, MT=451 PRESENT IT IS IMPOSSIBLE FOR THIS PROGRAM TO
C     DETERMINE WHICH VERSION OF THE ENDF/B FORMAT THE DATA IS IN, AND
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
C     OUTPUT FORMAT OF ENERGIES
C     -------------------------
C     IN THIS VERSION OF RECENT ALL FILE 3 ENERGIES WILL BE OUTPUT IN
C     F (INSTEAD OF E) FORMAT IN ORDER TO ALLOW ENERGIES TO BE WRITTEN
C     WITH UP TO 9 DIGITS OF ACCURACY. IN PREVIOUS VERSIONS THIS WAS AN
C     OUTPUT OPTION. HOWEVER USE OF THIS OPTION TO COMPARE THE RESULTS
C     OF ENERGIES WRITTEN IN THE NORMAL ENDF/B CONVENTION OF 6 DIGITS
C     TO THE 9 DIGIT OUTPUT FROM THIS PROGRAM DEMONSTRATED THAT FAILURE
C     TO USE THE 9 DIGIT OUTPUT CAN LEAD TO LARGE ERRORS IN THE DATA
C     JUST DUE TO TRANSLATION OF ENERGIES FROM THEIR INTERNAL (BINARY)
C     REPRESENTATION TO THE ENDF/B FORMAT.
C
C     ACCURACY OF ENERGY
C     ------------------
C     IN ORDER TO ALLOW ENERGIES TO BE ACCURATELY OUTPUT TO 9 DIGITS
C     ON SHORT WORD LENGTH COMPUTERS (E.G. IBM) ALL ENERGIES AND
C     ENERGY DEPENDENT TERMS ARE READ AND TREATED IN DOUBLE PRECISION.
C
C     OUTPUT OF RESONANCE PARAMETERS
C     ------------------------------
C     A SPECIAL CONVENTION HAS BEEN INTRODUCED REGARDING RESONANCE
C     PARAMETERS. IN ORDER TO ALLOW THE USER TO DOPPLER BROADEN AND/OR
C     SELF-SHIELD CROSS SECTIONS THE RESONANCE PARAMETERS ARE ALSO
C     INCLUDED IN THE OUTPUT WITH THE EVALUATION. IN ORDER TO AVOID THE
C     POSSIBILITY OF ADDING THE RESONANCE CONTRIBUTION A SECOND TIME
C     TWO CONVENTIONS HAVE BEEN ADOPTED TO INDICATE THAT THE RESONANCE
C     CONTRIBUTION HAS ALREADY BEEN ADDED TO THE FILE 3 CROSS SECTIONS,
C
C     (1) WHEN THE DATA IS PROCESSED BY THIS PROGRAM LRP (IN MF=1,
C     MT=451) IS SET EQUAL TO 2. THIS IS A CONVENTION WHICH HAS BEEN
C     ADOPTED AS A STANDARD CONVENTION IN ENDF/B-VI, BUT IS ONLY TO BE
C     USED FOR PROCESSED DATA, AS OPPOSED TO THE ORIGINAL EVALUATIONS.
C     IN EVALUATIONS WHICH CONTAIN MF=1, MT=451 LRP CAN BE USED TO
C     DETERMINE IF THE MATERIAL HAS BEEN PROCESSED.
C
C     (2) THE LRU FLAG IN EACH SECTION OF FILE 2 DATA IS CHANGED TO
C     LRU=LRU+3. FOR EXAMPLE WHEN READING AN ENDF/B EVALUATION LRU=0
C     (NO RESONANCES), =1 (RESOLVED) OR =2 (UNRESOLVED) INDICATES THAT
C     THE DATA IS IN THE ORIGINAL ENDF/B FORM. LRU=3 (NO RESONANCES),
C     =4 (RESOLVED) OR =5 (UNRESOLVED) INDICATES THAT THE RESONANCE
C     CONTRIBUTION HAS ALREADY BEEN ADDED TO THE FILE 3 DATA. THIS
C     SECOND CONVENTION HAS BEEN ADOPTED AS INSURANCE THAT THE RESONANCE
C     CONTRIBUTION WILL NOT BE ADDED TWICE, EVEN FOR EVALUATIONS WHICH
C     DO NOT CONTAIN MF=1, MT=451 (EVALUATIONS WHICH CONTAIN MF=1,
C     MT=451 ARE COVERED BY CONVENTION (1), DESCRIBED ABOVE).
C
C     UNIFORM TREATMENT OF RESONANCE FORMALISMS
C     ==================================================================
C     NORMALIZATION
C     =============
C     ALL OF THE RESONANCE FORMALISMS INCLUDE A FACTOR OF,
C
C     PI*(FRACTIONAL ABUNDANCE)/(K**2)
C
C     THIS FACTOR HAS BEEN REMOVED FROM THE CALCULATION OF EACH TYPE
C     OF RESONANCE FORMALISM AND IS APPLIED AS A FINAL NORMALIZATION
C     AFTER THE CALCULATION, ONLY ONE PLACE IN THIS PROGRAM.
C
C     FOR SIMPLICITY THIS TERM IS NOT INCLUDED IN THE FOLLOWING
C     DERIVATIONS - IN ALL CASES THE ACTUAL CROSS SECTION IS A PRODUCT
C     OF THE ABOVE FACTOR TIMES THE RESULTS PRESENTED BELOW.
C
C     SIMILARITIES
C     ============
C     FOR THE RESOLVED RESONANCE REGION, EXCEPT FOR SINGLE LEVEL BREIT
C     WIGNER, PARAMETERS ALL OF THE FORMALISMS DEFINE THE CROSS SECTIONS
C     IN AN EQUIVALENT FORM,
C
C     TOTAL    = 2*GJ*REAL(1 - U)
C              = 2*GJ*(1 - REAL(U))
C     ELASTIC  =   GJ*(1 - U)**2
C              =   GJ*((1 - 2*REAL(U)) + (REAL(U)**2 + IM(U)**2))
C              = 2*GJ*(1 - REAL(U)) - GJ*(1 - (REAL(U)**2 + IM(U)**2))
C
C     SINCE THE FIRST TERM IS THE TOTAL, THE SECOND TERM MUST BE
C     ABSORPTION. SO WE FIND,
C
C     ABSORPTION = GJ*(1 - (REAL(U)**2 + IM(U)**2))
C
C     IN ALL CASES U IS DEFINED IN THE FORM,
C
C     U        = EXP(-I*2*PS)*((1-X) - I*Y)
C
C     WHERE (X) AND (Y) ARE RELATED TO THE SYMMETRIC AND ANTI-SYMMETRIC
C     CONTRIBUTIONS OF THE RESONANCES, RESPECTIVELY. ONLY THE DEFINITION
C     OF (X) AND (Y) WILL BE DIFFERENT FOR EACH RESONANCE FORMALISM.
C     BELOW WE WILL SHOW THAT WHAT MIGHT APPEAR TO BE A STRANGE CHOICE
C     OF DEFINITION OF THE SIGN OF (X) AND(Y) HAS BEEN SELECTED SO THAT
C     FOR BREIT-WIGNER PARAMETERS (X) AND (Y) CORRESPOND EXACTLY TO THE
C     SYMMETRIC AND ANTI-SYMMETRIC CONTRIBUTION OF THE RESONANCES.
C
C     U        = (COS(2*PS) - I*SIN(2*PS))*((1-X) - I*Y)
C              =   ((1-X)*COS(2*PS) - Y*SIN(2*PS))
C              =-I*((1-X)*SIN(2*PS) + Y*COS(2*PS))
C
C     REAL(U)  = ((1-X)*COS(2*PS) - Y*SIN(2*PS))
C     IM(U)    =-((1-X)*SIN(2*PS) + Y*COS(2*PS))
C
C     R(U)**2  =((1-X)*COS(2*PS))**2 + (Y*SIN(2*PS))**2
C               -2*(1-X)*Y*COS(2*PS)*SIN(2*PS)
C     I(U)**2  =((1-X)*SIN(2*PS))**2 + (Y*COS(2*PS))**2
C               +2*(1-X)*Y*COS(2*PS)*SIN(2*PS)
C
C     THE TERMS 2*(1-X)*Y*COS(2*PS)*SIN(2*PS) CANCEL AND UPON USING
C     THE IDENTITY COS(2*PS)**2 + SIN(2*PS)**2 = 1,
C
C     SUM      = (1-X)**2 + (Y)**2
C
C     WE NOW HAVE ALL THE QUANTITIES THAT WE NEED TO DEFINE THE CROSS
C     SECTIONS,
C
C     ELASTIC
C     =======
C     ELASTIC  =GJ*(1 - 2*REAL(U) + (REAL(U)**2 + IM(U)**2))
C              =GJ*(1 - 2*((1-X)*COS(2*PS)-Y*SIN(2*PS))+(1-X)**2+(Y)**2)
C
C     THIS CAN BE WRITTEN AS A SUM OF 2 SQUARES,
C
C     ELASTIC  =GJ*(COS(2*PS) - (1-X))**2 + (SIN(2*PS) + Y)**2)
C
C              =GJ*((COS(2*PS))**2 - 2*(1-X)*COS(2*PS) + (1-X)**2) +
C                   (SIN(2*PS))**2 + 2*Y*SIN(2*PS)    + (Y)**2)
C
C     AGAIN USING THE IDENTITY COS(2*PS)**2 + SIN(2*PS)**2 = 1, WE CAN
C     SEE THAT THE DEFINITION AS THE SUM OF 2 SQUARES IS IDENTICAL TO
C     THE PRECEDING DEFINITION OF THE ELASTIC.
C
C     ELASTIC  =GJ*(COS(2*PS) - (1-X))**2 + (SIN(2*PS) + Y)**2)
C              =GJ*((COS(2*PS)-1) + X)**2 + (SIN(2*PS) + Y)**2)
C
C     USING THE IDENTITY (1 - COS(2*PS))) = 2*SIN(PS)**2, WE OBTAIN
C     THE FINAL FORM FOR THE ELASTIC,
C
C     ELASTIC =GJ*(2*SIN(PS)**2 - X)**2 + (SIN(2*PS) + Y)**2)
C
C     ABSORPTION
C     ==========
C     ABSORPTION = GJ*(1 - (REAL(U)**2 + IM(U)**2))
C                = GJ*(1 - ((1-X)**2   + (Y)**2)
C                = GJ*(1 - (1 - 2*X + (X)**2 + (Y)**2)
C                = GJ*(2*X - (X)**2 + (Y)**2)
C
C     SINCE PHYSICALLY THE ABSORPTION CANNOT BE NEGATIVE WE CAN SEE
C     THAT (X) MUST BE POSITIVE AND 2*X MUST BE GREATER THAN
C     (X)**2 + (Y)**2, FOR ALL OF THE FORMALISMS.
C
C     TOTAL
C     =====
C     IN THIS PROGRAM THE TOTAL CROSS SECTION IS ALWAYS DEFINED TO BE
C     THE SUM OF ITS PARTS - SO THE ABOVE DEFINITION IS NEVER EXPLICITLY
C     USED. HOWEVER, WE CAN LEARN SOMETHING BY EXAMINING THE DEFINITION,
C
C     TOTAL    = 2*GJ*REAL(1 - U)
C              = 2*GJ*(1 - (((1-X)*COS(2*PS) - Y*SIN(2*PS)))
C              = 2*GJ*((1 - COS(2*PS))*(1-X) - (1-X) + Y*SIN(2*PS))
C              = 2*GJ*(2*SIN(PS)**2*(1-X)    - (1-X) + Y*SIN(2*PS))
C
C              = 4*GJ*SIN(PS)**2 +
C                2*GJ*((X-1) - 2*X*SIN(PS)**2 +  Y*SIN(2*PS))
C
C     THE IMPORTANT POINT TO NOTE IS THAT THE DEFINITION OF THE TOTAL
C     DOES NOT EXPLICITLY CONTAIN ANY DEPENDENCE ON X**2 AND Y**2 -
C     THE LEVEL-LEVEL INTERFERENCE TERMS.
C
C     THIS IMPLIES THAT IF A GIVEN SET OF RESONANCE PARAMETERS ARE USED
C     WITH THIS DEFINITION THEY WILL PRODUCE EXACTLY THE SAME TOTAL
C     CROSS SECTION - WHETHER WE CLAIM THE PARAMETERS HAVE BEEN
C     PRODUCED USING A SINGLE OR MULTI-LEVEL FIT. THIS RESULT COULD
C     BE VERY MISLEADING, IF THIS RESULT FOR THE TOTAL IS IMPLIED TO
C     MEAN THAT ONE INTERPRETATION OR THE OTHER WILL NOT HAVE ANY
C     EFFECT ON THE INDIVIDUAL CROSS SECTIONS.
C
C     STARTING FROM EXACTLY THE SAME RESONANCE PARAMETERS, RELATIVE TO
C     THE RESULTS OBTAINED USING THE SINGLE LEVEL FORMULA, MULTI-LEVEL
C     RESULTS WILL TEND TO ALWAYS DECREASE THE ABSORPTION AND INCREASE
C     THE ELASTIC. THIS CAN BE IMMEDIATELY SEEN FROM OUR GENERAL
C     MULTI-LEVEL DEFINITION OF ABSORPTION,
C
C     ABSORPTION =GJ*(2*X - ((X)**2 + (Y)**2))
C
C     THE SINGLE LEVEL ABSORPTION IS,
C
C     ABSORPTION =GJ*(2*X)
C
C     THE DIFFERENCE BETWEEN THE TWO IS -2*GJ*(X**2 + Y**2), SO THAT
C     REGARDLESS OF HOW WE DEFINE (X) AND (Y) THE INCLUSION OF THIS
C     TERM WILL ALWAYS DECREASE ABSORPTION. SINCE THE TOTAL CROSS
C     SECTION IS THE SAME IN BOTH CASE, THIS MEANS THAT THE ELASTIC
C     HAS BEEN INCREASED BY THIS AMOUNT.
C
C     AGAIN, THESE RESULTS ARE BASED ON STARTING FROM EXACTLY THE SAME
C     PARAMETERS - IN ANY ACTUAL CASE THE PARAMETERS BASED ON A SINGLE
C     OR MULTI-LEVEL FIT WILL BE QUITE DIFFERENT - THE POINT THAT WE
C     WANT TO STRESS HERE IS THAT YOU SHOULD NEVER USE PARAMETERS
C     WHICH HAVE BEEN DEFINED BY A FIT USING ONE FORMALISM - IN THE
C     EQUATIONS FOR A DIFFERENT FORMALISM - AND ASSUME THAT THE RESULTS
C     WILL BE CONSISTENT - AND NEVER USE THE TOTAL CROSS SECTION TO
C     SEE WHETHER OR NOT A SET OF SINGLE LEVEL PARAMETERS CAN BE USED
C     WITH A MULTI-LEVEL FORMALISM.
C
C     POTENTIAL CROSS SECTION
C     =======================
C     FAR FROM RESONANCES (X) AND (Y) WILL BE SMALL AND THE ELASTIC
C     CROSS SECTION REDUCES TO,
C
C     ELASTIC =GJ*(2*SIN(PS)**2)**2     + (SIN(2*PS))**2
C             =GJ*4*(SIN(PS)**4         + SIN(2*PS)**2
C
C     USING THE IDENTITY SIN(2*PS) = 2*SIN(PS)*COS(PS)
C
C             =4*GJ*(SIN(PS)**4         + (SIN(PS)*COS(PS))**2)
C             =4*GJ*SIN(PS)**2*(SIN(PS)**2 + COS(PS)**2)
C             =4*GJ*SIN(PS)**2
C
C     WHICH IS THE POTENTIAL CROSS SECTION. NOTE THAT THIS RESULT IS
C     INDEPENDENT OF THE FORMALISM USED, AS IT MUST PHYSICALLY BE,
C     AND AS SUCH ALTHOUGH AS YET WE HAVE NOT DEFINED IT, WE CAN
C     NOW SEE THAT IN ALL CASES (PS) MUST BE THE PHASE SHIFT AND FOR
C     CONSISTENCY IT MUST BE DEFINED USING EXACTLY THE SAME DEFINITION
C     IN ALL CASES.
C
C     IN ADDITION SINCE PHYSICALLY FOR EACH L VALUE WE EXPECT TO OBTAIN
C     A POTENTIAL CROSS SECTION,
C
C     4*(2*L+1)*SIN(PS)**2
C
C     OBVIOUSLY FOR CONSISTENCY WE MUST HAVE,
C
C     (2*L+1) = (SUM OVER J) GJ
C
C     ONLY IN THIS CASE WILL THE RESULTS BE CONSISTENT - THIS POINT WILL
C     BE DISCUSSED IN DETAIL BELOW.
C
C     WHAT ARE THIS TERMS (X) AND (Y)
C     ===============================
C     (X) AND (Y) CAN BE EASILY IDENTIFIED BY CONSIDERING THE SINGLE
C     AND MULTI-LEVEL BREIT WIGNER FORMALISMS. IN THESE CASES WE WILL
C     FIND THAT,
C
C     X        = GAM(N)*GAM(T)/2/DEN
C     Y        = GAM(N)*(E-ER)/DEN
C     DEN      = ((E-ER)**2 + (GAM(T)/2)**2)
C
C     EXTREME CARE HAS TO BE USED TO PROPERLY DEFINE (Y) SUCH THAT IT
C     IS NEGATIVE FOR E LESS THAN ER AND POSITIVE FOR E GREATER THAN
C     ER. I WILL MERELY MENTION THAT THE EQUATIONS FOR ALL FORMALISMS
C     IN ENDF-102 DO NOT CONSISTENTLY USE (E - ER) - IN SOME CASES
C     THIS IS WRITTEN AS (ER - E), WHICH CAN LEAD TO AN INCORRECT
C     SIGN IN THE DEFINITION OF THE (Y) THAT WE REQUIRE.
C
C     THE INTERFERENCE TERMS CAN BE WRITTEN IN TERMS OF,
C     1) LEVEL-SELF INTERFERENCE  = THE CONTRIBUTION OF EACH LEVEL
C                                   INTERFERRING WITH ITSELF
C     2) LEVEL-LEVEL INTERFERENCE = THE CONTRIBUTION OF EACH LEVEL
C                                   INTERFERRRING WITH ALL OTHER LEVELS
C
C     WE WILL REFER TO THESE TWO AS (L-S) AND (L-L),
C
C     X**2     = (GAM(N)*(GAM(T)/2)**2/(DEN)**2      + (L-L)
C              = (GAM(N)**2*((GAM(T)/2)**2)/(DEN)**2 + (L-L)
C     Y**2     = (GAM(N))**2*((E-ER))**2/(DEN)**2    + (L-L)
C
C     X**2+Y**2= GAM(N)**2*DEN/(DEN)**2 = GAM(N)**2/DEN + (L-L)
C
C     TO SEE THE EFFECT OF INCLUDING MULTI-LEVEL INTERFERENCE WE CAN
C     CONSIDER OUR GENERAL EXPRESSION FOR ABSORPTION,
C
C     ABSORPTION =GJ*(2*X - ((X)**2 + (Y)**2))
C
C     AND NOTE THAT FOR BOTH SINGLE AND MULTI-LEVEL BREIT WIGNER THE
C     ENDF-102 SAYS TO TREAT ABSORPTION IN A SINGLE LEVEL APPROXIMATION
C     I.E., IGNORE LEVEL-LEVEL INTERFERENCE. IF ALL INTERFERENCE IS
C     IGNORED THIS IS EQUIVALENT TO COMPLETELY IGNORING X**2 + Y**2 AND
C     DEFINING,
C
C     ABSORPTION =GJ*2*X
C                =2*GJ*GAM(N)*GAM(T)/DEN
C
C     WHICH IS INCORRECT - SINCE THIS SEEMS TO INDICATE EVERYTHING IS
C     ABSORBED. IN ORDER TO OBTAIN THE CORRECT EXPRESSION WE CANNOT
C     COMPLETELY IGNORE INTERFERENCE - WE CAN IGNORE LEVEL-LEVEL
C     INTERFERENCE, BUT WE MUST INCLUDE LEVEL-SELF INTERFERENCE,
C
C     X**2+Y**2= GAM(N)**2/DEN
C
C     ABSORPTION =GJ*(2*X - ((X)**2 + (Y)**2))
C                =GJ*GAM(N)*(GAM(T)-GAM(N))/DEN
C                =GJ*GAM(N)*GAM(A)/DEN
C
C     SUMMARY
C     =======
C     AN IMPORTANT POINT TO NOTE IS THE DEFINITION OF (X) AND (Y)
C     WHICH IN ALL CASES WILL CORRESPOND TO THE SYMMETRIC AND
C     ANTI-SYMMETRIC CONTRIBUTION OF THE RESONANCES. IN PARTICULAR
C     DEFINING (U) IN TERMS OF (1-X) INSTEAD OF (X) IS EXTREMELY
C     IMPORTANT. NOTE, THAT THE DEFINITION OF THE ELASTIC AND
C     ABSORPTION ONLY INVOLVE (X), NOT (1-X). FAR FROM RESONANCES
C     (X) CAN BE EXTREMELY SMALL, THEREFORE (1-X) WILL BE VERY CLOSE
C     TO (1). IF THE CALCULATION PROCEEDS BY FIRST CALCULATING (1-X)
C     AND THEN DEFINING (X) BY SUBTRACTING (1), EXTREME ROUND-OFF
C     PROBLEMS CAN RESULT. THESE PROBLEMS CAN BE AVOIDED BY IN ALL
C     CASES DEFINING (X) DIRECTLY, WITHOUT ANY DIFFERENCES.
C
C     IN EACH FORMALISM THE DEFINITION OF (X) AND (Y) MAY BE DIFFERENT
C     BUT ONCE WE HAVE DEFINED (X) AND (Y) WE CAN IMMEDIATELY WRITE
C     THE CROSS SECTIONS USING A UNIFORM DEFINITION,
C
C     ELASTIC =GJ*(2*SIN(PS)**2 - X)**2 + (SIN(2*PS) + Y)**2)
C
C     ABSORPTION =-GJ*(2*X + (X)**2 + (Y)**2)
C
C     AND DEFINE THE TOTAL AS THE SUM OF THESE 2 PARTS.
C
C     RELATIONSHIP TO SINGLE LEVEL
C     ============================
C     HOW DO THE SINGLE AND MULTI-LEVEL FORMALISMS COMPARE. TO SEE,
C     STARTING FROM OUR GENERAL DEFINITION OF THE ELASTIC IN THE FORM,
C
C     ELASTIC =GJ*(2*SIN(PS)**2 + X)**2 + (SIN(2*PS) + Y)**2)
C             =GJ*(4*SIN(PS)**4 - 4*X*SIN(PS)**2 + X**2
C                + SIN(2*PS)**2 + 2*Y*SIN(2*PS)  + Y**2)
C
C             =4*GJ*SIN(PS)**2 +
C                GJ*(X**2 + Y**2
C                   -4*X*SIN(PS)**2
C                   +2*Y*SIN(2*PS))
C
C     AND OUR SPECIFIC DEFINITIONS OF (X) AND (Y) FOR MULTI-LEVEL BREIT-
C     WIGNER PARAMETERS,
C
C     X        = GAM(N)*GAM(T)/2/DEN
C     Y        = GAM(N)*(E-ER)/DEN
C     DEN      = ((E-ER)**2 + (GAM(T)/2)**2)
C
C     X**2+Y**2= GAM(N)**2/DEN + (L-L)
C
C     WE CAN RECOGNIZE X**2 AND Y**2 AS THE INTERFERENCE - (L-S) + (L-L)
C     TERMS IN THE MULTI-LEVEL FORMALISM. IN ORDER TO OBTAIN THE SINGLE
C     LEVEL EQUATION WE CAN ASSUME THAT EACH LEVEL DOES NOT INTERFERE
C     WITH ANY OTHER LEVEL - THEREFORE THE (L-L) CONTRIBUTION IS ZERO.
C
C     ELASTIC =4*GJ*SIN(PS)**2 +
C                GJ*GAM(N)*(GAM(N)
C                           -2*GAM(T)*SIN(PS)**2
C                           +2*(E-ER)*SIN(2*PS))/DEN
C
C     WHICH IS THE FORM THAT IT APPEARS IN ENDF-102, EXCEPT FOR TWO
C     TYPOGRAPHICAL ERRORS IN THE SECOND TERM,
C
C     -2*GAM(T)*SIN(PS)**2
C
C     WHICH IN ENDF-102 IS WRITTEN,
C
C     -2*(GAM(T)-GAM(N))*SIN(2*PS)**2
C
C     PROGRAM CONVENTIONS
C     ==================================================================
C     MINIMUM INPUT DATA
C     ------------------
C     FOR EACH MATERIAL TO BE PROCESSED THE MINIMUM INPUT DATA ARE THE
C     RESONANCE PARAMETERS IN FILE 2. IF THERE ARE NO FILE 2 PARAMETERS
C     IN A GIVEN MATERIAL THE ENTIRE MATERIAL WILL SIMPLY BE COPIED.
C     NEITHER THE HOLLERITH SECTION (MF=1, MT=451) NOR THE BACKGROUND
C     CROSS SECTION (SECTIONS OF MF=3) NEED BE PRESENT FOR THIS PROGRAM
C     TO EXECUTE PROPERLY. HOWEVER, SINCE THE CONVENTIONS USED IN
C     INTERPRETING THE RESONANCE PARAMETERS DEPENDS ON ENDF/B VERSION
C     USERS ARE STRONGLY RECOMMENDED TO INSURE THAT MF=1, MT=451 IS
C     PRESENT IN EACH MATERIAL TO ALLOW THE PROGRAM TO DETERMINE THE
C     ENDF/B FORMAT VERSION.
C
C     RESONANCE PARAMETERS
C     --------------------
C     RESONANCE PARAMETERS MAY BE REPRESENTED USING ANY COMBINATION
C     OF THE REPRESENTATIONS ALLOWED IN ENDF/B,
C     (1) RESOLVED DATA
C         (A) SINGLE LEVEL BREIT-WIGNER
C         (B) MULTI-LEVEL BREIT-WIGNER
C         (C) ADLER-ADLER
C         (D) REICH-MOORE
C         (E) HYBRID R-FUNCTION
C     (2) UNRESOLVED DATA
C         (A) ALL PARAMETERS ENERGY INDEPENDENT
C         (B) FISSION PARAMETERS ENERGY DEPENDENT
C         (C) ALL PARAMETERS ENERGY DEPENDENT
C
C     THE FOLLOWING RESOLVED DATA FORMALISMS ARE NOT TREATED BY THIS
C     VERSION OF THE CODE AND WILL ONLY BE IMPLEMENTED AFTER EVALUATIONS
C     USING THESE FORMALISMS ARE AVAILABLE TO THE AUTHOR OF THIS CODE
C     FOR TESTING IN ORDER TO INSURE THAT THEY CAN BE HANDLED PROPERLY
C         (A) GENERAL R-MATRIX
C
C     CALCULATED CROSS SECTIONS
C     -------------------------
C     THIS PROGRAM WILL USE THE RESONANCE PARAMETERS TO CALCULATE THE
C     TOTAL, ELASTIC, CAPTURE AND POSSIBLY FISSION CROSS SECTIONS. THE
C     COMPETITIVE WIDTH WILL BE USED IN THESE CALCULATIONS, BUT THE
C     COMPETITIVE CROSS SECTION ITSELF WILL NOT BE CALCULATED. THE
C     ENDF/B CONVENTION IS THAT ALTHOUGH A COMPETITIVE WIDTH MAY BE
C     GIVEN, THE COMPETITIVE CROSS SECTION MUST BE SEPARATELY TABULATED
C     AS A SECTION OF FILE 3 DATA.
C
C     RESOLVED REGION
C     ---------------
C     IN THE RESOLVED REGION THE RESOLVED PARAMETERS ARE USED TO
C     CALCULATE COLD (0 KELVIN), LINEARLY INTERPOLABLE, ENERGY DEPENDENT
C     CROSS SECTIONS.
C
C     SCATTERING RADIUS
C     -----------------
C     FOR SINGLE OR MULTI LEVEL BREIT-WIGNER PARAMETERS THE SCATTERING
C     RADIUS MAY BE SPECIFIED IN EITHER ENERGY INDEPENDENT (CONSTANT)
C     OR ENERGY DEPENDENT FORM (A TABLE OF ENERGY VS. RADIUS AND AN
C     ASSOCIATED INTERPOLATION LAW). IN ALL OTHER CASE ONLY AN ENERGY
C     INDEPENDENT SCATTERING RADIUS IS ALLOWED.
C
C     FOR ANY ONE MATERIAL (I.E. MAT) IF ENERGY DEPENDENT SCATTERING
C     RADII ARE GIVEN THE TOTAL NUMBER OF INTERPOLATION REGIONS AND
C     TABULATED VALUES FOR THE ENTIRE MATERIAL CANNOT EXCEED,
C     200 - INTERPOLATION REGIONS
C     500 - TABULATED VALUES
C     IF THESE LIMITS ARE EXCEEDED THE PROGRAM WILL PRINT AN ERROR
C     MESSAGE AND TERMINATE.
C
C     IF YOU REQUIRE A LARGER NUMBER OF INTERPOLATION REGION AND/OR
C     TABULATED VALUES,
C     (1) INTERPOLATION REGIONS - INCREASE THE DIMENSION OF NBTRHO AND
C     INTRHO IN COMMON/TABRHO/ THROUGHOUT THE PROGRAM AND CHANGE MAXSEC
C     IN SUBROUTINE RDAP (MAXSEC = MAXIMUM NUMBER OF INTERPOLATION
C     REGIONS).
C     (2) TABULATED VALUES - INCREASE THE DIMENSION OF ERHOTB, RHOTAB
C     AND APTAB IN COMMON/TABRHO/ THROUGHOUT THE PROGRAM AND CHANGE
C     MAXRHO IN SUBROUTINE RDAP (MAXRHO = MAXIMUM NUMBER OF TABULATED
C     VALUES).
C
C     RESOLVED REICH-MOORE AND MULTI-LEVEL BREIT-WIGNER PARAMETERS
C     ------------------------------------------------------------
C     CROSS SECTIONS FOR REICH-MOORE PARAMETERS ARE CALCULATED ACCORDING
C     TO THE EQUATION (1) - (8) OF SECTION D.1.3 OF ENDF-102. IN ORDER
C     TO CALCULATE CROSS SECTIONS FROM MULTI-LEVEL PARAMETERS IN A
C     REASONABLE AMOUNT OF TIME THIS PROGRAM EXPRESSES THE CROSS SECTION
C     IN TERMS OF A SINGLE SUM OVER RESONANCES (SEE, ENDF-102, SECTION
C     D.1.2, EQUATIONS 6-7), RATHER THAN AS A DOUBLE SUM (SEE, ENDF-102
C     SECTION D.1.2, EQUATION 1-2). IN ORDER FOR THE ENDF-102 EQUATIONS
C     TO BE CORRECT THE PARAMETERS MUST MEET THE FOLLOWING CONDITIONS,
C
C     (1) FOR EACH L STATE ALL PHYSICALLY POSSIBLE J SEQUENCES MUST BE
C         PRESENT. ONLY IN THIS CASE WILL THE CONTRIBUTIONS OF THE
C         INDIVIDUAL J SEQUENCES ADD UP TO PRODUCE THE CORRECT POTENTIAL
C         SCATTERING CONTRIBUTION FOR THE L STATE (SEE, ENDF-102,
C         SECTION D.1.2, EQUATIONS 6-7). IF ANY J SEQUENCE IS MISSING
C         THE PROGRAM WILL PRINT A WARNING AND ADD THE J SEQUENCE WITH
C         NO RESONANCE PARAMETERS IN ORDER TO ALLOW THE POTENTIAL
C         SCATTERING TO BE CALCULATED CORRECTLY (THIS IS EQUIVALENT TO
C         ASSUMING THAT THE EVALUATOR REALIZES THAT ALL J SEQUENCES MUST
C         BE AND ARE PRESENT AND THAT THE EVALUATION STATES THAT THERE
C         ARE NO RESONANCES WITH CERTAIN PHYSICALLY POSSIBLE J VALUES...
C         IN THIS CASE POTENTIAL CONTRIBUTION MUST STILL BE CONSIDERED).
C
C         EXAMPLE
C         =======
C         AN EXAMPLE OF WHERE THIS OCCURS AND IS IMPORTANT TO CONSIDER
C         IS U-238 IN ENDF/B-4 AND 5 LIBRARIES WHERE FOR L=1 THERE IS
C         ONLY A J=1/2 SEQUENCE. NOT INCLUDING THE J=3/2 SEQUENCE LEADS
C         TO UNDERESTIMATING THE POTENTIAL SCATTERING AND PRODUCES
C         MINIMA IN THE ELASTIC CROSS SECTION WHICH ARE AN ORDER OF
C         MAGNITUDE LOWER THAN THE CROSS SECTIONS OBTAINED BE INCLUDING
C         THE J=3/2 SEQUENCE.
C
C     (2) FOR A GIVEN TARGET SPIN AND L VALUE THERE MAY BE 2 POSSIBLE
C         MEANS OF OBTAINING THE SAME J VALUE. WHEN THIS OCCURS IN
C         ORDER TO CALCULATE THE CORRECT POTENTIAL SCATTERING CROSS
C         SECTION IT IS IMPORTANT TO INCLUDE THE EFFECT OF BOTH
C         POSSIBLE J SEQUENCES, EVEN THOUGH FROM THE ENDF/B DATA IT IS
C         NOT POSSIBLE TO DETERMINE WHICH OF THE 2 POSSIBLE SEQUENCES
C         ANY GIVEN RESONANCE BELONGS TO. IN THIS CASE THIS PROGRAM
C         TREAT ALL RESONANCES WITH THE SAME J VALUE AS BELONGING TO
C         THE SAME J SEQUENCE (TO ALLOW INTERFERENCE) AND WILL ADD AN
C         ADDITIONAL J SEQUENCE WITH NO RESONANCES IN ORDER TO ALLOW
C         THE POTENTIAL CROSS SECTION TO BE CALCULATED CORRECTLY. WHEN
C         THIS OCCURS A WARNING MESSAGE IS PRINTED, BUT BASED ON THE
C         ENDF/B DATA THERE IS NOTHING WRONG WITH THE DATA AND THERE IS
C         NOTHING THAT THE USER CAN DO TO CORRECT OR IN ANY WAY MODIFY
C         THE DATA TO ELIMINATE THE PROBLEM.
C
C         EXAMPLE
C         =======
C         FOR A TARGET SPIN =1 AND L=1 THE 2 RANGES OF PHYSICALLY
C         POSSIBLE J ARE 1/2, 3/2, 5/2 AND 1/2, 3/2. BY CHECKING THE
C         ENDF/B DATA IT IS POSSIBLE TO INSURE THAT THE 3 POSSIBLE
C         J VALUES (1/2, 3/2, 5/2) ARE PRESENT AND TO INCLUDE ALL 3
C         J SEQUENCES IN THE CALCULATIONS. HOWEVER, UNLESS ALL 5
C         POSSIBLE J SEQUENCES ARE INCLUDED THE STATISTICAL WEIGHTS
C         OF THE J SEQUENCES WILL NOT SUM UP TO 2*L+1 AND THE
C         POTENTIAL CROSS SECTION WILL BE UNDERESTIMATED. IN THIS
C         EXAMPLE THE SUM OF THE 3 J SEQUENCES 1/2, 3/2, 5/2 IS 2,
C         RATHER THAN 3 AS IT SHOULD BE FOR L=1, AND THE CONTRIBUTION
C         OF THE L=1 RESONANCES TO THE POTENTIAL SCATTERING CROSS
C         SECTION WILL ONLY BE 2/3 OF WHAT IT SHOULD BE, UNLESS THE
C         OTHER 2 J SEQUENCES (WITH DUPLICATE J VALUES) ARE INCLUDED
C         IN THE CALCULATION.
C
C     (3) EACH RESONANCE MUST HAVE AN ASSIGNED, PHYSICALLY POSSIBLE
C         J VALUE. PHYSICALLY IMPOSSIBLE OR AVERAGE J VALUES CANNOT BE
C         UNIQUELY INTERPRETED USING THE EQUATIONS IN ENDF-102 AND
C         THEIR USE WILL USUALLY RESULT IN PHYSICALLY UNRELIABLE CROSS
C         SECTIONS. THIS PROGRAM WILL CHECK ALL J VALUES AND IF ANY ARE
C         ARE FOUND TO BE PHYSICALLY IMPOSSIBLE (BASED ON TARGET SPIN
C         AND L VALUE) AN ERROR MESSAGE WILL BE PRINTED TO INDICATE THAT
C         THE RECONSTRUCTED CROSS SECTIONS WILL BE UNRELIABLE AND THE
C         PROGRAM WILL CONTINUE. IN AN ATTEMPT TO CALCULATE THE CORRECT
C         POTENTIAL SCATTERING CROSS SECTION THIS PROGRAM WILL SUBTRACT
C         THE POTENTIAL SCATTERING CONTRIBUTION DUE TO ALL FICTICIOUS J
C         SEQUENCES AND ADD THE CONTRIBUTION OF ALL PHYSICALLY POSSIBLE
C         J SEQUENCES (AS DESCRIBED ABOVE).
C
C         WARNING (LET THE USER BEWARE)
C         =============================
C         (A) IT CANNOT BE STRESSED ENOUGH THAT CROSS SECTIONS OBTAINED
C             USING PHYSICALLY IMPOSSIBLE J VALUES FOR REICH-MOORE AND
C             MULTI-LEVEL BREIT-WIGNER RESONANCE PARAMETERS WILL RESULT
C             IN UNRELIABLE CROSS SECTIONS. THE DECISION TO HAVE THIS
C             PROGRAM CONTINUE TO PROCESS WHEN THIS CONDITION IS FOUND
C             IS BASED ON AN ATTEMPT TO ALLOW THE USER TO AT LEAST HAVE
C             SOME RESULTS (HOWEVER BAD THEY MAY BE) IF THERE IS NO
C             OTHER EVALUATED DATA AVAILABLE.
C         (B) EVEN THOUGH THE REICH-MOORE AND MULTI-LEVEL EQUATIONS ARE
C             DEFINED AS ABSOLUTE OR SQUARED CONTRIBUTIONS WHICH MUST
C             ALL BE PHYSICALLY POSSIBLE, ATTEMPTING TO CORRECT THE
C             POTENTIAL CROSS SECTION (AS DESCRIBED ABOVE) CAN LEAD TO
C             NEGATIVE ELASTIC CROSS SECTIONS. THIS IS BECAUSE BASED ON
C             THE INFORMATION AVAILABLE IN THE EVALUATION IT IS NOT
C             NOT POSSIBLE TO CORRECTLY ACCOUNT FOR THE INTERFERENCE
C             BETWEEN THE RESONANCE AND POTENTIAL CONTRIBUTIONS FOR EACH
C             J SEQUENCE.
C
C     UNRESOLVED RESONANCE REGION
C     ---------------------------
C     IN THE UNRESOLVED RESONANCE REGION THE UNRESOLVED PARAMETERS
C     ARE USED TO CALCULATE INFINITELY DILUTE AVERAGE CROSS SECTIONS.
C     NOTE, IT IS IMPORTANT TO UNDERSTAND THAT FROM THE DEFINITION OF
C     THE UNRESOLVED PARAMETERS IT IS NOT POSSIBLE TO UNIQUELY CALCULATE
C     ENERGY DEPENDENT CROSS SECTIONS. ONLY AVERAGES OR DISTRIBUTIONS
C     MAY BE CALCULATED.
C
C     UNRESOLVED INTERPOLATION
C     ------------------------
C     IN THE UNRESOLVED RESONANCE REGION CROSS SECTIONS AT EACH ENERGY
C     ARE CALCULATED BY INTERPOLATING PARAMETERS. THIS IS THE CONVENTION
C     USED IN ENDF/B-4 AND EARLIER VERSIONS OF ENDF/B. THE ENDF/B-5
C     CONVENTION OF INTERPOLATING CROSS SECTIONS, NOT PARAMETERS, HAS
C     BEEN ABANDONED AS IMPRACTICAL SINCE IT CAN LEAD TO THE SITUATION
C     WHERE EXACTLY THE SAME PHYSICAL DATA CAN LEAD TO DIFFERENT RESULTS
C     DEPENDING ON WHICH OF THE THREE ENDF/B UNRESOLVED PARAMTER FORMATS
C     IS USED. FOR EXAMPLE, GIVEN A SET OF ENERGY INDEPENDENT UNRESOLVED
C     PARAMETERS IT IS POSSIBLE TO CODE THESE PARAMETERS IN EACH OF THE
C     THREE ENDF/B UNRESOLVED PARAMETER FORMATS. SINCE PHYSICALLY WE
C     ONLY HAVE ONE SET OF PARAMETERS WE WOULD EXPECT THE RESULTS TO BE
C     INDEPENDENT OF HOW THEY ARE REPRESENTED IN ENDF/B. UNFORTUNATELY
C     USING THE ENDF/B-5 CONVENTION TO INTERPOLATE CROSS SECTIONS CAN
C     LEAD TO THREE COMPLETELY DIFFERENT RESULTS. IN CONTRAST USING THE
C     ENDF/B-4 AND EARLIER CONVENTION OF INTERPOLATING PARAMETERS LEADS
C     TO COMPLETELY CONSISTENT RESULTS.
C
C     INTERNAL REPRESENTATION OF UNRESOLVED PARAMETERS
C     ------------------------------------------------
C     ANY OF THE THREE POSSIBLE REPRESENTATIONS OF UNRESOLVED PARAMETERS
C     CAN BE UNIQUELY REPRESENTED IN THE ALL PARAMETERS ENERGY DEPENDENT
C     REPRESENTATIONS WITH THE APPROPRIATE (ENDF/B VERSION DEPENDENT)
C     INTERPOLATION LAW. THIS IS DONE BY THE PROGRAM WHILE READING THE
C     UNRESOLVED PARAMETERS AND ALL SUBSEQUENT CALCULATIONS NEED ONLY
C     CONSIDER THE ALL PARAMETERS ENERGY DEPENDENT REPRESENTATION.
C
C     RESONANCE RECONSTRUCTION STARTING ENERGY GRID
C     ---------------------------------------------
C     AS IN ANY ITERATIVE METHOD THE WAY TO SPEED CONVERGENCE IS TO TRY
C     TO START CLOSE TO THE ANSWER. THIS PROGRAM ATTEMPTS TO DO THIS BY
C     STARTING FROM AN ENERGY GRID WHICH IS A GOOD APPROXIMATION TO A
C     SIMPLE BREIT-WIGNER LINE SHAPE,
C
C     SIGMA(X)=1.0/(1.0+X*X)
C
C     WHERE X IS THE DISTANCE FROM THE PEAK IN HALF-WIDTHS
C
C     SUBROUTINE SUBINT HAS A BUILT-IN TABLE OF NODES WHICH ARE THE
C     HALF-WIDTH MULTIPLES TO APPROXIMATE THE SIMPLE BREIT-LINE SHAPE
C     TO WITHIN 1 PER-CENT OVER THE ENTIRE INTERVAL 0 TO 500 HALF-WIDTHS
C
C     BETWEEN ANY TWO RESOLVED RESONANCES THE STARTING GRID IS BASED ON
C     THE HALF-WIDTHS OF THE TWO RESONANCES. FROM THE LOWER ENERGY
C     RESONANCE UP TO THE MID-POINT BETWEEN THE RESONANCES (MID-POINT
C     IS DEFINED HERE AS AN EQUAL NUMBER OF HALF-WIDTHS FROM EACH
C     RESONANCE) THE HALF-WIDTH OF THE LOWER ENERGY RESONANCE IS USED.
C     FROM THE MID-POINT UP TO THE HIGHER ENERGY RESONANCE THE HALF-
C     WIDTH OF THE UPPER ENERGY RESONANCE IS USED.
C
C     WITH THIS ALOGORITHM CLOSELY SPACED RESONANCES WILL HAVE ONLY
C     A FEW STARTING NODES PER RESONANCE (E.G. U-235). WIDELY SPACED
C     RESONANCES WILL HAVE MORE NODES PER RESONANCE (E.G. U-238). FOR
C     A MIX OF S, P, D ETC. RESONANCES THIS ALOGORITHM GUARANTEES AN
C     ADEQUTE DESCRIPTION OF THE PROFILE OF EVEN EXTREMELY NARROW
C     RESONANCES (WHICH MAY IMMEDIATELY CONVERGENCE TO THE ACCURACY
C     REQUESTED, THUS MINIMIZING ITERATION).
C
C     BACKGROUND CROSS SECTIONS
C     -------------------------
C     THE PROGRAM WILL SEARCH FOR BACKGROUND CROSS SECTIONS FOR TOTAL
C     (MT=1), ELASTIC (MT=2), FISSION (MT=18), FIRST CHANCE FISSION
C     (MT=19) AND CAPTURE (MT=102).
C
C     (1) THE BACKGROUND CROSS SECTIONS (FILE 3) CAN BE PRESENT OR NOT
C         PRESENT FOR EACH REACTION.
C     (2) IF FOR A GIVEN REACTION THE BACKGROUND CROSS SECTION IS
C         PRESENT, IT WILL BE ADDED TO THE RESONANCE CONTRIBUTION AND
C         THE RESULT WILL BE OUTPUT.
C     (3) IF FOR A GIVEN REACTION THE BACKGROUND IS NOT PRESENT THE
C         PROGRAM WILL,
C         (A) IF THE INPUT TO THE PROGRAM SPECIFIES NO OUTPUT FOR
C             REACTIONS WITH NO BACKGROUND THERE WILL BE NO OUTPUT.
C         (B) IF THE INPUT TO THE PROGRAM SPECIFIES OUTPUT FOR REACTIONS
C             WITH NO BACKGROUND,
C             (I) THE RESONANCE CONTRIBUTION TO TOTAL, ELASTIC OR
C                 CAPTURE WILL BE OUTPUT.
C             (II) IF ALL FISSION RESONANCE PARAMETERS ARE ZERO THE
C                  FISSION CROSS SECTION (MT=18) WILL NOT BE OUTPUT.
C                  OTHERWISE THE RESONANCE CONTRIBUTION OF THE FISSION
C                  (MT=18) WILL BE OUTPUT.
C             (III) THERE WILL BE NO OUTPUT FOR FIRST CHANCE FISSION
C                   (MT=19).
C
C     COMBINING RESONANCES AND BACKGROUND CROSS SECTIONS
C     --------------------------------------------------
C     IN ORDER TO BE COMBINED WITH THE RESONANCE CONTRIBUTION THE
C     BACKGROUND CROSS SECTIONS MUST BE GIVEN AT 0 KELVIN TEMPERATURE
C     AND MUST BE LINEARLY INTERPOLABLE. IF THESE CONDITIONS ARE MET
C     THE RESONANCE AND BACKGROUND CONTRIBUTIONS WILL BE ADDED TOGETHER
C     AND OUTPUT. IF THESE CONDITIONS ARE NOT MET THE BACKGROUND CROSS
C     SECTION WILL BE IGNORED AND ONLY THE RESONANCE CONTRIBUTION WILL
C     BE OUTPUT. IF THE BACKGROUND HAS NOT BEEN ADDED TO THE RESONANCE
C     CONTRIBUTION AFTER THIS PROGRAM FINISHES THE USER CAN MAKE THE
C     RESONANCE AND BACKGROUND CONTRIBUTIONS COMPATIBLE BY,
C
C     (1) IF THE BACKGROUND IS NOT LINEARLY INTERPOABLE, LINEARIZE THE
C         BACKGROUND (E.G., USE PROGRAM LINEAR).
C     (2) IF THE BACKGROUND IS NOT GIVEN AT 0 KELVIN, DOPPLER BROADEN
C         THE RESONANCE (NOT BACKGROUND) CONTRIBUTION TO THE SAME
C         TEMPERATURE AS THE BACKGROUND (E.G., USE PROGRAM SIGMA1).
C
C     ONCE THE RESONANCE AND BACKGROUND CONTRIBUTIONS HAVE BEEN MADE
C     COMPATIBLE THEY CAN BE ADDED TOGETHER (E.G., USE PROGRAM MIXER).
C
C     THE RECONSTRUCTION OF THE RESONANCE CONTRIBUTION TO THE CROSS
C     SECTION CAN BE QUITE EXPENSIVE (IN TERMS OF COMPUTER TIME). SINCE
C     THE RECONSTRUCTION IS PERFORMED BEFORE THE BACKGROUND CROSS
C     SECTIONS ARE READ, THE ABOVE CONVENTIONS HAVE BEEN ADOPTED IN
C     ORDER TO AVOID LOSE OF COMPUTER TIME INVOLVED IN RECONSTRUCTING
C     THE RESONANCE CONTRIBUTION.
C
C     COMMON ENERGY GRID
C     ------------------
C     THIS PROGRAM WILL RECONSTRUCT THE RESONANCE CONTRIBUTION TO THE
C     TOTAL, ELASTIC, FISSION AND CAPTURE CROSS SECTIONS ALL ON THE
C     SAME ENERGY GRID. EACH REACTION WILL THEN BE COMBINED WITH ITS
C     BACKGROUND CROSS SECTION (IF ANY) AND OUTPUT WITHOUT ANY FURTHER
C     THINNING. IF THERE ARE NO BACKGROUND CROSS SECTIONS, OR IF THE
C     BACKGROUND CROSS SECTION FOR ALL FOUR REACTIONS ARE GIVEN ON A
C     COMMON ENERGY GRID, THE OUTPUT FROM THIS PROGRAM WILL BE ON A
C     COMMON ENERGY GRID FOR ALL FOUR REACTIONS.
C
C     THERMAL ENERGY
C     --------------
C     IF THE RESONANCE REGION SPANS THERMAL ENERGY (0.0253 EV) THIS
C     POINT IS ALWAYS INCLUDED IN THE COMMON ENERGY GRID USED FOR ALL
C     REACTIONS AND WILL ALWAYS APPEAR IN THE OUTPUT DATA.
C
C     SECTION SIZE
C     ------------
C     SINCE THIS PROGRAM USES A LOGICAL PAGING SYSTEM THERE IS NO LIMIT
C     TO THE NUMBER OF POINTS IN ANY SECTION, E.G., THE TOTAL CROSS
C     SECTION MAY BE REPRESENTED BY 200,000 DATA POINTS.
C
C     SELECTION OF DATA
C     -----------------
C     THE PROGRAM SELECTS MATERIALS TO BE PROCESSED BASED EITHER ON
C     MAT (ENDF/B MAT NO.) OR ZA. THE PROGRAM ALLOWS UP TO 100 MAT OR
C     ZA RANGES TO BE SPECIFIED. THE PROGRAM WILL ASSUME THAT THE
C     ENDF/B TAPE IS IN EITHER MAT OR ZA ORDER, WHICHEVER CRITERIA IS
C     USED TO SELECT MATERIALS, AND WILL TERMINATE WHEN A MAT OR ZA
C     IS FOUND THAT IS ABOVE THE RANGE OF ALL REQUESTS.
C
C     ALLOWABLE ERROR
C     ---------------
C     THE RECONSTRUCTION OF LINEARLY INTERPOLABLE CROSS SECTIONS FROM
C     RESONANCE PARAMETERS CANNOT BE PERFORMED EXACTLY. HOWEVER IT CAN
C     BE PERFORMED TO VIRTUALLY ANY REQUIRED ACCURACY AND MOST
C     IMPORTANTLY CAN BE PERFORMED TO A TOLERANCE THAT IS SMALL COMPARED
C     TO THE UNCERTAINTY IN THE CROSS SECTIONS THEMSELVES. AS SUCH THE
C     CONVERSION OF CROSS SECTIONS TO LINEARLY INTERPOLABLE FORM CAN BE
C     PERFORMED WITH ESSENTIALLY NO LOSS OF INFORMATION.
C
C     THE ALLOWABLE ERROR MAY BE ENERGY INDEPENDENT (CONSTANT) OR ENERGY
C     DEPENDENT. THE ALLOWABLE ERROR IS DESCRIBED BY A TABULATED
C     FUNCTION OF UP TO 20 (ENERGY,ERROR) PAIRS AND LINEAR INTERPOLATION
C     BETWEEN TABULATED POINTS. IF ONLY ONE TABULATED POINT IS GIVEN THE
C     ERROR WILL BE CONSIDERED CONSTANT OVER THE ENTIRE ENERGY RANGE.
C     WITH THIS ENERGY DEPENDENT ERROR ONE MAY OPTIMIZE THE OUTPUT FOR
C     ANY GIVEN APPLICATION BY USING A SMALL ERROR IN THE ENERGY RANGE
C     OF INTEREST AND A LESS STRINGENT ERROR IN OTHER ENERGY RANGES,
C     E.G., 0.1 PER-CENT FROM 0 UP TO THE LOW EV RANGE AND A LESS
C     STRINGENT TOLERANCE AT HIGHER ENERGIES.
C
C     DEFAULT ALLOWABLE ERROR
C     -----------------------
C     IN ORDER TO INSURE CONVERENCE OF THE RESONANCE RECONSTRUCTION THE
C     ALLOWABLE ERROR MUST BE POSITIVE. IF THE USER INPUTS AN ERROR FOR
C     RESONANCE RECONSTRUCTION THAT IS NOT POSITIVE IT WILL BE SET TO
C     THE DEFAULT VALUE (CURRENTLY 0.1 PER-CENT) AND INDICATED AS SUCH
C     IN THE OUTPUT LISTING.
C
C     INTERVAL HALVING ALGORITHM
C     -------------------------
C     THIS PROGRAM WILL START BY CALCULATING THE CROSS SECTIONS AT THE
C     ENERGIES CORRESPONDING TO THE PEAK OF EACH RESONANCE, AS WELL AS
C     A FIXED NUMBER OF HALF-WIDTHS ON EACH SIDE OF EACH RESONANCE.
C     STARTING FROM THIS BASIC GRID OF POINTS THE PROGRAM WILL CONTINUE
C     TO HALF EACH INTERVAL UNTIL THE CROSS SECTIONS FOR ALL REACTIONS
C     AT THE CENTER OF THE INTERVAL CAN BE DEFINED BY LINEAR
C     INTERPOLATION FROM THE ENDS OF THE INTERVAL TO WITHIN THE USER
C     SPECIFIED ACCURACY CRITERIA.
C
C     DISTANT RESONANCE TREATMENT
C     ---------------------------
C     THE OPTION TO TREAT DISTANT RESONANCES, WHICH WAS AVAILABLE IN
C     EARLIER VERSIONS OF THIS PROGRAM, IS NO LONGER AVAILABLE, BECAUSE
C     IT WAS FOUND TO PRODUCE UNRELIABLE RESULTS. IN THIS VERSION OF
C     THE PROGRAM ALL RESONANCES ARE TREATED EXACTLY.
C
C     PROGRAM OPERATION
C     ==================================================================
C     EDIT MODE
C     ---------
C     IT IS SUGGESTED THAT BEFORE RUNNING THIS PROGRAM TO RECONSTRUCT
C     CROSS SECTIONS FROM RESONANCE PARAMETERS (WHICH CAN BE QUITE
C     EXPENSIVE) THE USER FIRST RUN THE PROGRAM IN THE EDIT MODE (SEE,
C     DESCRIPTION OF INPUT PARAMETERS BELOW). IN THE EDIT MODE THE
C     PROGRAM WILL READ, LIST AND EXTENSIVELY CHECK THE CONSISTENCY OF
C     ALL RESONANCE PARAMETERS AND ENDF/B DEFINED RESONANCE FLAGS. THIS
C     IS A VERY INEXPENSIVE MEANS OF CHECKING ALL DATA BEFORE INVESTING
C     A LARGE AMOUNT OF MONEY IN RECONSTRUCTING CROSS SECTIONS. ANY AND
C     ALL DIGNOSTICS RECEIVED FROM THE EDIT WILL SUGGEST HOW TO CORRECT
C     THE EVALUATED DATA TO MAKE IT CONSISTENT BEFORE RECONSTRUCTING
C     CROSS SECTIONS. IN ORDER TO OBTAIN MEANINGFUL RESULTS FROM THE
C     RECONSTRUCTION ALL SUGGESTED CHANGES TO THE EVALUATION SHOULD BE
C     PERFORMED BEFORE TRYING RECONSTRUCTION (OTHERWISE THE RESULT OF
C     RECONSTRUCTION WILL NOT BE RELIABLE).
C
C     RECONSTRUCTION MODE
C     -------------------
C     FOR EACH REQUESTED MATERIAL
C     ---------------------------
C     IF SECTION MF=1, MT=451 IS PRESENT COMMENTS WILL BE ADD TO
C     DOCUMENT THAT THE MATERIAL HAS BEEN PROCESSED. MF=1, MT=451 WILL
C     ALSO BE USED TO DETERMINE THE VERSION OF THE ENDF/B FORMAT WHICH
C     WILL ALLOW THE PROGRAM TO USE THE APPROPRIATE CONVENTIONS.
C
C     ALL OF THE FILE 2 RESONANCE PARAMETERS ARE FIRST READ AND THE
C     LINEARLY INTERPOLABLE CONTRIBUTION OF THE RESONANCE PARAMETERS
C     TO THE TOTAL, ELASTIC, CAPTURE AND FISSION CROSS SECTIONS IS
C     CALCULATED SIMULTANEOUSLY USING A COMMON ENERGY GRID FOR ALL
C     FOUR REACTIONS.
C
C     AFTER THE RESONANCE CONTRIBUTION HAS BEEN RECONSTRUCTED EACH OF
C     THE FIVE REACTIONS (MT=1, 2, 18, 19, 102) IS CONSIDERED SEPARATELY
C     FOR COMBINATION WILL THE BACKGROUND CROSS SECTION, IF ANY, AS
C     DESCRIBED ABOVE.
C
C     OUTPUT WILL INCLUDE THE ENTIRE EVALUATION, INCLUDING RESONANCES
C     PARAMETERS WITH LRU MODIFIED (AS DESCRIBED ABOVE) TO INDICATE
C     THAT THE RESONANCE CONTRIBUTION HAS ALREADY BEEN ADDED TO THE
C     FILE 3 CROSS SECTIONS.
C
C     THE CYCLE OF RECONSTRUCTING THE RESONANCE CONTRIBUTION AND ADDING
C     THE BACKGROUND WILL BE REPEATED FOR EACH MATERIAL REQUESTED.
C
C-----2016/3/10 - This option is no longer allowed - today's computers
C                 are so mjuch faster that this option is no longer
C                 needed.
C     PROCESS ONLY A PORTION OF RESONANCE REGION
C     ==================================================================
C     MODERN EVALUATIONS MAY BE EXTREMELY LARGE AND IT MAY NOT BE
C     POSSIBLE TO PROCESS AN ENTIRE EVALUATION (I.E., ADD THE RESONANCE
C     CONTRIBUTION) DURING A SINGLE COMPUTER RUN.
C
C     ALSO IN THE CASE WHERE YOU ARE ONLY INTERESTED IN THE CROSS
C     SECTIONS OVER A SMALL ENERGY RANGE, YOU MAY NOT WANT TO PROCESS
C     AN ENTIRE EVALUATION, E.G., IF YOU ONLY WANT TO KNOW WHAT THE
C     CROSS SECTIONS ARE NEAR THERMAL ENERGY, 0.0253 EV.
C
C     IN ORDER TO ALLOW AN EVALUATION TO BE PROCESSED USING A NUMBER OF
C     SHORTER COMPUTER RUNS AN OPTION HAS BEEN ADDED TO THIS PROGRAM TO
C     ALLOW THE USER TO SPECIFY THE ENERGY RANGE TO BE PROCESSED.
C
C     USING THIS OPTION YOU MAY START AT THE LOWEST ENERGY (ZERO UP TO
C     SOME ENERGY) AND USE THE RESULTS OF THIS RUN AS INPUT TO THE
C     NEXT RUN, WHERE YOU CAN SPECIFY THE NEXT ENERGY RANGE. THIS
C     CYCLE CAN BE REPEATED UNTIL YOU HAVE PROCESSED THE ENTIRE
C     EVALUATION.
C
C     WARNING - THIS OPTION SHOULD BE USED WITH EXTREME CARE - THIS
C     OPTION HAS BEEN RELUCTANTLY ADDED - RELUCTANTLY BECAUSE IT CAN
C     BE EXTREMELY DANGEROUS TO USE THIS OPTION UNLESS YOU CAREFULLY
C     CHECKED WHAT YOU ARE DOING.
C
C     THE OPTION SHOULD ONLY BE USED AS FOLLOWS,
C     1) YOU MUST PROCESS USING ENERGY RANGES STARTING AT LOW ENERGY
C        AND WORKING YOUR WAY TOWARD HIGH ENERGY, E.G.,
C         0.0   TO  3.0+3
C         3.0+3 TO 10.0+3
C        10.0+3 TO 80.0+3, ETC.
C     2) FOR THE LAST ENERGY RANGE THE LOWER ENERGY LIMIT MUST BE
C        NON-ZERO (WHERE TO START) AND THE UPPER ENERGY LIMIT MUST
C        BE ZERO (NO LIMIT)
C        80.0+3 TO  0.0
C
C     IF YOU ARE ONLY INTERESTED IN THE CROSS SECTION OVER A NARROW
C     ENERGY INTERVAL AND DO NOT INTENT TO MAKE ANY OTHER USE OF THE
C     RESULTS, YOU CAN IGNORE THESE WARNINGS AND MERELY SPECIFY ANY
C     ENERGY INTERVAL OVER WHICH YOU WISH CALCULATIONS TO BE
C     PERFORMED.
C
C     NORMALLY WHEN THIS PROGRAM PROCESSES AN EVALUATION IT WILL SET
C     FLAGS IN THE EVALUATION TO PREVENT THE SAME RESONANCE
C     CONTRIBUTION FROM BEING ADDED TO THE CROSS SECTION MORE THAN
C     ONCE, SHOULD YOU USE THE OUTPUT FROM THIS PROGRAM AS INPUT TO
C     THE PROGRAM.
C
C     WHEN PROCESSING ONLY PORTIONS OF THE RESONANCE REGION THIS
C     PROGRAM CANNOT SET THESE FLAGS TO PROTECT AGAINST ADDING THE
C     RESONANCE CONTRIBUTION MORE THAN ONCE - WHICH MAKES USE OF
C     THIS OPTION EXTREMELY DANGEROUS.
C
C     ONLY YOU CAN CHECK TO MAKE SURE THAT YOU HAVE CORRECTLY
C     INCLUDED EACH ENERGY RANGE ONLY ONCE - SEE THE COMMENT LINES
C     AT THE END OF SECTION, MF=1, MT=451, FOR A COMPLETE RECORD
C     OF EACH RUN USING THIS PROGRAM. THIS SECTION WILL CONTAIN
C     LINES OF THE FORM
C
C     ***************** PROGRAM RECENT (VERSION 2021-1) *************
C     ONLY PROCESS  0.00000+ 0 TO  3.00000+ 3 EV
C     ***************** PROGRAM RECENT (VERSION 2021-1) *************
C     ONLY PROCESS  3.00000+ 3 TO  1.00000+ 4 EV
C     ***************** PROGRAM RECENT (VERSION 2021-1) *************
C     ONLY PROCESS  1.00000+ 4 TO  8.00000+ 4 EV
C     ***************** PROGRAM RECENT (VERSION 2021-1) *************
C     ONLY PROCESS  8.00000+ 4 TO  2.00000+ 7 EV
C
C     YOU SHOULD CHECK TO INSURE THAT THERE ARE NO OVERLAPPING ENERGY
C     RANGES OR MISSING ENERGY RANGES.
C
C     WHEN YOU INDICATE BY INPUT THAT YOU ARE ABOUT TO PROCESS THE
C     LAST ENERGY RANGE (SEE ABOVE, LOWER ENERGY LIMIT = NON-ZERO,
C     UPPER ENERGY LIMIT = ZERO), THIS PROGRAM WILL ASSUME THAT
C     YOU HAVE NOW COMPLETED ALL PROCESSING - AND ONLY THEN WILL
C     IT SET FLAGS IN THE EVALUATION TO PREVENT THE RESONANCE
C     CONTRIBUTION FROM BEING ADDED MORE THAN ONCE. FOR THIS REASON
C     YOU CANNOT PROCESS STARTING WITH ENERGY INTERVALS AT HIGH
C     ENERGY AND WORKING TOWARD LOW ENERGY - YOU MUST START AT LOW
C     ENERGY AND WORK TOWARD HIGH ENERGY.
C-----2016/3/10 - This option is no longer allowed - today's computers
C
C     I/O FILES
C     ==================================================================
C     INPUT FILES
C     -----------
C     UNIT  DESCRIPTION
C     ----  -----------
C       2   INPUT LINE (BCD - 80 CHARACTERS/RECORD)
C      10   ORIGINAL ENDF/B DATA (BCD - 80 CHARACTERS/RECORD)
C
C     OUTPUT FILES
C     ------------
C     UNIT  DESCRIPTION
C     ----  -----------
C       3   OUTPUT REPORT (BCD - 120 CHARACTERS/RECORD)
C      11   FINAL ENDF/B DATA (BCD - 80 CHARACTERS/RECORD)
C
C     SCRATCH FILES
C     -------------
C     UNIT  DESCRIPTION
C     ----  -----------
C      12   SCRATCH FILE FOR DATA RECONSTRUCTED FROM RESONANCE
C           PARAMETERS (BINARY - 100200 WORDS/RECORD)
C      14   SCRATCH FILE FOR COMBINED FILE 2 AND 3 DATA
C           (BINARY - 40080 WORDS/RECORD)
C
C     OPTIONAL STANDARD FILE NAMES (SEE SUBROUTINE FILEIO)
C     ==================================================================
C     UNIT  FILE NAME
C     ----  ----------
C       2   RECENT.INP
C       3   RECENT.LST
C      10   ENDFB.IN
C      11   ENDFB.OUT
C      12   (SCRATCH)
C      14   (SCRATCH)
C
C     INPUT CARDS
C     ==================================================================
C     LINE  COLS.  FORMAT  DESCRIPTION
C     ----  -----  ------  -----------
C       1    1-11    I11   RETRIEVAL CRITERIA (0=MAT, 1=ZA)
C                          THIS OPTION DEFINED WHETHER COLUMNS 1-22 OF
C                          SUBSEQUENT INPUT CARDS SHOULD BE INTERPRETED
C                          TO BE MAT OR ZA RANGES.
C           12-22   E11.4  FILE 2 MINIMUM ABSOLUTE CROSS SECTION
C                          (IF 1.0E-10 OR LESS IS INPUT THE PROGRAM
C                          WILL USE 1.0E-10)
C           23-33    I11   TREATMENT OF REACTIONS FOR WHICH BACKGROUND
C                          CROSS SECTION IS NOT GIVEN.
C                          = 0 - IGNOR (I.E. NO OUTPUT)
C                          = 1 - OUTPUT RESONANCE CONTRIBUTION.
C                          THIS OPTION IS USEFUL WITH PARTIAL EVALUATION
C                          (E.G. ENDF/B-5 DOSIMETRY LIBRARY) WHERE ONLY
C                          ONE OR MORE OF THE REACTIONS ARE OF ACTUAL
C                          INTEREST.
C                          WARNING...THE USE OF THIS FIELD HAS BEEN
C                          CHANGED. THIS FIELD WAS PREVIOUSLY USED TO
C                          DEFINE THE PRECISION OF THE CALCULATION AND
C                          OUTPUT. THE FORMER DEFINITION OF THIS FIELD
C                          WAS...
C                          MINIMUM ENERGY SPACING FLAG
C                          = 0 - 6 DIGIT MINIMUM ENERGY SPACING.
C                                STANDARD 6 DIGIT E11.4 OUTPUT.
C                          = 1 - 9 DIGIT MINIMUM ENERGY SPACING.
C                                STANDARD 6 DIGIT E11.4 OUTPUT.
C                          = 2 - 9 DIGIT MINIMUM ENERGY SPACING.
C                                VARIABLE 9 DIGIT F FORMAT OUTPUT.
C                          FROM EXPERIENCE IT HAS BEEN FOUND THAT
C                          FAILURE TO SET THIS OPTION TO 2 CAN RESULT
C                          IN LARGE ERRORS IN THE FINAL DATA. THEREFORE
C                          INTERNALLY THIS OPTION IS SET TO 2.
C           34-44    I11   OPERATING MODE
C                          = 0 - CACULATE. MINIMUM OUTPUT LISTING
C                          = 1 - CACULATE. LIST ALL RESONANCE PARAMETERS
C                          = 2 - EDIT MODE. NO CALCULATION. LIST ALL
C                                RESONANCE PARAMETERS.
C                          NOTE, THE EDIT MODE (=2) IS THE SUGGESTED
C                          MODE TO FIRST TEST THE CONSISTENCY OF THE
C                          EVALUATED DATA, BEFORE RECONSTRUCTING CROSS
C                          SECTIONS (SEE, COMMENTS ABOVE).
C           45-55    I11   NEGATIVE CROSS SECTIOIN TREATMENT
C                          = 0 - O.K. - NO CHANGE
C                          = 1 - SET = 0
C           56-66    I11   MONITOR MODE SELECTOR
C                          = 0 - NORMAL OPERATION
C                          = 1 - MONITOR PROGRESS OF RECONSTRUCTION OF
C                                FILE 2 DATA AND COMBINING FILE 2 AND
C                                FILE 3 DATA. EACH TIME A PAGE OF DATA
C                                POINTS IS WRITTEN TO A SCRATCH FILE
C                                PRINT OUT THE TOTAL NUMBER OF POINTS
C                                ON SCRATCH AND THE LOWER AND UPPER
C                                ENERGY LIMITS OF THE PAGE (THIS OPTION
C                                MAY BE USED IN ORDER TO MONITOR THE
C                                EXECUTION SPEED OF LONG RUNNING JOBS).
C       2    1-72    A72   ENDF/B INPUT DATA FILENAME
C                          (STANDARD OPTION = ENDFB.IN)
C       3    1-72    A72   ENDF/B OUTPUT DATA FILENAME
C                          (STANDARD OPTION = ENDFB.OUT)
C     4-N    1-11    I11   MINIMUM MAT OR ZA (SEE COLS. 1-11, LINE 1)
C           12-22    I11   MAXIMUM MAT OR ZA (SEE COLS. 1-11, LINE 1)
C                          UP TO 100 MAT OR ZA RANGES MAY BE SPECIFIED,
C                          ONE RANGE PER LINE. THE LIST IS TERMINATED
C                          BY A BLANK LINE. IF THE THE UPPER LIMIT OF
C                          ANY REQUEST IS LESS THAN THE LOWER LIMIT THE
C                          UPPER LIMIT WILL BE SET EQUAL TO THE LOWER
C                          LIMIT. IF THE FIRST REQUEST LINE IS BLANK IT
C                          WILL TERMINATE THE REQUEST LIST AND CAUSE ALL
C                          DATA TO BE RETRIEVED (SEE EXAMPLE INPUT).
C----- 2016/3/10 - Partial Processing no longer allowed.
C                  If these fields are not blank the code will STOP
C                  with a WARNING that this is no longer allowed.
C           23-33   E11.4  LOWER ENERGY LIMIT FOR PROCESSING.
C           34-44   E11.4  UPPER ENERGY LIMIT FOR PROCESSING.
C                         *THE LOWER AND UPPER ENERGY LIMITS MUST BE
C                          ZERO, OR BLANK, UNLESS YOU WISH TO ONLY
C                          PROCESS A PORTION OF RESONANCE REGIONS.
C                         *THESE ENERGY LIMITS ARE ONLY READ FROM THE
C                          FIRST MAT/ZA REQUEST LINE
C                         *IF BOTH ARE ZERO (OR BLANK) THE ENTIRE
C                          RESONANCE REGION FOR EACH MATERIAL WILL BE
C                          PROCESSED
C                         *IF LIMITS ARE INPUT ONLY THAT PORTION OF THE
C                          RESONANCE REGION FOR EACH MATERIAL WHICH
C                          LIES BETWEEN THESE LIMITS WILL BE PROCESSED
C                         *SEE INSTRUCTIONS ABOVE BEFORE USING THIS
C                          OPTION.
C----- 2016/3/10 - Partial Processing no longer allowed.
C     VARY   1-11   E11.4  ENERGY FOR FILE 2 ERROR LAW     (  SEE   )
C           12-22   E11.4  ERROR FOR FILE 2 ERROR LAW      (COMMENTS)
C                                                          ( BELOW  )
C
C     NOTE, THIS VERSION OF THE PROGRAM DOES NOT THIN THE COMBINED FILE
C     FILE 2 + 3 DATA. AS SUCH THE ERROR LAW FOR COMBINING FILE 2 + 3
C     WHICH WAS REQUIRED IN EARLIER VERSIONS OF THIS CODE ARE NO LONGER
C     REQUIRED.
C
C     THE FILE 2 ERROR LAW MAY BE ENERGY INDEPENDENT (DEFINED BY A
C     SINGLE ERROR) OR ENERGY DEPENDENT (DEFINED BY UP TO 20 ENERGY,
C     ERROR PAIRS). FOR THE ENERGY DEPENDENT CASE LINEAR INTERPOLATION
C     WILL BE USED TO DEFINE THE ERROR AT ENERGIES BETWEEN THOSE AT
C     WHICH THE ERROR IS TABULATED. THE ERROR LAW IS TERMINATED BY A
C     BLANK LINE. IF ONLY ONE ENERGY, ERROR PAIR IS GIVEN THE LAW WILL
C     BE CONSIDERED TO BE ENERGY INDEPENDENT. IF MORE THAN ONE PAIR
C     IS GIVEN IT BE CONSIDERED TO BE ENERGY DEPENDENT (NOTE, THAT
C     FOR A CONSTANT ERROR THE ENERGY INDEPENDENT FORM WILL RUN FASTER.
C     HOWEVER, FOR SPECIFIC APPLICATIONS AN ENERGY DEPENDENT ERROR MAY
C     BY USED TO MAKE THE PROGRAM RUN CONSIDERABLE FASTER).
C
C     ALL ENERGIES MUST BE IN ASCENDING ENERGY ORDER. FOR CONVERGENCE
C     OF THE FILE 2 RECONSTRUCTION ALGORITHM ALL THE ERRORS MUST BE
C     POSITIVE. IF ERROR IS NOT POSITIVE IT WILL BE SET EQUAL TO THE
C     STANDARD OPTION (CURRENTLY 0.001, CORRRESPONDING TO 0.1 PER-CENT).
C     IF THE FIRST LINE OF THE ERROR LAW IS BLANK IT WILL TERMINATE THE
C     ERROR LAW AND THE ERROR WILL BE TREATED AS ENERGY INDEPENDENT,
C     EQUAL TO THE STANDARD OPTION (CURRENTLY, 0.1 PER-CENT). SEE,
C     EXAMPLE INPUT 4.
C
C     EXAMPLE INPUT NO. 1
C     -------------------
C     CONSIDER ALL URANIUM ISOTOPES AND TH-232. CONSIDER CROSS SECTIONS
C     WHICH ARE LARGER THAN 1.0E-8 BARNS IN ABSOLUTE VALUE. ONLY OUTPUT
C     REACTIONS FOR WHICH A BACKGROUND IS GIVEN. LIST ALL PARAMETERS AND
C     CALCULATE CROSS SECTIONS. MONITOR THE EXECUTION PROGRESS OF THE
C     PROGRAM. BETWEEN 0 AND 100 EV USE 0.1 PER-CENT ACCURACY. BETWEEN
C     100 EV AND 1 KEV VARY THE ACCURACY FROM 0.1 TO 1 PER-CENT. ABOVE
C     1 KEV USE 1 PER-CENT ACCURACY.
C
C     EXPLICITLY SPECIFY THE STANDARD FILENAMES.
C
C     THE FOLLOWING 11 INPUT CARDS ARE REQUIRED.
C
C          1 1.00000-08          0          1          0         1
C ENDFB.IN
C ENDFB.OUT
C      92000      92999
C      90232                  (UPPER LIMIT AUTOMATICALLY SET TO 90232)
C                             (END REQUEST LIST)
C 0.00000+ 0 1.00000-03
C 1.00000+02 1.00000-03
C 1.00000+03 1.00000-02
C 1.00000+09 1.00000-02
C                             (END FILE 2 ERROR LAW)
C
C     EXAMPLE INPUT NO. 2
C     -------------------
C     CONSIDER ALL URANIUM ISOTOPES AND TH-232. CONSIDER CROSS SECTIONS
C     WHICH ARE LARGER THAN 1.0E-8 BARNS IN ABSOLUTE VALUE. ONLY OUTPUT
C     REACTIONS FOR WHICH A BACKGROUND IS GIVEN. CROSS SECTIONS WILL BE
C     CALCULATED, BUT PARAMETERS WILL NOT BE LISTED. THE PROGRESS OF THE
C     PROGRAM WILL NOT BE MONITORED. USE 0.1 PER-CENT ACCURACY FOR ALL
C     ENERGIES. SINCE 0.1 PER-CENT IS THE STANDARD OPTION FOR THE ERROR
C     LAW THE FIRST ERROR LAW LINE MAY BE LEFT BLANK.
C
C     LEAVE THE DEFINITION OF THE FILENAMES BLANK - THE PROGRAM WILL
C     THEN USE THE STANDARD FILENAMES.
C
C     THE FOLLOWING 7 INPUT CARDS ARE REQUIRED.
C
C          1 1.00000-08          0          0          0         0
C
C
C      92000      92999
C      90232                  (UPPER LIMIT AUTOMATICALLY SET TO 90232)
C                             (END REQUEST LIST)
C                             (USE STANDARD OPTION FOR ERROR LAW)
C
C     EXAMPLE INPUT NO. 3
C     -------------------
C     THE SAME AS EXAMPLE INPUT NO. 2, ONLY IN THIS CASE ONLY CALCULATE
C     CROSS SECTIONS OVER THE ENERGY RANGE 0.01 TO 0.1 EV - ACROSS THE
C     THERMAL ENERGY RANGE. NOTE, THE ONLY DIFFERENCE BETWEEN THE INPUT
C     PARAMETERS IN THIS CASE AND IN EXAMPLE NO. 2, IS THAT ON THE
C     SECOND INPUT LINE WE HAVE ADDED THE ENERGY RANGE 0.01 TO 0.1 EV.
C     USE \PREPRO94\LINEAR\ENDFB.OUT AS INPUT AND ENDFB.OUT AS OUTPUT -
C     SINCE ENDFB.OUT IS THE STANDARD OUTPUT FILENAME THE NAME CAN BE
C     EITHER INCLUDED IN THE INPUT OR LEFT BLANK.
C
C     THE FOLLOWING 7 INPUT CARDS ARE REQUIRED.
C
C          1 1.00000-08          0          0          0         0
C \PREPRO94\LINEAR\ENDFB.OUT
C ENDFB.OUT
C      92000      92999 1.00000- 2 1.00000- 1
C      90232                  (UPPER LIMIT AUTOMATICALLY SET TO 90232)
C                             (END REQUEST LIST)
C                             (USE STANDARD OPTION FOR ERROR LAW)
C
C     EXAMPLE INPUT NO. 4
C     -------------------
C     RECONSTRUCT ALL DATA. OUTPUT ALL REACTIONS, REGARDING OF WHETHER
C     OR NOT THERE IS A BACKGROUND CROSS SECTION. DO NOT MONITOR THE
C     PROGRESS OF THE PROGRAM. RECONSTRUCT CROSS SECTIONS TO 1 PER-CENT
C     ACCURACY. USE \ENDFB6\LINEAR\ZA092238 AS INPUT AND
C     \ENDFB6\RECENT\ZA092238 AS OUTPUT.
C
C     THE FOLLOWING 6 INPUT CARDS ARE REQUIRED.
C
C          0 0.0                 1          0          0         0
C \ENDFB6\ZA092238
C \ENDFB6\RECENT\ZA092238
C                       (RETRIEVE ALL DATA, END REQUEST LIST)
C            1.00000- 2
C                       (END FILE 2 ERROR LAW)
C
C     EXAMPLE INPUT NO. 5
C     -------------------
C     RECONSTRUCT ALL DATA. ONLY OUTPUT REACTIONS FOR WHICH A BACKGROUND
C     CROSS SECTION IS GIVEN. DO NOT MONITOR THE PROGRESS OF THE PROGRAM
C     RECONSTRUCT CROSS SECTIONS TO 0.1 PER-CENT ACCURACY. USE ENDFB.IN
C     AS INPUT AND ENDFB.OUT AS OUTPUT.
C
C     THIS CORRESPONDS TO USING ALL OF THE STANDARD OPTONS BUILT-IN TO
C     THE PROGRAM AND ALL INPUT CARDS MAY BE BLANK.
C
C     IN THIS CASE THE FOLLOWING 5 INPUT CARDS ARE REQUIRED.
C     (ZEROES ARE INDICATED ON THE FIRST LINE, BELOW, ONLY TO INDICATE
C     WHERE THE LINE IS. THE ACTUAL INPUT LINE CAN BE COMPLETELY BLANK).
C
C          0 0.0                 0          0          0         0
C                       (USE STANDARD INPUT FILENAME = ENDFB.IN)
C                       (USE STANDARD OUTPUT FILENAME = ENDFB.OUT)
C                       (RETRIEVE ALL DATA, END REQUEST LIST)
C                       (0.1 ERROR, END FILE 2 ERROR LAW)
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
C-----08/08/2012 DEFINE CODE NAME
      CHARACTER*8 CODENAME
      COMMON/NAMECODE/CODENAME
      CHARACTER*4 LINE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/MAXIE/NEDSEC,NEDRES,NEDNOD
      COMMON/PAGER/NPAGE,NPAGP1,NPAGM1
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/COPC/LINE(17)
      COMMON/COPI/MFIELD(3)
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/MINNIE/EMIN,EMAX,DEMIN
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/LISOCOM/LISO
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C-----08/08/2012 DEFINE CODE NAME
      CODENAME = 'RECENT  '
C-----INITIALIZE TIMER
      CALL TIMER
c***** DEBUG
c-----Print ALL RRR sequences that do not span RRR
c     open(50,file='RRR-BAD',access='APPEND')
c     open(51,file='RRR-OK' ,access='APPEND')
c-----Print ALL URR sequences that do not span URR
c     open(50,file='URR-BAD',access='APPEND')
c     open(51,file='URR-OK' ,access='APPEND')
c***** DEBUG
c-----------------------------------------------------------------------
C
C     DEFINE ALL I/O UNITS.
C
c-----------------------------------------------------------------------
      CALL FILEIO
C-----DEFINE THE NUMBER OF POINTS IN EACH PAGE OF DATA.
C-----01/04/07 - SWITCHED FROM MAXRES TO MAXPTX.
      NPAGE=MAXPTX
      NPAGP1=NPAGE+1
      NPAGM1=NPAGE-1
C-----DEFINE NUMBER OF SECTIONS, RESONANCES AND NODES.
      NEDSEC=0
      NEDRES=0
      NEDNOD=0
C-----DEFINE MINIMUM AND MAXIMUM ENERGIES OF INTEREST AND MINIMUM
C-----ALLOWABLE ENERGY INTERVAL FOR ANY ONE RESONANCE REGION.
      EMIN  = 1.0d-5
      EMAX  = 1.0d+10
      DEMIN = 1.0d-05
C-----DEFINE COMMONLY USED CONSTANTS.
      ZERO  = 0.0d0
      HALF  = 0.5d0
      ONE   = 1.0d0
      TWO   = 2.0d0
      THREE = 3.0d0
      FOUR  = 4.0d0
      EIGHT = 8.0d0
      TEN   = 1.0d1
      PI    = DACOS(-1.0d0)
      PI2   = TWO*PI
C-----OUTPUT PROGRAM IDENTIFICATION.
      WRITE(OUTP,130)
      WRITE(*   ,130)
C-----READ AND CHECK ALL INPUT PARAMETERS.
      CALL READIN
c-----------------------------------------------------------------------
C
C     READ, LIST AND OUTPUT ENDF/B TAPE LABEL.
C
c-----------------------------------------------------------------------
      CALL COPYL
      WRITE(OUTP,140) LINE,MFIELD(1)
      WRITE(*   ,140) LINE,MFIELD(1)
C-----INITIALIZE FISSILE FLAG OFF AND MF=1, MT=451 SECTION PRESENT FLAG
C-----TO OFF.
   10 LFI=0
      MT451=0
C-----FIND NEXT REQUESTED MAT AND TERMINATE IF RETURNED MAT IS NOT
C-----POSITIVE.
      CALL NXTMAT
      IF(MATH.GT.0) GO TO 40
C-----END OF RUN. TEND LINE HAS ALREADY BEEN OUTPUT.
   20 CONTINUE
      WRITE(OUTP,160) MAXSEC,MAXRES,MAXRES,NEDSEC,NEDNOD,NEDRES
      IF(NEDSEC.GT.MAXSEC.OR.NEDNOD.GT.MAXRES.OR.NEDRES.GT.MAXRES)
     1 WRITE(OUTP,170)
      WRITE(OUTP,180)
      WRITE(*   ,160) MAXSEC,MAXRES,MAXRES,NEDSEC,NEDNOD,NEDRES
      IF(NEDSEC.GT.MAXSEC.OR.NEDNOD.GT.MAXRES.OR.NEDRES.GT.MAXRES)
     1 WRITE(*   ,170)
      WRITE(*   ,180)
c-----Print final WARNING if data not tabulated to same Maximum Energy
      CALL MAXIE4(1)
c***** DEBUG
c-----Print ALL RRR sequences that do not span RRR
c     CLOSE(50)
c     CLOSE(51)
c***** DEBUG
      CALL ENDIT
c-----------------------------------------------------------------------
C
C     FIND FILE 1 OR 2.
C
c-----------------------------------------------------------------------
   30 CALL CONTI
      IF(MTH.gt.0) go to 40
      CALL CONTO               ! SEND
      IF(MATH.lt.0) go to 20
      IF(MATH.eq.0) go to 10
      go to 30
c  50 IF(MFH-1) 90,60,70       ! MF/MT=1/451 Comments
c  60 IF(MTH-451) 80,130,90
   40 IF(MFH.lt.1) go to 70
      IF(MFH.gt.1) go to 50
      IF(MTH.lt.451) go to 60
      IF(MTH.eq.451) go to 110
      go to 70
   50 IF(MFH.lt.2) go to 70
      IF(MFH.eq.2) go to 120
      go to 90
C-----COPY SECTION.
   60 CALL CONTO
      CALL COPYS
      GO TO 30
C-----COPY FILE.
   70 CALL CONTO
   80 CALL COPYF
      GO TO 30
C-----COPY MAT
   90 CALL CONTO
  100 CALL COPYM
      GO TO 10
C-----MF=1, MT=451 FOUND. ADD COMMENT CARDS AND DEFINE ENDF/B FORMAT
C-----VERSION - TO USE ALL CONVENTIONS ASSOICATED WITH CORRECT FORMAT
C-----VERSION).
  110 MT451=1
      CALL FILE1(IMDONE)
C-----COPY REMAINDER OF MAT IF NO RESONANCE PARAMETERS OR MAT HAS
C-----ALREADY BEEN PROCESSED. OTHERWISE COPY TO END OF FILE 1 (MF=1).
      IF(IMEDIT.EQ.2) GO TO 80
      IF(IMDONE.le.0) go to 100
      go to 80
C-----FILE 2 FOUND. PRINT WARNING MESSAGE IF NO MF=1, MT=451.
  120 IF(MT451.LE.0) WRITE(OUTP,150)
C-----PROCESS ALL OF FILE 2 (LIST ALL PARAMETERS AND/OR PROCESS INTO
C-----POINTWISE FORM).
      CALL CONTO
      CALL FILE2
C-----EDIT FILE 3 OR COMBINE FILE 2 AND 3 CONTRIBUTIONS AND OUTPUT.
C-----COPY REMAINDER OF MAT.
      CALL FILE3
C-----ENTIRE MATERIAL HAS PROCESSED. PROCEED TO NEXT MAT.
      GO TO 10
  130 FORMAT(' Calculate Cross Sections from Resonance Parameters',
     1 ' (RECENT 2021-1)'/1X,78('='))
  140 FORMAT(1X,78('=')/' ENDF/B Tape Label'/1X,78('=')/1X,16A4,A2,I4)
  150 FORMAT(1X,7('WARNING...'),'WARNING'/
     1 ' No Section MF=1, MT=451.'/
     2 ' (1) Cannot Determine ENDF/B Format Version. Will'/
     3 '     Assume and Use ALL ENDF/B-6 Conventions.'/
     4 ' (2) Cannot Determine if there are Resonance Parameters(LRP).'/
     6 ' (3) Cannot Determine if Material is Fissile (LFI).'/
     7 ' Will Read Data to Answer Points (2) AND (3).')
  160 FORMAT(
     1 ' Core Allocation and Requirements'/1X,78('=')/
     2 9X,'Sections   Nodes Parameter'/28X,'Storage'/1X,78('=')/
     3 ' Allocated',I7,I8,I10/
     4 ' Required ',I7,I8,I10/1X,78('='))
  170 FORMAT(1X,7('WARNING...'),'WARNING'/
     1       ' WARNING...Before Using this Program to Calculate'/
     2       '           Cross Sections the Core Allocation MUST'/
     3       '           be Increased to at Least the Requirements'/
     4       '           Described Above. Failure to do this Will'/
     5       '           Cause the Program to Abort During Eexecution'/
     6 1X,78('='))
  180 FORMAT(' End of Run'/1X,78('='))
      END
      SUBROUTINE READIN
C=======================================================================
C
C     READ AND CHECK ALL INPUT PARAMETERS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*4 MESS1,MESS2,MESS3,MESS4,MESS6
      CHARACTER*72 NAMEIN,NAMEOUT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/MATZA/MODGET,NMATZA,MATMIN(101),MATMAX(101)
      COMMON/OKERR3/ENER3(1000),ER3(1000),MAXER3,NOERR
      COMMON/ERRORCOM/ERRXC3,ERMINMAX,ERMT2,ERMT102,ERSOFT,XCLOW
      COMMON/FIXPOT/MYPOT
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
      COMMON/NAMEX/NAMEIN,NAMEOUT
c***** DEBUG
c     COMMON/MISSINGCOM/EMISSING
c***** DEBUG
      DIMENSION MESS1(2),MESS2(9,2),MESS3(10,3),MESS4(9,2),
     1 MESS6(2)
C-----DEFINE STANDARD MINIMUM ALLOWABLE ERROR (PRESENTLY 0.1 PER-CENT).
      DATA ERRMIN/1.0d-3/
C-----DEFINE MINIMUM ABSOLUTE CROSS SECTION OF INTEREST (CROSS SECTION
C-----MAY BE POSITIVE OR NEGATIVE, BUT IF BOTH ENDS OF ANY INTERVAL ARE
C-----CLOSER TO ZERO THAN XCINP  THE INTERVAL WILL NOT BE FURTHER
C-----SUB-DIVIDED).
c-----2021/3/22 - No longer an input option
      DATA XCINP/1.0d-30/
C-----DEFINE ALL OUTPUT MESSAGES.
      DATA MESS1/' MAT','  ZA'/
      DATA MESS2/
     1 '  No',' Out','put ','    ','    ','    ','    ','    ','    ',
     2 '    ',' Out','put ','(Res','onan','ce C','ontr','ibut','ion)'/
      DATA MESS3/
     1 ' Cal','cula','te. ','Mini','mum ','Outp','ut L','isti','ng. ',
     2 '    ',
     3 ' Cal','cula','te. ','List',' Res','onan','ce P','aram','eter',
     4 's.  ',
     5 ' Edi','t Mo','de. ','List',' Res','onan','ce P','aram','eter',
     6 's.  '/
      DATA MESS4/
     1 '  No',' Cha','nge ','(All','ow N','egat','ive ','Outp','ut) ',
     2 '  Ma','ke =',' 0 (','No N','egat','ive ','Outp','ut) ','    '/
      DATA MESS6/' Off','  On'/
C-----INITIALIZE TO ITERATE
      NOERR = 0
C-----READ AND PRINT INTERPRETATION OF FIRST LINE OF INPUT PARAMETERS.
      IF(ISTAT1.EQ.1) GO TO 20
C-----2017/5/6 - Changed all floating point to character.
      READ(INP,10,END=20,ERR=20)
     1 MODGET,(FIELD(j,1),j=1,11),IMBACK,IMEDIT,MAKEPLUS,MONITR
   10 FORMAT(I11,11A1,2I11,I11,I11)
      CALL IN9(XCLOW,FIELD(1,1))
c-----2021/3/22 - Now fixed = not an input option
      XCLOW = XCINP
C-----2017/5/6 - Changed all floating point to character.
      GO TO 30
C-----DEFINE DEFAULT VALUES
   20 ISTAT1   = 1
      MODGET   = 0
c-----2020/12/26 - Changed from 0
      XCLOW    = XCINP
      IMBACK   = 1
      IMEDIT   = 1
      MAKEPLUS = 1
      MONITR   = 1
   30 IF(MODGET.NE.0) MODGET=1
      IF(IMBACK.NE.0) IMBACK=1
      IF(MAKEPLUS.NE.0) MAKEPLUS=1
C-----INITIALIZE TO ADD MISSING OR DUPLICATE (L,S,J) SEQUENCES.
      MYPOT=1
      IF(IMEDIT.LT.10) GO TO 40
C-----AS REQUESTED, TURN ON OVERRIDE NOT TO ADD MISSING OR DUPLICATE
C-----(L,S,J) SEQUENCES.
      MYPOT=0
      IMEDIT=IMEDIT-10*(IMEDIT/10)
   40 IF(IMEDIT.LT.0.OR.IMEDIT.GT.2) IMEDIT=2
C-----IF IN EDIT MODE SET OTAPE = 0 TO INDICATE NO ENDF/B OUTPUT.
      IF(IMEDIT.EQ.2) OTAPE=0
      IF(MONITR.LE.0) MONITR=0
      IF(MONITR.GT.0) MONITR=1
      MIN=2
      IF(XCLOW.GT.0.0d0) GO TO 50
      XCLOW=XCINP
      MIN=1
   50 CALL OUT9(XCLOW,FIELD(1,1))
      WRITE(OUTP,380) MESS1(MODGET+1),(FIELD(M,1),M=1,11),
     1                      (MESS2(J,IMBACK+1),J=1,9)
      WRITE(OUTP,390) (MESS3(J,IMEDIT+1),J=1,10),
     1                (MESS4(J,MAKEPLUS+1),J=1,9),
     2 MESS6(MONITR+1)
      WRITE(*   ,380) MESS1(MODGET+1),(FIELD(M,1),M=1,11),
     1                      (MESS2(J,IMBACK+1),J=1,9)
      WRITE(*   ,390) (MESS3(J,IMEDIT+1),J=1,10),
     1                (MESS4(J,MAKEPLUS+1),J=1,9),
     2 MESS6(MONITR+1)
C-----PRINT WARNING MESSAGE IF OVERRIDE NOT TO ADD MISSING OR DUPLICATE
C-----(L,S,J) SEQUENCES HAS BEEN TURNED ON.
      IF(MYPOT.EQ.0) WRITE(OUTP,500)
c-----------------------------------------------------------------------
C
C     READ FILENAMES - IF BLANK USE STANDARD FILENAMES
C
c-----------------------------------------------------------------------
C-----INPUT DATA.
      IF(ISTAT1.EQ.1) GO TO 70
      READ(INP,60,END=70,ERR=70) NAMEIN
   60 FORMAT(A72)
      IF(NAMEIN.EQ.' ') NAMEIN = 'ENDFB.IN'
C-----OUTPUT DATA.
      READ(INP,60,END=80,ERR=80) NAMEOUT
      IF(NAMEOUT.EQ.' ') NAMEOUT = 'ENDFB.OUT'
      GO TO 90
C-----USE DEFAULT FILENAMES
   70 NAMEIN  = 'ENDFB.IN'
   80 NAMEOUT = 'ENDFB.OUT'
      ISTAT1 = 1
C-----PRINT FINAL FILENAMES
   90 WRITE(OUTP,100) NAMEIN,NAMEOUT
      WRITE(*   ,100) NAMEIN,NAMEOUT
  100 FORMAT(1X,78('=')/
     1 ' ENDF/B Input and Output Data Filenames'/1X,A72/
     2 1X,A72)
c-----------------------------------------------------------------------
C
C     OPEN ENDF/B DATA FILES
C
c-----------------------------------------------------------------------
      CALL FILIO2
C***** DEBUG - ACTIVATE FOR UNRESOLVED COMPETITION LISTING
C     OPEN(22,FILE='RECENT.COMPETE')
c     OPEN(22,FILE='RECENT.MISSINGLJ')
c     write(22,2200)
c2200 format(' Missing (L.J) Cross Section')
c     EMISSING = 0.0d0
C***** DEBUG - ACTIVATE FOR UNRESOLVED COMPETITION LISTING
c-----------------------------------------------------------------------
C
C     TERMINATE IF ERROR OPENING ENDF/B DATA FILE
C
c-----------------------------------------------------------------------
      IF(ISTAT2.EQ.1) THEN
      WRITE(OUTP,110) NAMEIN
      WRITE(   *,110) NAMEIN
  110 FORMAT(//' ERROR - open ENDF/B data file'/1X,A72//)
      CALL ENDERROR
      ENDIF
C-----READ SELECTION RANGES (EITHER MAT OR ZA). IF MAXIMUM IS LESS
C-----THAN MINIMUM SET IT EQUAL TO MINIMUM.
      IF(MODGET.EQ.0) WRITE(OUTP,400)
      IF(MODGET.EQ.1) WRITE(OUTP,410)
      IF(MODGET.EQ.0) WRITE(*   ,400)
      IF(MODGET.EQ.1) WRITE(*   ,410)
      IF(ISTAT1.EQ.1) GO TO 130
C-----2017/5/6 - Changed all floating point to character.
      READ(INP,120) MATMIN(1),MATMAX(1),((FIELD(KK,KKK),KK=1,11),
     1 KKK=1,2)
  120 FORMAT(2I11,22A1)
C-----2017/5/6 - Changed all floating point to character.
      GO TO 140
C-----DEFINE DEFAULT VALUES
  130 ISTAT1    = 1
      MATMIN(1) = 0
      MATMAX(1) = 0
      GO TO 160
C-----------------------------------------------------------------------
C
C     2016/3/10 - Partial Range Processing no longer allowed.
C
C-----------------------------------------------------------------------
C-----CONVERT ENERGY RANGE TO PROCESS FROM CHARACTERS TO FLOATING POINT.
  140 CALL IN9(EPART1,FIELD(1,1))
      CALL IN9(EPART2,FIELD(1,2))
C-----DEFINE WHETHER ALL OR A PORTION OF THE RESONANCE REGION WILL BE
C-----PROCESSED.
      NPART=0
      IF(EPART1.GT.0.0d0) NPART=1
      IF(EPART2.GT.0.0d0) NPART=2
      IF(NPART.LE.0) GO TO 160
      CALL OUT9(EPART1,FIELD(1,1))
      CALL OUT9(EPART2,FIELD(1,2))
      WRITE(OUTP,150) ((FIELD(M,KK),M=1,11),KK=1,2)
      WRITE(*   ,150) ((FIELD(M,KK),M=1,11),KK=1,2)
  150 FORMAT(///' ERROR - You Have Tried to Turned on the Override to',
     1                                              ' ONLY Process a'/
     2          '         Portion of the Resonance Region ',
     3                                     11A1,' to ',11A1,' eV.'/
     2          '         This option is no longer allowed.'/
     3          '         Execution Terminated.'///)
      CALL ENDERROR
C-----------------------------------------------------------------------
C
C     Requested MAT/ZA Ranges.
C
C-----------------------------------------------------------------------
C-----IF NO MAT/ZA RANGE USE STANDARD OPTION = ALL.
  160 IF(MATMIN(1).GT.0.OR.MATMAX(1).GT.0) GO TO 170
      MATMAX(1)=9999
      MODGET=0
      NMATZA=2
C-----Default = ALL
      WRITE(OUTP,430) MATMIN(1),MATMAX(1)
      WRITE(*   ,430) MATMIN(1),MATMAX(1)
      go to 180
C-----Define Range
  170 IF(MATMAX(1).LT.MATMIN(1)) MATMAX(1)=MATMIN(1)
      WRITE(OUTP,420) MATMIN(1),MATMAX(1)
      WRITE(*   ,420) MATMIN(1),MATMAX(1)
C-----PROCESS REMAINING DATA REQUESTS.
  180 DO 190 NMATZA=2,101
      IF(ISTAT1.EQ.1) GO TO 210
      READ(INP,120,END=200,ERR=200) MATMIN(NMATZA),MATMAX(NMATZA)
c-----Check input and define defaults.
      IF(MATMIN(NMATZA).LE.0.AND.MATMAX(NMATZA).LE.0) GO TO 210
      IF(MATMAX(NMATZA).LT.MATMIN(NMATZA)) MATMAX(NMATZA)=MATMIN(NMATZA)
      WRITE(OUTP,420) MATMIN(NMATZA),MATMAX(NMATZA)
      WRITE(*   ,420) MATMIN(NMATZA),MATMAX(NMATZA)
  190 CONTINUE
      GO TO 330
C-----------------------------------------------------------------------
C
C     READ AND PRINT FILE 2 ERROR LAW. ERROR MUST BE POSITIVE AND
C     ENERGIES MUST BE IN ASCENDING ORDER. IF ERROR IS ZERO SET TO
C     STANDARD OPTION. ERROR LAW IS TERMINATED BY BLANK LINE. IF
C     FIRST LINE IS BLANK TERMINATE ERROR LAW AND SET ERROR TO
C     ENERGY INDEPENDENT STANDARD OPTION (CURRENTLY 0.1 PER-CENT).
C
C-----------------------------------------------------------------------
  200 ISTAT1 = 1
  210 NMATZA=NMATZA-1
      IF(ISTAT1.EQ.1) GO TO 230
C-----2017/5/6 - Changed all floating point to character.
      READ(INP,220,END=230,ERR=230) ((FIELD(j,k),j=1,11),k=1,2)
  220 FORMAT(22A1)
      CALL IN9(ENER3(1),FIELD(1,1))
      CALL IN9(ER3  (1),FIELD(1,2))
C-----2017/5/6 - Changed all floating point to character.
      GO TO 240
  230 ISTAT1   = 1
      ENER3(1) = 0.0d0
      ER3(1)   = 0.0d0
  240 IF(ENER3(1).LT.0.0d0) ENER3(1)=0.0d0
      CALL OUT9(ENER3(1),FIELD(1,1))
      IF(ER3(1).GT.0.0d0) GO TO 250
      ER3(1)=ERRMIN
      PERCNT=100.0d0*ERRMIN
      CALL OUT9(ER3(1),FIELD(1,2))
      WRITE(OUTP,450) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
      WRITE(*   ,450) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
      IF(ENER3(1).GT.0.0d0) GO TO 260
      MAXER3=2
      GO TO 300
  250 PERCNT=100.0d0*ER3(1)
      CALL OUT9(ER3(1),FIELD(1,2))
      WRITE(OUTP,440) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
      WRITE(*   ,440) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
  260 DO 290 MAXER3=2,1000
      IF(ISTAT1.EQ.1) GO TO 300
C-----2017/5/6 - Changed all floating point to character.
      READ(INP,220,END=230,ERR=230) ((FIELD(j,k),j=1,11),k=1,2)
      CALL IN9(ENER3(MAXER3),FIELD(1,1))
      CALL IN9(ER3  (MAXER3),FIELD(1,2))
C-----2017/5/6 - Changed all floating point to character.
      IF(ENER3(MAXER3).LE.0.0d0.AND.ER3(MAXER3).LE.0.0d0) GO TO 300
      CALL OUT9(ENER3(MAXER3),FIELD(1,1))
      IF(ER3(MAXER3).LE.0.0d0) GO TO 270
      PERCNT=100.0d0*ER3(MAXER3)
      CALL OUT9(ER3(MAXER3),FIELD(1,2))
      WRITE(OUTP,460) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
      WRITE(*   ,460) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
      GO TO 280
  270 ER3(MAXER3)=ERRMIN
      PERCNT=100.0d0*ERRMIN
      CALL OUT9(ER3(MAXER3),FIELD(1,2))
      WRITE(OUTP,470) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
      WRITE(*   ,470) ((FIELD(M,I),M=1,11),I=1,2),PERCNT
  280 IF(ENER3(MAXER3).LT.ENER3(MAXER3-1)) GO TO 370
  290 CONTINUE
      GO TO 360
C-----INITIALIZE FILE 2 RECONSTRUCTION LAW INDICES AND ALLOWABLE ERROR.
  300 MAXER3=MAXER3-1
      ERRXC3=ER3(MAXER3)
C-----02/14/04 - CHECK FOR NO ITERATION = LARGE ALLOWABLE ERROR
      DO I=1,MAXER3
      IF(ER3(I).LT.1.0d0) go to 320
      ENDDO
      NOERR = 1 ! SET FOR NO ITERATION
c-----2020/12/13 - WARNING - if NO iteration.
      write(OUTP,310)
      write(*   ,310)
  310 format(' WARNING...No Iteration to Convergence')
c-----------------------------------------------------------------------
c
c     2020/12/17 - Reaction Dependent Convergence Criteria
c
c-----------------------------------------------------------------------
  320 CALL SETERROR(2)   ! 2 = RECENT
      RETURN
c-----------------------------------------------------------------------
C
C     ERROR MESSAGE SECTION. PRINT ERROR MESSAGE AND TERMINATE.
C
c-----------------------------------------------------------------------
C-----OVER 100 MAT OR ZA RANGES.
  330 WRITE(OUTP,340)
      WRITE(*   ,340)
  340 FORMAT(///' ERROR - Over 100 Ranges----Execution Terminated'///)
  350 CALL ENDERROR
C-----OVER 1000 ENTRIES IN ERROR LAW.
  360 WRITE(OUTP,480)
      WRITE(*   ,480)
      GO TO 350
C-----ERROR LAW ENERGIES NOT IN ASCENDING ORDER.
  370 WRITE(OUTP,490)
      WRITE(*   ,490)
      GO TO 350
  380 FORMAT(
     1 ' Retrieval Criteria-----------',7X,A4/
     2 ' File 2 Mimimum Cross Section-',11A1,
     3 ' (No Longer an Input Option)'/
     4 ' Reactions with No Background-',9A4)
  390 FORMAT(
     1 ' Calculate/Edit Mode----------',1X,10A4/
     2 ' Negative Cross Sections------',1X,9A4/
     3 ' Monitor Mode-----------------',7X,A4)
  400 FORMAT(1X,78('=')/' Requested MAT Ranges'/1X,78('=')/
     1 4X,'Mimimum',4X,'Maximum'/1X,78('='))
  410 FORMAT(1X,78('=')/' Requested ZA Ranges'/1X,78('=')/
     1 4X,'Mimimum',4X,'Maximum'/1X,78('='))
  420 FORMAT(2I11)
  430 FORMAT(2I11,' (Default Option)')
  440 FORMAT(1X,78('=')/' Allowable Uncertainty'/1X,78('=')/
     1 6X,'Energy',' Uncertainty',3X,'per-cent'/1X,78('=')/
     2 1X,11A1,1X,11A1,F11.3)
  450 FORMAT(1X,78('=')/' File 2 Reconstruction Error'/1X,78('=')/
     1 6X,'Energy',6X,'Error',3X,'per-cent'/1X,78('=')/
     3 1X,11A1,1X,11A1,F11.3,' (Default Option)')
  460 FORMAT(1X,11A1,1X,11A1,F11.3)
  470 FORMAT(1X,11A1,1X,11A1,F11.3,' (Default Option)')
  480 FORMAT(' Over 1000 Ranges--Execution Terminated')
  490 FORMAT(' Energies MUST be in Ascending Order----',
     1 'Execution Terminated')
  500 FORMAT(1X,78('=')/
     1 ' WARNING - You Have Turned on the Override NOT to Add Missing'/
     2 ' or Duplicate (L,S,J) Sequences to Account for their Potential'/
     3 ' Contribution. This is NOT Recommended, Based on the Decision'/
     4 ' of the National Nuclear Data Center, Brookhaven National Lab,'/
     5 ' Private Communication, Charles Dunford, (April 1991)'/
     6  1X,78('='))
      END
      SUBROUTINE NXTMAT
C=======================================================================
C
C     FIND NEXT REQUESTED MATERIAL BASED EITHER ON ZA OR MAT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 ZABCD
      CHARACTER*4 FMT5,FMTHOL
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/MATZA/MODGET,NMATZA,MATMIN(101),MATMAX(101)
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/WHATZA/IZANOW,MATNOW,TEMP3,IVERSE,INT45
      COMMON/HOLFMT/FMTHOL,ZABCD(12)
      COMMON/LISOCOM/LISO
      DIMENSION IZAMIN(101),IZAMAX(101)
      EQUIVALENCE (MATMIN(1),IZAMIN(1)),(MATMAX(1),IZAMAX(1))
      DATA FMT5/' V'/
C-----READ NEXT LINE AND CHECK FOR END OF ENDF/B TAPE.
   10 CALL CONTI
      IF(MTH.gt.0) go to 20
      IF(MATH.lt.0) go to 60
      go to 10
C-----DEFINE FIXED POINT ZA.
   20 IZANOW=C1H
C-----COMPARE MAT OR ZA TO SELECTION CRITERIA.
      IMHIGH=0
      DO 50 IMATZA=1,NMATZA
      IF(MODGET.NE.0) GO TO 30
      IF(MATH.lt.MATMIN(IMATZA)) go to 40
      IF(MATH.eq.MATMIN(IMATZA)) go to 70
      IF(MATH.le.MATMAX(IMATZA)) go to 70
      go to 50
   30 IF(IZANOW.lt.IZAMIN(IMATZA)) go to 40
      IF(IZANOW.eq.IZAMIN(IMATZA)) go to 70
      IF(IZANOW.le.IZAMAX(IMATZA)) go to 70
   40 IMHIGH=1
   50 CONTINUE
C-----THIS MATERIAL HAS NOT BEEN REQUESTED. IF BEYOND RANGE OF ALL
C-----REQUESTS RUN IF COMPLETED. IF NOT SKIP TO NEXT MATERIAL.
      IF(IMHIGH.LE.0) GO TO 60
C-----SKIP TO MATERIAL END (MEND) LINE.
      CALL SKIPM
      GO TO 10
C-----END OF RUN. RETURN NEGATIVE MATH AS INDICATOR. OUTPUT TAPE END
C-----(TEND) RECORD.
   60 MATH=-1
      MFH=0
      MTH=0
      CALL OUTT
      WRITE(OUTP,80)
      WRITE(*   ,80)
      RETURN
c-----------------------------------------------------------------------
c
C     THIS MATERIAL REQUESTED. INITIALIZE OUTPUT SEQUENCE NUMBER,
C     ENDF/B FORMAT VERSION NUMBER, ASSUME ENDF/B-6 FORMAT AND
C     INITIALIZE FILE 3 TEMPERATURE TO ZERO (IF MF=1, MT-451 IS
C     PRESENT THE ENDF/B VERSION NUMBER WILL BE RE-DEFINED BASED
C     ON THE FORMAT OF MF=1, MT=451).
c
c-----------------------------------------------------------------------
   70 NOSEQ=1
      MATNOW=MATH
      FMTHOL=FMT5
      IVERSE=6
      TEMP3=0.0d0
      INT45=2
      LISO =0
C-----DEFINE BCD EQUIVALENT OF ZA - in case no FILE1
c-----2020/3/21 - Added Target Isomer State
      CALL ZAHOLM(IZANOW,LISO,ZABCD)
      RETURN
   80 FORMAT(1X,78('*')/' End of ENDF/B Input Data'/1X,78('*'))
      END
      SUBROUTINE FILE1(IMDONE)
C=======================================================================
C
C     ADD COMMENTS AT THE END OF FILE 1, SECTION 451 TO INDICATE
C     THAT THIS MATERIAL HAS BEEN PROCESSED BY PROGRAM RECENT AND
C     TO SPECIFY THE MAXIMUM ALLOWABLE ERROR.
C
C     DEFINE FORMAT TO BE ENDF/B-4, 5 or 6.
C
C     THE ENDF/B FORMAT CAN BE DETERMINED FROM THE SECOND LINE.
C     ENDF/B-4  = N1 > 0, N2 = 0,LINE COUNT (POSITIVE)
C     ENDF/B-5  = N1 = N2 =0
C     ENDF/B-6  =      N2 = VERSION NUMBER (6 OR MORE)
C
C     This first line has already been read
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 ZABCD,FIELD,PROGDOC1
      CHARACTER*4 FMTHOL,FMTTAB
      CHARACTER*66 PROGDOC
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/OKERR3/ENER3(1000),ER3(1000),MAXER3,NOERR
      COMMON/ERRORCOM/ERRXC3,ERMINMAX,ERMT2,ERMT102,ERSOFT,XCLOW
      COMMON/WHATZA/IZANOW,MATNOW,TEMP3,IVERSE,INT45
      COMMON/HOLFMT/FMTHOL,ZABCD(12)
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
      COMMON/LISOCOM/LISO
      DIMENSION FMTTAB(3),PROGDOC(9),PROGDOC1(66,9)
      EQUIVALENCE (PROGDOC(1),PROGDOC1(1,1))
      DATA FMTTAB/'4 ','5 ','6 '/
c-----------------------------------------------------------------------
c
C     DOCUMENTATION TO ADD TO ENDF/B OUTPUT - EACH LINE IS 66
C     CHARACTERS LONG - FIELDS 12345678901 ARE FILLED IN WITH
C     11 CHARACTERS DURING EXECUTION.
c
c-----------------------------------------------------------------------
C               1         2         3         4         5         6
C       12345678901234567890123456789012345678901234567890123456789012
C       3456
      DATA PROGDOC/
     1 ' ***************** Program RECENT (VERSION 2021-1) ***********',
     2 ' Only Process12345678901 to12345678901 eV                     ',
     3 ' for All Data Greater than12345678901 barns in Absolute Value ',
     4 ' Data Linearized to within an Accuracy of12345678901 per-cent ',
     5 ' Data Linearized Using Energy Dependent Uncertainty           ',
     6 '      Energy    Accuracy                                      ',
     7 '        (eV)  (per-cent)                                      ',
     8 ' ----------- -----------                                      ',
     9 ' 12345678901 12345678901                                      '/
C-----FILL IN REMAINDER OF FIRST LINE
      PROGDOC1(63,1) = '*'
      PROGDOC1(64,1) = '*'
      PROGDOC1(65,1) = '*'
      PROGDOC1(66,1) = '*'
C-----INITIALIZE MAT ALREADY PROCESSED FLAG TO OFF.
      IMDONE=1
C-----SAVE FLAGS WHICH INDICATES WHETHER OR NOT MAT IS FISSIONABLE AND
C-----IF RESONANCE PARAMETERS ARE PRESENT.
      LRP=L1H
      LFI=L2H
c
c     Read the second line
c
C-----HEAD LINE OF SECTION HAS BEEN READ. READ SECOND LINE AND TEST
C-----FOR ENDF/B-4 FORMAT.
      CALL CARDI(C1A,C2A,L1A,L2A,N1A,N2A)
      LISOX = L2A      ! Save an potentia; target isomer number
C-----4: N1 > 0, N2 = 0
      IF(N1A.LE.0.OR.N2A.NE.0) GO TO 30
c-----------------------------------------------------------------------
C
C     ENDF/B-4 FORMAT. UPDATE NUMBER OF COMMENT CARDS AND OUTPUT
C     TWO CARDS SET FLAGS FOR ENDF/B-4 FORMAT.
C
c-----------------------------------------------------------------------
C-----IF THERE ARE RESONANCE PARAMETERS AND THEIR CONTRIBUTION HAS NOT
C-----BEEN ADDED TO THE BACKGROUND SET LRP TO INDICATE THAT IT HAS NOW
C-----BEEN ADDED. ONLY ADD COMMENT CARDS IF MAT WILL BE PROCESSED.
      IF(LRP.NE.1) GO TO 10
      L1H=2
      N1OUT=N1A+3
      IF(MAXER3.GT.1) N1OUT=N1OUT+MAXER3+3
      GO TO 20
   10 N1OUT=N1A
   20 CALL CONTO
      CALL CARDO(C1A,C2A,L1A,L2A,N1OUT,N2A)
      N1X=N1A
      FMTHOL=FMTTAB(1)
      IVERSE=4
      INT45=5
      GO TO 90
C-----NOT ENDF/B-4. READ THIRD LINE AND TEST FOR ENDF/B-5 FORMAT.
c
c     Read the third line
c
   30 CALL CARDI(C1B,C2B,L1B,L2B,N1B,N2B)
      IF(N2A.GT.0) GO TO 60
c-----------------------------------------------------------------------
C
C     ENDF/B-5 FORMAT. UPDATE NUMBER OF COMMENT CARDS AND OUTPUT
C     THREE CARDS AND SET FLAGS FOR ENDF/B-5 FORMAT.
C
c-----------------------------------------------------------------------
C-----IF THERE ARE RESONANCE PARAMETERS AND THEIR CONTRIBUTION HAS NOT
C-----BEEN ADDED TO THE BACKGROUND SET LRP TO INDICATE THAT IT HAS NOW
C-----BEEN ADDED. ONLY ADD COMMENT CARDS IF MAT WILL BE PROCESSED.
      IF(LRP.NE.1) GO TO 40
      L1H=2
      N1OUT=N1B+3
      IF(MAXER3.GT.1) N1OUT=N1OUT+MAXER3+3
      GO TO 50
   40 N1OUT=N1B
   50 CALL CONTO
      CALL CARDO(C1A,C2A,L1A,L2A,N1A,N2A)
      CALL CARDO(C1B,C2B,L1B,L2B,N1OUT,N2B)
      N1X=N1B
      FMTHOL=FMTTAB(2)
      IVERSE=5
      INT45=2
      GO TO 90
c-----------------------------------------------------------------------
C
C     ENDF/B-6 FORMAT. UPDATE NUMBER OF COMMENT CARDS AND OUTPUT
C     THREE CARDS AND SET FLAGS FOR ENDF/B-6 FORMAT.
C
c-----------------------------------------------------------------------
c
c     Read the fourth line
c
C-----READ FOURTH LINE.
   60 CALL CARDI(C1C,C2C,L1C,L2C,N1C,N2C)
C-----IF THERE ARE RESONANCE PARAMETERS AND THEIR CONTRIBUTION HAS NOT
C-----BEEN ADDED TO THE BACKGROUND SET LRP TO INDICATE THAT IT HAS NOW
C-----BEEN ADDED. ONLY ADD COMMENT CARDS IF MAT WILL BE PROCESSED.
      IF(LRP.NE.1) GO TO 70
      L1H=2
      N1OUT=N1C+3
      IF(MAXER3.GT.1) N1OUT=N1OUT+MAXER3+3
      GO TO 80
   70 N1OUT=N1C
   80 CALL CONTO
      CALL CARDO(C1A,C2A,L1A,L2A,N1A,N2A)
      CALL CARDO(C1B,C2B,L1B,L2B,N1B,N2B)
      N1X=N1C
      FMTHOL=FMTTAB(3)
      IVERSE=6
      LISO  = LISOX
      INT45=2
      TEMP3=C1C
      INPART=N1B/10
C-----SET DERIVED MATERIAL FLAG.
      L1C=1
      CALL CARDO(C1C,C2C,L1C,L2C,N1OUT,N2C)
C-----IF NO RESONANCE PARAMETERS OR THERE CONTRIBUTION HAS ALREADY BEEN
C-----ADDED TO THE BACKGROUND COPY MAT. OTHERWISE INSERT COMMENTS.
   90 IF(LRP.NE.1) GO TO 130
c-----------------------------------------------------------------------
C
C     SKIP OUTPUT IF IN EDIT (I.E. NO OUTPUT) MODE.
C
c-----------------------------------------------------------------------
      IF(OTAPE.LE.0) GO TO 140
C-----COPY TO END OF HOLLERITH.
      DO 100 N=1,N1X
      CALL COPY1
  100 CONTINUE
c-----------------------------------------------------------------------
C
C     ADD COMMENTS TO DOCUMENT WHAT WAS DONE TO DATA
C
c-----------------------------------------------------------------------
C-----OUTPUT PROGRAM VERSION I.D.
      CALL HOLLYO(PROGDOC1(1,1))
c-----------------------------------------------------------------------
C
C     DESCRIBE RESONANCE RECONSTRUCTION CRITERIA.
C
c-----------------------------------------------------------------------
C-----OUTPUT MINIMUM CROSS SECTION
      CALL OUT9(XCLOW,PROGDOC1(27,3))
      CALL HOLLYO(PROGDOC1(1,3))
      IF(MAXER3.GT.1) GO TO 110
C-----OUTPUT ENERGY INDEPENDENT ERROR USED FOR RECONSTRCUTION.
      PERCNT=100.0d0*ER3(1)
      CALL OUT9(PERCNT,PROGDOC1(42,4))
      CALL HOLLYO(PROGDOC1(1,4))
      GO TO 140
C-----OUTPUT 4 LINE TITLE
  110 CALL HOLLYO(PROGDOC1(1,5))
      CALL HOLLYO(PROGDOC1(1,6))
      CALL HOLLYO(PROGDOC1(1,7))
      CALL HOLLYO(PROGDOC1(1,8))
      DO 120 I=1,MAXER3
      PERCNT=100.0d0*ER3(I)
      CALL OUT9(ENER3(I),PROGDOC1( 2,9))
      CALL OUT9(PERCNT  ,PROGDOC1(14,9))
      CALL HOLLYO(PROGDOC1(1,9))
  120 CONTINUE
      GO TO 140
c-----------------------------------------------------------------------
C
C     END OF HOLLERITH OUTPUT TO FILE1
C
c-----------------------------------------------------------------------
C-----INDICATE THAT MAT NEED NOT BE PROCESSED (EITHER NO PARAMETERS OR
C-----THEIR CONTRIBUTION HAS ALREADY BEEN ADDED TO THE BACKGROUND).
  130 IMDONE=0
C-----DEFINE BCD EQUIVALENT OF ZA.
c-----2020/3/21 - Added Target Isomer State
  140 CALL ZAHOLM(IZANOW,LISO,ZABCD)
C-----IDENTIFY MAT BEING PROCESSED.
      WRITE(OUTP,150) ZABCD,MATNOW
      WRITE(*   ,150) ZABCD,MATNOW
  150 FORMAT(1X,78('*')/' Processing ',12A1,' MAT=',I5/
     1 1X,78('*'))
C-----IDENTIFY ENDF/B FORMAT.
      WRITE(OUTP,180) FMTHOL
      WRITE(*   ,180) FMTHOL
C-----DEFINE WHETHER OR NOT MATERIAL IS FISSILE.
      IF(LFI.EQ.0) WRITE(OUTP,190)
      IF(LFI.GT.0) WRITE(OUTP,200)
      IF(LFI.EQ.0) WRITE(*   ,190)
      IF(LFI.GT.0) WRITE(*   ,200)
C-----DEFINE WHETHER OR NOT THERE ARE RESONANCE PARAMETERS AND WHETHER
C-----THERE CONTRIBUTION HAS ALREADY BEEN ADDED TO THE BACKGROUND CROSS
C-----SECTION.
      IF(LRP.EQ.0) WRITE(OUTP,210)
      IF(LRP.EQ.1) WRITE(OUTP,220)
      IF(LRP.EQ.2.AND.IMEDIT.NE.2) WRITE(OUTP,230)
      IF(LRP.EQ.2.AND.IMEDIT.EQ.2) WRITE(OUTP,240)
      IF(LRP.EQ.0) WRITE(*   ,210)
      IF(LRP.EQ.1) WRITE(*   ,220)
      IF(LRP.EQ.2.AND.IMEDIT.NE.2) WRITE(*   ,230)
      IF(LRP.EQ.2.AND.IMEDIT.EQ.2) WRITE(*   ,240)
C-----FOR ENDF/B-6 FORMATTED DATA DEFINE PROJECTILE AND BACKGROUND
C-----TEMPERATURE.
      IF(IVERSE.NE.6) GO TO 170
      IF(INPART.EQ.1) WRITE(OUTP,250) INPART
      IF(INPART.EQ.1) WRITE(*   ,250) INPART
      IF(INPART.NE.1) WRITE(OUTP,260) INPART
      IF(INPART.NE.1) WRITE(*   ,260) INPART
      CALL OUT9(TEMP3,FIELD(1,1))
      WRITE(OUTP,270) (FIELD(M,1),M=1,11)
      WRITE(*   ,270) (FIELD(M,1),M=1,11)
c-----------------------------------------------------------------------
c
c     ERROR if Background Temperature is not = 0
c
c-----------------------------------------------------------------------
      if(TEMP3.ne.0.0d0) then
      write(outp,160)
      write(*   ,160)
  160 format(1x,9('ERROR==='),'ERROR=='/
     1 ' This code calculates 0 Kelvin cross section and Outputs them',
     2 ' to MF=3.'/
     2 ' To be COMPATIBLE the Background Temperature included in',
     3 ' MF/MT=1/451'/
     3 ' MUST be 0 Kelvin. PLEASE correct the Temperature in',
     4 ' MF/MT=1/451 and'/' re-run this code.'/1x,79('=')/
     5 ' EXECUTION TERMINATED.')
      CALL ENDERROR
      endif
  170 RETURN
  180 FORMAT(' Based on the Format and Contents of MF/MT=1/451'/
     1 ' (1) ENDF/B-',A2,' Format.')
  190 FORMAT(' (2) Material is NOT Fissile (LFI=0).')
  200 FORMAT(' (2) Material is Fissile (LFI=1).')
  210 FORMAT(' (3) No Resonance Parameters Given (LRP=0).',
     1 ' MAT Copied.')
  220 FORMAT(' (3) Resonance Parameters are Given (LRP=1).')
  230 FORMAT(' (3) Resonance Data Already Added to Background Cross'/
     1       '     Section (LRP=2). MAT Copied.')
  240 FORMAT(' (3) Resonance Data Already Added to Background Cross'/
     1       '     Section (LRP=2).')
  250 FORMAT(' (4) Projectile ZA =',I7,' (Neutron).')
  260 FORMAT(' (4) Projectile ZA =',I7,' (Expect Neutron = 1).')
  270 FORMAT(' (5) Background Temperature ',11A1,' Kelvin.')
      END
      SUBROUTINE FILE2
C=======================================================================
C
C     READ ALL FILE2 DATA AND PROCESS INTO POINTWISE FORM.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 SECONDS
      CHARACTER*1 FIELD
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR2,ISCR23
      COMMON/SUBS/ESUB(3000),ENODP,ENODM,WIDP,WIDM,ISUB,NSUB
      COMMON/PAGER/NPAGE,NPAGP1,NPAGM1
      COMMON/POINTN/NPOINT,KPOINT
      COMMON/OKERR3/ENER3(1000),ER3(1000),MAXER3,NOERR
      COMMON/ERRORCOM/ERRXC3,ERMINMAX,ERMT2,ERMT102,ERSOFT,XCLOW
      COMMON/MINNIE/EMIN,EMAX,DEMIN
      COMMON/EDGES/ERANGE(50),NODLOW(50),NODHI(50),NRANGE,IRANGE
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FIELDC/FIELD(11,12)
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
      COMMON/NEG1COM/NEG1    ! 2014/11/02 - add to FILE2 and FILE3
      COMMON/THRESCOM/NTHRES(11)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DATA IUSE2/0/
c-----reset LRF=7 used and IMFISSY flags for each evaluation
      call same0
C-----READ ALL FILE 2 PARAMETERS.
      CALL READ2
c-----DFEFINE REACTIONS TO CALCULATE
      call Answer1
c-----2021/3/23 - Initialize Threshold test
      do ir=1,NREACT
      if(MTREACT(ir).gt.102) then
      NTHRES(ir) = 0                ! Search for threshold (MT>102)
      else
      NTHRES(ir) = 1                ! NO Search (MT=1,2, 18, 102)
      endif
      enddo
c-----------------------------------------------------------------------
c
C     INITIALIZE POINT COUNTS
C     NPOINT = NUMBER OF POINTS ON SCRATCH. POINT TOTAL AT END.
C     KPOINT = NUMBER OF POINTS IN CORE (0 TO MAXRES).
C     KPTP1  = INDEX TO POSITION INTO WHICH NEXT POINT WILL BE STORED.
C     NBASE  = NUMBER OF POINTS GENERATED UP TO END OF LAST ENERGY.
C     RANGE).
c
c-----------------------------------------------------------------------
      NPOINT=0
      KPOINT=0
      KPTP1=1
      NBASE=0
      NEG1 =0    ! added 2014/11/02 - to initialize NEG1
C-----IF THERE ARE NO SECTIONS WHOSE RESONANCE CONTRIBUTION MUST BE
C-----ADDED TO BACKGROUND CROSS SECTION THERE IS NOTHING ELSE TO DO.
      IF(NSECT.GT.0) GO TO 10
      WRITE(OUTP,480)
      WRITE(*   ,480)
      GO TO 410
C-----IF IN EDIT MODE RETURN AFTER READING ALL FILE 2 DATA.
   10 IF(IMEDIT.EQ.2) GO TO 410
c-----------------------------------------------------------------------
C
C     CALCULATE RESONANCE CONTRIBUTION TO CROSS SECTION.
C
c-----------------------------------------------------------------------
      WRITE(OUTP,470)
      WRITE(*   ,470)
C-----INITIALIZE SAVED POINT COUNT.
      NSAVE=0
c-----------------------------------------------------------------------
C
C     SET UP LOOP OVER ENERGY RANGES (E.G. RESOLVED AND UNRESOLVED)
C
c-----------------------------------------------------------------------
      DO 400 IRANGE=2,NRANGE
      IRM1=IRANGE-1
C-----INITIALIZE FLAG TO DEFINE TYPE OF RANGE (RESOLVED, UNRESOLVED
C-----OR BOTH).
      IR=0
      IU=0
C-----DEFINE WHICH SECTIONS CONTRIBUTE TO THIS ENERGY RANGE.
      DO 30 I=1,NSECT
      IF(EL(I).LT.ERANGE(IRANGE).AND.EH(I).GT.ERANGE(IRM1)) GO TO 20
C-----TURN OFF SECTION.
      MODE(I)=-IABS(MODE(I))
      GO TO 30
C-----TURN ON SECTION.
   20 MODE(I)=IABS(MODE(I))
C-----DETERMINE TYPE OF RESONANCE REGION (I.E. RESOLVED, UNRESOLVED,
C-----BOTH OR NONE).
      IF(MODE(I).LT.11) IR=1
      IF(MODE(I).GE.11) IU=1
   30 CONTINUE
C-----IDENTIFY TYPE OF ENERGY REGION.
      NREG=1+IR+2*IU
c-----------------------------------------------------------------------
C
C     SET UP LOOP OVER ENERGY NODES IN CURRENT ENERGY RANGE.
C
c-----------------------------------------------------------------------
C-----DEFINE WHICH NODES LIE IN THIS ENERGY RANGE AND SET UP LOOP OVER
C-----NODES.
      NODE1=NODLOW(IRANGE)+1
      NODE2=NODHI(IRANGE)
      DO 260 KNODE=NODE1,NODE2
C-----DEFINE INTERVAL END POINTS.
      ENODM=ENODE(KNODE-1)
      WIDM=WIDNOD(KNODE-1)
      ENODP=ENODE(KNODE)
      WIDP=WIDNOD(KNODE)
C-----DEFINE SUB-INTERVALS.
      CALL SUBINT
c-----------------------------------------------------------------------
C
C     SPECIAL TREATMENT FOR FIRST ENERGY POINT. IF FIRST RESONANCE
C     REGION STARTS ABOVE 1.0E-5 EV START TABLE AT LOWER LIMIT OF
C     RESONANCE REGION WITH ONE POINT WITH ZERO CROSS SECTION FOLLOWED
C     BY A POINT WITH THE SAME ENERGY AND THE CROSS SECTION CALCULATED
C     AT THE LOWER LIMIT OF THE RESONANCE REGION. IF FIRST RESONANCE
C     STARTS AT 1.0E-5 START TABLE WITH CROSS SECTION CALCULATED AT
C     LOWER ENERGY LIMIT OF RESONANCE REGION.
C
c-----------------------------------------------------------------------
      IF(KPOINT.LE.0) GO TO 40
C-----CALCULATE CROSS SECTION AT FIRST POINT OF EACH ENERGY REGION.
      IF(KNODE.le.NODE1) go to 60
      go to 70
C-----INITIALIZE CROSS SECTION TABLE TO ONE POINT WITH ZERO CROSS
C-----SECTION AT THE BEGINNING OF THE TABLE UNLESS THE TABLE STARTS
C-----AT 1.0-5 EV OR LESS (IN WHICH CASE TABLE WILL START WITH NON-ZERO
C-----CROSS SECTIONS).
   40 IF(ERANGE(1).LE.1.001d0*EMIN) GO TO 60
      KPOINT=1
      KPTP1=2
      ETAB2(1)=ERANGE(1)
      DO 50 IR=1,NREACT
      SIG2(IR,1)=0.0d0
   50 CONTINUE
C-----INITIALIZE INDEX TO FIRST SUB-INTERVAL AND INDICATE THAT THE
C-----THE SUB-INTERVAL HAS NOT YET BEEN SUB-DIVIDED.
   60 ISUB=1
C-----CALCULATE CROSS SECTION AT NEXT ENERGY (EITHER FIRST OR SECOND
C-----ENERGY).
      ETAB2(KPTP1)=ESUB(1)
      CALL SIGMA(ETAB2(KPTP1),SIG2(1,KPTP1))
C-----SAVE STARTING POINT (NO CONVERGENCE TESTS UNTIL SECOND POINT IS
C-----GENERATED).
      GO TO 140
c-----------------------------------------------------------------------
C
C     INTERNAL SUB-INTERVAL. SET UP LOOP OVER SUB-INTERVALS REMAINING
C     SUB-INTERVALS (NOTE, AT THIS POINT THE ENERGY INTERVAL UP TO AND
C     INCLUDING THE ENERGY OF THE FIRST SUB-INTERVAL (ISUB=1) HAS
C     ALREADY BEEN CALCULATED, EITHER BY CALCULATING THE FIRST POINT
C     OR AS THE LAST POINT OF THE PRECEDING ENERGY INTERVAL).
C
c-----------------------------------------------------------------------
C-----INITIALIZE INDEX TO SECOND SUB-INTERVAL AND INDICATE THAT THE
C-----SUB-INTERVAL HAS NOT YET BEEN SUB-DIVIDED.
   70 ISUB=2
C-----CALCULATE CROSS SECTION AT END OF SUB-INTERVAL.
   80 ETAB2(KPTP1)=ESUB(ISUB)
      CALL SIGMA(ETAB2(KPTP1),SIG2(1,KPTP1))
c***** DEBUG
c     if(ETAB2(KPTP1).ge.83664.99.and.
c    1   ETAB2(KPTP1).le.83665.01) then
c     write(3,3456) KPTp1,ETAB2(KPTP1),SIG2(1,KPTP1)
c3456 format(' k/e/s=',i8,1p2d12.5,' #1')
c     endif
c***** DEBUG
C-----SKIP IF NO ITERATION
      IF(NOERR.GT.0) GO TO 140
c-----------------------------------------------------------------------
C
C     ITERATION AND CONVERGENCE TESTS.
C
c-----------------------------------------------------------------------
C-----DEFINE ENERGY AT MIDPOINT AND TEST FOR CONVERGENCE BASED ON SHORT
C-----ENERGY INTERVAL.
   90 EA=ETAB2(KPOINT)
      EB=ETAB2(KPTP1)
      EMID=HALF*(EA+EB)
      CALL INCORE10(EMID)
c
c     Do not sub-divide beyond 10 digits
c
      IF(EMID.LE.EA.OR.EMID.GE.EB) GO TO 140
C-----DEFINE CROSS SECTION AT MIDPOINT.
      CALL SIGMA(EMID,SIGMID)
C-----IF AN ENERGY DEPENDENT ERROR LAW IS USED DEFINE ERROR AT EMID.
  100 IF(MAXER3.GT.1) CALL ERROK2(EMID)
C-----DEFINE CONTRIBUTION OF EACH ENDPOINT TO MIDPOINT.
      DE=EB-EA
      WTA=(EB-EMID)/DE
      WTB=(EMID-EA)/DE
c-----------------------------------------------------------------------
c
C     TEST EACH REACTION FOR CONVERGENCE. TO BE ACCEPTABLE ALL REACTIONS
C     MUST CONVERGE (I.E., IF EVEN ONE REACTION FAIL ONE CONVEREGENCE
C     TEST CONTINUE SUB-DIVIDING AND INTERATING TO CONVERGENCE).
c
c-----------------------------------------------------------------------
      DO 110 IR=1,NREACT
      MTNOW = MTREACT(IR)                        ! ENDF MT #
C-----DEFINE EXACT END POINT CROSS SECTIONS.
      SIGA=SIG2(IR,KPOINT)
      SIGB=SIG2(IR,KPTP1)
C-----DEFINE EXACT AND LINEARLY INTERPOLATED MID-POINT CROSS SECTIONS.
      SIGM  =SIGMID(IR)
      SIGLIN=WTA*SIGA+WTB*SIGB
c-----------------------------------------------------------------------
c
c     Do not iterate toward zero
c
c-----------------------------------------------------------------------
      if(SIGA.lt.0.0d0.or.SIGB.lt.0.0d0) go to 110
c-----------------------------------------------------------------------
c
c     2021/3/22 - Low Cross Section
c     1) No iteration below 1.0d-10
c     2)         "          1.0d-5  for MT > 102
c     3) softer iteration above     for MT > 102
c
c     Note, this section handles ALL MT > 102
c
c-----------------------------------------------------------------------
c-----ALL
      if(SIGM.le.1.0d-10) go to 110  ! no iteration below 1.0d-10
c-----MT > 102
      if(MTNOW.gt.102)    then       ! MT>102: NOT 1, 2, 18, 102
c-----WARNING: 1.0d-6 is TOO SMALL for (n,p)
      if(SIGM.le.1.0d-5 ) go to 110  ! no iteration below 1.0d-5
c-----ERSOFT = 10 X ERRXC3 for MT > 102
      IF(DABS(SIGM-SIGLIN).le.DABS(ERSOFT*SIGM)) go to 110 ! converged
      go to 120                                            ! no
      endif
c-----------------------------------------------------------------------
c
c     2009/02/22 - Stricter converge toward max/min
C
C     TEST FOR ITERATION TOWARD MIN/MAX (MINIMUM IS INDICATED IF
C     THE EXACT CROSS SECTION AT THE MID-POINT IS LESS THAN THE
C     EXACT CROSS SECTION AT BOTH ENDS OF THE INTERVAL).
c
c     ERRMINAX = EROK3/10 - not less than 10-5
c
c-----------------------------------------------------------------------
c-----Near maximum
      IF(SIGM.GT.SIGA.AND.SIGM.GT.SIGB) then
      IF(DABS(SIGM-SIGLIN).le.DABS(ERMINMAX*SIGM)) go to 110
      go to 120
      endif
c-----Near minimum
      IF(SIGM.LT.SIGA.AND.SIGM.LT.SIGB) then
      IF(DABS(SIGM-SIGLIN).le.DABS(ERMINMAX*SIGM)) go to 110
      go to 120
      endif
c-----------------------------------------------------------------------
c
c     Minimum Change Test: Require 1% change in cross section
c
c-----------------------------------------------------------------------
C-----TEST FOR CROSS SECTION CHANGE.
c-----2021/01/09 - Prevent iteration toward 0.0
      if(SIGA.gt.0.0d0.and.SIGB.gt.0.0d0) then
c-----2019/11/19 - Decreased from 1.4 to 1.1
      IF(SIGA.GT.1.1d0*SIGB.OR.SIGB.GT.1.1d0*SIGA) GO TO 120 ! NO
      endif
c-----------------------------------------------------------------------
c
c     Test for Convergence
c     1) Strict for Elastic = interference minima
c     2) strict for Others  = narrow capture
c     43 Normal for total and all others, e.g., (n.p).,,,
c
c-----------------------------------------------------------------------
c
c     Elastic = ERMT2
c
      if(MTNOW.eq.2) then
      IF(DABS(SIGM-SIGLIN).le.DABS(ERMT2*SIGM)) go to 110
      go to 120
      endif
c
c     Fission & Capture = ERMT102
c
      if(MTNOW.eq.18.or.MTNOW.eq.102) then
      IF(DABS(SIGM-SIGLIN).le.DABS(ERMT102*SIGM)) go to 110
      go to 120
      endif
c
c     Total & ALL Others
c
      IF(DABS(SIGM-SIGLIN).gt.DABS(ERRXC3*SIGM)) go to 120
c
C     END OF CONVERGENCE TEST LOOP. REACTION HAS PASSED ALL TESTS AND
C     FOR THIS REACTION MID-POINT IS NOT REQUIRED.
c
  110 CONTINUE
C-----CONVERGENCE FOR ALL REACTIONS. KEEP END POINT.
      GO TO 140
c-----------------------------------------------------------------------
C
C     NO CONVERGENCE. SAVE VALUE FROM END OF INTERVAL AND SHORTEN
C     INTERVAL.
C
c-----------------------------------------------------------------------
  120 IF(NSAVE.LT.MAXSAVE) NSAVE=NSAVE+1
      ESAVE(NSAVE)=ETAB2(KPTP1)
      ETAB2(KPTP1)=EMID
      DO 130 IR=1,NREACT
      SIGSAVE(IR,NSAVE)=SIG2(IR,KPTP1)
      SIG2(IR,KPTP1)=SIGMID(IR)
  130 CONTINUE
      GO TO 90
c-----------------------------------------------------------------------
C
C     CONVERGENCE.
C
c-----------------------------------------------------------------------
c
C     SAVE END OF INTERVAL BY INCREASING KPOINT BY ONE. IF IN CORE PAGE
C     IS FULL UNLOAD IT TO SCRATCH, MOVE LAST POINT TO TABLE BEGINNING
C     AND RE-INITIALIZE IN CORE POINT INDICES.
c
c-----------------------------------------------------------------------
  140 IF(KPOINT.LT.NPAGE) GO TO 210
      IF(NPOINT.EQ.0.AND.IUSE2.GT.0) REWIND ISCR2
c-----------------------------------------------------------------------
C
C     DEFINE START OF EACH REACTION
C
c-----------------------------------------------------------------------
      IF(NEG1.NE.0) GO TO 180
      NEG1 = 1
      DO 170 II=1,NREACT
      IF(NEGTAB(II).NE.0) GO TO 170
      DO 150 I=1,NPAGE
      IF(SIG2X(II,I).GT.0.0d0) GO TO 160
  150 CONTINUE
      NEG1 = 0
      GO TO 170
  160 NEGTAB(II) = I + NPOINT
      IF(I.GT.1) NEGTAB(II) = I - 1
  170 CONTINUE
c-----------------------------------------------------------------------
C
C     OUTPUT TO SCRATCH.
C
c-----------------------------------------------------------------------
  180 WRITE(ISCR2) ETAB2X,SIG2X
      NPOINT=NPOINT+NPAGE
C-----IF REQUESTED PRINT MESSAGE EVERYTIME A PAGE IS OUTPUT TO SCRATCH.
      IF(MONITR.EQ.0) GO TO 190
      CALL TIMER1(SECONDS)
      CALL OUT9(ETAB2(    1),FIELD(1,1))
      CALL OUT9(ETAB2(NPAGE),FIELD(1,2))
      WRITE(OUTP,490) NPOINT,((FIELD(M,I),M=1,11),I=1,2),SECONDS
      WRITE(*   ,490) NPOINT,((FIELD(M,I),M=1,11),I=1,2),SECONDS
  190 ETAB2(1)=ETAB2(NPAGP1)
      DO 200 IR=1,NREACT
      SIG2(IR,1)=SIG2(IR,NPAGP1)
  200 CONTINUE
      KPTP1=1
  210 KPOINT=KPTP1
      KPTP1=KPOINT+1
c-----------------------------------------------------------------------
C
C     IF THERE ARE NO REMAINING SAVED POINTS THE CURRENT SUB-INTERVAL
C     IS NOW FINISHED. IF THERE ARE REMAINING SAVED POINTS USE THEM
C     TO DEFINE NEXT ITERATION INTERVAL.
C
c-----------------------------------------------------------------------
      IF(NSAVE.lt.1) go to 250
      IF(NSAVE.gt.1) go to 230
C-----ONLY 1 POINT SAVED...USE IT TO DEFINE ENDPOINT AND BRANCH BACK
C-----TO DEFINE MIDPOINT...THEN ITERATE.
      ETAB2(KPTP1)=ESAVE(1)
      DO 220 IR=1,NREACT
      SIG2(IR,KPTP1)=SIGSAVE(IR,1)
  220 CONTINUE
      NSAVE=0
      GO TO 90
C-----MORE THAN 1 POINT SAVED...USE LAST 2 SAVED POINTS TO DEFINE
C-----MIDPOINT AND ENDPOINT...THEN ITERATE.
  230 NSAVM1=NSAVE-1
      ETAB2(KPTP1)=ESAVE(NSAVM1)
      EMID=ESAVE(NSAVE)
      DO 240 IR=1,NREACT
      SIG2(IR,KPTP1)=SIGSAVE(IR,NSAVM1)
      SIGMID(IR)=SIGSAVE(IR,NSAVE)
  240 CONTINUE
      NSAVE=NSAVE-2
C-----CONVERGENCE IF ENERGY INTERVAL IS TOO SMALL.
      EA=ETAB2(KPOINT)
      EB=ETAB2(KPTP1)
      IF(EMID.LE.EA.OR.EMID.GE.EB) GO TO 140
C-----USE 3 CURRENTLY KNOWN POINTS TO TEST FOR CONVERGENCE.
      GO TO 100
c-----------------------------------------------------------------------
C
C     END OF SUB-INTERVAL LOOP. CONTINUE UNTIL ALL SUB-INTERVALS HAVE
C     BEEN USED.
C
c-----------------------------------------------------------------------
  250 ISUB=ISUB+1
      IF(ISUB.LE.NSUB) GO TO 80
c-----------------------------------------------------------------------
C
C     END OF NODE LOOP.
C
c-----------------------------------------------------------------------
  260 CONTINUE
c-----------------------------------------------------------------------
C
C     END OF 1 RESONANCE REGION. IS THIS THE END OF THE ENTIRE RESONANCE
C     REGION (E.G., RESOLVED AND UNRESOLVED).
C
c-----------------------------------------------------------------------
      IF(IRANGE.LT.NRANGE) GO TO 390
c-----------------------------------------------------------------------
C
C     END OF ENTIRE RESONANCE REGION. ADD LAST POINT WITH ZERO CROSS
C     SECTION AND LEAVE DATA IN CORE OR MOVE TO SCRATCH.
C
c-----------------------------------------------------------------------
C-----IF IN CORE PAGE IS FULL UNLOAD IT TO SCRATCH.
      IF(KPOINT.LT.NPAGE) GO TO 320
      IF(NPOINT.LE.0.AND.IUSE2.GT.0) REWIND ISCR2
c-----------------------------------------------------------------------
C
C     DEFINE START OF EACH REACTION.
C
c-----------------------------------------------------------------------
      IF(NEG1.NE.0) GO TO 300
      NEG1 = 1
      DO 290 II=1,NREACT
      IF(NEGTAB(II).NE.0) GO TO 290
      DO 270 I=1,NPAGE
      IF(SIG2X(II,I).GT.0.0d0) GO TO 280
  270 CONTINUE
      NEG1 = 0
      GO TO 290
  280 NEGTAB(II) = I + NPOINT
      IF(I.GT.1) NEGTAB(II) = I - 1
  290 CONTINUE
c-----------------------------------------------------------------------
C
C     OUTPUT TO SCRATCH
C
c-----------------------------------------------------------------------
  300 WRITE(ISCR2) ETAB2X,SIG2X
      NPOINT=NPOINT+NPAGE
C-----IF REQUESTED PRINT MESSAGE EVERYTIME A PAGE IS OUTPUT TO SCRATCH.
      IF(MONITR.EQ.0) GO TO 310
      CALL TIMER1(SECONDS)
      CALL OUT9(ETAB2(    1),FIELD(1,1))
      CALL OUT9(ETAB2(NPAGE),FIELD(1,2))
      WRITE(OUTP,490) NPOINT,((FIELD(M,I),M=1,11),I=1,2),SECONDS
      WRITE(*   ,490) NPOINT,((FIELD(M,I),M=1,11),I=1,2),SECONDS
  310 KPTP1=1
  320 KPOINT=KPTP1
      KPTP1=KPOINT+1
      ETAB2(KPOINT)=ERANGE(IRANGE)
      DO 330 IR=1,NREACT
      SIG2(IR,KPOINT)=0.0d0
  330 CONTINUE
C-----------------------------------------------------------------------
C
C     END OF RESONANCE REGION
C
C-----------------------------------------------------------------------
C
C     DEFINE START OF EACH REACTION
C
c-----------------------------------------------------------------------
      IF(NEG1.NE.0) GO TO 370
      NEG1 = 1
      DO 360 II=1,NREACT
      IF(NEGTAB(II).NE.0) GO TO 360
      DO 340 I=1,KPOINT
      IF(SIG2X(II,I).GT.0.0d0) GO TO 350
  340 CONTINUE
      NEG1 = 0
      GO TO 360
  350 NEGTAB(II) = I + NPOINT
      IF(I.GT.1) NEGTAB(II) = I - 1
  360 CONTINUE
c-----------------------------------------------------------------------
C
C     DEFINE TOTAL POINT COUNT.
C
c-----------------------------------------------------------------------
  370 NPOINT=NPOINT+KPOINT
C-----ALL POINTS HAVE BEEN CALCULATED. LEAVE IN CORE OR SAVE ON SCRATCH.
      IF(NPOINT.LE.NPAGE) GO TO 380
C-----OUTPUT TO SCRATCH
      WRITE(ISCR2) ETAB2X,SIG2X
      END FILE ISCR2
      REWIND ISCR2
      IUSE2=1
C-----IF REQUESTED PRINT MESSAGE EVERYTIME A PAGE IS OUTPUT TO SCRATCH.
      IF(MONITR.EQ.0) GO TO 380
      CALL TIMER1(SECONDS)
      CALL OUT9(ETAB2(     1),FIELD(1,1))
      CALL OUT9(ETAB2(KPOINT),FIELD(1,2))
      WRITE(OUTP,490) NPOINT,((FIELD(M,I),M=1,11),I=1,2),SECONDS
      WRITE(*   ,490) NPOINT,((FIELD(M,I),M=1,11),I=1,2),SECONDS
C-----SET IN CORE POINT COUNT TO ZERO (NCOUNT IS NOW THE TOTAL POINT
C-----COUNT).
  380 KPOINT=0
C-----PRINT SUMMARY OF ENERGY RANGE.
  390 NKPONT=NPOINT+KPOINT
      NBASE=NKPONT-NBASE
      CALL OUT9(ERANGE(IRM1  ),FIELD(1,1))
      CALL OUT9(ERANGE(IRANGE),FIELD(1,2))
      IF(NREG.EQ.1) WRITE(OUTP,420)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.2) WRITE(OUTP,430)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.3) WRITE(OUTP,440)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.4) WRITE(OUTP,450)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.1) WRITE(*   ,420)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.2) WRITE(*   ,430)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.3) WRITE(*   ,440)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      IF(NREG.EQ.4) WRITE(*   ,450)
     1 ((FIELD(M,I),M=1,11),I=1,2),NBASE
      NBASE=NKPONT
C-----END OF ENERGY RANGE LOOP.
  400 CONTINUE
c-----------------------------------------------------------------------
C
C     ALL CROSS SECTION POINTS HAVE NOW BEEN CALCULATED. PRINT SUMMARY
C     OF ENTIRE RESONANCE REGION.
C
c-----------------------------------------------------------------------
C-----OUTPUT SUMMARY OF TOTAL NUMBER OF POINTS GENERATED.
      WRITE(OUTP,460) NPOINT
      WRITE(*   ,460) NPOINT
  410 RETURN
  420 FORMAT(2X,22A1,I11,' Not in Any Resonance Region'/
     1 35X,' (WARNING - Not Expected - Check Data)')
  430 FORMAT(2X,22A1,I11,' Resolved')
  440 FORMAT(2X,22A1,I11,' Unresolved')
  450 FORMAT(2X,22A1,I11,' Overlapping Resolved/Unresolved'/
     1 35X,' (WARNING - This is Illegal in ENDF/B)')
  460 FORMAT(1X,78('=')/' Entire Resonance Region',I11,' Points'/
     1 1X,78('='))
  470 FORMAT(1X,78('=')/' Reconstructing Cross Sections from',
     1 ' Resonance Parameters'/1X,78('=')/
     2 '        E-Low     E-High   Points  ',
     3 ' Type of Resonance Region'/
     3 '        (eV)       (eV)   Generated',
     4 ' Messages'/1X,78('='))
  480 FORMAT(1X,78('=')/' No Resonance Contribution to Add to',
     1 ' Background Cross Section. MAT Copied.')
  490 FORMAT(25X,I10,11A1,' to',11A1,' eV',F11.2,' Sec.')
      END
      SUBROUTINE ERROK2(E)
C=======================================================================
C
C     DEFINE ALLOWABLE ERROR FOR RECONSTRUCTION OF ENERGY DEPENDENT
C     CROSS SECTIONS FROM FILE 2 RESONANCE PARAMETERS. THE ERROR LAW
C     CAN BE ENERGY INDEPENDENT (CONSTANT) OR ENERGY DEPENDENT
C     (GIVEN BY A LINEARLY INTERPOLABLE TABLE IN ENERGY VS. ERROR).
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/OKERR3/ENER3(1000),ER3(1000),MAXER3,NOERR
      COMMON/ERRORCOM/ERRXC3,ERMINMAX,ERMT2,ERMT102,ERSOFT,XCLOW
C-----INITIALIZE INDEX TO INTERPOLATION TABLE.
      DATA MINER2/2/
C-----INTERPOLATE TO FIND FIRST TABULATED ENERGY ABOVE E.
      IF(E.le.ENER3(1)) go to 80
      DO 10 NOWER2=MINER2,MAXER3
      IF(E.lt.ENER3(NOWER2)) go to 20
      IF(E.eq.ENER3(NOWER2)) go to 70
   10 CONTINUE
C-----EXTEND ERROR AS CONSTANT ABOVE TABULATED RANGE.
      GO TO 90
   20 NOWER1=NOWER2-1
      IF(E.eq.ENER3(NOWER1)) go to 60
      IF(E.gt.ENER3(NOWER1)) go to 50
      DO 30 NOWER2=2,MAXER3
      IF(E.lt.ENER3(NOWER2)) go to 40
      IF(E.eq.ENER3(NOWER2)) go to 70
   30 CONTINUE
      GO TO 90
C-----DEFINE INDEX AND INTERPOLATE ERROR.
   40 NOWER1=NOWER2-1
   50 ERRXC3=((ENER3(NOWER2)-E)*ER3(NOWER1)+
     1 (E-ENER3(NOWER1))*ER3(NOWER2))/(ENER3(NOWER2)-ENER3(NOWER1))
      MINER2=NOWER2
      RETURN
C-----EXACT MATCH TO TABULATED ENERGY. DEFINE INDEX AND ERROR.
   60 ERRXC3=ER3(NOWER1)
      MINER2=NOWER1
      IF(MINER2.LE.1) MINER2=2
      RETURN
C-----EXACT MATCH TO TABULATED ENERGY. DEFINE INDEX AND ERROR.
   70 ERRXC3=ER3(NOWER2)
      MINER2=NOWER2
      RETURN
C-----EXTEND ERROR AS CONSTANT BELOW TABULATED RANGE.
   80 ERRXC3=ER3(1)
      MINER2=2
      RETURN
C-----EXTEND ERROR AS CONSTANT ABOVE TABULATED RANGE.
   90 ERRXC3=ER3(MAXER3)
      MINER2=MAXER3
      RETURN
      END
      SUBROUTINE READ2
C=======================================================================
C
C     READ ALL FILE2 DATA. DEFINE ENERGY RANGES AND ENERGY NODES WITHIN
C     EACH ENERGY RANGE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*28 FISLST,REGLST,RTYPE,UTYPE,HOLNRO,HOLNAP
      COMMON/WHATZA/IZANOW,MATNOW,TEMP3,IVERSE,INT45
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/NAPRHO/NRO,NAPS
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/ETABS/NODES
      COMMON/RANGER/LOW,LHI
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/MINNIE/EMIN,EMAX,DEMIN
      COMMON/EDGES/ERANGE(50),NODLOW(50),NODHI(50),NRANGE,IRANGE
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/LRUNOW/LRUIN
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FIELDC/FIELD(11,12)
      COMMON/LISOCOM/LISO
      CHARACTER*1 ZABCDI
      COMMON/ZABCDCOM/ZABCDI(12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION FISLST(3),REGLST(4),RTYPE(8),
     1 UTYPE(4),HOLNRO(3),HOLNAP(3)
      DATA FISLST/
     1 ' (No Fission Widths)        ',
     2 ' (Fission Widths Given)     ',
     3 ' (ERROR - MUST be 0 or 1)   '/
      DATA REGLST/
     1 ' (No Parameters)            ',
     2 ' (Resolved Region)          ',
     3 ' (Unresolved Region)        ',
     4 ' (ERROR - See Below)        '/
      DATA RTYPE/
     1 ' (Single Level Breit-Wigner)',
     2 ' (Multi-Level Breit-Wigner) ',
     3 ' (Reich-Moore)              ',
     4 ' (Adler-Adler)              ',
     5 ' (General R-Matrix)         ',
     6 ' (Hybrid R-Function)        ',
     7 ' (Reich-Moore + Competition)',
     8 ' (ERROR - see Below)        '/
      DATA UTYPE/
     1 ' (All Energy Independent)   ',
     2 ' (Fission Energy Dependent) ',
     3 ' (All Energy Dependent)     ',
     4 ' (ERROR - see Below)        '/
      DATA HOLNRO/
     1 '(Energy Independent)        ',
     2 '(Energy Dependent)          ',
     3 '(ERROR - MUST be 0 or 1)    '/
      DATA HOLNAP/
     1 '(Calculate)                 ',
     2 '(= Scattering Radius)       ',
     3 '(ERROR - MUST be 0 to 2)    '/
C-----DEFINE THERMAL ENERGY FOR INCLUSION AS A NODE IF THE RESONANCE
C-----REGION SPANS THERMAL ENERGY.
      DATA ETHERM/2.53D-02/
C-----DEFINE MINIMUM ENERGY OF RESONANCE REGION (ALLOWING FOR ROUNDOFF).
      EMINL=0.999d0*EMIN
C-----FILE 2 FOUND. DEFINE ZA, AWR AND NIS (NUMBER OF ISOTOPES).
      ZA=C1H
      AWR=C2H
      NIS=N1H
C-----INITIALIZE FISSILE FLAG OFF (INDICATES ALL FISSION WIDTHS READ SO
C-----FAR ARE ZERO - IF THIS FLAG IS NOT RESET WHILE READING PARAMETERS
C-----THE MAT WILL BE CONSIDERED TO BE NON-FISSILE AND FISSION CROSS
C-----SECTIONS WILL NOT BE OUTPUT).
      LFWX  =0
      LFWSUM=0
C-----PRINT ZA, ATOMIC WEIGHT AND NUMBER OF ISOTOPES.
      IZA=ZA
c-----2020/3/27 - Updated assuming LISO applies
      CALL ZAHOLM(IZA,LISO,ZABCDI)
      CALL OUT9(AWR,FIELD(1,1))
      WRITE(OUTP,410) ZABCDI,(FIELD(M,1),M=1,11),NIS
      WRITE(*   ,410) ZABCDI,(FIELD(M,1),M=1,11),NIS
c-----------------------------------------------------------------------
C
C     INITIALIZE RESONANCE PARAMETER TABLE INDICES.
C
C     INITIALIZE SECTION COUNT (A SECTION IS DATA FOR ONE ISOTOPE,
C     ENERGY RANGE AND L VALUE) AND THE NUMBER OF ENERGY RANGES WITH
C     ENERGY DEPENDENT SCATTERING RADIUS. INITIALIZE ALL SECTION
C     INDICES TO INDICATE NO ENERGY DEPENDENT SCATTERING RADIUS.
c
c-----------------------------------------------------------------------
      NSECT=0
      NUMRHO=0
      DO 10 I=1,MAXSEC
      NRHO(I)=0
   10 CONTINUE
C-----INITIALIZE COUNT OF ENERGY NODES.
      NODES=0
C-----INITIALIZE INDEX TO NEXT RESONANCE TO READ.
      LHI=0
c-----------------------------------------------------------------------
C
C     READ DATA FOR EACH ISOTOPE.
C
c-----------------------------------------------------------------------
C-----INITIALIZE SUM OF FRACTIONAL ABUNDANCES.
      ABNSUM=0.0d0
      DO 320 IS=1,NIS
C-----SAVE CURRENT LFWX FLAG AND SET LFWX=0 FOR EACH ISOTOPE.
      IF(LFWX.gt.0) LFWSUM=LFWX
      LFWX=0
C-----DEFINE ISOTOPE ZA, ABUNDANCE, FLAG DEFINING WHETHER OR NOT FISSION
C-----WIDTHS ARE GIVEN AND NUMBER OF ENERGY RANGES.
      CALL CARDIO(ZAI,ABN,L1,LFW,NER,N2)
C-----INCREMENT SUM OF FRACTONAL ABUNDANCE.
      ABNSUM=ABNSUM+ABN
      IZAI=ZAI
c-----2020/3/27 - Updated assuming LISO applies
      CALL ZAHOLM(IZAI,LISO,ZABCDI)
      LFWO=LFW+1
      IF(LFW.NE.0.AND.LFW.NE.1) LFWO=3
      CALL OUT9(ABN,FIELD(1,1))
      WRITE(OUTP,460) IS,ZABCDI,(FIELD(M,1),M=1,11),LFW,FISLST(LFWO),NER
      WRITE(*   ,460) IS,ZABCDI,(FIELD(M,1),M=1,11),LFW,FISLST(LFWO),NER
c-----------------------------------------------------------------------
C
C     PRINT WARNING IF SUM OF FRACTIONAL ABUNDANCES IS NOT CLOSE TO
C     1.0
C
c-----------------------------------------------------------------------
      IF(IS.NE.NIS) GO TO 20
      IF(DABS(ABNSUM-1.0d0).LE.0.001d0) GO TO 20
      WRITE(OUTP,420)
      IF(NIS.EQ.1) WRITE(OUTP,430) ABNSUM
      IF(NIS.GT.1) WRITE(OUTP,440) ABNSUM
      WRITE(OUTP,450)
      WRITE(*   ,420)
      IF(NIS.EQ.1) WRITE(*   ,430) ABNSUM
      IF(NIS.GT.1) WRITE(*   ,440) ABNSUM
      WRITE(*   ,450)
c-----------------------------------------------------------------------
C
C     READ DATA FOR EACH ENERGY RANGE.
C
c-----------------------------------------------------------------------
   20 DO 300 IE=1,NER
C-----DEFINE ENERGY RANGE, TYPE OF RESONANCE RANGE (NO PARAMETERS/
C-----RESOLVED/UNRESOLVED) AND TYPE OF PARAMETER.
      CALL CARDI(ELX,EHX,LRUIN,LRF,N1,N2)
c-----Save original formalism = it ma be changed for testing
      LRFIN = LRF
c***** DEBUG
c-----Activate to change LRF=3 to LRF=2
c-----This is the one and only line to change
c     if(LRF.eq.3) LRF = 2
c***** DEBUG
c-----------------------------------------------------------------------
C
C     IF ENDS OF RESONANCE REGION ARE VERY CLOSE TO THE ENDS OF ANY
C     OTHER RESONANCE REGION INSURE THEY ARE EXACTLY THE SAME (THIS
C     PROCEDURE WILL AVOID MICRO OVERLAPS OR HOLES BETWEEN RESONANCE
C     REGIONS).
C
c-----------------------------------------------------------------------
      IF(NSECT.LE.0) GO TO 40
      DO 30 MSECT=1,NSECT
C-----FIRST CHECK FOR SAME ENERGY REGION (COMPARE LOWER TO LOWER AND
C-----UPPER TO UPPER ENERGY LIMITS).
      IF(DABS(ELX-EL(MSECT)).LE.DEMIN*EL(MSECT)) ELX=EL(MSECT)
      IF(DABS(EHX-EH(MSECT)).LE.DEMIN*EH(MSECT)) EHX=EH(MSECT)
C-----NEXT CHECK FOR ADJOINING ENERGY REGIONS (COMPARE LOWER TO UPPER
C-----AND UPPER TO LOWER ENERGY LIMITS).
      IF(DABS(ELX-EH(MSECT)).LE.DEMIN*EH(MSECT)) ELX=EH(MSECT)
      IF(DABS(EHX-EL(MSECT)).LE.DEMIN*EL(MSECT)) EHX=EL(MSECT)
   30 CONTINUE
C-----SAVE FLAG TO DETERMINE WHETHER OR NOT SCATTERING RADIUS IS
C-----ENERGY DEPENDENT (ONLY LEGAL FOR BREIT-WIGNER PARAMETERS).
   40 NRO=N1
C-----SAVE FLAG TO DETERMINE HOW TO USE THE CHANNEL AND SCATTERING RADII
      NAPS=N2
c-----------------------------------------------------------------------
C
C     DEFINE LRU FOR INTERNAL USE
C     LRUIN=0 -     NO RESONANCE PARAMETERS
C          =1 -     RESOLVED PARAMETERS.
C          =2 -     UNRESOLVED PARAMETERS.
C          =3 - 5 - SAME AS 0 - 2, ONLY THE RESONANCE CONTRIBUTION HAS
C                   ALREADY BEEN ADDED TO THE FILE 3 CROSS SECTIONS
C                   (USUALLY INDICATING THAT THIS MAT HAS ALREADY BEEN
C                   PROCESSED BY THIS OR A SIMILAR PROGRAM).
C     IF LRUIN IS 3 TO 5 THE PARAMETERS WILL BE READ BUT IGNORED IN ALL
C     SUBSEQUENT RESONANCE REGION CALCULATIONS.
C     SAVE LRU AS INPUT (LRUIN), DEFINE LRU FOR OUTPUT INDICATING THAT
C     THE RESONANCE CONTRIBUTION HAS ALREADY BEEN ADDED TO CROSS SECTION
C     (LRUOUT) AND DEFINE LRU FOR READING (LRU= 0, 1 OR 2).
C
C     PRINT ENERGY LIMITS AND TYPE OF RESONANCE REGION (UNRESOLVED OR
C     UNRESOLVED).
C
c-----------------------------------------------------------------------
      LRUOUT=LRUIN
C-----DO NOT CHANGE PARAMETER TYPE FOR ENDF/B-6 DATA (FILE 1 IS USED
C-----TO DEFINE THAT MATERIAL HAS BEEN PROCESSED).
      IF(IVERSE.EQ.6) GO TO 50
      IF(LRUOUT.GE.0.AND.LRUOUT.LE.2) LRUOUT=LRUOUT+3
   50 LRU=LRUIN
      IF(LRU.GT.2) LRU=LRU-3
C-----DEFINE AND PRINT INTERPRETATION OF TYPE OF RESONANCE REGION.
      LRULST=LRUIN+1
      IF(LRUIN.GE.3.AND.LRUIN.LE.5) LRULST=LRULST-3
      IF(LRUIN.LT.0.OR.LRUIN.GT.5) LRUIN=4
      CALL OUT9(ELX,FIELD(1,1))
      CALL OUT9(EHX,FIELD(1,2))
      WRITE(OUTP,470) ((FIELD(M,I),M=1,11),I=1,2),LRUIN,REGLST(LRULST)
      WRITE(*   ,470) ((FIELD(M,I),M=1,11),I=1,2),LRUIN,REGLST(LRULST)
c-----------------------------------------------------------------------
C
C     DEFINE AND PRINT INTERPRETATION OF TYPE OF RESOLVED PARAMETERS.
C
c-----------------------------------------------------------------------
      IF(LRU.NE.1) GO TO 60
      LRFLST=LRF
      IF(LRF.LT.1.OR.LRF.GT.7) LRFLST=8
      WRITE(OUTP,480) LRF,RTYPE(LRFLST)
      WRITE(*   ,480) LRF,RTYPE(LRFLST)
      GO TO 70
c-----------------------------------------------------------------------
C
C     DEFINE AND PRINT INTERPRETATION OF TYPE OF UNRESOLVED PARAMETERS.
C
c-----------------------------------------------------------------------
   60 IF(LRU.NE.2) GO TO 70
      LRFLST=4
      IF(LRF.EQ.1.AND.LFW.EQ.0) LRFLST=1
      IF(LRF.EQ.1.AND.LFW.EQ.1) LRFLST=2
      IF(LRF.EQ.2) LRFLST=3
      WRITE(OUTP,490) LRF,UTYPE(LRFLST)
      WRITE(*   ,490) LRF,UTYPE(LRFLST)
c-----------------------------------------------------------------------
C
C     PRINT INTERPRETATION OF NRO (ENERGY INDEPENDENT OR DEPENDENT
C     SCATTERING RADIUS) AND NAPS (CALCULATE CHANNEL RADIUS OR DEFINE
C     IT AS EQUAL TO THE SCATTERING RADIUS).
C
c-----------------------------------------------------------------------
C-----DEFINE WHETHER OR SCATTERING RADIUS IS ENERGY DEPENDENT.
   70 KNRO=NRO+1
      IF(NRO.LT.0.OR.NRO.GT.1) KNRO=3
      WRITE(OUTP,500) NRO,HOLNRO(KNRO)
      WRITE(*   ,500) NRO,HOLNRO(KNRO)
C-----DEFINE WHETHER CHANNEL RADIUS WILL BE CALCULATED OR SET EQUAL TO
C-----THE SCATTERING RADIUS.
      KNAPS=NAPS+1
      IF(KNAPS.EQ.3) KNAPS=2
      IF(NAPS.LT.0.OR.NAPS.GT.2) KNAPS=3
      WRITE(OUTP,510) NAPS,HOLNAP(KNAPS)
      WRITE(*   ,510) NAPS,HOLNAP(KNAPS)
c-----------------------------------------------------------------------
C
C     PRINT WARNING IF RESONANCE CONTRIBUTION HAS ALREADY BEEN ADDED TO
C     BACKGROUND CROSS SECTIONS.
C
c-----------------------------------------------------------------------
      IF(LRUIN.GE.3.AND.LRUIN.LE.5) WRITE(OUTP,520) LRUIN
      IF(LRUIN.GE.3.AND.LRUIN.LE.5) WRITE(*   ,520) LRUIN
c-----------------------------------------------------------------------
C
C     ERROR IF RESONANCE REGION BOUNDARIES ARE NOT IN ASCENDING ENERGY
C     ORDER OR NOT IN EXPECTED ENERGY RANGE.
C
c-----------------------------------------------------------------------
C-----IF RESONANCE REGION BOUNDARIES ARE NOT IN ASCENDING ENERGY ORDER
C-----PRINT MESSAGE AND TERMINATE.
      IF(ELX.GE.EHX) GO TO 80
C-----IF RESONANCE REGION BOUNDARIES ARE NOT IN EXPECTED ENERGY RANGE
C-----PRINT MESSAGE AND TRUNCATE RANGE TO EXPECTED RANGE.
      IF(ELX.GE.EMINL.AND.ELX.LE.EMAX.AND.EHX.GE.EMINL.AND.EHX.LE.EMAX)
     1 GO TO 100
      IF(ELX.LT.EMIN) ELX=EMIN
      IF(ELX.GT.EMAX) ELX=EMAX
      IF(EHX.LT.EMIN) EHX=EMIN
      IF(EHX.GT.EMAX) EHX=EMAX
      CALL OUT9(EMIN,FIELD(1,1))
      CALL OUT9(EMAX,FIELD(1,2))
      WRITE(OUTP,600) ((FIELD(M,I),M=1,11),I=1,2)
      WRITE(*   ,600) ((FIELD(M,I),M=1,11),I=1,2)
      CALL OUT9(ELX,FIELD(1,1))
      CALL OUT9(EHX,FIELD(1,2))
      WRITE(OUTP,610) ((FIELD(M,I),M=1,11),I=1,2)
      WRITE(*   ,610) ((FIELD(M,I),M=1,11),I=1,2)
      IF(ELX.LT.EHX) GO TO 100
   80 WRITE(OUTP,90)
      WRITE(*   ,90)
   90 FORMAT(///' ERROR - Resonance Region Energy Limits MUST be in'/
     1       '            Ascending Energy Order.'/
     2       '            Execution Terminated.'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     ERROR STOP IF LRU, LRF OR LRF, LFW COMBINATION IS ILLEGAL.
C     UNLESS THESE PARAMETERS ARE CORRECT THE PROGRAM CANNOT DETERMINE
C     THE FORMAT OF THE ENDF/B DATA.
C
c-----------------------------------------------------------------------
  100 CALL CARDO(ELX,EHX,LRUOUT,LRF,N1,N2)
C-----IF NECESSARY READ ENERGY DEPENDENT SCATTERING RADIUS.
C-----06/24/05 - MOVED CALL REAP AFTER ABOVE OUTPUT LINE
      IF(NRO.EQ.1) CALL RDAP
C-----TERMINATE IF ILLEGAL LRU.
      IF(LRUIN.GE.0.AND.LRUIN.LE.5) GO TO 120
      WRITE(OUTP,110) LRUIN
      WRITE(*   ,110) LRUIN
  110 FORMAT(///' ERROR - Illegal LRU=',I5,' (Expect 0 to 5).'/
     2          '         Cannot Determine Format of ENDF/B Data.'/
     3          '         Execution Terminated.'///)
      CALL ENDERROR
C-----COPY SECTION WITH NO PARAMETERS.
  120 IF(LRU.EQ.0) GO TO 180
c-----------------------------------------------------------------------
C
C     TERMINATE IF ILLEGAL RESOLVED REGION LRF.
C
c-----------------------------------------------------------------------
      IF(LRU.NE.1) GO TO 140
C-----02/14/04 - CHANGE TO 7 FOR NEW REICH-MOORE
      IF(LRF.GE.1.AND.LRF.LE.7) GO TO 160
      WRITE(OUTP,130) LRF
      WRITE(*   ,130) LRF
  130 FORMAT(///' ERROR - Illegal LRF=',I5,' (Expect 1 to 7).'/
     1          '         Cannot Determine Format of ENDF/B Data'/
     2          '         Execution Terminated'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     TERMINATE IF ILLEGAL UNRESOLVED LRF, LFW COMBINATION.
C
c-----------------------------------------------------------------------
  140 IF(LRU.NE.2) GO TO 160
      LRFLST=4
      IF(LRF.EQ.1.AND.LFW.EQ.0) LRFLST=1
      IF(LRF.EQ.1.AND.LFW.EQ.1) LRFLST=2
      IF(LRF.EQ.2) LRFLST=3
      IF(LRFLST.NE.4) GO TO 160
      WRITE(OUTP,150) LRF,LFW
      WRITE(*   ,150) LRF,LFW
  150 FORMAT(///' ERROR - Illegal LRF=',I5,' LFW=',I5,' Combination'/
     2          '         (Expect LRF=1,LFW=0 OR LRF=1,LFW=1 OR LRF=2)'/
     3          '         Cannot Determine Format of ENDF/B Data'/
     4          '         Execution Terminated'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     ERROR IF ILLEGAL VALUE FOR NRO (ENERGY INDEPENDENT OR DEPENDENT
C     SCATTERING RADIUS) AND/OR NAPS (CALCULATE CHANNEL RADIUS OR SET
C     IT EQUAL TO THE SCATTERING RADIUS).
C
c-----------------------------------------------------------------------
  160 IF(KNRO.NE.3) GO TO 170
C-----ILLEGAL NRO VALUE.
      WRITE(OUTP,580) NRO
      WRITE(*   ,580) NRO
      NRO=0
  170 IF(KNAPS.NE.3) GO TO 190
C-----ILLEGAL NAPS VALUE.
      WRITE(OUTP,590) NAPS
      WRITE(*   ,590) NAPS
      NAPS=0
      GO TO 190
c-----------------------------------------------------------------------
C
C     READ AND SAVE ALL PARAMETERS.
C
c-----------------------------------------------------------------------
C-----COPY SECTION WITH NO PARAMETERS.
  180 CALL CARDIO(SPI,AP,L1,L2,N1,N2)
      CALL OUT9(SPI,FIELD(1,1))
      CALL OUT9(AP ,FIELD(1,2))
      WRITE(OUTP,530) ((FIELD(M,I),M=1,2),I=1,2)
      WRITE(*   ,530) ((FIELD(M,I),M=1,2),I=1,2)
      GO TO 300
c-----------------------------------------------------------------------
c
C     SAVE INITIAL RESONANCE TABLE PARAMETERS. IF RESONANCE CONTRIBUTION
C     FOR THIS SECTION HAS ALREADY BEEN ADDED TO THE FILE 3 CROSS
C     SECTION THEY WILL BE RESTORED AFTER THE DATA HAS BEEN READ
C     (THIS WILL EFFECTIVELY IGNORE THE SECTION).
c
c-----------------------------------------------------------------------
  190 NSECTI=NSECT
      NODESI=NODES
      LHII=LHI
C-----ARE PARAMETERS RESOLVED OR UNRESOLVED.
      IF(LRU.EQ.2) GO TO 260
C-----READ ALL RESOLVED RESONANCE PARAMETERS.
      GO TO (200,200,210,220,230,240,250),LRF
c-----------------------------------------------------------------------
c
C     FOR ALL VERSIONS OF ENDF/B FORMAT ASSUME GENERAL REICH-MOORE
C     TREATMENT WITH TWO FISSION CHANNELS (NOTE, ENDF/B-6 REICH-MOORE
C     FORMAT HAS BEEN UPDATED TO BE EXACTLY THE SAME AS THE FORMAT IN
C     EARLIER VERSIONS OF ENDF/B).
c
c-----------------------------------------------------------------------
  200 CALL RDBW
      GO TO 270
  210 CALL RDRM1
      GO TO 270
  220 CALL RDAA
      GO TO 270
  230 CALL RDGRM
      GO TO 270
  240 CALL RDHRF
      GO TO 270
C-----2003 - NEW REICH-MOORE WITH COMPETITION
  250 CALL RDRML
      GO TO 270
C-----READ UNRESOLVED PARAMETERS (ANY OF THE 3 REPRESENTATIONS).
  260 CALL RDUR
c-----------------------------------------------------------------------
C
C     IF RESONANCE CONTRIBUTION OF CURRENT SECTION ALREADY BEEN ADDED
C     TO CROSS SECTIONS, SKIP THIS SECTION.
C
c-----------------------------------------------------------------------
  270 IF(LRUIN.NE.1.AND.LRUIN.NE.2) GO TO 280
      GO TO 290
C-----RESTORE INITIAL RESONANCE TABLE PARAMETERS (I.E., IGNOR CURRENT
C-----SECTION).
  280 NSECT=NSECTI
      NODES=NODESI
      LHI=LHII
      GO TO 300
C-----NO. USE LOWER AND UPPER ENERGY LIMIT AS NODES (USE ZERO WIDTH).
  290 CALL NOODLE(ELX,ZERO,EMIN,EMAX)
      CALL NOODLE(EHX,ZERO,EMIN,EMAX)
C-----END OF ENERGY RANGE LOOP.
  300 CONTINUE
C-----END OF ISOTOPE LOOP. TEST CONSISTENCY OF LFW AND LFI FLAGS AND
C-----FISSION DATA READ.
      IF(LFW.EQ.0.AND.LFWX.GT.0) WRITE(OUTP,540) LFW
      IF(LFW.GT.0.AND.LFWX.EQ.0) WRITE(OUTP,550) LFW
      IF(LFW.EQ.0.AND.LFWX.GT.0) WRITE(*   ,540) LFW
      IF(LFW.GT.0.AND.LFWX.EQ.0) WRITE(*   ,550) LFW
C-----ONLY CHECK LFI IF SECTION MF=1, MT-451 IS PRESENT (SINCE LFI IS
C-----DEFINED IN MF=1, MT=451).
      IF(MT451.LE.0) GO TO 310
      IF(LFI.EQ.0.AND.LFWX.GT.0) WRITE(OUTP,560) LFI
      IF(LFI.GT.0.AND.LFWX.EQ.0) WRITE(OUTP,570) LFI
      IF(LFI.EQ.0.AND.LFWX.GT.0) WRITE(*   ,560) LFI
      IF(LFI.GT.0.AND.LFWX.EQ.0) WRITE(*   ,570) LFI
C-----DEFINE CUMULATIVE LFWX FLAG.
  310 if(LFWX.gt.0) LFWSUM = 1
      IF(LFWSUM.GT.0.OR.LFWX.GT.0) LFWX=1
  320 CONTINUE
C-----ALL PARAMETERS READ. COPY TO END OF FILE 2.
      CALL COPYF
c-----------------------------------------------------------------------
C
C     ALL RESONANCE PARAMETERS HAVE BEEN READ. IF THERE ARE NO SECTIONS
C     WHOSE RESONANCE CONTRIBUTION MUST BE ADDED TO THE CROSS SECTIONS
C     THERE IS NOTHING TO DO. OTHERWISE, (1) DEFINE TABLE OF ENERGY
C     RANGES (USUALLY 1 OR 2, I.E. RESOLVED AND/OR UNRESOLVED), (2)
C     DEFINE WHICH NODES ARE WITHIN EACH ENERGY RANGE.
C
c-----------------------------------------------------------------------
      IF(NSECT.LE.0) GO TO 400
c-----------------------------------------------------------------------
C
C     DEFINE ENERGY RANGES.
C
c-----------------------------------------------------------------------
C-----DEFINE TABLE OF ENDS OF ALL RESONANCE REGIONS.
      IRANGE=0
      DO 350 IS=1,NSECT
      ELTNOW=EL(IS)
      EHTNOW=EH(IS)
      IF(ELTNOW.LT.EMIN) ELTNOW=EMIN
      IF(EHTNOW.GT.EMAX) EHTNOW=EMAX
      IF(ELTNOW.GE.EHTNOW) GO TO 350
C-----IGNOR REPEATED ENERGY RANGES.
      IF(IRANGE.LT.2) GO TO 340
      DO 330 I=1,IRANGE,2
      IF(ELTNOW.EQ.ERANGE(I).AND.EHTNOW.EQ.ERANGE(I+1)) GO TO 350
  330 CONTINUE
C-----SAVE BOTH ENDS OF ENERGY REGION.
  340 IRANGE=IRANGE+1
      ERANGE(IRANGE)=ELTNOW
      IRANGE=IRANGE+1
      ERANGE(IRANGE)=EHTNOW
  350 CONTINUE
C-----SORT RESONANCE REGION BOUNDARIES INTO ASCENDING SORTS.
      CALL SORTD(ERANGE,IRANGE)
C-----ELIMINATE DUPLICATE ENERGY LIMITS.
      NRANGE=1
      DO 360 IS=2,IRANGE
      IF(DABS(ERANGE(IS)-ERANGE(NRANGE)).LE.DABS(DEMIN*ERANGE(NRANGE)))
     1 GO TO 360
      NRANGE=NRANGE+1
      ERANGE(NRANGE)=ERANGE(IS)
  360 CONTINUE
C-----ADD THERMAL ENERGY (0.0253 EV) IF RESONANCE REGION SPANS THERMAL
C-----ENERGY.
      CALL NOODLE(ETHERM,ZERO,ERANGE(1),ERANGE(NRANGE))
c-----------------------------------------------------------------------
C
C     DEFINE WHICH NODES CONTRIBUTE TO EACH RESONANCE REGION.
C
c-----------------------------------------------------------------------
      IN1=1
      IN2=1
      DO 390 IRANGE=2,NRANGE
      IF(IN1.GE.NODES) GO TO 380
      DO 370 IN2=IN1,NODES
      IF(ENODE(IN2).ge.ERANGE(IRANGE)) go to 380
  370 CONTINUE
      IN2=NODES
  380 NODLOW(IRANGE)=IN1
      NODHI(IRANGE)=IN2
      IN1=IN2
  390 CONTINUE
  400 RETURN
  410 FORMAT(1X,78('=')/' Listing of All Resonance Parameters'/
     1 1X,78('=')/
     1       ' Element or Material------------------',12A1/
     2       ' Atomic Weight Ratio------------------',11A1/
     3       ' Number of Isotopes-------------------',I11)
  420 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' For ENDF/B Evaluations the Fractional Abundance is Defined'/
     2 ' to be the Fraction of Each Isotope in the Evaluation.'/
     3 ' It is NOT the Naturally Occurring Isotopic Abundance'/
     4 ' Unless the Evaluation Contains Data for ALL Isotopes of'/
     5 ' an Element. For Any Evaluation the Sum of the Fractional'/
     6 ' Abundances for All Isotopes Included in the Evaluation'/
     7 ' Should be 1.0.')
  430 FORMAT(' For this Evaluation there is Only ONE Isotope'/
     1 ' and the Fractional Abundance is',F10.5,' Rather Than 1.0.')
  440 FORMAT(' For this Evaluation the Sum of the Fractional'/
     1 ' Abundances is',F10.5,' Rather than 1.0.')
  450 FORMAT(
     1 ' Either You are Doing this on Purpose for Some Special'/
     2 ' Application or Else the ENDF/B Data is Incorrect.'/
     3 ' This Program will Continue with the Calculations Asuuming'/
     4 ' that the Abundances Read from the Evaluation are Correct.'/
     5 ' Check ENDF/B Data and Correct the Fractional Abundances.')
  460 FORMAT(1X,78('=')/
     1       ' Isotope Number-----------------------',I11/
     2       ' Isotope------------------------------',12A1/
     3       ' Fractional Abundance-----------------',11A1/
     4       ' LFW (Fission Widths)-----------------',I11,A28/
     4       ' Number of Energy Ranges--------------',I11)
  470 FORMAT(1X,78('=')/
     1 ' Lower Limit of the Energy Range------',11A1,' eV'/
     2 ' Upper Limit of the Energy Range------',11A1,' eV'/
     3 ' LRU (Type of Region)-----------------',I11,A28)
  480 FORMAT(' LRF (Type of Resolved Parameters)----',I11,A28)
  490 FORMAT(' LRF (Energy Dependence of Widths)----',I11,A28)
  500 FORMAT(' NRO (Scattering Radius)--------------',I11,1X,A28)
  510 FORMAT(' NAPS (Channel Radius)----------------',I11,1X,A28)
  520 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' Note, LRU=',I2,' Indicates that the Resonance Contribution'/
     2 ' has Already been Added to the Cross Sections. This Section'/
     3 ' will be Read, but Ignored in All Resonance Calculations.')
  530 FORMAT(1X,78('=')/' No Parameters'/1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Effective Scattering Radius (A+)-----',11A1)
  540 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' WARNING - LFW=',I2,' (Indicates NO Fission Widths Given).'/
     1 '           but Fission Data Read are NOT ZERO. Will Ignor'/
     2 '           LFW and Assume Data Read is Correct.'/1X,78('='))
  550 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' WARNING - LFW=',I2,' (Indicates Fission Widths Given).'/
     1 '           but Fission Data Read are ALL ZERO. Will Ignor'/
     2 '           LFW and Assume Data Read is Correct.'/1X,78('='))
  560 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' WARNING - LFI=',I2,' from MF=1, MT=451 (Indicates Material',
     2 ' is NOT Fissile)'/
     1 '           but Fission Data Read are NOT ZERO. Will Ignor',
     2 ' LFI and Assume'/
     2 '           Data Read is Correct.'/1X,78('='))
  570 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' WARNING - LFI=',I2,' from MF=1, MT=451 (Indicates Material',
     2 ' is Fissile)'/
     1 '           but Fission Data Read are ALL ZERO. Will Ignor',
     2 ' LFI and Assume'/
     2 '           Data Read is Correct.'/1X,78('='))
  580 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' WARNING - NRO=',I5,' (Illegal Value)'/
     2 10X,' in Order to Continue this Program will Set NRO=0'/
     3 ' WARNING - This May Result in Errors - Check ENDF/B Data.')
  590 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' WARNING - NAPS=',I5,' (Illegal Value)'/
     2 10X,' In Order to Continue this Program will Set NAPS=0'/
     3 ' WARNING - This May Result in Errors - Check ENDF/B Data.')
  600 FORMAT(1X,7('WARNING...'),'WARNING'/
     1 ' Program Expects Resonance Region Between ',11A1,' and',
     2 11A1,' eV.')
  610 FORMAT(
     3 ' Program Will Truncate Resonance Region to',11A1,' and',
     2 11A1,' eV.'/
     4 ' Check Evaluated Data.'/1X,78('='))
      END
      SUBROUTINE FILE3
C=======================================================================
C
C     OUTPUT DATA IN ENDF/B FORMAT. ALL SECTIONS OF FILE 3 DATA WILL BE
C     PROCESSED BY THIS SUBROUTINE. ALL SECTIONS EXCEPT TOTAL, ELASTIC,
C     FISSION AND CAPTURE WILL BE COPIED BY THIS ROUTINE. FOR TOTAL,
C     ELASTIC, FISSION AND CAPTURE IF THERE ARE BACKGROUND CROSS SECTION
C     THE RESONANCE AND BACKGROUND CROSS SECTIONS WILL BE COMBINED AND
C     OUTPUT. IF THERE IS NO BACKGROUND CROSS SECTION ONLY THE RESONANCE
C     CONTRIBUTION WILL BE OUTPUT. IN EITHER CASE THE DATA (RESONANCE OR
C     RESONANCE PLUS BACKGROUND CONTRIBUTIONS) WILL NOT BE THINNED, I.E.
C     THE OUTPUT ENERGY GRID WILL BE THE UNION OF THE RESONANCE AND
C     BACKGROUND ENERGY GRIDS.
C
C     AFTER ALL OF FILE 3 HAS BEEN PROCESSED THE REMAINDER OF THE MAT
C     WILL BE COPIED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      REAL*4 SECONDS
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*8 REACTS,RSORT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/UNITS/ISCR2,ISCR23
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/POINTN/NPOINT,KPOINT
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/EDGES/ERANGE(50),NODLOW(50),NODHI(50),NRANGE,IRANGE
      COMMON/PAGER/NPAGE,NPAGP1,NPAGM1
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/LASTE/ELAST
      COMMON/FLAGS/MINUS3,IMPLUS
      COMMON/WHATZA/IZANOW,MATNOW,TEMP3,IVERSE,INT45
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
      COMMON/NEG1COM/NEG1    ! 2014/11/02 - add to FILE2 and FILE3
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
c-----07/26/09 - ADDED Q-VALUE FOR COMPETITIVE MF=3 OUTPUT
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION MREACT(20),MTADD(20),REACTS(20),QADD(11),NEGADD(11),
     1 NBTO(1),INTO(1),IHEAD(7),ISAVE1(7),SAVE1(2),MTCP(103:107)
      CHARACTER*8 BCDCP(103:107)
C-----3/29/07 - ADDED MISSING EQUIVALENCE
      EQUIVALENCE (L1H,IHEAD(1))
C     CHARGED.. 103 104 105 106 107
      DATA BCDCP/
     1 '   (n,p)',
     1 '   (n,d)',
     1 '   (n,t)',
     1 ' (n,He3)',
     1 '   (n,a)'/
c       12345678
      DATA MTCP/600,650,700,750,800/
      DATA MREACT/1,2, 4, 4,  3,15*0/
      DATA MTADD/ 1,2,18,19,102,15*0/
      DATA REACTS/
     1 '   Total',
     2 ' Elastic',
     3 ' Fission',
     4 ' (n,f) 1',
     5 ' Capture',
     6 15*' Others '/
      DATA INTO/2/
      DATA IZERO/0/
      DATA IONE/1/
      DATA IUSE23/0/
      DATA ISAVE1/0,0,0,0,0,0,0/
      DATA SAVE1/0.0d0,0.0d0/
      DATA OKDIFF/1.0d-9/
      DATA ZEROD /0.0d0/
c-----------------------------------------------------------------------
C
C     DEFINE REACTIONS TO OUTPUT.
C
c-----------------------------------------------------------------------
C-----initialize MT count for current MAT
      CALL MAXIE0
C-----SET UP INITIALLY FOR SIMPLE CASE = TOTAL, ELASTIC, CAPTURE
      MREACT(1) = 1     ! total
      MREACT(2) = 2     ! elastic
      MREACT(3) = 3     ! capture
      NEGADD(1) = NEGTAB(1)
      NEGADD(2) = NEGTAB(2)
      NEGADD(3) = NEGTAB(3)
      MTADD (1) = 1
      MTADD (2) = 2
      MTADD (3) = 102
      REACTS(1) = '   Total'
      REACTS(2) = ' Elastic'
      REACTS(3) = ' Capture'
      QADD  (1) = 0.0d0
      QADD  (2) = 0.0d0
      QADD  (3) = 0.0d0
      NROUT     = 3
c-----------------------------------------------------------------------
      IF(LRF7.ne.0) GO TO 10  ! LRF7 = Counter of LRF=7 sections
c-----------------------------------------------------------------------
C-----SIMPLE CASE - IF NECESSARY ADD 2 FISSION CHANNELS (MT=18 AND 19)
      IF(NREACT.EQ.3) GO TO 70
      MREACT(3) = 4     ! (n,f)
      MREACT(4) = 4     ! (n,f) first chance
      MREACT(5) = 3     ! capture
      NEGADD(3) = NEGTAB(4)
      NEGADD(4) = NEGTAB(4)
      NEGADD(5) = NEGTAB(3)
      MTADD (3) = 18
      MTADD (4) = 19
      MTADD (5) = 102
      REACTS(3) = ' Fission'
      REACTS(4) = ' (n,f) 1'
      REACTS(5) = ' Capture'
      QADD  (3) = 0.0d0
      QADD  (4) = 0.0d0
      QADD  (5) = 0.0d0
      NROUT     = 5
      GO TO 70
C-----------------------------------------------------------------------
C
C     GENERAL LRF=7 CASE
C
C-----------------------------------------------------------------------
   10 NROUT  = NREACT
      KREACT = NREACT
C-----ASSUME 1 AND 2 ARE TOTAL AND ELASTIC - DEFINE ALL OTHERS.
      DO 40 J=3,NREACT
      MREACT(J) = J
      NEGADD(J) = NEGTAB (J)
      MTADD (J) = MTREACT(J)
      REACTS(J) = '   Other'
      QADD  (J) = QREACT (J)
      IF(MTADD(J).EQ.18) GO TO 20
      DO K=103,107
      IF(MTADD(J).EQ.K) GO TO 30
      ENDDO
      if(MTADD(J).eq.102) REACTS(J) = ' Capture'
      if(MTADD(J).eq. 19) REACTS(J) = ' (n,f) 1'
      if(MTADD(J).ge. 50.and.
     1   MTADD(J).le. 91) REACTS(J) = '   Level'
      GO TO 40
c-----MT=18 - add MT=19, second chance fission
   20 REACTS(J) = ' Fission'
      KREACT = KREACT + 1
      MREACT(KREACT) = MREACT(J)
      NEGADD(KREACT) = NEGTAB(J)
      MTADD (KREACT) = 19
      REACTS(KREACT) = ' (n,f) 1'
      QADD  (KREACT) = QREACT(J)
      GO TO 40
c-----MT = 103 rhrough 107 - add charged particle level
   30 REACTS(J) = BCDCP(K)
      KREACT = KREACT + 1
      MREACT(KREACT) = MREACT(J)
      NEGADD(KREACT) = NEGTAB(J)
      MTADD (KREACT) = MTCP(K)
      REACTS(KREACT) = '   Level'
      QADD  (KREACT) = QREACT(J)
   40 CONTINUE
      NREACT = KREACT
      NROUT  = KREACT
C-----------------------------------------------------------------------
c
C     SORT INTO MT ORDER (Total and elastic already in order).
c
C-----------------------------------------------------------------------
      DO 60 J =  3,NREACT
      DO 50 JJ=J+1,NREACT
      IF(MTADD(JJ).GE.MTADD(J)) GO TO 50
      MM1   = MTADD (JJ)
      MM2   = MREACT(JJ)
      MM3   = NEGADD(JJ)
      RSORT = REACTS(JJ)
      QSORT = QADD  (JJ)
      MTADD (JJ) = MTADD (J)
      NEGADD(JJ) = NEGADD(J)
      MREACT(JJ) = MREACT(J)
      REACTS(JJ) = REACTS(J)
      QADD  (JJ) = QADD  (J)
      MTADD (J)  = MM1
      MREACT(J)  = MM2
      NEGADD(J)  = MM3
      REACTS(J)  = RSORT
      QADD  (J)  = QSORT
   50 CONTINUE
   60 CONTINUE
c-----------------------------------------------------------------------
c
c     Print summary.
c
c-----------------------------------------------------------------------
   70 write(*,80) (MTADD(kk),kk=1,NROUT)
      write(3,80) (MTADD(kk),kk=1,NROUT)
   80 format(' Summary of Output MT #'/1x,78('-')/(10i5))
c-----------------------------------------------------------------------
C
C     IF NO POINTS WERE GENERATED IN THE RESONANCE CALCULATION MERELY
C     COPY REMAINDER OF MAT UNLESS IN EDIT MODE.
C
c-----------------------------------------------------------------------
      IF(IMEDIT.EQ.2) GO TO 90
      IF(NPOINT.LE.0) GO TO 730
      WRITE(OUTP,750)
      WRITE(*   ,750)
      GO TO 100
   90 WRITE(OUTP,760)
      WRITE(*   ,760)
C-----INITIALIZE END OF FILE 3 FLAG OFF.
  100 MF3END=0
c-----------------------------------------------------------------------
c
c     Output results
c
c-----------------------------------------------------------------------
c-----initialize to ALL cross sections negative
      NEG1 = 0
      do i=1,NROUT
      NEGTAB(i) = 0
      enddo
C-----LOOP OVER RESONANCE REACTIONS.
      DO 660 IREACT=1,NROUT
C-----INITIALIZE TO OUTPUT ALL POINTS (MAY BE LESS FOR COMPETITION)
      NOUT2 = NPOINT
      MSKIP = 0
C-----DEFINE INDEX TO CURRENT REACTION.
      INDEX=MREACT(IREACT)
      IF(INDEX.LE.0) GO TO 660
C-----INITIALIZE NEGATIVE CROSS SECTION POINT COUNT.
      MINUS3=0
      IMPLUS=0
C-----INITIALIZE LAST ENERGY READ FROM FILE 3 FOR ASCENDING ENERGY TEST.
      ELAST=0.0d0
c-----------------------------------------------------------------------
c
C     TEST FOR CURRENT STATUS
C     MF3END=-1 - STILL IN FILE 3. RESTORE LAST LINE READ.
C           = 0 - STILL IN FILE 3. READ NEXT LINE.
C           = 1 - END OF FILE 3 READ (NO BACKGROUND)
C           = 2 - PAST END OF FILE 3 READ (NO BACKGROUND)
c
c-----------------------------------------------------------------------
      IF(MF3END.eq.0) go to 120
      IF(MF3END.gt.0) go to 580
C-----RESTORE LAST LINE READ.
      C1H=SAVE1(1)
      C2H=SAVE1(2)
      DO 110 I=1,7
      IHEAD(I)=ISAVE1(I)
  110 CONTINUE
      MF3END=0
      GO TO 150
C-----READ FIRST LINE OF NEXT SECTION OF FILE 3, FEND OR MEND LINE.
  120 CALL CONTI
      IF(MFH.eq.3) go to 150
      IF(MFH.gt.3) go to 130
C-----END OF FILE 3. SET FLAG. IF MEND LINE TREAT AS PAST END OF FILE 3.
      IF(MATH.LE.0) GO TO 130
      MF3END=1
      GO TO 580
C-----PAST THE END OF FILE 3. SET FLAG AND SAVE LINE.
  130 MF3END=2
      SAVE1(1)=C1H
      SAVE1(2)=C2H
      DO 140 I=1,7
      ISAVE1(I)=IHEAD(I)
  140 CONTINUE
      GO TO 580
C-----------------------------------------------------------------------
C
C     STILL IN FILE 3. SEE IF RESONANCE CONTRIBUTION MUST BE ADDED TO
C     CURRENT CROSS SECTION. DEPENDING ON CURRENT MT FROM FILE 3
C     EITHER,
C     (1) COPY SECTION - NOT UP TO REQUIRED SECTION YET.
C     (2) COMBINE RESONANCE AND BACKGROUND CONTRBUTION -SECTION MATCH.
C     (3) OUTPUT RESONANCE CONTRIBUTION - NO BACKGROUND.
C
C-----------------------------------------------------------------------
  150 CONTINUE
      IF(MTH.eq.MTADD(IREACT)) go to 160
      IF(MTH.gt.MTADD(IREACT)) go to 560
C-----COPY SECTION.
      CALL CONTO
      CALL COPYS
      GO TO 120
C-----------------------------------------------------------------------
C
C     BACKGROUND DATA PRESENT.
C
C-----------------------------------------------------------------------
C-----READ SECTION LEADER LINE AND INTERPOLATION LAW.
  160 CALL CARDI(C1,C2,L1,L2,N1,N2)
      CALL TERPI(NBT(1),INT(1),N1)
c-----2019/1/3 - Additional Interpolation Law Tests
      CALL TERPTEST(NBT(1),INT(1),N1,N2,3) ! INT MUST = 2
C-----DEFINE BACKGROUND TEMPERATURE.
      IF(IVERSE.NE.6) GO TO 170
      TEMP=TEMP3
      GO TO 180
  170 TEMP=C1
      IF(MTH.EQ.1) TEMP1=TEMP
      IF(L2.NE.0) TEMP=TEMP1
c-----------------------------------------------------------------------
c
c     2018/12/05 - If Background Temperature is not 0 Kelvin
c                  print ERROR message and Terminate = previously
c                  output resonance contribution = WRONG!!!!
c
c-----------------------------------------------------------------------
  180 IF(TEMP.LE.0.0d0) GO TO 190
      CALL OUT9(TEMP,FIELD(1,1))
      WRITE(OUTP,890) REACTS(IREACT),MTADD(IREACT),NOUT2,IZERO,
     1 NOUT2,(FIELD(M,1),M=1,11)
      WRITE(*   ,890) REACTS(IREACT),MTADD(IREACT),NOUT2,IZERO,
     1 NOUT2,(FIELD(M,1),M=1,11)
      CALL ENDERROR
  190 IF(IMEDIT.NE.2) GO TO 200
      WRITE(OUTP,790) REACTS(IREACT),MTADD(IREACT),N2
      WRITE(*   ,790) REACTS(IREACT),MTADD(IREACT),N2
      CALL SKIPS
      GO TO 660
C-----IF ALL FISSION WIDTHS ARE ZERO COPY BACKGROUND (NO RESONANCE
C-----CONTRIBUTION...BOTH FISSION AND FIRST CHANCE FISSION).
  200 IF(LFWX.NE.0) GO TO 210
      IF(MTH.NE.18.AND.MTH.NE.19) GO TO 210
      WRITE(OUTP,850) REACTS(IREACT),MTADD(IREACT),IZERO,N2,N2
      WRITE(*   ,850) REACTS(IREACT),MTADD(IREACT),IZERO,N2,N2
C-----OUTPUT SECTION HEADER AND LEADER CARDS AND INTERPOLATION LAW.
      CALL CONTO
      CALL CARDO(C1,C2,L1,L2,IONE,N2)
      NBTO(1)=N2
      CALL TERPO(NBTO,INTO,1)
C-----COPY REMAINDER OF SECTION.
      CALL COPYS
      GO TO 660
C-----------------------------------------------------------------------
C
C     COMBINE RESONANCE AND BACKGROUND CONTRIBUTIONS.
C
C-----------------------------------------------------------------------
C-----DEFINE RESONANCE AND TABULATED POINT COUNTS.
  210 NPT2=NPOINT
      NPT3=N2
      LEFT2=NPOINT
      LEFT3=N2
C-----INITIALIZE COMBINED POINT COUNTS.
      IPT23=0
      NPT23=0
C-----INITIALIZE NON-ZERO CROSS SECTION FLAG.
      KMPLUS=0
C-----INITIALIZE RESONANCE AND TABULATED POINT INDICES.
      IPT2=1
      IPT3=1
      KPT2=IPT2
      KPT3=IPT3
C-----IF REQUIRED POSITION FILE 2 SCRATCH FILE FOR READING.
      IF(NPOINT.GT.NPAGE) REWIND ISCR2
C-----READ FIRST PAGE OF TABULATED DATA.
      IF(NPT3.GT.NPAGE) NPT3=NPAGE
      CALL POINTI(ETAB3,SIG3,NPT3)
      LEFT3=LEFT3-NPT3
C-----IF ENTIRE BACKGROUND FITS IN CORE AND IT IS ZERO AT ALL ENERGIES
C-----IGNORE IT.
      IF(LEFT3.GT.0) GO TO 260
      DO 220 I=1,NPT3
      IF(DABS(SIG3(I)).NE.0.0d0) GO TO 260
  220 CONTINUE
C-----------------------------------------------------------------------
C
C     BACKGROUND IS ZERO = IGNORED. ADDITION NOT REQUIRED.
C
C-----------------------------------------------------------------------
      NPT23=NPOINT   ! Output Points = Recontruction Points
      WRITE(OUTP,800) REACTS(IREACT),MTADD(IREACT),NPOINT,N2,NPT23
      WRITE(*   ,800) REACTS(IREACT),MTADD(IREACT),NPOINT,N2,NPT23
C-----OUTPUT SECTION HEAD AND LEADER CARDS AND INTERPOLATION LAW.
C-----USE Q VALUE AS INPUT WITH BACKGROUND = NO C2 CHANGE HERE
      CALL CONTO
      CALL CARDO(C1,C2,L1,L2,IONE,NPT23)
      NBTO(1)=NPT23
      CALL TERPO(NBTO,INTO,1)
c-----Save MAT/MF/MT
      CALL MAXIE1(MATH,MFH,MTH)
c-----------------------------------------------------------------------
C
C     COPY FILE 2 POINTS TO FILE 3 ARRAYS AND OUTPUT A PAGE AT A TIME.
C
c-----------------------------------------------------------------------
      IPT23=0
      NPT2=1
c-----Page Loop
  230 IPT2=NPT2+NPAGM1
      IF(IPT2.GT.NPOINT) IPT2=NPOINT
      IF(NPOINT.GT.NPAGE) READ(ISCR2) ETAB2X,SIG2X
      JPT2=(IPT2-NPT2)+1
      DO 250 I=1,JPT2
      IPT23=IPT23+1
      IF(IPT23.LE.NPAGE) GO TO 240
C-----IF REQUEST MAKE ALL NEGATIVE CROSS SECTIONS = 0
      IF(MAKEPLUS.EQ.1) THEN
      DO KP=1,NPAGE
      IF(SIG23(KP).LT.0.0d0) THEN
      MINUS3=MINUS3+1
      SIG23(KP)=0.0d0
      ENDIF
      ENDDO
      ENDIF
      CALL POINTO(ETAB23,SIG23,NPAGE)
c-----Save Maximum energy and value
      CALL MAXIE2(ETAB23(NPAGE),SIG23(NPAGE))
      IPT23=1
  240 ETAB23(IPT23)=ETAB2(I)
      SIG23(IPT23)=SIG2(INDEX,I)
      NPT2=NPT2+NPAGE
  250 CONTINUE
      IF(NPT2.LE.NPOINT) GO TO 230
C-----IF REQUEST MAKE ALL NEGATIVE CROSS SECTIONS = 0
      IF(MAKEPLUS.EQ.1) THEN
      DO KP=1,IPT23
      IF(SIG23(KP).LT.0.0d0) THEN
      MINUS3=MINUS3+1
      SIG23(KP)=0.0d0
      ENDIF
      ENDDO
      ENDIF
C-----OUTPUT LAST PAGE OF POINTS.
      CALL POINTO(ETAB23,SIG23,IPT23)
c-----Save Maximum energy and value
      CALL MAXIE2(ETAB23(IPT23),SIG23(IPT23))
C-----COPY SEND LINE.
      CALL COPYS
      GO TO 650
C-----------------------------------------------------------------------
C
C     RESONANCE AND BACKGROUND CONTRIBUTIONS MUST BE ADDED TOGETHER.
C
C-----------------------------------------------------------------------
  260 E3=ETAB3(IPT3)
      XC3=SIG3(IPT3)
      E3LST=E3
      XC3LST=XC3
C-----INSURE FIRST PAGE OF RESONANCE DATA IS IN CORE AND DEFINE FIRST
C-----POINT.
      IF(NPOINT.LE.NPAGE) GO TO 270
      NPT2=NPAGE
      READ(ISCR2) ETAB2X,SIG2X
  270 LEFT2=LEFT2-NPT2
      E2=ETAB2(1)
      XC2=SIG2(INDEX,1)
      E2LST=E2
      XC2LST=XC2
C-----SELECT LOWEST ENERGY.
  280 IF(E2.eq.E3) go to 410
      IF(E2.gt.E3) go to 290
C-----TREAT SMALL DIFFERENCES AS EQUALITY (SAME TO ABOUT 9 DIGITS).
      IF(DABS(E2-E3).LE.OKDIFF*E2) GO TO 410
      GO TO 300
  290 IF(DABS(E3-E2).LE.OKDIFF*E3) GO TO 400
      GO TO 370
c-----------------------------------------------------------------------
C
C     FILE 2 ENERGY IS LOWER. INTERPOLATE FILE 3 DATA, OR DEFINE TO
C     BE ZERO IF FILE 3 DOES NOT SPAN ENERGY RANGE.
C
c-----------------------------------------------------------------------
  300 E23=E2
      IF(KPT3.GT.1) GO TO 310
      XC23=XC2
      GO TO 330
  310 IF(E3.GT.E3LST) GO TO 320
      XC23=XC2+XC3
      GO TO 330
  320 XC23=XC2+((E3-E2)*XC3LST+(E2-E3LST)*XC3)/(E3-E3LST)
C-----DEFINE NEXT FILE 2 POINT (USE EITHER NEXT POINT - FROM CORE OR
C-----SCRATCH - OR EXTEND CROSS SECTION BEYOND TABULATED RANGE AS
C-----ZERO).
  330 E2LST=E2
      XC2LST=XC2
      IPT2=IPT2+1
      KPT2=KPT2+1
      IF(IPT2.LE.NPT2) GO TO 360
      IF(LEFT2.GT.0) GO TO 350
C-----EXTEND CROSS SECTION AS ZERO (IF CROSS SECTION IS NOT YET ZERO
C-----SET CROSS SECTION TO ZERO AT CURRENT ENERGY. FOR ALL FOLLOWING
C-----POINTS LEAVE CROSS SECTION EQUAL TO ZERO AND SET CURRENT ENERGY
C-----EQUAL TO CURRENT ENERGY FROM FILE 3.)
      IF(XC2.LE.0.0d0) GO TO 340
      XC2=0.0d0
      GO TO 490
  340 E2=E3
      GO TO 490
C-----LOAD NEXT PAGE FROM SCRATCH AND RE-INITIALIZE IN CORE INDEX.
  350 IF(NPT2.GT.LEFT2) NPT2=LEFT2
      READ(ISCR2) ETAB2X,SIG2X
      LEFT2=LEFT2-NPT2
      IPT2=1
C-----USE NEXT POINT FROM CORE.
  360 E2=ETAB2(IPT2)
      XC2=SIG2(INDEX,IPT2)
      GO TO 490
c-----------------------------------------------------------------------
C
C     FILE 3 ENERGY IS LOWER. INTERPOLATE FILE 2 DATA, OR DEFINE TO
C     BE ZERO IF FILE 2 DOES NOT SPAN ENERGY RANGE.
C
c-----------------------------------------------------------------------
  370 E23=E3
      IF(KPT2.GT.1) GO TO 380
      XC23=XC3
      GO TO 450
  380 IF(E2.GT.E2LST) GO TO 390
      XC23=XC2+XC3
      GO TO 450
  390 XC23=XC3+((E2-E3)*XC2LST+(E3-E2LST)*XC2)/(E2-E2LST)
      GO TO 450
c-----------------------------------------------------------------------
C
C     ENERGIES ARE VERY CLOSE AND WILL BE TREATED AS EQUAL. SELECT THE
C     SMALLER ENERGY FOR OUTPUT.
C
c-----------------------------------------------------------------------
  400 E2=E3
c-----------------------------------------------------------------------
C
C     ENERGIES ARE EQUAL.
C
c-----------------------------------------------------------------------
  410 E23=E2
      XC23=XC2+XC3
C-----DEFINE NEXT FILE 2 POINT (USE EITHER NEXT POINT - FROM CORE OR
C-----SCRATCH - OR EXTEND CROSS SECTION BEYOND TABULATED RANGE AS
C-----ZERO).
      E2LST=E2
      XC2LST=XC2
      IPT2=IPT2+1
      KPT2=KPT2+1
      IF(IPT2.LE.NPT2) GO TO 440
      IF(LEFT2.GT.0) GO TO 430
c-----------------------------------------------------------------------
c
C     EXTEND CROSS SECTION AS ZERO (IF CROSS SECTION IS NOT YET ZERO
C     SET CROSS SECTION TO ZERO AT CURRENT ENERGY. FOR ALL FOLLOWING
C     POINTS LEAVE CROSS SECTION EQUAL TO ZERO AND SET CURRENT ENERGY
C     EQUAL TO CURRENT ENERGY FROM FILE 3.)
c
c-----------------------------------------------------------------------
      IF(XC2.LE.0.0d0) GO TO 420
      XC2=0.0d0
      GO TO 450
  420 E2=E3
      GO TO 450
C-----LOAD NEXT PAGE FROM SCRATCH AND RE-INITIALIZE IN CORE INDEX.
  430 IF(NPT2.GT.LEFT2) NPT2=LEFT2
      READ(ISCR2) ETAB2X,SIG2X
      LEFT2=LEFT2-NPT2
      IPT2=1
C-----USE NEXT POINT FROM CORE.
  440 E2=ETAB2(IPT2)
      XC2=SIG2(INDEX,IPT2)
C-----DEFINE NEXT FILE 3 POINT (USE EITHER NEXT POINT - FROM CORE OR
C-----FILE - OR EXTEND CROSS SECTION BEYOND TABULATED RANGE AS ZERO).
  450 E3LST=E3
      XC3LST=XC3
      IPT3=IPT3+1
      KPT3=KPT3+1
      IF(IPT3.LE.NPT3) GO TO 480
      IF(LEFT3.GT.0) GO TO 470
C-----EXTEND CROSS SECTION AS ZERO (IF CROSS SECTION IS NOT YET ZERO
C-----SET CROSS SECTION TO ZERO AT CURRENT ENERGY. FOR ALL FOLLOWING
C-----POINTS LEAVE CROSS SECTION EQUAL TO ZERO AND SET CURRENT ENERGY
C-----EQUAL TO CURRENT ENERGY FROM FILE 2.)
      IF(XC3.LE.0.0d0) GO TO 460
      XC3=0.0d0
      GO TO 490
  460 E3=E2
      GO TO 490
C-----LOAD NEXT PAGE FROM INPUT FILE AND RE-INITIALIZE IN CORE INDEX.
  470 IF(NPT3.GT.LEFT3) NPT3=LEFT3
      CALL POINTI(ETAB3,SIG3,NPT3)
      LEFT3=LEFT3-NPT3
      IPT3=1
C-----USE NEXT POINT FROM CORE.
  480 E3=ETAB3(IPT3)
      XC3=SIG3(IPT3)
C-----STORE COMBINED POINT.
  490 IPT23=IPT23+1
      IF(IPT23.LE.NPAGE) GO TO 510
      IF(NPT23.LE.0.AND.IUSE23.GT.0) REWIND ISCR23
      NPT23=NPT23+NPAGE
      WRITE(ISCR23) ETAB23,SIG23
C-----IF REQUESTED PRINT MESSAGE EVERYTIME A PAGE IS OUTPUT TO SCRATCH.
      IF(MONITR.EQ.0) GO TO 500
      CALL TIMER1(SECONDS)
      CALL OUT9(ETAB23(    1),FIELD(1,1))
      CALL OUT9(ETAB23(NPAGE),FIELD(1,2))
      WRITE(OUTP,880) NPT23,((FIELD(M,I),M=1,11),I=1,2),SECONDS
      WRITE(*   ,880) NPT23,((FIELD(M,I),M=1,11),I=1,2),SECONDS
  500 IPT23=1
c-----------------------------------------------------------------------
C
C     SAVE NEXT COMBINED POINT.
C
c-----------------------------------------------------------------------
  510 ETAB23(IPT23)=E23
      SIG23(IPT23)=XC23
c-----------------------------------------------------------------------
C
C     ONLY ALLOW UP TO 2 ENERGY POINTS WITH ZERO CROSS SECTION AT THE
C     BEGINNING OF THE TABLE.
C
c-----------------------------------------------------------------------
      IF(KMPLUS.NE.0) GO TO 530
      IF(XC23.NE.0.0d0) GO TO 520
      IF(IPT23.LE.2) GO TO 530
C-----KEEP SHIFTING POINTS FORWARD AND RESETTING POINT COUNT.
      ETAB23(2)=ETAB23(IPT23)
      SIG23(2)=SIG23(IPT23)
      IPT23=2
      GO TO 530
  520 KMPLUS=1
C-----CONTINUE COMBINING POINTS IF ANYMORE LEFT.
  530 IF(IPT2.LE.NPT2.OR.LEFT2.GT.0) GO TO 280
c-----------------------------------------------------------------------
c
C     WHEN OUT OF FILE 2 POINTS INSURE CURRENT FILE 2 ENERGY (WITH ZERO
C     CROSS SECTION) IS EQUAL TO FILE 3 ENERGY. NOTE, WHEN ENERGIES ARE
C     EQUAL NEXT E2 IS DEFINED BEFORE NEXT E3. THEREFORE WHEN E2 IS
C     BEING EXTEND TO HIGHER ENERGY WITH ZERO CROSS SECTION E2 DEFINED
C     ABOVE WILL BE SET EQUAL TO THE PRECEDING E3. THE FOLLOWING
C     STATEMENT WILL INSURE THAT IN THIS CASE E2 IS EXTEND AS EQUAL TO
C     THE CURRENT E3.
c
c-----------------------------------------------------------------------
      IF(XC2.EQ.0.0d0) E2=E3
      IF(IPT3.LE.NPT3.OR.LEFT3.GT.0) GO TO 280
C-----END OF SECTION. DEFINE COMBINED POINT COUNT AND IF REQUIRED SET UP
C-----SCRATCH FILE TO READ.
      NPT23=NPT23+IPT23
      IF(NPT23.LE.NPAGE) GO TO 540
      WRITE(ISCR23) ETAB23,SIG23
      END FILE ISCR23
      REWIND ISCR23
      IUSE23=1
C-----IF REQUESTED PRINT MESSAGE EVERYTIME A PAGE IS OUTPUT TO SCRATCH.
      IF(MONITR.EQ.0) GO TO 540
      CALL TIMER1(SECONDS)
      CALL OUT9(ETAB23(    1),FIELD(1,1))
      CALL OUT9(ETAB23(IPT23),FIELD(1,2))
      WRITE(OUTP,880) NPT23,((FIELD(M,I),M=1,11),I=1,2),SECONDS
      WRITE(*   ,880) NPT23,((FIELD(M,I),M=1,11),I=1,2),SECONDS
C-----OUTPUT SECTION HEAD AND LEADER CARDS AND INTERPOLATION LAW.
  540 WRITE(OUTP,780) REACTS(IREACT),MTADD(IREACT),NPOINT,N2,NPT23
      WRITE(*   ,780) REACTS(IREACT),MTADD(IREACT),NPOINT,N2,NPT23
      CALL CONTO
C-----USE Q VALUE FROM BACKGROUND = NO C2 CHANGE HERE
      CALL CARDO(C1,C2,L1,L2,IONE,NPT23)
      NBTO(1)=NPT23
      CALL TERPO(NBTO,INTO,1)
c-----Save MAT/MF/MT
      CALL MAXIE1(MATH,MFH,MTH)
C-----OUTPUT DATA A PAGE AT A TIME.
      IPTB=1
  550 IF(NPT23.GT.NPAGE) READ(ISCR23) ETAB23,SIG23
      IPT23=IPTB+NPAGM1
      IF(IPT23.GT.NPT23) IPT23=NPT23
      JPT23=(IPT23-IPTB)+1
C-----IF REQUEST MAKE ALL NEGATIVE CROSS SECTIONS = 0
      IF(MAKEPLUS.EQ.1) THEN
      DO KP=1,JPT23
      IF(SIG23(KP).LT.0.0d0) THEN
      MINUS3=MINUS3+1
      SIG23(KP)=0.0d0
      ENDIF
      ENDDO
      ENDIF
      CALL POINTO(ETAB23,SIG23,JPT23)
c-----Save Maximum energy and value
      CALL MAXIE2(ETAB23(JPT23),SIG23(JPT23))
      IPTB=IPTB+NPAGE
      IF(IPTB.LE.NPT23) GO TO 550
C-----COPY SEND LINE.
      CALL COPYS
      GO TO 650
C-----------------------------------------------------------------------
C
C     BEYOND REQUIRED SECTION (I.E. NO BACKGROUND). SAVE CURRENT LINE
C     AND SET FLAG.
C
C-----------------------------------------------------------------------
  560 SAVE1(1)=C1H
      SAVE1(2)=C2H
      DO 570 I=1,7
      ISAVE1(I)=IHEAD(I)
  570 CONTINUE
      MF3END=-1
C-----------------------------------------------------------------------
C
C     NO BACKGROUND. IF FISSION CROSS SECTION AND ALL FISSION WIDTHS
C     ARE ZERO SIMPLY SKIP THE REACTION (I.E., NO OUTPUT). OTHERWISE
C     USE INPUT PARAMETERS TO DECIDE TO EITHER HAVE NO OUTPUT OR TO
C     OUTPUT RESONANCE CONTRIBUTION.
C
C-----------------------------------------------------------------------
C-----DO NOT OUTPUT FISSION CROSS SECTION UNLESS FISSION WIDTHS ARE NOT
C-----ZERO.
  580 IF(MTADD(IREACT).EQ.18.OR.MTADD(IREACT).EQ.19) THEN
      IF(LFWX.LE.0) GO TO 660
      ENDIF
C-----IF IN EDIT MODE PRINT MESSAGE AND CONTINUE.
      IF(IMEDIT.NE.2) GO TO 590
      WRITE(OUTP,770) REACTS(IREACT),MTADD(IREACT)
      WRITE(*   ,770) REACTS(IREACT),MTADD(IREACT)
      GO TO 660
C-----USE INPUT OPTION TO DECIDE WHETHER OR NOT TO OUTPUT SECTION.
C-----07/10/16 - CORRECTED FOR NO MT=19 OUTPUT IF NO BACKGHROUND
  590 IF(MTADD(IREACT).NE.19) GO TO 600
C-----NO OUTPUT. PRINT MESSAGE EXPLAINING WHY.
      WRITE(OUTP,830) REACTS(IREACT),MTADD(IREACT),IZERO,IZERO,IZERO
      WRITE(*   ,830) REACTS(IREACT),MTADD(IREACT),IZERO,IZERO,IZERO
      GO TO 660
  600 IF(IMBACK.EQ.1) GO TO 610
C-----NO OUTPUT. PRINT MESSAGE EXPLAINING WHY.
      WRITE(OUTP,820) REACTS(IREACT),MTADD(IREACT),IZERO,IZERO,IZERO
      WRITE(*   ,820) REACTS(IREACT),MTADD(IREACT),IZERO,IZERO,IZERO
      GO TO 660
C-----------------------------------------------------------------------
C
C     OUTPUT RESONANCE CONTRIBUTION. DEFINE OUTPUT SECTION MAT/MF/MT.
C
C-----------------------------------------------------------------------
  610 MATH=MATNOW
      MFH=3
      MTH=MTADD(IREACT)
C-----USE Q-VALUE DEFINED BY RESONANCE PARAMETER INPUT
      C2OUT = QADD(IREACT)
C-----------------------------------------------------------------------
C
C     DEFINE POSSIBLE OUTSET FROM START OF SECTION TO ALLOW FOR
C     THRESHOLD
C
C-----------------------------------------------------------------------
      IF(NEGADD(IREACT).LE.0) NEGADD(IREACT) = 1
      MSKIP = NEGADD(IREACT) - 1
      MSKIP = 3*(MSKIP/3)
      NOUT2 = NPOINT - MSKIP
C-----------------------------------------------------------------------
C
C     PRINT DESCRIPTION OF SECTION.
C
C-----------------------------------------------------------------------
C-----SECTION OF O.K. TO BE COMBINED WITH BACKGROUND.
      WRITE(OUTP,810) REACTS(IREACT),MTADD(IREACT),NOUT2,IZERO,NOUT2
      WRITE(*   ,810) REACTS(IREACT),MTADD(IREACT),NOUT2,IZERO,NOUT2
C-----OUTPUT SECTION HEAD CARDS AND INTERPOLATION LAW.
      CALL CARDO(ZA,AWR,IZERO,IZERO,IZERO,IZERO)
      CALL CARDO(ZEROD,C2OUT,IZERO,IZERO,IONE,NOUT2)
      NBTO(1)=NOUT2
      CALL TERPO(NBTO,INTO,1)
c-----Save MAT/MF/MT
      CALL MAXIE1(MATH,MFH,MTH)
C-----RESONANCE CONTRIBUTION A PAGE AT A TIME.
      IF(NPOINT.GT.NPAGE) REWIND ISCR2
      IPOINT=1
  620 IF(NPOINT.GT.NPAGE) READ(ISCR2) ETAB2X,SIG2X
      KPOINT=IPOINT+NPAGM1
      IF(KPOINT.GT.NPOINT) KPOINT=NPOINT
      KOUT=(KPOINT-IPOINT)+1
      DO 630 K=1,KOUT
      SIG3(K)=SIG2(INDEX,K)
  630 CONTINUE
C-----IF REQUEST MAKE ALL NEGATIVE CROSS SECTIONS = 0
      IF(MAKEPLUS.EQ.1) THEN
      DO KP=1,KOUT
      IF(SIG3(KP).LT.0.0d0) THEN
      MINUS3=MINUS3+1
      SIG3(KP)=0.0d0
      ENDIF
      ENDDO
      ENDIF
      IF(MSKIP.LE.0) THEN
C-----NO THRESHOLD OR NOW ABOVE IT
      CALL POINTO(ETAB2,SIG3,KOUT)
c-----Save Maximum energy and value
      CALL MAXIE2(ETAB2(KOUT),SIG3(KOUT))
      ELSE
C-----SKIP TO THRESHOLD
      IF(MSKIP.GT.KOUT) GO TO 640
      KKOUT = KOUT  - MSKIP
      NOUT1 = MSKIP + 1
      CALL POINTO(ETAB2(NOUT1),SIG3(NOUT1),KKOUT)
c-----Save Maximum energy and value
      CALL MAXIE2(ETAB2(NOUT1),SIG3(NOUT1))
  640 MSKIP = MSKIP - KOUT
      IF(MSKIP.LE.0) MSKIP = 0
      ENDIF
      IPOINT=IPOINT+NPAGE
      IF(IPOINT.LE.NPOINT) GO TO 620
C-----ADD SEND LINE.
      CALL OUTS(MATH,MFH)
c-----------------------------------------------------------------------
C
C     END OF SECTION LOOP
C
c-----------------------------------------------------------------------
C-----PRINT WARNING IF CROSS SECTION IS NEGATIVE.
  650 IF(MINUS3.GT.0) THEN
      WRITE(OUTP,860) MINUS3
      WRITE(*   ,860) MINUS3
C-----2018/2/9 - Added message that < 0 set = 0
      if(MAKEPLUS.eq.1) THEN
      WRITE(OUTP,900)
      WRITE(*   ,900)
      ENDIF
      ENDIF
C-----PRINT WARNING MESSAGE IF CROSS SECTION IS NOT POSITIVE AT ANY ENER
C-----END OF REACTION LOOP.
      IF(IMPLUS.LE.0) WRITE(OUTP,870)
      IF(IMPLUS.LE.0) WRITE(*   ,870)
  660 CONTINUE
      WRITE(OUTP,840)
      WRITE(*   ,840)
C-----------------------------------------------------------------------
C
C     ALL RESONANCE REACTIONS HAVE BEEN OUTPUT. TEST FOR CURRENT STATUS
C     MF3END=-1 - STILL IN FILE 3. RESTORE LAST LINE READ, OUTPUT AND
C                 COPY TO END OF FILE 3.
C           = 0 - STILL IN FILE 3. COPY TO END OF FILE 3.
C           = 1 - END OF FILE 3 READ. OUTPUT FEND LINE.
C           = 2 - PAST END OF FILE READ. OUTPUT FEND LINE. RESTORE LAST
C                 LINE AND OUTPUT.
C     IF ALL CASES COPY REMAINDER OF MAT.
C
C-----------------------------------------------------------------------
      IF(MF3END.lt.0) go to 670
      IF(MF3END.eq.0) go to 690
      IF(MF3END.lt.1) go to 690
      IF(MF3END.eq.1) go to 700
      go to 710
C-----STILL IN FILE 3. RESTORE LAST LINE READ AND OUTPUT.
  670 C1H=SAVE1(1)
      C2H=SAVE1(2)
      DO 680 I=1,7
      IHEAD(I)=ISAVE1(I)
  680 CONTINUE
      CALL CONTO
C-----STILL IN FILE 3. COPY TO END OF FILE 3.
  690 CALL COPYF
      GO TO 730
C-----END OF FILE 3 REACHED. OUTPUT FEND.
  700 CALL OUTF(MATH)
      GO TO 730
C-----PAST END OF FILE 3. OUTPUT FEND. RESTORE AND OUTPUT LAST LINE.
  710 CALL OUTF(MATH)
      C1H=SAVE1(1)
      C2H=SAVE1(2)
      DO 720 I=1,7
      IHEAD(I)=ISAVE1(I)
  720 CONTINUE
      CALL CONTO
C-----IF LAST LINE WAS MEND THERE IS NOTHING ELSE TO DO.
      IF(MATH.LE.0) GO TO 740
C-----COPY REMAINDER OF MAT.
  730 CALL COPYM
c-----Print WARNING if not all output MTs have same Maximum Energy
  740 CALL MAXIE3(1)
C-----PRINT RUNNING TIME
      CALL TIMEMAT
      RETURN
  750 FORMAT(1x,78('=')/' Combining File 2 and File 3 Data'/1X,78('=')/
     1 ' Reaction   MT  File 2  File 3  Combined'/
     2 '                Points  Points    Points Comments'/1X,78('='))
  760 FORMAT(1X,78('=')/' Edit of File 3 Data'/1X,78('=')/
     1 ' Reaction   MT  File 3'/
     2 '                Points Comments'/1X,78('='))
  770 FORMAT(1X,A8,I5,8X,' No Background.')
  780 FORMAT(1X,A8,I5,2I8,I10)
  790 FORMAT(1X,A8,I5,I8)
  800 FORMAT(1X,A8,I5,2I8,I10,' Background = 0 at ALL Energies ',
     1 '- Ignored.')
  810 FORMAT(1X,A8,I5,2I8,I10,' No Background. Output Resonance Part.')
  820 FORMAT(1X,A8,I5,2I8,I10,' No Background. No Output (as per Input',
     1 ' Parameter)')
  830 FORMAT(1X,A8,I5,2I8,I10,' No Background. Never Output for first',
     1 ' chance fission')
  840 FORMAT(1X,78('='))
  850 FORMAT(1X,7('WARNING...'),'WARNING'/
     1 1X,A8,I5,2I8,I10,' All Fission Widths are ZERO.'/
     3 35X,' WilL Output Background Cross Section.')
  860 FORMAT(1X,7('WARNING...'),'WARNING'/
     1 35X,' Above Cross Section is Negative at',I6,' Energies.')
  870 FORMAT(1X,7('WARNING...'),'WARNING'/
     1 35X,' Above Cross Section is NOT Positive at ANY Energy')
  880 FORMAT(25X,I10,11A1,' to',11A1,' eV',F11.2,' Sec.')
  890 FORMAT(1X,A8,I5,2I8,I10,' Background ',11A1,' Kelvin.'/
     1       9('ERROR==='),'ERROR=='/
     2 ' This code calculates 0 Kelvin cross section and Outputs them',
     3 ' to MF=3.'/
     4 ' To be COMPATIBLE the Background Temperature included in',
     5 ' MF/MT=1/451'/
     6 ' MUST be 0 Kelvin. PLEASE correct the Temperature in',
     7 ' MF/MT=1/451 and'/' re-run this code.'/1x,79('=')/
     8 ' EXECUTION TERMINATED.')
  900 FORMAT(
     1 35X,' As per INPUT Option, Negative Cross Section Set = 0.')
      END
      SUBROUTINE WARNRRR(ERESLAST)
C=======================================================================
c
c     2020/9/17 - Print WARNING = Resolved resonances do not span
c                                 entire Resolved energy range.
c
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/INDATD/ELX,EHX
      CHARACTER*1 FIELDEHX
      DIMENSION FIELDEHX(11,2)
c--------------------------
      if(ERESLAST.ge.EHX) return
      CALL OUT9(ERESLAST,FIELDEHX(1,1))
      CALL OUT9(EHX     ,FIELDEHX(1,2))
      write(3,10) FIELDEHX
      write(*,10) FIELDEHX
   10 FORMAT(1x,78('-')/
     1 ' WARNING - Above (L, j) sequence tabulates resonances up to',
     2 11A1/
     3 '           Does not span the entire Resolved Region up to  ',
     4 11A1)
      return
      end
      SUBROUTINE WARNURR(ERESLAST)
C=======================================================================
c
c     2020/9/17 - Print WARNING = Unresolved resonances do not span
c                                 entire Unresolved energy range.
c
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/INDATD/ELX,EHX
      CHARACTER*1 FIELDEHX
      DIMENSION FIELDEHX(11,2)
c---------------------------
      if(ERESLAST.ge.EHX) return
      CALL OUT9(ERESLAST,FIELDEHX(1,1))
      CALL OUT9(EHX     ,FIELDEHX(1,2))
      write(3,10) FIELDEHX
      write(*,10) FIELDEHX
   10 FORMAT(1x,78('-')/
     1 ' WARNING - Above (L, j) sequence tabulates resonances up to',
     2 11A1,
     3 '           Does not span the entire Unresolved Region up to',
     4 11A1)
      return
      end
      SUBROUTINE RDBW
C=======================================================================
C
C     READ BREIT-WIGNER SINGLE OR MULTI LEVEL DATA FOR ONE ENERGY RANGE.
C     EACH L STATE WILL BE TREATED AS A SEPERATE SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*40 LRXHOL
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/NAPRHO/NRO,NAPS
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/RANGER/LOW,LHI
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
      DIMENSION LRXHOL(3)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DATA LRXHOL/
     1 '(No Competitive Widths)                 ',
     2 '(WARNING - Defined LC=LRX-1)            ',
     3 '(ERROR - < 0 Illegal - Will Assume = 0) '/
c       1234567890123456789012345678901234567890
      DATA C3/1.23D-01/
      DATA C5/8.0d-2/
C-----UPDATED NOV. 12, 1998 AS PER CSEWG SUBCOMMITTEE RECOMMENDATION
c-----Updated 9/01/04 - ENDF-102, appendix H
      DATA C4/1.00866491578d0/
      DATA C6/2.196807122623d-3/
C-----INITIALIZE NEGATIVE WIDTH COUNT
      NEGSUM = 0
C-----DEFINE TARGET SPIN, SPIN UP SCATTERING RADIUS AND NUMBER OF L
C-----STATES.
      CALL CARDIO(SPI,AP,L1,L2,NLS,N2)
      CALL OUT9(SPI,FIELD(1,1))
      IF(NRO.EQ.0) GO TO 10
      WRITE(OUTP,150) (FIELD(M,1),M=1,11),NLS
      WRITE(*   ,150) (FIELD(M,1),M=1,11),NLS
      GO TO 20
   10 CALL OUT9(AP ,FIELD(1,2))
      WRITE(OUTP,160) ((FIELD(M,I),M=1,11),I=1,2),NLS
      WRITE(*   ,160) ((FIELD(M,I),M=1,11),I=1,2),NLS
c-----------------------------------------------------------------------
C
C     L STATE LOOP.
C
c-----------------------------------------------------------------------
   20 DO 130 IL=1,NLS
C-----DEFINE ATOMIC WEIGHT RATIO, Q VALUE FOR COMPETITIVE REACTION,
C-----L VALUE, COMPETITIVE REACTION FLAG AND NUMBER OF RESONANCES.
      CALL CARDIO(AWRI,QX,LNOW,LRX,NRS6X,NRS)
C-----CHECK LRX
      LRXOUT = 1               ! LRX = 0
      IF(LRX.GT.0) LRXOUT = 2  ! LRX > 0
      IF(LRX.LT.0) LRXOUT = 3  ! LRX < 0
      CALL OUT9(AWRI,FIELD(1,1))
      CALL OUT9(QX  ,FIELD(1,2))
      WRITE(OUTP,170) ((FIELD(M,I),M=1,11),I=1,2),
     1 LNOW,LRX,LRXHOL(LRXOUT)
      WRITE(*   ,170) ((FIELD(M,I),M=1,11),I=1,2),
     1 LNOW,LRX,LRXHOL(LRXOUT)
      IF(LRX.LT.0) LRX = 0   ! SET AFTER OUTPUT LISTING
      IF(LRX.GT.0) THEN
      WRITE(OUTP,180) LRX-1
      WRITE(*   ,180) LRX-1
      ENDIF
      WRITE(OUTP,190) NRS
      WRITE(*   ,190) NRS
c-----------------------------------------------------------------------
C
C     CONSTRUCT TABLE OF PERMISSIBLE J-VALUES
C     (THIS TABLE WILL BE USED TO CHECK FOR MISSING OR ILLEGAL J-VALUES)
C
c-----------------------------------------------------------------------
      CALL SETJ(LNOW)
C-----INITIALIZE FLAGS TO CHECK COMPETITIVE WIDTHS.
      LRXIN=0
      LRXNEG=0
C-----INCREMENT SECTION COUNT AND INDICES TO RESONANCE PARAMETER TABLE.
      CALL LIMIT1(1)
C-----DEFINE ALL PARAMETERS FOR A SECTION.
      AWRICM=AWRI/(AWRI+ONE)
C-----EITHER CALCULATE CHANNEL RADIUS OR DEFINE TO BE EQUAL TO THE
C-----SCATTERING RADIUS.
      IF(NAPS.NE.0) GO TO 30
      APX=C3*((C4*AWRI)**(ONE/THREE))+C5
      GO TO 40
   30 APX=AP
   40 AK1=C6*AWRICM
      AK2=AK1*AK1
      BETA(NSECT)=PI*ABN/AK2
      RHOX2(NSECT)=APX*APX*AK2
      RHOP1(NSECT)=AP*AK1
      EL(NSECT)=ELX
      EH(NSECT)=EHX
      QVALUE(NSECT)=QX/AWRICM
      NLOW(NSECT)=LOW
      NHIGH(NSECT)=LHI
      LVALUE(NSECT)=LNOW
c-----2016/11/16 - Added for L-state dependent fission.
      LFWL(NSECT) = 0
c-----10/05/28 - added for potential correction
      POTL(NSECT) = 4*LNOW + 2  ! 2(2l+1) total weight of all sequences
      ADDL(NSECT) = 0.0d0       ! initialize weight of missing sequences
      LRXTAB(NSECT)=LRX
      NAPTAB(NSECT)=NAPS
      MODE(NSECT)=LRF
c-----------------------------------------------------------------------
C
C     06/09/05 - THIS WAS ALREADY DONE FOR BREIT-WIGNER - NOW ALL.
C
C     IF ENERGY DEPENDENT SCATTERING RADIUS DEFINE INDEX TO TABULATED
C     DATA (OTHERWISE INDEX HAS ALREADY BEEN INITIALIZED TO 0).
C     CALCULATE ENERGY DEPENDENT RHOP FROM SCATTERING RADII.
C
c-----------------------------------------------------------------------
      IF(NRO.GT.0) THEN
      NRHO(NSECT)=NUMRHO
      INX3=INXRHO(3,NUMRHO)
      INX4=INXRHO(4,NUMRHO)
      DO I=INX3,INX4
      RHOTAB(I)=APTAB(I)*AK1
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
C
C     READ RESONANCE PARAMETERS (6 PER RESONANCE).
C
C     (1) ENERGY
C     (2) J
C     (3) TOTAL WIDTH
C     (4) NEUTRON WIDTH
C     (5) CAPTURE WIDTH
C     (6) FISSION WIDTH
C
C     SOME OF THESE PARAMETERS WILL BE CONVERTED TO THE FORM REQUIRED
C     FOR CALCULATIONS.
C
C     (2) STATISTICAL WEIGHT, GJ (J NOT NEEDED)
C     (3) COMPETITIVE WIDTH DIVIDED BY PENETRATION FACTOR (TOTAL WIDTH
C         NOT NEEDED)
C     (4) NEUTRON WIDTH DIVIDED BY PENETRATION FACTOR (NEUTRON WIDTH IS
C         ONLY USED IN A FORM DIVIVED BY THE PENETRATION FACTOR).
C
c-----------------------------------------------------------------------
      CALL LISPAR6(LOW,NRS6X)
      IF(IMEDIT.EQ.0) GO TO 50
      IF(LRF.EQ.1) WRITE(OUTP,200)
      IF(LRF.EQ.2) WRITE(OUTP,210)
      IF(LRF.EQ.1) WRITE(*   ,200)
      IF(LRF.EQ.2) WRITE(*   ,210)
C-----FOR EACH SECTION (ISOTOPE, ENERGY RANGE, L VALUE) SORT
C-----RESONANCES IN ASCENDING (J, E) ORDER.
   50 CALL ORDER(LOW,LHI)
      AJNOW=RESTAB(2,LOW)
C-----CHECK FOR LEGAL J-VALUE AND DEFINE STATISTICAL WEIGHT.
      CALL CHECKJ(AJNOW,LNOW,GJ,LRF)
      DO 120 JR=LOW,LHI
c***** DEBUG
c-----Change LRF=3 to LRF=2 if input (LRFIN) was actually = 3
c-----WARNING - This section is controlled by an earlier definition
c-----          setiing LRFIN = LRF to bypass this section - unless
c-----          LRF is redefined from 3 to 2 (one line to change
c-----          earlier in code)
      if(LRFIN.eq.3) then
      W3 = DABS(RESTAB(3,JR))     ! elastic    LRF=3 input
      W4 = DABS(RESTAB(4,JR))     ! capture
      W5 = DABS(RESTAB(5,JR))     ! fission
      W6 = DABS(RESTAB(6,JR))
      RESTAB(6,JR) = W5 + W6      ! fission    LRF=2 input equivalent
      RESTAB(5,JR) = W4           ! capture
      RESTAB(4,JR) = W3           ! elastic
      RESTAB(3,JR) = W3+W4+W5+W6  ! total
      endif
c***** DEBUG
C-----IF FISSION WIDTH IS NOT ZERO TURN ON FISSILE FLAG.
      IF(DABS(RESTAB(6,JR)).NE.0.0d0) LFWX=1
      IF(DABS(RESTAB(6,JR)).NE.0.0d0) LFWL(NSECT) = LFWL(NSECT) + 1
C-----INSERT DIVIDING LINE BETWEEN DIFFERENT J VALUES.
      IF(DABS(AJNOW-RESTAB(2,JR)).LE.0.01d0) GO TO 60
c-----------------------------------------------------------------------
c
c     2020/9/17 - Test (L, J) sequence spans resonance region
c
c-----------------------------------------------------------------------
      call WARNRRR(RESTAB(1,JR-1))
      AJNOW=RESTAB(2,JR)
C-----CHECK FOR LEGAL J-VALUE AND DEFINE STATISTICAL WEIGHT.
      CALL CHECKJ(AJNOW,LNOW,GJ,LRF)
      IF(IMEDIT.NE.0) WRITE(OUTP,230)
C-----SEE IF TOTAL EQUALS ELASTC + CAPTURE + FISSION.
   60 GAMC=RESTAB(3,JR)-(RESTAB(4,JR)+RESTAB(5,JR)+RESTAB(6,JR))
C-----IF DIFFERENCE IS VERY SMALL COMPARED TO TOTAL SET IT TO ZERO.
      IF(DABS(GAMC).LE.0.00001d0*RESTAB(3,JR)) GAMC=0.0d0
C-----CHECK FOR NEGATIVE DIFFERENCES AND SIGNIFICANT POSITIVE DIFFERENCE
      IF(GAMC.LT.0.0d0) LRXNEG=LRXNEG+1
      IF(GAMC.NE.0.0d0) LRXIN=LRXIN+1
C-----LIST RESONANCE PARAMETERS.
C-----CHECK FOR NEGATIVE WIDTHS
      NEGRES = 0
      DO I=3,6
      IF(RESTAB(I,JR).LT.0.0d0) NEGRES = NEGRES + 1
      ENDDO
      IF(IMEDIT.EQ.0.AND.NEGRES.EQ.0) GO TO 90
      DO 70 I=1,6
      CALL OUT9(RESTAB(I,JR),FIELD(1,I))
   70 CONTINUE
      CALL OUT9(GAMC        ,FIELD(1,7))
      WRITE(OUTP,220) (FIELD(M,1),M=1,11),RESTAB(2,JR),
     1 ((FIELD(M,I),M=1,11),I=3,7)
      IF(NEGRES.GT.0) THEN
      NEGSUM = NEGSUM + NEGRES
      WRITE(   *,220) (FIELD(M,1),M=1,11),RESTAB(2,JR),
     1 ((FIELD(M,I),M=1,11),I=3,7)
      WRITE(   *,80) NEGRES
      WRITE(OUTP,80) NEGRES
   80 FORMAT(' ERROR - ',I1,' Negative Widths')
      ENDIF
C-----REPLACE J VALUE BY STATISTICAL WEIGHT.
   90 RESJTAB(JR) = RESTAB(2,JR)
      RESTAB(2,JR)= GJ
C-----04/21/07 - IF NECESSARY DEFINE RHOX2
      IF(NRO.EQ.1.AND.NAPS.EQ.1) THEN
      ERABS=DABS(RESTAB(1,JR))
      CALL SETRHO1(ERABS,NSECT)
      ENDIF
C-----DEFINE COMPETITIVE WIDTH DIVIDED BY PENETRATION FACTOR.
C-----(WILL REPLACE TOTAL WIDTH IN RESTAB).
      IF(LRX.LE.0) GO TO 100
C-----IF NEGATIVE OR ZERO COMPETITIVE WIDTH SET IT TO ZERO.
      IF(GAMC.LE.0.0d0) GO TO 100
      RHOZ2=DABS(DABS(RESTAB(1,JR))+QVALUE(NSECT))*RHOX2(NSECT)
C-----08/30/04 - CHANGED TO ||Er| + Q| - from |Er + Q|
c-----           These are the SAME if Er > 0
C-----04/09/10 - CHANGED FROM FACTS2 TO FACTS3 - SHIFT NOT USED.
      CALL FACTS3(LRX-1,RHOZ2,PENFACC)
C-----02/14/04 - PROTECT AGAINST ZERO ENERGY RESONANCES
      IF(PENFACC.NE.0.0d0) THEN
      RESTAB(3,JR)=GAMC/PENFACC
      ELSE
      RESTAB(3,JR)=0.0d0
      ENDIF
      GO TO 110
  100 RESTAB(3,JR)=0.0d0
C-----DEFINE NEUTRON WIDTH DIVIDED BY PENETRATION FACTOR.
  110 RHO2X=DABS(RESTAB(1,JR))*RHOX2(NSECT)
      CALL FACTS2(LVALUE(NSECT),RHO2X,SHIFT2(JR),PENFAC)
C-----02/14/04 - PROTECT AGAINST ZERO ENERGY RESONANCES
      IF(PENFAC.NE.0.0d0) THEN
      RESTAB(4,JR)=RESTAB(4,JR)/PENFAC
      ELSE
      RESTAB(4,JR)=0.0d0
      ENDIF
  120 CONTINUE
c-----------------------------------------------------------------------
c
c     2020/9/17 - Test (L, J) sequence spans resonance region
c
c-----------------------------------------------------------------------
      call WARNRRR(RESTAB(1,LHI))
C-----PRINT ERROR MESSAGES IF WIDTHS DO NOT ADD UP OR COMPETITIVE
C-----WIDTHS ARE NEGATIVE, OR LRX=0 AND ALL COMPETITIVE WIDTHS ARE
C-----ZERO.
      IF(LRX.EQ.0.AND.LRXIN.GT.0) WRITE(OUTP,240) LRXIN
      IF(LRX.GT.0.AND.LRXNEG.GT.0) WRITE(OUTP,250) LRXNEG
      IF(LRX.GT.0.AND.LRXIN.EQ.0) WRITE(OUTP,260)
C-----IF NO POSITIVE COMPETITIVE WIDTHS SET FLAG OFF.
      IF(LRXIN.EQ.0) LRXTAB(NSECT)=0
c-----------------------------------------------------------------------
C
C     FOR MULTI-LEVEL PARAMETERS PRINT WARNING IF ANY J SEQUENCES ARE
C     MISSING AND ADD MISSING J SEQUENCE WITH NO RESONANCES IN ORDER TO
C     ALLOW POTENTIAL SCATTERING TO BE CORRECTLY CALCULATED.
C
c-----------------------------------------------------------------------
      IF(LRF.EQ.2) THEN
      LSECT = NSECT
      CALL MISSINGJ(LNOW,LSECT)
      ENDIF
C-----END OF L STATE LOOP.
  130 CONTINUE
c-----------------------------------------------------------------------
C
C     STOP IF NEGATIVE WIDTHS
C
c-----------------------------------------------------------------------
      IF(NEGSUM.GT.0) THEN
      WRITE(   *,140) NEGSUM
      WRITE(OUTP,140) NEGSUM
  140 FORMAT(///
     1 ' ERROR - Execution Terminated because of',i8,
     1 ' Negative Widths.'///)
      CALL ENDERROR
      ENDIF
      RETURN
  150 FORMAT(1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Energy Dependent Scattering Radius---(LISTED ABOVE)'/
     3       ' Number of L Values-------------------',I11)
  160 FORMAT(1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Effective Scattering Radius (A+)-----',11A1/
     3       ' Number of L Values-------------------',I11)
  170 FORMAT(1X,78('=')/
     1       ' Atomic Weight Ratio of Isotope-------',11A1/
     2       ' Competitive Reaction Q Value---------',11A1,' eV'/
     3       ' Angular Momentum (L)-----------------',I11/
     4       ' LRX (Competitive Widths)-------------',I11,1X,A40)
  180 FORMAT(
     1       ' LC (Competitive L Value: LRX-1)------',I11)
  190 FORMAT(
     5       ' Number of Resonances-----------------',I11)
  200 FORMAT(1X,78('=')/' Single Level Breit-Wigner',
     1 ' Resonance Parameters'/1X,78('=')/
     1 '      Energy  J Value      Total    Neutron',
     2 '    Capture    Fission Competition'/
     2 8X,'(eV)',9X,4(7X,'(eV)'),8X,'(eV)'/1X,78('='))
  210 FORMAT(1X,78('=')/' Multi-Level Breit-Wigner',
     1 ' Resonance Parameters'/1X,78('=')/
     1 '      Energy  J Value      Total    Neutron',
     2 '    Capture    Fission Competition'/
     2 8X,'(eV)',9X,4(7X,'(eV)'),8X,'(eV)'/1X,78('='))
  220 FORMAT(1X,11A1,F7.2,2X,44A1,1X,11A1)
  230 FORMAT(1X,78('-'))
  240 FORMAT(1X,78('=')/1X,7('WARNING...'),'WARNING'/
     1 ' LRX=0 IndicateS ALL Competitive Widths Should be ZERO and'/
     2 ' Data Should have Total Width = (Elastic+Capture+Fission).'/
     3 I5,' Resonances do NOT Satisfy this Condition.'/
     4 ' This Program will Assume that LRX is Correct and that ALL'/
     5 ' Competitive Widths are ZERO and will Define the Total Width'/
     6 ' to be the Sum of Elastic+Capture+Fission for ALL Resonances.'/
     7 ' Check Evaluated Data. Correct Either Sum of Widths or LRX.')
  250 FORMAT(1X,78('=')/1X,7('WARNING...'),'WARNING'/
     1 ' LRX=1 Indicates Competitive Widths Present.'/
     2 I5,' Competitive Widths are Negative.'/
     3 ' Will Set ALL Negative Competitive Widths Equal to ZERO.'/
     4 ' Check Evaluated Data. Correct Either Sum of Widths or LRX.')
  260 FORMAT(1X,78('=')/1X,7('WARNING...'),'WARNING'/
     1 ' LRX=1 Indicates Competitive Widths Present.'/
     2 ' There are NO Positive Competitive Widths.'/
     3 ' Will Assume Competitive Widths are ALL ZERO.'/
     4 ' Check Evaluated Data. Correct Either Sum of Widths or LRX.')
      END
      SUBROUTINE RDRM1
C=======================================================================
C
C     READ REICH-MOORE DATA FOR ONE ENERGY RANGE.
C     EACH L STATE WILL BE TREATED AS A SEPERATE SECTION.
C
C     THIS ROUTINE USES THE GENERAL REICH-MOORE FORMALISM WITH TWO
C     FISSION CHANNELS AS DEFINED IN ENDF/B-IV. THIS ROUTINE WILL BE
C     USED TO READ DATA IN ANY VERSION OF THE ENDF/B FORMAT (NOTE,
C     THE ENDF/B-6 REICH-MOORE FORMAT HAS NOW BEEN UPDATED TO BE
C     EXACTLY THE SAME AS EARLIER VERSIONS OF THE ENDF/B FORMAT).
C
C     CHECK FOR PRELIMINARY ENDF/B-6 FORMAT(NOW ABANDONED). TERMINATE
C     EXECUTION IF DATA IS IN PRELIMINARY ENDF/B-6 FORMAT.
C
C     FIELD DEFINITIONS FOR EACH RESONANCE
C
C     FIELD          PRELIMINARY          CURRENT
C                    ENDF/B-6  FORMAT     ENDF/B-6  FORMAT
C     =====          ================     ================
C       1            ENERGY               ENERGY
C       2            J                    J
C       3            TOTAL WIDTH          ELASTIC WIDTH
C       4            ELASTIC WIDTH        CAPTURE WIDTH
C       5            CAPTURE WIDTH        FISSION WIDTH 1
C       6            NOT USED             FISSION WIDTH 2
C                    (FISSION NOT
C                    ALLOWED)
C
C     IF THIRD FIELD (PRELIMINARY TOTAL) IS EQUAL TO THE SUM OF THE
C     FOURTH (PRELIMINARY ELASTIC) AND FIFTH (PRELIMINARY CAPTURE)
C     AND SIXTH FIELD IS ZERO (FISSION NOT ALLOWED IN PRELIMINARY
C     FORMAT) FOR ALL RESONANCES THIS PROGRAM WILL ASSUME THAT THE
C     DATA IS IN THE PRELIMINARY REICH-MOORE FORMAT AND TERMINATE
C     EXECUTION WITH A WARNING MESSAGE THAT THE DATA MUST BE CONVERTED
C     TO THE CURRENT REICH-MOORE FORMAT BEFORE IT CAN BE PROCESSED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/NAPRHO/NRO,NAPS
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/RANGER/LOW,LHI
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DATA C3/1.23D-01/
      DATA C5/8.0d-2/
C-----UPDATED NOV. 12, 1998 AS PER CSEWG SUBCOMMITTEE RECOMMENDATION
c-----Updated 9/01/04 - ENDF-102, appendix H
      DATA C4/1.00866491578d0/
      DATA C6/2.196807122623d-3/
C-----DEFINE CONSTANT TO ALLOW UP TO 1 PER-CENT DIFFERENCE BETWEEN
C-----TOTAL AND ELASTIC PLUS CAPTURE FOR TEST OF PRELIMINARY ENDF/B-6
C-----FORMAT.
      DATA ALLOW/0.01d0/
C-----INITIALIZE NEGATIVE WIDTH COUNT
      NEGSUM = 0
C-----INITIALIZE NO CAPTURE
      NOCAPSUM = 0
C-----INITIALIZE FLAG TO INDICATE THAT DATA IS IN THE PRELIMINARY
C-----ENDF/B-6 FORMAT.
      IMOLD=1
C-----DEFINE TARGET SPIN, SCATTERING RADIUS AND NUMBER OF L STATES.
      CALL CARDIO(SPI,AP,L1,L2,NLS,N2)
      CALL OUT9(SPI,FIELD(1,1))
      CALL OUT9(AP ,FIELD(1,2))
      WRITE(OUTP,180) ((FIELD(M,I),M=1,11),I=1,2),NLS
      WRITE(*   ,180) ((FIELD(M,I),M=1,11),I=1,2),NLS
c-----------------------------------------------------------------------
C
C     L STATE LOOP.
C
c-----------------------------------------------------------------------
      DO 130 IL=1,NLS
C-----DEFINE ATOMIC WEIGHT RATIO, L DEPENDENT SCATTERING RADIUS, L VALUE
C-----AND NUMBER OF RESONANCES.
      CALL CARDIO(AWRI,APL,LNOW,L2,NRS6X,NRS)
      AWRICM = AWRI/(AWRI+ONE)
      CALL OUT9(AWRI,FIELD(1,1))
      CALL OUT9(APL ,FIELD(1,2))
      WRITE(OUTP,190) ((FIELD(M,I),M=1,11),I=1,2),LNOW,NRS
      WRITE(*   ,190) ((FIELD(M,I),M=1,11),I=1,2),LNOW,NRS
C-----PRINT WARNING IF L DEPENDENT SCATTERING RADIUS IS NOT POSITIVE.
      IF(APL.GT.0.0d0) GO TO 10
C-----IF L DEPENDENT RADIUS IS NOT POSITIVE USE SCATTERING RADIUS.
C-----(SEE, ENDF/B-6 FORMATS AND PROCEDURES MANUAL).
      APL=AP
      CALL OUT9(APL ,FIELD(1,2))
      WRITE(OUTP,200) (FIELD(KK,2),KK=1,11)
      WRITE(*   ,200) (FIELD(KK,2),KK=1,11)
c-----------------------------------------------------------------------
C
C     CONSTRUCT TABLE OF PERMISSIBLE J-VALUES
C     (THIS TABLE WILL BE USED TO CHECK FOR MISSING OR ILLEGAL J-VALUES)
C
c-----------------------------------------------------------------------
   10 CALL SETJ(LNOW)
C-----INCREMENT SECTION COUNT AND INDICES TO RESONANCE PARAMETER TABLE.
      CALL LIMIT1(1)
C-----DEFINE ALL PARAMETERS FOR A SECTION.
C-----EITHER CALCULATE CHANNEL RADIUS OR DEFINE TO BE EQUAL TO THE
C-----SCATTERING RADIUS.
      IF(NAPS.NE.0) GO TO 20
      APX=C3*((C4*AWRI)**(ONE/THREE))+C5
      GO TO 30
   20 APX=APL
c-----------------------------------------------------------------------
C
C     APX = IS USED IN PENETRABILITIES AND SHIFT FACTORS.
C         = EITHER BASED ON FORMULA OR EQUAL TO APL, DEPENDING ON NAPS.
C     APL = IS USED IN THE HARD SPHERE PHASE SHIFTS.
C     (SEE, ENDF/B-6 FORMATS AND PROCEDURES MANUAL, PAGE 2.6)
C
c-----------------------------------------------------------------------
   30 AK1=C6*AWRICM
      AK2=AK1*AK1
      BETA(NSECT)=PI*ABN/AK2
      RHOX2(NSECT)=APX*APX*AK2
      RHOP1(NSECT)=APL*AK1
      EL(NSECT)=ELX
      EH(NSECT)=EHX
      NLOW(NSECT)=LOW
      NHIGH(NSECT)=LHI
      LVALUE(NSECT)=LNOW
C-----Added for L-State Dependent fission.
      LFWL(NSECT)  = 0
C-----10/05/28 - added for potential correction
      POTL(NSECT) = 4*LNOW + 2  ! 2(2l+1) total weight of all sequences
      ADDL(NSECT) = 0.0d0       ! initialize weight of missing sequences
c----- 02/08/08 - ADDED NAPS DEFINITION
      NAPTAB(NSECT)=NAPS
      MODE(NSECT)=LRF
c-----------------------------------------------------------------------
C
C     06/09/05 - THIS WAS ALREADY DONE FOR BREIT-WIGNER - NOW ALL.
C
C     IF ENERGY DEPENDENT SCATTERING RADIUS DEFINE INDEX TO TABULATED
C     DATA (OTHERWISE INDEX HAS ALREADY BEEN INITIALIZED TO 0).
C     CALCULATE ENERGY DEPENDENT RHOP FROM SCATTERING RADII.
C
c-----------------------------------------------------------------------
      IF(NRO.GT.0) THEN
      NRHO(NSECT)=NUMRHO
      INX3=INXRHO(3,NUMRHO)
      INX4=INXRHO(4,NUMRHO)
      DO I=INX3,INX4
      RHOTAB(I)=APTAB(I)*AK1
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
C
C     READ RESONANCE PARAMETERS (6 PER RESONANCE).
C
C     (1) ENERGY
C     (2) J
C     (3) NEUTRON WIDTH
C     (4) CAPTURE WIDTH
C     (5) FIRST FISSION WIDTH
C     (6) SECOND FISSION WIDTH
C
C     SOME OF THESE PARAMETERS WILL CONVERTED TO THE FORM REQUIRED
C     FOR CALCULATIONS.
C
C     (2) STATISTICAL WEIGHT, GJ (J IS NOT NEEDED)
C     (3) 1/2 NEUTRON WIDTH DIVIDED BY PENETRATION FACTOR (NEUTRON WIDTH
C         IS ONLY USED DIVIDED BY THE PENETRATION FACTOR)
C     (4) 1/2 THE CAPTURE WIDTH (THIS IS THE ONLY FORM IN WHICH THE
C         CAPTURE WIDTH IS USED)
C     (5) SIGNED SQUARE ROOT OF 1/2 FIRST FISSION WIDTH (ONLY FORM USED)
C     (6) SIGNED SQUARE ROOT OF 1/2 SECOND FISSION WIDTH (ONLY FORM USED
C
c-----------------------------------------------------------------------
      CALL LISPAR6(LOW,NRS6X)
      IF(IMEDIT.NE.0) WRITE(OUTP,210)
C-----FOR EACH SECTION (ISOTOPE, ENERGY RANGE, L VALUE) SORT
C-----RESONANCES IN ASCENDING (J, E) ORDER.
      CALL ORDER(LOW,LHI)
      AJNOW=RESTAB(2,LOW)
C-----CHECK FOR LEGAL J-VALUE AND DEFINE STATISTICAL WEIGHT.
      CALL CHECKJ(AJNOW,LNOW,GJ,2)
      DO 110 JR=LOW,LHI
C-----INSERT DIVIDING LINE BETWEEN DIFFERENT J VALUES.
      IF(DABS(AJNOW-RESTAB(2,JR)).LE.0.01d0) GO TO 40
c-----------------------------------------------------------------------
c
c     2020/9/17 - Test (L, J) sequence spans resonance region
c
c-----------------------------------------------------------------------
      call WARNRRR(RESTAB(1,JR-1))
      AJNOW=RESTAB(2,JR)
C-----CHECK FOR LEGAL J-VALUE AND DEFINE STATISTICAL WEIGHT.
      CALL CHECKJ(AJNOW,LNOW,GJ,2)
      IF(IMEDIT.NE.0) WRITE(OUTP,230)
C-----CHECK FOR NEGATIVE WIDTHS OR NO CAPTURE
   40 NEGRES = 0
      NOCAPT = 0
      DO I=3,4
      IF(RESTAB(I,JR).LT.0.0d0) NEGRES = NEGRES + 1
      ENDDO
      IF(RESTAB(4,JR).LE.0.0d0) NOCAPT = 1
      IF(IMEDIT.EQ.0.AND.NEGRES.EQ.0) GO TO 80
C-----LIST RESONANCE PARAMETERS.
      DO 50 I=1,6
      CALL OUT9(RESTAB(I,JR),FIELD(1,I))
   50 CONTINUE
      WRITE(OUTP,220) (FIELD(M,1),M=1,11),RESTAB(2,JR),
     1 ((FIELD(M,I),M=1,11),I=3,6)
C-----NEGATIVE WIDTHS?
      IF(NEGRES.GT.0) THEN
      NEGSUM = NEGSUM + NEGRES
      WRITE(OUTP,220) (FIELD(M,1),M=1,11),RESTAB(2,JR),
     1 ((FIELD(M,I),M=1,11),I=3,6)
      WRITE(   *,60) NEGRES
      WRITE(OUTP,60) NEGRES
   60 FORMAT(' ERROR - ',I1,' Negative Widths')
      ENDIF
C-----NO CAPTURE?
      IF(NOCAPT.GT.0) THEN
      NOCAPSUM = NOCAPSUM + NOCAPT
      WRITE(OUTP,220) (FIELD(M,1),M=1,11),RESTAB(2,JR),
     1 ((FIELD(M,I),M=1,11),I=3,6)
      WRITE(   *,70)
      WRITE(OUTP,70)
   70 FORMAT(' ERROR - Capture MUST be Positive')
      ENDIF
C-----CHECK FOR PRELIMINARY ENDF/B-6 FORMAT.
   80 IF(IMOLD.LE.0) GO TO 100
C-----NOT PRELIMINARY FORMAT IF THIRD FIELD IS NOT THE SUM OF THE
C-----FOURTH AND FIFTH FIELDS.
      IF(DABS(RESTAB(3,JR)-(RESTAB(4,JR)+RESTAB(5,JR))).GT.
     1 DABS(ALLOW*RESTAB(3,JR))) GO TO 90
C-----NOT PRELIMINARY FORMAT IF SIXTH FIELD IS NOT ZERO.
      IF(DABS(RESTAB(6,JR)).EQ.0.0d0) GO TO 100
C-----DATA IS NOT IN THE PRELIMINARY ENDF/B-6 FORMAT. TURN OFF FLAG
C-----AND STOP TESTING FOR PRELIMINARY FORMAT.
   90 IMOLD=0
C-----REPLACE J VALUE BY STATISTICAL WEIGHT.
  100 RESJTAB(JR) = RESTAB(2,JR)
      RESTAB(2,JR)= GJ
C-----DEFINE SIGNED SQUARE ROOT OF 1/2 FISSION WIDTHS.
      GAMF1=DSQRT(DABS(0.5d0*RESTAB(5,JR)))
      IF(RESTAB(5,JR).LT.0.0d0) GAMF1=-GAMF1
      RESTAB(5,JR)=GAMF1
      GAMF2=DSQRT(DABS(0.5d0*RESTAB(6,JR)))
      IF(RESTAB(6,JR).LT.0.0d0) GAMF2=-GAMF2
      RESTAB(6,JR)=GAMF2
C-----IF FISSION WIDTHS ARE NOT ZERO TURN ON FISSILE FLAG.
      IF(GAMF1.NE.0.0d0.OR.GAMF2.NE.0.0d0) LFWX=1
      IF(GAMF1.NE.0.0d0.OR.GAMF2.NE.0.0d0) LFWL(NSECT) = LFWL(NSECT) + 1
      IF(GAMF1.NE.0.0d0.and.GAMF2.NE.0.0d0) IMBOTH = IMBOTH + 1
C-----DEFINE 1/2 NEUTRON WIDTH DIVIDED BY PENETRATION FACTOR.
C-----04/21/07 - IF NECESSARY DEFINE RHOX2
      ERABS=DABS(RESTAB(1,JR))
      IF(NRO.EQ.1.AND.NAPS.EQ.1) CALL SETRHO1(ERABS,NSECT)
      RHO2X=ERABS*RHOX2(NSECT)
      CALL FACTS3(LVALUE(NSECT),RHO2X,PENFAC)
C-----02/14/04 - PROTECT AGAINST ZERO ENERGY RESONANCES
      IF(PENFAC.NE.0.0d0) THEN
      RESTAB(3,JR)=0.5d0*RESTAB(3,JR)/PENFAC
      ELSE
      RESTAB(3,JR)=0.0d0
      ENDIF
C-----DEFINE 1/2 CAPTURE WIDTH.
      RESTAB(4,JR)=0.5d0*RESTAB(4,JR)
  110 CONTINUE
c-----------------------------------------------------------------------
c
c     2020/9/17 - Test (L, J) sequence spans resonance region
c
c-----------------------------------------------------------------------
      call WARNRRR(RESTAB(1,LHI))
c-----------------------------------------------------------------------
C
C     PRINT WARNING IF ANY J SEQUENCES ARE MISSING AND ADD MISSING J
C     SEQUENCE WITH NO RESONANCES IN ORDER TO ALLOW POTENTIAL SCATTERING
C     TO BE CORRECTLY CALCULATED.
C
c-----------------------------------------------------------------------
      LSECT = NSECT
      CALL MISSINGJ(LNOW,LSECT)
c-----------------------------------------------------------------------
c
c     2016/11/19 - Added Summary
c
c-----------------------------------------------------------------------
      iii = (NHIGH(NSECT) - NLOW(NSECT)) + 1
      write(OUTP,120) NSECT,LVALUE(NSECT),iii,LFWL(NSECT),IMBOTH
      write(   *,120) NSECT,LVALUE(NSECT),iii,LFWL(NSECT),IMBOTH
  120 FORMAT(1X,78('-')/' Summary of the Above Section'/1x,78('-')/
     1       ' Section Number.........--------------',I11/
     2       ' L Value................--------------',I11/
     3       ' Total Number of Resonances-----------',I11/
     4       ' Number With Non-ZERO Fission Width---',I11/
     5       ' Number With 2 Non-ZERO Fission Widths',I11)
c-----------------------------------------------------------------------
c
c     2016/11/19 - Added Summary
c
c-----------------------------------------------------------------------
C-----END OF L STATE LOOP.
  130 CONTINUE
C-----TERMINATE EXECUTION IF DATA IS IN THE PRELIMINARY ENDF/B-6
C-----FORMAT.
      IF(IMOLD.LE.0) GO TO 150
      WRITE(OUTP,140)
      WRITE(*   ,140)
  140 FORMAT(///' ERROR'/
     1 ' Data are in the Preliminary ENDF/B-6 Format, which is No'/
     2 ' Longer Used. Data MUST be Reformated to Comform to the'/
     3 ' Current ENDF/B-6 Reich-Moore Format Before it can be'/
     4 ' Processed by this Program. Execution Terminated.'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     STOP IF NEGATIVE WIDTHS
C
c-----------------------------------------------------------------------
  150 IF(NEGSUM.GT.0) THEN
      WRITE(   *,160) NEGSUM
      WRITE(OUTP,160) NEGSUM
  160 FORMAT(///
     1 ' ERROR - Execution Terminated because of',i8,
     2 ' Negative Widths.'///)
      CALL ENDERROR
      ENDIF
c-----------------------------------------------------------------------
C
C     STOP IF NO CAPTURE
C
c-----------------------------------------------------------------------
      IF(NOCAPSUM.GT.0) THEN
      WRITE(   *,170) NOCAPSUM
      WRITE(OUTP,170) NOCAPSUM
  170 FORMAT(///
     1 ' Execution Terminated because of No Capture in',i8,
     2 ' Resonances.'///)
      CALL ENDERROR
      ENDIF
      RETURN
  180 FORMAT(1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Scattering Radius--------------------',11A1/
     3       ' Number of L Values-------------------',I11)
  190 FORMAT(1X,78('=')/
     1       ' Atomic Weight Ratio of Isotope-------',11A1/
     2       ' L Dependent Scattering Radius--------',11A1/
     3       ' Angular Momentum (L)-----------------',I11/
     4       ' Number of Resonances-----------------',I11)
  200 FORMAT(1X,7('WARNING...'),'WARNING'/
     1 ' L Dependent Scattering Radius in the Evaluation is ZERO.'/
     2 ' Have Defined it to be Equal to the Scattering Radius',11A1/
     3 ' (see, ENDF/B-6 Formats and Procedures Manual, page 2.11)')
  210 FORMAT(1X,78('=')/' Reich-Moore Resonance Parameters'/1X,78('=')/
     1 '      Energy  J Value    Neutron    Capture  Fission-1',
     2 '  Fission-2'/8X,'(eV)',9X,4(7X,'(eV)')/1X,78('='))
  220 FORMAT(1X,11A1,F7.2,2X,44A1)
  230 FORMAT(1X,78('-'))
      END
      SUBROUTINE RDAA
C=======================================================================
C
C     READ ADLER-ADLER DATA FOR ONE ENERGY RANGE.
C     SINCE ADLER-ADLER PARAMETERS ARE INDEPENDENT OF L AND J THE
C     ENTIRE ENERGY RANGE WILL BE TREATED AS ONE SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*8 REACTR
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/RANGER/LOW,LHI
      COMMON/WHATZA/IZANOW,MATNOW,TEMP3,IVERSE,INT45
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
      COMMON/NAPRHO/NRO,NAPS
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION REACTR(3)
      DATA REACTR/
     1 'Total   ',
     2 'Fission ',
     3 'Capture '/
C-----UPDATED NOV. 12, 1998 AS PER CSEWG SUBCOMMITTEE RECOMMENDATION
      DATA C6/2.196807122623D-03/
C-----DEFINE TARGET SPIN, SPIN UP SCATTERING RADIUS AND NUMBER OF L
C-----STATES.
      CALL CARDIO(SPI,AP,L1,L2,NLS,N2)
      CALL OUT9(SPI,FIELD(1,1))
      CALL OUT9(AP ,FIELD(1,2))
      WRITE(OUTP,120) ((FIELD(M,I),M=1,11),I=1,2),NLS
      WRITE(*   ,120) ((FIELD(M,I),M=1,11),I=1,2),NLS
C-----DEFINE ATOMIC WEIGHT RATIO, TYPE AND NUMBER OF BACKGROUND
C-----PARAMETERS.
      CALL CARDIO(AWRI,XN,LI,L2,NRS6X,NRS)
      CALL OUT9(AWRI,FIELD(1,1))
      WRITE(OUTP,130) (FIELD(M,1),M=1,11),LI,NRS
      WRITE(*   ,130) (FIELD(M,1),M=1,11),LI,NRS
C-----INCREMENT SECTION COUNT AND INDICES TO RESONANCE PARAMETER TABLE
C-----(RESONANCE TABLE INDICES WILL ONLY REFER TO BACKGROUND).
      CALL LIMIT1(1)
C-----DEFINE ALL PARAMETERS FOR A SECTION.
      AK1=C6*AWRI/(AWRI+ONE)
      BETA(NSECT)=PI*ABN/(AK1*AK1)
      RHOP1(NSECT)=TWO*AP*AK1
      EL(NSECT)=ELX
      EH(NSECT)=EHX
      NLOW(NSECT)=LOW
C-----MODE DEPENDS ON VERSION OF ENDF/B FORMAT.
      MODE(NSECT)=LRF
c-----------------------------------------------------------------------
C
C     06/09/05 - THIS WAS ALREADY DONE FOR BREIT-WIGNER - NOW ALL.
C
C     IF ENERGY DEPENDENT SCATTERING RADIUS DEFINE INDEX TO TABULATED
C     DATA (OTHERWISE INDEX HAS ALREADY BEEN INITIALIZED TO 0).
C     CALCULATE ENERGY DEPENDENT RHOP FROM SCATTERING RADII.
C
c-----------------------------------------------------------------------
      IF(NRO.GT.0) THEN
      NRHO(NSECT)=NUMRHO
      INX3=INXRHO(3,NUMRHO)
      INX4=INXRHO(4,NUMRHO)
      DO I=INX3,INX4
      RHOTAB(I)=APTAB(I)*AK1
      ENDDO
      ENDIF
      IF(IVERSE.LT.5) MODE(NSECT)=8
      LVALUE(NSECT)=LI
c----- 02/08/08 - ADDED NAPS DEFINITION
      NAPTAB(NSECT)=NAPS
c-----------------------------------------------------------------------
C
C     READ BACKGROUND CROSS SECTIONS.
C
C     LI = 1 = TOTAL
C        = 2 = FISSION
C        = 3 = TOTAL + FISSION
C        = 4 = CAPTURE
C        = 5 = TOTAL + CAPTURE
C        = 6 = FISSION + CAPTURE
C        = 7 = ALL 3
C
C     FOR CONVENIENCE ALWAYS ALLOW ROOM FOR 3 SETS OF BACKGROUND
C     CROSS SECTIONS AND POSITION BACKGROUND CROSS SECTIONS TO
C     STANDARD POSITIONS (TOTAL, FISSION AND CAPTURE, IN THAT ORDER).
C
c-----------------------------------------------------------------------
C-----INIIALIZE ALL OFF.
      LHI=LOW+2
      DO I=LOW,LHI
      DO K=1,6
      RESTAB(K,I) = 0.0d0
      ENDDO
      ENDDO
      IF(NRS.LE.0) GO TO 80
      GO TO (10,20,30,40,50,60,70),LI
   10 CALL LISTIO(RESTAB(1,LOW  ),6) ! TOTAL
      GO TO 80
   20 CALL LISTIO(RESTAB(1,LOW+1),6) ! FISSION
      GO TO 80
   30 CALL LISTIO(RESTAB(1,LOW  ),6) ! TOTAL
      CALL LISTIO(RESTAB(1,LOW+1),6) ! FISSION
      GO TO 80
   40 CALL LISTIO(RESTAB(1,LOW+2),6) ! CAPTURE
      GO TO 80
   50 CALL LISTIO(RESTAB(1,LOW  ),6) ! TOTAL
      CALL LISTIO(RESTAB(1,LOW+2),6) ! CAPTURE
      GO TO 80
   60 CALL LISTIO(RESTAB(1,LOW+1),6) ! FISSION
      CALL LISTIO(RESTAB(1,LOW+2),6) ! CAPTURE
      GO TO 80
C-----TOTAL, FISSION AND CAPTURE. NOTHING TO Do.
   70 CALL LISTIO(RESTAB(1,LOW  ),6) ! TOTAL
      CALL LISTIO(RESTAB(1,LOW+1),6) ! FISSION
      CALL LISTIO(RESTAB(1,LOW+2),6) ! CAPTURE
C-----SET FISSION FLAG IF ANY FISSION BACKGROUND
   80 DO I=1,6
      IF(DABS(RESTAB(I,LOW+1)).NE.0.0d0) LFWX=1
      IF(DABS(RESTAB(I,LOW+1)).NE.0.0d0) LFWL(NSECT) = LFWL(NSECT) + 1
      ENDDO
c-----------------------------------------------------------------------
C
C     LIST BACKGROUND CROSS SECTIONS IN STANDARD POSITION.
C
c-----------------------------------------------------------------------
      IF(IMEDIT.LE.0) GO TO 90
      WRITE(OUTP,140)
C-----TOTAL
      DO I=1,6
      CALL OUT9(RESTAB(I,LOW  ),FIELD(1,I))
      ENDDO
      WRITE(OUTP,150) REACTR(1),((FIELD(M,I),M=1,11),I=1,6)
C-----FISSION
      DO I=1,6
      CALL OUT9(RESTAB(I,LOW+1),FIELD(1,I))
      ENDDO
      WRITE(OUTP,150) REACTR(2),((FIELD(M,I),M=1,11),I=1,6)
C-----CAPTURE
      DO I=1,6
      CALL OUT9(RESTAB(I,LOW+2),FIELD(1,I))
      ENDDO
      WRITE(OUTP,150) REACTR(3),((FIELD(M,I),M=1,11),I=1,6)
c-----------------------------------------------------------------------
C
C     L STATE LOOP.
C
c-----------------------------------------------------------------------
   90 DO 110 IL=1,NLS
C-----READ L VALUE AND NUMBER OF J STATES.
      CALL CARDIO(XN,XN,LNOW,L2,NJS,N2)
      WRITE(OUTP,190) LNOW,NJS
c-----------------------------------------------------------------------
C
C     J STATE LOOP.
C
c-----------------------------------------------------------------------
      DO IJ=1,NJS
C-----READ J VALUE AND NUMBER OF RESONANCES
      CALL CARDIO(AJ,C2,L1,L2,NRS12,NRS)
      WRITE(OUTP,200) AJ,NRS
C-----INCREMENT INDICES TO RESONANCE PARAMETER TABLE.
      CALL LIMIT1(2)
C-----READ RESONANCE PARAMETERS - 12 PARAMETERS PER RESONANCE.
      CALL LISPAR12(LOW,NRS12)
      IF(IMEDIT.NE.0) WRITE(OUTP,160)
      DO 100 JR=LOW,LHI
      IF(DABS(RESTAB(7,JR)).NE.0.0d0.OR.DABS(RESTAB(8,JR)).NE.0.0d0)
     1 LFWX = 1
      IF(DABS(RESTAB(7,JR)).NE.0.0d0.OR.DABS(RESTAB(8,JR)).NE.0.0d0)
     1 LFWL(NSECT) = 1
c-----------------------------------------------------------------------
C
C     LIST RESONANCE PARAMETERS - NOW STORED TO 12 WORDS PER RESONANCE
C
c-----------------------------------------------------------------------
      IF(IMEDIT.EQ.0) GO TO 100
C-----TOTAL
      DO I=1,4
      CALL OUT9(RESTAB(I  ,JR),FIELD(1,I))
      ENDDO
      WRITE(OUTP,170) REACTR(1),((FIELD(M,I),M=1,11),I=1,4)
C-----FISSION
      DO I=1,4
      CALL OUT9(RESTAB(I+4,JR),FIELD(1,I))
      ENDDO
      WRITE(OUTP,170) REACTR(2),((FIELD(M,I),M=1,11),I=1,4)
C-----CAPTURE
      DO I=1,4
      CALL OUT9(RESTAB(I+8,JR),FIELD(1,I))
      ENDDO
      WRITE(OUTP,170) REACTR(3),((FIELD(M,I),M=1,11),I=1,4)
      WRITE(OUTP,180)
  100 CONTINUE
C-----END OF J STATE LOOP.
      ENDDO
C-----END OF L STATE LOOP.
  110 CONTINUE
C-----DEFINE UPPER INDEX TO RESONANCE TABLE.
      NHIGH(NSECT)=LHI
      RETURN
  120 FORMAT(1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Effective Scattering Radius (A+)-----',11A1/
     3       ' Number of L Values-------------------',I11)
  130 FORMAT(1X,78('=')/
     1       ' Atomic Weight Ratio of Isotope-------',11A1/
     2       ' LI-----------------------------------',I11/
     3       ' Number of Background Sets------------',I11)
  140 FORMAT(1X,78('=')/' Adler-Adler Resonance Parameters'/1X,78('=')/
     1 ' Background'/1X,78('=')/' Reaction',
     1 '         A1         A2         A3         A4         B1',
     2 '         B2'/1X,78('='))
  150 FORMAT(1X,A8,66A1)
  160 FORMAT(1X,78('=')/' Resonance Parameters'/1X,78('=')/' Reaction',
     1  '  Resonance HALF-Width  Symmetric Asymmetric'/
     2  9X,'   Energy     (eV)      Parameter Parameter'/
     3  9X,'    (eV)                  (GRT)     (GIT)  '/1X,78('='))
  170 FORMAT(1X,A8,44A1)
  180 FORMAT(1X,78('='))
  190 FORMAT(1X,78('=')/
     1       ' L Value------------------------------',I11/
     2       ' Number of J Values-------------------',I11)
  200 FORMAT(' J Value------------------------------',5X,F6.2/
     3       ' Number of Resonances-----------------',I11)
      END
      SUBROUTINE RDHRF
C=======================================================================
C
C     READ HYBRID R-FUNCTION DATA FOR ONE ENERGY RANGE.
C     EACH (L,S,J) STATE WILL BE TREATED AS A SEPARATE SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*4 MTHOL,ANSWER
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/NAPRHO/NRO,NAPS
      COMMON/RANGER/LOW,LHI
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/HRFTAB/QHRF(4),MTHRF(4),NHRF
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION MTOK(9),MTHOL(3,10),ANSWER(3,2)
      DATA ANSWER/' (No',')   ',
     1 ' (Ye','s)  ',
     2 ' (ER','ROR)'/
C-----DEFINE ALLOWED INELASTIC AND CHARGED PARTICLE MT NUMBERS.
      DATA MTOK/51,52,53,54,103,104,105,106,107/
      DATA MTHOL/
     1 'Inel','asti','c   ',
     2 'Inel','asti','c   ',
     3 'Inel','asti','c   ',
     4 'Inel','asti','c   ',
     5 'n,p ','    ','    ',
     6 'n,d ','    ','    ',
     7 'n,t ','    ','    ',
     8 'n,He','-3  ','    ',
     9 'n,Al','pha ','    ','NOT ','Allo','wed '/
      DATA C3/1.23D-01/
      DATA C5/8.0d-2/
C-----UPDATED NOV. 12, 1998 AS PER CSEWG SUBCOMMITTEE RECOMMENDATION
c***** Updated 9/01/04 - ENDF-102, appendix H
      DATA C4/1.00866491578d0/
c***** Updated 9/01/04
      DATA C6/2.196807122623d-3/
C-----DEFINE TARGET SPIN, SPIN UP SCATTERING RADIUS AND NUMBER OF L
C-----STATES.
      CALL CARDIO(SPI,C2,L1,L2,NLS,N2)
      CALL OUT9(SPI,FIELD(1,1))
      WRITE(OUTP,230) (FIELD(M,1),M=1,11),NLS
      WRITE(*   ,230) (FIELD(M,1),M=1,11),NLS
C-----DEFINE NUMBER OF EACH KIND OF REACTION.
      CALL CARDIO(C1,C2,NGRE,NFRE,NIRE,NCRE)
      WRITE(OUTP,240) NGRE,NFRE,NIRE,NCRE
      WRITE(*   ,240) NGRE,NFRE,NIRE,NCRE
C-----CHECK FOR ALLOWABLE VALUES.
      IF(NGRE.LT.0.OR.NGRE.GT.1) GO TO 10
      IF(NFRE.LT.0.OR.NFRE.GT.1) GO TO 10
      IF(NIRE.LT.0.OR.NIRE.GT.4) GO TO 10
      IF(NCRE.LT.0.OR.NCRE.GT.4) GO TO 10
      NHRF=NIRE+NCRE
      IF(NHRF.LE.4) GO TO 30
   10 WRITE(OUTP,20) NHRF
      WRITE(*   ,20) NHRF
   20 FORMAT(' ERROR - Illegal Number of Reactions',I3,' (Must be 4)'/
     1       '         Execution Terminated.'///)
      CALL ENDERROR
C-----DEFINE KIND OF REACTION.
   30 CALL CARDIO(C1,C2,MTHRF(1),MTHRF(2),MTHRF(3),MTHRF(4))
C-----READ Q-VALUES.
      CALL CARDIO(C1,C2,L1,L2,N1,N2)
      CALL LISTIO(QHRF,N1)
C-----LIST MT NUMBERS AND Q-VALUES.
      IF(NHRF.LE.0) GO TO 100
      IF(IMEDIT.NE.0) WRITE(OUTP,250)
C-----CHECK AND LIST MT NUMBERS AND Q-VALUES.
      IERR=0
      DO 60 I=1,NHRF
      DO 40 J=1,9
      IF(MTHRF(I).EQ.MTOK(J)) GO TO 50
   40 CONTINUE
      J=10
      IERR=1
C-----TEMPORARILY ONLY ALLOW INELASTIC REACTIONS.
   50 IF(J.GT.4.AND.J.LE.9) IERR=2
      CALL OUT9(QHRF(I),FIELD(1,1))
      IF(IMEDIT.NE.0) WRITE(OUTP,260) MTHRF(I),
     1 (FIELD(M,1),M=1,11),(MTHOL(K,J),K=1,3)
C-----FROM HERE ON TREAT ALL INELASTIC AS THE SAME MT NUMBER (MT=51).
      IF(MTHRF(I).GE.51.AND.MTHRF(I).LE.54) MTHRF(I)=51
   60 CONTINUE
c-----------------------------------------------------------------------
C
C     TERMINATED EXECUTION IF REACTION IS NOT ALLOWED.
C
c-----------------------------------------------------------------------
      IF(IERR.EQ.0) GO TO 100
      IF(IERR.EQ.2) GO TO 80
C-----ILLEGAL MT NUMBER.
      WRITE(OUTP,70)
      WRITE(*   ,70)
   70 FORMAT(///' ERROR - Illegal MT Number. Execution Terminated.'///)
      CALL ENDERROR
C-----CHARGED PARTICLE REACTION (TEMPORARILY NOT ALLOWED)
   80 WRITE(OUTP,90)
      WRITE(*   ,90)
   90 FORMAT(///' ERROR -',
     1 ' Currently Program Does NOT Allow Charged Particle Reactions.'/
     2 ' Execution Terminated.'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     L STATE LOOP.
C
c-----------------------------------------------------------------------
  100 DO 220 IL=1,NLS
C-----DEFINE ATOMIC WEIGHT RATIO, L VALUE AND NUMBER OF S-VALUES
C-----(CHANNEL SPINS).
      CALL CARDIO(AWRI,C1,LNOW,L2,NSS,N2)
C-----DEFINE ATOMIC WEIGHT DEPENDENT QUANTITIES.
      AWRICM=AWRI/(AWRI+ONE)
C-----INITIALIZE CHANNEL RADIUS TO CALCULATED VALUE (MAY BE CHANGED
C-----LATER AFTER SCATTERING RADIUS IS READ).
      APX=C3*((C4*AWRI)**(ONE/THREE))+C5
      AK1=C6*AWRICM
      AK2=AK1*AK1
      CALL OUT9(AWRI,FIELD(1,1))
      WRITE(OUTP,270) (FIELD(M,1),M=1,11),LNOW,NSS
c-----------------------------------------------------------------------
C
C     CONSTRUCT TABLE OF PERMISSIBLE J-VALUES
C     (THIS TABLE WILL BE USED TO CHECK FOR MISSING OR ILLEGAL J-VALUES)
C
c-----------------------------------------------------------------------
      CALL SETJ(LNOW)
c-----------------------------------------------------------------------
C
C     S-VALUE LOOP.
C
c-----------------------------------------------------------------------
      DO 210 IS=1,NSS
C-----DEFINE S-VALUE AND NUMBER OF J-VALUES.
      CALL CARDIO(AS,C2,L1,L2,NJS,N2)
      CALL OUT9(AS,FIELD(1,1))
      WRITE(OUTP,280) (FIELD(M,1),M=1,11),NJS
c-----------------------------------------------------------------------
C
C     J STATE LOOP.
C
c-----------------------------------------------------------------------
      DO IJ=1,NJS
C-----DEFINE J-VALUE, CHANNEL RADIUS, FLAGS FOR BACKGROUND AND OPTICAL
C-----MODEL PHASE SHIFTS AND THE NUMBER OF RESONANCES.
      CALL CARDIO(AJ,AC,LBK,LPS,NLSJ12,NLSJ)
C-----CHECK FOR LEGAL J-VALUE AND DEFINE STATISTICAL WEIGHT.
      CALL CHECKJ(AJ,LNOW,GJ,2)
C-----IF REQUESTED SET CHANNEL RADIUS EQUAL TO SCATTERING RADIUS.
      IF(NAPS.NE.0) APX=AC
      LBKIN=LBK+1
      IF(LBK.LT.0.OR.LBK.GT.1) LBKIN=3
      LPSIN=LPS+1
      IF(LPS.LT.0.OR.LPS.GT.1) LPSIN=3
      CALL OUT9(AJ,FIELD(1,1))
      CALL OUT9(AC,FIELD(1,2))
      WRITE(OUTP,290) ((FIELD(M,I),M=1,11),I=1,2),LBK,
     1 ANSWER(1,LBKIN),ANSWER(2,LBKIN),LPS,ANSWER(1,LPSIN),
     2 ANSWER(2,LPSIN),NLSJ
C-----CHECK FOR ILLEGAL BACKGROUND OR OPTICAL MODEL PHASE SHIFT FLAG.
      IF(LBKIN.NE.2.AND.LPSIN.NE.2) GO TO 120
      WRITE(OUTP,110)
      WRITE(*   ,110)
  110 FORMAT(///' ERROR -',
     1 ' Background and Optical Phase Shift Flags MUST be 0 or 1.'/
     2 ' Execution Terminated.'///)
      CALL ENDERROR
C-----TEMPORARILY DO NOT ALLOW TABULATED BACKGROUND OR OPTICAL MODEL
C-----PHASE SHIFT.
  120 IF(LBK.EQ.0.AND.LPS.EQ.0) GO TO 140
      WRITE(OUTP,130)
c-----2017/9/6 - Added missing format # 130
      WRITE(*   ,130)
  130 FORMAT(///' ERROR -',
     1 ' Currently Program Does NOT Allow Tabulated Background or'/
     2 ' Optical Phase Shift. Execution Terminated.'///)
      CALL ENDERROR
C-----ALLOW SPACE FOR 12 PARAMETERS PER RESONANCE.
  140 NRS=  NLSJ
C-----INCREMENT SECTION COUNT AND INDICES TO RESONANCE PARAMETER TABLE.
      CALL LIMIT1(1)
C-----DEFINE ALL PARAMETERS FOR A SECTION.
      BETA(NSECT)=PI*ABN/AK2
      RHOX2(NSECT)=APX*APX*AK2
      RHOP1(NSECT)=AC*AK1
      EL(NSECT)=ELX
      EH(NSECT)=EHX
      GJTAB(NSECT)=GJ
      NLOW(NSECT)=LOW
      NHIGH(NSECT)=LHI
      LVALUE(NSECT)=LNOW
      LRXTAB(NSECT)=NHRF
c----- 02/08/08 - ADDED NAPS DEFINITION
      NAPTAB(NSECT)=NAPS
      MODE(NSECT)=LRF
c-----------------------------------------------------------------------
C
C     06/09/05 - THIS WAS ALREADY DONE FOR BREIT-WIGNER - NOW ALL.
C
C     IF ENERGY DEPENDENT SCATTERING RADIUS DEFINE INDEX TO TABULATED
C     DATA (OTHERWISE INDEX HAS ALREADY BEEN INITIALIZED TO 0).
C     CALCULATE ENERGY DEPENDENT RHOP FROM SCATTERING RADII.
C
c-----------------------------------------------------------------------
      IF(NRO.GT.0) THEN
      NRHO(NSECT)=NUMRHO
      INX3=INXRHO(3,NUMRHO)
      INX4=INXRHO(4,NUMRHO)
      DO I=INX3,INX4
      RHOTAB(I)=APTAB(I)*AK1
      ENDDO
      ENDIF
C-----DEFINE EXCITATION ENERGIES.
      IF(NHRF.LE.0) GO TO 160
      DO 150 I=1,NHRF
      EXCITE(I,NSECT)=QHRF(I)/AWRICM
  150 CONTINUE
c-----------------------------------------------------------------------
C
C     READ RESONANCE PARAMETERS (12 PER RESONANCE).
C
C     (1) ENERGY
C     (2) NEUTRON WIDTH
C     (3) CAPTURE WIDTH
C     (4) FISSION WIDTH
C     (5) FIRST COMPETITIVE WIDTH
C     (6) SECOND COMPETITIVE WIDTH
C     (7) THIRD COMPETITIVE WIDTH
C     (8) FOURTH COMPETITIVE WIDTH
C     (9) FIRST EXIT CHANNEL L-VALUE
C    (10) SECOND EXIT CHANNEL L-VALUE
C    (11) THIRD EXIT CHANNEL L-VALUE
C    (12) FOURTH EXIT CHANNEL L-VALUE
C
C     SOME OF THESE PARAMETERS WILL BE CONVERTED TO THE FORM REQUIRED
C     FOR CALCULATIONS.
C
C     (2) NEUTRON WIDTH DIVIDED BY PENETRATION FACTOR
C   (5-8) INELASTIC WIDTH DIVIDED BY PENETRATION FACTOR
C
c-----------------------------------------------------------------------
  160 CALL LISPAR12(LOW,NLSJ12)
      IF(IMEDIT.NE.0) WRITE(OUTP,300)
      DO 200 JR=LOW,LHI
C-----LIST RESONANCE PARAMETERS.
      IF(IMEDIT.EQ.0) GO TO 180
      LOUT1=RESTAB( 9,JR)
      LOUT2=RESTAB(10,JR)
      LOUT3=RESTAB(11,JR)
      LOUT4=RESTAB(12,JR)
      DO 170 I=1,6
      CALL OUT9(RESTAB(I,JR  ),FIELD(1,I))
  170 CONTINUE
      CALL OUT9(RESTAB(7,JR  ),FIELD(1,7))
      CALL OUT9(RESTAB(8,JR  ),FIELD(1,8))
      WRITE(OUTP,310) ((FIELD(M,I),M=1,11),I=1,8),LOUT1,LOUT2,
     1 LOUT3,LOUT4
C-----04/21/07 - IF NECESSARY DEFINE RHOX2
  180 ERABS=DABS(RESTAB(1,JR))
      IF(NRO.EQ.1.AND.NAPS.EQ.1) CALL SETRHO1(ERABS,NSECT)
      RHO2X=ERABS*RHOX2(NSECT)
C-----DEFINE NEUTRON WIDTH DIVIDED BY PENETRATION FACTOR.
      CALL FACTS2(LVALUE(NSECT),RHO2X,SHIFT2(JR),PENFAC)
C-----02/14/04 - PROTECT AGAINST ZERO ENERGY RESONANCES
      IF(PENFAC.NE.0.0d0) THEN
      RESTAB(2,JR)=RESTAB(2,JR)/PENFAC
      ELSE
      RESTAB(2,JR)=0.0d0
      ENDIF
C-----DEFINE INELASTIC WIDTHS DIVIDED BY PENETRATION FACTOR.
      IF(NHRF.LE.0) GO TO 200
c----- 5 -  9 = WIDTHS
C----- 9 - 12 = L VALUES
C-----INITIALIZE INDICES TO WIDTH AND EXIT CHANNEL L-VALUE.
      JJ1=4
      JJ2=8
      DO 190 IHRF=1,NHRF
      JJ1=JJ1+1
      JJ2=JJ2+1
      IF(MTHRF(IHRF).NE.51) GO TO 190
      RHOZ2=DABS(RESTAB(1,JR)+EXCITE(IHRF,NSECT))*RHOX2(NSECT)
      LCOM=RESTAB(JJ2,JR)
c-----10/10/10 - switched from FACTS2 to FACTS3 - shift not used
      CALL FACTS3(LCOM,RHOZ2,PENFAC)
C-----02/14/04 - PROTECT AGAINST ZERO ENERGY RESONANCES
      IF(PENFAC.NE.0.0d0) THEN
      RESTAB(JJ1,JR)=RESTAB(JJ1,JR)/PENFAC
      ELSE
      RESTAB(JJ1,JR)=0.0d0
      ENDIF
  190 CONTINUE
C-----END OF RESONANCE LOOP.
  200 CONTINUE
C-----END OF J LOOP.
      ENDDO
C-----END OF S LOOP.
  210 CONTINUE
c-----------------------------------------------------------------------
C
C     PRINT WARNING IF ANY J SEQUENCES ARE MISSING AND ADD MISSING J
C     SEQUENCE WITH NO RESONANCES IN ORDER TO ALLOW POTENTIAL
C     SCATTERING TO BE CORRECTLY CALCULATED.
C
c-----------------------------------------------------------------------
      LSECT = NSECT
      CALL MISSINGJ(LNOW,LSECT)
C-----END OF L LOOP.
  220 CONTINUE
      RETURN
  230 FORMAT(1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Number of L Values-------------------',I11)
  240 FORMAT(1X,78('=')/
     1       ' Number of Capture Reactions----------',I11,' (0 TO 1)'/
     2       ' Number of Fission Reactions----------',I11,' (0 TO 1)'/
     3       ' Number of Inelastic Reactions--------',I11,' (0 TO 4)'/
     3       ' Number of Charged Particle Reactions-',I11,' (0 TO 4)')
  250 FORMAT(1X,78('='))
  260 FORMAT(' MT and Q-Value (eV)------------------',I11,11A1,1X,3A4)
  270 FORMAT(1X,78('=')/
     1       ' Atomic Weight Ratio of Isotope-------',11A1/
     2       ' L-Value------------------------------',I11/
     3       ' Number of S-Values (Channel Spin)----',I11)
  280 FORMAT(1X,78('=')/
     1       ' S-Value------------------------------',11A1/
     2       ' Number of J-Values-------------------',I11)
  290 FORMAT(1X,78('=')/
     1       ' J-Value------------------------------',11A1/
     2       ' Channel Radius (10E-12 cm)-----------',11A1/
     3       ' Background Tabulated-----------------',I11,2A4/
     4       ' Optical Model Phase SshiftTabulated--',I11,2A4/
     5       ' Number of Resonances-----------------',I11)
  300 FORMAT(1X,112('=')/' Hybrid R-Function Resonance Parameters'/
     1 1X,112('=')/
     2 '                                              Competitive',
     3 ' Widths                          Exit Channel'/
     2 '      Energy    Neutron    Capture    Fission Reaction-1',
     4 ' Reaction-2 Reaction-3 Reaction-4 L-Values'/
     6 '        (eV)       (eV)       (eV)       (eV)       (eV)',
     7 '       (eV)       (eV)       (eV) L-1 L-2 L-3 L-4'/
     8 1X,112('='))
  310 FORMAT(1X,88A1,4I4)
      END
      SUBROUTINE RDGRM
C=======================================================================
C
C     GENERAL R-MATRIX TREATMENT NO LONGER ALLOWED IN ENDF.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      WRITE(OUTP,10)
      WRITE(*   ,10)
      CALL ENDERROR
   10 FORMAT(///' ERROR -',
     1 ' General R-Matrix Parameters are NOT Yet Treated by this'/
     2 ' Program....Execution Terminated.'/1X,78('=')/
     3 ' In Order to Demonstrate that this Formalism is Generally'/
     4 ' Useful the Author of this Code Asks that Evaluators Send'/
     5 ' Him Evaluations Using this Formalism and Their Calculated'/
     6 ' Cross Sections for Comparison.'/1X,78('=')/
     7 ' Until Parameters and Calculated Cross Sections Using this'/
     8 ' Formalism are Available it is NOT Possible to Verify the'/
     9 ' Accuracy of Any Results Calculated by this Code.'/1X,78('='))
      END
      SUBROUTINE RDUR
C=======================================================================
C
C     READ UNRESOLVED RESONANCE PARAMETERS. THIS ROUTINE HANDLES ALL
C     POSSIBLE REPRESENTATIONS OF UNRESOLVED PARAMETERS,
C     (1) NO FISSION WIDTHS, ALL PARAMETERS ENERGY INDEPENDENT
C     (2) FISSION WIDTHS GIVEN, ONLY FISSION WIDTHS ENERGY DEPENDENT
C     (3) FISSION WIDTHS GIVEN, ALL PARAMETERS ENERGY DEPENDENT
C
C     FOR SIMPLICITY IN LATER CALCULATIONS ALL INPUT REPRESENTATIONS
C     ARE INTERNALLY CONVERTED TO THE ALL PARAMETERS ENERGY DEPENDENT
C     FORM TREATING EACH (L, J) STATE AS A SEPARATE SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      CHARACTER*4 TYPINT
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/NAPRHO/NRO,NAPS
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/RANGER/LOW,LHI
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/WHATZA/IZANOW,MATNOW,TEMP3,IVERSE,INT45
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
      COMMON/LRUNOW/LRUIN
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION DUMSET(6),DUMBO(6),TYPINT(4,6)
      EQUIVALENCE (DUMSET(1),DX),(DUMSET(2),AJ),(DUMSET(3),AMUN),
     1 (DUMSET(4),GNO),(DUMSET(5),GG)
      DATA TYPINT/
     1 '(His','togr','am) ','    ',
     2 '(Lin',' X-L','in Y',')   ',
     3 '(Log',' X-L','in Y',')   ',
     4 '(Lin',' X-L','og Y',')   ',
     5 '(Log',' X-L','og Y',')   ',
     6 '(ERR','OR) ','    ','    '/
      DATA DUMSET/6*0.0d0/
      DATA DUMBO /6*0.0d0/
      DATA C3/1.23d-1/
      DATA C5/8.0d-2/
C-----UPDATED NOV. 12, 1998 AS PER CSEWG SUBCOMMITTEE RECOMMENDATION
c***** Updated 9/01/04 - ENDF-102, appendix H
      DATA C4/1.00866491578d0/
c***** Updated 9/01/04
      DATA C6/2.196807122623d-3/
C-----INITIALIZE NEGATIVE WIDTH COUNT
      NEGSUM = 0
C-----PRINT TITLE FOR OUTPUT LISTING.
      IF(LRF.EQ.1.AND.LFW.EQ.0) WRITE(OUTP,260)
      IF(LRF.EQ.1.AND.LFW.EQ.1) WRITE(OUTP,270)
      IF(LRF.EQ.2) WRITE(OUTP,280)
      IF(LRF.EQ.1.AND.LFW.EQ.0) WRITE(*   ,260)
      IF(LRF.EQ.1.AND.LFW.EQ.1) WRITE(*   ,270)
      IF(LRF.EQ.2) WRITE(*   ,280)
c-----------------------------------------------------------------------
C
C     READ BEGINNING OF EACH TYPE OF REPRESENTATION AND DEFINE ALL
C     COMMON TERMS.
C
C     DEFINE TARGET SPIN, SPIN UP SCATTERING RADIUS AND,
C     (LRF=1, LFW=1) - NUMBERS OF ENERGIES AT WHICH FISSION WIDTHS ARE
C                      GIVEN AND NUMBER OF L VALUES, OR
C     (OTHERWISE)    - NUMBER OF L VALUES.
C
C     FOR ENDF/B-6 FORMATS CHECK L1 FIELD (LSSF) TO SEE IF THE
C     UNRESOLVED INFINITELY DILUTE CROSS SECTION HAS ALREADY BEEN
C     INCLUDED IN THE FILE 3 CROSS SECTIONS. IF IT HAS CHANGE LRUIN
C     TO EFFECTIVELY IGNORE SECTION WHILE CALCULATING RESONANCE
C     CONTRIBUTION TO THE FILE 3 CROSS SECTIONS.
C
c-----------------------------------------------------------------------
      CALL CARDI(SPI,AP,LSSF,L2,NE,NLS)
      CALL CARDO(SPI,AP,LSSF,L2,NE,NLS)
      IF(LFW.NE.1.OR.LRF.NE.1) NLS=NE
C-----LIST SPIN AND SPIN UP SCATTERING.
      CALL OUT9(SPI,FIELD(1,1))
      CALL OUT9(AP ,FIELD(1,2))
      WRITE(OUTP,320) ((FIELD(M,I),M=1,11),I=1,2)
      WRITE(*   ,320) ((FIELD(M,I),M=1,11),I=1,2)
C-----FOR ENDF/B-6 PRINT INTERPRETATION OF LSSF.
      IF(IVERSE.NE.6) GO TO 10
      if(LSSF.eq.0) then
      WRITE(OUTP,300) LSSF
      WRITE(*   ,300) LSSF
      else
      WRITE(OUTP,310) LSSF
      WRITE(*   ,310) LSSF
      endif
C-----LIST NUMBER OF L VALUES.
   10 WRITE(OUTP,330) NLS
      WRITE(*   ,330) NLS
c-----------------------------------------------------------------------
C
C     FOR ENDF/B-6 IF LRUIN INDICATES THAT CROSS SECTIONS NOT YET
C     ADDED TO FILE 3, BUT LSSF INDICATES THAT THEY HAVE PRINT
C     WARNING AND CHANGE LRUIN TO IGNORE THIS SECTION.
C
c-----------------------------------------------------------------------
      IF(IVERSE.NE.6.OR.LRUIN.EQ.5.OR.LSSF.EQ.0) GO TO 20
      WRITE(OUTP,290) LSSF
      WRITE(*   ,290) LSSF
      LRUIN=5
c-----------------------------------------------------------------------
C
C     IF ONLY FISSION WIDTH ARE ENERGY DEPENDENT READ ENERGIES AT WHICH
C     FISSION WIDTHS ARE GIVEN.
C
c-----------------------------------------------------------------------
   20 IF(LRF.NE.1.OR.LFW.NE.1) GO TO 40
C-----INCREMENT SECTION COUNT (ALLOWING ROOM TO INSERT THE DEGREES OF
C-----FREEDOM BEFORE THE ENERGIES).
      NRS=NE+1
      CALL LIMIT1(1)
      LOWP1=LOW+1
C-----READ ENERGIES AT WHICH FISSION WIDTHS ARE GIVEN AND MOVE THEM
C-----INTO THE 1-ST POSITION FOR EACH RESONANCE.
      JRC=(LHI-LOWP1)+1
      CALL LISTIO(ENRES(LOWP1),JRC)
      DO 30 JR1=LOWP1,LHI
      CALL NOODLE(ENRES(JR1),ZERO,ELX,EHX)
      RESTAB(1,JR1)=ENRES(JR1)
   30 CONTINUE
C-----SAVE INDEX TO FIRST ENERGY TO ALLOW COPY OF ENERGIES FOR EACH
C-----SECTION.
      LOWP1X=LOWP1
c-----------------------------------------------------------------------
C
C     L STATE LOOP.
C
c-----------------------------------------------------------------------
   40 DO 240 IL=1,NLS
C-----DEFINE ATOMIC WEIGHT RATIO, L VALUE AND NUMBER OF J STATES.
      CALL CARDIO(AWRI,C2,LNOW,L2,NJS,NJSX)
      IF(LRF.EQ.1.AND.LFW.EQ.0) NJS=NJSX
      CALL OUT9(AWRI,FIELD(1,1))
      WRITE(OUTP,340) (FIELD(M,1),M=1,11),LNOW,NJS
      WRITE(*   ,340) (FIELD(M,1),M=1,11),LNOW,NJS
C-----DEFINE COMMON PARAMETERS FOR ALL SECTIONS WITH SAME L VALUE.
C-----EITHER CALCULATE CHANNEL RADIUS OR DEFINE TO BE EQUAL TO THE
C-----SCATTERING RADIUS.
      IF(NAPS.NE.0) GO TO 50
      APX=C3*((C4*AWRI)**(ONE/THREE))+C5
      GO TO 60
   50 APX=AP
   60 AK1=C6*AWRI/(AWRI+ONE)
      BETAD=PI*ABN/(AK1*AK1)
      RHOX2D=(APX*AK1)**2
      RHOP1D=AP*AK1
c-----------------------------------------------------------------------
C
C     J STATE LOOP.
C
c-----------------------------------------------------------------------
      DO IJ=1,NJS
C-----READ NEXT LINE FOR (LRF=1, LFW=1 OR LRF=2, ANY LFW...NOTHING TO
C-----READ FOR LRF=1, LFW=0).
      IF(LRF.NE.2) GO TO 70
C-----DEFINE J VALUE, INTERPOLATION LAW AND NUMBER OF ENERGIES.
      CALL CARDIO(AJ,C2,INTX,L2,NET6P6,NE)
C-----DEFINE THE NUMBER OF RESONANCE LOCATIONS WHICH WILL BE USED
C-----ALLOWING FOR THE PRECEDING DEGREES OF FREEDOM.
      NRS=NE+1
      GO TO 90
   70 IF(LFW.EQ.0) GO TO 80
C-----DEFINE THE NUMBER OF DEGREES OF FREEDOM FOR FISSION (ASSUME THE
C-----SAME NUMBER OF ENERGIES, I.E. NEED NOT RE-DEFINE NE OR NRS AT
C-----THIS POINT).
      CALL CARDIO(C1,C2,L1,MUF,NEP6,N2)
      IF(IL.EQ.1.AND.IJ.EQ.1) GO TO 100
      GO TO 90
C-----INDICATE 3 LOCATIONS PER (L, J) STATE (FIRST LOCATION CONTAINS
C-----PARAMETERS AND DEGREES OF FREEDOM, THE SECOND AND THIRD CONTAIN
C-----ENERGY POINTS AT THE LOWER AND UPPER ENERGY LIMITS OF UNRESOLVED
C-----REGION.
   80 NRS=3
C-----INCREMENT SECTION COUNT AND INDICES TO RESONANCE PARAMETER TABLE
   90 CALL LIMIT1(1)
  100 LOWP1=LOW+1
C-----DEFINE ALL PARAMETERS FOR A SECTION.
      BETA(NSECT)=BETAD
      RHOX2(NSECT)=RHOX2D
      RHOP1(NSECT)=RHOP1D
      EL(NSECT)=ELX
      EH(NSECT)=EHX
      NLOW(NSECT)=LOW
      NHIGH(NSECT)=LHI
      LVALUE(NSECT)=LNOW
c----- 02/08/08 - ADDED NAPS DEFINITION
      NAPTAB(NSECT)=NAPS
      MODE(NSECT)=11
c-----------------------------------------------------------------------
C
C     06/09/05 - THIS WAS ALREADY DONE FOR BREIT-WIGNER - NOW ALL.
C
C     IF ENERGY DEPENDENT SCATTERING RADIUS DEFINE INDEX TO TABULATED
C     DATA (OTHERWISE INDEX HAS ALREADY BEEN INITIALIZED TO 0).
C     CALCULATE ENERGY DEPENDENT RHOP FROM SCATTERING RADII.
C
c-----------------------------------------------------------------------
      IF(NRO.GT.0) THEN
      NRHO(NSECT)=NUMRHO
      INX3=INXRHO(3,NUMRHO)
      INX4=INXRHO(4,NUMRHO)
      DO I=INX3,INX4
      RHOTAB(I)=APTAB(I)*AK1
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
C
C     SELECT PARAMETER REPRESENTATION.
C
c-----------------------------------------------------------------------
C-----ARE ALL WIDTHS ENERGY DEPENDENT.
      IF(LRF.EQ.2) GO TO 160
C-----ARE FISSION WIDTHS GIVEN.
      IF(LFW.NE.0) GO TO 120
c-----------------------------------------------------------------------
C
C     ALL PARAMETERS ARE ENERGY INDEPENDENT. CONVERT TO ENERGY DEPENDENT
C     FORM USING INTERPOLATION LAW 1 (HISTOGRAM).
C
c-----------------------------------------------------------------------
C-----READ LEVEL SPACING, J, NEUTRON DEGREES OF FREEDOM, NEUTRON AND
C-----CAPTURE WIDTHS (SEE, ABOVE EQUIVALENCE TO MEMBERS OF DUMSET).
      CALL LISTIO(DUMSET,6)
C-----DEFINE INTERPOLATION LAW TO BE HISTOGRAM.
      INTX=1
C-----DEFINE NUMBER OF DEGREES OF FREEDOM FOR COMPETITION, CAPTURE
C-----AND FISSION (10) AND ELASTIC (AS INPUT).
      RESTAB(3,LOW)=10.0d0
      RESTAB(4,LOW)=AMUN
      RESTAB(5,LOW)=10.0d0
      RESTAB(6,LOW)=10.0d0
C-----COPY PARAMETERS AS CONSTANT BETWEEN LOWER AND UPPER ENERGY LIMITS
C-----OF THE UNRESOLVED REGION.
      ENRES(LOWP1)=ELX
      ENRES(LHI)=EHX
      RESTAB(1,LOWP1)=ELX
      RESTAB(1,LHI)=EHX
      DO 110 I=LOWP1,LHI
      RESTAB(2,I)=DX
      RESTAB(3,I)=0.0d0
      RESTAB(4,I)=GNO
      RESTAB(5,I)=GG
      RESTAB(6,I)=0.0d0
  110 CONTINUE
      GO TO 170
c-----------------------------------------------------------------------
C
C     ENERGY DEPENDENT FISSION WIDTHS, ALL OTHER PARAMETERS ARE ENERGY
C     INDEPENDENT. TREAT EACH (L, J) STATE AS A SECTION AND CONVERT
C     DATA TO ALL PARAMETERS ENERGY DEPENDENT FORM USING IMPLIED ENDF/B
C     VERSION DEPENDENT INTERPOLATION LAW.
C
c-----------------------------------------------------------------------
C-----READ LEVEL SPACING, J, NEUTRON DEGREES OF FREEDOM, NEUTRON AND
C-----CAPTURE WIDTHS (SEE, ABOVE EQUIVALENCE TO MEMBERS OF DUMSET).
  120 CALL LISTIO(DUMSET,6)
C-----READ ENERGY DEPENDENT FISSION WIDTHS UP TO 6 AT A TIME AND THEN
C-----MOVE THEM INTO THE 6-TH POSITION FOR EACH RESONANCE.
      JR=LOWP1
      DO 140 JR1=LOWP1,LHI,6
      JR2=JR1+5
      IF(JR2.GT.LHI) JR2=LHI
      JR3=(JR2-JR1)+1
      CALL LISTIO(DUMBO,JR3)
      DO 130 K=1,JR3
      RESTAB(6,JR)=DUMBO(K)
      JR=JR+1
  130 CONTINUE
  140 CONTINUE
C-----DEFINE INTERPOLATION LAW BASED ON ENDF/B FORMAT VERSION (E.G.,
C-----ENDF/B-4 = LOG-LOG, ENDF/B-5 = LINEAR-LINEAR).
      INTX=INT45
C-----DEFINE NUMBER OF DEGREES OF FREEDOM FOR COMPETITION AND CAPTURE
C-----(10) AND ELASTIC AND FISSION (AS INPUT).
      RESTAB(3,LOW)=10.0d0
      RESTAB(4,LOW)=AMUN
      RESTAB(5,LOW)=10.0d0
      RESTAB(6,LOW)=MUF
C-----COPY ENERGIES AT WHICH FISSION WIDTHS ARE GIVEN FROM FIRST SECTION
C-----COPY SPACING, COMPETITIVE, NEUTRON AND CAPTURE WIDTHS AS CONSTANTS
      JRX=LOWP1X
      DO 150 JR=LOWP1,LHI
      ENRES(JR)=ENRES(JRX)
      RESTAB(1,JR)=RESTAB(1,JRX)
      RESTAB(2,JR)=DX
      RESTAB(3,JR)=0.0d0
      RESTAB(4,JR)=GNO
      RESTAB(5,JR)=GG
      JRX=JRX+1
  150 CONTINUE
      GO TO 170
c-----------------------------------------------------------------------
C
C     ALL WIDTHS ARE ENERGY DEPENDENT.
C
c-----------------------------------------------------------------------
C-----READ NUMBER OF DEGREES OF FREEDOM FOLLOWED BY ALL RESONANCE
C-----PARAMETERS.
  160 CALL LISTIO(RESTAB(1,LOW),6)
      CALL LISPAR6(LOW+1,6*(NRS-1))
c-----------------------------------------------------------------------
C
C     LIST DATA CONVERTED TO ALL PARAMETERS ENERGY DEPENDENT FORM.
C
C     SAVE INTERPOLATION LAW AND DEFINE STATISTICAL WEIGHT (THESE 2
C     QUANTITIES ARE SAVED IN THE 2 UNUSED LOCATIONS PRECEDING THE
C     NUMBER OF DEGREES OF FREEDOM FOR EACH REACTION).
c
c-----------------------------------------------------------------------
  170 RESTAB(1,LOW)=INTX
      CALL GJWAIT(SPI,AJ,GJ)
      RESTAB(2,LOW)=GJ
C-----LIST INTERPOLATION LAW, J AND DEGREES OF FREEDOM.
      IF(IMEDIT.EQ.0) GO TO 180
      LSTINT=6
      IF(INTX.GE.1.AND.INTX.LE.5) LSTINT=INTX
      WRITE(OUTP,350) INTX,(TYPINT(I,LSTINT),I=1,4),AJ,
     1 (RESTAB(K,LOW),K=3,6)
      IF(IMEDIT.NE.2) GO TO 180
C-----IN EDIT MODE IF INTERPOLATION LAW IS INCORRECT PRINT WARNING.
      IF(LSTINT.EQ.6) WRITE(OUTP,370) INTX
      GO TO 200
C-----IN CALCULATION MODE IF INTERPOLATION LAW IS INCORRECT PRINT
C-----WARNING AND TERMINATE.
  180 IF(INTX.GE.1.AND.INTX.LE.5) GO TO 200
      WRITE(OUTP,190) INTX
      WRITE(*   ,190) INTX
  190 FORMAT(///' ERROR - Unresolved Interpolation Law=',I5,
     1 ' (MUST be 1 to 5).'/
     2          '         Cannot Interpolate Unresolved Data.'/
     3          '         Execution Terminated'///)
      CALL ENDERROR
C-----LIST PARAMETERS.
  200 DO 230 JR=LOWP1,LHI
C-----IF FISSION WIDTH IS NOT ZERO TURN ON FISSILE FLAG.
      IF(DABS(RESTAB(6,JR)).NE.0.0d0) LFWX=1
      IF(DABS(RESTAB(6,JR)).NE.0.0d0) LFWL(NSECT) = LFWL(NSECT) + 1
C-----CHECK FOR NEGATIVE WIDTHS
      NEGRES = 0
      DO I=3,6
      IF(RESTAB(I,JR).LT.0.0d0) NEGRES = NEGRES + 1
      ENDDO
      IF(IMEDIT.EQ.0.AND.NEGRES.EQ.0) GO TO 230
      DO 210 I=1,6
      CALL OUT9(RESTAB(I,JR),FIELD(1,I))
  210 CONTINUE
      GAMT=RESTAB(3,JR)+RESTAB(4,JR)+RESTAB(5,JR)+RESTAB(6,JR)
      CALL OUT9(GAMT        ,FIELD(1,7))
      WRITE(OUTP,360) ((FIELD(M,I),M=1,11),I=1,7)
C-----NEGATIVE WIDTHS?
      IF(NEGRES.GT.0) THEN
      NEGSUM = NEGSUM + NEGRES
      WRITE(   *,360) ((FIELD(M,I),M=1,11),I=1,7)
      WRITE(   *,220) NEGRES
      WRITE(OUTP,220) NEGRES
  220 FORMAT(' ERROR - ',I1,' Negative Widths')
      ENDIF
  230 CONTINUE    ! End resonance table loop
c-----------------------------------------------------------------------
c
c     2020/9/17 - Print WARNING = Unresolved resonances do not span
c                                 entire Unresolved energy range.
c
c-----------------------------------------------------------------------
      call WARNURR(RESTAB(1,LHI))
C-----END OF L AND J STATE LOOPS.
c-----------------------------------------------------------------------
C
C     PRINT WARNING IF ANY J SEQUENCES ARE MISSING AND ADD MISSING J
C     SEQUENCE WITH NO RESONANCES IN ORDER TO ALLOW POTENTIAL
C     SCATTERING TO BE CORRECTLY CALCULATED.
C
c-----------------------------------------------------------------------
      if(IJ.eq.NJS) then       ! After last j state for current L
      LSECT = NSECT
      CALL MISSINGJ(LNOW,LSECT)
      endif
C-----END OF J LOOP.
      ENDDO
C-----END OF L LOOP.
  240 CONTINUE
c-----------------------------------------------------------------------
C
C     STOP IF NEGATIVE WIDTHS
C
c-----------------------------------------------------------------------
      IF(NEGSUM.GT.0) THEN
      WRITE(   *,250) NEGSUM
      WRITE(OUTP,250) NEGSUM
  250 FORMAT(///' ERROR -',
     1 ' Execution Terminated because of',i8,' Negative Widths.'///)
      CALL ENDERROR
      ENDIF
      RETURN
  260 FORMAT(1X,78('=')/' Energy Independent Unresolved Parameters')
  270 FORMAT(1X,78('=')/' Unresolved Fission Widths Energy Dependent')
  280 FORMAT(1X,78('=')/' All Unresolved Parameters Energy Dependent')
  290 FORMAT(1X,78('=')/ 1X,7('WARNING...'),'WARNING'/
     1 ' Note, LSSF=',I2,' Indicates that the Resonance Contribution'/
     2 ' of this section of Resonance Parameters (MF=2) has already'/
     3 ' been added to the Cross Sections (MF=3). This section will be'/
     3 ' read, but Ignored in ALL Resonance Calculations.')
  300 FORMAT(' LSSF---------------------------------',I11,
     1 ' (Add to File 3)')
  310 FORMAT(' LSSF---------------------------------',I11,
     1 ' (Included in File 3)')
  320 FORMAT(1X,78('=')/
     1       ' Nuclear Spin of Target---------------',11A1/
     2       ' Effective Scattering Radius (A+)-----',11A1)
  330 FORMAT(' Number of L Values-------------------',I11)
  340 FORMAT(1X,78('=')/
     1       ' Atomic Weight Ratio of Isotope-------',11A1/
     2       ' L Value------------------------------',I11/
     3       ' Number of J Values-------------------',I11)
  350 FORMAT(1X,78('=')/32X,' Degrees of Freedom'/1X,78('=')/
     1 '   Interpolation  J Value  Competition    Neutron   Capture ',
     2 '    Fission'/7X,' Law'/1X,78('=')/I2,1X,3A4,A1,
     3 F9.2,6X,F6.2,1X,3(5X,F6.2)/
     3 1X,78('=')/' Resonance Parameters'/1X,78('=')/'      Energy',
     3 '     Level Competition    Neutron    Capture    Fission',
     4 '      Total'/
     5 12X,'    Spacing',5(6X,'Width')/1X,7(7X,'(eV)')/1X,78('='))
  360 FORMAT(1X,77A1)
  370 FORMAT(1X,78('=')/1X,7('WARNING...'),'WARNING'/
     1 ' Unresolved Interpolation Law=',I5,
     2 ' (MUST be 1 to 5). Cannot Interpolate Unresolved Data.'/
     3 ' Correct Evaluated Data Before Using it for Calculations.'/
     4 ' If Data is NOT Corrected this Program will Abort During',
     5 ' Calculations.'/' Execution Terminated'/1X,78('='))
      END
      SUBROUTINE GJWAIT(SPI,AJ,GJ)
C=======================================================================
C
C     DEFINE STATISTICAL WEIGHT BASED ON S AND J.
C
C     NOTE - THIS SHOULD BE THE ONLY PLACE IN THE PROGRAM THAT DEFINES
C            STATISTICAL WEIGHTS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      GJ=(TWO*AJ+ONE)/(FOUR*SPI+TWO)
      RETURN
      END
      SUBROUTINE SETJ(LNOW)
C=======================================================================
C
C     USE CHANNEL SPIN AND L VALUE TO DEFINE TABLE OF LEGAL J VALUES
C     AND STATISTICAL WEIGHTS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/JFIX/GJSET(50),GJBAD,TABJ(50),JOK(50),JMISS,NUMJ,IGJBAD
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
C-----INITIALIZE SUM OF STATISTICAL WEIGHTS.
      CALL SUMJ(GJBAD,LNOW,1)
C-----CHANNEL SPIN I+0.5 FIRST
      NUMJ=1
      ELL=LNOW
      CHSPIN=SPI+0.5D+00
      TABJ(NUMJ)=DABS(CHSPIN-ELL)
      ENDJ=CHSPIN+ELL
   10 IF(TABJ(NUMJ).EQ.ENDJ) GO TO 20
      TABJ(NUMJ+1)=TABJ(NUMJ)+1.0d0
      NUMJ=NUMJ+1
      GO TO 10
C-----CHANNEL SPIN I-0.5 NEXT.
   20 IF(SPI.EQ.0.0d0) GO TO 40
      NUMJ=NUMJ+1
      CHSPIN=DABS(SPI-0.5d0)
      TABJ(NUMJ)=DABS(CHSPIN-ELL)
      ENDJ=CHSPIN+ELL
   30 IF(TABJ(NUMJ).EQ.ENDJ) GO TO 40
      TABJ(NUMJ+1)=TABJ(NUMJ)+1.0d0
      NUMJ=NUMJ+1
      GO TO 30
C-----SORT J VALUES INTO ASCENDING ORDER, ELIMINATE DUPLICATES AND
C-----DEFINE STATISTIC WEIGHTS.
   40 IF(NUMJ.GT.1) CALL SORTS(TABJ,NUMJ)
      NUMIN=0
C-----INITIALIZE ERROR MESSAGE COUNT
      MYERROR = 0
      DO 80 I=1,NUMJ
      IF(I.EQ.1) GO TO 60
      DO 50 II=1,NUMIN
      IF(DABS(TABJ(II)-TABJ(I)).LT.0.01d0) GO TO 70
   50 CONTINUE
   60 NUMIN=NUMIN+1
      TABJ(NUMIN)=TABJ(I)
      JOK(NUMIN)=0
C-----DEFINE STATISTICAL WEIGHT.
      AJ=TABJ(NUMIN)
      CALL GJWAIT(SPI,AJ,GJSET(NUMIN))
      GO TO 80
C-----DUPLICATE J VALUE. ONLY ADD POTENTIAL IF OPTION IS TURNED ON
C-----AND THESE ARE NOT SINGLE LEVEL PARAMETERS.
   70 IF(LRF.EQ.1) GO TO 80
C-----PRINT WARNING BEFORE FIRST ERROR MESSAGE
      IF(MYERROR.EQ.0) WRITE(OUTP,90)
      MYERROR = MYERROR + 1
      WRITE(OUTP,100) LNOW,TABJ(I)
   80 CONTINUE
C-----PRINT FINAL LINE IF ANY ERROR MESSAGES
      IF(MYERROR.GT.0) WRITE(OUTP,110)
      NUMJ=NUMIN
      RETURN
   90 FORMAT(1X,78('-')/1X,7('WARNING...'),'WARNING')
  100 FORMAT(
     1 ' L=',I2,' J =',F7.3,' Corresponds to 2 Resonance Sequences.')
  110 FORMAT(1X,78('-'))
      END
      SUBROUTINE SUMJ(GJNOW,LNOW,MYWAY)
C=======================================================================
C
C     EITHER,
C     MYWAY = 1 - INITIALIZE SUM OF STATISICAL WEIGHTS (GJ)
C           = 2 - ADD GJ TO SUM
C           = 3 - COMPARE SUM TO (2*L+1) AND PRINT WARNING IF THEY
C                 DO NOT AGREE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/FIXPOT/MYPOT
      COMMON/JFIX/GJSET(50),GJBAD,TABJ(50),JOK(50),JMISS,NUMJ,IGJBAD
      GO TO (10,20,30),MYWAY
C-----INITIALIZE SUM.
   10 GJSUM=0.0d0
      RETURN
C-----ADD TO SUM
   20 GJSUM=GJSUM+GJNOW
      RETURN
C-----COMPARE SUM TO (2*L+1). PRINT ERROR MESSAGE IF MORE THAN 1
C-----PER-CENT DISAGREEMENT.
   30 GJWANT=2*LNOW+1
      IF(DABS(GJWANT-GJSUM).LE.0.01d0*GJWANT) GO TO 50
      WRITE(OUTP,60) LNOW,GJWANT,GJSUM
C-----IF REQUESTED ADD MISSING SEQUENCES
      IF(MYPOT.EQ.0) GO TO 40
      WRITE(OUTP,70)
      IGJBAD = 1
      GJBAD  = GJWANT - GJSUM
      RETURN
C-----OTHERWISE, DO NOT ADD SEQUENCES
   40 WRITE(OUTP,80)
   50 IGJBAD = 0
      GJBAD  = 0.0d0
      RETURN
   60 FORMAT(1X,78('-')/1X,7('WARNING...'),'WARNING'/
     1 ' FOR L =',I3,' Expect Sum of Statistical Weights (GJ) to Equal'/
     2 ' (2*L + 1) =',F7.3/
     3 ' Found     =',F7.3)
   70 FORMAT(
     4 ' Corrective Action Will be Taken to Correctly Calculate'/
     4 ' the Potential Scattering Cross Section - This Procedure is'/
     5 ' Based on the Decision of the National Nuclear Data Center,'/
     6 ' Brookhaven National Laboratory, Private Communication,'/
     7 ' Charles Dunford, (April 1991)')
   80 FORMAT(
     1 ' No Corrective Action Taken - Based on Input Parameter')
      END
      SUBROUTINE CHECKJ(AJNOW,LNOW,GJ,MYWAY)
C=======================================================================
C
C     DEFINE GJ AND CHECK J VALUE AGAINST TABLE OF LEGAL VALUES.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/JFIX/GJSET(50),GJBAD,TABJ(50),JOK(50),JMISS,NUMJ,IGJBAD
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/FIXPOT/MYPOT
C-----99/06/18 - UPDATED FOR NEW REICH-MOORE CONVENTION
      AJPLUS = DABS(AJNOW)
C-----DEFINE STATISTICAL WEIGHT.
      CALL GJWAIT(SPI,AJPLUS,GJ)
C-----ADD TO SUM OF WEIGHTS.
      CALL SUMJ(GJ,LNOW,2)
c-----------------------------------------------------------------------
C
C     DO NOT CHECK SINGLE LEVEL J VALUE - AVERAGE J VALUE ALLOWED.
C
c-----------------------------------------------------------------------
      IF(MYWAY.NE.2) GO TO 30
C-----CHECK J VALUE.
      DO 10 I=1,NUMJ
      IF(DABS(AJPLUS-TABJ(I)).LE.0.01d0) GO TO 20
   10 CONTINUE
      WRITE(OUTP,40) LNOW,AJPLUS
      GO TO 30
C-----INDICATE THAT J SEQUENCE HAS BEEN FOUND.
   20 JOK(I)=1
   30 RETURN
   40 FORMAT(1X,78('-')/1X,7('WARNING...'),'WARNING'/
     1 ' Based on Target Spin and L=',I2,' J =',F7.3,
     2 ' is NOT Physically Possible.'/
     3 ' The Use of Fictitious J Values is NOT Allowed in ENDF/B.'/
     5 ' Cross Sections Obtained from these Parameters will be',
     6 ' Unreliable.'/
     7 ' Correct Evaluation Before Attempting Reconstruction.'/
     8 1X,78('-'))
      END
      SUBROUTINE MISSINGJ(LNOW,LSECT)
C=======================================================================
C
C     IF NECESSARY ADD SECTION FOR MISSING J VALUES, ONLY IF MYPOT = 1.
C
C     CHECK SUM OF STATISTICAL WEIGHTS.
C
C     NEW SECTION WILL HAVE MOST PARAMETERS THE SAME AS LSECT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/JFIX/GJSET(50),GJBAD,TABJ(50),JOK(50),JMISS,NUMJ,IGJBAD
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/RANGER/LOW,LHI
      COMMON/FIXPOT/MYPOT
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      JMISS=0
      DO 20 J=1,NUMJ
      IF(JOK(J).GT.0) GO TO 20
C-----PRINT TITLE BEFORE FIRST MISSING J VALUE.
      IF(JMISS.GT.0) GO TO 10
      JMISS=1
      WRITE(OUTP,50) LNOW
C-----MISSING J-VALUE. INCREMENT BAD J SEQUENCE COUNT AND ADD
C-----STATISTICAL WEIGHT.
   10 WRITE(OUTP,60) TABJ(J)
   20 CONTINUE
      IF(JMISS.LE.0) GO TO 30
      WRITE(OUTP,70)
   30 CALL SUMJ(GJNOW,LNOW,3)
      IF(IGJBAD.EQ.0.OR.MYPOT.LE.0) GO TO 40
c-----10/05/28 - define correction for missing sequences
      ADDL(LSECT) = 2.0d0*GJBAD
   40 RETURN
   50 FORMAT(1X,78('-')/1X,7('WARNING...'),'WARNING'/
     1 ' for L=',I2,' The Following J Sequences are Missing.')
   60 FORMAT(' J =',F8.2)
   70 FORMAT(1X,78('-'))
      END
      SUBROUTINE RDAP
C=======================================================================
C
C     READ, WRITE AND SAVE ENERGY DEPENDENT SCATTERING RADIUS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/LASTE/ELAST
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADD INCLUDE
      INCLUDE 'recent.h'
C-----READ TAB1 LEADER LINE AND INTERPOLATION LAW.
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      CALL CARDO(C1,C2,L1,L2,N1,N2)
C-----INCREMENT NUMBER OF ENERGY RANGES WITH ENERGY DEPENDENT SCATTERING
C-----RADIUS AND DEFINE INDICES TO INTERPOLATION LAW AND TABULATED DATA.
      NUMRHO=NUMRHO+1
      IF(NUMRHO.GT.1) GO TO 10
      INXRHO(1,1)=1
      INXRHO(3,1)=1
      GO TO 20
   10 INXRHO(1,NUMRHO)=INXRHO(2,NUMRHO-1)+1
      INXRHO(3,NUMRHO)=INXRHO(4,NUMRHO-1)+1
   20 INXRHO(2,NUMRHO)=INXRHO(1,NUMRHO)+N1-1
      INXRHO(4,NUMRHO)=INXRHO(3,NUMRHO)+N2-1
      INX1=INXRHO(1,NUMRHO)
      INX2=INXRHO(2,NUMRHO)
      INX3=INXRHO(3,NUMRHO)
      INX4=INXRHO(4,NUMRHO)
C-----TEST FOR TABLE OVERFLOW.
      IF(INX2.LE.MAXSEC.AND.INX4.LE.MAXRHO) GO TO 40
      WRITE(OUTP,30) INX2,MAXSEC,INX4,MAXRHO
c-----2018/1/23 - Added on-line output.
      WRITE(*   ,30) INX2,MAXSEC,INX4,MAXRHO
   30 FORMAT(///' ERROR'/
     1 ' Energy Dependent Scattering Radius Table Overflow'/
     2 ' Interpolation Regions---',I6,' (Cannot Exceed',I6,')'/
     3 ' Tabulated Energies------',I6,' (Cannot Exceed',I6,')'/
     4 ' Increase Dimension in COMMON/TABRHO/ and Re-Run'/
     5 ' Execution Terminated.'///)
      CALL ENDERROR
C-----READ AND CHECK INTERPOLATION LAW.
   40 CALL TERPI(NBTRHO(INX1),INTRHO(INX1),N1)
c-----2019/1/3 - Additional Interpolation Law Tests
      CALL TERPTEST(NBTRHO(INX1),INTRHO(INX1),N1,N2,1)
      CALL TERPO(NBTRHO(INX1),INTRHO(INX1),N1)
C-----PRINT TITLE IN EDIT MODE.
      IF(IMEDIT.GT.0) WRITE(OUTP,80) N1,N2
C-----READ AND CHECK TABULATED ENERGY DEPENDENT SCATTERING RADIUS.
      ELAST=0.0d0
      CALL POINTI(ERHOTB(INX3),APTAB(INX3),N2)
      CALL POINTO(ERHOTB(INX3),APTAB(INX3),N2)
      IF(IMEDIT.GT.0) WRITE(OUTP,90)
      IERAP=0
      DO 50 I=INX3,INX4
      IF(APTAB(I).LE.0.0d0) IERAP=1
      IF(IMEDIT.EQ.0) GO TO 50
      CALL OUT9(ERHOTB(I),FIELD(1,1))
      CALL OUT9(APTAB (I),FIELD(1,2))
      WRITE(OUTP,100) I,((FIELD(M,J),M=1,11),J=1,2)
   50 CONTINUE
      IF(IERAP.EQ.0) GO TO 70
      WRITE(OUTP,60)
      WRITE(*   ,60)
   60 FORMAT(///' ERROR - Energy Dependent Scattering Law Error.'/
     3 '         Energies MUST be Monotonically Increasing.'/
     4 '         Scattering Radius MUST be Positive.'/
     5 '         Execution Terminated.'///)
      CALL ENDERROR
   70 RETURN
   80 FORMAT(1X,78('=')/
     1 ' Energy Dependent Scattering Radius Interpolation Law'/
     2 1X,78('=')/
     3 I5,' Interpolation Ranges'/
     4 I5,' Tabulated Values'/
     5 1X,78('=')/' Interpolation Law'/1X,78('=')/
     6 ' Index  Boundary       Law'/1X,78('='))
   90 FORMAT(1X,78('=')/
     1 ' Energy Dependent Scattering Radius'/1X,78('=')/
     2 ' Index      Energy      Radius'/
     3 '             (eV)      (Fermi)'/1X,78('='))
  100 FORMAT(I6,1X,11A1,1X,11A1)
      END
      SUBROUTINE LIMIT1(IPATH)
C=======================================================================
C
C     CORE ALLOCATION.
C
C     INCREMENT SECTION COUNT AND LOWER AND/OR UPPER INDICES TO
C     RESONANCE PARAMETER TABLE.
C
C     IF AVAILABLE CORE ALLOCATION IS EXCEEDED TERMINATE UNLESS PROGRAM
C     IS IN THE EDIT MODE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE,ADDSEC,ADDRES
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/RANGER/LOW,LHI
      COMMON/MAXIE/NEDSEC,NEDRES,NEDNOD
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DATA ADDSEC/0/
      DATA ADDRES/0/
C-----SELECT PATH.
      IF(IPATH.EQ.2) GO TO 30
C-----INCREMENT SECTION COUNT AND INSURE THAT CORE ALLOCATION WILL NOT
C-----BE EXCEEDED.
C-----11/24/2012 - ELIMINATED NHIGH(0) INDEXING ERROR.
      IF(NSECT.GT.0) THEN
      LHI = NHIGH(NSECT)
      ELSE
      LHI = 0
      ENDIF
C-----11/24/2012 - ELIMINATED NHIGH(0) INDEXING ERROR.
      NSECT=NSECT+1
      IF((NSECT+ADDSEC).GT.NEDSEC) NEDSEC=NSECT+ADDSEC
      IF(NSECT.LE.MAXSEC) GO TO 30
C-----AVAILABLE CORE EXCEEDED. TERMINATE UNLESS IN EDIT MODE.
      IF(IMEDIT.NE.2) GO TO 10
      ADDSEC=MAXSEC
      NSECT=1
      GO TO 30
   10 WRITE(OUTP,20) MAXSEC
      WRITE(*   ,20) MAXSEC
   20 FORMAT(///' ERROR - More than',I6,' Sections'/
     1          '         Re-Run Program in Edit Mode to Determine'/
     1          '         Memory Requirements.'/
     2          '         Execution Terminated'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     INCREMENT LOWER AND UPPER INDICES TO RESONANCE PARAMETER TABLE.
C     PARAMETER TABLE.
C
c-----------------------------------------------------------------------
C-----DEFINE INDICES FOR NEXT SET OF RESONANCES TO READ AND INSURE THAT
C-----CORE ALLOCATION WILL NOT BE EXCEEDED.
   30 LOW=LHI+1
      LHI=LHI+NRS
      IF((LHI+ADDRES).GT.NEDRES) NEDRES=LHI+ADDRES
      IF(LHI.LE.MAXRES) GO TO 60
C-----AVAILABLE CORE EXCEEDED. TERMINATE UNLESS IN EDIT MODE.
      IF(IMEDIT.NE.2) GO TO 40
      ADDRES=ADDRES+(LOW-1)
      LOW=1
      LHI=NRS
      GO TO 60
   40 WRITE(OUTP,50) MAXRES
      WRITE(*   ,50) MAXRES
   50 FORMAT(///' ERROR - More than',I6,' Resonances'/
     1          '         Re-Run Program in Edit Mode to Determine'/
     2          '         Requirements.'/
     3          '         Execution Terminated'///)
      CALL ENDERROR
   60 RETURN
      END
      SUBROUTINE SIGMA(E,SIGNOW)
C=======================================================================
C
C     CALCULATE CONTRIBUTION FROM ALL SECTIONS.
C
C     NOTE - THE ALGORITHMS FOR ALL POSSIBLE TYPES OF RESONANCE
C            PARAMETERS HAVE BEEN WRITTEN IN A FORM TO REMOVE A
C            COMMON FACTOR OF,
C
C            PI*ABUNDANCE*(LAMBDA**2)
C
C            AFTER CALCULATING THE CONTRIBUTION FROM ANY TYPE OF
C            RESONANCE PARAMETER THIS FACTOR IS USED TO MULTIPLY
C            THE RESULT TO DEFINE THE FINAL CROSS SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      common/rmlfinal/rmlsigma(11)
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      COMMON/ERRORCOM/ERRXC3,ERMINMAX,ERMT2,ERMT102,ERSOFT,XCLOW
c-----2017/4/13 - Deleted Lrf
      COMMON/MRMLWCOM/Npp,Ngroup,Nres,Nro7,Naps7,Kg,Minr,Maxr
      COMMON/THRESCOM/NTHRES(11)
c***** DEBUG
c     COMMON/MISSINGCOM/EMISSING
c***** DEBUG
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION SIGNOW(*)
C-----IF ANY SECTIONS HAVE ENERGY DEPENDENT SCATTERING RADIUS DEFINE
C-----RADIUS AT ENERGY E.
      IF(NUMRHO.GT.0) CALL SETRHO(E)
C-----INITIALIZE ALL CROSS SECTIONS (EXCEPT TOTAL)
      DO J=2,NREACT
      SIGNOW(J)=0.0d0
      ENDDO
c***** DEBUG
c     SIGXSUM = 0.0d0      ! Competition
c     SIGLSUM = 0.0d0      ! missing (L,J) sequences
c***** DEBUG
c-----------------------------------------------------------------------
C
C     ADD CONTRIBUTION FROM EACH SECTION.
C
c-----------------------------------------------------------------------
      DO 140 ISECT=1,NSECT
C-----SELECT PARAMETER TYPE.
      MODE1=MODE(ISECT)
C-----SKIP SECTION IF IT DOES NOT CONTRIBUTE TO THIS ENERGY RANGE.
      IF(MODE1.LE.0) GO TO 140
C-----DEFINE INDICES FOR CURRENT SECTION OF RESONANCES.
      LOW=NLOW(ISECT)
      LHI=NHIGH(ISECT)
      Ngr1 = NGRTAB(1,ISECT)
      Ngr2 = NGRTAB(2,ISECT)
      Npp  = NPPTAB(ISECT)
C-----INITIALIZE CONTRIBUTION OF SECTION TO SUM.
      SIGNTI=0.0d0
      SIGNNI=0.0d0
      SIGNGI=0.0d0
      SIGNFI=0.0d0
c***** DEBUG
c     SIGNXI=0.0d0      ! Competition
c     SIGNLI=0.0d0      ! Missing (L,J) sequences
c***** DEBUG
c-----------------------------------------------------------------------
C
C     RESOLVED    = 1 TO 10
C     UNRESOLVED = 11 TO 12 (12 NO LONGER USED)
C
c-----------------------------------------------------------------------
C              1   2   3   4   5   6   7   8   9  10
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90, 90,
     1       110,120), MODE1
c-----------------------------------------------------------------------
C
C     UNRESOLVED
C
c-----------------------------------------------------------------------
C-----LRF = 1) SINGLE LEVEL BREIT-WIGNER.
   10 CALL SIGBW1(E)
      GO TO 130
C-----LRF = 2) MULTI-LEVEL BREIT-WIGNER.
   20 CALL SIGBWM(E)
      GO TO 130
C-----LRF = 3) REICH-MOORE.
   30 CALL SIGRM1(E)
      GO TO 130
C-----LRF = 4) ADLER-ADLER (ENDF/B-5 AND LATER).
   40 CALL SIGAA5(E)
      GO TO 130
C-----LRF = 5) GENERAL R-MATRIX.
   50 CALL SIGGRM(E)
      GO TO 130
C-----LRF = 6) HYBRID R-FUNCTION.
   60 CALL SIGHRF(E)
      GO TO 130
C-----LRF = 7) NEW (2003) REICH-MOORE WITH COMPETITION
   70 CALL SIGRML(E)
      do j=1,NREACT
      SIGNOW(j) = rmlsigma(j)
c-----2021/2/23 - Threshold test MT > 102
      if(NTHRES(j).le.0) then
      if(SIGNOW(j).lt.XCLOW) then
      SIGNOW(j) = 0.0d0              ! Below lowest = below threshold
      else
      NTHRES(j) = 1
      endif                          ! threshold reached
      endif                          ! threshold reached
      enddo
C-----SKIP NORMALIZATION
      GO TO 140
C-----LRF = 4A) ADLER-ADLER (ENDF/B-4 AND EARLIER).
   80 CALL SIGAA4(E)
      GO TO 130
C-----ILLEGAL
   90 WRITE(3,100)
      WRITE(*,100)
  100 FORMAT(///' ERROR - Illegal Resonance Parameter Mode.'/
     1          '         Execution Terminated.'///)
      CALL ENDERROR
c-----------------------------------------------------------------------
C
C     UNRESOLVED
C
c-----------------------------------------------------------------------
C-----UNRESOLVED (INTERPOLATE PARAMETERS).
  110 CALL SIGURP(E)
      GO TO 130
C-----UNRESOLVED (INTERPOLATE CROSS SECTIONS) - NO LONGER USED
  120 CALL SIGURS(E)
c-----------------------------------------------------------------------
C
C     ALL
C
c-----------------------------------------------------------------------
C-----MULTIPLY CONTRIBUTION OF SECTION BY PI*ABUNDANCE*(LAMBDA**2)
C-----(WHICH IS BETAE) AND ADD TO SUM.
  130 BETAE=BETA(ISECT)/E
      SIGNOW(2)=SIGNOW(2)+SIGNNI*BETAE
      SIGNOW(3)=SIGNOW(3)+SIGNGI*BETAE
      SIGNOW(4)=SIGNOW(4)+SIGNFI*BETAE
c***** DEBUG
c     SIGXSUM  =SIGXSUM  +SIGNXI*BETAE  ! OPTIONAL COMPETIRION
c     SIGLSUM  =SIGLSUM  +SIGNLI*BETAE  ! Missing (L,J) sequences
c***** DEBUG
C-----END OF SECTION LOOP.
  140 CONTINUE
C-----DEFINE TOTAL TO EQUAL SUM OF PARTS.
      TOTSUM = 0.0d0
      DO J=2,NREACT
      TOTSUM = TOTSUM + SIGNOW(J)
      ENDDO
      SIGNOW(1) = TOTSUM
C***** DEBUG - ACTIVATE FOR UNRESOLVED COMPETITION LISTING
C     IF(SIGXSUM.GT.0.0d0) WRITE(22,2200) E,SIGNOW(2),SIGNOW(3),SIGXSUM
C2200 FORMAT(1P5D12.4)
c     IF(E.gt.1.5d0*EMISSING) then
c     WRITE(22,2200) E,SIGLSUM,SIGNOW(2)
c2200 FORMAT(1P3D11.4)
c     EMISSING = E
c     endif
C***** DEBUG - ACTIVATE FOR UNRESOLVED COMPETITION LISTING
      RETURN
      END
      SUBROUTINE SETRHO(E)
C=======================================================================
C
C     FOR ALL SECTIONS WHICH HAVE AN ENERGY DEPENDENT SCATTERING RADIUS
C     INTERPOLATE TABULATED VALUES TO DEFINE SCATTERINMG RADIUS AT E.
C     STORE INTERPOLATED VALUES IN THE ARRAY RHOP1 (THE REMAINDER OF
C     THE CALCULATIONS CAN THEN PROCEED IGNORING ENERGY DEPENDENCE OF
C     THE SCATTERING RADIUS).
C
C=======================================================================
      INCLUDE 'implicit.h'
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C-----SELECT SECTIONS WITH ENERGY DEPENDENT SCATTERING RADIUS.
      DO 70 KSECT=1,NSECT
      IF(NRHO(KSECT).EQ.0) GO TO 70
C-----DEFINE INDICES TO INTERPOLATION LAW AND TABULATED DATA.
      LNX=NRHO(KSECT)
      INX1=INXRHO(1,LNX)
      INX2=INXRHO(2,LNX)
      INX3=INXRHO(3,LNX)
      INX4=INXRHO(4,LNX)
C-----FIND ENERGY OR ENERGY RANGE WHERE RADII ARE GIVEN.
      DO 10 L=INX3,INX4
      IF(E.lt.ERHOTB(L)) go to 20
      IF(E.eq.ERHOTB(L)) go to 50
   10 CONTINUE
C-----EXTEND RADIUS AS CONSTANT OUTSIDE TABULATED ENERGY RANGE.
      L=INX4
      GO TO 50
   20 IF(L.EQ.INX3) GO TO 50
C-----E IS BETWEEN ERHOTB(L-1) AND ERHOTB(L).
      L=L-1
C-----DEFINE INTERPOLATION LAW. DEFINE TRUE POINT INDEX.
      INX5=L-INX3+1
      DO 30 J=INX1,INX2
      IF(INX5.GT.NBTRHO(J)) GO TO 40
   30 CONTINUE
      J=INX2+1
C-----POINT IS IN INTERPOLATION REGION J-1.
   40 J=J-1
C-----INTERPOLATE RADIUS TO ENERGY E.
      CALL RHOINT(E,RHOP1(KSECT),ERHOTB(L),RHOTAB(L),INTRHO(J))
      GO TO 60
C-----DEFINE RADIUS SET AT E (E IS AN ENERGY AT WHICH RADIUS IS
C-----TABULATED OR E IS OUTSIDE TABULATED RANGE AND WILL BE EXTENDED
C-----AS CONSTANT).
   50 RHOP1(KSECT)=RHOTAB(L)
C-----IF REQUESTED SET CHANNEL RADIUS EQUAL TO SCATTERING RADIUS.
   60 IF(NAPTAB(KSECT).EQ.1) RHOX2(KSECT)=RHOP1(KSECT)**2
   70 CONTINUE
      RETURN
      END
      SUBROUTINE SETRHO1(ERABS,KSECT)
C=======================================================================
C
C     DEFINE SCATTERING RADIUSA, ETC., AT ONE RESONANCE ENERGY.
C
C     THIS IS A SHORTER VERSION OF SETRHO.
C
C=======================================================================
      INCLUDE 'implicit.h'
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C-----DEFINE INDICES TO INTERPOLATION LAW AND TABULATED DATA.
      LNX=NRHO(KSECT)
      INX1=INXRHO(1,LNX)
      INX2=INXRHO(2,LNX)
      INX3=INXRHO(3,LNX)
      INX4=INXRHO(4,LNX)
C-----FIND ENERGY OR ENERGY RANGE WHERE RADII ARE GIVEN.
      DO 10 L=INX3,INX4
      IF(ERABS.lt.ERHOTB(L)) go to 20
      IF(ERABS.eq.ERHOTB(L)) go to 50
   10 CONTINUE
C-----EXTEND RADIUS AS CONSTANT OUTSIDE TABULATED ENERGY RANGE.
      L=INX4
      GO TO 50
   20 IF(L.EQ.INX3) GO TO 50
C-----ERABS IS BETWEEN ERHOTB(L-1) AND ERHOTB(L).
      L=L-1
C-----DEFINE INTERPOLATION LAW. DEFINE TRUE POINT INDEX.
      INX5=L-INX3+1
      DO 30 J=INX1,INX2
      IF(INX5.GT.NBTRHO(J)) GO TO 40
   30 CONTINUE
      J=INX2+1
C-----POINT IS IN INTERPOLATION REGION J-1.
   40 J=J-1
C-----INTERPOLATE RADIUS TO ENERGY ERABS.
      CALL RHOINT(ERABS,RHOP1(KSECT),ERHOTB(L),RHOTAB(L),INTRHO(J))
      GO TO 60
C-----DEFINE RADIUS SET AT ERABS (ERABS IS AN ENERGY AT WHICH RADIUS IS
C-----TABULATED OR ERABS IS OUTSIDE TABULATED RANGE AND WILL BE EXTENDED
C-----AS CONSTANT).
   50 RHOP1(KSECT)=RHOTAB(L)
C-----IF REQUESTED SET CHANNEL RADIUS EQUAL TO SCATTERING RADIUS.
   60 RHOX2(KSECT)=RHOP1(KSECT)**2
      CONTINUE
      RETURN
      END
      SUBROUTINE RHOINT(E,RHOP1,ERHOTB,RHOTAB,INTX)
C=======================================================================
C
C     INTERPOLATE ENERGY DEPENDENT SCATTERING RADIUS.
C
C     THIS ROUTINE HAS BEEN RECODED IN ORDER TO AVOID ROUND-OFF
C     PROBLEMS ON SHORT WORD LENGTH COMPUTERS, E.G. IBM-360, 370, ETC.
C     THIS ROUTINE IS NOW SLIGHTLY LESS EFFICIENT THAN IN ITS PREVIOUS
C     FORM. HOWEVER ALL INTERPOLATION IS NOW DEFINED AS A WEIGHTED SUM
C     OF TERMS, AS OPPOSED TO THE PREVIOUS FORM WHICH USED DIFFERENCES.
C
C     AN ILLEGAL INTERPOLATION CODE OR A NON-POSITIVE ENERGY WHERE LOG
C     ENERGY INTERPOLATION IS REQUIRED INDICATES EITHER AN ERROR IN THE
C     DATA AS IT APPEARS IN THE ENDF/B FORMAT, OR AN ERROR IN THIS
C     PROGRAM. THEREFORE ERRORS OF THIS TYPE WILL CAUSE THE PROGRAM TO
C     PRINT A WARNING MESSAGE AND TERMINATE EXECUTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FIELDC/FIELD(11,12)
      DIMENSION ERHOTB(*),RHOTAB(*)
C-----CHECK INTERPOLATION CODE.
      IF(INTX.LT.1.OR.INTX.GT.5) GO TO 60
C-----DEFINE ENERGIES AT THE 2 ENDS OF THE INTERVAL.
      E1=ERHOTB(1)
      E2=ERHOTB(2)
C-----CHECK FOR ZERO LENGTH INTERVAL.
      DE12=E2-E1
      IF(DE12.EQ.0.0d0) GO TO 100
C-----SELECT INTERPOLATION LAW.
      GO TO (10,20,30,40,50),INTX
c-----------------------------------------------------------------------
C
C     HISTOGRAM. CONSTANT EQUAL TO VALUE AT LOWER ENERGY LIMIT.
C
c-----------------------------------------------------------------------
   10 RHOP1=RHOTAB(1)
      RETURN
c-----------------------------------------------------------------------
C
C     LINEAR X AND LINEAR Y.
C
c-----------------------------------------------------------------------
   20 WT2=(E-E1)/DE12
      WT1=ONE-WT2
      RHOP1=WT1*RHOTAB(1)+WT2*RHOTAB(2)
      RETURN
c-----------------------------------------------------------------------
C
C     LOG X AND LINEAR Y.
C
c-----------------------------------------------------------------------
C-----INSURE ALL X VALUES ARE POSITIVE FOR LOG.
   30 IF(E1.LE.0.0d0.OR.E2.LE.0.0d0.OR.E.LE.0.0d0) GO TO 80
      WT2=DLOG(E/E1)/DLOG(E2/E1)
      WT1=ONE-WT2
      RHOP1=WT1*RHOTAB(1)+WT2*RHOTAB(2)
      RETURN
c-----------------------------------------------------------------------
C
C     LINEAR X AND LOG Y.
C
c-----------------------------------------------------------------------
   40 WT2=(E-E1)/DE12
      WT1=ONE-WT2
      RHOP1=DEXP(WT1*DLOG(RHOTAB(1))+WT2*DLOG(RHOTAB(2)))
      RETURN
c-----------------------------------------------------------------------
C
C     LOG X AND LOG Y.
C
c-----------------------------------------------------------------------
   50 IF(E1.LE.0.0d0.OR.E2.LE.0.0d0.OR.E.LE.0.0d0) GO TO 80
      WT2=DLOG(E/E1)/DLOG(E2/E1)
      WT1=ONE-WT2
      RHOP1=DEXP(WT1*DLOG(RHOTAB(1))+WT2*DLOG(RHOTAB(2)))
      RETURN
C-----ILLEGAL INTERPOLATE CODE.
   60 WRITE(OUTP,70) INTX
      WRITE(*   ,70) INTX
   70 FORMAT(///' ERROR - Interpolating Scattering Radius.'/
     2          '         Illegal Interpolation Code =',I5,
     2                                    ' (MUST be 1 to 5).'/
     3          '         Correct Evaluated Data and Re-Run Program.')
      CALL ENDERROR
C-----ILLEGAL LOG ENERGY INTERPOLATION WITH NEGATIVE VALUES.
   80 CALL OUT9(E1,FIELD(1,1))
      CALL OUT9(E2,FIELD(1,2))
      CALL OUT9(E ,FIELD(1,3))
      WRITE(OUTP,90) ((FIELD(M,J),M=1,11),J=1,3)
      WRITE(*   ,90) ((FIELD(M,J),M=1,11),J=1,3)
   90 FORMAT(///' ERROR - Interpolating Scattering Radius.'/
     1          '         Illegal Log Energy Interpolation',
     2                                  ' Using Negative Energy.'/
     3          '         Interpolation Code=',I5,
     3                                    ' (Cannot be 3 or 5).'/
     4          '         E1,E2,E=',3(11A1,1X)/
     5          '         Correct Evaluated Data and Re-Run Program.'/
     6          '         Execution Terminated.'///)
      CALL ENDERROR
C-----ZERO LENGTH ENERGY INTERVAL.
  100 CALL OUT9(E1,FIELD(1,1))
      CALL OUT9(E2,FIELD(1,2))
      WRITE(OUTP,110) ((FIELD(M,J),M=1,11),J=1,2)
      WRITE(*   ,110) ((FIELD(M,J),M=1,11),J=1,2)
  110 FORMAT(///' ERROR - Interpolating Scattering Radius.'/
     2          '         Illegal Interpolation Over ZERO',
     2                                  ' Length Interval.'/
     3          '         E1,E2=',11A1,1X,11A1/
     4          '         Correct Evaluated Data and Rr-Run Program.'/
     5          '         Execution Terminated.'///)
      CALL ENDERROR
      RETURN        ! Dummy cannot be reached
      END
      SUBROUTINE SIGBW1(E)
C=======================================================================
C
C     ADD CONTRIBUTION OF ONE SECTION OF SINGLE LEVEL BREIT-WIGNER
C     PARAMETERS.
C
C     DEFINITIONS FROM ENDF-102 FORMATS AND PROCEDURES MANUAL
C     =======================================================
C     ELASTIC
C     =======
C     THE DEFINITION OF SINGLE LEVEL ELASTIC SCATTERING IN ENDF-102 IS
C     INCORRECT. HERE WE USE THE CORRECT DEFINITION,
C
C     ELASTIC = (2*L + 1)*SIN(PS)**2 +
C
C       GJ*GAM(N)*(GAM(N) - 2*GAM(T)*SIN(PS)**2 + 2*DE*SIN(2*PS))/DEN
C
C     CAPTURE
C     =======
C     CAPTURE = GAM(N)*GAM(C)/DEN
C
C     FISSION
C     =======
C     FISSION = GAM(N)*GAM(F)/DEN
C
C     DE  = (E - ER)
C     DEN = ((DE)**2 + (GAM(T)/2)**2)
C
C     SUMMED OVER ALL RESONANCES WITH THE SAME L VALUE. NOTE, THE
C     POTENTIAL CONTRIBUTION IS INCLUDED ONLY ONCE FOR EACH L VALUE
C     AND THE TERMS GJ, 2*SIN(PS)**2, 2*SIN(2*PS) ARE THE SAME FOR
C     ALL RESONANCES WITH THE SAME (L,J) VALUE. THEREFORE THE ROUTINE
C     WILL DEFINE 3 SEPERATE SUMS,
C
C     1) GAM(N)*GAM(N)/DEN
C     2) GAM(N)*GAM(T)/DEN
C     3) GAM(N)*DE/DEN
C
C     ONLY AFTER SUMMING THESE EXPRESSIONS OVER ALL RESONANCES WILL
C     THEY BE MULTIPLIER BY THE RESONANCE INDEPENDENT FACTORS TO
C     DEFINE THE CROSS SECTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
c-----------------------------------------------------------------------
C
C     DEFINE RESONANCE INDEPENDENT QUANTITIES.
C
c-----------------------------------------------------------------------
      RHO2=E*RHOX2(ISECT)
      RHOP=RHOP1(ISECT)*DSQRT(E)
C-----DEFINE PENETRATION FACTOR, SHIFT FACTOR, PHASE SHIFT.
      CALL FACTS2(LVALUE(ISECT),RHO2,SF2,PF)
      CALL FACPHI(LVALUE(ISECT),RHOP,PS)
      COSPS=DCOS(PS)
      SINPS=DSIN(PS)
C-----DEFINE SIN(2*PS) AND 2*SIN(PS)**2
      SIN2PS=TWO*SINPS*COSPS
      SINPS2=TWO*SINPS*SINPS
C-----DEFINE PENETRATION FACTOR FOR COMPETITIVE WIDTH.
      IF(LRXTAB(ISECT).LE.0) GO TO 10
      RHOZ2=(E+QVALUE(ISECT))*RHOX2(ISECT)
c-----10/10/10 - switched from FACTS2 to FACTS3 - shift not used
      CALL FACTS3(LRXTAB(ISECT)-1,RHOZ2,PFC)
      GO TO 20
   10 PFC=0.0d0
c-----------------------------------------------------------------------
C
C     J VALUE LOOP.
C
c-----------------------------------------------------------------------
C-----DEFINE STATISTICAL WEIGHT FOR ALL RESONANCES WITH THE SAME (L,J).
   20 GJ=RESTAB(2,LOW)
      AJNOW = RESJTAB(LOW)
C-----INITIALIZE CONTRIBUTION OF (L,J) SEQUENCE.
      SIGNN1=ZERO
      SIGNN2=ZERO
      SIGNN3=ZERO
      SIGNGJ=ZERO
      SIGNFJ=ZERO
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP. ADD CONTRIBUTION OF ALL RESONANCES WITH SAME (L,J)
C
c-----------------------------------------------------------------------
      DO 30 JR=LOW,LHI
C-----ONLY USE RESONANCES WITH SAME J VALUE.
      IF(DABS(AJNOW-RESJTAB(JR)).GT.0.01d0) GO TO 40
      ER=ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 30  ! Note, DABS to only get = 0
      GAMC=PFC*RESTAB(3,JR)
      GN=RESTAB(4,JR)
      GAMN=PF*GN
      GAMG=RESTAB(5,JR)
      GAMF=RESTAB(6,JR)
      GAMT=GAMN+GAMG+GAMF+GAMC
      GAMT2=HALF*GAMT
      DE=E-(ER+GN*(SHIFT2(JR)-SF2))
      DEN=DE*DE+GAMT2*GAMT2
      COMFAC=GAMN/DEN
      SIGNN1=SIGNN1+COMFAC*GAMN
      SIGNN2=SIGNN2+COMFAC*GAMT
      SIGNN3=SIGNN3+COMFAC*DE
      SIGNGJ=SIGNGJ+COMFAC*GAMG
      SIGNFJ=SIGNFJ+COMFAC*GAMF
C-----END OF RESONANCE LOOP.
   30 CONTINUE
      JR=LHI+1
C-----MULTIPLY CONTRIBUTION OF (L,J) SEQUENCE BY STATISTICAL WEIGHT
C-----AND ADD TO SUM...NOTE, SINPS2 IS 2*SIN(PS)**2
   40 SIGNNI=SIGNNI+GJ*(SIGNN1-SIGNN2*SINPS2+TWO*SIGNN3*SIN2PS)
      SIGNGI=SIGNGI+GJ*SIGNGJ
      SIGNFI=SIGNFI+GJ*SIGNFJ
C-----TEST FOR ANOTHER J VALUE.
      LOW=JR
      IF(LOW.LE.LHI) GO TO 20
c-----------------------------------------------------------------------
C
C     ADD POTENTIAL SCATTERING CONTRIBUTION FOR L VALUE.
C
c-----------------------------------------------------------------------
C-----ADD 2*(2*L+1)*[2*SIN(PS)**2] (NOTE, SINPS2 IS 2*SIN(PS)**2)
      SIGNNI=SIGNNI+POTL(ISECT)*SINPS2
      RETURN
      END
      SUBROUTINE SIGBWM(E)
C=======================================================================
C
C     ADD CONTRIBUTION OF ONE SECTION OF MULTI-LEVEL BREIT-WIGNER
C     PARAMETERS.
C
C     DEFINITIONS FROM ENDF-102 FORMATS AND PROCEDURES MANUAL
C     =======================================================
C     CAPTURE AND FISSION CROSS SECTIONS ARE CALCULATED EXACTLY AS IN
C     THE CASE OF SINGLE LEVEL BREIT-WIGNER RESONANCES (FOR DETAILS
C     SEE, SUBROUTINE SIGBW1). HERE WE WILL ONLY CONSIDER THE ELASTIC
C     CROSS SECTION.
C
C     THE ELASTIC CROSS SECTION IS DEFINED TO BE,
C
C     ELASTIC = GJ*(1 - U(N,N))**2
C
C                                     I*GAM(N)
C     U(N,N)= EXP(-I*2*PS)*(I + ====================)
C                               ((ER-E)-I*GAM(T)/2)
C
C                                I*GAM(N)*((ER-E)+I*GAM(T)/2)
C           = EXP(-I*2*PS)*(I + =======================================)
C                               ((ER-E)-I*GAM(T)/2)*((ER-E)+I*GAM(T)/2)
C
C                                I*GAM(N)*((ER-E)-GAM(N)*GAM(T)/2)
C           = EXP(-I*2*PS)*(I + =======================================)
C                               ((ER-E)**2+(GAM(T)/2)**2)
C
C     U(N,N) = (COS(2*PS) - I*SIN(2*PS))*((1 - R) - I*S)
C
C     R      = GAM(N)*GAM(T)/2/DEN
C     S      = GAM(N)*DE/DEN
C     GAM(N) = NEUTRON WIDTH
C     GAM(R) = TOTAL WIDTH
C     DE     = E - ER
C     DEN    = ((E - ER)**2 + GAM(T)**2)
C
C     SUMMED OVER RESONANCES FOR EACH (L,J) SEQUENCE.
C
C     FROM OUR DEFINITION OF A UNIFORM TREATMENT OF ALL FORMALISMS WE
C     CAN IMMEDIATELY IDENTIFY,
C
C     X =  R
C     Y =  S
C
C     AND SUBSTITUTE INTO OUR GENERAL EXPRESSION,
C
C     ELASTIC =GJ*(2*SIN(PS)**2 - R)**2 + (SIN(2*PS) + S)**2)
C
C     ABSORPTION (CAPTURE AND FISSION) ARE TREATED IN A SINGLE LEVEL
C     APPROXIMATION,
C
C     CAPTURE =GJ*GAM(N)*GAM(G)/DEN
C
C     FISSION =GJ*GAM(N)*GAM(F)/DEN
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
c-----------------------------------------------------------------------
C
C     DEFINE RESONANCE INDEPENDENT QUANTITIES.
C
c-----------------------------------------------------------------------
      RHO2=E*RHOX2(ISECT)
      RHOP=RHOP1(ISECT)*DSQRT(E)
C-----DEFINE PENETRATION FACTOR, SHIFT FACTOR, PHASE SHIFT.
      CALL FACTS2(LVALUE(ISECT),RHO2,SF2,PF)
      CALL FACPHI(LVALUE(ISECT),RHOP,PS)
      COSPS=DCOS(PS)
      SINPS=DSIN(PS)
C-----DEFINE SIN(2*PS) AND 2*SIN(PS)**2
      SIN2PS=TWO*SINPS*COSPS
      SINPS2=TWO*SINPS*SINPS
C-----DEFINE PENETRATION FACTOR FOR COMPETITIVE WIDTH.
      IF(LRXTAB(ISECT).LE.0) GO TO 10
      RHOZ2=(E+QVALUE(ISECT))*RHOX2(ISECT)
c-----10/10/10 - switched from FACTS2 to FACTS3 - shift not used
      CALL FACTS3(LRXTAB(ISECT)-1,RHOZ2,PFC)
      GO TO 20
   10 PFC=0.0d0
c-----------------------------------------------------------------------
C
C     J VALUE LOOP.
C
c-----------------------------------------------------------------------
C-----DEFINE STATISTICAL WEIGHT FOR ALL RESONANCES WITH THE SAME (L,J).
   20 GJ=RESTAB(2,LOW)
      AJNOW = RESJTAB(LOW)
C-----INITIALIZE CONTRIBUTION OF (L,J) SEQUENCE.
      SIGNN1=ZERO
      SIGNN2=ZERO
      SIGNGJ=ZERO
      SIGNFJ=ZERO
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP. ADD CONTRIBUTION OF ALL RESONANCES WITH SAME (L,J)
C
c-----------------------------------------------------------------------
      DO 30 JR=LOW,LHI
C-----ONLY USE RESONANCES WITH SAME J VALUE.
      IF(DABS(AJNOW-RESJTAB(JR)).GT.0.01d0) GO TO 40
      ER=ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 30  ! Note, DABS to only get = 0
      GAMC=PFC*RESTAB(3,JR)
      GN=RESTAB(4,JR)
      GAMN=PF*GN
      GAMG=RESTAB(5,JR)
      GAMF=RESTAB(6,JR)
      DE=E-(ER+GN*(SHIFT2(JR)-SF2))
      GAMT2=HALF*(GAMN+GAMG+GAMF+GAMC)
      DEN=DE*DE+GAMT2*GAMT2
      COMFAC=GAMN/DEN
      SIGNN1=SIGNN1+COMFAC*GAMT2
      SIGNN2=SIGNN2+COMFAC*DE
      SIGNGJ=SIGNGJ+COMFAC*GAMG
      SIGNFJ=SIGNFJ+COMFAC*GAMF
C-----END OF RESONANCE LOOP.
   30 CONTINUE
      JR=LHI+1
C-----DEFINE NEUTRON CROSS SECTION, MULTIPLY BY STATISTICAL WEIGHT AND
C-----ADD CONTRIBUTION OF (L,J) SEQUENCE TO SUM.
   40 SIGNNI=SIGNNI+GJ*((SINPS2-SIGNN1)**2+(SIN2PS+SIGNN2)**2)
      SIGNGI=SIGNGI+GJ*SIGNGJ
      SIGNFI=SIGNFI+GJ*SIGNFJ
C-----TEST FOR ANOTHER J VALUE.
      LOW=JR
      IF(LOW.LE.LHI) GO TO 20
c-----------------------------------------------------------------------
C
C     ADD MISSING SEQUENCES TO POTENTIAL
C
c-----------------------------------------------------------------------
C-----ADD 2*GJ*[2*SIN(PS)**2] (NOTE, SINPS2 IS 2*SIN(PS)**2)
      SIGNNI=SIGNNI+ADDL(ISECT)*SINPS2
c***** DEBUG
c     SIGNLI=SIGNLI+ADDL(ISECT)*SINPS2
c***** DEBUG
      RETURN
      END
      SUBROUTINE SIGRM1(E)
C=======================================================================
C
C     ADD CONTRIBUTION OF ONE SECTION OF REICH-MOORE PARAMETERS.
C
C     DEFINITIONS FROM ENDF-102 FORMATS AND PROCEDURES MANUAL
C     =======================================================
C     THE CROSS SECTIONS ARE DEFINED TO BE,
C
C     TOTAL        =2*GJ*REAL(I - U(N,N))
C     ABSORPTION   =4*GJ*(REAL(RHO(N,N)) - RHO(N,N)**2)
C                  =  GJ*(I - U(N,N)**2)
C     ELASTIC      =  GJ*(I - U(N,N))**2
C     FISSION      =4*GJ*(SUM OVER C)(RHO(N,C)**2)
C     CAPTURE      = ABSORPTION - FISSION
C
C     WHICH ARE COMPLETELY DEFINED IN TERMS OF U(N,N) AND RHO(N,C),
C
C     RHO(N,C)     =I - INVERSE(I - K)
C     U(N,N)       =EXP(-I*2*PS)*(2*INVERSE(I - K) - I)
C                  =(COS(2*PS) - I*SIN(2*PS))*(2*INVERSE(I - K) - I)
C                  =(COS(2*PS) - I*SIN(2*PS))*(1 - 2*RHO(N,N))
C
C                  =COS(2*PS)*(I-2*REAL(RHO)) - I*2*SIN(2*PS)*IM(RHO)
C
C     REAL(U(N,N)) =   COS(2*PS)*(I-2*REAL(RHO))
C     IM(U(N,N))   =-2*SIN(2*PS)*IM(RHO)
C
C     MATRIX ELEMENTS
C     =======================================================
C
C                     I*SQRT(GAM(C)/2*GAM(C*)/2)
C     (I - K)    =I - ==============================================
C                     ((ER-E) -I*(GAM(R)/2)
C
C                      I*SQRT(GAM(C)/2*GAM(C*)/2)*((ER-E) + I*(GAM(R)/2)
C                =I - ==================================================
C                     ((ER-E) -I*(GAM(R)/2)*((ER-E) + I*(GAM(R)/2)
C
C                     SQRT(GAM(C)/2*GAM(C*)/2)*(GAM(R)/2)
C                =I + ==============================================
C                     ((ER-E)**2 + (GAM(R)/2)**2)
C
C                     SQRT(GAM(C)/2*GAM(C*)/2)*(ER-E)
C                =  - ==============================================
C                     ((ER-E)**2 + (GAM(R)/2)**2)
C
C     (I - K)      = (R + I) - I*S
C
C     R            = SQRT(GAM(C)/2*GAM(C*)/2)*(GAM/2)/DEN
C     S            = SQRT(GAM(C)/2*GAM(C*)/2)*(ER-E)/DEN
C     GAM(R)       = ELIMINATED RADIATIVE WIDTH
C     GAM(C)       = PARTIAL WIDE FOR CHANNEL C FOR A RESONANCE
C     DEN          = ((ER - E)**2 + (GAM/2)**2)
C
C     SUMMED OVER RESONANCES FOR EACH (L,J) SEQUENCE.
C
C     PHYSICALLY (R) IS THE SYMMETRIC CONTRIBUTION OF THE RESONANCES
C     AND IS ALWAYS POSITIVE. SIMILARLY (S) IS THE ANTI-SYMMETRIC
C     CONTRIBUTION OF THE RESONANCES AND IS NEGATIVE FOR ENERGIES
C     LESS THAN THE RESONANCE AND POSITIVE FOR ENERGIES GREATER THAN
C     THE RESONANCE.
C
C     NOTE, IN ENDF-102 THE 2 VARIABLES DESCRIBED CORRESPOND TO (R+I)
C     AND (S). IN ORDER TO IMPROVE NUMERICAL STABILITY AND SIMPLIFY
C     THE RESULTS THE FOLLOWING EQUATIONS WILL BE DEFINED IN TERMS OF
C     (R) AND (S), SO THAT THE SOLUTION CAN BE DEFINED DIRECTLY IN
C     TERMS OF THE SYMMETRIC AND ANTI-SYMMETRIC RESONANCE CONTRIBUTIONS.
C
C     SQUARES, SUCH AS RHO(N,N)**2 AND (I - U(N,N))**2, MUST BE
C     EVALUATED IN TERMS OF THE VARIABLE AND COMPLEX CONGEGATE, E.G.,
C     F(X)    =  A + B*I
C     F(X)**2 = (A + B*I)*(A - B*I) = A**2 + B**2
C
C     SO THAT,
C     RHO(N,N)**2     = (REAL(RHO(N,N))**2 + (IM(RHO(N,N))**2
C     (1 - U(N,N))**2 = (REAL(I - U(N,N))**2) + (IM(U(N,N))**2
C                     = I - 2*REAL(U(N,N))
C                     + (REAL(U(N,N))**2)+(IM(U(N,N))**2)
C                     = I - 2*REAL(U(N,N)
C                     + (U(N,N))**2
C     SOLUTION
C     =======================================================
C     INVERTING THE MATRIX
C     =======================================================
C     SOLVING FOR R' (NOT R' + I) AND S'
C     ==================================
C     I = [(R+I)+iS][(R'+I)+iS']
C     1) I = (R+I)(R'+I) - S*S'
C     2) 0 = (R+I)S' + (R'+I)S
C
C     2) (R+I)S'  = -(R'+I)S
C     2)       S' = -(R'+I)S/(R+I)
C
C     1) I = (R+I)(R'+I) + S*(R'+I)S/(R+I)
C       (R+I) = (R+I)^2(R'+I) + S^2(R'+I)
C       (R+I) = (R'+I)[(R+I)^2+S^2] ....................
C       (R+I) = [(R+I)^2+S^2] + R'[(R+I)^2+S^2]        .
C       (R+I) = [R^2+2R+I+S^2] + R'[(R+I)^2+S^2]       .
C      -[R^2+R+S^2] = R'[(R+I)^2+S^2]                  .
C      -[R(R+I)+S^2] = R'[(R+I)^2+S^2]                 .
C                                                      .
C     R' = -[R(R+I)+S^2]/[(R+I)^2+S^2]                 .
C                                                      .
C     S' = -(R'+I)S/(R+I) ..............................
C
C     S' = -S/[(R+I)^2+S^2]
C
C     IF THERE IS NO FISSION THEN R, S, RI AND SI ARE MERELY SCALARS
C     AND THE INVERSION IS ACCOMPLISHED AS DESCRIBED HERE.
C
C     IF THERE IS FISSION R, S, RI AND SI ARE 3 X 3 MATRICES AND THE
C     INVERSION IS ACCOMPLISHED BY MATRIX INVERSION.
C
C     IN EITHER CASE THE ABOVE RELATIONSHIPS ARE AT LEAST SYMBOLICALLY
C     CORRECT AND CAN BE USED TO DEFINE USEFUL PROPERTIES OF THE
C     INVERSE MATRIX. AT LEAST SYMBOLICALLY WE CAN TREAT THESE TERMS
C     AS A LINEAR SYSTEM OF 2 EQUATIONS IN 2 UNKNOWNS AND IMMEDIATELY
C     WRITE THE SOLUTION IN THE FORM,
C
C     RI      = -(R*(R+I)+S*S)/((R+I)*(R+I)+S*S)
C     SI      =  ((R+I)*S - S*R)/((R+I)*(R+I)+S*S)
C             = -S/((R+I)*(R+I)+S*S)
C
C     DEFINITION OF TERMS APPEARING IN THE CROSS SECTIONS
C     =======================================================
C     RHO(N,C)       =I - INVERSE(I - K)
C                    =I - ((RI+I)-I*SI)
C                    =-(RI - I*SI)
C     REAL(RHO(N,C)) =-RI
C     IM(RHO(N,C))   = SI
C     RHO(N,C)**2    =(RI)**2 + (SI)**2
C
C     U(N,N)         =(COS(2*PS) - I*SIN(2*PS))*(2*INVERSE(I - K) - I)
C                    =(COS(2*PS)-I*SIN(2*PS))*(2*INVERSE(I-K)-1) + I)
C                    =(COS(2*PS)-I*SIN(2*PS))*(1 - 2*RHO(N,N))
C                    =(COS(2*PS)-I*SIN(2*PS))*(1 + 2*RI - I*2*SI)
C
C                    =COS(2*PS)*(1 + 2*RI) + SIN(2*PS)*2*SI
C                    -I*(COS(2*PS)*2*SI - SIN(2*PS)*(1 + 2*RI))
C
C     REAL(U(N,N))   =COS(2*PS)*(1 + 2*RI) + SIN(2*PS)*2*SI
C     IM(U(N,N))     =-(2*COS(2*PS)*SI - SIN(2*PS)*(1 + 2*RI))
C     (U(N,N))**2    =REAL(U(N,N))**2 + IM(U(N,N))**2
C     REAL(U(N,N))**2=(COS(2*PS)**2)*(((1 + 2*RI)**2)
C                    +4*(SIN(2*PS)**2)*(SI**2)
C                    +4*COS(2*PS)*SIN(2*PS)*(1 + 2*RI)*SI
C     IM(U(N,N))**2  =(SIN(2*PS)**2)*(((1 + 2*RI)**2)
C                    +4*(COS(2*PS)**2)*(SI**2)
C                    -4*COS(2*PS)*SIN(2*PS)*(1 + 2*RI)*SI
C
C     NOTE,           4*COS(2*PS)*SIN(2*PS)*(1 + 2*RI)*SI CANCELS OUT
C                     AND COS(2*PS)**2 + SIN(2*PS)**2 = 1
C
C     (U(N,N))**2    =((1 + 2*RI)**2 +4*(SI**2)
C                    = 1 + 4*(RI + RI**2 +SI**2)
C
C     DEFINITION OF THE CROSS SECTIONS
C     =======================================================
C     TOTAL
C     =======================================================
C     IN THIS PROGRAM THE TOTAL IS DEFINED TO BE THE SUM OF ITS PART,
C
C     TOTAL = ELASTIC + ABSORPTION
C
C     HOWEVER FOR COMPLETENESS WE WILL DEFINE IT HERE.
C
C     TOTAL        =2*GJ*REAL(1 - U(N,N))
C                  =2*GJ*(1 -((COS(2*PS)*(2*RI+1)+SIN(2*PS)*2*SI)))
C                  =2*GJ*(1 - COS(2*PS)-2*(COS(2*PS)*RI-SIN(2*PS)*SI))
C                  =4*GJ*(SIN(PS)**2 - (COS(2*PS)*RI-SIN(2*PS)*SI))
C
C     HERE WE HAVE USED THE IDENTITY 1 - COS(2*PS) = 2*SIN(PS)**2
C
C     ABSORPTION
C     =======================================================
C     ABSORPTION   =4*GJ*(REAL(RHO(N,N) - RHO(N,N)**2)
C                  =4*GJ*(-RI           - (RI**2 + SI**2))
C                  =-4*GJ*(RI + RI**2 + SI**2))
C
C     FISSION
C     =======================================================
C     FISSION      =SUM RHO(N,C)**2
C
C     WHERE RHO(N,C) ARE THE OFF DIAGONAL TERMS OF THE MATRICES.
C     WRITTEN EXPLIVITLY,
C
C     FISSION      =RHO(1,2)**2 + RHO(1,3)**2
C                  =RI(1,2)**2 + RI(1,3)**2 + SI(1,2)**2 + SI(1,3)**2
C
C     CAPTURE
C     =======================================================
C     CAPTURE IS DEFINED TO BE,
C
C     CAPTURE      = ABSORPTION - FISSION
C
C     WHEN THERE IS NO FISSION CAPTURE IS EQUAL TO ABSORPTION.
C
C     ELASTIC
C     =======================================================
C     ELASTIC      =  GJ*(1 - U(N,N))**2
C
C     NOTE THAT THIS DEFINITION IS EXACTLY THE SAME AS IN THE CASE
C     OF MULTI-LEVEL BREIT-WIGNER RESONANCES, IF WE USE (RI) AND (SI)
C     INSTEAD OF (R) AND (S) (SEE, SUBROUTINE SIGBWM FOR DETAILS).
C     THEREFORE WE SHOULD NOT BE SURPRISED TO FIND A SIMILAR
C     EXPRESSION FOR THE ELASTIC CROSS SECTION,
C
C     ELASTIC=GJ*(1 - U(N,N))**2
C            =GJ*(1-2*REAL(U(N,N))+(REAL(U(N,N))**2+IM(U(N,N))**2)
C            =GJ*(1-2*(COS(2*PS)*(RI+1)-SIN(2*PS)*SI)+(RI+1)**2+SI**2)
C            =GJ*(1-2*(COS(2*PS)*(RI+1)-SIN(2*PS)*SI)+(RI+1)**2+SI**2)
C
C     BY ADDING AND SUBTRACTING TERMS THIS CAN BE WRITTEN AS THE SUM OF
C     THE SQUARE OF 2 TERMS,
C
C     ELASTIC=GJ*((COS(2*PS)-(RI+1))**2 + (SIN(2*PS)+SI)**2)
C
C            =GJ*(COS(2*PS)**2 -2*(RI+1)*COS(2*PS) + (RI+1)**2
C                +SIN(2*PS)**2 +2*SI*SIN(2*PS)     + SI**2)
C             ========================================================
C            =GJ*(1-2*(COS(2*PS)*(RI+1)-SIN(2*PS)*SI)+(RI+1)**2+SI**2)
C
C     HERE WE HAVE USED THE IDENTITY COS(2*PS)**2+SIN(2*PS)**2 = 1
C
C     THIS FORM CAN BE FURTHER SIMPLIFIED TO AVOID ROUND-OFF,
C
C     (COS(2*PS)-(RI+1))**2 = ((COS(2*PS)-1)-RI)**2
C                          = (-2*SIN(PS)**2-RI)**2
C                          = (2*SIN(PS)**2+RI)**2
C     TO FIND,
C
C     ELASTIC  = GJ*(2*SIN(PS)**2+RI)**2 + (SIN(2*PS)+SI)**2)
C
C    THIS IS THE FORM IN WHICH THE ELASTIC CROSS SECTION IS CALCULATED,
C
C    POTENTIAL CROSS SECTION
C    =======================
C    FAR FROM RESONANCES AND FOR (L,J) SEQUENCES WHICH DO NOT HAVE ANY
C    RESONANCES (RI) AND (SI) WILL BE SMALL OR ZERO AND WE FIND THAT THE
C    ELASTIC CROSS SECTION REDUCES TO THE POTENTIAL CROSS SECTION,
C
C    ELASTIC  =GJ*(4*SIN(PS)**4 + SIN(2*PS)**2)
C
C    USING THE IDENTITY SIN(2*PS) = 2*SIN(PS)*COS(PS)
C
C             =GJ*(4*SIN(PS)**4 + 4*(SIN(PS)*COS(PS))**2)
C             =4*GJ*SIN(PS)**2*(SIN(PS)**2 + COS(PS)**2)
C
C    BUT SIN(PS)**2 + COS(PS)**2 = 1, TO FIND,
C
C    ELASTIC  =4*GJ*SIN(PS)**2
C
C    IT IS IMPORTANT TO REALIZE THAT EVEN IN THE CASE WHERE THERE ARE
C    NO RESONANCES SPECIFIED FOR A (L,J) SEQUENCE THE CROSS SECTION
C    DOES NOT BECOME ZERO, SINCE THERE IS STILL A CONTRIBUTION TO THE
C    POTENTIAL CROSS SECTION.
C
C     NUMERICAL STABILITY
C     =======================================================
C     OBVIOUSLY, PHYSICALLY THE TOTAL, ELASTIC AND ABSORPTION CANNOT
C     BE NEGATIVE, AND ALL OF THE ABOVE EQUATIONS REFLECT THIS FACT.
C     HOWEVER, CARE MUST BE USED TO AVOID NUMERICAL INSTABILITY.
C
C     FROM THE DEFINITION OF ABSORPTION,
C
C     ABSORPTION   =-4*GJ*(RI + RI**2 + SI**2)
C
C     BUT BY THE ABOVE DEFINITIONS,
C
C     RI      = -(R*(R+I)+S*S)/((R+I)*(R+I)+S*S)
C     SI      = -S/((R+I)*(R+I)+S*S)
C
C     RI      = -((R+I)*(R+I)+S*S - (R+I))/((R+I)*(R+I)+*S*S)
C             = (R+I)/((R+I)*(R+I)+S*S) - I
C     RI**2   = (R+I)**2/((R+I)**2+S**2)**2)-2*(R+I)/((R+I)**2+S**2)+1
C     SI**2   =     S**2/((R+I)**2+S**2)**2)
C
C     RI**2+SI**2 = (I - 2*(R+I) + ((R+I)**2+S**2))/((R+I)**2+S**2)
C     RI          = (  +   (R+I) - ((R+I)**2+S**2))/((R+I)**2+S**2)
C     =============================================================
C     SUM         = (I -   (R+I)                  )/((R+I)**2+S**2)
C                 =-R/((R+I)**2+S**2)
C
C     ABSORPTION  =4*GJ*R/((R+I)*(R+I)+S*S)
C
C     FROM THE DEFINITION OF R WE CAN SEE THAT THIS IS MERELY THE
C     SYMMETRIC CONTRIBUTION OF THE RESONANCES, WHICH IS INHERENTLY
C     POSITIVE AND NOT SUBJECT TO ROUND-OFF ERROR. THEREFORE THIS
C     DEFINITION OF THE ABSORPTION WILL BE INHERENTLY STABLE IF WE
C     AVOID PROBLEMS BY INITIALLY DEFINING (R) RATHER THAN (R+I) AND
C     USE THIS QUANTITY TO DEFINE THE ABSORPTION (NOTE, IF WE FIRST TRY
C     TO DEFINE (R+I) IT INVOLVES ADDING I WHICH INTRODUCES ROUND-OFF
C     ERROR FAR FROM RESONANCES - IF WE SUBSEQUENTLY TRY TO DEFINE R
C     BY SUBTRACTING I WE CAN MERELY INTRODUCE MORE ROUND-OFF - HOWEVER
C     IF WE DEFINE THE QUANTITY (R) DIRECTLY WE DO NOT INTRODUCE ANY
C     ROUND-OFF).
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FISSY/LFWX,LFI,MT451,LFWSUM
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C----- 07/26/09 - EXPANDED DIMENSION FROM 3 X 3 TO 10 X 10
      DIMENSION R(10,10),S(10,10),RI(10,10),SI(10,10)
      EQUIVALENCE (RI(1,1),RI11),(SI(1,1),SI11),(RI(1,2),RI12),
     1 (SI(1,2),SI12),(RI(1,3),RI13),(SI(1,3),SI13),
     2 (R(1,1),R11),(S(1,1),S11)
c-----------------------------------------------------------------------
C
C     DEFINE RESONANCE INDEPENDENT QUANTITIES.
C
c-----------------------------------------------------------------------
      RHO2=E*RHOX2(ISECT)
      RHOP=RHOP1(ISECT)*DSQRT(E)
C-----DEFINE PENETRATION FACTOR, SHIFT FACTOR, PHASE SHIFT.
      CALL FACTS3(LVALUE(ISECT),RHO2,PF)
      CALL FACPHI(LVALUE(ISECT),RHOP,PS)
      COSPS=DCOS(PS)
      SINPS=DSIN(PS)
C-----DEFINE SIN(2*PS) AND 2*SIN(PS)**2
      SIN2PS=TWO*SINPS*COSPS
      SINPS2=TWO*SINPS*SINPS
C-----IS THIS SECTUION FISSIONABLE.
      IF(LFWL(ISECT).NE.0) GO TO 40
C-----------------------------------------------------------------------
C
C     NO FISSION.
C
C     J VALUE LOOP.
C
C-----------------------------------------------------------------------
C-----DEFINE STATISTICAL WEIGHT FOR ALL RESONANCES WITH THE SAME (L,J).
   10 GJ=RESTAB(2,LOW)
      AJNOW = RESJTAB(LOW)
C-----INITIALIZE ELEMENTS.
      R11=ZERO
      S11=ZERO
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP. ADD CONTRIBUTION OF ALL RESONANCES WITH SAME (L,J)
C
c-----------------------------------------------------------------------
      DO 20 JR=LOW,LHI
C-----ONLY USE RESONANCES WITH SAME STATISTICAL WEIGHT, I.E. J VALUE.
      IF(DABS(AJNOW-RESJTAB(JR)).GT.0.01d0) GO TO 30
      ER = ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 20  ! Note, DABS to only get = 0
      GAMN=PF*RESTAB(3,JR)
      GAMNG2=RESTAB(4,JR)
      DE = ER - E
      DEN=DE*DE+GAMNG2*GAMNG2
      DE2=DE/DEN
      GAMNG4=GAMNG2/DEN
C-----DEFINE TERMS IN UPPER TRIANGLE OF MATRIX.
      R11=R11+GAMNG4*GAMN
      S11=S11+DE2*GAMN
C-----END OF RESONANCE LOOP.
   20 CONTINUE
      JR=LHI+1
   30 DET = (R11+ONE)**2+S11**2
      SI11=-S11/DET
      RI11=-(R11*(R11+ONE)+S11**2)/DET
c-----------------------------------------------------------------------
C
C     ADD CONTRIBUTION OF CURRENT (L,J) SEQUENCE TO SUM.
C
c-----------------------------------------------------------------------
C-----ABSORPTION (IN THIS CASE ONLY CAPTURE).
      SIGNG1=-FOUR*GJ*(RI11+(RI11**2+SI11**2))
C-----ELASTIC.
      SIGNNI=SIGNNI+GJ*((SINPS2+TWO*RI11)**2+(SIN2PS+TWO*SI11)**2)
C-----CAPTURE (ABSORPTION).
      SIGNGI=SIGNGI+SIGNG1
C-----TEST FOR ANOTHER J VALUE.
      LOW=JR
      IF(LOW.LE.LHI) GO TO 10
c-----------------------------------------------------------------------
C
C     ADD MISSING SEQUENCES TO POTENTIAL
C
c-----------------------------------------------------------------------
C-----ADD 2*GJ*[2*SIN(PS)**2] (NOTE, SINPS2 IS 2*SIN(PS)**2)
      SIGNNI=SIGNNI+ADDL(ISECT)*SINPS2
c***** DEBUG
c     SIGNLI=SIGNLI+ADDL(ISECT)*SINPS2
c***** DEBUG
      RETURN
C-----------------------------------------------------------------------
C
C     FISSION
C
C     J VALUE LOOP.
C
C-----------------------------------------------------------------------
C-----DEFINE STATISTICAL WEIGHT FOR ALL RESONANCES WITH THE SAME (L,J).
   40 GJ=RESTAB(2,LOW)
      AJNOW = RESJTAB(LOW)
C-----INITIALIZE MATRICES.
      DO 60 I=1,3
      DO 50 J=1,3
      R (I,J)=ZERO
      S (I,J)=ZERO
   50 CONTINUE
   60 CONTINUE
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP. ADD CONTRIBUTION OF ALL RESONANCES WITH SAME (L,J)
C
c-----------------------------------------------------------------------
      DO 70 JR=LOW,LHI
C-----ONLY USE RESONANCES WITH SAME STATISTICAL WEIGHT, I.E. J VALUE.
      IF(DABS(AJNOW-RESJTAB(JR)).GT.0.01d0) GO TO 80
      ER = ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 70  ! Note, DABS to only get = 0
      GAMN=PF*RESTAB(3,JR)
      GAMN2=DSQRT(GAMN)
      GAMNG2=RESTAB(4,JR)
      GAMFA2=RESTAB(5,JR)
      GAMFA=GAMFA2*GAMFA2
      GAMFB2=RESTAB(6,JR)
      GAMFB=GAMFB2*GAMFB2
      DE = ER - E
      DEN=DE*DE+GAMNG2*GAMNG2
      DE2=DE/DEN
      GAMNG4=GAMNG2/DEN
      GAMNFA=GAMN2*GAMFA2
      GAMNFB=GAMN2*GAMFB2
      GAMFAB=GAMFA2*GAMFB2
C-----DEFINE TERMS IN UPPER TRIANGLE OF MATRIX.
      R(1,1)=R(1,1)+GAMNG4*GAMN
      R(1,2)=R(1,2)+GAMNG4*GAMNFA
      R(1,3)=R(1,3)+GAMNG4*GAMNFB
      R(2,2)=R(2,2)+GAMNG4*GAMFA
      R(2,3)=R(2,3)+GAMNG4*GAMFAB
      R(3,3)=R(3,3)+GAMNG4*GAMFB
      S(1,1)=S(1,1)+DE2*GAMN
      S(1,2)=S(1,2)+DE2*GAMNFA
      S(1,3)=S(1,3)+DE2*GAMNFB
      S(2,2)=S(2,2)+DE2*GAMFA
      S(2,3)=S(2,3)+DE2*GAMFAB
      S(3,3)=S(3,3)+DE2*GAMFB
C-----END OF RESONANCE LOOP.
   70 CONTINUE
      JR=LHI+1
C-----MAKE MATRICES SYMMETRIC.
   80 R(2,1)=R(1,2)
      S(2,1)=S(1,2)
      R(3,1)=R(1,3)
      S(3,1)=S(1,3)
      R(3,2)=R(2,3)
      S(3,2)=S(2,3)
C-----INVERT COMPLEX MATRIX.
C----- 2/20/10 - switch to general form if needed.
c----- 1/31/17 - Not needed = checked - both give EXACTLY same answer
      CALL FROBNS3(R,S,RI,SI)
c-----------------------------------------------------------------------
c
C      2/20/10 - switch to general form if needed.
C
C     ADD CONTRIBUTION OF CURRENT (L,J) SEQUENCE TO SUM.
C
C     (SEE, EQULVALENCE FOR DEFINITION OF RI11, SI11, RI12, SI12, RI13,
C      SI13).
c
c-----------------------------------------------------------------------
      GJ4=FOUR*GJ
C-----ABSORPTION (IN THIS CASE CAPTURE + FISSION).
      SIGNG1=-GJ4*(RI11+(RI11**2+SI11**2))
C-----ELASTIC.
      SIGNNI=SIGNNI+GJ*((SINPS2+TWO*RI11)**2+(SIN2PS+TWO*SI11)**2)
C-----FISSION.
      SIGNF1=GJ4*(RI12**2+RI13**2+SI12**2+SI13**2)
C-----CAPTURE (ABSORPTION - FISSION).
      SIGNGI=SIGNGI+(SIGNG1-SIGNF1)
C-----FISSION.
      SIGNFI=SIGNFI+SIGNF1
C-----TEST FOR ANOTHER J VALUE.
      LOW=JR
      IF(LOW.LE.LHI) GO TO 40
c-----------------------------------------------------------------------
C
C     ADD MISSING SEQUENCES TO POTENTIAL
C
c-----------------------------------------------------------------------
C-----ADD 2*GJ*[2*SIN(PS)**2] (NOTE, SINPS2 IS 2*SIN(PS)**2)
      SIGNNI=SIGNNI+ADDL(ISECT)*SINPS2
c***** DEBUG
c     SIGNLI=SIGNLI+ADDL(ISECT)*SINPS2
c***** DEBUG
      RETURN
      END
      SUBROUTINE SIGAA4(E)
C=======================================================================
C
C     ADD CONTRIBUTION OF ONE SECTION OF ADLER-ADLER PARAMETERS.
C     THIS WILL INCLUDE THE BACKGROUND CROSS SECTION PLUS ALL
C     RESONANCES, FOR ALL VALUES OF L AND J.
C
C     THIS ROUTINE USES THE ENDF/B-4 AND EARLIER DEFINITIONS OF THE
C     ADLER-ADLER PARAMETERS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DATA COMFC1/0.0d0/
C-----DEFINE ARITHMETIC STATEMENT FUNCTION FOR CALCULATION OF BACKGROUND
      BACKGR(JR)=((((RESTAB(4,JR)/E+RESTAB(3,JR))/E+RESTAB(2,JR))/E+
     1 RESTAB(1,JR))+(RESTAB(6,JR)*E+RESTAB(5,JR))*E)
C-----DEFINE ARITHMETIC STATEMENT FUNCTION FOR CALCULATION OF RESONANCE.
      RESER(GR,GI)=COMFC1*(GR*COSPS+GI*SINPS)+COMFC2*(GI*COSPS-GR*SINPS)
c-----------------------------------------------------------------------
C
C     DEFINE RESONANCE INDEPENDENT QUANTITIES.
C
c-----------------------------------------------------------------------
      ESQRT=DSQRT(E)
C-----TYPE OF BACKGROUND (NOT L VALUE).
      LI=LVALUE(ISECT)
C-----ADLER-ADLER PARAMETERS ARE ONLY DEFINED FOR L=0, IN WHICH CASE
C-----THE PHASE SHIFT IS EQUAL TO RHO (NO NEED TO CALL FACPHI).
      RHOP=RHOP1(ISECT)*DSQRT(E)
      COSPS=DCOS(RHOP)
      SINPS=DSIN(RHOP)
c-----------------------------------------------------------------------
C
C     DEFINE BACKGROUND CROSS SECTIONS.
C
c-----------------------------------------------------------------------
      GO TO (10,20,30,40,50,60,70),LI
   10 SIGNTI=BACKGR(LOW)
      GO TO 80
   20 SIGNFI=BACKGR(LOW)
      GO TO 80
   30 SIGNTI=BACKGR(LOW)
      SIGNFI=BACKGR(LOW+1)
      GO TO 80
   40 SIGNGI=BACKGR(LOW)
      GO TO 80
   50 SIGNTI=BACKGR(LOW)
      SIGNGI=BACKGR(LOW+1)
      GO TO 80
   60 SIGNFI=BACKGR(LOW)
      SIGNGI=BACKGR(LOW+1)
      GO TO 80
   70 SIGNTI=BACKGR(LOW)
      SIGNFI=BACKGR(LOW+1)
      SIGNGI=BACKGR(LOW+2)
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP.
C
c-----------------------------------------------------------------------
C-----DEFINE BASE ADDRESS FOR RESONANCE PARAMETERS.
   80 LOW=LOW+3
      DO 90 JR=LOW,LHI
      ER = ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 90 ! Note, DABS to only get = 0
C-----CONSTRAIN FIT TO USE SAME RESONANCE ENERGY AND TOTAL WIDTH FOR
C-----ALL REACTIONS (SEE ENDF/B-102 PROCEDURES MANUAL).
      DET=ER-E
      DWT=RESTAB(2,JR)
      DEN=DET*DET+DWT*DWT
      COMFC1=DWT/DEN
      COMFC2=DET/DEN
      SIGNTI=SIGNTI+RESER(RESTAB(3 ,JR),RESTAB( 4,JR))
      SIGNFI=SIGNFI+RESER(RESTAB(7 ,JR),RESTAB( 8,JR))
      SIGNGI=SIGNGI+RESER(RESTAB(11,JR),RESTAB(12,JR))
C-----END OF RESONANCE LOOP.
   90 CONTINUE
C-----MULTIPLY ALL BY THE SQUARE ROOT OF E AND ADD TERM TO TOTAL.
      SIGNTI=ESQRT*SIGNTI+TWO*(ONE-COSPS)
      SIGNFI=ESQRT*SIGNFI
      SIGNGI=ESQRT*SIGNGI
C-----DEFINE ELASTIC BY SUBTRACTION.
      SIGNNI=SIGNTI-(SIGNFI+SIGNGI)
      RETURN
      END
      SUBROUTINE SIGAA5(E)
C=======================================================================
C
C     ADD CONTRIBUTION OF ONE SECTION OF ADLER-ADLER PARAMETERS.
C     THIS WILL INCLUDE THE BACKGROUND CROSS SECTION PLUS ALL
C     RESONANCES, FOR ALL VALUES OF L AND J.
C
C     THIS ROUTINE USES THE ENDF/B-4 AND 6 DEFINITIONS OF THE
C     ADLER-ADLER PARAMETERS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C-----DEFINE ARITHMETIC STATEMENT FUNCTION FOR CALCULATION OF BACKGROUND
      BACKGR(JR)=((((RESTAB(4,JR)/E+RESTAB(3,JR))/E+RESTAB(2,JR))/E+
     1 RESTAB(1,JR))+(RESTAB(6,JR)*E+RESTAB(5,JR))*E)
c-----------------------------------------------------------------------
C
C     DEFINE RESONANCE INDEPENDENT QUANTITIES.
C
c-----------------------------------------------------------------------
      ESQRT=DSQRT(E)
C-----TYPE OF BACKGROUND (NOT L VALUE).
      LI=LVALUE(ISECT)
C-----ADLER-ADLER PARAMETERS ARE ONLY DEFINED FOR L=0, IN WHICH CASE
C-----THE PHASE SHIFT IS EQUAL TO RHO (NO NEED TO CALL FACPHI).
      RHOP=RHOP1(ISECT)*ESQRT
      COSPS=DCOS(RHOP)
      SINPS=DSIN(RHOP)
C-----INITIALIZE SUMS.
      SIGNT1=ZERO
      SIGNT2=ZERO
      SIGNT3=ZERO
      SIGNT4=ZERO
      SIGNF1=ZERO
      SIGNF2=ZERO
      SIGNG1=ZERO
      SIGNG2=ZERO
c-----------------------------------------------------------------------
C
C     DEFINE BACKGROUND CROSS SECTIONS.
C
c-----------------------------------------------------------------------
      GO TO (10,20,30,40,50,60,70),LI
   10 SIGNTI=BACKGR(LOW)
      GO TO 80
   20 SIGNFI=BACKGR(LOW)
      GO TO 80
   30 SIGNTI=BACKGR(LOW)
      SIGNFI=BACKGR(LOW+1)
      GO TO 80
   40 SIGNGI=BACKGR(LOW)
      GO TO 80
   50 SIGNTI=BACKGR(LOW)
      SIGNGI=BACKGR(LOW+1)
      GO TO 80
   60 SIGNFI=BACKGR(LOW)
      SIGNGI=BACKGR(LOW+1)
      GO TO 80
   70 SIGNTI=BACKGR(LOW)
      SIGNFI=BACKGR(LOW+1)
      SIGNGI=BACKGR(LOW+2)
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP.
C
c-----------------------------------------------------------------------
C-----DEFINE BASE ADDRESS FOR RESONANCE PARAMETERS.
   80 LOW=LOW+3
      DO 90 JR=LOW,LHI
      ER = ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 90 ! Note, DABS to only get = 0
C-----CONSTRAIN FIT TO USE SAME RESONANCE ENERGY AND TOTAL WIDTH FOR
C-----ALL REACTIONS (SEE ENDF/B-102 PROCEDURES MANUAL).
      DET=ER-E
      DWT=RESTAB(2,JR)
      GRT=RESTAB(3,JR)
      GIT=RESTAB(4,JR)
      DEN=DET*DET+DWT*DWT
      COMFC1=DWT/DEN
      COMFC2=DET/DEN
      SIGNT1=SIGNT1+COMFC1*GRT
      SIGNT2=SIGNT2+COMFC1*GIT
      SIGNT3=SIGNT3+COMFC2*GIT
      SIGNT4=SIGNT4+COMFC2*GRT
      SIGNF1=SIGNF1+COMFC1*RESTAB( 7,JR)
      SIGNF2=SIGNF2+COMFC2*RESTAB( 8,JR)
      SIGNG1=SIGNG1+COMFC1*RESTAB(11,JR)
      SIGNG2=SIGNG2+COMFC2*RESTAB(12,JR)
C-----END OF RESONANCE LOOP.
   90 CONTINUE
C-----ADD POTENTIAL TO TOTAL, MULTIPLY BY THE SQUARE ROOT OF E.
      SIGNTI=TWO*(ONE-COSPS)+ESQRT*(SIGNTI+
     1 (SIGNT1+SIGNT3)*COSPS+(SIGNT2-SIGNT4)*SINPS)
      SIGNFI=ESQRT*(SIGNFI+SIGNF1+SIGNF2)
      SIGNGI=ESQRT*(SIGNGI+SIGNG1+SIGNG2)
C-----DEFINE ELASTIC BY SUBTRACTION.
      SIGNNI=SIGNTI-(SIGNFI+SIGNGI)
      RETURN
      END
      SUBROUTINE SIGHRF(E)
C=======================================================================
C
C     ADD CONTRIBUTION OF ONE SECTION (L,S,J) OF HYBRID R-FUNCTION
C     PARAMETERS.
C
C     EACH (L,S,J) STATE WILL BE TREATED AS A SEPARATE SECTION.
C
C     THE EQUATIONS DEFINED IN ENDF-102 SAY TO USE R-MATRIX FOR THE
C     ELASTIC AND EXACTLY SINGLE LEVEL BREIT-WIGNER FOR ALL OTHER
C     REACTIONS. THIS LEADS TO AN ODD MIXTURE TO EQUATIONS IN THE
C     FOLLOWING TREATMENT.
C
C     CAPTURE AND FISSION CROSS SECTIONS ARE CALCULATED EXACTLY AS IN
C     THE CASE OF SINGLE LEVEL BREIT-WIGNER RESONANCES (FOR DETAILS
C     SEE, SUBROUTINE SIGBW1). HERE WE WILL ONLY CONSIDER THE ELASTIC
C     CROSS SECTION.
C
C     TERMS USED TO DEFINE ELASTIC CROSS SECTION
C     ==========================================
C     WE DEFINE,
C
C     RP   = RLSJ*PLSJ
C     RR0P = RR0LSJ*PLSJ
C     RI0P = RI0LSJ*PLSJ
C
C            GAM(N)
C     RP   = ========================        + (RR0P + I*RI0P)
C            2*((ER-E) - I*(GAM(R)/2)
C
C            GAM(N)*(((ER-E)+I*GAM(R)/2)
C          = ===========================     + (RR0P + I*RI0P)
C            2*((ER-E)**2+(GAM(R)/2)**2)
C
C            GAM(N)*(ER-E)+I*GAM(N)*GAM(R)/2
C          = =============================== + (RR0P + I*RI0P)
C            2*((ER-E)**2+(GAM(R)/2)**2)
C
C     RP   = RRP   + I*RIP
C
C     RRP  =(GAM(N)*(ER-E))/DEN1         + RR0P
C     RIP  =(GAM(N)*GAM(R)/2)/DEN1       + RI0P
C     DEN1 =2*((ER-E)**2+(GAM(R)/2)**2)
C
C     SUMMED OVER RESONANCES FOR EACH (L,S,J) SEQUENCE. FOLLOWING THE
C     SUM OVER RESONANCES WE DEFINE,
C
C            1 + I*PLSJ*RLSJ   1 + I*RP   (1 - RIP) + I*RRP
C     XLSJ = =============== = ========   =================
C            1 - I*PLSJ*RLSJ   1 - I*RP   (1 + RIP) - I*RRP
C
C            ((1 - RIP) + I*RRP)*((1 + RIP) + I*RRP)
C          = =======================================
C            ((1 + RIP)**2 + (RRP)**2)
C
C            1 - RIP**2 - RRP**2 + I*2*RRP
C          = =============================
C            ((1 + RIP)**2 + (RRP)**2)
C
C                  2*(RIP+ RIP**2 + RRP**2 - I*RRP)
C          = 1 -  =================================
C                 ((1 + RIP)**2 + (RRP)**2)
C
C     XLSJ = (1 - R) - I*IS
C
C     R    = 2*(RIP+((RIP)**2+(RRP)**2)/DEN2
C     S    =-2*RRP/DEN2
C     DEN2 =(1 + RIP)**2 + (RRP)**2
C
C     DEFINITION OF THE ELASTIC CROSS SECTION
C     =======================================
C     THE ELASTIC CROSS SECTION IS DEFINED TO BE,
C
C     ELASTIC      = GJ*(1 - ULSJ)**2
C
C     ULSJ         = EXP(-I*2*PS)*XLSJ
C                  = (COS(2*PS) - I*SIN(2*PS))*((R + 1) - I*S)
C
C     ELASTIC  = GJ*((2*SIN(PS)**2+R)**2 + (SIN(2*PS)+S)**2)
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C-----TEMPORARILY DEFINE REAL AND IMAGINARY BACKGROUND TO BE ZERO.
      DATA R0R/0.0d0/
      DATA R0I/0.0d0/
c-----------------------------------------------------------------------
C
C     DEFINE RESONANCE INDEPENDENT QUANTITIES.
C
c-----------------------------------------------------------------------
      GJ=GJTAB(ISECT)
      RHO2=E*RHOX2(ISECT)
      RHOP=RHOP1(ISECT)*DSQRT(E)
C-----DEFINE PENETRATION FACTOR, SHIFT FACTOR, PHASE SHIFT.
      CALL FACTS2(LVALUE(ISECT),RHO2,SF2,PF)
      CALL FACPHI(LVALUE(ISECT),RHOP,PS)
      COSPS=DCOS(PS)
      SINPS=DSIN(PS)
C-----DEFINE SIN(2*PS) AND 2*SIN(PS)**2
      SIN2PS=TWO*SINPS*COSPS
      SINPS2=TWO*SINPS*SINPS
C-----DEFINE PENETRATION FACTORS FOR COMPETITIVE WIDTHS, IF ANY.
      LRX=LRXTAB(ISECT)
C-----INITIALIZE CONTRIBUTION OF (L,S,J) SEQUENCE.
      RR=PF*R0R
      RI=PF*R0I
      SIGNGJ=ZERO
      SIGNFJ=ZERO
c-----------------------------------------------------------------------
C
C     RESONANCE LOOP. ADD CONTRIBUTION OF ALL RESONANCES WITH SAME
C     (L,S,J).
C
c-----------------------------------------------------------------------
      DO 30 JR=LOW,LHI
      ER=ENRES(JR)
C-----02/14/04 - SKIP ZERO ENERGY RESONANCES
      IF(DABS(ER).LE.0.0d0) GO TO 30 ! Note, DABS to only get = 0
C-----DEFINE NEUTRON, CAPTURE AND FISSION WIDTHS. INITIALIZE ELIMINATED
C-----WIDTH TO CAPTURE (COMPETING REACTIONS, IF ANY, WILL BE ADDED
C-----BELOW).
      DE = ER - E
      GN=RESTAB(2,JR)
      GAMN=PF*GN
      GAMG=RESTAB(3,JR)
      GAMF=RESTAB(4,JR)
      GAMER=GAMG
C-----DEFINE COMPETITIVE WIDTHS, IF ANY AND ADD TO ELIMINATED WIDTH.
      IF(LRX.LE.0) GO TO 20
C----- 5 -  8 = WIDTHS
C----- 9 - 12 = L VALUES
C-----INITIALIZE INDEX TO WIDTH AND EXIT CHANNEL L-VALUE.
      LL1=4
      LL2=8
C-----DEFINE COMPETITIVE WIDTHS AND ADD TO ELIMINATED WIDTH.
      DO 10 I=1,LRX
      LL1=LL1+1
      LL2=LL2+1
      LCOM=RESTAB(LL2,JR)
      RHOZ2=(E+EXCITE(I,ISECT))*RHOX2(ISECT)
c-----10/10/10 - switched from FACTS2 to FACTS3 - shift not used
      CALL FACTS3(LCOM,RHOZ2,PFC)
      GAMC=PFC*RESTAB(LL1,JR)
      GAMER=GAMER+GAMC
   10 CONTINUE
C-----ADD CONTRIBUTION OF RESONANCE.
   20 GAMER2=HALF*GAMER
      DE=E-(ER+GN*(SHIFT2(JR)-SF2))
C-----R-MATRIX TREATMENT FOR ELASTIC.
      DEN=DE*DE+GAMER2*GAMER2
      COMFAC=GAMN/DEN
      RR=RR+COMFAC*DE
      RI=RI+COMFAC*GAMER
C-----BREIT-WIGNER TREATMENT FOR CAPTURE AND FISSION.
      GAMT=GAMER+GAMN+GAMF
      GAMT2=HALF*GAMT
      DEN=DE*DE+GAMT2*GAMT2
      COMFAC=GAMN/DEN
      SIGNGJ=SIGNGJ+COMFAC*GAMG
      SIGNFJ=SIGNFJ+COMFAC*GAMF
C-----END OF RESONANCE LOOP.
   30 CONTINUE
C-----DEFINE 2 TERMS FOR UNIFORM TREATMENT.
      DEN2=(ONE+RI)**2+RR**2
      RI=TWO*(RI+RI**2+RR**2)/DEN2
      RR=-TWO*RR/DEN2
C-----MULTIPLY CONTRIBUTION OF (L,S,J) SEQUENCE BY STATISTICAL WEIGHT
C-----AND ADD TO SUM (NOTE, SINPS2 = 2*SIN(PS)**2).
      SIGNNI=SIGNNI+GJ*((SINPS2+RI)**2+(SIN2PS+RR)**2)
      SIGNGI=SIGNGI+GJ*SIGNGJ
      SIGNFI=SIGNFI+GJ*SIGNFJ
      RETURN
      END
      SUBROUTINE SIGGRM(E)
C=======================================================================
C
C     GENERAL R-MATRIX FORMALISM
C     ==========================
C
C     GENERAL R-MATRIX FORMALISM HAS NOT YET BEEN IMPLEMENTED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DATA IMOK/0/
      IF(IMOK.NE.0) GO TO 20
      E=0.0d0
      WRITE(OUTP,10)
      WRITE(*   ,10)
   10 FORMAT(///' ERROR - General R-Matrix Formalism has NOT yet',
     1                                        ' been Implemented'/
     2          '         Execution Terminated'///)
      CALL ENDERROR
   20 RETURN
      END
      SUBROUTINE SIGURP(E)
C=======================================================================
C
C     DEFINE CROSS SECTIONS FOR A SECTION OF UNRESOLVED RESONANCE
C     (LRU=2), ENERGY DEPENDENT PARAMETERS (LRF=2).
C
C     CROSS SECTIONS ARE DEFINED BY INTERPOLATING PARAMETERS (NOT CROSS
C     SECTIONS).
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/RANGER/LOW,LHI
      COMMON/PARAMS/RHO2,RHOP,BETAE,SF2,PF,PS,COSPS,SINPS,SIGNTI,
     1 SIGNNI,SIGNGI,SIGNFI,SIGNXI,SIGNLI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION DUMSET(6)
      EQUIVALENCE (DUMSET(2),DX),(DUMSET(3),GXX),(DUMSET(4),GNOX),
     1 (DUMSET(5),GGX),(DUMSET(6),GFX)
C-----DEFINE L VALUE.
      LNOW=LVALUE(ISECT)
C-----DEFINE INTERPOLATION LAW, STATISTICAL WEIGHT AND DEGREES OF
C-----FREEDOM FOR EACH REACTION (CAPTURE DEGREES OF FREEDOM IS ASSUMED
C-----TO BE INFINITY).
      INTX=RESTAB(1,LOW)
      GJ=RESTAB(2,LOW)
      MUX=RESTAB(3,LOW)
      MUN=RESTAB(4,LOW)
      MUF=RESTAB(6,LOW)
      AMUN=MUN
C-----FIND ENERGY OR ENERGY RANGE WHERE PARAMETERS ARE GIVEN.
      LOWP1=LOW+1
      DO 10 L=LOWP1,LHI
      IF(E.lt.ENRES(L)) go to 20
      IF(E.eq.ENRES(L)) go to 30
   10 CONTINUE
C-----EXTEND PARAMETERS AS CONSTANT OUTSIDE THEIR TABULATED ENERGY
C-----RANGE.
      L=LHI
      GO TO 30
   20 IF(L.EQ.LOWP1) GO TO 30
C-----INTERPOLATE PARAMETERS TO ENERGY E.
      CALL TERPUP(E,DUMSET,L,INTX)
      GO TO 40
C-----DEFINE PARAMETER SET AT E (E IS AN ENERGY AT WHICH PARAMETERS
C-----IS TABULATED).
   30 DX=RESTAB(2,L)
      GXX=RESTAB(3,L)
      GNOX=RESTAB(4,L)
      GGX=RESTAB(5,L)
      GFX=RESTAB(6,L)
C-----CALCULATE PENETRABILITY (VL) AND PHASE SHIFT(PS)
   40 E2=DSQRT(E)
      RHO2=E*RHOX2(ISECT)
      RHOC=E2*RHOP1(ISECT)
      CALL UNFAC(LNOW,RHO2,RHOC,AMUN,VL,PS)
C-----DEFINE NEUTRON WIDTH.
      GNX=GNOX*VL*E2
C-----CALCULATE FLUCTUATION INTEGRALS (RN, RC AND RF).
C-----9/20/10 - ADDED RX
      CALL GNRL3(GNX,GGX,GFX,GXX,MUN,MUF,MUX,RN,RC,RF,RX)
C-----DEFINE COMMON FACTOR FOR ALL REACTIONS.
      TEMP=PI2*GJ*GNX/DX
C-----DEFINE CROSS SECTIONS FOR THIS (L, J) STATE.
      SIGNNI=(RN*GNX-TWO*(DSIN(PS)**2))*TEMP
      SIGNGI=RC*GGX*TEMP
      SIGNFI=RF*GFX*TEMP
      SIGNXI=RX*GXX*TEMP ! OPTIONAL COMPETITION
C-----ADD L COMPONENT OF POTENTIAL SCATTERING DURING PASS THROUGH
C-----FIRST J STATE OF EACH L VALUE (INDICATED BY EITHER A NEW MODE
C-----MODE=RESONANCE REPRESENTATION, OR NEW L VALUE).
      IF(ISECT.EQ.1) GO TO 50
      IF(MODE(ISECT-1).EQ.MODE(ISECT).AND.
     1 LVALUE(ISECT-1).EQ.LVALUE(ISECT)) GO TO 60
   50 SIGNNI=SIGNNI+(EIGHT*FLOAT(LNOW)+FOUR)*((DSIN(PS))**2)
   60 RETURN
      END
      SUBROUTINE SIGURS(E)
C=======================================================================
C
C     CROSS SECTIONS ARE DEFINED BY INTERPOLATING CROSS SECTIONS (NOT
C     PARAMETERS). THIS ROUTINE IS PRESENTLY NOT IMPLEMENTED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DATA IMOK/0/
      IF(IMOK.NE.0) GO TO 20
      E=0.0d0
      WRITE(OUTP,10)
      WRITE(*   ,10)
   10 FORMAT(///' ERROR - Unresolved Parameter Interpolation has',
     1                                ' not yet been imlemented.'/
     2          '         Execution Terminated')
      CALL ENDERROR
   20 RETURN
      END
      SUBROUTINE FROBNS3(AM1,B,CM1,D)
C=======================================================================
C
C     3 X 3 MATRIX
C     ------------
C     STARTING FROM THE COMPLEX MATRIX (A+I)+I*B, WHERE A AND B ARE
C     SYMMETRIC, INVERT THE COMPLEX MATRIX TO DEFINE INVERSE (C+I)+I*D.
C
C     THE FROBENIUS-SCHUR METHOD IS USED TO INVERT THE COMPLEX MATRIX.
C
C     STARTING FROM THE DEFINITION OF THE COMPLEX MATRIX AND ITS INVERSE
C
C     IDENTITY = ((A+I)+I*B)*((C+I)+I*D)
C
C     WE OBTAIN 2 REAL MATRIX EQUATIONS
C
C     I       = (A+I)*(C+I)-B*D
C     0       = B*(C+I)+(A+I)*D
C
C    -A       = (A+I)*C-B*D
C    -B       = B*C+(A+I)*D
C
C     FROM THE SECOND EQUATION,
C
C     (A+I)*D = -(B+B*C) = -B*(C+I)
C     D       =-INVERSE((A+I))*(B*(C+I))
C
C     SUBSTITUTING THIS EXPRESSION FOR (D) INTO THE FIRST EQUATION.
C
C    -A       = (A+I)*C+B*INVERSE((A+I))*(B*(C+I))
C    -A       =((A+I)+B*INVERSE((A+I))*B)*C+B*INVERSE((A+I))*B
C
C     C       =-INVERSE((A+I)+B*INVERSE((A+I))*B)*(A+B*INVERSE((A+I))*B)
C     D       =-INVERSE((A+I))*B*(C+I)
C
C     THIS METHOD REQUIRES ONLY 2 MATRIX INVERSIONS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C----- 07/26/09 - EXPANDED DIMENSION FROM 3 X 3 TO 10 X 10
      DIMENSION A(10,10),B(10,10),C(10,10),D(10,10),AI(10,10),
     1 AIB(10,10),AM1(10,10),CM1(10,10),C2(10,10),C3(10,10)
C-----DEFINE A = (A-I)+I
      DO 20 I=1,3
      DO 10 J=1,3
      A(I,J)=AM1(I,J)
   10 CONTINUE
      A(I,I)=A(I,I)+ONE
   20 CONTINUE
C-----DEFINE THE INVERSE OF A = 3 X 3
      CALL COFACT3(A,AI)
C-----DEFINE INVERSE(A)*B
      DO 50 I=1,3
      DO 40 J=1,3
      SUM=ZERO
      DO 30 K=1,3
      SUM=SUM+AI(I,K)*B(K,J)
   30 CONTINUE
      AIB(I,J)=SUM
   40 CONTINUE
   50 CONTINUE
C-----STORE A+B*INVERSE(A)*B IN C AND A'+B*INVERSE(A)*B IN C2.
      DO 80 I=1,3
      DO 70 J=1,3
      SUM=ZERO
      DO 60 K=1,3
      SUM=SUM+B(I,K)*AIB(K,J)
   60 CONTINUE
      C(I,J)=A(I,J)+SUM
      C2(I,J)=AM1(I,J)+SUM
   70 CONTINUE
   80 CONTINUE
C-----DEFINE THE INVERSE OF A+B*INVERSE(A)*B
      CALL COFACT3(C,C3)
C-----DEFINE C' AND C = C' + I
      DO 110 I=1,3
      DO 100 J=1,3
      SUM=ZERO
      DO 90 K=1,3
      SUM=SUM+C3(I,K)*C2(K,J)
   90 CONTINUE
      C(I,J)=-SUM
      CM1(I,J)=-SUM
  100 CONTINUE
      C(I,I)=C(I,I)+ONE
  110 CONTINUE
C-----DEFINE D.
      DO 140 I=1,3
      DO 130 J=1,3
      SUM=ZERO
      DO 120 K=1,3
      SUM=SUM+AIB(I,K)*C(K,J)
  120 CONTINUE
      D(I,J)=-SUM
  130 CONTINUE
  140 CONTINUE
      RETURN
      END
      SUBROUTINE COFACT3(A,AI)
C=======================================================================
C
C     USE METHOD OF COFACTORS TO INVERT SYMMETRIC 3 X 3 MATRIX (A).
C     SINCE (A) IS SYMMETRIC, ITS INVERSE (AI) WILL ALSO BE
C     SYMMETRIC.
C
C=======================================================================
      INCLUDE 'implicit.h'
C-----07/26/09 - EXPANDED DIMENSION FROM 3 X 3 TO 10 X 10
      DIMENSION A(10,10),AI(10,10)
C-----DEFINE DETERMINANT (ASSUMING SYMMETRIC).
      DET1=A(2,2)*A(3,3)-A(2,3)*A(3,2)
      DET2=A(2,3)*A(3,1)-A(2,1)*A(3,3)
      DET3=A(2,1)*A(3,2)-A(2,2)*A(3,1)
      DET=A(1,1)*DET1+A(1,2)*DET2+A(1,3)*DET3
C-----DEFINE INVERSE (ASSUMING SYMMETRIC).
      AI(1,1)=DET1/DET
      AI(2,1)=DET2/DET
      AI(3,1)=DET3/DET
      AI(1,2)=AI(2,1)
      AI(2,2)=(A(1,1)*A(3,3)-A(3,1)*A(1,3))/DET
      AI(3,2)=(A(1,2)*A(3,1)-A(1,1)*A(3,2))/DET
      AI(1,3)=AI(3,1)
      AI(2,3)=AI(3,2)
      AI(3,3)=(A(1,1)*A(2,2)-A(1,2)*A(2,1))/DET
      RETURN
      END
      SUBROUTINE ORDER(LOW,LHI)
C=======================================================================
C
C     FOR EACH SECTION (I.E., SAME ISOTOPE, ENERGY RANGE, L VALUE) SORT
C     RESONANCES INTO ASCENDING (J, E) ORDER.
C
C     08/29/09 - UPDATED FOR 12 PARAMETERS PER RESONANCE
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 SWITCH
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
C-----SORT IS NOT REQUIRED IF ONLY 1 RESONANCE.
      IF(LOW.EQ.LHI) GO TO 60
      LTOP=LHI
      LOWP1=LOW+1
c-----------------------------------------------------------------------
C
C     J, E SORT.
C
c-----------------------------------------------------------------------
      DO 50 IR=LOWP1,LHI
      JRM1=LOW
      SWITCH=0
      DO 40 JR=LOWP1,LTOP
      IF(RESTAB(2,JRM1).lt.RESTAB(2,JR)) go to 30
      IF(RESTAB(2,JRM1).gt.RESTAB(2,JR)) go to 10
      IF(ENRES(JRM1).le.ENRES(JR)) go to 30
C-----EXCHANGE RESONANCES INTO J, E ORDER.
   10 SWITCH=1
      DRES=ENRES(JRM1)
      ENRES(JRM1)=ENRES(JR)
      ENRES(JR)=DRES
C----- 08/29/09 - UPDATED FOR 12 PARAMETERS PER RESONANCE
      DO 20 I=1,12
      A=RESTAB(I,JRM1)
      RESTAB(I,JRM1)=RESTAB(I,JR)
      RESTAB(I,JR)=A
   20 CONTINUE
   30 JRM1=JR
   40 CONTINUE
C-----STOP SORT IF ALL RESONANCES ARE ALREADY IN J, E ORDER.
      IF(SWITCH.LE.0) GO TO 60
C-----RESONANCE WITH LARGEST J, E IS NOW IN LAST LOCATION (LTOP).
C-----SHORTEN LENGTH OF TABLE TO CONTINUE SORT OF REMAINING RESONANCES.
      LTOP=LTOP-1
   50 CONTINUE
   60 RETURN
      END
      SUBROUTINE SORTS(X,LX)
C=======================================================================
C
C     SORT AN ARRAY INTO ASCENDING FLOATING POINT ORDER.
C
C     ARGUMENTS
C     ---------
C     X      = ARRAY TO SORT (DIMENSION LX)
C     LX     = NUMBER OF ELEMENTS TO SORT
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 SWITCH
      DIMENSION X(LX)
C-----IF LESS THAN TWO (2) ELEMENTS NO SORT IS REQUIRED.
      IF(LX.LT.2) RETURN
C-----SET INNER LOOP INDICES
      LTOP=LX+1
C-----SET UP OUTER LOOP
      DO 30 IN=2,LX
C-----INITIALIZE EXCHANGE SWITCH OFF.
      SWITCH=0
C-----SET UPPER INDEX TO INNER LOOP
      LTOP=LTOP-1
C-----SET LARGEST ELEMENT INDICATOR TO FIRST ELEMENT
      LBIG=1
C-----SET UP INNER LOOP
      DO 20 J=2,LTOP
C-----COMPARE ELEMENTS
      IF(X(LBIG).GT.X(J)) GO TO 10
C-----ELEMENTS ARE IN NUMERICAL ORDER. RESET INDEX TO LARGER ELEMENT.
      LBIG=J
      GO TO 20
C-----ELEMENTS ARE NOT IN NUMERICAL ORDER. SET INTERCHANGE SWITCH.
   10 SWITCH=1
C-----END OF INNER LOOP
   20 CONTINUE
C-----ARE ALL ELEMENTS ALREADY IN ORDER......
      IF(SWITCH.LE.0) RETURN
C-----NO. MOVE LARGEST ELEMENT TO TOP OF REMAINING TABLE
      DUMMY=X(LBIG)
      X(LBIG)=X(LTOP)
      X(LTOP)=DUMMY
   30 CONTINUE
      RETURN
      END
      SUBROUTINE SORTD(X,LX)
C=======================================================================
C
C     SORT AN ARRAY INTO ASCENDING DOUBLE PRECISION ORDER.
C
C     ARGUMENTS
C     ---------
C     X      = ARRAY TO SORT (DIMENSION LX)
C     LX     = NUMBER OF ELEMENTS TO SORT
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 SWITCH
      DIMENSION X(LX)
C-----IF LESS THAN TWO (2) ELEMENTS NO SORT IS REQUIRED.
      IF(LX.LT.2) RETURN
C-----SET INNER LOOP INDICES
      LTOP=LX+1
C-----SET UP OUTER LOOP
      DO 30 IN=2,LX
C-----INITIALIZE EXCHANGE SWITCH OFF.
      SWITCH=0
C-----SET UPPER INDEX TO INNER LOOP
      LTOP=LTOP-1
C-----SET LARGEST ELEMENT INDICATOR TO FIRST ELEMENT
      LBIG=1
C-----SET UP INNER LOOP
      DO 20 J=2,LTOP
C-----COMPARE ELEMENTS
      IF(X(LBIG).GT.X(J)) GO TO 10
C-----ELEMENTS ARE IN NUMERICAL ORDER. RESET INDEX TO LARGER ELEMENT.
      LBIG=J
      GO TO 20
C-----ELEMENTS ARE NOT IN NUMERICAL ORDER. SET INTERCHANGE SWITCH.
   10 SWITCH=1
C-----END OF INNER LOOP
   20 CONTINUE
C-----ARE ALL ELEMENTS ALREADY IN ORDER......
      IF(SWITCH.LE.0) RETURN
C-----NO. MOVE LARGEST ELEMENT TO TOP OF REMAINING TABLE
      DUMMY=X(LBIG)
      X(LBIG)=X(LTOP)
      X(LTOP)=DUMMY
   30 CONTINUE
      RETURN
      END
      SUBROUTINE SUBINT
C=======================================================================
C
C     11/01/14 - UPDATED FOR ENERGY DEPENDENT STEPS
C     1) 1 UP TO 0.1 KEV
C     2) 2 UP TO   1 KEV
C     3) 4 ABOVE   1 KEV
C     WARNING - TO WORK CORRECTLY (ITEMS=256) MUST BE A MULTIPLE OF 4.
C     ==================================================================
C     DEFINE SUBINTERVALS BETWEEN NODES ACCORDING TO THE WIDTH OF
C     THE TWO ADJACENT RESONANCES. THIS ROUTINE HAS A BUILT-IN TABLE
C     OF MULTIPLES OF HALF-WIDTHS REQUIRED TO FIT A SIMPLE BREIT-WIGNER
C     LINE SPACE TO WITHIN 1.0 PER-CENT ACCURACY OVER THE RANGE 0 TO
C     500 HALF-WIDTHS,
C
C     SIGMA(X)=1.0/(1.0+X*X)
C
C     WHERE X IS THE DISTANCE PEAK IN HALF-WIDTHS
C
C     STARTING FROM THE LOWER ENERGY END OF AN INTERVAL AND USING
C     SUB-INTERVALS BASED UPON THE WIDTH AT THE LOWER ENERGY END, NODES
C     ARE INSERTED AT SUCESSIVELY HIGHER ENERGIES UNTIL A NODE IS
C     CLOSER (IN HALF-WIDTH UNITS) TO THE UPPER ENERGY END OF THE
C     INTERVAL. THEN USING SUB-INTERVALS BASED UPON THE WIDTH AT THE
C     UPPER ENERGY, NODES ARE INSERTED UP TO THE UPPER ENERGY END OF
C     THE INTERVAL.
C
C     WITH THIS ALOGORITHM CLOSELY SPACED RESONANCES WILL HAVE ONLY
C     A FEW SUB-INTERVALS PER INTERVAL (E.G. U-235). WIDELY SPACED
C     RESONANCES WILL HAVE MORE SUB-INTERVALS PER INTERVAL (E.G. U-238).
C     FOR A MIX OF S, P, D ETC. RESONANCES THIS ALOGORITHM GUARANTEES
C     AN ADEQUTE DESCRIPTION OF THE PROFILE OF EVEN EXTREMELY NARROW
C     RESONANCES (WHICH MAY IMMEDIATELY CONVERGENCE TO THE ACCURACY
C     REQUESTED, THUS MINIMIZING ITERATION).
C
C     BE SURE ALL NODES ARE INCORE10 = PREVENT REPEAT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/SUBS/ESUB(3000),ENODP,ENODM,WIDP,WIDM,ISUB,NSUB
c-----2020/12/15 - times.h = Starting grid
      INCLUDE 'recent.times.h'
c-----2020/12/17 - Default Node Spacing
      DATA DNODE/1.1d0/
c-----2021/01/08 - NSTEP = step size in TIMES table
      DATA NSTEP/4/
c***** DEBUG
c-----------------------------------------------------------------------
C
C     On first pass check step order.
C     Activate the below if you change the above TIMES table.
C
c-----------------------------------------------------------------------
c     data IPASS/0/
c     if(IPASS.eq.0) then
c     do i=1,NTIMES
c     if(TIMES(i).le.TIMES(i-1)) then
c     write(*,9999) i,TIMES(i-1),TIMES(i)
c9999 format(' Step size ERROR=',i5,1p2d12.5)
c     stop
c     endif
c     enddo
c     IPASS = 1
c     endif
c***** DEBUG
c-----------------------------------------------------------------------
c
c     Hanlet Widths at BOTH Ends of energy interval ENODM to ENODP
c
c     4 cases - Width at each end = 0 or > 0
c
c-----------------------------------------------------------------------
C-----START FIRST SUBINTERVAL AT BEGINNING OF INTERVAL.
      NSUB=1
      ESUB(1)=ENODM
      CALL INCORE10(ESUB(1))
      IF(WIDM.gt.0.0d0) go to 70   ! Lower Width
      IF(WIDP.le.0.0d0) go to 50   ! Upper Width
c-----------------------------------------------------------------------
c
c     Lower WIDTH = 0 - space by 1.1 (DN defined above)
c     Upper WIDTH > 0 - Use TIMES table in range ENODM to ENODP
c
c-----------------------------------------------------------------------
c
c     Initial Positions both inside interval ENODM to ENODP
c
c-----Position LOWER in range (just above ENODM)
      EP = ENODM*DNODE
c-----position UPPER in range (just above ENODM)
      do IM = NTIMES,0,-NSTEP
      EM = ENODP - WIDP*TIMES(IM)
      if(EM.gt.ENODM) go to 10
      enddo
      go to 130                    ! Discontinuty ENODM = ENODP
c
c     Iteration Loop
c
c-----pick lower
   10 if(EP.lt.EM) go to 20         ! Use EP
      if(EM.lt.EP) go to 30         ! Use EM
c-----Use BOTH
      IM = IM-NSTEP                 ! Use BOTH
      if(IM.lt.0) go to 130         ! IM=0: EM=ENODP ===> finished
      EM = ENODP - WIDP*TIMES(IM)
   20 EN = EP                       ! Use EP
      EP = EP*DNODE
      go to 40
c-----Use EM
   30 EN = EM                       ! Use EM
      IM = IM - NSTEP
      if(IM.lt.0) go to 130         ! IM=0: EM=ENODP ===> finished
      EM = ENODP - WIDP*TIMES(IM)
c-----Use EN
   40 CALL INCORE10(EN)
      if(EN.gt.ESUB(NSUB)) then
      NSUB = NSUB + 1
      ESUB(NSUB) = EN
      endif
      go to 10
c-----------------------------------------------------------------------
c
c     Lower WIDTH = 0
c     Upper WIDTH = 0
c
c     Return only ends or at low E space point by 1.1
c
c-----------------------------------------------------------------------
   50 if(ENODP.gt.1.0d-1) go to 130   ! at Higher E = Return only ends
      EP = ENODM*DNODE                ! Space points
   60 if(EP.ge.ENODP) go to 130       ! from ENODM up to ENODP
      EN   = EP
      CALL INCORE10(EN)
      if(EN.gt.ESUB(NSUB)) then
      NSUB = NSUB + 1
      ESUB(NSUB) = EN
      endif
      EP   = EP*DNODE
      go to 60
c-----------------------------------------------------------------------
c
c     Lower WIDTH > 0
c     Upper WIDTH = 0
c
c     Space Points from Table from ENODM up to ENODP
c
c-----------------------------------------------------------------------
   70 IF(WIDP.gt.0.0d0) go to 80
      do IP=1,NTIMES,NSTEP
      EP = ENODM + TIMES(IP)*WIDM
      if(EP.ge.ENODP) go to 130
      EN   = EP
      CALL INCORE10(EN)
      if(EN.gt.ESUB(NSUB)) then
      NSUB = NSUB + 1
      ESUB(NSUB) = EN
      endif
      enddo
      go to 130
c-----------------------------------------------------------------------
c
c     Lower WIDTH > 0
c     Upper WIDTH > 0
c
c     Insert points from table from both ends
c
c-----------------------------------------------------------------------
c-----Start of LOWER end of range
   80 IP = 1
      EP = ENODM + WIDM*TIMES(IP)
c-----position UPPER in range
      do IM=NTIMES,0,-NSTEP
      EM = ENODP - WIDP*TIMES(IM)
      if(EM.gt.ENODM) go to 90
      enddo
      go to 130                     ! Discontinuity ENODM = ENODP
c
c     Iteration Loop
c
c-----pick lower
   90 if(EP.lt.EM) go to 100        ! Use EP
      if(EM.lt.EP) go to 110        ! Use EM
c-----Use BOTH
      IM = IM-NSTEP                 ! Use BOTH
      if(IM.le.0) go to 130         ! IM=0: EM=ENODP ===> finished
      EM = ENODP - WIDP*TIMES(IM)
  100 EN = EP                       ! Use EP
      IP = IP + NSTEP
      if(IP.le.NTIMES) then         ! From tsble or beyond UPPER end
      EP = ENODM + WIDM*TIMES(IP)
      else
      EP = ENODP + ENODP
      endif
      go to 120
  110 EN = EM                       ! Use EM
      IM = IM - NSTEP
      if(IM.le.0) go to 130         ! IM=0: EM=ENODP ===> finished
      EM = ENODP - WIDP*TIMES(IM)
c-----Use EN
  120 CALL INCORE10(EN)
      if(EN.gt.ESUB(NSUB)) then
      NSUB = NSUB + 1
      ESUB(NSUB) = EN
      endif
      go to 90
c-----------------------------------------------------------------------
c
c     End of ALL cases
c
c-----------------------------------------------------------------------
C-----ADD ENDPOINT AS NODE.
  130 if(ENODP.gt.ESUB(NSUB)) then
      NSUB=NSUB + 1
      ESUB(NSUB)=ENODP
      CALL INCORE10(ESUB(NSUB))
      endif
      RETURN
      END
      SUBROUTINE NOODLE(ERES,WID,ELOW,EHIGH)
C=======================================================================
C
C     CORE ALLOCATION FOR NODES.
C
C     SAVE ENERGY (ERES) AND WIDTH (WID) AS NODE IN ASCENDING ENERGY
C     ORDER IF IT IS IN THE ENERGY RANGE (ELOW,EHIGH). OTHERWISE IGNOR.
C
C     IF CORE ALLOCATION IS EXCEEDED TERMINATE UNLESS IN EDIT MODE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE,ADDNOD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/ETABS/NODES
      COMMON/MAXIE/NEDSEC,NEDRES,NEDNOD
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DATA ADDNOD/0/
C-----ONLY USE NODES IN RESONANCE REGION.
      IF(ERES.LT.ELOW.OR.ERES.GT.EHIGH) GO TO 90
C-----IF NO OTHER NODES YET SAVE FIRST NODE.
      IF(NODES.LE.0) GO TO 40
C-----COMPARE TO ALL NODES SAVED SO FAR.
      DO 10 I=1,NODES
      IF(ERES.lt.ENODE(I)) go to 20
      IF(ERES.gt.ENODE(I)) go to 10
C-----SAME AS PREVIOUS ENERGY. ADD WIDTHS AND RETURN.
      WIDNOD(I)=WIDNOD(I)+WID
      GO TO 90
   10 CONTINUE
C-----NEW NODE TO ADD TO END OF TABLE.
      GO TO 40
C-----MOVE ALL FOLLOWING NODES BACK ONE LOCATION IN TABLE AND INSERT
C-----NODE IN ENERGY ORDER.
   20 NN=NODES+1
      IF((NN+ADDNOD).GT.NEDNOD) NEDNOD=NN+ADDNOD
      IF(NN.GT.MAXRES) GO TO 60
      DO 30 J=I,NODES
      ENODE(NN)=ENODE(NN-1)
      WIDNOD(NN)=WIDNOD(NN-1)
      NN=NN-1
   30 CONTINUE
      ENODE(I)=ERES
      WIDNOD(I)=WID
      NODES=NODES+1
      GO TO 90
C-----ADD NEW NODE TO END OF TABLE.
   40 NODES=NODES+1
      IF((NODES+ADDNOD).GT.NEDNOD) NEDNOD=NODES+ADDNOD
      IF(NODES.GT.MAXRES) GO TO 60
   50 ENODE(NODES)=ERES
      WIDNOD(NODES)=WID
      GO TO 90
C-----TOO MANY NODES. PRINT ERROR MESSAGE AND TERMINATE UNLESS IN EDIT
C-----MODE.
   60 IF(IMEDIT.NE.2) GO TO 70
      ADDNOD=ADDNOD+MAXRES
      NODES=1
      GO TO 50
   70 WRITE(OUTP,80) MAXRES
      WRITE(*   ,80) MAXRES
   80 FORMAT(///' ERROR - More than',I5,' Nodes'/
     1          '         Re-Run Program in Edit Mode to Determine',
     2                                          ' Memory Requirements'/
     3          '         Execution Terminated'///)
      CALL ENDERROR
c***** DEBUG
c  90 write(3,3333) NODES,ERES,ENODE(NODES),ELOW,EHIGH
c3333 format(' n/e/low/hi=',i8,1p4d12.5)
c     RETURN
c***** DEBUG
   90 RETURN
      END
      SUBROUTINE FACTS2(L,RHO2,SF2,PF)
C=======================================================================
C
C     CALCULATE 1/2 THE SHIFT FACTOR (SF2) AND PENETRATION FACTOR (PF)
C     FROM L VALUE AND RHO2 (RHO2=RHO*RHO).
C
C     THE SHIFT FACTOR IS USED TO DEFINE THE ENERGY DEPENDENT RESONANCE
C     PEAK ENERGY IN THE FORM
C
C     ERP=ER+0.5*GAMN*(SF(ABS(ER))-SF(E))
C
C     IN ORDER TO AVOID HAVING TO MULTIPLY BY 0.5 EVERYTIME THE SHIFT
C     FACTOR IS USED THIS ROUTINE WILL RETURN 1/2 THE SHIFT FACTOR.
C     THIS IS DONE SIMPLY BY DEFINING ALL CONSTANTS WHICH ARE USED TO
C     DERIVE THE SHIFT FACTOR TO BE 1/2 THERE NORMAL DEFINITION (THUS
C     COMPLETELY AVOIDING THE MULTIPLICATION BY 1/2).
C
C     FOR NEGATIVE ENERGY PENETRATION AND SHIFT FACTORS ARE SET EQUAL
C     TO ZERO (SEE SUMMARY OF CSEWG MEETING, MAY 16-17, 1979
C     RESONANCE SUB-COMMITTEE REPORT). THIS CAN OCCUR FOR THE
C     COMPETITIVE WIDTH AT ENERGIES BELOW THE COMPETITIVE THRESHOLD.
C
C     04/29/04 - ADDED EXTENSION ABOVE L = 5 TO INFINITY
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA DNINE/9.0d0/
      DATA CA/1.5d0/
      DATA CB/2.25d2/
      DATA C3/4.5d1/
      DATA C4/6.0d0/
      DATA C5/3.375d2/
      DATA C6/3.0d0/
      DATA C7/1.1025d4/
      DATA C8/1.575d3/
      DATA C9/1.35d2/
      DATA C10/1.0d1/
      DATA C11/2.2050d4/
      DATA C12/2.3625d3/
      DATA C13/1.35d2/
      DATA C14/5.0d0/
      DATA C15/8.93025d5/
      DATA C16/9.9225d4/
      DATA C17/6.3d3/
      DATA C18/3.15d2/
      DATA C19/1.5d1/
      DATA C20/2.2325625d6/
      DATA C21/1.9845d5/
      DATA C22/9.45d3/
      DATA C23/3.15d2/
      DATA C24/7.5d0/
      IF(RHO2.GT.ZERO) GO TO 10
      SF2=ZERO
      PF=ZERO
      RETURN
   10 RHO=DSQRT(RHO2)
      IF(L.GT.5) GO TO 80
      GO TO (20,30,40,50,60,70),L+1
C-----S-WAVE (L=0)
   20 SF2=ZERO
      PF=RHO
      RETURN
C-----P-WAVE (L=1)
   30 DEN=ONE+RHO2
      PF=RHO2*RHO/DEN
      SF2=-HALF/DEN
      RETURN
C-----D-WAVE (L=2)
   40 RHO4=RHO2*RHO2
      DEN=THREE*RHO2+RHO4+DNINE
      PF=RHO4*RHO/DEN
      SF2=-(DNINE+CA*RHO2)/DEN
      RETURN
C-----F-WAVE (L=3)
   50 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      DEN=CB+C3*RHO2+C4*RHO4+RHO6
      PF=RHO6*RHO/DEN
      SF2=-(C5+C3*RHO2+C6*RHO4)/DEN
      RETURN
C-----G-WAVE (L=4)
   60 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      RHO8=RHO4*RHO4
      DEN=C7+C8*RHO2+C9*RHO4+C10*RHO6+RHO8
      PF=RHO8*RHO/DEN
      SF2=-(C11+C12*RHO2+C13*RHO4+C14*RHO6)/DEN
      RETURN
C-----(L=5)
   70 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      RHO8=RHO4*RHO4
      RHO10=RHO4*RHO6
      DEN=C15+C16*RHO2+C17*RHO4+C18*RHO6+C19*RHO8+RHO10
      PF=RHO10*RHO/DEN
      SF2=-(C20+C21*RHO2+C22*RHO4+C23*RHO6+C24*RHO8)/DEN
      RETURN
c-----------------------------------------------------------------------
C
C     (L>5)
C
c-----------------------------------------------------------------------
C-----FIRST DEFINE L = 5 = EXACTLY AS ABOVE, BUT HERE WE MUST START
C-----WITH THE SHIFT FACTOR - ABOVE 1/2 THE SHIFT FACTOR IS USED -
C-----AT THE END THIS IS CORRECTED TO RETURN 1/2 THE SHIFT FACTOR.
   80 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      RHO8=RHO4*RHO4
      RHO10=RHO4*RHO6
      DEN=C15+C16*RHO2+C17*RHO4+C18*RHO6+C19*RHO8+RHO10
      PF=RHO10*RHO/DEN
      SF=-2.0d0*(C20+C21*RHO2+C22*RHO4+C23*RHO6+C24*RHO8)/DEN
C-----THEN RECURSION TO L
      DO LL=6,L
      FLL  = LL
      PAR  = FLL-SF
      DEN  = PAR**2+PF**2
      PF   = RHO2*PF/DEN
      SF   = RHO2*PAR/DEN - FLL
      ENDDO
C-----RETURN 1/2 SHIFT FACTOR
      SF2  = SF/2.0d0
      RETURN
      END
      SUBROUTINE FACTS3(L,RHO2,PF)
C=======================================================================
C
C     SAME AS FACTS2 WITHOUT THE SHIFT FACTOR
C     REICH-MOORE DOES NOT USE SHIFT FACTOR
C     USE THIS DURING READ AND SIGMA CALCULATIONS
C
C     CALCULATE 1/2 THE SHIFT FACTOR (SF2) AND PENETRATION FACTOR (PF)
C     FROM L VALUE AND RHO2 (RHO2=RHO*RHO).
C
C     THE SHIFT FACTOR IS USED TO DEFINE THE ENERGY DEPENDENT RESONANCE
C     PEAK ENERGY IN THE FORM
C
C     ERP=ER+0.5*GAMN*(SF(ABS(ER))-SF(E))
C
C     IN ORDER TO AVOID HAVING TO MULTIPLY BY 0.5 EVERYTIME THE SHIFT
C     FACTOR IS USED THIS ROUTINE WILL RETURN 1/2 THE SHIFT FACTOR.
C     THIS IS DONE SIMPLY BY DEFINING ALL CONSTANTS WHICH ARE USED TO
C     DERIVE THE SHIFT FACTOR TO BE 1/2 THERE NORMAL DEFINITION (THUS
C     COMPLETELY AVOIDING THE MULTIPLICATION BY 1/2).
C
C     FOR NEGATIVE ENERGY PENETRATION AND SHIFT FACTORS ARE SET EQUAL
C     TO ZERO (SEE SUMMARY OF CSEWG MEETING, MAY 16-17, 1979
C     RESONANCE SUB-COMMITTEE REPORT). THIS CAN OCCUR FOR THE
C     COMPETITIVE WIDTH AT ENERGIES BELOW THE COMPETITIVE THRESHOLD.
C
C     04/29/04 - ADDED EXTENSION ABOVE L = 5 TO INFINITY
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA DNINE/9.0d0/
      DATA CB/2.25d2/
      DATA C3/4.5d1/
      DATA C4/6.0d0/
      DATA C7/1.1025d4/
      DATA C8/1.575d3/
      DATA C9/1.35d2/
      DATA C10/1.0d1/
      DATA C15/8.93025d5/
      DATA C16/9.9225d4/
      DATA C17/6.3d3/
      DATA C18/3.15d2/
      DATA C19/1.5d1/
      DATA C20/2.2325625d6/
      DATA C21/1.9845d5/
      DATA C22/9.45d3/
      DATA C23/3.15d2/
      DATA C24/7.5d0/
      IF(RHO2.GT.ZERO) GO TO 10
      PF=ZERO
      RETURN
   10 RHO=DSQRT(RHO2)
      IF(L.GT.5) GO TO 80
      GO TO (20,30,40,50,60,70),L+1
C-----S-WAVE (L=0)
   20 PF=RHO
      RETURN
C-----P-WAVE (L=1)
   30 DEN=ONE+RHO2
      PF=RHO2*RHO/DEN
      RETURN
C-----D-WAVE (L=2)
   40 RHO4=RHO2*RHO2
      DEN=THREE*RHO2+RHO4+DNINE
      PF=RHO4*RHO/DEN
      RETURN
C-----F-WAVE (L=3)
   50 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      DEN=CB+C3*RHO2+C4*RHO4+RHO6
      PF=RHO6*RHO/DEN
      RETURN
C-----G-WAVE (L=4)
   60 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      RHO8=RHO4*RHO4
      DEN=C7+C8*RHO2+C9*RHO4+C10*RHO6+RHO8
      PF=RHO8*RHO/DEN
      RETURN
C-----(L=5)
   70 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      RHO8=RHO4*RHO4
      RHO10=RHO4*RHO6
      DEN=C15+C16*RHO2+C17*RHO4+C18*RHO6+C19*RHO8+RHO10
      PF=RHO10*RHO/DEN
      RETURN
c-----------------------------------------------------------------------
C
C     (L>5)
C
c-----------------------------------------------------------------------
C-----FIRST DEFINE L = 5 = EXACTLY AS ABOVE, BUT HERE WE MUST START
C-----WITH THE SHIFT FACTOR - ABOVE 1/2 THE SHIFT FACTOR IS USED -
C-----AT THE END THIS IS CORRECTED TO RETURN 1/2 THE SHIFT FACTOR.
   80 RHO4=RHO2*RHO2
      RHO6=RHO4*RHO2
      RHO8=RHO4*RHO4
      RHO10=RHO4*RHO6
      DEN=C15+C16*RHO2+C17*RHO4+C18*RHO6+C19*RHO8+RHO10
      PF=RHO10*RHO/DEN
      SF=-200d0*(C20+C21*RHO2+C22*RHO4+C23*RHO6+C24*RHO8)/DEN
C-----THEN RECURSION TO L
      DO LL=6,L
      FLL  = LL
      PAR  = FLL-SF
      DEN  = PAR**2+PF**2
      PF   = RHO2*PF/DEN
      SF   = RHO2*PAR/DEN - FLL
      ENDDO
      RETURN
      END
      SUBROUTINE FACPHI(L,RHOP,PS)
C=======================================================================
C
C     CALCULATE PHASE SHIFT (PS) FROM L VALUE AND RHOP.
C
C     04/29/04 - ADDED EXTENSION ABOVE L = 5 TO INFINITY
C     02/20/14 - No Lower Value Cutoff (compatible with SAMRML)
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA CA/1.5d1/
      DATA CB/6.0d0/
      DATA C3/1.05d2/
      DATA C4/1.0d1/
      DATA C5/4.5d1/
      DATA C6/9.45d2/
      DATA C7/4.2d2/
C-----COPIED FROM FACT2S FOR L = 5 PENETRATION AND SHIFT FACTORS
      DATA C15/8.93025d5/
      DATA C16/9.9225d4/
      DATA C17/6.3d3/
      DATA C18/3.15d2/
      DATA C19/1.5d1/
      DATA C20/2.2325625d6/
      DATA C21/1.9845d5/
      DATA C22/9.45d3/
      DATA C23/3.15d2/
      DATA C24/7.5d0/
      IF(L.GT.5) GO TO 90
      GO TO (10,20,30,40,50,60),L+1
C-----S-WAVE (L=0)
   10 PS=RHOP
      RETURN
C-----P-WAVE (L=1)
   20 PS=RHOP-DATAN(RHOP)
      GO TO 80
C-----D-WAVE (L=2)
   30 RATIO=THREE/(THREE-RHOP*RHOP)
      GO TO 70
C-----F-WAVE (L=3)
   40 RHOP2=RHOP*RHOP
      RATIO=(CA-RHOP2)/(CA-CB*RHOP2)
      GO TO 70
C-----G-WAVE (L=4)
   50 RHOP2=RHOP*RHOP
      RATIO=(C3-C4*RHOP2)/(C3-(C5-RHOP2)*RHOP2)
      GO TO 70
C-----(L=5)
   60 RHOP2=RHOP*RHOP
      RATIO=(C6-(C3-RHOP2)*RHOP2)/(C6-(C7-CA*RHOP2)*RHOP2)
   70 PS=RHOP-DATAN(RATIO*RHOP)
   80 RETURN
c-----------------------------------------------------------------------
C
C     (L>5)
C
c-----------------------------------------------------------------------
C-----FIRST DEFINE L = 5
   90 RHOP2=RHOP*RHOP
C-----PENETRATION AND LEVEL SHIFT FACTORS
      RHOP4=RHOP2*RHOP2
      RHOP6=RHOP4*RHOP2
      RHOP8=RHOP4*RHOP4
      RHOP10=RHOP4*RHOP6
      DEN=C15+C16*RHOP2+C17*RHOP4+C18*RHOP6+C19*RHOP8+RHOP10
      PFX=RHOP10*RHOP/DEN
      SF=-2.0d0*(C20+C21*RHOP2+C22*RHOP4+C23*RHOP6+C24*RHOP8)/DEN
C-----PHASE SHIFT
      RHOP2=RHOP*RHOP
      RATIO=(C6-(C3-RHOP2)*RHOP2)/(C6-(C7-CA*RHOP2)*RHOP2)
      PS=RHOP-DATAN(RATIO*RHOP)
C-----THEN RECURSION TO L
      DO LL=6,L
      FLL  = LL
      PAR  = FLL-SF
      PS   = PS - DATAN(PFX/PAR)
      DEN  = PAR**2+PFX**2
      PFX   = RHOP2*PFX/DEN
      SF   = RHOP2*PAR/DEN - FLL
      ENDDO
      RETURN
      END
      SUBROUTINE UNFAC(L,RHO2,RHOC,AMUN,VL,PS)
C=======================================================================
C
C     CALCULATE THE PENETRABILITY FACTOR (VL) AND PHASE SHIFT (PS)
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA DNINE/9.0d0/
      LL=L+1
      GO TO (10,20,30),LL
C-----S-WAVE
   10 VL=AMUN
      PS=RHOC
      GO TO 40
C-----P-WAVE
   20 VL=AMUN*RHO2/(ONE+RHO2)
      PS=RHOC-DATAN(RHOC)
      GO TO 40
C-----D-WAVE
   30 RHO4=RHO2*RHO2
      VL=AMUN*RHO4/(DNINE+THREE*RHO2+RHO4)
      PS=RHOC-DATAN(THREE*RHOC/(THREE-RHOC*RHOC))
   40 RETURN
      END
      SUBROUTINE GNRL3(GNX,GGX,GFX,GXX,MUN,MUF,MUX,RN,RC,RF,RX)
C-----9/20/10 - ADDED RX to above argument list
C=======================================================================
C
C     CALCULATE UNRESOLVED RESONANCE FLUCTUATION FUNCTION
C     (ORIGINAL CODING FROM AVERAGE4 BY MULKI BHAT)
C     (NEW WEIGHTING SCHEME FROM MC SQUARED)
C
C     THIS ROUTINE HAS BEEN MODIFIED TO CALCULATE ELASTIC, CAPTURE
C     AND FISSION FLUCTUATION FUNCTIONS ALL DURING ONE CALL (AS
C     OPPOSED TO THE ORIGINAL VERSION WHICH CALCULATED EACH REACTION
C     SEPARATELY).
C
C     GNX, GGX, GFX AND GXX ARE THE WIDTHS FOR ELASTIC, CAPTURE,
C     FISSION AND COMPETITION. MUN, MUF AND MUX ARE THE NUMBER OF
C     DEGREES OF FREEDOM FOR ELASTIC, FISSION AND COMPETITION (INFINITE
C     NUMBER OF DEGREES ASSUMED FOR CAPTURE). RN, RC AND RF ARE THE
C     CALCULATED FLUCTUATION INTEGRALS FOR ELASTIC, CAPTURE AND FISSION
C
C     THE NUMBER OF DEGREES OF FREEDOM FOR EACH DISTRIBUTION (ELASTIC,
C     FISSION OR COMPETITION) MAY BE 1 TO 4. IF THE NUMBER OF DEGREES
C     OF FREEDOM FOR ANY DISTRIBUTION IS LESS THAN 1 OR MORE THAN 4
C     IT WILL BE TREATED AS AN INFINITE NUMBER OF DEGREES OF FREEDOM
C     (WHICH INFERS THAT THE WIDTHS ARE NOT DISTRIBUTED, BUT ARE RATHER
C     ALL EQUAL TO THE AVERAGE VALUE). THIS LAST CASE IS SIMULATED BY
C     DEFINING AN ADDITIONAL 10 POINT QUADRATURE IN WHICH THE WEIGHT
C     FOR ONE POINT IS 1.0 AND THE WEIGHT FOR ALL OTHER POINTS IS ZERO.
C     FOR THE ONE POINT OF WEIGHT 1.0 THE AVERAGE WIDTH WILL BE USED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      DIMENSION XX(5,10),WW(5,10)
      DATA XX/
     A 3.0013465D-03,1.3219203D-02,1.0004488D-03,1.3219203D-02,1.0D+0,
     1 7.8592886D-02,7.2349624D-02,2.6197629D-02,7.2349624D-02,0.0D+0,
     2 4.3282415D-01,1.9089473D-01,1.4427472D-01,1.9089473D-01,0.0D+0,
     3 1.3345267D+00,3.9528842D-01,4.4484223D-01,3.9528842D-01,0.0D+0,
     4 3.0481846D+00,7.4083443D-01,1.0160615D+00,7.4083443D-01,0.0D+0,
     5 5.8263198D+00,1.3498293D+00,1.9421066D+00,1.3498293D+00,0.0D+0,
     6 9.9452656D+00,2.5297983D+00,3.3150885D+00,2.5297983D+00,0.0D+0,
     7 1.5782128D+01,5.2384894D+00,5.2607092D+00,5.2384894D+00,0.0D+0,
     8 2.3996824D+01,1.3821772D+01,7.9989414D+00,1.3821772D+01,0.0D+0,
     9 3.6216208D+01,7.5647525D+01,1.2072069D+01,7.5647525D+01,0.0D+0/
      DATA WW/
     A 1.1120413D-01,3.3773418D-02,3.3376214D-04,1.7623788D-03,1.0D+0,
     1 2.3546798D-01,7.9932171D-02,1.8506108D-02,2.1517749D-02,0.0D+0,
     2 2.8440987D-01,1.2835937D-01,1.2309946D-01,8.0979849D-02,0.0D+0,
     3 2.2419127D-01,1.7652616D-01,2.9918923D-01,1.8797998D-01,0.0D+0,
     4 1.0967668D-01,2.1347043D-01,3.3431475D-01,3.0156335D-01,0.0D+0,
     5 3.0493789D-02,2.1154965D-01,1.7766657D-01,2.9616091D-01,0.0D+0,
     6 4.2930874D-03,1.3365186D-01,4.2695894D-02,1.0775649D-01,0.0D+0,
     7 2.5827047D-04,2.2630659D-02,4.0760575D-03,2.5171914D-03,0.0D+0,
     8 4.9031965D-06,1.6313638D-05,1.1766115D-04,8.9630388D-10,0.0D+0,
     9 1.4079206D-08,0.0000000D+00,5.0989546D-07,0.0000000D+00,0.0D+0/
C-----INITIALIZE FLUCTUATION INTEGRALS FOR ELASTIC, CAPTURE AND FISSION.
      RN=0.0d0
      RC=0.0d0
      RF=0.0d0
      RX=0.0d0  ! OPTIONAL COMPETITION
C-----INTEGRALS ARE ZERO IF NEUTRON AND CAPTURES WIDTHS ARE NOT
C-----POSITIVE.
      IF(GNX.LE.0.0d0.OR.GGX.LE.0.0d0) RETURN
C-----INSURE NUMBER OF DEGREES OF FREEDOM FOR ELASTIC WIDTHS IN O.K.
      IF(MUN.LT.1.OR.MUN.GT.4) MUN=5
C-----IS THERE FISSION.
      IF(GFX.lt.0.0d0) go to 120
      IF(GFX.gt.0.0d0) go to 50
c-----------------------------------------------------------------------
C
C     NOT FISSILE.
C
c-----------------------------------------------------------------------
C-----NO FISSION. IS THERE A COMPETITIVE WIDTH.
      IF(GXX.GT.0.0d0) GO TO 20
C-----NO COMPETITIVE WIDTH. 1-D QUADRATURE.
      DO 10 J=1,10
      XJ=XX(MUN,J)
      FACTOR=WW(MUN,J)*XJ/(GNX*XJ+GGX)
      RN=RN+XJ*FACTOR
      RC=RC+FACTOR
   10 CONTINUE
      RETURN
C-----COMPETITIVE WIDTH. 2-D QUADRATURE. INSURE NUMBER OF DEGREES OF
C-----FREEDOM FOR COMPETITIVE WIDTH IS O.K.
   20 IF(MUX.LT.1.OR.MUX.GT.4) MUX=5
      DO 40 J=1,10
      XJ=XX(MUN,J)
      WJXJ=WW(MUN,J)*XJ
      EFFJ=GNX*XJ+GGX
      DO 30 K=1,10
      XK=XX(MUX,K)
      FACTOR=WW(MUX,K)*WJXJ/(EFFJ+GXX*XK)
      RN=RN+XJ*FACTOR
      RX=RX+XK*FACTOR  ! OPTONAL COMPETITION
      RC=RC+FACTOR
   30 CONTINUE
   40 CONTINUE
      RETURN
c-----------------------------------------------------------------------
C
C     FISSILE.
C
c-----------------------------------------------------------------------
C-----FISSION PRESENT. INSURE NUMBER OF DEGREES OF FREEDOM FOR FISSION
C-----IS O.K.
   50 IF(MUF.LT.1.OR.MUF.GT.4) MUF=5
C-----IS THERE A COMPETITIVE WIDTH.
      IF(GXX.GT.0.0d0) GO TO 80
C-----NO COMPETITIVE WIDTH. 2-D QUADRATURE.
      DO 70 J=1,10
      XJ=XX(MUN,J)
      WJXJ=WW(MUN,J)*XJ
      EFFJ=GNX*XJ+GGX
      DO 60 K=1,10
      XK=XX(MUF,K)
      FACTOR=WW(MUF,K)*WJXJ/(EFFJ+GFX*XK)
      RN=RN+XJ*FACTOR
      RC=RC+FACTOR
      RF=RF+XK*FACTOR
   60 CONTINUE
   70 CONTINUE
      RETURN
C-----COMPETITIVE WIDTH. 3-D QUADRATURE. INSURE NUMBER OF DEGREES OF
C-----FREEDOM FOR COMPETITIVE WIDTH IS O.K.
   80 IF(MUX.LT.1.OR.MUX.GT.4) MUX=5
      DO 110 J=1,10
      XJ=XX(MUN,J)
      WJXJ=WW(MUN,J)*XJ
      EFFJ=GNX*XJ+GGX
      DO 100 K=1,10
      XK=XX(MUF,K)
      WKWJXJ=WW(MUF,K)*WJXJ
      EFFJK=EFFJ+GFX*XK
      DO 90 I=1,10
      XL=XX(MUX,I)
      FACTOR=WW(MUX,I)*WKWJXJ/(EFFJK+GXX*XL)
      RN=RN+XJ*FACTOR
      RX=RX+XL*FACTOR  ! OPTIONAL COMPETITION
      RC=RC+FACTOR
      RF=RF+XK*FACTOR
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
  120 RETURN
      END
      SUBROUTINE TERPUP(E,DUMSET,L,INTX)
C=======================================================================
C
C     INTERPOLATE UNRESOLVED RESONANCE PARAMETERS. RESONANCE SPACING,
C     COMPETITIVE, NEUTRON, CAPTURE AND FISSION WIDTHS ARE ALL
C     INTERPOLATED AT THE SAME TIME AND RETURNED IN LOCATIONS DUMSET(2)
C     THROUGH DUMSET(6), RESPECTIVELY.
C
C     THIS ROUTINE HAS BEEN RECODED IN ORDER TO AVOID ROUND-OFF
C     PROBLEMS ON SHORT WORD LENGTH COMPUTERS, E.G. IBM-360, 370, ETC.
C     THIS ROUTINE IS NOW SLIGHTLY LESS EFFICIENT THAN IN ITS PREVIOUS
C     FORM. HOWEVER ALL INTERPOLATION IS NOW DEFINED AS A WEIGHTED SUM
C     OF TERMS, AS OPPOSED TO THE PREVIOUS FORM WHICH USED DIFFERENCES.
C
C     THIS ROUTINE IS ONLY USED TO INTERPOLATE PARAMETERS IN THE
C     UNRESOLVED RESONANCE REGION. IN ORDER TO INSURE THAT IN ALL CASES
C     INTERPOLATION CAN BE PERFORMED, IF THE PARAMETER IS NOT POSITIVE
C     AND LOG INTERPOLATION OF THE CROSS SECTION IS SPECIFIED (I =4 OR
C     5), THE INTERPOLATION OF THE CROSS SECTION WILL BE SWITCHED TO
C     LINEAR (I=2 OR 3) AND A WARNING MESSAGE WILL BE PRINTED.
C
C     AN ILLEGAL INTERPOLATION CODE OR A NON-POSITIVE ENERGY WHERE LOG
C     ENERGY INTERPOLATION IS REQUIRED INDICATES EITHER AN ERROR IN THE
C     DATA AS IT APPEARS IN THE ENDF/B FORMAT, OR AN ERROR IN THIS
C     PROGRAM. THEREFORE ERRORS OF THIS TYPE WILL CAUSE THE PROGRAM TO
C     PRINT A WARNING MESSAGE AND TERMINATE EXECUTION.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*1 FIELD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION DUMSET(6)
C-----CHECK INTERPOLATION CODE.
      IF(INTX.LT.1.OR.INTX.GT.5) GO TO 170
C-----DEFINE INDEX TO LOWER ENERGY LIMIT.
      LM1=L-1
C-----DEFINE ENERGIES AT THE 2 ENDS OF THE INTERVAL.
      E1=ENRES(LM1)
      E2=ENRES(L)
C-----CHECK FOR ZERO LENGTH INTERVAL.
      DE12=E2-E1
      IF(DE12.EQ.0.0d0) GO TO 210
C-----SELECT INTERPOLATION LAW.
      GO TO (10,110,140,30,70),INTX
c-----------------------------------------------------------------------
C
C     HISTOGRAM. CONSTANT EQUAL TO VALUE AT LOWER ENERGY LIMIT.
C
c-----------------------------------------------------------------------
   10 DO 20 I=2,6
      DUMSET(I)=RESTAB(I,LM1)
   20 CONTINUE
      RETURN
c-----------------------------------------------------------------------
C
C     LINEAR X AND LOG Y.
C
c-----------------------------------------------------------------------
   30 WT2=(E-E1)/DE12
      WT1=ONE-WT2
      DO 60 I=2,6
C-----IF Y IS NOT POSITIVE OR CONSTANT SET FLAG AND USE LINEAR Y
C-----INTERPOLATION.
      IF(RESTAB(I,LM1).GT.0.0d0.AND.RESTAB(I,L).GT.0.0d0) GO TO 40
      IF(RESTAB(I,LM1).EQ.RESTAB(I,L)) GO TO 50
C-----LOG INTERPOLATION OF PARAMETERS IS NOT POSSIBLE. SWITCH TO LINEAR.
      GO TO 120
   40 DUMSET(I)=DEXP(WT1*DLOG(RESTAB(I,LM1))+WT2*DLOG(RESTAB(I,L)))
      GO TO 60
   50 DUMSET(I)=RESTAB(I,LM1)
   60 CONTINUE
      RETURN
c-----------------------------------------------------------------------
C
C     LOG X AND LOG Y.
C
c-----------------------------------------------------------------------
   70 IF(E1.LE.0.0d0.OR.E2.LE.0.0d0.OR.E.LE.0.0d0) GO TO 190
      WT2=DLOG(E/E1)/DLOG(E2/E1)
      WT1=ONE-WT2
      DO 100 I=2,6
C-----IF Y IS NOT POSITIVE OR CONSTANT SET FLAG AND USE LINEAR Y
C-----INTERPOLATIOM.
      IF(RESTAB(I,LM1).GT.0.0d0.AND.RESTAB(I,L).GT.0.0d0) GO TO 80
      IF(RESTAB(I,LM1).EQ.RESTAB(I,L)) GO TO 90
C-----LOG INTERPOLATION OF PARAMETERS IS NOT POSSIBLE. SWITCH TO LINEAR.
      GO TO 150
   80 DUMSET(I)=DEXP(WT1*DLOG(RESTAB(I,LM1))+WT2*DLOG(RESTAB(I,L)))
      GO TO 100
   90 DUMSET(I)=RESTAB(I,LM1)
  100 CONTINUE
      RETURN
c-----------------------------------------------------------------------
C
C     LINEAR X AND LINEAR Y.
C
c-----------------------------------------------------------------------
  110 WT2=(E-E1)/DE12
      WT1=ONE-WT2
  120 DO 130 I=2,6
      DUMSET(I)=WT1*RESTAB(I,LM1)+WT2*RESTAB(I,L)
  130 CONTINUE
      RETURN
c-----------------------------------------------------------------------
C
C     LOG X AND LINEAR Y.
C
c-----------------------------------------------------------------------
C-----INSURE ALL X VALUES ARE POSITIVE FOR LOG.
  140 IF(E1.LE.0.0d0.OR.E2.LE.0.0d0.OR.E.LE.0.0d0) GO TO 190
      WT2=DLOG(E/E1)/DLOG(E2/E1)
      WT1=ONE-WT2
  150 DO 160 I=2,6
      DUMSET(I)=WT1*RESTAB(I,LM1)+WT2*RESTAB(I,L)
  160 CONTINUE
      RETURN
C-----ILLEGAL INTERPOLATE CODE.
  170 WRITE(OUTP,180) INTX
      WRITE(*   ,180) INTX
  180 FORMAT(///' ERROR - Interpolating Unresolved Parameters.'/
     1          '         Illegal Interpolation Code =',I5,
     2                                    ' (MUST be 1 to 5).'/
     3          '         Correct Evaluated Data and Re-Run Program.'/
     4          '         Execution Terminated.'///)
      CALL ENDERROR
C-----ILLEGAL LOG ENERGY INTERPOLATION WITH NEGATIVE VALUES.
  190 CALL OUT9(E1,FIELD(1,1))
      CALL OUT9(E2,FIELD(1,2))
      CALL OUT9(E ,FIELD(1,3))
      WRITE(OUTP,200) ((FIELD(M,J),M=1,11),J=1,3)
      WRITE(*   ,200) ((FIELD(M,J),M=1,11),J=1,3)
  200 FORMAT(///' ERROR - Interpolating Unresolved Parameters.'/
     2          '         Illegal Log Energy Interpolation Using',
     2                                        ' Negative Energy.'/
     3          '         Interpolation Code=',I5,' (Cannot be 3, 5).'/
     4          '         E1,E2,E=',3(11A1,1X)/
     5          '         Correct Evaluated Data and Re-Run Program.'/
     6          '         Execution Terminated.'///)
      CALL ENDERROR
C-----ZERO LENGTH ENERGY INTERVAL.
  210 CALL OUT9(E1,FIELD(1,1))
      CALL OUT9(E2,FIELD(1,2))
      WRITE(OUTP,220) ((FIELD(M,J),M=1,11),J=1,2)
      WRITE(*   ,220) ((FIELD(M,J),M=1,11),J=1,2)
  220 FORMAT(///' ERROR - Interpolating Unresolved Parameters.'/
     1          '         Illegal Interpolation Over ZERO',
     2                                 ' Length Interval.'/
     3          '         E1,E2=',11A1,1X,11A1/
     4          '         Correct Evaluated Data and Re-Run Program.'/
     5          '         Execution Terminated.'///)
      CALL ENDERROR
      RETURN
      END
      SUBROUTINE LISPAR6(IXLOW,IX)
C=======================================================================
C
C     SAME AS LISPAR - BUT 6 PARAMETERS PER RESONANCE
C
C     READ AND LIST ALL RESONANCE PARAMETERS
C
C     PARAMETERS ARE READ IN DOUBLE PRECISION.
C     THE ENERGY AND WIDTH ARE SAVED IN DOUBLE PRECISION TO DEFINE THE
C     ENERGY GRID.
C
C     IXLOW   = STARTING INDEX TO RESONANCE PARAMETER TABLE
C     IX      = NUMBER OF WORDS TO READ
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 FIELD
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION XD1(12)
      DATA ZEROD/0.0d0/
c-----2020/7/22 - Initialize highest energy resonance
      EHIRRR = -1.0d10                    ! allow for bound < 0 eV
c-----------------------------------------------------------------------
C
C     READ PARAMETERS FOR EACH RESONANCE.
C
c-----------------------------------------------------------------------
      KXLOW=IXLOW-1
      DO 100 I1=1,IX,6
C-----READ IN DOUBLE PRECISION.
      CALL LISTIO9(XD1,6)
C-----COPY TO PARAMETER ARRAY.
      KXLOW=KXLOW+1
      ENRES(KXLOW)=XD1(1)
      DO 10 I=1,6
      RESTAB(I,KXLOW)=XD1(I)
   10 CONTINUE
c-----2020/7/22 - Keep track of highest energy resonance
      if(RESTAB(1,KXLOW).gt.EHIRRR) EHIRRR = RESTAB(1,KXLOW)
c-----------------------------------------------------------------------
C
C     SAVE RESONANCE ENERGY AS A NODE.
C
c-----------------------------------------------------------------------
      IF(LRU.EQ.2) GO TO 90
C-----RESOLVED.
      GO TO (20,20,30,40,50,60,70),LRF
C-----BREIT-WIGNER.
   20 WIDTOT=XD1(3)
      GO TO 80
C-----REICH-MOORE
   30 WIDTOT=XD1(3)+XD1(4)+DABS(XD1(5))+DABS(XD1(6))
      GO TO 80
C-----ADLER-ADLER
   40 WIDTOT=XD1(2)
      GO TO 80
C-----GENERAL-R-MATRIX (NOT IMPLEMENTED).
   50 WIDTOT=ZEROD
      GO TO 80
C-----HYBRID-R-MATRIX
   60 WIDTOT=XD1(2)+XD1(3)+XD1(4)+XD1(5)+XD1(6)+XD1(7)+XD1(8)
      GO TO 80
C-----NEW (2003) REICH-MOORE
   70 WIDTOT = DABS(XD1(2))+DABS(XD1(3))+
     1         DABS(XD1(4))+DABS(XD1(5))
C-----DEFINE RESOLVED REGION NODE.
   80 if(XD1(1).eq.EHX) then         ! Ignore resonsnce on boundary
      WIDUSE = 0.0d0
      else
      WIDUSE = WIDTOT
      endif
      CALL NOODLE(XD1(1),WIDUSE,ELX,EHX)
      GO TO 100
c-----------------------------------------------------------------------
C
C     DEFINE UNRESOLVED REGION NODE.
C
c-----------------------------------------------------------------------
   90 CALL NOODLE(XD1(1),ZEROD,ELX,EHX)
  100 CONTINUE
      RETURN
      END
      SUBROUTINE LISPAR12(IXLOW,IX)
C=======================================================================
C
C     SAME AS LISPAR - BUT 12 PARAMETERS PER RESONANCE
C
C     READ AND LIST ALL RESONANCE PARAMETERS
C
C     PARAMETERS ARE READ IN DOUBLE PRECISION.
C     THE ENERGY AND WIDTH ARE SAVED IN DOUBLE PRECISION TO DEFINE THE
C     ENERGY GRID.
C
C     IXLOW   = STARTING INDEX TO RESONANCE PARAMETER TABLE
C     IX      = NUMBER OF WORDS TO READ
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 FIELD
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/FIELDC/FIELD(11,12)
C-----2/14/04 - ADDED INCLUDE
      INCLUDE 'recent.h'
      DIMENSION XD1(12)
      DATA ZEROD/0.0d0/
c-----2020/7/22 - Initialize highest energy resonance
      EHIRRR = -1.0d10                    ! allow for bound < 0 eV
c-----------------------------------------------------------------------
C
C     READ PARAMETERS FOR EACH RESONANCE.
C
c-----------------------------------------------------------------------
      KXLOW=IXLOW-1
      DO 100 I1=1,IX,12
C-----READ IN DOUBLE PRECISION.
      CALL LISTIO9(XD1,12)
C-----COPY TO PARAMETER ARRAY.
      KXLOW=KXLOW+1
      ENRES(KXLOW)=XD1(1)
      DO 10 I=1,12
      RESTAB(I,KXLOW)=XD1(I)
   10 CONTINUE
c-----2020/7/22 - Keep track of highest energy resonance
      if(RESTAB(1,KXLOW).gt.EHIRRR) EHIRRR = RESTAB(1,KXLOW)
c-----------------------------------------------------------------------
C
C     SAVE RESONANCE ENERGY AS A NODE.
C
c-----------------------------------------------------------------------
      IF(LRU.EQ.2) GO TO 90
C-----RESOLVED.
      GO TO (20,20,30,40,50,60,70),LRF
C-----BREIT-WIGNER.
   20 WIDTOT=XD1(3)
      GO TO 80
C-----REICH-MOORE
   30 WIDTOT=XD1(3)+XD1(4)+DABS(XD1(5))+DABS(XD1(6))
      GO TO 80
C-----ADLER-ADLER
   40 WIDTOT=XD1(2)
      GO TO 80
C-----GENERAL-R-MATRIX (NOT IMPLEMENTED).
   50 WIDTOT=ZEROD
      GO TO 80
C-----HYBRID-R-MATRIX
   60 WIDTOT=XD1(2)+XD1(3)+XD1(4)+XD1(5)+XD1(6)+XD1(7)+XD1(8)
      GO TO 80
C-----NEW (2003) REICH-MOORE
   70 WIDTOT = DABS(XD1(2))+DABS(XD1(3))+
     1         DABS(XD1(4))+DABS(XD1(5))
C-----DEFINE RESOLVED REGION NODE.
   80 CALL NOODLE(XD1(1),WIDTOT,ELX,EHX)
      GO TO 100
c-----------------------------------------------------------------------
C
C     DEFINE UNRESOLVED REGION NODE.
C
c-----------------------------------------------------------------------
   90 CALL NOODLE(XD1(1),ZEROD,ELX,EHX)
  100 CONTINUE
      RETURN
      END
      SUBROUTINE FILEIO
C=======================================================================
C
C     DEFINE ALL I/O UNITS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*72 NAMEIN,NAMEOUT
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/UNITS/ISCR2,ISCR23
      COMMON/NAMEX/NAMEIN,NAMEOUT
C-----DEFINE ALL I/O UNITS.
      INP=2
      OUTP=3
      ITAPE=10
      OTAPE=11
      ISCR2=12
      ISCR23=14
C-----DEFINE ALL FILE NAMES.
      OPEN(OUTP,FILE='RECENT.LST',STATUS='UNKNOWN')
      CALL SCRATCH1(ISCR2 ,'RECENT.001  ')
      CALL SCRATCH1(ISCR23,'RECENT.002  ')
      OPEN(INP,FILE='RECENT.INP',STATUS='OLD',ERR=10)
      ISTAT1 = 0
      RETURN
   10 ISTAT1 = 1
      RETURN
      ENTRY FILIO2
C=======================================================================
C
C     DEFINE ENDF/B DATA I/O UNITS AND OPTIONALLY DEFINE FILE NAMES.
C
C=======================================================================
      OPEN(OTAPE,FILE=NAMEOUT,STATUS='UNKNOWN')
      OPEN(ITAPE,FILE=NAMEIN,STATUS='OLD',ERR=20)
      ISTAT2 = 0
      RETURN
   20 ISTAT2 = 1
      RETURN
      END
      SUBROUTINE COULOMB(RHO, LNOW, ETA, PENCOUL)
C=======================================================================
C
C     CALCULATE PENETRATION FACTOR INCLUDING COULOMB
C     ----------------------------------------------
C     04/09/10 - LIBERATED (I.E., STOLEN) FROM NANCY LARSON'S SAMMY CODE
C                MODIFIED TO ONLY CALCULATE PENETRATION FACTOR -
C                NO SHIFT FACTOR OR PHASE SHIFT.
C
C=======================================================================
      INCLUDE 'implicit.h'
      PARAMETER (MAXL=100)
      COMMON/COOLCOM/F(MAXL),FPR(MAXL),G(MAXL),GPR(MAXL),SIGCOOL(MAXL)
c-----------------------------------------------------------------------
C
C     INITIALIZE AND TEST
C
c-----------------------------------------------------------------------
      IFAIL   = 0
      PENCOUL = 0.0d0
      IF (LNOW.LT.0) RETURN
      LLMAX = LNOW + 2
      IF (LLMAX.GT.100) RETURN
c-----------------------------------------------------------------------
C
C     CALCULATE PENETRATION FACTOR
C
c-----------------------------------------------------------------------
      IF (RHO.LT.0.02d0) THEN
C-----SMALL RHO
      CALL COOL1 (ETA, RHO, LNOW, LLMAX, PENCOUL)
      ELSE
C-----LARGE RHO
      CALL COOL2 (RHO, ETA, LNOW, LLMAX, PENCOUL, IFAIL, IEXP)
C-----IF LARGE RHO BRANCH FAILS, TRY SMALL RHO
      IF (IFAIL.NE.0) THEN
      ELSE IF (IFAIL.GT.0) THEN
      CALL COOL1 (ETA, RHO, LNOW, LLMAX, PENCOUL)
      ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE COOL1 (EETA,RRHO,LNOW,LLMAX,PENCOUL)
C=======================================================================
C
C     GERRY HALE'S ROUTINE FOR SMALL RHO
C
C=======================================================================
      INCLUDE 'implicit.h'
      PARAMETER (MAXL=100)
      COMMON/COOLCOM/F(MAXL),FPR(MAXL),G(MAXL),GPR(MAXL),SIGCOOL(MAXL)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA FIVE /5.0d0/
C
      ETA = EETA
      RHO = RRHO
      LMAX = LLMAX
C
C
      IF (ETA.GT.TEN*RHO .AND. ETA.GT.FIVE) THEN
C ------------------------------------------------
C ***    HERE FOR ETA>>RHO
      CALL BIGETA (ETA, RHO, LNOW, LLMAX, PENCOUL)
C ------------------------------------------------
C
      ELSE
C
C ------------------------------------------------
C ***    HERE FOR ETA AND RHO NOT SO VERY DIFFERENT
C
      IF (ETA.GE.FIVE) THEN
C *** GENERATE U, UPR, FIRST FOR RHOI.NE.RHO
C *** I.E., USE ASYMPTOTIC FORMULA FOR LARGE ETA TO GIVE U=G0(2*ETA,ETA)
      CALL ASYMP1 (ETA, RHOI, U, UPR)
C *** NOW GENERATE TAYLOR SERIES EXPANSION OF G0(RHO) AROUND G0(RHOI),
C *** WITH RHOI REDEFINED IF NECESSARY TO OBTAIN CONVERGENCE
      CALL TAYLOR (ETA, RHO, U, UPR, RHOI)
C
      ELSE
C ***       GENERATE SIGCOOL TO USE IN ASYMPTOTIC EXPANSION FOR RHOI
      CALL XSIGLL (ETA, SIGMA0, LMAX)
C ***       USE ASYMPTOTIC FORMULA TO GIVE G0(RHOI,ETA)
      CALL ASYMP2 (ETA, RHO, U, UPR, RHOI, SIGMA0)
      IF (DABS(U).LE.1.0d25) THEN
      CALL TAYLOR (ETA, RHO, U, UPR, RHOI)
      ENDIF
      ENDIF
C
C ***    NOW FIND PENCOUL, ETC...
      G0 = U
      G0PR = UPR
      IF (DABS(G0).GT.1.D+25) THEN
      LLMAX  = 0
      G  (1) = G0
      GPR(1) = G0PR
      F  (1) = 0.0d0
      FPR(1) = 0.0d0
      ELSE
      CALL GETFG (ETA, RHO, LLMAX, LNOW, G0, G0PR)
      ENDIF
      LPLUS1  = LNOW + 1
      ASQ     = F(LPLUS1)**2 + G(LPLUS1)**2
      PENCOUL = RHO/ASQ
C
C ------------------------------------------------
C
      ENDIF
      RETURN
      END
      SUBROUTINE COOL2 (XX, ETA1, LNOW, LLMAX, PENCOUL, IFAIL, IEXP)
C=======================================================================
C
C     NANCY LARSON'S ROUTINE FOR LARGE RHO
C
C=======================================================================
      INCLUDE 'implicit.h'
      PARAMETER (MAXL=100)
      COMMON/COOLCOM/F(MAXL),FPR(MAXL),G(MAXL),GPR(MAXL),SIGCOOL(MAXL)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA TEN2/100.0d0/
      DATA TM30/1.0d-30/
      DATA ABORT/2.0d4/
C      DATA RT2EPI /0.79788 45608 02865 35587 98921 19868 76373 D0/
C *** THIS CONSTANT IS  DSQRT(TWO/PI)&  USE Q0 FOR IBM REAL*16& D0 FOR
C ***  REAL*8 + CDC DOUBLE P&  E0 FOR CDC SINGLE P; AND TRUNCATE VALUE.
C
      ACCUR = 1.0d-16
C ***            CHANGE ACCUR TO SUIT MACHINE AND PRECISION REQUIRED
C
C
      IFAIL = 0
      IEXP  = 1
      ETA   = ETA1
      GJWKB = ZERO
      PACCQ = ONE
      ACC   = ACCUR
      ACC4  = ACC*TEN2*TEN2*0.1d0
C                          *0.1d0 ADDED BY ROS 8 APR 98
      ACCH  = DSQRT(ACC)
      IF (XX.LE.ACCH) GO TO 50
      X   = XX
      XLM = ZERO
C
      E2MM1 = ETA*ETA + XLM*XLM + XLM
C           =   ETA^2 + LL(LL+1)
C
      if(X*(X-TWO*ETA) .LT. XLM*XLM + XLM) then
      IXLTURN = 1
      else
      IXLTURN = 0
      endif
C
      XLL   = DFLOAT(LLMAX)
C ***         XLL IS MAX LAMBDA VALUE, OR 0.5 SMALLER FOR J,Y BESSELS
C ***         DETERMINE STARTING ARRAY ELEMENT (1) FROM 0
      L1X = LLMAX + 1
C
C
C
C *** EVALUATE CF1 = FX  = FPRIME(XL,ETA,X)/FX(XL,ETA,X)
C
      XI  = ONE/X
      FCL = ONE
      PK  = XLL + ONE
      PX  = PK  + ABORT
   10 CONTINUE
      EK  = ETA / PK
      FX  = (EK+PK*XI)*FCL + (FCL-ONE)*XI
      PK1 =  PK + ONE
      IF (DABS(ETA*X+PK*PK1).LE.ACC) THEN
C ***    TEST ENSURES B1.NE.ZERO FOR NEGATIVE ETA; FIXUP IS EXACT.
      FCL  = (ONE+EK*EK)/(ONE+(ETA/PK1)**2)
      PK   = TWO + PK
      GO TO 10
      ENDIF
      D  = ONE / ( (PK+PK1)*(XI+EK/PK1) )
      DF = -FCL*(ONE+EK*EK)*D
      IF (FCL.NE.ONE) FCL = -ONE
      IF (D.LT.ZERO ) FCL = -FCL
      FX  = FX + DF
c-----------------------------------------------------------------------
C
C       BEGIN CF1 LOOP ON PK = K = LAMBDA + 1
C
c-----------------------------------------------------------------------
      P = ONE
   20 CONTINUE
      PK  = PK1
      PK1 = PK1 + ONE
      EK  = ETA / PK
      TK  = (PK+PK1)*(XI+EK/PK1)
      D   =  TK - D*(ONE+EK*EK)
      IF (DABS(D).LE.ACCH) THEN
      WRITE (6,30) D, DF, ACCH, PK, EK, ETA, X
   30 FORMAT(/' CF1 ACCURACY LOSS: D,DF,ACCH,K,ETA/K,ETA,X = ',
     *         1P7E9.2/)
      P = P + ONE
      IF (P.GT.TWO) GO TO 60
      ENDIF
      D = ONE/D
      IF (D.LT.ZERO) FCL = -FCL
      DF = DF*(D*TK-ONE)
      FX = FX + DF
      IF (PK.GT.PX) GO TO 60
      IF (DABS(DF).GE.DABS(FX)*ACC) GO TO 20
c-----------------------------------------------------------------------
C
C     DOWNWARD RECURRENCE TO LAMBDA=XLM. ARRAY G, IF PRESENT,STORES RL
C
c-----------------------------------------------------------------------
      IF (LLMAX.GT.0) THEN
      FCL = FCL*TM30
      FPL = FCL*FX
      FPR(L1X) = FPL
      F (L1X) = FCL
      XL  = XLL
      RL  = ONE
      ELXX   = ZERO
      DO LP=1,LLMAX
      ELXX   = ETA/XL
      RL     = DSQRT(ONE+ELXX*ELXX)
      SL     =  ELXX + XL*XI
      L      =  L1X - LP
      FCL1   = (FCL *SL+FPL)/RL
      FPL    =  FCL1*SL-FCL *RL
      FCL    =  FCL1
      F(L)  =  FCL
      FPR(L) = FPL
      G(L+1)= RL
      XL     = XL - ONE
      ENDDO
      IF (FCL.EQ.ZERO) FCL = ACC
      FX  = FPL/FCL
c-----------------------------------------------------------------------
c
C     NOW WE HAVE REACHED LAMBDA = 0
C     EVALUATE CF2 = P + I.Q  AGAIN USING STEED'S ALGORITHM
C     SEE TEXT FOR COMPACT COMPLEX CODE FOR SP CDC OR NON-ANSI IBM
c
c-----------------------------------------------------------------------
      ENDIF
C
C
      if (IXLTURN.ne.0) then
c-----2014/11/02 - corrected third argument to floating point
c-----             it was MAX(XLM,ZERO) here, but not in Subroutine
      XLMMAX = DMAX1(XLM,ZERO)
      CALL JWKB (X, ETA, XLMMAX, FJWKB, GJWKB, IEXP)
      endif
C
C
      IF (IEXP.GT.1 .OR. GJWKB.GT.ONE/(ACCH*TEN2)) THEN
c-----------------------------------------------------------------------
c
C     ARRIVE HERE IF [G(XLM).GT.10**6] OR [(IEXP.GT.250+XLTURN)=.TRUE.]
C     ARRIVE HERE IF [G(XLM).GT.10**6] OR [(IEXP.GT.70 &XLTURN)=.TRUE.]
C     IN OTHER WORDS, WHERE VALUES ARE EXTREME
c
c-----------------------------------------------------------------------
      W   = FJWKB
      GAM = GJWKB*W
      P   = FX
      Q   = ONE
C
      ELSE
C
      IXLTURN = 0
      TA =  TWO*ABORT
      PK =  ZERO
      WI =  ETA + ETA
      P  =  ZERO
      Q  =  ONE - ETA*XI
      AR = -E2MM1
      AI =  ETA
      BR =  TWO*(X-ETA)
      BI =  TWO
      DR =  BR/(BR*BR+BI*BI)
      DI = -BI/(BR*BR+BI*BI)
      DP = -XI*(AR*DI+AI*DR)
      DQ =  XI*(AR*DR-AI*DI)
   40 CONTINUE
      P  = P  + DP
      Q  = Q  + DQ
      PK = PK + TWO
      AR = AR + PK
      AI = AI + WI
      BI = BI + TWO
      D  = AR*DR - AI*DI + BR
      DI = AI*DR + AR*DI + BI
      C  = ONE/(D*D+DI*DI)
      DR =  C*D
      DI = -C*DI
      A  = BR*DR - BI*DI - ONE
      B  = BI*DR + BR*DI
      C  = DP*A  - DQ*B
      DQ = DP*B  + DQ*A
      DP = C
      IF (PK.GT.TA) GO TO 70
      IF (DABS(DP)+DABS(DQ).GE.(DABS(P)+DABS(Q))*ACC) GO TO 40
      PACCQ = HALF*ACC/DMIN1(DABS(Q),ONE)
      IF (DABS(P).GT.DABS(Q)) PACCQ = PACCQ*DABS(P)
      IF (Q.LE.ACC4*DABS(P)) GO TO 80
      GAM = (FX-P)/Q
C ***    SOLVE FOR FCM=FX AT LAMBDA=XLM, THEN FIND NORM FACTOR W=W/FCM
      W   = ONE/ DSQRT((FX-P)*GAM+Q)
C
      ENDIF
C
C
      FCM = DSIGN(W,FCL)
C ***                    = (SIGN OF FCL) * DABS(W)
      F(1) = FCM
      IF (IXLTURN.eq.0) THEN
      GCL = FCM*GAM
      ELSE
      GCL = GJWKB
      ENDIF
      G(1) = GCL
      GPL =  GCL * (P-Q/GAM)
      GPR(1) = GPL
      FPR(1) = FCM * FX
c-----------------------------------------------------------------------
C
C     UPWARD RECURRENCE FROM G(1),GPR(1)  STORED VALUE IS RL
C     RENORMALISE F,FPR AT EACH LAMBDA AND CORRECT REGULAR DERIVATIVE
C        XL   = XLM HERE  AND RL = ONE , ELXX = ZERO FOR BESSELS
C
c-----------------------------------------------------------------------
      W    = W/DABS(FCL)
      DO L=1,LLMAX
      XL = XL + ONE
      ELXX = ETA/XL
      RL = G(L+1)
      SL = ELXX + XL*XI
      GCL1     = ( SL*GCL-GPL )/RL
      GPL      =   RL*GCL - SL*GCL1
      GCL      = GCL1
      G (L+1) = GCL1
      GPR(L+1) = GPL
      FPR(L+1) = W * FPR(L+1)
      F (L+1) = W * F (L+1)
      ENDDO
c-----------------------------------------------------------------------
C
C GENERATE PENETRABILITY, SHIFT FACTOR, AND PHASE SHIFT, & DERIVATIVES
C NOTE THAT IEXP = 1 MEANS "NORMAL" VERSION WORKED...
C
c-----------------------------------------------------------------------
      L = LNOW + 1
      IF (IEXP.GT.1) THEN
      IF (IEXP.LT.150) THEN
      ASQ = G(L)**2
      AAA = TEN ** (-IEXP*2)
      PENCOUL = XX/ASQ * AAA
      ENDIF
C        NOTE WE'VE ALREADY SET DEFAULTS TO ZERO SO NO NEED TO REPEAT
      ELSE
      ASQ = F(L)**2 + G(L)**2
      PENCOUL = XX/ASQ
      ENDIF
C
      RETURN
C-----------------------------------------------------------------------
C
C     ERROR CONDITION
C
C-----------------------------------------------------------------------
   50 IFAIL = -1
      RETURN
   60 IFAIL =  1
      RETURN
   70 IFAIL =  2
      RETURN
   80 IFAIL =  3
      RETURN
      END
      SUBROUTINE JWKB (XX, ETA1, XL, FJWKB, GJWKB, IEXP)
C=======================================================================
C
C *** COMPUTES JWKB APPROXIMATIONS TO COULOMB FUNCTIONS FOR XL.GE.0
C *** AS MODIFIED BY BIEDENHARN ET.AL. PHYS REV 97 (1955) 542-554
C *** CALLS DMAX1, DSQRT, DLOG, DEXP, DATAN2, DFLOAT, INT
C *** BARNETT FEB 1981
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
      DATA ALOGE /0.434294481903251816667932d0/
C *** ALOGE IS LOG-BASE-10 OF E ==> E=DEXP(1.0D0),ALOGE=DLOG10(E)
      DATA SIX35 /0.1714285714285714285714285714285714285714285d0/
C *** SIX35 IS 6.0/35.0
C
      X     = XX
      ETA   = ETA1
      GH2   = X* (ETA+ETA-X)
      XLL1  = DMAX1 ( XL*XL + XL, ZERO )
      IF (GH2+XLL1.LE.ZERO) RETURN
      HLL = XLL1 + SIX35
      HL  = DSQRT(HLL)
      SL  = ETA/HL + HL/X
      RL2 = ONE + ETA*ETA/HLL
      GH  = DSQRT(GH2+HLL)/X
      PHI = X*GH - HALF*( HL*DLOG((GH+SL)**2/RL2) - DLOG(GH) )
      IF (ETA.NE.ZERO) PHI = PHI - ETA*DATAN2(X*GH,X-ETA)
C
      PHI10 = -PHI*ALOGE
      IEXP  = INT(PHI10)
      IF (IEXP.GT.70) THEN
      GJWKB = TEN**(PHI10-DFLOAT(IEXP))
      ELSE
      GJWKB = DEXP(-PHI)
      IEXP = 0
      ENDIF
      FJWKB = HALF/(GH*GJWKB)
      RETURN
      END
      SUBROUTINE ASYMP1 (EETA, RHOI, U, UPR)
C=======================================================================
C
C *** PURPOSE -- CALCULATE ASYMPTOTIC EXPANSION OF G0 & G0PR FROM EQS.
C ***            14.5.12B AND 14.5.13B IN ABROMOWITZ & STEGUN
C ***            NOTE THAT THESE FORMULAE ARE VALID FOR RHOI=2*ETA
C
C=======================================================================
      INCLUDE 'implicit.h'
      ETA = EETA
      CETA = (ETA)**0.3333333333333333d0
      SETA = DSQRT(CETA)
      TEMP = 1.0d0/CETA**2
      U = 1.223404016d0*SETA*
     *     (1.0d0 + TEMP**2*(.04959570165 d0 + TEMP*(-.008888888889 d0 +
     *              TEMP**2*(.002455199181d0 + TEMP*(-.0009108958061d0 +
     *              TEMP**2*.0002534684115d0)))))
      UPR = -.7078817734d0*
     *     (1.0d0 + TEMP*(-.1728260369  d0 + TEMP**2*(.0003174603174d0 +
     *              TEMP*(-.003581214850d0 + TEMP**2*(.0003117824680d0
     *            - TEMP*.0009073966427d0)))))/SETA
      RHOI  =  2.0d0*ETA
      RETURN
      END
      SUBROUTINE XSIGLL (EETA, SIGMA0, LMAX)
C=======================================================================
C
C PURPOSE -- GENERATE SIGCOOL(LL) FOR LL=1 THRU LMAX+1(IE L=0 THRU LMAX)
C
C=======================================================================
      INCLUDE 'implicit.h'
      PARAMETER (MAXL=100)
      COMMON/COOLCOM/F(MAXL),FPR(MAXL),G(MAXL),GPR(MAXL),SIGCOOL(MAXL)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DIMENSION BER(5)
      DATA MMMXXX /100000/
      DATA SMALL /0.000001d0/
      DATA EULER /0.577215664901532860606512d0/
      DATA BER /0.1666666666666666666666666666666666667d0,
     *         -0.0333333333333333333333333333333333333d0,
     *          0.0238095238095238095238095238095238095d0,
     *         -0.0333333333333333333333333333333333333d0,
     *          0.0757575757575757575757575757575757576d0/
C     DATA BER [ 1/6, -1/30, 1/42, -1/30, 5/66 ]
C
      ETA = EETA
      PETA =  DABS(ETA)
C
      IF (PETA.GE.3.0d0) THEN
C
C ***    HERE (DABS(ETA).GE.3) SO USE TABLE BER TO ESTIMATE SIGMA0
      SUM = ZERO
      DO I=1,5
      XI = I
      M = 2*I - 1
      XM = M
      SUM = SUM + BER(I) / (TWO*XI*XM*(PETA**M))
      ENDDO
      SIGMA0 = PI/4.0d0 + PETA*(DLOG(PETA)-ONE) - SUM
C ***    EQ. 14.6.16 GIVES ALL BUT "-SUM" OF THIS EQUATION ???
C
      ELSE
C
C ***    HERE DABS(ETA).LT.3 SO GENERATE SIGMA0 MORE ACCURATELY, FROM
C ***       EQ. 14.5.6 PG 650 ABRAMOWITZ & STEGUN
      SUMAS = ZERO
      DO IS=1,MMMXXX
      S = IS
      TEMPX = PETA/S
      IF (S.LE.TWO*PETA) THEN
C ***          THIS IS EXACT FROM EQ. 6.1.27 FOR ARG(GAMMA(1+PETA/S))
C ***             USING EQ. 6.3.2 FOR PSI(1)
      AS = TEMPX - DATAN(TEMPX)
      ELSE
C ***    THIS IS APPROXIMATION FOR ATAN(TEMPX) VIA TAYLOR EXPANSION
C ***             FOR PETA/S -> 0
      AS = ZERO
      K = 0
      DO J=1,MMMXXX
      M = J + J + 1
      XM = M
      ADD = (TEMPX**M)/XM
      IF (K.EQ.0) THEN
      AS = AS + ADD
      K = 1
      ELSE
      AS = AS - ADD
      K = 0
      ENDIF
      IF (DABS(ADD/AS).LE.SMALL) GO TO 10
      ENDDO
   10 CONTINUE
      ENDIF
      SUMAS = SUMAS + AS
      IF (DABS(AS/SUMAS).LE.SMALL) GO TO 20
      ENDDO
   20 CONTINUE
      SIGMA0 = - EULER*PETA + SUMAS
C
      ENDIF
      IF (ETA.LT.ZERO) SIGMA0 = -SIGMA0
      SIGCOOL(1) = SIGMA0
C
C *** NOW SET SIGCOOL(LL) FOR ALL L
      IF (LMAX.GT.0) THEN
      DO LL=1,LMAX
      XL = DFLOAT(LL)
      SIGCOOL(LL+1) = SIGCOOL(LL) + DATAN(ETA/XL)
      ENDDO
      ENDIF
C
      RETURN
      END
      SUBROUTINE ASYMP2 (EETA, RRHO, U, UPR, RHOI, SIGMA0)
C=======================================================================
C
C *** PURPOSE -- CALCULATE U, UPR FOR LARGE RHOI BUT FINITE ETA
C ***            USING EQS. 14.5.1-8 ON PG 540 ABROMOWITZ & STEGUN
C ***            NOTE THAT RHOI IS CHOSEN TO BE "LARGE ENOUGH" SO THAT
C ***            FORMULA IS VALID.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DIMENSION ZOLD(4), ZNEW(4), Z(4), BIGZ(4)
      DATA DEL/100.0d0/
      DATA EPSLON/0.000001d0/
C
      ETA = EETA
      RHO = RRHO
      RHOI = DMAX1 (RHO*TWO, TEN, TEN*ETA)
CX      RHOI = DMAX1 (RHO, TEN, TEN*ETA)
C
   10 CONTINUE
C *** INITIALIZE
      XN = ZERO
      ZOLD(1) = ONE
      ZOLD(2) = ZERO
      ZOLD(3) = ZERO
      ZOLD(4) = ONE - ETA/RHOI
      DO I=1,4
      Z(I) = ZOLD(I)
      BIGZ(I) = DABS(Z(I))
      ENDDO
C
      JCHECK = 0
      DO N=1,100
      TEMP = TWO*(XN+ONE)*RHOI
      AN = (TWO*XN+ONE)*ETA/TEMP
      BN = (ETA*ETA-XN*(XN+ONE))/TEMP
      XN = XN + ONE
      ZNEW(1) = AN*ZOLD(1) - BN*ZOLD(2)
      ZNEW(2) = AN*ZOLD(2) + BN*ZOLD(1)
      ZNEW(3) = AN*ZOLD(3) - BN*ZOLD(4) - ZNEW(1)/RHOI
      ZNEW(4) = AN*ZOLD(4) + BN*ZOLD(3) - ZNEW(2)/RHOI
      ICHECK = 0
      DO I=1,4
      Z(I)    = Z(I) + ZNEW(I)
      ZOLD(I) = ZNEW(I)
      TEMP2   = DABS(Z(I))
      BIGZ(I) = DMAX1 (BIGZ(I), DABS(Z(I)))
      IF (BIGZ(I)/TEMP2 .GT. DEL) GO TO 20
      IF (DABS(ZNEW(I)/Z(I)) .LE. EPSLON) ICHECK = ICHECK + 1
      ENDDO
      W = Z(1)*Z(4) - Z(2)*Z(3)
      IF (DABS(W).GT.TEN) GO TO 20
      IF (ICHECK.EQ.4) THEN
      JCHECK = JCHECK + 1
      IF (JCHECK.GE.4) GO TO 30
      ELSE
      JCHECK = 0
      ENDIF
      ENDDO
C
C
   20 CONTINUE
C OOPS.  RHOI ISN'T BIG ENOUGH FOR THE ASYMPTOTIC FORMULA TO CONVERGE.
C        DOUBLE RHOI & TRY AGAIN.
      RHOI = RHOI*2.0d0
      GO TO 10
C
   30 CONTINUE
C HERE THE FORMULA FOR Z'S HAS CONVERGED.
C ERGO CALCULATE U, UPR, RHOI:
      PHI = RHOI - ETA*DLOG(TWO*RHOI) + SIGMA0
      COSPHIX = COS(PHI)
      SINPHIX = SIN(PHI)
      G0   = Z(1)*COSPHIX - Z(2)*SINPHIX
      G0PR = Z(3)*COSPHIX - Z(4)*SINPHIX
      U = G0
      UPR = G0PR
      RETURN
      END
      SUBROUTINE TAYLOR (EETA, RRHO, U, UPR, RHOI)
C=======================================================================
C
C *** PURPOSE -- DO TAYLOR SERIES INTEGRATION OF G0, G0PR FOR ARG=RHO,
C ***            STARTING FROM THE (NOW KNOWN) VALUES AT ARG=RHOI
C
C
C *** FIND SOLUTION OF DIFFERENTIAL EQUATION
C         U" + (1-2*ETA/RHO) U = 0    (COULOMB EQN FOR L=0, 14.1.1 A&S)
C     VIA TAYLOR EXPANSION
C         U(RHO) = U + U' D + U" D**2/2 + U"' D**3/6 + U"" D**4/4! + ...
C     WHERE RIGHT-HAND-SIDE IS EVALUATED AT RHOI
C     AND WHERE D = DELTA = RHO-RHOI
C
C     REWRITING U(RHO) = SUM  A(N)  WHERE N=1 TO INFINITY, WITH
C
C          A(1) = U
C          A(2) = U'  D
C          A(3) = U"  D**2 / 2!
C          A(4) = U"' D**3 / 3!
C          A(5) = U"" D**4 / 4!
C          ...
C          A(N+1) = U[N] D**N / N!
C
C
C     SUBSTITUTING EQUATION (1) INTO A(3) GIVES
C     A(3) = {-(1-2 ETA/RHOI) U } D**2/2
C          = - (1-2 ETA/RHOI) D**2 / 2 * A(1)
C     A(4) = - { 2 ETA/RHOI**2 U + (1-2 ETA/RHOI) U'} D**3/ 3!
C          = - { 2 ETA/RHOI**2 A(1) + (1-2 ETA/RHOI) A(2) /D} D**3/3!
C          = - {   2 ETA/RHOI**2  D**3   /3! A(1)
C                + (1-2 ETA/RHOI) D**2 2!/3! A(2)  }
C
C          = - {     1/RHOI       D    /3     A(3)
C                + (1-2 ETA/RHOI) D**2 /(3*2) A(2)
C                +                D**3 /(3*2) A(1) }  USING (3) ABOVE
C
C    A(5) = {4ETA/RHOI**3 U - 4ETA/RHOI**2 U'-(1-2 ETA/RHOI) U"}D**4/4!
C         = - {   2/RHOI        D**3   /4! A(2)
C              + (1-2 ETA/RHOI) D**2 2!/4! A(3)
C              +  2/RHOI        D    3!/4! A(4) }  USE A(4) TO REPLACE A
C
C     A(N) = - {    1/RHOI       D    (N-3) /(N-1)        A(N-1)
C               + (1-2 ETA/RHOI) D**2       /((N-1)(N-2)  A(N-2)
C               +                D**3       /((N-1)(N-2)  A(N-3) }
C
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DIMENSION A(100)
      DATA EPSLON/1.0d-6/
      DATA BIGGER/1.0d10/
      DATA BIGGST/1.0d30/
      DATA DEL   /100.0d0/
C
      ETA = EETA
      RHO = RRHO
      DELTA = RHO - RHOI
      IF (DELTA.EQ.ZERO) RETURN
C
   10 CONTINUE
      A(1) = U
      A(2) = DELTA*UPR
      A(3) = - DELTA*DELTA/TWO*(ONE-TWO*ETA/RHOI)*A(1)
      NSTART = 4
   20 CONTINUE
      JCHECK = 0
      SUM   = ZERO
      SUMPR = ZERO
      BIG   = ZERO
      BIGPR = ZERO
      DO N=1,100
      XN = DFLOAT(N-1)
      IF (N.GE.NSTART) THEN
      A(N) = - ( DELTA*(XN-ONE)*(XN-TWO  )*A(N-1) +
     *               (  RHOI-TWO*ETA)*(DELTA**2)*A(N-2) +
     *                                (DELTA**3)*A(N-3)    ) /
     *               (RHOI*(XN-ONE)*XN)
      IF (A(N).GT.BIGGER) GO TO 30
      ENDIF
      SUM   = SUM   +    A(N)
      SUMPR = SUMPR + XN*A(N)
      IF (SUM  .GE.BIGGST) GO TO 30
      IF (SUMPR.GE.BIGGST) GO TO 30
      BIG   = DMAX1 (BIG  , DABS(SUM)  )
      BIGPR = DMAX1 (BIGPR, DABS(SUMPR))
      IF (SUM.EQ.ZERO .OR. SUMPR.EQ.ZERO) THEN
      JCHECK = 0
      ELSE
C        ELSE IF (SUM.NE.ZERO .AND. SUMPR.NE.ZERO) THEN
      IF (DABS(BIG  /SUM  ).GE.DEL) GO TO 30
      IF (DABS(BIGPR/SUMPR).GE.DEL) GO TO 30
      IF (DABS(A(N) /SUM  ).GE.EPSLON .OR.
     *          DABS(XN*A(N)/SUMPR).GE.EPSLON) THEN
      JCHECK = 0
      ELSE
      JCHECK = JCHECK + 1
      IF (JCHECK.GE.4) GO TO 40
      ENDIF
      ENDIF
      ENDDO
      N = 100
C
   30 CONTINUE
c-----------------------------------------------------------------------
c
C     THE SERIES DID NOT CONVERGE FOR U(RHO) USING TAYLOR EXPANSION
C     AROUND ARG=RHOI.  ERGO, FIND U(RHOI+X), WHERE X=DELTA/2, USING
C     TAYLOR EXPANSION AROUND ARG=RHOI.  ASSUMING THIS CONVERGES,
C     REDEFINE RHOI TO BE RHOI + X AND TRY AGAIN TO GET U(RHO).
c
c-----------------------------------------------------------------------
      NSTART = MAX0 (NSTART,N+1)
      M = NSTART - 1
      DELTA = DELTA/TWO
      TEMP = TWO
      DO N=1,M
      TEMP = TEMP/TWO
      A(N) = A(N)*TEMP
      ENDDO
      GO TO 20
C
   40 CONTINUE
c-----------------------------------------------------------------------
c
C     HERE WE KNOW SUM = U(RHOI+DELTA).  REDEFINE RHOI & DELTA AND TRY
C     AGAIN TO FIND U(RHO) AS TAYLOR EXPANSION AROUND ARG=RHOI.
c
c-----------------------------------------------------------------------
      U = SUM
      UPR = SUMPR/DELTA
      RHOI = RHOI + DELTA
      DELTA = RHO - RHOI
      IF (DABS(DELTA).GE.EPSLON) GO TO 10
      RETURN
      END
      SUBROUTINE GETFG (ETA, RHO, LLMAX, LNOW, G0, G0PR)
C=======================================================================
C
C PURPOSE -- CALCULATE G & F FOR ALL L, GIVEN G(L=0) AND DERIV G(L=0)
C
C=======================================================================
      INCLUDE 'implicit.h'
      PARAMETER (MAXL=100)
      COMMON/COOLCOM/F(MAXL),FPR(MAXL),G(MAXL),GPR(MAXL),SIGCOOL(MAXL)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      LMAX = LLMAX
C
C *** SET G(L)
      LIMIT =  MAX0 (3, LMAX+1)
      G  (1) = G0
      GPR(1) = G0PR
      G  (2) = ((ETA+ONE/RHO)*G(1)-GPR(1))/DSQRT(ETA**2+ONE)
C ***        FROM ABRAMOWITZ & STEGUN EQ. 14.2.2
      DO L=3,LIMIT
      XL = L - 1
      TEMPX = DSQRT(XL*XL+ETA*ETA)
      G(L) = (TWO*XL-ONE)/TEMPX *
     *          (ETA/(XL-ONE)+XL/RHO)*G(L-1) -
     *          XL/TEMPX * DSQRT(ONE+(ETA/(XL-ONE))**2)*G(L-2)
C ***    FROM ABRAMOWITZ & STEGUN EQ. 14.2.3 REWRITTEN
      IF (DABS(G(L)).GT.1.0d+12 .AND. L.GT.LNOW) THEN
      LIMIT = L
      GO TO 10
      ENDIF
      ENDDO
C
   10 CONTINUE
C *** FIND MAXIMUM L VALUE TO USE IN FIGURING F(LIMIT)
C *** I.E. FIND J SUCH THAT G(J) < 1.E-4 * G(J-1) THREE
C *** TIMES IN A ROW
      GM2 = G(LIMIT-1)
      GM1 = G(LIMIT  )
      IL = - 1
      DO J=LIMIT,10000
      XL = J
      TEMPX = DSQRT(XL*XL+ETA*ETA)
      GM = (TWO*XL-ONE)/TEMPX*(ETA/(XL-ONE)+XL/RHO)*GM1 -
     *          XL/TEMPX* DSQRT(ONE+(ETA/(XL-ONE))**2)*GM2
C ***    AGAIN EQ. 14.2.3
C GMH    IF ( (G(LIMIT)/GM)**2 .GT. 1.0D-8 ) IL = -2
      IF ( DABS(G(LIMIT)/GM) .GT. 1.0d-4 ) IL = -2
      IF (IL.GT.0) GO TO 20
      IL = IL + 1
      GM2 = GM1
      GM1 = GM
      ENDDO
C
   20 CONTINUE
C *** FIGURE APPROXIMATE F(LIMIT+3), F(LIMIT+4), ... F(J-1) IN REVERSE
C ***    ORDER.  DO NOT STORE THESE NUMBERS ANYWHERE PERMANENT
      XL = J
      FP1 = XL/GM/DSQRT(XL**2+ETA**2)
C ***       FROM ABRAMOWITZ & STEGUN EQ. 14.2.5 WITH
C ***                         F(L)G(L-1) ASSUMED TO BE NEGLIGIBLE
      FP2 = 0
      L = J - 1
      DO LL=1,J-3-LIMIT
      L = L - 1
      XL = L
      TEMP2 = DSQRT( (XL+ONE)**2 + ETA**2 )
      FP = ( (TWO*XL+THREE)*(ETA/(XL+TWO)+ (XL+ONE)/RHO )*FP1 -
     *        (XL+ONE) * DSQRT(ONE+(ETA/(XL+TWO))**2)*FP2)/ TEMP2
C ***    FROM ABRAMOWITZ & STEGUN EQ. 14.2.3 AGAIN, IN REVERSE
      FP2 = FP1
      FP1 = FP
      ENDDO
C
C *** NOW FIGURE F(1), F(2), ..., F(LIMIT), F(LIMIT+1),
C ***      AGAIN IN REVERSE ORDER.  STORE THESE IN ARRAY F(L)
      DO LL=1,LIMIT+2
      L = L - 1
      XL = L
      TEMP2 = DSQRT( (XL+ONE)**2 + ETA**2 )
      FP = ( (TWO*XL+THREE)*(ETA/(XL+TWO) + (XL+ONE)/RHO )*FP1 -
     *        (XL+ONE) * DSQRT(ONE+(ETA/(XL+TWO))**2)*FP2)/ TEMP2
      F(L+1) = FP
      FP2 = FP1
      FP1 = FP
      ENDDO
C
      CONTINUE
C *** GENERATE DERIVATIVE FUNCTIONS FPR & GPR FOR L=1,LIMIT
C ***      (REMEMBER, WE ALREADY HAVE GPR(1))
      FPR(1) = (ONE/RHO+ETA) * F(1) - DSQRT(ONE+ETA**2)*F(2)
      DO L=2,LIMIT
      XL = L
      FPR(L) = ( XL/RHO + ETA/XL ) * F(L)   -
     *       DSQRT(ONE+(ETA/XL)**2)   * F(L+1)
      TEMPX = ETA/(XL-ONE)
      GPR(L) = DSQRT(ONE+TEMPX**2)  *G(L-1)
     *           - ((XL-ONE)/RHO+TEMPX)*G(L)
      ENDDO
C
      IF (LIMIT.LE.LMAX) LLMAX = LIMIT - 1
      RETURN
      END
      SUBROUTINE BIGETA (EETA,RRHO,LNOW,LLMAX,PENCOUL)
C=======================================================================
C
C *** FORMULAE 14.6.7-8 PAGE 542 IN ABRAMOWITZ & STEGUN, FOR ETA >> RHO
C
C=======================================================================
      INCLUDE 'implicit.h'
      PARAMETER (MAXL=100)
      COMMON/COOLCOM/F(MAXL),FPR(MAXL),G(MAXL),GPR(MAXL),SIGCOOL(MAXL)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA EULER /0.577215664901532860606512d0/
C
      ETA = EETA
      RHO = RRHO
      Q = TWO*RHO*ETA
      ZHALF = DSQRT(Q)
      Z = ZHALF * TWO
C
C *** EVALUATE I_0(Z) FROM 9.6.12 A&S
      SUM = ONE
      A   = Q
      DO K=1,100
      IF (SUM+A.EQ.SUM) GO TO 20
      SUM = SUM + A
      A = A * Q/DFLOAT(K+1)**2
      ENDDO
      WRITE(3,10)
      WRITE(*,10)
   10 FORMAT(///' ERROR - STOP IN BIGETA IN RML/MRML08.F    # 1'///)
      CALL ENDERROR
   20 CONTINUE
      AI0 = SUM
C
C *** EVALUATE K_0(Z) FROM 9.6.13 A&S
      SUM = - (DLOG(ZHALF)+EULER) * AI0
      A = Q
      B = ONE
      DO K=1,100
      IF (SUM+A*B.EQ.SUM) GO TO 40
      SUM = SUM + A*B
      B = B + ONE/DFLOAT(K+1)
      A = A * Q/DFLOAT(K+1)**2
      ENDDO
      WRITE(*,30)
      WRITE(3,30)
   30 FORMAT(///' ERROR - STOP IN BIGETA IN RML/MRML08.F    # 2'///)
      CALL ENDERROR
   40 CONTINUE
      AK0 = SUM
C
C *** EVALUATE I_1(Z) FROM 9.6.10 A&S
      SUM = ONE
      A   = Q/TWO
      DO K=1,100
      IF (SUM+A.EQ.SUM) GO TO 60
      SUM = SUM + A
      A = A * Q/(DFLOAT(K+1)*DFLOAT(K+2))
      ENDDO
      WRITE(*,50)
      WRITE(3,50)
   50 FORMAT(///' ERROR - STOP IN BIGETA IN RML/MRML08.F    # 3'///)
      CALL ENDERROR
   60 CONTINUE
      AI1 = SUM * ZHALF
C
C *** EVALUATE K_1(Z) FROM 9.6.11 A&S
      SUM = ONE/Z + (DLOG(ZHALF)+EULER)*AI1 - ZHALF*HALF
      A = ZHALF * Q/TWO
      B = ONE
      C = HALF**2
      DO K=1,100
      IF (SUM-A*(B+C).EQ.SUM) GO TO 80
      SUM = SUM - A*(B+C)
      C = HALF/DFLOAT(K+2)
      B = B + ONE/DFLOAT(K+1)
      A = A * Q/(DFLOAT(K+1)*DFLOAT(K+2))
      ENDDO
      WRITE(*,70)
      WRITE(3,70)
   70 FORMAT(///' ERROR - STOP IN BIGETA IN RML/MRML08.F    # 4'///)
      CALL ENDERROR
   80 CONTINUE
      AK1 = SUM
c-----------------------------------------------------------------------
C
C     NOW HAVE I_0(Z), I_1(Z), K_0(Z), K_1(Z)
C     ERGO, CAN GET F,G,FP,GP FOR L = 0, FROM EQ. 14.6.8 A&S
C
c-----------------------------------------------------------------------
      C = DSQRT(PI*RHO)
      D = DSQRT(TWO*PI*ETA)
      F  (1) = C * AI1
      FPR(1) = D * AI0
      C = TWO*C/PI
      D = TWO*D/PI
      G  (1) =   C * AK1
      GPR(1) = - D * AK0
C
C *** STORE RESULTS BECAUSE THEY'LL BE CHANGED IN GETFG
      G0 = G(1)
      G0PR = GPR(1)
C
      IF (LNOW.GT.0) THEN
C ***    OBTAIN VALUES FOR ALL L'S
      CALL GETFG (ETA, RHO, LLMAX, LNOW, G0, G0PR)
      ENDIF
      A = PI*ETA
      B = DEXP(-A)
      N = LNOW + 1
      IF (F(N)*B*B+G(N).EQ.G(N)) THEN
      PENCOUL = ( RHO*B /G(N))/G(N) * B
C        NO NEED TO ZERO THE OTHERS BECAUSE THAT'S ALREADY BEEN DONE
      ELSE
      F(N) = F(N)*B
      G(N) = G(N)/B
      FPR(N) = FPR(N)*B
      GPR(N) = GPR(N)/B
c-----put GETPS inline
      LPLUS1  = LNOW + 1
      ASQ     = F(LPLUS1)**2 + G(LPLUS1)**2
      A       = DSQRT(ASQ)
      PENCOUL = RHO/ASQ
      ENDIF
      RETURN
      END
      SUBROUTINE RDRML
C=======================================================================
C
C
C     NEW (2003) REICH-MOORE WITH COMPETITION
C
C     READ REICH-MOORE DATA FOR ONE ENERGY RANGE.
C     ALL DATA WILL BE TREATED AS A SINGLE SECTION.
C
C     THIS ROUTINE USES THE GENERAL REICH-MOORE FORMALISM WITH TWO
C     FISSION CHANNELS AS DEFINED IN ENDF/B-IV. THIS ROUTINE WILL BE
C     USED TO READ DATA IN ANY VERSION OF THE ENDF/B FORMAT (NOTE,
C     THE ENDF/B-6 REICH-MOORE FORMAT HAS NOW BEEN UPDATED TO BE
C     EXACTLY THE SAME AS EARLIER VERSIONS OF THE ENDF/B FORMAT).
C
C     CHECK FOR PRELIMINARY ENDF/B-6 FORMAT(NOW ABANDONED). TERMINATE
C     EXECUTION IF DATA IS IN PRELIMINARY ENDF/B-6 FORMAT.
C
C     FIELD DEFINITIONS FOR EACH RESONANCE
C
C     FIELD          PRELIMINARY          CURRENT
C                    ENDF/B-6  FORMAT     ENDF/B-6  FORMAT
C     =====          ================     ================
C       1            ENERGY               ENERGY
C       2            J                    J
C       3            TOTAL WIDTH          ELASTIC WIDTH
C       4            ELASTIC WIDTH        CAPTURE WIDTH
C       5            CAPTURE WIDTH        FISSION WIDTH 1
C       6            NOT USED             FISSION WIDTH 2
C                    (FISSION NOT
C                    ALLOWED)
C
C     IF THIRD FIELD (PRELIMINARY TOTAL) IS EQUAL TO THE SUM OF THE
C     FOURTH (PRELIMINARY ELASTIC) AND FIFTH (PRELIMINARY CAPTURE)
C     AND SIXTH FIELD IS ZERO (FISSION NOT ALLOWED IN PRELIMINARY
C     FORMAT) FOR ALL RESONANCES THIS PROGRAM WILL ASSUME THAT THE
C     DATA IS IN THE PRELIMINARY REICH-MOORE FORMAT AND TERMINATE
C     EXECUTION WITH A WARNING MESSAGE THAT THE DATA MUST BE CONVERTED
C     TO THE CURRENT REICH-MOORE FORMAT BEFORE IT CAN BE PROCESSED.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
c-----2021/2/26 - Added Tables
      COMMON/TABLECOM/LTABLE(100),AJTABLE(100),ISHTABLE(100),
     1 NTABLO(0:100),NTABHI(0:100),NTABLE,ITABLE
      NTABLE     = 0      ! no tables yet
      NTABHI(0)  = 0      ! set for first table
c-----Set FIXED dimensions for necessary arrays
      CALL Setdim
c-----Read and store particle-pair information
      CALL READPART
c-----Read and store resonance parameters
      CALL READRESP
C-----Check quantum numbers for consistency
      CALL CHEKQUAN
C-----Generate Zke, Zkte, Zkfe, Zeta7 for use in calculating k, rho,& et
      CALL Fxradi
C-----Generate energy-independent channel constants; count parameters
      CALL Betset
      RETURN
      END
      SUBROUTINE same1(Awrx,Nrox,Napsx)
c-----2014/2/20 - ereslow, ereshigh passed through common
C=======================================================================
C
C     DEFINE PARAMETERS READ BY RECENT FOR SAMRML
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
      COMMON/INDATD/ELX,EHX
      COMMON/NAPRHO/NRO,NAPS
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
c-----2021/2/19 = RHO without energy
      COMMON/RHOCOM/RHOBASE
c-----2021/2/19 -  Shift calculation
      DATA C3/1.23D-01/
      DATA C5/8.0d-2/
C-----UPDATED NOV. 12, 1998 AS PER CSEWG SUBCOMMITTEE RECOMMENDATION
c-----Updated 9/01/04 - ENDF-102, appendix H
      DATA C4/1.00866491578d0/
      DATA C6/2.196807122623d-3/
c-----------------------------------------------------------------------
c
c     First check for ERRORS.
c
c-----------------------------------------------------------------------
      if(NIS.ne.1) then
      write(*,10)
      write(3,10)
   10 format(///' ERROR - Only one isotope allowed for LRF=7 data.'///)
      CALL ENDERROR
      endif
c-----------------------------------------------------------------------
c
c     O.K. - set parameters.
c
c-----------------------------------------------------------------------
      LRF7     = LRF7+1 ! LRF7 = # of LRF=7 Energy Ranges.
      Awrx     = AWR
      Nrox     = NRO
      Napsx    = NAPS
      ereslow  = ELX
      ereshigh = EHX
c-----------------------------------------------------------------------
c
c     Shift calculation parameters
c
c-----------------------------------------------------------------------
      THIRD = 1.0d0/3.0d0
      APXRML=C3*((C4*AWR)**(THIRD))+C5
      AK1RML=C6*AWR/(AWR+1.0d0)               ! C6*AWRICM
      RHOBASE=(APXRML*AK1RML)**2
      RETURN
      ENTRY same2(NLSX)
C=======================================================================
C
C     DEFINE PARAMETERS READ BY SAMRML FOR RECENT
C
C=======================================================================
      NLS  = NLSX
      RETURN
      ENTRY same0
C=======================================================================
C
C     Reset LRF=7 used flag for each evaluation.
C
C=======================================================================
      LRF7    = 0   ! # of LRF=7 Sections
      IMFISSY = 0
c-----2014/2/20 - added for multi-region
      KresSum = 0
      KgrSum  = 0
      NppSum  = 0
      RETURN
      END
      SUBROUTINE same3(NRESX,Npp)
C=======================================================================
C
C     DEFINE PARAMETERS READ BY SAMRML FOR RECENT
C
c     1 dummy section of Lrf = 7
c
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'recent.h'
      COMMON/INDATD/ELX,EHX
      COMMON/MAXIE/NEDSEC,NEDNOD,NEDRES
      COMMON/ETABS/NODES
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      NSECT       = NSECT + 1
      EL(NSECT)   = ELX
      EH(NSECT)   = EHX
      NGRTAB(1,NSECT) = Ngr1
      NGRTAB(2,NSECT) = Ngr2
      NPPTAB(  NSECT) = Npp
      MODE(NSECT) = 7
      NEDSEC      = 1
      NEDNOD      = NODES
      NEDRES      = NRESX
      if(NSECT.eq.1) then
      ii1 = 1
      else
      ii1 = NHIGH(NSECT-1) + 1
      endif
      NLOW(NSECT)  = ii1
      NHIGH(NSECT) = ii1 + NRESX - 1
      return
      END
      SUBROUTINE SIGRML(Enow)
c=======================================================================
c
c     Calculate ALL cross sections at energy ENOW
c
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
c-----define RECENT energy for SAMRML
      Su = Enow
C-----Generate energy-dependent pieces of cross section (no derivs)
      CALL Abpart
C-----Form the cross section Crss (no Deriv)
      CALL Crosss
C-----Final answer
      CALL Answers
      return
      END
      SUBROUTINE READPART
c=======================================================================
C
C *** PURPOSE -- Read particle-pair definitions for Lrf=7
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
c-----limit of resolved region for SAMRML
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      common/usemt/dumrml(100),Quse(11),Mtuse(11)
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
      character*1 FIELD(11,12)
      DATA ONEX  /1.0d0/
c
c     Particle Masses: Updated 2021/1/29 - Wikipedia
c
      DATA Aneutron /1.00866491588d0/
      DATA Proton   /1.00727646662d0/
      DATA Deuteron /2.01410177811d0/
      DATA Triton   /3.01604920000D0/
      DATA He3      /3.01602930000d0/
      DATA Alpha    /4.0015061791270/
c-----------------------------------------------------------------------
      CALL CARDIO(C1x, C2x, L1x, L2x, NLx, L2x)
c-----initialize - only on first region
      if(NppSum.le.0) then
      do i=1,11
      Quse    (i) = 0.0d+0
      Mtuse   (i) = 0
      QuseSum (i) = 0.0d+0
      MtuseSum(i) = 0
      ENDDO
      endif
c-----define parameters read by SAMRML for RECENT
      call same2(NLx)
c-----and for SAMRML
      Ngroup = NLx
      CALL CARDIO(C1x, C2x, Npp, L2x, N1x, N2x)
C-----Spi and Ap fields are not used for LRF=7
      write(*,10) Ngroup,Npp
      write(3,10) Ngroup,Npp
   10 format(1x,78('=')/
     3 ' Number of J Values-------------------',I11/
     4 ' Number of Particle-Pairs-------------',I11)
c-----------------------------------------------------------------------
C
C     Read information re particle-pairs
c
c-----------------------------------------------------------------------
      if(IMEDIT.ne.0) then
      Write(3,20)
   20 format(1x,78('=')/' Particle Pair Information'/1x,78('=')/
     1       '   I',6X,'MassA',6X,'MassB',9X,'ZA',9X,'ZB',6X,'SpinA',
     1         6X,'SPinB'/
     2         4X,10X,'Q',5X,'Pentra',6X,'Shift',9X,'MT',4X,'ParityA',
     3         4X,'ParityB'/1x,78('-'))
      endif
      DO I=1,Npp
      CALL LISTIO(dumrml(1),12)
      Ema    (I)   = dumrml(1)
      Emb    (I)   = dumrml(2)
      ZaX          = dumrml(3)
      ZbX          = dumrml(4)
      Spina  (I)   = dumrml(5)
      Spinb  (I)   = dumrml(6)
      Qqq    (I)   = dumrml(7)
      XLpent       = dumrml(8)
      xIshift      = dumrml(9)
      aMT          = dumrml(10)
      Pa     (I)   = dumrml(11)
      Pb     (I)   = dumrml(12)
      Ishift (I)   = xIshift + 0.01d0
      Lpent  (I)   = xLpent + 0.01d0
      Kza    (I)   = ZaX
      Kzb    (I)   = ZbX
      Mt7     (I)  = aMt
      Quse   (I)   = Qqq(I)
      Mtuse  (I)   = Mt7(I)
c-----Accumulate MT from ALL regions
      if(NppSum.gt.0) then
      do k=1,NppSum
      if(MtuseSum(k).eq.Mtuse(I)) go to 30
      enddo
      endif
c-----New MT
      NppSum = NppSum + 1
      MtuseSum(NppSum) = Mtuse(I)
      QuseSum (NppSum) = Quse(I)
   30 continue
      call OUT9(Ema(I)  ,FIELD(1,1))
      call OUT9(Emb(I)  ,FIELD(1,2))
      call OUT9(Spina(I),FIELD(1,3))
      call OUT9(Spinb(I),FIELD(1,4))
      call OUT9(Qqq(I)  ,FIELD(1,5))
      call OUT9(Pa(I)   ,FIELD(1,6))
      call OUT9(Pb(I)   ,FIELD(1,7))
      if(IMEDIT.ne.0) then
      write( 3,40) I,((FIELD(II,JJ),II=1,11),JJ=1,2), Kza(I),
     1 Kzb(I),((FIELD(II,JJ),II=1,11),JJ=3,4),(FIELD(II,5),II=1,11),
     2 Lpent(I),Ishift(I),Mt7(I),((FIELD(II,JJ),II=1,11),JJ=6,7)
   40 FORMAT(  I4,22A1    ,I11,I11,22A1/
     1         4X,11A1   ,I11,I11,I11,22A1)
      endif
      ENDDO
C
C     Fill in the defaults
C
      DO I=1,Npp
      IF (Mt7(I).EQ.102) THEN
C ***       Mt7 =102 => one "particle" is gamma
      ELSE IF (Mt7(I).EQ.2) THEN
C ***       Mt7 =  2 => neutron
      Ema(I) = ONEX
      Spina(I) = 0.5d0
      Kza(I) = 0
      Lpent(I) = 1
      ELSE IF (Mt7(I).EQ.18) THEN
C ***       Mt7 = 18 => fission
      Lpent(I) = 0
      ELSE IF (Mt7(I).GT.50 .AND. Mt7(I).LT.99) THEN
C ***  50 < Mt7 < 99 => inelastic
      Ema(I) = ONEX
      Spina(I) = 0.5d0
      Kza(I) = 0
      Lpent(I) = 1
      ELSE IF (Mt7(I).EQ.103) THEN
C ***       Mt7 =103 => proton
      Ema(I) = Proton/Aneutron
      Spina(I) = 0.5d0
      Kza(I) = 1
      Lpent(I) = 1
      ELSE IF (Mt7(I).EQ.104) THEN
C ***       Mt7 =104 => deuteron
      Ema(I) = Deuteron/Aneutron
      Spina(I) = 1.0d0
      Kza(I) = 1
      Lpent(I) = 1
      ELSE IF (Mt7(I).EQ.105) THEN
C ***       Mt7 =105 => triton
      Ema(I) = Triton/Aneutron
      Spina(I) = 0.5d0
      Kza(I) = 1
      Lpent(I) = 1
      ELSE IF (Mt7(I).EQ.106) THEN
C ***       Mt7 =106 => He3
      Ema(I) = He3/Aneutron
      Spina(I) = 0.5d0
      Kza(I) = 2
      Lpent(I) = 1
      ELSE IF (Mt7(I).EQ.107) THEN
C ***       Mt7 =107 => alpha
      Ema(I) = Alpha/Aneutron
      Spina(I) = 0.0d0
      Pa(I) = 0.0d0
      Kza(I) = 2
      Lpent(I) = 1
      ELSE
      ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE READRESP
c=======================================================================
C
C *** Purpose -- Read the channel information and the resonance paramete
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      common/usemt/dumrml(100),Quse(11),Mtuse(11)
      COMMON/FILE2COM/ZA,AWR,ZAI,ABN,SPI,AP,AWRI,QX,NRS,NIS,LFW,NER,
     1 LRU,LRF,LRFIN,NLS
c-----2021/2/19 - Added Tables
      COMMON/TABLECOM/LTABLE(100),AJTABLE(100),ISHTABLE(100),
     1 NTABLO(0:100),NTABHI(0:100),NTABLE,ITABLE
      COMMON/RHOCOM/RHOBASE
      dimension MTTITLE(11)
      character*1 FIELD(11,12)
      character*4 MTBCD
      DATA MTBCD/'MT='/
c-----2014/2/20 - multi-region - sum groups and resonances
      Ngr1   = KgrSum + 1
      Ngr2   = KgrSum + Ngroup
c-----Insure groups are in legal range.
      if(Ngr2.gt.MaxNgroup) then
      write(*,10) Ngr2,MaxNgroup
      write(3,10) Ngr2,MaxNgroup
   10 format(//' ERROR - Ngr2=',i5,' Exceeeds MaxNgroup=',i5/
     1         '         Increase MaxNgroup and try again.'/
     2         '         Execution Terminated.'///)
      CALL ENDERROR
      endif
      KgrSum = Ngr2
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      DO Igroup=Ngr1,Ngr2
c-----2021/2/26  Added tables
      NTABLE = NTABLE + 1
C
      CALL CARDIO(Spin(Igroup), Parity(Igroup),L1x, L2x, N1x, IchanP1)
      call OUT9(Spin(Igroup)  ,FIELD(1,1))
      call OUT9(Parity(Igroup),FIELD(1,2))
      write( 3,20) Igroup, ((FIELD(II,JJ),II=1,11),JJ=1,2), IchanP1
      write( *,20) Igroup, ((FIELD(II,JJ),II=1,11),JJ=1,2), IchanP1
   20 format(1x,78('=')/
     1       ' J Value #----------------------------',I11/
     1       ' J Value------------------------------',11A1/
     1       ' Parity-------------------------------',11A1/
     1       ' Number of Spin-Pairs-----------------',I11)
C
      Ichan         = IchanP1 - 1
      Nchan(Igroup) = Ichan
C ***    Read channel parameters
      Ix     = 0
      I      = 0
      Kgamma = 0
      if(IMEDIT.ne.0) then
      Write(3,30)
   30 FORMAT(1x,78('-')/
     1       ' Spin-Pair#',4X,'L Value',3X,'    Spin',8X,'Bnd',
     1       8X,'Rde',8X,'Rdt')
      endif
c-----Ich = Channel = width count - DO NOT OFFSET BY SUM
      DO Ich=1,IchanP1
      CALL LISTIO(dumrml(1),6)
      Pp  = dumrml(1)
      eLz = dumrml(2)
      Spz = dumrml(3)
      Bnd = dumrml(4)
      Rde = dumrml(5)
      Rdt = dumrml(6)
      Mpp = Pp
      if(IMEDIT.ne.0) then
      call OUT9(eLz,FIELD(1,1))
      call OUT9(Spz,FIELD(1,2))
      call OUT9(Bnd,FIELD(1,3))
      call OUT9(Rde,FIELD(1,4))
      call OUT9(Rdt,FIELD(1,5))
      write( 3,40) Mpp,((FIELD(II,JJ),II=1,11),JJ=1,5)
   40 FORMAT(i11,55A1)
      endif
      Ippx = Pp
      IF (Ippx.EQ.1) THEN
      Kgamma =  Ich
      ELSE
      I = I + 1
      Ipp(I,Igroup) = Ippx
      IF (Ipp(I,Igroup).EQ.2) Ix = Ix + 1
C                  Pp=2 =>incident channel, except really it's Mt7(Ipp)=
C                         that defines what's an incident channel...
      Lspin  (I,Igroup) = eLz
c-----Added tables
      LTABLE(NTABLE)    = eLz
      AJTABLE(NTABLE)   = Spin(igroup)
      ISHTABLE(NTABLE)  = Ishift(2)
      Chspin7(I,Igroup) = Spz
      Bndry  (I,Igroup) = Bnd
      Rdeff  (I,Igroup) = Rde
      Rdtru  (I,Igroup) = Rdt
      ENDIF
      ENDDO
      IF (Ix.EQ.0) THEN
c-----2016/3/8 - Added ERROR message
      write(*,50)
      write(3,50)
   50 format(//' ERROR - At least one Spin-Pair# MUST = 2'/
     1         '         Check and Correct Data and Try Again.'/
     2         '         Execution Terminated.'///)
c-----2016/3/8 - Added ERROR message
      CALL ENDERROR
      ENDIF
c-----------------------------------------------------------------------
C
C      Read number of resonances for this spin group
c
c      Read resonance parameters
c
c-----------------------------------------------------------------------
      write(3,60)
   60 format(1x,78('-'))
      CALL CARDIO(C1, C2, L1x, Nresg(Igroup), N1x, N2x)
      write( 3,70) Nresg(Igroup)
      write( *,70) Nresg(Igroup)
   70 format(' Number of Resonances-----------------',i11)
c-----2021/2/26 -
      NTABLO(NTABLE) = NTABHI(NTABLE-1) + 1
      NTABHI(NTABLE) = NTABHI(NTABLE-1) + Nresg(Igroup)
c-----------------------------------------------------------------------
C
C     Read and store the resonance parameters for this spin group
c
c-----------------------------------------------------------------------
c-----Save # of resonances - in case it = 0
      NRESGRP = Nresg(Igroup)
      IF (Nresg(Igroup).EQ.0) THEN
c-----skip 1 line
      CALL LISTIO(dumrml(1),1)
      ELSE
c-----read resonances
      do i=1,MaxNchan
      if(Ipp(i,igroup).le.0) go to 80
      enddo
      i = MaxNchan + 1
   80 i = i - 1
      if(IMEDIT.ne.0) then
      write(3,90) (MTBCD,Mtuse(Ipp(k,igroup)),k=1,i)
   90 format(1x,78('=')/' Reich-Moore Resonance Parameters'/1x,78('=')/
     1       '     Energy'/'       (eV)',
     1       5X,'MT=102',11(5x,A3,i3)/1x,78('='))
c-----Save list of MTs in output order
      do k=1,i
      MTTITLE(k) = Mtuse(Ipp(k,igroup))
      enddo
      endif
      DO Ires=1,Nresg(Igroup)
      KresSum = KresSum + 1
c-----Check available storage
      if(KresSum.gt.MaxNres) then
      write(*,100) KresSum,MaxNres
      write(3,100) KresSum,MaxNres
  100 format(//' ERROR - Number of resonances',i8,' exceeds maximum',
     1 ' allowed',i8/
     3         '         Increase MaxNres and try again.'/
     2         '         Execution Terminated.'///)
      call ENDERROR
      endif
c-----2014/2/20 - Initialize = energy + max. # of widths.
      do k=1,MaxNchan+1
      dumrml(k) = 0.0d+0
      enddo
      CALL LISTIO(dumrml(1),Ichan+2)
      Eres  (KresSum)  = dumrml(1)
      Gamgam(KresSum)  = dumrml(2)
c-----------------------------------------------------------------------
c
c     Define TOTAL Width
c
c-----------------------------------------------------------------------
      widtot        = dabs(dumrml(2))              ! 0 = MT=102
      if(Ichan.gt.0) then
      do I=1,Ichan
      widtot        = widtot + dabs(dumrml(I+2))
      Gamma(I,KresSum) = dumrml(I+2)
      call OUT9(Gamma(I,KresSum),FIELD(1,I+2))
      enddo
      endif
c-----------------------------------------------------------------------
c
c     Output Listing
c
c-----------------------------------------------------------------------
      call OUT9(Eres(KresSum)  ,FIELD(1,1))
      call OUT9(Gamgam(KresSum),FIELD(1,2))
      if(IMEDIT.ne.0) then
      write(3,110) ((FIELD(K,I),K=1,11),I=1,Ichan+2)
  110 format(132A1)
      endif
c-----------------------------------------------------------------------
c
c     use resonance energy as a node.
c
c-----------------------------------------------------------------------
      CALL NOODLE(Eres(KresSum),widtot,ereslow,ereshigh)
c
      IF (Kgamma.NE.1) THEN
      X = Gamma(Kgamma+1,KresSum)
      If (Kgamma.GT.1) THEN
      DO I=Kgamma,2,-1
      Gamma(I,KresSum) = Gamma(I-1,KresSum)
      ENDDO
      ENDIF
      Gamma(1,KresSum) = Gamgam(KresSum)
      Gamgam(KresSum)  = X
      ENDIF
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
c
c     2020/9/17 - Test (L, J) sequence spans resonance region
c
c-----------------------------------------------------------------------
      if(NRESGRP.gt.0) then         ! Skip if no resonances
      call WARNRRR(Eres(KresSum))
      endif                         ! no resonances
      ENDDO
C
c-----sum # of resonances
c-----2014/2/20 - multi-region sum
      if(Ngr1.gt.1) Nresg(Ngr1) = Nresg(Ngr1-1) + Nresg(Ngr1)
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      IF (Ngr2.GT.Ngr1) THEN
      DO Igroup=Ngr1+1,Ngr2
      Nresg(Igroup) = Nresg(Igroup-1) + Nresg(Igroup)
      ENDDO
      ENDIF
c-----Nres = sum of # of resonances
      Nres = Nresg(Ngr2)
c-----KresSum = one-by-one count of resonances
      IF (KresSum.NE.Nresg(Ngr2)) then
      write(3,120) Ngroup,Nresg(Ngr2),KresSum
      write(*,120) Ngroup,Nresg(Ngr2),KresSum
  120 format(' ERROR - Nresg/KresSum=',3i8,' MUST be equal.'/
     1       '         Execution Terminated.'///)
      CALL ENDERROR
      endif
c-----------------------------------------------------------------------
c
c     Summary of sequences
c
c-----------------------------------------------------------------------
      write(*,130)
      write(3,130)
  130 format(1x,78('=')/' Summary of Sequences (MT #s used)'/1x,78('-'))
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      DO Igroup=Ngr1,Ngr2
      do i=1,MaxNchan
      if(Ipp(i,igroup).le.0) go to 140
      enddo
      i = MaxNchan + 1
  140 i = i - 1
      write(*,150) igroup,(Mtuse(Ipp(k,igroup)),k=1,i)
      write(3,150) igroup,(Mtuse(Ipp(k,igroup)),k=1,i)
  150 format(10i5)
      ENDDO
      write(*,160)
      write(3,160)
  160 format(1x,78('='))
c
c     set up RECENT to use 1 section
c
      call same3(Nres,Npp)
      RETURN
      END
      SUBROUTINE CHEKQUAN
c=======================================================================
C
C *** PURPOSE -- Report quantum number information etc when particle-pai
C ***            definitions are given
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C-----------------------------------------------------------------------
C
C     2017/04/06 - RML (LRF=7) Shift NO Longer Allow - set=0, continue
C     2020/11/03 - Shift o.k - check Lpent & IShift - each 0 or 1
C                              if not, define = 0 - and continue
C
C-----------------------------------------------------------------------
      DO J=1,Npp
c-----Lpent
      IF (Lpent(J).LT.0 .OR. Lpent(J).GT.1) THEN
      write(3,10) J,Lpent(J)
      write(*,10) J,Lpent(J)
   10 format(1x,78('=')/
     1 ' WARNING..J/Lpent(J)=',2i5,' Expect either 0 or 1'/
     2 '                     ',10x,' Will assume Lpent=0.'/1x,78('='))
      Lpent(J) = 0
      ENDIF
c-----Ishift
C-----------------------------------------------------------------------
C
c     Valentin Insert - Start
C
C-----------------------------------------------------------------------
      if(Ishift(J).ne.0) then
      write(3,20) Ishift(J)
      write(*,20) Ishift(J)
   20 format(1x,78('-')/
     1 ' WARNING...IShift=',i5,' LRF=7 Shift Option Used')
C-----------------------------------------------------------------------
C
c     Valentin Insert - End
C
C-----------------------------------------------------------------------
      Ishift(J) = 1
      ENDIF
      ENDDO
c-----------------------------------------------------------------------
C
c     2014/2/20 - Extended for Multiple Energy Ranges.
c
c-----------------------------------------------------------------------
      DO J=Ngr1,Ngr2
C
      IF (dMOD(Spin(J),HALF).NE.ZERO) THEN
      Write (3,30) J, Spin(J)
      Write (*,30) J, Spin(J)
   30 FORMAT (' ERROR - Quantum Numbers for Spin Group '/
     1        '         Number ', I3,' spin =', F10.5,
     2                                 '(MUST be multiple of 1/2)'/
     3        '         Execution Terminated.'///)
      CALL ENDERROR
      ENDIF
C
      Goj(J) = ZERO
      Nent(J) = 0
      Next(J) = 0
      Nchanj = Nchan(J)
      DO N=1,Nchanj
      IF (Ipp(N,J).EQ.2) THEN
      Nent(J) = Nent(J) + 1
      IF (Goj(J).EQ.ZERO) THEN
      Goj(J) = (TWO*DABS(Spin (      J) )+ONE)/
     *                   ( (TWO*DABS(Spina(Ipp(N,J)))+ONE)
     *                   * (TWO*DABS(Spinb(Ipp(N,J)))+ONE) )
      ENDIF
      ELSE IF (Ipp(N,J).GT.2) THEN
      Next(J) = Next(J) + 1
      ELSE
      ENDIF
      IF (Lspin(N,J).LT.0) THEN
      write(3,40) N,J,Lspin(N,J)
   40 format(' WARNING..N/J/Lspin(N,J)=',3i5,' Is Negative')
      ENDIF
      IF (Chspin7(N,J).EQ.ZERO .OR. Spin(J).EQ.ZERO) THEN
      ELSE
      X = ONE
      I = MOD(Lspin(N,J),2)
      IF (I.NE.0) X = -X
      IF (Chspin7(N,J).LT.ZERO) X = -X
      IF (Spin(J).LT.ZERO) X = -X
      IF (X.LT.ZERO) THEN
      Write (3,50) J,N, Spin(J), Lspin(N,J), Chspin7(N,J)
   50 FORMAT (' *** Parity problem ***', /,
     *               '     Group and channel #', 2I5, /,
     *               '     Spin, L, Chspin7 =', F7.1, I4, F7.1)
      ENDIF
      ENDIF
      Smin = DABS( DABS(Spina(Ipp(N,J))) - DABS(Spinb(Ipp(N,J))) )
      Smax =       DABS(Spina(Ipp(N,J))) + DABS(Spinb(Ipp(N,J)))
      IF (DABS(Chspin7(N,J)).LT.Smin) THEN
      Write (3,80) J, N
      Write (3,60) Chspin7(N,J), Spina(Ipp(N,J)),
     *             Spinb(Ipp(N,J)), Smin
c               Write (*,11500) J, N
c               Write (*,11501) Chspin7(N,J), Spina(Ipp(N,J)),
c    *             Spinb(Ipp(N,J)), Smin
   60 FORMAT (' ******* |Chspin|<|Spina-Spinb|=Smin',3X,4F5.1)
      ELSE IF (DABS(Chspin7(N,J)).GT.Smax) THEN
      Write (3,80) J, N
      Write (3,70) Chspin7(N,J), Spina(Ipp(N,J)),
     *             Spinb(Ipp(N,J)), Smax
   70 FORMAT (' ******* |Chspin|>|Spina+Spinb|=Smax',3X,4F5.1)
      ENDIF
   80 FORMAT (/, ' ****** Error in quantum numbers for',
     *         1x, 'Group Number', I3, ' and Channel Number', I2)
      IF (Lpent(Ipp(N,J)).NE.0) THEN
      Smin = DABS(DABS(Dfloat(Lspin(N,J))-DABS(Chspin7(N,J))))
      Smax = Dfloat(Lspin(N,J)) + DABS(Chspin7(N,J))
      IF (DABS(Spin(J)).LT.Smin) THEN
      Write (3,80) J, N
      Write (3,90) Spin(J), Lspin(N,J), Chspin7(N,J),Smin
   90 FORMAT(' ****** |Spin|<|Lspin-Chspin|=Smin', 3X,4F5.1)
      ELSE IF (DABS(Spin(J)).GT.Smax) THEN
      Write (3,80) J, N
      Write (3,100) Spin(J), Lspin(N,J), Chspin7(N,J),Smin
  100 FORMAT(' ****** |Spin|>|Lspin-Chspin|=Smin', 3X,4F5.1)
      ENDIF
      ENDIF
      ENDDO
      ENDDO
C
      RETURN
      END
      SUBROUTINE Setdim
c=======================================================================
c
c     SAMRML routine with fixed dimensions
c
c     Call once for first Lrf=7 case,
c     to initialize ALL parameters.
c
c=======================================================================
c-----ADD ALL to insure initialization
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
c-----energy limits of resolved range
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
c-----(define parameters read by RECENT for SAMRML)---------
c-----2014/2/20 - ereslow, ereshigh passed through common
      call same1(Awr7,Nro7,Naps7)
c-----(initializememory storage parameters)-------
      RETURN
      END
      SUBROUTINE Fxradi
c=======================================================================
C
C *** Purpose -- fix the following parameters:
C ***            Zke , where k   = Zke * sqrt(E)
C ***            Zkte, where Rho = ka = Zkte * sqrt(E) in penetrability
C ***            Zkfe, where Rho = ka = Zkfe * sqrt(E) in phi
C ***            Zeta7, where eta = Zeta7/sqrt(E) for charged particles
C
C ***            Also fix masses
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
c-----------------------------------------------------------------------
C
c     initialize
c
c     2014/2/20 - Extended for Multiple Energy Ranges.
c
c-----------------------------------------------------------------------
      do ii=Ngr1,Ngr2
      do jj=1,MaxNchan
      Zke  (jj,ii) = 0.0d+0
      Zkfe (jj,ii) = 0.0d+0
      Zkte (jj,ii) = 0.0d+0
      Zeta7(jj,ii) = 0.0d+0
      Echan(jj,ii) = 0.0d+0
      enddo
      enddo
C
      Emneut = 1.00866491578d0
C            = mass of neutron in amu
      Hbarrr = 6.582118890d-16
C            = Planck's constant over 2 Pi in (eV s)
      Amuevv = 931.494013d+06
C            = atomic mass unit in eV
      Cspeed = 2.99792458d+08
C            = speed of light in m/s
      Fininv = 1.0d0/137.03599976d0
C            = alpha = fine structure constant
C            = e^2 / (hbar c) dimensionless
      Ff     = 1.0d+15
      Twomhb = dSQRT(2.0d0*Emneut*Amuevv)/(Hbarrr*Ff*Cspeed)
      Etac   =           Fininv * Amuevv /(Hbarrr*Ff*Cspeed) * Emneut
C
      DO Ippx=1,Npp
      IF (Ema(Ippx).EQ.ZERO) THEN
      Ema(Ippx) = ONE
      ENDIF
      IF (Emb(Ippx).EQ.ZERO) THEN
      Emb(Ippx) = Awr7
      ENDIF
      ENDDO
c-----------------------------------------------------------------------
C
c     2014/2/20 - Extended for Multiple Energy Ranges.
c
c-----------------------------------------------------------------------
      DO Kgroup=Ngr1,Ngr2
      NchanK = Nchan(Kgroup)
      Factor = Emb(2) + Ema(2)
      Alabcm = Emb(2)/Factor
      Factor = Alabcm/Ema(2)
      DO Ichan=1,NchanK
      Ippx = Ipp(Ichan,Kgroup)
      Aa = Emb(Ippx) + Ema(Ippx)
      Aa = Emb(Ippx)/Aa
      IF (Qqq(Ippx).NE.ZERO .AND. Echan(Ichan,Kgroup).EQ.ZERO)THEN
      Echan(Ichan,Kgroup) = - Qqq(Ippx) / Alabcm
      ENDIF
      Redmas = Aa * Ema(Ippx)
      Z = Twomhb * dSQRT(Redmas*Factor)
      Zke (Ichan,Kgroup) = Z
      Zkfe(Ichan,Kgroup) = Z*Rdeff(Ichan,Kgroup)*TEN
      Zkte(Ichan,Kgroup) = Z*Rdtru(Ichan,Kgroup)*TEN
      Dcoulomb = Kzb(Ippx)*Kza(Ippx)
      IF(Dcoulomb.gt.0.0d0)
     1 Zeta7(Ichan,Kgroup) = Etac*Dcoulomb*Redmas/Zke(Ichan,Kgroup)
      ENDDO
      ENDDO
C
      RETURN
      END
      SUBROUTINE Abpart
c=======================================================================
C
C *** Purpose -- Generate Alphar and Alphai => for cross section
C ***            and Upr and Upi = Energy-dependent pieces of Pr & Pi
C ***            Also generate Pr and Pi = partial of R wrt U-parameters
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/RANGER/LOW,LHI
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
C *** Generate Alphar =  (DEL E) / ( (DEL E)**2 + (Gamgam/2)**2 )
C ***      AND Alphai = Gamgam/2 / ( Ditto )
C
c     DO N=1,Nres
      DO N=LOW,LHI
      Difen (N) = Eres(N) - Su
      Aa = Difen(N)**2 + Gbetpr(3,N)
      Xden  (N) = ONE/Aa
      Alphar(N) = Difen(N)*Xden(N)
      Alphai(N) = Gbetpr(2,N)*Xden(N)
      ENDDO
      RETURN
      END
      SUBROUTINE Setr
c=======================================================================
C
C *** PURPOSE -- GENERATE Linv = 1/(S-B+IP)
C ***                    Rootp = sqrt(P)
C ***                     Rmat = SUM Beta7*Beta7/((DEL E)-i(GAMGAM/2))
C ***                     Ymat = Linv - Rmat
C ***            Also return Lrmat = 1 if no R-matrix contribution
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      DATA Tiny /1.d-8/
C
      Nnntot = Nchann + 1
      DO I=1,Nchann
      Nnntot = Nnntot - 1
      IF (Su.GE.ZERO .AND. Su.LE.Echan(Nnntot,Kg)) THEN
      Nchann = Nnntot - 1
      ELSE
      GO TO 10
      ENDIF
      ENDDO
   10 CONTINUE
C
C *** INITIALIZE Rmat = R-MATRIX
C
      KL = 0
      DO K=1,Nchann
      DO L=1,K
      KL = KL + 1
      Rmat(1,KL) = ZERO
      Rmat(2,KL) = ZERO
      ENDDO
      ENDDO
C
      IF (Maxr.GE.Minr .AND. Minr.GT.0) THEN
      DO Ires=Minr,Maxr
      KL = 0
      DO K=1,Nchann
      DO L=1,K
      KL = KL + 1
      IF (Su.GT.Echan(K,Kg) .AND. Su.GT.Echan(L,Kg) .AND.
     *              Beta7(KL,Ires).NE.ZERO) THEN
      Rmat(1,KL) = Rmat(1,KL) + Alphar(Ires)*Beta7(KL,Ires)
      Rmat(2,KL) = Rmat(2,KL) + Alphai(Ires)*Beta7(KL,Ires)
      ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
C
C *** Check if Rmat is Zero; if so, set Lrmat=1
      KL = 0
      DO K=1,Nchann
      DO L=1,K
      KL = KL + 1
      IF (Rmat(1,KL).NE.ZERO) GO TO 20
      IF (Rmat(2,KL).NE.ZERO) GO TO 20
      ENDDO
      ENDDO
      Lrmat = 1
   20 CONTINUE
C
C
C *** GENERATE Rootp,PH,(H+Rmat) matrices
C ***          Rootp = SQRT(P)
C ***             PH = 1/(S-B+IP)
C ***           Ymat = (1/(S-B+IP) - Rmat)
C
      KL = 0
      DO K=1,Nchann
      DO L=1,K
      KL = KL + 1
      Ymat(1,KL) = - Rmat(1,KL)
      Ymat(2,KL) = - Rmat(2,KL)
      ENDDO
      ENDDO
C
      II = 0
c
c     Loop over channels
c
      DO 50 I=1,Nchann
      Ipx = Ipp(I,Kg)
      II = II + I
      Rootp (I) =  ONE
      Elinvr(I) =  ZERO
      Elinvi(I) = -ONE
c-----------------------------------------------------------------------
c
c     Define penetrability.
c
c-----------------------------------------------------------------------
c-----assume below threshold
      Iffy = 0
      P = 1.0d0
c-----------------------------------------------------------------------
c
c     Only if incident energy is above threshold and Lpent is ON
c
c-----------------------------------------------------------------------
      IF (Su.le.Echan(I,Kg)) go to 40   ! above threshold?
      IF (Lpent(Ipx).NE.1) go to 30     ! penetration turned on
c-----------------------------------------------------------------------
c
c     Penetrability is Calculated
c
c-----------------------------------------------------------------------
      Lsp  = Lspin(I,Kg)
      Bound = Bndry(I,Kg)
c-----2012/2/19 - added DABS
      Ex   = dSQRT (DABS(Su-Echan(I,Kg)))
      Rho  = Zkte(I,Kg)*Ex
      Rhof = Zkfe(I,Kg)*Ex
C-----coulomb or not?
      IF (Zeta7(I,Kg).EQ.ZERO) THEN
c-----no coulomb
      Rho2z = Rho**2  ! SAMRML uses Rho, not Rhof
C-----------------------------------------------------------------------
C
c     Valentin Insert - Start
C
C-----------------------------------------------------------------------
c     CALL FACTS3 (Lsp, Rho2z, P)
      CALL FACTS2 (Lsp, Rho2z, S, P)
c               =              =
C-----------------------------------------------------------------------
C
c     Valentin Insert - End
C
C-----------------------------------------------------------------------
      ELSE
c-----coulomb
      Eta = Zeta7(I,Kg)/Ex
      CALL COULOMB (Rho, Lsp, Eta, P)
      ENDIF
      if(P.le.1.0d-35) P = 0.0d0
c-----------------------------------------------------------------------
c
c     no shift
c
c-----------------------------------------------------------------------
      call FACPHI(Lsp, Rhof, Psf) ! SAMRML uses Rhof, not Rho
      Sinphi(I) = Dsin(Psf)
      Cosphi(I) = Dcos(Psf)
      Sinsqr(I) = Sinphi(I)**2             ! sin^2(PS)
      Sin2ph(I) = TWO*Sinphi(I)*Cosphi(I)  ! sin(2*PS)
      Rootp(I)  = dSQRT(P)
c-----------------------------------------------------------------------
c
c     Add results to matrices
c
c-----------------------------------------------------------------------
      IF(.NOT.((ONE-P*Rmat(2,II).EQ.ONE .OR. P.LT.Tiny))) THEN
c-----Most cases ===> P is not close to zero
C-----------------------------------------------------------------------
C
c     Valentin Insert - Start
C
C-----------------------------------------------------------------------
      IF(ISHIFT(Ipx).eq.0) THEN
      Hr = 0.0d0
      Hi = 0.0d0
      if(P.ne.0.0d0) Hi = -1.0d0/P
      ELSE
      Sh = TWO*S - BNDRY(I,Kg)
      DEN = Sh*Sh + P*P
      Hr  = Sh/DEN
      Hi  = -P/DEN
      ENDIF
C-----------------------------------------------------------------------
C
c     Valentin Insert - End
C
C-----------------------------------------------------------------------
      Elinvr(I) = Hr
      Elinvi(I) = Hi
      Ymat(1,II) = Hr + Ymat(1,II)
      Ymat(2,II) = Hi + Ymat(2,II)
      ELSE
C-----Rare cases ===> P is very small but non-ZERO
      Ymat(1,II) = P*Ymat(1,II)
      Ymat(2,II) = P*Ymat(2,II) - ONE
      Rmat(1,II) = P*Rmat(1,II)
      Rmat(2,II) = P*Rmat(2,II)
      IF (Nchann.GT.1) THEN
      IF (I.GT.1) THEN
      DO J=1,I-1
      Ji = (I*(I-1))/2 + J
      Ymat(1,Ji) = Rootp(I)*Ymat(1,Ji)
      Ymat(2,Ji) = Rootp(I)*Ymat(2,Ji)
      Rmat(1,Ji) = Rootp(I)*Rmat(1,Ji)
      Rmat(2,Ji) = Rootp(I)*Rmat(2,Ji)
      ENDDO
      ENDIF
      IF (I.LT.Nchann) THEN
      DO J=I+1,Nchann
      Ji = (J*(J-1))/2 + I
      Ymat(1,Ji) = Rootp(I)*Ymat(1,Ji)
      Ymat(2,Ji) = Rootp(I)*Ymat(2,Ji)
      Rmat(1,Ji) = Rootp(I)*Rmat(1,Ji)
      Rmat(2,Ji) = Rootp(I)*Rmat(2,Ji)
      ENDDO
      ENDIF
c-----end channels > 1 loop
      ENDIF
c-----end of rare case - assume P = 1
      Rootp(I) = ONE
      Elinvr(I) = ZERO
      Elinvi(I) = -ONE
      ENDIF
      go to 50
C-----Penetrability is NOT Calculated
   30 Ymat(2,II) = Ymat(2,II) - ONE
      go to 50
c-----Below threshold - assume P = 0
   40 Rootp (I) = ZERO
      Elinvr(I) = ONE
      Elinvi(I) = ZERO
c-----end of channel loop
   50 CONTINUE
C
      IF (Lrmat.NE.1) THEN
C ***    Check if one channel is irrelevant; if so, set to unity
      KL = 0
      DO K=1,Nchann
      KL = KL + K
      IF (Ymat(1,KL).EQ.ZERO .AND. Ymat(2,KL).EQ.ZERO) THEN
      Ymat(1,KL) = ONE
      ENDIF
      ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE Crosss
c=======================================================================
C
C *** PURPOSE -- Form the cross sections Sigma(Ksigma) and the
C ***   ( partial derivatives of the cross section with respect to
C ***   the resonance parameters ) = Dsigma(Ksigma,Ipar)
C
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
      common/rmlcross/Sigma(100)
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
C-----note - Fourpi = 4*pi/100
      DATA Fourpi /0.1256637061435917295385057d0/
C
C *** Initialize
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      do ii=1,NppSum
      Sigma(ii) = 0.0d0
      enddo
c-----------------------------------------------------------------------
c
c     an implicit loop is here faster than an explicit loop.
c
c-----------------------------------------------------------------------
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      Kg = Ngr1
c-----------------------------------------------------------------------
C
C     DO LOOP OVER GROUPS (IE SPIN-PARITY GROUPS)  -
C     GOES TO END OF ROUTINE
C
c-----------------------------------------------------------------------
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      if(Kg.le.1) then
      Maxr   = 0
      else
      Maxr   = Nresg(Kg-1)
      endif
c-----------------------------------------------------------------------
C
C     Initialize for this group
c     2014/2/20 - Extended for Multiple Energy Ranges.
c
c-----------------------------------------------------------------------
   10 do ii=1,NppSum
      Crss(ii) = 0.0d0
      enddo
C
      Minr = Maxr + 1
      Maxr = Nresg(Kg)
      Nchann = Nchan(Kg)
      Nentnn = Nent(Kg)
      Nextnn = Next(Kg)
C
      Lrmat = 0
C ***    Set R-Matrix and other necessary arrays
c
      CALL Setr
C
      IF (Lrmat.EQ.1) THEN
C ***       Calculate Xq & Xxxx matrices if trivial
      CALL Zeror
      ELSE
C
C ***       INVERT Ymat; note that Xqr is "Dummy" here
      CALL Yinvrs
C ***       GENERATE XQ & Xxxx matrices
      CALL Setxqx
      ENDIF
C
      Dgoj = Goj(Kg)
C ***    Generate cross section pieces
c
      CALL Sectio
C
C ***    Done calculating contribution for this group; ergo, add to tota
      DO Ip=1,Npp
      Sigma(Ip) = Crss(Ip) + Sigma(Ip)
      ENDDO
C
c----- end of implicit loop
      Kg = Kg + 1
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      if(Kg.le.Ngr2  ) go to 10
C
C *** Normalize properly
      DO Ip=1,Npp
C-----note - Fourpi = 4*pi/100
      Sigma(Ip) = Sigma(Ip)*Fourpi/Su
      ENDDO
C
C
      RETURN
      END
      SUBROUTINE Answer1
c=======================================================================
c
c     define Mt7's to Output.
c
c=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
c-----limit of resolved region for SAMRML
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      common/usemt/dumrml(100),Quse(11),Mtuse(11)
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
      common/FISSY/LFWX,LFI,MT451,LFWSUM
c
      if(LRF7.gt.0) go to 10  ! LRF7 = # of LRF=7 sections
c-----------------------------------------------------------------------
c
c     NO LRF=7 = Simple case of other formalisms.
c
c-----------------------------------------------------------------------
      MTREACT(1) = 1
      MTREACT(2) = 2
      MTREACT(3) = 102
      QREACT (1) = 0.0d0
      QREACT (2) = 0.0d0
      QREACT (3) = 0.0d0
      NREACT     = 3
      if(LFWSUM.gt.0) then
      MTREACT(4) = 18
      QREACT (4) = 0.0d0
      NREACT     = 4
      endif
      go to 20
c-----------------------------------------------------------------------
c
c     General LRF = 7 case
c
c-----------------------------------------------------------------------
c-----this NOW handles fission before and after LRF=7 section.
   10 IMFISSY = 0
      if(LFWSUM.gt.0) IMFISSY = 1
c-----does LRF=7 section have fission?
      nofiss = 0
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      do i=1,NppSum
      if(MtuseSum(i).eq.18) nofiss = 1 ! yes
      enddo
c-----assume MT =1, 2 and 102 are first 3 output
      MTREACT(1) = 1
      MTREACT(2) = 2
      MTREACT(3) = 102
      QREACT (1) = 0.0d0
      QREACT (2) = 0.0d0
      QREACT (3) = 0.0d0
      NREACT     = 3
c-----if other range has fission and current does not, add fission.
      if(nofiss.ne.0) IMFISSY = 0
      if(IMFISSY.eq.1.and.nofiss.eq.0) then
      MTREACT(4) = 18
      QREACT (4) = 0.0d0
      NREACT     = 4
      endif
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      if(NPPSum.le.2) go to 20
      do i=1,NppSum
      if(MtuseSum(i).ne.2.and.MtuseSum(i).ne.102) then
      NREACT = NREACT + 1
      MTREACT(NREACT) = MtuseSum(i)
      QREACT (NREACT) = QuseSum (i)
      endif
      enddo
c-----------------------------------------------------------------------
c
c     All cases - print summary
c
c-----------------------------------------------------------------------
   20 write(*,30) (MTREACT(i),i=1,NREACT)
      write(3,30) (MTREACT(i),i=1,NREACT)
   30 format(1x,78('=')/' Summary of Calculated MT #'/1x,78('-')/(10i5))
      return
      END
      SUBROUTINE Answers
C=======================================================================
c
c     Transform from Samrml to RECENT results
c
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlcross/Sigma(100)     ! Samrml results
      common/rmlfinal/rmlsigma(11)   ! RECENT results
      common/outmt/QREACT(11),MTREACT(11),NEGTAB(11),NREACT,IMFISSY,LRF7
c-----------------------------------------------------------------------
c
c     WARNING - rmlsigma is NOT initialized = MUST define ALL.
c
c-----------------------------------------------------------------------
c-----Initialize ALL.
      do i=1,11
      rmlsigma(i) = 0.0d0
      enddo
c----=total = elastic + non-elastic
      rmlsigma(1) = Sigma(1) + Sigma(2)
c-----elastic
      rmlsigma(2) = Sigma(1)
c-----capture (subtract others, if any)
      if(Npp.le.2) then
c-----only capture
      rmlsigma(3) = Sigma(2)
      if(IMFISSY.eq.1) rmlsigma(4) = 0.0d0
      return
      endif
c-----otherwise, subtract others to define capture
      subrml = 0.0d0
      do i=3,Npp
      subrml = subrml + Sigma(i)
      enddo
      rmlsigma(3) = Sigma(2) - subrml
c-----define remaining
      KPP = 3
      if(IMFISSY.eq.1) then   ! if needed, define fission = 0
      rmlsigma(4) = 0.0d0
      KPP = 4
      endif
      do i=3,Npp
      KPP = KPP + 1
      rmlsigma(KPP) = Sigma(i)
      enddo
      RETURN
      END
      SUBROUTINE Zeror
C ======================================================================
C
C The following routines are from
C -------------------------------
C NIST Guide to Available Math Software.
C Source for module Coulfg from package COULOMB.
C Retrieved from TIBER on Fri Apr 3 15:08:25 1998.
C ======================================================================
C *** Converted to double precision.   8 Apr 98.   ROS
C *** Specialized to avoid excess computations in SAMMY.  29 Dec 99. NML
C *** Latest update 19 June 2002 NML
C
C ======================================================================
C=======================================================================
C
C     Initialize arrays
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      N = Nchann
      JK = 0
      DO J=1,N
      DO K=1,J
      JK = JK + 1
      Xxxxr(JK) = ZERO
      Xxxxi(JK) = ZERO
      ENDDO
      ENDDO
      DO J=1,N
      DO K=1,N
      Xqr(K,J) = ZERO
      Xqi(K,J) = ZERO
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE Scale3 (V1, V2, V3, Rmat)
C=======================================================================
C
C=======================================================================
      INCLUDE 'implicit.h'
      DIMENSION Rmat(2,6)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
      Aa = 1.D10
      V1 = ZERO
      V2 = ZERO
      V3 = ZERO
C
      IF (DABS(Rmat(1,1)).GE.Aa .OR. DABS(Rmat(2,1)).GE.Aa) THEN
      Bb = DABS(Rmat(1,1))
      Cc = DABS(Rmat(2,1))
      IF (Cc.GT.Bb) Bb = Cc
      V1 = dSQRT(Bb)
      Rmat(1,1) = Rmat(1,1)/Bb
      Rmat(2,1) = Rmat(2,1)/Bb
      Rmat(1,2) = Rmat(1,2)/V1
      Rmat(2,2) = Rmat(2,2)/V1
      Rmat(1,4) = Rmat(1,4)/V1
      Rmat(2,4) = Rmat(2,4)/V1
      ENDIF
C
      IF (DABS(Rmat(1,3)).GE.Aa .OR. DABS(Rmat(2,3)).GE.Aa) THEN
      Bb = DABS(Rmat(1,3))
      Cc = DABS(Rmat(2,3))
      IF (Cc.GT.Bb) Bb = Cc
      V2 = dSQRT(Bb)
      Rmat(1,2) = Rmat(1,2)/V2
      Rmat(2,2) = Rmat(2,2)/V2
      Rmat(1,3) = Rmat(1,3)/Bb
      Rmat(2,3) = Rmat(2,3)/Bb
      Rmat(1,5) = Rmat(1,5)/V2
      Rmat(2,5) = Rmat(2,5)/V2
      ENDIF
C
      IF (DABS(Rmat(1,6)).GE.Aa .OR. DABS(Rmat(2,6)).GE.Aa) THEN
      Bb = DABS(Rmat(1,6))
      Cc = DABS(Rmat(2,6))
      IF (Cc.GT.Bb) Bb = Cc
      V3 = dSQRT(Bb)
      Rmat(1,4) = Rmat(1,4)/V3
      Rmat(2,4) = Rmat(2,4)/V3
      Rmat(1,5) = Rmat(1,5)/V3
      Rmat(2,5) = Rmat(2,5)/V3
      Rmat(1,6) = Rmat(1,6)/Bb
      Rmat(2,6) = Rmat(2,6)/Bb
      ENDIF
C
      RETURN
      END
      SUBROUTINE Unscl3 (V1, V2, V3, Rmat, Rinv)
C=======================================================================
C
C=======================================================================
      INCLUDE 'implicit.h'
      DIMENSION Rmat(2,6), Rinv(2,6)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
      IF (V1.GT.ZERO) THEN
      Bb = V1**2
      Rmat(1,1) = Rmat(1,1)*Bb
      Rmat(2,1) = Rmat(2,1)*Bb
      Rmat(1,2) = Rmat(1,2)*V1
      Rmat(2,2) = Rmat(2,2)*V1
      Rmat(1,4) = Rmat(1,4)*V1
      Rmat(2,4) = Rmat(2,4)*V1
      Rinv(1,1) = Rinv(1,1)/Bb
      Rinv(2,1) = Rinv(2,1)/Bb
      Rinv(1,2) = Rinv(1,2)/V1
      Rinv(2,2) = Rinv(2,2)/V1
      Rinv(1,4) = Rinv(1,4)/V1
      Rinv(2,4) = Rinv(2,4)/V1
      ENDIF
C
      IF (V2.GT.ZERO) THEN
      Bb = V2**2
      Rmat(1,2) = Rmat(1,2)*V2
      Rmat(2,2) = Rmat(2,2)*V2
      Rmat(1,3) = Rmat(1,3)*Bb
      Rmat(2,3) = Rmat(2,3)*Bb
      Rmat(1,5) = Rmat(1,5)*V2
      Rmat(2,5) = Rmat(2,5)*V2
      Rinv(1,2) = Rinv(1,2)/V2
      Rinv(2,2) = Rinv(2,2)/V2
      Rinv(1,3) = Rinv(1,3)/Bb
      Rinv(2,3) = Rinv(2,3)/Bb
      Rinv(1,5) = Rinv(1,5)/V2
      Rinv(2,5) = Rinv(2,5)/V2
      ENDIF
C
      IF (V3.GT.ZERO) THEN
      Bb = V3**2
      Rmat(1,4) = Rmat(1,4)*V3
      Rmat(2,4) = Rmat(2,4)*V3
      Rmat(1,5) = Rmat(1,5)*V3
      Rmat(2,5) = Rmat(2,5)*V3
      Rmat(1,6) = Rmat(1,6)*Bb
      Rmat(2,6) = Rmat(2,6)*Bb
      Rinv(1,4) = Rinv(1,4)/V3
      Rinv(2,4) = Rinv(2,4)/V3
      Rinv(1,5) = Rinv(1,5)/V3
      Rinv(2,5) = Rinv(2,5)/V3
      Rinv(1,6) = Rinv(1,6)/Bb
      Rinv(2,6) = Rinv(2,6)/Bb
      ENDIF
C
      RETURN
      END
      SUBROUTINE Xspfa (Bp, N, Kpvt, Info)
C=======================================================================
C
C *** modified October 14, 1993, by NML to use for complex arrays
C
C     Xspfa FACTORS A COMPLEX SYMMETRIC MATRIX STORED IN
C     PACKED FORM BY ELIMINATION WITH SYMMETRIC PIVOTING.
C
C     TO SOLVE  A*X = B , FOLLOW Xspfa BY Xspsl.
C     TO COMPUTE  INVERSE(A)*C , FOLLOW Xspfa BY Xspsl.
C     TO COMPUTE  DETERMINANT(A) , FOLLOW Xspfa BY Xspdi.
C     TO COMPUTE  INVERSE(A) , FOLLOW Xspfa BY Xspdi... which I don't ha
C
C     ON ENTRY
C
C        Bp      REAL*8 (2,(N*(N+1)/2))
C                Bp(1,*) is real part, Bp(2,*) is imaginary part.
C                THE PACKED FORM OF A SYMMETRIC MATRIX  A .  THE
C                COLUMNS OF THE UPPER TRIangle ARE STORED SEQUENTIALLY
C                IN A ONE-DIMENSIONAL ARRAY OF LENGTH  N*(N+1)/2 .
C                SEE COMMENTS BELOW FOR DETAILS.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     OUTPUT
C
C        Bp      A BLOCK DIAGONAL MATRIX AND THE MULTIPLIERS WHIch
C                WERE USED TO OBTAIN IT STORED IN PACKED FORM.
C                THE FACTORIZATION CAN BE WRITTEN  A = U*D*TRANS(U)
C                WHERE  U  IS A PRODUCT OF PERMUTATION AND UNIT
C                UPPER TRIANGULAR MATRICES , TRANS(U) IS THE
C                TRANSPOSE OF  U , AND  D  IS BLOCK DIAGONAL
C                WITH 1 BY 1 AND 2 BY 2 BLOCKS.
C
C        Kpvt    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        Info    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF THE K-TH PIVOT BLOCK IS SINGULAR. THIS IS
C                     Not AN ERROR CONDITION FOR THIS SBROUTINE,
C                     BUT IT DOES INDICATE THAT SSPSL OR SSPDI MAY
C                     DIVIDE BY ZERO IF CALLED.
C
C     PACKED STORAGE
C
C          THE FOLLOWING PROGRAM SEGMENT WILL PACK THE UPPER
C          TRIangle OF A SYMMETRIC MATRIX.
C
C                K = 0
C                DO J=1,N
C                   DO I=1,J
C                      K = K + 1
C                      Bp(K) = A(I,J)
C                   ENDDO
C                ENDDO
C
C     SuB-ROUTINES AND FNCTIONS
C       BLAS Xaxpy, Xswap, Ixamax
C       FORTRAN DABS, dMAX1, dSQRT
C
C     INTERNAL VARIABLES
C=======================================================================
      INCLUDE 'implicit.h'
      DIMENSION Bp(2,*), Kpvt(*)
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
c-----------------------------------------------------------------------
C
C     INITIALIZE
C
C     Alpha IS USED IN CHOOSING PIVOT BLOCK SIZE.
c
c-----------------------------------------------------------------------
      Alpha = (ONE + dSQRT(17.0d0))/8.0d0
C
      Info = 0
c-----------------------------------------------------------------------
C
C     MAIN LOOP ON K, WHIch GOES FROM N TO 1.
C
c-----------------------------------------------------------------------
      K = N
      Ik = (N*(N-1))/2
   10 CONTINUE
c-----------------------------------------------------------------------
C
C        LEAVE THE LOOP IF K=0 OR K=1.
C
C     ...EXIT
c
c-----------------------------------------------------------------------
      IF (K.EQ.0) GO TO 20
      IF (K.LE.1) THEN
      Kpvt(1) = 1
      IF (Bp(1,1).EQ.ZERO .AND. Bp(2,1).EQ.ZERO) Info = 1
C     ......EXIT
      GO TO 20
      ENDIF
c-----------------------------------------------------------------------
C
C        THIS SECTION OF CODE DETERMINES THE KIND OF
C        ELIMINATION TO BE PERFORMED.  WHEN IT IS COMPLETED,
C        Kstep WILL BE SET TO THE SIZE OF THE PIVOT BLOCK, AND
C        ISWAP WILL BE SET TO 1 IF AN INTERCHANGE IS REQUIRED.
C
c-----------------------------------------------------------------------
      Km1 = K - 1
      Kk = Ik + K
      Absakk = Bp(1,Kk)**2 + Bp(2,Kk)**2
c-----------------------------------------------------------------------
C
C        DETERMINE THE LARGEST OFF-DIAGONAL ELEMENT IN COLUMN K.
C
c-----------------------------------------------------------------------
      Imax = Ixamax (K-1, Bp(1,Ik+1), 1)
      Imk = Ik + Imax
      Colmax = Bp(1,Imk)**2 + Bp(2,Imk)**2
C
      IF (Absakk .GE. Alpha*Colmax) THEN
C
      Kstep = 1
      ISWAP = 0
C
      ELSE
c-----------------------------------------------------------------------
C
C     DETERMINE THE LARGEST OFF-DIAGONAL ELEMENT IN ROW Imax.
c
c-----------------------------------------------------------------------
      Rowmax = ZERO
      Imaxp1 = Imax + 1
      Im = (Imax*(Imax-1))/2
      Imj = Im + 2*Imax
      DO J=Imaxp1,K
      Aa = Bp(1,Imj)**2 + Bp(2,Imj)**2
      Rowmax = dMAX1 (Rowmax, Aa)
      Imj = Imj + J
      ENDDO
      IF (Imax.NE.1) THEN
      Jmax = Ixamax (Imax-1, Bp(1,IM+1), 1)
      Jmim = Jmax + IM
      Aa = Bp(1,Jmim)**2 + Bp(2,Jmim)**2
      Rowmax = dMAX1(Rowmax, Aa)
      ENDIF
      Imim = Imax + Im
      Aa = Bp(1,Imim)**2 + Bp(2,Imim)**2
      IF (Aa .GE. Alpha*Rowmax) THEN
      Kstep = 1
      ISWAP = 1
      ELSE
      IF (Absakk .GE. Alpha*Colmax*(Colmax/Rowmax)) THEN
      Kstep = 1
      ISWAP = 0
      ELSE
      Kstep = 2
      if(Imax .NE. Km1) then
      ISWAP = 1
      else
      ISWAP = 0
      endif
      ENDIF
      ENDIF
C
      ENDIF
C
      IF (dMAX1(Absakk,Colmax) .EQ. ZERO) THEN
C
C           COLUMN K IS ZERO.  SET Info AND ITERATE THE LOOP.
      Kpvt(K) = K
      Info = K
      ELSE
C
      IF (Kstep.NE.2) THEN
C              1 X 1 PIVOT BLOCK.
      if(ISWAP.ne.0) then
C                 PERFORM AN INTERCHANGE.
      CALL Xswap (Imax, Bp(1,IM+1), 1, Bp(1,Ik+1), 1)
      Imj = Ik + Imax
      DO Jj=Imax,K
      J = K + Imax - Jj
      Jk = Ik + J
      T = Bp(1,Jk)
      Bp(1,Jk) = Bp(1,Imj)
      Bp(1,Imj) = T
      T = Bp(2,Jk)
      Bp(2,Jk) = Bp(2,Imj)
      Bp(2,Imj) = T
      Imj = Imj - (J - 1)
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
C
C     PERFORM THE ELIMINATION.
c
c-----------------------------------------------------------------------
      Ij = Ik - (K-1)
      DO Jj=1,Km1
      J = K - Jj
      Jk = Ik + J
      Aa = Bp(1,Kk)**2 + Bp(2,Kk)**2
      Dmulk  = -(Bp(1,Jk)*Bp(1,Kk)+Bp(2,Jk)*Bp(2,Kk))/Aa
      Dmulki =  (Bp(1,Jk)*Bp(2,Kk)-Bp(2,Jk)*Bp(1,Kk))/Aa
      T  = Dmulk
      TI = Dmulki
      CALL Xaxpy (J, T, TI, Bp(1,Ik+1), 1, Bp(1,Ij+1), 1)
      Bp(1,Jk) = Dmulk
      Bp(2,Jk) = Dmulki
      Ij = Ij - (J - 1)
      ENDDO
c-----------------------------------------------------------------------
C
C     SET THE PIVOT ARRAY.
C
c-----------------------------------------------------------------------
      Kpvt(K) = K
      IF (ISWAP.ne.0) Kpvt(K) = Imax
C
      ELSE
C           ELSE IF (Kstep.EQ.2)
C
C              2 X 2 PIVOT BLOCK.
      Km1k = Ik + K - 1
      Ikm1 = Ik - (K-1)
      IF (ISWAP.ne.0) THEN
c-----------------------------------------------------------------------
C
C     PERFORM AN INTERCHANGE.
c
c-----------------------------------------------------------------------
      CALL Xswap (Imax, Bp(1,IM+1), 1, Bp(1,Ikm1+1), 1)
      Imj = Ikm1 + Imax
      DO Jj=Imax,Km1
      J = Km1 + Imax - Jj
      Jkm1 = Ikm1 + J
      T = Bp(1,Jkm1)
      Bp(1,Jkm1) = Bp(1,Imj)
      Bp(1,Imj) = T
      T = Bp(2,Jkm1)
      Bp(2,Jkm1) = Bp(2,Imj)
      Bp(2,Imj) = T
      Imj = Imj - (J - 1)
      ENDDO
      T = Bp(1,Km1k)
      Bp(1,Km1k) = Bp(1,Imk)
      Bp(1,Imk) = T
      T = Bp(2,Km1k)
      Bp(2,Km1k) = Bp(2,Imk)
      Bp(2,Imk) = T
      ENDIF
c-----------------------------------------------------------------------
C
C     PERFORM THE ELIMINATION.
c
c-----------------------------------------------------------------------
      Km2 = K - 2
      IF (Km2.NE.0) THEN
      Aa = Bp(1,Km1k)**2 + Bp(2,Km1k)**2
      Ak  = (Bp(1,Kk)*Bp(1,Km1k)+Bp(2,Kk)*Bp(2,Km1k))/Aa
      Aki = (Bp(2,Kk)*Bp(1,Km1k)-Bp(1,Kk)*Bp(2,Km1k))/Aa
      Km1km1 = Ikm1 + K - 1
      Akm1  = ( Bp(1,Km1km1)*Bp(1,Km1k) +
     *                      Bp(2,Km1km1)*Bp(2,Km1k) ) /Aa
      Akm1i = ( Bp(2,Km1km1)*Bp(1,Km1k) -
     *                      Bp(1,Km1km1)*Bp(2,Km1k) ) /Aa
      Denom  = ONE - (Ak*Akm1-Aki*Akm1i)
      Denomi =     - (Ak*Akm1i+Aki*Akm1)
      Dd = Denom**2 + Denomi**2
      Ij = Ik - (K-1) - (K-2)
      DO Jj=1,Km2
      J = Km1 - Jj
      Jk = Ik + J
      Bk  = (Bp(1,Jk)*Bp(1,Km1k)+Bp(2,Jk)*Bp(2,Km1k))/Aa
      Bki = (Bp(2,Jk)*Bp(1,Km1k)-Bp(1,Jk)*Bp(2,Km1k))/Aa
      Jkm1 = Ikm1 + J
      Bkm1  = ( Bp(1,Jkm1)*Bp(1,Km1k) +
     *                         Bp(2,Jkm1)*Bp(2,Km1k) ) /Aa
      Bkm1i = ( Bp(2,Jkm1)*Bp(1,Km1k) -
     *                         Bp(1,Jkm1)*Bp(2,Km1k) ) /Aa
      Xx  = Akm1*Bk - Akm1i*Bki - Bkm1
      XxI = Akm1*Bki + Akm1i*Bk - Bkm1i
      Dmulk  = (Xx*Denom+XxI*Denomi)/Dd
      Dmulki = (XxI*Denom-Xx*Denomi)/Dd
      Xx  = Ak*Bkm1 - Aki*Bkm1i - Bk
      XxI = Ak*Bkm1i + Aki*Bkm1 - Bki
      Dmlkm1 = (Xx*Denom+XxI*Denomi)/Dd
      Dmlkmi = (XxI*Denom-Xx*Denomi)/Dd
      T  = Dmulk
      TI = Dmulki
      CALL Xaxpy (J, T, TI, Bp(1,Ik+1), 1, Bp(1,Ij+1), 1)
      T  = Dmlkm1
      TI = Dmlkmi
      CALL Xaxpy (J, T, TI, Bp(1,Ikm1+1), 1,Bp(1,Ij+1),1)
      Bp(1,Jk) = Dmulk
      Bp(2,Jk) = Dmulki
      Bp(1,Jkm1) = Dmlkm1
      Bp(2,Jkm1) = Dmlkmi
      Ij = Ij - (J-1)
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
C
C              SET THE PIVOT ARRAY.
c
c-----------------------------------------------------------------------
      Kpvt(K) = 1 - K
      IF (ISWAP.ne.0) Kpvt(K) = -Imax
      Kpvt(K-1) = Kpvt(K)
      ENDIF
      ENDIF
      Ik = Ik - (K-1)
      IF (Kstep.EQ.2) Ik = Ik - (K-2)
      K = K - Kstep
      GO TO 10
   20 CONTINUE
      RETURN
      END
      INTEGER*4 FUNCTION Ixamax (N, Sx, Incx)
C=======================================================================
C
C     FINDS THE INDEX OF ELEMENT HAVING Maximum squared value
C
C=======================================================================
      INCLUDE 'implicit.h'
      DIMENSION Sx(2,*)
C
      Ixamax = 0
      IF (N.LT.1) RETURN
      Ixamax = 1
      IF (N.EQ.1) RETURN
C
      IF (Incx.NE.1) THEN
c-----------------------------------------------------------------------
C
C        CODE FOR INCREMENT Not EQUAL TO 1
c
c-----------------------------------------------------------------------
      Ix = 1
      Smax = Sx(1,1)**2 + Sx(2,1)**2
      Ix = Ix + Incx
      DO I=2,N
      Aa = Sx(1,Ix)**2 + Sx(2,Ix)**2
      IF (Aa.GT.Smax) THEN
      Ixamax = I
      Smax = Aa
      ENDIF
      Ix = Ix + Incx
      ENDDO
C
      ELSE
c-----------------------------------------------------------------------
C
C        CODE FOR INCREMENT EQUAL TO 1
c
c-----------------------------------------------------------------------
      Smax = Sx(1,1)**2 + Sx(2,1)**2
      DO I=2,N
      Aa = Sx(1,I)**2 + Sx(2,I)**2
      IF (Aa.GT.Smax) THEN
      Ixamax = I
      Smax = Aa
      ENDIF
      ENDDO
C
      ENDIF
      RETURN
      END
      SUBROUTINE Xaxpy (N, Sa, Sai, Sx, Incx, Sy, Incy)
C=======================================================================
C
C     Complex CONSTANT TIMES A VECTOR PLUS Another VECTOR.
C     USES UNROLLED LOOP FOR INCREMENTS EQUAL TO ONE.
C
C=======================================================================
      INCLUDE 'implicit.h'
c-----2021/6/14 - change from ,1) to ,*) - Sx and Sy = dummy arg
c     DIMENSION Sx(2,1), Sy(2,1)
      DIMENSION Sx(2,*), Sy(2,*)
C
      IF (N.LE.0) RETURN
      IF (Sa.EQ.0.0d0 .AND. Sai.EQ.0.0d0) RETURN
C
      IF (Incx.NE.1 .OR. Incy.NE.1) THEN
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          Not EQUAL TO 1
      Ix = 1
      Iy = 1
      IF (Incx.LT.0) Ix = (-N+1)*Incx + 1
      IF (Incy.LT.0) Iy = (-N+1)*Incy + 1
      DO I=1,N
      Aa = Sy(1,Iy) + Sa*Sx(1,Ix) - Sai*Sx(2,Ix)
      Sy(2,Iy) = Sy(2,Iy) + Sa*Sx(2,Ix) + Sai*Sx(1,Ix)
      Sy(1,Iy) = Aa
      Ix = Ix + Incx
      Iy = Iy + Incy
      ENDDO
C
C
      ELSE
c-----------------------------------------------------------------------
C
C     CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C     CLEAN-UP LOOP
c
c-----------------------------------------------------------------------
      M = MOD (N, 4)
      IF (M.NE.0) THEN
      DO I=1,M
      Aa = Sy(1,I) + Sa*Sx(1,I) - Sai*Sx(2,I)
      Sy(2,I) = Sy(2,I) + Sa*Sx(2,I) + Sai*Sx(1,I)
      Sy(1,I) = Aa
      ENDDO
      IF (N.LT.4) RETURN
      ENDIF
      Mp1 = M + 1
      DO I=Mp1,N,4
      Aa        = Sy(1,I  ) + Sa*Sx(1,I  ) - Sai*Sx(2,I  )
      Sy(2,I  ) = Sy(2,I  ) + Sa*Sx(2,I  ) + Sai*Sx(1,I  )
      Sy(1,I  ) = Aa
      Aa        = Sy(1,I+1) + Sa*Sx(1,I+1) - Sai*Sx(2,I+1)
      Sy(2,I+1) = Sy(2,I+1) + Sa*Sx(2,I+1) + Sai*Sx(1,I+1)
      Sy(1,I+1) = Aa
      Aa        = Sy(1,I+2) + Sa*Sx(1,I+2) - Sai*Sx(2,I+2)
      Sy(2,I+2) = Sy(2,I+2) + Sa*Sx(2,I+2) + Sai*Sx(1,I+2)
      Sy(1,I+2) = Aa
      Aa        = Sy(1,I+3) + Sa*Sx(1,I+3) - Sai*Sx(2,I+3)
      Sy(2,I+3) = Sy(2,I+3) + Sa*Sx(2,I+3) + Sai*Sx(1,I+3)
      Sy(1,I+3) = Aa
      ENDDO
C
      ENDIF
      RETURN
      END
      SUBROUTINE XdotQ(N, Sx, Incx, Sy, Incy)
C=======================================================================
c
c     2021/1/30 - Changed from FUNCTION Xdot to SUBROUTINE XdotQ
c                 with Xdotr & Xdoti return through common XDOTCOM
c
C=======================================================================
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
c-----2021/1/29 - Moved   Xdoti from Function argument to common
c-----            Created Xdotr from FUNCTION Xdot (now XdotQ)
      COMMON/XDOTCOM/Xdotr,Xdoti
c-----2021/6/14 - change from ,1) to ,*) - Sx and Sy = dummy arg
c     DIMENSION Sx(2,1), Sy(2,1)
      DIMENSION Sx(2,*), Sy(2,*)
C
      Stemp  = ZERO
      Stempi = ZERO
      Xdotr  = ZERO
      Xdoti  = ZERO
      IF (N.LE.0) RETURN
      IF (Incx.NE.1 .OR. Incy.NE.1) THEN
c-----------------------------------------------------------------------
C
C     CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL TO 1
c
c-----------------------------------------------------------------------
      Ix = 1
      Iy = 1
      IF (Incx.LT.0) Ix = (-N+1)*Incx + 1
      IF (Incy.LT.0) Iy = (-N+1)*Incy + 1
      DO I=1,N
      Stemp  = Stemp  + Sx(1,Ix)*Sy(1,Iy) - Sx(2,Ix)*Sy(2,Iy)
      Stempi = Stempi + Sx(2,Ix)*Sy(1,Iy) + Sx(1,Ix)*Sy(2,Iy)
      Ix = Ix + Incx
      Iy = Iy + Incy
      ENDDO
      Xdotr = Stemp
      Xdoti = Stempi
C
      ELSE
c-----------------------------------------------------------------------
C
C     CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C     CLEAN-UP LOOP
c
c-----------------------------------------------------------------------
      M = MOD (N, 5)
      IF (M.NE.0) THEN
      DO I=1,M
      Stemp  = Stemp  + Sx(1,I)*Sy(1,I) - Sx(2,I)*Sy(2,I)
      Stempi = Stempi + Sx(2,I)*Sy(1,I) + Sx(1,I)*Sy(2,I)
      ENDDO
      IF (N.LT.5) THEN
      Xdotr = Stemp
      Xdoti = Stempi
      RETURN
      ENDIF
      ENDIF
      Mp1 = M + 1
      DO I=Mp1,N,5
      Stemp  = Stemp  + Sx(1,I  )*Sy(1,I  ) - Sx(2,I  )*Sy(2,I  )
     *                      + Sx(1,I+1)*Sy(1,I+1) - Sx(2,I+1)*Sy(2,I+1)
     *                      + Sx(1,I+2)*Sy(1,I+2) - Sx(2,I+2)*Sy(2,I+2)
     *                      + Sx(1,I+3)*Sy(1,I+3) - Sx(2,I+3)*Sy(2,I+3)
     *                      + Sx(1,I+4)*Sy(1,I+4) - Sx(2,I+4)*Sy(2,I+4)
      Stempi = Stempi + Sx(2,I  )*Sy(1,I  ) + Sx(1,I  )*Sy(2,I  )
     *                      + Sx(2,I+1)*Sy(1,I+1) + Sx(1,I+1)*Sy(2,I+1)
     *                      + Sx(2,I+2)*Sy(1,I+2) + Sx(1,I+2)*Sy(2,I+2)
     *                      + Sx(2,I+3)*Sy(1,I+3) + Sx(1,I+3)*Sy(2,I+3)
     *                      + Sx(2,I+4)*Sy(1,I+4) + Sx(1,I+4)*Sy(2,I+4)
      ENDDO
      Xdotr = Stemp
      Xdoti = Stempi
C
      ENDIF
      RETURN
      END
      SUBROUTINE Xswap (N, SX, Incx, SY, Incy)
C=======================================================================
C
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
C
C=======================================================================
      INCLUDE 'implicit.h'
c-----2021/6/14 - change from ,1) to ,*) - Sx and Sy = dummy arg
c     DIMENSION Sx(2,1), Sy(2,1)
      DIMENSION Sx(2,*), Sy(2,*)
C
      IF (N.LE.0) RETURN
      IF (Incx.NE.1 .OR. Incy.NE.1) THEN
c-----------------------------------------------------------------------
C
C     CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL TO 1
c
c-----------------------------------------------------------------------
      Ix = 1
      Iy = 1
      IF (Incx.LT.0) Ix = (-N+1)*Incx + 1
      IF (Incy.LT.0) Iy = (-N+1)*Incy + 1
      DO I=1,N
      Stemp    = Sx(1,Ix)
      Sx(1,Ix) = Sy(1,Iy)
      Sy(1,Iy) = Stemp
      Stemp    = Sx(2,Ix)
      Sx(2,Ix) = Sy(2,Iy)
      Sy(2,Iy) = Stemp
      Ix = Ix + Incx
      Iy = Iy + Incy
      ENDDO
C
      ELSE
c-----------------------------------------------------------------------
C
C     CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C     CLEAN-UP LOOP
c
c-----------------------------------------------------------------------
      M = MOD (N, 3)
      IF (M.NE.0) THEN
      DO I=1,M
      Stemp   = Sx(1,I)
      Sx(1,I) = Sy(1,I)
      Sy(1,I) = Stemp
      Stemp   = Sx(2,I)
      Sx(2,I) = Sy(2,I)
      Sy(2,I) = Stemp
      ENDDO
      IF (N.LT.3) RETURN
      ENDIF
      Mp1 = M + 1
      DO I=Mp1,N,3
      Stemp     = Sx(1,I  )
      Sx(1,I  ) = Sy(1,I  )
      Sy(1,I  ) = Stemp
      Stemp     = Sx(2,I  )
      Sx(2,I  ) = Sy(2,I  )
      Sy(2,I  ) = Stemp
      Stemp     = Sx(1,I+1)
      Sx(1,I+1) = Sy(1,I+1)
      Sy(1,I+1) = Stemp
      Stemp     = Sx(2,I+1)
      Sx(2,I+1) = Sy(2,I+1)
      Sy(2,I+1) = Stemp
      Stemp     = Sx(1,I+2)
      Sx(1,I+2) = Sy(1,I+2)
      Sy(1,I+2) = Stemp
      Stemp     = Sx(2,I+2)
      Sx(2,I+2) = Sy(2,I+2)
      Sy(2,I+2) = Stemp
      ENDDO
C
      ENDIF
      RETURN
      END
      SUBROUTINE Xspsl (Bp, N, Kpvt, B)
C=======================================================================
C
C     Xspsl SOLVES THE complex SYMMETRIC SYSTEM
C                 A * X = B
C     USING THE FACTORS COMPUTED BY Xspfa.
C
C     ON ENTRY
C
C        Bp      REAL*8 (2,N*(N+1)/2)
C                THE OUTPUT FROM SSPFA.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        Kpvt    INTEGER(N)
C                THE PIVOT VECTOR FROM SSPFA.
C
C        B       REAL(2,N)
C                THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A Division BY ZERO MAY OCCUR IF  Xspco  HAS SET RCOND .EQ. 0.0
C        OR  Xspfa  HAS SET Info .NE. 0  .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL Xspfa (Bp, N, Kpvt, Info)
C           IF (Info .NE. 0) GO TO ...
C           DO J=1,P
C              CALL Xspsl (Bp, N, Kpvt, C(1,J))
C           ENDDO
C
C     SBROUTINES AND FNCTIONS
C
C     BLAS Xaxpy, XdotQ
C     FORTRAN IABS
C
C     INTERNAL VARIABLES.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
c-----2021/1/29 - Moved Xdoti from Function argument to common
c-----            Created Xdotr from FUNCTION Xdot (now XdotQ)
      COMMON/XDOTCOM/Xdotr,Xdoti
c-----2021/6/14 - change from ,1) to ,*) - Bp and B = dummy arg
c     DIMENSION Bp(2,1), B(2,1), Kpvt(*)
      DIMENSION Bp(2,*), B(2,*), Kpvt(*)
c-----------------------------------------------------------------------
C
C     LOOP BACKWARD APPLYING THE TRANSFORMATIONS AND
C     D INVERSE TO B.
C
c-----------------------------------------------------------------------
      K = N
      Ik = (N*(N-1))/2
   10 CONTINUE
      IF (K.NE.0) THEN
      Kk = Ik + K
C
      IF (Kpvt(K).GE.0) THEN
C
C           1 X 1 PIVOT BLOCK.
      IF (K.NE.1) THEN
      Kp = Kpvt(K)
      IF (Kp.NE.K) THEN
C                 INTERCHANGE.
      Temp    = B(1,K)
      B(1,K)  = B(1,Kp)
      B(1,Kp) = Temp
      Temp    = B(2,K)
      B(2,K)  = B(2,Kp)
      B(2,Kp) = Temp
      ENDIF
C              APPLY THE TRANSFORMATION.
      CALL Xaxpy (K-1, B(1,K), B(2,K), Bp(1,Ik+1), 1, B(1,1),1)
      ENDIF
C           APPLY D INVERSE.
      Aa = Bp(1,Kk)**2 + Bp(2,Kk)**2
      Ab     = (B(1,K)*Bp(1,Kk)+B(2,K)*Bp(2,Kk))/Aa
      B(2,K) = (B(2,K)*Bp(1,Kk)-B(1,K)*Bp(2,Kk))/Aa
      B(1,K) = Ab
      K = K - 1
      Ik = Ik - K
C
      ELSE
C        ELSE IF (Kpvt(K).LT.0)
C
C           2 X 2 PIVOT BLOCK.
      Ikm1 = Ik - (K-1)
      IF (K.NE.2) THEN
      Kp = IABS(Kpvt(K))
      IF (Kp.NE.K-1) THEN
C                 INTERCHANGE.
      Temp     = B(1,K-1)
      B(1,K-1) = B(1,Kp)
      B(1,Kp)  = Temp
      Temp     = B(2,K-1)
      B(2,K-1) = B(2,Kp)
      B(2,Kp)  = Temp
      ENDIF
C              APPLY THE TRANSFORMATION.
      CALL Xaxpy (K-2, B(1,K  ), B(2,K  ), Bp(1,Ik  +1),
     *                       1, B(1,1), 1)
      CALL Xaxpy (K-2, B(1,K-1), B(2,K-1), Bp(1,Ikm1+1),
     *                       1, B(1,1), 1)
      ENDIF
C           APPLY D INVERSE.
      Km1k = Ik + K - 1
      Kk = Ik + K
      Aa = Bp(1,Km1k)**2 + Bp(2,Km1k)**2
      Ak  = (Bp(1,Kk)*Bp(1,Km1k)+Bp(2,Kk)*Bp(2,Km1k))/Aa
      Aki = (Bp(2,Kk)*Bp(1,Km1k)-Bp(1,Kk)*Bp(2,Km1k))/Aa
      Km1km1 = Ikm1 + K - 1
      Akm1  = (Bp(1,Km1km1)*Bp(1,Km1k)+Bp(2,Km1km1)*Bp(2,Km1k))/Aa
      Akm1i = (Bp(2,Km1km1)*Bp(1,Km1k)-Bp(1,Km1km1)*Bp(2,Km1k))/Aa
      Bk  = (B(1,K)*Bp(1,Km1k)+B(2,K)*Bp(2,Km1k))/Aa
      Bki = (B(2,K)*Bp(1,Km1k)-B(1,K)*Bp(2,Km1k))/Aa
      Bkm1  = (B(1,K-1)*Bp(1,Km1k)+B(2,K-1)*Bp(2,Km1k))/Aa
      Bkm1i = (B(2,K-1)*Bp(1,Km1k)-B(1,K-1)*Bp(2,Km1k))/Aa
      Denom  = Ak*Akm1 - Aki*Akm1i - ONE
      Denomi = Ak*Akm1i + Aki*Akm1
      Dd = Denom**2 + Denomi**2
      Ab  = Akm1*Bk - Akm1i*Bki - Bkm1
      Abi = Akm1i*Bk + Akm1*Bki - Bkm1i
      B(1,K) = (Ab*Denom+Abi*Denomi)/Dd
      B(2,K) = (Abi*Denom-Ab*Denomi)/Dd
      Ab  = Ak*Bkm1 - Aki*Bkm1i - Bk
      Abi = Aki*Bkm1 + Ak*Bkm1i - Bki
      B(1,K-1) = (Ab*Denom+Abi*Denomi)/Dd
      B(2,K-1) = (Abi*Denom-Ab*Denomi)/Dd
      K = K - 2
      Ik = Ik - (K+1) - K
C
      ENDIF
C
      GO TO 10
      ENDIF
c-----------------------------------------------------------------------
C
C     LOOP FORWARD APPLYING THE TRANSFORMATIONS.
c
c-----------------------------------------------------------------------
      K = 1
      Ik = 0
   20 CONTINUE
      IF (K.LE.N) THEN
      IF (Kpvt(K).GE.0) THEN
C
C           1 X 1 PIVOT BLOCK.
      IF (K.NE.1) THEN
C              APPLY THE TRANSFORMATION.
c-----2021/1/29 - Moved Xdoti from argument of Xdot to common XDOTCOM
      CALL XdotQ (K-1, Bp(1,Ik  +1), 1, B(1,1), 1)
      B(1,K) = B(1,K) + Xdotr
      B(2,K) = B(2,K) + Xdoti
      Kp = Kpvt(K)
      IF (Kp.NE.K) THEN
C                 INTERCHANGE.
      Temp    = B(1,K )
      B(1,K ) = B(1,Kp)
      B(1,Kp) = Temp
      Temp    = B(2,K )
      B(2,K ) = B(2,Kp)
      B(2,Kp) = Temp
      ENDIF
      ENDIF
      Ik = Ik + K
      K  =  K + 1
C
      ELSE
C
C           2 X 2 PIVOT BLOCK.
      IF (K.NE.1) THEN
C              APPLY THE TRANSFORMATION.
c-----2021/1/29 - Moved Xdoti from argument of Xdot to common XDOTCOM
      CALL XdotQ (K-1, Bp(1,Ik  +1), 1, B(1,1), 1)
      B(1,K  ) = B(1,K  ) + Xdotr
      B(2,K  ) = B(2,K  ) + Xdoti
      Ikp1 = Ik + K
c-----2021/1/29 - Moved Xdoti from atgument of Xdot to common XDOTCOM
      CALL XdotQ (K-1, Bp(1,Ikp1+1), 1, B(1,1), 1)
      B(1,K+1) = B(1,K+1) + Xdotr
      B(2,K+1) = B(2,K+1) + Xdoti
      Kp = IABS(Kpvt(K))
      IF (Kp.NE.K) THEN
C                 INTERCHANGE.
      Temp    = B(1,K )
      B(1,K ) = B(1,Kp)
      B(1,Kp) = Temp
      Temp    = B(2,K )
      B(2,K ) = B(2,Kp)
      B(2,Kp) = Temp
      ENDIF
      ENDIF
      Ik = Ik + K + K + 1
      K  = K  + 2
C
      ENDIF
      GO TO 20
      ENDIF
      RETURN
      END
      SUBROUTINE Setxqx
C=======================================================================
C
C *** purpose -- form XQ & XXXX matrices, where
C ***            XQ   = Yinv * Rmat       and
C ***            XXXX = P/L + sqrt(P)/L         (1/L-R)**-1          sqr
C ***                 =       sqrt(P)/(S-B+IP) * Yinv       * Rmat * sqr
C ***                 =       sqrt(P)/L        * XQ                * sqr
C
C ***            Note that the matrix W defined in SAMMY manual is given
C ***                 by W(c,c') = delta(c,c') + 2i XXXX(c,c')
C ***                 as in Eq. (III.D.4) in SAMMY manual R3
C
C ***         ie W    = I + 2i XXXX
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
C
      do i=1,Nchann
      do j=1,Nchann
      Xqr(i,j) = 0.0d0
      Xqi(i,j) = 0.0d0
      enddo
      enddo
C
C *** Xqr(k,i) = (L**-1-R)**-1 * R ... note asymmetry
      DO I=1,Nchann
      DO J=1,Nchann
      Ij = Ijkl(J,I)
      DO K=1,Nchann
      Jk = Ijkl(K,J)
      Xqr(K,I) = Xqr(K,I) + Yinv(1,Ij)*Rmat(1,Jk) -
     *                               Yinv(2,Ij)*Rmat(2,Jk)
      Xqi(K,I) = Xqi(K,I) + Yinv(1,Ij)*Rmat(2,Jk) +
     *                               Yinv(2,Ij)*Rmat(1,Jk)
      ENDDO
      ENDDO
      ENDDO
C
C *** Xxxx = sqrt(P)/L  * xq * sqrt(P) ... symmetric
      IJ = 0
      DO I=1,Nchann
      Plr = Rootp(I)*Elinvr(I)
      Pli = Rootp(I)*Elinvi(I)
      DO J=1,I
      Ij = Ij + 1
      Xxxxr(Ij) = Rootp(J)* (Xqr(J,I)*Plr-Xqi(J,I)*Pli)
      Xxxxi(Ij) = Rootp(J)* (Xqi(J,I)*Plr+Xqr(J,I)*Pli)
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE Sectio
C=======================================================================
C
C *** purpose -- generate pieces of cross sections (except for "4 pi / E
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
c-----2014/2/20 - Extended for Multiple Energy Ranges.
      DO Jj=1,NppSum
      Crss(Jj) = ZERO
      ENDDO
c-----------------------------------------------------------------------
C
C     Entrance channel, Ipp=2
C        elastic  Crss(1) = g*0.25* sum(entrance chs c,c')
C                                         times |(1-U(c,c'))| **2 / Zz
C                         = g* [ sin(phi)**2 * (1-2XXXXi)
C                                - sin(2phi)*XXXXr
C                                + (XXXXr**2 + XXXXi**2) ] / Zz
c
c-----------------------------------------------------------------------
      Ii = 0
      Ij = 0
      DO I=1,Nentnn
      Zz = Zke(I,Kg)**2
      Ii = Ii + I
      Termn = Sinsqr(I)*( ONE - TWO * Xxxxi(Ii) )
     *                    - Sin2ph(I)*Xxxxr(Ii)
      Termn = Termn / Zz
      DO J=1,I
      Ij = Ij + 1
      Ar = ( Xxxxr(Ij)**2 + Xxxxi(Ij)**2 )/Zz
      IF (I.NE.J) Ar = Ar + Ar
      Termn = Termn + Ar
      ENDDO
      Crss(1) = Termn + Crss(1)
      ENDDO
      Crss(1) = Crss(1)*Dgoj
c-----------------------------------------------------------------------
c
C     End of Ipp=2 term (elastic)
C
C     Ipp=1 term, sort-of
C        absorption = g*0.25 * sum(inc c)
C                      [ 1 -  sum(inc c') |U(c,c')| **2 ] / Zz
C                   = - g* (XXXXr**2 + XXXXi**2) / Zz
c
c-----------------------------------------------------------------------
      Ii = 0
      Ij = 0
      DO I=1,Nentnn
      Ii = Ii + I
      Zz = Zke(I,Kg)**2
      Terma =  Xxxxi(Ii) / Zz
      DO J=1,I
      Ij = Ij + 1
      Ar = (- Xxxxr(Ij)**2 - Xxxxi(Ij)**2) / Zz
      IF (I.NE.J) Ar = Ar + Ar
      Terma = Terma + Ar
      ENDDO
      Crss(2) = Terma + Crss(2)
      ENDDO
      Crss(2) = Crss(2)*Dgoj
c-----------------------------------------------------------------------
c
C     End of absorption term
C
C     All other channels, classed by particle-pair number
C        reaction ch c'= g*0.25 * sum(inc c) |U(c,c')|**2 / Zz
C                      = g* (XxxxR**2 + XxxxI**2) / Zz
c
c-----------------------------------------------------------------------
      DO Jj=1,Nextnn
      J = Jj + Nentnn
      IF (J.LE.Nchann) THEN
      Ip = Ipp(J,Kg)
      DO I=1,Nentnn
      Zz = Zke(I,Kg)**2
      Ij = (J*(J-1))/2 + I
C              Ij = Ijkl(I,J) but I < J always
      Crss(Ip) = Crss(Ip) + (Xxxxr(Ij)**2+Xxxxi(Ij)**2)/Zz
      ENDDO
      ENDIF
      ENDDO
      IF (Npp.GT.2) THEN
      DO Ip=3,Npp
      Crss(Ip) = Crss(Ip)*Dgoj
      ENDDO
      ENDIF
c-----------------------------------------------------------------------
c
C     End of other reaction types
C
c-----------------------------------------------------------------------
      RETURN
      END
      INTEGER*4 FUNCTION Ijkl (M,N)
C=======================================================================
C
C     Define Ijkl INDEX
C
C=======================================================================
      INCLUDE 'implicit.h'
      IF (M.LE.N) THEN
      Ijkl = (N*(N-1))/2 + M
      ELSE
      Ijkl = (M*(M-1))/2 + N
      ENDIF
      RETURN
      END
      SUBROUTINE Betset
C=======================================================================
C
C *** PURPOSE -- GENERATE Betapr, Gbetpr, Beta7, U
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
      common/comresol/ereslow,ereshigh,QuseSum(11),KresSum,KgrSum,
     1 Ngr1,Ngr2,MtuseSum(11),NppSum
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
c-----------------------------------------------------------------------
C
C     Convert to Betapr parameters
C
c-----------------------------------------------------------------------
      IF (Nres.GT.0) THEN
c-----------------------------------------------------------------------
C
c     2014/2/20 - Extended for Multiple Energy Ranges.
c
c-----------------------------------------------------------------------
      if(Ngr1.le.1) then
      Minres = 1
      else
      Minres = Nresg(Ngr1-1) + 1
      endif
      DO Ig=Ngr1,Ngr2
C
      Mmaxc = Nchan(Ig)
      DO J=1,Mmaxc
      Lsp = Lspin(J,Ig)
      DO Ires=Minres,Nresg(Ig)
      Betapr(J,Ires) = ZERO
      Rho = ZERO
      P = ONE
      IF (Gamma(J,Ires).EQ.ZERO) THEN
      ELSE
      IF (Lpent(Ipp(J,Ig)).NE.0) THEN
c-----2012/2/19 - Removed DABS
      Ex = DABS (      Eres(Ires) -Echan(J,Ig) )
      IF (Ex.NE.ZERO) THEN
      Ex = dSQRT(Ex)
      Rho = Zkte(J,Ig)*Ex
      IF (Zeta7(J,Ig).EQ.ZERO) THEN
c-----no coulomb
      Rho2z = Rho**2
      CALL FACTS3 (Lsp, Rho2z, P)
      if(P.le.0.0d0) P = 1.0d0
      ELSE
c-----coulomb
      Eta = Zeta7(J,Ig)/Ex
      CALL COULOMB (Rho, Lsp, Eta, P)
      if(P.le.0.0d0) P = 1.0d0
      ENDIF
      ENDIF
      ENDIF
      Betapr(J,Ires) = dSQRT(HALF*DABS(Gamma(J,Ires))/P)
      IF (Gamma(J,Ires).LT.ZERO) Betapr(J,Ires) =
     *                                      - Betapr(J,Ires)
      ENDIF
      ENDDO
      ENDDO
c-----------------------------------------------------------------------
C
C     Generate Beta7 parameters
c
c-----------------------------------------------------------------------
      DO Ires=Minres,Nresg(Ig)
      KL = 0
      DO K=1,Mmaxc
      DO L=1,K
      KL = KL + 1
      Beta7(KL,Ires) = Betapr(L,Ires)*Betapr(K,Ires)
      ENDDO
      ENDDO
      ENDDO
C
      DO Ires=Minres,Nresg(Ig)
      Gbetpr(2,Ires) = HALF*DABS(Gamgam(Ires))
      Gbetpr(1,Ires) = dSQRT(Gbetpr(2,Ires))
      IF (Gamgam(Ires).LT.ZERO) Gbetpr(1,Ires)= -Gbetpr(1,Ires)
      Gbetpr(3,Ires) = Gbetpr(2,Ires)**2
      ENDDO
C
      Minres = Nresg(Ig) + 1
      ENDDO
C
      ENDIF
C
C
      RETURN
      END
      SUBROUTINE Yinvrs
C=======================================================================
C
C *** PURPOSE -- INVERT Ymat TO GIVE Yinv
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
c
      if(Nchann.gt.3) go to 40
      go to (10,20,30),Nchann
c----- 1 Channel
   10 CALL Onech
      return
c----- 2 Channels
   20 CALL Twoch
      return
c----- 3 Channels
   30 CALL Threech
      return
c----- >3 Channels
   40 CALL Fourch
      return
      END
      SUBROUTINE Onech
C=======================================================================
C
C *** PURPOSE -- Invert Ymat to give Yinv, for the one-channel case
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
      IF (Ymat(1,1)+Ymat(2,1).EQ.Ymat(1,1)) THEN
      Yinv(1,1) = ONE/Ymat(1,1)
      Yinv(2,1) = -(Ymat(2,1)/Ymat(1,1))/Ymat(1,1)
C
      ELSE IF (Ymat(1,1)+Ymat(2,1).EQ.Ymat(2,1)) THEN
      Yinv(1,1) = (Ymat(1,1)/Ymat(2,1))/Ymat(2,1)
      Yinv(2,1) = -ONE/Ymat(2,1)
C
      ELSE IF (Ymat(1,1).EQ.ZERO) THEN
      Yinv(1,1) = ZERO
      Yinv(2,1) = -ONE/Ymat(2,1)
C
      ELSE
      Aa = Ymat(1,1)**2 + Ymat(2,1)**2
      Yinv(1,1) = Ymat(1,1)/Aa
      Yinv(2,1) = -Ymat(2,1)/Aa
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE Twoch
C=======================================================================
C
C *** PURPOSE -- Invert Ymat to give Yinv, for the two-channel case
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
      IF (Ymat(1,1).NE.ZERO .OR. Ymat(1,2).NE.ZERO .OR.
     *    Ymat(1,3).NE.ZERO) THEN
c-----------------------------------------------------------------------
C
C        First step must be to scale such that one large number
C           does not cause this to blow up...
c
c-----------------------------------------------------------------------
      V = ZERO
      K = 0
      DO I=1,3
      IF (DABS(Ymat(1,I)).GT.V) K = I
      IF (DABS(Ymat(1,I)).GT.V) V = DABS(Ymat(1,I))
      IF (DABS(Ymat(2,I)).GT.V) K = I
      IF (DABS(Ymat(2,I)).GT.V) V = DABS(Ymat(2,I))
      ENDDO
      IF (V.GT.ZERO) THEN
      IF (K.EQ.2) THEN
      DO I=1,3
      Ymat(1,I) = Ymat(1,I)/V
      Ymat(2,I) = Ymat(2,I)/V
      ENDDO
      ELSE
      Ymat(1,K) = Ymat(1,K)/V
      Ymat(2,K) = Ymat(2,K)/V
      Ymat(1,2) = Ymat(1,2)/dSQRT(V)
      Ymat(2,2) = Ymat(2,2)/dSQRT(V)
      ENDIF
      ENDIF
C
      Bbr = Ymat(1,1)*Ymat(1,3) -
     *      Ymat(2,1)*Ymat(2,3) - Ymat(1,2)**2 +
     *      Ymat(2,2)**2
      Bbi = Ymat(1,1)*Ymat(2,3) +
     *      Ymat(2,1)*Ymat(1,3) - 2.*Ymat(1,2)*
     *      Ymat(2,2)
C
      IF (Bbr+Bbi.NE.Bbi) THEN
      IF (Bbr+Bbi.NE.Bbr) THEN
      Aa = ONE/(Bbr*Bbr+Bbi*Bbi)
      Aar = Bbr*Aa
      Aai = -Bbi*Aa
      ELSE
      Aar = ONE/Bbr
      Aai = -(Bbi/Bbr)/Bbr
      ENDIF
      ELSE
      Aar = (Bbr/Bbi)/Bbi
      Aai = -ONE/Bbi
      ENDIF
C
      Yinv(1,1) =  Aar*Ymat(1,3) - Aai*Ymat(2,3)
      Yinv(2,1) =  Aar*Ymat(2,3) + Aai*Ymat(1,3)
      Yinv(1,2) = -Aar*Ymat(1,2) + Aai*Ymat(2,2)
      Yinv(2,2) = -Aar*Ymat(2,2) - Aai*Ymat(1,2)
      Yinv(1,3) =  Aar*Ymat(1,1) - Aai*Ymat(2,1)
      Yinv(2,3) =  Aar*Ymat(2,1) + Aai*Ymat(1,1)
C
      IF (V.NE.ZERO) THEN
      IF (K.EQ.2) THEN
      DO I=1,3
      Yinv(1,I) = Yinv(1,I)/V
      Yinv(2,I) = Yinv(2,I)/V
      ENDDO
      ELSE
      Yinv(1,K) = Yinv(1,K)/V
      Yinv(2,K) = Yinv(2,K)/V
      Yinv(1,2) = Yinv(1,2)/dSQRT(V)
      Yinv(2,2) = Yinv(2,2)/dSQRT(V)
      ENDIF
      ENDIF
C
C
      ELSE IF (Ymat(2,2).NE.ZERO) THEN
C ***    Here when real part of Ymat is ZERO everywhere, imaginary part
C ***       dense
      Yinv(1,1) = ZERO
      Yinv(1,2) = ZERO
      Yinv(1,3) = ZERO
      V = Ymat(2,1)*Ymat(2,3) - Ymat(2,2)**2
      Yinv(2,1) = -Ymat(2,3)/V
      Yinv(2,2) =  Ymat(2,2)/V
      Yinv(2,3) = -Ymat(2,1)/V
C
C
      ELSE
C ***    Here when only real part of Ymat is ZERO everywhere; imaginary
C ***       part is ZERO off-diagonal and non-ZERO on diagonal
      Yinv(1,1) =  ZERO
      Yinv(1,2) =  ZERO
      Yinv(1,3) =  ZERO
      Yinv(2,1) = -ONE /(Ymat(2,1))
      Yinv(2,2) =  ZERO
      Yinv(2,3) = -ONE /(Ymat(2,3))
C
C
      ENDIF
      RETURN
      END
      SUBROUTINE Threech
C=======================================================================
C
C *** Purpose -- Invert Ymat to give Yinv, for three-channel case
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
      CALL Scale3 (V1, V2, V3, Ymat(1,1))
C
      Izz = 0
      DO I=1,6
      IF (Ymat(1,I).NE.ZERO) Izz = 1
      ENDDO
C
      IF (Ymat(2,2).NE.ZERO) Izz = 1
      IF (Ymat(2,4).NE.ZERO) Izz = 1
      IF (Ymat(2,5).NE.ZERO) Izz = 1
C
C
      IF (Izz.EQ.0) THEN
C
C ***    Here if only imaginary parts of diagonal terms are nonZero
      DO I=1,6
      Yinv(1,I) = ZERO
      Yinv(2,I) = ZERO
      ENDDO
      Yinv(2,1) = -ONE/Ymat(2,1)
      Yinv(2,3) = -ONE/Ymat(2,3)
      Yinv(2,6) = -ONE/Ymat(2,6)
C
      ELSE
C
C ***    Here real parts of diagonal terms are nonZero
      Fc1r = Ymat(1,3)*Ymat(1,6) -
     *          Ymat(2,3)*Ymat(2,6) - Ymat(1,5)**2 +
     *          Ymat(2,5)**2
      Fc1i = Ymat(2,3)*Ymat(1,6) +
     *          Ymat(1,3)*Ymat(2,6) - TWO*Ymat(1,5)*
     *          Ymat(2,5)
      Fc2r = Ymat(1,2)*Ymat(1,6) -
     *          Ymat(2,2)*Ymat(2,6) -
     *          Ymat(1,4)*Ymat(1,5) +
     *          Ymat(2,4)*Ymat(2,5)
      Fc2i = Ymat(2,2)*Ymat(1,6) +
     *          Ymat(1,2)*Ymat(2,6) -
     *          Ymat(1,4)*Ymat(2,5) -
     *          Ymat(2,4)*Ymat(1,5)
      Fc3r = Ymat(1,2)*Ymat(1,5) -
     *          Ymat(2,2)*Ymat(2,5) -
     *          Ymat(1,4)*Ymat(1,3) +
     *          Ymat(2,4)*Ymat(2,3)
      Fc3i = Ymat(2,2)*Ymat(1,5) +
     *          Ymat(1,2)*Ymat(2,5) -
     *          Ymat(2,4)*Ymat(1,3) -
     *          Ymat(1,4)*Ymat(2,3)
C
      Aar = Ymat(1,1)*Fc1r -  Ymat(1,2)*Fc2r +
     *         Ymat(1,4)*Fc3r - (Ymat(2,1)*Fc1i
     *           -Ymat(2,2)*Fc2i+Ymat(2,4)*Fc3i)
      Aai = Ymat(1,1)*Fc1i -  Ymat(1,2)*Fc2i +
     *         Ymat(1,4)*Fc3i + (Ymat(2,1)*Fc1r
     *           -Ymat(2,2)*Fc2r+Ymat(2,4)*Fc3r)
C
      IF (Aar+Aai.NE.Aai) THEN
      IF (Aar+Aai.NE.Aar) THEN
      Aa = Aar**2 + Aai**2
      Dcr = Aar/Aa
      Dci = -Aai/Aa
      ELSE
      Dcr = ONE/Aar
      Dci = -(Aai/Aar)/Aar
      ENDIF
      ELSE
      Dcr = (Aar/Aai)/Aai
      Dci = -ONE/Aai
      ENDIF
C
      Yinv(1,1) = Fc1r*Dcr - Fc1i*Dci
      Yinv(2,1) = Fc1r*Dci + Fc1i*Dcr
      Yinv(1,2) = -Fc2r*Dcr + Fc2i*Dci
      Yinv(2,2) = -Fc2r*Dci - Fc2i*Dcr
      Yinv(1,3) = (Ymat(1,1)*Ymat(1,6)
     *                    -Ymat(1,4)**2
     *                    -Ymat(2,1)*Ymat(2,6)
     *                    +Ymat(2,4)**2               )  *  Dcr
     *              - (    Ymat(2,1)*Ymat(1,6)
     *                -TWO*Ymat(1,4)*Ymat(2,4)
     *                    +Ymat(1,1)*Ymat(2,6)   )  *  Dci
      Yinv(2,3) = (Ymat(1,1)*Ymat(1,6)
     *                    -Ymat(1,4)**2
     *                    -Ymat(2,1)*Ymat(2,6)
     *                    +Ymat(2,4)**2             )   *   Dci
     *              + (    Ymat(2,1)*Ymat(1,6)
     *                -TWO*Ymat(1,4)*Ymat(2,4)
     *                    +Ymat(1,1)*Ymat(2,6) )   *   Dcr
      Yinv(1,4) = Fc3r*Dcr - Fc3i*Dci
      Yinv(2,4) = Fc3r*Dci + Fc3i*Dcr
      Yinv(1,5) = -(Ymat(1,1)*Ymat(1,5)
     *                     -Ymat(1,2)*Ymat(1,4)
     *                     -Ymat(2,1)*Ymat(2,5)
     *                     +Ymat(2,2)*Ymat(2,4)  ) * Dcr
     *               + (    Ymat(1,1)*Ymat(2,5)
     *                     -Ymat(1,2)*Ymat(2,4)
     *                     +Ymat(2,1)*Ymat(1,5)
     *                     -Ymat(2,2)*Ymat(1,4)  ) * Dci
      Yinv(2,5) = -(Ymat(1,1)*Ymat(1,5)
     *                     -Ymat(1,2)*Ymat(1,4)
     *                     -Ymat(2,1)*Ymat(2,5)
     *                     +Ymat(2,2)*Ymat(2,4)  ) * Dci
     *                 - (  Ymat(1,1)*Ymat(2,5)
     *                     -Ymat(1,2)*Ymat(2,4)
     *                     +Ymat(2,1)*Ymat(1,5)
     *                     -Ymat(2,2)*Ymat(1,4)  ) * Dcr
      Yinv(1,6) = (Ymat(1,1)*Ymat(1,3)
     *                    -Ymat(1,2)**2
     *                    -Ymat(2,1)*Ymat(2,3)
     *                    +Ymat(2,2)**2              ) * Dcr
     *             - (     Ymat(2,1)*Ymat(1,3)
     *                -TWO*Ymat(1,2)*Ymat(2,2)
     *                    +Ymat(1,1)*Ymat(2,3)  ) * Dci
      Yinv(2,6) = (Ymat(1,1)*Ymat(1,3)
     *                    -Ymat(1,2)**2
     *                    -Ymat(2,1)*Ymat(2,3)
     *                    +Ymat(2,2)**2              ) * Dci
     *             + (     Ymat(2,1)*Ymat(1,3)
     *                -TWO*Ymat(1,2)*Ymat(2,2)
     *                    +Ymat(1,1)*Ymat(2,3)  ) * Dcr
      CALL Unscl3 (V1, V2, V3, Ymat(1,1), Yinv(1,1))
      ENDIF
      RETURN
      END
      SUBROUTINE Fourch
C=======================================================================
C
C *** PURPOSE -- calculate Ymat**-1, for any number of channels.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INCLUDE 'rmlcom.h'
      common/rmlflags/Dgoj,Nchann,Lrmat,Nentnn,Nextnn
      DIMENSION Dummy(2,MaxNchan),Kpvt(100)
      equivalence (Xqr(1,1),Dummy(1,1))
      COMMON/CONJOB/PI,PI2,ZERO,HALF,ONE,TWO,THREE,FOUR,EIGHT,TEN
C
      CALL Xspfa (Ymat(1,1), Nchann, Kpvt(1), Info)
      IF (Info.NE.0) Write (3,10) Info
   10 FORMAT (' Problem in Xspfa with Info=', I5)
      Kj = 0
      DO K=1,Nchann
      DO J=1,Nchann
      Dummy(1,J) = ZERO
      Dummy(2,J) = ZERO
      ENDDO
      Dummy(1,K) = ONE
      CALL Xspsl (Ymat(1,1), Nchann, Kpvt(1), Dummy(1,1))
      DO J=1,K
      Kj = Kj + 1
      Yinv(1,KJ) = Dummy(1,J)
      Yinv(2,KJ) = Dummy(2,J)
      ENDDO
      ENDDO
      RETURN
      END
