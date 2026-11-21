      SUBROUTINE FILE2(RES1,RES2,URES1,URES2)
C=======================================================================
C
C     READ RESONANCE PARAMETERS IN ORDER TO DEFINE THE ENERGY RANGE
C     OF THE RESOLVED AND UNRESOLVED RESONANCE REGIONS.
C
C=======================================================================
      INCLUDE 'implicit.h'
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH
      COMMON/LEADER/C1,C2,L1,L2,N1,N2,MAT,MF,MT
C-----INITIALIZE ALL ENERGIES TO ZERO.
      RES1 =0.0d0
      RES2 =0.0d0
      URES1=0.0d0
      URES2=0.0d0
C-----HEAD RECORD ALREADY READ. DEFINE NUMBER OF ISOTOPES.
      NIS=N1H
C-----SET UP DO LOOP OVER ALL ISOTOPES
      DO 220 IS=1,NIS
      CALL CARDI(C1H,C2H,L1H,LFW,NER,N2H)
C-----DO LOOP OVER ALL ENERGY RANGES
      DO 210 JER=1,NER
      CALL CARDI( EL, EH,LRU,LRF,N1H,N2H)
c-----------------------------------------------------------------------
C
C     2017/5/16 - Added Energy dependent scattering radius
C
c-----------------------------------------------------------------------
      NRO = N1H
      if(NRO.eq.1) then
c-----Energy dependent scattering radius - skip Tab1 record
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      do i = 1,N1,3          ! Interpolation law (NBT,INT) pairs
      CALL SKIP1
      enddo
      do i = 1,N2,3          ! Scattering Radius (X,Y) pars
      CALL SKIP1
      enddo
      endif
C-----DEFINE LRU FOR INTERNAL USE AS ORIGINAL LRU (BEFORE RECENT).
      IF(LRU.GT.3) LRU=LRU-3
C-----Select resolved or unresolved.
      IF(LRU.eq.1) go to 10    ! Resolved
      IF(LRU.gt.1) go to 30    ! Unresolved
C-----NO RESONANCE PARAMETERS.
      CALL CARDI(C1H,C2H,L1H,L2H,N1H,N2H)
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
c-----------------------------------------------------------------------
c     EBD of READ - Only 1 isotopre & resolved = no unresolved
c     LIST2 Done in FILE2/EVALPLOT
c-----------------------------------------------------------------------
      if(NIS.le.1.and.NER.le.1) THEN
      RETURN
      ENDIF
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
c-----------------------------------------------------------------------
c     EBD of READ - Only 1 isotopre - BOTH resolved + unresolved
c     LIST2 Done in FILE2/EVALPLOT
c-----------------------------------------------------------------------
      if(NIS.le.1) THEN
      RETURN
      ENDIF
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
   60 CALL CARDI(C1H,C2H,L1H,L2H,NLS,N2H)
C-----LOOP OVER ALL L STATES
      DO 75 ILS=1,NLS
C-----READ NEXT LINE.
      CALL CARDI(C1H,C2H,L1H,L2H,N1H,NRS)
C-----COPY RESONANCE PARAMETERS.
      DO 70 IRS=1,NRS
      CALL SKIP1
   70 CONTINUE
   75 CONTINUE
      GO TO 210
C-----------------------------------------------------------------------
C
C     ADLER-ADLER FORMALISM
C
C-----------------------------------------------------------------------
C-----READ NEXT LINE.
   80 CALL CARDI(C1H,C2H,L1H,L2H,NLS,N2H)
C-----READ BACKGROUND CORRECTIONS.
      CALL CARDI(C1H,C2H,L1H,L2H,NX6,N2H)
C-----COPY BACKGROUND CORRECTION CONSTANTS.
      DO 90 I=1,NX6,6
      CALL SKIP1
   90 CONTINUE
C-----LOOP OVER L STATES
      DO 108 I=1,NLS
      CALL CARDI(C1H,C2H,L1H,L2H,NJS,N2H)
C-----LOOP OVER J STATES
      DO 105 J=1,NJS
      CALL CARDI(C1H,C2H,L1H,L2H,N1H,NLJ)
C-----COPY ALL RESONANCE DATA
      DO 100 K=1,NLJ
      CALL SKIP1
      CALL SKIP1
  100 CONTINUE
  105 CONTINUE
  108 CONTINUE
      GO TO 210
C-----------------------------------------------------------------------
C
C     NEW REICH-MOORE FORMALISM
C
C-----------------------------------------------------------------------
C-----DEFINE NUMBER OF J STATES
  110 CALL CARDI(C1,C2,L1,L2,NJS,N2)
C-----DEFINE NUMBER OF PARTICLE-PAIRS
      CALL CARDI(C1,C2,L1,L2,NPP12,N2)
C-----SKIP PARTICLE-PAIR DATA
      DO N=1,NPP12,6
      CALL SKIP1
      ENDDO
C-----LOOP OVER J STATES
      DO 120 IJ=1,NJS
C-----J, PARITY AND NUMBER OF CHANNELS
      CALL CARDI(C1,C2,L1,L2,NCH6,N2)
C-----SKIP CHANNEL DATA
      DO N=1,NCH6,6
      CALL SKIP1
      ENDDO
C-----DEFINE NUMBER OF RESONANCES
      CALL CARDI(C1,C2,L1,L2,N1,NRS)
C-----SKIP RESONANCE PARAMETERS
      DO N=1,NRS
      CALL SKIP1
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
      CALL CARDI(C1H,C2H,L1H,L2H,NLS,N2H)
C-----LOOP OVER ALL L-STATES
      DO 145 ILS=1,NLS
      CALL CARDI(C1H,C2H,L1H,L2H,N1H,NJS)
      DO 140 N=1,NJS
      CALL SKIP1
  140 CONTINUE
  145 CONTINUE
      GO TO 210
C-----FISSION WIDTHS GIVEN (LFW=1)
  150 CALL CARDI(C1H,C2H,L1H,L2H,NE,NLS)
C-----COPY FISSION WIDTH ENERGY POINTS
      DO 160 I=1,NE,6
      CALL SKIP1
  160 CONTINUE
C-----LOOP OVER L-STATES
      DO 180 I=1,NLS
      CALL CARDI(C1H,C2H,L1H,L2H,NJS,N2H)
C-----LOOP OVER J STATES
      DO 170 J=1,NJS
      CALL CARDI(C1H,C2H,L1H,L2H,NEP6,N2H)
      DO K=1,NEP6,6
      CALL SKIP1
      ENDDO
  170 CONTINUE
  180 CONTINUE
      GO TO 210
C-----------------------------------------------------------------------
C
C     UNRESOLVED WITH ENERGY DEPENDENT WIDTHS.
C
C-----------------------------------------------------------------------
C-----READ NEXT LINE.
  190 CALL CARDI(C1H,C2H,L1H,L2H,NLS,N2H)
C-----DO LOOP OVER L-STATES
      DO 200 I=1,NLS
      CALL CARDI(C1H,C2H,L1H,L2H,NJS,N2H)
      DO 198 J=1,NJS
      CALL CARDI(C1H,C2H,L1H,L2H,NE6P6,N2H)
C-----COPY NUMBER OF DEGREES OF FREEDOM AND PARAMETERS.
      DO 195 K=1,NE6P6,6
      CALL SKIP1
  195 CONTINUE
  198 CONTINUE
  200 CONTINUE
  210 CONTINUE
  220 CONTINUE
  230 RETURN
      END
