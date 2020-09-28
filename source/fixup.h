C=======================================================================
C
C     FIXUP COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
c-----2017/03/07 - INCREASED TO 3,000,000 FROM 600,000
c-----2017/05/25 - DECREASED TO 2,700,000: 3,000,000 too big for LINUX
c-----2018/01/20 - DECREASED TO 1,800,000: Better for many computers
      PARAMETER (MAXPOINT = 1800000)
c----- 1,800,000 X 8 bytes X 8 arrays = 115,200,000 bytes
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
c-----2019/1/5 - Added NBT and INT dimension 100 = PREPRO Standard
      COMMON/NBTINT/NBT(100),INT(100)
c-----Blank COMMON
      COMMON XA(MAXPOINT),XB(MAXPOINT),XC(MAXPOINT),XCORE(MAXPOINT),
     1       YA(MAXPOINT),YB(MAXPOINT),YC(MAXPOINT),YCORE(MAXPOINT)