      IMPLICIT NONE
      
      INTEGER STDIN,STDOUT,NDP
      PARAMETER (STDIN=5,STDOUT=6,NDP=7)
      DOUBLE PRECISION DPI
      PARAMETER (DPI=3.141592653589793238462643D0)
      
      INTEGER I,J,NSTRT,JFLAG
      INTEGER COMMAS(1000)
      CHARACTER*2000 GDR2

      INTEGER Dup
      CHARACTER*13 Var
      CHARACTER*100 INFILE
      REAL*16 RA_ICRS,e_RA_ICRS,DE_ICRS,e_DE_ICRS,
     +                 Plx,pmRA,e_pmRA,pmDE,e_pmDE,FG,e_FG,
     +                 Gmag

      INTEGER IHMSF(4),IDMSF(4)
      CHARACTER*1 SIGN
      DOUBLE PRECISION A,B,JD,e_G

      IF (IARGC().NE.1) PAUSE 'ONE PARAMETER AT LINE COMMAND IS NEEDED'
      CALL GETARG(1,INFILE)

      OPEN(UNIT=1,
     +     FILE=INFILE,
     +     FORM='FORMATTED',
     +     ACCESS='SEQUENTIAL',
     +     STATUS='OLD'
     +    )

      READ(1,'(A2000)')GDR2
1     READ(1,'(A2000)',END=2)GDR2
      J=1
      DO I=1,2000
       IF (GDR2(I:I).EQ.';') THEN
        COMMAS(J)=I
        J=J+1
       END IF
      END DO
      J=J-1

      RA_ICRS=0D0
      e_RA_ICRS=0D0
      DE_ICRS=0D0
      e_DE_ICRS=0D0
      Plx=0D0
      pmRA=0D0
      e_pmRA=0D0
      pmDE=0D0
      e_pmDE=0D0
      FG=0D0
      e_FG=0D0
      Gmag=0D0

      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(5)+1:COMMAS(6)-1),NSTRT,RA_ICRS,
     +JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(6)+1:COMMAS(7)-1),NSTRT,e_RA_ICRS,
     +JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(7)+1:COMMAS(8)-1),NSTRT,DE_ICRS,
     +JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(8)+1:COMMAS(9)-1),NSTRT,e_DE_ICRS,
     +JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(9)+1:COMMAS(10)-1),NSTRT,Plx,JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(12)+1:COMMAS(13)-1),NSTRT,pmRA,JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(13)+1:COMMAS(14)-1),NSTRT,e_pmRA,
     +JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(14)+1:COMMAS(15)-1),NSTRT,pmDE,JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(15)+1:COMMAS(16)-1),NSTRT,e_pmDE,
     +JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(47)+1:COMMAS(48)-1),NSTRT,FG,JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(48)+1:COMMAS(49)-1),NSTRT,e_FG,JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'
      NSTRT=1
      CALL sla_DFLTIN (GDR2(COMMAS(50)+1:COMMAS(51)-1),NSTRT,Gmag,JFLAG)
      IF(JFLAG.EQ.2)PAUSE 'DFLTIN CONVERSION PROBLEM'


1010  FORMAT(I2.2,1X,I2.2,1X,I2.2,A1,I4.4,2X,
     +       A1,I2.2,1X,I2.2,1X,I2.2,A1,I3.3,2X,
     +       2(F7.3),2X,F16.8,1X,4(F8.3),1X,F6.3,2X,F5.3)

       e_G=2.5D0/log(10D0)*e_FG/FG
       JD=2457206.37421083D0

       A=RA_ICRS*DPI/180D0
       B=DE_ICRS*DPI/180D0
       SIGN='+'
       CALL sla_DR2TF(NDP-3,A,SIGN,IHMSF)
       CALL sla_DR2AF(NDP-4,B,SIGN,IDMSF)

       WRITE(STDOUT,1010)
     + IHMSF(1),IHMSF(2),IHMSF(3),'.',IHMSF(4),
     + SIGN,IDMSF(1),IDMSF(2),IDMSF(3),'.',IDMSF(4),
     + e_RA_ICRS/1000D0,e_DE_ICRS/1000D0,JD,
     + pmRA/1000D0,pmDE/1000D0,
     + e_pmRA/1000D0,e_pmDE/1000D0,Gmag,e_G

       GOTO 1
2      CLOSE(1)

       STOP
       END
     
      SUBROUTINE sla_DR2TF (NDP, ANGLE, SIGN, IHMSF)
*+
*     - - - - - -
*      D R 2 T F
*     - - - - - -
*
*  Convert an angle in radians to hours, minutes, seconds
*  (double precision)
*
*  Given:
*     NDP      i      number of decimal places of seconds
*     ANGLE    d      angle in radians
*
*  Returned:
*     SIGN     c      '+' or '-'
*     IHMSF    i(4)   hours, minutes, seconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of ANGLE, the format of DOUBLE PRECISION floating-point
*         numbers on the target machine, and the risk of overflowing
*         IHMSF(4).  For example, on the VAX, for ANGLE up to 2pi, the
*         available floating-point precision corresponds roughly to
*         NDP=12.  However, the practical limit is NDP=9, set by the
*         capacity of the 32-bit integer IHMSF(4).
*
*     3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where ANGLE is very nearly 2pi and rounds up to 24 hours,
*         by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
*
*  Called:  sla_DD2TF
*
*  P.T.Wallace   Starlink   19 March 1999
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the 
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
*    Boston, MA  02111-1307  USA
*
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

*  Turns to radians
      DOUBLE PRECISION T2R
      PARAMETER (T2R=6.283185307179586476925287D0)



*  Scale then use days to h,m,s routine
      CALL sla_DD2TF(NDP,ANGLE/T2R,SIGN,IHMSF)

      END

      SUBROUTINE sla_DR2AF (NDP, ANGLE, SIGN, IDMSF)
*+
*     - - - - - -
*      D R 2 A F
*     - - - - - -
*
*  Convert an angle in radians to degrees, arcminutes, arcseconds
*  (double precision)
*
*  Given:
*     NDP      i      number of decimal places of arcseconds
*     ANGLE    d      angle in radians
*
*  Returned:
*     SIGN     c      '+' or '-'
*     IDMSF    i(4)   degrees, arcminutes, arcseconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of ANGLE, the format of DOUBLE PRECISION floating-point
*         numbers on the target machine, and the risk of overflowing
*         IDMSF(4).  For example, on the VAX, for ANGLE up to 2pi, the
*         available floating-point precision corresponds roughly to
*         NDP=12.  However, the practical limit is NDP=9, set by the
*         capacity of the 32-bit integer IDMSF(4).
*
*     3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where ANGLE is very nearly 2pi and rounds up to 360 deg,
*         by testing for IDMSF(1)=360 and setting IDMSF(1-4) to zero.
*
*  Called:  sla_DD2TF
*
*  P.T.Wallace   Starlink   19 March 1999
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the 
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
*    Boston, MA  02111-1307  USA
*
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IDMSF(4)

*  Hours to degrees * radians to turns
      DOUBLE PRECISION F
      PARAMETER (F=15D0/6.283185307179586476925287D0)



*  Scale then use days to h,m,s routine
      CALL sla_DD2TF(NDP,ANGLE*F,SIGN,IDMSF)

      END

      SUBROUTINE sla_DD2TF (NDP, DAYS, SIGN, IHMSF)
*+
*     - - - - - -
*      D D 2 T F
*     - - - - - -
*
*  Convert an interval in days into hours, minutes, seconds
*  (double precision)
*
*  Given:
*     NDP      i      number of decimal places of seconds
*     DAYS     d      interval in days
*
*  Returned:
*     SIGN     c      '+' or '-'
*     IHMSF    i(4)   hours, minutes, seconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of DAYS, the format of DOUBLE PRECISION floating-point numbers
*         on the target machine, and the risk of overflowing IHMSF(4).
*         For example, on the VAX, for DAYS up to 1D0, the available
*         floating-point precision corresponds roughly to NDP=12.
*         However, the practical limit is NDP=9, set by the capacity of
*         the 32-bit integer IHMSF(4).
*
*     3)  The absolute value of DAYS may exceed 1D0.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where DAYS is very nearly 1D0 and rounds up to 24 hours,
*         by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
*
*  P.T.Wallace   Starlink   19 March 1999
*
*  Copyright (C) 1999 Rutherford Appleton Laboratory
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the 
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
*    Boston, MA  02111-1307  USA
*
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION DAYS
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER (D2S=86400D0)

      INTEGER NRS,N
      DOUBLE PRECISION RS,RM,RH,A,AH,AM,AS,AF



*  Handle sign
      IF (DAYS.GE.0D0) THEN
         SIGN='+'
      ELSE
         SIGN='-'
      END IF

*  Field units in terms of least significant figure
      NRS=1
      DO N=1,NDP
         NRS=NRS*10
      END DO
      RS=DBLE(NRS)
      RM=RS*60D0
      RH=RM*60D0

*  Round interval and express in smallest units required
      A=ANINT(RS*D2S*ABS(DAYS))

*  Separate into fields
      AH=AINT(A/RH)
      A=A-AH*RH
      AM=AINT(A/RM)
      A=A-AM*RM
      AS=AINT(A/RS)
      AF=A-AS*RS

*  Return results
      IHMSF(1)=MAX(NINT(AH),0)
      IHMSF(2)=MAX(MIN(NINT(AM),59),0)
      IHMSF(3)=MAX(MIN(NINT(AS),59),0)
      IHMSF(4)=MAX(NINT(MIN(AF,RS-1D0)),0)

      END

      SUBROUTINE sla_DFLTIN (STRING, NSTRT, DRESLT, JFLAG)
*+
*     - - - - - - -
*      D F L T I N
*     - - - - - - -
*
*  Convert free-format input into double precision floating point
C
C  User note: double precision replaced by real*16
C
*
*  Given:
*     STRING     c     string containing number to be decoded
*     NSTRT      i     pointer to where decoding is to start
*     DRESLT     d     current value of result
*
*  Returned:
*     NSTRT      i      advanced to next number
*     DRESLT     d      result
*     JFLAG      i      status: -1 = -OK, 0 = +OK, 1 = null, 2 = error
*
*  Notes:
*
*     1     The reason DFLTIN has separate OK status values for +
*           and - is to enable minus zero to be detected.   This is
*           of crucial importance when decoding mixed-radix numbers.
*           For example, an angle expressed as deg, arcmin, arcsec
*           may have a leading minus sign but a zero degrees field.
*
*     2     A TAB is interpreted as a space, and lowercase characters
*           are interpreted as uppercase.
*
*     3     The basic format is the sequence of fields #^.^@#^, where
*           # is a sign character + or -, ^ means a string of decimal
*           digits, and @, which indicates an exponent, means D or E.
*           Various combinations of these fields can be omitted, and
*           embedded blanks are permissible in certain places.
*
*     4     Spaces:
*
*             .  Leading spaces are ignored.
*
*             .  Embedded spaces are allowed only after +, -, D or E,
*                and after the decomal point if the first sequence of
*                digits is absent.
*
*             .  Trailing spaces are ignored;  the first signifies
*                end of decoding and subsequent ones are skipped.
*
*     5     Delimiters:
*
*             .  Any character other than +,-,0-9,.,D,E or space may be
*                used to signal the end of the number and terminate
*                decoding.
*
*             .  Comma is recognized by DFLTIN as a special case;  it
*                is skipped, leaving the pointer on the next character.
*                See 13, below.
*
*     6     Both signs are optional.  The default is +.
*
*     7     The mantissa ^.^ defaults to 1.
*
*     8     The exponent @#^ defaults to Q0.
*
*     9     The strings of decimal digits may be of any length.
*
*     10    The decimal point is optional for whole numbers.
*
*     11    A "null result" occurs when the string of characters being
*           decoded does not begin with +,-,0-9,.,D or E, or consists
*           entirely of spaces.  When this condition is detected, JFLAG
*           is set to 1 and DRESLT is left untouched.
*
*     12    NSTRT = 1 for the first character in the string.
*
*     13    On return from DFLTIN, NSTRT is set ready for the next
*           decode - following trailing blanks and any comma.  If a
*           delimiter other than comma is being used, NSTRT must be
*           incremented before the next call to DFLTIN, otherwise
*           all subsequent calls will return a null result.
*
*     14    Errors (JFLAG=2) occur when:
*
*             .  a +, -, D or E is left unsatisfied;  or
*
*             .  the decimal point is present without at least
*                one decimal digit before or after it;  or
*
*             .  an exponent more than 100 has been presented.
*
*     15    When an error has been detected, NSTRT is left
*           pointing to the character following the last
*           one used before the error came to light.  This
*           may be after the point at which a more sophisticated
*           program could have detected the error.  For example,
*           DFLTIN does not detect that '1D999' is unacceptable
*           (on a computer where this is so) until the entire number
*           has been decoded.
*
*     16    Certain highly unlikely combinations of mantissa &
*           exponent can cause arithmetic faults during the
*           decode, in some cases despite the fact that they
*           together could be construed as a valid number.
*
*     17    Decoding is left to right, one pass.
*
*     18    See also FLOTIN and INTIN
*
*  Called:  sla__IDCHF
*
*  P.T.Wallace   Starlink   18 March 1999
*
*  Copyright (C) 1999 Rutherford Appleton Laboratory
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the 
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
*    Boston, MA  02111-1307  USA
*
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER NSTRT
      REAL*16 DRESLT
      INTEGER JFLAG

      INTEGER*16 NPTR,MSIGN,NEXP,NDP,NVEC,NDIGIT,ISIGNX,J
      REAL*16 DMANT,DIGIT



*  Current character
      NPTR=NSTRT

*  Set defaults: mantissa & sign, exponent & sign, decimal place count
      DMANT=0Q0
      MSIGN=1
      NEXP=0
      ISIGNX=1
      NDP=0

*  Look for sign
 100  CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO ( 400,  100,  800,  500,  300,  200, 9110, 9100, 9110),NVEC
*             0-9    SP   D/E    .     +     -     ,   ELSE   END

*  Negative
 200  CONTINUE
      MSIGN=-1

*  Look for first leading decimal
 300  CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO ( 400, 300,  800,  500, 9200, 9200, 9200, 9200, 9210),NVEC
*             0-9   SP   D/E    .     +     -     ,   ELSE   END

*  Accept leading decimals
 400  CONTINUE
      DMANT=DMANT*1D1+DIGIT
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO ( 400, 1310,  900,  600, 1300, 1300, 1300, 1300, 1310),NVEC
*             0-9   SP    D/E    .     +     -     ,   ELSE   END

*  Look for decimal when none preceded the point
 500  CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO ( 700, 500, 9200, 9200, 9200, 9200, 9200, 9200, 9210),NVEC
*             0-9   SP   D/E    .     +     -     ,   ELSE   END

*  Look for trailing decimals
 600  CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO ( 700, 1310,  900, 1300, 1300, 1300, 1300, 1300, 1310),NVEC
*             0-9   SP    D/E    .     +     -     ,   ELSE   END

*  Accept trailing decimals
 700  CONTINUE
      NDP=NDP+1
      DMANT=DMANT*1D1+DIGIT
      GO TO 600

*  Exponent symbol first in field: default mantissa to 1
 800  CONTINUE
      DMANT=1Q0

*  Look for sign of exponent
 900  CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO (1200, 900, 9200, 9200, 1100, 1000, 9200, 9200, 9210),NVEC
*             0-9   SP   D/E    .     +     -     ,   ELSE   END

*  Exponent negative
 1000 CONTINUE
      ISIGNX=-1

*  Look for first digit of exponent
 1100 CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO (1200, 1100, 9200, 9200, 9200, 9200, 9200, 9200, 9210),NVEC
*             0-9   SP    D/E    .     +     -     ,   ELSE   END

*  Use exponent digit
 1200 CONTINUE
      NEXP=NEXP*10+NDIGIT
      IF (NEXP.GT.100) GO TO 9200

*  Look for subsequent digits of exponent
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO (1200, 1310, 1300, 1300, 1300, 1300, 1300, 1300, 1310),NVEC
*             0-9   SP    D/E    .     +     -     ,   ELSE   END

*  Combine exponent and decimal place count
 1300 CONTINUE
      NPTR=NPTR-1
 1310 CONTINUE
      NEXP=NEXP*ISIGNX-NDP

*  Skip if net exponent negative
      IF (NEXP.LT.0) GO TO 1500

*  Positive exponent: scale up
 1400 CONTINUE
      IF (NEXP.LT.10) GO TO 1410
      DMANT=DMANT*1D10
      NEXP=NEXP-10
      GO TO 1400
 1410 CONTINUE
      IF (NEXP.LT.1) GO TO 1600
      DMANT=DMANT*1D1
      NEXP=NEXP-1
      GO TO 1410

*  Negative exponent: scale down
 1500 CONTINUE
      IF (NEXP.GT.-10) GO TO 1510
      DMANT=DMANT/1D10
      NEXP=NEXP+10
      GO TO 1500
 1510 CONTINUE
      IF (NEXP.GT.-1) GO TO 1600
      DMANT=DMANT/1D1
      NEXP=NEXP+1
      GO TO 1510

*  Get result & status
 1600 CONTINUE
      J=0
      IF (MSIGN.EQ.1) GO TO 1610
      J=-1
      DMANT=-DMANT
 1610 CONTINUE
      DRESLT=DMANT

*  Skip to end of field
 1620 CONTINUE
      CALL sla__IDCHF(STRING,NPTR,NVEC,NDIGIT,DIGIT)
      GO TO (1720, 1620, 1720, 1720, 1720, 1720, 9900, 1720, 9900),NVEC
*             0-9   SP    D/E    .     +     -     ,   ELSE   END

 1720 CONTINUE
      NPTR=NPTR-1
      GO TO 9900


*  Exits

*  Null field
 9100 CONTINUE
      NPTR=NPTR-1
 9110 CONTINUE
      J=1
      GO TO 9900

*  Errors
 9200 CONTINUE
      NPTR=NPTR-1
 9210 CONTINUE
      J=2

*  Return
 9900 CONTINUE
      NSTRT=NPTR
      JFLAG=J

      END

      SUBROUTINE sla__IDCHF (STRING, NPTR, NVEC, NDIGIT, DIGIT)
*+
*     - - - - - -
*      I D C H F
*     - - - - - -
*
*  Internal routine used by DFLTIN
*
*  Identify next character in string
C
C  User note: double precision replaced by real*16
C
*
*  Given:
*     STRING      char        string
*     NPTR        int         pointer to character to be identified
*
*  Returned:
*     NPTR        int         incremented unless end of field
*     NVEC        int         vector for identified character
*     NDIGIT      int         0-9 if character was a numeral
*     DIGIT       double      equivalent of NDIGIT
*
*     NVEC takes the following values:
*
*      1     0-9
*      2     space or TAB   !!! n.b. ASCII TAB assumed !!!
*      3     D,d,E or e
*      4     .
*      5     +
*      6     -
*      7     ,
*      8     else
*      9     outside field
*
*  If the character is not 0-9, NDIGIT and DIGIT are either not
*  altered or are set to arbitrary values.
*
*  P.T.Wallace   Starlink   22 December 1992
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the 
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
*    Boston, MA  02111-1307  USA
*
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER*16 NPTR,NVEC,NDIGIT
      REAL*16 DIGIT

      CHARACTER K
      INTEGER*16 NCHAR

*  Character/vector tables
      INTEGER*16 NCREC
      PARAMETER (NCREC=19)
      CHARACTER KCTAB(NCREC)
      INTEGER*16 KVTAB(NCREC)
      DATA KCTAB/'0','1','2','3','4','5','6','7','8','9',
     :           ' ','D','d','E','e','.','+','-',','/
      DATA KVTAB/10*1,2,4*3,4,5,6,7/


*  Handle pointer outside field
      IF (NPTR.LT.1.OR.NPTR.GT.LEN(STRING)) THEN
         NVEC=9
      ELSE

*     Not end of field: identify the character
         K=STRING(NPTR:NPTR)
         DO NCHAR=1,NCREC
            IF (K.EQ.KCTAB(NCHAR)) THEN

*           Recognized
               NVEC=KVTAB(NCHAR)
               NDIGIT=NCHAR-1
               DIGIT=REAL(NDIGIT,16)
               GO TO 2300
            END IF
         END DO

*     Not recognized: check for TAB    !!! n.b. ASCII assumed !!!
         IF (K.EQ.CHAR(9)) THEN

*        TAB: treat as space
            NVEC=2
         ELSE

*        Unrecognized
            NVEC=8
         END IF

*     Increment pointer
 2300    CONTINUE
         NPTR=NPTR+1
      END IF

      END
