!+OPENTE2.COM                      (D. SNEPP   MAY-84)
! PURPOSE - MODIFIES POINTS IN AIRFOIL STRING TO SATISFY
!           DESIRED TRAILING EDGE THICKNESS
!         - THICKNESS DEFINED AS DELTA-Z, UPPER VS LOWER
!         - PATTERNED AFTER GCS ROUTINE - ADJUST1
!         - EITHER OPEN-ONLY, OR OPEN-OR-CLOSE
!         - PART OF WING LOFTING ROUTINES
!
! PRE-SET SYMBOL - $TEOP= TRAILING EDGE OPEN (DELTA-Z)
!                         > 0 : OPEN ONLY
!                         < 0 : OPEN OR CLOSE
!                         = 99: CLOSE TO ZERO THICKNESS
!*ZAFIN,, INPUT - AIRFOIL STRING (X, Y, Z)
!*ZAFOUT,, OUTPUT - NEW AIRFOIL STRING (X, Y, Z)
!---------------------------------------------------------
SET NOVERIFY   ! WKC
$call get_mode(,,,,,,ifd)
$if ifd=0 then
  set nofiled
$endif
$push
$pushG
! HISTORY
! - CREATED: 16-MAY-84 D. SNEPP - FROM WNGLFT3.COM
! - MODIFIED: 7-FEB-91 W. CAPRON - TO WORK WITH AGPS VERSION 9.01
! - MODIFIED: 26-APR-94 A. WINN - TOOK OUT DEALLOCATES AND PUT IN 
!                                 PUSH AND POP AND REMOVED
!                                 OBSOLETE.COM CALL
! - Modified: 24-Sep-99 W. Capron - Added some pops and popgs.
!
!
!
!    VERIFY INPUTS VALID
$CALL GET_LENGTH(ZAFIN,NOBJ)
$IF NOBJ <= 0 THEN
     $WRITE '=== INPUT ZAFIN NOT VALID - ABORTING ==='
     $pop
     $popg
     SET VERIFY
     $UNWIND
$ENDIF
!
!    EXAMINE INPUT TEOP
$IF TEOP = 0 THEN
     $WRITE '**  TEOP=0 - DO NOT MODIFY AIRFOIL - ABORTING  **'
     $WRITE '** TO FORCE ZERO T.E. THICKNESS, INPUT TEOP=99 **'
     $pop
     $popg
     SET VERIFY
     $UNWIND
$ENDIF
!
!    DEFINE ACTUAL T.E. THICKNESS DESIRED
$IF TEOP >= 99. THEN
     $TEOZ=0.
$ELSE
     $TEOZ=ABS(TEOP)
$ENDIF
!
!     DETERMINE TRAILING EDGE
$CALL GET_COORD(ZAFIN.1,0,0,XUP,YUP,ZUP)        ! TE UPPER
$CALL GET_COORD(ZAFIN.L,0,0,XLW,YLW,ZLW)        ! TE LOWER
$XTE = ( XUP + XLW )/2.0
$YTE = ( YUP + YLW )/2.0
$ZTE = ( ZUP + ZLW )/2.0
!
!    COMPUTE CURRENT T.E. THICKNESS
$ZTHK = ABS(ZUP-ZLW)
!
!     DRAW INPUT AIRFOIL POINTS
!DRA  ZAFIN.*  COL=GREEN ERA=YES  VIEW=90,0,0  ! WKC ADDED COLOR
!
!    SET FLAG TO DETERMINE IF T.E. IS TO BE MODIFIED
!    LOPN=0 : MODIFY  //  LOPN=1 : DO NOT MODIFY
!    (1) DEFAULT : DO NOT MODIFY
$LOPN=0
!    (2) NEGATIVE TEOP : MODIFY
$IF TEOP < 0. THEN
     $LOPN=1
!     $WRITE '    - T.E. THICKNESS= ',ZTHK,' - FORCE TO ',TEOZ
!     PAUSE ____-_ADJUST_VIEW_-_PRESS_ESC_TO_PROCEED
$ENDIF
!    (3) ZTHK < TEOZ : MODIFY
$IF ZTHK < TEOZ THEN
     $LOPN=1
!     $WRITE '    - T.E. THICKNESS= ',ZTHK,' -  OPEN TO ',TEOZ
!     PAUSE ____-_ADJUST_VIEW_-_PRESS_ESC_TO_PROCEED
$ENDIF
!    (4) TEOP = 99 : MODIFY
$IF TEOP >= 99. THEN
     $LOPN=1
!     $WRITE '    - T.E. THICKNESS= ',ZTHK,' - CLOSE TO ZERO'
!     PAUSE ____-_ADJUST_VIEW_-_PRESS_ESC_TO_PROCEED
$ENDIF
!
!
!    PROCESS AIRFOIL
$IF LOPN = 1 THEN
!
!         MODIFY T.E. - OPEN OR CLOSE AS NECESSARY
!
     $WRITE '    - ADJUSTING T.E. THICKNESS FROM ',ZTHK,' TO ',TEOZ
!
!         FIT CURVE TO AIRFOIL STRING
     FCV   QCURVE ZAFIN CUBIC   !  NO NO  ! WKC
!        FIND LEADING EDGE
!     $CALL GET_LIMITS(QCURVE,1,SMIN,SMAX) ! WKC
     LIM QCURVE X QXMIN MAX= [(.5),(.5)]   ! WKC
!     $CALL GET_COORD(QCURVE,SMIN,0,XLE,YLE,ZLE)   ! WKC
     $CALL GET_COORD(QXMIN,0,0,XLE,YLE,ZLE)        ! WKC
     $CALL GET_COORD(QXMIN.1,0,0,SMIN)             ! WKC
     DEL QXMIN.1                                   ! WKC
     DEL QXMIN                                     ! WKC
!
!         DETERMINE BREAK BETWEEN UPPER AND LOWER SURFACES
     $FOR J=1 TO NOBJ DO
         $CALL GET_PVAL(QCURVE,J,SPAR)
         $IF SMIN <= SPAR THEN
             $NBRK = J-1
             $J = NOBJ+1  ! -- EXIT DO-LOOP
         $ENDIF
     $ENDDO
!
!         OPEN UP TRAILING EDGE USING MODIFIED
!         ADJUST1 SUBROUTINE FROM GCS SYSTEM
!         (THIS PORTION FROM L. GOTKIN)
     $FOR ISRF = 1 TO 2 DO
          $IF ISRF = 1 THEN
!                  UPPER SURFACE
              $WRITE '      - ADJUSTING UPPER SURFACE'
              $CALL GET_COORD(ZAFIN.1,0,0,X2,Y2,Z2)
              $SIGN = 1.0
              $NST  = 1
              $NEND = NBRK
          $ELSE
!                LOWER SURFACE
              $WRITE '      - ADJUSTING LOWER SURFACE'
              $CALL GET_COORD(ZAFIN.L,0,0,X2,Y2,Z2)
              $SIGN = -1.0
              $NST  = NBRK+1
              $NEND = NOBJ
          $ENDIF
!
!              COMPUTE TRANSFORMATION COEFFICIENTS
          $DTXX = XTE - XLE
          $DTX  = X2  - XLE
          $DTZZ = ZTE + .5*SIGN*ABS(TEOZ) - ZLE
          $DTZ  = Z2  - ZLE
          $C    = SQRT(DTXX*DTXX + DTZZ*DTZZ)
          $D    = SQRT(DTX*DTX   + DTZ*DTZ)
          $RAT  = C/D
          $SI   = (DTZZ/C)*(DTX/D) - (DTXX/C)*(DTZ/D)
          $CO   = (DTXX/C)*(DTX/D) + (DTZZ/C)*(DTZ/D)
!              TRANSFORM POINTS
          $FOR J = NST TO NEND DO
              $CALL GET_COORD(ZAFIN.<J>,0,0,XAIR,YAIR,ZAIR)
              $XTEM = XLE + RAT*( CO*(XAIR-XLE) - SI*(ZAIR-ZLE) )
              $ZAIR = ZLE + RAT*( CO*(ZAIR-ZLE) + SI*(XAIR-XLE) )
              $XAIR = XTEM
!                  ADD MODIFIED POINT TO STRING
              CST ZAFOUT DATA=(<XAIR>,<YAIR>,<ZAIR>)
!              DRA ZAFOUT.L ERA=NO COL=RED  ! WKC ADDED COLOR
          $ENDDO -- END OF POINT-ADJUSTING DO-LOOP
     $ENDDO -- END OF 2-SURFACE DO-LOOP
!        CLEAN UP
     DEL [QCURVE.*,QCURVE]
$ELSE
!         ACCEPT AIRFOIL STRING AS-IS
     $WRITE '    - T.E. THICKNESS= ',ZTHK,' - ACCEPT AS-IS'
     CST  ZAFOUT  DATA=ZAFIN.*  SPACE=XYZ
$ENDIF ! -- END OF IF-CHECK T.E. MODIFICATION
!
!    DRA NEW AIRFOIL STRING
!DRA  ZAFOUT  ERA=NO
!
$popg
$pop
!
$if ifd=0 then
  set filed
$endif
SET VERIFY
! OUTPUT: ZAFOUT (STRING)
! END OF OPENTE2.COM
