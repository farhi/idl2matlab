PRO	GAUSS_SQUARE,X,A,F,PDER
;+
; NAME:
;	GAUSS_FUNCT
;
; PURPOSE:
;	EVALUATE THE SUM OF A GAUSSIAN AND A 2ND ORDER POLYNOMIAL
;	AND OPTIONALLY RETURN THE VALUE OF IT`S PARTIAL DERIVATIVES.
;	NORMALLY, THIS FUNCTION IS USED BY CURVEFIT TO FIT THE
;	SUM OF A LINE AND A VARYING BACKGROUND TO ACTUAL DATA.
;
; CATEGORY:
;	E2 - CURVE AND SURFACE FITTING.
; CALLING SEQUENCE:
;	FUNCT,X,A,F,PDER
; INPUTS:
;	X = VALUES OF INDEPENDENT VARIABLE.
;	A = PARAMETERS OF EQUATION DESCRIBED BELOW.
; OUTPUTS:
;	F = VALUE OF FUNCTION AT EACH X(I).
;
; OPTIONAL OUTPUT PARAMETERS:
;	PDER = (N_ELEMENTS(X),6) ARRAY CONTAINING THE
;		PARTIAL DERIVATIVES.  P(I,J) = DERIVATIVE
;		AT ITH POINT W/RESPECT TO JTH PARAMETER.
; COMMON BLOCKS:
;	NONE.
; SIDE EFFECTS:
;	NONE.
; RESTRICTIONS:
;	NONE.
; PROCEDURE:
;	F = A(0)*EXP(-Z^2/2) + A(3) + A(4)*X + A(5)*X^2
;	Z = (X-A(1))/A(2)
; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(2) is 0.
;	Added to Gauss_fit, when the variable function name to
;		Curve_fit was implemented.  DMS, Nov, 1990.
;   Modified, TCH, Nov 2000. Multipeakfit
;-
  nterms=N_ELEMENTS(a)>1
  a=[a,0.,0.,0.,0.,0.,0.]
  a=a(0:nterms-1)
  F =FLTARR(N_ELEMENTS(X))
  IF (nterms-1)/3 GT 0 THEN BEGIN
    EZ=FLTARR(N_ELEMENTS(X),(nterms-1)/3)
    Z =EZ
  ENDIF
  FOR i=0,nterms-4,3 DO BEGIN
    IF A(i+2) ne 0.0 THEN Z(*,i/3) = (X-A(i+1))/A(i+2) ELSE Z(*,i/3) = 10.
    EZ(*,i/3) = EXP(-Z(*,i/3)^2/2.)*(ABS(Z(*,i/3)) LE 7.) 
    F = F + A(i)*EZ(*,i/3) 
  ENDFOR
  F = F + A((nterms-1)/3*3) 
  IF N_ELEMENTS(A) GT (nterms-1)/3*3+1 THEN F = F + A((nterms-1)/3*3+1)*X 
  IF N_ELEMENTS(A) GT (nterms-1)/3*3+2 THEN F = F + A((nterms-1)/3*3+2)*X^2 
  IF N_PARAMS(0) LE 3 THEN BEGIN
      a=a(0:nterms-1)
	  RETURN 
  ENDIF
  PDER = FLTARR(N_ELEMENTS(X),nterms) 
  FOR i=0,nterms-4,3 DO BEGIN
    PDER(*,i) = EZ(*,i/3)	
    if a(i+2) ne 0. then PDER(*,i+1) = A(i) * EZ(*,i/3) * Z(*,i/3)/A(i+2)
    PDER(*,i+2) = PDER(*,i+1) * Z(*,i/3)
  ENDFOR
  PDER(*,(nterms-1)/3*3) = 1.
  IF N_ELEMENTS(A) GT (nterms-1)/3*3+1 THEN PDER(*,(nterms-1)/3*3+1) = X
  IF N_ELEMENTS(A) GT (nterms-1)/3*3+2 THEN PDER(*,(nterms-1)/3*3+2) = X^2
  RETURN
END
