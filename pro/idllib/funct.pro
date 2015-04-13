; $Id: funct.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO	FUNCT,X,A,F,PDER
;+
; NAME:
;	FUNCT
;
; PURPOSE:
;	Evaluate the sum of a Gaussian and a 2nd-order polynomial
;	and optionally return the value of its partial derivatives.
;	Normally, this function is used by CURVEFIT to fit the
;	sum of a line and a varying background to actual data.
;
; CATEGORY:
;	E2 - Curve and surface fitting.
;
; CALLING SEQUENCE:
;	FUNCT, X, A, F [, Pder]
;
; INPUTS:
;	X:	The values of the independent variable.
;	A:	The parameters of the equation described in PROCEDURE below.
;
; OUTPUTS:
;	F:	The value of the function at each X(i).
;
; OPTIONAL OUTPUT PARAMETERS:
;	Pder:	An array of the size (N_ELEMENTS(X),6) that contains the
;		partial derivatives.  Pder(i,j) represents the derivative
;		at the i'th point with respect to j'th parameter.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	F = A(0)*EXP(-Z^2/2) + A(3) + A(4)*X + A(5)*X^2
;	Z = (X-A(1))/A(2)
;
; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(2) is 0.
;-
	ON_ERROR,2                        ;Return to caller if an error occurs
	if a(2) ne 0.0 then Z = (X-A(1))/A(2) $	;GET Z
	else z= 10.
	EZ = EXP(-Z^2/2.)*(ABS(Z) LE 7.) ;GAUSSIAN PART IGNORE SMALL TERMS
	F = A(0)*EZ + A(3) + A(4)*X + A(5)*X^2 ;FUNCTIONS.
	IF N_PARAMS(0) LE 3 THEN RETURN ;NEED PARTIAL?
;
	PDER = FLTARR(N_ELEMENTS(X),6) ;YES, MAKE ARRAY.
	PDER(0,0) = EZ		;COMPUTE PARTIALS
	if a(2) ne 0. then PDER(0,1) = A(0) * EZ * Z/A(2)
	PDER(0,2) = PDER(*,1) * Z
	PDER(*,3) = 1.
	PDER(0,4) = X
	PDER(0,5) = X^2
	RETURN
END

