; $Id: poly.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION POLY,X,C
;+
; NAME:
;	POLY
;
; PURPOSE:
;	Evaluate a polynomial function of a variable.
;
; CATEGORY:
;	C1 - Operations on polynomials.
;
; CALLING SEQUENCE:
;	Result = POLY(X,C)
;
; INPUTS:
;	X:	The variable.  This value can be a scalar, vector or array.
;
;	C:	The vector of polynomial coefficients.  The degree of 
;		of the polynomial is N_ELEMENTS(C) - 1.
;
; OUTPUTS:
;	POLY returns a result equal to:
;		 C(0) + c(1) * X + c(2)*x^2 + ...
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
;	Straightforward.
;
; MODIFICATION HISTORY:
;	DMS, Written, January, 1983.
;-

on_error,2		;Return to caller if an error occurs
N = N_ELEMENTS(C)-1	;Find degree of polynomial
Y = c(n)
for i=n-1,0,-1 do y = y * x + c(i)
return,y
end

