; $Id: gaussfit.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO	GAUSS_FUNCT,X,A,F,PDER
; NAME:
;	GAUSS_FUNCT
;
; PURPOSE:
;	EVALUATE THE SUM OF A GAUSSIAN AND A 2ND ORDER POLYNOMIAL
;	AND OPTIONALLY RETURN THE VALUE OF IT'S PARTIAL DERIVATIVES.
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
;
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



Function Gaussfit, x, y, a
;+
; NAME:
;	GAUSSFIT
;
; PURPOSE:
; 	Fit the equation y=f(x) where:
;
; 		F(x) = A0*EXP(-z^2/2) + A3 + A4*x + A5*x^2
; 			and
;		z=(x-A1)/A2
;
;	A0 = height of exp, A1 = center of exp, A2 = sigma (the width).
;	A3 = constant term, A4 = linear term, A5 = quadratic term.
; 	The parameters A0, A1, A2, A3 are estimated and then CURVEFIT is 
;	called.
;
; CATEGORY:
;	?? - fitting
;
; CALLING SEQUENCE:
;	Result = GAUSSFIT(X, Y [, A])
;
; INPUTS:
;	X:	The independent variable.  X must be a vector.
;	Y:	The dependent variable.  Y must have the same number of points
;		as X.
;
; OUTPUTS:
;	The fitted function is returned.
;
; OPTIONAL OUTPUT PARAMETERS:
;	A:	The coefficients of the fit.  A is a six-element vector as 
;		described under PURPOSE.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	The peak or minimum of the Gaussian must be the largest
;	or smallest point in the Y vector.
;
; PROCEDURE:
;	If the (MAX-AVG) of Y is larger than (AVG-MIN) then it is assumed
;	that the line is an emission line, otherwise it is assumed there
;	is an absorbtion line.  The estimated center is the MAX or MIN
;	element.  The height is (MAX-AVG) or (AVG-MIN) respectively.
;	The width is found by searching out from the extrema until
;	a point is found less than the 1/e value.
;
; MODIFICATION HISTORY:
;	DMS, RSI, Dec, 1983.
;-
;
on_error,2                      ;Return to caller if an error occurs
n = n_elements(y)		;# of points.
c = poly_fit(x,y,1,yf)		;fit a straight line.
yd = y-yf			;difference.

ymax=max(yd) & xmax=x(!c) & imax=!c	;x,y and subscript of extrema
ymin=min(yd) & xmin=x(!c) & imin=!c
a=fltarr(6)			;coefficient vector
if abs(ymax) gt abs(ymin) then i0=imax else i0=imin ;emiss or absorp?
i0 = i0 > 1 < (n-2)		;never take edges
dy=yd(i0)			;diff between extreme and mean
del = dy/exp(1.)		;1/e value
i=0
while ((i0+i+1) lt n) and $	;guess at 1/2 width.
	((i0-i) gt 0) and $
	(abs(yd(i0+i)) gt abs(del)) and $
	(abs(yd(i0-i)) gt abs(del)) do i=i+1
a = [yd(i0), x(i0), abs(x(i0)-x(i0+i)), c(0), c(1), 0.] ;estimates
!c=0				;reset cursor for plotting
return,curvefit(x,y,replicate(1.,n),a,sigmaa, $
		function_name = "GAUSS_FUNCT") ;call curvefit
end

