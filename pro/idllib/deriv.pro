; $Id: deriv.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Function Deriv, X, Y
;+
; NAME:
;	DERIV
;
; PURPOSE:
;	Perform numerical differentiation using 3-point, Lagrangian 
;	interpolation.
;
; CATEGORY:
;	Numerical analysis.
;
; CALLING SEQUENCE:
;	Dy = Deriv(Y)	 	;Dy(i)/di, point spacing = 1.
;	Dy = Deriv(X, Y)	;Dy/Dx, unequal point spacing.
;
; INPUTS:
;	Y:  Variable to be differentiated.
;	X:  Variable to differentiate with respect to.  If omitted, unit 
;	    spacing for Y (i.e., X(i) = i) is assumed.
;
; OPTIONAL INPUT PARAMETERS:
;	As above.
;
; OUTPUTS:
;	Returns the derivative.
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
;	See Hildebrand, Introduction to Numerical Analysis, Mc Graw
;	Hill, 1956.  Page 82.
;
; MODIFICATION HISTORY:
;	Written, DMS, Aug, 1984.
;-
;

on_error,2              ;Return to caller if an error occurs
n = n_elements(x)
if n lt 3 then message, 'Parameters must have at least 3 points'

if n_params(0) ge 2 then begin
	if n ne n_elements(y) then message,'Vectors must have same size'
	d = float(shift(y,-1) - shift(y,1))/(shift(x,-1) - shift(x,1))
	d(0) = (-3.0*y(0) + 4.0*y(1) - y(2))/(x(2)-x(0))
	d(n-1) = (3.*y(n-1) - 4.*y(n-2) + y(n-3))/(x(n-1)-x(n-3))
   end else begin
	d = (shift(x,-1) - shift(x,1))/2.
	d(0) = (-3.0*x(0) + 4.0*x(1) - x(2))/2.
	d(n-1) = (3.*x(n-1) - 4.*x(n-2) + x(n-3))/2.
   endelse
return, d
end
