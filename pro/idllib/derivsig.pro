; $Id: derivsig.pro,v 1.2 1993/10/15 21:57:36 doug Exp $
Function Derivsig, X, Y, sigx, sigy
;+
; NAME:
;	DERIVSIG
;
; PURPOSE:
;	This function computes the standard deviation of a derivative
;       as found by the DERIV function, using the input variables of
;	DERIV and the standard deviations of those input variables.
;
; CATEGORY:
;	Numerical analysis.
;
; CALLING SEQUENCE:
;	sigDy = Derivsig(sigy)		;sigma(Dy(i)/di), point spacing = 1.
;	sigDy = Derivsig(X,Y,sigx,sigy) ;sigma(Dy/Dx), unequal point spacing.
;
; INPUTS:
;	Y:	The variable to be differentiated. Omit if X is omitted.
;	X:	The Variable to differentiate with respect to. If omitted,
;		unit spacing is assumed for Y, i.e. X(i) = i.
;       sigy:	The standard deviation of Y. (Vector if used alone in
;		call, vector or constant if used with other parameters)
;       sigx:	The standard deviation of X (either vector or constant).
;		Use "0.0" if the abscissa is exact; omit if X is omitted.
;
; OPTIONAL INPUT PARAMETERS:
;	As above.
;
; OUTPUTS:
;	This function returns the standard deviation of the derivative.
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
;	See Bevington, "Data Analysis and Reduction for the Physical
;           Sciences," McGraw-Hill (1969), Chap 4.
;
; MODIFICATION HISTORY:
;       Written by Richard Bonomo at the University of Wisconsin - Madison
;       department of Electrical and Computer Engineering, July, 1991.
;	"DERIV" written by DMS, Aug, 1984.
;-
;

on_error,2              ;Return to caller if an error occurs
prms=n_params(0)
n = n_elements(x)
if (n lt 3) and (prms gt 1) then message, 'X must have at least 3 points'
if (n lt 3) and (prms eq 1) then message, $
    'sigy must be a vector of at least 3 points if used alone'

if ((prms ne 1) and (prms ne 4)) then message,$
   'function DERIVSIG must be called with either 1 or 4 parameters'

if prms eq 1 then begin ; unit spacing assumed
        sigy=x
        if n_elements(sigy) eq 1 then sigy=fltarr(n) + sigy
        sigd=sqrt(0.25*(shift(sigy,-1)*shift(sigy,-1) + $
         shift(sigy,1)*shift(sigy,1)))
        sigd(0)=sqrt(0.25*(sigy(0)^2*9.0 + sigy(1)^2*16.0 + sigy(2)^2))
        sigd(n-1)=sqrt(0.25*(sigy(n-1)^2*9.0 + sigy(n-2)*16.0 + sigy(n-3)))
endif
if prms eq 4 then begin
        if n ne n_elements(y) then message,'Vectors must have same size'
        if n_elements(sigy) eq 1 then sigy=fltarr(n) + sigy
        nix=n_elements(sigx)
        if (nix eq 1) and (sigx(0) ne 0.0) then sigx=fltarr(n) + sigx
        nix=n_elements(sigx)
        if (nix ne n) and (nix ne 1) then message,$
           'sigx vector must have the same length as X, or be a scalar'
        dsq=shift(x,-1)-shift(x,1)
        dsq=dsq*dsq
        dy=shift(y,-1)-shift(y,1)
        sigd=(shift(sigy,-1)*shift(sigy,-1) + shift(sigy,1)*shift(sigy,1))/dsq
        if (nix ne 1) then sigd=sigd + (shift(sigx,-1)^2*dy^2 + $
         shift(sigx,1)^2*dy^2)/(dsq*dsq)
        sigd=sqrt(sigd)
        dsq=x(2)-x(0)
        dsq=dsq*dsq
;
        sigd(0)=(sigy(0)^2*9.0 + sigy(1)^2*16.0 + sigy(2)^2)/dsq
        if (nix ne 1) then sigd(0) = sigd(0) + (sigx(2)^2*(3.0*y(0) - $
         4.0*y(1) + y(2))^2 + sigx(0)^2*(-3.0*y(0) + 4.0*y(1) - y(2))^2)/$
         (dsq*dsq)
        sigd(0)=sqrt(sigd(0))
;
        dsq=x(n-1)-x(n-3)
        dsq=dsq*dsq
        sigd(n-1)=(sigy(n-1)^2*9.0 + sigy(n-2)^2*16.0 + sigy(n-3)^2)/dsq
        if (nix ne 1) then sigd(n-1) = sigd(n-1) + (sigx(n-1)^2*(-3.0*y(n-1)$
         + 4.0*y(n-2) - y(n-3))^2 + sigx(n-3)^2*(3.0*y(n-1) - 4.0*y(n-2)$
         + y(n-3))^2)/(dsq*dsq)
        sigd(n-1)=sqrt(sigd(n-1))
endif
return, sigd
end
