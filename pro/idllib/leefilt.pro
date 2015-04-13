; $Id: leefilt.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Function Leefilt, A, N, Sig
;+
; NAME:
;	LEEFILT
;
; PURPOSE:
;	Performs the Lee filter algorithm on an image array using a 
;	box of size 2N+1.  This function can also be used on vectors.
;
; CATEGORY:
;	E3 Smoothing (image).
;
; CALLING SEQUENCE:
;	Result = LEEFILT(A [, N, Sig])
;
; INPUTS:
;	A:	The input image array or one-dimensional vector.
;
; OPTIONAL INPUT PARAMETERS:
;	N:	The size of the filter box is 2N+1.  The default value is 5.
;
;	Sig:	Estimate of the standard deviation.  The default is 5.
;		If Sig is negative the procedure requests a value to be 
;		entered, and displays the resulting image.  This cycle 
;		continues until a zero value of Sig is entered.
;
; OUTPUTS:
;	The filtered image or vector is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Displays the filtered image in an IDL window using TVSCL if Sig
;	was negative.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	The LEE (Computer Graphics 197?) technique smooths additive
;	image noise by generating statistics in a local neighborhood
;	and comparing them to the expected values.
;
; MODIFICATION HISTORY:
;	Written, 24-Nov-1982, by R. A. Howard, Naval Research Lab,
;				 Washington, DC 20375
;-
;
on_error,2                      ;Return to caller if an error occurs
NP = N_params(0)
if np lt 3 then Sig = 5.	;supply defaults
if np lt 2 then n = 5
pl = sig le 0.			;true if interactive mode
mean = smooth(float(a),2*n+1)	;make mean
f = (a-mean)^2			;deviation
var = smooth(f,2*n+1)
loop:
	if pl then read,'Type in <sigma> ',sig
	if sig eq 0 then goto,endp
	sig = sig ^2
	var = var/(var+sig)
	f = a - mean
	f=f*var
	f = mean+f
	if pl then begin
		tvscl,f
		goto , loop
		endif
endp:	return,f
end

