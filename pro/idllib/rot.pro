; $Id: rot.pro,v 1.2 1993/11/03 18:59:40 dave Exp $

;Rotate and magnify an image
FUNCTION ROT,A,ANGLE,MAG,X0,y0, INTERP = interp, MISSING = missing, $
        PIVOT = pivot, CUBIC = cubic
;+
; NAME:
;	ROT
;
; PURPOSE:
;	Rotate, magnify or demagnify, and/or translate an image.
;
; CATEGORY:
;	Z3 - Image processing, geometric transforms.
;
; CALLING SEQUENCE:
;	Result = ROT(A, Angle, [Mag, X0, Y0], MISSING = missing,
;		INTERP = Interp, CUBIC = Cubic)
;
; INPUTS:
;	A:	The image array to be rotated.  This array may be of any type,
;		but it must have two dimensions.
;
;	ANGLE:	Angle of rotation in degrees CLOCKWISE. (Why?,
;		because of an error in the old ROT.)
;
; OPTIONAL INPUT PARAMETERS:
;	MAG:	Magnification/demagnification factor.  A value of 1.0 = no
;		change, > 1 is magnification and < 1 is demagnification.
;
;	X0:	X subscript for the center of rotation.  If omitted, X0 equals
;		the number of columns in the image divided by 2.
;
;	Y0:	Y subscript for the center of rotation.  If omitted, y0 equals
;		the number of rows in the image divided by 2.
;
; KEYWORDS:
;	INTERP:	Set this keyword for bilinear interpolation.  If this keyword
;		is set to 0 or omitted, nearest neighbor sampling is used.
;		Note that setting this keyword is the same as using the 
;		ROT_INT User Library function.  This change (and others) 
;		essentially makes ROT_INT obsolete.
;
;	CUBIC:	If set, uses "Cubic convolution" interpolation.  A more
;		accurate, but more time-consuming, form of interpolation.
;		CUBIC has no effect when used with 3 dimensional arrays.
;
;      MISSING:	The data value to substitute for pixels in the output image 
;		that map outside the input image.
;
;      PIVOT: Setting this keyword causes the image to pivot around the point
;		X0, Y0, so that this point maps into the same point in the
;		output image.  If this keyword is set to 0 or omitted, then the
;		point X0, Y0 in the input image is mapped into the center of
;		the output image.
;
; OUTPUTS:
;	ROT returns a rotated, magnified, and translated version of the
;	input image.  Note that the dimensions of the output image are
;	always the same as those of the input image.
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
;	The POLY_2D function is used to translate, scale, and
;	rotate the original image.
;
; EXAMPLE:
;	Create and display an image.  Then display a rotated and magnified
;	version.  Create and display the image by entering:
;
;		A = BYTSCL(DIST(256))
;		TV, A
;
;	Rotate the image 33 degrees and magnify it 1.5 times.  Use bilinear
;	interpolation to make the image look nice.  Enter:
;
;		B = ROT(A, 33, 1.5, /INTERP)
;		TV, B
;	
; MODIFICATION HISTORY:
;	June, 1982. 	Written by DMS, RSI.
;
;	Feb, 1986. 	Modified by Mike Snyder, ES&T Labs, 3M Company.
;	 		Adjusted things so that rotation is exactly on the 
;			designated center.
;
;	October, 1986.  Modified by DMS to use the POLY_2D function.
;
;	Aug, 1988.	Added INTERP keyword.
;       Dec, 1992.      Added PIVOT keyword, William Thompson, NASA/GSFC.
;	Nov, 1993.	Added CUBIC keyword, DMS/RSI.
;-
;
;
on_error,2		;Return to caller if error
B = FLOAT(SIZE(A))	;Get dimensions
IF N_PARAMS(0) LT 5 THEN BEGIN
	X0 = (B(1)-1)/2.		;Center of rotation in X.
	Y0 = (B(2)-1)/2.		; and in Y.
	IF N_PARAMS(0) LT 3 THEN MAG = 1. ;Mag specified?
	ENDIF
;
	IF KEYWORD_SET(PIVOT) THEN BEGIN
		XC = X0
		YC = Y0
	END ELSE BEGIN
		xc = (b(1)-1)/2.        ;center of output image.
		yc = (b(2)-1)/2.
	ENDELSE
	theta = -angle/!radeg		;angle in degrees CLOCKWISE.
	c = cos(theta)*mag		;cos theta * mag
	s = sin(theta)*mag
;
	kx = -xc+c*x0-s*y0		;useful constants.
	ky = -yc+s*x0+c*y0
	kk = 1./(1.+s^2/c^2)
;
	cx = kk* [s/c^2*ky+kx/c,s/c^2,1/c,0.] ;x coeff...
	cy = kk * [-s/c^2*kx+ky/c,1/c,-s/c^2,0.] ;y coeff.

	i = 0				;assume no interpolation
	if keyword_set(interp) then i=1 ;Bilinear
	if keyword_set(Cubic) then i=2  ;Cubic

	if n_elements(missing) eq 0 then return,poly_2d(a,cx,cy, i) $
	else return, poly_2d(a,cx,cy, i, missing = missing) 
END
