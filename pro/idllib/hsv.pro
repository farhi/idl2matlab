; $Id: hsv.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Pro HSV, Vlo, Vhi, Satlo, Sathi, Hue, Loops, Colr
;+
; NAME:
;	HSV
;
; PURPOSE:
;	Make a color table based on the HSV (Hue, Saturation, and Value) 
;	color system.
;
; CATEGORY:
;	Z4 - Image processing, color table manipulation
;
; CALLING SEQUENCE:
;	HLS, Vlo, Vhi, Satlo, Sathi, Hue, Loops [, Colr]
;
; INPUTS:
;	Vlo:	Starting value, from 0 to 100%.
;
;	Vhi:	Ending value, from 0 to 100%.
;
;	Satlo:	Starting saturation, from 0 to 100%.
;
;	Sathi:	Ending saturation, from 0 to 100%.
;
;	Hue:	Starting Hue, from 0 to 360 degrees.  Red = 0 degs,
;		green = 120, blue = 240.
;
; 	Loops:	The number of loops through the color spiral.  This
;		parameter does not have to be an integer.  A negative value
;		causes the loops to traverse the spiral in the opposite
;		direction.
;
; OUTPUTS:
;	No required outputs.
;
; OPTIONAL OUTPUT PARAMETERS:
;	Colr:	A (256,3) integer array containing the R, G, and B values
;		that were loaded into the color tables.
;		Red = colr(*, 0), green = colr(*, 1), blue = colr(*, 2).
;
; COMMON BLOCKS:
;	COLORS:	Contains the red, green and blue color vectors on exit.
;
; SIDE EFFECTS:
;	The color tables are loaded.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Adapted from a program on page 616, Fundamentals of Interactive
;	Computer Graphics, Foley and Van Dam.
;
;	Using the input parameters, a spiral through the single-ended HSV 
;	cone is traced.  Points along the cone are converted from HLS to RGB.
;
; MODIFICATION HISTORY:
;	Written, DMS, Jan, 1983.
;	Added common block COLORS, DMS, Dec, 1983 and Apr, 1987.
;-
	common colors,red,green,blue,cur_red,cur_green,cur_blue
	on_error,2                        ;Return to caller if an error occurs
	S = (sathi-satlo)/25600.*findgen(256)+satlo/100.
	V = (Vhi-Vlo)/25600.*findgen(256)+vlo/100.
	H = Loops*360./256.*findgen(256)+ Hue
	Hmin = Min(H)/360.
	IF HMIN LT 0. THEN HMIN = FIX(HMIN)-1 ELSE $
		HMIN =FIX(HMIN)
	H= ((H - Hmin*360.) mod 360.)/60.	;Sector, 0 to 5.
	IH = FIX(H)
	f=h-ih			;fractional part
	x = FIX(255.*[[v*(1.-s)],[v*(1.-s*f)],[v*(1.-s*(1.-f))],[v]]);4 choices
	mat = [[3,2,0],[1,3,0],[0,3,2],[0,1,3],[2,0,3],[3,0,1]] ;selector
;
	colr = intarr(256,3)		;define tables
	for i=0,255 do $
		if s(i) eq 0. then colr(i,*)=255.*v(i) else $
		 for j=0,2 do colr(i,j)=x(i,mat(j,ih(i)))
	red = colr(*,0) & green = colr(*,1) & blue = colr(*,2)
	tvlct,red,green,blue
	cur_red = red & cur_green = green & cur_blue = blue ;save current clrs
	return
end
