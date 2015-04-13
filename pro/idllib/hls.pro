; $Id: hls.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Pro HLS, Litlo, Lithi, Satlo, Sathi, Hue, Loops, Colr
;+
; NAME:
;	HLS
;
; PURPOSE:
;	Make a color table based on the HLS (Hue, Lightness, Saturation) 
;	color system.
;
; CATEGORY:
;	Z4 - Image processing, color table manipulation
;
; CALLING SEQUENCE:
;	HLS, Litlo, Lithi, Satlo, Sathi, Hue, Loops [, Colr]
;
; INPUTS:
;	Litlo:	Starting lightness, from 0 to 100%.
;
;	Lithi:	Ending lightness, from 0 to 100%.
;
;	Satlo:	Starting saturation, from 0 to 100%.
;
;	Sathi:	Ending stauration, from 0 to 100%.
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
;		Red = colr(*,0), green = colr(*,1), blue = colr(*,2).
;
; COMMON BLOCKS:
;	COLORS:	Contains the red, green, and blue vectors on exit.
;
; SIDE EFFECTS:
;	The color tables are loaded.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Adapted from program on page 619, Fundamentals of Interactive
;	Computer Graphics, Foley and Van Dam.
;
;	Using the input parameters, a spiral through the double-
;	ended HLS cone is traced.  Points along the cone
;	are converted from HLS to RGB.
;
; MODIFICATION HISTORY:
;	Written, DMS, Jan, 1983.
;	Changed common block, dms, 4/1987.
;-
	common colors,red,green,blue,cur_red,cur_green,cur_blue
	on_error,2                      ;Return to caller if an error occurs
	S = (sathi-satlo)/25600.*findgen(256)+satlo/100.
	L = (Lithi-Litlo)/25600.*findgen(256)+litlo/100.
	HG = Loops*360./256.*findgen(256)+ Hue
	Hmin = Min(hg)/360.
	IF HMIN LT 0. THEN HMIN = FIX(HMIN)-1 ELSE $
		HMIN =FIX(HMIN)
	HG = (HG - Hmin*360.) mod 360.	;Make all positive
	HR = (HG +120) mod 360.
	HB = (HG +240) mod 360.
;
	N2 = (L LE .5)*(L+L*S) + (L GT .5)*(L+S-L*S)
	N1 = 2*L - N2
	N21 = (N2-N1)/60.
	COLR = FLTARR(256,3)
;
	FOR I=0,255 DO BEGIN		;What a mess.
	  IF S(I) EQ 0. THEN COLR(I,*)=L(I) ELSE BEGIN
		IF HR(I) LT 60. THEN COLR(I,0)=N1(I)+N21(I)*HR(I) ELSE $
		IF HR(I) LT 180. THEN COLR(I,0)=N2(I) ELSE $
		IF HR(I) LT 240. THEN COLR(I,0)=N1(I)+N21(I)*(240.-HR(I)) ELSE $
			COLR(I,0)=N1(I)
		IF HG(I) LT 60. THEN COLR(I,1)=N1(I)+N21(I)*HG(I) ELSE $
		IF HG(I) LT 180. THEN COLR(I,1)=N2(I) ELSE $
		IF HG(I) LT 240. THEN COLR(I,1)=N1(I)+N21(I)*(240.-HG(I)) ELSE $
			COLR(I,1)=N1(I)
		IF HB(I) LT 60. THEN COLR(I,2)=N1(I)+N21(I)*HB(I) ELSE $
		IF HB(I) LT 180. THEN COLR(I,2)=N2(I) ELSE $
		IF HB(I) LT 240. THEN COLR(I,2)=N1(I)+N21(I)*(240.-HB(I)) ELSE $
			COLR(I,2)=N1(I)
		ENDELSE
	   ENDFOR
;
	COLR = FIX(COLR*255.)<255		;CVT TO 0, 255.
	RED = COLR(*,0)				;SAVE IN COMMON
	GREEN = COLR(*,1)
	BLUE = COLR(*,2)
	TVLCT,RED,GREEN,BLUE			;LOAD COLORS
	cur_red = red & cur_green = green & cur_blue = blue
	RETURN
END

