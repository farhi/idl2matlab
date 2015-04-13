; $Id: h_eq_ct.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro h_eq_ct, image	;Histogram equalize color tables from image
;+
; NAME:
;	H_EQ_CT
;
; PURPOSE:
;	Histogram-equalize the color tables for an image or a region
;	of the display.
;
; CATEGORY:
;	Image processing.
;
; CALLING SEQUENCE:
;	H_EQ_CT, Image	;To histogram equalize from an image.
;	H_EQ_CT		;To histogram equalize from a region
;
; INPUTS:
;	Image:	Image whose histogram is to be used in determining
;		the new color tables.  If this value is omitted, the user 
;		is prompted to mark the diagonal corners of a region of the 
;		display.
;
;		Image MUST be a byte image, scaled the same way as
;		the image loaded to the display.
;
; OUTPUTS:
;	No explicit outputs.  The result is applied to the current color
;	tables.
;
; COMMON BLOCKS:
;	COLORS:	The IDL color table common block.
;
; SIDE EFFECTS:
;	The current color table is modified.
;
; RESTRICTIONS:
;	If a parameter is supplied, it is assumed to be an image that
;	was just displayed.
;
; PROCEDURE:
;	Either the image parameter or the region of the display marked by
;	the user is used to obtain a pixel-distribution histogram.  The
;	cumulative integral is taken and scaled.  This function is applied
;	to the current color tables.
;
; MODIFICATION HISTORY:
;	DMS, March, 1988, written.
;	DMS, May, 1990, added BOX_CURSOR.
;	AB, 21 September 1992,renamed from HIST_EQUAL_CT to H_EQ_CT to
;		avoid DOS filename limitations. HIST_EQUAL_CT is still
;		available as a wrapper to this routine under operating
;		systems that can handle longer file names.
;-

common colors,r,g,b,cur_red,cur_green,cur_blue

on_error,2                      ;Return to caller if an error occurs
nc = !d.table_size	;# of colors in device
if nc eq 0 then message, 'Device has static color tables, Can''t adjust'
if n_elements(image) gt 0 then h = histogram(image) $
	else begin
	box_cursor, x0, y0, xs, ys, /message
	h = histogram(tvrd(x0, y0, xs, ys))
	endelse
for i=1,n_elements(h)-1 do h(i) = h(i)+h(i-1)
h = long(bytscl(h, top = nc-1))
if n_elements(r) le 0 then begin	;color tables defined?
	r=indgen(nc) & g=r & b=r & endif
cur_red = r(h) & cur_green = g(h) & cur_blue = b(h)
tvlct,cur_red, cur_green, cur_blue
return
end

