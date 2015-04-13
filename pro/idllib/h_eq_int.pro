; $Id: h_eq_int.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro h_eq_int, image	;Histogram equalize color tables from image
;+
; NAME:
;	H_EQ_INT
;
; PURPOSE:
;	Interactively histogram-equalize the color tables of an image
;	or a region of the display.  By moving the cursor across
;	the screen, the amount of histogram equalization is varied.
;
; CATEGORY:
;	Image processing.
;
; CALLING SEQUENCE:
;	H_EQ_INT, Image		;To histogram equalize from an image.
;	H_EQ_INT		;To histogram equalize from a region.
;
; INPUTS:
;	Image:	The image whose histogram is to be used in determining
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
;	A window is created and the histogram equalization function is 
;	plotted.
;
;	A linear ramp is overplotted.  Move the cursor from left
;	to right to vary the amount of histogram equalization applied to the 
;	color tables from 0 to 100%.  Press the right mouse button to exit.
;
; MODIFICATION HISTORY:
;	DMS, November, 1989, written.
;	AB, 21 September 1992,renamed from HIST_EQUAL_INT to H_EQ_INT to
;		avoid DOS filename limitations. HIST_EQUAL_INT is still
;		available as a wrapper to this routine under operating
;		systems that can handle longer file names.
;	JWG, 14 December 1992,routine did not restore font.
;-

common colors,r,g,b,cur_red,cur_green,cur_blue

on_error,2                      ;Return to caller if an error occurs
nc = !d.table_size	;# of colors in device
if nc eq 0 then message, 'Device has static color tables, Can''t adjust'
if n_elements(image) gt 0 then h = histogram(image) $
	else begin
	print,'Mark opposite corners of area of interest:'
	tvrdc,x,y,/dev,1	;one corner
	wait,.5			;Necessary for fast machines
	tvrdc,x1,y1,/dev,1
	x0 = x < x1
	y0 = y < y1
	x1 = x > x1
	y1 = y > y1
	h = histogram(tvrd(x0,y0,x1-x0+1, y1-y0+1))
	endelse
h(0) = 0		;For 0 backgrounds
for i=1,n_elements(h)-1 do h(i) = h(i)+h(i-1)
h = long(bytscl(h, top = nc-1))

old_window = !d.window
window,xs=400, ys=300,title='Histogram Equalization',/free
plot,h
oplot,[0,nc-1],[0,nc-1]
tvcrs,.5,.5,/norm

!err = 0
fact = 0.0
oldy = findgen(nc)
oldfact = 0.0
x = findgen(nc)
if n_elements(r) le 0 then begin	;color tables defined?
	r=indgen(nc) & g=r & b=r & endif
oldfont = !p.font
!p.font = 0
oldfacts = string(oldfact,format='(f5.3)')+' equalized'
xyouts,.1,.95,/norm,oldfacts

while !err ne 4 do begin
	tvrdc,xx,yy,2		;wait for movement
	fact = float(xx)/nc < 1.0 > 0.0
	if fact ne oldfact then begin
;		oplot,x,oldy,psym=3,color=0	;Erase old
		oldy = long(fact * h + (1.-fact) * x)
;		oplot,x,oldy,psym=3
		cur_red = r(oldy) & cur_green = g(oldy) & cur_blue = b(oldy)
		tvlct,cur_red, cur_green, cur_blue
		xyouts,.1,.95,/norm, oldfacts, col=0
		oldfacts = string(oldfact,format='(f5.3)')+' equalized'
		xyouts,.1,.95,/norm,oldfacts
		oldfact = fact
		endif
	endwhile
wdelete
!p.font = oldfont
wset,old_window
return
end

