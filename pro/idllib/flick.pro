; $Id: flick.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro flick,a,b,rate	;Flicker between the two output frames at a given rate.
			; a and b = images scaled from 0 to 255.
			;To terminate, type any key except F or S.
			;Rate = frames per second.
			; Type F to flick faster by a factor of 2
			;  S to flick slower by a factor of 2.
;+
; NAME:
;	FLICK
;
; PURPOSE:
;	Flicker between two output images at a given rate.
;
; CATEGORY:
;	Image display, animation.
;
; CALLING SEQUENCE:
;	FLICK, A, B, Rate
;
; INPUTS:
;	A:	Byte image number 1, scaled from 0 to 255.
;	B:	Byte image number 2, scaled from 0 to 255.
;
; OPTIONAL INPUT PARAMETERS:
;	Rate:	The flicker rate.  The default is 1.0 sec/frame
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Sunview: Modifies the display, changes the write mask.
;	X and Windows: uses two additional pixmaps.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;  SunView:
;	Image A is written to the bottom 4 bits of the display.
;	Image B is written to the top 4 bits.
;	Two color tables are created from the current table, one that
;	shows the low 4 bits using 16 of the original colors, and one
;	that shows the high 4 bits.  The color table is changed to
;	switch between images.
;  Other window systems:
;	two off screen pixmaps are used to contain the images.
;
; MODIFICATION HISTORY:
;	DMS, 3/ 88.
;	DMS, 4/92, Added X window and MS window optimizations.
;-
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
on_error,2                        ;Return to caller if an error occurs

if n_elements(rate) eq 0 then rate = 1.0 ;Parameter there?
ichl = 0
sfact = 1.5		;Speed steps

if !d.name eq "SUN" then begin
	if n_elements(r_orig) eq 0 then begin	;colors defined?
		r_orig=indgen(256) & g_orig = r_orig & b_orig = r_orig
		endif

	p1 = 16 * [[ lindgen(256)/16], [ lindgen(256) and 15]] ;(256,2)

	device, set_write=240	;load top 4 bits
	tv,a
	empty
	device, set_write=15	;load bottom 4 bits
	tv,b/16b
	empty
	device,set_write=255	;re-enable all 8 bits

	while 1 do begin	;loop infinitely over each chl
		p = p1(*,ichl)	;get appropriate table
		tvlct,r_orig(p), g_orig(p), b_orig(p) ;load 4 bit table
		wait,1./rate	;This also empties the graphics buffer
		chr = get_kbrd(0) ;Read character
		case strupcase(chr) of
	"F":	rate = rate*sfact	;Faster
	"S": 	rate = rate/sfact	;Slower
	"":	ichl = 1 - ichl	;Other image
	else:	goto,done
		endcase
	endwhile
;
	done:	tvlct, r_orig, g_orig, b_orig
	empty
	return

ENDIF ELSE BEGIN			;Assume X or Windows
	if !d.window lt 0 then window
	cwin = !d.window
	pix = intarr(2)		;Make 2 pixmaps
	for i=0,1 do begin
		window, /FREE, /PIX, xs = !d.x_size, ys = !d.y_size
		pix(i) = !d.window
		if i eq 0 then tv,a else tv,b
		endfor
	wset, cwin
	while 1 do begin	;loop infinitely over each chl
		device, copy=[0,0,!d.x_size, !d.y_size, 0, 0, pix(ichl)]
		wait,1./rate	;This also empties the graphics buffer
		chr = get_kbrd(0) ;Read character
		case strupcase(chr) of
	"F":	rate = rate*sfact	;Faster
	"S": 	rate = rate/sfact	;Slower
	"":	ichl = 1 - ichl	;Other image
	else:	goto,done1
		endcase
	endwhile
;
done1:	wdelete, pix(0), pix(1)
	return
ENDELSE
end
