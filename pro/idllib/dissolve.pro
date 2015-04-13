; $Id: dissolve.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro DISSOLVE, image, SIZ = siz, X0 = x0, Y0 = y0, DELAY = delay, $
	ORDER = order
;+
; NAME:
;	DISSOLVE
;
; PURPOSE:
;	A digital "dissolve" effect for images.  Copies the pixels (arranged
;	into square tiles) from the image to the display in pseudo-random
;	order.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	DISSOLVE, Image
;
; INPUTS:
;	Image:	The image to be displayed.  It is assumed that the image is
;		already scaled.  Byte-valued images display most rapidly.
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; KEYWORD PARAMETERS:
;	SIZ:	Size of square tile.  The default is 32 x 32 pixels.
;
;	X0, Y0: The X and Y offsets of the lower-left corner of the image on 
;		screen.  The defaults are X0 = 0 and Y0 = 0.
;
;	DELAY:	The wait between displaying tiles.  The default is 0.01 secs.
;
;	ORDER:	The Image display order:  	0 = default = bottom up.
;						1 = top-down.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The image is written to the screen.
;
; RESTRICTIONS:
;	None, but the effect is dependent upon the speed of the
;	machine/display.
;
; PROCEDURE:
;	An integer pseudo-random number generator is used to decide
;	which tile to display.  The algorithm is taken from "Graphics Gems",
;	Andrew Glassner, ed., Academic Press, 1990, Page 221.
;
; MODIFICATION HISTORY:
;	DMS, Sept, 1990.
;-

; Do a random dissolve, using the TV command.
; 
if n_elements(siz) le 0 then siz = 32	;Default square size....
if n_elements(x0) le 0 then x0 = 0
if n_elements(y0) le 0 then y0 = 0
if n_elements(delay) le 0 then delay = 0.01
if n_elements(order) le 0 then order = 0

s = size(image)
nx = s(1)
ny = s(2)
siz = siz < nx < ny		;Never smaller than dims

nxs = (nx + siz - 1) / siz	;Squares across
nys = (ny + siz - 1) / siz	;Squares up/down


rwidth = alog(nys)/alog(2)	;Bits/dimension
cwidth = alog(nxs)/alog(2)
if rwidth ne fix(rwidth) then rwidth = rwidth + 1  ;Ceiling fcn
if cwidth ne fix(cwidth) then cwidth = cwidth + 1 
rwidth = fix(rwidth)
cwidth = fix(cwidth)
regwidth = rwidth + cwidth	;Total width

; The shift reg mask
mask = ([ '3'xl, '6'xl, '0c'xl,'14'xl, '30'xl, '60'xl, 'b8'xl, '0110'xl, $
	'240'xl, '500'xl, '0ca0'xl, '01b00'xl, '3500'xl, $
	'6000'xl, 'b400'xl, '12000'xl, '20400'xl, '72000'xl, $
	'90000'xl, '140000'xl, '300000'xl, '400000'xl ])(regwidth-2)

colmask = ishft(1,cwidth) -1	;Mask to extract column

start_seq = long(systime(1) mod ishft(1,regwidth-1)) ;Random starting cell
if start_seq le 0 then start_seq = 1L
seq = start_seq
repeat begin
	row = ishft(seq, -cwidth)	;The row chunk
	col = seq and colmask		;Col chunk
	if (row lt nys) and (col  lt nxs) then begin  ;Within image?
		y00 = row * siz
		x00 = col * siz
		y01 = (y00 + siz-1) < (ny-1)
		x01 = (x00 + siz-1) < (nx-1)
		if order ne 0 then $
		  y02 = ny - y00 - (y01-y00) > 0 else y02 = y00
		tv,image(x00:x01, y00:y01), x00 + x0, y02+y0, order = order
		wait,delay
		endif
	if seq and 1 then seq = ishft(seq, -1) xor mask $
	else seq = ishft(seq, -1)
endrep until seq eq start_seq

if order eq 0 then tv,image(0:siz-1, 0:siz-1),x0,y0 $	;Last chunk
else tv,image(0:siz-1, 0:siz-1),x0, y0 + ny-siz, /order
empty
end
