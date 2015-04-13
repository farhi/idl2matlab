; $Id: tek_color.pro,v 1.3 1993/06/28 23:57:20 dave Exp $

pro tek_color, Start_index, Ncolors
;+
; NAME:
;	TEK_COLOR
;
; PURPOSE:
;	Load a color table similar to the default Tektronix 4115 color table.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	TEK_COLOR [[, Start_index] , Ncolors]
;
; INPUTS:
;	Start_index = optional starting index of palette.  If omitted,
;		use 0.
;	Ncolors = Number of colors to load.  32 is the max and the default.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	Colors.
; SIDE EFFECTS:
;	Ncolors color indices, starting at Start_index are loaded with
;	the Tektronix 4115 default color map.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Just copy the colors.  This table is useful for the
;	display of graphics in that the colors are distinctive.
;
;	Basic colors are:  0 - black, 1 - white, 2 - red, 3 - green, 
;	4 - blue, 5 - cyan, 6 - magenta, 7 - yellow, 8 - orange, etc.
; MODIFICATION HISTORY:
;	DMS, Jan, 1989.
;	DMS, June, 1992.  Added colors common.
;	DMS, Apr, 1993, Added start_index and ncolors.
;-
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if n_elements(ncolors) le 0 then ncolors = 32
if n_elements(start_index) le 0 then start_index = 0

if n_elements(r_orig) lt (ncolors + start_index) then begin
	r_orig = bytscl(indgen(256), max=!d.n_colors-1, min = 0)
	g_orig = r_orig
	b_orig = r_orig
	endif

;	The tektronix colors
r = bytscl([ 0,100,100,0,0,0,100,100,100,60,0,0,55,100,33,67, $
	100,75,45,17,25,50,75,100,67,40,17,17,17,45,75,90])
g = bytscl([ 0,100,0,100,0,100,0,100,50,83,100,50,0,0,33,67, $
	100,100,100,100,83,67,55,33,90,90,90,67,50,33,17,9])
b = bytscl([ 0,100,0,0,100,100,83,0,0,0,60,100,83,55,33,67, $
	33,45,60,75,83,83,83,90,45,55,67,90,100,100,100,100])

if ncolors lt 32 then begin		;Trim?
	r = r(0:ncolors-1)
	g = g(0:ncolors-1)
	b = b(0:ncolors-1)
	endif
s = start_index < (256 - ncolors)	;Never over top

r_orig(s) = r
g_orig(s) = g
b_orig(s) = b

tvlct, r_orig, g_orig, b_orig
r_curr = r_orig
g_curr = g_orig
b_curr = b_orig
end
