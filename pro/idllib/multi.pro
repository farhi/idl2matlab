; $Id: multi.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro multi, n	;Multiple wrap of existing color tables
;+
; NAME:
;	MULTI
;
; PURPOSE:
;	Expand the current color table to wrap around N times.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	MULTI, N
;
; INPUTS:
;	N:	The number of times the color table will wrap.  This 
;		parameter does not need not be an integer.
;
; OUTPUTS:
;	No explicit outputs, color tables are loaded.
;
; COMMON BLOCKS:
;	COLORS, the IDL color table common block, contains current color 
;	tables, loaded by LOADCT, ADJCT, HLS, HSV, etc.
;
; SIDE EFFECTS:
;	Color tables are loaded.
;
; RESTRICTIONS:
;	One of the above procedures must have been called.
;
; PROCEDURE:
;	Tables are expanded by a factor of n.
;
; EXAMPLE:
;	Display an image, load color table 1, and make that color table
;	"wrap around" 3 times.  Enter:
;		TVSCL, DIST(256)	;A simple image.
;		LOADCT, 1		;Load color table 1.
;		MULTI, 3		;See how the new color table affects
;					;the image.
; 
; MODIFICATION HISTORY:
;	DMS, May, 1984.
;	Changed common, DMS, 4/87.
;-
common colors,r,g,b,cur_red,cur_green,cur_blue
on_error,2                      ;Return to caller if an error occurs
if n_params() eq 0 then n = 1	;Default = 1 wrap
m = n_elements(r)		;size of tables
if m le 0 then begin
		m = 256		;Default to 256.
		r=indgen(m)  & g = r  & b = r
		end
mm = (indgen(M)*n) mod m	;calc subscripts
cur_red = r(mm) & cur_green = g(mm) & cur_blue = b(mm)
tvlct,cur_red,cur_green,cur_blue ;load it
return
end




























