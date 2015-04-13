; $Id: reduce_colors.pro,v 1.2 1993/10/06 16:34:05 doug Exp $

PRO REDUCE_COLORS, Image, Values
;+
; NAME:
;	REDUCE_COLORS
;
; PURPOSE:
;	This procedure reduces the number of colors used in an image
;	by eliminating pixel values without members.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	REDUCE_COLORS, Image, Values
;
; INPUTS:
;	Image:  The original image array. Note that the input array is
;		replaced by its color-reduced equivalent.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	Image:  The color-reduced image array.
;	Values: A vector of non-zero pixel values. If Image contains
;		pixel values from 0 to M, Values will be an M+1 element
;		vector containing the mapping from the old values to
;		the new. Values(I) contains the new color index of old
;		pixel index I.
;
; SIDE EFFECTS:
;	Input array is overwritten.
;
; PROCEDURE:
;	The pixel distribution histogram is obtained and the WHERE
;	function is used to find bins with non-zero values. Next,
;	a lookup table is made where table(old_pixel_value) contains
;	new_pixel_value, and then applied to the image.
;		
; EXAMPLE:
;	To reduce the number of colors and display an image with the
;	original color tables R, G, B:
;	  REDUCE_COLORS, Image, V
;	  TVLCT, R(V), G(V), B(V)
;
; MODIFICATION HISTORY:
;	DMS,	RSI, Oct, 1992.
;-

h = histogram(image, omax = mx, min = 0) ;Find distribution
Values = where(h)			;Non-zero elements
	;Make translation table using lowest possible precision
if mx le 255 then table = bytarr(mx+1) $
else if mx le 32767 then table = intarr(mx+1L) $
else table = lonarr(mx+1L)

table(values) = lindgen(n_elements(values))	;Fill the table

	;Translate to reduced palette
image = table(temporary(image))
end
