; Copyright (c) 1992, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
function hist_2d, im1, im2
;+
; NAME:
;	HIST_2D
;
; PURPOSE:
;	Return the density function (histogram) of two variables.
;
; CATEGORY:
;	Image processing, statistics, probability.
;
; CALLING SEQUENCE:
;	Result = hist_2d(V1, V2)
; INPUTS:
;	V1 and V2 = arrays containing the variables.  They must be
;		of byte, integer, or longword type, and contain
;		no negative elements.
;
; OUTPUTS:
;	The two dimensional density function of the two variables,
;	a longword array of dimensions (MAX(v1)+1, MAX(v2)+1).  Result(i,j)
;	is equal to the number of sumultaneous occurences of V1 = i,
;	and V2 = j, at the same element.
;	
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Data must be in byte, integer, or longword form.  To use with
;	floating point data, scale into the range of integers.
;
; PROCEDURE:
;	Creates a combines array from the two variables, equal to the
;	linear subscript in the resulting 2D histogram, then applies
;	the standard histogram function.
;
;	The following pseudo-code shows what the result contains,
;	not how it is computed:
;		r = LONARR(MAX(v1)+1, MAX(v2)+1)  ;Result
;		FOR i=0, N_ELEMENTS(v1)-1 DO $
;		  r(v1(i), v2(i)) = r(v1(i), v2(i)) +1

; EXAMPLE:
;	Return the 2D histogram of two byte images:
;		R = HIST_2D(image1, image2)
;	Return the 2D histogram made from two floating point images
;	with range of -1 to +1, and with 100 bins:
;		R = HIST_2D(long((f1+1) * 50), long((f2+1) * 50))
;
; MODIFICATION HISTORY:
; 	Written by:
;	DMS, Sept, 1992		Written
;-

; Form the 2 dimensional histogram of two byte, integer, or longword
;  images.  They must not contain negative numbers.
; Result(i,j) = density of pixel value i in im1, and pixel value j
;	in im2.
; Input images must be, of course, the same size....
;

s1 = size(im1)		;Check types
s2 = size(im2)
if (s1(s1(0)+1) gt 3) or (s2(s2(0)+1) gt 3) then $
	message, 'Arrays must be byte, integer, or longword'


m1 = max(im1, min=mm1)+1L	;Get size of resulting rows / columns
m2 = max(im2, min=mm2)+1L
if mm1 lt 0 or mm2 lt 0 then message,'Arrays contain negative elements'

sum = m1 * im2 + im1		;Combine with im1 in low part & im2 in high
h = histogram(sum, min = 0, max= m1 * m2 -1)  ;Get the 1D histogram
return, reform(h, m1, m2, /overwrite) ;and make it 2D
end
