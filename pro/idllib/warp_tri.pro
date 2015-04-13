; $Id: warp_tri.pro,v 1.3 1994/02/28 16:16:57 dave Exp $

function warp_tri, xo, yo, xi, yi, im_in, OUTPUT_SIZE = output_size, $
	QUINTIC = quintic, EXTRAPOLATE = extra
; xo, yo = coordinates of tie points in output image.
; xi, yi = coordinates of tie points in im_in.
;+
; NAME:
;	WARP_TRI
;
; PURPOSE:
;	This function warps images using control (tie) points.
;
; CATEGORY:
;	Image processing, geometric transformation.
;
; CALLING SEQUENCE:
;	Result = WARP_TRI(Xo, Yo, Xi, Yi, Im_in)
;
; INPUTS:
;	Xo, Yo:	     Vectors containing the locations of the tie points
;		     in the output image.
;	Xi, Yi:	     Vectors containing the location of the tie points
;		     in the input image (Im_in). Xi, Yi must be the same
;		     length as Xo, Y0.
;	Im_in:	     The image to be warped. May be any type of data.
;
; KEYWORD PARAMETERS:
;	OUTPUT_SIZE: A 2-element vector containing the size of the
;		     output image. If omitted, the output image is the
;		     same size as Im_in.
;	QUINTIC:     Set this keyword to use smooth quintic interpolation.
;		     Quintic interpolation is slower but the
;		     derivatives are continuous across triangles,
;		     giving a more pleasing result than the default
;		     linear interpolation. 
;	EXTRAPOLATE: Set to true to extrapolate outside the convex
;		     hull of the tie points. Setting this keyword implies
;		     the use of QUINTIC interpolation.
;
; OUTPUTS:
;	This function returns an image array with the specified
;	geometric correction applied. Points at locations (Xi, Yi)
;	are shifted to (Xo, Yo).
;
; PROCEDURE:
;	The irregular grid defined by (Xo, Yo) is triangulated
;	using TRIANGULATE. Then the surfaces defined by (Xo, Yo, Xi)
;	and (Xo, Yo, Yi) are interpolated using TRIGRID to get
;	the locations in the input image of each pixel in the output
;	image. Finally, INTERPOLATE is called to obtain the result.
;	Linear interpolation is used by default.  Smooth quintic
;	interpolation is used if the QUINTIC keyword is set.
;
; MODIFICATION HISTORY:
;	DMS, Jan, 1992.
;	DMS, Jul, 1992, added quintic interpolation.
;-

s = SIZE(im_in)
if s(0) ne 2 then MESSAGE, 'Warp_tri - Im_in param must be 2D'

TRIANGULATE, xo, yo, tr, bounds

if n_elements(output_size) ge 2 then begin
	nx = output_size(0)
	ny = output_size(1)
endif else begin
	nx = s(1)
	ny = s(2)
endelse

gs = [1,1]				;Grid spacing
b = [0,0, nx-1, ny-1]			;Bounds

; This style is called early schizophrenic capitalization.

if KEYWORD_SET(extra) then $
	return, INTERPOLATE(im_in,  $
	  TRIGRID(xo,yo,xi,tr, gs, b, /QUINT, EXTRA = bounds), $
	  TRIGRID(xo,yo,yi,tr, gs, b, /QUINT, EXTRA = bounds))$
ELSE $
	return, INTERPOLATE(im_in, $
	  TRIGRID(xo,yo,xi,tr, gs, b, QUINT=KEYWORD_SET(quintic)), $
	  TRIGRID(xo,yo,yi,tr, gs, b, QUINT=KEYWORD_SET(quintic)))
end

