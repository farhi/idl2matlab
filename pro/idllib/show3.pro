; $Id: show3.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro show3, image, interp = interp, sscale=sscale  ;Show an image three ways...
;+
; NAME:
;	SHOW3
;
; PURPOSE:
;	Show a 2D array three ways in a display that combines SURFACE, 
;	CONTOUR, and an image (color/gray scale pixels).
;
; CATEGORY:
;	Display, graphics.
;
; CALLING SEQUENCE:
;	SHOW3, Image [, INTERP = Interp, SSCALE = Sscale]
;
; INPUTS:
;	Image:	The 2-dimensional array to display.
;
; KEYWORD PARAMETERS:
;	INTERP:	Set this keyword to use bilinear interpolation on the pixel 
;		display.  This technique is slightly slower, but for small 
;		images, it makes a better display.
;
;	SSCALE:	Reduction scale for surface. The default is 1.  If this
;		keyword is set to a value other than 1, the array size 
;		is reduced by this factor for the surface display.  That is, 
;		the number of points used to draw the wire-mesh surface is
;		reduced.  If the array dimensions are not an integral multiple
;		of SSCALE, the image is reduced to the next smaller multiple.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A new plot is generated.
;
; RESTRICTIONS:
;	The display gets too "busy" when displaying larger (say 50 by 50),
;	images, especially if they are noisy.  It can be helpful to use
;	the SSCALE keyword or the SMOOTH and/or REBIN functions to smooth the 
;	surface plot.
;
;	You might want to modify the calls to CONTOUR and SURFACE slightly
;	to customize the display to your tastes, i.e., with different colors,
;	skirts, linestyles, contour levels, etc.
;
; PROCEDURE:
;	First, do a SURFACE with no data to establish the 3D to 2D scaling.
;	Then convert the coordinates of the corner pixels of the array to
;	2D.  Use POLYWARP to get the warping polynomial to warp the
;	2D image into the area underneath the SURFACE plot.  Output the image,
;	output the surface (with data) and then output the contour plot at
;	the top (z=1).
;
; MODIFICATION HISTORY:
;	DMS. Jan, 1988.
;	Added fudges for PostScript, April, 1988.
;	Fixed bug where contour plot was occasionally clipped. Dec, 1990.
;-
on_error,2              ;Return to caller if an error occurs
s = size(image)		;Get size of image
nx = s(1)		;Columns
ny = s(2)		;Rows

if n_elements(sscale) eq 0 then sscale = 1 ;Default scale
sscale = fix(sscale)		;To Integer

if ((nx mod sscale) ne 0) or ((ny mod sscale) ne 0) then begin
	nx = (nx/sscale) * sscale ;To multiple
	ny = (ny/sscale) * sscale
	img = image(0:nx-1, 0:ny-1)
  endif else img = image

surface,img,/save,/nodata,xst=1,yst=1,zaxis=1 ;Set up scaling
empty			;Don't make 'em wait watching an empty screen.

xorig = [0.,nx-1,0.,nx-1]	;4 corners X locns in image
yorig = [0.,0.,ny-1,ny-1]	;4 corners Y locns

x = xorig * !x.s(1) + !x.s(0)	;Normalized X coord
y = yorig * !y.s(1) + !y.s(0)	;Normalized Y
			;To Homogeneous coords,  and transform
p = [[x],[y],[fltarr(4)],[replicate(1,4)]] # !P.T 
u = p(*,0)/p(*,3) * !d.x_vsize	;Scale U coordinates to device
v = p(*,1)/p(*,3) * !d.y_vsize	;and V
;
;	Now, the 4 corners of the place for the image are in u and v
;
u0 = min(u) & v0 = min(v)		;Lower left corner of screen box
su = max(u)- u0+1 & sv = max(v) - v0+1	;Size of new image
if (!d.flags and 1) eq 1 then begin	;Scalable pixels (PostScript)?
	fact = 50		;Yes, shrink it
	miss = 255		;Missing values are white
	c_color=[0,0]		;Contour in only one color, black
 endif else begin
	fact = 1 		;one pixel/output coordinate
	miss = 0		;missing is black
	c_color=[150,200,250]
 endelse

if (!d.flags and 512) ne 0 then $  ;White background?
	miss = 255 else miss = 0
;
	;Get polynomial coeff for warp
if !d.n_colors gt 2 then top = !d.n_colors -1 else top = 255
polywarp, xorig, yorig, (u-u0)/fact, (v-v0)/fact, 1, kx, ky 
if n_elements(interp) eq 0 then interp = 0
a = poly_2d(bytscl(img, top=top), kx, ky, interp,su/fact,sv/fact,$
		 missing = miss) ;Warp it
tv,a,u0,v0,xsize = su, ysize = sv
surface,rebin(img,nx/sscale, ny/sscale),$
	/save,/noerase,xst=1,yst=1, bot=128 ;Show the surface
			;And finally, draw contour on top
contour,img,/t3d,/noerase,zval=1.0,xst=1,yst=1,c_color = c_color,/noclip
end
