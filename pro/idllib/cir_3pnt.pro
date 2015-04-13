; $Id: cir_3pnt.pro,v 1.2 1993/10/04 19:50:01 doug Exp $

PRO cir_3pnt, x, y, r, x0, y0

;+
; NAME:
;	CIR_3PNT
;
; PURPOSE:
;	This procedure returns the radius and center of a circle,
;	given 3 points on the circle. This is analogous to finding
;	the circumradius and circumcircle of a triangle; the center
;	of the circumcircle is the point at which the three perpendicular
;	bisectors of the triangle formed by the points meet.
;
; CATEGORY:
;	Analytical geometry.
;
; CALLING SEQUENCE:
;	CIR_3PNT, X, Y, R, X0, Y0
;
; INPUTS:
;	X: A three-element vector containing the X-coordinates of the points.
;	Y: A three-element vector containing the Y-coordinates of the points.
;
; OUTPUTS:
;	R: The radius of the circle. The procedure returns 0.0 if the points
;	   are co-linear.
;	X0, Y0: The coordinates of the center of the circle. The procedure
;	        returns 0.0 if the points are co-linear.
;
; PROCEDURE:
;	Derived from Glasser, ed.  Graphics Gems, Volume 1, Page 22.
;
; EXAMPLE:
;	X = [1.0, 2.0, 3.0]
;	Y = [1.0, 2.0, 1.0]
;	CIR_3PNT, X, Y, R, X0, Y0
;	Print, 'The radius is: ', R
;	Print, 'The center of the circle is at: ', X0, Y0
	
; MODIFICATION HISTORY:
; 	Written by:	DMS, RSI, Nov, 1992.
;-

x = double(x)
y = double(y)

d1 = (x(2)-x(0)) * (x(1)-x(0)) + (y(2)-y(0)) * (y(1)-y(0))
d2 = (x(2)-x(1)) * (x(0)-x(1)) + (y(2)-y(1)) * (y(0)-y(1))
d3 = (x(0)-x(2)) * (x(1)-x(2)) + (y(0)-y(2)) * (y(1)-y(2))

c1 = d2 * d3
c2 = d3 * d1
c3 = d1 * d2
c = c1 + c2 + c3 + 0.0		;Force to floating or dbl
if abs(c lt 1e-14) then begin	;Colinear?
	r =  0.
	x0 = 0.
	y0 = 0.
	return
	endif
r = sqrt((d1 + d2)*(d2 + d3) * (d3 + d1)/c)/2.
v = [ c2 + c3, c3 + c1, c1 + c2 ]/(2.*c)
x0 = total(v * x)
y0 = total(v * y)
end
