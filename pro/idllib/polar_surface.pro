; $Id: polar_surface.pro,v 1.2 1993/10/05 21:57:01 doug Exp $

function polar_surface, z, r, theta, GRID = grid, SPACING = sp, $
	BOUNDS = bounds, QUINTIC = quintic, MISSING = missing
;+
; NAME:
;	POLAR_SURFACE
;
; PURPOSE:
;	This function interpolates a surface from polar coordinates
;	(R, Theta, Z) to rectangular coordinates (X, Y, Z).
;
; CATEGORY:
;	Gridding.
;
; CALLING SEQUENCE:
;	Result = POLAR_SURFACE(Z, R, Theta)
;
; INPUTS:
;	Z:	 An array containing the surface value at each point.
;		 If the data are regularly gridded (GRID=1) in R and 
;		 Theta, Z is a two dimensional array, where Z(i,j) has a
;		 radius of R(i) and an azimuth of Theta(j).  If GRID is
;		 not set, R(i) and Theta(i) contain the radius and azimuth
;		 of each Z(i).
;	R:	 The radius. If GRID is set, Z(i,j) has a radius of R(i).
;		 If GRID is not set, R must have the same number of elements
;		 as Z, and contains the radius of each point.
;	Theta:   The azimuth, in radians. If GRID is set, Z(i,j) has an
;		 azimuth of Theta(j). If GRID is not set, Theta must
;		 have the same number of elements as Z, and contains
;		 the azimuth of each point.
;
; KEYWORD PARAMETERS:
;	GRID:    Set GRID to indicate that Z is regularly gridded in R
;		 and Theta.
;	SPACING: A two element vector containing the desired grid spacing
;		 of the resulting array in X and Y.  If omitted, the grid
;		 will be approximately 51 by 51.
;	BOUNDS:  A four element vector, [X0, Y0, X1, Y1], containing the
;		 limits of the XY grid of the resulting array.  If omitted,
;		 the extent of input data sets the limits of the grid.
;	QUINTIC: If set, the function uses quintic interpolation, which is
;		 slower but smoother than the default linear interpolation.
;	MISSING: A value to use for areas within the grid but not within
;		 the convex hull of the data points. The default is 0.0.
;
; OUTPUTS:
;	This function returns a two-dimensional array of the same type as Z.
;
; PROCEDURE:
;	First, each data point is transformed to (X, Y, Z). Then
;	the TRIANGULATE and TRIGRID procedures are used to interpolate
;	the surface over the rectangular grid.
;
; EXAMPLE:
;	r = findgen(50) / 50.		  		;Radius
;	theta = findgen(50) * (2 * !pi / 50.) 		;Theta
;	z = r # sin(theta)		;Make a function (tilted circle)
;	SURFACE, POLAR_SURFACE(z, r, theta, /GRID) 	 ;Show it
;
; MODIFICATION HISTORY:
;	DMS 	Sept, 1992	Written
;-


IF keyword_set(grid) THEN BEGIN		;Regulary gridded?
	s = size(z)
	if s(0) ne 2 then message, "Z must be 2d if GRID is set"
	if n_elements(r) ne s(1) then $
		message, "R has wrong number of elements"
	if n_elements(theta) ne s(2) then $
		message, "Theta has wrong number of elements"
	x = r # cos(theta)
	y = r # sin(theta)
ENDIF ELSE BEGIN
	if (n_elements(r) ne n_elements(z)) or $
		(n_elements(theta) ne n_elements(z)) then $
		message, "R and Theta must have as many elements as Z"
	x = r * cos(theta)
	y = r * sin(theta)
ENDELSE

TRIANGULATE, x, y, tr		;Triangulate it
if n_elements(bounds) lt 4 then $
	bounds = [ min(x, max=x1), min(y, max=y1), x1, y1]
if n_elements(sp) lt 2 then $
	sp = [ bounds(2) - bounds(0), bounds(3) - bounds(1)] / 50.
if n_elements(missing) le 0 then missing = 0

RETURN, TRIGRID(x, y, z, tr, sp, bounds, QUINTIC = KEYWORD_SET(quintic), $
	MISSING = missing)
END

