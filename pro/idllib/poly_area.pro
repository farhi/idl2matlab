; $Id: poly_area.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Function Poly_area,x,y
;+
; NAME:
;	POLY_AREA
;
; PURPOSE:
;	Return the area of a polygon given the coordinates
;	of its vertices.
;
; CATEGORY:
;	Analytical Geometry
;
; CALLING SEQUENCE:
;	Result = POLY_AREA(X, Y)
;
; INPUTS:
;	It is assumed that the polygon has N vertices with N sides
;	and the edges connect the vertices in the order:
;
;	[(x1,y1), (x2,y2), ..., (xn,yn), (x1,y1)].
;
;	i.e. the last vertex is	connected to the first vertex.
;
;	X:	An N-element vector of X coordinate locations for the vertices.
;
;	Y:	An N-element vector of Y coordinate locations for the vertices.
;
; OUTPUTS:
;	POLY_AREA returns the area of the polygon.  This value is always 
;	positive.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	The area is computed as:
;		Area = 	1/2 * [ x1y2 + x2y3 + x3y4 +...+x(n-1)yn + xny1 
;			- y1x2 - y2x3 -...-y(n-1)xn - ynx1)
;
; MODIFICATION HISTORY:
;	DMS, July, 1984.
;-
on_error,2                      ;Return to caller if an error occurs
n = n_elements(x)
if (n le 2) then message, 'Not enough vertices'
if n ne n_elements(y) then message,'X and Y arrays must have same size'

xx=float(x)		;Be sure its floating
return,abs(total(xx*shift(y,-1) - y*shift(xx,-1))/2.) ;This is it.
end

