; $Id: voronoi.pro,v 1.5 1995/03/01 23:59:02 dave Exp $

function isright, x0, y0, x1, y1, x2, y2 
; return 1 if Pnt0 is to right of Pnt1-> Pnt2
; return 0 if it is on the line.
; return -1 if Pnt0 is to the left of Pnt1 -> Pnt2

z = (x0-x1) * (y2-y1) - (y0-y1) * (x2-x1)
if z gt 0.0 then return, 1
if z lt 0.0 then return, -1
return, 0
end


PRO voronoi_get_intersect, rect, x0, y0, dx, dy, xi, yi, nedge
;  Return the closest intersection of the line thru (x0, y0), with
;	slope (dx / dy), and the rectangle rect.  The intersection must
;	be in the "positive" direction.
;	Set (xi, yi) to the intersection point, and 
;	nedge to the edge index.  Edge 0 is the bottom, going CCW.
;
nedge = -1
tmin = 0.

if dy ne 0.0 then begin
    t = (rect(1) - y0) / dy	;Bottom = edge 0?
    if t ge 0.0 then begin
	tmin = t
	nedge = 0
	yi = rect(1)		;Intersection with bottom
	xi = x0 + t * dx
	endif
    t = (rect(3) - y0) / dy	;Top edge = 2
    if t ge 0.0 then $
      if (nedge lt 0) or (t lt tmin) then begin
	tmin = t
	nedge = 2
	yi = rect(3)
	xi = x0 + t * dx
	endif
endif				;dy ne 0

if dx ne 0.0 then begin		;Check sides?
    t = (rect(0) - x0) / dx	;Left edge = 3
    if t ge 0.0 then $
       if (nedge lt 0) or (t lt tmin) then begin
	tmin = t
	nedge = 3
	xi = rect(0)
	yi = y0 + t * dy
	endif
    t = (rect(2) - x0) / dx	;Right edge = 1
    if t ge 0.0 then $
       if (nedge lt 0) or (t lt tmin) then begin
	tmin = t
	nedge = 1
	xi = rect(2)
	yi = y0 + t * dy
	endif
endif				;Dx ne 0
end




pro VORONOI_SHOW, n		;Illustrate using and drawing Voronoi polygons
; This procedure generates N random points (default = 12).
;	and then draws the voronoi polygons, with the points and
;	delaunay triangulation overlaid.

if n_elements(n) le 0 then n = 12	;Make the random points
seed = 1211567L
x = randomu(seed, n)
y = randomu(seed, n)

triangulate, x, y, tr, CONN=c		;Triangulate them
plot,x,y,/psym, xrange=[-.5,1.5], yrange=[-.5,1.5]
tek_color				;Discrete color tables.
range = 0				;Init bounding rectangle

for i=0, n-1 do begin		;Each VORONOI region for each point
	voronoi, x, y, i, c, xp, yp, range	  ;Get the ith polygon
	xp = xp > (-10) < 10		;Clip to reasonable space
	yp = yp > (-10) < 10
	polyfill, xp, yp, color = (i mod 13) + 2	;Show it
	endfor

oplot,x,y,/psym, color=1		;Show points & triangulation
for i=0, n_elements(tr)/3-1 do begin	;The triangles
	t = [tr(*,i),tr(0,i)]		;Subscripts of triangle & back to 0
	plots,x(t), y(t)
	endfor
end



PRO voronoi, x, y, i0, c, xp, yp, rect
; Copyright (c) 1992, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	VORONOI
;
; PURPOSE:
;	This procedure computes the Voronoi polygon of a point within
;	an irregular grid of points, given the Delaunay triangulation.
;	The Voronoi polygon of a point contains the region closer to
;	that point than to any other point.
;
; CATEGORY:
;	Gridding.
;
; CALLING SEQUENCE:
;	VORONOI, X, Y, I0, C, Xp, Yp, Rect
;
; INPUTS:
;	X:	An array containing the X locations of the points.
;	Y:	An array containing the Y locations of the points.
;	I0:	Index of the point of which to obtain the Voronoi polygon.
;	C:	A connectivity list from the Delaunay triangulation.
;		This list is produced with the CONNECTIVITY keyword
;		of the TRIANGULATE procedure.
;	Rect	the bounding rectangle:  [Xmin, Ymin, Xmax, Ymax].
;		Because the Voronoi polygon (VP) for points on the convex hull
;		extends to infinity, a clipping rectangle must be supplied to
;		close the polygon.  This rectangle has no effect on the VP of
;		interior points.  If this rectangle does not enclose all the
;		Voronoi vertices, the results will be incorrect.  If this
;		parameter, which must be a named variable, is undefined or
;		set to a scalar value, it will be calculated.
;
; OUTPUTS:
;	Xp, Yp:	The vertices of voroni polygon, VP.
;
; RESTRICTIONS:
;	The polygons only cover the convex hull of the set of points.
;
; PROCEDURE:
;	For interior points, the polygon is constructed by connecting
;	the midpoints of the lines connecting the point with its Delaunay
;	neighbors. Polygons are traversed in a counterclockwise direction.
;
;	For exterior points, the set described by the midpoints of the
;	connecting lines, plus the circumcenters of the two triangles
;	that connect the point to the two adjacent exterior points.
;
; EXAMPLE:
;	See the example procedure, VORONOI_SHOW, contained in this file.
;	To illustrate Voronoi polygons, after compiling this file (voronoi):
;		VORONOI_SHOW, Npoints  (try anywhere from 3 to 1000, default=12)
;
;	To draw the voroni polygons of each point of an irregular 
;	grid:
;	  x = randomu(seed, n)			 ;Random grid of N points
;	  y = randomu(seed, n)
;	  triangulate, x, y, tr, CONN=c			  ;Triangulate it
;	  rect = 0
;	  for i=0, n-1 do begin
;		voronoi, x, y, i, c, xp, yp, rect	  ;Get the ith polygon
;		polyfill, xp, yp, color = (i mod 10) + 2  ;Draw it
;		endfor
;
; MODIFICATION HISTORY:
;	DMS, RSI.	Dec, 1992. Original version.
;	DMS, RSI	Feb, 1995. Added bounding rectangle which simplified
;				   logic and better illustrated VPs for points
;				   on the convex hull.
;-
COMMON VORONOI_COMMON, first	;Only print warning once.

if n_params() lt 7 and n_elements(first) le 0 and !quiet eq 0 then begin
  Message, /INFO, 'New revision. For more efficient use, supply the Rect parameter'
  first = 1
  endif

p = c(c(i0):c(i0+1)-1)  	;Verts of polygon
np = n_elements(p)
xp = fltarr(np, /NOZERO)
yp = fltarr(np, /NOZERO)
ext = i0 eq p(0)		;True if exterior point

; Each vertex is simply the circumcenter of a Delaunay triangle
; 		containing the point in question.
for i=0, np-1 do begin		;Traverse adjacency list for point i0.
    m = p(i)
    j = p((i + 1) mod np)	;Successor
    cir_3pnt, x([m,j,i0]), y([m,j,i0]), r, x0, y0
    xp(i) = x0
    yp(i) = y0
    endfor

if ext eq 0 then return	;If interior point, we're all done....

;	*** Point is on the Convex Hull ****
if n_elements(rect) ne 4 then begin  ;Initialize bounding rect?
    i1 = i0			;Follow boundary CCW
    xmin = min(x, max=xmax)
    ymin = min(y, max=ymax)
    xr = (xmax-xmin)*.05	;Fudge factor of 5%
    yr = (ymax-ymin)*.05
    rect = [xmin-xr, ymin-yr, xmax+xr, ymax+yr]  ;Initial bound box
    repeat begin		;Get circumctr of each boundary triangle
	k = c(i1)
	m = c(k+1)
	j = c(k+2)
	cir_3pnt, x([m,j,i1]), y([m,j,i1]), r, x0, y0
	rect(0) = rect(0) < x0
	rect(2) = rect(2) > x0
	rect(1) = rect(1) < y0
	rect(3) = rect(3) > y0	;Keep extremes
	i1 = m			;Next boundary point
	endrep until i1 eq i0
    endif
    
;	Now get intersections of perpendicular  bisectors of the two edges
;	on the convex hull, with vertex point i0, with the bounding rectangle.
;
j = p(1)
voronoi_get_intersect, rect, xp(1), yp(1), y(j)-y(i0), x(i0)-x(j), x0, y0, edge0
xp(0) = x0
yp(0) = y0
j = p(np - 1)
voronoi_get_intersect, rect, xp(np-2), yp(np-2), y(i0)-y(j), x(j)-x(i0), $
	x0, y0, edge1
xp(np-1) = x0
yp(np-1) = y0

if (edge0 < edge1) lt 0 then begin   ;Either out of bounds?
    MESSAGE, /INFO, 'Bounding rectangle does not enclose Voronoi polygon'
    xp(0) = xp(1)		;Fudge polygon, its wrong anyway...
    yp(0) = yp(1)
    xp(np-1) = xp(np-2)
    yp(np-1) = yp(np-2)
    return
endif

while edge1 ne edge0 do begin		;Add corner(s) of bound rect if necess.
    edge1 = (edge1 + 1) mod 4		;Go CCW
    xp = [xp, rect(([0,2,2,0])(edge1))]
    yp = [yp, rect(([1,1,3,3])(edge1))]
    endwhile
return
end
