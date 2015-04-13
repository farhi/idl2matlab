; $Id: shade_surf_irr.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro SHADE_SURF_IRR, z, x, y, AX = ax, AZ = az, SHADES = shades, $
	PLIST = plist,  IMAGE = Image
;+
; NAME:
;	SHADE_SURF_IRR
;
; PURPOSE:
;	Make a shaded surface representation of an irregulary gridded
;	elevation dataset.
;
;	The data must be representable as an array of quadrilaterals.  This 
;	routine should be used when the (X, Y, Z) arrays are too irregular to 
;	be drawn by SHADE_SURF, but are still semi-regular.
;
; CATEGORY:
;	Graphics, surface plotting.
;
; CALLING SEQUENCE:
;	SHADE_SURF_IRR, Z, X, Y
;
; INPUTS:
;	Z:	A 2D array of elevations.  This array must be dimensioned 
;		as (NX, NY).
;
;	X:	A 2D array containing the X location of each Z value.  This
;		array must be dimensioned as (NX, NY).
;
;	Y:	A 2D array containing the Y location of each Z value.  This
;		array must be dimensioned as (NX, NY).
;
; KEYWORD PARAMETERS:
;	AX:	The angle of rotation about the X axis.  The default is 30
;		degrees.  This parameter is passed to SURFR.
;
;	AZ:	The angle of rotation about the Z axis.  The default is 30
;		degrees.  This parameter is passed to SURFR.
;
;	IMAGE:	Set this keyword to an array that will contain the resulting
;		shaded surface image.  The variable is returned as a byte 
;		array of the same size as the currently selected graphics 
;		device.
;
;	PLIST:	Set this keyword to an array that will contain the polygon
;		list on return.  This feature is useful when you want to make a
;		number of images from the same set of vertices and polygons.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The currently selected display is modified.
;
; RESTRICTIONS:
;	The grid described by X and Y must consist of quadrilaterals,
;	must be semi-regular, and must be in "CLOCKWISE" order:
;	i.e., each cell must be defined by the vertices:
;
;		v(i,j), v(i+1,j),v(i+1,j+1), and v(i,j+1).
;
;	Clockwise ordering:
;
;		x(i,j) <= x(i+1, j) ... for all j
;	and	y(i,j) <= y(i, j+1) ... for all i.
;
;	WARNING:  This restriction is not checked.
;
; PROCEDURE:
;	First, SURFR is called to establish the 3D to 2D transformation.
;	Then the vertex and polygon data structures required by the
;	POLYSHADE function are built and passed that function.  POLYSHADE
;	returns the shaded image which is then displayed by TV.
;
;	This simple procedure can be modified to use and/or accept
;	additional keywords.
;
; MODIFICATION HISTORY:
;	Oct, 1989, DMS.	
;	DMS, 	Modified to use SURFR instead of SURFACE. and to return the
;		polygon list.
;-

on_error,2                      ;Return to caller if an error occurs
s = size(z)
if s(0) ne 2 then begin
	print,'Shade - Not 2d'
	return
	endif

nx = s(1)			; # of columns
ny = s(2)			; # of rows

if n_elements(ax) eq 0 then ax = 30.	;Default rotations
if n_elements(az) eq 0 then az = 30.
;
;	Establish axis scaling:
;
scale = 0.9			;The fraction of the screen we use.
minx = min(x, max = maxx)
miny = min(y, max = maxy)
minz = min(z, max = maxz)

sx = scale / (maxx - minx)	;Scale factors
sy = scale / (maxy - miny)
sz = scale / (maxz - minz)

!x.s = [(1.-scale)/2. - minx * sx, sx]  ;Set axis scalings
!y.s = [(1.-scale)/2. - miny * sy, sy]
!z.s = [(1.-scale)/2. - minz * sz, sz]

surfr, ax = ax, az = az  ;establish rotation

plist = intarr(5,nx-1,ny-1)	;Make polygons

for i=0,nx-2 do for j=0,ny-2 do begin
	ll = i + j*nx		;Vertex index
	plist(0,i,j) = [ 4, ll, ll+1, ll+nx+1, ll+nx]
	endfor
if n_elements(shades) eq 0 then $
  image = polyshade(x,y,z,plist,/t3d,/data) $
 else $
  image = polyshade(x,y,z,plist,/t3d,/data, shades = shades)

tv, image
end
