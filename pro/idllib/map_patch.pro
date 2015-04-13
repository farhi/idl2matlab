; $Id: map_patch.pro,v 1.2 1995/06/30 15:22:19 dave Exp $
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

function reduce_360, a		;Reduce an angle to the range of +- 180.
b = a
r = (b+180.)/360.
t = where(r lt 0.0, count)
if count gt 0 then b(t) = b(t) + ceil(-r(t))*360
t = where(r gt 1.0, count)
if count gt 0 then b(t) = b(t) - floor(r(t))*360
return, b
end


FUNCTION  map_patch, Image_Orig, Lons, Lats, $
		XSTART = xstart, YSTART = ystart, $
		XSIZE = xsize, YSIZE = ysize, $
		LON0 = lon0, LON1 = lon1, $
		LAT0 = lat0, LAT1 = lat1, $
		MISSING = missing, MAX_VALUE = max_value, $
		TRIANGULATE=triangulate, DEBUG = debug
;+
;NAME:
;     map_patch
;PURPOSE:
;	This function returns an interpolated grid in device space,
;	given a set of rectangularly gridded values, in latitude/longitude 
;	space on the globe.
;Category:
;        Mapping
;Calling Sequence:
;        result = map_patch(Image_Orig [, Lons] [, Lats])
;INPUT:
;      Image_Orig- A two-dimensional array containing the data
;               to be overlayed on map.  It has Nx columns,
;		and Ny rows.  The cell connectivity must be rectangular.
;		Computations are performed in floating point.
;		Rows and columns must arranged in increasing longitude
;		and latitude order.
;	Lons-	An Nx element vector containing the longitude
;		of each column.  lon(image_orig(i,j)) = Lons(i).
;		This optional parameter may be omitted if the
;		longitudes are equally spaced and are
;		specified with the LON0 and LON1 keywords.
;	Lats-	An Ny element vector containing the latitude for each
;		row. lat(image_orig(i,j)) = lats(j)  This optional
;		parameter may be omitted if the latitudes are equally
;		spaced and are specified with the LAT0 and LAT1 keywords.
;KEYWORDS:
;	LAT0-	The latitude of the first column of data.  Default=-90.
;	LAT1-	The latitude of the last column of data.  Default=+90.
;	LON0-	The longitude of the first row of data.  Default=-180.
;	LON1-	The longitude of the last row of data.  Default=180-360/Ny.
;	MISSING = value to set areas outside the valid map coordinates.
;		If omitted, areas outside the map are set to 255 (white) if
;		the current graphics device is PostScript, or 0 otherwise.
;	MAX_VALUE = values in Image_Orig greater than MAX_VALUE
;		are considered missing.  Pixels in the output image
;		that depend upon missing pixels will be set to MISSING.
;	TRIANGULATE = if set, the points are converted to device space
;		and then triangulated.  Specify this keyword if the connectivity
;		of the points is not rectangular AND monotonic in device space.
; Optional Output Keywords:
;	xstart --- the  x coordinate where the left edge of the image
;		should be placed on the screen.
;	ystart --- the y coordinate where th bottom edge of the image
;		should be placed on the screen.
;	xsize ---  returns the width of the resulting image expressed in
;		graphic coordinate units.  If current graphics device has
;		scalable pixels,  the value of XSIZE and YSIZE should
;		be passed to the TV procedure.
;	ysize ---  returns the pixel height of the resulting image, if the
;		current graphics device has scalable pixels. 
;
; Restrictions:
;    The four corner points of each cell must be contiguous.  This
;	means that the seam of a map must lie on a cell boundary,
;	not in its interior, splitting the cell.
;	
; Output:
;      The warped image is returned.
;
; Procedure:  An image space algorithm is used, so the time required
;	is roughly proportional to the size in pixels of the final image.
;	This routine often produces better results than MAP_IMAGE, because
;	it works in image (graphic) space, rather than in object (data)
;	space.  Each rectangular cell is divided by a diagonal,
;	into two triangles.  The trianges are then interpolated into
;	the image array using TRIGRID.
;
;MODIFICATION HISTORY:
;	DMS of RSI, July, 1994.		Written.
;-

ON_ERROR,2

; t0 = systime(1)
if (!x.type NE 2) THEN  $        ;Need Mapping Coordinates
   message, "Current window must have map coordinates"

s = size(Image_Orig)
if s(0) ne 2 THEN message, " Image must be a two- dimensional array."
Nx = s(1)           ; # of columns
Ny = s(2)           ; # of rows
Nx1 = Nx-1

n = N_elements(Image_orig)
if Nx le 1 or Ny le 1 THEN $
    message, 'Each dimension must be greater than 1."

if n_elements(lons) le 0 then begin	;Make longitudes?
    if n_elements(lon0) le 0 then lon0 = -180.
    if n_elements(lon1) le 0 then lon1 = lon0 - 360./nx + 360.
    dx = lon1-lon0
    if dx le 0 then dx = dx + 360.
    lons = findgen(nx) * (dx/(nx-1.)) + lon0
    endif

if n_elements(lats) le 0 then begin	;Make lats?
    if n_elements(lat0) le 0 then lat0 = -90.
    if n_elements(lat1) le 0 then lat1 = 90.
    lats = findgen(ny) * ((lat1-lat0)/(ny-1.)) + lat0
    endif

if n_elements(lats) ne ny then message, "Lats has incorrect size"
if n_elements(lons) ne nx then message, "Lons has incorrect size"

mlonmin = !map.out(2)		;Extent of map limits
mlonmax = !map.out(3)
mlatmin = !map.out(4)
mlatmax = !map.out(5)
iproj = !map.projection

;	stereographic orthog	    lambert eq area
hemis = iproj eq 1 or iproj eq 2 or iproj eq 4   ;TRUE if hemispheric map

equatorial = !map.p0lat eq 0.0		;True if cen on equator
polar = abs(!map.p0lat) eq 90.		;True if cen on pole
;	True if data covers all longitudes
wrap = abs(reduce_360(lons(nx-1) + 360./nx)-lons(0)) lt 1e-4

ll = reduce_360(lons - mlonmin)
junk = ll ge 0			;Look for 0 to 1 transition...
junk = junk - shift(junk,1)
junk = where(junk eq 1, count)
ixstart = junk(0)

junk = reduce_360(mlonmax-lons) gt 0
junk = junk - shift(junk,1)
junk = where(junk eq 255b, count)
ixend = junk(0)
if lons(ixend) gt mlonmax then ixend = (ixend+nx-1) mod nx
;print, 'lonmin, lonmax, ixstart, ixend = ', mlonmin, mlonmax, ixstart, ixend

;		Default ranges.
if  ixend le ixstart then nxx = ixend-ixstart + nx + 1 $
else nxx = ixend-ixstart+1
goodx = (lindgen(nxx) + ixstart) mod nx
if wrap and (not (hemis and equatorial)) and $
	reduce_360(mlonmax - mlonmin) eq 0.0 and $
	(goodx(0) ne goodx(nxx-1))then goodx = [goodx, goodx(0)]
goody = lindgen(ny)

if polar then goody = where(lats ge mlatmin and lats le mlatmax)
y = lons(goodx)

; This kludge takes care of the left edge being mapped on the right edge
if y(0) ge mlonmax then y(0) = reduce_360(y(0)+.0001)

nx1 = n_elements(goodx)
ny1 = n_elements(goody)
lo = y # replicate(1.0, ny1)
la = replicate(1.0, nx1) # lats(goody)

; Image elements we want
image1 = image_orig(goodx # replicate(1L,ny1) + replicate(nx,nx1) # goody)

scale = !d.flags and 1		;TRUE if device has scalable pixels

IF scale THEN BEGIN		;Fudge for postscript?
	scalef = 0.02		;PostScript scale factor
	scale_orig = [!x.s, !y.s]
	!x.s = !x.s * scalef
	!y.s = !y.s * scalef
ENDIF ELSE scalef = 1

;***** print,systime(1)-t0, ' seconds (before coord convers)'
		;Get XY device coords of data points
y = (convert_coord(lo, la, /data, /to_device))(0:1,*)

if scale then begin		;Restore scale
	!x.s = scale_orig(0:1) & !y.s = scale_orig(2:3)
	endif

x = reform(y(0,*), nx1, ny1)		;Back to nx by ny
y = reform(y(1,*), nx1, ny1)
;print, systime(1)-t0, ' seconds (after coord convers)'

xlim = !d.x_size * !x.window * scalef
ylim = !d.y_size * !y.window * scalef

good = (x gt xlim(0)) and (x lt xlim(1)) and (y gt ylim(0)) and (y le ylim(1))
if n_elements(max_value) gt 0 then good = good and (image1 le MAX_VALUE)
if n_elements(missing) le 0 then missing = 0
bad = where(good eq 0b, count)
;   print, count, ' bad elements'
good = where(good)

if keyword_set(triangulate) then begin  ;Make our own triangulation?
    if count gt 0 then begin
	x = x(good)
	y = y(good)
	image1=image1(good)
	count = 0
	endif
    triangulate, x, y, triangles
endif else begin		;Make a regular triangular grid
    i = (nx1-1) * 6			;Vertices / row
    t = lonarr(i, /NOZERO)
    triangles = lonarr(i, ny1-1,/NOZERO)
    ; Create 1st row of indices of triangular patches
    for i=0, nx1-2 do t(i*6) = [i,i+1,i,i,i+1,i+1] + [0,nx1,nx1,0,nx1,0]
    for iy=0L, ny1-2 do triangles(0,iy) = t + iy * nx1
      ;**** print,systime(1)-t0, ' seconds (Triangles)'
endelse

; Show grid structure.
if keyword_set(debug) then for i=0L, n_elements(triangles)/3-1 do begin
    t = triangles(i*3+[0,1,2,0])
    plots,x(t),y(t), /dev
    endfor

if count gt 0 then begin
	xstart = floor(min(x(good), max=maxx))
	ystart = floor(min(y(good), max=maxy))
	rect = [xstart, ystart, ceil(maxx), ceil(maxy)]
	good = float(image1)
	good(bad) = 1.0e12
	good = trigrid(x,y,good, triangles, [1.,1.], rect, MAX_VALUE=1.0e10, $
		MISSING = missing)
endif else begin
	xstart = floor(min(x, max=maxx))
	ystart = floor(min(y, max=maxy))
	rect = [xstart, ystart, ceil(maxx), ceil(maxy)]
	good = trigrid(x,y,image1, triangles, [1.,1.], rect, MISSING = missing)
endelse

xsize = ceil((rect(2) - rect(0))/scalef)
ysize = ceil((rect(3) - rect(1))/scalef)

xstart = long(xstart / scalef)
ystart = long(ystart / scalef)

;print,systime(1)-t0, ' seconds'
return, good
end
