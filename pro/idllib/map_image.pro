; $Id: map_image.pro,v 1.8 1995/07/27 22:42:42 stevep Exp $

; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

function map_image_missing, image_orig, max_value, min_value
;
; Return an array of 1's where the data are outside the range of min_value
; to max_value.  Max_value and/or min_value may be undefined.  If both are
; undefined, return a -1.
;
k = n_elements(max_value) * 2 + n_elements(min_value)
case k of
0: return, -1
1: return, Image_orig le min_value
2: return, Image_orig ge max_value
3: return, (Image_orig ge max_value) or (Image_orig le min_value)
endcase
end


function  map_image, Image_Orig, startx, starty, xsize, ysize, $
                LATMIN = latmin,   LATMAX = latmax,  $
                LONMIN = lonmin,   LONMAX = lonmax,  $
                COMPRESS = compress, BILINEAR = bilin, $
		WHOLE_MAP = whole_map, SCALE = scalef, $
		MISSING = missing, MAX_VALUE = max_value, MIN_VALUE=min_value
;+NODOCUMENT
;NAME:
;     map_image
;PURPOSE:
;       This function returns the Image_Orig image warped to fit
;       the map specified by the system variable !Map. Image_Orig
;       must be centered at 0.
;Category:
;        Mapping
;Calling Sequence:
;        result = map_image(Image_Orig [, xstart, ystart [, xsize, ysize]])
;INPUT:
;      Image_Orig--- A two-dimensional array representing geographical
;               image to be overlayed on map.  It has Nx columns,
;		and Ny rows.
;KEYWORDS:
;	LATMIN --- the latitude corresponding to the first row of Image_Orig.
;		The default value is -90.  Latitude and Longitude values
;		refer to the CENTER of each cell.
;	LATMAX --- the latitude corresponding to last row of Image_Orig. The
;		default is  90 - (180. / Ny).
;	LONMIN --- the longitude corresponding to the first column of
;		Image_Orig. The default value is -180.
;	LONMAX --- the longitude corresponding to the last column
;		of Image_Orig. The default is  180. - (360./Nx).
;		For images crossing the international dateline, lonmax
;		will be less than lonmin.
;		If the longitude of the last column is equal to 
;		(lonmin - (360. /Nx)) MODULO 360.
;		it is assumed that the image covers all longitudes.
;	BILINEAR --- A flag, if set, to request bilinear interpolation. The
;		default is nearest neighbor.
;	COMPRESS --- Interpolation compression flag.  Setting this to
;		a higher number saves time --- lower numbers produce
;		more accurate results.  Setting this to 1
;		solves the inverse map transformation for every
;		pixel of the output image.  Default = 4 for output devices
;		with fixed pixel sizes. Fix is used to make this an int.
;	WHOLE_MAP = Set to defeat the code that determines the extent
;		of the image on the map.  Some projections are so
;		tricky (especially the satellite) that it is difficult
;		to find where the image goes on the map.  Setting this
;		keyword computes the entire map, at the cost of more
;		time.  For images which cover the entire map, setting
;		this keyword produces no penalty.
;	SCALE = pixel / graphics scale factor for devices with scalable
;		pixels (e.g. PostScript).  Default = 0.02 pixels/graphic_coord.
;		This yields an approximate output image size of 350 x 250.
;		Make this number larger for more resolution (and larger
;		PostScript files, and images), or smaller for faster
;		and smaller, less accurate images.
;	MISSING = value to set areas outside the valid map coordinates.
;		If omitted, areas outside the map are set to 255 (white) if
;		the current graphics device is PostScript, or 0 otherwise.
;	MAX_VALUE = values in Image_Orig greater than or equal to MAX_VALUE
;		are considered missing.  Pixels in the output image
;		that depend upon missing pixels will be set to MISSING.
;	MIN_VALUE = values in Image_Orig less than or equal to MIN_VALUE
;		are considered missing.
; Optional Output Parameters:
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
;Output:
;      The warped image is returned.
;
; Procedure:  An image space algorithm is used, so the time required
;	is roughly proportional to the size of the final image.
;
;MODIFICATION HISTORY:
;       CAB, Feb, 1992. Map_image has been changed to handle images
;       	crossing the international dateline in a more convenient way.
;       	Specifically, it no longer requires that the keyword LONMIN be
;       	greater than or equal to -180 or the keyword LONMAX be
;		less than or equal to 180.
;	DMS, Aug, 1992.  Completly rewritten.  Uses different algorithms.
;	DMS, Dec, 1992.  Coordinates were off by part of a pixel bin.
;		Also, round when not doing bi-linear interpolation.
;	DMS, Sep, 1993.  Added MAX_VALUE keyword.
;	DMS, Nov, 1994.  Added MIN_VALUE keyword.
;       SVP, Mar, 1995.  Compress is now fix()'d. y is now scaled correctly.
;-

ON_ERROR,2

;t0 = systime(1)
if (!x.type NE 2) THEN  $        ;Need Mapping Coordinates
   message, "Current window must have map coordinates"

s = size(Image_Orig)
if s(0) ne 2 THEN message, " Image must be a two- dimensional array."
s1 = s(1)           ; # of columns
s2 = s(2)           ; # of rows
if s(1) le 1 or s(2) le 1 THEN $
    message, 'Each dimension must be greater than 1."

; If both latmin & latmax are missing, assume image covers entire globe,
;       without duplication
if N_ELEMENTS(latmin) eq 0 then latmin = -90.
; if N_ELEMENTS(latmax) EQ 0 THEN latmax = 90. - 180./s2
if N_ELEMENTS(latmax) EQ 0 THEN latmax = 90.

; If both lonmin & lonmax are missing, assume image covers all longitudes
;       without duplication.
if N_ELEMENTS(lonmin) EQ 0 THEN lonmin = -180.
;if N_ELEMENTS(lonmax) EQ 0 THEN lonmax = lonmin - 360./s1 + 360.
if N_ELEMENTS(lonmax) EQ 0 THEN lonmax = 180.

latmin = float(latmin) 	&	lonmin = float(lonmin)
latmax = float(latmax) 	&	lonmax = float(lonmax)

;	Does image wrap around globe?
wrap = ((lonmin - 360./s1 + 720.) mod 360.) - ((lonmax + 720.) mod 360.)
wrap = abs(wrap) lt 1e-4	;Allow for roundoff

;
;       Map.out(2:3) contains   Longitude min, max of current projection
;				always in range of -180 to +180.
;       Map.out(4:5) contains   Latitude min, max

;Extents of projection on map
lnmin = !map.out(2)            & lnmax = !map.out(3)
ltmin = !map.out(4) 	       & ltmax = !map.out(5)

if ((ltmin eq -90) or (ltmax eq 90)) then begin     
      lnmin=-180 & lnmax= 180 
      wrap=1                                                  
endif       

                ;Intersection of two rectangles in lat/lon
ltlim = [ltmin > latmin, ltmax < latmax]

if wrap eq 0 then lnlim = [lnmin > lonmin, lnmax < lonmax] $
	else lnlim = [-180, 180]

; Find the extent of the our limits in the map on the screen by
;       making a n x n array of lon/lats spaced over the extent of
;       the image and saving the extrema.


scale = !d.flags and 1		;TRUE if device has scalable pixels
if n_elements(scalef) le 0 then scalef = 0.02   ;PostScript scale factor
IF scale THEN BEGIN		;Fudge for postscript?
	!x.s = !x.s * scalef
	!y.s = !y.s * scalef
ENDIF ELSE scalef = 1

if keyword_set(compress) then compress=fix(compress)
if n_elements(compress) le 0 then compress = 4  ;Default value

; Missing data value should equal the background or user-supplied value.
if n_elements(missing) le 0 then begin
  if (!d.flags and 512 ne 0) then missing = !d.n_colors-1 else missing = 0
  endif

xw = scalef * !x.window * !d.x_size         ;Map extent on screen
yw = scalef * !y.window * !d.y_size

IF KEYWORD_SET(whole_map) THEN BEGIN
	x = [0, (!d.x_size-1) * scalef]	;Use whole area
	y = [0, (!d.y_size-1) * scalef]
ENDIF ELSE BEGIN		;Try to figure it out 
  n = 31                        ;Subdivisions
  x = (findgen(n) * ((lnlim(1)-lnlim(0))/float(n-1)) + lnlim(0)) # replicate(1,n)
  y = replicate(1,n) # (findgen(n) * ((ltlim(1)-ltlim(0))/float(n-1)) + ltlim(0))
  x = convert_coord(x,y, /DATA, /TO_DEVICE)       ;Latlon to device
  
  y = reform(x(1,*), n^2, /OVER)          ;Get device coords separately
  x = reform(x(0,*), n^2, /OVER)
  good = where((x lt 1e10) and (y lt 1e10))
  x = x(good) & y = y(good)
ENDELSE

screen_x = fix([ xw(0) > min(x), xw(1) < max(x) ]);Screen extent of our image
screen_y = fix([ yw(0) > min(y), yw(1) < max(y) ])

;       Get next larger multiple of compress for resulting image size.
nx = (screen_x(1) - screen_x(0)+compress)/compress
ny = (screen_y(1) - screen_y(0)+compress)/compress
                        ;X and Y screen coordinates
x = (findgen(nx) * compress) # replicate(1.0,ny) + screen_x(0)
y = replicate(1.0, nx) # (findgen(ny) * compress) + screen_y(0)

x = convert_coord(x, y, /DEVICE, /TO_DATA)      ;Screen to lat/lon

y = reform(x(1,*), nx, ny, /OVER)       ;Separate lat/longit
x = reform(x(0,*), nx, ny, /OVER)

w = where(x lt lonmin, count)        ;Handle longitude wrap-around
while count gt 0 do begin
        x(w) = x(w) + 360.0
        w = where(x lt lonmin, count)
        endwhile


sx = ((s1-1.)/(lonmax - lonmin))  ;Scale from lat/lon to pixels
sy = ((s2-1.)/(latmax - latmin))


;               Now interpolate the screen image from the original.
if KEYWORD_SET(bilin) THEN BEGIN
; If the image wraps around the globe, we must treat those pixels
; after the last column and before the first column specially. 

    badb = map_image_missing(image_orig, max_value, min_value)
    x = (x - lonmin) * sx		;To pixels
    y = (y - latmin) * sy

    if wrap then begin
	col1 = where(x gt (s1-1), count)	;Wrap around elements
	if count le 0 then goto, full_image
	threecol = [Image_Orig(s1-1,*), Image_Orig(0:1,*)]
	col1x = x(col1) - (s1-1)	;Interpolate value
	if badb(0) ne -1 then begin	;Missing data value?
	    bad = interpolate(float(badb), x, y, miss=0)
	    bad(col1) = interpolate(float([badb(s1-1,*), badb(0:1,*)]), $
			col1x, y(col1), miss=0)
	    bad = where(bad, count)
	    x = interpolate(Image_Orig, x, y, miss = missing) ; full image
		;Add in points that wrapped in X between s1-1 and s1.
	    x(col1) = interpolate(threecol, col1x, y(col1), miss = missing)
	    if count gt 0 then x(bad) = missing
	endif else begin			;badb
	    x = interpolate(Image_Orig, x, y, miss = missing) ; full image
		;Add in points that wrapped in X between s1-1 and s1.
	    x(col1) = interpolate(threecol, col1x, y(col1), miss = missing)
	endelse
    endif else begin
  full_image:
	if badb(0) ne -1 then begin
	    bad = where(interpolate(float(badb), x, y, miss = 0), count)
	    x = interpolate(Image_Orig, x, y, miss = missing) ;No wrap
	    if count gt 0 then x(bad) = missing
	endif else x = interpolate(Image_Orig, x, y, miss = missing) ;No wrap
    endelse
ENDIF ELSE BEGIN
;  Scale to pixel coords & round.  
;  This is the same as: x = (x-lonmin) * sx + 0.5, but faster for arrays.
	x = (x - (lonmin - .5/sx)) * sx
	y = (y - (latmin - .5/sy)) * sy
	bad = where(x lt 0 or x ge s1 or y lt 0 or y ge s2, count)
	if count gt 0 then begin
	    x(bad) = 0
	    y(bad) = 0
	    x = Image_Orig(x, y)
	    badb = map_image_missing(x, max_value, min_value)
	    if badb(0) ne -1 then begin
		worse = where(badb, count)
		if count gt 0 then x(worse) = missing
		endif		;Max value
	    x(bad) = missing
	endif else begin
	    x = Image_Orig(x,y)
	    badb = map_image_missing(x, max_value, min_value)
	    if badb(0) ne -1 then begin
		bad = where(badb, count)
		if count gt 0 then x(bad) = missing
		endif		;Max value
	endelse			;Count
ENDELSE

startx = fix(screen_x(0) / scalef)
starty = fix(screen_y(0) / scalef)
xsize = fix(nx / scalef * compress)
ysize = fix(ny / scalef * compress)

if compress ne 1 then $		;Resample to screen?
   x = rebin(x, nx*compress, ny*compress)  ;Interpolate screen image if necess.
;print,systime(1)-t0,' seconds'

IF scale THEN BEGIN		;Unfudge scale factors
	!x.s = !x.s / scalef
	!y.s = !y.s / scalef
	ENDIF	
return, x
end

