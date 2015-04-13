; $Id: sph_scat.pro,v 1.1 1995/02/02 21:33:26 dave Exp $
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

function sph_scat, lon, lat, f, GS = gs, BOUNDS = bounds, NLON=nlon, $
	NLAT = nlat, GOUT = gsout, BOUT = boundsout
;+
; NAME:
;	SPH_SCAT
;
; PURPOSE:
;	Interpolate to a regular grid given scattered samples on the
;	surface of a sphere.
; CATEGORY:
;	Interpolation.
; CALLING SEQUENCE:
;	Result = SPH_SCAT(lon, lat, f)
; INPUTS:
;	lon = sample longitudes, a vector, in degrees.  lon, lat, and 
;		f must have the same number of points.
;	lat = sample latitudes, a vector, in degreees.
;	f = data values measured at lon and lat.  f(i) = sample value
;		at lon(i), lat(i).  
; KEYWORD PARAMETERS:
;	GS:	  If present, GS must be a two-element vector [XS, YS],
;		  where XS is the spacing between grid points in longitude,
;		  and YS is the spacing in latitude. The default is based on
;		  the extents of lon and lat. If the grid starts at longitude
;		  Lonmin and ends at Lonmax, then the default horizontal
;		  spacing is (Lonmax - Lonmin)/(NX-1). YS is computed in the
;		  same way. The default grid size, if neither NX or NY
;		  are specified, is 26 by 26.
;	BOUNDS:   If present, BOUNDS must be a four element array containing
;		  the grid limits in longitude and latitude of the output grid:
;		  [Lonmin, Latmin, Lonmax, Latmax]. If not specified, the grid
;		  limits are set to the extent of lon and lat.   Warning:
;		  to cover all longitudes, you must directly specify BOUNDS.
;	NX:       The output grid size in the longitude direction. NX need not
;	  	  be specified if the size can be inferred from GS and
;		  BOUNDS. The default value is 26.
;	NY:       The output grid size in the latitude direction. See NX. 
;	BOUT:	  the actual extent of the regular grid, arranged as in
;		  bounds.  An optional output parameter.
;	GOUT:     The actual grid spacing, a two element optional output array.
;	
; OUTPUTS:
;	Result = regularly interpolated result.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Timing. on a Sun SPARCstation LX producing a 36 x 36 output
;	grid (1296 points), t is ~ .578 + .00368 * N + 2.39e-06 * N^2.
;	For example:  
;	N	16	64	256	1024	4096
;	Time	.7	.8	1.6	6.6	56
;	Output points are produced at a rate of approximately 2000
;	points per second.
;	
; PROCEDURE:
;	This routine is a convenience interface to the Spherical gridding
;	and interpolation provided by TRIANGULATE and TRIGRID.  The
;	methods are based on the work of Robert Renka, Interpolation of Data
;	on the Surface of a Sphere, Oak Ridge Natl Lab Technical Paper
;	CSD-108.  The procedure consists of generating a triangulation of the
;	scattered data points, estimating the gradients with a local method,
;	and then constructing a triangle based interpolant of the data and
;	gradient estimates.  The interpolant is C(1) continuous.
; EXAMPLE:
;	Create 50 random longitudes and latitudes, make a function value,
;	and then interpolate, obtaining a 360 x 360 array of
;	10 degree by 5 degree resolution that covers the sphere:
;
;	lon = randomu(seed, 50) * 360. -180.  ;Make random scattered points
;	lat = randomu(seed, 50) * 180. -90.
;	z = sin(lat*!DTOR)		;Make a function to fit
;	c = cos(lat*!DTOR)
;	x = cos(lon*!DTOR) * c
;	y = sin(lon*!DTOR) * c
;	f =  sin(x+y) * sin(x*z)	;The dependent variable
;  ** Now, given lon, lat, and f, interpolate the data:
;	result = sph_scat(lon, lat, f, bounds=[0, -90, 350, 85], gs=[10,5])
;	
; MODIFICATION HISTORY:
;	DMS, November, 1994.  Written.
;-

n = n_elements(lon)
if n ne n_elements(lat) or n ne n_elements(f) then $
	message, 'lon, lat, and f must have the same number of elements'
if n le 3 then $
	message, 'Must have at least 3 points'
;		Construct bounds if necessary
if n_elements(bounds) ne 4 then $
    boundsout = [ min(lon, max=lonmax), min(lat, max=latmax), lonmax, latmax] $
else boundsout = bounds
;		Get gs, nx, and ny.
if n_elements(gs) ne 2 then begin
    if n_elements(nx) le 0 then nx = 26
    if n_elements(ny) le 0 then ny = 26
    gsout = [boundsout(2)-boundsout(0), boundsout(3)-boundsout(1)] / $
		float([nx-1, ny-1])
endif else gsout = gs
fcopy = f		;will be rearranged.
TRIANGULATE, 1.0*lon, 1.0*lat, SPHERE=s, tr, FVALUE=fcopy, /DEGREES
return, TRIGRID(fcopy, SPHERE=s, gsout, boundsout, /DEGREES)
end
