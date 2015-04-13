; $Id: ll_arc_distance.pro,v 1.3 1994/06/10 21:07:53 dan Exp $

FUNCTION LL_ARC_DISTANCE, lon_lat0, arc_dist, az, DEGREES = degs
;+
; NAME:
;	LL_ARC_DISTANCE
;
; PURPOSE:
; 	This function returns the longitude and latitude [lon, lat] of
;	a point a given arc distance (-pi <= Arc_Dist <= pi), and azimuth (Az),
;	from a specified location Lon_lat0.
;
; CATEGORY:
;	Mapping, geography.
;
; CALLING SEQUENCE:
;	Result = LL_ARC_DISTANCE(Lon_lat0, Arc_Dist, Az)
;
; INPUTS:
;    	Lon_lat0: A 2-element vector containing the longitude and latitude
;		  of the starting point. Values are assumed to be in radians
;		  unless the keyword DEGREES is set.
;    	Arc_Dist: The arc distance from Lon_lat0. The value must be between
;		  -!PI and +!PI. To express distances in arc units, divide
;		  by the radius of the globe expressed in the original units.
;		  For example, if the radius of the earth is 6371 km, divide
;		  the distance in km by 6371 to obtain the arc distance.    
;    	Az:	  The azimuth from Lon_lat0. The value is assumed to be in
;		  radians unless the keyword DEGREES is set.
;
; KEYWORD PARAMETERS:
;    	DEGREES:  Set this keyword to express all measurements and
;		  results in degrees.
;
; OUTPUTS:
;	This function returns a two-element vector containing the
;	longitude and latitude of the resulting point. Values are
;	in radians unless the keyword DEGREES is set.
;
; PROCEDURE:
;	Formula from Map Projections - a working manual.  USGS paper
;	1395.  Equations (5-5) and (5-6).
;
; EXAMPLE:
;	Lon_lat0 = [1.0, 2.0]		; Initial point specified in radians	
;	Arc_Dist = 2.0			; Arc distance in radians
;	Az = 1.0			; Azimuth in radians
;	Result = LL_ARC_DISTANCE(Lon_lat0, Arc_Dist, Az)
;	PRINT, Result
;     	  2.91415    -0.622234
;
; MODIFICATION HISTORY:
;	DMS, Aug, 1992.  Written.
;       DJC, Jun, 1994.  Added test for zero arc distance.
;                        Renamed "dist" variable to "arc_dist" for
;                        compatibility with IDL "Dist" function.
;-

; Return the [lon, lat] of the point a given arc distance 
;	(-pi <= arc_dist <= pi),
; and azimuth (az), from lon_lat0.
;

if (arc_dist eq 0) then return, lon_lat0

cdist = cos(arc_dist)		;Arc_Dist is always in radians.
sdist = sin(arc_dist)

if keyword_set(degs) then s = !dtor else s = 1.0

ll = lon_lat0 * s	;To radians
sinll1 = sin(ll(1))
cosll1 = cos(ll(1))
phi = asin(sinll1 * cdist + cosll1 * sdist * cos(az * s))
lam = ll(0) + atan(sdist * sin(az * s), $
	cosll1*cdist - sinll1 * sdist * cos(az * s))
while lam lt -!pi do lam = lam + 2 * !pi
while lam gt !pi do lam = lam - 2 * !pi
return, [lam, phi] / s
end



