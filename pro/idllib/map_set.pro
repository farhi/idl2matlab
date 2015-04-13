; $Id: map_set.pro,v 1.33 1995/06/28 19:19:27 dave Exp $
; Copyright (c) 1993-1994, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

FUNCTION map_uv_bounds, iproj
;
; Return the UV limits of the map, [umin, vmin, umax, vmax],
;	for the given projection.

  st = !map.sat(0) > 1.0
  st = sqrt((st-1.)/(st+1.))	;Map limit radius for satellite projection
  s2 = sqrt(2.0)
				;mercator limit in V @ map limit
  mvl = alog(tan((!map.out(5) < 89.9 > (-89.9)+ 90.)*!dtor/2.))
  mvll= alog(tan((!map.out(4) < 89.9 > (-89.9)+ 90.)*!dtor/2.))
;		This table contains the UV limits for each projection....
;         stereo(1-0)     ortho(2-1)	   conic(3-2)
;	    lamb(4-3)       gnomic(5-4)    azimuthal(6-5)
  bounds = [[-1.,-1.,1.,1.],    [-1.,-1.,1.,1.], [-180.,-1.,180.,1.], $
	    [-s2,-s2,s2,s2],[-2.,-2.,2.,2.], [-!pi,-!pi,!pi,!pi]]
;
;	    satell(7-6)   cylindrical(8-7) mercator(9-8)
;	    mollweide(10-9)  Sinusoidal(14-10)	   Aitoff(15-11)
  bounds = [[bounds],  $
           [-st,-st,st,st],[-180.,-90.,180.,90.],    [-!pi,mvll,!pi,mvl], $
           [-2.,-1.,2.,1.], [-!pi,-!pi/2.,!pi,!pi/2.], [-2.*s2,-s2,2.*s2,s2]]

;
;   Simple projections???
;
  k = iproj
  if iproj eq 18 or iproj eq 11 or iproj eq 12 or iproj eq 13 then k = iproj-3
  if k eq 15 then k = 11 $       ;Special case  Aitoff
  else if k eq 14 then k = 10 $  ;Sinusoidal
  else k = k - 1
  plimit = bounds(*,k)
;
;	SPECIAL CASE for tilted satellite proj:
;
  if iproj eq 7 and !map.sat(2) ne 0 then begin
    map_satellite_limit, 30, xr, yr
    plimit = [min(xr, max=xrmax), min(yr, max=yrmax), xrmax, yrmax]
    endif

return, plimit
END

pro map_satellite_limit, n, xr, yr
; Return n or less points, defining the limit of the limb of a satellite
; projection
    a = findgen(n) * (2 * !pi / (n-1))
    st = !map.sat(0) > 1.0
    st = sqrt((st-1.)/(st+1.))	;Map limit radius for satellite projection
    xr = cos(a) * st		;Taken from Snyder Pg. 175
    yr = sin(a) * st
    t = yr * !map.sat(5) + xr * !map.sat(4)
    a = t * !map.sat(2)/(!map.sat(0)-1.0) + !map.sat(3)
    good = where(a gt 0.0, count)
    if count lt n then begin	;Extract & shift so pnts on map are contiguous
	i0 = 0			;Get 1st good pnt following a bad pnt
	for i=1,count-1 do if good(i-1)+1 ne good(i) then i0 = i
	good = shift(good,-i0)
    endif
    t = t(good) & a = a(good)
    xr = (xr(good) * !map.sat(5) - yr(good) * !map.sat(4))*!map.sat(3)/a
    yr = t/a
end


pro map_horizon, FILL=fill, _EXTRA=Extra ;Draw/fill the horizon (limb) of a map

proj = !map.projection
if proj eq 5 or proj eq 3 then return	;Cant do gnomic or conic

p = map_uv_bounds(proj)
n = 360		;# of vertices
a = findgen(n+1) * (2 * !pi / n)

;		Projections that map to a circle or ellipse
;          0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
circle = [ 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1]

if proj eq 7 and !map.sat(2) ne 0 then begin	;Satellite projection, tilted
    map_satellite_limit, 360, xr, yr
endif else if circle(proj) then begin		;Std circular projection
    xr = (p(2)-p(0))/2. * cos(a)
    yr = (p(3)-p(1))/2. * sin(a)
endif else if proj eq 14 then begin		;Sinusoidal
    flon = (!map.out(3)-!map.out(2))/360.
    xr = flon * !pi * cos(a)
    yr = fltarr(n+1)
    for i=0,n do case fix(a(i)/(!pi/2)) of
0:	yr(i) = a(i)
1:	yr(i) = !pi-a(i)
2:	yr(i) = !pi-a(i)
3:	yr(i) = a(i)-2*!pi
4:	yr(i) = 0.0		;Last pnt
	endcase
endif else begin		;Rectangular

  dx0= p(0)+findgen(n/4)*(p(2)-p(0))/(n/4.)
  dx1= replicate(p(2),(n/4))
  dx2= p(2)+findgen(n/4)*(p(0)-p(2))/(n/4.)
  dx3= replicate(p(0),(n/4))
  xr= [dx0,dx1,dx2,dx3,dx0(0)]

  dy0= replicate(p(1),(n/4))
  dy1= p(1)+findgen(n/4)*(p(3)-p(1))/(n/4.)
  dy2= replicate(p(3),(n/4))
  dy3= p(3)+findgen(n/4)*(p(1)-p(3))/(n/4.)
  yr= [dy0,dy1,dy2,dy3,dy0(0)]

;  xr = [p(0), p(2), p(2), p(0), p(0)]
;  yr = [p(1), p(1), p(3), p(3), p(1)]
endelse

!x.type=0			;Plot in UV space
if keyword_set(fill) then $
   polyfill, xr, yr, _EXTRA=Extra $
else plots, xr, yr, _EXTRA=Extra
!x.type=2
end

pro mapp_set, latmin, latmax, lonmin, lonmax, p0lon, p0lat, rot, $
	iproj, limit, t3d=t3d, zvalue=zvalue, $
	border = border, title = title, sat_p = sat_p,    $
	xmargin = xmargin, ymargin = ymargin, $
	position=position, color=color, isotropic = isotropic

!x.s = [0,1]		;normal coverage for uv plane
!y.s = [0,1]

if (n_elements(zvalue) eq 0) then zvalue = 0
if (n_elements(color)  eq 0) then $
    if (!d.flags and 512) ne 0 then color = 0 else color = !d.n_colors-1

dtor = acos(0.0d0)/90.			;Need a double precision !DTOR

if iproj eq 3 then begin		;Conic?
	!map.phioc = rot		;Parameter order is diff
	!map.p0lat = (p0lat + p0lon)/2.	;Center latitude
	if !map.p0lat lt 0.0 then !map.sino = -1 else !map.sino = 1
	chi1 = (90. - !map.sino * p0lat) * dtor
	if p0lat eq p0lon then !map.coso = cos(chi1) $
	else begin
	    chi2 = (90. - !map.sino * p0lon) * dtor
	    !map.coso = alog(sin(chi1)/sin(chi2))/ $
			alog(tan(0.5*chi1)/tan(0.5*chi2))
	    endelse
        !map.out(8)=p0lat * dtor	;Ucen
        !map.out(9)=p0lon * dtor	;Vcen
	goto, get_bounds
endif

;		Not conic, all other projections

x1 = rot   * dtor		; Need the extra precision -KDB 10/93
x2 = p0lat * dtor

if (iproj ge 8) then  BEGIN
 !map.out(8) = x1		;ucen
 !map.out(11) = x2		;vrng
ENDIF ELSE !map.out(8)=x2

!map.out(9) = p0lon * dtor	;vcen
sinr = sin(x1)			;Stash cos / sin of map center lat and rotation

;		Try to avoid round-off errors, test for special cases
if abs(rot) eq 90.0 then cosr = 0.0 else cosr = cos(x1)
if (p0lat mod 180.) eq 0.0 then sino = 0.0 else sino = sin(x2)
if abs(p0lat) eq 90.0 then coso = 0.0 else coso = cos(x2)
del1 = .0001
!map.sat = 0.0		;Clear satellite proj params

if iproj ge 8 and iproj ne 14 then begin   ;Cylindrical projections
  if (abs(p0lat) ge del1) or $	;Simple case?
	((abs(rot) ge del1) and (abs(abs(rot)-180.) ge del1))  THEN BEGIN
	sint = coso * cosr	;NO...  complicated case
	cost = sqrt(1.-sint*sint)
	tmp1 =  sinr / cost
	tmp2 =  sino / cost
	!map.phioc = p0lon - atan(tmp1, - cosr * tmp2) * !radeg
	sinr = tmp1 * coso
	cosr = -tmp2
	sino = sint
	coso = cost
  endif else begin		;Simple cases
	if abs(rot) lt del1 then sino = 1.0 else begin 
	   sino = -1.
	   !map.phioc = p0lon + 180.
	   endelse
	coso = 0.0
	sinr = 0.0
	cosr = 1.0
	!map.projection = iproj+3	;Make a simple projection
  endelse
endif			;Cylindrical projections

if iproj eq 7 then begin	;Special params for satellite projection.
	!map.sat(0) = sat_p(0)	;Salt = sat_p(0)
	    ;Save em.  sat(1) = TRUE for simple case (Vertical perspective)
	!map.sat(1) = (sat_p(1) eq 0.0) and (sat_p(2) eq 0.0)
	!map.sat(2) = sin(dtor * sat_p(1))	;Salpha = sat_p(1)
	!map.sat(3) = cos(dtor * sat_p(1))	;calpha
	!map.sat(4) = sin(dtor * sat_p(2))	;Sbeta = sat_p(2)
	!map.sat(5) = cos(dtor * sat_p(2))	;cbeta
	endif	

!map.coso =  coso
!map.sinr = sinr
!map.sino = sino
!map.cosr = cosr
if (iproj eq 2) then !map.sat(0) =0.


GET_BOUNDS:
IF n_elements(limit) lt 4 THEN BEGIN	;Explicit limits?
  plimit = map_uv_bounds(iproj)
  umin = plimit(0) & umax = plimit(2)
  vmin = plimit(1) & vmax = plimit(3)
ENDIF ELSE IF n_elements(limit) eq 4 and abs(p0lat) eq 90 and $
   (limit(3)-limit(1) ge 180.) then begin  ;Polar?
				;Special case for polar projections
  p0 = convert_coord(limit(1), p0lat, /to_norm, /data)  ;Center
  umin = p0(0) & umax=umin
  vmin = p0(1) & vmax = vmin
  if p0lat eq 90 then lat = limit(0) else lat = limit(2)  ;Other latitude
  del = (lonmax-lonmin)/16. < 15.0
  for lon=float(lonmin), lonmax, del do begin
	p0 = convert_coord(lon, lat, /to_norm, /data) ;Edges
	if max(!map.out(0:1)) ge 1e3 then message, $
		'Map_set, limit point not mappable, lat,lon='+ $
		strtrim(lat,2) + ',' + strtrim(lon,2)
	umin = umin < p0(0) & umax = umax > p0(0)
	vmin = vmin < p0(1) & vmax = vmax > p0(1)
	endfor  
ENDIF ELSE BEGIN  			;4 or 8 element limit specified

;	8 point limit:
; limit =  [ lat0, lon0, lat1, lon1, lat2, lon2, lat3, lon3] specify
; four points on the map which give, respectively,
; the leftmost, topmost, rightmost, and bottom
; most points to be shown.

; 	4 point limit:
; limit = [latmin, lonmin, latmax, lonmax]
;  Get the rectangular coordinates of the limit.
	if n_elements(limit) eq 8 then begin
	    x = convert_coord(limit(1), limit(0), /TO_NORM, /DATA)
	    umin = !map.out(0)		;Left
	    x = convert_coord(limit(3), limit(2), /TO_NORM, /DATA)
	    vmax = !map.out(1)		;top
	    x = convert_coord(limit(5), limit(4), /TO_NORM, /DATA)
	    umax = !map.out(0)		;Right
	    x = convert_coord(limit(7), limit(6), /TO_NORM, /DATA)
	    vmin = !map.out(1)		;bottom
	    if (max([umin, vmax, umax, vmin]) gt 1.0e3) then $
		message, 'Map_set, limit point not mappable.', /CONTINUE
	endif else begin		;4 Point limit
;		Cover the four corners + the midpoints.
	    lat0 = (limit(0)+limit(2))/2.  ;Midpoints
	    lon0 = (limit(1)+limit(3))/2.
	    lats = [ limit([0,0,0]), lat0, limit([2,2,2]),lat0]
	    lons = [ limit(1), lon0, limit([3,3,3]), lon0, limit([1,1])]
	    umin=1.0e12
	    umax=-1.0e12
	    vmin = umin
	    vmax = umax
	    for i=0,7 do begin		;Use 4 corners + midpoints
		x = convert_coord(lons(i), lats(i), /TO_NORM, /DATA)
;********* print, lons(i), lats(i), !map.out(0:1)
		if max(!map.out(0:1)) ge 1e3 then $
		    message, 'Map_set, limit point not mappable, lat,lon='+ $
		      strtrim(lats(i),2) + ',' + strtrim(lons(i),2), /CONTINUE $
		else begin
		  umin = !map.out(0) < umin
		  umax = !map.out(0) > umax
		  vmin = !map.out(1) < vmin
		  vmax = !map.out(1) > vmax
		endelse
	        endfor
	endelse			;4 point limit
ENDELSE

if iproj eq 3 then  BEGIN		;Set tolerance for detecting wraps.
   ueps = 135.				;Conic?
   umin = umin / 180.			;Scale to units of pi
   umax = umax / 180.
endif else ueps = 0.75 * ABS(umax-umin)
veps = 0.75 * ABS(vmax-vmin)
plimit = [umin, vmin, umax, vmax]

!map.out(6) = ueps
!map.out(7) = veps

if n_elements(xmargin) ne 2 THEN xmargin = [1,1]
if n_elements(ymargin) ne 2 THEN ymargin = [1,2]
if not N_Elements(title) THEN title =" "

fudge = 0.01			;was 0.01
eps = (umax - umin) * fudge		;Extend the axes by a Fudge factor
umax = umax + eps
umin = umin - eps
eps = (vmax - vmin) * fudge
vmax = vmax + eps
vmin = vmin - eps

; print, umin, vmin, umax, vmax
;
; This establishes the plot scaling between uv coordinates and the 
;  plot range.

if keyword_set(position) then  $
    plot, [umin,umax], [vmin,vmax], xsty=5, ysty=5, /nodata,    $
	title=title, /noerase, position=position $
else begin			;Position not set
    plot, [umin,umax], [vmin,vmax], xsty=5, ysty=5, /nodata,    $
         xmargin= xmargin, ymargin= ymargin, title=title, /noerase
endelse

if keyword_set(isotropic) then begin
    aspect = !d.x_size*(!x.window(1)-!x.window(0))/(plimit(2)-plimit(0))/$
	(!d.y_size * (!y.window(1)-!y.window(0))/(plimit(3)-plimit(1)))
    if aspect gt 1 then begin	;Fudge to make isotropic
	   !x.s(1) = !x.s(1)/aspect	;Make narrower
	   t = .5*(1.-1./aspect) * (!x.window(1)-!x.window(0))	;Width
	   !x.window = !x.window + [t,-t]
	   !x.s(0) = !x.window(0) - umin * !x.s(1)
	   !p.clip([0,2]) = !x.window * !d.x_size
	endif else begin
	   !y.s(1) = !y.s(1)*aspect
	   t = .5 * (1.-aspect) * (!y.window(1)-!y.window(0))  ;Height
	   !y.window = !y.window + [t,-t]
	   !y.s(0) = !y.window(0) - vmin * !y.s(1)
	   !p.clip([1,3]) = !y.window * !d.y_size
	endelse
endif			;Isotropic

!x.type = 2			;Reset mapping type
if keyword_set(border) then  $
	plots ,!x.window([0,1,1,0,0]), color=color,   $
	       !y.window([0,0,1,1,0]), zvalue, /norm, /noclip, t3d=t3d
end



function map_grid_incr, span

if span eq 0 then return, 45.
ipow = 0
t = span < 450.
while t lt 5 do begin t = t * 10 & ipow = ipow +1 & endwhile
increments = [ 1., 2., 4., 5., 10., 15., 30., 45.]
i = 0
while t gt (increments(i) * 10) do i = i + 1
return, increments(i) / 10^ipow
end


pro map_grid, label=label,  latdel = latdel, no_grid=no_grid, zvalue=zvalue,$
       londel=londel, glinestyle = glinestyle, glinethick = glinethick,   $
       color = color, lonlab = lonlab, latlab = latlab, lonalign = lonalign,$
       latalign = latalign, charsize = charsize, t3d=t3d, $
	whole_map=whole_map
;
; Put a grid on a previously established map projection.
;
if (!x.type NE 2) THEN $  ; make sure we have mapping coordinates
   message, 'map_grid---Current ploting device must have mapping coordinates'

; no grid? - in case someone wants just to put labels
if not keyword_set(no_grid) then no_grid = 0

if n_elements(zvalue) eq 0 then zvalue = 0

; Get lat/lon ranges from !MAP
lonmin = !map.out(2) & lonmax1 = !map.out(3)
if lonmax1 le lonmin then lonmax1 = lonmax1 + 360.
latmin = !map.out(4) & latmax = !map.out(5)

if n_elements(t3d) le 0 then t3d = 0

if n_elements(latdel) eq 0 then latd = 1 else latd = latdel
if n_elements(londel) eq 0 then lond = 1 else lond = londel

; if WHOLE_MAP specified, or the deltas are < 1,
; do not convert the limits into integers
if (not keyword_set(whole_map)) then begin
   if abs(latmax - latmin) gt 5. and latd ge 1 then begin ;Make range integers
	latmin = float(floor(latmin))
	latmax = ceil(latmax)
	endif
   if abs(lonmax1 - lonmin) gt 5 and lond ge 1 then begin
	lonmin = float(floor(lonmin))
	lonmax1 = ceil(lonmax1)
;;	!map.out(3) = lonmax1 	;Save it...
	endif
endif

			;Default grid spacings...
if n_elements(latdel) eq 0 then latdel = map_grid_incr(latmax-latmin)
if n_elements(londel) eq 0 then londel = map_grid_incr(lonmax1-lonmin)

if N_Elements(glinestyle) EQ 0 THEN glinestyle =1
if N_Elements(glinethick) EQ 0 THEN glinethick =1

if n_elements(color) le 0 then begin	;Default color?
	if (!d.flags and 512) ne 0 then color = 0 else color = !d.n_colors-1
	endif

if N_Elements(label) NE 0 OR (N_ELEMENTS(Latlab) ne 0) $
	OR (N_Elements(LonLab) NE 0) THEN BEGIN
	printno = 1  
	printno2 = 1
	if N_Elements(Latlab) eq 0 THEN Latlab = (lonmin + lonmax1)/2
	if N_ELements(LonLab) eq 0 THEN LonLab = (latmin +latmax)/2
endif ELSE BEGIN
	printno = -1
	printno2 = -1
ENDELSE
                                    ; of grid numbers
if n_elements(latalign) eq 0 THEN latalign = .5	;Text alignment of lat labels
if n_elements(lonalign) eq 0 THEN lonalign = .5 ;Text alignment of lon labels
if n_elements(charsize) eq 0 THEN charsize =  1

step = 4 < (latmax - latmin)/10.
len = long((latmax-latmin) / step + 1)
lati = (float(latmax-latmin) / (len-1.)) * findgen(len) + latmin   ;lats


First = 1

for lon = lonmin, lonmax1, londel do begin
    if (lon lt -180) then lon2 =lon +360  $
    	else if (lon gt 180) then lon2 = -360 +lon $
    	else lon2 = lon
    pres = convert_coord(lon, latmin,/to_norm)
    pres = !map.out(0:1)
    pres1 = convert_coord(lon, latmax,/to_norm)
    pres1 = !map.out(0:1)
    lon1 = lon
    if First eq 1  THEN First = 0 else $
      if abs(pres(0) - past(0)) GE !map.out(6)   OR  $
         abs(pres(1) - past(1)) GE !map.out(7)   OR  $
         abs(pres1(0) - past1(0)) GE !map.out(6) OR  $
         abs(pres1(1) - past1(1)) GE !map.out(7)     $
      THEN BEGIN
       if(lon ge 0) then dd = .0001 else dd = -.0001
       lon1 = lon - dd
      ENDIF  
    past = pres
    past1 = pres1
    
    if (not no_grid) then plots, lon1, lati, zvalue, $
	color = color, t3d=t3d, NOCLIP=0,linestyle=glinestyle,thick=glinethick

    if lon2 ne long(lon2) then fmt = '(f7.2)' else fmt = '(i4)'
    if (printno eq 1) and $  ;Dont repeat -180....
	((lonmin ne -180) or (lonmax1 ne 180) or (lon ne -180)) then $
	xyouts,lon, LonLab, z=zvalue, ali=lonalign, t3d=t3d, color=color,$
          strtrim(string(lon2,format=fmt),2), charsize = charsize
    printno  = 1 - printno
 endfor

step = 4 < (lonmax1 - lonmin)/10.
len = (lonmax1-lonmin)/step + 1
loni = findgen(len)*step + lonmin

if (loni(len-1) NE lonmax1) THEN  BEGIN
    loni = [loni, lonmax1]
    len = len + 1
ENDIF

for lat = float(latmin), latmax, latdel do begin
   if lat ne long(lat) then fmt = '(f7.2)' else fmt = '(i4)'
   if printno2 eq 1 then xyouts,latlab,lat, z=zvalue, ali=latalign, t3d=t3d, $
	strtrim(string(lat, format=fmt),2), charsize = charsize, color=color
   printno2 = 1 - printno2
   if (not no_grid) then plots,loni, lat, zvalue, $
	NOCLIP=0,linestyle=glinestyle,color = color, thick=glinethick, t3d=t3d
 endfor
;    !map.out(5) = latmax

end

PRO map_limits, latmin, latmax, lonmin, lonmax, p0lon,p0lat, iproj, Rot
;
;  Sets map limits if none are specified...
;

if iproj eq 3 then begin	;Conic???
    lonmin = Rot - 180.
    lonmax = Rot + 180.
ENDIF ELSE BEGIN		;Not Conic
    lonmin = P0lon - 180.
    lonmax = P0lon + 180.
    latmax  = 90.
    latmin = -90.
ENDELSE

;
;stereo, ortho, lambert (covers a hemisphere) ??
;
if iproj eq 1 or iproj eq 2 or iproj eq 4 THEN BEGIN
    if p0lat eq 0.0 then begin	;Equatorial?
	lonmax = p0lon + 90 	;center on p0lon
	lonmin = p0lon - 90
    endif else begin		;Center on latitude
	latmin = p0lat - 90. > (-90.)
	latmax = p0lat + 90. < 90.
    endelse
endif			;Hemispheric coverage.

IF iproj eq 5 THEN BEGIN		;gnomic
  if p0lat eq 0.0 then BEGIN ; center on equator
	lonmax = p0lon + 60
	lonmin = p0lon - 60
	latmin = p0lat - 60
	latmax = p0lat + 60
	ENDIF
  if p0lat eq 90.0  then  begin
	latmin = 30
	latmax = 90.0
	ENDIF
  IF p0lat eq -90.0 then BEGIN
	latmin = -90.0
	latmax = -30.0
	ENDIF
  ENDIF					;Gnomic

IF iproj eq 3 THEN BEGIN		;Conic
	sgn = (p0lat ge 0) * 2 - 1	; + 1 or -1
	t0 = sgn * 15.
	t1 = sgn * 75.
        latmin = t0 < t1
        latmax = t0 > t1
        lonmin = -180. + Rot
        lonmax = 180.  + Rot
	ENDIF				;Conic

IF iproj eq 9 or iproj eq 12 THEN BEGIN  ;mercator, merc/simple
	latmin = -80
	latmax = 80
	ENDIF

;		Reduce longitudes
 while lonmin gt 180.    do lonmin = lonmin - 360.
 while lonmin lt (-180.) do lonmin = lonmin + 360.
 while lonmax gt 180.    do lonmax = lonmax - 360.
 while lonmax lt (-180.) do lonmax = lonmax + 360.
; Wrap around?  Allow for round-off.
 if (lonmax-1.0e-5) lt lonmin then lonmax = lonmax + 360.
;;;;;; *************** print,lonmin, lonmax
END


pro map_set, p0lat, p0lon, rot, proj = proj, $
	CYLINDRICAL = cyl,      MERCATOR = merc,  $
	MOLLWEIDE = moll,       STEREOGRAPHIC = stereo,  $
        ORTHOGRAPHIC = ortho,   Conic = cone,            $
	LAMBERT = lamb,         GNOMIC = gnom,           $
        AZIMUTHAL = azim,       SATELLITE = satel,       $
        SINUSOIDAL = sinu,      AITOFF = aitoff,          $
  latdel = latdel, londel = londel, limit = limit,       $
  Sat_P = Sat_p, title = title, noborder = noborder,        $
  noerase = noerase, label = label, $
  glinestyle = glinestyle, glinethick=glinethick,       $
  mlinestyle=mlinestyle, mlinethick=mlinethick,		$
  color = color, con_color = con_color,hires=hires,     $
  continents = continents, grid = grid, xmargin = xmargin, $
  ymargin = ymargin, lonlab = lonlab, latlab = latlab, $
  lonalign = lonalign, latalign = latalign, charsize = charsize, $
  advance = advance, usa=usa, t3d=t3d, position=position, zvalue=zvalue, $
  whole_map = whole_map, isotropic = isotropic, horizon = horizon
;+NODOCUMENT
; NAME:
;      map_set
; PURPOSE:
;        The procedure map_set establishes
;        the axis type and coordinate conversion mechanism
;        for mapping  points on the earth's surface, expressed
;        in latitude and longitude, to points on a plane, according
;        to one of ten possible map projections. The user may 
;        may select the map projection, the map center, polar rotation
;        and geographical limits.
;
;        The user may request map_set to plot the
;        graticule and continental boundaries by setting
;        the keywords Grid and Continent.
;
; CATEGORY:
;           Mapping
; CALLING SEQUENCE:
;                 map_set, p0lat, p0lon, rot, proj = proj,            $
;              	     CYLINDRICAL = cyl,      MERCATOR = merc,         $
;   	             MOLLWEIDE = moll,       STEREOGRAPHIC = stereo,  $
;                    ORTHOGRAPHIC = ortho,   Conic = cone,            $
;   	             LAMBERT = lamb,         GNOMIC = gnom,           $
;                    AZIMUTHAL = azim,       SATELLITE = satel,       $
;                    SINUSOIDAL = sinu,      AITOFF  = aitoff,         $
;                    latdel = latdel, londel = londel, limit = limit, $
;                    Sat_p = Sat_p, $
;                     title = title, noborder = noborder, $
;                    noerase = noerase, label = label,                $
;                    glinestyle = glinestyle, glinethick=glinethick,  $
;                    mlinestyle=mlinestyle, mlinethick=mlinethick,    $
;                    color = color, con_color = con_color, 	      $
;		     continents = continents, grid = grid, lonlab = lonlab,$
;                    latlab = latlab, lonalign = lonalign,           $
;                    latalign = latalign, charsize = charsize,hires=hires
;
; OPTIONAL INPUT:
;               P0lat--- For all but Lambert's conformal conic projection
;                        with two standard parallels, P0lat should be set
;                        to the latitude of the point on the earth's surface
;                        to be mapped to the center of the projection plane.
;                        If the projection type is sinusoidal, P0lat must be
;                        0. -90 <= P0lat <= 90. If P0lat is not set, the
;                        default value is 0. If the user has selected
;                        Lambert's conformal conic projection with two
;                        standard parallels, P0lat should be set to the
;                        latitude in degrees of one of the parallels.
;                        If not both P0lat and P0lon are defined, P0lat is
;                        20 by default.
;
;               P0lon--- For all but Lambert's conformal conic projection with
;                        two standard parallels, P0lon should be set
;                        by the user to the longitude of the point on the
;                        earth's surface to be mapped to the center of
;                        the map projection. -180 <= P0lon <= 180. If P0lon
;                        is not set, the default value is zero.
;
;              Rot-----  The user should set the argument Rot to the angle
;                        through which the North direction should be
;                        rotated around the line L between the earth's center
;                        and the point (P0lat, P0lon). Rot is measured in 
;                        degrees with the positive direction being clockwise
;                         rotation around L. Default value is 0.
;
; KEYWORDS:
;
;         Advance       = if set, advance to the next frame, when finished,
;			  when there are multiple plots on the screen.
;         Azimuthal     = azimuthal equidistant projection
;         Conic         = Conic projection
;         Cylindrical   = cylindrcal equidistant projection
;         Gnomic        = gnomonic projection
;         Lambert       = Lambert's equal area projection
;         Mercator      = Mercator's projection
;         Mollweide     = Mollweide type projection
;         Orthographic  = Orthographic projection
;	  Satellite	= Satellite (General perspective) projection
;         Sinusoidal    = sinsoidal projection
;         Stereographic = stereographic projection
;         Aitoff        = Aitoff's projection
;         charsize      = size of characters in labels.
;         Color         = color of the map boundary.
;	  con_color	= color of continents.
;         Continents    = flag to signal that continental boundaries
;                         should be drawn.         
;         Glinestyle    = linestyle of gridlines. (default = 1 = dotted)
;         Glinethick    = thickness of gridlines. (default = 1 = normal)
;         Grid          = flag to signal that gridlines should be drawn.
;	  Horizon	= if set, draw the horizon (limb) around the map.
;	  Isotropic	= if set, map will be made isotropic and 
;			  X and Y will have a 1:1 aspect ratio.
;         Label         = flag to signal labeling of parallels and 
;                         meridians in the grid.
;
;         latalign     = the aligment of the text baseline for
;                        latitude labels. A value of 0.0 left justifies
;                        the label, 1.0 right justifies it and
;                         0.5 centers. The default value is .5.
;                         
;         Latdel        = if set, spacing of parallels drawn on grid. 
;			  Default is 10 <  (latmin - latmax) / 10).
;         LatLab       =  if set, longitude at which to place latitude
;                         labels. Default is longitude of center.
;         Limit         =  A four or eight element vector.
;			  If a four element vector, [Latmin, Lonmin, Latmax,
;                          Lonmax] specifying the boundaries of the region
;                          to be mapped. (Latmin, Lonmin) and (Latmax,
;                          Lonmax) are the latitudes and longitudes of
;                          two diagonal points on the boundary with
;                          Latmin < Latmax and Lonmin < Lonmax.
;			   For maps that cross the international dateline,
;			   specify west longitudes as angles between
;			   180 and 360.
;			  If an eight element vector: [ lat0, lon0, 
;			    lat1, lon1, lat2, lon2, lat3, lon3] specify
;			    four points on the map which give, respectively,
;			    the location of a point on the left edge, 
;			    top edge, right edge, and bottom edge of
;			    the map extent.
;         lonalign     = the aligment of the text baseline for
;                        longitude labels. A value of 0.0 left justifies
;                        the label, 1.0 right justifies it and
;                         0.5 centers it. Default value is .5.
;        Londel         = if set, spacing of meridians drawn on grid.
;                         Y axis, and the screen border
;        LonLab       =  if set, latitude at which to place longitude
;                         labels. Default is longitude of center.
;        Mlinethick     = thickness of continental boundaries. Default is 1.
;        Mlinestyle     = linestyle of continental boundaries. Default is
;                         0 = normal.
;        Noborder       = if set, no border is drawn around the map.
;        Noerase        = flag to signal that map_set should not erase the
;                         current plot window. Default is to erase.
; 
;	Position	= Optional POSITION keyword for PLOT to specify the
;			  location of the map in the drawing area. See manual.
;	Sat_p		= an three element array of additional parameters
;			  required by the satellite projection.  The parameters
;			  are:  [ P, omega, gamma], where:  P = distance of
;			  viewpoint from center of globe, in units of the 
;			  globe's radius (P may be negative).
;			  Gamma = rotation of 
;			  projection plane clockwise from north.
;			  Omega = downward tilt of the 
;			  viewing plane in degrees.  The
;			  projection plane is first rotated gamma degrees 
;			  clockwise from north, and then tilted down
;			  omega degrees.  If both gamma and omega are zero,
;			  a Vertical Perspective projection results.
;	T3D 		= 1 to use the existing 3D transformation, 0 
;			  or omitted for normal.
;       Title           = Title of plot to be centered over plot window.
;       XMargin         = if set, a two element vector that specifies
;                         in character units the vertical margin between
;                         the map and the screen border.
;       YMargin         = if set, a two element vector that specifies
;                         in character units the horizontal margin between
;                         the map and the screen border.
;       Usa             = If set draw the state boundries
;	zvalue		= This keyword goes with T3D and specifies
;			  the height of the mapping plane.
;	whole_map	= in some cases where the limit is small, the gridlines
;			  will not cover the whole map.  This keyword can be
;			  specified to eliminate that problem.
;       hires           = Use the High Resolution CIA database for
;                         drawing the continents. Use map_continents to
;                         get the rivers, coasts, & boundaries
;
; Modification history:
;			Written, Ann Bateson, 1991
;	SMR, 10-'91	Added a check for additional satelite parameters
;			   when /SATELITE is set.
;       UCSB, ESRG,5-92 Added USA flag to check and map state boundries
;	DMS,  6-92	Fixed bug that incorrectly printed grid values with
;				small lat/lon ranges.
;	DMS, 8-92	Added 8 element lat/lon keyword, fixed problems
;				with Satellite, conics with 1 std parallel,
;				smaller and/or wierd maps
;	DMS, 3-93	Extended axes by a fudge factor, improved labelling.
;	JIY, 4-93	Added ZVALUE keyword to work with T3D.
;			Added NO_GRID keyword to MAP_GRID for displaying
;			labels only.
;			Added WHOLE_MAP keyword to make the gridlines cover
;			the entire map area when the limit is small
;			COLOR now works with map border and labels.
;   	KDB, 10-93	Fixed bug that would cause divide by zero errors
;			when P0lat was small and Rot equal to 0.0.
;       KDB,  5-94	Removed FONT=0 from plot calls. All vector fonts
;			can now be used. Font selection now depends on 
;			the value of !p.font
;	DMS, 6-94	Added HORIZON and ISOTROPIC keywords.  Cleaned
;			up a lot of logic.  Limits for maps of the entire
;			globe are done more cleanly and a lot quicker.
;       MWR, 12-94	Automatically create a window if !D.WINDOW=-1.
;			Calculations for [XY]MARGIN depend on valid !D values.
;       SVP,  2-95      Changed to CIA World Maps, added HIRES KEYWORD
;                       Also added Rivers, Coasts, Polygons, and Countries
;                       Thanks Thomas.
;-
;
; !Map.out coorespondence:
;  float p, q;			/* (0,1) u,v coordinates of last point */
;  float umin, umax;		/* (2,3) longitude min & max.  umin represents
;				   the longitude on the left edge of the
;				   map and is always <= umax. umin may equal
;				   umax for whole globe coverage.  */
;  float vmin, vmax;		/* (4,5) latitude min & max */
;  float ueps, veps;		/* (6,7) Epsilon for wrap-around detection */
;  float ucen, vcen;		/* (8,9) Center & rotation.. */
;  float UNUSED, vrng;		/* (10,11) Center latitude */

on_error, 2

multi_save = !p.multi(0)

; Set Hires to zero if not called
if not keyword_set(hires) then hires=0
;  Determine the projection
if n_elements(proj) ne 0 then iproj = proj $        
  else if keyword_set(stereo) then iproj =1 $
  else if keyword_set(ortho ) then iproj =2 $
  else if keyword_set(cone  ) then iproj =3 $
  else if keyword_set(lamb  ) then iproj =4 $
  else if keyword_set(gnom  ) then iproj =5 $
  else if keyword_set(azim  ) then iproj =6 $
  else if keyword_set(satel ) then iproj =7 $
  else if keyword_set(merc  ) then iproj =9 $
  else if keyword_set(moll  ) then iproj =10 $
  else if keyword_set(sinu  ) then iproj =14 $
  else if keyword_set(aitoff) then iproj =15 $
  else iproj=8		                         	;Assume cylindrical

IF (iproj eq 7) and (NOT(KEYWORD_SET(sat_p))) THEN $
    MESSAGE, "Satellite parameters must be set for satellite projection."

border = keyword_set(noborder) eq 0

; Supply defaults for p0lat p0lon = center of projection.
if iproj eq 3 then $		;Conic?
   if n_elements(p0lat) eq 0 or n_elements(p0lon) eq 0 then begin
	p0lat = 20.
	p0lon = 60.
   endif

if n_elements(p0lat) eq 0 then p0lat = 0.0
if n_elements(p0lon) eq 0 then p0lon = 0.0

if abs(p0lat) gt 90.0 then $
   message,'Latitude must be in range of +/- 90 degrees'
if abs(p0lon) gt 360.0 then $
   message,'Longitude must be in range of +/- 360 degrees'

IF n_elements(limit) eq 4 then BEGIN
	latmin = limit(0) & latmax = limit(2)
	lonmin = limit(1) & lonmax = limit(3)
ENDIF ELSE if n_elements(limit) eq 8 then BEGIN
	latmin = min([limit([0,2,4,6]), p0lat], max=latmax)
	lonmin = min([limit([1,3,5,7]), p0lon], max=lonmax)
ENDIF

if !P.multi(0) eq 0 and keyword_set(advance) then erase

if n_elements(rot) eq 0 then rot = 0.	;Default rotation
if n_elements(t3d) le 0 then t3d = 0

IF (iproj eq 14) THEN BEGIN		;Sinusoidal must have rot=0, p0lat=0
    if P0lat NE 0 THEN  $
      message,'MAP_SET--- Sinusoidal projection must have P0lat = 0',/CONT
    if Rot NE 0 THEN   $
      message,'MAP_SET--- Sinusoidal projection must have Rot = 0',/CONT
ENDIF


; Need to create a new window if the currect graphics device is
; X and there are no valid windows.  Subsequent calculations for
; [XY]MARGIN depend on current and valid values in !D.

IF (!D.WINDOW EQ -1) AND ((!D.FLAGS and 256) ne 0) THEN WINDOW

if KEYWORD_SET(xmargin) THEN BEGIN	;Check for margins > screen
   csize = !d.X_SIZE/!d.X_CH_SIZE	;1/Char width in normalized units
   if total(xmargin) gt csize THEN message,'map_set--- xmargin too large'
ENDIF

IF KEYWORD_SET(ymargin) THEN BEGIN
   csize = !d.Y_SIZE/!d.Y_CH_SIZE
   if total(ymargin) gt csize THEN message,'map_set--- ymargin too large'
ENDIF

!map.projection = iproj		;Set mapping parameters
!x.type = 2			;Indicates map transform
!map.phioc = p0lon		;Save Center longitude /lat for internals
!map.p0lat = p0lat

if (not KEYWORD_SET(noerase)) and (not KEYWORD_SET(advance)) THEN erase

if (N_Elements(limit) ne 4) and (N_Elements(limit) ne 8) then $
	map_limits, latmin, latmax, lonmin, lonmax, p0lon, p0lat, iproj, Rot

!map.out(2) = lonmin	& !map.out(3) = lonmax
!map.out(4) = latmin	& !map.out(5) = latmax

mapp_set,latmin,latmax,lonmin,lonmax,p0lon,p0lat,rot,iproj, limit,  $
      border=border, title =title, sat_p = sat_p, xmargin = xmargin, $
      ymargin = ymargin, t3d=t3d, position = position, zvalue=zvalue, $
      color=color, Isotropic = isotropic

if keyword_set(grid) or keyword_set(label) then $		;Grid?
	map_grid, label=label, latdel=latdel, zvalue=zvalue, $
          londel = londel , glinestyle = glinestyle, $
          glinethick = glinethick, color = color, latlab = latlab,$
          lonlab = lonlab, lonalign = lonalign, latalign = latalign, $
          charsize = charsize, t3d=t3d, whole_map=whole_map, $
	  no_grid = (not keyword_set(grid))

if keyword_set(horizon) then map_horizon, color=color

if keyword_set(continents) then begin		;Continents?
	if n_elements(con_color) le 0 then begin
	    if n_elements(color) gt 0 then con_color = color $
		else begin
		  if (!d.flags and 512) ne 0 then color = 0 $
		    else color = !d.n_colors-1
		endelse
        endif		;con_color
	    map_continents,color=con_color,t3d=t3d,hires=hires, $
             continents=continents,mlinestyle=mlinestyle, $
             mlinethick=mlinethick,zvalue=zvalue
endif

if keyword_set(usa) ne 0 then $			;US State outlines?
        map_continents, color = con_color, t3d=t3d, $
        mlinestyle = mlinestyle, mlinethick = mlinethick, us=usa, zvalue=zvalue

if KEYWORD_SET(ADVANCE) and !P.Multi(0) gt 0 THEN $
     !P.Multi(0) = !P.Multi(0) - 1 $
else !p.multi(0) = !p.multi(1) * !p.multi(2) - 1
       
RETURN
END
