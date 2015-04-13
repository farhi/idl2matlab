
FUNCTION map_getindex,indx
;
; Used to read in the index file for
; each map data file
;
openr, lun, indx, /xdr, /get_lun
segments=0L & readu, lun, segments

dx_map=replicate({ fptr:0L, npts:0L,latmax:0.,latmin:0.,$
                   lonmax:0.,lonmin:0. },segments )

readu, lun, dx_map & free_lun, lun
free_lun,lun
return, dx_map
END

pro fmap_horizon, xr, yr, zr
; Return the horizon (limb) of a map in U,V
; different from map_horizon in that the 
; coords are clockwise. This is used for
; clipping of the polygons

n = 360		;# of vertices

proj = !map.projection
if proj eq 5 or proj eq 3 then begin
        xr = fltarr(n+1) & yr = fltarr(n+1) & zr = fltarr(n+1)
        return
endif

p = map_uv_bounds(proj)
a = findgen(n+1) * (2 * !pi / n)

;		Projections that map to a circle or ellipse
;          0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
circle = [ 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1]

if proj eq 7 and !map.sat(2) ne 0 then begin	;Satellite projection, tilted
    map_satellite_limit, 360, xr, yr
endif else if circle(proj) then begin		;Std circular projection
    xr = (p(2)-p(0))/2. * cos(a)
    yr = (p(3)-p(1))/(-2.) * sin(a)
endif else if proj eq 14 then begin		;Sinusoidal
    flon = (!map.out(3)-!map.out(2))/360.
    xr = flon * !pi * cos(a)
    yr = fltarr(n+1)
    k = fix(a/(!pi/2))			;Quadrant
    yr = temporary(a)
    q12 = where(k eq 1 or k eq 2)
    yr(q12) = !pi - yr(q12)
    q4 = where(k eq 3)
    yr(q4) = yr(q4) - 2*!pi
    yr(n) = 0.0
endif else begin		;Rectangular

  dx0= p(2)+findgen(n/4)*(p(0)-p(2))/(n/4.)
  dx1= replicate(p(0),(n/4))
  dx2= p(0)+findgen(n/4)*(p(2)-p(0))/(n/4.)
  dx3= replicate(p(2),(n/4))
  xr= [dx0,dx1,dx2,dx3,dx0(0)]

  dy0= replicate(p(1),(n/4))
  dy1= p(1)+findgen(n/4)*(p(3)-p(1))/(n/4.)
  dy2= replicate(p(3),(n/4))
  dy3= p(3)+findgen(n/4)*(p(1)-p(3))/(n/4.)
  yr= [dy0,dy1,dy2,dy3,dy0(0)]

;  xr = [p(0), p(2), p(2), p(0), p(0)]
;  yr = [p(1), p(1), p(3), p(3), p(1)]
endelse

zr = yr < 0.5 > 0.5
END

pro poly_process,xy,poly_type,color,zvalue,t3d,spacing,orientation
common map_continents, xhor, yhor, zhor, limits
;
; Process the input polygon and plot it.
; --------------------------------------
;
; *  Identify the non-visible points of the polgon.
;        ( the bad array )
; *  If there are no bad points, plot it
;
; *  If there are bad points, fill in the holes with horizon pts.
;
;
	 n=n_elements(xy(0,*))
      FFMT='(4(f8.4,x),a,i4,a,4(f8.4,x))'
    bad=intarr(n)
;
;   Prepare the Jump arrays to determine wrapping.
;
     pt=convert_coord(xy(1,*),xy(0,*),/TO_NORM,/DATA)
     jumpx=[reform(abs(pt(0,0:n-2)-pt(0,1:n-1))),0]
     jumpy=[reform(abs(pt(1,0:n-2)-pt(1,1:n-1))),0]
     max_x=0.1 & max_y=0.1

for i=0,n-1 do begin

    if ((pt(1,i) eq 1E12) or (pt(0,i) eq 1E12)) then begin
       bad(i)=1 
    endif else begin
      if ((jumpx(i) gt max_x ) or (jumpy(i) gt max_y )) then bad(i)=2
     p=convert_coord(xy(1,i),xy(0,i),/TO_NORM,/DATA)
      m0=!map.out(0) & m1=!map.out(1)
      del=0.0
      if ((m1 gt limits(3)-del) or (m1 lt limits(1)+del) or $
          (m0 gt limits(2)-del) or (m0 lt limits(0)+del)) then $
          bad(i)= 3
;         ((xy(0,i) gt !map.out(5)) or (xy(0,i) lt !map.out(4)) or $
;          (xy(1,i) gt !map.out(3)) or (xy(1,i) lt !map.out(2)) ) then begin
;            bad(i)=3
;      endif
    endelse
endfor

    bad_close,bad,xy,poly_type,color,zvalue,t3d,spacing,orientation
end

pro bad_close,bad,xy,poly_type,color,tzvalue,t3d,spacing,orientation
;
;
;
index=where(bad ge 1,badcount)
n=n_elements(xy(0,*))

if n_elements(tzvalue) ne n then  $
         tzvalue=fltarr(n) > 0.5

if badcount eq 0 then begin
   poly_draw,xy,poly_type,color,tzvalue,t3d,spacing,orientation,2
;   spoly_draw,xy,poly_type,color,tzvalue,t3d,spacing,orientation
endif else if badcount lt n then begin
;
;  Go to first bad point, rotate all arrays so that
;  bad=[1,1,1,......,0,....,0,1]
;
   firstbad=-1*index(0) 
   if firstbad ne 0 then begin ; reform the xy array
       xy(0,*)=shift(xy(0,*),firstbad)
       xy(1,*)=shift(xy(1,*),firstbad)
       bad=shift(bad,firstbad)
       tzvalue=shift(tzvalue,firstbad)
   endif
;
; OK start with first zero
;
j=0
  while j lt n-2 do begin
      if bad(j) eq 0 then begin
         ind=where(bad(j:n-2) ge 1,more_count)
         if more_count eq 0 then  begin
            if n-1-j gt 5 then begin
             poly_close,xy(*,j:n-2),poly_type,color,tzvalue(j:n-2),$
                      t3d,spacing,orientation
            endif else begin
             plots,xy(*,j:n-2),color=color,/data,t3d=t3d
            endelse
	    goto, done
         endif else begin
            if ind(0) gt 20 then begin
             poly_close,xy(*,j:ind(0)+j),poly_type,color,tzvalue(j:ind(0)+j),$
                      t3d,spacing,orientation
            endif else begin
             plots,xy(*,j:ind(0)+j),color=color,/data,t3d=t3d
            endelse
            j=j+ind(0)
         endelse
      endif
     j=j+1
  endwhile
endif
done:
end



pro poly_close,xy,poly_type,color,zvalue,t3d,spacing,orientation
common map_continents, xhor, yhor, zhor, limits

;
; This program closes the input polygon with the current horizon.
; (the start and end points need to be connected)
; 
; Requirements:
;
; xy is at least a (2,3) array of lats and longs
; Mercator and Molleweide projections give inconsistant results
;
; xy(0,*) are the lats
; xy(1,*) are the lons
;

n=n_elements(xy(0,*))

del_lat=xy(0,n-1)-xy(0,0)
ave_lat=(xy(0,n-1)+xy(0,0))/2.0

rdel_lon=(xy(1,n-1)-xy(1,0))
del_lon=rdel_lon*cos(ave_lat*!dtor)

;
;  Obtain the coordinates of the coordinate polygon in map
;  coordinates
;
for i=0,n-1 do begin
    pt=convert_coord(xy(1,i),xy(0,i),/TO_NORM,/DATA)
    m0=!map.out(0) & m1=!map.out(1)
    xy(1,i)=m0 & xy(0,i)=m1
endfor
;
; Only use coordinates at least ldiff away from an edge
; ldiff=0.05
;
 ldiff=0.02
gindex=where((xy(0,*) lt limits(3)-ldiff) and $
             (xy(0,*) gt limits(1)+ldiff) and $
             (xy(1,*) lt limits(2)-ldiff) and $
             (xy(1,*) gt limits(0)+ldiff),n)
if n lt 5 then goto,skipit
;
; Reform the xy and zvalue arrays
;
xy=xy(*,gindex) & zvalue=zvalue(gindex)
;
; find horizon point closest to each of the extremes
;

diff=sqrt((xhor-xy(1,0))^2. + (yhor-xy(0,0))^2.)
min0=min(diff,diff_s0)

diff=sqrt((xhor-xy(1,n-1))^2. + (yhor-xy(0,n-1))^2.)
min0=min(diff,diff_s1)

nhorizon=n_elements(xhor)
test_1=0    & test_2 =0

if abs(diff_s0-diff_s1) le nhorizon/2. then test_1 = 1

if diff_s1 le diff_s0 then test_2 =1

ddiff=abs(diff_s1-diff_s0)

nsegs=min([ddiff,nhorizon-ddiff])
;
; Setup the temporary xy (new) and zvalue (newz) arrays
;
new=fltarr(2,n+nsegs) & newz=fltarr(n+nsegs)
new(*,0:n-1)=xy & newz(0:n-1)=zvalue
;
; Create/insert the horizon patching segment
;

if test_1 xor test_2 then begin ; go down
   for i=0,nsegs-1 do begin
        idd=diff_s1-i
        if idd lt 0 then idd=idd+nhorizon
        if idd gt nhorizon then idd=idd-nhorizon
	new(1,n+i)=xhor(idd)
	new(0,n+i)=yhor(idd)
        newz(n+i)=zhor(idd)
   endfor
endif else begin ; go up
   for i=0,nsegs-1 do begin
        idd=diff_s1+i
        if idd lt 0 then idd=idd+nhorizon
        if idd gt nhorizon-1 then idd=idd-nhorizon
	new(1,n+i)=xhor(idd)
	new(0,n+i)=yhor(idd)
        newz(n+i)=zhor(idd)
   endfor
;
; xy     = new
; zvalue = newz
;
endelse

poly_draw,new,poly_type,color,newz,t3d,spacing,orientation,0
skipit:
end

pro poly_draw,xy,poly_type,color,zvalue,t3d,spacing,orientation,intype
;
; CLIP IN DEVICE COORDINATES, THEN PLOT WITH POLYFILL
;
 !x.type=intype
     out=convert_coord(xy(1,*),xy(0,*),/TO_DEVICE)
   winll=convert_coord(!x.window(0),!y.window(0),/NORM,/TO_DEVICE)
   winur=convert_coord(!x.window(1),!y.window(1),/NORM,/TO_DEVICE)

   index=where(out(0,*) gt winll(0) and $
               out(0,*) lt winur(0) and $
               out(1,*) gt winll(1) and $
               out(1,*) lt winur(1),count)

   if count gt 3 then begin
      CASE poly_type OF
            2: polyfill,out(0,index),out(1,index),out(2,index),/DEVICE,$
                   color=color,spacing=spacing,orientation=orientation,$
                   /LINE_FILL
         ELSE: polyfill,out(0,index),out(1,index),out(2,index),/DEVICE,$
                   color=color
      ENDCASE
   endif
 !x.type=2
end



pro spoly_draw,xy,poly_type,color,zvalue,t3d,spacing,orientation
lonmin = !map.out(2) 
lonmax = !map.out(3) 

latmin = !map.out(4) > (-89.99) < 89.99
latmax = !map.out(5) > (-89.99) < 89.99

if abs(lonmax-lonmin) ge 360 or (lonmin eq lonmax) then begin
   lonmin=-179.99 & lonmax=179.99
endif

climits=[lonmin,latmin,lonmax,latmax]

     CASE poly_type OF
       2: polyfill, ((xy(1,*) > lonmin) <lonmax ), $
                    ((xy(0,*) > latmin) <latmax ), $
                    /DATA, zvalue, CLIP=climits, NOCLIP=0, t3d=t3d, $
                    spacing=spacing,orientation=orientation, $
                    /LINE_FILL,color=color
          
     ELSE: polyfill, ( (xy(1,*) > lonmin) <lonmax), $
                     ( (xy(0,*) > latmin) <latmax), $
                   /DATA, zvalue, CLIP=climits, NOCLIP=0, t3d=t3d, $
                  color = color
     ENDCASE
end

pro map_continents, MLINESTYLE = mlinestyle, COLOR = color, T3D=t3d, $
	MLINETHICK = mlinethick, USA = kusa, CONTINENTS = kcont, $
	ZVALUE=zvalue, RIVERS=krivers, HIRES=khires, $
	COUNTRIES=kcountries,SPACING=spacing,$
        FILL_CONTINENTS=kfill_continents,COASTS=kcoasts, $
	ORIENTATION=korientation
;
;
;       if fill_continents =1 you get solid polygons
;       if fill_continents =2 you get lines
;

common map_continents, xhor, yhor, zhor, limits

;  t0 = systime(1)
cont = keyword_set(kcont)
usa = keyword_set(kusa)
rivers = keyword_set(krivers)
coasts = keyword_set(kcoasts)
countries = keyword_set(kcountries)
hires = keyword_set(khires)
if not keyword_set(kfill_continents) then $
                    fill_continents = 0 else $
                    fill_continents=kfill_continents 
if not keyword_set(korientation) then $
                    orientation = 0 else $
                    orientation=korientation 
if (usa+countries+coasts+rivers eq 0) then cont = 1
if n_elements(spacing) eq 0 then spacing = 0.5     ; cm
if n_elements(zvalue) eq 0 then zvalue = 0

if (!x.type NE 2) THEN message,'Map transform not established.'
; if (fill_continents ne 0 and (!map.projection eq 7 $
;      or !map.projection eq 3 or !map.projection eq 5)) then begin 
;      print,'----------------------------------------------------'
;      print,'  Sorry, but continent filling does not work well   '
;      print,'         in the current projection. You can still   '
;      print,'         get continental outlines by using fill=0.  '
;      print,'----------------------------------------------------'
;endif
;
;md=0.03
md=0.01
;
latmin = !map.out(4)-md & latmax = !map.out(5)+md
lonmin = !map.out(2)+md & lonmax = !map.out(3)-md
if lonmax le lonmin then lonmax = lonmax + 360.
test_lon = ((lonmax-lonmin) mod 360.) ne 0.0

if n_elements(mlinestyle) EQ 0 THEN mlinestyle=0
if N_Elements(mlinethick) EQ 0 THEN mlinethick=1
if n_elements(t3d) le 0 then t3d = 0
if n_elements(color) le 0 then begin	;Default color?
	if (!d.flags and 512) ne 0 then color = 0 else color = !d.n_colors-1
	endif

maxlat=0. & minlat=0. & maxlon=0. & minlon=0.
minlon=lonmin
if lonmax gt 180.  then lonmin=180.-lonmax
if minlon lt -180. then lonmax=360.+minlon

map_file=FILEPATH('supmap.dat',subdir=['resource','maps'])
openr, lun, /get, map_file,/xdr,/stream
npts = 0L
; 	cont us_only  both
fbyte = [ 0, 71612L, 165096L]
nsegs = [ 283, 325, 594 ]

ij = 0			;Default = continents only
if rivers ne 0 then begin
   if (hires ne 0) then begin
     rfile=FILEPATH('rhigh.ndx',subdir=['resource','maps','high'])
     a=findfile(rfile,count=file_count) 
     rdat=FILEPATH('rhigh.dat',subdir=['resource','maps','high'])
     b=findfile(rdat,count=dat_count) 
;
;    Make sure that the hires files are present, drop to low
;    resolution if that are missing.
; 
     if (file_count eq 1 and dat_count eq 1) then begin
           rndx=map_getindex(rfile)
     endif else begin
       print,' '
       print,' Warning: Hi Resolution Map Files could not be located. '
       print,'  Attempting to substitute low resolution Rivers file. '
       print,' '
       rndx=map_getindex(FILEPATH('rlow.ndx',subdir=['resource','maps','low']))
       rdat=FILEPATH('rlow.dat',subdir=['resource','maps','low'])
     endelse
   endif else begin
      rndx=map_getindex(FILEPATH('rlow.ndx',subdir=['resource','maps','low']))
      rdat=FILEPATH('rlow.dat',subdir=['resource','maps','low'])
   endelse

	openr,riv,/get,rdat,/xdr,/stream
        rindex=where( (rndx.latmin lt latmax) and $
                      (rndx.latmax gt latmin) and $
                      (rndx.lonmin lt lonmax) and $
                     ((rndx.lonmax gt lonmin)  or $
                      (rndx.lonmax gt lonmax-360.)) ,rcount)

	if rcount ge 0 then begin
	 for i=0,rcount-1 do begin
		point_lun,riv,rndx(rindex(i)).fptr
                xy=fltarr(2,rndx(rindex(i)).npts)
		readu,riv,xy
	 plots,xy(1,*),xy(0,*),zvalue,NOCLIP=0,t3d=t3d,thick=mlinethick, $
	      linestyle=mlinestyle,color=color,/data
        endfor
	endif
	free_lun,riv
endif

if countries ne 0 then begin
   if (hires ne 0) then begin
     bfile=FILEPATH('bhigh.ndx',subdir=['resource','maps','high'])
     a=findfile(bfile,count=file_count) 
     bdat=FILEPATH('bhigh.dat',subdir=['resource','maps','high'])
     b=findfile(bdat,count=dat_count) 
;
;    Make sure that the hires files are present, drop to low
;    resolution if that are missing.
; 
     if (file_count eq 1 and dat_count eq 1) then begin
           bndx=map_getindex(bfile)
     endif else begin
       print,' '
       print,' Warning: Hi Resolution Map Files could not be located. '
       print,'  Attempting to substitute low resolution Countries file. '
       print,' '
       bndx=map_getindex(FILEPATH('blow.ndx',subdir=['resource','maps','low']))
       bdat=FILEPATH('blow.dat',subdir=['resource','maps','low'])
     endelse
   endif else begin
       bndx=map_getindex(FILEPATH('blow.ndx',subdir=['resource','maps','low']))
       bdat=FILEPATH('blow.dat',subdir=['resource','maps','low'])
   endelse
	openr,biv,/get,bdat,/xdr,/stream
        bindex=where( (bndx.latmin lt latmax) and $
                      (bndx.latmax gt latmin) and $
                      (bndx.lonmin lt lonmax) and $
                     ((bndx.lonmax gt lonmin)  or $
                      (bndx.lonmax gt lonmax-360.)) ,bcount)
	if bcount ge 0 then begin
	 for i=0,bcount-1 do begin
		point_lun,biv,bndx(bindex(i)).fptr
                xy=fltarr(2,bndx(bindex(i)).npts)
		readu,biv,xy
	 plots,xy(1,*),xy(0,*),zvalue,NOCLIP=0,t3d=t3d,thick=mlinethick, $
	      linestyle=mlinestyle,color=color,/data
        endfor
	endif
	free_lun,biv
endif

if coasts ne 0 then begin
   if (hires ne 0) then begin
     cfile=FILEPATH('chigh.ndx',subdir=['resource','maps','high'])
     a=findfile(cfile,count=file_count) 
     cdat=FILEPATH('chigh.dat',subdir=['resource','maps','high'])
     b=findfile(cdat,count=dat_count) 
;
;    Make sure that the hires files are present, drop to low
;    resolution if that are missing.
; 
     if (file_count eq 1 and dat_count eq 1) then begin
           cndx=map_getindex(cfile)
     endif else begin
       print,' '
       print,' Warning: Hi Resolution Map Files could not be located. '
       print,'  Attempting to substitute low resolution Coastal file. '
       print,' '
       cndx=map_getindex(FILEPATH('clow.ndx',subdir=['resource','maps','low']))
       cdat=FILEPATH('clow.dat',subdir=['resource','maps','low'])
     endelse
   endif else begin
       cndx=map_getindex(FILEPATH('clow.ndx',subdir=['resource','maps','low']))
       cdat=FILEPATH('clow.dat',subdir=['resource','maps','low'])
   endelse
	openr,civ,/get,cdat,/xdr,/stream
        cindex=where( (cndx.latmin lt latmax) and $
                      (cndx.latmax gt latmin) and $
                      (cndx.lonmin lt lonmax) and $
                     ((cndx.lonmax gt lonmin)  or $
                      (cndx.lonmax gt lonmax-360.)) ,ccount)
	if ccount ge 0 then begin
	 for i=0,ccount-1 do begin
		point_lun,civ,cndx(cindex(i)).fptr
                xy=fltarr(2,cndx(cindex(i)).npts)
		readu,civ,xy

	 plots,xy(1,*),xy(0,*),zvalue,NOCLIP=0,t3d=t3d,thick=mlinethick, $
	      linestyle=mlinestyle,color=color,/data
        endfor
	endif
	free_lun,civ
endif

if (cont ne 0 or fill_continents ne 0) then begin
    fmap_horizon, xhor, yhor, zhor
    limits = map_uv_bounds(!map.projection)
    clipsave = !p.clip
    !p.clip=limits
   if (hires ne 0 ) then begin
     pfile=FILEPATH('phigh.ndx',subdir=['resource','maps','high'])
     a=findfile(pfile,count=file_count) 
     pdat=FILEPATH('phigh.dat',subdir=['resource','maps','high'])
     b=findfile(pdat,count=dat_count) 
;
;    Make sure that the hires files are present, drop to low
;    resolution if that are missing.
; 
     if (file_count eq 1 and dat_count eq 1) then begin
           pndx=map_getindex(pfile)
     endif else begin
       print,' '
       print,' Warning: Hi Resolution Map Files could not be located. '
       print,'  Attempting to substitute low resolution polygon file. '
       print,' '
       pndx=map_getindex(FILEPATH('plow.ndx',subdir=['resource','maps','low']))
       pdat=FILEPATH('plow.dat',subdir=['resource','maps','low'])
     endelse
   endif else begin
       pndx=map_getindex(FILEPATH('plow.ndx',subdir=['resource','maps','low']))
       pdat=FILEPATH('plow.dat',subdir=['resource','maps','low'])
   endelse
   openr,piv,/get,pdat,/xdr,/stream
   pindex=where( (pndx.latmin lt latmax) and $
                 (pndx.latmax gt latmin) and $
                 (pndx.lonmin lt lonmax) and $
                ((pndx.lonmax gt lonmin)  or  $
                 (pndx.lonmax gt lonmax-360.)), pcount)

   if pcount ge 0 then begin
      for i=0,pcount-1 do begin
	point_lun,piv,pndx(pindex(i)).fptr
        xy=fltarr(2,pndx(pindex(i)).npts)
        readu,piv,xy

      CASE fill_continents OF
        1: poly_process,xy,fill_continents,color,zvalue,t3d,$
		spacing,orientation
        2: poly_process,xy,fill_continents,color,zvalue,t3d,$
		spacing,orientation
        ELSE: plots,xy(1,*),xy(0,*),zvalue,NOCLIP=1,t3d=t3d,thick=mlinethick, $
	      linestyle=mlinestyle,color=color,/data
      ENDCASE
      endfor
    endif
    free_lun,piv
    !p.clip = clipsave
endif

if usa ne 0 then begin
	if cont then ij = 2 else ij = 1

  point_lun, lun, fbyte(ij)

  for i=1,nsegs(ij) do begin	;Draw each segment
	READU, lun, npts,maxlat,minlat,maxlon,minlon
	npts = npts / 2		;# of points
	xy = fltarr(2,npts)
	READU, lun, xy
	if (maxlat lt latmin) or (minlat gt latmax) then goto,skipit
	if test_lon then $
	  if ((maxlon lt lonmin) or (minlon gt lonmax)) then BEGIN
            if (lonmax gt 180. and maxlon + 360. ge lonmin) then goto,goon
            goto, skipit
          ENDIF
  goon:	plots, xy(1,*), xy(0,*), zvalue, NOCLIP=0, t3d=t3d, $
		THICK=mlinethick,linestyle=mlinestyle, color = color
  skipit:
  endfor
endif
FREE_LUN, lun
;  print, systime(1) - t0, ' seconds'
end
