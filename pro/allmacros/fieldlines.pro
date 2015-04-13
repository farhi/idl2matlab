PRO fieldlines,charge,x,y,z,probe=probe,len=len,nsteps=nsteps,sx=sx,sy=sy,sz=sz,nvecs=nvecs,noprint=noprint,pixel=pixel,$
                            ps=ps,color=color,noopen=noopen,file=file,noclose=noclose

;+
;fieldlines,charge2d(*,0:5*642+641),indgen(1000+1),[0],indgen(z2dr(1)-z2dr(0)+1),probe=-1.602E-19,sx=indgen(70)*10+280,sy=intarr(70),sz=intarr(70)+5
;-

IF KEYWORD_SET(ps) THEN BEGIN
  SET_PLOT, 'PS'
  IF NOT KEYWORD_SET(file) THEN file='fieldlines.ps'
  IF NOT KEYWORD_SET(noopen) THEN DEVICE, FILE=file, /COLOR
  TVLCT, [0,255,0,0], [0,0,255,0], [0,0,0,255]
  IF NOT KEYWORD_SET(color) THEN color=1 ; red
ENDIF
IF NOT KEYWORD_SET(pixel) THEN pixel=1E-6 ; 
IF NOT KEYWORD_SET(probe) THEN probe=-1. ; negative standard probe, might be an electron
;s = size(x)
;if s(0) ne 3 then message,'fieldlines: Vx, Vy, and Vz must be 3D arrays'
;nx = s(1)
;ny = s(2)
;nz = s(3)
nx = N_ELEMENTS(x)
ny = N_ELEMENTS(y)
nz = N_ELEMENTS(z)
dim=3
IF nx LE 1 THEN dim=0
IF ny LE 1 THEN dim=1
IF nz LE 1 THEN dim=2
xr=max(x)-min(x)
yr=max(y)-min(y)
zr=max(z)-min(z)
xs=min(x)
ys=min(y)
zs=min(z)
if n_elements(nsteps) le 0 then nsteps = (nx > ny > nz) /5
if n_elements(len) le 0 then len = 2.0  ;Default length
if n_elements(arrowsize) le 0 then arrowsize = 0.05
if n_elements(sx) le 0 then begin	;Starting points specified?
	if n_elements(nvecs) le 0 then nvecs = 200
	dummy=randomu(seed, nvecs)
	x1 = randomu(seed, nvecs) * xr + xs
	y1 = randomu(seed, nvecs) * yr + ys
	z1 = randomu(seed, nvecs) * zr + zs
endif else begin
	x1 = float(sx)
	y1 = float(sy)
	z1 = float(sz)
	nvecs = n_elements(x1) < n_elements(y1) < n_elements(z1)
endelse
HT=x1*0.
Joule=HT
flags=INTARR(nvecs)+1
E=FLTARR(3,nvecs)
xp1=(x1-xs)/xr*(nx-1)
yp1=(y1-ys)/yr*(ny-1)
zp1=(z1-zs)/zr*(nz-1)
i=0
WHILE i LT nsteps DO BEGIN
	x0 = x1
	y0 = y1
	z0 = z1
	;PRINT,x1(0),y1(0),z1(0)
	xp0=xp1
	yp0=yp1
	zp0=zp1
	;PRINT,xp1(0),yp1(0),zp1(0)
	FOR j=0,nvecs-1 DO BEGIN
	  IF flags(j) THEN E(*,j)=field([x0(j),y0(j),z0(j),probe],charge,len/2.)
	ENDFOR
	u0=e(0,*)
	v0=e(1,*)
	w0=e(2,*)
	;u0=interpolate(u,xp0,yp0,zp0)
	;v0=interpolate(v,xp0,yp0,zp0)
	;w0=interpolate(w,xp0,yp0,zp0)
	E0=sqrt(u0^2+v0^2+w0^2)
	dx=u0/E0*len*flags
	dy=v0/E0*len*flags
	dz=w0/E0*len*flags
	x1 = dx + x0
	y1 = dy + y0
	z1 = dz + z0
	index=WHERE(x1 LT min(x) OR y1 LT min(y) or z1 LT min(z) or x1 GT max(x) or y1 GT max(y), count)
	IF count GT 0 THEN flags(index)=0
	HT=HT+SQRT(((dx)^2+(dy)^2+(dz)^2))*E0*flags*pixel
	Joule=HT*probe
	eV=Joule/1.602E-19
	;velocity=SQRT(2.*ABS(Joule)/9.109E-31)
        xp1=(x1-xs)/xr*(nx-1)
        yp1=(y1-ys)/yr*(ny-1)
        zp1=(z1-zs)/zr*(nz-1)
	;IF NOT KEYWORD_SET(noprint) THEN IF flags(0) THEN PRINT,xp1(0),yp1(0),zp1(0)
	IF i NE 0 THEN BEGIN
	  FOR j=0,nvecs-1 DO BEGIN
	    IF flags(j) THEN BEGIN
              IF KEYWORD_SET(ps) THEN BEGIN
	        IF dim EQ 3 THEN plots, [xp0(j), xp1(j)],[yp0(j),yp1(j)],[zp0(j),zp1(j)],/t3d,color=color
	        IF dim EQ 0 THEN plots, [yp0(j),yp1(j)],[zp0(j),zp1(j)],color=color
	        IF dim EQ 1 THEN plots, [xp0(j),xp1(j)],[zp0(j),zp1(j)],color=color
	        IF dim EQ 2 THEN plots, [xp0(j),xp1(j)],[yp0(j),yp1(j)],color=color
	      ENDIF ELSE BEGIN
	        IF dim EQ 3 THEN plots, [xp0(j), xp1(j)],[yp0(j),yp1(j)],[zp0(j),zp1(j)],/t3d
	        IF dim EQ 0 THEN plots, [yp0(j),yp1(j)],[zp0(j),zp1(j)]
	        IF dim EQ 1 THEN plots, [xp0(j),xp1(j)],[zp0(j),zp1(j)]
	        IF dim EQ 2 THEN plots, [xp0(j),xp1(j)],[yp0(j),yp1(j)]
	      ENDELSE
            ENDIF
          ENDFOR
	ENDIF
	IF NOT KEYWORD_SET(noprint) THEN IF flags(0) THEN PRINT,x1(0),y1(0),z1(0),HT(0),u0(0),v0(0),w0(0),E0(0)
	IF count EQ nvecs THEN BEGIN
	  PRINT, 'all electrons arrived or left the box'
	  i=nsteps-1
	ENDIF
	i=i+1
ENDWHILE

;print,HT(WHERE(HT GT 0))
;print,''
print,MAX(HT)

IF KEYWORD_SET(ps) AND NOT KEYWORD_SET(noclose) THEN DEVICE, /CLOSE

END