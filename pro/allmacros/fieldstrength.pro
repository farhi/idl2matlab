PRO fieldstrength,u,v,w,x,y,z,len=len,nsteps=nsteps,sx=sx,sy=sy,sz=sz,dir=dir

IF NOT KEYWORD_SET(dir) THEN dir=1.
s = size(u)
if s(0) ne 3 then message,'FLOW3: Vx, Vy, and Vz must be 3D arrays'
nx = s(1)
ny = s(2)
nz = s(3)
xr=max(x)-min(x)
yr=max(y)-min(y)
zr=max(z)-min(z)
xs=min(x)
ys=min(y)
zs=min(z)
if n_elements(nsteps) le 0 then nsteps = (nx > ny > nz) /5
if n_elements(len) le 0 then len = 2.0  ;Default length
if n_elements(arrowsize) le 0 then arrowsize = 0.05
zscale = len/max(sqrt(u^2+v^2+w^2))  ;Make max step = len
if n_elements(sx) le 0 then begin	;Starting points specified?
	if n_elements(nvecs) le 0 then nvecs = 200
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
flags=INTARR(nsteps)
FOR i=0,nsteps DO BEGIN
	x0 = x1
	y0 = y1
	z0 = z1
	index=WHERE( x0 LE min(x) OR  y0 LE min(y) or  z0 LE min(z) or  x0 GE max(x) or  y0 GE max(y) or  z0 GE max(z), count )
	if count GT 0 THEN flag(index)=1
	xp0=(x0-xs)/xr*nx
	yp0=(y0-ys)/yr*ny
	zp0=(z0-zs)/zr*nz
	u0=interpolate(u,xp0,yp0,zp0)
	v0=interpolate(v,xp0,yp0,zp0)
	w0=interpolate(w,xp0,yp0,zp0)
	E0=sqrt(u0^2+v0^2+w0^2)
	dx=u0/E0*len*dir
	dy=v0/E0*len*dir
	dz=w0/E0*len*dir
	x1 = dx + x0
	y1 = dy + y0
	z1 = dz + z0
	HT=HT+SQRT(((dx)^2+(dy)^2+(dz)^2)*((u0)^2+(v0)^2+(w0)^2))
	IF i NE 0 THEN BEGIN
	  FOR j=0,nvecs-1 DO BEGIN
	    xp1=(x1-xs)/xr*nx
	    yp1=(y1-ys)/yr*ny
	    zp1=(z1-zs)/zr*nz
	    ;PRINT,j, x0(j), x1(j),y0(j),y1(j),z0(j),z1(j)
            plots, [xp0(j), xp1(j)],[yp0(j),yp1(j)],[zp0(j),zp1(j)],/t3d
            ;plots, [x0(j),y0(j),z0(j)],[ x1(j),y1(j),z1(j)],/t3d
          ENDFOR
	ENDIF
	PRINT,x0(0),y0(0),z0(0),x1(0),y1(0),z1(0),HT(0)
ENDFOR

print,HT

END