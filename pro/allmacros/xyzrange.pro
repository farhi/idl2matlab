PRO xyzrange,xr,yr,zr,x3d,y3d,z3d,x1d,y1d,z1d

x1d=FINDGEN((xr(1)-xr(0))/xr(2)+1)*xr(2)+xr(0)
y1d=FINDGEN((yr(1)-yr(0))/yr(2)+1)*yr(2)+yr(0)
z1d=FINDGEN((zr(1)-zr(0))/zr(2)+1)*zr(2)+zr(0)

nx=N_ELEMENTS(x1d)
ny=N_ELEMENTS(y1d)
nz=N_ELEMENTS(z1d)

x3d=FLTARR(nx,ny,nz)
y3d=FLTARR(nx,ny,nz)
z3d=FLTARR(nx,ny,nz)

FOR iy=0,ny-1 DO BEGIN
    FOR iz=0,nz-1 DO BEGIN

      x3d(*,iy,iz)=x1d

    ENDFOR
ENDFOR

FOR ix=0,nx-1 DO BEGIN
    FOR iz=0,nz-1 DO BEGIN

      y3d(ix,*,iz)=y1d

    ENDFOR
ENDFOR

FOR ix=0,nx-1 DO BEGIN
  FOR iy=0,ny-1 DO BEGIN

      z3d(ix,iy,*)=z1d

  ENDFOR
ENDFOR

END