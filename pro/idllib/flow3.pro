; $Id: flow3.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro blob3, x0, y0, z0		;Draw a blob at (x0, y0, z0) (may be arrays)
n = n_elements(x0)-1

hfact = .010		;Size of heads
u = 1./[!x.s(1), !y.s(1),!z.s(1)] * hfact	;Size in data coords
u0 = [[1,0,0],[0,1,0],[0,0,1]]	;Directions
for i=0,n-1 do begin
	p0 = [ x0(i), y0(i), z0(i)]
	for j=0,2 do begin
		v = u * u0(*,j)
		plots,[[p0+v],[p0-v]],/T3D
		endfor
	endfor
end


pro arrow3, x0, y0, z0, x1, y1, z1, flags, arrowsize
; Draw an arrow-head at the
; ends of vectors (x0,y0,z0) to (x1, y1, z1).  Params may be arrays.
; Flags(i) = 1 to draw head at end of ith vector.
;
dx = x1-x0
dy = y1-y0
dz = z1-z0
zz = sqrt(dx^2 + dy^2 + dz^2) > 1e-6	;Length


dx = dx/zz		;Cos th
dy = dy/zz		;Sin th

u0 = [-.866/!x.s(1), -.5/!y.s(1), 0]*arrowsize	;Arrow head vectors (XY plane)
u1 = [-.866/!x.s(1),  .5/!y.s(1), 0]*arrowsize

n = n_elements(x0)-1

for i=0,n-1 do if flags(i) then begin	;Heads?
	xx = x1(i)
	yy = y1(i)
	zz = z1(i)
	dx0 = dx(i)
	dy0 = dy(i)
	xx0 = xx + dx0*u0(0) - dy0 * u0(1)
	yy0 = yy + dx0*u0(1) + dy0 * u0(0)
	xx1 = xx + dx0*u1(0) - dy0 * u1(1)
	yy1 = yy + dx0*u1(1) + dy0 * u1(0)
	plots, [xx0,xx,xx1],[yy0,yy,yy1],[zz,zz,zz],/t3d
	endif
end


pro flow3, vx, vy, vz, nvecs=nvecs, nsteps = nsteps, len = len, blob=blob, $
	sx = sx, sy = sy, sz = sz, arrowsize = arrowsize
;+
; NAME:
;	FLOW3 - Draw 3D flow/velocity field.
; PURPOSE:
;	Draw lines representing a 3D flow/velocity field.
; CATEGORY:
;	Graphics.
; CALLING SEQUENCE:
;	FLOW3, vx, vy, vz
; INPUTS:
;	Vx, Vy, Vz = 3D arrays containing X, Y, and Z components
;		of the field.
; KEYWORD PARAMETERS:
;	Sx, Sy, Sz = Optional vectors containing the starting coordinates
;	  of the flow lines. If omitted random starting points are chosen.
;	Nvecs = Number of random flow lines to draw (Default = 200).
;	  Only used if Sx, Sy, Sz are not present.
;	Len = Length of each step used to follow flow lines.  Default = 2.0
;	  Expressed in units of largest field vector, i.e. the length of
;	  the longest step is set to len times the grid spacing.
;	Nsteps = number of steps used to follow the flow lines.  Default =
;	  largest dimension of vx / 5.
;	Blob = 1 to draw a blob at the beginning of each flow line and
;	  suppress the arrows.
;	Arrowsize = size of arrowheads, default = 0.05
; OUTPUTS:
;	None.  Graphics are produced on the currently selected graphics
;	device.
; COMMON BLOCKS:
;	None.
; RESTRICTIONS:
;	Works best with Z buffer output device.
; PROCEDURE:
;	The 3D scaling system must be set before calling this procedure.
;	For example:  scale3, xr=[0,nx-1], yr=[0,ny-1], zr = [0,nz-1]
;	where nx, ny, and nz are the 1st, 2nd, and 3rd dimensions of
;	VX, VY, and VZ.
; MODIFICATION HISTORY:
;	DMS - RSI, Nov, 1991.
;-



s = size(vx)
if s(0) ne 3 then message,'FLOW3: Vx, Vy, and Vz must be 3D arrays'
nx = s(1)
ny = s(2)
nz = s(3)
if n_elements(nsteps) le 0 then nsteps = (nx > ny > nz) /5

if n_elements(sx) le 0 then begin	;Starting points specified?
	if n_elements(nvecs) le 0 then nvecs = 200
	x1 = randomu(seed, nvecs) * nx
	y1 = randomu(seed, nvecs) * ny
	z1 = randomu(seed, nvecs) * nz
endif else begin
	x1 = float(sx)
	y1 = float(sy)
	z1 = float(sz)
	nvecs = n_elements(x1) < n_elements(y1) < n_elements(z1)
endelse

if n_elements(len) le 0 then len = 2.0  ;Default length
if n_elements(arrowsize) le 0 then arrowsize = 0.05

zscale = len/max(sqrt(vx^2+vy^2+vz^2))  ;Make max step = len

flags  = replicate(1b,nvecs)	;TRUE if flow inside cube

for i=0,nsteps do begin		;Draw each element
	x0 = x1
	y0 = y1
	z0 = z1
	x1 = interpolate(vx, x0,y0,z0)*zscale + x0
	y1 = interpolate(vy, x0,y0,z0)*zscale + y0
	z1 = interpolate(vz, x0, y0, z0)*zscale + z0
	out = (x1 lt 0) or (x1 ge nx) or (y1 lt 0) or (y1 ge ny) or $
		(z1 lt 0) or (z1 ge nz)
	flags = flags and (1b-out)
	if (i ne 0) or keyword_set(blob) then $
	   for j=0,nvecs-1 do if flags(j) then $
		plots, [x0(j), x1(j)],[y0(j),y1(j)],[z0(j),z1(j)],/t3d
        if keyword_set(blob) and (i eq 0) then $
		blob3, x0, y0, z0
	if ((i eq 0) or (i eq nsteps)) and (keyword_set(blob) eq 0) then $
		arrow3, x0, y0, z0, x1, y1, z1, flags, arrowsize
	endfor

end
