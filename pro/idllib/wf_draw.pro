; $Id: wf_draw.pro,v 1.2 1993/10/06 18:34:31 doug Exp $
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	WF_DRAW
;
; PURPOSE:
;	This procedure draws weather fronts of various types with
;	spline smoothing.
;
; CATEGORY:
;	Meterology.
;
; CALLING SEQUENCE:
;	WF_DRAW, X, Y
;
; INPUTS:
;  	X, Y:        Vectors of abcissae and ordinates defining the
;		     front to be drawn.
;
; KEYWORD PARAMETERS:
;  	COLD:	     A cold front. The default is a plain line with
;		     no annotations.
;  	WARM:	     A warm front.
;  	OCCLUDED:    An occluded front.
;  	STATIONARY:  A stationary front.
;  	CONVERGENCE: A convergence line.
;  	FRONT_TYPE:  Index of type of front to draw, COLD=1, WARM=2, etc.
;		     Not required if plain line is desired or if an
;		     explicit front type keyword is specified.
;  	THICK:	     Line thickness. The default = 1.0.
;  	INTERVAL:    The spline interpolation interval, in normalized
;		     units. The default = 0.01. Larger values give coarser
;		     approximations to curves, smaller values make more
;		     interpolted points.
;  	COLOR:       The color to use. The default = !P.COLOR.
;  	DEVICE:      If set, X and Y are in device coordinates.
;  	DATA:        If set, X and Y are in data coordinates.
;  	NORM:	     If set, X and Y are in normalized coordinates.
;		     This is the default.
;  	PSYM:	     If set, draw a marker (index = PSYM) on each actual
;		     (X, Y) data point.
;  	SYM_LEN:     The length and spacing factor for front symbols,
;		     in normalized units. The default = 0.15.
;  	SYM_HT:      The height of front symbols, in normalized units.
;		     The default = 0.02.
;
; OUTPUTS:
;	No explicit outputs.
;
; SIDE EFFECTS:
;	Draws objects on current graphics device.
;
; PROCEDURE:
;	Uses parametric spline interpolation to smooth the lines.
;	POLYFILL is used to make the annotations on the front lines.
;
; EXAMPLE:
;	Draw a front given 3 points:
;	  WF_DRAW, [40, 20, 40], [30, 40, 25], /DATA, /COLD
;
; MODIFICATION HISTORY:
;	DMS, RSI.   August, 1993.  Written.
;-


PRO SPLINE_INTERP, x, y, xp, yp, INTERVAL=interval, TORIG = t, TSPLINE=tt
; Interpolate the points X(i),Y(i) using cubic spline interpolation,
;	returning the result in xp, yp.
; Try to make the distance between the resulting points
; approximately equal to INTERVAL.


	;Basic interval, in normalized coords.
if N_ELEMENTS(interval) le 0 then interval = .01
n = n_elements(x)
t = sqrt((x-shift(x,1))^2 + (y-shift(y,1))^2)	;Parametric variable
t(0) = 0.0
r = ceil(t/interval)		;# of pnts for each interv
tt = fltarr(total(r)+1, /nozero)
j = 0L
for i=1, n-1 do begin		;Each interval
    k = long(r(i))		;# of points in this interval
    tt(j) = findgen(k)*(t(i)/k) + t(i-1)  ;Parameteric variable
    t(i) = t(i) + t(i-1)	;Cumulative t value
    j = j + k
    endfor
tt(j) = t(n-1)			;Last t.
xp = SPLINE(t,x,tt)		;Now use SPLINE function
yp = SPLINE(t,y,tt)
end



pro wf_draw, x, y, FRONT_TYPE = pat, COLD=cold, WARM=warm, $
	OCCLUDED=occluded, STATIONARY=stationary, $
	CONVERGENCE=CONVERGENCE, THICK=thick, INTERVAL=interval, $
	COLOR = c, DEVICE = device, DATA = data, NORMAL = normal, $
	PSYM = psym, SYM_LEN = slen, SYM_HT = sht
;

if n_elements(interval) le 0 then interval = 0.01	;Default norm interval
if n_elements(c) eq 0 then c = !p.color
if n_elements(thick) eq 0 then thick = 0.0

if n_elements(pat) le 0 then pat = 0	;Default pattern = none
if keyword_set(COLD) then pat = 1 else $
if keyword_set(warm) then pat = 2 else $
if keyword_set(occluded) then pat = 3 else $
if keyword_set(stationary) then pat = 4 else $
if keyword_set(convergence) then pat = 5

if keyword_set(data) then xp = convert_coord(x,y,/data, /to_norm) $
else if keyword_set(device) then xp = convert_coord(x,y,/device, /to_norm) $
else xp = convert_coord(x,y,/norm, /to_norm)

if n_elements(x) gt 2 then $	;Enough to interpolate?
	spline_interp, xp(0,*), xp(1,*), xs, ys, INTERVAL=interval $
else begin
	xs = xp(0,*)
	ys = xp(1,*)
endelse


;	Length of basic pattern in normalized units.
if n_elements(slen) le 0 then patlen = 0.15 else patlen = slen
;	Pattern height (distance from line)
if n_elements(sht) le 0 then sht = 0.02

PLOTS, xs, ys, thick=thick, COLOR = c, /NORM	;Draw basic line.

pxt = [0, .2, .4]	;Triangle pattern, occupies .4 of each pattern interval
pyt = [0, 1., 0.]
			;half-circle pattern
pxc = [ 0.00, 0.0666667, 0.133333, 0.200, 0.266667, 0.333333,  0.400] ;t
pyc = [ 0.0, 0.50,  0.866,  1.0, 0.866, 0.5, 0.]	;Approx sin(t)

case pat of
1:   BEGIN			;Cold = triangles
	px = pxt * patlen
	py = pyt * sht
	goto, draw_polys
    ENDCASE
2:   BEGIN			;Warm = half circles.
	px = pxc * patlen
	py = pyc * sht
	goto, draw_polys
    ENDCASE
3:   BEGIN			;occlud = alternate tris, circles, same side
	px = [pxt, pxc + 0.45] * patlen
	py = [pyt, pyc] * sht
	goto, draw_polys
    ENDCASE
4:   BEGIN			;stationary = alt circ, triangs, alt sides
	px = [pxt, pxc + 0.45] * patlen
	py = [pyt, -pyc] * sht
	goto, draw_polys
    ENDCASE
5:   BEGIN			;converg = hash marks
	px = [0., .2, .25, .4] * patlen
	py = [0,1,0,-1.] * sht
	patlen = patlen / 2.	;Half as long

draw_polys:
	dx = xs(1:*)-xs		;Dx(i) = x(i+1) - x(i)
	dy = ys(1:*)-ys		;Get derivatives
	dt = sqrt(dx^2 + dy^2)	;Distance between points
	dx = dx / dt
	dy = dy / dt
	nt = n_elements(dt)
	tt = fltarr(nt+1)	;Cumulative distance
	for i=1, nt do tt(i) = tt(i-1) + dt(i-1) ;cumulative t

	t = 0.0			;Pattern start
	l = 0L			;Subscript in dx,dy,dt.
	n = n_elements(px)
	n2 = 2 * n
	xp = fltarr(n2)		;Vertices of polygons
	yp = fltarr(n2)
	tend = tt(nt)

	while t lt tend do begin
	    tx = t + px		;Parameteric values
	    for i=0,n-1 do begin
		if tx(i) ge tend then begin
		    x0 = xs(nt)
		    y0 = ys(nt)
		    xp(i) = x0
		    yp(i) = y0
		endif else begin		    
		    while tt(l) lt tx(i) do l=l+1L  ;Straddle
		    l1 = l-1L > 0L		;tt(l1) < tx(i) <= tt(l)
		    p0 = (tx(i) - tt(l1))/dt(l1)	;Interpol fract
		    x0 = xs(l1) + p0 * (xs(l) - xs(l1))  ;Point on front line
		    y0 = ys(l1) + p0 * (ys(l) - ys(l1))
		    xp(i) = x0 - py(i) * dy(l1)   ;Rotate 90 for symbol pnt
		    yp(i) = y0 + py(i) * dx(l1)
		    endelse
		k = n2-i-1
		xp(k) = x0		;Baseline for return
		yp(k) = y0
		endfor
	    if pat eq 5 then begin	;Special case for convergence
		plots, xp(0:1), yp(0:1), color=c, thick=thick, /NORM
		plots, xp(2:3), yp(2:3), color=c, thick=thick, /NORM
	    endif $
	    else polyfill, xp, yp, color=c, /NORM  ;Otherwise fill polygon
	    t = t + patlen
	endwhile
    ENDCASE	    
else: q=0
endcase

if n_elements(psym) gt 0 then begin	;Mark data points?
    if n_elements(xp) gt 0 then PLOTS, xp, /NORM, PSYM=psym, COLOR=c $
	else PLOTS, x,y, /NORM, PSYM=psym, COLOR=c
    endif
end



pro test_wf_draw
map_set, limit = [25, -125, 50, -70], /grid, /usa
wf_draw, [ -120, -110, -100], [30, 50, 45], /COLD, /DATA, THICK=2
wf_draw, [ -80, -80, -75], [ 50, 40, 35], /WARM, /DATA, THICK=2
wf_draw, [ -80, -80, -75]-10., [ 50, 40, 35], /OCCLUDED, /DATA, THICK=2
wf_draw, [ -120, -105], [ 40,35], /STATION, /DATA, THICK=2
wf_draw, [ -100, -90, -90], [ 30,35,40], /CONVERG, /DATA, THICK=2

names = ['None','Cold', 'Warm', 'Occluded', 'Stationary', 'Convergent']
x = [.015, .30]
y = 0.04
dy = 0.05
ty = n_elements(names) * dy + y
polyfill, x([0,1,1,0]), [0, 0, ty, ty], /NORM, color=!p.background
for i=0, n_elements(names)-1 do begin
	wf_draw, x, y, /NORM, FRONT_TYPE=i, THICK=2
	xyouts, x(1)+0.015, y(0), names(i), /NORM, CHARS=1.5
	y = y + dy
	endfor
end
