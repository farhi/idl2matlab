; $Id: slicer.pro,v 1.17 1995/06/30 20:17:42 dave Exp $

; MODIFICATION HISTORY:
; bmh - 10/14/93 - The following minor bug fixes.
;                  When no elements are found during an iso-surface display,
;                  an error message was displayed to an invalid device name.
;                  When no oblique slices are selected, the slicer_oblique
;                  should return.
;
;
; Conventions:
; Faces = faces of cube.  Numbered 0 to 5:
;		0 = X = c0(0)  (Corner 0)
;		1 = Y = c0(1)
;		2 = Z = c0(2)
;		3 = X = c1(0)  (Corner 1)
;		4 = Y = c1(1)
;		5 = Z = c1(2)
; Orientations:
;	6 = reset to default.
;	0 = xy exchange, 1 = xz exchange, 2 = yz exchange.
;	3 = x reverse, 4 = y reverse, 5 = z reverse.
; Vertex indices:	Faces
;	0	0,0,0   0,1,2
;	1	N,0,0	1,2,3
;	2	0,N,0	0,2,4
;	3	N,N,0	2,3,4
;	4	0,0,N	0,1,5
;	5	N,0,N	1,3,5
;	6	0,N,N	0,4,5
;	7	N,N,N	3,4,5
;
; Edge Index	Vertex Indices
;	0	0-1
;	1	1-2
;	2	2-3
;	3	3-1
;	4	0-4
;	5	1-5
;	6	2-6
;	7	3-7
;	8	4-5
;	9	5-6
;	10	6-7
;	11	7-4
;
; Modes:
;	0 = Slices
;	1 = Cube
;	2 = Cut
;	3 = Isosurface
;	4 = Probe
;	5 = rotations


function p_inside_poly, polyv, p
;	polyv = (2,n) array of polygon vertices, either clockwise or
;		counter-clockwise order.  Polygon must be convex.
;	p = (2) point coordinates.
;	return 0 if point is outside polygon, 1 if inside.
;
x = float(p(0))
y = float(p(1))
np = n_elements(polyv)/2		;# of vertices
x1 = polyv(0,np-1)		;Start at last point
y1 = polyv(1,np-1)

pos = -1
for i=0,np-1 do BEGIN
	x2 = polyv(0,i)
	y2 = polyv(1,i)
	k = (x*(y1-y2) + y * (x2-x1) + x1*y2-y1*x2)  ;Side of line point is on
	if k eq 0 THEN BEGIN	;On line?
	    if (y lt (y1< y2)) or (y gt (y1 > y2)) or  $  ;Check more
		(x lt (x1 < x2)) or (x gt (x1 > x2)) THEN return,0
	ENDIF ELSE BEGIN		;Not on line
		k = k gt 0		;1 if on right side
		if pos lt 0 then pos = k
		if pos ne k THEN return,0
	ENDELSE
	x1=x2		;Previous vertex
	y1=y2
	ENDFOR
return,1
end




Function slicer_plane_int, dummy
; Return the intersections of the plane with the volume cube.
; V(3,12) = intersections of plane with the 12 edges.
; Flags(12) = 1 if there is an intersection at that edge.
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

Jn = sl.orthop(0:2)			;Plane eqn
Jd = sl.orthop(3)

v = fltarr(3,12)
k = 0

for i=0,11 do begin			;Faces
    p0 = sl.p0(*,sl.edges(0,i))		;Beginning of edge
    p1 = sl.p0(*,sl.edges(1,i))		;End of edge
    lv = p1-p0
    t = total(lv * Jn)
    if t ne 0 then begin
	t = -(jd + total(p0 * Jn)) / t
	p = p0 + t * lv			;Point of intersection
	d = min((p - p0) * (p1-p))
	if d ge 0.0 then begin		;Within edge?
	    v(0,k) = p			;Yes, save intersection
	    k = k + 1
	    endif		  
	endif				;T ne 0
    endfor		    		;i

; Sort into order:
d = max(abs(Jn), m)			;Largest plane coeff = what we ignore
u = v([m+1, m+2] mod 3, 0:k-1)		;Get other 2 coords
a = fltarr(k)				;Angle measure
d = min(u(1,*), dmin)			;Get lowest point
u_dx = u(0,*)-u(0,dmin)
u_dy = u(1,*)-u(1,dmin)
zero_ind = Where((u_dx EQ 0.0) AND (u_dy EQ 0.0))
if (zero_ind(0) GE 0L) THEN u_dx(zero_ind) = 1.0
a=atan(u_dy, u_dx)
zero_ind=0 & u_dx=0 & u_dy=0
a(dmin) = -100.				;Anchor = first
n = sort(a)				;Go around anchor point & back
return, v(*, [n, n(0)])
end




PRO SLICER_PLAYBACK, FILE = file, Commands
; Play back a journal.  Commands are either in the designated file
; or in the string array Commands".
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
COMMON slicer_common1, old_slice, d0, z0, az, el

if n_elements(file) gt 0 THEN BEGIN	;Read from file
	OPENR, unit, /GET_LUN, file, ERROR = i
	if i ne 0 then begin	;OK?
		widget_control, sl.file_text(1), set_value = !ERR_STRING
		return
		ENDIF
	commands = strarr(100)		;Read up to 100 lines
	i = 0
	a = ''
	while not eof(unit) do begin
		readf, unit, a
		commands(i) = a
		i = i + 1
		endwhile
	ncommands = i
	free_lun, unit
ENDIF ELSE ncommands = n_elements(commands)

IF mode ne 7 THEN BEGIN			;Not our mode?
	if sl.mode_bases(mode) ne 0 THEN $  ;Remove panel if mapped
		WIDGET_CONTROL, sl.mode_bases(mode), MAP=0
	mode = 7			;New mode = ours
	WIDGET_CONTROL, sl.mode_bases(mode), MAP=1
	ENDIF

pars = fltarr(10)

for i=0, ncommands-1 do begin		;Each command
	s = strtrim(strcompress(commands(i)))	;Parse it, extracting fields
	j = 0
	m = 0
	while j lt strlen(s) do begin	;While string left
		k = strpos(s, ' ', j)	;Find next blank
		if k le 0 then k = strlen(s)  ;if none, go to end of string
		if j eq 0 then cmd = strmid(s,0,k) $
		else begin pars(m) = strmid(s,j,k) & m=m+1 & endelse
		j = k+1
		endwhile
	WIDGET_CONTROL, sl.file_text(1), SET_VALUE = strmid(s,0,32)
	case strupcase(cmd) of		;Interpret commands.....
"UNDO": slicer_undo
"ORI":  BEGIN    ;AXIS(3), AXIS_REVERSE(3), ROTATIONS(2)
	sl.axex = pars(0:2)
	sl.axrev = pars(3:5)
	sl.rotation = pars(6:7)
	for j=0,1 do WIDGET_CONTROL, sl.rslide(j), SET_VALUE = sl.rotation(j)
	WIDGET_CONTROL, sl.rslide(2), SET_VALUE=string(pars(8))  ;Aspect
	SLICER_ORIENTATION
	ENDCASE
"TRANS": BEGIN   ;On/Off Threshold(%)
	sl.trans = pars(0)
	if sl.trans eq 0 then pars(1) = 100
	sl.threshold = pars(1) /100. * sl.nc3
	WIDGET_CONTROL, sl.threshold_slider, SET_VALUE = pars(1)
	ENDCASE
"SLICE": BEGIN	  ;Axis, slice_value, interp, expose, 0 for orthogonal
		;Azimuth, Elev, interp, expose, 1, x0, y0, z0 for oblique
	sl.interp = pars(2)
	sl.expose = pars(3)
	if pars(4) eq 0 then begin	;Orthogonal slices?
	    WIDGET_CONTROL, sl.draw_butt(sl.expose), /SET_BUTTON
	    SLICER_DRAW, pars(0), pars(1)
	ENDIF ELSE BEGIN		;Oblique slice
	    az = pars(0)
	    el = pars(1)
	    s = sin(el * !DTOR)
	    sl.orthop(0) = sin(-az * !dtor) * s
	    sl.orthop(1) = cos(-az * !dtor) * s
	    sl.orthop(2) = cos(el * !dtor)
	    z0 = pars(5:7)
	    sl.orthop(3) = -total(sl.orthop * z0)
	    d0 = slicer_plane_int()
	    slicer_oblique
	ENDELSE
	ENDCASE
"COLOR": BEGIN	 ;Table_num (-1 = present), low, high, shading
	sl.stretch = pars(1:2)
	sl.shading = pars(3)/100.
	for j=0,2 do WIDGET_CONTROL, sl.cslide(j), SET_VALUE = pars(j+1)
	slicer_colors, 	pars(0)
	ENDCASE
"ISO":  BEGIN		;value, hi/lo
	sl.isop.value = pars(0) / 100. * (sl.amax-sl.amin) + sl.amin
	sl.isop.hi_lo = pars(1)
	DO_ISOSURFACE
	ENDCASE
"ERASE":  slicer_erase
"WAIT":  wait, pars(0)
"CUBE": BEGIN		;mode (1 = block, 0 = cutout), cut_ovr, interp,
			; start_coords(3), end_coords(3)
	mode = pars(0)
	sl.cut_ovr = pars(1)
	sl.interp = pars(2)
	sl.p0cube = reform(pars(3:8), 3,2)
	DO_CUBE	
	ENDCASE
    ENDCASE
  ENDFOR
mode=7
WIDGET_CONTROL, sl.file_text(1), SET_VALUE = 'Playback Done'
end

; Journal events if journal file is open.
PRO SLICER_JOURNAL, name, params
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

if sl.journal le 0 then return
if n_elements(params) le 0 then params = 0.
printf, sl.journal, name, params, format='(A, 1x, 10F10.3)'
end


PRO SLICER_UNDO		;Undo last operation
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

SLICER_JOURNAL, "UNDO"
if n_elements(zb_last) le 1 then return
set_plot,'Z'
tmp = tvrd(/WORDS, CHANNEL=1)  ;Read depth buffer & swap
tv, zb_last, CHANNEL=1, /WORDS
zb_last = temporary(tmp)
tmp = tvrd()		;Swap them
tv, z_last
slicer_show, z_last
z_last = tmp
end


PRO slicer_orientation, i	;i = orientation
;  Set the New Orientation
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
if n_elements(i) gt 0 THEN BEGIN
	if i le 2 THEN BEGIN
		j = 2 * i
		ll = ([0,1,0,2,1,2])(j:j+1)  ;axes to swap
		t = sl.axex(ll(0)) & sl.axex(ll(0)) = sl.axex(ll(1)) & 
		sl.axex(ll(1)) = t
	ENDIF ELSE if i eq 6 THEN BEGIN
		sl.axex = [0,1,2]		;default transformation
		sl.axrev = intarr(3)
	ENDIF ELSE sl.axrev(i-3) = 1-sl.axrev(i-3)  ;reverse
ENDIF		

d = [ 0., dims(0), 0., dims(1), 0., dims(2)]
f = 1.0
WIDGET_CONTROL, sl.rslide(2), GET_VALUE=s
ON_IOERROR, bad_aspect
f = (float(s))(0)
if (f le 0.0) then goto, bad_aspect
IF f gt 1. THEN BEGIN
	x = (f-1)/2.
	d(0) = [-x * dims(0), (x+1) * dims(0), -x * dims(1), (x+1) * dims(1)]
ENDIF ELSE BEGIN
	x = (1-f)/2
	d(4) = [-x * dims(2), (x+1) * dims(2)]
ENDELSE
bad_aspect:
SLICER_JOURNAL, "ORI", [sl.axex, sl.axrev, sl.rotation, f ]

for i=0,2 do if sl.axrev(i) THEN BEGIN	;Swap endpoints for reversed axes
	j=i*2
	t = d(j) & d(j) = d(j+1) & d(j+1) = t
	ENDIF

!x.type = 0		;make sure its linear
scale3, xrange=d(0:1), yrange=d(2:3), zrange=d(4:5), ax = sl.rotation(0), $
	az = sl.rotation(1)

k = 1		;current y axis
if sl.axex(0) ne 0 THEN BEGIN	;swap x?
	if sl.axex(0) eq 1 THEN BEGIN &	t3d, /XYEXCH & k = 0
	ENDIF ELSE t3d,/XZEXCH
ENDIF

if k ne sl.axex(1) THEN t3d,/YZEXCH
slicer_erase
if mode le 2 THEN draw_orientation
sl.pt_inverse = invert(!p.t)
end





PRO slicer_oblique		;Do an oblique slice
;	Plane eqn is in sl.orthop
;	d0 is the intersections of the plane with the edges of the volume
;		cube.
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
COMMON slicer_common1, old_slice, d0, z0, az, el

if n_elements(d0) le 3 then return		;Anything?

SLICER_JOURNAL, "TRANS", [sl.trans, sl.threshold * 100. / sl.nc3]
SLICER_JOURNAL, "SLICE", [az, el, sl.interp, sl.expose, 1., z0 ]

widget_control, sl.pos_text, set_value = 'Extracting Oblique Slice'
set_plot,'Z'
z = (z_last = tvrd())			;save previous image & z
zb = (zb_last = tvrd(CHANNEL=1, /WORDS))
erase
polyfill, d0, /T3D, /DATA	;Mark the slice
z1 = tvrd(CHANNEL=1, /WORDS)	;Read depth buffer

if sl.expose then points = where((z1 lt zb_last) and (z1 ne z1(0,0))) $
	else points = where(z1 gt zb_last)

if points(0) lt 0 then begin
	z=z_last
	zb = zb_last
	goto, done
	endif

widget_control, sl.pos_text, set_value = $
	STRING(n_elements(points), FORMAT="(i6, ' points')")

ones = replicate(1., n_elements(points))

	;Make 3D homogen array in  data array subscripts coords
v = [ [(points mod !d.x_size) / float(!d.x_size)], $ ;Norm X
	[(points / !d.x_size)   / float(!d.y_size)], $ ;Norm Y
	[ z1(points)/65530. + 0.5], $		;Normalized Z coords
	[ ones ]]				;Homogeneous coord

v = ((temporary(v) # sl.pt_inverse) - $		;To v subscripts
		(ones # [!x.s(0), !y.s(0), !z.s(0)])) / $
		(ones # [ !x.s(1), !y.s(1), !z.s(1)])


;		Either interpolate or pick nearest neighbor
if sl.interp THEN v = interpolate(a, v(*,0), v(*,1), v(*,2)) $
    else v = a(long(temporary(v)) # [1, dims(0), dims(0) * dims(1)])

v = bytscl(temporary(v), max=sl.amax, min=sl.amin, top = sl.nc3-1) ;face data

if sl.trans THEN BEGIN		;Transparency?  Remove those under thresh.
	good = where(v ge sl.threshold)
	v = v(good)
	points = points(good)
	endif

dummy = max(abs(sl.orthop(0:2)), kmax)	;Axis with smallest variation
if kmax ne 0 then z(points) = v + byte(kmax * sl.nc3)  $ ;Shade it
else z(points) = v

zb(points) = z1(points)		;Update depth buffer

done: tv, z					;Now show it
tv, zb, /WORDS, CHANNEL=1		;and update depth buffer

widget_control, sl.pos_text, set_value = $
	  STRING(z0, az, el, FORMAT="('(',3f5.1,') A=', i4, ' E=',i4)")
slicer_show
end




PRO slicer_draw, ax, slice		;draw a slice.
;  ax = axis, 0 for x, 1 for Y, 2 for Z. slice = plane number.
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

WIDGET_CONTROL, sl.base, /HOURGLASS
SLICER_JOURNAL, "TRANS", [sl.trans, sl.threshold * 100. / sl.nc3]
SLICER_JOURNAL, "SLICE", [ax, slice, sl.interp, sl.expose, 0.]

set_plot,'Z'
z_last = tvrd()			;save previous image & z
zb_last = tvrd(CHANNEL=1, /WORDS)

d0 = [0,0,0]
d1 = dims -1
d0(ax) = slice
d1(ax) = slice

if sl.expose THEN erase		;Get a clean slice
				;extract & scale the slice
offset = byte(ax*sl.nc3)	;bias for this slice
p = bytscl(a(d0(0):d1(0), d0(1):d1(1), d0(2):d1(2)), $
	max=sl.amax, min=sl.amin, top = sl.nc3-1)

if sl.trans THEN t = (sl.threshold > 1) + offset $  ;lower limit
	ELSE t = 0
if ax ne 0 THEN p = p + offset	;add bias
d1 = dims-1
s = replicate(slice, 4)

case ax of
0:	polyfill, s, [0,d1(1),d1(1),0],[0,0,d1(2),d1(2)],/T3D,$
		pat=reform(p, dims(1), dims(2), /OVER), $
		image_coord = [0,0, d1(1),0, d1(1),d1(2), 0,d1(2)], $
		image_interp= sl.interp, trans=t
1:	polyfill, [0,d1(0),d1(0),0],s,[0,0,d1(2),d1(2)],/T3D,$
		pat=reform(p, dims(0), dims(2), /OVER), $
		image_coord = [0,0, d1(0),0, d1(0),d1(2), 0,d1(2)], $
		image_interp= sl.interp, trans=t
2:	polyfill,[0,d1(0),d1(0),0],[0,0,d1(1),d1(1)],s,/T3D,$
		pat=reform(p, dims(0), dims(1), /OVER),$
		image_coord = [0,0, d1(0),0, d1(0),d1(1), 0,d1(1)], $
		image_interp= sl.interp, trans=t
	ENDCASE

if sl.expose THEN BEGIN
	z = tvrd(/WORDS, CHANNEL=1)	;The new slice
	pnts = where((z gt zb_last) + (z eq z(0,0))) ;where we display prev
	if n_elements(pnts) gt 1 then begin
		z(pnts) = zb_last(pnts)		;New Z
		tv, z, /WORDS, CHANNEL=1
		z = tvrd()			;New display
		z(pnts) = z_last(pnts)
		tv, z
		ENDIF
	ENDIF
slicer_show
end

PRO slicer_colors, table	;load our color table
; Table = index of color table to load, -1 to retain present.
; The color palette is repeated 3 times, once for each of the possible face
; directions.  The colors indices:
;  3 * sl.nc3  = red
;  3 * sl.nc3 + 1 = green
;  3 * sl.nc3 + 2 = blue
;  3 * sl.nc3 + 3 = white

COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

v = [0, .5, 1.]		;Shading of the planes.
if n_params() eq 0 then table = -1
if table ge 0 then loadct, /SILENT, table
SLICER_JOURNAL, "COLOR", [table, sl.stretch, 100. * sl.shading]

if sl.stretch(0) eq sl.stretch(1) then $
	q = lindgen(sl.nc3) * sl.nc1 / (sl.nc3-1) $
else BEGIN		;Use contrast max and min
	s = 100./(sl.stretch(1)-sl.stretch(0))
	int = -s * sl.stretch(0)/100.
	q = long(3*(findgen(sl.nc3) * s + int))
ENDELSE

r_curr = bytarr(3*sl.nc3)
g_curr = r_curr
b_curr = r_curr
s = 1.-sl.shading
for i=0,2 do begin		;3 faces
	v0 = sl.shading * v(i) * 255
	r_curr(i*sl.nc3) = s * r_orig(q) + v0
	g_curr(i*sl.nc3) = s * g_orig(q) + v0
	b_curr(i*sl.nc3) = s * b_orig(q) + v0
	endfor
tvlct, r_curr, g_curr, b_curr
	; load last 4 colors as red, green, blue, white
tvlct, [255,0,0,255],[0,255,0,255],[0,0,255,255],3*sl.nc3
end

PRO DO_ISOSURFACE		;Draw the isosurface
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed


WIDGET_CONTROL, sl.base, /HOURGLASS
SLICER_JOURNAL, "ISO", [(sl.isop.value-sl.amin)/(sl.amax-sl.amin)*100, $
		sl.isop.hi_lo]
set_plot,'Z'
widget_control, sl.pos_text, set_value='Computing Polygons'
shade_volume, a, sl.isop.value, verts, polys, $
	low = sl.isop.hi_lo
if n_elements(verts) eq 0 then begin
	widget_control, sl.pos_text, set_value = $
	'No surface at this value'
	set_plot,sl.gdev
endif else begin
  widget_control, sl.pos_text, set_value = $
	strtrim((size(verts))(2),2)+' Vertices, ' + $
	strtrim((size(polys))(1)/4,2) + ' Polygons.'
  z_last = tvrd()			;Save old display
  zb_last = tvrd(CHANNEL=1, /WORDS)
  SET_SHADING, Values=[0, sl.nc3-1]
  b = polyshade(verts,polys,/T3D,top=sl.nc3-1)
  verts = 0 & polys = 0		;Free space
  slicer_show
endelse
end



PRO draw_cube, c0, c1, faces, color
;	draw a cube whose opposite corners are [c0(0),c0(1),c0(2)],
;	and [c1(0), c1(1), p2(3)].
;	color = drawing color.
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
p0 = intarr(3,8)
p1 = float(p0)
cc = [[c0],[c1]]
for i=0,7 do BEGIN
	p0(0,i) = [ cc(0, i and 1), cc(1, (i/2 and 1)), cc(2, (i/4 and 1))]
	p1(0,i) = convert_coord(p0(*,i), /T3D,/TO_DEVICE,/DATA)
	ENDFOR

if n_elements(color) le 0 THEN color = sl.nc1
f = sl.facevs
flags = bytarr(8,8)	;line flags, dont draw same line twice
for i=0,n_elements(faces)-1 do BEGIN
    ff = [ f(*,faces(i)), f(0,faces(i))]  ;Vertex indices
    for j=0,3 do begin
	k = ff(j) < ff(j+1) & l = ff(j) > ff(j+1)
	if not flags(k,l) then plots, p1(*,[k,l]), color = color, /dev
	flags(k,l) = 1
	ENDFOR
    ENDFOR
end
 




PRO slicer_erase		;draw the background
;	call with no params to erase all.
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

SLICER_JOURNAL, "ERASE"
set_plot,'Z'
erase
sl.p1 = convert_coord(sl.p0, /T3D, /TO_DEVICE, /DATA)  ;save dev coords
s = strarr(8)
for i=0,7 do BEGIN
	s(i) = string(sl.p0(*,i),format ="(' (',i0,',',i0,',',i0,')')")
	ENDFOR
junk = max(sl.p1(2,*), j)
sl.v_close = j			;index of closest vertex
p = where(sl.facevs eq sl.v_close)/4   ;indices of closest verts
colors = [ sl.nc1, sl.nc3*3+1]  ;white, green
for i=0,5 do BEGIN		;draw faces
	k= (where(p eq i))(0) lt 0 	;1 = not close, 0 = close
	draw_cube, [0,0,0], dims-1, i, colors(k)
	ENDFOR
for i=0,7 do xyouts, sl.p1(0,i),sl.p1(1,i),/DEVICE,s(i), $
	color=colors(i ne sl.v_close)
z_last = tvrd()
zb_last = tvrd(CHANNEL=1, /WORDS)
slicer_show
end



Function slicer_plane_int, dummy
; Return the intersections of the plane with the volume cube.
; V(3,12) = intersections of plane with the 12 edges.
; Flags(12) = 1 if there is an intersection at that edge.
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

Jn = sl.orthop(0:2)			;Plane eqn
Jd = sl.orthop(3)

v = fltarr(3,12)
k = 0

for i=0,11 do begin			;Faces
    p0 = sl.p0(*,sl.edges(0,i))		;Beginning of edge
    p1 = sl.p0(*,sl.edges(1,i))		;End of edge
    lv = p1-p0
    t = total(lv * Jn)
    if t ne 0 then begin
	t = -(jd + total(p0 * Jn)) / t
	p = p0 + t * lv			;Point of intersection
	d = min((p - p0) * (p1-p))
	if d ge 0.0 then begin		;Within edge?
	    v(0,k) = p			;Yes, save intersection
	    k = k + 1
	    endif		  
	endif				;T ne 0
    endfor		    		;i

; Sort into order:
d = max(abs(Jn), m)			;Largest plane coeff = what we ignore
u = v([m+1, m+2] mod 3, 0:k-1)		;Get other 2 coords
a = fltarr(k)				;Angle measure
d = min(u(1,*), dmin)			;Get lowest point
;p0 = u(*,dmin)
;for i=0,k-1 do if i ne dmin then begin	;Get angles
;	d = u(*,i) - p0			;Dx,dy
;	d0 = total(abs(d))		;abs(dx) + abs(dy)
;	if d0 ne 0. then begin
;	    t = d(1) / d0		;Proportional to angle w. horizontal
;	    if d(0) lt 0. then t = 2. - t $
;	    else if d(1) lt 0. then t = t + 4
;	    endif else t = 0.
;	a(i) = t
;	endif
u_dx = u(0,*)-u(0,dmin)
u_dy = u(1,*)-u(1,dmin)
zero_ind = Where((u_dx EQ 0.0) AND (u_dy EQ 0.0))
if (zero_ind(0) GE 0L) THEN u_dx(zero_ind) = 1.0
a=atan(u_dy, u_dx)
zero_ind=0 & u_dx=0 & u_dy=0
a(dmin) = -100.				;Anchor = first
n = sort(a)				;Go around anchor point & back
return, v(*, [n, n(0)])
end

PRO slicer_show, image
;move the Z buffer to the X display.  leave device set to X.
; if parameter is present, show it rather than reading the Z buffer
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

if n_params() eq 0 then begin
	set_plot,'Z'
	image = tvrd()
	endif
set_plot,sl.gdev
wset, sl.window
tv, image
sl.cube_on = 0
end

PRO draw_orientation	;draw the orientation cube in the small window
; Draw the outline of the 3 frontmost faces of the main cube.
; Draw the fixed axis plane in green.
; If the mode is cut or cube, draw the selected cube.  Draw the back faces
;	in blue, and label the selection points.

COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
COMMON slicer_common1, old_slice, d0, z0, az, el

i = sl.mode_bases(mode)
widget_control, i, get_uvalue = draw		;the widget id
widget_control, draw(0), get_value=window	;the window number
wset, window
device, set_graph = 3				;Copy mode
kc = 3 * sl.nc3 + 1				;Drawing color
erase

if mode le 0 then begin			;Single slice?
    if sl.ortho eq 0 then begin		;Orthogonal?
	mark_oblique, kc
	goto, done
    endif
	z = [0,0,0]
	d1 = dims-1
  endif else begin			;Block.
	z = sl.p0cube(*,0) < sl.p0cube(*,1)
	d1 = sl.p0cube(*,0) > sl.p0cube(*,1)
  endelse
					; draw fixed plane:
nlines = 6
p = z
d = (d1-z) / float(nlines-1)

p(fixed) = (d1(fixed) + z(fixed))/2.
d(fixed) = 0.0

for i=0,nlines-1 do BEGIN		;draw fixed direction
	xx = replicate(p(0),2)
	yy = replicate(p(1),2)
	zz = replicate(p(2),2)
	if fixed ne 0 THEN plots, [z(0),d1(0)], yy,zz, /T3D, COLOR=kc, /DATA
	if fixed ne 1 THEN plots, xx, [z(1),d1(1)],zz, /T3D, COLOR=kc, /DATA
	if fixed ne 2 THEN plots, xx, yy, [z(2),d1(2)],/T3D, COLOR=kc, /DATA
	p = p + d
	ENDFOR

if mode ne 0 then begin		;Do cube
	draw_cube, sl.p0cube(*,0), sl.p0cube(*,1), indgen(6), kc+1  ;all faces
		;Close faces in white
	draw_cube, sl.p0cube(*,0), sl.p0cube(*,1), $
			where(sl.facevs eq sl.v_close)/4
	for i=0,1 do begin		;Label them
		p = convert_coord(sl.p0cube(*,i), /T3D, /TO_DEV, /DATA)
		xyouts, p(0), p(1), strtrim(i,2), /device
		endfor
	endif

done: 
draw_cube, [0,0,0], dims-1, where(sl.facevs eq sl.v_close)/4  ;draw close faces
wset, sl.window
return
end


PRO mark_oblique, color			;Draw an oblique slice
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
COMMON slicer_common1, old_slice, d0, z0, az, el

if n_elements(d0) lt 3 then return

plots, d0, /T3D, COLOR=color, /DATA
z1 = [[z0], [z0]]
for i=0,2 do begin
	z2 = z1
	z2(i,0) = 0. & z2(i,1) = dims(i)-1.
	plots, z2, /T3D, COLOR = color, /DATA
	endfor
return
end



PRO mark_slice, ev		;mark a horizontal or vertical slice

COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
COMMON slicer_common1, old_slice, d0, z0, az, el

;		loop until the mouse is released.
oldbuttons = sl.lbuttons
sl.lbuttons = oldbuttons and (not ev.release) or (ev.press)
press = ev.press and 1			;New left button press?
p = float([ev.x, ev.y])			;device coords of mouse

if (n_elements(old_slice) le 0) or (ev.press ne 0) then old_slice = -1
kslice = -1				;assume no face
kc = sl.xcol				;XOR drawing color

if sl.lbuttons and 1 THEN BEGIN		;Marking a face?
    f = where(sl.facevs eq sl.v_close)/4  ;3 faces to check that are front
    d1 = dims-1
    z1 = intarr(3)
    d = ''
    for i=0,2 do BEGIN			;find the face
	j = f(i)			;face index
	p1 = sl.p1(0:1, sl.facevs(*,j))	;vertices
	if p_inside_poly(p1, p) THEN BEGIN
	    face = j
	    p = p / [ !d.x_size, !d.y_size]	;Get data coords. normalized
	    face_dim = face mod 3	;Fixed coord along face
	    if face le 2 THEN k = 0 ELSE k = dims(face_dim)-1
	    p = COORD2to3(p(0), p(1), face_dim, k, sl.pt_inverse)
	    ip = fix(p + 0.5)		;round
	    if ip(fixed) ge dims(fixed) then goto, NO_BREAK	;Outside
	    kslice = ip(fixed)		;slice number
	    d1(fixed) = kslice
	    z1(fixed) = kslice
	    goto, NO_BREAK
	    ENDIF
    ENDFOR
  NO_BREAK:
ENDIF					;Marking a face

device, SET_GRAPH=6	 		;set XOR mode
valid = kslice ge 0

if sl.ortho eq 0 then begin		;Oblique???
    if n_elements(az) eq 0 then begin	;Initialize?
	az = 0.  & el = 0. & z0 = dims / 2.
	sl.orthop = [ 0., 0., 1., -total(dims)/2]
	endif
    el1 = el & az1 = az & z1 = z0	;Old params
    if (sl.lbuttons and 4) ne 0 then begin	;New azimuth/elev?
	if sl.oangle then $
	    el1 = fix((p(0)/!d.x_size * 198) - 99.) > (-90) < 90 $
	else az1 = fix((p(0)/!d.x_size * 396) - 198.) > (-180) < 180
	if (el1 ne el) or (az1 ne az) then begin
	    s = sin(el1 * !dtor)
	    sl.orthop(0) = sin(-az1 * !dtor) * s
	    sl.orthop(1) = cos(-az1 * !dtor) * s
	    sl.orthop(2) = cos(el1 * !dtor)
	    valid = 1
	    endif
    endif else if valid then begin	;New origin?
	p(face_dim) = z0(face_dim)	;New coords, 1 remains unchanged
	z1 = p
	valid = total(abs(z0-z1)) ne 0.	;Change...
	ENDIF

    if (valid) then begin		;Draw new
	sl.orthop(3) = -total(sl.orthop * z0)
        IF old_slice gt 0 THEN mark_oblique, kc	;Erase old
	old_slice = 0
	el = el1 & az = az1 & z0 = z1	;New parameters
	if valid and (sl.lbuttons ne 0) THEN BEGIN  ;Update obliq outline
	    WIDGET_CONTROL, sl.pos_text, set_value = $
		STRING(z0, az, el, FORMAT="('(',3f5.1,') A=', i4, ' E=',i4)")
	    d0 = slicer_plane_int()		;New intersections
	    mark_oblique, kc	;Draw new
	    old_slice = 1		;show its visible
	    ENDIF
	ENDIF			;Valid
    if ev.release ne 0 then begin	;Released button?
	if old_slice then mark_oblique, kc
	draw_orientation	;Draw resulting plane
	old_slice = 0
	endif
ENDIF ELSE BEGIN			;Orthogonal.
    IF (kslice ne old_slice) THEN BEGIN ;Movement?
        if (press eq 0) and (old_slice ge 0)then $    ;Erase old?
	    draw_cube, z0, d0, fixed, kc
	if valid then $
          d = 'Position: ' + string(ip, format="('(',i4,',',i4,',',i4,')')") $
	else d = ''
	WIDGET_CONTROL, sl.pos_text, set_value=d
	if valid and sl.lbuttons THEN BEGIN
	    draw_cube, z1, d1, fixed, kc
	    z0 = z1
	    d0 = d1
	    ENDIF
	IF (ev.release eq 1) and (old_slice ge 0) THEN BEGIN
	    device, SET_GRAPH=3
	    slicer_draw, fixed, old_slice	;Mark the slice
	    ENDIF			;Release
	old_slice = kslice		;Save current position
	ENDIF				;Movement
ENDELSE

device, SET_GRAPH=3    ;normal mode
end


PRO do_cube				;Draw a cube or cut
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

WIDGET_CONTROL, sl.base, /HOURGLASS
SET_PLOT, 'Z'
z =  (z_last = tvrd())			;Save previous image & current cont
zb = (zb_last = tvrd(CHANNEL=1, /WORDS))


SLICER_JOURNAL, "TRANS", [sl.trans, sl.threshold * 100. / sl.nc3]
SLICER_JOURNAL, "CUBE", [ mode, sl.cut_ovr, sl.interp, reform(sl.p0cube, 6) ]

d0 = sl.p0cube(*,0) < sl.p0cube(*,1)		;Lower corner
d1 =  (sl.p0cube(*,0) > sl.p0cube(*,1)) -d0
coords = fltarr(3,8)
for i=0,7 do coords(0,i) = $		;Verts of our cube
   [ (i and 1) * d1(0), (i and 2)/2 *d1(1), (i and 4)/4 * d1(2)] + d0
faces = where(sl.facevs eq sl.v_close)/4   ;close faces
relation = mode eq 1

FOR face = 0, 5 do $			;Draw the faces of the cube or cut
  IF ((where(face eq faces))(0) ge 0) eq relation then begin  ;Do this face?
	v = 0 & ones = 0 & z0 = 0 & q = 0	;Clear things out
	erase					;Reset Z buffer
	verts = sl.facevs(*,face)		;Vertices
	polyfill, coords(*, verts), /T3D	;Draw polygon for face
	z0 = tvrd(CHANNEL=1, /WORDS)		;Now read the Z buffer
	if mode eq 1 then $			;Cube? or Cut?
		points = where(z0 gt zb) $	;New points for cube
	else if sl.cut_ovr then points = where(z0 ne z0(0,0)) $ ;Cut mode?
	else points = where((z0 ne z0(0,0) and (zb gt z0)))  ;Over mode

	widget_control, sl.pos_text, set_value = $
		'Creating face ' + strtrim(face,2)
	if points(0) ne -1 THEN	BEGIN		;Anything to do?
	  ones = replicate(1, n_elements(points))
		;Make 3D homogen array in  data array subscripts coords
	  v = [ [(points mod !d.x_size) / float(!d.x_size)], $ ;Norm X
		[(points / !d.x_size)   / float(!d.y_size)], $ ;Norm Y
		[ z0(points)/65530. + 0.5], $		;Normalized Z coords
		[ ones ]]				;Homogeneous coord
		;Get subscripts in data cube (data coords)
	  v = ((v # sl.pt_inverse) - $
			(ones # [!x.s(0), !y.s(0), !z.s(0)])) / $
			(ones # [ !x.s(1), !y.s(1), !z.s(1)])
;		Either interpolate or pick nearest neighbor
	  widget_control, sl.pos_text, set_value = $
		(['Sampling','Interpolating'])(sl.interp) + $
		' face ' + strtrim(face,2) + ',  ' + $
		strtrim(n_elements(v)/3,2) + ' Pixels'

	  if sl.interp THEN v = interpolate(a, v(*,0), v(*,1), v(*,2)) $
	  else v = a(long(temporary(v)) # [1, dims(0), dims(0) * dims(1)])
					;Update our points
	  if sl.trans THEN BEGIN	;Transparency?
		good = where(v ge sl.threshold, count)
		if count le 0 then goto, skipit
		v = v(good)
		points = points(good)
		endif
	  offset = byte((face mod 3) * sl.nc3)   ;Offset
	  q = bytscl(v, max=sl.amax, min=sl.amin, top = sl.nc3-1) ;face data
				   ;Get subscripts in data cube
	  if offset ne 0 then q = q + offset
	  z(points) = q			;Store the new face
	  zb(points) = z0(points)	;in both buffers
    skipit:
	ENDIF				;Anything to do
  ENDIF					;Each face
tv, z					;Now show it
tv, zb, /WORDS, CHANNEL=1		;and update depth buffer
v = 0 & ones = 0 & z0 = 0 & q = 0	;Clear things out
z = 0 & zb = 0
widget_control, sl.pos_text, set_value = 'Done.'
slicer_show
end




PRO mark_cube1, p0, ip	;Draw the outline of the cube in the main drawable
; p0 = cube coordinates (3,2).
; ip = index of corner that is marked (0 or 1).
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

draw_cube, p0(*,0),p0(*,1), indgen(6), sl.xcol   ;Basic cube
p1 = [[p0(*,ip)], [p0(*,ip)]]
p = sl.p0(*,sl.v_close)
for i=0,2 do begin		;Lines to faces
	p1(i,0) = p(i)
	plots, p1, color=128, /T3D
	p1(i,0) = p1(i,1)
	endfor
end



PRO mark_cube, ev			;Mark a cube in the main window
;  Use the XOR graphics mode to avoid killing the display
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed

if n_params() eq 0 then	begin		;Erase old cube?
	device, SET_GRAPHICS = 6	;Set XOR mode
	if sl.cube_on then mark_cube1, sl.p0cube, sl.cube_ip
	sl.cube_on = 0
	device, SET_GRAPHICS=3
	return
	ENDIF

oldbuttons = sl.lbuttons
sl.lbuttons = oldbuttons and (not ev.release) or (ev.press)
press = ev.press and 1			;New left button press?

if sl.lbuttons eq 0 then begin		;Released all buttons?
	if oldbuttons ne 0 then draw_orientation  ;Update viewbox
	return
	ENDIF

x = ev.x / float(!d.x_size)		;Normalized coords
y = ev.y / float(!d.y_size)
d1 = dims -1

p0cube = sl.p0cube		;Save old coords
cube_ip = sl.cube_ip

sl.cube_ip = sl.lbuttons eq 2		;Corner index
q = sl.p0cube(fixed, sl.cube_ip)	;Fixed axis/point
p = fix(COORD2TO3(x, y, fixed, q, pti)+0.5) ;3D coords
sl.p0cube(*,sl.cube_ip) = p < d1 > 0	;New corner value
sl.p0cube(fixed, sl.cube_ip) = q
if (total(abs(sl.p0cube - p0cube)) eq 0.0) and $ ;No change?
   cube_ip eq sl.cube_ip then return

device, SET_GRAPHICS = 6	;Set XOR mode
if sl.cube_on then mark_cube1, p0cube, cube_ip  ;Erase prev
sl.cube_on = 1
mark_cube1, sl.p0cube, sl.cube_ip
d = string(sl.p0cube, format="('(', 3i4, ') (', 3i4, ')')")
widget_control, sl.pos_text, set_value = d  ;label it
DEVICE, SET_GRAPHICS = 3		;Restore
end





PRO slicer_event, ev
COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed


wset, sl.window		;Our window

if ev.id eq sl.draw THEN BEGIN		;mouse press?
  IF mode le 2 THEN BEGIN		;Slice or cube mode?
    IF  ((ev.press and 4) ne 0) and sl.ortho THEN BEGIN  ;Right but= chg plane?
	fixed = (fixed + 1) mod 3	;bump plane
	draw_orientation
	return
	ENDIF	
    if mode eq 0 then BEGIN		;Slice mode?
	if (ev.press and 2) ne 0 then goto, probe_it  ;Middle = probe
	i = sl.lbuttons or ev.press	;New state
	if ((sl.ortho eq 0) and (i ne 0)) or (sl.ortho and i) THEN BEGIN
	    mark_slice, ev		;mark the slice
	    return
	    endif
    ENDIF ELSE if mode le 2 THEN BEGIN
	mark_cube, ev 			;Move the cube
    ENDIF
    return
  ENDIF	ELSE BEGIN			;Other modes
    if ev.press eq 0 then return
  probe_it: if (n_elements(zb_last) le 1) THEN return
    set_plot,'Z'
    z = float(tvrd(ev.x, ev.y, /WORD, /CHANNEL))/65530. + .5 ;To 0-1
    set_plot,sl.gdev
    p = [ ev.x, ev.y, z(0)] / [!d.x_size, !d.y_size, 1.] ;normalized coords
    d = 'No Data Value'
    if p(2) gt .01 THEN BEGIN	;anything there?
	  p = [p,1] # sl.pt_inverse	;inverse transform to normalized
	  p = (p - [!x.s(0),!y.s(0),!z.s(0)])/[!x.s(1),!y.s(1),!z.s(1)]
	  p = fix(p + 0.5)		;round
	  p = p > 0 < (dims-1)		;to range
	  x = a(p(0), p(1), p(2))+0
	  y = fix(100.*(x - sl.amin)/(sl.amax - sl.amin))  ;To %
	  d = 'Position: '+string(p,format="('(',i0,',',i0,',',i0,')')") + $
		', Data= ' + strtrim(x,2) + ' (' + strtrim(y,2) + '%)'
	ENDIF			;Something there
	widget_control, sl.pos_text, set_value = d
  ENDELSE
  return
ENDIF			;Drawable window

if ev.id eq sl.isop.drawable then begin		;Isosurface threshold
	if ev.press eq 0 then return
	x = (ev.x - sl.isop.xs(0)) / sl.isop.xs(1)
	x = x > sl.amin < sl.amax
	WIDGET_CONTROL, sl.isop.slider, $
	   SET_VALUE = 100.*(x - sl.amin) / (sl.amax-sl.amin)
	sl.isop.value = x
	return
	ENDIF
if ev.id eq sl.file_text(0) then return	;Ignore return in file name widget
if ev.id eq sl.rslide(2) then BEGIN
	slicer_orientation
	return
	ENDIF
;		here, it must be a button or a slider:
widget_control, ev.id, get_uvalue = eventval
case eventval of
"CANCUBE" : mark_cube		;Undraw outline
"COLORS" : slicer_colors, (where(sl.color_button eq ev.id))(0)
"CUTINTO": sl.cut_ovr = 0
"CUTOVER": sl.cut_ovr = 1
"ERASE"  : slicer_erase
"EXIT"   : BEGIN
	widget_control, ev.top, /DESTROY
	if sl.rbase ne 0 then WIDGET_CONTROL, sl.rbase, /DESTROY
	z_last = 0
	zb_last = 0
	goto, close_journal
	ENDCASE
"EXPOSE0": sl.expose = 0
"EXPOSE1": sl.expose = 1
"GOCUBE" : do_cube
"HELP" : BEGIN
	xdisplayfile, filepath("slicer.txt", subdir=['help', 'widget']), $
		title = "Slicer help", $
		group = ev.top, $
		width = 72, height = 24
	return
	ENDCASE

"INTERP0": BEGIN
	sl.interp = 0
	goto, set_interp
	ENDCASE
"INTERP1": BEGIN
	sl.interp = 1
  set_interp:  i = WIDGET_INFO(ev.id, /parent)
	WIDGET_CONTROL, i, GET_UVALUE = i	;Buttons
	WIDGET_CONTROL, i(1-sl.interp), /SENS
	WIDGET_CONTROL, i(sl.interp), SENS=0
	ENDCASE

"ORTHO0":  BEGIN			;On oblique
	sl.ortho = 0
	WIDGET_CONTROL, sl.obuttons, MAP=1
	draw_orientation
	ENDCASE
"ORTHO1":  BEGIN
	sl.ortho = 1
	WIDGET_CONTROL, sl.obuttons, MAP=0
	draw_orientation
	ENDCASE
"AZIM" : sl.oangle = 0
"ELEV" : sl.oangle = 1
"GOOBL" :  BEGIN			;Do an oblique slice
	WIDGET_CONTROL, ev.top, /HOURGLASS
	slicer_oblique
	end


"ORIENTATION": SLICER_ORIENTATION, (where(sl.ori_butt eq ev.id))(0)
"PLAYBACK" : BEGIN
	WIDGET_CONTROL, sl.file_text(0), GET_VALUE=name
	name = strtrim(name(0),2)
	SLICER_PLAYBACK, FILE = name
	ENDCASE
"RECORD" : BEGIN
start_journal: if sl.journal ne 0 then free_lun, sl.journal  ;Close old
	sl.journal = 0
	WIDGET_CONTROL, sl.file_text(0), GET_VALUE=name
	name = strtrim(name(0))
	openw, i, name, ERROR=j, /GET_LUN
	if j ne 0 then begin		;OK?
		widget_control, sl.file_text(1), set_value = !ERR_STRING
		return
		ENDIF
	widget_control, sl.file_text(1), set_value = 'Journal Active'
	sl.journal = i
	ENDCASE
"RECORDOFF" : BEGIN
	widget_control, sl.file_text(1), set_value = 'Journal Closed'
close_journal: if sl.journal ne 0 then free_lun, sl.journal  ;Close old
	sl.journal = 0
	ENDCASE
"THRESHOLD" : BEGIN
	sl.threshold = sl.nc3 * ev.value / 100.
	sl.trans = (ev.value ge 1) and (ev.value le 99)  ;On if reasonable
	ENDCASE
"HIGH": sl.isop.hi_lo = 0
"LOW" : sl.isop.hi_lo = 1
"ISOSLIDE" : sl.isop.value = (ev.value / 100.)*(sl.amax-sl.amin) + sl.amin
"GO":   do_isosurface
"SHADING": BEGIN
	sl.shading = ev.value /100.
	slicer_colors
	ENDCASE
"STMAX": BEGIN
	sl.stretch(1) = ev.value
	slicer_colors
	ENDCASE
"STMIN": BEGIN
	sl.stretch(0) = ev.value
	slicer_colors
	ENDCASE
"UNDO" : slicer_undo
"XROTATION": BEGIN
	sl.rotation(0) = ev.value
	slicer_orientation
	ENDCASE
"ZROTATION": BEGIN
	sl.rotation(1) = ev.value
	slicer_orientation
	ENDCASE

ELSE :   BEGIN			;mode button?
	k = where(eventval eq sl.mode_names, count)  ;Match with mode name?
	if count eq 1 THEN BEGIN		;switch mode
		if sl.cube_on then mark_cube	;Remove the cube if vis
		fixed = 0			;Reset fixed direction
		if sl.mode_bases(mode) ne 0 THEN $  ;Remove panel if mapped
			WIDGET_CONTROL, sl.mode_bases(mode), MAP=0
		mode = k(0)			;New mode
		if sl.mode_bases(mode) ne 0 THEN $  ;Map new base
			WIDGET_CONTROL, sl.mode_bases(mode), MAP=1
		if mode le 2 THEN BEGIN		;Slice or cube?
			draw_orientation
			ENDIF
		if mode eq 3 THEN BEGIN		;Draw histogram
			WSET, sl.isop.window
			type = size(a)
			int = type(type(0) + 1) le 3  ;True if int type
			j = (sl.amax -sl.amin)/100. ;bin size
			if int then j = j > 1
			h = histogram(a, max=sl.amax, min = sl.amin, bin=j)
			if int THEN j = fix(j + .99)
			k = sort(h)
			n = n_elements(h)
			x = findgen(n) * j + sl.amin < sl.amax
			xsave = !x.s & ysave = !y.s
			PLOT,x,h, xst = 9, yst=8, ymargin=[2,0], $
				yrange= [0,h(k(n-8))], yticks=1, chars=.75, $
				xticks=4
			sl.isop.xs = !x.s * !d.x_size
			WSET, sl.window
			!x.s = xsave & !y.s = ysave
			ENDIF			;Isosurface
		widget_control, sl.obuttons, $	;Oblique controls
			MAP = (mode eq 0) and (sl.ortho eq 0)
		RETURN
	ENDIF
	print,'Unknown event: ', eventval
	help, /STRUCT, ev
	ENDCASE
ENDCASE
end

PRO slicer, GROUP = group, RANGE = range, COMMANDS = commands, $
	CMD_FILE = cmd_file, RESOLUTION = resolution, DETACHED = detached, $
        MODAL = modal
;+
; NAME:
;	SLICER
;
; PURPOSE:
;	Widget based application to show 3D volume slices and isosurfaces.
;
; CATEGORY:
;	Volume display / rendering.
;
; CALLING SEQUENCE:
;	COMMON VOLUME_DATA, A
;	A = your_volume_data
;	SLICER
;
; INPUTS:
;	Variable A in VOLUME_DATA common contains volume data.  See EXAMPLE
;	section below.
;
; KEYWORD PARAMETERS:
;     COMMANDS:	An optional string array of commands to execute
;		before entering the interactive mode.  Commands are
;		in the form of a keyword optionally followed one or more 
;		numeric, blank-separated parameters.  For example:
;			"COMMAND P1 P2 P3 ... Pn"
;		Keywords and parameters are:
;		UNDO:	Undo previous operation.
;		ORI X_Axis Y_Axis Z_axis X_Rev Y_Rev Z_Rev X_Rot Z_Rot Asp
;			This command sets the orientation for the SLICER 
;			display.  X_Axis, Y_Axis, and Z_Axis should be 0 for 
;			data x, 1 for data y, and 2 for data z.  
;			X_Rev, Y_Rev, and Z_Rev should be 0 for normal, 1 for 
;			reversed.  Asp is the Z axis aspect ratio w/ respect 
;			to X, Y.  X_Rot and Z_Rot are the rotations of the 
;			X and Z axes in degrees (30 is the default).
;			For example, to interchange the X and Z axes and
;			reverse the Y use the string:
;				ORI 2 1 0 0 1 0 30 30
;		TRANS On_Off Threshold:  Use this command to turn transparency 
;			on or off and set the transparency threshold value.
;			1 means on, 0 means off.  Threshold is expressed in 
;			percent of data range (0 = min data value, 100 = max 
;			data value).
;		SLICE Axis Value Interp 0:  Draw an orthogonal slice along
;			the given axis (0=x, 1=y, 2=z) at Value.  Set Interp
;			equal to 1 for interpolation, 0 for nearest neighbor.
;			Expose = 1 to remove, 0 for normal slice.
;		SLICE Azimuth, Elev, Interp, Expose, 1, x0, y0, z0:  Draw
;			an oblique slice.  The oblique plane crosses the
;			XY plane at angle Azimuth, with an elevation of Elev.
;			It passes thru the point (x0, y0, z0).
;		COLOR Table_Index Low High Shading:  Set the color tables.
;			Table_Index is the pre-defined color table number (see
;			LOADCT), or -1 to retain the present table.  Low, High
;			and Shading are expressed in percent.
;		ISO Threshold Hi_Lo:  Draw an iso-surface.  Threshold is the 
;			isosurface threshold value.  Hi_Lo should be set to 1
;			to view the low side, 0 for the high side.
;		ERASE:	Erase the display.
;		CUBE Mode Cut_Ovr Interp X0 Y0 Z0 X1 Y1 Z1:  Draw cube 
;			(mode = 1) or cut-out (mode = 0).
;			Cut_Ovr should be set to  1 for cut-over, 0 for 
;			cut-thru.  Interp should be 1 for interpolation, 0 
;			for nearest neighbor.  (X0,Y0,Z0) is the lower corner 
;			of the cube.  (X1,Y1,Z1) is the upper corner. 
;			(X0 < X1, etc.)
;		WAIT Secs:  Wait the designated time (in seconds).
;
;     CMD_FILE:	A string that contains the name of a file containing SLICER
;		commands to execute as described above.
;
;	DETACHED: if set, put the drawable in a separate window. (Good
;		for large windows.)
;	GROUP:	The base ID of the widget that calls SLICER.  When this 
;		keyword is specified, the death of the caller results in the
;		death of the SLICER.
;
;	RANGE:	A two-element array containing minimum and maximum data
;		values of interest.  If this keyword is omitted, the data is 
;		scanned for the minimum and maximum.
;       MODAL:  If set, then the slicer runs in modal mode.
;
;    RESOLUTION: a two element vector giving the width and height of
;		the drawing window.  Default = 55% by 44% of screen width.
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	COMMON VOLUME_DATA, A   ;Used to pass in the volume data.
;	COMMON SLICER_COMMON   ;Used internally.
;	COMMON SLICER_COMMON1  ;Used internally.
;
; SIDE EFFECTS:
;	On exit, the Z-buffer contains the most recent image generated by
;	SLICER.  The image may be redisplayed on a different device by 
;	reading the Z-buffer contents, plus the current color table.
;	Widgets are created on the X window display.
;
; RESTRICTIONS:
;	Widgets are required.
;	The volume data must fit in memory.
;
; PROCEDURE:
;	The slicer program has the following modes:
;	Slices:         Displays orthogonal slices thru the data volume.
;	Block:          Displaces the surfaces of a selected block inside
;	                the volume. 
;	Cutout:         Cuts blocks from previously drawn objects.
;	Isosurface:     Draws an isosurface contour.
;	Probe:          Displays the position and value of objects
;	                using the mouse.
;	Colors:         Manipulates the color tables and contrast.
;	Rotations:      Sets the orientation of the display.
;
; EXAMPLE:
;	Data is transferred to the SLICER via the VOLUME_DATA common block
;	instead of as an argument.  This technique is used because volume
;	datasets can be very large and hence, the duplication that occurs when
;	passing values as arguments is a waste of memory.  Suppose that you 
;	want to read some data from the file "head.dat" into IDL for use
;	in the SLICER.  Before you read the data, establish the VOLUME_DATA
;	common block with the command:
;
;		COMMON VOLUME_DATA, VOL
;
;	The VOLUME_DATA common block has just one variable in it.  The variable
;	can have any name.  Here, we're using the name "VOL".  Now read the
;	data from the file into VOL.  For example:
;
;		OPENR, 1, "head.dat"
;		VOL = FLTARR(20, 30, 42)
;		READU, 1, VOL
;		CLOSE, 1
;
;	Now you can run the SLICER widget application by entering:
;
;		SLICER
;
;	The data stored in VOL is the data being worked on by the SLICER.
;
;	To obtain the image in the slicer window after slicer is finished:
;	(Use the image with the current color tables).
;
;	SET_PLOT, 'Z'   ;Use the Z buffer graphics device
;	a = TVRD()	;Read the image
;
; MODIFICATION HISTORY:
;	DMS - RSI, Oct, 1991.
;	DMS - RSI, Mar, 1992.  Added Journaling and expose mode.
;				Fixed bug with 24 bit color.
;	DMS - RSI, Jan, 1993.  Added oblique slices.
;       DJC - RSI, Jun, 1994.  Fixed oblique slice initialization and
;                              atan(0,0) problem (on HP).
;       DJC - RSI, Feb, 1995.  Added modal keyword.
;       DJC - RSI, Feb, 1995.  Changed "poly" variable to "polyv" to
;                              avoid clash with math "poly" function.
;       DJC - RSI, Mar, 1995.  Fixed shading values for iso-surface.
;-



COMMON volume_data, a
COMMON slicer_common, dims, sl, z_last, zb_last, mode, fixed
COMMON slicer_common1, old_slice, d0, z0, az, el



mode = 0
fixed = 0
sl_width = 240		;Slider width

mode_names = [ 'Slices', 'Block', 'Cutout', 'Isosurface', 'Probe', 'Colors', $
	'Rotations', 'Journal' ]
nmodes = n_elements(mode_names)		;# of modes

isop = { ISOP, hi_lo : 1, value: 0.0, window : 0, drawable : 0L, slider : 0L, $
		xs : fltarr(2) }

;	Main data structure
sl = {  SLICER, base : 0L, $		;Main base
	draw:0L, $			;Big drawable
	window:0, $			;Big drawable window index
	trans:0, $			;Transparency flag
	threshold_slider: 0L, $		;Threshold slider 
	threshold:0b, $			;Transp threshold in pixel values
	mode_names : mode_names, $	;Names of modes
	interp: 1, $			;Interpolation flag
	ortho : 1, $			;TRUE for orthogonal slices
	orthop : fltarr(4), $		;Plane eqn for ortho slices
	mode_bases : lonarr(nmodes), $	;Mode panel bases
	nc3: 0, $			;# of colors per partition (3 of them)
	nc1: 0, $			;# of colors we use
	amax : 0.0, $			;Data max, min
	amin : 0.0, $
	xcol : 0, $			;XOR Drawing color
	color_button:lonarr(24), $	;Color  table buttons (up to 24)
	axex: intarr(3), $		;TRUE to reverse axis
	axrev: intarr(3), $		;Axis permutations
	ori_butt: lonarr(7), $		;Orientation buttons
	draw_butt: lonarr(4), $		;Slice draw/expose buttons
	pos_text : 0L, $		;Label widget at bottom
	rotation: [ 30., 30.], $	;Current rotations
	v_close: 0, $			;Index of closest vertex
	p0 : fltarr(3,8), $		;Data coords of cube corners
	p1 : fltarr(3,8), $		;Device coords of cube corners
	pt_inverse : fltarr(4,4), $	;Inverse of !P.T
	vfaces : intarr(3,8), $		;Face index vs vertex index
	facevs : intarr(4,6), $		;Vertex index vs faces
	edges : intarr(2,12), $		;Vertices vs edge index
	isop: isop, $			;Isosurface parameters
	p0cube : intarr(3,2), $		;Corner coords of cube selection
	cut_ovr : 0, $			;Cut mode
	cube_on : 0, $			;If cube is on
	cube_ip : 0, $			;Corner of cube
	shading : 0.20, $		;Amount of differential shading
	file_text : LONARR(2), $	;File name text widgets
	cslide : LONARR(3), $		;Color sliders
	rslide : LONARR(3), $		;Rotation sliders, aspect text
	journal : 0, $			;Journal file
	stretch : [0,100], $		;Stretch params
	lbuttons : 0, $			;Last button state
	expose : 0, $			;Sice mode (0=slice, 1=expose)
	gdev : !D.NAME, $		;Graphics device
	obuttons : 0L, $		;Oblique buttons
	oangle : 1, $			;active angle for oblique
	rbase: 0L }			;Drawable base

;  Faces vs vertex index
sl.vfaces = [[0,1,2],[1,2,3],[0,2,4],[2,3,4],[0,1,5], [1,3,5], [0,4,5], $
		[3,4,5]]
;  Vertex indices vs faces  (clockwise order).
sl.facevs = [ [0,2,6,4], [0,4,5,1], [2,0,1,3], [1,5,7,3], [3,7,6,2], $
		[6,7,5,4]]
;
; vertex numbers vs Edge index (12 edges)
sl.edges = [[0,1],[1,3],[2,3],[0,2], [0,4], [1,5],[2,6],[3,7], $
	  [4,5],[5,7],[6,7],[4,6]]

if XRegistered("slicer") THEN RETURN
if n_elements(resolution) lt 2 then begin
	device, get_screen = resolution
	resolution(0) = 5 * resolution(0) / 9
	resolution(1) = 4 * resolution(0) / 5
	endif

set_plot,'Z'
device, /z_buffering, set_resolution = resolution
set_plot,sl.gdev

z_last = 0
zb_last = 0

s = size(a)
if s(0) ne 3 THEN $
	MESSAGE,'Slicer: volume_data common block does not contain 3D data'
dims = s(1:3)
d1 = dims-1

sl.p0cube = [[dims/4], [3 * dims/4]]
for i=0,7 do sl.p0(*,i) = $		;Data coords of corners
	[ (i and 1) * d1(0), (i and 2)/2 * d1(1), (i and 4)/4 * d1(2)]

sl.orthop = [ 0., 0., 1., -dims(2)/2.]	;Initial orthogonal plane

if n_elements(range) ge 2 THEN BEGIN	;Range specified?
	sl.amax = range(1)
	sl.amin = range(0)
ENDIF ELSE BEGIN
	sl.amax = max(a, min = q)
	sl.amin = q
ENDELSE

old_slice = 0
az = 0.0
el = 0.0
z0 = [0.0, 0.0, 0.0]
d0 = slicer_plane_int()

sl.base = WIDGET_BASE(TITLE='IDL Slicer', /ROW)
lbase = WIDGET_BASE(sl.base, /COLUMN)
if keyword_set(detached) THEN BEGIN
	rbase = WIDGET_BASE(Title='Slicer', EVENT_PRO='SLICER_EVENT')
	sl.rbase = rbase
endif else rbase = WIDGET_BASE(sl.base)


sl.obuttons = WIDGET_BASE(rbase, /ROW)
junk = WIDGET_BASE(sl.obuttons, /exclusive, /row)
junk1 = WIDGET_BUTTON(junk, VALUE='Azimuth', UVALUE='AZIM')
junk1 = WIDGET_BUTTON(junk, VALUE='Elevation', UVALUE='ELEV')
WIDGET_CONTROL, junk1, /SET_BUTTON
junk1 = WIDGET_BUTTON(sl.obuttons, VALUE = 'Go', UVALUE='GOOBL')
WIDGET_CONTROL, sl.obuttons, MAP=0   ;Remove buttons for oblique mode

sl.draw = WIDGET_DRAW(rbase, XSIZE=resolution(0), YSIZE=resolution(1),$
	RETAIN=2, /BUTTON_EVENTS, /MOTION)

junk = WIDGET_BASE(lbase, COLUMN=3)
junk1 = WIDGET_BUTTON(junk, value="Done", uvalue = "EXIT", /NO_REL)
junk1 = WIDGET_BUTTON(junk, value="Erase", uvalue = "ERASE", /NO_REL)
junk1 = WIDGET_BUTTON(junk, value="Undo", uvalue = "UNDO", /NO_REL)
junk1 = WIDGET_BUTTON(junk, value="Help", uvalue = "HELP", /NO_REL)
junk1 = WIDGET_BUTTON(junk, VALUE='Orientation',/MENU)
ori_names = [ 'X Y Exchange', 'X Z Exchange', 'Y Z Exchange',$
	'X Reverse','Y Reverse','Z Reverse', 'Reset']
for i=0,6 do sl.ori_butt(i) = WIDGET_BUTTON(junk1, VALUE=ori_names(i),$
		UVALUE = 'ORIENTATION')
widget_control, sl.ori_butt(0), SET_BUTTON=1

junk1 = WIDGET_BUTTON(junk, VALUE='Interpolation',/MENU)
junk2 = WIDGET_BUTTON(junk1, VALUE='Off', UVALUE='INTERP0')
junk3 = WIDGET_BUTTON(junk1, VALUE='On', UVALUE='INTERP1')
WIDGET_CONTROL, junk3, SENS=0		;Its on now.
widget_control, junk1, set_uvalue=[junk2, junk3]

junk1 = WIDGET_BASE(lbase, /FRAME, COLUMN=3, /EXCLUSIVE)

for i=0,nmodes-1 do $	; Mode buttons
    junk2 = WIDGET_BUTTON(junk1, value=sl.mode_names(i), $
		uvalue=sl.mode_names(i), /NO_RELEASE)

junk = WIDGET_BASE(lbase, /FRAME, /COLUMN)
mode_base = WIDGET_BASE(junk)		;For the mode dependent bases

for i=0,nmodes-1 do $
   if i ne 2 then sl.mode_bases(i) = WIDGET_BASE(mode_base, uvalue=0L, /COLUMN)


parent = sl.mode_bases(0)		; slices mode
junk = WIDGET_DRAW(parent, XSIZE = sl_width, $
	YSIZE = sl_width * float(resolution(1)) / resolution(0))
WIDGET_CONTROL, parent, SET_UVALUE= junk

junk2 = WIDGET_BASE(parent, /ROW)
junk3 = WIDGET_BASE(junk2, /EXCLUSIVE, /ROW)
sl.draw_butt(0) = $
    WIDGET_BUTTON(junk3, VALUE = 'Draw', UVALUE='EXPOSE0', /NO_REL)
sl.draw_butt(1) = $
    WIDGET_BUTTON(junk3, VALUE = 'Expose', UVALUE='EXPOSE1', /NO_REL)
junk3 = WIDGET_BASE(junk2, /EXCLUSIVE, /ROW)
sl.draw_butt(2) = $
    WIDGET_BUTTON(junk3, VALUE = 'Orthogonal', UVALUE='ORTHO1', /NO_REL)
sl.draw_butt(3) = $
    WIDGET_BUTTON(junk3, VALUE = 'Oblique', UVALUE='ORTHO0', /NO_REL)

WIDGET_CONTROL, sl.draw_butt(0), /SET_BUTTON
WIDGET_CONTROL, sl.draw_butt(2), /SET_BUTTON

parent = sl.mode_bases(1)		;Cube & Cut modes
junk = WIDGET_BASE(parent, /ROW)
junk1 = WIDGET_BUTTON(junk, value=' GO ', uvalue='GOCUBE', /NO_RELEASE)
junk1 = WIDGET_BUTTON(junk, value=' Cancel ', uvalue='CANCUBE', /NO_REL)
junk1 = WIDGET_BASE(junk, /EXCLUSIVE, /ROW)
junk = WIDGET_BUTTON(junk1, VALUE="Cut Into", UVALUE="CUTINTO", /NO_REL)
junk = WIDGET_BUTTON(junk1, VALUE="Cut Over", UVALUE="CUTOVER", /NO_REL)
junk = WIDGET_DRAW(parent, XSIZE = sl_width, $
	YSIZE = sl_width * float(resolution(1)) / resolution(0))
widget_control, parent, set_uvalue= junk

sl.mode_bases(2) = sl.mode_bases(1)	;Cut is copy of cube

parent = sl.mode_bases(3) 		 ; Isosurface mode
junk = widget_button(parent, value='GO', UVALUE='GO')
junk = widget_base(parent, /row)
junk1 = widget_label(junk, value='Display: ')
junk = widget_base(junk, /row, /exclusive)
junk1 = widget_button(junk, value='High Side', uvalue='HIGH', /NO_REL)
junk1 = widget_button(junk, value='Low Side', uvalue='LOW', /NO_REL)
widget_control, junk1, /set_button	;Set low value
sl.isop.slider = WIDGET_SLIDER(parent, xsize=sl_width, MINIMUM = 0, $
		UVALUE = "ISOSLIDE", $
		TITLE = 'Isosurface Threshold (%)', $
		MAXIMUM = 100)
isodraw = WIDGET_DRAW(parent, XSIZE=sl_width, YSIZE = 100, /BUTTON_EVENTS)

;	Color tables
parent = sl.mode_bases(5)
junk1 = widget_base(parent, /ROW)
junk = WIDGET_BUTTON(junk1, VALUE = 'Color Tables', /MENU)
junk2 = 0
loadct, get_names = junk2
n = n_elements(junk2) < 24		;# of buttons to make
FOR i = 0, n-1 DO sl.color_button(i) = $	;Make color pull down buttons
	WIDGET_BUTTON(junk, VALUE=STRTRIM(junk2(i),2), uvalue='COLORS')

sl.cslide(0) = WIDGET_SLIDER(parent, xsize = sl_width, MINIMUM=0, /DRAG, $
	MAXIMUM=100, UVALUE = "STMIN", Title="Contrast Minimum", VALUE=0)
sl.cslide(1) = WIDGET_SLIDER(parent, xsize = sl_width, MINIMUM=0, /DRAG, $
	MAXIMUM=100, UVALUE = "STMAX", Title="Contrast Maximum", VALUE=100)
sl.cslide(2) = WIDGET_SLIDER(parent, xsize = sl_width, MINIMUM=0, /DRAG, $
	MAXIMUM=100, UVALUE = "SHADING", Title="Differential Shading (%)", $
	VALUE=20)


parent = sl.mode_bases(6)		;Rotations mode
sl.rslide(0) = WIDGET_SLIDER(parent, xsize=sl_width, MINIMUM=-90, MAXIMUM=90, $
	UVALUE = "XROTATION", Title="X Axis Rotation", VALUE=30)
sl.rslide(1) = WIDGET_SLIDER(parent, xsize=sl_width, MINIMUM=-179, $
	MAXIMUM=179, UVALUE = "ZROTATION", Title="Z Axis Rotation", VALUE=30)
junk = WIDGET_BASE(parent, /frame, /row)
junk1 = WIDGET_LABEL(junk, VALUE='Z Aspect Ratio:')
sl.rslide(2) = WIDGET_TEXT(junk, VALUE='1.0     ', /EDIT, YSIZE=1, XSIZE=10)


parent = sl.mode_bases(7)		;Journal mode
junk = WIDGET_BASE(parent, /COLUMN)
junk1 = WIDGET_BUTTON(junk, VALUE='Start Recording', UVALUE='RECORD', /NO_REL)
junk1 = WIDGET_BUTTON(junk, VALUE='Stop Recording', UVALUE='RECORDOFF',$
		/NO_REL)
junk1 = WIDGET_BUTTON(junk, VALUE='Playback', UVALUE='PLAYBACK', /NO_REL)
junk = WIDGET_BASE(parent, /ROW)
junk1 = WIDGET_LABEL(junk, value='File Name:')
sl.file_text(0) = WIDGET_TEXT(junk, xsize=24, ysize=1, $
	value='slicer.jou'+string(replicate(32b,14)), /EDIT, /FRAME)
sl.file_text(1) = WIDGET_TEXT(parent, xsize=32, ysize=1, $
	value='Journal Closed', /FRAME)

;	Transparency buttons / slider
junk = WIDGET_BASE(lbase, /FRAME, /COLUMN)
sl.threshold_slider = WIDGET_SLIDER(junk, xsize=sl_width, $
	MINIMUM=0, MAXIMUM=100,$
	UVALUE="THRESHOLD", TITLE="Transparency Threshold (%)", VALUE=0)

junk1 = WIDGET_BASE(lbase, /FRAME)	;Message base
sl.pos_text = WIDGET_TEXT(junk1, xsize=40, ysize=1)

; Unmap mode dependent widgets  (Leave journal mapped because of obscure bug)
for i=1, nmodes-1 do widget_control, sl.mode_bases(i), MAP=0

WIDGET_CONTROL, sl.base, /REALIZE
if sl.rbase ne 0 then WIDGET_CONTROL, sl.rbase, /REALIZE
DEVICE, SET_GRAPHICS=3		;Ensure copy graphics mode

WIDGET_CONTROL, sl.draw, get_value = junk
sl.window = junk		;Main window
WIDGET_CONTROL, isodraw, get_value = junk
sl.isop.window = junk		;Isosurface drawable
sl.isop.drawable = isodraw

sl.nc1 = (!d.n_colors < 256) -1 ;Colors we can use
sl.nc3 = (sl.nc1-3)/3		;Colors per orientation

slicer_orientation,6		;Reset to default orientation, erase
slicer_colors, 0

if n_elements(commands) gt 0 then slicer_playback, commands  ;Execute cmds?
if n_elements(cmd_file) gt 0 then slicer_playback, file = cmd_file

device,	TRANSLATION = tbl	;Read hdw translation table
				;Distance between white and black
if !d.name eq 'X' then sl.xcol = tbl(0) xor tbl(sl.nc1)  $
else sl.xcol = 196		;Windows.

tbl=0				;Kill it
XManager, "slicer", sl.base, EVENT_HANDLER = slicer_events, GROUP = group, $
   MODAL = Keyword_Set(modal)
end
