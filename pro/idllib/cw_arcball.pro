; $Id: cw_arcball.pro,v 1.5 1994/01/12 22:56:21 alan Exp $
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_ARCBALL
;
; PURPOSE:
;	CW_ARCBALL is a compound widget for intuitively specifying
;	three-dimensional orientations.
;
; CATEGORY:
;	Widget, 3d graphics
;
; CALLING SEQUENCE:
;	Widget_id = CW_ARCBALL(Parent)
;
; INPUTS:
;       PARENT:	The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	FRAME:	If set, draws a frame around the widget.
;		The default is FRAME=0.
;	LABEL:	A string containing the widget's label.
;	VALUE:	An initial value for the 3 x 3 rotation matrix. This
;		must be a valid rotation matrix (no translation or
;		perspective) where: transpose(value) = inverse(value).
;		This can be the upper-left corner of !P.T after executing
;		the command T3D, /RESET, ROTATE=[x,y,z]. The default
;		is the identity matrix.
;	UVALUE:	The initial user value for the widget.
;	SIZE:	The size of the square drawable area containing the arcball,
;		in pixels.  Default size = 192.	
;	UPDATE:	If set, the widget will send an event each time
;		the mouse button is released after a drag operation.
;		Otherwise, an event is only sent when the Update
;		button is pressed.
;	COLORS:	A 6-element array containing the color indices to be used.
;		  Colors(0) = View axis color
;		  Colors(1) = object axis color, 
;		  Colors(2) = XZ plane +Y side (body top) color, 
;		  Colors(3) = YZ plane (fin) color,
;		  Colors(4) = XZ plane -Y side (body bottom),
;		  Colors(5) = background color.
;		Default value = [ 1,7,2,3,7,0], which yields good colors
;		with the TEK_COLOR table.
;		  (white, yellow, red, green, yellow, black).
;	RETAIN: Retain parameter for window, 0 = none, 1 = server's default,
;		2 = use backing store.  default = 1.
;
; OUTPUTS:
;	The ID of the widget is returned.
;
; SIDE EFFECTS:
;	Events are generated as described above. The current graphics window
;	is changed.
;
; RESTRICTIONS:
;	This widget can generate any rotation about any axis.
;	Not all rotations are compatible with the IDL SURFACE
;	procedure, which is restricted to rotations that project the
;	object Z axis parallel to the view Y axis.
;
; PROCEDURE:
;	This widget is based on "ARCBALL: A User Interface for
;	Specifying Three-Dimensional Orientation Using a Mouse", by Ken
;	Shoemake, Computer Graphics Laboratory, University of Pennsylvania,
;	Philadelphia, PA 19104. "In Arcball, human factors and mathematical
;	fundamentals come together exceptionally well."
;
;	The user drags a simulated track-ball with the mouse to interactively
;	obtain arbitrary rotations. Sequences of rotations may be cascaded.
;	The rotations may be unconstrained (about any axis), constrained to
;	the view X, Y, or Z axes, or to the object's X, Y, or Z axis.
;
;	Use the call:
;		WIDGET_CONTROL, id, /SET_VALUE
;	to draw the arcball after the widget is initially realized.
;	Also, the SET_VALUE entry will set the widget's value to the
;	given 3x3 rotation matrix and redraw the widget.
;
;	The WIDGET_CONTROL, id, GET_VALUE=v
;	call returns the current 3x3 rotation matrix.
;	
; EXAMPLE:
;	See the procedure ARCBALL_TEST, contained in this file.
;	To test CW_ARCBALL:
;	.RUN cw_arcball
;	ARCBALL_TEST
;
; MODIFICATION HISTORY:
;	DMS, RSI, September, 1993.  Written
;	ACY, RSI, January, 1994.  Correct test on initial value.
;-


function arcball_constrain, pt0, axis
; Project the point pt0 onto the plane perpendicular
; to axis and passing thru the center of the sphere.

proj = pt0 - total(axis * pt0) * axis
norm = sqrt(total(proj^2))
if norm gt 0.0 then begin
    s = 1.0/norm
    if proj(2) lt 0.0 then s = -s
    pt = s * proj
endif else if axis(2) eq 1.0 then pt = [1.0, 0., 0.] $
else pt = [ -axis(1), axis(0), 0.0 ] / sqrt(total(axis(0:1)^2))
return, pt
end


function quaternion_m3, q
; Given a unit quaternion, q, return its 3x3 rotation matrix.
x = q(0)
y = q(1)
z = q(2)
w = q(3)
a = [[ w^2+x^2-y^2-z^2, 2*(x*y+w*z), 2*(x*z-w*y)], $
     [ 2*(x*y-w*z), w^2-x^2+y^2-z^2, 2*(y*z+w*x)], $
     [ 2*(x*z+w*y), 2*(y*z-w*x), w^2-x^2-y^2+z^2]]
return, a
end



PRO ARCBALL_ARC, p0, p1, cx, cy, radius, COLOR=col
; Given 2 points on the unit sphere p0(3) and p1(3), draw the great circle
; arc connecting them.  (cx, cy) = center of sphere in whatever units are
; specified via the EXTRA parameter.  Radius = radius of sphere.
;
len = acos(total(p0 * p1) < 1.0)
if len eq 0.0 then return
n = 12                  ;# of line segments
s = sin(findgen(n+1) * (len / n))  ;Sines
p = fltarr(2,n+1,/nozero)
c = [cx, cy]
for i=0, n do p(0,i) = radius * (p0 * s(n-i) + p1 * s(i))/s(n) + c
plots, p, COLOR=col, /DEVICE
end




PRO ARCBALL_AXIS_ARC, p, cx, cy, radius, COLOR=c
; Draw the half great circle, whose plane is perpendicular to p.
;
s = sqrt(1.0 - p(2)^2 > 0.0)
if s eq 0.0 then begin      ;Outline of circle
    x = [1,0,-1,0,1]
    y = [0,1,0,-1,0]
    for i=0,3 do arcball_arc, [x(i),y(i),0.0], [x(i+1), y(i+1), 0.0], $
			cx, cy, radius, COLOR=c
endif else begin
    p0 = [ -p(0)*p(2)/s, -p(1)*p(2)/s, s]
    p1 = [ p(1)/s, -p(0)/s, 0.0]
    arcball_arc, p0, p1, cx, cy, radius, COLOR=c
    arcball_arc, p0, -p1, cx, cy, radius,COLOR=c
endelse
end


PRO CW_ARCBALL_DRAW, state, DO_ARC = do_arc  ;Draw the arcball spaceship.
; If do_arc is set, draw the great circle between pt0 and pt1

if state.window eq 0 then begin   ;First call?
    WIDGET_CONTROL, state.draw_id, GET_VALUE=v
    state.window = v
    ENDIF

wset, state.window
col = state.colors
erase, col(5)
wsize = state.wsize
c = state.center
r = state.radius
plots, [0,wsize], c, /device, color=col(0)
plots, c, [0,wsize], /device, color=col(0)
plots, state.circle, /device, color=col(0)
if keyword_set(do_arc) then begin
	plots, state.pt0 * r + c, /PSYM, /DEVICE, COLOR=col(0)
	arcball_arc, state.pt0, state.pt1, c, c, r, COLOR = col(2)
	ENDIF
rot = state.cvalue

rr = .4 * r * rot 		;*** Draw the object
offset = [c, c, 0] # replicate(1,3)
t0 = rr # [[0,0,1], [-0.5,0,-1], [.5,0,-1]]   ;Main triangle
t1 = rr # [[0,0,1], [0,.5,-1], [0,0,-1.]]

q = crossp(t0(*,0) - t0(*,1), t0(*,2) - t0(*,1))  ;which one first?
;;; [1,7,2,3]
if q(2) ge 0 then begin
    polyfill, t0+offset, color=col(2), /DEVICE
    polyfill, t1+offset, color=col(3), /DEVICE
endif else begin
    polyfill, t1+offset, color=col(3), /DEVICE
    polyfill, t0+offset, color=col(4), /DEVICE
endelse


for i=0,2 do begin		;Draw each object axis
    p = fltarr(3,3,3)
    p(i) = 1.0
    p1 = rot # p
    arcball_axis_arc, p1, c, c, r, COLOR=col(1)
    endfor
end

PRO CW_ARCBALL_HELP, top
s = size(top)
if s(s(0)+1) eq 8 then begin		;Called with structure?  Quit.
	WIDGET_CONTROL, top.top, /DESTROY
	RETURN
	END

a = widget_base(/column, title='ARCBALL Help')	;Not structure.
b = WIDGET_TEXT(a, value = [ $
	'Use the mouse to drag and rotate the simulated trackball.', $
	'Rotate about the view Z axis by dragging outside the circle.', $
	' ', $
	'Use the Constraint button to constrain rotations about the', $
	'axis closest to where the mouse is first clicked.'], $
	XSIZE=72, YSIZE = 6)
b = WIDGET_BUTTON(a, VALUE='Dismiss', /NO_REL)
WIDGET_CONTROL, a, /REAL
XMANAGER, 'Arcball Help', a, $
	EVENT_HANDLER='CW_ARCBALL_HELP', /MODAL, GROUP=top
end

Function CW_ARCBALL_EVENT,  event

WIDGET_CONTROL, event.id, GET_UVALUE=child	;Widget with state
WIDGET_CONTROL, child, GET_UVALUE=state, /NO_COPY 

case event.id of
state.draw_id: BEGIN		;Draw event
    if (event.press or state.buttons or event.release) eq 0 then goto, done
    xy = ([event.x, event.y] - state.center) / state.radius
    r = total(xy^2)		;Distance from ctr
    if r gt 1.0 then pt1 = [xy/sqrt(r), 0.0] $      ;Outside circle, z = 0.
    else pt1 = [xy, sqrt(1.0-r)]
    c = state.constrain
    if event.press ne 0 then begin
	if c(0) ne 0 then begin
	    ident = [[1.,0.,0.], [0.,1.,0.],[0.,0.,1.]]
	    if c(0) eq 2 then ident = state.value # ident  ;Object axes?
	    jmax = -1000.
	    for i=0,2 do begin		;Find closest
		t = total(arcball_constrain(pt1, ident(*,i)) * pt1)
		if t gt jmax then begin jmax = t & j = i & endif
		endfor
	    state.constrain(1) = j
	    state.pt0 = arcball_constrain(pt1, ident(*,j))
	endif else state.pt0 = pt1		;Constrained...
	state.buttons = 1
    endif else if state.buttons ne 0 then begin  ;Drag event
	if c(0) ne 0 then begin
	    ident = [[1.,0.,0.], [0.,1.,0.],[0.,0.,1.]]
	    if c(0) eq 2 then ident = state.value # ident  ;Object axes?
	    pt1 = arcball_constrain(pt1, ident(*,c(1)))	
	    endif
	state.pt1 = pt1
	pt0 = state.pt0
	q = [crossp(pt0, pt1), total(pt0*pt1)]  ;Quaternion
	state.cvalue = quaternion_m3(q) # state.value
	CW_ARCBALL_DRAW, state, /DO_ARC
	ENDIF
    if event.release ne 0 then begin
	state.value = state.cvalue
	state.buttons = 0
	if state.update_id eq 0 then goto, update  ;Auto update?
	endif
    ENDCASE
state.constrain_id: state.constrain(0) = event.index
state.help_id: cw_arcball_help, event.top
state.reset_id: BEGIN
    state.value =  [[1.,0.,0.], [0.,1.,0.],[0.,0.,1.]]
    state.cvalue = state.value
    CW_ARCBALL_DRAW, state
    if state.update_id eq 0 then goto, update  ;Auto update?
    ENDCASE
state.update_id: BEGIN
  update:
    value = { id: state.base, $
	top: event.top, $
	handler: event.handler, $
	value: state.value }
    WIDGET_CONTROL, child, SET_UVALUE=state, /NO_COPY  ;Save value
    return, value
    ENDCASE
ENDCASE

done:
WIDGET_CONTROL, child, SET_UVALUE=state, /NO_COPY 
return, 0
end

FUNCTION CW_ARCBALL_GET, id	;Return current 3x3 matrix
child = WIDGET_INFO(id, /CHILD)
WIDGET_CONTROL, child, GET_UVALUE=state, /NO_COPY
value = state.value
WIDGET_CONTROL, child, SET_UVALUE=state, /NO_COPY
return, value
end

PRO CW_ARCBALL_SET, id, value
child = WIDGET_INFO(id, /CHILD)
WIDGET_CONTROL, child, GET_UVALUE=state, /NO_COPY
if n_elements(value) eq 9 then BEGIN
	state.value = value
	state.cvalue = value
	ENDIF
CW_ARCBALL_DRAW, state
WIDGET_CONTROL, child, SET_UVALUE=state, /NO_COPY
end


FUNCTION CW_ARCBALL, parent, FRAME=frame, LABEL=label, $
	VALUE=sval, UVALUE=uvalue, RETAIN=retain, $
	SIZE=xsize, UPDATE = autou, COLORS = colors
;
  if keyword_set(parent) eq 0 then $
    parent = WIDGET_BASE(title='CW_ARCBALL')

  framet = keyword_set(frame)
  base = WIDGET_BASE(parent, FUNC_GET_VALUE='CW_ARCBALL_GET', $
	PRO_SET_VALUE='CW_ARCBALL_SET', EVENT_FUN='CW_ARCBALL_EVENT')
  if n_elements(uvalue) gt 0 then WIDGET_CONTROL, base, SET_UVALUE=uvalue

  base1 = WIDGET_BASE(base, /column, FRAME=framet)
  if n_elements(label) ne 0 then $
    junk = WIDGET_LABEL(base1, value=label)

  if n_elements(xsize) eq 0 then xsize = 192
  if n_elements(retain) le 0 then retain = 1
  draw_id = widget_draw(base1, xsize = xsize, ysize=xsize, $
    /BUTTON_EVENTS, /MOTION_EVENTS, UVALUE=base1, RETAIN = retain)
  constrain_id = CW_BSELECTOR(base1, ['None', 'View Axes', 'Object Axes'], $
	LABEL_LEFT = 'Constraint:', UVALUE=base1)
  junk = WIDGET_BASE(base1, /row)
  if keyword_set(autou) eq 0 then $
      update_id = WIDGET_BUTTON(junk, VALUE='Update', /NO_REL, UVALUE=base1) $
  else update_id = 0L
  reset_id = WIDGET_BUTTON(junk, VALUE='Reset', /NO_REL, UVALUE=base1)
  help_id = WIDGET_BUTTON(junk, VALUE='Help', /NO_REL, UVALUE=base1)

  if n_elements(sval) gt 0 then begin
	s = size(sval)
	if s(0) ne 2 or s(1) ne 3 or s(2) ne 3 then $
		message, 'Value must be a 3x3 matrix.'
	err = max(abs(sval - sval # transpose(sval) # sval))  ;Valid rot mat?
	if err gt 1.0e-6 then $
		message,'Value is an invalid rotation matrix'
  endif else sval = [[1.,0.,0.], [0.,1.,0.],[0.,0.,1.]]
; Colors are 0: View axes, 1: object axes, 2: top color, 3: sail color, 
;	4: bottom color, 5: background color.
  if n_elements(colors) ne 6 then colors = [ 1,7,2,3,7,0]

	

  np = 36               ;# of points in circle
  state = { CW_ARCB_STATE, $
    base: base, $		;Main base
    draw_id: draw_id, $		;Control id's
    constrain_id: constrain_id, $
    update_id: update_id, $
    reset_id: reset_id, $
    help_id: help_id, $
    wsize: xsize, $		;Window size
    radius: xsize * .42, $	;Circle radius
    center: xsize / 2., $	;Center x & y coords
    window: 0, $		;index
    circle: fltarr(2, np), $	;XY pnts for main circle
    colors: colors, $		;fixed axes, object axes, color1, color2
    value: sval, $		;Initial Rotation matrix
    cvalue: sval, $		;Current rotation matrix
    buttons: 0L, $		;mouse buttons
    pt0: fltarr(3), $		;Beginning of drag on sphere
    pt1: fltarr(3), $		;Current drag point on sphere
    constrain: [0,0]}		;type of constraint & axis index
  x = findgen(np) * ((2 * !pi) / (np-1))
  y = state.radius * sin(x) + state.center
  x = state.radius * cos(x) + state.center
  state.circle = transpose([[x],[y]])

  WIDGET_CONTROL, base1, SET_UVALUE=state, /NO_COPY
  return, base
END


;*****************************************************************
pro arcball_event, event	;Used by ARCBALL_TEST and ARCBALL_HELP
; If a button event is sent to this procedure, its top level base
; is destroyed, otherwise the event is printed.
;
if tag_names(event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' then $
    widget_control, event.top, /DESTROY $
else print, 'Arcball:', event.value
end

PRO ARCBALL_TEST		;Simple test program.
a = WIDGET_BASE(/COLUMN, TITLE='Arcball Test')
b = CW_ARCBALL(a, LABEL='Test me', size=256, /UPDATE)
c = WIDGET_BUTTON(a, value='Done')
WIDGET_CONTROL, a, /REALIZE
tek_color
WIDGET_CONTROL, b, SET_VALUE=1	;Draw first ball
XMANAGER, 'Arcball', a		;Manage it
end
