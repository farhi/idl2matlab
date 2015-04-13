; $Id: annotate.pro,v 1.10 1995/01/25 21:58:05 billo Exp $
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
; Parameter definitions:
; For all objects:
; 0	p0_x
; 1	p0_y
; 2	p1_x
; 3	p1_y
; 4	color index
; 5	thickness
; 6	linestyle
; 7	size
; 8	fill style (0=none, 1 = solid, 2 = lines
; 9	fill space
; 10	fill angle
; 11	npoints / nchars
; 12	alignment
; 13    font
; 14	orientation
; 15	head size
; 17    line/arrow/solid_arrow
; 18	eccen
; 19    object type
; 20    interpolation
;
; Modes:
;	0 Text
;	1 Lines/Arrows
;	2 Polylines/Polygons, Drag
;	3 Circles
;	4 Squares



pro xmgr_fake, top, window
; simulate the xmanager routine for a non-widgetized environment.
; deliver events from the ann widget as well as motion and button
; events from the selected idl window.

buttons = 0
ix0 = -1
iy0 = -1

while 1 do begin
    ev = widget_event(top, BAD_ID = bad, /NOWAIT)   ;Widget event?
    if bad ne 0L then return			;Gone? Done.
    if ev.id ne 0L then annotate_event, ev $	;Dispatch it.
    else begin				;Check for mouse
	if !d.window ne window then wset, window
	CURSOR, ix, iy, /DEV, /NOWAIT
        if (ix < iy) ge 0 then begin    ;In window?
	    j = !err			;New buttons
	    i = j xor buttons		;changed buttons
	    if i ne 0 then $		;Button change...
	        ANN_DRAW_EVENT, { ANN_FAKE_EVENT, $
		    x:ix, y:iy, press: i and j, release: i and buttons }, top $
	    else if ((ix ne ix0) or (iy ne iy0)) then $
	        ANN_DRAW_EVENT, { ANN_FAKE_EVENT, $
		    x:ix, y:iy, press: 0L, release: 0L }, top
	    ix0 = ix
	    iy0 = iy
	    buttons = j
            endif               ;ix, iy >= 0
    endelse                 ;no widget event
  endwhile
end



function b_button, a	;Return a 1 bit deep bitmap given a byte image.
			;width of image MUST be a multiple of 8.
s = size(a)
b = bytarr(s(1)/8, s(2))
if (s(1) and 7) ne 0 then $
	message, 'B_BUTTON: image width must be a multiple of 8.'
subs = lindgen(n_elements(b)) * 8
for ibit = 0,7 do b = b or byte(2^ibit) * (a(subs+ibit) ne 0b)
return, reverse(b,2)
end


FUNCTION ann_closest, xy, n, pos	;return index of closest point
d = (xy(0,0:n-1)-pos(0))^2 + (xy(1,0:n-1)-pos(1))^2
junk = min(d, i)
return, i
end

FUNCTION ann_closest_obj, st
WIDGET_CONTROL, st.objlist, GET_UVALUE=l  ;Object list
dmin = 1e6
pos = st.pos
psize = n_elements(st.p)			;Elements/object
nrows = n_elements(l)/psize
if n_elements(l) le 1 then return, 0
for i=0L, nrows-1 do begin  ;Each object
    p = l(*,i)			;Ith object
    k = i
    n = long(p(11))
    case p(19) of			;Which object?
0:  BEGIN			;A mess...
	x = p(2)		;Text width
	dx = cos(p(14)* !dtor) * x
	dy = sin(p(14)* !dtor) * x
        q = [p(0) + dx * (0.5-p(12)), p(1) + dy * (0.5-p(12))]
	i = i + (n + psize - 1)/psize	;Next object
    ENDCASE
1:  q = [p(0)+p(2), p(1)+p(3)]/2.   ;Lines
2:  BEGIN			;Polygon
	j = psize * (i+1)
	xy = reform(l(j: j+2*n-1), 2, n)  ;Extract points
	xmin=min(xy(0,*), max=xmax)
	ymin=min(xy(1,*), max=ymax)
	q = [xmin+xmax, ymin+ymax]/2.
	i = i + (2*n+psize-1)/psize	;Next element
    ENDCASE
3:  q = p([2,3])		;Circle
4:  q = [p(0)+p(2), p(1)+p(3)]/2.   ;box
else: q = pos + 10		;Unknown obj, dist = big
    ENDCASE
    d = total((pos - q)^2)	;Distance...
    if d lt dmin then begin dmin = d & imin = k & endif
ENDFOR

if dmin lt 0.04 then return, imin else return, -1
END				;Ann_closest_obj



PRO ANN_GET_NUM_EVENT, ev
on_ioerror, bad_again
widget_control, ev.id, get_value=v
widget_control, ev.top, get_uvalue=u
val = float(v(0))
if val lt u.minv or val gt u.maxv then goto, bad_again
WIDGET_CONTROL, u.id, SET_VALUE=v(0)	;Save the correct value
WIDGET_CONTROL, ev.top, /DESTROY	;Done
return
bad_again:  WIDGET_CONTROL, ev.id, SET_VALUE=''
return
END


pro CW_CONF_EVENT, ev	;On any event, save the value and kill it
WIDGET_CONTROL, ev.id, GET_UVALUE=i	;Button index
WIDGET_CONTROL, ev.top, GET_UVALUE=temp ;Temp storage location
WIDGET_CONTROL, temp, SET_UVALUE=i	;save choice
WIDGET_CONTROL, ev.top, /DESTROY	;Kill it
return
END


FUNCTION CW_CONFIRM, message, choices
; Make a Modal widget with the given message as text, and with buttons
;   corresponding to the choices.  Return the index of the button pressed.
;
    temp = WIDGET_BASE()
    a = WIDGET_BASE(title='Confirmation',/COLUMN, UVALUE=temp)
    for i=0, N_ELEMENTS(message)-1 DO junk = widget_label(a, value=message(i))
    b = WIDGET_BASE(a, /ROW)
    for i=0,n_elements(choices)-1 do $
	junk = WIDGET_BUTTON(b, VALUE=choices(i), /NO_REL, UVALUE=i)
    WIDGET_CONTROL, a, /REALIZE
    XMANAGER, 'Confirmation', a, /MODAL, EVENT_HANDLER='CW_CONF_EVENT'
    WIDGET_CONTROL, temp, GET_UVALUE=u, /DESTROY
return, u
end


FUNCTION ann_get_num, str, id, minv, maxv
; Get & check  a numeric value from a text widget.  If illegal format,
; use a modal widget to get a correct value.
	
on_ioerror, bad			;1st level call
v = float(str(0))
if v lt minv or v gt maxv then goto, bad
return, v			;OK

bad:  t = widget_base(title='Invalid Number', /column)
WIDGET_CONTROL, t, set_uvalue = { id:id, minv: minv, maxv: maxv} ;Save params
b = WIDGET_LABEL(t, value= 'An invalid number was entered')
b = WIDGET_LABEL(t, value = 'Range = '+string(minv)+' to '+string(maxv))
b = WIDGET_LABEL(t, value= 'Please enter the correct value')
j = WIDGET_TEXT(t, /frame, xsize=10, ysize=1, /EDIT)
WIDGET_CONTROL, t, /REAL
xmanager, 'Invalid Number', t, /MODAL, EVENT_HANDLER = 'ann_get_num_event'
WIDGET_CONTROL, id, GET_VALUE=v	;The correct value
return, float(v(0))
end



PRO ann_reset_mode, st, mode, submode, NOCLEAN = noclean
;Clean out the current object, reset mode

if n_elements(mode) le 0 then mode = st.mode	;Save old mode
if n_elements(submode) le 0 then submode = 0
newmode = st.mode ne mode

if newmode then begin
    m = st.mode_bases
    WIDGET_CONTROL, m(n_elements(m)-1), MAP=0	;Options base
    WIDGET_CONTROL, m(st.mode), MAP=0   ;Unmap old
    WIDGET_CONTROL, st.mode_buttons(mode), SET_BUTTON=1
    if mode ge 2 then begin		;Set polyfill controls?
	p = st.p
	WIDGET_CONTROL, st.spline_id, SET_VALUE=p(20)
        WIDGET_CONTROL, st.poly_style(mode), SET_VALUE=p(8)
	WIDGET_CONTROL, st.poly_angle(mode), SET_VALUE=p(10)
	WIDGET_CONTROL, st.poly_spacing(mode), SET_VALUE=p(9)
	endif
    st.mode = mode
    WIDGET_CONTROL, m(mode), MAP=1
    wset, st.draw_win		;Redraw the view window
    device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, st.backing(1)]
    endif

if keyword_set(noclean) eq 0 then begin	;Remove current object
    st.p(11) = 0
    endif

st.open = 0
st.minor_mode = submode
st.p(19) = st.mode
WIDGET_CONTROL, st.minor_mode_id, SET_VALUE=submode
end


PRO ann_add_object, st			;Add current object to list
wset, st.backing(1)			;Make it permanent
p = st.p
ann_draw_object, st, p			;Draw obj in backing store
wset, st.draw_win
device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, st.backing(1)] ;To draw window

psize = n_elements(p)
if p(19) eq 0 then begin		;text?
	n = strlen(st.txt)		;# of chars, Save in an array
	if n eq 0 then return		;Nothing to save
	p(11) = n
	a = bytarr(psize, (n+psize-1)/psize) ; Addtl bytes
	a(0) = byte(st.txt)
	p = [[p],[float(a)]]
endif else if p(19) eq 2 then begin	;Polygon?
	n = long(p(11))*2		;# of pnts required
	a = fltarr(psize, (n+psize-1)/psize)  ;Array into which to insert
	WIDGET_CONTROL, st.xy, GET_UVALUE=xy, /NO_COPY
	a(0) = xy(0:n-1)		;Insert it
	p = [[p], [a]]
endif	
	
WIDGET_CONTROL, st.objlist, GET_UVALUE = l, /NO_COPY
if n_elements(l) gt 1 then p = [[l], [p]]  ;Add to end of list
WIDGET_CONTROL, st.objlist, SET_UVALUE = p, /NO_COPY  ;Add to list
ann_reset_mode, st		;Reset mode
end				;Ann_add_object



pro ann_set_controls, st, p	;Set controls to corresp to object
WIDGET_CONTROL, st.color_id, SET_VALUE=p(4)
WIDGET_CONTROL, st.thick_id, SET_VALUE=p(5)*10.
WIDGET_CONTROL, st.linestyle_id, SET_VALUE=p(6)

case p(19) of
0:	BEGIN		;Text
	WIDGET_CONTROL, st.txt_size_id, SET_VALUE=p(7)*10.
	WIDGET_CONTROL, st.txt_id, SET_VALUE=st.txt
	WIDGET_CONTROL, st.ori_id, SET_VALUE=(p(14) + 360) mod 360
	WIDGET_CONTROL, st.txt_ali_id, SET_VALUE=p(12)*2
	WIDGET_CONTROL, st.txt_font_id, SET_VALUE=p(13)
	ENDCASE
1:	BEGIN		;Line/arrow
	WIDGET_CONTROL, st.line_arrow_id, SET_VALUE=p(17)
	WIDGET_CONTROL, st.head_size_id, SET_VALUE=p(15)*10.
	ENDCASE
2:	BEGIN	
	WIDGET_CONTROL, st.spline_id, SET_VALUE=p(20)
do_poly: i = long(p(19))
        WIDGET_CONTROL, st.poly_style(i), SET_VALUE=p(8)
	WIDGET_CONTROL, st.poly_angle(i), SET_VALUE=p(10)
	WIDGET_CONTROL, st.poly_spacing(i), SET_VALUE=p(9)
	ENDCASE
3: 	BEGIN		;Circle
	WIDGET_CONTROL, st.ecc_id, SET_VALUE=(p(18)-1.)*10.
	goto, do_poly
	ENDCASE
4:	goto, do_poly	;Rectangle
ENDCASE
END




pro ann_refresh_list, st, FROM_SCRATCH=redraw, WINDOW=window, MONO = mono
; FROM_SCRATCH = redraw from backing store(0)
; WINDOW = destination window
; MONO = color index to gray scale translation table

if n_elements(mono) le 0 then mono = 0
if n_elements(window) eq 1 then WSET, window

if keyword_set(redraw) then $	;Redraw from scratch?
    device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, st.backing(0)]

WIDGET_CONTROL, st.objlist, GET_UVALUE=l, /NO_COPY
if n_elements(l) le 1 then goto, done	;Nothing...
s = size(l)
psize = s(1)			;# of columns
n = n_elements(l) / psize	;# of rows
for i=0L, n-1 do begin		;Do each object....
    p = l(*,i)			;The object
    m = 0			;# of addtl elements
    k = (i+1) * psize		;Next row
    if p(19) eq 0 then begin	;Text
	m = long(p(11))
	ann_draw_object, { txt: string(byte(l(k:k+m-1)))} , p, MONO = mono
    endif else if p(19) eq 2 then begin  ;polygon?
	m = long(p(11))*2
	xy = reform(l(k:k+m-1), 2, m/2)
	ann_draw_object, st, p, xy=xy, MONO= mono
    endif else ann_draw_object, st, p, MONO= mono
    i = i + (m+psize-1)/psize	;Next row
    endfor
done:  WIDGET_CONTROL, st.objlist, SET_UVALUE=l, /NO_COPY
end



function ann_load_obj, st, obj	;Load nth object from list.  Remove
; the object from the list.  return 0 if error, 1 if ok.
; reload st with object.

WIDGET_CONTROL, st.objlist, GET_UVALUE = l, /NO_COPY
if n_elements(l) le 1 then begin
quit:   WIDGET_CONTROL, st.objlist, SET_UVALUE=l, /NO_COPY
   return, 0
   endif
psize = n_elements(st.p)
n = n_elements(l) / psize	;# of objects
if obj lt 0 or obj ge n then goto, quit  ;There?
p = l(*,obj)
k = (obj+1) * psize
m = 0
mode = fix(p(19))
if mode eq 0 then begin		;Text?
    m = long(p(11))
    st.txt = string(byte(l(k:k+m-1)))  ;fetch the text
endif else if mode eq 2 then begin	;Polygon?
    j = long(p(11))
    m = j*2				;# of elements
    xy = reform(l(k:k+m-1), 2, j)
    WIDGET_CONTROL, st.xy, SET_UVALUE=xy, /NO_COPY
endif else m = 0

st.p = p			;Get the object...

m = (m+psize-1)/psize		;# of addtl rows
; print, 'loaded',obj,', rows ',m,', n =',n
if n eq (m+1) then l = 0 $	;last object
else BEGIN			;Remove this object
    v = replicate(1,n)
    v(obj:obj+m) = 0		;Rows we remove
    l = l(*,where(v))
ENDELSE

WIDGET_CONTROL, st.objlist, SET_UVALUE = l, /NO_COPY  ;Restore list
ann_refresh_list, st, window=st.backing(1), /FROM_SCRATCH
ann_set_controls, st, p		;Reset the controls
return, 1
end

	

pro ann_xfer_file, st, file, SAVE=save, LOAD=load
; Transfer an annotate load/save file.

WIDGET_CONTROL, st.objlist, GET_UVALUE=l, /NO_COPY	;Current object list
psize = n_elements(st.p)
n = n_elements(l) / psize
magic = '414e4e00'XL

if keyword_set(save) then begin	;Save??
    if n eq 0 then begin
	i = CW_CONFIRM('There is nothing to save.', 'OK')
	WIDGET_CONTROL, st.objlist, SET_UVALUE=l, /NO_COPY
	return
	endif

    openw, unit, /GET_LUN, file, /XDR  ;Make the file
    writeu, unit, magic, psize	;Magic number, # of columns
    p = float([ !d.x_size, !d.y_size, !d.x_ch_size, !d.y_ch_size ])  ;Params
    writeu, unit, p

    for i=0L, n-1 do begin		;Write each object
	p = l(*,i)			;The object
	m = 0				;# of addtl elements
	k = (i+1) * psize		;Next row
	writeu, unit, p			;The array
	if p(19) eq 0 then begin	;Text?
	    m = long(p(11))		;Strlen
	    writeu, unit, byte(l(k:k+m-1))  ;The characters
	endif else if p(19) eq 2 then begin  ;Polygon?
	    m = long(p(11))*2
	    writeu, unit, l(k:k+m-1)	;The points
	endif
	i = i + (m+psize-1)/psize
	endfor
    writeu, unit, replicate(-1.0, psize)
    free_lun, unit
    WIDGET_CONTROL, st.objlist, SET_UVALUE=l, /NO_COPY  ;Restore

endif else begin				;LOAD
    openr,unit, /GET_LUN, file, /XDR, ERROR=i  ;Try to open it...
    if i lt 0 then begin		;No file..
	i = CW_CONFIRM(['File not found or unreadable', file], 'OK')
	return
	endif
    i = 0L			;Read magic
    columns = 0L
    readu, unit, i, columns
    if i ne magic then begin
	i = CW_CONFIRM(['File is not an annotate data file.', file], 'OK')
	return
	endif
    params = fltarr(4)
    readu, unit, params

    while 1 do begin		;Read each record
	p = fltarr(columns)
	readu, unit, p
;	print, p(19)
	if p(19) eq -1 then begin	;Done....
	    free_lun, unit
	    WIDGET_CONTROL, st.objlist, SET_UVALUE = l, /NO_COPY ;save list
	    ann_refresh_list, st, WINDOW=st.backing(1), /FROM_SCRATCH
	    wset, st.draw_win
	    device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, st.backing(1)]
	    return
	    endif
	m = 0L
	if p(19) eq 0 then begin	;string
	    m = long(p(11))		;# of addtl elements
	    a = bytarr(m)
	    readu, unit, a
	    p = [[p], [fltarr(psize, (m+psize-1)/psize)]]  ;what we add
	    p(psize) = a		;Insert chars....
	endif else if p(19) eq 2 then begin	;polygon
	    m = long(p(11))*2		;# of addtl elements
	    a = fltarr(m)
	    readu, unit, a
	    p = [[p], [fltarr(psize, (m+psize-1)/psize)]]  ;what we add
	    p(psize) = a
	endif
;		Adjust size for possible expansion....
	if columns lt psize then p = [p, fltarr(columns-psize)] $
	else if psize lt columns then p = p(0:psize-1)

	if n_elements(l) gt 1 then l = [[temporary(l)], [p]] $
	else l = p
	endwhile
endelse
end

pro annotate_ps, st, draw_ps, include_image

tvlct, r,g,b, /get              ;current color table
old = !d.name			;Current device name

if include_image then begin
    file = PICKFILE(FILE='annotate.ps', /WRITE, FILTER='*.ps', /NOCONF)
    if draw_ps then WSET, st.backing(0) $ ;Original image
    else WSET, st.backing(1)	;image + our annotations
    a = tvrd()
    endif

width = st.ps_width		;Output width
ch = [!d.x_ch_size, !d.y_ch_size] / float([!d.x_size, !d.y_size])
if st.ps_units then width = width / 2.54	;From cm to inches
height = width * !d.y_size / !d.x_size	;Height	
SET_PLOT,'PS'

if include_image then begin	;Include image with new file?
    DEVICE, file=file, COLOR=st.ps_color
    if st.ps_orien then DEVICE, /LANDSCAPE $
    else DEVICE, /PORTRAIT
    DEVICE, XSIZE = width, YSIZE=height, /INCHES	;Set size
    tvlct,r,g,b
    if st.ps_color eq 0 then begin	;To BW from RGB
	l = bytscl(r * .3 + .6 * g + .11 * b)
	a = l(a)
	endif
    tv, a                   ;Output image
    endif

if draw_ps then begin
    device, set_char = ch * !d.x_size  ;Proportional char size (aspect = same)
    ann_refresh_list, st, MONO = l
    endif

if include_image then device, /close
set_plot, old               ;done
end


pro annotate_event, ev

WIDGET_CONTROL, ev.top, GET_UVALUE = st, /NO_COPY   ;Our data structure
WIDGET_CONTROL, ev.id, GET_UVALUE = u		;The object uvalue

ldr = strmid(u, 0, 1)
IF ldr eq 'M' THEN BEGIN		;New mode?
    ann_reset_mode, st, fix(strmid(u,1,2)), 0
    goto, done
    ENDIF

IF ldr eq '#' THEN BEGIN
    WIDGET_CONTROL, ev.id, GET_VALUE = v
    ldr = '@'
ENDIF

IF ldr eq '@' THEN BEGIN		;Execute string?
	i = EXECUTE(strmid(u,2,100))	;Execute the string....
	IF strmid(u, 1, 1) eq 'R' THEN BEGIN ;Redraw?
redraw:   if st.open then begin
	    wset, st.draw_win
	    handle = st.minor_mode eq 1
	    p = st.p
	    ANN_DRAW_OBJECT, st, p, handle, REFRESH=st.backing(1)
	    st.p = p
	    st.handle = handle
	    ENDIF
	ENDIF		;Redraw
ENDIF ELSE IF u eq 'Help' THEN BEGIN	;ldr eq '@'
	xdisplayfile, filepath("annotate.txt", subdir=['help', 'widget']), $
;	xdisplayfile, 'annotate.txt', $
		title = "Annotate Help", $
		group = ev.top, $
		width = 72, height = 24
ENDIF else if u eq 'Options' THEN BEGIN
	m = st.mode_bases
	WIDGET_CONTROL, m(st.mode), MAP=0
	WIDGET_CONTROL, m(n_elements(m)-1), /MAP
ENDIF else if u eq 'Dismiss' THEN BEGIN
	m = st.mode_bases
	WIDGET_CONTROL, m(n_elements(m)-1), MAP=0
	WIDGET_CONTROL, m(st.mode), MAP=1
ENDIF ELSE IF u eq 'TOPROW' THEN BEGIN
    WIDGET_CONTROL, ev.top, /HOURGLASS
    case ev.value of
   'Exit' : BEGIN
	    for i=0,1 do wdelete, st.backing(i)
	    widget_control, ev.top, /DESTROY
	    return
	    ENDCASE
    'Save' : BEGIN
    save_file:	   if st.open then ann_add_object, st	;Add current object?
	   ann_xfer_file, st, st.file, /SAVE
	   ENDCASE
    'Save As': BEGIN
	st.file = PICKFILE(FILE=st.file, /WRITE)
	goto, save_file
	ENDCASE
    'Load': BEGIN
	st.file = PICKFILE(FILE=st.file, /READ, /MUST_EXIST, FILTER='*.dat')
	ann_xfer_file, st, st.file, /LOAD
	ENDCASE
    'TIFF': BEGIN
	file = PICKFILE(FILE='annotate.tif', /WRITE, FILTER='*.tif', /NOCONF)
	WSET, st.backing(1)
	tvlct, red, green, blue, /GET
	TIFF_WRITE, file, tvrd(/ORDER), 1, RED=red, GREEN=green, BLUE=blue
	ENDCASE
    'GIF': BEGIN
	file = PICKFILE(FILE='annotate.gif', /WRITE, FILTER='*.gif', /NOCONF)
	WSET, st.backing(1)
	WRITE_GIF, file, tvrd(ORDER=0)
	ENDCASE
    'PostScript': annotate_ps, st, 0, 1		;Postscript bitmap
    'Everything': annotate_ps, st, 1, 1		;do it with PS commands
    'Objects only': annotate_ps, st, 1, 0
    'Clear': BEGIN
	    WIDGET_CONTROL, st.objlist, SET_UVALUE=0  ;Clean up object list
	    wset, st.backing(1)		;redraw all objects except this one
	    device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, st.backing(0)]
	    wset, st.draw_win
	    device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, st.backing(0)]
	    ann_reset_mode, st, st.mode, 0
	ENDCASE
    ENDCASE
ENDIF ELSE IF u eq 'Save' THEN BEGIN
    if st.open THEN ann_add_object, st
ENDIF ELSE help, /st, u, ev


done: WIDGET_CONTROL, ev.top, SET_UVALUE = st, /NO_COPY
end


PRO ANN_MOVE_RESIZE, st, mode, first, last

if st.open eq 0 then return
handle = st.handle
pos = st.pos
p = st.p
if first then begin		;Initial hit?
    np = ([2,2,4,4,4])(mode)	;# of points
		;distances from handle points and midpoint
    h = handle(*,0:np-1)
    d = [[h], [total(h,2)/np]] - (pos # replicate(1.,np+1))
    d = min(total(d^2, 1), i)	;Dist squared, get closest
    if i eq np then i = 5	;always use 5 for last point
    if d lt 0.01 then begin	;Close enough?
	st.ihandle = i
	if mode eq 0 then begin	;Text mode?
	    if p(7) eq 0.0 then p(7) = 1.0	;Size
	    st.temp(0) = p(7) / p(2)		;1./(Length of siz=1)
	 endif else if mode eq 3 then begin	;Circle?  Save params.
	    c = p(2:3)
	    st.temp = [p(0:1) - c, total((pos - c)^2)]  ;dx,dy, len
	endif
        if i ne 5 then st.orig = handle(*,np-1-i) $  ;Anchor point
	else st.orig = pos
    endif else st.ihandle = -1
endif					;First

ihandle = st.ihandle
if ihandle lt 0 then return		;A hit?
d = pos - st.orig		;Movement
; if d(0) eq 0.0 and d(1) eq 0.0 and first eq 0 then goto, no_move

if ihandle eq 5 then begin		;Move?
    st.orig = pos		;Reset it
    if mode eq 2 then begin	;Polygons?
	if p(11) gt 1 THEN BEGIN   ;Polygons
	    WIDGET_CONTROL, st.xy, GET_UVALUE=xy, /NO_COPY
	    xy = xy + (d # replicate(1, p(11)))  ;Translate
	    WIDGET_CONTROL, st.xy, SET_UVALUE=xy, /NO_COPY
	    ENDIF
    endif else p(0) = p(0:3) + [d,d]  ;Just move origin
ENDIF ELSE BEGIN			;Sizing handle
    if (mode gt 1) then begin		;Need scale factor?
	s = handle(*,ihandle) - st.orig	;stretch factor
	if min(abs(s)) lt 0.001 then goto, no_move
	s = (pos - st.orig) / s	  ;Scale factor
	if min(abs(s)) lt 0.001 then goto, no_move
    endif
    case mode of
0:	BEGIN			;Text
	r = sqrt(total(d^2))		;Length of line
	if r lt 0.001 then goto, no_move
	p(14) = atan(d(1), d(0)) * !radeg	;New orientation
	if ihandle eq 0 then p(14) = p(14) + 180.
	p(7) = r * st.temp(0)		;New size
	k = long(3 * ihandle + 2 * p(12)) ;Subs from 0 to 5.
	p(0) = pos + ([0, -0.5, -1., -1, -0.5, 0])(k) * d
	ENDCASE
1:	p(ihandle * 2) = pos		;Line, just move vertex
2:	if p(11) gt 1 THEN BEGIN	;Polygon, Scale all verts
	    WIDGET_CONTROL, st.xy, GET_UVALUE=xy, /NO_COPY
	    xy0 = st.orig # replicate(1.0,p(11))
	    xy = (xy - xy0) * (s # replicate(1.0,p(11))) + xy0
	    WIDGET_CONTROL, st.xy, SET_UVALUE=xy, /NO_COPY
	    ENDIF
3:	BEGIN			;Circle
	    c = p(2:3)		;Center
	    r = sqrt(total((pos - c)^2)/st.temp(2))  ;Dist from ctr
	    p(0) = st.temp(0:1) * r + c
	ENDCASE
4:	BEGIN			;Rectangle
	    x0 = [st.orig, st.orig]
	    p(0) = (p(0:3) - x0) * s([0,1,0,1]) + x0
	ENDCASE
    ENDCASE
ENDELSE

no_move:
    ANN_DRAW_OBJECT, st, p, handle, REFRESH=st.backing(1)  ;Refresh
    st.p = p
    st.handle = handle
END			;ANN_MOVE_RESIZE




PRO ANN_DRAW_EVENT, ev, ann_base	;Handle events from drawable

if n_elements(ann_base) eq 0 then $
	WIDGET_CONTROL, ev.id, GET_UVALUE = ann_base
WIDGET_CONTROL, ann_base, GET_UVALUE=st, /NO_COPY
mode = st.mode

WSET, st.draw_win
if ev.press eq 4 then begin	;Right button to close object
	if st.open then ann_add_object, st
	goto, skip_it
endif else if ev.press eq 2 then begin
	if st.open eq 0 then goto, skip_it
	i = st.minor_mode eq 0	;Switch modes
        st.minor_mode = i	;Reset minor mode
	WIDGET_CONTROL, st.minor_mode_id, SET_VALUE=i
	p = st.p
	ANN_DRAW_OBJECT, st, p, i, REFRESH=st.backing(1)
	st.handle = i
	goto, skip_it
endif

pos = [ev.x, ev.y]

first = 0
drag = 0
last = 0
if st.buttons eq 0 then begin	;First time?
  if ev.press ne 1 then begin	;Move?
	if !x.s(1) ne 0 then pos = convert_coord(pos, /DEVICE, /TO_DATA)
	WIDGET_CONTROL, st.instruct, SET_VALUE=string(pos(0:1))
	goto, skip_it
  endif else begin
	first = 1
	WIDGET_CONTROL, st.instruct, SET_VALUE='Right button to close'
	st.buttons = 1
  endelse
endif else begin		;Not first
  if ev.release ne 1 then drag = 1 $
  else begin
	last = 1
	st.buttons = 0
  endelse
endelse


g = st.granularity
pos = round(pos/g)*g / [!d.x_size, !d.y_size]
if drag and (st.pos(0) eq pos(0)) and (st.pos(1) eq pos(1)) then goto, skip_it
st.pos = pos


if st.minor_mode eq 1 then begin	;MOVE/RESIZE
    ANN_MOVE_RESIZE, st, mode, first, last
    goto, skip_it
ENDIF ELSE if st.minor_mode eq 2 then begin	;Select
    if first then begin
	ann_reset_mode, st, st.mode, 2
	k = ann_closest_obj(st)
	if k ge 0 then begin		;Found one?
	   st.temp(0) = k
	   if ann_load_obj(st, k) then begin	;Find it?
		p = st.p
		st.minor_mode = 1	;Now in move/resize
		ann_reset_mode, st, p(19), 1, /NOCLEAN
		st.open = 1
		h = 1
		wset, st.draw_win
		ANN_DRAW_OBJECT, st, p, h, REFRESH=st.backing(1)
		st.handle = h
		ANN_MOVE_RESIZE, st, st.mode, first, last
		endif
	endif
    endif
    goto, skip_it
ENDIF			;Minor = 2

p = st.p

if st.open eq 0 then begin	;First time
	p(11) = 0
	WIDGET_CONTROL, st.fill_mode_id, SET_VALUE=0
	WIDGET_CONTROL, st.minor_mode_id, SET_VALUE=0
	st.fill_mode = 0
	st.minor_mode = 0
	ENDIF
st.open = 1

case mode of
0: BEGIN			;Text
    IF first THEN BEGIN
	WIDGET_CONTROL, st.txt_id, GET_VALUE=h
	st.txt = h(0)
	ENDIF
no_name_label:
    if first then p(2) = pos
    p(0) = pos
    h = last
    ANN_DRAW_OBJECT, st, p, h, REFRESH=st.backing(1)
    if last then begin
	st.handle = h
	st.minor_mode = 1		;Go to move resize mode
	WIDGET_CONTROL, st.minor_mode_id, SET_VALUE=1
	endif
   ENDCASE

1: goto, no_name_label		;Dragging line or arrow


2:  BEGIN			;Polygon/polyline
    n = long(p(11))
    if n eq 0L THEN BEGIN	;Initialize line buffer?
	xy = FLTARR(2,256)
	ENDIF ELSE WIDGET_CONTROL, st.xy, GET_UVALUE=xy, /NO_COPY
   case st.fill_mode of		;What are we doing?
   0: BEGIN		;Vector /drag mode
	if n_elements(xy) le n*2 then xy = [[xy], [FLTARR(2,n)]]  ;Extend it
	xy(0,n) = st.pos	;Last point
	if st.buttons then p(11) = n+1 	;Dragging
      ENDCASE		;Vector	
   1: BEGIN		;Edit mode
	if n eq 0 then goto, skip_it1
	if first then begin
		if n_elements(xy) le n*2 then xy = [[xy], [FLTARR(2,n)]]
		j = ann_closest(xy, n, pos)
		xy(0,n) = j
	endif else j = xy(0,n)
	xy(0,j) = pos		
      ENDCASE
   2: BEGIN		;Delete
	if (n le 0) or (last eq 0) then goto, skip_it1
	j = ann_closest(xy, n, pos)	;Point to delete
	p(11) = n-1
	if n eq 1 then goto, skip_it
	if j eq 0 then xy = xy(*,1:*) $
	else if j eq n-1 then xy = xy(*, 0:n-2) $
	else xy = [[xy(*,0:j-1)], [xy(*, j+1:*)]]
      ENDCASE		;delete
  ENDCASE		;st.fill_mode
  WIDGET_CONTROL, st.xy, SET_UVALUE=xy, /NO_COPY
  ANN_DRAW_OBJECT, st, p, REFRESH=st.backing(1)
 ENDCASE		;2

3: goto, no_name_label		;Circle


4: goto, no_name_label		;Box
ENDCASE

skip_it:
    if n_elements(p) gt 1 then st.p = p
    WIDGET_CONTROL, ann_base, SET_UVALUE=st, /NO_COPY
    return
skip_it1:
    WIDGET_CONTROL, st.xy, SET_UVALUE=xy, /NO_COPY
    goto, skip_it
end



pro ANN_DRAW_OBJECT, st, p, handle, REFRESH = refresh, XY=xy, MONO = mono

c = long(p(4))			;Color index
if keyword_set(mono) then c = mono(c)	;Outputting Postscript for BW?

if n_elements(refresh) gt 0 then $  ;Refresh?
	device, copy = [0,0, !d.x_size, !d.y_size, 0, 0, refresh]

case p(19) of		;What type of object
0: BEGIN		;Text
	str = '!' + strtrim(fix(p(13))>3,2) + st.txt + '!3'
	xyouts, p(0), p(1), /NORM, str, $
		COLOR = c, CHARSIZE = p(7), $
		ALI = p(12), ORI=p(14), CHARTHICK = p(5), WIDTH=x
	p(2) = x 		;Save width
	if keyword_set(handle) then begin
	    dx = cos(p(14)* !dtor) * x
	    dy = sin(p(14)* !dtor) * x
	    handle = [[ p(0)-p(12) * dx, p(1)-p(12) * dy], $
		[p(0) + (1.-p(12)) * dx, p(1) + (1.-p(12)) * dy]]
	    n = 2
do_handle:  PLOTS, handle, /NORM, PSYM=6
	    PLOTS, total(handle,2)/n, /NORM, PSYM=6  ;The center
	    ENDIF
   ENDCASE

1: BEGIN
    if p(17) ne 0 THEN BEGIN	;Lines/Arrows
	if p(15) le 0.0 then p(15) = 1.0
	ARROW, p(2), p(3), p(0), p(1), /NORM, $
		COLOR = c, THICK = p(5), SOLID=p(17) eq 2, $
		HSIZE = p(15) * !D.X_SIZE/20.
    ENDIF ELSE PLOTS, p([2,0]), p([3,1]), /NORM, COLOR=c, THICK=p(5), $
		LINESTYLE=p(6)
    if keyword_set(handle) then begin
	n = 2
	handle = [[p(0), p(1)], [p(2), p(3)]]
	goto, do_handle
	ENDIF
   ENDCASE

2: BEGIN		;Polygon
	n = p(11)
do_fill:
	if n gt 1 THEN BEGIN
	   if n_elements(xy) le 1 then WIDGET_CONTROL, st.xy, GET_UVALUE = xy
	   if (p(19) eq 2) and (p(20) eq 1) and (n gt 2) then $  ;Splines?
	       spline_p, reform(xy(0,0:n-1)), reform(xy(1,0:n-1)), $
			x, y, INTERV= 0.01 $
	   else begin			;Not splines
		x = xy(0,0:n-1)
		y = xy(1,0:n-1)
	   endelse
	   if (p(8) ne 1) or (n eq 2) THEN BEGIN   ;Outline it?
		plots, x, y, /NORM, COLOR = c, $  ;Draw outline
		    THICK = p(5), LINESTYLE = p(6)
		if p(8) eq 2 THEN BEGIN  ;line fill?
		   plots, x([n-1,0]), y([n-1,0]), /NORM, COLOR = c, $ ;Close it
		    THICK = p(5), LINESTYLE = p(6)
		   if n ge 3 THEN POLYFILL, x, y, /NORM, COLOR = c, $
			THICK = p(5), ORI = p(10), SPACING = p(9)/100.
		ENDIF
	  ENDIF ELSE IF N GE 3 THEN $		;Solid fill
		POLYFILL, x,y, /NORM, COLOR = c
	  ENDIF
	if keyword_set(handle) then begin	;Draw resizing handles
	   xmin=min(x, max=xmax)
	   ymin=min(y, max=ymax)
	   n = 4
	   handle = [[xmin, ymin], [xmin, ymax], [xmax,ymin],[xmax, ymax]]
	   goto, do_handle
	   ENDIF
   ENDCASE
3: BEGIN		;Circle/ Ellipse
	a = !d.x_size / float(!d.y_size)	;Aspect ratio
	dx = p(0) - p(2)
	dy = p(1) - p(3)
	r = sqrt(dx^2 + dy^2)
	if r eq 0.0 then return
	dx = dx/r
	dy = dy/r
	n = 128
	t = findgen(n) * (( 2 * !pi)/ (n-1))	;Angles..
;			The whole ball of wax
	xy = [[cos(t) * r], [sin(t) * r/p(18)]] # [[dx, -dy],[dy, dx]]
	xy = reform([p(2) + xy(*,0), p(3) + a*xy(*,1)], n,2,/OVER)
	xy = transpose(xy)
	goto, do_fill
   ENDCASE	;Circle
4: BEGIN	;Box
	xy = p([[0,1],[0,3],[2,3],[2,1],[0,1]])
	n = 5
	goto, do_fill
   ENDCASE
else: print,'Unknown object'
ENDCASE

end



pro ann_make_draw_button, st
; Make mode buttons
a = widget_base(st.base, /ROW, Exclusive=!version.os ne 'Win32')
id = lonarr(n_elements(st.mode_buttons))	;Id arrays

wsize = 32
window, xsize=wsize, ysize=wsize, /pix, /free
w2 = wsize/2			;Handy constants
w9 = 9*wsize/10
w1 = wsize/10
w3 = wsize/3

xyouts, w1, wsize/4, 'Abc', /dev, chars= wsize/(3.2* !d.x_ch_size)	;Text
id(0) = WIDGET_BUTTON(a, /NO_REL, value= b_button(tvrd()))
erase

arrow, w1, w3, w9, w3, /device, hsize = 10	;Arrows/ lines
plots, [w1, w9], wsize - [w3, w3], lines=3, /DEV
id(1) = WIDGET_BUTTON(a, /NO_REL, value= b_button(tvrd()))
erase

polyfill, [w1,w2,w1], [w1, w9, w9], /dev	;Polygons
plots, [ 2, 4, 6, 8, 10, 12, 10, 8, 8]*wsize/16, $	;Drag/Draw
       [ 1, 3, 7, 9, 9,  11, 16, 14, 6]*wsize/20, /dev
id(2) = WIDGET_BUTTON(a, /NO_REL, value= b_button(tvrd()))
erase


n = 32				;Circles
t = findgen(n) *(2 * !pi / (n-1))
plots, wsize/3 * cos(t) + (w2), wsize/3 * sin(t) + w2, /dev
id(3) = WIDGET_BUTTON(a, /NO_REL, value= b_button(tvrd()))
erase

plots, [w1,w1, w9, w9, w1], [w1, w9, w9, w1, w1], /dev   ;Squares
id(4) = WIDGET_BUTTON(a, /NO_REL, value= b_button(tvrd()))
erase

for i=0, n_elements(id)-1 do $		;Set up uv = Mn
	WIDGET_CONTROL, id(i), SET_UVALUE='M'+strtrim(i,2)
WIDGET_CONTROL, id(0), /SET_BUTTON	;Set initial choice
wdelete
st.mode_buttons = id
end





PRO ANNOTATE, DRAWABLE = draw, WINDOW = window, LOAD_FILE=load_file, $
	COLOR_INDICES=color_indices, TEK_COLORS = tek_colors

;+
; NAME:
;	ANNOTATE
;
; PURPOSE:
;	This procedure is a general purpose drawing program/widget to
;	annotate displays. Drawing objects include text, lines, arrows,
;	polygons, rectangles, circles, and ellipses.
;
; CATEGORY:
;	Widgets.  Will also work with plain windows.
;
; CALLING SEQUENCE:
;	ANNOTATE
;
; INPUTS:
;	No required inputs.
;
; KEYWORD PARAMETERS:
;	COLOR_INDICES:	an array of color indices from which the user
;			can choose colors. For example, to allow the user
;			to choose 10 colors, spread evenly over the
;			available indices, set the keyword as folows:
;			COLOR_INDICES = INDGEN(10) * (!D.N_COLORS-1) / 9
;	DRAWABLE:	the widget ID of the draw widget for the annotations.
;			This is mutually exclusive with WINDOW.
;	LOAD_FILE:	the name of an annotation format file to load after
;			initialization.
;	TEK_COLORS:	if set, the Tektronix color table is loaded
;			starting at color index TEK_COLORS(0), with
;			TEK_COLORS(1) color indices. The Tektronix color
;			table contains up to 32 distinct colors suitable
;			for graphics.
;	WINDOW:		the window index number of the window to receive the
;			annotations.
;
; OUTPUTS:
;	This procedure has no explicit outputs. Menu choices exist to
;	write TIFF, GIF, or PostScript bitmap files. Encapsulated
;	or standalone PostScript files may also be created.
;
; SIDE EFFECTS:
;	Annotations are made in the designated window or draw widget.
;
; RESTRICTIONS:
;	This is a simple drawing program.
;
; PROCEDURE:
;	If neither TEK_COLORS or COLOR_INDICES are specified, the default
;	is to load 10 colors, evenly distributed over those available.
;
;	If neither WINDOW or DRAWABLE are specified, the current window
;	is used.
;
; EXAMPLE:
;	TVSCL, HANNING(300,200)	;Output an image in the current window
;	ANNOTATE		;Annotate it
;
; MODIFICATION HISTORY:
;	DMS, RSI, July, 1993.  Original version.
;-



nmodes = 5
sl_width = 128

st = { ANN_STATE, $
    mode : 0, $
    minor_mode_id: 0L, $
    minor_mode: 0, $
    p: fltarr(21), $		;General params
    open : 0, $			;NE 0 if an object is open
    base : 0L, $
    current: 0L, $		;Current object if saved on list
    objlist : 0L, $		;object container
    xy: 0L, $
    pos:  fltarr(2), $		;Current position
    orig: fltarr(2), $		;Beginning of drag
    txt: 'Text',$		;Current contents of text widget
    mode_buttons: lonarr(nmodes), $
    mode_bases : lonarr(nmodes+1), $
    draw_win : 0L, $
    backing : lonarr(2), $	;0 = original bitmap, 1 = with closed objects
    color_id: 0L, $
    thick_id: 0L, $
    txt_id: 0L, $
    txt_size_id: 0L, $
    txt_ali_id: 0L, $
    txt_font_id: 0L, $
    ori_id: 0L, $
    linestyle_id : 0L, $
    buttons: 0L, $
    granularity : 1.0, $
    instruct: 0L, $
    line_arrow_id: 0L, $
    head_size_id: 0L, $
    fill_mode_id: 0L, $
    fill_mode: 0, $
    handle: fltarr(2,4), $
    temp: fltarr(3), $
    poly_style: lonarr(nmodes), $
    poly_angle: lonarr(nmodes), $
    poly_spacing: lonarr(nmodes), $
    ecc_id: 0L, $
    spline_id: 0L, $
    file: 'annotate.dat', $
    ihandle: 0, $
    ps_encap: 0, $
    ps_color: 0, $
    ps_orien: 0, $
    ps_width: 5.0, $
    ps_units: 0 }

st.p(18) = 1.0	;Eccent

ann_base = WIDGET_BASE(title='Annotate', /COLUMN)	;Our base
st.base = ann_base

win = !d.window		;Default window
use_xmgr = 0
if n_elements(window) eq 1 then win = window $
else if n_elements(draw) eq 1 then begin
	widget_control, draw, get_value = win, EVENT_PRO = 'ANN_DRAW_EVENT', $
	    SET_UVALUE = ann_base
	use_xmgr = 1
	endif
if win lt 0 then message,'No draw window active'
st.draw_win = win


wset, win
nx = !d.x_size
ny = !d.y_size

for i=0,1 do begin		;Get 2 backing pixmaps.
    window, /FREE, /PIXMAP, xs = nx, ys = ny
    device, copy = [0,0,nx, ny, 0, 0, win]  ;Copy original window
    st.backing(i) = !d.window
    endfor


st.objlist = WIDGET_BASE(ann_base, UVALUE = 0, $	;Object list holder
    xsize = 2, ysize=2)
st.xy = WIDGET_BASE(ann_base, UVALUE = 0, $	;Polygon points holder
    xsize = 2, ysize=2)

junk = WIDGET_BASE(ann_base, /ROW)
tmp = CW_PDMENU(junk, /RETURN_NAME, UVALUE='TOPROW', [ $
	{CW_PDMENU_S, flags: 1, name: 'File'}, $
	{CW_PDMENU_S, flags: 0, name: 'Load'}, $
	{CW_PDMENU_S, flags: 0, name: 'Save'}, $
	{CW_PDMENU_S, flags: 0, name: 'Save As'}, $
	{CW_PDMENU_S, flags: 1, name: 'Write PostScript'}, $
	{CW_PDMENU_S, flags: 0, name: 'Everything'}, $
	{CW_PDMENU_S, flags: 2, name: 'Objects only'}, $
	{CW_PDMENU_S, flags: 1, name: 'Export Bitmap'}, $
	{CW_PDMENU_S, flags: 0, name: 'GIF'}, $
	{CW_PDMENU_S, flags: 0, name: 'PostScript'}, $
	{CW_PDMENU_S, flags: 2, name: 'TIFF'}, $
	{CW_PDMENU_S, flags: 0, name: 'Clear'}, $
	{CW_PDMENU_S, flags: 2, name: 'Exit'}])

tmp = WIDGET_BUTTON(junk, value='Help', UVALUE='Help', /NO_REL)
tmp = WIDGET_BUTTON(junk, value='Options', UVALUE='Options', /NO_REL)

junk = WIDGET_BASE(ann_base, /ROW)
tmp = WIDGET_BUTTON(junk, VALUE='Save', UVALUE='Save', /NO_REL)
st.minor_mode_id = CW_BGROUP(junk, /EXCLUSIVE, /ROW, /NO_REL, $
	LABEL_LEFT = 'Mode:', $
	['Draw', 'Edit','Select'], $
        UVALUE='@Rst.minor_mode = ev.value', SET_VALUE=0)

ann_make_draw_button, st

scolor = 0
ncolors = 10
if n_elements(tek_colors) ge 1 then begin
    scolor = tek_colors(0)
    if n_elements(tek_colors) ge 2 then ncolors = tek_colors(1) else ncolors=8
    color_indices = indgen(ncolors) + scolor
endif else if n_elements(color_indices) gt 0 then begin
	ncolors = n_elements(color_indices)
endif else begin
	color_indices = (!d.n_colors-1) * lindgen(ncolors) / (ncolors-1)
endelse

st.color_id = CW_CLR_INDEX(ann_base, LABEL = 'Color:', XSIZE=160, $
	NCOLORS = ncolors, START_COLOR = scolor, $
	COLOR_VALUES=color_indices, uvalue = '@Rst.p(4) = ev.value')
st.p(4) = color_indices(ncolors-1)	;Init color

junk = WIDGET_BASE(ann_base, /ROW)
st.linestyle_id = CW_BSELECTOR(junk, LABEL_TOP = 'Linestyle:', $
	['Solid', 'Dots', 'Dashes', 'Dash-Dot', 'Dash-3 Dots', $
	'Long Dash'], uvalue = '@Rst.p(6) = ev.value')
st.thick_id = WIDGET_SLIDER(junk, XSIZE = sl_width, MIN=0, $
	MAX=200, TITLE = 'Thickness', /DRAG, $
	UVALUE='@Rst.p(5) = ev.value/10.')

st.instruct = WIDGET_TEXT(ann_base, value = ' ', xsize=32, ysize=1, /FRAME)

tmp_base = WIDGET_BASE(ann_base, /FRAME)
for i=0, nmodes do BEGIN
	base = WIDGET_BASE(tmp_base, /COLUMN)
	st.mode_bases(i) = base
	WIDGET_CONTROL, base, map = i eq 0
	ENDFOR

;
; Text widget
top = st.mode_bases(0)
junk = WIDGET_BASE(top, /ROW)
junk1 = WIDGET_LABEL(junk, Value = 'Text: ')
st.txt_id = WIDGET_TEXT(junk, xs = 32, ys = 1, /frame, /edit, $
	uvalue = '#Rst.txt=v(0)', value=st.txt)
junk = WIDGET_BASE(top, /ROW)
st.txt_size_id = WIDGET_SLIDER(junk, TITLE='Size', Value=0, XSIZE = sl_width, $
	MAX=100, MIN=0, /DRAG, UVALUE = '@Rst.p(7) = ev.value/10.')
st.ori_id = WIDGET_SLIDER(junk, TITLE='Orientation', Value = 0, $
	MAX = 360, min = 0, /DRAG, UVALUE = '@Rst.p(14) = ev.value', $
	XSIZE = sl_width)

st.txt_ali_id = CW_BGROUP(top, LABEL_LEFT='Alignment:', /EXCLUSIVE, /ROW, $
	['Left', 'Center', 'Right' ], SET_VALUE=0, /NO_REL, $
	UVALUE='@Rst.p(12) = ev.value/2.')
fonts = ['Simplex Roman  ', 'Simplex Greek', 'Duplex Roman', $
	'Complex Roman', 'Complex Greek', 'Complex Italic', $
	'Math & Special', 'Special', 'Gothic', 'Script', 'Complex Script', $
	'Gothic Italian', 'Gothic German', 'Cyrillic', 'Triplex Roman', $
	'Triplex Italic']
junk = WIDGET_BASE(top, /ROW)
junk1 = WIDGET_LABEL(junk, VALUE = 'Font:')
st.txt_font_id = CW_BSELECTOR(junk, fonts, $
	uvalue = '@Rst.p(13) = ev.value + 3')

; Arrow Widget
top = st.mode_bases(1)
junk = WIDGET_BASE(top, /ROW)
st.line_arrow_id = CW_BGROUP(junk, /EXCLUSIVE, /ROW, /NO_REL, $
	['Line','Arrow', 'Solid Arrow'],  $
	UVALUE='@Rst.p(17)=ev.value', SET_VALUE=0, LABEL_LEFT='Mode:')
st.head_size_id = WIDGET_SLIDER(top, TITLE='Head Size', VALUE = 0, $
	XSIZE=sl_width, MAX=100, /DRAG, UVALUE='@Rst.p(15) = ev.value/10.')


	
; Polygon Widget
top = st.mode_bases(2)
st.fill_mode_id = CW_BGROUP(top, /EXCLUSIVE, /ROW, LABEL_LEFT = 'Mode:', $
	['Draw', 'Edit', 'Delete'], /NO_REL, $
        UVALUE='@Rst.fill_mode = ev.value', SET_VALUE=0)


; Circle widget
top = st.mode_bases(3)
st.ecc_id = WIDGET_SLIDER(top, TITLE='Eccentricity', VALU=0, /DRAG, $
        XSIZE=sl_width, MIN=0, MAX=100, UVALUE='@Rst.p(18)=ev.value/10.+1')

; Square Widget  has nothing unique


for i=2, nmodes-1 do  begin	;Make polyfill controls for the bases
    top = st.mode_bases(i)
    st.poly_style(i) = CW_BGROUP(top, /EXCLUSIVE, /ROW, $
	LABEL_LEFT = 'Fill: ', /NO_REL, $
	['None', 'Solid', 'Lines'],  $
        UVALUE='@Rst.p(8)=ev.value', SET_VALUE=0)
    if i eq 2 then $
	st.spline_id = CW_BGROUP(top, /EXCLUSIVE, /ROW, /NO_REL, $
		LABEL_LEFT='Interpolation:', ['None', 'Spline'], $
		UVALUE='@Rst.p(20)=ev.value', SET_VALUE=0)
    junk = WIDGET_BASE(top, /ROW)
    st.poly_angle(i) = WIDGET_SLIDER(junk, TITLE='Line Angle', VALU=0, /DRAG, $
        MIN=0, MAX=180, UVALUE='@Rst.p(10)=ev.value', XSIZE=sl_width)
    st.poly_spacing(i) = WIDGET_SLIDER(junk, TITLE='Line Spacing', $
	VALU=0, /DRAG, MIN=0, MAX=50, UVALUE='@Rst.p(9)=ev.value', $
	XSIZE=sl_width)
    endfor



;*******************************************



; Options Widget
top = st.mode_bases(nmodes)
junk = WIDGET_BASE(top, /ROW)
junk1 = WIDGET_LABEL(junk, value = 'Grid granularity:')
junk1 = WIDGET_TEXT(junk, /EDIT, /FRAME, xsize=4, ysize=1, value='1', $
	UVALUE = '# st.granularity=ann_get_num(v(0), ev.id, 1, 128)')

junk = WIDGET_BASE(top, /COLUMN, /FRAME)
junk1 = WIDGET_LABEL(junk, VALUE= 'PostScript Options')
junk1 = WIDGET_BASE(junk, /ROW)
junk2 = CW_BGROUP(junk1, /EXCLUSIVE, /ROW, /NO_REL, $
	['Std', 'Encapsulated'], $
	UVALUE='@ st.ps_encap=ev.value', SET_VALUE=0)
junk2 = CW_BGROUP(junk1, /EXCLUSIVE, /ROW, /NO_REL, $
	['Mono', 'Color'], $
	UVALUE='@ st.ps_color=ev.value', SET_VALUE=0)
junk1 = CW_BGROUP(junk, /EXCLUSIVE, /ROW, /NO_REL, $
	['Portrait', 'Landscape'], $
	UVALUE='@ st.ps_orien=ev.value', SET_VALUE=0)
junk1 = WIDGET_BASE(junk, /ROW)
junk2 = WIDGET_LABEL(junk1, VALUE='Width:')
junk2 = WIDGET_TEXT(junk1, /EDIT, /FRAME, xsize=5, ysize=1, value='5.0', $
	UVALUE='# st.ps_width=ann_get_num(v(0), ev.id, .1, 200)')
junk2 = CW_BGROUP(junk1, /EXCLUSIVE, /ROW, /NO_REL, $
	['In', 'Cm'], UVALUE='@ st.ps_units=ev.value', SET_VALUE=0)

junk = WIDGET_BUTTON(top, VALUE='Dismiss', UVALUE='Dismiss')


wset, win
wshow, win

if n_elements(tek_colors) ge 1 then tek_color, scolor, ncolors

widget_control, ann_base, /REAL
WIDGET_CONTROL, st.color_id, SET_VALUE = color_indices(ncolors-1)   ;Set color

if keyword_set(load_file) then $	;Load a file?
	ann_xfer_file, st, load_file, /LOAD
WIDGET_CONTROL, ann_base, SET_UVALUE = st, /NO_COPY

if use_xmgr then xmanager, 'annotate', ann_base $
else xmgr_fake, ann_base, win
end

