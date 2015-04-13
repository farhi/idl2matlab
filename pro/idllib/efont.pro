; $Id: efont.pro,v 1.5 1995/01/26 03:57:53 billo Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.


Function MESSAGE_BOX, msg, YES_NO = yes_no, OK_CANCEL = OK_CANCEL
; Display strings in MSG (may be an array), in a modal dialog box.
; 

a = widget_base(/column)

for i=0, n_elements(msg)-1 do $
	b = widget_text(a, value = msg(i), xsize = strlen(msg(i)))
c = WIDGET_BASE(a, /ROW)
if keyword_set(yes_no) then begin
	r1 = WIDGET_BUTTON(c, value = 'Yes')
	r0 = WIDGET_BUTTON(c, value = 'No')
endif else if keyword_set(ok_cancel) then begin
	r1 = WIDGET_BUTTON(c, value = 'OK')
	r0 = WIDGET_BUTTON(c, value = 'Cancel')
endif else r1 = WIDGET_BUTTON(c, value='OK')

WIDGET_CONTROL, a, /realize
; XMANAGER, 'Message_box', a, EVENT_HANDLER = 'KILL_ME', /MODAL
x = widget_event(a)
widget_control, x.top, /destroy
return, x.id eq r1
end


PRO translate_vects, in, x, y, pen_up   ;Given packed vectors,
; return the X, Y, and Pen_up components.

x = (ishft(in, -7) and 127)	;Get X and Y components
y = (in and 127)
neg = where(x and 64, count)
if count gt 0 then x(neg) = x(neg)-128
neg = where(y and 64, count)
if count gt 0 then y(neg) = y(neg)-128
pen_up = (in and 16384) ne 0		;Pen up bit
end


PRO draw_char, x0, y0, siz, vects, color = c

if vects(0) eq -1 then return  ;Anything?
n = n_elements(vects)
if (vects(0) and 16384) eq 0 then begin	;Scale factor & offset?
	s = vects(0)/500. * siz
	off = vects(1)
	st = 2
endif else begin
	s = siz
	off = 0
	st = 0
endelse

translate_vects, vects, x, y, pen_up

if n_elements(c) le 0 then c = 1
for i=st, n-1 do plots, s * x(i)+x0, s * y(i)+y0+off, /DEVICE, $
	CONTINUE = pen_up(i) eq 0, COLOR = c
end


PRO redraw, map
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

if n_elements(map) gt 0 then begin
	mapped = map
	WIDGET_CONTROL, fdraw, MAP=map eq 0
	WIDGET_CONTROL, cdraw, MAP=map eq 1
endif
	
WIDGET_CONTROL, fnum_txt, SET_VALUE='Font '+strtrim(fnum,2)
if mapped eq 0 then begin
	wset, fwin
	erase
	draw_font
endif else begin
	wset, cwin
	erase
	draw_grid
	plots, [1,1] * sx * cur_chartab.width + x_0, [0, !d.y_size-1], $
		color=4, /DEV, LINES=2
	if cur_vects(0) ne -1 then draw_char, x_0, y_0, sx, cur_vects
	t = '"' + string(byte(cur_char)) + '" = ' + $
	   strtrim(cur_char,2) + '(10) '+ $
	    string(cur_char, format='(O3)') + '(8) ' +  $
	    string(cur_char, format='(Z2)') + '(16). ' + $
	    strtrim(n_elements(cur_vects), 2) + ' Vectors'
	WIDGET_CONTROL, cinfo_txt, SET_VALUE = t
	WIDGET_CONTROL, cwidth_txt, set_value=strtrim(fix(cur_chartab.width),2)
	endelse
END




PRO draw_font
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

for i=0,15 do xyouts, (i+2)*chx, !d.y_size - chy, /DEV, siz=1.5, $
	string(i, format='(z1)')
for i= 2, (nchars+32)/16-1 do begin	;Rows
	y = !d.y_size - (i+0) * chy
	xyouts, 0, y, /DEV, string(i, format='(z1)'), siz=1.5
	for j=0, 15 DO BEGIN
	    c = i*16 + j
	    offset = chartab(c-32).offset
	    nv = chartab(c-32).nvecs
	    if nv ne 0 then begin
		draw_char, (j+2) *chx, y, 1.0, vectors(offset : offset + nv-1)
		endif
	    ENDFOR
	ENDFOR
END


PRO draw_grid, color
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

if n_elements(color) le 0 then color = 3

nx = !d.x_size / sx
ny = !d.y_size / sx
x_0 = (!d.x_size/2 - sx * 16 + sx-1)/sx * sx  ;Grid point of origin
y_0 = (!d.y_size/2 - sx * 16 + sx-1)/sx * sx
c = 3

if sx le 8 then dx = (16/sx) * sx else dx = sx
for i=0,!d.x_size-1, dx do for j=0, !d.y_size-1, dx do $
	plots, i, j, psym=1, color=c, /dev
plots, [0, !d.x_size-1], [y_0, y_0], /DEV, color=2, lines=2
plots, [x_0, x_0], [0, !d.y_size-1], /DEV, color=2, lines=2
return

for i=0, !d.x_size-1, sx do begin
	if (i-x_0 eq 0) or (i-x_0 eq 32*sx) then l = 2 else l = 1
	plots, [i, i], [0, !d.y_size-1], /DEV, color = c, LINES=L
	endfor

for i=0, !d.y_size-1, sx do begin
	if (i-y_0 eq 0) or (i-y_0 eq 32*sx) then l=2 else l =1
	plots, [0, !d.x_size-1], [ i, i],  /DEV, color = c, LINES=l
	endfor
end

PRO ADD_VECTOR, v0, v1		;V0 and v1 are in screen coords..
; dup_move = 0 to add a vector, 1 to move a vector by v1-v0, and 2 to
;  move & duplicate.
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move


x0 = FIX((v0(0) - x_0) / sx) and 127	;To our coords
x1 = FIX((v1(0) - x_0) / sx) and 127
y0 = FIX((v0(1) - y_0) / sx) and 127
y1 = FIX((v1(1) - y_0) / sx) and 127

if x0 eq x1 and y0 eq y1 then return	;Nothing to add
e0 = fix(ishft(x0, 7) + y0)		;Encoded new vects
e1 = fix(ishft(x1, 7) + y1)

if cur_vects(0) eq -1 then begin	;No vectors yet
	cur_vects = [ e0 + 16384, e1]
	return
	endif

if dup_move eq 0 then begin
	cur_vects = [ cur_Vects, e0 + 16384, e1]  ;Dumb way
	return
	endif

translate_vects, cur_vects, x, y, pen_up	;Disassemble
case dup_move of
1:	BEGIN
	x = x + (x1-x0)
	y = y + (y1-y0)
	ENDCASE
2:	BEGIN
	x = [ x, x + (x1-x0)]
	y = [ y, y + (y1-y0)]
	pen_up = [pen_up, pen_up]
	ENDCASE
3:	BEGIN
	x = fix(x * float(x1)/float(x0) + 0.5)
	y = fix(y * float(y1)/float(y0) + 0.5)
	ENDCASE
ENDCASE
cur_vects = ishft(x and 127,7) + (y and 127) + 16384 * pen_up  ;Recombine
dup_move = 0
redraw
end


PRO Order_vectors, ctab, cvects
; Order the vectors for the character, combining where possible.

common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

print,'Segments in  = ', fix(total((cvects and 16384) ne 0))
merge_loop: 
    n = n_elements(cvects)
    bsegs = where((cvects and 16384) ne 0, count)	;Beginning segments
    if count lt 2 then goto, merge_done
    esegs = [ bsegs(1:*) - 1, n-1]		;Ending segments

    b = cvects(bsegs) and 16383
    e = cvects(esegs)
    for i=0, count-2 do for j=i+1,count-1 do begin    ;Dumb search
	if b(i) eq e(j) then begin
	    v = [ cvects(bsegs(j):esegs(j)), cvects(bsegs(i)+1:esegs(i))]
	    goto, merge_vects
	    endif
	if b(i) eq b(j) then begin 
	    v = [ reverse(cvects(bsegs(j):esegs(j))), $
		  cvects(bsegs(i)+1: esegs(i))]
	    goto, merge_vects
	    endif
	if e(i) eq e(j) then begin
	    v = [ cvects(bsegs(i):esegs(i)), $
		reverse(cvects(bsegs(j): esegs(j)-1))]
	    goto, merge_vects
	    endif
	if e(i) eq b(j) then begin
	    v = [ cvects(bsegs(i):esegs(i)), cvects(bsegs(j)+1:esegs(j))]
	merge_vects: v = v and 16383	;Mask off pen up bits
	    v(0) = v(0) or 16384	;Pen up on first pnt
	    if count eq 2 then cvects = v $	;Only two
	    else begin
	        k = replicate(1, n)
	        k(bsegs(j):esegs(j)) = 0
	        k(bsegs(i):esegs(i)) = 0
	        cvects = [ cvects(where(k)), v]	;Combine
	    endelse
	    goto, merge_loop
	endif
    endfor

merge_done: ctab.nvecs = n_elements(cvects)
print,'Segments out = ', fix(total((cvects and 16384) ne 0))
end


Function pnt_line, x0, y0, lx0, ly0, lx1, ly1
;  Return the perpendicular distance between the line thru (lx0, ly0)
;  and (lx1, ly1) and the point x0, y0.
; Add to that distance, the distance to the closest point if the 
; perpendicular is not on the line segment.
; 
p0 = float([x0, y0])
p1 = [lx1, ly1]
lu = [lx0, ly0]

lv = float(p1 - lu)
l = sqrt(total(lv*lv))
if l eq 0 then return, sqrt(total((lu-p0)^2))	;Line is a point
lv = lv / l
ln = [ -lv(1), lv(0) ]
lc = -total(ln * lu)

q = lc + total(ln * p0)
q = p0 - q * ln		;The point on the line....

d= (q - p1) * (q - lu)  ;Both are neg or 0 if on line..
if d(0) gt 0 or d(1) gt 0 then begin
	d1 = sqrt(total((p0-p1)^2))
	d2 = sqrt(total((p0-lu)^2))
	return, d1 < d2
	endif

return, sqrt(total((p0 - q)^2))
end


PRO REMOVE_VECTOR, x0, y0		;Remove the vector closest to x0,y0
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

device, SET_GRAPHICS=6, BYPASS=1  	;To XOR mode
draw_char, x_0, y_0, sx, cur_vects, color = xor_color  ;Erase character

n = n_elements(cur_vects)
if n le 2 then begin			;Only one segment?
	cur_vects = -1
	goto, remove_done
	endif

translate_vects, cur_vects, x, y, pen_up
dmin = 1e6

for i=0, n-2 do $		;Each vector
    if pen_up(i+1) eq 0 then begin  ;dont do next vector
	d = pnt_line(x0, y0, x(i), y(i), x(i+1), y(i+1))
	if d lt dmin then begin
		dmin = d
		j = i
		endif
	endif			;Pen up

first_seg = pen_up(j)
if j eq n-2 then last_seg = 1 else last_seg = pen_up(j+2)

if first_seg and last_seg then to_remove = [j, j+1] $
else if not (first_seg or last_seg) then $	;Split
	cur_vects(j+1) = cur_vects(j+1) + 16384 $
else if first_seg then begin
	to_remove = j			;Remove first
	cur_vects(j+1) = cur_vects(j+1) + 16384  ;and make 2nd first
endif else to_remove = j+1		;Remove last

if n_elements(to_remove) gt 0 then begin	;Remove offending segs
	good = replicate(1, n)	   
	good(to_remove) = 0
	cur_vects = cur_vects(where(good))
	endif

remove_done: draw_char, x_0, y_0, sx, cur_vects, color = xor_color
device, set_graphics = 3, BYPASS = 0	;Back to normal
end


FUNCTION str_to_ccode, t	;Return decimal, hex, or octal number
; Formats:  0nn Octal, 0xnn Hex, else Decimal
; 
if strmid(t,0,2) eq '0x' then begin	;Hex
	fmt = '(z8)'
	t = strmid(t,2,100)
endif else if strmid(t,0,1) eq '0' then fmt = '(o6)' $
else fmt = '(i6)'
on_ioerror, bad_num
i = 0
reads, t, i, format=fmt
return, i
bad_num:  junk = message_box(['Invalid character code. Formats = ', $
		'0xnn for hex, 0nn for octal, nnn for decimal'])
return, -1		;For error
end



PRO VIEW_EVENT, event		;Events from the view base

if event.press ne 0 then begin
	WIDGET_CONTROL, event.top, /DESTROY
endif else begin
	widget_control, event.top, get_uvalue=t	;Get parameters
	x = event.x - t(2)
	y = event.y - t(3)
	z = t(1)			;zoom factor
	if x lt 0 then x = (x - z/2) / z $   ;Round in proper direction
	else x = (x + z/2) / z
	if y lt 0 then y = (y - z/2) / z $
	else y = (y + z/2) / z
	WIDGET_CONTROL, t(0), set_value=strtrim(x,2) + ', '+strtrim(y,2)
endelse
end


PRO CMODE_EVENT, event		;For character editor window
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

if event.id eq cdraw then begin
	x = event.x - x_0
	y = event.y - y_0
	if x lt 0 then x = (x - sx/2) / sx $	 ;Round in proper direction
	else x = (x + sx/2) / sx
	if y lt 0 then y = (y - sx/2) / sx $
	else y = (y + sx/2) / sx

	WIDGET_CONTROL, cpos_txt, set_value=strtrim(x,2) + ', '+strtrim(y,2)
	this = sx * [x,y] + [x_0, y_0]  ;Screen coords
	if event.press eq 1 then begin  ;Initiate dragging a vector
	    drag = this
	    prev = drag
	    buttons = 1
	    return
	    endif
	if event.release eq 1 then begin  ;Done dragging a vector...
	    add_vector, drag, this
	    buttons = 0
	    changed = 1
	    return
	    endif
	if buttons ne 0 then begin
	    if (this(0) eq prev(0)) and (this(1) eq prev(1)) THEN RETURN
	    device, SET_GRAPHICS=6, BYPASS=1  ;To XOR mode
	    if prev(0) ne drag(0) or prev(1) ne drag(1) then $
		plots, [drag(0), prev(0)], [drag(1), prev(1)], /DEV, $
			color = xor_color
		prev = this
		plots, [prev(0), drag(0)], [prev(1), drag(1)], /DEV, $
			color = xor_color
		DEVICE, SET_GRAPHICS=3, BYPASS=0
		endif		;Buttons
	if event.press eq 2 then begin		;Middle button to remove
	    if cur_vects(0) eq -1 then return	;Nothing to remove...
	    changed = 1
	    remove_vector, x, y
	    endif		;Remove
	RETURN
ENDIF

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

dup_move = 0
CASE eventval of
"CCODE": BEGIN		;Formats:  0nn Octal, 0xnn Hex, else Decimal
	WIDGET_CONTROL, cinfo_txt, GET_VALUE=t
	i = str_to_ccode(t(0))
	if i lt 32 or i ge (nchars+32) then return
	cur_char = i
	changed = 1
	cur_chartab = chartab(i-32)
	redraw
	return
	ENDCASE
"SCHAR": BEGIN			;Replace char in font
schar0: changed = 0
	save_char, cur_char, cur_chartab, cur_vects
	redraw, 0
	ENDCASE
"SHRINK": dup_move = 3
"DMOVE": dup_move = 2
"MOVE":  dup_move = 1
"WIDTH": BEGIN
	WIDGET_CONTROL, cwidth_Txt, GET_VALUE=t
	cur_chartab.width = fix(t(0))
	REDRAW, 1
	ENDCASE
"REDRAW": redraw
"VIEW": BEGIN
	if changed then begin
		i = MESSAGE_BOX('Saved changes to character?', /YES_NO)
		if i ne 0 then goto, schar0
		endif
	redraw, 0
	ENDCASE
else:  print, eventval
ENDCASE
END
	
PRO save_char, cindex, ctab, cvects		;Save character whose
;  code is cindex, whose struct is ctab, and whose vectors are cvects
;  in the current font.

common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

if cur_char lt 32 then return
c = cindex - 32

offset = chartab(c).offset		;Old beginning
iend = fix(chartab(c).nvecs) + offset	;Old end

if cvects(0) eq -1 then begin		;Removed character?
    ctab.nvecs = 0		;No vectors
    if offset ne iend then begin	;Remove old?
	good = replicate(1,n_elements(vectors))
	good(offset:iend-1) = 0
	vectors = vectors(where(good))
	endif		
    goto, schar1
    endif
		;Adding a non-zero length character.....
order_vectors, ctab, cvects		;Combine where possible
if offset ne 0 then t = [ vectors(0:offset-1), cvects ] $
else t = cvects
if iend ne n_elements(vectors) then vectors = [t, vectors(iend:*) ] $
else vectors = t

schar1: ctab.offset = offset
 chartab(c) = ctab
 for i=c+1, n_elements(chartab)-1 do $    ;Re-align other chars
	chartab(i).offset = chartab(i-1).offset + fix(chartab(i-1).nvecs)
end



PRO save_font, unit, fonttab, index, vectors, chartab

MAX_FONT = 40
mask = '7fffffff'xl
nchars = n_elements(chartab)
point_lun, unit, 0

k = 320				;Starting offset in each file
f = fonttab			;New font table
save = 0
for i=0, MAX_FONT-1 do begin	;Re-arrange each font
    if i eq index then begin	;This font?
	l = n_elements(vectors) * 2 ;Length in bytes of vectors
	k0 = k			;Where we write
	f(0,i) = k0		;Where we start
	if nchars eq 224 then f(1,i) = l or (not mask) $
	else f(1,i) = l
    endif
    if f(0,i) eq -1 then l = 0 else begin  ;Length
        l = f(1,i) and mask		;# of bytes in vects
        if (f(1,i) and (not mask)) ne 0 then l = 4 * 224 + l $
	else l = 4 * 96 + l
	f(0,i) = k		;Starting pos
	if (i gt index) and save eq 0 then $  ;What we have to save
		save = fonttab(0,i)	 ;Where we read
	endelse
    k = k + l
endfor

byteorder, f, /HTONL			;To Network order
writeu, unit, f				;The fonttable
byteorder, f, /NTOHL			;& Back again

if save ne 0 then begin			;Save following fonts
	point_lun, unit, save
	big = max(fonttab(0,*), last)	;Get # of bytes in file
	big = big + (fonttab(1,last) and mask)	;Last byte + 1 of file
	temp = bytarr(big-save, /nozero)
	readu, unit, temp
	endif
point_lun, unit, k0			;Where we write
off = chartab.offset			;Offsets of characters
byteorder, off, /HTONS			;To network order
tc = chartab
tc.offset = off
writeu, unit, tc, vectors		;Write our font

if save ne 0 then writeu, unit, temp	;Following fonts	
fonttab = f				;New font table
print, 'Saved font', index, ' at:', k0
end


PRO cload_proc, event
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

WIDGET_CONTROL, event.id, GET_UVALUE=b
if n_elements(b) le 0 then return

a = event.top			;Top level widget
WIDGET_CONTROL, a, GET_UVALUE=u	;Widget ID's of text widgets....
WIDGET_CONTROL, u(0), GET_VALUE=Fnum_T
index = str_to_ccode(Fnum_t(0))	;Font index
;   if index lt 0 or index ge MAX_FONT then return
if index lt 3 or index ge 30 then return

CASE b of 
"CLOAD": BEGIN
	changed = 1
	WIDGET_CONTROL, u(1), GET_VALUE=Cnum_T
	ccode = str_to_ccode(Cnum_t(0))
	if ccode lt 32 then return
	read_font, index, fonttab, unit, n, c, v	;Read the font
	if n_elements(c) le 1 then return
	if ccode lt 32 or (ccode-32) ge n then goto, bad_code
	old_offset = cur_chartab.offset		;Prev char
	cur_chartab = c(ccode-32)
	offset = cur_chartab.offset
	cur_chartab.offset = old_offset		;Vectors of current char
	nv = fix(cur_chartab.nvecs)
	if nv gt 0 then cur_vects = v(offset : offset + nv-1) $
	else cur_vects = -1
	redraw, 1
	WIDGET_CONTROL, cwidth_txt, SET_VALUE=STRTRIM(FIX(cur_chartab.width),2)
	WIDGET_CONTROL, coff_txt, SET_VALUE=STRTRIM(cur_char_offset,2)
	WIDGET_CONTROL, cscale_txt, SET_VALUE=STRTRIM(cur_char_scale)
	ENDCASE
"CVIEW": BEGIN			;View a character in a new window
	WIDGET_CONTROL, u(1), GET_VALUE=Cnum_T
	ccode = str_to_ccode(Cnum_t(0))
	if ccode lt 32 then return
	read_font, index, fonttab, unit, n, c, v	;Read the font
	if n_elements(c) le 1 then return
	if ccode lt 32 or (ccode-32) ge n then goto, bad_code
	a = WIDGET_BASE(Title='Font '+strtrim(index,2) + ' Char = ' + $
		string(byte(ccode)), /COLUMN)
	t = WIDGET_TEXT(a, xsize=30, ysize=1)
	b = WIDGET_DRAW(a, xsize=!d.x_size, ysize = !d.y_size, /BUTTON, $
		EVENT_PRO = 'VIEW_EVENT', /MOTION)
	c = c(ccode-32)			;The char
	if c.nvecs eq 0 then return	;Nothing there

	widget_control, a, /realize
	widget_control, b, get_value= pwin	;Preview window
	WIDGET_CONTROL, a, SET_UVALUE=[t, sx, x_0, y_0]	;Save the text widget
	draw_grid
	plots, [1,1] * sx * c.width + x_0, [0, !d.y_size-1], $
		color=4, /DEV, LINES=2
	draw_char, x_0, y_0, sx, v(c.offset: c.offset+c.nvecs -1)
	wset, cwin
	ENDCASE
"FLOAD": BEGIN
	changed = 0
	if fonttab(0,index) eq -1 then goto, bad_code
	read_font, index, fonttab, unit, nchars, chartab, vectors
	fnum = index
	redraw, 0
	ENDCASE
"FSAVE": BEGIN
	changed = 0
	if n_elements(vectors) le 1 then begin
		del = message_box('Current font has no characters')
		goto, done
		endif
	close, unit
	openu, unit, FILEPATH("hersh1.chr", subdir=['resource', 'fonts'])
	save_font, unit, fonttab, index, vectors, chartab
	redraw, 0
	fnum = index
	ENDCASE
ENDCASE

done: WIDGET_CONTROL, event.top, /destroy	;All done
return

bad_code:  junk = message_box("Invalid character or font code.")
return
end




PRO rw_font_char, charflg, writeflg

common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

name = (['Font', 'Character'])(charflg)
ierr = 0

open_again: a = widget_base(Title='Read/Write Individual '+ name, /COLUMN)
if ierr ne 0 then junk = WIDGET_LABEL(a, VALUE=f(0) + ' not found.')
a0 = WIDGET_BASE(a, /row)
junk = WIDGET_LABEL(a0, VALUE='File Name: ')
b = WIDGET_TEXT(a0, /EDIT, xsize=20, ysize=1, /FRAME, UVALUE='OK')
junk = WIDGET_BUTTON(a, VALUE='Cancel', UVALUE='CANCEL')
WIDGET_CONTROL, a, /REAL
event = WIDGET_EVENT(a)
WIDGET_CONTROL, event.id, GET_UVALUE=t

IF t EQ "CANCEL" THEN BEGIN
	WIDGET_CONTROL, a, /DESTROY
	return
	ENDIF

WIDGET_CONTROL, b, GET_VALUE=f
WIDGET_CONTROL, a, /DESTROY

if charflg then begin
    if writeflg then begin	;Save character
	openw, unit, /GET_LUN, f(0), /XDR
	writeu, unit, cur_chartab
	nv = fix(cur_chartab.nvecs)
	offset = cur_chartab.offset
	if nv gt 0 then v = vectors(offset: offset + nv-1) else v = -1
	writeu, unit, v
    endif else begin		;Read character
	print,'read'
	openr, unit, /GET_LUN, f(0), /XDR, ERROR=ierr
	if ierr ne 0 then goto, open_again
	readu, unit, cur_chartab
	nv = fix(cur_chartab.nvecs)
	cur_vects = intarr(nv, /NOZERO)
	readu, unit, cur_Vects
	save_char, cur_char, cur_chartab, cur_vects
    endelse
    redraw, 1
endif else begin		;Fonts....
    if writeflg then begin
	openw, unit, /GET_LUN, f(0), /XDR
	writeu, unit, n_elements(chartab), n_elements(vectors)
	writeu, unit, chartab
	writeu, unit, vectors
    endif else begin		;Read a font...
	openr, unit, /GET_LUN, f(0), /XDR, ERROR=ierr
	if ierr ne 0 then goto, open_again
	nc = 0L
	nv = 0L
	readu, unit, nc, nv
	chartab = replicate({CTAB}, nc)
	readu, unit, chartab
	vectors = intarr(nv, /NOZERO)
	readu, unit, vectors
    endelse
    redraw, 0
endelse

free_lun, unit
end


PRO efont_event, event
common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

WIDGET_CONTROL, event.top, /HOURGLASS

if event.id eq fdraw then begin		;Select a character?
	if event.press ne 0 then return
	x = (event.x) / chx - 2
	y = (!d.y_size - event.y ) / chy + 1
	if (x lt 0) or (x gt 15) or $
		(y lt 2) or (y ge (nchars+32)/16) then return
	cur_char = x + y * 16
	cur_chartab = chartab(cur_char-32)
	offset = cur_chartab.offset   ;Vectors of current char
	WIDGET_CONTROL, cwidth_txt, SET_VALUE=STRTRIM(FIX(cur_chartab.width),2)
	WIDGET_CONTROL, coff_txt, SET_VALUE=STRTRIM(cur_char_offset,2)
	WIDGET_CONTROL, cscale_txt, SET_VALUE=STRTRIM(cur_char_scale)
	nv = fix(cur_chartab.nvecs)
	if nv gt 0 then cur_vects = vectors(offset : offset + nv-1) $
	else cur_vects = -1
	redraw, 1
	return
	endif

dup_move = 0
WIDGET_CONTROL, event.id, GET_UVALUE = eventval

IF STRMID(eventval, 0, 1) eq '@' THEN BEGIN
	junk = execute(strmid(eventval, 1, 100))
	return
	ENDIF

CASE eventval of
"SET8":  BEGIN
	if nchars eq 224 then return
	nchars = 224
	t = chartab(0)
	t.nvecs = 0
	t.width = 16
	t.offset = chartab(95).offset
	chartab = [ chartab, replicate(t, 128) ]
	help, chartab
	if mapped eq 0 then redraw
	ENDCASE

"XLOAD": BEGIN
	window, /free, /pix, xsize=128, ysize=128
	siz = '720'
	device, font='-monotype-gill sans-medium-r-normal-sans-0-' + siz + $
		'-75-75-*'
	xyouts, 64, 64, /DEV, string(byte(cur_char)), /FONT, COLOR=xor_color
	device, font = 'fixed'
	a = tvrd()
	wdelete
	redraw, 1
	zoom = 7
	device, SET_GRAPHICS=6		;Xor mode
	tv, rebin(a, 128*zoom, 128*zoom), x_0-(64*zoom), y_0-(64*zoom)
	device, SET_GRAPHICS=3
	ENDCASE
"CVIEW": BEGIN
	uv = "CVIEW"	
	goto, cload_cview
	ENDCASE
"CLOAD": BEGIN		;Load a character from another font....
	uv = "CLOAD"
cload_cview:	a = WIDGET_BASE(TITLE='Load Hershey character', /column)
	junk = widget_base(a, /row)
	junk1 = WIDGET_LABEL(junk, VALUE = 'Font Number:')
	font_t = WIDGET_TEXT(junk, XSIZE=8, YSIZE=1, /EDIT, $
		VALUE=STRTRIM(fnum,2))
	junk = widget_base(a, /row)
	junk1 = WIDGET_LABEL(junk, VALUE = 'Character Code:')
	char_t = WIDGET_TEXT(junk, XSIZE=8, YSIZE=1, /EDIT)
	junk = WIDGET_BUTTON(a, VALUE="OK", UVALUE=uv)
	WIDGET_CONTROL, a, /REALIZE, SET_UVALUE=[font_t, char_t]
	XMANAGER, 'LoadBox', a, EVENT_HANDLER='CLOAD_PROC', /MODAL
	changed = 1
	ENDCASE
"SCHAR":  rw_font_char, 1, 1		;rw_font_char, char_flag, write_flag
"SFONT":  rw_font_char, 0, 1
"RCHAR":  rw_font_char, 1, 0
"RFONT":  rw_font_char, 0, 0

"FLOAD": BEGIN
	a = WIDGET_BASE(TITLE="Load Font", /COLUMN)
	t = "FLOAD"
get_font_num:
	junk = widget_base(a, /row)
	junk1 = WIDGET_LABEL(junk, VALUE = 'Font Number:')
	font_t = WIDGET_TEXT(junk, XSIZE=8, YSIZE=1, /EDIT, $
		VALUE=strtrim(fnum,2))
	junk = WIDGET_BUTTON(a, VALUE="OK", UVALUE=T)
	WIDGET_CONTROL, a, /REALIZE, SET_UVALUE=[font_t, 0]
	changed = 0
	XMANAGER, 'LoadBox', a, EVENT_HANDLER='CLOAD_PROC', /MODAL
	ENDCASE
"HELP" : XDisplayFile, FILEPATH("efont.txt", subdir=['help', 'widget']), $
                TITLE = "EFONT Help", $
                GROUP = event.top, $
                WIDTH = 72, HEIGHT = 24
"SAVE":	BEGIN
	a = WIDGET_BASE(TITLE="Save Font", /COLUMN)
	t = "FSAVE"
	goto, get_font_num
	ENDCASE
"DONE":  BEGIN
;	save_font_file
bail_out: FREE_LUN, unit
	WIDGET_CONTROL, event.top, /DESTROY
	ENDCASE
;"CANCEL":  goto, bail_out
ELSE: help, eventval
ENDCASE
end


pro read_font, index, fonttab, unit, nchars, ctab, vects
; Read the font numbered index from the open unit.  (We assume a font table
;  of 40 elements.)  Return


nfonts = n_elements(fonttab)/2
if nfonts lt 1 then begin		;Read fonttab?
	nfonts = 40
	fonttab = lonarr(2, nfonts)	;Get font directory
	point_lun, unit, 0
	readu, unit, fonttab
	byteorder, fonttab, /NTOHL	;To our order
	endif

if index lt 0 or index ge nfonts then begin
	junk = MESSAGE_BOX('Font index must be in range of 0 to '+ $
		string(nfonts))
	return
	endif
if fonttab(0,index) eq -1 then begin
	junk = MESSAGE_BOX('Font ' + string(index) + ' does not exist')
	return
	endif

mask = '7fffffff'xl
if (fonttab(1,index) and (not mask)) ne 0 then nchars = 256-32 else $
	nchars = 128-32

len = fonttab(1, index) and mask
ctab = replicate({CTAB, nvecs: 0b, width: 0b, offset: 0}, nchars)

point_lun, unit, fonttab(0,index) and '0fffffff'xl	;Beginning of font
readu, unit, ctab
k = 0				;Current offset
for i=0, nchars-1 do begin	;Un swap shorts
	j = ctab(i).offset
	byteorder, j, /NTOHS
	if j eq 0 then j = k $	;Put empty chars in their proper place
	else if j ne k then print,'Inconsistent vector offset/length, chr = ',$
		i+32
	ctab(i).offset = j
	k = k + fix(ctab(i).nvecs)
	endfor

vects = intarr(len/2)		;Read the vectors
on_ioerror, bad
readu, unit, vects
bad: 
byteorder, vects, /NTOHS
end


pro efont, GROUP = GROUP, init_font
;+
; NAME:
;	EFONT
;
; PURPOSE:
;	This widget provides a vector font editor and display.
;
; CATEGORY:
;	Fonts.
;
; CALLING SEQUENCE:
;	EFONT, Init_font
;
; INPUTS:
;	Init_font: The initial font index, from 3 to 29.  Default = 3.
;
; KEYWORD PARAMETERS:
;	GROUP:     The widget group, if part of a hierarchy.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	efont_com.
;
; SIDE EFFECTS:
;	Reads and modifies a local copy of the file hersh1.chr.
;
; RESTRICTIONS:
;	A basic editor.
;
; PROCEDURE:
;	Call EFONT and press the HELP button for instructions.
;
; MODIFICATION HISTORY:
;	DMS	Nov, 1992.
;	WSO, 1/95, Updated for new directory structure
;-



common efont_com, unit, fonttab, nchars, chartab, vectors, fwin, cwin, fnum, $
	fdraw, cdraw, cur_char, cur_chartab, cur_vects, cur_char_offset, $
	cur_char_scale, chx, chy, x_0, y_0, sx, cpos_txt, cinfo_txt, $
	cwidth_txt, coff_txt, cscale_txt, mapped, MAX_FONT, $
	drag, prev, xor_color, buttons, mask, fnum_txt, changed, $
	dup_move

if XRegistered('efont') ne 0 THEN RETURN

MAX_FONT = 40			;# of fonts in header
zooms = [ 4, 8,12, 16, 20]	;Zoom factors...
chx = 32			;Char cell sizes
chy = 32
sx = 16				;Zoom factor
x_0 = 50
y_0 = 50
mapped = 0
cur_char = 32
cur_char_scale = 1
cur_char_offset = 0
buttons = 0
changed = 0
mask = '7fffffff'xl 		;Low bits for file start
dup_move = 0
if n_elements(init_font) gt 0 then fnum = init_font else fnum = 3

openr, unit, FILEPATH("hersh1.chr", subdir=['resource', 'fonts']), /GET_LUN, ERROR=i
if i ne 0 then begin
	junk = message_box(['Open Failed for hersh1.chr', !ERR_STRING])
	return
	endif

readu, unit, fonttab
byteorder, fonttab, /NTOHL	;To our order

main_base = WIDGET_BASE(Title='Hershey Font Editor', /COLUMN)

top_line = WIDGET_BASE(main_base, /ROW)

junk = WIDGET_BUTTON(top_line, /NO_REL, VALUE='Done', UVALUE='DONE')
junk = WIDGET_BUTTON(top_line, /MENU, VALUE='File')
junk1 = WIDGET_BUTTON(junk, /NO_REL, VALUE='Save Character', UVALUE='SCHAR')
junk1 = WIDGET_BUTTON(junk, /NO_REL, VALUE='Save Font', UVALUE='SFONT')
junk1 = WIDGET_BUTTON(junk, /NO_REL, VALUE='Read Character', UVALUE='RCHAR')
junk1 = WIDGET_BUTTON(junk, /NO_REL, VALUE='Read Font', UVALUE='RFONT')
; junk = WIDGET_BUTTON(top_line, /NO_REL, VALUE='Cancel', UVALUE='CANCEL')
junk = WIDGET_BUTTON(top_line, /NO_REL, VALUE='Help', Uvalue = 'HELP')
junk = WIDGET_BUTTON(top_line, /NO_REL, VALUE='Save Font', UVALUE='SAVE')
junk = WIDGET_BUTTON(top_line, /MENU, VALUE='Load')
junk1 = WIDGET_BUTTON(junk, VALUE='Font', UVALUE="FLOAD")
junk1 = WIDGET_BUTTON(junk, VALUE='Character', UVALUE="CLOAD")
junk1 = WIDGET_BUTTON(junk, VALUE='Character, View Only', UVALUE="CVIEW")
;;;;; junk1 = WIDGET_BUTTON(junk, VALUE='Preview X font', UVALUE='XLOAD')

junk = WIDGET_BASE(top_line, /ROW, /FRAME)
junk1 = WIDGET_LABEL(junk, value = 'Zoom:')
junk1 = WIDGET_BASE(junk, /EXCLUSIVE, /ROW)
for i=0, n_elements(zooms)-1 do BEGIN
    junk2 = WIDGET_BUTTON(junk1, VALUE=strtrim(zooms(i),2), $
	UVALUE = '@sx='+strtrim(zooms(i),2) + '& if mapped then redraw',$
		 /NO_REL)
    if zooms(i) eq sx then WIDGET_CONTROL, junk2, /SET_BUTTON	;Set default
    ENDFOR
junk = WIDGET_BUTTON(top_line, VALUE='Set 8 bits', UVALUE='SET8', /NO_REL)
fnum_txt = WIDGET_TEXT(top_line, value='Font '+strtrim(fnum,2), $
		/FRAME, XSIZE=8)


read_font, fnum, fonttab, unit, nchars, chartab, vectors ;Init font
cur_chartab = chartab(32)		;Any char will do

base = WIDGET_BASE(main_base)
wbases = lonarr(2)
wbases(0) = WIDGET_BASE(base, /COLUMN)
wbases(1) = WIDGET_BASE(base, /COLUMN, EVENT_PRO = 'CMODE_EVENT')
fdraw = WIDGET_DRAW(wbases(0), XSIZE = 640, YSIZE = 640, RETAIN=2, /BUTTON)

c_row = WIDGET_BASE(wbases(1), /ROW)
; Save
; Save as
; Load character
; Draw character
; 
junk = WIDGET_BUTTON(c_row, VALUE="View Font", UVALUE="VIEW", /NO_REL)
junk = WIDGET_BUTTON(c_row, VALUE="Redraw", UVALUE="REDRAW", /NO_REL)
junk = WIDGET_BUTTON(c_row, VALUE="Save", UVALUE="SCHAR", /NO_REL)
junk = WIDGET_BUTTON(c_row, VALUE= 'Move/Scale', /MENU)
junk1 = WIDGET_BUTTON(junk, VALUE='Move', UVALUE='MOVE', /NO_REL)
junk1 = WIDGET_BUTTON(junk, VALUE='Move & Duplicate', UVALUE='DMOVE', /NO_REL)
junk1 = WIDGET_BUTTON(junk, VALUE='Scale', UVALUE='SHRINK', /NO_REL)

junk = WIDGET_LABEL(c_row, VALUE = 'Current Char:')
cinfo_txt = WIDGET_TEXT(c_row, xsize = 36, ysize = 1, /FRAME, /EDIT, $
	UVALUE="CCODE")

c_row = WIDGET_BASE(wbases(1), /ROW)
junk = WIDGET_LABEL(c_row, VALUE = 'Position:')
cpos_txt = WIDGET_TEXT(c_row, xsize=8, ysize=1, /FRAME)

junk = WIDGET_LABEL(c_row, value= 'Width:')
cwidth_txt = WIDGET_TEXT(c_row, xsize=4, ysize= 1, /EDIT, /FRAME, $
	VALUE='16', UVALUE="WIDTH")
junk = WIDGET_LABEL(c_row, value= 'Offset:')
coff_txt = WIDGET_TEXT(c_row, xsize=4, ysize= 1, /EDIT, /FRAME, $
	VALUE='0', UVALUE="OFF")
junk = WIDGET_LABEL(c_row, value= 'Scale:')
cscale_txt = WIDGET_TEXT(c_row, xsize=6, ysize= 1, /EDIT, /FRAME, $
	VALUE='16', UVALUE="SCALE")

cdraw = WIDGET_DRAW(wbases(1), XSIZE = 640, YSIZE = 640, RETAIN=2,  $
	/BUTTON, /MOTION)

WIDGET_CONTROL, main_base, /REALIZE
WIDGET_CONTROL, cdraw, GET_VALUE = cwin
WIDGET_CONTROL, fdraw, GET_VALUE = fwin
WIDGET_CONTROL, wbases(1), MAP=0

wset, fwin
draw_font
loadct, 0, /silent
tek_color
device, translation = junk
xor_color = junk(0) xor junk(1)		;Color to drag with

XMANAGER, 'efont', main_base, EVENT_HANDLER = 'efont_event', $
	GROUP = group
end

