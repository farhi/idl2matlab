; $Id: xfont.pro,v 1.5 1995/07/03 15:45:24 idl Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	XFONT
;
; PURPOSE:
;	XFONT is a modal widget for selecting and viewing an X Windows font.
; 
; CATEGORY:
;	Widgets, Fonts
;
; CALLING SEQUENCE:
;	Selected_font = XFONT()
;
; INPUTS:
;	No explicit inputs.
;
; KEYWORD PARAMETERS:
;	GROUP:    The widget ID of the widget that calls XFONT. When this
;		  ID is specified, a death of the caller results in a death
;		  of XFONT.
;	PRESERVE: If set, XFONT saves the server font directory in common
;		  blocks so that subsequent calls to XFONT start-up much
;		  faster. If not set, the common block is cleaned.
;
; OUTPUTS:
;	A string containing the font name.  If nothing is selected, or
;	the CANCEL button is pressed, a null string is returned.
;
; COMMON BLOCKS:
;	XFONT_COM.
;
; SIDE EFFECTS:
;	Initiates the XManager if it is not already running.
;	Resets the current X Window font.  
;
; RESTRICTIONS:
;	The current X window font is manipulated without being restored.
;
; PROCEDURE:
;	Create and register the widget and then exit.
;
; MODIFICATION HISTORY:
;	Modified from a template written by: Hans-Joachim Bothe, CreaSo GmbH,
;		November, 1991, by DMS, RSI, November, 1992.
;	1 July 1995, AB, Fixed sizing of toggle buttons.
;-




FUNCTION xfont_select, sstring		;Find fonts matching sstring
;  Return -1 if none there....
common xfont_com, s, f, xreg, private, nfields, nreg, uniqi, uniqs, $
	t_buttons, t_list, s_list, s_string, t_button_select, t_text, $
	l_text, to_do, t_selections, selected_font, view_text, $
	view_window, reg_base, priv, display_string, s_text, preserve, $
	point_base, point_txt, size_index



l = replicate(1b, nreg)			;Matching fonts so far

for i=0, n_elements(to_do)-1 do begin
	if s_string(i) ne '*' then begin  ;Search this field
		bad = where(f(i,*) ne s_string(i), count)
		if count gt 0 then l(bad) = 0
		endif
	endfor
return, where(l)
end


FUNCTION xfont_scal_fname, name
; Given a font name, s.  See if field 8 (the size field, counting the
; first '-') contains the string '0'.  If so, substitute the contents of 
; the point_size text widget, followed by -75-75.
;	
common xfont_com, s, f, xreg, private, nfields, nreg, uniqi, uniqs, $
	t_buttons, t_list, s_list, s_string, t_button_select, t_text, $
	l_text, to_do, t_selections, selected_font, view_text, $
	view_window, reg_base, priv, display_string, s_text, preserve, $
	point_base, point_txt, size_index

t = str_sep(name, '-')
if n_elements(t) lt 11 then return, name
if fix(t(8)) ne 0 then return, name
widget_control, point_txt, GET_VALUE = x
if fix(x(0)) eq 0 then return, name	;No size specified
t(8) = x		;Substitute point size
t(9) = '75'		;Diddle our fields
t(10) = '75'
n = n_elements(t)
r = ''
for i=1, n_elements(t)-1 do r = r + '-' + t(i)   ;Re-combine

return ,r
end






PRO xfont_event, event			;Main & only event procedure
                     
common xfont_com, s, f, xreg, private, nfields, nreg, uniqi, uniqs, $
	t_buttons, t_list, s_list, s_string, t_button_select, t_text, $
	l_text, to_do, t_selections, selected_font, view_text, $
	view_window, reg_base, priv, display_string, s_text, preserve, $
	point_base, point_txt, size_index



WIDGET_CONTROL, event.top, /HOURGLASS
WIDGET_CONTROL, event.id, GET_UVALUE=eventval

   ; Determine event type

CASE eventval OF

    "TBUTTON": BEGIN		;Font-type button
	if t_button_select ne -1 then $
		widget_control, t_buttons(t_button_select), $
			SET_BUTTON = 0  ;Remove prev
	t_button_select = (where(event.id eq t_buttons))(0)  ;New button
	n = t_button_select
	s_string(n) = '*'	;Make this one wild
	WIDGET_CONTROL, t_text(n), SET_VALUE = '*'
	q = xfont_select(s_string)	;Fonts OK so far.

	if q(0) ne -1 then begin	;Anything there?
		t_strings = f(n,q)	;Names that we can select
		t_selections = uniq(t_strings, sort(t_strings))
		t_selections = t_strings(t_selections)
	endif else t_selections = '<Nothing Matches>'
	WIDGET_CONTROL, t_list, set_value = t_selections
	goto, update_s_list
	ENDCASE
	
    "T_LIST": BEGIN		;Picked a list item
	if t_button_select eq -1 then return
	s_string(t_button_select) = t_selections(event.index)
	WIDGET_CONTROL, t_text(t_button_select), $
		SET_VALUE = t_selections(event.index)
	q = xfont_select(s_string)	;Matching fonts
	if t_button_select eq size_index then begin
		i =  s_string(size_index) eq '0' 
		WIDGET_CONTROL, point_base, MAP=i
		endif
    update_s_list:
	selected_font = -1
	if q(0) ne -1 then BEGIN
		WIDGET_CONTROL, s_list, SET_VALUE=s(xreg(q))
		selected_font = xreg(q(0))
	ENDIF ELSE WIDGET_CONTROL, s_list, SET_VALUE = '<No matching fonts>'
	WIDGET_CONTROL, l_text, SET_VALUE= strtrim(n_elements(q),2) + $
	   ' matching fonts.'
	goto, view_font
	ENDCASE	

    "S_LIST": BEGIN
	if priv then selected_font = private(event.index) $
	else begin
		q = xfont_select(s_string)	;Matching fonts
		selected_font = xreg(q(event.index))
	endelse
; 	print,s(selected_font)
  view_font:
	if selected_font lt 0 then begin
	    WIDGET_CONTROL, view_text, SET_VALUE = 'No font selected'
	    return
	    endif
	font = xfont_scal_fname(s(selected_font))
	WIDGET_CONTROL, view_text, SET_VALUE=font
	swin = !d.window
	wset, view_window
	erase
	device, font = font
	xyouts, 10, !d.y_size - 1.5 * !d.y_ch_size, /DEV, /FONT, display_string
	if swin ge 0 then wset, swin
	ENDCASE
    "PRIV":  BEGIN
	WIDGET_CONTROL, reg_base, map=0
	WIDGET_CONTROL, s_list, SET_VALUE = s(private)
	priv = 1
	ENDCASE
    "REG":   BEGIN
	WIDGET_CONTROL, reg_base, map=1
	priv = 0
	q = xfont_select(s_string)	;Matching fonts
	goto, update_s_list
	ENDCASE

    "DRAW": WIDGET_CONTROL, event.top, /DESTROY   ;Clicked draw window

    "VIEW": BEGIN
	if selected_font lt 0 then return
	device, font = xfont_scal_fname(s(selected_font))
	junk = WIDGET_BASE(title = s(selected_font))
	chx = !d.x_ch_size * 2. > 8	;X Spacing
	chy = !d.y_ch_size * 1.7 > 12	;Y Spacing
	draw = WIDGET_DRAW(junk, xsize = chx * 18, ysize = chy * 18, $
		/BUTTON_EVENTS, UVALUE="DRAW", RET=2)
	WIDGET_CONTROL, junk, /realize
	WIDGET_CONTROL, draw, GET_VALUE = i
	WSET, i
	for i=0,15 do xyouts, (i+2)*chx, !d.y_size - chy, /DEV, $
		string(i, format='(z1)')
	for i= 0, 15 do begin
		y = !d.y_size - (i+2) * chy
		xyouts, 0, y, /DEV, string(i, format='(z1)')
		k = i*16
		for j=0, 15 do $
		    if j+k ne 0 then xyouts, (j+2) *chx, y, /DEV, /FONT, $
				string(byte(j+k))
		ENDFOR
	XMANAGER, 'xfont', junk, EVENT_HANDLER = 'xfont_event', $
		GROUP_LEADER = event.top
	ENDCASE
    "HELP": BEGIN
;	xdisplayfile, 'xfont.txt', $	;Debugging
	xdisplayfile, filepath("xfont.txt", subdir=['help', 'widget']), $  ;Working
		title = "xfont help", $
		group = event.top, $
		width = 72, height = 24
	ENDCASE

    "STEXT": BEGIN
	WIDGET_CONTROL, s_text, GET_VALUE = display_string
	display_string = display_string(0)
	goto, view_font
	ENDCASE

    "PTXT": goto, view_font	
    "DONE": BEGIN
	if selected_font ge 0 then begin
		s_string = xfont_scal_fname(s(selected_font))
		goto, exit
		endif
	ENDCASE
    "CANCEL": BEGIN
	selected_font = -1
    exit:
	WIDGET_CONTROL, event.top, /DESTROY
	if preserve eq 0 then BEGIN	;Clean up space consuming arrays
		f = 0
		if selected_font ge 0 then s = s(selected_font) else s = ''
		private = 0
		uniqs = 0
		xreg = 0
		ENDIF
	ENDCASE
ENDCASE
END




Function xfont, GROUP = GROUP, PRESERVE_FONT_INFO = pres

common xfont_com, s, f, xreg, private, nfields, nreg, uniqi, uniqs, $
	t_buttons, t_list, s_list, s_string, t_button_select, t_text, $
	l_text, to_do, t_selections, selected_font, view_text, $
	view_window, reg_base, priv, display_string, s_text, preserve, $
	point_base, point_txt, size_index
                     

   ; Check for other copies and do nothing if xfont is already running:

   IF(XRegistered('xfont') NE 0) THEN RETURN, ''

   if n_elements(pres) gt 0 then preserve = pres else preserve = 0
   pwin = -1
   selected_font = -1
   t_button_select = -1
   priv = 0
   display_string = 'The quick brown fox jumped over the lazy dog.' + $
	'!C!CABCDEFG abcdefg 01234567'

   to_do = [ 0,1,2,3,4,7 ]	;Fields we care about
   m = n_elements(to_do)
   field_names = [ 'Foundry', 'Family', 'Weight', 'Slant', 'Width', $
		'Size' ]
   t_buttons = lonarr(m)
   t_text = lonarr(m)
   s_string = replicate('*', m)
   size_index = 5		;The index of the point size field


   if !d.window lt 0 then $	;So we don't create an empty window
	window, /pix, xs=100, ys=100, /free, pwin

   if n_elements(s) gt 1 then goto, processed_fonts  ;Saved info?
   t0 = systime(1)
   device, font ='*', get_fontname=s	  ;Get the fonts
   nf = n_elements(s)			;# of fonts

   char1 = strmid(s, 0, 1)		;Parse fontname strings
   ext = where(char1 eq '+', count)  	;Any Font name extensions?
   for i=0, count-1 do begin		;Remove them
	j = ext(i)
	s(j) = strmid(s(j), strpos(s(j), '-'), 1000)
	endfor
				;Separate the X window fonts
   if count gt 0 then xreg = [ where(char1 eq '-'), ext ] $
   else xreg = where(char1 eq '-')

   private = lonarr(nf)		;Get private fonts
   private(xreg) = 1		;Ones that are X
   private = where(private eq 0)  ;Ones that aren't
   private = private(sort(s(private)))  ;Sort into lexical order

   nreg = n_elements(xreg)	; Separate information from font names

   f = strarr(m, nreg)		;Fields we care about
   nfields = max(to_do)
   this_one = replicate(-1, nfields+1)  ;-1 in fields we don't want
   this_one(to_do) = indgen(m)    ;Index in ones we do

   for j=0, nreg-1 do begin	;Each registered font
	t = s(xreg(j))		;The string
	anchor = 1		;1st char to search
	for i=0, nfields do begin	;Extract each field
		n = strpos(t, '-', anchor)
		k = this_one(i)
		if k ge 0 then begin
		   tt = strmid(t, anchor, n - anchor)
		   if tt eq '' then f(k,j) = '<blank>' else f(k,j) = tt
		   endif
		anchor = n + 1
		endfor
	endfor

   uniqi = lonarr(m+1)		;Indices to unique names
   
   for i=0L, m-1 do begin	;Get unique items for each field
	t = f(i,*)
	t = uniq(t, sort(t))
	uniqi(i+1) = n_elements(t) + uniqi(i)	;Startind indices
	if i eq 0 then uniqs = t $
	else uniqs = [ uniqs, t]
	endfor
;   print, systime(1) - t0, ' seconds getting fonts'


processed_fonts:
   xfontbase = WIDGET_BASE(TITLE='Font Widget', /COLUMN)
   junk = WIDGET_BASE(xfontbase, /ROW)
   junk1 = WIDGET_BUTTON(junk, value = "OK", UVALUE = "DONE", /NO_REL)
   junk1 = WIDGET_BUTTON(junk, value = "Cancel", UVALUE = "CANCEL", /NO_REL)
   junk1 = WIDGET_BUTTON(junk, value = "View", UVALUE = "VIEW", /NO_REL)
   junk1 = WIDGET_BUTTON(junk, value = "Help", UVALUE = "HELP", /NO_REL)
   junk1 = WIDGET_BASE(junk, /ROW, /EXCLUSIVE)
   junk2 = WIDGET_BUTTON(junk1, VALUE = 'Registered', UVALUE='REG')
   junk3 = WIDGET_BUTTON(junk1, VALUE = 'Private', UVALUE='PRIV')
   WIDGET_CONTROL, junk2, SET_BUTTON=1
   
   point_base = WIDGET_BASE(junk, /ROW, /FRAME)
   junk2 = WIDGET_LABEL(point_base, VALUE='deciPoint Size:')
   point_txt = WIDGET_TEXT(point_base, xsize=4, /EDIT, value='120', $
		UVALUE='PTXT')
   WIDGET_CONTROL, point_base, MAP=0

   reg_base = WIDGET_BASE(xfontbase, /ROW)
   lbase = WIDGET_BASE(reg_base, /COLUMN)
   rbase = WIDGET_BASE(reg_base, /COLUMN)

   for i=0, n_elements(to_do)-1 do begin	;Make exclusive bases
	j = to_do(i)
	junk = widget_base(lbase, /ROW, /FRAME)
	junk2 = WIDGET_BASE(junk, /EXCLUSIVE)
	t_buttons(i) = $
		WIDGET_BUTTON(junk2, value= field_names(i), /NO_RELEASE, $
			UVALUE='TBUTTON')
	t_text(i) = WIDGET_TEXT(junk, value = '*', ysize=1, xsize=24)
	endfor
   ; Set all the buttons to the width of the widest
   junk = max(strlen(field_names), junk2)
   geo = WIDGET_INFO(t_buttons(junk2), /geometry)
   for i=0, n_elements(to_do)-1 do $
   	widget_control, scr_xsize=geo.scr_xsize, t_buttons(i)

   l_text = WIDGET_TEXT(lbase, /FRAME, xsize = 32)

   t_list = WIDGET_LIST(rbase, value = string(replicate(77b,24)), $
			UVALUE = 'T_LIST', ysize = 9)

   base = xfontbase
   s_list = WIDGET_LIST(base, value = string(replicate(77b,56)), $
			UVALUE = 'S_LIST', ysize = 8)

   junk  = WIDGET_BASE(base, /ROW, /FRAME)
   junk1 = WIDGET_LABEL(junk, VALUE='Current font:')
   view_text = WIDGET_TEXT(junk, value = string(replicate(77B, 64)), $
		XSIZE=64)

   junk = WIDGET_BASE(base, /ROW, /FRAME)
   junk1 = WIDGET_LABEL(junk, VALUE='Display Text:')
   s_text = WIDGET_TEXT(junk, value = display_string, $
		XSIZE=64, UVALUE = "STEXT", /EDIT)

   view_draw = WIDGET_DRAW(base, xsize = 400, ysize=120, RETAIN = 2)
   
   WIDGET_CONTROL, xfontbase, /REALIZE

   WIDGET_CONTROL, t_list, SET_VALUE=' '	;Remove beginning junk
   WIDGET_CONTROL, s_list, SET_VALUE=' '
   WIDGET_CONTROL, view_text, SET_VALUE=' '
   WIDGET_CONTROL, view_draw, GET_VALUE = view_window

   ; Register the widgets with the XManager.
 
   if pwin ge 0 then wdelete, pwin		;Clean up initialization
   XManager, 'xfont', xfontbase, $
                EVENT_HANDLER = 'xfont_event', $
		GROUP_LEADER = GROUP, /MODAL

   if selected_font lt 0 then return, '' else $
	return, s_string
END
