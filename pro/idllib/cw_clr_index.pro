; $Id: cw_clr_index.pro,v 1.1 1993/10/27 16:02:46 troy Exp $
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:	
;	CW_CLR_INDEX
;
; PURPOSE:
;	CW_CLR_INDEX is a compound widget for the selection of a color
;	index. A horizontal color bar is displayed. Clicking on the bar sets
;	the color index.
;
; CATEGORY:
;	Compound Widgets
;
; CALLING SEQUENCE:
;	Widget = CW_CLR_INDEX(Parent)
;
; INPUTS:
;	Parent:	      ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	COLOR_VALUES: A vector of color indices containing the colors to
;		      be displayed in the color bar. If omitted, NCOLORS
;		      and START_COLOR specify the range of color indices.
;	EVENT_FUNCT:  The name of an optional user-supplied event function.
;		      This function is called with the return value structure
;		      whenever a button is pressed, and follows the conventions ;		      for user-written event functions.
;	FRAME:        If set, a frame will be drawn around the widget.
;	LABEL:        A text label that appears to the left of the color bar.
;	NCOLORS:      The number of colors to place in the color bar.  
;		      The default = !D.N_COLORS.
;	START_COLOR:  The starting color index, placed at the left of the bar.
;	UVALUE:       The user value to be associated with the widget.
;	XSIZE:        The width of the color bar in pixels. The default =192.
;	YSIZE:        The height of the color bar in pixels. The default = 12.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	This widget generates event structures with the following definition:
;
;	Event = { CW_COLOR_INDEX, ID: base, TOP: ev.top, HANDLER: 0L, VALUE: c}
;	Value is the color index selected.
;
; PROCEDURE:
;	Standard Compound widget.  Use WIDGET_CONTROL, SET_VALUE and GET_VALUE
;	to change/read the widget's value.
;
; EXAMPLE:
;	A = WIDGET_BASE(TITLE='Example', /COLUMN)
;	B = CW_CLR_INDEX(A, LABEL='Color:')
;
; MODIFICATION HISTORY:
;	DMS,	June, 1993.	Written.
;	TAC,	Oct, 1993.	Changed name to cw_clr_index
;-

function CW_COLOR_INDEXE, ev		;Color index widget's event proc
if ev.press ne 0 then return, 0
base = ev.handler
widget_control, widget_info(base, /child), get_uvalue = state
c = long(state.start_color + (ev.x * state.scale))  ;New color
if state.extra ne 0L then BEGIN
    WIDGET_CONTROL, state.extra, GET_UVALUE=cv
    c = cv(c)
    endif
CW_COLOR_INDEXS, base, c	
ret =  { CW_COLOR_INDEX, ID: base, TOP: ev.top, HANDLER: 0L, VALUE: c}
if state.efun eq '' then return, ret $
else return, CALL_FUNCTION(state.efun, ret)
end


function CW_COLOR_INDEXG, id
	widget_control, widget_info(id, /child), get_uvalue = state
	return, state.value
end


pro CW_COLOR_INDEXS, id, value		;Set color index widget value
widget_control, (draw = widget_info(id, /child)), get_uvalue = state, /NO_COPY
old_win = !d.window

if state.inited eq 0 then begin
    widget_control, state.pwin_id, get_value = i
    wset, i
    x = long(state.scale * findgen(!d.x_size))	;0 to n_colors-1
    if state.extra ne 0L then begin
	WIDGET_CONTROL, state.extra, GET_UVALUE=cv
	x = cv(x)
	ENDIF
    tv, x # replicate(1, !d.y_size) + state.start_color
    state.inited = 1
    endif

state.value = value			;Save new value
widget_control, state.rect_id, get_value = i
wset, i
tv, replicate(value, !d.x_size, !d.y_size)
wset, old_win
widget_control, state.txt_id, set_value = '(' + strtrim(value,2) + ')'
widget_control, draw, set_uvalue = state, /NO_COPY
return
end


function CW_CLR_INDEX, parent, LABEL = label, FRAME = frame, $
	UVALUE = uvalue, XSIZE = xsize, YSIZE = ysize, NCOLORS = ncolors, $
	START_COLOR = start_color, EVENT_FUNC = efun, COLOR_VALUES = cv

if n_elements(frame) eq 0 then frame = 0
if n_elements(uvalue) eq 0 then uvalue = 0
if n_elements(xsize) eq 0 then xsize = 192
if n_elements(ysize) eq 0 then ysize = 12
if n_elements(cv) gt 0 then begin
	ncolors = n_elements(cv)
	start_color = 0
	endif
if n_elements(ncolors) eq 0 then ncolors = !d.N_COLORS
if n_elements(start_color) eq 0 then start_color = 0
if n_elements(label) eq 0 then label = ''

base = widget_base(parent, /ROW, FRAME = frame, $
	EVENT_FUNC = 'CW_COLOR_INDEXE', FUNC_GET_VALUE = 'CW_COLOR_INDEXG', $
	PRO_SET_VALUE = 'CW_COLOR_INDEXS', UVALUE = uvalue)
if n_elements(efun) le 0 then efun = ''
state = { CW_C_INDEX_STATE, $
	txt_id : 0L, rect_id : 0L, pwin_id : 0L, inited : 0, efun: efun, $
	scale : float(ncolors) / xsize, value : 0L, $
	start_color : long(start_color), extra: 0L }
child = widget_label(base, value = label)
state.txt_id = widget_text(base, xsize=5, ysize = 1, value = '(0)')
if n_elements(cv) gt 0 then BEGIN
    WIDGET_CONTROL, state.txt_id, SET_UVALUE=cv
    state.extra = state.txt_id
    endif

state.rect_id = widget_draw(base, /FRAME, XSIZE = 16, YSIZE = ysize, $
	UVALUE = 0, RETAIN=2)
state.pwin_id = widget_draw(base, /frame, xsize = xsize, $
	ysize = ysize, /BUTTON)
widget_control, child, set_uvalue = state
return, base
end

