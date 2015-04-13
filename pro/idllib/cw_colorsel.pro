; $Id: cw_colorsel.pro,v 1.5 1994/05/23 20:10:25 ali Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_COLORSEL
;
; PURPOSE:
;	CW_COLORSEL is a compound widget that displays all the colors
;	in the current colormap and allows the user to select the color
;	indices via the mouse or with sliders.
;
; CATEGORY:
;	Compund widgets.
;
; CALLING SEQUENCE:
; 	widget = CW_COLORSEL(Parent)
;
; INPUTS:
;	Parent:	 The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	FRAME:	 If set, a frame is drawn around the widget.
;	UVALUE:	 The user value for the widget.
;	XOFFSET: The X offset position
;	YOFFSET: The Y offset position
;
; OUTPUTS:
;	The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	This widget generates event structures containing a field named
;	VALUE, which contains the colormap index selected by the user.
;
; PROCEDURE:
;	The COLORSEL widget displays all the colors in the current
;	colormap in a 16x16 (320x320 pixels) grid. To select a color
;	index, the user moves the mouse pointer over the desired
;	color square and presses any mouse button. Alternatively, the
;	color index can be selected by moving one of the three sliders
;	provided around the grid.
;
;	WIDGET_CONTROL, SET_VALUE=index can be used to set the current
;		color index.
;
;	WIDGET_CONTROL, SET_VALUE=-1 informs the widget to initialize
;		itself and redraw. It should be called when any of the
;		following happen:
;			- The widget needs redrawing.
;			- The brightest or darkest color has changed. 
;
;	WIDGET_CONTROL, GET_VALUE=var can be used to retrieve the
;		current color index.
;
; MODIFICATION HISTORY:
;	March 30, 1992, AB
;		Removed the relevant code from XPALETTE.PRO and modified
;		it to create this reusable widget cluster.
;	September 4, 1992, SR
;		Fixed a bug where the value of the xslider was calculated
;		as negative and WIDGET_CONTROL, SET_VALUE failed.
;	7 April 1993, AB, Removed state caching.
;	October 20, 1993, KDB 
;		Changed return value in function CSEL_GET_VALUE
;		from state.cur_idx to ret
;	23 May 1994, AB
;		Added NOTIFY_REALIZE routine to eliminate the need
;		to call "WIDGET_CONTROL, SET_VALUE=-1" when the widget
;		is realized.
;-

pro CSEL_SETSLIDERS, state, type, cur_x, cur_y, idx
; Set the three position sliders according to the supplied values.
; Move the mark to the new location.
; entry:
;	type - Controls operation of procedure. Can have the following
;		values:
;		0 - Set all three sliders from IDX. state.x and state.y
;		    are updated.
;		1 - Set Row and Column sliders from IDX. Update
;		    state.x and state_y.
;		2 - Set Column and Index sliders from CUR_X and CUR_Y.
;		3 - Set Row and Index sliders from CUR_X and CUR_Y.

  change_x = 0
  change_y = 0
  change_idx = 0
  nc = !D.table_size

  if (type lt 2) then begin		; From IDX
    if (idx ge nc) then idx = nc -1
    if (type eq 0) then change_idx = 1	; Update slider to match
    ; Calculate current Y and see if slider value needs changing
    tmp = idx / 16
    if (idx ne cur_y) then begin
      cur_y = tmp
      change_y = 1
    endif
    ; Calculate current X and see if slider value needs changing
    tmp = idx - (cur_y * 16)
    if (tmp GE 0) AND (tmp ne cur_x) then begin
      cur_x = tmp
      change_x = 1
    endif
  endif else begin			; From CUR_X and CUR_Y
    if (type eq 2) then begin
      tmp = cur_x
      while (((cur_y * 16) + tmp) ge nc) do tmp = tmp - 1
      if (tmp ne cur_x) then begin
        cur_x = tmp
        change_x = 1
      endif
    endif else begin
      tmp = cur_y
      while (((tmp * 16) + cur_x) ge nc) do tmp = tmp - 1
      if (tmp ne cur_y) then begin
        cur_y = tmp
        change_y = 1
      endif
    endelse
    IDX = (cur_y * 16B) + cur_x
    change_idx = 1
  endelse

  if (change_x) then WIDGET_CONTROL, set_value=cur_x, state.column
  if (change_y) then WIDGET_CONTROL, set_value=cur_y, state.row
  if (change_idx) then WIDGET_CONTROL, set_value=idx, state.by_index
  state.x = cur_x
  state.y = cur_y
  CSEL_MVMARK, state, idx

end







function CSEL_NEW_COLORS, state
; Choose the best foreground and background colors for the current
; color maps. Returns 1 if the colors changed, 0 otherwise.

  res = 0
  luminance = ct_luminance(dark=dark_col, bright=bright_col)

  if (bright_col ne state.bright_idx) then begin
    state.bright_idx = bright_col
    res = 1
  endif

  if (dark_col ne state.dark_idx) then begin
    state.dark_idx = dark_col
    res = 1
  endif

  if (res ne 0) then begin
    marker = bytarr(20, 20) + state.dark_idx
    marker(2, 2) = bytarr(16, 16) + state.bright_idx
    marker(4, 4) = bytarr(12, 12) + state.dark_idx
    marker(6, 6) = bytarr(8, 8) + state.bright_idx
    marker(8, 8) = bytarr(4, 4) + state.dark_idx
    state.marker = marker

    marker = bytarr(20, 20) + state.dark_idx
    marker(1:18, 2:17) = bytarr(18, 16) + state.bright_idx
    state.unreach = marker
 
  endif

  return, res
end







pro CSEL_MVMARK, state, to_index
; Move the marker and update the current color square.

  cur_idx = state.cur_idx

  new_pos = to_index ne cur_idx

  ; Restore current square
  wset, state.spectrum_win
  if (new_pos) then begin
    tv, bytarr(20, 20) + byte(cur_idx), cur_idx
    state.cur_idx = (cur_idx = to_index)
  endif
  tv, state.marker, cur_idx


end







function CSEL_EVENT, ev

  ; Recover the state of this compound widget
  parent = ev.handler
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY


  case (ev.id) of

  state.by_index: begin
    set_slide_idx:
      CSEL_SETSLIDERS, state, 1, state.x, state.y, ev.value
    end

  state.column: begin
    CSEL_SETSLIDERS, state, 2, byte(ev.value), state.y, tmp
    end

  state.row : begin
    CSEL_SETSLIDERS, state, 3, state.x, byte(ev.value), tmp
    end

  state.spectrum: begin
      if (ev.press ne 0) then begin
	tmp = ((319 - ev.y) / 20 ) * 16 + (ev.x / 20)
	CSEL_SETSLIDERS, state, 0, state.x, state.y, tmp
      endif else begin
        WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
	return, 0		; Swallow key release events
      endelse
      end
    endcase

  value=state.cur_idx
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  return, {COLORSEL_EVENT, ID: parent, TOP:ev.top, HANDLER:0L, VALUE: value }

end







pro CSEL_REALIZE, id

  ;Retrieve the state information.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY


  ; Initialize everything
  ; What is the spectrum window id?
  WIDGET_CONTROL, state.spectrum, get_value=tmp
  state.spectrum_win=tmp

  junk = CSEL_NEW_COLORS(state)
  ; Draw spectrum widgets
  wset, state.spectrum_win
  tmp=bytarr(20,20)
  for i = 0, !d.table_size-1 do begin
    tv, tmp, i
    tmp = tmp + 1B
  endfor


  ; Draw the unreachable squares
  wset, state.spectrum_win
  tmp = state.unreach
  for i = !d.table_size, 255 do tv, tmp, i

  ; Highlight the current position using the marker
  CSEL_MVMARK, state, state.cur_idx

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
end







pro CSEL_SET_VALUE, id, value

  if (value eq -1) then begin
    CSEL_REALIZE, id
  endif else begin
    ;Retrieve the state information.
    stash = WIDGET_INFO(id, /CHILD)
    WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

    CSEL_SETSLIDERS, state, 0, 0, 0, value

    WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  endelse

end







function CSEL_GET_VALUE, id

  ;Retrieve the state information.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  ret = state.cur_idx

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  return, ret 
end







function CW_COLORSEL, parent, FRAME = frame, UVALUE = uval, $
	XOFFSET=xoff, YOFFSET=yoff


  state = { by_index:0L, $		; By index slider widget
	    row:0L, $			; Row slider widget
	    column:0L, $		; Column slider widget
	    spectrum:0L, $		; Spectrum widget 
	    first_child:0L, $		; ID of first child of base.
	    x:0L, y:0L, $		; Current X and Y of marker in spectrum
	    cur_idx:0, $		; Current index
	    marker:bytarr(20,20,/nozero), $	; Byte array used as marker
	    unreach:bytarr(20,20,/nozero), $	; Image for unreachable colors
	    spectrum_win:0, $		; Spectrum draw window index
	    bright_idx:-1, dark_idx:-1}	; Dark and bright colors for marker


  on_error,2              ;Return to caller if an error occurs

  IF NOT (KEYWORD_SET(frame))  THEN frame = 0
  IF NOT (KEYWORD_SET(uval))  THEN uval = 0
  IF NOT (KEYWORD_SET(xoff))  THEN xoff = 0
  IF NOT (KEYWORD_SET(yoff))  THEN yoff = 0

  version = WIDGET_INFO(/VERSION)
  newer_motif = (version.style eq 'Motif') and (version.release ne '1.0')

  ; Create widgets
  base = WIDGET_BASE(parent,  frame=frame, uvalue=uval, $
	EVENT_FUNC='CSEL_EVENT', FUNC_GET_VALUE='CSEL_GET_VALUE', $
	PRO_SET_VALUE='CSEL_SET_VALUE', NOTIFY_REALIZE='CSEL_REALIZE', $
	XOFFSET=xoff, YOFFSET=yoff)
  if (newer_motif) then begin
        state.row = WIDGET_SLIDER(base, min=15, max=0, xoff = 325, $
	    yoff= 75, /vert, val=0, title='Row', ysize=320)
  endif else begin
        state.row = WIDGET_SLIDER(base, min=15, max=0, xoff = 325, $
		yoff= 75, /vert, val=0, title='Row', ysize=320, xsize=75)
  endelse
  state.column = WIDGET_SLIDER(base, max=15, title='Column', $
	xsize=320, yoff=400)
  state.spectrum=WIDGET_DRAW(base, xsize=320, ysize=320, /frame, /button, $
	yoff=75)
  state.by_index = WIDGET_SLIDER(base, max=!d.table_size-1, $
	title = 'By Index', xsize=320)


  state.cur_idx = 0
  state.x = 0
  state.y = 0

  ; Stash the state
  WIDGET_CONTROL, WIDGET_INFO(base, /CHILD), SET_UVALUE=state, /NO_COPY

  return, base
end

