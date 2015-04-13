; $Id: cw_zoom.pro,v 1.5 1994/11/23 20:37:25 alan Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_ZOOM
;
; PURPOSE:
;	This compound widget displays two images: an original image
;	in one window and a portion of the original image in another.
;	The user may select the center of the zoom region, the zoom scale,
;	the interpolation style, and the method of indicating the zoom center.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	Widget = CW_ZOOM(Parent)
;
; INPUTS:
;       Parent:	 The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	FRAME:	 If set, a frame will be drawn around the widget. The
;		 default is FRAME=0 (no frame).
;	MAX:	 The maximum zoom scale, which must be greater than
;		 or equal to 1. The default = 20.
;	MIN:	 The minimum zoom scale, which must be greater than
;		 or equal to 1. The default = 1.
;	RETAIN:	 Controls the setting for backing store for both windows.
;		 If backing store is provided, a window which was obscured
;		 will be redrawn when it becomes exposed. Set RETAIN=0 for
;		 no backing store. Set RETAIN=1 to "request backing store
;		 from server" (this is the default). Set RETAIN=2 for IDL
;		 to provide backing store.
;	SAMPLE:	 Set to zero for bilinear interpolation, or to a non-zero
;		 value for nearest neighbor interpolation. Bilinear
;		 interpolation gives higher quality results, but requires
;		 more time. The default is SAMPLE=0 (bilinear interpolation).
;	SCALE:	 The initial integer scale factor to use for the zoomed image.
;		 The default is SCALE=4. The scale must be greater than or
;		 equal to 1.
;	TRACK:	 Set to zero if the zoom window should be updated only when
;		 the mouse button is pressed. Set to a non-zero value if the
;		 zoom window should be updated continuously as the cursor
;		 is moved across the original image. Note: On slow systems,
;		 /TRACK performance can be inadequate. The default is TRACK=0.
;	UVALUE:	 The user value for the widget.
;	XSIZE:	 The width of the window (in pixels) for the original image.
;		 The default is 500.
;	YSIZE:	 The height of the window (in pixels) for the original image.
;		 The default is 500.
;	X_SCROLL_SIZE: The width of the visible part of the original image.
;		       This may be smaller than the actual width controlled
;		       by the XSIZE keyword. The default is 0, for no
;		       scroll bar.
;	Y_SCROLL_SIZE: The height of the visible part of the original image.
;		       This may be smaller than the actual height controlled
;		       by the YSIZE keyword. The default is 0, for no
;		       scroll bar.
;	X_ZSIZE: The width of the window for the zoomed image.
;		 The default is 250.
;	Y_ZSIZE: The height of the window for the zoomed image.
;		 The default is 250.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	When the "Report Zoom to Parent" button is pressed, this widget
;	will generate an event structure containing several data fields.
;		x_zsize, y_zsize:	size of the zoomed image
;		x0, y0:			lower left corner in original image
;		x1, y1:			upper right corner in original image
;	This event is a report to the parent that allows retrieval of the
;	zoomed image using WIDGET_CONTROL.
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		original, unzoomed image displayed by the widget.
;		The value may not be set until the widget has been
;		realized.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		zoomed image displayed by the widget.
;
; MODIFICATION HISTORY:
;	June 30, 1992, ACY
;       7 April 1993, AB, Removed state caching.
;	13 June, 1994, ACY, Save window and set to zoom prior to erase
;			    Add byte conversion in set_value
;	23 November, 1994, ACY, add code to handle cases in which the
;			set_value image is larger or smaller than the
;			original image.  Also remove scaling on display
;			operation (only scale the image when it is set.)
;-

;-----------------------------------------------------------------------------

PRO zoom_set_value, id, value

  ON_ERROR, 2						;return to caller

  ; Retrieve the state
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY


  ; Put the value into the state structure
  ;state.orig_image = byte(value)
  ; Handle cases where set_value image is smaller or larger than orig_image
  temp_size = size(value)
  set_x_sz = temp_size(1)
  set_y_sz = temp_size(2)
  ; get the smaller section common to both orig_image and set_value
  new_x_sz = state.x_im_sz < set_x_sz
  new_y_sz = state.y_im_sz < set_y_sz
  ; Set the state value.  Scale to range of the display here
  ; so that display operations after this can simply use TV
  ; and preserve the same range as in the original image
  state.orig_image(0:new_x_sz-1,0:new_y_sz-1) = $
     bytscl(value(0:new_x_sz-1,0:new_y_sz-1), top=!d.n_colors-1)


  ; Get the window number from the draw widget.  This can only be done
  ; after the widget has been realized.
  WIDGET_CONTROL, state.draw, GET_VALUE=win_temp
  state.draw_win = win_temp(0)
  WIDGET_CONTROL, state.zoom, GET_VALUE=win_temp
  state.zoom_win = win_temp(0)

  ; Use TV to display an image in the draw widget.  Set the window for
  ; the TV command since there may be other draw windows.
  ;Save window number
  save_win = !D.WINDOW
  WSET, state.draw_win
  TV, state.orig_image
  ;Restore window
  IF (save_win NE -1) THEN WSET, save_win

  draw_zoom, state, state.oldx, state.oldy

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

END

;-----------------------------------------------------------------------------

FUNCTION zoom_get_value, id

  ON_ERROR, 2                                           ;return to caller

  ; Retrieve the state
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  ; Get the value from the state structure
  ret = state.zoom_image

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  RETURN, ret
END

;-----------------------------------------------------------------------------

PRO draw_zoom, state, newx, newy

  ; compute size of rectangle in original image
  ; round up to make sure image fills zoom window
  rect_x = long(state.x_zm_sz / float(state.scale) + 0.999)
  rect_y = long(state.y_zm_sz / float(state.scale) + 0.999)

  ; compute location of origin of rect (user specified center)
  x0 = newx - rect_x/2
  y0 = newy - rect_y/2

  ; make sure rectangle fits into original image
  ;left edge from center
  x0 = x0 > 0
  ; limit right position
  x0 = x0 < (state.x_im_sz - rect_x)

  ;bottom
  y0 = y0 > 0
  y0 = y0 < (state.y_im_sz - rect_y)

  IF (state.scale EQ 1) THEN BEGIN
    ;Save window number
    save_win = !D.WINDOW
    WSET, state.zoom_win
    IF (rect_x GT state.x_im_sz OR rect_y GT state.y_im_sz) THEN BEGIN
      ERASE
      IF (rect_x GT state.x_im_sz) THEN x0 = 0 & rect_x = state.x_im_sz
      IF (rect_y GT state.x_im_sz) THEN y0 = 0 & rect_y = state.y_im_sz
    ENDIF
    ; don't use tvscl here, to preserve same range as in unzoomed image
    TV, state.orig_image(x0:x0+rect_x-1,y0:y0+rect_y-1)
    ;Restore window
    IF (save_win NE -1) THEN WSET, save_win
  ENDIF ELSE BEGIN
    ;Make integer rebin factors.  These may be larger than the zoom image
    dim_x = rect_x * state.scale
    dim_y = rect_y * state.scale

    x1 = x0 + rect_x - 1
    y1 = y0 + rect_y - 1

    temp_image = rebin(state.orig_image(x0:x1,y0:y1), $
                       dim_x, dim_y, $
                       sample=state.sample)

    ;Save the zoomed image
    state.zoom_image = $
                     temp_image(0:state.x_zm_sz-1,0:state.y_zm_sz-1)

    ;Save the corners in original image
    state.x0 = x0
    state.y0 = y0
    state.x1 = x1
    state.y1 = y1

    ;Display the new zoomed image
    ;Save window number
    save_win = !D.WINDOW
    WSET, state.zoom_win
    ; don't use tvscl here, to preserve same range as in unzoomed image
    TV, state.zoom_image
    ;Restore window
    IF (save_win NE -1) THEN WSET, save_win

 ENDELSE

END


;-----------------------------------------------------------------------------

FUNCTION zoom_event, event

  ; Retrieve the structure from the child that contains the sub ids
  parent=event.handler
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY


  CASE event.id OF
    state.draw: $
       IF state.track GT 0 OR event.press EQ 1 THEN BEGIN
          draw_zoom, state, event.x, event.y
          state.oldx = event.x
          state.oldy = event.y
       ENDIF

    state.slide: $
       BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE = temp_scale
          IF (temp_scale LT 1) THEN temp_scale = 1
          state.scale = temp_scale
          draw_zoom, state, state.oldx, state.oldy
       END

    state.sample_base: $
       CASE event.value OF
          state.nn_id: BEGIN
			  state.sample = 1
			  draw_zoom, state, state.oldx, state.oldy
		       END
          state.bilin_id: BEGIN
                             state.sample = 0
                             draw_zoom, state, state.oldx, state.oldy
                	  END
       ENDCASE

    state.track_base: $
       CASE event.value OF
          state.notrack_id:  state.track = 0
          state.track_id:    state.track = 1
       ENDCASE
    state.report_id: begin
	  ret = {ZOOM_EVENT, ID:parent, $
		TOP:event.top, HANDLER:0L, $
		x_zsize:state.x_zm_sz, y_zsize:state.y_zm_sz, $
		x0:state.x0, y0:state.y0, $
		x1:state.x1, y1:state.y1}
	  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
	  return, ret
	end
ENDCASE

; Swallow events, except for the REPORT event
WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
RETURN, 0

END

;-----------------------------------------------------------------------------


FUNCTION cw_zoom, parent, $
		FRAME=frame, $
		MAX=max, $
		MIN=min, $
		RETAIN=retain, $
		SAMPLE=sample, $
		SCALE=scale, $
		TRACK=track, $
		UVALUE = uval, $
		XSIZE=xsize, $
		YSIZE=ysize, $
		X_SCROLL_SIZE=x_scroll_size, $
		Y_SCROLL_SIZE=y_scroll_size, $
		X_ZSIZE=x_zsize, $
		Y_ZSIZE=y_zsize

  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'

  ON_ERROR, 2						;return to caller

  ; Defaults for keywords
  IF (N_ELEMENTS(frame) EQ 0) THEN frame = 0L
  IF (N_ELEMENTS(max) EQ 0) THEN max = 20L
  IF (N_ELEMENTS(min) EQ 0) THEN min = 1L
  IF (N_ELEMENTS(retain) EQ 0) THEN retain = 1L
  IF (N_ELEMENTS(sample) EQ 0) THEN sample = 0L
  IF (N_ELEMENTS(scale) EQ 0) THEN scale = 4L
  IF (N_ELEMENTS(track) EQ 0) THEN track = 0L
  IF (N_ELEMENTS(uval) EQ 0)  THEN uval = 0L
  IF (N_ELEMENTS(xsize) EQ 0) THEN xsize = 500L
  IF (N_ELEMENTS(ysize) EQ 0) THEN ysize = 500L
  IF (N_ELEMENTS(x_scroll_size) EQ 0) THEN x_scroll_size = 0L
  IF (N_ELEMENTS(y_scroll_size) EQ 0) THEN y_scroll_size = 0L
  IF (N_ELEMENTS(x_zsize) EQ 0) THEN x_zsize = 250L
  IF (N_ELEMENTS(y_zsize) EQ 0) THEN y_zsize = 250L

  base = WIDGET_BASE(parent, $
			EVENT_FUNC = 'zoom_event', $
			FRAME = frame, $
			FUNC_GET_VALUE='ZOOM_GET_VALUE', $
			PRO_SET_VALUE='ZOOM_SET_VALUE', $
			/ROW, $
			UVALUE = uval)

  lcol = WIDGET_BASE(base, /COLUMN)

  ; A widget called 'draw' is created.
  draw = WIDGET_DRAW(lcol, $
	/BUTTON_EVENTS, $	;generate events when buttons pressed
	/MOTION_EVENTS, $
	/FRAME, $
	RETAIN = retain, $
	XSIZE = xsize, $
	YSIZE = ysize, $
	X_SCROLL_SIZE = x_scroll_size, $
	Y_SCROLL_SIZE = y_scroll_size)

  rcol = WIDGET_BASE(base, /COLUMN)

  ; The REPORT button:
  report = WIDGET_BUTTON(rcol, $
		VALUE = 'REPORT ZOOM TO PARENT')

  ; A label containing some instructions:
  wdrlabel = WIDGET_LABEL(rcol, $
	   VALUE = 'Press left button to zoom.')

  ; A widget called 'zoom' is created.
  zoom = WIDGET_DRAW(rcol, $
        /FRAME, $
        RETAIN = retain, $
        XSIZE = x_zsize, $
        YSIZE = y_zsize)

  IF (min LT 1) THEN min = 1
  IF (max LT 1) THEN max = 1
  slide = WIDGET_SLIDER(rcol, $
                        MINIMUM = min, $
                        MAXIMUM = max, $
                        VALUE = scale, $
                        TITLE = 'Zoom Scale', $
                        /FRAME)

  ;make sure sample is 0 or 1
  IF (sample GT 0) THEN sample = 1
  sample_base = cw_bgroup(rcol, ['Bilinear', 'Nearest Neighbor'], $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME, $
		IDS=sample_ids, $
		LABEL_TOP = 'Interpolation Style', $
		/NO_RELEASE, $
		/RETURN_ID, $
		SET_VALUE = sample)

  ;make sure track is 0 or 1
  IF (track GT 0) THEN track = 1
  track_base = cw_bgroup(rcol, ['Button Press Only', 'Track Cursor'], $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME, $
		IDS=track_ids, $
		LABEL_TOP = 'Cursor Input Style', $
		/NO_RELEASE, $
		/RETURN_ID, $
		SET_VALUE = track)

  state = {	orig_image:	BYTARR(xsize,ysize), $
		zoom_image:	BYTARR(x_zsize,y_zsize), $
		draw:		draw, $
		zoom:		zoom, $
		slide:		slide, $
		sample_base:	sample_base, $
		bilin_id:	sample_ids(0), $
		nn_id:		sample_ids(1), $
		track_base:	track_base, $
		notrack_id:	track_ids(0), $
		track_id:	track_ids(1), $
		report_id:	report, $
		draw_win:	-1L, $
		zoom_win:	-1L, $
		x_im_sz:	xsize, $
		y_im_sz:	ysize, $
		retain:		1L, $
		track:		track, $
		scale:		scale, $
		sample:		sample, $
		x_zm_sz:	x_zsize, $
		y_zm_sz:	y_zsize, $
		oldx:		xsize / 2L, $
		oldy:		ysize / 2L, $
		x0:		0L, $
		y0:		0L, $
		x1:		0L, $
		y1:		0L $
		}

  WIDGET_CONTROL, WIDGET_INFO(base, /CHILD), SET_UVALUE=state, /NO_COPY
  RETURN, base

END
