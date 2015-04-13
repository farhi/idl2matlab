; $Id: slide_image.pro,v 1.3 1994/09/23 14:07:54 kirk Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	SLIDE_IMAGE
;
; PURPOSE:
;	Create a scrolling graphics window for examining large images.
;	By default, 2 draw widgets are used.  The left draw widget shows
;	a reduced version of the complete image, while the draw widget on
;	the right displays the actual image with scrollbars that allow sliding
;	the visible window.
;
; CALLING SEQUENCE:
;	SLIDE_IMAGE [, Image]
;
; INPUTS:
;	Image:	The 2-dimensional image array to be displayed.  If this 
;		argument is not specified, no image is displayed. The 
;		FULL_WINDOW and SCROLL_WINDOW keywords can be used to obtain 
;		the window numbers of the 2 draw widgets so they can be drawn
;		into at a later time.
;
; KEYWORDS:
;      CONGRID:	Normally, the image is processed with the CONGRID
;		procedure before it is written to the fully visible
;		window on the left. Specifying CONGIRD=0 will force
;		the image to be drawn as is.
;
;  FULL_WINDOW:	A named variable in which to store the IDL window number of \
;		the non-sliding window.  This window number can be used with 
;		the WSET procedure to draw to the scrolling window at a later
;		point.
;
;	GROUP:	The widget ID of the widget that calls SLIDE_IMAGE.  If this
;		keyword is specified, the death of the caller results in the
;		death of SLIDE_IMAGE.
;
;	ORDER:	This keyword is passed directly to the TV procedure
;		to control the order in which the images are drawn. Usually,
;		images are drawn from the bottom up.  Set this keyword to a
;		non-zero value to draw images from the top down.
;
;     REGISTER:	Set this keyword to create a "Done" button for SLIDE_IMAGE
;		and register the widgets with the XMANAGER procedure.
;
;		The basic widgets used in this procedure do not generate
;		widget events, so it is not necessary to process events
;		in an event loop.  The default is therefore to simply create
;		the widgets and return.  Hence, when register is not set, 
;		SLIDE_IMAGE can be displayed and the user can still type 
;		commands at the "IDL>" prompt that use the widgets.
;
;	RETAIN:	This keyword is passed directly to the WIDGET_DRAW
;		function, and controls the type of backing store
;		used for the draw windows.  If not present, a value of
;		2 is used to make IDL handle backing store.
;
; SLIDE_WINDOW:	A named variable in which to store the IDL window number of 
;		the sliding window.  This window number can be used with the 
;		WSET procedure to draw to the scrolling window at a later 
;		time.
;
;	TITLE:	The title to be used for the SLIDE_IMAGE widget.  If this
;		keyword is not specified, "Slide Image" is used.
;
;	TOP_ID:	A named variable in which to store the top widget ID of the 
;		SLIDE_IMAGE hierarchy.  This ID can be used to kill the 
;		hierarchy as shown below:
;
;			SLIDE_IMAGE, TOP_ID=base, ...
;			.
;			.
;			.
;			WIDGET_CONTROL, /DESTROY, base
;
;	XSIZE:	The maximum width of the image that can be displayed by
;		the scrolling window.  This keyword should not be confused 
;		with the visible size of the image, controlled by the XVISIBLE
;		keyword.  If XSIZE is not specified, the width of Image is 
;		used.  If Image is not specified, 256 is used.
;
;     XVISIBLE:	The width of the viewport on the scrolling window.  If this 
;		keyword is not specified, 256 is used.
;
;	YSIZE:	The maximum height of the image that can be displayed by
;		the scrolling window.  This keyword should not be confused 
;		with the visible size of the image, controlled by the YVISIBLE
;		keyword.  If YSIZE is not present the height of Image is used.
;		If Image is not specified, 256 is used.
;
;     YVISIBLE:	The height of the viewport on the scrolling window. If
;		this keyword is not present, 256 is used.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Widgets for displaying a very large image are created.
;	The user typically uses the window manager to destroy
;	the window, although the TOP_ID keyword can also be used to
;	obtain the widget ID to use in destroying it via WIDGET_CONTROL.
;
; RESTRICTIONS:
;	Scrolling windows don't work correctly if backing store is not 
;	provided.  They work best with window-system-provided backing store
;	(RETAIN=1), but are also usable with IDL provided backing store 
;	(RETAIN=2).
;
;	Various machines place different restrictions on the size of the
;	actual image that can be handled.
;
; MODIFICATION HISTORY:
;	7 August, 1991, Written by AB, RSI.
;	10 March, 1993, ACY, Change default RETAIN=2
;	23 Sept., 1994  KDB, Fixed Typo in comments. Fixed error in
;			Congrid call. xvisible was used instead of yvisible.
;-


pro SLIDE_IMG_EVENT, ev
  WIDGET_CONTROL, ev.top, /DESTROY
end







pro slide_image, image, CONGRID=USE_CONGRID, ORDER=ORDER, REGISTER=REGISTER, $
	RETAIN=RETAIN, SHOW_FULL=SHOW_FULL, SLIDE_WINDOW=SLIDE_WINDOW, $
	XSIZE=XSIZE, XVISIBLE=XVISIBLE, YSIZE=YSIZE, YVISIBLE=YVISIBLE, $
	TITLE=TITLE, TOP_ID=BASE, FULL_WINDOW=FULL_WINDOW, GROUP = GROUP

  if (n_params() ne 0) then begin
    image_size = SIZE(image)
    if (image_size(0) ne 2) then message,'Image must be a 2-D array'
    if (n_elements(XSIZE) eq 0) then XSIZE = image_size(1)
    if (n_elements(YSIZE) eq 0) then YSIZE = image_size(2)
  endif else begin
    image_size=bytarr(1)
    if (n_elements(XSIZE) eq 0) then XSIZE = 256
    if (n_elements(YSIZE) eq 0) then YSIZE = 256
  endelse
  if (n_elements(xvisible) eq 0) then XVISIBLE=256
  if (n_elements(Yvisible) eq 0) then YVISIBLE=256
  if(n_elements(SHOW_FULL) eq 0) THEN SHOW_FULL = 1
  if(not KEYWORD_SET(ORDER)) THEN ORDER = 0
  if(not KEYWORD_SET(USE_CONGRID)) THEN USE_CONGRID = 1
  if(n_elements(RETAIN) eq 0) THEN RETAIN = 2
  if(n_elements(TITLE) eq 0) THEN TITLE='Slide Image'
  if(not KEYWORD_SET(REGISTER)) THEN REGISTER = 0

  if (REGISTER) then begin
    base = WIDGET_BASE(title=title, /COLUMN)
    junk = WIDGET_BUTTON(WIDGET_BASE(base), value='Done')
    ibase = WIDGET_BASE(base, /ROW)
  endif else begin
    base = WIDGET_BASE(title=title, /ROW)
    ibase = base
  endelse

  if (SHOW_FULL) then begin
      fbase = WIDGET_BASE(ibase, /COLUMN, /FRAME)
        junk = WIDGET_LABEL(fbase, value='Full Image')
        all = widget_draw(fbase,retain=retain,xsize=xvisible,ysize=yvisible)
      sbase = WIDGET_BASE(ibase, /COLUMN, /FRAME)
        junk = WIDGET_LABEL(sbase, value='Full Resolution')
        scroll = widget_draw(sbase, retain=retain,xsize=xsize,ysize=ysize, $
		/scroll, x_scroll_size=xvisible, y_scroll_size=yvisible)
    WIDGET_CONTROL, /REAL, base
    WIDGET_CONTROL, get_value=FULL_WINDOW, all
  endif else begin
    scroll = widget_draw(ibase, retain=retain, xsize=xsize, ysize=ysize, $
	/frame, /scroll, x_scroll_size=xvisible, y_scroll_size=yvisible)
    WIDGET_CONTROL, /REAL, base
    FULL_WINDOW=-1
  endelse

  WIDGET_CONTROL, get_value=SLIDE_WINDOW, scroll

  ; Show the image(s) if one is present
  if (image_size(0) ne 0) then begin
    cur_win = !d.window
    if (SHOW_FULL) then begin
      WSET, FULL_WINDOW
      if (use_congrid) then begin
	TV, congrid(image, xvisible, yvisible), ORDER=ORDER
      endif else begin
	TV, image, ORDER=ORDER
      endelse
    endif
  WSET, SLIDE_WINDOW
  TV, image, ORDER=ORDER
  if (cur_win ne -1) then WSET, cur_win
  endif
  if (n_elements(group) eq 0) then group=base

  if (REGISTER) then XMANAGER, 'SLIDE_IMAGE', base, event='SLIDE_IMG_EVENT', $
	GROUP_LEADER = GROUP

end
