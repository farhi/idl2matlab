; $Id: cw_orient.pro,v 1.4 1993/10/07 23:06:44 doug Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

PRO drawbox, state, FINAL = FINAL

	; This routine is responsible for drawing the cube when the
	; cw_orient compound widget is in the trackball mode.  When 
	; the FINAL keyword is set, the view is first erased and then
	; the origin indicator is added.

  COMMON CW_ORIENT_BOX, box, faces, norms

  wind = !D.WINDOW				; set to the view window and
  wset, state.trackball				; make sure to save the old

  IF (KEYWORD_SET(final)) THEN ERASE

  IF ( NOT(KEYWORD_SET(box))) THEN BEGIN	; build the face vertices
						; topology and normals only
						; once
    verts = [[.15, .15, .15],[.15, .15, .85],[.15, .85, .85],[.15, .85, .15], $
	     [.85, .15, .15],[.85, .15, .85],[.85, .85, .85],[.85, .85, .15]]

    faces = [[0,1,2,3,0], $
             [1,2,6,5,1], $
             [4,5,6,7,4], $
             [4,7,3,0,4], $
             [2,6,7,3,2], $
             [1,5,4,0,1]]

    norms = [[-1, 0, 1, 0, 0, 0, 0], $		; The last entry in the norms
	     [0, 0, 0, 0, 1, -1, 0], $		; is the origin.  Each normal
	     [0, 1, 0, -1, 0, 0, 0], $		; is a vector from the origin
	     [1, 1, 1, 1, 1, 1, 1 ]] 		; to the endpoint in +/- XYZ.
  ENDIF
  ns = norms # !P.T				; Translate the norms for the
						; current !P.T
  FOR i = 0, 5 DO BEGIN
    IF (ns(6,2) - ns(i,2) LT 0) THEN $		; Compute the Z component of
	PLOTS, verts(*, faces(*,i)), $		; each normal and if it is less
		/T3D, /NORMAL, $		; than zero, the cooresponding
		COLOR = !D.N_COLORS / 2		; face can be seen so draw it.
  ENDFOR

  IF (KEYWORD_SET(FINAL)) THEN BEGIN		; If a final drawing, then
    plots, [.15, .15, .15], $			; draw the little origin 
	   [.25, .15, .15], $			; indicator.
	   [.15, .15, .25], /T3D, /NORMAL
    plots, [.25, .15], $
	   [.15,.15], $
	   [.15,.15], /T3D, /NORMAL
  ENDIF

  WSET, wind
END ; end of drawbox routine

;---------------------------------------------------------------------------

function CW_ORIENT_EVENT, ev

  COMMON CW_OR_PRIVATE, scaler
  COMMON CW_TMP, xlast, zlast, xrot, zrot

  base = ev.handler
  stash = WIDGET_INFO(base, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY


  CASE ev.id OF

    state.modemenu: BEGIN

      CASE ev.value OF

	1: BEGIN; If slider mode was selected, map the sliders and set current.
            IF (state.currentbase NE state.slidebase) THEN BEGIN
	      WIDGET_CONTROL, state.slidebase, MAP = 1
	      WIDGET_CONTROL, state.currentbase, MAP = 0
	      state.currentbase = state.slidebase
	    ENDIF
	    GOTO, SWALLOW_EV_RET
	  END

	2: BEGIN; If trackball mode was selected, make sure window id is known
	    IF (state.trackballid NE state.currentbase) THEN BEGIN
	      WIDGET_CONTROL, state.trackbase, MAP = 1
	      WIDGET_CONTROL, state.currentbase, MAP = 0
	      state.currentbase = state.trackballid
	      IF state.trackball EQ 0 THEN BEGIN
		WIDGET_CONTROL, state.trackballid, GET_VALUE = temp
		state.trackball = temp(0)
	      ENDIF
	      drawbox, state, /FINAL
	    ENDIF
	    GOTO, SWALLOW_EV_RET
	  END

	3: BEGIN; If reset menu selection was made.
	    WIDGET_CONTROL, state.xslide, GET_UVALUE = xrot
	    WIDGET_CONTROL, state.zslide, GET_UVALUE = zrot
	    t3d, /RESET, TRANSLATE = [-.5, -.5, -.5], SCALE = scaler
	    t3d, ROTATE = [-90, xrot, 0]
	    t3d, ROTATE = [zrot, 0, 0]
	    t3d, TRANSLATE = [.5, .5, .5]
	    WIDGET_CONTROL, state.xslide, SET_VALUE = xrot
	    WIDGET_CONTROL, state.zslide, SET_VALUE = zrot
	    IF (state.trackballid EQ state.currentbase) THEN $
	      drawbox, state, /FINAL
	  END
	ENDCASE ; End of ev.value CASE
      END ; End of case state.modemenu.

    state.trackballid: BEGIN
	IF ev.press THEN BEGIN
		; Begin tracking the trackball as the mouse button is down.
		; Set the drawing mode to XOR
	  WIDGET_CONTROL, state.xslide, GET_VALUE = xrot
	  WIDGET_CONTROL, state.zslide, GET_VALUE = zrot
	  xlast = ev.x
	  zlast = ev.y
	  state.trackmode = 1
	  DEVICE, SET_GRAPHICS_FUNCTION = 6
	  GOTO, SWALLOW_EV_RET
	ENDIF

	IF ev.release THEN BEGIN
		; Once finished tracking, do range checking and then restore
		; drawing mode and redraw the cube.
	  IF (xrot LT 0) THEN xrot = xrot + 360
	  IF (zrot LT 0) THEN zrot = zrot + 360
	  IF (xrot GT 360) THEN xrot = xrot - 360
	  IF (zrot GT 360) THEN zrot = zrot - 360
	  WIDGET_CONTROL, state.xslide, SET_VALUE = xrot
	  WIDGET_CONTROL, state.zslide, SET_VALUE = zrot
  	  state.trackmode = 0
  	  DEVICE, SET_GRAPHICS_FUNCTION = 3
  	  drawbox, state, /FINAL
	ENDIF

	IF state.trackmode THEN BEGIN
  	  drawbox, state
		; For each direction, figure out the magnitude of movement
		; first and then the direction.
	  IF (xlast NE ev.x) THEN BEGIN
  	    xangle = ASIN((100.0 - ABS(xlast-ev.x)) / 100.0) * 1.7
	    IF ((xlast - ev.x) LT 0) THEN xangle = -xangle
  	    xlast = ev.x
  	  ENDIF ELSE xangle = 0

	  IF (zlast NE ev.y) THEN BEGIN
	    zangle = -ASIN((100.0 - ABS(zlast-ev.y)) / 100.0) * 1.7
	    IF ((zlast - ev.y) LT 0) THEN zangle = -zangle
	    zlast = ev.y
	  ENDIF ELSE zangle = 0

	  xrot = xrot - xangle			; Apply the changes to the
	  zrot = zrot - zangle			; rotation values.

	  t3d, /RESET, TRANSLATE = [-.5, -.5, -.5], SCALE = scaler
	  t3d, ROTATE = [-90, xrot, 0]
	  t3d, ROTATE = [zrot, 0, 0]
	  t3d, TRANSLATE = [.5, .5, .5]

	  drawbox, state
	  GOTO, SWALLOW_EV_RET			; Don't return an event until
	ENDIF					; the user lets go.

	IF (ev.release EQ 0) THEN GOTO, SWALLOW_EV_RET
      END; case state.trackballid


    ELSE: BEGIN	; Handle the sliders which are the only events not covered
		; above.
	WIDGET_CONTROL, state.xslide, GET_VALUE = xrot
	WIDGET_CONTROL, state.zslide, GET_VALUE = zrot
	t3d, /RESET, TRANSLATE = [-.5, -.5, -.5], SCALE = scaler
	t3d, ROTATE = [-90, xrot, 0]
	t3d, ROTATE = [zrot, 0, 0]
	t3d, TRANSLATE = [.5, .5, .5]
      END

  ENDCASE ; End of ev.id CASE

  ev.id = base
  ev.handler = 0

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  RETURN, ev

swallow_ev_ret:
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  RETURN, 0

END ; End of cw_orient_event routine.

;---------------------------------------------------------------------------

FUNCTION CW_ORIENT, parent, UVALUE = UVALUE, XSIZE = XSIZE, YSIZE = YSIZE, $
			FRAME = FRAME, AX = AX, AZ = AZ, TITLE = TITLE

; Copyright (c) 1992, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_ORIENT
;
; PURPOSE:
;	This compound widget provides a means to interactively adjust the
;	three dimensional drawing transformation (!P.T). The compound
;	widget only returns events when !P.T has been altered.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget = CW_ORIENT(Parent)
;
; INPUTS:
;       Parent:	The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	AX:	The initial rotation in the X direction. If not set, the 
;		default is 30 degrees.
;	AZ:	The initial rotation in the Z direction. If not set, the 
;		default is 30 degrees.
;	FRAME:	If set, a frame will be drawn around the widget. The
;		default is FRAME=0.
;	TITLE:	The title of the widget. (Default is no title.)
;	UVALUE:	The user value of the widget.
;	XSIZE:	Determines the width of the widget. The default is 100.
;	YSIZE:	Determines the height of the widget. The default is 100.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	CW_OR_PRIVATE: Private to this module.
;
; SIDE EFFECTS:
;	This widget generates event structures each time a modification
;	to the orientation is made.  This widget also resets !P.T to 
;	reflect the changed orientation.
;
; MODIFICATION HISTORY:
;	August 7, 1992, SMR
;       7 April 1993, AB, Removed state caching.
;-

  COMMON CW_OR_PRIVATE, scaler

  ON_ERROR, 2				; Return to caller.

  	; Define the contents of the state structure.

  state = {trackball:0L, trackballid:0L, $
		slidebase:0L, trackbase:0L, currentbase:0L, $
		xslide:0L, zslide:0L, $
		trackmode:0, modemenu:0L}

  	; Set default values for the keywords.

  IF N_ELEMENTS(FRAME) NE 0	THEN frm = 1 ELSE frm = 0
  IF N_ELEMENTS(UVALUE) NE 0	THEN uval = UVALUE ELSE uval = 0
  IF N_ELEMENTS(XSIZE) NE 0	THEN xsi = XSIZE ELSE xsi = 120
  IF N_ELEMENTS(YSIZE) NE 0	THEN ysi = YSIZE ELSE ysi = 120
  IF N_ELEMENTS(AX) NE 0	THEN xrot = AX ELSE xrot = 30
  IF N_ELEMENTS(AZ) NE 0	THEN zrot = AZ ELSE zrot = 30
  IF N_ELEMENTS(TITLE) NE 0	THEN tit = TITLE ELSE tit = ''

  orientbase = WIDGET_BASE(parent, $
			EVENT_FUNC = "CW_ORIENT_EVENT", $
			UVALUE = uval, $
			/COLUMN, $
			FRAME = frm)

  IF (TIT NE '') THEN title = WIDGET_LABEL(orientbase, $
			VALUE = TIT)

  panelbase = WIDGET_BASE(orientbase)
  state.slidebase = WIDGET_BASE(panelbase, /COLUMN, MAP = 1)
  state.currentbase = state.slidebase

  state.modemenu = cw_pdmenu(orientbase, [ $
		{CW_PDMENU_S, flags:1, name:'Orientation'}, $
		{CW_PDMENU_S, flags:0, name:'Sliders'}, $
		{CW_PDMENU_S, flags:0, name:'Trackball'},   $
		{CW_PDMENU_S, flags:0, name:'Reset Transformation'}], ids = i)

  state.xslide = WIDGET_SLIDER(state.slidebase, $
			MIN = 0, MAX = 359, VALUE = xrot, $
			TITLE = "X angle", $
			XSIZE = 120, $
			UVALUE = xrot)
  state.zslide = WIDGET_SLIDER(state.slidebase, $
			MIN = 0, MAX = 359, VALUE = zrot, $
			TITLE = "Z angle", $
			XSIZE = 120, $
			UVALUE = zrot)

  state.trackbase = WIDGET_BASE(panelbase, MAP = 0)
  state.trackballid = WIDGET_DRAW(state.trackbase, $
			XSIZE = xsi, YSIZE = ysi, $
			/MOTION, /BUTTON)

  WIDGET_CONTROL, WIDGET_INFO(orientbase, /CHILD), SET_UVALUE=state, /NO_COPY

  scaler = REPLICATE(1./SQRT(3),3)

  !X.S = [0, 1]
  !Y.S = [0, 1]
  !Z.S = [0, 1]
  t3d, /RESET, TRANSLATE = [-.5, -.5, -.5], SCALE = scaler
  t3d, ROTATE = [-90, xrot, 0]
  t3d, ROTATE = [zrot, 0, 0]
  t3d, TRANSLATE = [.5, .5, .5]

  RETURN, orientbase
END
