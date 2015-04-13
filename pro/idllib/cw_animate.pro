; $Id: cw_animate.pro,v 1.11 1995/04/20 23:09:28 billo Exp $

;
; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_ANIMATE
;
; PURPOSE:
;	This widget displays an animated sequence of images using
;	X-windows Pixmaps. This is a compound widget, based on the
;	XINTERANIMATE procedure, with the following advantages:
;		- It can be included in other applications.
;		- Multiple copies can be run simultaneously.
;
;	The speed and direction of the display can be adjusted using
;	the widget interface.
;
; CATEGORY:
;	Image display, compound widgets.
;
; CALLING SEQUENCE:
;	To initially create:
;		widget = CW_ANIMATE(PARENT, SIZEX, SIZEY, NFRAMES)
;
;	To reinitialize when another animation is loaded:
;		CW_ANIMATE_INIT, ANIMATEBASE, SIZEX, SIZEY, NFRAMES
;
;	To load a single image:
;		CW_ANIMATE_LOAD, WIDGET, IMAGE = IMAGE, FRAME = FRAME_INDEX
;
;	To load a single image that is already displayed in an existing window:
;
;		CW_ANIMATE_LOAD, WIDGET, FRAME = FRAME_INDEX, $
;			WINDOW = [WINDOW_NUMBER [, X0, Y0, SX, SY]]
;
;	(This technique is much faster than reading back from the window.)
;
;	To display the animation after all the images have been loaded:
;
;		CW_ANIMATE, WIDGET [, RATE]
;
;	To get a copy of the vector of Pixmaps being used by the widget.
;	If this routine is called, the widget does not destroy the pixmaps
;	when it is destroyed. The user can then provide them to a later
;	call to CW_ANIMATE to re-use them while skipping the Pixmap creation
;	and rendering step:
;
;		CW_ANIMATE_GETP, widget, PIXMAPS
;
; INPUTS:
;   CW_ANIMATE:
;		PARENT:	 The ID of the parent widget.
;		SIZEX:	 The width of the displayed image.
;		SIZEY:	 The height of the displayed image.
;		NFRAMES: The number of frames in the animation sequence.
;
;   CW_ANIMATE_INIT:
;		ANIMATEBASE: The ID of the base animation widget.
;		SIZEX:	 The width of the displayed image.
;		SIZEY:	 The height of the displayed image.
;		NFRAMES: The number of frames in the animation sequence.
;
;  	CW_ANIMATE_LOAD:
;		WIDGET:	 The ID of the widget (as created with CW_ANIMATE)
;			 into which the image should be loaded.
;
;   CW_ANIMATE_RUN:
;		WIDGET:	 The ID of the widget (as created with CW_ANIMATE)
;			 into which the image should be loaded.
;		RATE:	 A value between 0 and 100 that represents the
;			 speed of the animation as a percentage of the
;			 maximum display rate. The fastest animation has
;			 a value of 100 and the slowest  has a value of 0.
;			 The default animation rate is 100.
;       STOP:    If this keyword is set, the animation is stopped.
;       NFRAMES: Specifies the number of frames to animate, must
;                        <= the number specified in CW_ANIMATE().
;
; KEYWORD PARAMETERS:
;   	CW_ANIMATE:
;		PIXMAPS: This keyword provides the new widget with a vector
;			 of pre-existing pixmap (off screen window) IDs.
;			 This vector is usually obtained from a call to
;			 CW_ANIMATE_GETP applied to a previous animation
;			 widget.
;		UVALUE:  A user supplied value to be stored in the widget's
;			 user value field.
;               NO_KILL: If NOT set, an "End Animation" button is added to the
;			 animation base.  If set the button is not added.
;		OPEN_FUNC: A user supplied string that specifies a callback
;			 function name. When a value is specified for this
;			 keyword, an "Open..." pushbutton is added to the
;			 window.  When the "Open..." pushbutton is clicked
;			 the OPEN_FUNC function is called to load new
;			 animation data.
;
;   	CW_ANIMATE_INIT:
;		PIXMAPS: This keyword provides the new widget with a vector
;			 of pre-existing pixmap (off screen window) IDs.
;			 This vector is usually obtained from a call to
;			 CW_ANIMATE_GETP applied to a previous animation
;			 widget.
;
;   	CW_ANIMATE_LOAD:
;		CYCLE:   If set, cycle. Normally, frames are displayed
;			 going either forward or backwards. If CYCLE is
;			 set, reverse direction after the last frame in
;			 either direction is displayed.
;		FRAME: 	 The frame number to be loaded. This is a value
;			 between 0 and NFRAMES. If not supplied, frame 0
;		  	 is loaded.
;		IMAGE:   The image to be loaded.
;		ORDER:   Set this keyword to display images from the top
;			 down instead of the default bottom up. This keyword
;			 is only used when loading images with the IMAGE
;			 keyword.
;		TRACK:   If set, the frame slider tracks the current frame.
;			 Default is not to track.
;		WINDOW:  When this keyword is specified, an image is copied
;			 from an existing window to the animation pixmap.
;			 When using X windows, this technique is much faster
;			 than reading from the display and then loading with
;			 the IMAGE keyword.
;
;			 The value of this parameter is either an IDL window
;			 number (in which case the entire window is copied),
;			 or a vector containing the window index and the
;			 rectangular bounds of the area to be copied. For
;			 example:
;			 WINDOW = [Window_Number, X0, Y0, Sx, Sy]
;
;      		XOFFSET: The horizontal offset, in pixels from the left of
;			 the frame, of the image in the destination window.
;
;      		YOFFSET: The vertical offset, in pixels from the bottom of
;			 the frame, of the image in the destination window.
;
; OUTPUTS:
;	No explicit outputs.
;
; SIDE EFFECTS:
;	If the widget is realized before calls to CW_ANIMATE_LOAD, the frames
;	are displayed as they are loaded. This provides the user with an
;	indication of how things are progressing.
;
;	When the widget is destroyed, it destroys the pixmaps used in the
;	animation, unless they were previously obtained via CW_ANIMATE_GETP
;       and the KILL_ANYWAY keyword was not set.
;
;	The only event returned by this widget indicates that the user
;	has pressed the DONE button. The parent application should use
;	this as a signal to kill the animation widget via WIDGET_CONTROL.
;
; RESTRICTIONS:
;	If more than one animation widget is running at a time, they
;	will fight for resources and run slower.
;
; PROCEDURE:
;	When initialized, this procedure creates pixmaps containing the
;	frames of the animation sequence. Once the images are loaded,
;	they are displayed by copying the images from the pixmap or buffer
;	to the visible draw widget.
;
; EXAMPLE:
;	Assume the following event handler procedure exists:
;		PRO EHANDLER, EV
;		  WIDGET_CONTROL, /DESTROY, EV.TOP
;		end
;
;	Enter the following commands to open the file ABNORM.DAT (a series
;	of images of a human heart) and load the images it contains into
;	an array H:
;
;		OPENR, 1, FILEPATH('abnorm.dat', SUBDIR = 'images')
;		H = BYTARR(64, 64, 16)
;		READU, 1, H
;		CLOSE, 1
;		H = REBIN(H, 128, 128, 16)
;
;	Create an instance of the animation widget at load the frames:
;
;		base = widget_base()
;		animate = CW_ANIMATE(base, 128, 128, 16)
;		WIDGET_CONTROL, /REALIZE, base
;		for i=0,15 do CW_ANIMATE_LOAD, animate, FRAME=i, IMAGE=H(*,*,I)
;
;	Start the animation:
;
;		CW_ANIMATE_RUN, animate
;		XMANAGER, "CW_ANIMATE Demo", base, EVENT_HANDLER = "EHANDLER"
;
;	Pressing the DONE button kills the application.
;
; MODIFICATION HISTORY:
;	AB, June 1992		Heavily based on the XINTERANIMATE procedure.
;	SR, September 1992	Fixed a problem when a paused animation's
;				frame selection was moved and the resulting
;				frame change ended up in another animation.
;	SR, November  1992	Fixed a problem when a single paused animation
;				would fail when the frame selection slider
;				event tried to set do a bad drawing window.
;	DMS/AB, March, 1993	Got rid of state caching. Got rid of
;				XMANAGER background tasks in favor of new
;				"WIDGET_CONTROL,timer=" feature.
;	ACY, October 1993	Set RETAIN=2 for draw widget to prevent
;				clipping by an overlapping window when
;				loading frames.
;       DMS, Dec, 1993          Added STOP and NFRAMES keywords to CW_ANIMATE_RUN.
;                               Added KILL_ANYWAY keyword to CW_ANIMATE_GETP.
;       WSO, Jan, 1995          Added OPEN_FUNC keyword and updated UI.
;-

PRO SetBitmapButtons, state

  COMMON BitmapButtons, $
             reversebutton, blk_reversebutton, $
             pausebutton, blk_pausebutton, $
             playbutton, blk_playbutton, $
             cycleForwardBtn, blk_cycleForwardBtn

  WIDGET_CONTROL, state.currentAction, SET_VALUE = state.currentBitmap

  IF state.framedelta EQ 0 THEN BEGIN  ; paused
     WIDGET_CONTROL, state.wPauseButton, SET_VALUE = blk_pausebutton
     state.currentAction = state.wPauseButton
     state.currentBitmap = pausebutton
  ENDIF ELSE BEGIN
     IF state.framedelta GT 0 THEN BEGIN  ; animating forward
        IF state.cycle THEN BEGIN
           WIDGET_CONTROL, state.wCyclePlayButton, SET_VALUE = blk_cycleForwardBtn
           state.currentAction = state.wCyclePlayButton
           state.currentBitmap = cycleForwardBtn
        ENDIF ELSE BEGIN
           WIDGET_CONTROL, state.wPlayButton, SET_VALUE = blk_playbutton
           state.currentAction = state.wPlayButton
           state.currentBitmap = playbutton
        ENDELSE
     ENDIF ELSE BEGIN                       ; animating backwards
        WIDGET_CONTROL, state.wReversePlayButton, SET_VALUE = blk_reversebutton
        state.currentAction = state.wReversePlayButton
        state.currentBitmap = reversebutton
     ENDELSE
  ENDELSE
END


PRO CW_ANIMATE_CLN, widget
; When the widget dies, clean up here. Widget is the ID of the
; *child* actually holding the state in its UVALUE.

     ; kills state stored in widget
   WIDGET_CONTROL, widget, GET_UVALUE = state, /NO_COPY

   IF (N_ELEMENTS(state) GT 0) THEN BEGIN
      IF (state.dont_kill_pixmaps EQ 0) THEN BEGIN
         HANDLE_VALUE, state.pwinHdl, pwin
         FOR i=0, N_ELEMENTS(pwin)-1 DO BEGIN
            IF (pwin(i) GE 0) THEN $
               WDELETE, pwin(i)
         ENDFOR
         HANDLE_FREE, state.pwinHdl
      ENDIF
        ; Restore the state
      WIDGET_CONTROL, widget, SET_UVALUE = state, /NO_COPY
   ENDIF
END





FUNCTION CW_ANIMATE_EV, event

     ; Retrieve the structure from the child that contains the sub ids
   wAnimateBase = event.handler
   wTopBase = WIDGET_INFO(wAnimateBase, /CHILD)
    ;This kills the old uvalue
   WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY
   ret = 0

   CASE event.id OF
      wAnimateBase: $
         IF (state.framedelta NE 0) THEN BEGIN            ; Animation
            WIDGET_CONTROL, wAnimateBase, TIMER=state.delay
            curframe = state.curframe
            nframes = state.nframes

            curframe = curframe + state.framedelta            ; New frame

            r = 0.0
            IF state.cycle THEN BEGIN
               IF curframe LT 0 THEN BEGIN
                  state.framedelta = 1
                  curframe = 0
                  t = systime(1)
                  r = 2 * nframes / float(t-state.loop_start_t)
               ENDIF
               IF curframe GE nframes THEN BEGIN
                  state.framedelta = -1
                  curframe = nframes-1
               ENDIF
            ENDIF ELSE BEGIN
               WHILE curframe LT 0 DO $
                  curframe = curframe + nframes ; Into range
               WHILE curframe GE nframes DO $
                  curframe = curframe - nframes
               IF curframe EQ 0 THEN BEGIN                  ; Display rate?
                  t = systime(1)
                  r = nframes / FLOAT(t-state.loop_start_t)  ; Rate in Frames/Sec
               ENDIF
            ENDELSE

            state.curframe = curframe
            IF r NE 0.0 THEN BEGIN            ;Update time?
               WIDGET_CONTROL, state.wFramesPerSecValue, SET_VALUE = $
                  STRING(r, FORMAT='(f6.1)')
               state.loop_start_t = t
            ENDIF
            WSET, state.draw_win                  ;Set to the drawing window
            HANDLE_VALUE, state.pwinHdl, pwin, /NO_COPY
            IF pwin(curframe) GE 0 THEN $            ;Next frame
               DEVICE, COPY =[0, 0, state.sizex, state.sizey, 0, 0, pwin(curframe)]
            IF state.track THEN $
               WIDGET_CONTROL, state.wFramesIndicatorSlider, SET_VALUE = curframe
            HANDLE_VALUE, state.pwinHdl, pwin, /SET, /NO_COPY
            EMPTY
         ENDIF

      state.wFramesSpeedSlider : BEGIN            ;New rate
         WIDGET_CONTROL, state.wFramesSpeedSlider, GET_VALUE = temp
         IF temp EQ 100 THEN $
            state.delay=0. $
         ELSE $
            state.delay= 2./(1.+temp)
         END

      state.wFramesIndicatorSlider : BEGIN
         WIDGET_CONTROL, state.wFramesIndicatorSlider, GET_VALUE = temp
         IF (temp NE state.curframe) THEN BEGIN
            WSET, state.draw_win
            state.curframe = temp
            HANDLE_VALUE, state.pwinHdl, pwin, /NO_COPY
            IF (pwin(temp) GE 0) THEN $
               DEVICE, COPY = [0, 0, state.sizex, state.sizey, 0, 0, pwin(temp)]
            HANDLE_VALUE, state.pwinHdl, pwin, /SET, /NO_COPY
            EMPTY
         ENDIF
         END

      state.wPauseButton : $
         IF (state.framedelta NE 0) THEN BEGIN
            WIDGET_CONTROL, state.wFramesIndicatorSlider, SET_VALUE = state.curframe
            WIDGET_CONTROL, state.wFramesIndicatorSlider, SENSITIVE = 1
            WIDGET_CONTROL, state.wFramesPerSecValue, SET_VALUE = $
                  STRING(0.0, FORMAT='(f6.1)')
            state.framedelta = 0
            SetBitmapButtons, state
         ENDIF

      state.wPlayButton : BEGIN
         WIDGET_CONTROL, state.wFramesIndicatorSlider, SENSITIVE = 0
         WIDGET_CONTROL, state.wFramesSpeedSlider, SENSITIVE = 1
         IF (state.framedelta EQ 0) THEN $
            WIDGET_CONTROL, wAnimateBase, TIMER=state.delay
         state.framedelta = 1
         state.cycle = 0
         SetBitmapButtons, state
         END

      state.wReversePlayButton : BEGIN
         WIDGET_CONTROL, state.wFramesIndicatorSlider, SENSITIVE = 0
         WIDGET_CONTROL, state.wFramesSpeedSlider, SENSITIVE = 1
         IF (state.framedelta EQ 0) THEN $
            WIDGET_CONTROL, wAnimateBase, TIMER=state.delay
         state.framedelta = -1
         state.cycle = 0
         SetBitmapButtons, state
         END

      state.wCyclePlayButton : BEGIN
         WIDGET_CONTROL, state.wFramesIndicatorSlider, SENSITIVE = 0
         WIDGET_CONTROL, state.wFramesSpeedSlider, SENSITIVE = 1
         IF (state.framedelta EQ 0) THEN $
            WIDGET_CONTROL, wAnimateBase, TIMER=state.delay
         state.framedelta = 1
         state.cycle = 1
         SetBitmapButtons, state
         END

      state.wActiveSliderCheck: BEGIN
         state.track = event.select
         END

      state.wHelpButton : $
         XDISPLAYFILE, "animatedemo.hlp", TITLE = "Animation Help", $
            GROUP = event.top, WIDTH = 55, HEIGHT = 16, $
            TEXT = [ $
               "                                                       ", $
               "      The animation widget is used for displaying", $
               "a series of images created with IDL as an animation.  The", $
               "user can select the speed, direction or specific frames in", $
               "the animation.", $
               "      The top slider is used to control the speed of", $
               "the animation.  Moving it to the far right is one hundred", $
               "percent, as fast as the animation can go.  If there are", $
               "other IDL widget applications using background tasks,", $
               "they can slow down the animation.  Closing the other", $
               "applications can speed up the animation.", $
               "      The four bitmap buttons are reverse play, pause, ", $
               "forward play and cycle.  Use them to select a direction or to", $
               "pause the animation and view specific framestate.", $
               "      The bottom slider is used to view single frames", $
               "from the animation.  The animation must be paused to ", $
               "use the frame selection slider."   ]

      state.wColorsButton : $
         XLOADCT, GROUP = event.top

      state.wOpenButton : BEGIN
           ; get a copy before the structure is killed
         open_func = state.open_func
         framedelta = state.framedelta
         wFramesSpeedSlider = state.wFramesSpeedSlider
           ; Need to restore state since the following routines use it
         WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY
         CW_ANIMATE_RUN, wAnimateBase, /STOP
           ; Disable all controls until all frames are loaded
         WIDGET_CONTROL, wAnimateBase, SENSITIVE = 0
         fileOK = CALL_FUNCTION(open_func, event.top, wAnimateBase)

         IF fileOK THEN BEGIN
            WIDGET_CONTROL, wFramesSpeedSlider, GET_VALUE = rate
            IF framedelta EQ 0 THEN $
               framedelta = 1
            CW_ANIMATE_RUN, wAnimateBase, rate, DELTA=framedelta, /LASTFRAME
         ENDIF ELSE $
              ; Disable all controls until all frames are loaded
            WIDGET_CONTROL, wAnimateBase, SENSITIVE = 1


           ; Need structure back - This kills the old uvalue
         WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY
         END

      state.wEndAnimationButton: $
         ret = {ID:wAnimateBase, TOP:event.top, HANDLER:0L, action:"DONE" }

      ELSE:
      ENDCASE

   WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY   ; Restore the state
   RETURN, ret

END





pro CW_ANIMATE_LOAD, widget, IMAGE = image, FRAME = frame, ORDER = order, $
      WINDOW = window, XOFFSET = xoffset, YOFFSET = yoffset, $
      TRACK = track, CYCLE = cycle


   wTopBase = WIDGET_INFO(widget, /CHILD)
   WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY
   HANDLE_VALUE, state.pwinHdl, pwin, /NO_COPY

   old_window = !D.WINDOW

   displayload = WIDGET_INFO(widget, /REALIZED)
   IF (displayload NE 0) THEN BEGIN
      WIDGET_CONTROL, GET_VALUE=temp, state.wImageArea
      state.draw_win = temp
      WSET, state.draw_win
      ; In case the draw widget size is different than that requested.
      state.sizex = !D.X_VSIZE
      state.sizey = !D.Y_VSIZE
   ENDIF

   ; Default values and range checking
   IF (N_ELEMENTS(yoffset) EQ 0) THEN $
      yoffset = 0
   IF (N_ELEMENTS(xoffset) EQ 0) THEN $
      xoffset = 0
   IF (N_ELEMENTS(frame)) GT 0 THEN BEGIN
      IF (frame LT 0) OR (frame GE N_ELEMENTS(pwin)) THEN $
         MESSAGE, "Frame number must be from 0 to nframes -1."
   ENDIF ELSE $
      frame=0


   j = N_ELEMENTS(window)            ;check to see if WINDOW was set

   IF (j GT 0) THEN BEGIN            ;Copy image from window?
      IF (j LT 5) THEN BEGIN            ;If coords not spec, use all
         WSET, window(0)
         p = [ window(0), 0, 0, !D.X_VSIZE, !D.Y_VSIZE ]  ;Get size of window
      ENDIF ELSE $
         p = window

      IF pwin(frame) LT 0 THEN BEGIN            ;Create pixwin?
         WINDOW, /FREE, XSIZE = state.sizex, YSIZE = state.sizey, /PIXMAP
         pwin(frame) = !D.WINDOW
      ENDIF

      IF (p(3) GT state.sizex) OR (p(4) GT state.sizey) THEN $
         MESSAGE, "Window parameter larger than setup"

      IF displayload THEN BEGIN            ;Load display window
         WSET, state.draw_win                        ;Show it?
         IF state.draw_win NE p(0) THEN $            ;Copy to show window?
            DEVICE, COPY = [ p(1), p(2), p(3), p(4), xoffset, yoffset, p(0)]
         WSET, pwin(frame)                  ;Pixmap destination
            ;Copy from display window to pixmap
         DEVICE, COPY = [ xoffset, yoffset, p(3), p(4), xoffset, yoffset, $
            state.draw_win ]
      ENDIF ELSE BEGIN                  ;load / no show
         WSET, pwin(frame)
         DEVICE, COPY = [ p(1), p(2), p(3), p(4), xoffset, yoffset, p(0)]
      ENDELSE

      EMPTY
      IF (N_ELEMENTS(state.draw_win) EQ 0) THEN $
         state.draw_win = -1

      IF (old_window GE 0) THEN $
         WSET, old_window

       ; When displayload is set, the frame slider should update to show frame num
      IF displayload THEN $
         WIDGET_CONTROL, state.wFramesIndicatorSlider, SET_VALUE = frame
      GOTO, Done
   ENDIF                                    ;So WINDOW was not set.

   IF N_ELEMENTS(image) NE 0  THEN BEGIN            ;Make sure image was set.

       ; When displayload is set, the draw widget should be updated
       ; to show the new frame being loaded and the frame slider
       ; should be set correspondingly
      IF (displayload NE 0) THEN BEGIN
         WIDGET_CONTROL, state.wFramesIndicatorSlider, SET_VALUE = frame
         WSET, state.draw_win
         TV, image
         EMPTY
      ENDIF

       ;Make sure the image is of a valid size and report if not.
      sz = SIZE(image)
      IF ((sz(0) NE 2) OR (sz(1) GT state.sizex) OR (sz(2) GT state.sizey)) THEN $
         MESSAGE, "Image parameter must be 2D of size" + $
                  STRING(state.sizex)+ STRING(state.sizey)

      IF N_ELEMENTS(order) EQ 0 THEN $
         ORDER = 0        ;Default order
      IF pwin(frame) LT 0 THEN BEGIN
         WINDOW, /FREE, xsize = state.sizex, ysize = state.sizey, /pixmap
         pwin(frame) = !D.WINDOW
      ENDIF ELSE $
         WSET, pwin(frame)
      TV, image, xoffset, yoffset, ORDER = order
      EMPTY
      IF (old_window GE 0) THEN $
         WSET, old_window
      GOTO, Done
   ENDIF                                    ;End of "if IMAGE was set".

  Done: HANDLE_VALUE, state.pwinHdl, pwin, /SET, /NO_COPY

   WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY  ;Restore uvalue

END ;CW_ANIMATE_LOAD




pro CW_ANIMATE_RUN, widget, rate, STOP = stop, NFRAMES = nframes, $
                       DELTA = delta, LASTFRAME=lastFrame

   wTopBase = WIDGET_INFO(widget, /CHILD)
   WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY

   old_window = !D.WINDOW                        ;Save old window

   ; Refuse to run if the cluster isn't realized.
   IF (WIDGET_INFO(widget, /REALIZED) EQ 0) THEN $
      MESSAGE,'Animation widget must be realized before it can run'

   IF KEYWORD_SET(stop) THEN BEGIN           ;Stop the animation
      state.framedelta = 0                ;This shows we've stopped.
      WIDGET_CONTROL, state.wFramesIndicatorSlider, SET_VALUE = state.curframe
      WIDGET_CONTROL, state.wFramesIndicatorSlider, SENSITIVE = 1
      SetBitmapButtons, state
      GOTO, done
   ENDIF

   ; It is realized now, so get the draw widget window ID.
   WIDGET_CONTROL, GET_VALUE=temp, state.wImageArea
   state.draw_win = temp
   WSET, temp
   EMPTY

   WIDGET_CONTROL, widget, /SENSITIVE
   WIDGET_CONTROL, state.wFramesIndicatorSlider, SENSITIVE = 0
   IF N_ELEMENTS(nframes) GT 0 THEN BEGIN        ;Nframes spec?
      HANDLE_VALUE, state.pwinHdl, pwin, /NO_COPY
      IF nframes GT N_ELEMENTS(pwin) THEN $
         MESSAGE, 'Run called with too many frames'
      HANDLE_VALUE, state.pwinHdl, pwin, /SET, /NO_COPY
      state.nframes = nframes
   ENDIF


   ;Set up the initial values used by the background task
   IF N_ELEMENTS(lastFrame) NE 0 THEN $
      state.curframe = state.nframes $
   ELSE $
      state.curframe = 0

   IF N_ELEMENTS(delta) NE 0 THEN $
      state.framedelta = -1 > delta < 1 $           ; In range?
   ELSE $
      state.framedelta = 1

   IF N_ELEMENTS(rate) NE 0 THEN BEGIN
      rate = 0 > rate < 100            ; In range?
      WIDGET_CONTROL, state.wFramesSpeedSlider, SET_VALUE = rate
   ENDIF ELSE $
      rate = 100

   IF rate EQ 100 THEN $
      state.delay=0.0 $
   ELSE $
      state.delay = 2./(1.+rate)

   SetBitmapButtons, state

   state.loop_start_t = SYSTIME(1)            ;Start of loop time
   WIDGET_CONTROL, widget, TIMER=state.delay, EVENT_FUNC = 'CW_ANIMATE_EV'
 done:
   WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY      ;Rewrite state
END


pro CW_ANIMATE_GETP, widget, PIXMAPS, KILL_ANYWAY = kill_anyway
; Return the vector of pixmap ID's associated with the animation
; widget in named variable PIXMAPSTATE. Frames without a pixmap contain a -1.
; This routine should not be called until all the frames are loaded, or the
; vector will not be complete. It should be called before the call to
; CW_ANIMATE_RUN.
;
; Note: Normally, the animation widget destroys its pixmaps when it
;      is destroyed. If this routine is called however, the pixmaps
;      are not destroyed. Cleanup becomes the responsibility of the
;      caller.
   wTopBase = WIDGET_INFO(widget, /CHILD)
   WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY
   HANDLE_VALUE, state.pwinHdl, pwin, /NO_COPY
   pixmaps = pwin
   HANDLE_VALUE, state.pwinHdl, pwin, /SET, /NO_COPY
   IF KEYWORD_SET(kill_anyway) EQ 0 THEN $
      state.dont_kill_pixmaps = 1
   WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY
END


PRO CW_ANIMATE_INIT, wAnimateBase, sizex, sizey, nframes, PIXMAPS=old_pixmaps

   ON_ERROR, 2                                    ;return to caller

   wTopBase = WIDGET_INFO(wAnimateBase, /CHILD)
   WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY

   nparams = N_PARAMS()
   IF (nparams LT 3) OR (nparams GT 4) THEN $
      MESSAGE, 'Incorrect number of arguments'
   IF NOT (KEYWORD_SET(uval)) THEN $
      uval = 0

   n = N_ELEMENTS(old_pixmaps)
   IF (n GT 0) THEN BEGIN
      nframes = n
      pwin = old_pixmaps
   ENDIF ELSE $
      pwin = REPLICATE(-1, nframes)            ;Array of window indices

   IF (nframes LE 1) THEN $
      MESSAGE, "Animations must have 2 or more frames"

       ; save the number of frames to animate in the animation structure
   state.nframes = nframes
       ; save the Pixmap array to animate in the animation structure
   IF HANDLE_INFO(state.pwinHdl) THEN BEGIN
       ; Need to temporarily restore state since the following routine uses it
      WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY
      CW_ANIMATE_CLN, wTopBase
       ; Need structure back - This kills the widget uvalue
      WIDGET_CONTROL, wTopBase, GET_UVALUE = state, /NO_COPY
   ENDIF

   state.pwinHdl = HANDLE_CREATE()
   HANDLE_VALUE, state.pwinHdl, pwin, /SET, /NO_COPY

   WIDGET_CONTROL, state.wFramesIndicatorSlider, SET_SLIDER_MAX = nframes - 1

   IF state.wImageArea NE 0 THEN BEGIN
        ; to avoid flash - only set the size if it changes
      IF (state.sizex NE sizex OR state.sizey NE sizey) THEN $
         WIDGET_CONTROL, state.wImageArea, XSIZE =sizex, YSIZE=sizey
   ENDIF ELSE BEGIN
      wImageBase = WIDGET_BASE(wTopBase, /COLUMN)      ;To prevent stretching
      state.wImageArea = WIDGET_DRAW(wImageBase, XSIZE =sizex, YSIZE=sizey, $
                         XOFFSET = 280, YOFFSET = 20, RETAIN = 2)
   ENDELSE

       ; save the x dimensions of draw widget in the animation structure
   state.sizex = sizex
       ; save the y dimensions of draw widget in the animation structure
   state.sizey = sizey

       ; Disable all controls until all frames are loaded
   WIDGET_CONTROL, wAnimateBase, SENSITIVE = 0

   WIDGET_CONTROL, wTopBase, SET_UVALUE = state, /NO_COPY

END


; Setup the play reverse, pause, play forward and cycle pushbutton bitmaps. These
; variables reside in the "BitmapButtons" common block.
; Both a depressed (blk_) and a not-depressed version are needed for each button.
PRO InitBitmapButtons

  COMMON BitmapButtons

   reversebutton = [[000B, 000B, 000B], [000B, 032B, 000B], [000B, 048B, 000B],$
		[000B, 056B, 000B], [000B, 060B, 000B], [000B, 046B, 000B], $
		[000B, 231B, 015B], [144B, 003B, 024B], [016B, 231B, 027B], $
		[080B, 238B, 027B], [208B, 060B, 026B], [208B, 056B, 026B], $
		[208B, 048B, 026B], [208B, 032B, 026B], [208B, 000B, 026B], $
		[208B, 000B, 026B], [208B, 255B, 027B], [016B, 000B, 024B], $
		[240B, 255B, 031B], [224B, 255B, 015B], [000B, 000B, 000B], $
		[000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B] ]
   blk_reversebutton = [[255B, 255B, 255B], [255B, 223B, 255B],[255B, 207B, 255B],$
		[255B, 199B, 255B], [255B, 195B, 255B], [255B, 209B, 255B], $
		[255B, 024B, 240B], [111B, 252B, 231B], [239B, 024B, 228B], $
		[175B, 017B, 228B], [047B, 195B, 229B], [047B, 199B, 229B], $
		[047B, 207B, 229B], [047B, 223B, 229B], [047B, 255B, 229B], $
		[047B, 255B, 229B], [047B, 000B, 228B], [239B, 255B, 231B], $
		[015B, 000B, 224B], [031B, 000B, 240B], [255B, 255B, 255B], $
		[255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B] ]
   pausebutton = [[000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B], $
		[192B, 195B, 003B], [192B, 194B, 002B], [192B, 194B, 002B], $
		[192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
		[192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
		[192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
		[192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
		[192B, 194B, 002B], [192B, 195B, 003B], [192B, 195B, 003B], $
		[000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B] ]
   blk_pausebutton = [[255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B],$
		[063B, 060B, 252B], [063B, 061B, 253B], [063B, 061B, 253B], $
		[063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
		[063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
		[063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
		[063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
		[063B, 061B, 253B], [063B, 060B, 252B], [063B, 060B, 252B], $
		[255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B] ]
   playbutton = [[000B, 000B, 000B], [000B, 004B, 000B], [000B, 012B, 000B], $
		[000B, 028B, 000B], [000B, 060B, 000B], [000B, 116B, 000B], $
		[240B, 231B, 000B], [024B, 192B, 009B], [216B, 231B, 008B], $
		[216B, 119B, 010B], [088B, 060B, 011B], [088B, 028B, 011B], $
		[088B, 012B, 011B], [088B, 004B, 011B], [088B, 000B, 011B], $
		[088B, 000B, 011B], [216B, 255B, 011B], [024B, 000B, 008B], $
		[248B, 255B, 015B], [240B, 255B, 007B], [000B, 000B, 000B], $
		[000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B] ]
   blk_playbutton = [[255B, 255B, 255B], [255B, 251B, 255B], [255B, 243B, 255B],$
		[255B, 227B, 255B], [255B, 195B, 255B], [255B, 139B, 255B], $
		[015B, 024B, 255B], [231B, 063B, 246B], [039B, 024B, 247B], $
		[039B, 136B, 245B], [167B, 195B, 244B], [167B, 227B, 244B], $
		[167B, 243B, 244B], [167B, 251B, 244B], [167B, 255B, 244B], $
		[167B, 255B, 244B], [039B, 000B, 244B], [231B, 255B, 247B], $
		[007B, 000B, 240B], [015B, 000B, 248B], [255B, 255B, 255B], $
		[255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B] ]
  cycleForwardBtn = [[000B, 000B, 000B], [000B, 000B, 000B], [000B, 128B, 000B], $
		[000B, 128B, 001B], [000B, 128B, 003B], [248B, 255B, 006B], $
		[008B, 000B, 012B], [008B, 000B, 024B], [248B, 255B, 012B], $
		[248B, 255B, 006B], [000B, 128B, 003B], [000B, 129B, 001B], $
		[128B, 129B, 000B], [192B, 001B, 000B], [096B, 255B, 015B], $
		[048B, 000B, 008B], [024B, 000B, 008B], [048B, 255B, 015B], $
		[096B, 255B, 015B], [192B, 001B, 000B], [128B, 001B, 000B], $
		[000B, 001B, 000B], [000B, 000B, 000B], [000B, 000B, 000B] ]
   blk_cycleForwardBtn = [[255B, 255B, 255B], [255B, 255B, 255B], [255B, 127B, 255B], $
		[255B, 127B, 254B], [255B, 127B, 252B], [007B, 000B, 249B], $
		[247B, 255B, 243B], [247B, 255B, 231B], [007B, 000B, 243B], $
		[007B, 000B, 249B], [255B, 127B, 252B], [255B, 126B, 254B], $
		[127B, 126B, 255B], [063B, 254B, 255B], [159B, 000B, 240B], $
		[207B, 255B, 247B], [231B, 255B, 247B], [207B, 000B, 240B], $
		[159B, 000B, 240B], [063B, 254B, 255B], [127B, 254B, 255B], $
		[255B, 254B, 255B], [255B, 255B, 255B], [255B, 255B, 255B] ]
END


function CW_ANIMATE, parent, sizex, sizey, nframes, UVALUE=uval, $
    PIXMAPS=old_pixmaps, TRACK = track, CYCLE=cycle, DRAW = draw, $
    NO_KILL = no_kill, OPEN_FUNC=open_func

  COMMON BitmapButtons

   ON_ERROR, 2                                    ;return to caller

      ; Set the bitmaps for the bitmap buttons
   InitBitmapButtons

   nparams = N_PARAMS()
   IF (nparams LT 3) OR (nparams GT 4) THEN $
      MESSAGE, 'Incorrect number of arguments'
   IF NOT (KEYWORD_SET(uval))  THEN $
      uval = 0
   IF NOT (KEYWORD_SET(open_func)) THEN $
      open_func = 0

   wAnimateBase = WIDGET_BASE(parent, /COLUMN)
   wTopBase = WIDGET_BASE(wAnimateBase, /ROW)
   wControlBase = WIDGET_BASE(wTopBase, /COLUMN, /FRAME, XPAD=10, YPAD=10, SPACE=20)
   wVCRButtonBase = WIDGET_BASE(wControlBase, /ROW)
   wReversePlayButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = reversebutton)
   wPauseButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = blk_pausebutton)
   wPlayButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = playbutton)
   wCyclePlayButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = cycleForwardBtn)
   currentAction = wPauseButton
   currentBitmap = pausebutton

   wSpeedBase = WIDGET_BASE(wControlBase, /COLUMN)
   wSpeedBaseLabel = WIDGET_LABEL(wSpeedBase, VALUE = "Animation Speed:", /ALIGN_LEFT)
   wFramesSpeedBase = WIDGET_BASE(wSpeedBase, TITLE = "Animation Speed", /COLUMN, /FRAME)
   wFramesPerSecBase = WIDGET_BASE(wFramesSpeedBase, /ROW)
   wFramesPerSecLabel = WIDGET_LABEL(wFramesPerSecBase, VALUE = "Frames/Sec:")
   wFramesPerSecValue = WIDGET_LABEL(wFramesPerSecBase, VALUE = '0.000')
   wFramesSpeedSlider = WIDGET_SLIDER(wFramesSpeedBase, /DRAG, VALUE = 100, $
             MAXIMUM = 100, MINIMUM = 0, /SUPPRESS_VALUE)

   wFrameBase = WIDGET_BASE(wControlBase, /COLUMN)
   wFrameBaseLabel = WIDGET_LABEL(wFrameBase, VALUE = "Animation Frame:", /ALIGN_LEFT)
   wFrameIndicatorBase = WIDGET_BASE(wFrameBase, TITLE = "Animation Frame", /COLUMN, /FRAME)
   wFramesIndicatorSlider = WIDGET_SLIDER(wFrameIndicatorBase, /DRAG, VALUE = 0, $
             MAXIMUM = nframes - 1, MINIMUM = 0)
   wActiveSliderCheck = CW_BGROUP(wFrameIndicatorBase, ['Active Slider'], $
           FRAME = 0, /NONEXCLUSIVE, /RETURN_INDEX, $
           SET_VALUE=KEYWORD_SET(track))

   wButtonBase = WIDGET_BASE(wAnimateBase, /ROW, /ALIGN_LEFT)
   IF KEYWORD_SET(no_kill) THEN $
      wEndAnimationButton = 0L $
   ELSE $
      wEndAnimationButton = WIDGET_BUTTON(wButtonBase, VALUE='End Animation')
   wColorsButton = WIDGET_BUTTON(wButtonBase, VALUE='Colors...')
   IF (KEYWORD_SET(open_func)) THEN $
      wOpenButton = WIDGET_BUTTON(wButtonBase, VALUE='Open...') $
   ELSE $
      wOpenButton = 0
   wHelpButton = WIDGET_BUTTON(wButtonBase, VALUE='Help')

   IF N_ELEMENTS(draw) EQ 1 THEN $
      wImageArea = draw $
   ELSE $
      wImageArea = 0

   ; Set the event handler function. This cluster does not get or set a value
   ; Make sure it lingers so the cleanup routine can get at its state.
   WIDGET_CONTROL, wAnimateBase, SET_UVALUE = uval, EVENT_FUNC = 'CW_ANIMATE_EV', $
       /DELAY_DESTROY

   ;pwin = REPLICATE(-1, nframes)            ;Array of window indices

   ; This structure gets stuffed into the uval. of the first child
   ; of wAnimateBase
   WIDGET_CONTROL, wTopBase, SET_UVALUE = $
      { wEndAnimationButton: wEndAnimationButton, $      ; End button
        wColorsButton: wColorsButton, $                  ; Adjust color palette button
        wOpenButton: wOpenButton, $                       ; Open file button
        open_func: open_func, $                          ; Open file function
        wHelpButton: wHelpButton, $                      ; Help button
        wActiveSliderCheck: wActiveSliderCheck, $        ; button group widget
        wFramesSpeedSlider: wFramesSpeedSlider, $        ; Speed selection slider
        wReversePlayButton: wReversePlayButton, $        ; Reverse button
        wPauseButton: wPauseButton, $                    ; Stop (pause) button
        wPlayButton: wPlayButton, $                      ; Forward button
        wCyclePlayButton: wCyclePlayButton, $            ; Cycle forward button
        currentAction : currentAction, $                 ; current action button id
        currentBitmap : currentBitmap, $                 ; current button bitmap
        wFramesIndicatorSlider: wFramesIndicatorSlider, $; Frame selection slider
        wFramesPerSecValue: wFramesPerSecValue, $        ; Animation rate display
        wImageArea: wImageArea, $                        ; Draw widget for animation
        draw_win: -1, $                                  ; Window # of draw widget
        sizex: 0, sizey: 0, $                            ; Dimensions of draw widget
        nframes: 0, $                                    ; # of frames in animation
        curframe: 0, $
        cycle: KEYWORD_SET(cycle), $                     ; Ne 0 to cycle
        track: KEYWORD_SET(track), $                     ; Ne 0 to track with slider
        framedelta: 0, $                                 ; # frames to step.
        delay: 0.0D, $                                   ; Delay between frames
        loop_start_t: 0.0D, $                            ; System time at start
        dont_kill_pixmaps: 0, $                          ; TRUE if pixmaps preserved on kill
        pwinHdl: 0L }                                    ; handle to the Pixmap array

  ; When the child holding the state gets killed, have a cleanup
  ; procedure called to mop up
   WIDGET_CONTROL, wTopBase, KILL_NOTIFY = 'CW_ANIMATE_CLN'

   CW_ANIMATE_INIT, wAnimateBase, sizex, sizey, nframes, PIXMAPS=old_pixmaps

   RETURN, wAnimateBase

END
