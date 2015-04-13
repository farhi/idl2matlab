; $Id: cw_rgbslider.pro,v 1.5 1994/05/13 00:14:54 dave Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_RGBSLIDER
;
; PURPOSE:
;	CW_RGBSLIDER is a compund widget that provides three sliders for
;	adjusting color values. The RGB, CMY, HSV, and HLS color systems
;	can all be used. No matter which color system is in use,
;	the resulting color is always supplied in RGB, which is the
;	base system for IDL.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	Widget = CW_RGBSLIDER(Parent)
;
; INPUTS:
;       Parent:	  The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	CMY:	  If set, the initial color system used is CMY.
;	DRAG:	  Set to zero if events should only be generated when
;		  the mouse button is released. If this keyword is set,
;		  events will be generated continuously when the sliders
;		  are adjusted. Note: On slow systems, /DRAG performance
;		  can be inadequate. The default is DRAG=0.
;	FRAME:	  If set, a frame will be drawn around the widget. The
;		  default is FRAME=0 (no frame drawn).
;	HSV:	  If set, the initial color system used is HSV.
;	HLS:	  If set, the initial color system used is HLS.
;	LENGTH:	  The length of the sliders. The default = 256.
;	RGB:	  If set, the initial color system used is RGB.
;		  This is the default.
;	UVALUE:	  The user value for the widget.
;	VERTICAL: If set, the sliders will be oriented vertically.
;		  The default is VERTICAL=0 (horizontal sliders).
;	COLOR_INDEX: If set, display a small rectangle with the
;		  selected color, using the given index.
;		  The color is updated as the values are changed.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	This widget generates event structures containing a three fields
;	named 'R', 'G', and 'B' containing the Red, Green, and Blue
;	components of the selected color.
;
; PROCEDURE:
;	The CW_RGBSLIDER widget has the following controls:
;
;	Color System Selection: A pulldown menu which allows the user
;		to change between the supported color systems.
;
;	Color adjustment sliders: Allow the user to select a new color
;		value. 
;
;	By adjusting these controls, the user selects color values which
;	are reported via the widget event mechanism.
;
; MODIFICATION HISTORY:
;	April 1, 1992, AB
;		Based on the RGB code from XPALETTE.PRO, but extended to
;		support color systems other than RGB.
;       7 April 1993, AB, Removed state caching.
;	10 May 1994, DMS, Added Color_index param to display color.
;-

pro CW_RGB_CHNG_CS, state, base_idx
  ; Change the current color system.
  ; State is the state structure to use (READ-ONLY)
  ; base_idx is the index into state.select_b of the base to change to.

  case (base_idx) of
    0: begin		; RGB
	WIDGET_CONTROL, state.r_sl, set_value=state.r_v
	WIDGET_CONTROL, state.g_sl, set_value=state.g_v
	WIDGET_CONTROL, state.b_sl, set_value=state.b_v
	end
    1: begin		; CMY
	WIDGET_CONTROL, state.c_sl, set_value=255-state.r_v
	WIDGET_CONTROL, state.m_sl, set_value=255-state.g_v
	WIDGET_CONTROL, state.y_sl, set_value=255-state.b_v
        end
    2: begin		; HSV
	COLOR_CONVERT, state.r_v, state.g_v, state.b_v, a, b, c, /RGB_HSV
	WIDGET_CONTROL, state.hsv_h_sl, set_value=(state.h_v = a)
	WIDGET_CONTROL, state.hsv_s_sl, set_value=(state.f1_v = b)
	WIDGET_CONTROL, state.hsv_v_sl, set_value=(state.f2_v = c)
	end
    3: begin		; HLS
	COLOR_CONVERT, state.r_v, state.g_v, state.b_v, a, b, c, /RGB_HLS
	WIDGET_CONTROL, state.hls_h_sl, set_value=(state.h_v = a)
	WIDGET_CONTROL, state.hls_l_sl, set_value=(state.f1_v = b)
	WIDGET_CONTROL, state.hls_s_sl, set_value=(state.f2_v = c)
	end

  endcase

  ; Change the displayed slider base
  WIDGET_CONTROL, map=0, state.select_b(state.curr_b_idx)
  WIDGET_CONTROL, map=1, state.select_b(base_idx)
  state.curr_b_idx = base_idx
end



PRO CW_RGB_SET_DRAW, state
if state.color_window lt 0 then begin	;Initialize?
	WIDGET_CONTROL, state.color_draw, GET_VALUE=i
	WSET, i
	state.color_window = i
	ERASE, state.color_index		;Set to our value
	endif
TVLCT, state.r_v, state.g_v, state.b_v, state.color_index
end




function CW_RGB_EVENT, ev

  ; Recover the state of this compound widget
  base = ev.handler
  stash = WIDGET_INFO(base, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  case (ev.id) of
    state.select_s : begin
        if (state.curr_b_idx ne (ev.value-1)) then $
		CW_RGB_CHNG_CS,state,ev.value-1
	WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
        return, 0		; Swallow the event
      end

    state.r_sl: state.r_v = ev.value
    state.g_sl: state.g_v = ev.value
    state.b_sl: state.b_v = ev.value

    state.c_sl: state.r_v = 255 - ev.value
    state.m_sl: state.g_v = 255 - ev.value
    state.y_sl: state.b_v = 255 - ev.value

    state.hsv_h_sl: begin
	state.h_v = ev.value
	goto, calc_new_hsv
	end
    state.hsv_s_sl: begin
	state.f1_v = ev.value
	goto, calc_new_hsv
	end
    state.hsv_v_sl: begin
	state.f2_v = ev.value
      calc_new_hsv:
	COLOR_CONVERT, state.h_v, state.f1_v, state.f2_v, a, b, c, /HSV_RGB
	state.r_v = a
        state.g_v = b
	state.b_v = c
	end
    

    state.hls_h_sl: begin
	state.h_v = ev.value
	goto, calc_new_hls
	end
    state.hls_l_sl: begin
	state.f1_v = ev.value
	goto, calc_new_hls
	end
    state.hls_s_sl: begin
	state.f2_v = ev.value
      calc_new_hls:
	COLOR_CONVERT, state.h_v, state.f1_v, state.f2_v, a, b, c, /HLS_RGB
	state.r_v = a
        state.g_v = b
	state.b_v = c
	end
    
  endcase

  if state.color_index ge 0 then CW_RGB_SET_DRAW, state

  ; Return an RGB event
  ret = { RGB_EVENT, ID: base, TOP:ev.top, HANDLER:0L, $
            R:state.r_v, G:state.g_v, B:state.b_v }
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  return, ret

end







pro CW_RGB_SET_VAL, id, value

  ; Recover the state of this compound widget
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  state.r_v = value(0)
  state.g_v = value(1)
  state.b_v = value(2)

  case (state.curr_b_idx) of
    0: begin	; RGB
	WIDGET_CONTROL, state.r_sl, set_value=state.r_v
	WIDGET_CONTROL, state.g_sl, set_value=state.g_v
	WIDGET_CONTROL, state.b_sl, set_value=state.b_v
	end
    1: begin	; CMY
	WIDGET_CONTROL, state.c_sl, set_value=255-state.r_v
	WIDGET_CONTROL, state.m_sl, set_value=255-state.g_v
	WIDGET_CONTROL, state.y_sl, set_value=255-state.b_v
        end
    2: begin	; HSV
	COLOR_CONVERT, state.r_v, state.g_v, state.b_v, a, b, c, /RGB_HSV
	WIDGET_CONTROL, state.hsv_h_sl, set_value=(state.h_v = a)
	WIDGET_CONTROL, state.hsv_s_sl, set_value=(state.f1_v = b)
	WIDGET_CONTROL, state.hsv_v_sl, set_value=(state.f2_v = c)
	end
    3: begin	; HLS
	COLOR_CONVERT, state.r_v, state.g_v, state.b_v, a, b, c, /RGB_HLS
	WIDGET_CONTROL, state.hls_h_sl, set_value=(state.h_v = a)
	WIDGET_CONTROL, state.hls_l_sl, set_value=(state.f1_v = b)
	WIDGET_CONTROL, state.hls_s_sl, set_value=(state.f2_v = c)
	end
  endcase
  if state.color_index ge 0 then CW_RGB_SET_DRAW, state
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
end







function CW_RGB_GET_VAL, id

  ; Recover the state of this compound widget
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  ret = [ state.r_v, state.g_v, state.b_v ]

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  return, ret
end








function CW_RGBSLIDER, parent, vertical=vertical, frame=frame, drag=drag, $
		       uvalue=uvalue, rgb=map_rgb, cmy=map_cmy, $
			hsv=map_hsv, hls=map_hls, LENGTH = length, $
			COLOR_INDEX=color_index

  IF NOT (KEYWORD_SET(drag))  THEN drag=0
  IF NOT (KEYWORD_SET(frame))  THEN frame=0
  IF NOT (KEYWORD_SET(vertical))  THEN vertical=0
  IF NOT (KEYWORD_SET(uvalue))  THEN uvalue=0
  IF NOT (KEYWORD_SET(length))  THEN length=256
  IF N_ELEMENTS(Color_index) le 0 then color_index=-1

  map_rgb = KEYWORD_SET(map_rgb)
  map_cmy = KEYWORD_SET(map_cmy)
  map_hsv = KEYWORD_SET(map_hsv)
  map_hls = KEYWORD_SET(map_hls)
  ; Make sure only one color system is selected.
  junk = map_rgb + map_cmy + map_hsv + map_hls
  if (junk eq 0) then map_rgb = 1	; RGB is the default
  if (junk gt 1) then $
	message, 'The RGB, CMY, HSV, and HLS keywords are mutually exclusive.'

  ; The variables in the state struct have suffixes that identify their class
  ;	_b - The various slider bases
  ;	_s - The pulldown menu selection buttons
  ;     _sl - The sliders
  ;     _v - Current values in various systems
  state = { CW_RGBSLIDER_STATE, $
	    r_v:0B, g_v:0B, b_v:0B, $		; Current RGB for all C.S.
	    h_v:0.0, f1_v:0.0, f2_v:0.0, $	; Curr. for HLS and HSV
	    r_sl:0L, g_sl:0L, b_sl:0L, $	; RGB sliders
	    c_sl:0L, m_sl:0L, y_sl:0L, $	; CMY sliders
	    hsv_h_sl:0L, hsv_s_sl:0L, hsv_v_sl:0L, $	; HSV sliders
	    hls_h_sl:0L, hls_l_sl:0L, hls_s_sl:0L, $	; HLS sliders
	    select_s:0L, select_b:lonarr(4), curr_b_idx:0L, $ ;Slider bases
	    first_child:0L, $			; Where the state is kept
	    color_index: long(color_index), $
	    color_window: -1L, $
	    color_draw: 0L }

  on_error,2              ;Return to caller if an error occurs

  base = WIDGET_BASE(parent, /COLUMN, FRAME=frame, EVENT_FUNC='CW_RGB_EVENT',$
        FUNC_GET_VALUE='CW_RGB_GET_VAL', PRO_SET_VALUE='CW_RGB_SET_VAL', $
	UVALUE=uvalue)
  junk = { CW_PDMENU_S, flags:0, name:'' }

  junk1 = WIDGET_BASE(base, /ROW)
  state.select_s=CW_PDMENU(junk1, [ { CW_PDMENU_S, 1, 'Select Color System' },$
	{ CW_PDMENU_S, 0, 'RGB  (Red/Green/Blue)' }, $
	{ CW_PDMENU_S, 0, 'CMY  (Cyan/Magenta/Yellow)' }, $
	{ CW_PDMENU_S, 0, 'HSV  (Hue/Saturation/Value)' }, $
	{ CW_PDMENU_S, 0, 'HLS  (Hue/Lightness/Saturation)' }])
  if color_index gt 0 then $
	state.color_draw = WIDGET_DRAW(junk1, xsize=24, ysize=24, RET=2)

  selbase = WIDGET_BASE(base)

  fslide_fmt = '(F4.2)'
  if (vertical eq 0) then begin
    tmp = WIDGET_BASE(selbase, /COLUMN, /MAP)
      state.r_sl = WIDGET_SLIDER(tmp, max=255, title='Red', $
				  drag=drag, xsize=length)
      state.g_sl = WIDGET_SLIDER(tmp, max=255, title='Green', $
				 drag=drag, xsize=length)
      state.b_sl = WIDGET_SLIDER(tmp, max=255, title='Blue', $
				 drag=drag, xsize=length)
      state.select_b(0) = tmp
    tmp = WIDGET_BASE(selbase, /COLUMN, MAP=0)
      state.c_sl = WIDGET_SLIDER(tmp, max=255, title='Cyan', $
				 drag=drag, xsize=length)
      state.m_sl = WIDGET_SLIDER(tmp, max=255, title='Magenta', $
				 drag=drag, xsize=length)
      state.y_sl = WIDGET_SLIDER(tmp, max=255, title='Yellow', $
				 drag=drag, xsize=length)
      state.select_b(1) = tmp
    tmp = WIDGET_BASE(selbase, /COLUMN, MAP=0)
      state.hsv_h_sl = WIDGET_SLIDER(tmp, max=360, title='Hue', $
				     drag=drag, xsize=length)
      state.hsv_s_sl = CW_FSLIDER(tmp, max=1.0, title='Saturation', $
			          drag=drag, xsize=length, format=fslide_fmt)
      state.hsv_v_sl = CW_FSLIDER(tmp, max=1.0, title='Value', $
			          drag=drag, xsize=length, format=fslide_fmt)
      state.select_b(2) = tmp
    tmp = WIDGET_BASE(selbase, /COLUMN, MAP=0)
      state.hls_h_sl = WIDGET_SLIDER(tmp, max=360, title='Hue', $
			             drag=drag, xsize=length)
      state.hls_l_sl = CW_FSLIDER(tmp, max=1.0, title='Lightness', $
			          drag=drag, xsize=length, format=fslide_fmt)
      state.hls_s_sl = CW_FSLIDER(tmp, max=1.0, title='Saturation', $
			          drag=drag, xsize=length, format=fslide_fmt)
      state.select_b(3) = tmp
  endif else begin				; Vertical sliders
    tmp = WIDGET_BASE(selbase, /ROW, /MAP)
      state.r_sl = WIDGET_SLIDER(tmp, max=255, title='R',drag=drag, $
				 ysize=length, /vertical)
      state.g_sl = WIDGET_SLIDER(tmp, max=255, title='G', $
				 drag=drag, ysize=length, /vertical)
      state.b_sl = WIDGET_SLIDER(tmp, max=255, title='B', $
				 drag=drag, ysize=length, /vertical)
      state.select_b(0) = tmp
    tmp = WIDGET_BASE(selbase, /ROW, MAP=0)
      state.c_sl = WIDGET_SLIDER(tmp, max=255, title='C', $
				 drag=drag, ysize=length, /vertical)
      state.m_sl = WIDGET_SLIDER(tmp, max=255, title='M', $
				 drag=drag, ysize=length, /vertical)
      state.y_sl = WIDGET_SLIDER(tmp, max=255, title='Y', $
				 drag=drag, ysize=length, /vertical)
      state.select_b(1) = tmp
    tmp = WIDGET_BASE(selbase, /ROW, MAP=0)
      state.hsv_h_sl = WIDGET_SLIDER(tmp, max=360, title='H', $
				     drag=drag, ysize=length, /vertical)
      state.hsv_s_sl = CW_FSLIDER(tmp, max=1.0, title='S', $
				   drag=drag, ysize=length, /vertical, $
				   format=fslide_fmt)
      state.hsv_v_sl = CW_FSLIDER(tmp, max=1.0, title='V', $
				   drag=drag, ysize=length, /vertical, $
				   format=fslide_fmt)
      state.select_b(2) = tmp
    tmp = WIDGET_BASE(selbase, /ROW, MAP=0)
      state.hls_h_sl = WIDGET_SLIDER(tmp, max=360, title='H', $
			             drag=drag, ysize=length, /vertical)
      state.hls_l_sl = CW_FSLIDER(tmp, max=1.0, title='L',drag=drag,$
			          ysize=length, /vertical, format=fslide_fmt)
      state.hls_s_sl = CW_FSLIDER(tmp, max=1.0, title='S',drag=drag,$
			          ysize=length, /vertical, format=fslide_fmt)
      state.select_b(3) = tmp
  endelse

  ; Stash the state
  WIDGET_CONTROL, WIDGET_INFO(base, /CHILD), SET_UVALUE=state, /NO_COPY

  ; If the initial color system is not RGB, switch it now before it's realized
  if (map_cmy) then CW_RGB_CHNG_CS, nstate, 1
  if (map_hsv) then CW_RGB_CHNG_CS, nstate, 2
  if (map_hls) then CW_RGB_CHNG_CS, nstate, 3

  return, base
end
