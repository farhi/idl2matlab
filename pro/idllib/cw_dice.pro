; $Id: cw_dice.pro,v 1.4 1993/11/16 18:37:58 ali Exp $

; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_DICE
;
; PURPOSE:
;	CW_DICE is a compound widget that implements a single die.
;	This widget uses a button with a bitmap label.
;
;	The primary purpose of this compound widget is to serve as
;	a full example of a realistic compound widget for the IDL
;	User's Guide.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	Widget = CW_DICE(Parent)
;
; INPUTS:
;       Parent:	  The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	TUMBLE_CNT: The widget simulates the tumbling of a dice by
;		changing the bitmap on the dice several times before
;		settling down to a final value. The number of "tumbles"
;		is specified by the TUMBLE_CNT keyword. The default is 10.
;	TUMBLE_PERIOD: The amount of time in seconds between each tumble
;		of the dice. The default is .05 seconds.
;	UVALUE:	  The user value for the widget.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS
;	CW_DICE_BLK: Used to store dice faces, and the current
;		random number generator seed for the CW_DICE class.
;
; SIDE EFFECTS:
;	This widget generates event structures containing an extra
;	field named VALUE giving the final value resulting from a dice roll.
;	Such events are only sent when the user presses the dice button.
;
; PROCEDURE:
;	The CW_DICE widget consists of a single pushbutton that
;	displays its current dice value as a bitmask. If the user presses
;	the button, it tumbles for a moment and then the new value is
;	displayed and an event is issued.
;
;	The current value of the dice is available via the
;	WIDGET_CONTROL,GET_VALUE command.
;
;	The current value can be set by issuing the
;	WIDGET_CONTROL, SET_VALUE command. If the requested value is
;	outside the range [1,6], then the dice tumbles to a new value
;	as if the user had pressed the button, but no event is issued.
;
; MODIFICATION HISTORY:
;	24 October 1993, AB, RSI
;-

pro CW_DICE_ROLL, dice, state
  ; Given the state structure from a CW_DICE instance, this routine,
  ; sets it to a new value

  COMMON CW_DICE_BLK, seed, faces

  IF (state.remaining EQ 0) THEN BEGIN		; Initial request
    state.remaining = state.tumble_cnt
    state.value = FIX(6 * RANDOMU(seed) + 1)	; Get final answer up front
  ENDIF 

  IF (state.remaining EQ 1) THEN BEGIN
    value = state.value			; Last time, so use final answer
  ENDIF ELSE BEGIN			; Not last time, use random value
    value = FIX(6 * RANDOMU(seed) + 1)	; Intermediate value
    WIDGET_CONTROL, dice, TIMER=state.tumble_period
  ENDELSE

  WIDGET_CONTROL, dice, set_value=faces(*,*,value-1)

  state.remaining = state.remaining - 1
end







FUNCTION CW_DICE_EVENT, ev

  ; Recover the state of this compound widget
  base = ev.handler
  stash = WIDGET_INFO(base, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  ; Roll the die and display the new result.
  CW_DICE_ROLL, stash, state

  ; If this invocation is not from the timer, issue an event
  if (TAG_NAMES(ev, /STRUCTURE_NAME) ne 'TIMER_EVENT') THEN $
    ret = { CW_DICE_EVENT,ID: base,TOP:ev.top,HANDLER:0L,VALUE:state.value } $
  ELSE ret = 0

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  RETURN, ret

end







pro CW_DICE_SET_VAL, id, value

  COMMON CW_DICE_BLK, seed, faces

  ; Recover the state of this compound widget
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  ; If value out of range, roll the dice ourselves
  IF (value < 1) OR (value > 6) THEN BEGIN
    CW_DICE_ROLL, stash, state
  ENDIF ELSE BEGIN		; Use the requested value.
    state.value=value
    WIDGET_CONTROL, stash, SET_VALUE=faces(*,*,value-1)
  endelse

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
end







function CW_DICE_GET_VAL, id

  ; Recover the state of this compound widget
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  ret = state.value

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  return, ret
end








function CW_DICE, parent, value, UVALUE=uvalue, TUMBLE_CNT=tumble_cnt, $
	TUMBLE_PERIOD=tumble_period

  COMMON CW_DICE_BLK, seed, faces

  IF NOT (KEYWORD_SET(tumble_cnt))  THEN tumble_cnt=10
  IF (tumble_cnt lt 1) then tumble_cnt = 10
  IF NOT (KEYWORD_SET(tumble_period))  THEN tumble_period=.05
  IF (tumble_period lt 0) then tumble_period = .05
  IF NOT (KEYWORD_SET(uvalue))  THEN uvalue=0

  on_error,2              ;Return to caller if an error occurs


  ; Generate the die faces. Each bitmap is 32x32 bits, 4x32 bytes, or 1x32
  ; longwords. This completely non-obvious computation was derived by
  ; using the X Window bitmap program to obtain the bitmaps. Then, I used the
  ; READ_X11_BITMAP program to read them into IDL and did some analysis
  ; to convert them into a programmatically generated longword array.
  ; The final step is to cast the longword array into the 6 byte arrays.
  faces = lonarr(192)
  i4=indgen(4)+1
  s5=[0,5]
  pos=[13, 77, 141, 36, 54, 67, 87, 100, 118, 131, 151, 164, 182]
  v1=['c00300'x,'c00300'x,'c00300'x,'e0010000'x,'8007'x,'f0000000'x,'f'x, $
      'e0018007'x,'e0018007'x,'f000000f'x,'f000000f'x,'f0c0030f'x,'f0c0030f'x]
  v2=['e00700'x,'e00700'x,'e00700'x,'f0030000'x,'c00f'x,'f8010000'x,'801f'x, $
      'f003c00f'x,'f003c00f'x,'f801801f'x,'f801801f'x,'f8e1871f'x,'f8e1871f'x]
  for i = 0, n_elements(pos)-1 do begin
    faces(s5+pos(i)) = v1(i)
    faces(i4+pos(i)) = v2(i)
  endfor
  faces = byte(faces,0,4,32,6)
  BYTEORDER, faces, /HTONL		; Little endian machines need swap

  ; Use RANDOMU to pick the initial value of the die, unless the user
  ; provided one.
  if (n_elements(value) eq 0) then value = fix(6 * randomu(seed) + 1)

  ; The state of this compound widget:
  ;	- It's currently displayed value.
  ;	- The number of times the dice should tumble before settling down.
  ;	- The amount of time in seconds in between tumbles.
  ;	- The number of tumbles left before the event should be reported.
  state = { value:value, tumble_cnt:FIX(tumble_cnt), $
	    tumble_period:tumble_period, remaining:0 }


  base = WIDGET_BASE(parent, UVALUE=uvalue, EVENT_FUNC='CW_DICE_EVENT',$
        FUNC_GET_VALUE='CW_DICE_GET_VAL', PRO_SET_VALUE='CW_DICE_SET_VAL')

  die = WIDGET_BUTTON(base, value=faces(*,*,value-1))

  ; Stash the state
  WIDGET_CONTROL, WIDGET_INFO(base, /CHILD), SET_UVALUE=state, /NO_COPY

  return, base
end
