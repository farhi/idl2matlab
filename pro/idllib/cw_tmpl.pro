; $Id: cw_tmpl.pro,v 1.2 1993/04/07 20:55:16 ali Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;
; PURPOSE:
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget = CW_TMPL(parent)
;
; INPUTS:
;       PARENT - The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	UVALUE - Supplies the user value for the widget.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; MODIFICATION HISTORY:
;-


PRO tmpl_set_value, id, value

	; This routine is used by WIDGET_CONTROL to set the value for
	; your compound widget.  It accepts one variable.  
	; You can organize the variable as you would like.  If you have
	; more than one setting, you may want to use a structure that
	; the user would need to build and then pass in using 
	; WIDGET_CONTROL, compoundid, SET_VALUE = structure.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the state.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

	; Set the value here.

	; Restore the state.
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

END



FUNCTION tmpl_get_value, id

	; This routine is by WIDGET_CONTROL to get the value from 
	; your compound widget.  As with the set_value equivalent,
	; you can only pass one value here so you may need to load
	; the value by using a structure or array.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

	; Get the value here


	; Restore the state.
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

        ; Return the value here.

END

;-----------------------------------------------------------------------------

FUNCTION tmpl_event, ev

	; This routine handles all the events that happen in your
	; compound widget and if the events need to be passed along
	; this routine should return the new event.  If nobody needs
	; to know about the event that just occured, this routine 
	; can just return 0.  If your routine never needs to pass
	; along an event, this routine can be a procedure instead
	; of a function.  Whichever type used must be set below in the
	; WIDGET_BASE call using either the EVENT_PROC or EVENT_FUNC 
	; keyword.  An event function that returns a scalar 0 is 
	; essentially an event procedure.

  parent=ev.handler


	; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

	; Process your compound widgets events here.
	; If the event doesn't need to propagate up any further, just 
	; return 0 and the event will stop here.  Otherwise, modify
	; the event for your usage and return it.


        ; Restore the state structure
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

	; You may need to add more tags to the event structure for your 
	; compound widget.  If so do it after the first three which are
	; required and preserve the order of the first three.

  RETURN, { ID:parent, TOP:ev.top, HANDLER:0L }
END

;-----------------------------------------------------------------------------

FUNCTION cw_tmpl, parent, UVALUE = uval

	; You should not use the user value of the main base for
	; your compound widget as the person using your compound widget
	; may want it for his or her own use.  
	; You also should not use the user value of the first widget you
	; install in the base as it is used to keep track of the state.

	; state structure for your compound widget.

  IF (N_PARAMS() EQ 0) THEN MESSAGE, 'Must specify a parent for Cw_Tmpl'

  ON_ERROR, 2					;return to caller

	; Defaults for keywords
  IF NOT (KEYWORD_SET(uval))  THEN uval = 0

	; Rather than use a common block to store the widget IDs of the 
	; widgets in your compound widget, put them into this structure so
	; that you can have multiple instances of your compound widget.
  state = { id:0 }

	; Here the widget base that encompasses your compound widget's 
	; individual components is created.  This is the widget ID that
	; is passed back to the user to represent the entire compound
	; widget.  If it gets mapped, unmapped, sensitized or otherwise
	; effected, each of its individual subcomponents will also be 
	; effected.  You can see that the event handler is installed here.
	; As events occur in the sub-components of the compound widgets,
	; the events are passed up the tree until they hit this base 
	; where the event handler you define above handles them.  Similarily
	; whenever WIDGET_CONTROL, SET/GET_VALUE is called on this base,
	; the routine defined by the FUNC_GET/PRO_SET_VALUE is called to
	; set the value of the compound widget.  None of the three keywords
	; that override the standard behaviour are not required so it 
	; depends on your usage whether they are needed.
  mainbase = WIDGET_BASE(parent, UVALUE = uval, $
		EVENT_FUNC = "tmpl_event", $
		FUNC_GET_VALUE = "tmpl_get_value", $
		PRO_SET_VALUE = "tmpl_set_value")

	; Here you would define the sub-components of your widget.  There
	; is an example component which is just a label.
  state.id = WIDGET_LABEL(mainbase, VALUE = "Compound Widget Template")

	; Save out the initial state structure into the first childs UVALUE.
  WIDGET_CONTROL, WIDGET_INFO(base, /CHILD), SET_UVALUE=state, /NO_COPY

	; Return the base ID of your compound widget.  This returned
	; value is all the user will know about the internal structure
	; of your widget.
  RETURN, mainbase

END





