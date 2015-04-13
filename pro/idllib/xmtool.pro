; $Id: xmtool.pro,v 1.2 1993/06/17 22:33:34 steve Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

PRO XManTool_event, event

COMMON MANAGED, ids, names, $
		nummanaged, inuseflag, $
		backroutines, backids, backnumber, nbacks, validbacks, $
		blocksize, $
		cleanups, $
		outermodal

COMMON MYMANTOOL, mylast, toollist, toolbase, mynum, selectedwid, $
		litcontbase, enabler, disablelist, selectedback

WIDGET_CONTROL, event.id, GET_UVALUE = evntval

CASE evntval OF
  "COLADJ": xloadct, GROUP = event.top

  "TOOLLIST" : BEGIN
		 validids = WHERE(ids NE 0)
		 selectedwid = validids(event.index)
		 selectedwid = selectedwid(0)
		 WIDGET_CONTROL, litcontbase, $
			/SENSITIVE
	       END

  "KILLER" : BEGIN
	       WIDGET_CONTROL, litcontbase, $
			SENSITIVE = 0
	       WIDGET_CONTROL, ids(selectedwid), /DESTROY
	     END

  "SHOWSTER" : BEGIN
	       WIDGET_CONTROL, litcontbase, $
			SENSITIVE = 0
	       WIDGET_CONTROL, ids(selectedwid), /SHOW
	     END

   "UPDATE" : BEGIN
   		WIDGET_CONTROL, event.id, TIMER = 1
		newids = ids(WHERE(ids NE 0))
		IF(N_ELEMENTS(mylast) EQ 0) THEN $
		  update = 1 $
		ELSE BEGIN
		  ; if there are any differences between old and new 
		  ; lists, update
		  IF ( N_ELEMENTS(mylast) NE N_ELEMENTS(newids) ) THEN $
		    update = 1 $
		  ELSE BEGIN
		    idval = [mylast EQ newids]
		    toss = WHERE(idval EQ 0, update)
		  ENDELSE
		ENDELSE
		IF(update NE 0) THEN BEGIN
		  WIDGET_CONTROL, toollist, $
			SET_VALUE = names(WHERE(names NE ''))
		  mylast = newids
		ENDIF
	      END

  "DONE": WIDGET_CONTROL, event.top, /DESTROY

  ELSE:
ENDCASE

END
;-----------------------------------------------------------------------------

PRO XMTool, GROUP = GROUP

;+
; NAME:
;	XMTOOL
;
; PURPOSE:
;	Provide a tool for viewing Widgets currently being managed by the 
;	XMANAGER.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XMTOOL
;
; KEYWORD PARAMETERS:
;	GROUP:	The widget ID of the group leader that the XMANAGERTOOL 
;		is to live under.  If the group is destroyed, the 
;		XMANAGERTOOL is also destroyed.
;
; COMMON BLOCKS:
;	MYMANTOOL:  This ommon block keeps track of the state of the XMANAGER
;		the last time the XMANAGERTOOL background routine was called.
;
; SIDE EFFECTS:
;	This procedure creates a widget that has the ability to destroy other 
;	widgets being managed by the XManager.
;
; RESTRICTIONS:
;	Only one instance of the XMANAGERTOOL can run at one time.
;
; PROCEDURE:
;	Initiate the widget and then let the timer routine update the
;	lists as the widgets being managed by the XMANAGER are changed.
;
; MODIFICATION HISTORY:
;	Written by Steve Richards, Dec, 1990.
;	SMR - 6/93	Modified the routine to work with a timer instead
;			of the obsolete background tasks.
;-

COMMON MANAGED, ids, names, $
		nummanaged, inuseflag, $
		backroutines, backids, backnumber, nbacks, validbacks, $
		blocksize, $
		cleanups, $
		outermodal

COMMON MYMANTOOL, mylast, toollist, toolbase, mynum, selectedwid, $
		litcontbase, enabler, disablelist, selectedback

IF(XRegistered("XManagerTool")) THEN RETURN

toolbase = WIDGET_BASE(TITLE = "Xmanager Tool", $
		/COLUMN, $
		SPACE = 10, $
		UVALUE = "UPDATE")
buttonbase = WIDGET_BASE(toolbase, /ROW)
tooldone = WIDGET_BUTTON(buttonbase, $
		VALUE = "Exit Xmanager Tool", $
		UVALUE = "DONE")
coladjust = WIDGET_BUTTON(buttonbase, $
		VALUE = "XLoadct...", $
		UVALUE = "COLADJ")
litbase = WIDGET_BASE(toolbase, $
		/COLUMN, $
;		/FRAME, $
		SPACE = 10)
toollabel = WIDGET_LABEL(litbase, $
		VALUE = "Managed Widgets")
toollist = WIDGET_LIST(litbase, $
		VALUE = "XmanagerTool      ", $
		YSIZE = 10, $
		UVALUE = "TOOLLIST")
litcontbase = WIDGET_BASE(litbase, $
		/ROW)
showster = WIDGET_BUTTON(litcontbase, $
		VALUE = "Bring To Front", $
		UVALUE = "SHOWSTER")
killer = WIDGET_BUTTON(litcontbase, $
		VALUE = "Kill Widget", $
		UVALUE = "KILLER")

WIDGET_CONTROL, litcontbase, SENSITIVE = 0
WIDGET_CONTROL, toolbase, /REALIZE

mylast = ''
IF(KEYWORD_SET(nummanaged)) THEN mynum = nummanaged ELSE mynum = 0

WIDGET_CONTROL, toolbase, TIMER = 1

Xmanager, "XManagerTool", $
		toolbase, $
		EVENT_HANDLER = "XManTool_event", $
		GROUP_LEADER = GROUP

END

