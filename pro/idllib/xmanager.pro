; $Id: xmanager.pro,v 1.8 1995/04/27 23:25:33 davee Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

PRO XUnregister, corpse

	;This procedure removes a dead widget from the Xmanagers common
	;block.  The space taken by the dead widget in the id and name
	;lists is just zeroed.

COMMON MANAGED, ids, names, $
		nummanaged, inuseflag, $
		backroutines, backids, backnumber, nbacks, validbacks, $
		blocksize, $
		cleanups, $
		outermodal

janitor = 0						;check to see if the
victim = WHERE(ids EQ corpse, janitor)			;dying widget has a
IF(janitor NE 0) THEN $					;cleanup routine and
  IF(STRLEN(cleanups(victim(0))) NE 0) THEN $		;call it if so.
    CALL_PROCEDURE, cleanups(victim(0)), corpse

deadindex = WHERE(ids EQ corpse, found)			;find dead index
IF(found NE 0) THEN BEGIN
  IF(KEYWORD_SET(backroutines)) THEN BEGIN		;find living background
    backindex = WHERE(backids EQ corpse, backfound)
    IF(backfound NE 0) THEN BEGIN			;if background tasks
      backroutines(backindex) = ''			;are registered for
      backids(backindex) = 0L				;the corpse, remmove
      backnumber = 0					;them
      nbacks = nbacks - backfound
    ENDIF
    IF(nbacks NE 0) THEN $				;keep a list of which
      validbacks = WHERE(backids NE 0) $		;background slots are
    ELSE validbacks = -1				;valid in the id list
  ENDIF
  ids(deadindex) = 0L					;zero out the dead
  names(deadindex) = ''					;widgets list entries
  cleanups(deadindex) = ''
  nummanaged = nummanaged - found
ENDIF
END		;end of XUnregister procedure

;-----------------------------------------------------------------------------

PRO XManager, NAME, ID, $
		BACKGROUND = BACKGROUND, $
		CLEANUP = CLEANUP, $
		EVENT_HANDLER = EVENT_HANDLER, $
		GROUP_LEADER = GROUP_LEADER, $
		JUST_REG = JUST_REG, $
		MODAL = MODAL
;+
; NAME:
;	XMANAGER
;
; PURPOSE:
;	Provide main event loop and management for widgets created using IDL.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XMANAGER [, Name, ID]
;
; OPTIONAL INPUTS:
;	NAME:	A string that contains the name of the routine that creates 
;		the widget.
;  
;	ID:	The widget ID of the base of the new widget.
;
; KEYWORD PARAMETERS:
;   BACKGROUND:
;      -------------------------------------------------------------------
;      | PLEASE NOTE: This keyword is OBSOLETE. It's functionality is    |
;      | provided by the TIMER keyword to the WIDGET_CONTROL procedure.  |
;      -------------------------------------------------------------------
;		A string that contains the name of a background task procedure
;		to be called when the event loop is idle.  The background task
;		procedure has only one parameter.  The parameter is the widget
;		ID of the top-level base widget associated with the background
;		task.  The background task should be very short and quick in 
;		execution.  Long, complicated routines will hinder the 
;		smoothness of event processing.  See the "Simple Widget
;		Examples" (available from the XDEMO examples menu or by
;		entering WEXMASTER at the IDL prompt) for an example of a 
;		background task widget.
;
;      CLEANUP:	This keyword contains a string that is the name of the
;		routine called when the widget dies.  If not specified,
;		no routine is called.  The cleanup routine must accept one 
;		parameter which is the widget id of the dying widget.
;
;EVENT_HANDLER:	The name of the routine that is to be called when a
;		widget event occurs in the widget being registered.  If this 
;		keyword is not supplied, the Xmanager will try to process the
;		event by calling the routine name with "_event" appended.  See
;		below for a more detailed explanation.
;
; GROUP_LEADER:	The widget id of the group leader for the
;		widget being processed.  When the leader dies either by the 
;		users actions or some other routine, all widgets that have that
;		leader will also die.
;
;		For example, a widget that views a help file for a demo 
;		widget would have that demo widget as it's leader.  When
;		the help widget is registered, it sets the keyword 
;		GROUP_LEADER to the widget id of the demo widget. If 
;		the demo widget is destroyed, the help widget led by 
;		the it would be killed by the XMANAGER.
;
;     JUST_REG:	This keyword tells the manager to just register the widget
;		but not to start doing the event processing.  This is useful
;		when you want to register a group of related top level widgets
;		before beginning processing.
;
;	 MODAL:	When this keyword is set, the widget that is being registered
;		traps all events and desensitizes all the other widgets.  It
;		is useful when input from the user is necessary to continue.
;		Once the modal widget dies, the others are resensitized and the
;		normal event processing is restored.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	MANAGED:  Common block used for widget maintenance.  This common block
;		is internal and should not be referenced outside the IDL
;		supplied routines.  It is subject to change without notice.
;	XMAN_BCK_OBSOLETE: Used to keep track of BACKGROUND keyword message.
;	
;
; SIDE EFFECTS:
;	XMANAGER takes control of event processing and locks out the IDL 
;	prompt until all widgets have been destroyed.
;
; RESTRICTIONS:
;	Widgets being registered with the Xmanager must provide at least two 
;	routines.  The first routine creates the widget and registers it with
;	the manager and the second routine processes the events that occur 
;	within that widget.  Optionally, a third routine can is used for the
;	processing of background tasks while awaiting input.  An example 
;	widget is suplied below that uses only two routines.  For more 
;	information on using the background tasks, see the XEXAMPLE routine
;	that is found in this same library.  It demonstrates background 
;	routines.  Also, the "Simple Widget Examples", that can be viewed by
;	entering WEXMASTER at the IDL prompt, demonstrate many aspects of
;	widget programming. 
;
;   !!!!! WARNING !!!!!!!!! WARNING !!!!!!!!! WARNING !!!!!!!!! WARNING !!!!!
;
;	Although this is a library routine, it may change in the future in
;	its internal implementation.  For future upgradability, it is best
;	not to modify or even worry about what this routine does internally.
;
; EXAMPLE USE:
;	To create a widget named Example that is just a base widget with a done
;	button using the XMANAGER you would do the following:
;
;
;	;------ first - the event handler routine ------;
;
;     PRO example_event, ev			;this is the routine that 
;						;deals with the events in the 
;						;example widget.
;	
;	WIDGET_CONTROL, ev.id, GET_UVALUE = uv	;the uservalue is retrieved 
;						;from the widget where the 
;						;event occured
;
;	if(uv eq 'DONE') then $			;if the event occured in the
;	  WIDGET_CONTROL, ev.top, /DESTROY	;done button then kill the 
;     END					;widget example
;
;
;	;------ second - the main routine ------;
;
;     PRO example				;this is the main routine
;						;that builds the widget and
;						;registers it with the Xmanager
;	
;	base = WIDGET_BASE(TITLE = 'Example')	;first the base is created
;	
;	done = WIDGET_BUTTON(base, $		;next the done button is 
;			     TITLE = 'DONE', $	;created and it's user value
;			     UVALUE = 'DONE')	;set to "DONE"
;
;	WIDGET_CONTROL, base, /REALIZE		;the widget is realized
;
;	XManager, 'example', base		;finally the example widget
;						;is registered with the 
;						;Xmanager
;     END
;
;	notes:	First the event handler routine is listed.  The handler
;		routine has the same name as the main routine with the 
;		characters "_event" added.  If you would like to use another
;		event handler name, you would need to pass it's name in as
;		a string to the EVENT_HANDLER keyword.  Also notice that the
;		event routine is listed before the main routine.  This is 
;		because the compiler will not compile the event routine if
;		it was below the main routine.  This is only needed if both
;		routines reside in the same file and the file name is the same
;		as the main routine name with the ".pro" extension added.
;
;
; PROCEDURE:
;	When the first widget is registered, initialize the lists and then 
;	start processing events.  Continue registering widgets and dispatching
;	events until all the widgets have been destroyed.  When a widget is 
;	killed, destroy all widgets that list the destroyed widget as their 
;	leader, if any.
;
; RELATED FUNCTIONS AND PROCEDURES:
;	XREGISTERED, XMANAGERTOOL, XBACKREGISTER
;
; MODIFICATION HISTORY: Written by Steve Richards, November, 1990
;	SMR, Mar,  1991	Added a cleanup routine keyword to allow dying
;			widgets to clean themselves up when dying.
;	SMR, May,  1991 Fixed a bug found by Diane Parchomchuk where 
;			an error occured when registering a widget 
;			right after destroying another.
;	SMR & ACY, July, 1991
;			Fixed a bug found by Debra Wolkovitch where
;			lone widgets being destroyed and new ones 
;			created caused problems.
;	SMR, Sept, 1991	Changed cleanup to use the new WIDGET_INFO
;			routine.
;	SMR & ACY, Oct,  1991
;			Fixed a bug where a background event that 
;			unregistered itself after a time would result
;			in an XMANAGER error.
; 	SMR, Mar.  1992	Changed XMANAGER to use enhanced widget functions for
;			event processing.
;	SMR, Nov.  1992 Changed modal widget handling allowing nesting of
;			modal widgets.  The first modal desensitizes all 
;			current widgets and subsequent modals only desensitize
;			the modal that called them.
;	JIY, Apr.  1993 Changed modal widget handling process to not run
;			the event loop for nested modal widgets.
;			Allowed for multiple modal widgets.
;	AB & SMR, 17 November 1993
;			Added ID validity checking to desensitizing of modal
;			widgets to fix a bug where already dead widgets
;			were being accessed.
;	DJE, Feb, 1995
;			Made it so that non-modal widgets created from a modal
;			widget have events processed in the modal widget's
;			event loop. This fixes a bug where xmanager wouldn't
;			return immediately if there was a modal widget somewhere
;			in the nesting, even though a non-modal widget was being
;			added. The nesting level could get _very_ deep.
;	DJE, Apr 1995
;			Pass a local variable to WIDGET_EVENT in the MODAL
;			case, instead of passing the common block variable
;			outermodal. This avoids a bug where outermodal gets
;			changed behind WIDGET_EVENT's back.
;-

COMMON MANAGED, ids, names, $
		nummanaged, inuseflag, $
		backroutines, backids, backnumber, nbacks, validbacks, $
		blocksize, $
		cleanups, $
		outermodal

COMMON XMAN_BCK_OBSOLETE, obsolete

IF(NOT(KEYWORD_SET(outermodal))) THEN outermodal = 0	;initialization

doeventloop = 0						;flag set if the main
							;event loop to be used

;------------- New Widget Registration Section ---------

IF(N_PARAMS() EQ 0) THEN BEGIN				;no parameters so just
  IF (NOT(KEYWORD_SET(nummanaged))) THEN RETURN		;start up the xmanager
  validids = WIDGET_INFO(ids(0:nummanaged-1), /VALID)	;loop if widgets 
  badids = WHERE((validids EQ 0), deaduns)		;present
  IF(deaduns NE 0) THEN $
    FOR i = 0, deaduns-1 do $
      XUnRegister, ids(badids(i))			;event loop again
  IF(KEYWORD_SET(ids)) THEN doeventloop = 1
  IF(NOT(KEYWORD_SET(nummanaged))) THEN $
    MESSAGE, 'No Widgets Are Currently being managed'
ENDIF ELSE IF(N_PARAMS() NE 2) THEN BEGIN
  MESSAGE, 'Wrong Number of Arguments, ' + $
	   'Usage = Xmanager, [name, id]'

ENDIF ELSE BEGIN					;register a new widget

  IF (KEYWORD_SET(IDS)) THEN BEGIN			;make sure that the new
    position = WHERE(ID EQ IDS, found)			;widget ID is not
    IF (found NE 0) THEN $				;currently in the IDS
      XUnregister, ID					;common block.
  ENDIF							;This is only
							;true when a widget is
							;killed and another is
							;created before
							;events are processed

  IF(NOT KEYWORD_SET(EVENT_HANDLER)) THEN $		;if no event handler 
		EVENT_HANDLER = NAME + '_event'		;was specified, assume
							;that the widget name +
							;"_event" exists

  IF(KEYWORD_SET(BACKGROUND)) THEN BEGIN
    ; Make sure the window manager will not kill a widget that has a
    ; background in progress
    WIDGET_CONTROL, id, /DELAY_DESTROY
    if not KEYWORD_SET(obsolete) then begin
      obsolete = 1
      message,/INFO,"The BACKGROUND keyword to theXMANAGER procedure is obsolete. It is superceeded by the TIMER keyword to the WIDGET_CONTROL procedure."
    endif
  endif

  WIDGET_CONTROL, id, /MANAGED				; Mark it.

  IF(NOT KEYWORD_SET(CLEANUP)) THEN $			;specify a null cleanup
		CLEANUP = ''				;if keyword not set

  IF(KEYWORD_SET(GROUP_LEADER)) THEN $			;register the group
    WIDGET_CONTROL, ID, GROUP_LEADER = GROUP_LEADER	;leader

  IF(NOT(KEYWORD_SET(nummanaged))) THEN $		;keep track of the
    nummanaged = 1 $					;number of managed
  ELSE $						;widgets
    nummanaged = nummanaged + 1

  IF(NOT(KEYWORD_SET(inuseflag)) AND $			;if just_register is
     NOT(KEYWORD_SET(JUST_REG))) THEN $			;set, don't process
	doeventloop = 1					;events

  IF(N_ELEMENTS(ids) EQ 0) THEN BEGIN			;create the first block
    blocksize = 64					;and set the size of
    ids = LONARR(blocksize)				;the blocks to be used
    names = STRARR(blocksize)				;in the future
    cleanups = STRARR(blocksize)
    freeindex = 0
  ENDIF ELSE BEGIN
    freeindex = WHERE(ids EQ 0, found)			;otherwise find holes
    IF(found EQ 0) THEN BEGIN				;in the current blocks
      ids = [ids, LONARR(blocksize)]			;or allocate a new one
      names = [names, STRARR(blocksize)]		;if all are full
      cleanups = [cleanups, STRARR(blocksize)]
      freeindex = nummanaged - 1
    ENDIF
  ENDELSE

  names(freeindex(0)) = NAME				;put the information
  ids(freeindex(0)) = ID				;into the blocks
  cleanups(freeindex(0)) = CLEANUP

  IF(NOT(KEYWORD_SET(nbacks))) THEN $
    nbacks = 0

  IF(KEYWORD_SET(BACKGROUND)) THEN BEGIN

    nbacks = nbacks + 1

    IF(N_ELEMENTS(backids) EQ 0) THEN BEGIN
      backids = LONARR(blocksize)			;build the first block
      backroutines = STRARR(blocksize)			;for background tasks
      freebacks = 0
      backnumber = 0
    ENDIF ELSE BEGIN					;otherwise, just
      freebacks = WHERE(backids EQ 0, found)		;find free spots or
      IF(found EQ 0) THEN BEGIN				;make new ones if full
        backids = [backids, LONARR(blocksize)]
        backroutines = [backroutines, $
			STRARR(blocksize)]
        freebacks = nbacks - 1
      ENDIF
    ENDELSE

    backids(freebacks(0)) = ID				;add the background
    backroutines(freebacks(0)) = STRUPCASE(BACKGROUND)	;in the first free spot

    validbacks = WHERE(backids NE 0)

    WIDGET_CONTROL, /EVENT_BREAK			;since an xmanager
							;loop could have called
							;this routine that is
							;not processing bcks, 
							;break out so this one
  ENDIF ;background set					;gets processed.

  WIDGET_CONTROL, ID, KILL_NOTIFY = 'XUnregister', $	;set the new widget's
		      EVENT_PRO = EVENT_HANDLER		;callbacks

;----------------- Modal Section -----------------------

  IF (keyword_set(outermodal) and (not keyword_set(modal))) THEN BEGIN

    ; a non-modal widget was added when there was already a modal widget
    ; up - just add the new widget to the modal list and return
    outermodal = [ outermodal, ID ]
    doeventloop = 0

    ; need to break out of the outer widget_event call so that the
    ; outer xmanager can see that outmodal has changed
    WIDGET_CONTROL, /EVENT_BREAK
    
  END ELSE IF (KEYWORD_SET(MODAL)) THEN BEGIN
							;when a modal widget
							;is called, all widgets
    IF (KEYWORD_SET(outermodal)) THEN BEGIN		;currently sensitive
      IF ((where(outermodal eq ID))(0) eq -1) THEN BEGIN
							;need to be desenitized
        senslist = outermodal				;making sure that a
        numsens  = n_elements(senslist)			;widget ID is not being
      ENDIF ELSE numsens = 0				;reused.
      doeventloop = 0
    ENDIF ELSE BEGIN					;nested calls to modal
      desens = where(ids NE ID, found)			;widgets are handled by
      IF (found NE 0) THEN BEGIN			;the stack so each 
	senslist = ids(desens)				;new modal only
	numsens = N_ELEMENTS(senslist)			;desensitizes the modal
      ENDIF ELSE numsens = 0				;below it.
    ENDELSE

    oldouter = outermodal
    outermodal = ID

    IF (numsens NE 0) THEN $	
      FOR i = 0, numsens - 1 DO $			;desensitize all
        IF (WIDGET_INFO(senslist(i), /VALID) EQ 1) THEN $ ;widgets active when
	  WIDGET_CONTROL, senslist(i), SENSITIVE = 0	;this modal is called.
							;either all or the 
							;modal that called me.

    bad = 0
    while (not bad) do begin				;loop as long as 
      ; pass a local var to widget_event, since outermodal could be
      ; changed by a reentrant call to xmanager
      temp = outermodal
      junk = widget_event (temp, bad_id = bad)	;modal list is alive.
      if ((where(outermodal eq bad))(0) eq -1) then bad = 0 else begin
        good = where(outermodal ne bad)
        if (good(0) eq -1) then bad = 1 else begin
          outermodal = outermodal (good)
          bad = 0;
        endelse
      endelse
    endwhile

    IF (numsens NE 0) THEN $
      FOR i = 0, numsens - 1 DO $
        IF(WIDGET_INFO(senslist(i),/VALID) EQ 1) THEN $	;now resensitize only
	  WIDGET_CONTROL, senslist(i), /SENSITIVE	;the widgets made 
      							;insensitive by this
							;modal request

    outermodal = oldouter				;restore my caller
							;as the outer modal
  ENDIF ;modal
ENDELSE ;xmanager called with 2 params

;----------------- Main Event Loop ---------------------

IF(doeventloop NE 0) THEN BEGIN				;if not modal and the
  WHILE(KEYWORD_SET(nummanaged)) DO BEGIN		;event loop should be
    inuseflag = 1					;executed, do it!
    active = WIDGET_INFO(/ACTIVE)
    WHILE (active NE 0) DO BEGIN
      IF(nbacks NE 0) THEN BEGIN			;Non-blocking BG task
        WHILE(nbacks NE 0) DO BEGIN
	  IF(backnumber GE nbacks) THEN backnumber = 0;
	  CALL_PROCEDURE, backroutines(validbacks(backnumber)), $
			  backids(validbacks(backnumber))
	  backnumber = backnumber + 1
	  newevent = WIDGET_EVENT(/NOWAIT)
        ENDWHILE
      ENDIF ELSE $					;Blocking mode
        newevent = WIDGET_EVENT()
      active = WIDGET_INFO(/ACTIVE)
    ENDWHILE
    nummanaged = 0
  ENDWHILE
  inuseflag = 0						;done so unset the 
ENDIF ;doeventloop NE 0					;inuse flag

END ;xmanager.pro
;----------------------- end of procedure XManager ----------------------------
