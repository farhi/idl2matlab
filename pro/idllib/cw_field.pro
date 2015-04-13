; $Id: cw_field.pro,v 1.10 1994/05/06 16:09:51 kirk Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_FIELD
;
; PURPOSE:
;	This widget cluster function manages a data entry field widget.
;	The field consists of a label and a text widget.  CW_FIELD's can
;	be string fields, integer fields or floating-point fields.  The
;	default is an editable string field.
;
; CATEGORY:
;	Widget Clusters.
;
; CALLING SEQUENCE:
;	Result = CW_FIELD(Parent)
;
; INPUTS:
;	Parent:	The widget ID of the widget to be the field's parent.
;
; KEYWORD PARAMETERS:
;	TITLE:	A string containing the text to be used as the label for the
;		field.  The default is "Input Field:".
;
;	VALUE:	The initial value in the text widget.  This value is
;		automatically converted to the type set by the STRING,
;		INTEGER, and FLOATING keywords described below.
;
;	UVALUE:	A user value to assign to the field cluster.  This value
;		can be of any type.
;
;	FRAME:	The width, in pixels, of a frame to be drawn around the
;		entire field cluster.  The default is no frame.
;
;RETURN_EVENTS:	Set this keyword to make cluster return an event when a
;		<CR> is pressed in a text field.  The default is
;		not to return events.  Note that the value of the text field
;		is always returned when the WIDGET_CONTROL, field, GET_VALUE=X
;		command is used.
;
;   ALL_EVENTS: Like RETURN_EVENTS but return an event whenever the
;		contents of a text field have changed.
;
;	COLUMN:	Set this keyword to center the label above the text field.
;		The default is to position the label to the left of the text
;		field.
;
;	ROW:	Set this keyword to position the label to the left of the text
;		field.  This is the default.
;
;	XSIZE:	An explicit horizontal size (in characters) for the text input
;		area.  The default is to let the window manager size the
;		widget.  Using the XSIZE keyword is not recommended.
;
;	YSIZE:	An explicit vertical size (in lines) for the text input
;		area.  The default is 1.
;
;	STRING:	Set this keyword to have the field accept only string values.
;		Numbers entered in the field are converted to their string
;		equivalents.  This is the default.
;
;     FLOATING:	Set this keyword to have the field accept only floating-point
;		values.  Any number or string entered is converted to its
;		floating-point equivalent.
;
;      INTEGER:	Set this keyword to have the field accept only integer values.
;		Any number or string entered is converted to its integer
;		equivalent (using FIX).  For example, if 12.5 is entered in
;		this type of field, it is converted to 12.
;
;	LONG:	Set this keyword to have the field accept only long integer
;		values.  Any number or string entered is converted to its
;		long integer equivalent (using LONG).
;
;	FONT:	A string containing the name of the X Windows font to use
;		for the TITLE of the field.
;
;    FIELDFONT:	A string containing the name of the X Windows font to use
;		for the TEXT part of the field.
;
;	NOEDIT:	Normally, the value in the text field can be edited.  Set this
;		keyword to make the field non-editable.
;
; OUTPUTS:
;	This function returns the widget ID of the newly-created cluster.
;
; COMMON BLOCKS:
;	None.
;
; PROCEDURE:
;	Create the widgets, set up the appropriate event handlers, and return
;	the widget ID of the newly-created cluster.
;
; EXAMPLE:
;	The code below creates a main base with a field cluster attached
;	to it.  The cluster accepts string input, has the title "Name:", and
;	has a frame around it:
;
;		base = WIDGET_BASE()
;		field = CW_FIELD(base, TITLE="Name:", /FRAME)
;		WIDGET_CONTROL, base, /REALIZE
;
; MODIFICATION HISTORY:
; 	Written by:	Keith R. Crosley   June 1992
;			KRC, January 1993 -- Added support for LONG
;					     integers.
;		        AB, 7 April 1993, Removed state caching.
;			JWG, August 1993, Completely rewritten to make
;				use of improved TEXT widget functionality
;			ACY, 25 March, 1994, fix usage of FRAME keyword
;                       KDB, May 1994, Initial value =0 would result
;                                      in a null text field. Fixed
;                                      keyword check.
;
;-

;
;  Check and return the portion of a string that
;	is a valid floating point number.
;
FUNCTION CW_FIELD_VALIDATE, Value
	;	Look for invalid mantissa

    IF Value EQ '' THEN RETURN, ''

    Chars	= [ BYTE(Value), 0b ]
    Curr	= 0
    NeedDecimal	= 1

	;	Valid #s
	;	[+-]<number>[exponent]
	;	number ::= [0-9]+[.[0-9]*] or .[0-9]+
	;	exponent ::= {eEdD}[+-][0-9]+

	;	Signed value?
    IF Chars(Curr) EQ 43b OR Chars(Curr) EQ 45b THEN Curr = Curr + 1

	;	Look for digits before the decimal point
    IF Chars(Curr) GE 48b AND Chars(Curr) LE 57b THEN BEGIN
	NeedDecimal	= 0

	;	while(isdigit(*p))++p;
	WHILE Chars(Curr) GE 48b AND Chars(Curr) LE 57b DO Curr = Curr + 1
    ENDIF

	;	Must have .[0-9]+

    IF NeedDecimal THEN BEGIN
	IF Chars(Curr) NE 46b THEN RETURN,''	; invalid #
	Curr	= Curr + 1
	IF Chars(Curr) LT 48b OR Chars(Curr) GT 57b THEN RETURN,''

	;	while(isdigit(*p))++p;
	WHILE Chars(Curr) GE 48b AND Chars(Curr) LE 57b DO Curr = Curr + 1
    ENDIF ELSE BEGIN

	;	Might have .[0-9]*

	IF Chars(Curr) EQ 46b THEN BEGIN
	    Curr	= Curr + 1
		;	while(isdigit(*p))++p;
	    WHILE Chars(Curr) GE 48b AND Chars(Curr) LE 57b DO Curr = Curr + 1
	ENDIF
    ENDELSE

	;	Exponent?
    Dummy	= WHERE(Chars(Curr) EQ BYTE("dDeE"), Count)
    IF Count THEN BEGIN
	; Save exponent position in case the exponent is invalid
	; and only mantissa is valid number.
	SaveCurr	= Curr - 1

	Curr		= Curr + 1	; skip 'e'
	;	Signed exponent?
	IF Chars(Curr) EQ 43b OR Chars(Curr) EQ 45b THEN Curr = Curr + 1

	;	At least one digit after 'e' or exponent is malformed
	IF Chars(Curr) LT 48b OR Chars(Curr) GT 57b THEN BEGIN
		Curr	= SaveCurr	; Revert -- invalid exponent
	ENDIF ELSE BEGIN
	;	find end of exponent digits
	    WHILE Chars(Curr) GE 48b AND Chars(Curr) LE 57b DO Curr = Curr + 1
	ENDELSE
    ENDIF

    RETURN,STRING(Chars(0:Curr))	; Chars from 0-Curr are valid
END


FUNCTION CW_FIELD_VALUE, Value, Type

    IF Type EQ 0 THEN RETURN, Value

    NValue	= CW_FIELD_VALIDATE(Value(0))

    CASE Type OF
    1:	RETURN, FLOAT(NValue)
    2:	RETURN, FIX(NValue)
    3:	RETURN, LONG(NValue)
    ENDCASE
END

;
;	Procedure to set the value of a CW_FIELD
;
PRO CW_FIELD_SET, Base, Value

	sValue	= Value		; Prevent alteration from reaching back to caller

	Sz	= SIZE(sValue)
	IF Sz(0) NE 7 THEN sValue = STRTRIM(Value,2)

	Child	= WIDGET_INFO(Base, /CHILD)
	WIDGET_CONTROL, Child, GET_UVALUE=State, /NO_COPY
	WIDGET_CONTROL, State.TextId, $
		SET_VALUE=STRTRIM(CW_FIELD_VALUE(sValue, State.Type),2)
	WIDGET_CONTROL, Child, SET_UVALUE=State, /NO_COPY
END

;
;	Function to get the value of a CW_FIELD
;
FUNCTION CW_FIELD_GET, Base

	Child	= WIDGET_INFO(Base, /CHILD)
	WIDGET_CONTROL, Child, GET_UVALUE=State, /NO_COPY
	WIDGET_CONTROL, State.TextId, GET_VALUE=Value

	Ret	= CW_FIELD_VALUE(Value, State.Type)

	WIDGET_CONTROL, Child, SET_UVALUE=State, /NO_COPY
	RETURN, Ret
END

;	Ascii assumptions
;
;	+ - .	= 43,45,46
;	0-9	= 48-57
;	DEde	= 68,69,100,101
;

;
;	Examine an input stream of characters.
;	Alter the field contents to reflect this.
;	If any character inserted in a single operation is invalid,
;		ignore the entire operation.
;	Consider that field may contain a (known) invalid state
;
PRO CW_FIELD_INT, Ch, State, Event, Altered

    Altered	= 0		; nothing so far
    Nil		= 0		; field has contents
    Minus	= 0		; field is not just a '-'
    Negate	= 0		; new text has no '-'s in it
    TextId	= State.TextId

	; Special Cases:
	;	We don't actually care where in the input string a
	;	'-' is.  If there is an odd number of them, we
	;	negate the value (see below)
	;
	;	Current String		Char		Result
	;	Nil			'-'		'-'
	;	-			'-'		Nil
	;	<any number>		'-'		-<number>

    WIDGET_CONTROL, TextId, GET_VALUE=Value
    Value	= Value(0)

    IF Value EQ '' THEN Nil = 1		; Value is nil string
    IF Value EQ '-' THEN Minus = 1	; Value is an invalid number


	;	<CR> 
    IF Ch EQ 10b THEN BEGIN
	Altered	= 2
	RETURN
    ENDIF

    IF Ch EQ 45b THEN Negate = 1 $
    ELSE IF Ch GE 48b AND Ch LE 57b THEN BEGIN
	Nil = 0 & Minus = 0
    ENDIF ELSE RETURN	; ![0-9]

    ;	Add new character (if any)

    Selection	= WIDGET_INFO(TextId, /TEXT_SELECT)
    TIP		= Selection(0)+1-Negate	; Text Insertion Point

    IF Negate EQ 0 THEN BEGIN
	WIDGET_CONTROL, TextId, SET_VALUE=STRING(Ch), /USE_TEXT_SELECT
	Altered	= 1
    ENDIF

    IF Negate THEN BEGIN
	IF Nil THEN BEGIN
	    WIDGET_CONTROL, TextId, SET_VALUE='-'
	    TIP	= 1
	ENDIF ELSE IF Minus THEN BEGIN
	    WIDGET_CONTROL, TextId, SET_VALUE=''
	ENDIF ELSE BEGIN
	    ;	We actually have a number to negate

	    WIDGET_CONTROL, TextId, GET_VALUE=Value
	    IValue	= LONG(Value)
	    TIP		= TIP + (IValue GT 0) - (IValue LT 0)
	    WIDGET_CONTROL, TextId, SET_VALUE=STRTRIM(-IValue,2)
	ENDELSE
	Altered	= 1
    ENDIF

    ; Set selection point
    IF Altered THEN WIDGET_CONTROL, TextId, SET_TEXT_SELECT=[TIP,0]
END


FUNCTION CW_FIELD_EXPONENT, Value, Idx

    BValue	= BYTE(Value)

    Idx	= WHERE(BValue EQ 68b, Count)
    IF Count EQ 1 THEN RETURN, 1
    Idx	= WHERE(BValue EQ 69b, Count)
    IF Count EQ 1 THEN RETURN, 1
    Idx	= WHERE(BValue EQ 100b, Count)
    IF Count EQ 1 THEN RETURN, 1
    Idx	= WHERE(BValue EQ 101b, Count)
    IF Count EQ 1 THEN RETURN, 1
    RETURN, 0
END

;
;	Floating point number are even more complicated.
;	There are more invalid states available.
;	Currently, we are more lax.
;
PRO CW_FIELD_FLOAT, Ch, State, Event, Altered

    TextId	= State.TextId
    WIDGET_CONTROL, TextId, GET_VALUE=Value
    Value	= Value(0)

    IF Ch EQ 10b THEN BEGIN
	Value	= CW_FIELD_VALIDATE(Value)
	WIDGET_CONTROL, TextId, SET_VALUE=Value
	WIDGET_CONTROL, TextId, SET_TEXT_SELECT=[Strlen(Value),0]
	Altered	= 2
	RETURN
    ENDIF

	;	Unfortunately, we have a lot of invalid states
	;	possible that aren't a problem.
	;	We make sure of just a minimum number of
	;	things:
	;	One EeDd per field.
	;	One decimal point, before the exponent
	;	- sign must be 1st char or follow the exponent.

    Selection	= WIDGET_INFO(TextId, /TEXT_SELECT)

    IF Ch GE 48b AND Ch LE 57b THEN BEGIN
	WIDGET_CONTROL, TextId, SET_VALUE=STRING(Ch), /USE_TEXT_SELECT
    ENDIF ELSE BEGIN
	CASE Ch OF

	46b:	BEGIN
		;	Ignore it if there is one already
		;	New decimal point must precede exponent

	    Idx	= WHERE(BYTE(Value) EQ 46b, Count)
	    IF Count EQ 1 THEN RETURN
	    IF CW_FIELD_EXPONENT( Value, Idx ) THEN BEGIN
		IF Idx(0) LT Selection(0) THEN RETURN
	    ENDIF
	    WIDGET_CONTROL, TextId, SET_VALUE=STRING(Ch), /USE_TEXT_SELECT
	END
	43b:	BEGIN	; + must follow exponent
	    IF CW_FIELD_EXPONENT( Value, Idx ) THEN BEGIN
		IF Idx(0)+1 EQ Selection(0) THEN BEGIN
		    WIDGET_CONTROL, TextId, SET_VALUE='+', /USE_TEXT_SELECT
		ENDIF ELSE RETURN
	    ENDIF ELSE RETURN
	END
	45b:	BEGIN
	    HaveExp	= CW_FIELD_EXPONENT( Value, Idx )
	    IF  (HaveExp AND Idx(0)+1 EQ Selection(0)) OR $
		(Selection(0) EQ 0 AND STRMID(Value,0,1) NE "-") THEN BEGIN
		WIDGET_CONTROL, TextId, SET_VALUE='-', /USE_TEXT_SELECT
	    ENDIF ELSE RETURN
	END

	68b:	GOTO, Exponent
	69b:	GOTO, Exponent
	100b:	GOTO, Exponent
	101b:	BEGIN
	Exponent:

	;	Replace if one exists. Otherwise allow it anywhere
	;	AFTER the decimal point
	    IF CW_FIELD_EXPONENT( Value, Idx ) THEN BEGIN
		Selection	= [ Idx, 1 ]
		WIDGET_CONTROL, TextId, SET_TEXT_SELECT=Selection
	    ENDIF ELSE BEGIN
		Idx	= WHERE(BYTE(Value) EQ 46b, Count)
		IF Count EQ 1 THEN BEGIN
		    IF Selection(0) LE Idx(0) THEN RETURN
		ENDIF
	    ENDELSE
	    WIDGET_CONTROL, TextId, SET_VALUE=STRING(Ch), /USE_TEXT_SELECT
	END

	ELSE:	RETURN	; Bad
	ENDCASE
    ENDELSE



    Altered	= 1
    WIDGET_CONTROL, TextId, SET_TEXT_SELECT=[Selection(0)+1,0]
END

FUNCTION CW_FIELD_EVENT, Event

    StateHolder	= WIDGET_INFO(Event.Handler, /CHILD)
    WIDGET_CONTROL, StateHolder, GET_UVALUE=State, /NO_COPY

	;	At this point, we need to look at what kind of field
	;	we have:

    Altered	= 0

    ;	If the user has types <CR> then update field

    IF State.Type NE 0 THEN BEGIN		; Not a String?

	IF Event.Type LE 1 THEN BEGIN		; Insert Characters?

	    IF State.Type EQ 1 THEN Procedure='CW_FIELD_FLOAT' $
	    ELSE Procedure='CW_FIELD_INT'

	    Altered	= 0
	    IF Event.Type EQ 0 THEN BEGIN
		CALL_PROCEDURE, Procedure, Event.Ch, State, Event, Altered
	    ENDIF ELSE BEGIN
		Chars	= BYTE(Event.Str)
		FOR I=0,N_ELEMENTS(Chars)-1 DO $
		    CALL_PROCEDURE, Procedure, Chars(I), State, Event, Altered
	    ENDELSE

	ENDIF ELSE IF Event.Type EQ 2 THEN BEGIN	; Delete Text

	    IF Event.Length GT 0 THEN BEGIN ; Bug in widget
		WIDGET_CONTROL, State.TextId, $
			SET_TEXT_SELECT=[Event.Offset,Event.Length]
		WIDGET_CONTROL, State.TextId, SET_VALUE='', /USE_TEXT_SELECT

		;	See if user wants an update
	        Altered = 1
	    ENDIF

	ENDIF
    ENDIF ELSE BEGIN
	;	All delete/add char events effect the contents of
	;	a string. <CR> is considered special.
	IF Event.Type GE 0 AND Event.Type LE 2 THEN Altered	= 1
	IF Event.Type EQ 0 THEN $
	   Altered	= 1 + (Event.Ch EQ 10b)
    ENDELSE


    Ret	= 0

	;	If the entry has been modified or <CR> was hit
	;	And the user is interested in all event or
	;	Just <CR> AND <CR> was the cause of update then
	;	send it
    IF State.Update NE 0 AND $
       Altered GE State.Update THEN BEGIN

	WIDGET_CONTROL, State.TextId, GET_VALUE=Value
	RValue	= CW_FIELD_VALUE(Value, State.Type)

	Ret	= {			$
		ID: Event.Handler,	$
		TOP: Event.Top,		$
		HANDLER: 0L,		$
		VALUE: RValue,		$
		TYPE: State.Type,	$
		UPDATE: Altered - 1	$	; 0=any,1=CR
	}
    ENDIF

    ;	Restore our state structure
    WIDGET_CONTROL, StateHolder, SET_UVALUE=State, /NO_COPY
    RETURN, Ret
END


FUNCTION CW_FIELD, Parent, COLUMN=Column, ROW=Row, $
	FLOATING=Float, INTEGER=Int, LONG=Long, STRING=String, $
	FONT=LabelFont, FRAME=Frame, TITLE=Title, UVALUE=UValue, VALUE=Value, $
	RETURN_EVENTS=ReturnEvents, ALL_EVENTS=AllUpdates, $
	FIELDFONT=FieldFont, NOEDIT=NoEdit, TEXT_FRAME=TextFrame, $
	XSIZE=XSize, YSIZE=YSize
;	FLOOR=vmin, CEILING=vmax

	;	Examine our keyword list and set default values
	;	for keywords that are not explicitly set.

    Column		= KEYWORD_SET(Column)
    Row			= 1 - Column
    AllEvents		= 1 - KEYWORD_SET(NoEdit)

	; Enum Update { None, All, CRonly }
    Update		= 0
    IF KEYWORD_SET(AllUpdates) THEN Update	= 1
    IF KEYWORD_SET(ReturnEvents) THEN Update	= 2

    IF KEYWORD_SET(FieldFont) EQ 0 THEN FieldFont=''
    IF KEYWORD_SET(Frame) EQ 0 THEN Frame=0
    IF KEYWORD_SET(LabelFont) EQ 0 THEN LabelFont=''
    IF KEYWORD_SET(Title) EQ 0 THEN Title="Input Field:"
    IF N_Elements(value) EQ 0 THEN value=''
    IF KEYWORD_SET(UValue) EQ 0 THEN UValue=0
    IF KEYWORD_SET(XSize) EQ 0 THEN XSize=0
    IF KEYWORD_SET(YSize) EQ 0 THEN YSize=1

				Type	= 0	; string is default
    IF KEYWORD_SET(Float) THEN 	Type	= 1
    IF KEYWORD_SET(Int) THEN	Type	= 2
    IF KEYWORD_SET(Long) THEN	Type	= 3

	;	Don't allow multiline non string widgets
    IF KEYWORD_SET(YSize) EQ 0 OR Type NE 0 THEN YSize=1
    TextFrame	= KEYWORD_SET( TextFrame )

	;	Build Widget

    Base	= WIDGET_BASE(Parent, ROW=Row, COLUMN=Column, UVALUE=UValue, $
			EVENT_FUNC='CW_FIELD_EVENT', $
			PRO_SET_VALUE='CW_FIELD_SET', $
			FUNC_GET_VALUE='CW_FIELD_GET', $
			FRAME=Frame )
    Label	= WIDGET_LABEL(Base, VALUE=Title, FONT=LabelFont)
    Text	= WIDGET_TEXT(Base, VALUE=STRTRIM(Value,2), $
			XSIZE=XSize, YSIZE=YSize, FONT=FieldFont, $
			ALL_EVENTS=AllEvents, $
			EDITABLE=(AllEvents AND TYPE EQ 0), $
			FRAME=TextFrame )

			; NO_ECHO=(AllEvents AND (TYPE NE 0)))

	; Save our internal state in the first child widget
    State	= {		$
	TextId:Text,		$
	Title:Title,		$
	Update:Update,		$
	Type:Type		$
    }
    WIDGET_CONTROL, WIDGET_INFO(Base, /CHILD), SET_UVALUE=State, /NO_COPY
    RETURN, Base
END
