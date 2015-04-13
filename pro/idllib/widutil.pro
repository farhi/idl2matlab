;
; $Id: widutil.pro,v 1.8 1994/06/01 23:08:48 ali Exp $
;
;  WidUtil
;   Miscellaneous Utility functions and procedures
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;



;
;  Event handler loop for Error dialog box
;
PRO ErrorEvent, Event

    ; The only event possible is a 'Done' so we don't bother
    ; to check, just bring the dialog down

    WIDGET_CONTROL, Event.top, /DESTROY
END


;
;  ErrorDialog
;       Create a dialog box and put an error message in it
;   Message can be a string or an array of strings
;
PRO ErrorDialog, Parent, Msg

    ;   Position the error dialog on top of its parent

    WIDGET_CONTROL, Parent, TLB_GET_OFFSET=Off
    Base    = WIDGET_BASE(/COLUMN, TITLE='ERROR!', $
                        XOFFSET=Off(0)+50, YOFFSET=Off(1)+50)

    ;   Add a label(line) for each line of the message
    FOR I=1,N_ELEMENTS(Msg) DO BEGIN
        Label   = WIDGET_LABEL(base,VALUE=Msg(I-1))
    ENDFOR

    ;   Make an acknowledge button for the user to press
    ;   We move it over (looks better).  Of course the fixed
    ;   offset stuff is not the best solution but seems to work
    Ok      = WIDGET_BUTTON(Base, VALUE='  OK  ', XOFF=70)  ; Bad?

    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, 'WidError', Base, /MODAL, EVENT_HANDLER='ErrorEvent'
END

;
;  Qstring(String)
;   Return a string which can be included in single quotes. That is.
;   double every single quote. E.g. QString("Do's 'n Don'ts") returns
;   Do''s ''n Don''ts.  Note that there is a bug in the IDL parser
;   which barfs on leading single quotes: '''' is a syntax error
;
;   Side Effect: Due to the nature of IDL variable passing, if
;   the string passed in is a named variable it will be altered
;
FUNCTION Qstring, String

    Len         = STRLEN(String)+1                  ; Starting length
    PrevQuote   = 0                                 ; Previous quote pos
    Quote       = STRPOS(String, "'", PrevQuote)    ; Current quote pos

    WHILE Quote NE -1 DO BEGIN                          ; given xx'yy
        Front       = STRMID(String,0,Quote+1)          ;   Front = xx'
        Back        = STRMID(String,Quote, Len - Quote) ;-) Back  = 'yy
        String      = Front + Back                      ; xx' + 'yy
        Len         = Len + 1                           ; string got longer

        PrevQuote   = Quote + 2                         ; quote is not previous quote
        Quote       = STRPOS(String, "'", PrevQuote)
    ENDWHILE
    RETURN, String
END


;
;  ClearVar
;   Reset a variable to <UNDEFINED> if it isn't already.
;
PRO ClearVar, Var
    IF N_ELEMENTS(Var) NE 0 THEN Dummy  = TEMPORARY(Var)
END


;
;  DoList
;       Given the pointer to a list of objects, perform a FIXED
;   function on each object in the list of the form:
;               Procstr,Ptr
;
PRO DoList, Ptr, ProcStr

    WHILE Ptr NE 0L DO BEGIN
        Next    = NextPtr(Ptr)
        Dummy   = EXECUTE(ProcStr + ",Ptr")
        Ptr     = Next
    ENDWHILE
END

;
;  DoFList
;       Given the pointer to a list of objects, perform an I/O
;   function on each object in the list of the form:
;               Procstr,Unit,Ptr
;
PRO DoFList, Ptr, ProcStr, Unit

    WHILE Ptr NE 0L DO BEGIN
        Next    = NextPtr(Ptr)
        Dummy   = EXECUTE(ProcStr + ",Unit,Ptr")
        Ptr     = Next
    ENDWHILE
END

;
;  DoFList2
;       Given the pointer to a list of objects, perform an I/O
;   function on each object in the list of the form:
;               Procstr,Unit1,Unit2,Ptr
;
PRO DoFList2, Ptr, ProcStr, Unit1, Unit2

    WHILE Ptr NE 0L DO BEGIN
        Next    = NextPtr(Ptr)
        Dummy   = EXECUTE(ProcStr + ",Unit1,Unit2,Ptr")
        Ptr     = Next
    ENDWHILE
END


;
;  GetType
;   Get the Type field out of an object.
;
PRO GetType, Ptr, Type
    Ptr2Obj, Ptr, Obj
    Type    = Obj.Type
    Obj2Ptr, Obj, Ptr
END


;
;  SetTag
;       Set an arbitrary field in an object given a pointer
;       to the object, the tag and its new value
;
PRO SetTag, Ptr, Tag, Value
    Ptr2Obj, Ptr, Obj
    Dummy       = EXECUTE("Obj."+ Tag + "= Value")
    Obj2Ptr, Obj, Ptr
END


;
;  NewId
;   Create a new name for an object
;
FUNCTION NewId

  COMMON WidEd_Comm

    New         = STRTRIM(LastId,2)
    LastId      = LastId + 1
    RETURN, New
END


;
;  VarId
;   Return the logical name of an object
;
FUNCTION VarId, Ptr
    Ptr2Obj, Ptr, Obj
    VarName = Obj.Type + Obj.Id
    Obj2Ptr, Obj, Ptr
    RETURN, VarName
END


;
;  GetId
;   Return what we think would be the best symbolic name for an object
;   This is either: the name the user gave it, its value(title) or its
;   logical name
;
FUNCTION GetId, Ptr

  COMMON WidEd_Comm

    IF Ptr EQ TopPtr THEN RETURN, 'Top Base'

    Ptr2Obj, Ptr, Obj

    IF Obj.Name NE '' THEN BEGIN
        Id  = Obj.Name
        Obj2Ptr, Obj, Ptr
        RETURN, Id
    ENDIF

    IF (Obj.Type EQ 'LABEL' OR Obj.Type EQ 'BUTTON') THEN BEGIN
        IF Obj.Value NE '' THEN BEGIN
            Id  = Obj.Value
            Obj2Ptr, Obj, Ptr
            RETURN, Id
        ENDIF
    ENDIF

    IF Obj.Type EQ 'FIELD' OR Obj.Type EQ 'SLIDER' OR $
       Obj.Type EQ 'FSLID' THEN BEGIN

        IF Obj.Title NE '' THEN BEGIN
            Id  = Obj.Title
            Obj2Ptr, Obj, Ptr
            RETURN, Id
        ENDIF
    ENDIF

    IF Obj.UValue NE '' THEN    Id = Obj.UValue $
    ELSE                        Id = Obj.Type + Obj.Id
    Obj2Ptr, Obj, Ptr
    RETURN, Id
END


;
;  UValue
;   If the user has not provided a UVALUE for an object we do so that
;   we can write an event handler.
;
FUNCTION UValue, Obj, Ptr
    IF Obj.UValue NE '' THEN RETURN, Obj.UValue
    RETURN, Obj.Type + Obj.Id
END


;
;  HasChildren
;   Returns TRUE if the object has children or is a base object
;   and has no children but thats OK. Otherwise return FALSE.
;
FUNCTION HasChildren, Ptr, NONE_OK=NoneOk

    ;   Bad pointers don't have children
    IF WIDGET_INFO(Ptr, /VALID_ID) EQ 0 THEN RETURN, 0

    Ptr2Obj, Ptr, Obj
    Name    = TAG_NAMES(Obj, /STRUCTURE)

    ;   Only Base objects can have children (so far)
    IF Name EQ 'WE_BASE' THEN BEGIN

        ; Actually has children or could have children but thats enough?
        IF Obj.Children NE 0 OR KEYWORD_SET(NoneOk) THEN BEGIN
            Obj2Ptr, Obj, Ptr
            RETURN, 1
        ENDIF
    ENDIF

    ;   Have a base object but it has no children and NoneOk is false

    Obj2Ptr, Obj, Ptr
    RETURN, 0
END


;
;  Dirty_Event
;   Event handler for the asking the user Dirty dialog (see below)
;
PRO Dirty_Event, Event

COMMON  WidDirty_Comm, DoCall

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev

    ;   Save First?
    IF Ev EQ "Yes" THEN FileSave

    ;   Do we want to do whatever it is that we were asking about
    ;   saving before doing? We do for Yes or No but not Cancel
    DoCall  = (Ev NE "Cancel")

    ;   Done
    WIDGET_CONTROL, Event.Top, /DESTROY
END


;
;  Dirty
;   Give a user a chance to save changes before destroying the
;   object tree.  A 'Do you want to save the object tree before
;   doing XXX?' failsafe.
;
;   The way this works might be a tad confusing.
;   Run the widget builder, add some widget and hit 'Quit'.
;   That will make this a lot more understandable.
;
PRO Dirty, Parent, Thing, Call

COMMON  WidDirty_Comm, DoCall

  COMMON WidEd_Comm    

    ;   If there is no chance of loosing data then just do it

    IF Dirty EQ 0 THEN Dummy=EXECUTE(Call) $
    ELSE BEGIN

        ;   Position the 'Wanna do <Thing>?' dialog on top of parent
        WIDGET_CONTROL, Parent, TLB_GET_OFFSET=Off
        Base    = WIDGET_BASE(/COLUMN, XOFFSET=Off(0)+50, YOFFSET=Off(1)+50)

        ;   Build question
        Label   = WIDGET_LABEL(Base, VALUE="Save changes before");
        Label   = WIDGET_LABEL(Base, VALUE=Thing+"?");

        ;   Build possible answers
        Base1   = WIDGET_BASE(Base, /ROW)
        Btn     = WIDGET_BUTTON(Base1, VALUE="  Yes  ", UVALUE="Yes")
        Btn     = WIDGET_BUTTON(Base1, VALUE="   No  ", UVALUE="No")
        Btn     = WIDGET_BUTTON(Base1, VALUE=" Cancel", UVALUE="Cancel")

        ;   Wait for user to answer your question
        WIDGET_CONTROL, Base, /REALIZE
        XMANAGER, 'WidError', Base, /MODAL, EVENT_HANDLER='Dirty_Event'

        ;   Event handler will set DoCall to TRUE if the user wants
        ;   to do whatever it is (Call).  Have to do this here to prevent
        ;   XMANAGER MODAL looping bug.

        IF DoCall THEN Dummy = EXECUTE(Call)
    ENDELSE
END


;
;  SAddCmd
;   Append a string keyword to a command string. Only append
;   keyword if value is not the null string ('') or the FORCE
;   keyword is set
;
;   E.g.
;       Cmd='WidCre(XXX' & SAddCmd, Cmd, "Hello", "VALUE"
;       HELP,Cmd
;   CMD STRING  = "WidCre(XXX,VALUE='Hello'"
;
PRO SAddCmd, Cmd, Value, Keyword, FORCE=Force
    IF Value NE '' OR KEYWORD_SET(FORCE) THEN $
        Cmd = Cmd + ',' + Keyword + "='" + QString(Value) + "'"
END


;
;  IAddCmd
;   Same as SAddCmd but for integer values. Only appends keyword
;   if value is non-zero or FORCE keyword set.
;
PRO IAddCmd, Cmd, Value, Keyword, FORCE=Force
    IF Value NE 0 OR KEYWORD_SET(FORCE) THEN $
        Cmd = Cmd + ',' + Keyword + '=' + STRTRIM(Value,2)
END


;
;  SetFocus
;   Set the focus to the given widget.  Id should be either:
;   a TEXT widget or a compound widget (base) whose first text object
;   in it is what should receive the keyboard focus
;
PRO SetFocus, Id

    IF WIDGET_INFO(Id, /TYPE) EQ 3 THEN BEGIN   ; Text Widget?
        TextId  = Id
    ENDIF ELSE BEGIN
        TextId  = WIDGET_INFO(Id, /CHILD)       ; Assume Base

        ;   Hunt through children looking for first text widget
        WHILE TextId NE 0 AND WIDGET_INFO(TextId,/TYPE) NE 3 DO BEGIN
            TextId  = WIDGET_INFO(TextId, /SIBLING)
        ENDWHILE
    ENDELSE

    WIDGET_CONTROL, TextId, /INPUT_FOCUS        ; Set keyboard focus
END


;
;  SetNextFocus
;   If the user has hit <CR> (event.update will be TRUE) and we
;   can find the current focus then determine the next focus and
;   give it the keyboard focus.
;
PRO SetNextFocus, Binfo, Event

    ;   Look for the current id in our list of known foci

    Current        = WHERE(BInfo.Foci EQ Event.Id, Count)
    IF Count EQ 1 THEN BEGIN

        ;   Did user hit <CR>? Goto next focus if they did
        IF Event.Update THEN BEGIN
            ;   Next is a relative term (wrap from last to first)
            Current = (Current(0) + 1) MOD N_ELEMENTS(BInfo.Foci)
            SetFocus, BInfo.Foci(Current)
        ENDIF
    ENDIF
END


;
;  GetValue
;   Given an object with an text field for a value (STRARR)
;   get that value.  If the value is nil then use the default (NoName)
;   value instead: Returning <UNDEFINED> is not good.
;
PRO GetValue, Obj, Names, NoName

;   (c.f. BuildEdit in widbuild.pro)

;   IF Obj.ValueType EQ 0 THEN BEGIN

        Ptr2Obj, Obj.Value1, Names, /COPY
        IF N_ELEMENTS(Names) EQ 0 THEN Names=NoName

;   ENDIF ELSE BEGIN
;       Unsupportable.
;       Names = '<User Code>'
;   ENDELSE
END

PRO WidUtil
END
