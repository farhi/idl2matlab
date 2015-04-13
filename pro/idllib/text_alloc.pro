;
; $Id: text_alloc.pro,v 1.8 1995/01/20 19:41:01 tonyh Exp $
;
;  WidText
;   Widget Text class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  TEXT_Icon
;       Return the text toolbar icon
;
FUNCTION TEXT_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 128b, 51b ], $
    [ 12b, 14b, 3b, 49b ], $
    [ 12b, 31b, 3b, 49b ], $
    [ 140b, 49b, 3b, 49b ], $
    [ 140b, 49b, 3b, 49b ], $
    [ 140b, 49b, 3b, 49b ], $
    [ 140b, 49b, 59b, 49b ], $
    [ 140b, 63b, 127b, 49b ], $
    [ 140b, 63b, 103b, 49b ], $
    [ 140b, 49b, 99b, 49b ], $
    [ 140b, 49b, 103b, 49b ], $
    [ 140b, 49b, 127b, 49b ], $
    [ 140b, 49b, 59b, 49b ], $
    [ 12b, 0b, 0b, 49b ], $
    [ 12b, 0b, 128b, 51b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END

;
;  TEXT_Copy
;   Copy a text.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO TEXT_Copy, Ptr, Copy
    GenCopy, Ptr, Copy, /HASVALUE
END


;
;  TEXT_Destroy
;   Release resources for the given text widget.
;
PRO TEXT_Destroy, Ptr
    GenDestroy, Ptr, /HASVALUE
END


;
;  TEXT_Event
;   Event handling routine for text dialog
;
PRO TEXT_Event, Event

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev                 ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY   ; Get Dialog Info
    Ptr2Obj, Binfo.ObjPtr, Obj                              ; Get Object

    CASE Ev OF

    'LITERAL':      BEGIN                   ; UNIMPLEMENTED (IDL v. Literal)
        Obj.ValueType       = 0
        WIDGET_CONTROL, Binfo.EditId, SENSITIVE=0
        END
    'CODEBASED':    BEGIN                   ; UNIMPLEMENTED (IDL v. Literal)
        Obj.ValueType       = 1
        WIDGET_CONTROL, Binfo.EditId, SENSITIVE=1
        END

    'MAINTEXT': WIDGET_CONTROL, Obj.Value1, SET_UVALUE=Event.Value
    'VALUETEXT':Obj.Value2      = Event.Value   ; UNIMPLEMENTED

    'FONT':     Obj.Font        = Event.Value
    'NAME':     Obj.Name        = Event.Value   ; Other
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value
    'XSIZE':    Obj.XSize       = Event.Value   ; XY Stuff
    'YSIZE':    Obj.YSize       = Event.Value
    'XOFFSET':  Obj.XOffset     = Event.Value
    'YOFFSET':  Obj.YOffset     = Event.Value

    'ALLEV':    Obj.AllEvent    = 1 - Obj.AllEvent
    'EDIT':     Obj.TextEdit    = 1 - Obj.TextEdit
    'NO_NL':    Obj.TextNoNL    = 1 - Obj.TextNoNL
    'SCROLLS':  Obj.TextScrolls = 1 - Obj.TextScrolls

    'XFONT':    DoXFont, Obj, Binfo.Foci(1)

    'DONE':     BEGIN
        Accept, Obj, Binfo.ObjPtr
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        WIDGET_CONTROL, Event.Top, /DESTROY
        RETURN
        END

    'CANCEL':   BEGIN
        Cancel, Obj, Binfo.ObjPtr
        RETURN
        END
    ELSE:           MESSAGE, 'Unprocessed event: ' + Ev
    ENDCASE

    Dirty   = 1     ; We've changed something since the last save

    SetNextFocus, Binfo, Event      ; Set next keyboard focus as necessary
    Obj2Ptr, Obj, Binfo.ObjPtr      ; Put object back into pointer
    WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  TEXT_Build
;   Create a dialog box a text object.  If ptr is nil then
;   create the object as well.
;
PRO TEXT_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    TEXT_Alloc, ParPtr, Ptr                 ; Allocate object if necessary
    MgrName = 'WE_TEXT' + STRTRIM(Ptr, 2)   ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if it already exists

    Title   = GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'
    Ptr2Obj, Ptr, Obj

    ;   Create dialog box

    IF SmallScreen(0) NE 0 THEN BEGIN
        Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg, $
                        X_SCROLL_SIZE=SmallScreen(0), $
                        Y_SCROLL_SIZE=SmallScreen(1) )
    ENDIF ELSE BEGIN
        Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)
    ENDELSE
    Foci    = LONARR(8)

    Base1   = WIDGET_BASE(Base, /COLUMN, /FRAME)
    ; Lab       = WIDGET_LABEL(Base1, VALUE="Basic Information")
    BuildEdit, Base1, Obj, _EditId
    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(0) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50,/STR)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")

    Base2   = WIDGET_BASE(Base1, COLUMN=3, /NONEXCLUSIVE)
    Button  = WIDGET_BUTTON(Base2, VALUE='Text Editable', UVALUE='EDIT')
    IF Obj.TextEdit THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='No Auto-Newline', UVALUE='NO_NL')
    IF Obj.TextNoNL THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Add Scrollbars', UVALUE='SCROLLS')
    IF Obj.TextScrolls THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Process All Events',UVALUE='ALLEV')
    IF Obj.AllEvent THEN WIDGET_CONTROL, Button, /SET_BUTTON

    BuildOther, Base1, Obj, Foci, 1, /FRAME

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Text Appearance Controls")
    BuildXY, Base1, Obj, Foci, 4, /SIZE, /OFFSET
    BuildOkCancel, Base, Obj

    DlgInfo     = {             $
        Foci:       Foci,       $
        EditId:     _EditId,    $
        ObjPtr:     Ptr         $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='TEXT_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  TEXT_Save
;   Save text information to a file.
;   Store value as well: If value is nil, make up a value.
;
;   FORMAT:
;       <text object>
;       <number of elements in value>
;       <value>
;
PRO TEXT_Save, Unit, Ptr
    GenWrite, Unit, Ptr, DEFAULT=''
END


;
;  TEXT_Restore
;   Read in a text object from a file
;
PRO TEXT_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "TEXT", 1
END


;
;  TEXT_Generate
;   Create a text object for previewing
;
PRO TEXT_Generate, Base, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    GetValue, Obj, Names, ""        ; Get Value (or use default)

    ;   Generate command string

    Cmd = 'Id = WIDGET_TEXT(Base,VALUE=Names'
    IF Obj.TextEdit THEN Cmd = Cmd + ',/EDITABLE'
    SAddCmd, Cmd, Obj.Font, 'FONT'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IF Obj.TextNoNL THEN Cmd = Cmd + ',/NO_NEWLINE'
    IF Obj.TextScrolls THEN Cmd = Cmd + ',/SCROLL'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'

    ;   Automatically make text big enough if the user
    ;   doesn't specify Ysize
    IF Obj.YSize EQ 0 THEN BEGIN
        Cmd = Cmd + ',YSIZE='+STRTRIM(N_ELEMENTS(Names),2)
    ENDIF

    Obj2Ptr, Obj, Ptr

    ; Create text widget by executing the command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create text ' + VarName(Ptr)
    ENDIF
END


;
;  TEXT_GenWid
;   Create IDL code for creating a text widget
;
PRO TEXT_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)                ; Get variable name of object
    Ptr2Obj, Ptr, Obj                   ; Get object info

    TextName    = 'TextVal' + STRTRIM(Ptr,2)    ; Create value name
    SaveStr, Unit, Ptr, Obj, TextName, ""       ; Generate value code

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_TEXT( ",A,",VALUE=",A)', $
        Name, Parent, TextName, /NO_EOL
    IF Obj.AllEvent THEN ISaveCmd, Unit, 1, "ALL_EVENTS"
    ISaveCmd, Unit, Obj.TextEdit, "EDITABLE"
    SSaveCmd, Unit, Obj.Font, "FONT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    ISaveCmd, Unit, Obj.TextNoNL, "NO_NEWLINE"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"

    ;   Automatically make text big enough if the user
    ;   doesn't specify Ysize

    IF Obj.YSize EQ 0 THEN BEGIN
        GetValue, Obj, Names, ""
        ISaveCmd, Unit, N_ELEMENTS(Names), "YSIZE"
    ENDIF

    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  TEXT_Alloc
;       Allocate a text object.  Don't allocate if ptr is non-nil
;
PRO TEXT_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr     = WIDGET_BASE(GROUP=TopDlg)    ; Make a pointer
    ValueId = WIDGET_BASE(GROUP=TopDlg)    ; Make a pointer for the value too

    ;   Make a Text object

    Obj     = {                 $
        WE_TEXT,                $
        Type:           'TEXT',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; Title or object name
        FrameSize:      0,      $
        Font:           '',     $
        UValue:         '',     $
        Value1:         ValueId,$
        Value2:         '',     $ ; UNIMPLEMENTED and unused
        ValueType:      0,      $ ; UNIMPLEMENTED and unused
        XSize:          0,      $
        YSize:          0,      $
        XOffset:        0,      $
        YOffset:        0,      $
        AllEvent:       0,      $ ; ALL_EVENT flag
        TextEdit:       1,      $ ; Set if can edit text (default TRUE)
        TextNoNL:       0,      $ ; Suppress auto Newline?
        TextScrolls:    0       $ ; Widget has scroll bars?
    }
    Obj2Ptr, Obj, Ptr                       ; Store object in pointer
END
