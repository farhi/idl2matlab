;
; $Id: field_alloc.pro,v 1.8 1995/01/20 19:41:01 tonyh Exp $
;
;  WidField
;   Widget Field class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  FIELD_Icon
;       Return the cw_field toolbar icon
;
FUNCTION FIELD_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 254b, 255b, 255b, 127b ], $
    [ 254b, 255b, 255b, 127b ], $
    [ 6b, 0b, 0b, 96b ], $
    [ 6b, 0b, 0b, 110b ], $
    [ 6b, 0b, 0b, 100b ], $
    [ 6b, 3b, 8b, 100b ], $
    [ 6b, 3b, 8b, 100b ], $
    [ 6b, 3b, 8b, 100b ], $
    [ 134b, 6b, 8b, 100b ], $
    [ 134b, 6b, 8b, 100b ], $
    [ 134b, 134b, 9b, 100b ], $
    [ 70b, 140b, 233b, 100b ], $
    [ 198b, 15b, 24b, 101b ], $
    [ 70b, 12b, 8b, 101b ], $
    [ 38b, 152b, 9b, 101b ], $
    [ 38b, 152b, 9b, 101b ], $
    [ 38b, 24b, 24b, 101b ], $
    [ 118b, 60b, 232b, 100b ], $
    [ 6b, 0b, 0b, 100b ], $
    [ 6b, 0b, 0b, 110b ], $
    [ 6b, 0b, 0b, 96b ], $
    [ 6b, 0b, 0b, 96b ], $
    [ 254b, 255b, 255b, 127b ], $
    [ 254b, 255b, 255b, 127b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  FIELD_Copy
;   Copy a field widget.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO FIELD_Copy, Ptr, Copy
    GenCopy, Ptr, Copy, /HASVALUE
END


;
;  FIELD_Destroy
;   Release resources for the given field
;
PRO FIELD_Destroy, Ptr
    GenDestroy, Ptr, /HASVALUE
END


;
;  FIELD_Event
;   Event handling routine for a field dialog.
;
PRO FIELD_Event, Event

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

    'TITLE':    Obj.Title       = Event.Value
    'FONT':     Obj.TitleFont   = Event.Value
    'TEXTFONT': Obj.TextFont    = Event.Value
    'NAME':     Obj.Name        = Event.Value   ; Other
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value
    'XSIZE':    Obj.XSize       = Event.Value   ; XY Stuff
    'YSIZE':    Obj.YSize       = Event.Value

    'STRING':   Obj.FieldType   = 0
    'FLOAT':    Obj.FieldType   = 1
    'INT':      Obj.FieldType   = 2
    'LONG':     Obj.FieldType   = 3

    'NONE':     Obj.FieldEv     = 0
    'CR_EV':    Obj.FieldEv     = 1
    'ALL_EV':   Obj.FieldEv     = 2
    'NOEDIT':   Obj.FieldNoEdit = 1 - Obj.FieldNoEdit
    'ISCOL':    Obj.FieldVertical   = 1 - Obj.FieldVertical

    'XFONT':    BEGIN
        WIDGET_CONTROL, /HourGlass
        NewFontName = XFont()
        IF NewFontName NE '' THEN BEGIN
            WIDGET_CONTROL, Binfo.Foci(1), SET_VALUE=NewFontName
            Obj.TitleFont   = NewFontName
        ENDIF
        END
    'XTFONT':   BEGIN
        WIDGET_CONTROL, /HourGlass
        NewFontName = XFont()
        IF NewFontName NE '' THEN BEGIN
            WIDGET_CONTROL, Binfo.Foci(2), SET_VALUE=NewFontName
            Obj.TextFont    = NewFontName
        ENDIF
        END

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
;  FIELD_Build
;   Create a dialog box a field object.  If ptr is nil then
;   create the object as well.
;
PRO FIELD_Build, Ptr, ParPtr
  COMMON WidEd_Comm

    FIELD_Alloc, ParPtr, Ptr
    MgrName = 'WE_FIELD' + STRTRIM(Ptr, 2)
    IF XRegistered(MgrName) THEN RETURN

    Title   = GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'

    Ptr2Obj, Ptr, Obj
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
    BuildEdit, Base1, Obj, _EditId, /SINGLE

    Foci(0) = Field(Base1, "Title:", Obj.Title, "TITLE", SIZE=50, /STRING)
    Btns    = LONARR(4)

    Base2   = WIDGET_BASE(Base1, /ROW)
    Label   = WIDGET_LABEL(Base2, VALUE="Field Type:")
    Base3   = WIDGET_BASE(Base2, /ROW, /EXCLUSIVE)
    Btns(0) = WIDGET_BUTTON(Base3, VALUE='String', UVALUE='STRING')
    Btns(1) = WIDGET_BUTTON(Base3, VALUE='Float', UVALUE='FLOAT')
    Btns(2) = WIDGET_BUTTON(Base3, VALUE='Int', UVALUE='INT')
    Btns(3) = WIDGET_BUTTON(Base3, VALUE='Long', UVALUE='LONG')
    WIDGET_CONTROL, Btns(Obj.FieldType), /SET_BUTTON

    Base2   = WIDGET_BASE(Base1, /ROW)
    Label   = WIDGET_LABEL(Base2, VALUE="Field Events:")
    Base3   = WIDGET_BASE(Base2, /ROW, /EXCLUSIVE)
    Btns(0) = WIDGET_BUTTON(Base3, VALUE='None', UVALUE='NONE')
    Btns(1) = WIDGET_BUTTON(Base3, VALUE='Event on <CR>', UVALUE='CR_EV')
    Btns(2) = WIDGET_BUTTON(Base3, VALUE='All Events', UVALUE='ALL_EV')
    WIDGET_CONTROL, Btns(Obj.FieldEv), /SET_BUTTON

    Base2   = WIDGET_BASE(Base1, /ROW, /NONEXCLUSIVE)
    Button  = WIDGET_BUTTON(Base2, VALUE='Field Text Fixed', UVALUE='NOEDIT')
    IF Obj.FieldNoEdit THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Field is Vertical', UVALUE='ISCOL')
    IF Obj.FieldVertical THEN WIDGET_CONTROL, Button, /SET_BUTTON

    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(1) = Field(Base2, "Title Font:", Obj.TitleFont, 'FONT', SIZE=50,/STR)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")

    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(2) = Field(Base2, "Text Font:", Obj.TextFont, 'TEXTFONT',SIZE=50,/STR)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XTFONT")

    BuildOther, Base1, Obj, Foci, 3

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Field Appearance Controls")
    BuildXY, Base1, Obj, Foci, 6, /SIZE
    BuildOkCancel, Base, Obj

    DlgInfo     = {             $
        Foci:       Foci,       $
        EditId:     _EditId,    $
        ObjPtr:     Ptr         $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='FIELD_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  FIELD_Save
;   Save field information to a file.
;   Store value as well: If value is nil, make up a value.
;
;   FORMAT:
;       <field object>
;       <number of elements in value>
;       <value>
;
PRO FIELD_Save, Unit, Ptr
    GenWrite, Unit, Ptr, DEFAULT=""
END


;
;  FIELD_Restore
;   Read in a field object from a file
;
PRO FIELD_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "FIELD", 1
END


;
;  FIELD_Generate
;   Create a field object for previewing
;
PRO FIELD_Generate, Base, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Generate command string

    GetValue, Obj, Names, ""
    Cmd = 'Id = CW_FIELD(Base,VALUE=Names'
    IF Obj.FieldVertical THEN Cmd = Cmd + ',/COLUMN'
    IF Obj.FieldNoEdit THEN Cmd = Cmd + ',/NOEDIT'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    SAddCmd, Cmd, Obj.TitleFont, 'FONT'
    SAddCmd, Cmd, Obj.TextFont, 'FIELDFONT'
    SAddCmd, Cmd, Obj.Title, 'TITLE'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'

    Types   = [ 'STRING', 'FLOAT', 'INTEGER', 'LONG']
    Cmd = Cmd + ',/' + Types(Obj.FieldType)

    Obj2Ptr, Obj, Ptr

    ; Create field widget

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create field ' + VarName(Ptr)
    ENDIF

END


;
;  FIELD_GenWid
;   Create IDL code for creating a field widget
;
PRO FIELD_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)                        ; Get variable name of object
    Ptr2Obj, Ptr, Obj                           ; Get object info

    FieldName   = 'FieldVal' + STRTRIM(Ptr,2)   ; Create value name
    SaveStr, Unit, Ptr, Obj, FieldName, ""      ; Generate value code

    XPRINTF, Unit, FORMAT='("  ",A," = CW_FIELD( ",A,",VALUE=",A)', $
        Name, Parent, FieldName, /NO_EOL
    IF Obj.FieldVertical THEN ISaveCmd, Unit, 1, "COLUMN" $
    ELSE ISaveCmd, Unit, 1, "ROW"

    Types   = [ 'STRING', 'FLOAT', 'INTEGER', 'LONG']
    ISaveCmd, Unit, 1, Types(Obj.FieldType)

    IF Obj.FieldEv EQ 1 THEN ISaveCmd, Unit, 1, "RETURN_EVENTS"
    IF Obj.FieldEv EQ 2 THEN ISaveCmd, Unit, 1, "ALL_EVENTS"

    SSaveCmd, Unit, Obj.TextFont, "FIELDFONT"
    SSaveCmd, Unit, Obj.TitleFont, "FONT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    SSaveCmd, Unit, Obj.Title, "TITLE"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  FIELD_Alloc
;       Allocate a field object.  Don't allocate if ptr is non-nil
;
PRO FIELD_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr     = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer
    ValueId = WIDGET_BASE(GROUP=TopDlg)

    Obj     = {                 $
        WE_FIELD,               $
        Type:           'FIELD',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; Title or object name
        Title:          '',     $
        FrameSize:      0,      $
        TitleFont:      '',     $
        TextFont:       '',     $
        UValue:         '',     $
        Value1:         ValueId,$
        Value2:         '',     $ ; UNIMPLEMENTED and unused
        ValueType:      0,      $ ; UNIMPLEMENTED and unused
        XSize:          0,      $
        YSize:          0,      $
        FieldType:      0,      $ ; Enum { String,Float,Int,Long }
        FieldEv:        0,      $ ; Enum { None,<CR>,All }
        FieldNoEdit:    0,      $ ; Set if CANT edit field
        FieldVertical:  0       $ ; Enum { Row,Column }
    }
    Obj2Ptr, Obj, Ptr
END
