;
; $Id: list_alloc.pro,v 1.7 1995/01/20 19:41:01 tonyh Exp $
;
;  WidList
;   Widget List class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  LIST_Icon
;       Return the list toolbar icon
;
FUNCTION LIST_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 4b, 0b, 64b, 47b ], $
    [ 228b, 207b, 199b, 63b ], $
    [ 228b, 207b, 199b, 63b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 228b, 255b, 65b, 38b ], $
    [ 228b, 255b, 65b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 252b, 255b, 127b, 38b ], $
    [ 252b, 255b, 127b, 38b ], $
    [ 28b, 96b, 120b, 38b ], $
    [ 28b, 96b, 120b, 38b ], $
    [ 252b, 255b, 127b, 38b ], $
    [ 252b, 255b, 127b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 228b, 255b, 67b, 38b ], $
    [ 228b, 255b, 67b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 228b, 255b, 207b, 63b ], $
    [ 228b, 255b, 207b, 63b ], $
    [ 4b, 0b, 64b, 47b ], $
    [ 4b, 0b, 64b, 38b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  LIST_Copy
;   Copy a list.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO LIST_Copy, Ptr, Copy
    GenCopy, Ptr, Copy, /HASVALUE
END


;
;  TEXT_Destroy
;   Release resources for the given list widget.
;
PRO LIST_Destroy, Ptr
    GenDestroy, Ptr, /HASVALUE
END


;
;  LIST_Event
;   Event handling routine for list dialog
;
PRO LIST_Event, Event

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
    'YSIZE':    Obj.YSize       = Event.Value   ; XY Stuff. Note: No X Size
    'XOFFSET':  Obj.XOffset     = Event.Value
    'YOFFSET':  Obj.YOffset     = Event.Value

    'XFONT':    DoXFont, Obj, Binfo.Foci(1)

    'DONE':     BEGIN
        Accept, Obj, Binfo.ObjPtr
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        WIDGET_CONTROL, Event.Top, /DESTROY
        RETURN
        END

    'CANCEL':   BEGIN
        Cancel, Obj, Binfo.ObjPtr
        ; WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        ; WIDGET_CONTROL, Event.Top, /DESTROY
        RETURN
        END
    ELSE:           MESSAGE, 'Unprocessed event: ' + Ev
    ENDCASE

    Dirty   = 1

    SetNextFocus, Binfo, Event      ; Set next keyboard focus as necessary
    Obj2Ptr, Obj, Binfo.ObjPtr      ; Put object back into pointer
    WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  LIST_Build
;   Create a dialog box a list object.  If ptr is nil then
;   create the object as well.
;
PRO LIST_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    LIST_Alloc, ParPtr, Ptr                 ; Allocate object if necessary
    MgrName = 'WE_LIST' + STRTRIM(Ptr, 2)   ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if it already exists

    Title   = GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'
    Ptr2Obj, Ptr, Obj

    ;   Create dialog box

    Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)
    Foci    = LONARR(7)

    Base1   = WIDGET_BASE(Base, /COLUMN, /FRAME)
    Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
    BuildEdit, Base1, Obj, _EditId

    Foci(0) = Field(Base1, "Y Size:", Obj.YSize, 'YSIZE', SIZE=8, /INT)
    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(1) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")

    BuildOther, Base1, Obj, Foci, 2

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="List Appearance Controls")
    BuildXY, Base1, Obj, Foci, 5, /OFFSET
    BuildOkCancel, Base, Obj

    DlgInfo     = {             $
        Foci:       Foci,       $
        EditId:     _EditId,    $
        ObjPtr:     Ptr         $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='LIST_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  LIST_Save
;   Save list information to a file.
;   Store value as well: If value is nil, make up a value.
;
;   FORMAT:
;       <list object>
;       <number of elements in value>
;       <value>
;
PRO LIST_Save, Unit, Ptr
    GenWrite, Unit, Ptr, DEFAULT='<Nil LIST>'
END


;
;  LIST_Restore
;   Read in a list object from a file
;
PRO LIST_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "LIST", 1
END


;
;  LIST_Generate
;   Create a list object for previewing
;
PRO LIST_Generate, Base, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    GetValue, Obj, Names, '<Nil LIST>'        ; Get Value (or use default)

    ;   Generate command string

    Cmd = 'Id = WIDGET_LIST(Base,VALUE=Names'

    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    SAddCmd, Cmd, Obj.Font, 'FONT'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'

    Obj2Ptr, Obj, Ptr

    ; Create LIST widget

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create LIST ' + VarName(Ptr)
    ENDIF
END


;
;  LIST_GenWid
;   Create IDL code for creating a list widget
;
PRO LIST_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)                ; Get variable name of object
    Ptr2Obj, Ptr, Obj                   ; Get object info

    ListName    = 'ListVal' + STRTRIM(Ptr,2)    ; Create value name
    SaveStr, Unit, Ptr, Obj, ListName, ""       ; Generate value code

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_LIST( ",A,",VALUE=",A)', $
        Name, Parent, ListName, /NO_EOL
    SSaveCmd, Unit, Obj.Font, "FONT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  LIST_Alloc
;       Allocate a list object.  Don't allocate if ptr is non-nil
;
PRO LIST_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr     = WIDGET_BASE(GROUP=TopDlg)
    ValueId = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer for the value too

    Obj     = {                 $
        WE_LIST,                $
        Type:           'LIST',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; Title or object name
        Font:           '',     $
        FrameSize:      0,      $
        UValue:         '',     $
        Value1:         ValueId,$
        Value2:         '',     $ ; UNIMPLEMENTED and unused
        ValueType:      0,      $ ; UNIMPLEMENTED and unused
        YSize:          1,      $
        XOffset:        0,      $
        YOffset:        0       $
    }
    Obj2Ptr, Obj, Ptr
END
