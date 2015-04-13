;
; $Id: bgroup_alloc.pro,v 1.8 1995/01/20 19:41:01 tonyh Exp $
;
;  WidBGroup
;   Widget Button Group (CW_BGROUP) class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;


;
;  BGROUP_Icon
;       Return the button group icon
;
FUNCTION BGROUP_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 224b, 0b, 0b, 0b ], $
    [ 176b, 1b, 0b, 0b ], $
    [ 88b, 3b, 0b, 0b ], $
    [ 172b, 6b, 0b, 0b ], $
    [ 86b, 13b, 0b, 0b ], $
    [ 171b, 26b, 0b, 0b ], $
    [ 87b, 29b, 0b, 0b ], $
    [ 175b, 30b, 0b, 0b ], $
    [ 94b, 143b, 255b, 63b ], $
    [ 188b, 135b, 255b, 63b ], $
    [ 248b, 3b, 0b, 0b ], $
    [ 240b, 1b, 0b, 0b ], $
    [ 224b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 224b, 0b, 0b, 0b ], $
    [ 176b, 1b, 0b, 0b ], $
    [ 88b, 3b, 0b, 0b ], $
    [ 172b, 6b, 0b, 0b ], $
    [ 86b, 13b, 0b, 0b ], $
    [ 171b, 26b, 0b, 0b ], $
    [ 87b, 29b, 0b, 0b ], $
    [ 175b, 30b, 0b, 0b ], $
    [ 94b, 143b, 255b, 63b ], $
    [ 188b, 135b, 255b, 63b ], $
    [ 248b, 3b, 0b, 0b ], $
    [ 240b, 1b, 0b, 0b ], $
    [ 224b, 0b, 0b, 0b ]  $
  ]
END


;
;  BGROUP_Copy
;       Copy a Button group. 2 copy methods.
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO BGROUP_Copy, Ptr, Copy
    GenCopy, Ptr, Copy, /HASVALUE
END

;
;  BGROUP_Destroy
;   Release resources for the given button group.
;
PRO BGROUP_Destroy, Ptr
    GenDestroy, Ptr, /HASVALUE
END


;
;  BGROUP_Event
;   Event handling routine for Button group dialogs.
;
PRO BGROUP_Event, Event

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev                 ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY   ; Get Dialog Info
    Ptr2Obj, Binfo.ObjPtr, Obj                              ; Get Object

    CASE Ev OF

    'Bbs':      BEGIN                               ; Base is a Bulletin Board
        Obj.BaseType        = 0
        WIDGET_CONTROL, Binfo.RowColId, SENSITIVE=0
        END
    'Row':                BEGIN                     ; Base is a row
        Obj.BaseType        = 1
        WIDGET_CONTROL, Binfo.RowColId, SENSITIVE=1
        END
    'Column':        BEGIN                          ; Base is a column
        Obj.BaseType        = 2
        WIDGET_CONTROL, Binfo.RowColId, SENSITIVE=1
        END

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
    'NORMAL':   Obj.BaseExcl    = 0             ; Exclusivity of Group
    'EXCL':     Obj.BaseExcl    = 1
    'NONEXCL':  Obj.BaseExcl    = 2
    'TOP':      Obj.TopTitle    = Event.Value   ; Basic Info Values
    'LEFT':     Obj.LeftTitle   = Event.Value

    'NROW':     Obj.NRowCol     = Event.Value   ; Base type Values
    'SPACE':    Obj.Space       = Event.Value
    'XPAD':     Obj.XPad        = Event.Value
    'YPAD':     Obj.YPad        = Event.Value
    'FONT':     Obj.Font        = Event.Value

    'NAME':     Obj.Name        = Event.Value   ; Other values
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value

    'XSIZE':    Obj.XSize       = Event.Value   ; XY Stuff
    'YSIZE':    Obj.YSize       = Event.Value
    'XOFFSET':  Obj.XOffset     = Event.Value
    'YOFFSET':  Obj.YOffset     = Event.Value
    'XSCROLL':  Obj.XScrollSize = Event.Value
    'YSCROLL':  Obj.YScrollSize = Event.Value

    'XFONT':    DoXFont, Obj, Binfo.Foci(6)
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
;  BGROUP_Build
;   Create a dialog box a button group object.  If ptr is nil then
;   create the object as well.
;
PRO BGROUP_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    BGROUP_Alloc, ParPtr, Ptr               ; Allocate object if necessary
    MgrName = 'WE_BGROUP' + STRTRIM(Ptr, 2) ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if it already exists

    Title   = GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'
    Ptr2Obj, Ptr, Obj

    ;   Create Dialog

    IF SmallScreen(0) NE 0 THEN BEGIN
        Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg, $
                        X_SCROLL_SIZE=SmallScreen(0), $
                        Y_SCROLL_SIZE=SmallScreen(1) )
    ENDIF ELSE BEGIN
        Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)
    ENDELSE
    Foci    = LONARR(16)

    Base1   = WIDGET_BASE(Base, /COLUMN)
    ; Lab       = WIDGET_LABEL(Base1, VALUE="Basic Information")
    BuildEdit, Base1, Obj, _EditId
    BuildBaseType, Base1, Obj, Foci, 0, RowColId, RowColBtns, /ROW

    Base2   = WIDGET_BASE(Base1, /ROW)
    Label   = WIDGET_LABEL(Base2, VALUE="Base Type:")
    Base3   = WIDGET_BASE(Base2, /ROW, /EXCLUSIVE)
    Btns    = LONARR(3)
    Btns(0) = WIDGET_BUTTON(Base3, VALUE='Normal Base', UVALUE='NORMAL')
    Btns(1) = WIDGET_BUTTON(Base3, VALUE='Exclusive', UVALUE='EXCL')
    Btns(2) = WIDGET_BUTTON(Base3, VALUE='Non Exclusive', UVALUE='NONEXCL')

    Base2   = WIDGET_BASE(Base1, /ROW)
    Foci(4) = Field(Base2, 'Top Title:', Obj.TopTitle, 'TOP', SIZE=20, /STRING)
    Foci(5) = Field(Base2, 'Left Title:', Obj.LeftTitle,'LEFT',SIZE=20,/STRING)

    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(6) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")

    BuildOther, Base1, Obj, Foci, 7

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Button Group Appearance Controls")
    BuildXY, Base1, Obj, Foci, 10, /SIZE, /OFFSET, /SCROLL
    BuildOkCancel, Base, Obj

    DlgInfo     = {             $
        Foci:       Foci,       $
        RowColId:   RowColId,   $
        ObjPtr:     Ptr         $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY  ; Save dialog info
    WIDGET_CONTROL, Base, /REALIZE                      ; Create dialog

    ;   Set controls in dialog to show current state
    WIDGET_CONTROL, RowColBtns(Obj.BaseType), /SET_BUTTON
    WIDGET_CONTROL, Btns(Obj.BaseExcl), /SET_BUTTON
    WIDGET_CONTROL, RowColId, SENSITIVE=(Obj.BaseType NE 0)

    ;   Hand off dialog to window manager
    XMANAGER, MgrName, Base, EVENT_HANDLER='BGROUP_Event', CLEANUP='MISC_Kill'

    Obj2Ptr, Obj, Ptr       ; Restore pointer
END

;
;  BGROUP_Save
;   Save button group information in a file.
;   Store value as well: If value is nil, make up a value.
;
;   FORMAT:
;       <bgroup object>
;       <number of elements in value>
;       <value>
;
PRO BGROUP_Save, Unit, Ptr
    GenWrite, Unit, Ptr, DEFAULT="<Nil BGROUP>"
END

;
;  BGROUP_Restore
;   Read in a button group object from a file
;
PRO BGROUP_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "BGROUP", 1
END

;
;  BGROUP_Generate
;   Create a button group object for previewing
;
PRO BGROUP_Generate, Base, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj

    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    GetValue, Obj, Names, "<Nil BGROUP>"        ; Get Value (or use default)

    ;   Generate command string

    Cmd = 'Id = CW_BGROUP(Base,Names'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    SAddCmd, Cmd, Obj.Font, 'FONT'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'
    IAddCmd, Cmd, Obj.XScrollSize, 'X_SCROLL_SIZE'
    IAddCmd, Cmd, Obj.YScrollSize, 'Y_SCROLL_SIZE'
    SAddCmd, Cmd, Obj.TopTitle, 'LABEL_TOP'
    SAddCmd, Cmd, Obj.LeftTitle, 'LABEL_LEFT'

    ;   Row/Column

    IF Obj.BaseType EQ 1 THEN               $
        IAddCmd, Cmd, Obj.NRowCol, 'ROW'    $
    ELSE IF Obj.BaseType EQ 2 THEN          $
        IAddCmd, Cmd, Obj.NRowCol, 'COLUMN'

    ; Normal/Exclusive/Nonexclusive

    IF Obj.BaseExcl NE 0 THEN BEGIN
        IF Obj.BaseExcl EQ 1 THEN       $
            Cmd = Cmd + ',/EXCLUSIVE'       $
        ELSE                    $
            Cmd = Cmd + ',/NONEXCLUSIVE'
    ENDIF ELSE BEGIN
        IAddCmd, Cmd, Obj.Space, 'SPACE'
        IAddCmd, Cmd, Obj.XPad, 'XPAD'
        IAddCmd, Cmd, Obj.YPad, 'YPAD'
    ENDELSE

    Obj2Ptr, Obj, Ptr

    ; Create bgroup by executing command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Button Group ' + VarName(Ptr)
    ENDIF

END

;
;  BGROUP_GenWid
;   Create IDL code for creating a button group (CW_BGROUP)
;
PRO BGROUP_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)                ; Get variable name of object
    Ptr2Obj, Ptr, Obj                   ; Get object info
    BtnName = 'Btns' + STRTRIM(Ptr,2)   ; Create value name
    SaveStr, Unit, Ptr, Obj, BtnName, "<Nil BGROUP>"    ; create value code.

    XPRINTF, FORMAT='("  ",A," = CW_BGROUP( ",A,", ",A)', $
        Unit, Name, Parent, BtnName, /NO_EOL

    ;   Row/Column Base?

    IF Obj.BaseType EQ 1 THEN           $
        ISaveCmd, Unit, Obj.NRowCol, 'ROW'  $
    ELSE IF Obj.BaseType EQ 2 THEN  $
        ISaveCmd, Unit, Obj.NRowCol, 'COLUMN'

    ; Normal/Exclusive/Nonexclusive base?

    IF Obj.BaseExcl NE 0 THEN BEGIN
        IF Obj.BaseExcl EQ 1 THEN BEGIN
            ISaveCmd, Unit, 1, "EXCLUSIVE"
        ENDIF ELSE BEGIN
            ISaveCmd, Unit, 1, "NONEXCLUSIVE"
        ENDELSE
    ENDIF ELSE BEGIN
        ISaveCmd, Unit, Obj.Space, 'SPACE'
        ISaveCmd, Unit, Obj.XPad, 'XPAD'
        ISaveCmd, Unit, Obj.YPad, 'YPAD'
    ENDELSE

    SSaveCmd, Unit, Obj.Font, "FONT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    SSaveCmd, Unit, Obj.LeftTitle, "LABEL_LEFT"
    SSaveCmd, Unit, Obj.TopTitle, "LABEL_TOP"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.XScrollSize, "X_SCROLL_SIZE"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    ISaveCmd, Unit, Obj.YScrollSize, "Y_SCROLL_SIZE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  BGROUP_Alloc
;       Allocate a button group object.  Don't do this if ptr is not NULL
;
PRO BGROUP_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr     = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer
    ValueId = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer for the value too

    ;   Make a Button Group object

    Obj     = {                 $
        WE_BGROUP,              $
        Type:           'BGROUP',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; next pointer (for lists)
        Name:           '',     $ ; object name
        BaseExcl:       0,      $ ; Enum { Normal, Exclusive, Non-Exclusive }
        BaseType:       1,      $ ; Enum { BBS, Row, Col } (Row is dflt)
        FrameSize:      0,      $
        Font:           '',     $
        NRowCol:        1,      $ ; Number of Rows or Columns
        Space:          0,      $
        TopTitle:       '',     $
        LeftTitle:      '',     $
        UValue:         '',     $
        Value1:         ValueId,$
        Value2:         '',     $ ; UNIMPLEMENTED and unused
        ValueType:      0,      $ ; UNIMPLEMENTED and unused
        XPad:           0,      $
        YPad:           0,      $
        XSize:          0,      $
        YSize:          0,      $
        XOffset:        0,      $
        YOffset:        0,      $
        XScrollSize:    0,      $
        YScrollSize:    0       $
    }
    Obj2Ptr, Obj, Ptr                       ; Store object in pointer
END
