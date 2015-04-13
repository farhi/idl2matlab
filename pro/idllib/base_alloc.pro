;
; $Id: base_alloc.pro,v 1.7 1994/06/01 23:08:48 ali Exp $
;
;  WidBase
;   Base object class definition.
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;


;
;  BASE_Icon
;       Return the base toolbar icon
;
FUNCTION BASE_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 60b, 207b, 243b, 60b ], $
    [ 60b, 207b, 243b, 60b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 204b, 49b, 239b, 49b ], $
    [ 76b, 74b, 33b, 48b ], $
    [ 204b, 121b, 239b, 48b ], $
    [ 76b, 74b, 40b, 48b ], $
    [ 204b, 73b, 239b, 49b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 60b, 207b, 243b, 60b ], $
    [ 60b, 207b, 243b, 60b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 12b, 0b, 0b, 48b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  BASE_Copy
;       Copy the given base object.  Remember to copy contents (children)
;   as well.
;
PRO BASE_Copy, Ptr, Copy
  COMMON WidEd_Comm

    Copy    = WIDGET_BASE(GROUP=TopDlg)    ; Make a pointer
    Ptr2Obj, Ptr, Obj, /COPY        ; Copy original pointer contents

    Child   = Obj.Children          ; Child list of original

    Obj.Children    = 0             ; Clear copy's child list
    Obj.LastChild   = 0
    Obj2Ptr, Obj, Copy              ; Store copy object into pointer

    ;   Now copy child list
    ;   Due to the nature of the copy routines, we need to make sure
    ;   NewChild is <UNDEFINED>. (C.f. XXX_Copy)

    WHILE Child NE 0 DO BEGIN

        GetType, Child, Type        ; Get object class name
        ClearVar, NewChild          ; make NewChild of type <UNDEFINED>

        CALL_PROCEDURE, Type+"_Copy", Child, NewChild       ; Make Copy
        AddChild, Copy, NewChild, /NO_UPDATE, /NO_CANCEL

        Child   = NextPtr(Child)    ; Get the next child in list
    ENDWHILE
END

;
;  BASE_Destroy
;   Release resources associated with the given object.  Recursively
;   remove children objects as well.
;
PRO BASE_Destroy, Ptr
    Ptr2Obj, Ptr, Obj                       ; Get the object
    DoList, Obj.Children, "Destroy"         ; Destroy children

    ;   Destroy dialog boxes associated with the object. They are
    ;   no longer useable

    IF WIDGET_INFO(Obj.Dialog, /VALID) THEN $
        WIDGET_CONTROL, Obj.Dialog, /DESTROY
    IF WIDGET_INFO(Obj.AttrDlg, /VALID) THEN $
        WIDGET_CONTROL, Obj.AttrDlg, /DESTROY

    ;   Destroy the pointer
    WIDGET_CONTROL, Ptr, /DESTROY

    ;   Object is in local variable and goes away as we return
END


;
;  BASE_BarEvent
;   Events from the pull down menu don't have a <STRING> type UVALUE to
;   use.  Thus we have a separate event routine to handle the menu items.
;
;   All base menubar events consist of adding child object
;
PRO BASE_BarEvent, Event

  COMMON WidEd_Comm

    ;   We will need the pointer for the Object associated with this
    ;   dialog box (the parent for the children we are about to create)

    WIDGET_CONTROL, Event.top, GET_UVALUE=Binfo, /NO_COPY

    IF Event.Value EQ 'Add.Hide Tool Bar' THEN BEGIN

        WIDGET_CONTROL, Binfo.ToolBar, MAP=0
        WIDGET_CONTROL, Binfo.ToolId, SET_VALUE='Show Tool Bar'
        WIDGET_CONTROL, Binfo.ToolId, SET_UVALUE='Add.Show Tool Bar' ; Hack
        SetTag, Binfo.ObjPtr, "TB_Showing", 0

    ENDIF ELSE IF Event.Value EQ 'Add.Show Tool Bar' THEN BEGIN

        WIDGET_CONTROL, Binfo.ToolBar, MAP=1
        WIDGET_CONTROL, Binfo.ToolId, SET_VALUE='Hide Tool Bar'
        WIDGET_CONTROL, Binfo.ToolId, SET_UVALUE='Add.Hide Tool Bar' ; Hack
        
        SetTag, Binfo.ObjPtr, "TB_Showing", 1

    ENDIF ELSE BEGIN

        IF STRMID(Event.Value,0,4) EQ "Add." THEN BEGIN
            Idx     = WHERE(STRMID(Event.Value,4,100) EQ AddList.Menu)
            Build   = AddList(Idx).Class
        ENDIF ELSE MESSAGE, 'Unprocessed event: ' + Event.Value

        ;   Allocate object and create a dialog box as well
        CALL_PROCEDURE, Build+'_Build', Ptr, Binfo.ObjPtr

        ;   Add child to our child list.  Note that Base object
        ;   do not get added to the active dialog box list
        AddChild, Binfo.ObjPtr, Ptr, NO_CANCEL=(Build EQ 'Base')

    ENDELSE

    ;   Restore dialog box information
    WIDGET_CONTROL, Event.top, SET_UVALUE=Binfo, /NO_COPY
END

;
;  BASE_Event
;   Normal event handling routine for a base object
;
PRO BASE_Event, Event

  COMMON WidEd_Comm

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

    ;   user has set some characteristic of the object to a new value

    'NROW':         Obj.NRowCol     = Event.Value
    'SPACE':        Obj.Space       = Event.Value
    'XPAD':         Obj.XPad        = Event.Value
    'YPAD':         Obj.YPad        = Event.Value
    'NAME':         Obj.Name        = Event.Value
    'FRAME':        Obj.FrameSize   = Event.Value
    'XSCROLL':      Obj.XScrollSize = Event.Value
    'YSCROLL':      Obj.YScrollSize = Event.Value

    ;   User wants to see the 'Additional attributes' dialog
    'ATTR':     BASE_BuildAttr, Event.Top, Binfo.ObjPtr, Obj

    ;   Additional Attribute Events
    'XSIZE':        Obj.XSize       = Event.Value
    'YSIZE':        Obj.YSize       = Event.Value
    'XOFFSET':      Obj.XOffset     = Event.Value
    'YOFFSET':      Obj.YOffset     = Event.Value
;   'EVENT_FUNC':   Obj.EventFunc   = Event.Value
    'EVENT_PROC':   Obj.EventProc   = Event.Value
    'GETFUNC':      Obj.GetFunc     = Event.Value
    'SETPROC':      Obj.SetProc     = Event.Value
    'KILLPROC':     Obj.KillProc    = Event.Value
    'DO_TLB':       Obj.TLBEvents   = 1 - Obj.TLBEvents
    'MAPPED':       Obj.BaseMapped  = 1 - Obj.BaseMapped

    'DONE':         BEGIN
                    Obj2Ptr, Obj, Binfo.ObjPtr
                    WIDGET_CONTROL, Event.top, SET_UVALUE=Binfo, /NO_COPY
                    WIDGET_CONTROL, Event.Top, /DESTROY
                    RETURN
                    END
    ELSE:           MESSAGE, 'Unprocessed event: ' + Ev
    ENDCASE

    Dirty   = 1     ; We've changed something since the last save

    SetNextFocus, Binfo, Event      ; Set next keyboard focus as necessary
    Obj2Ptr, Obj, Binfo.ObjPtr      ; Put object back into pointer
    WIDGET_CONTROL, Event.top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  BASE_Build
;   Create a dialog for a new/existing base object
;
PRO BASE_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    BASE_Alloc, ParPtr, Ptr                 ; Allocate object if necessary
    MgrName = 'WE_BASE' + STRTRIM(Ptr,2)    ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if we already have one

    Title= GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'
    Ptr2Obj, Ptr, Obj
    Base            = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)
    Foci            = LONARR(8)

    ;   Menu bar contents

    Menu            = MakeAddMenu()
    Menu(N_ELEMENTS(Menu)-1).flags  = 2
    MenuBarDesc     = [ { CW_PDMENU_S, 3, 'Add' }, $
                        { CW_PDMENU_S, 0, 'Hide Tool Bar' }, $
                        Menu ]
    If Obj.TB_Showing EQ 0 THEN MenuBarDesc(1).name = 'Show Tool Bar'
    MenuBar         = CW_PDMENU(Base, MenuBarDesc, IDS=Ids, /RETURN_FULL_NAME)
    ToolId          = Ids(1)

    BuildToolBar, Base, ToolBar

    BuildBaseType, Base, Obj, Foci, 0, RowColId, Btns
    Base1   = WIDGET_BASE(Base, /COLUMN, EVENT_PRO='BASE_Event')
    Foci(4) = Field(Base1, "Name:", Obj.Name, 'NAME', SIZE=50, /STRING)
    Foci(5) = Field(Base1, "Frame Size:", Obj.FrameSize, "FRAME", /INT)
    BuildXY, Base, Obj, Foci, 6, /SIZE
    Dummy   = WIDGET_BUTTON(Base, VALUE='More Attributes', $
                UVALUE='ATTR', EVENT_PRO='BASE_Event')

    Dummy   = WIDGET_LABEL(Base, VALUE=' ')
    Dummy   = WIDGET_BUTTON(Base, VALUE='Done', UVALUE='DONE', $
            EVENT_PRO='BASE_Event')

    Obj.Dialog  = Base      ; Save active dialog id in object

    DlgInfo = {                 $
        Foci:       Foci,       $
        RowColId:   RowColId,   $
        ToolBar:    ToolBar,    $
        ToolId:     ToolId,     $
        ObjPtr:     Ptr         $
    }

    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY  ; save dialog info
    WIDGET_CONTROL, Base, /REALIZE                      ; create dialog
    WIDGET_CONTROL, Btns(Obj.BaseType), /SET_BUTTON     ; Set basetype button
    WIDGET_CONTROL, RowColId, SENSITIVE=(Obj.BaseType NE 0)

    WIDGET_CONTROL, ToolBar, MAP=Obj.TB_Showing

    ;   Hand off dialog to window manager
    XMANAGER, MgrName, Base, EVENT_HANDLER='BASE_BarEvent', CLEANUP='DEP_Kill'

    Obj2Ptr, Obj, Ptr       ; Restore pointer
END


;
;  BASE_Save
;   Store object information in a file.
;   Store children as well.  Store the child's type and then the child.
;   End of list is indicated by the type string of "_END_"
;
;   FORMAT:
;       <Base object>
;       "ChildType"     E.g. "LABEL", "BUTTON", "DRAW", "TEXT", "BASE", ...
;       Child
;       ...
;       "_END_"
;
PRO BASE_Save, Unit, Ptr

  COMMON WidEd_Comm

    ON_IOERROR, BadWrite

    Ptr2Obj, Ptr, Obj

    WRITEU, Unit, Obj               ; Save Me

    Child   = Obj.Children          ; Save my children
    WHILE Child NE 0 DO BEGIN
        GetType, Child, Type
        WRITEU, Unit, Type          ; Save child Type as a <STRING>
        CALL_PROCEDURE, Type + "_Save", Unit, Child ; Save child
        Child   = NextPtr(Child)
    ENDWHILE

    WRITEU, Unit, "_END_"           ; Write End-Of-List marker

    Obj2Ptr, Obj, Ptr
    RETURN

  BadWrite:
    Dirty   = 2
END


;
;  urBase_Restore
;       There are 3 different base type objects: MAIN, DEP, and BASE
;   and restoring them has a lot of stuff in common. Thus this common
;   routine for restoring bases.
;
PRO urBASE_Restore, Unit, Parent, Ptr, Type

    ; Allocated a pointer of the corrent type
    CALL_PROCEDURE, Type+"_Alloc", Parent, Ptr

    Ptr2Obj, Ptr, Obj
    READU, Unit, Obj                        ; Read in the object
    Obj.Next        = 0                     ; Clear bogus values
    Obj.Children    = 0
    Obj.LastChild   = 0
    Obj.Dialog      = 0
    Obj.AttrDlg     = 0
    Obj.Parent      = Parent                ; Set true parent ptr
    Obj2Ptr, Obj, Ptr                       ; Store into pointer

    ;   Restore children
    WHILE 1 DO BEGIN
        Type    = ""
        READU, Unit, Type                   ; Get the Type
        IF Type EQ "_END_" THEN RETURN      ; End-Of-List Marker?

        ClearVar, Child
        CALL_PROCEDURE, Type+"_Restore", Unit, Ptr, Child    ; Get Child
        AddChild, Ptr, Child, /NO_UPDATE, /NO_CANCEL
    ENDWHILE
END

;
;  BASE_Restore
;   Read in a base object (and its children) from a file
;
PRO BASE_Restore, Unit, Parent, Ptr
    urBASE_Restore, Unit, Parent, Ptr, "BASE"
END

;
;   BASE_BuildAttr
;       Object contains information about the base.  Ptr is needed
;   for object naming.  Routine builds the 'extra' dialog box that
;   base objects have.
;
PRO BASE_BuildAttr, Leader, Ptr, Obj

    MgrName = 'WidBAttr' + STRTRIM(Ptr,2)       ; Addition Attr Dialog exists?
    IF XRegistered(MgrName) THEN RETURN

    ;   Create one.

    Foci    = LONARR(7)
    Title   = 'BASE ' + STRTRIM(Obj.Id,2) + ' Attributes'
    Base    = WIDGET_BASE(  GROUP_LEADER=Leader, /COLUMN, TITLE=Title)

    ;   Event Related Info

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Event Controls")

    Foci(0) = Field(Base1, "Function Name for GET_VALUE:", Obj.GetFunc, $
                "GETFUNC", SIZE=20, /STRING)
    Foci(1) = Field(Base1, "Procedure Name for SET_VALUE:", Obj.SetProc, $
                "SETPROC", SIZE=20, /STRING)
    Foci(2) = Field(Base1, "Procedure Name for KILL_NOTIFY:", Obj.KillProc, $
                "KILLPROC", SIZE=20, /STRING)

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Window Appearance Controls")
    BuildXY, Base1, Obj, Foci, 3, /SCROLL, /OFFSET
    Base2   = WIDGET_BASE(Base1, /NONEXCLUSIVE, /ROW)
    Btn     = WIDGET_BUTTON(Base2,              $
                VALUE='Base Starts Mapped', $
                UVALUE='MAPPED')
    IF Obj.BaseMapped EQ 1 THEN WIDGET_CONTROL, Btn, /SET_BUTTON
    Dummy   = WIDGET_LABEL(Base, VALUE=' ')
    Dummy   = WIDGET_BUTTON(Base, VALUE='Done', UVALUE='DONE')

    DlgInfo     = {         $
        Foci:       Foci,   $
        ObjPtr:     Ptr     $
    }
    Obj.AttrDlg = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='BASE_Event', CLEANUP='DEP_Kill'
END


;
;  BASE_Generate
;       Create a base object and its children for previewing.
;
PRO BASE_Generate, Base, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj

    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Generate command string

    Cmd = 'Id = WIDGET_BASE(Base'
    Cmd = Cmd + ",/TLB_SIZE_EVENTS"
    SAddCmd, Cmd, Obj.Name, 'TITLE'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'
    IAddCmd, Cmd, Obj.XScrollSize, 'X_SCROLL_SIZE'
    IAddCmd, Cmd, Obj.YScrollSize, 'Y_SCROLL_SIZE'

    ;   Row/Column?

    IF Obj.BaseType EQ 1 THEN           $
        IAddCmd, Cmd, Obj.NRowCol, 'ROW'    $
    ELSE IF Obj.BaseType EQ 2 THEN      $
        IAddCmd, Cmd, Obj.NRowCol, 'COLUMN'

    IAddCmd, Cmd, Obj.Space, 'SPACE'
    IAddCmd, Cmd, Obj.XPad, 'XPAD'
    IAddCmd, Cmd, Obj.YPad, 'YPAD'

    ; Create base by running command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        Obj2Ptr, Obj, Ptr
        MESSAGE,'Could not build base' + VarName(Ptr)
    ENDIF

    ; Create any child widgets

    Child   = Obj.Children
    WHILE Child NE 0L DO BEGIN
        GetType, Child, Type
        CALL_PROCEDURE, Type + "_Generate", Id, Child
        Child   = NextPtr(Child)
    ENDWHILE

    Obj2Ptr, Obj, Ptr
END

;
;  urBase_GenWid
;   Create code for generating any base class widget and children
;
PRO urBase_GenWid, Unit, Ptr, Obj, Name

    ;   Create base information

    ;   Row/Column Base?

    IF Obj.BaseType EQ 1 THEN           $
        ISaveCmd, Unit, Obj.NRowCol, 'ROW'  $
    ELSE IF Obj.BaseType EQ 2 THEN  $
        ISaveCmd, Unit, Obj.NRowCol, 'COLUMN'

    ISaveCmd, Unit, Obj.Space, 'SPACE'
    ISaveCmd, Unit, Obj.XPad, 'XPAD'
    ISaveCmd, Unit, Obj.YPad, 'YPAD'
;    SSaveCmd, Unit, Obj.EventFunc, "EVENT_FUNC"
;    SSaveCmd, Unit, Obj.EventProc, "EVENT_PROC"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    SSaveCmd, Unit, Obj.GetFunc, "FUNC_GET_VALUE"
    SSaveCmd, Unit, Obj.KillProc, "KILL_NOTIFY"
    ISaveCmd, Unit, Obj.BaseMapped, "MAP", /FORCE
    SSaveCmd, Unit, Obj.SetProc, "PRO_SET_VALUE"
    SSaveCmd, Unit, Obj.Name, "TITLE"
    ISaveCmd, Unit, Obj.TLBEvents, "TLB_SIZE_EVENTS"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.XScrollSize, "X_SCROLL_SIZE"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    ISaveCmd, Unit, Obj.YScrollSize, "Y_SCROLL_SIZE"

    XPRINTF, Unit, ')'
    XPRINTF, Unit

    ;   Now generate code for creating children objects.

    Child   = Obj.Children
    WHILE Child NE 0 DO BEGIN
        GetType, Child, Type
        CALL_PROCEDURE, Type+"_GenWid", Unit, Child, Name
        Child   = NextPtr(Child)

        XPRINTF, Unit    ; Put a blank line between each child
    ENDWHILE
END

;
;  BASE_GenWid
;   Generate IDL code for creating a BASE widget
;
PRO BASE_GenWid, Unit, Ptr, Parent
    Name    = VarId(Ptr)
    Ptr2Obj, Ptr, Obj
    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_BASE(",A)', Name, Parent,/NO_EOL
    urBase_GenWid, Unit, Ptr, Obj, Name
    Obj2Ptr, Obj, Ptr
END


;
;  BASE_Alloc
;       Allocate a base object (if Ptr is NULL or just invalid)
;   There are 3 base object classes (MAIN,DEP, and BASE) so they call
;   a common routine to allocate base objects.
;
PRO BASE_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; Ptr already allocated

    Ptr = WIDGET_BASE(GROUP=TopDlg)         ; Make a pointer
    MakeBaseObj, Parent, Obj, "BASE"        ; Make a base object
    Obj2Ptr, Obj, Ptr                       ; Store object in pointer
END
