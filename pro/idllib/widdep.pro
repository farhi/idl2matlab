;
; $Id: widdep.pro,v 1.11 1994/06/01 23:08:48 ali Exp $
;
;  WidDep
;   Dependent Dep object class definition.
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;


;
;  DEP_Alloc
;       Allocate a dependent base object (if Ptr is NULL or just invalid)
;   There are 3 base object classes (MAIN,DEP, and BASE) so they call
;   a common routine to allocate base objects.
;
PRO DEP_Alloc, Parent, Ptr
  COMMON WidEd_Comm
    IF KEYWORD_SET(Ptr) EQ 0 THEN BEGIN     ; Ptr already allocated
        Ptr = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer
        MakeBaseObj, Parent, Obj, "DEP"     ; Make a dep base object
        Obj2Ptr, Obj, Ptr                   ; Store object in pointer
        Dirty   = 1                         ; Things have changed since
                                            ; last save.
    ENDIF
END


;
;  DEP_Copy
;       Copy the given dependent base object.
;   Remember to copy contents (children) as well.
;
PRO DEP_Copy, Ptr, Copy
    BASE_Copy, Ptr, Copy
END

;
;  DEP_Destroy
;   Release resources associated with the given object.  Recursively
;   remove children objects as well.
;
PRO DEP_Destroy, Ptr
    BASE_Destroy, Ptr       ; Does the same thing
END


;
;  DEP_Kill
;   Base objects need to do things differently on a 'DONE' than
;   the more typical objects (c.f. MISC_Kill). Thus, a base object
;   specific kill routine.
;
PRO DEP_Kill, Dlg

    WIDGET_CONTROL, Dlg, GET_UVALUE=Binfo, /NO_COPY

    IF WIDGET_INFO( BInfo.ObjPtr, /VALID) THEN BEGIN

        Ptr2Obj, Binfo.ObjPtr, Obj

        ;   We may not have any object associated with the
        ;   dialog -- if we here as a result of the object
        ;   being destroyed programmatically.  In that case,
        ;   just don't do anything

        IF N_ELEMENTS(Obj) NE 0 THEN BEGIN
            ;   Which dialog disappeared?
            IF Obj.Dialog EQ Dlg THEN Obj.Dialog = 0        ; Dialog gone?
            IF Obj.AttrDlg Eq Dlg THEN Obj.AttrDlg = 0      ; Add'l gone?
            Obj2Ptr, Obj, Binfo.ObjPtr                      ; Save new state
        ENDIF
    ENDIF
END


;
;  DEP_BarEvent
;   Event from the pull down menu don't have a <STRING> type UVALUE
;   to use for determining event type.  Instead the event.value is
;   usable.  However, it is easier to provide a separate event handler
;
PRO DEP_BarEvent, Event

  COMMON WidEd_Comm

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
;  DEP_Event
;   Normal event handling routine for a dependent base object
;
PRO DEP_Event, Event

  COMMON WidEd_Comm

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev                 ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY   ; Get Dialog Info
    Ptr2Obj, Binfo.ObjPtr, Obj                              ; Get Object

    CASE Ev OF

    'Bbs':      BEGIN                   ; Base is a bulletin board
        Obj.BaseType        = 0
        WIDGET_CONTROL, Binfo.RowColId, SENSITIVE=0
        END
    'Row':                BEGIN
        Obj.BaseType        = 1
        WIDGET_CONTROL, Binfo.RowColId, SENSITIVE=1
        END
    'Column':        BEGIN
        Obj.BaseType        = 2
        WIDGET_CONTROL, Binfo.RowColId, SENSITIVE=1
        END

    ;   user has set some characteristic of the object to a new value

    'NROW':         Obj.NRowCol     = Event.Value
    'SPACE':        Obj.Space       = Event.Value
    'XPAD':         Obj.XPad        = Event.Value
    'YPAD':         Obj.YPad        = Event.Value
    'NAME':         Obj.Name        = Event.Value
    'XSCROLL':      Obj.XScrollSize = Event.Value
    'YSCROLL':      Obj.YScrollSize = Event.Value

    ;   User wants to see the 'Additional Attributes' dialog
    'ATTR':     DEP_BuildAttr, Event.Top, Binfo.ObjPtr, Obj

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
; DEP_Build
;   Create a dialog for a dependent base object. If ptr is nil then
;   create the object as well.
;
PRO DEP_Build, Ptr, Base

  COMMON WidEd_Comm

    DEP_Alloc, 0L, Ptr                      ; Allocate object if necessary
    MgrName = 'WE_BASE' + STRTRIM(Ptr,2)    ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if it already exists

    Title   = 'Dependent Base: ' + GetId(Ptr)
    Ptr2Obj, Ptr, Obj

    ;   Create dialog box

    Base            = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)
    Foci            = LONARR(7)

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
    Base1   = WIDGET_BASE(Base, /COLUMN, EVENT_PRO='DEP_Event')
    Foci(4) = Field(Base1, "Name:", Obj.Name, 'NAME', SIZE=50, /STRING)
    BuildXY, Base, Obj, Foci, 5, /SIZE
    Dummy   = WIDGET_BUTTON(Base, VALUE='More Attributes', $
                UVALUE='ATTR', EVENT_PRO='DEP_Event')

    Dummy   = WIDGET_LABEL(Base, VALUE=' ')
    Dummy   = WIDGET_BUTTON(Base, VALUE='Done', UVALUE='DONE', $
            EVENT_PRO='DEP_Event')

    DlgInfo = { $
        Foci:       Foci, $
        RowColId:   RowColId, $
        ToolBar:    ToolBar, $
        ToolId:     ToolId, $
        ObjPtr:     Ptr $
    }
    Obj.Dialog  = Base

    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    WIDGET_CONTROL, Btns(Obj.BaseType), /SET_BUTTON
    WIDGET_CONTROL, RowColId, SENSITIVE=(Obj.BaseType NE 0)

    WIDGET_CONTROL, ToolBar, MAP=Obj.TB_Showing

    XMANAGER, MgrName, Base, EVENT_HANDLER='DEP_BarEvent', CLEANUP='DEP_Kill'
    Obj2Ptr, Obj, Ptr
END



;
;  DEP_Save
;   Store object information in a file. Identical to BASE_Save routine.
;
PRO DEP_Save, Unit, Ptr
    BASE_Save, Unit, Ptr
END

;
;  BASE_Restore
;   Read in a dependent base object (and its children) from a file
;
PRO DEP_Restore, Unit, Parent, Ptr
    urBASE_Restore, Unit, Parent, Ptr, "DEP"
END

;
;   DEP_BuildAttr
;       Object contains information about the base.  Ptr is needed
;   for object naming.  Routine builds the 'extra' dialog box that
;   base objects have.
;
;   This is a merged routine -- works for MAIN and DEP base objects
;
PRO DEP_BuildAttr, Leader, Ptr, Obj


    MgrName = 'WidBAttr' + STRTRIM(Ptr,2)   ; Create name an test for
    IF XRegistered(MgrName) THEN RETURN     ; dialog already existing

    ;   Create Additional dialog

    Foci    = LONARR(8)
    IF Obj.Type EQ 'MAIN' THEN BEGIN        ; MAIN Obj special case
        Title   = 'Main Base Attributes'
    ENDIF ELSE BEGIN
        Title   = 'BASE ' + Obj.Id + ' Attributes'
    ENDELSE

    Base    = WIDGET_BASE(  GROUP_LEADER=Leader, /COLUMN, TITLE=Title)

    ;   Event Related Info

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Event Controls")

;    Foci(0) = Field(Base1, "Event Function Name:", Obj.EventFunc, $
;                "EVENT_FUNC", SIZE=20, /STRING)
    Foci(0) = Field(Base1, "Event Procedure Name:", Obj.EventProc, $
                "EVENT_PROC", SIZE=20, /STRING)
    Foci(1) = Field(Base1, "Function Name for GET_VALUE:", Obj.GetFunc, $
                "GETFUNC", SIZE=20, /STRING)
    Foci(2) = Field(Base1, "Procedure Name for SET_VALUE:", Obj.SetProc, $
                "SETPROC", SIZE=20, /STRING)
    Foci(3) = Field(Base1, "Procedure Name for KILL_NOTIFY:", Obj.KillProc, $
                "KILLPROC", SIZE=20, /STRING)
    Base2   = WIDGET_BASE(Base1, /NONEXCLUSIVE, /ROW)
    Btn     = WIDGET_BUTTON(Base2,              $
                    VALUE='Accept TLB Events',  $
                    UVALUE='DO_TLB')

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Window Appearance Controls")
    BuildXY, Base1, Obj, Foci, 4, /SCROLL, /OFFSET
    Base2   = WIDGET_BASE(Base1, /NONEXCLUSIVE, /ROW)
    Btn     = WIDGET_BUTTON(Base2,              $
                VALUE='Base Starts Mapped', $
                UVALUE='MAPPED')
    IF Obj.BaseMapped EQ 1 THEN WIDGET_CONTROL, Btn, /SET_BUTTON
    Dummy   = WIDGET_LABEL(Base, VALUE=' ')
    Dummy   = WIDGET_BUTTON(Base, VALUE='Done', UVALUE='DONE')

    DlgInfo     = { $
        Foci:       Foci, $
        ObjPtr:     Ptr $
    }
    Obj.AttrDlg = Base

    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER=Obj.Type+'_Event', $
            CLEANUP='DEP_Kill'
END


;
;  TestDraw
;   When creating a preview I thought it would be nice to draw
;   something to the dialogs.  Thus, after creating a preview we
;   search the object hierarchy looking for drawables to put
;   plots in.
;
PRO TestDraw, Ptr

    COMMON TestDraw_Comm, Seed

    Ptr2Obj, Ptr, Obj

    ;   We are only interested in 2 object types:
    ;       Draws to draw in
    ;       Base objects so that we can check their children

    CASE TAG_NAMES(Obj, /STRUCTURE) OF

    'WE_BASE': BEGIN
        Child       = Obj.Children
        WHILE Child NE 0L DO BEGIN
            TestDraw, Child
            Child   = NextPtr(Child)
        ENDWHILE
        END
    'WE_DRAW': BEGIN
        WIDGET_CONTROL, Obj.DrawId, GET_VALUE=WinId
        WSET, WinId

        CASE (FIX(Randomu(Seed,1) * 4))(0) OF
        0:      PLOT,[0,1],TITLE='Sample Plot'
        1:      SURFACE, DIST( (FIX(Randomu(Seed,1)*10))(0) + 2 ), $
                        TITLE='Sample Surface'
        2:      PLOT,sin(findgen(100)/5)/exp(findgen(100)/50), $
                        TITLE='Sample Plot'
        3:      PLOT, Randomu(Seed,10), TITLE='Sample Plot'
        ENDCASE
        END
    ELSE:           ; Do nothing
    ENDCASE
    Obj2Ptr, Obj, Ptr
END


;
;  DEP_Generate
;   Create a dependent (or MAIN) base object and its children
;   for previewing.
;
PRO DEP_Generate, Ptr, Id, Offset

  COMMON WidEd_Comm  

    Ptr2Obj, Ptr, Obj

    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Generate command string

    Cmd = 'Id = WIDGET_BASE(GROUP_LEADER=' + STRTRIM(TopDlg,2)

;   Tried allowing the user to resize a top level base and having
;   that affect the base's size.  I didn't like it so I have removed
;   that ability (c.f. wided.pro)
;   Cmd = Cmd + ",/TLB_SIZE_EVENTS"

    SAddCmd, Cmd, Obj.Name, 'TITLE'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'
    IAddCmd, Cmd, Obj.XScrollSize, 'X_SCROLL_SIZE'
    IAddCmd, Cmd, Obj.YScrollSize, 'Y_SCROLL_SIZE'


    ;   Row/Column

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
        Ptr2Obj, Child, ChObj
        Next    = ChObj.Next
        Type    = ChObj.Type
        Obj2Ptr, ChObj, Child
        CALL_PROCEDURE, Type + "_Generate", Id, Child
        Child   = Next
    ENDWHILE

    WIDGET_CONTROL, Id, /REALIZE
    WIDGET_CONTROL, Id, SET_UVALUE=Ptr

    ;XMANAGER, 'GenBase', Id, /JUST_REG (for TLB event)

    ;   If the user has moved the preview of the base and has not
    ;   specified a position via the control panel then
    ;   use the position of the previous preview as the location
    ;   to rebuild a preview

    ;   If the user has not provided positions for the underlying
    ;   objects, I create them in a cascade form

    IF Obj.XOffset EQ 0 AND Obj.YOffset EQ 0 THEN BEGIN
        Xoff    = BaseXY(0, Offset)
        Yoff    = BaseXY(1, Offset)

        IF Offset NE 0 AND Xoff EQ 0 AND Yoff EQ 0 THEN BEGIN
            ;   Add frame fudge factors
            Xoff    = Offset * 30 + 10
            Yoff    = Offset * 20 + 30
        ENDIF

        ;   Only move it if necessary.

        IF Xoff NE 0 OR Yoff NE 0 THEN $
            WIDGET_CONTROL, Id, TLB_SET_XOFF=Xoff, TLB_SET_YOFF=Yoff
    ENDIF

    XMANAGER, 'Gen', Id, /JUST_REG      ; Suck up events and ignore them

    Obj2Ptr, Obj, Ptr
    TestDraw, Ptr                       ; Put a plot into drawables
END


;
;  SaveEvent
;   Create an case statement in an event handler for the given object
;
PRO SaveEvent, Unit, Ptr

    IF Ptr EQ 0 THEN RETURN

    Id  = GetId( Ptr )
    Ptr2Obj, Ptr, Obj
    UVal    = UValue(Obj, Ptr)
    Type    = Obj.Type

    CASE Type OF

        'LABEL':            ; Labels don't generate events

        ;   Bases don't generate events but their children might
        'BASE':     DoFList, Obj.Children, 'SaveEvent', Unit

        ;   Menus get an event function all their own
        'PDMENU':           BEGIN
            Name    = Type + Obj.Id
            PRINTF, Unit, "  ; Event for ", Id
            PRINTF, Unit, "  '", QString(UVal), "': ", Name, "_Event, Event"
            END

        ;   Button groups have multiple formats (normal,exclusive,non-excl)
        'BGROUP':   BEGIN
            PRINTF, Unit, "  '", QString(UVal), "': BEGIN"
            IF Obj.BaseExcl EQ 2 THEN BEGIN
                PRINTF, Unit, $
                    "      IF Event.Select THEN Sel = 'On' ELSE Sel = 'Off'"
                Format='("      ",I0,": Print,''Button ",A," Turned '', Sel")'
            ENDIF ELSE BEGIN
                Format='("      ",I0,": Print,''Button ",A," Pressed''")'
            ENDELSE

            PRINTF, Unit, "      CASE Event.Value OF"

            GetValue, Obj, Names, "<Nil BGROUP>"
            FOR I=0,N_ELEMENTS(Names)-1 DO BEGIN
                PRINTF, Unit, I, QString(Names(I)), FORMAT=Format
            ENDFOR
            PRINTF, Unit, "      ELSE: Message,'Unknown button pressed'"
            PRINTF, Unit, "      ENDCASE"
            PRINTF, Unit, "      END"
            END


        ;   All other items just need a simple skeleton
        ELSE:   BEGIN
            PRINTF, Unit, "  '", QString(UVal), "': BEGIN"
            PRINTF, Unit, "      Print, 'Event for ", QString(Id), "'"
            PRINTF, Unit, "      END"
            END
    ENDCASE

    Obj2Ptr, Obj, Ptr
END


;
;  DEP_BaseEv
;   Write an event handler for a dependent base object
;
PRO DEP_BaseEv, Unit, OldUnit, Ptr

    ; if the user has chosen a name for the event handler routine
    ; we don't write the handler

    Id  = VarId(Ptr)

    Ptr2Obj, Ptr, Obj
    IF Obj.EventFunc NE '' OR Obj.EventProc NE '' OR $
       FindMagic(Id, Unit, OldUnit) THEN BEGIN
        Obj2Ptr, Obj, Ptr
        RETURN
    ENDIF

    ; Write the routine header

    BeginMagic, Unit, Id

    PRINTF, Unit, FORMAT='(//"PRO ",A,"_Event, Event")', Id
    PRINTF, Unit, FORMAT='(//A//A/)',           $
    '  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev',  $
    '  CASE Ev OF '

    ;   Write an case statement for each child object

    DoFList, Obj.Children, 'SaveEvent', Unit

    ;   Might need a case statement for a top level base
    ;   if the user desires it to receive TLB events.
    ;   Normally a base does not generate(receive) events.

    IF Obj.TLBEvents THEN BEGIN
        PRINTF, Unit, "  '", UValue(Obj,Ptr), "': BEGIN"
        PRINTF, Unit, "      Print, 'Event for ", Id, "'"
        PRINTF, Unit, "      END"
    ENDIF

    Obj2Ptr, Obj, Ptr

    ;   Write routine footer

    PRINTF, Unit, '  ENDCASE'
    PRINTF, Unit, 'END'

    EndMagic, Unit, Id
END


;
;  GetDrawables
;   Scan object tree looking for drawables.  If we find one,
;   write code to extract the window-id of the draw widget.
;
PRO GetDrawables, Unit, Ptr

    Ptr2Obj, Ptr, Obj

    CASE Obj.Type OF

    'MAIN': DoFList, Obj.Children, 'GetDrawables', Unit
    'DEP':  DoFList, Obj.Children, 'GetDrawables', Unit
    'BASE': DoFList, Obj.Children, 'GetDrawables', Unit
    'DRAW': BEGIN
        VarName = 'DRAW' + Obj.Id
        PRINTF, Unit
        PRINTF, Unit, '  ; Get drawable window index'
        PRINTF, Unit
        PRINTF, Unit, '  COMMON ', VarName, '_Comm, ', VarName, '_Id'
        PRINTF, Unit, '  WIDGET_CONTROL, ', VarName, ', GET_VALUE=', $
                VarName, '_Id'
        END
    ELSE:
    ENDCASE
    Obj2Ptr, Obj, Ptr
END


;
;  DEP_GenWid
;   Generate IDL code for creating a top level base widget
;
PRO DEP_GenWid, Unit, Ptr, Leader

    Name    = VarId(Ptr)

    Ptr2Obj, Ptr, Obj

    ;   BASE000 = WIDGET_BASE(...

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_BASE(")', Name, /NO_EOL
    IF KEYWORD_SET(Leader) THEN BEGIN
        XPRINTF, Unit, FORMAT='("GROUP_LEADER=",A)', VarId(Leader), /NO_EOL
    ENDIF ELSE BEGIN
        ; XPRINTF cannot have FORMAT w/o a positional arg
        XPRINTF, Unit, FORMAT='(A)', "GROUP_LEADER=Group", /NO_EOL
    ENDELSE

    ;   Create flag code
    urBase_GenWid, Unit, Ptr, Obj, Name

    Obj2Ptr, Obj, Ptr

    ;   Create code to realize top level base
    PRINTF, UNIT, FORMAT='("  WIDGET_CONTROL, ",A,", /REALIZE")', Name

    ;   Write code to get window ids
    GetDrawables, Unit, Ptr
END

PRO WidDep
END
