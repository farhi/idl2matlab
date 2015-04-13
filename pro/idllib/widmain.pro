;
; $Id: widmain.pro,v 1.12 1995/01/26 03:31:38 billo Exp $
;
;  WidMain
;   Main object class definition.
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;

;
;  UpdateMainDlg
;       Set controls in top level base to reflect current settings
;       in the MAIN widget object
;
PRO UpdateMainDlg

  COMMON WidEd_Comm

    ;   Now update top dialog box to reflect changes

    Ptr2Obj, TopDlg, DialogInfo
    Ptr2Obj, TopPtr, Obj

    Foci    = DialogInfo.Foci

    WIDGET_CONTROL, Foci(0), SET_VALUE=Obj.Name
    WIDGET_CONTROL, Foci(1), SET_VALUE=Obj.NRowCol
    WIDGET_CONTROL, Foci(2), SET_VALUE=Obj.Space
    WIDGET_CONTROL, Foci(3), SET_VALUE=Obj.XPad
    WIDGET_CONTROL, Foci(4), SET_VALUE=Obj.YPad
    WIDGET_CONTROL, Foci(5), SET_VALUE=Obj.XScrollSize
    WIDGET_CONTROL, Foci(6), SET_VALUE=Obj.YScrollSize

    ;   Become a row/col/bbs.  Work around toolkit dependencies
    FOR I=0,2 DO $
        WIDGET_CONTROL, DialogInfo.Btns(I), SET_BUTTON=(Obj.BaseType EQ I)

    WIDGET_CONTROL, DialogInfo.RowColId, SENSITIVE=(Obj.BaseType NE 0)

    Obj2Ptr, Obj, TopPtr
    Obj2Ptr, DialogInfo, TopDlg
END

;
;  MAIN_Alloc
;       Allocate a main object (if Ptr is NULL or just invalid)
;   There are 3 base object classes (MAIN,DEP, and BASE) so they call
;   a common routine to allocate base objects.
;
PRO MAIN_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    ; Make a pointer. Maybe.
    IF KEYWORD_SET(Ptr) EQ 0 THEN $
        Ptr = WIDGET_BASE()
    MakeBaseObj, Parent, Obj, "MAIN"                ; Make a base object
    Obj2Ptr, Obj, Ptr                               ; Store object in pointer

END

;
;  MAIN_Destroy
;   Remove children objects and dependent bases as well.
;   We don't destroy the main base, just reset all of its fields
;   to their default state.
;
PRO MAIN_Destroy, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    DoList, Obj.Children, "Destroy"     ; Destroy children
    DoList, Obj.Next, "Destroy"         ; Destroy dependent bases
    IF WIDGET_INFO(Obj.AttrDlg, /VALID) THEN $
        WIDGET_CONTROL, Obj.AttrDlg, /DESTROY
    MakeBaseObj, 0L, CleanObj, "MAIN"       ; Create a virgin MAIN
    Obj2Ptr, CleanObj, Ptr              ; Store into used MAIN pointer

    IF WIDGET_INFO(TopDlg, /VALID) EQ 0 THEN BEGIN
        PRint,'Invalid'
        RETURN
    ENDIF

    UpdateMainDlg
END


;
;  MAIN_BarEvent
;   Events from the pull down menu don't have a <STRING> type UVALUE to
;   use.  Thus we have a separate event routine to handle the menu items.
;
;
PRO MAIN_BarEvent, Event

  COMMON WidEd_Comm

    ;   Too complicated to use the /NO_COPY for this event routine

    WIDGET_CONTROL, Event.top, GET_UVALUE=Binfo

    Build       = ''
    Do_Generate = 0

    CASE Event.Value OF

    'File.New':         Dirty, Event.Top, "creating new widget tree", "FileNew"
    'File.Open...':     Dirty, Event.Top, "opening another file", "FileOpen"

    'File.Save...':                             FileSave
    'File.Create .PRO.Normal...':               FileGenPro, 1, 0
    'File.Create .PRO.Rewrite Everything...':   FileGenPro, 1, 1
    'File.Create .PRO.Without Header...':       FileGenPro, 0, 0
    'File.Show Object Tree...':                 ShowObjTree, Binfo.ObjPtr

    'File.Quit':        BEGIN
        Dirty, Event.Top, "quitting", $
                        "WIDGET_CONTROL,/DESTROY,"+STRTRIM(Event.top)
        RETURN
        END

    'Edit.Cut...':              CutChild
    'Edit.Copy...':             CutChild, /COPY
    'Edit.Paste...':            PasteChild
    'Edit.Edit Child...':       EditChild

    'Add.Hide Tool Bar':    BEGIN
        WIDGET_CONTROL, Binfo.ToolBar, MAP=0
        WIDGET_CONTROL, Binfo.ToolId, SET_VALUE='Show Tool Bar'
        WIDGET_CONTROL, Binfo.ToolId, SET_UVALUE='Add.Show Tool Bar' ; Hack
        END
    'Add.Show Tool Bar':    BEGIN
        WIDGET_CONTROL, Binfo.ToolBar, MAP=1
        WIDGET_CONTROL, Binfo.ToolId, SET_VALUE='Hide Tool Bar'
        WIDGET_CONTROL, Binfo.ToolId, SET_UVALUE='Add.Hide Tool Bar' ; Hack
        END

    'Add.Dependent Base':   BEGIN

        ;   the Build facility below only works for adding CHILDREN
        ;   Dependent bases are siblings so we need special code to
        ;   handle that

        DEP_Build, Ptr, Base                    ; Allocate base. Make dialog
        SetTag, Ptr, "Next", NextPtr(Binfo.ObjPtr)  ; Add base to TLB list
        SetTag, Binfo.ObjPtr, "Next", Ptr
        UpdateEdit                              ; Update Cut/Paste dialogs
        Do_Generate = 1                         ; Update preview.
        END

    'Rebuild':          BEGIN

        ;   Hitting the rebuild button causes the interface to
        ;   'Accept' on a newly created widget.  After this, a 'Cancel'
        ;   will not remove the child, instead it will revert it to
        ;   the state it was in when Rebuild was clicked.

        N   = N_ELEMENTS(NewDialogs)
        FOR I=1,N-1 DO BEGIN
            IF NewDialogs(I).OldPtr EQ 0L THEN BEGIN        ; Found new widget

                Ptr     = NewDialogs(I).ObjPtr
                GetType, Ptr, Type
                ClearVar,Copy
                CALL_PROCEDURE, Type+'_Copy', Ptr, Copy     ; Copy it
                NewDialogs(I).OldPtr    = Copy              ; Save copy
            ENDIF
        ENDFOR

        Do_Generate = 1
        END
    'Help':    BEGIN
        HelpFile        = "widabout.txt"
        GET_LUN, Unit
        OPENR, Unit, HelpFile, ERROR=OpenError
        IF OpenError NE 0 THEN $
            HelpFile = FilePath("widabout.txt", SUBDIR=["help", "widget"])
        FREE_LUN, Unit
        XDisplayFile, HelpFile, TITLE='About the Widget Builder'
        END

    ELSE:   BEGIN
        IF STRMID(Event.Value,0,4) EQ "Add." THEN BEGIN
            Idx     = WHERE(STRMID(Event.Value,4,100) EQ AddList.Menu)
            Build   = AddList(Idx).Class
        ENDIF ELSE MESSAGE, 'Unprocessed event: ' + Event.Value
        END
    ENDCASE

    ;   Common functionality for most Add.XXX requests

    IF Build NE '' THEN BEGIN
        ;   Allocate object and create a dialog box as well
        Ptr = 0L                ; Make an invalid ptr but not <UNDEFINED>
        CALL_PROCEDURE, Build+'_Build', Ptr, Binfo.ObjPtr

        ;   Add child to our child list.  Note that Base object
        ;   do not get added to the active dialog box list

        AddChild, Binfo.ObjPtr, Ptr, NO_CANCEL=(Build EQ 'BASE')
    ENDIF

    ;   See if we should update the preview
    IF Do_Generate THEN Generate
END


;
;  MAIN_Event
;   Normal event handling routine for a main object
;   These affect attributes of the MAIN widget
;
PRO MAIN_Event, Event

  COMMON WidEd_Comm

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY

    Ptr2Obj, Binfo.ObjPtr, Obj

    CASE Ev OF
    'TITLE':    Obj.Name    = Event.Value

    'Bbs':      BEGIN               ; Make base a bulletin board
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

    ;   Characteristic set in main dialog

    'NROW':         Obj.NRowCol     = Event.Value
    'SPACE':        Obj.Space       = Event.Value
    'XPAD':         Obj.XPad        = Event.Value
    'YPAD':         Obj.YPad        = Event.Value
    'XSCROLL':      Obj.XScrollSize = Event.Value
    'YSCROLL':      Obj.YScrollSize = Event.Value

    ;   User wants to see the 'Additional attributes' dialog
    'ATTR':         MAIN_BuildAttr, Event.Top, Binfo.ObjPtr, Obj

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
                    WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
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
;  MAIN_Build
;   Create the top level dialog box.  Create top level object as well
;   if ptr is nil.
;
PRO MAIN_Build, Ptr, Base

  COMMON WidEd_Comm 

    MAIN_Alloc, 0L, Ptr               ; Allocate object if necessary

    Ptr2Obj, Ptr, Obj

    AddMenu = MakeAddMenu()

    ;   Create menu bar

    MenuBarDesc = [                             $
        { CW_PDMENU_S, 1, 'File' },             $
          { CW_PDMENU_S, 0, 'New' },            $
          { CW_PDMENU_S, 0, 'Open...' },        $
          { CW_PDMENU_S, 0, 'Save...' },        $
          { CW_PDMENU_S, 1, 'Create .PRO' },    $
            { CW_PDMENU_S, 0, 'Normal...' },    $
            { CW_PDMENU_S, 0, 'Rewrite Everything...' },$
            { CW_PDMENU_S, 2, 'Without Header...' },    $
          { CW_PDMENU_S, 0, 'Show Object Tree...' }, $
          { CW_PDMENU_S, 2, 'Quit' },           $
        { CW_PDMENU_S, 1, 'Edit' },             $
          { CW_PDMENU_S, 0, 'Cut...' },         $
          { CW_PDMENU_S, 0, 'Copy...' },        $
          { CW_PDMENU_S, 0, 'Paste...' },       $
          { CW_PDMENU_S, 2, 'Edit Child...' },  $
        { CW_PDMENU_S, 1, 'Add' },              $
          { CW_PDMENU_S, 0, 'Hide Tool Bar' },  $
          AddMenu,                              $
          { CW_PDMENU_S, 2, 'Dependent Base' }, $
        { CW_PDMENU_S, 0, 'Rebuild' },          $
        { CW_PDMENU_S, 2, 'Help' }              $
    ]

    Title       = "Widget Builder"

    Base        = WIDGET_BASE(/COLUMN, TITLE=Title)
    MenuBar     = CW_PDMENU(Base, MenuBarDesc, IDS=Ids, /RETURN_FULL_NAME)

    ;   Save cut/paste ids.  These change sensitivity to reflect
    ;   changes in cutlist and existence of children.

    _CutId      = Ids(11)
    _CopyId     = Ids(12)
    _PasteId    = Ids(13)
    _EditId     = Ids(14)
    ToolId      = Ids(16)

    Foci        = LONARR(7)

    ;   At this point there are no children to cut/copy or edit
    WIDGET_CONTROL, _CutId, SENSITIVE=0
    WIDGET_CONTROL, _CopyId, SENSITIVE=0
    WIDGET_CONTROL, _EditId, SENSITIVE=0

    ;   There could be something to paste if the cutlist is non-nil
    WIDGET_CONTROL, _PasteId, SENSITIVE=(CutList NE 0L)

    BuildToolBar, Base, Toolbar         ; There could be a toolbar


    Base1   = WIDGET_BASE(Base, /COLUMN, EVENT_PRO='MAIN_Event')
    Label   = WIDGET_LABEL(Base1,VALUE='Top Level Base')

    Foci(0) = Field(Base1, "Title:", Obj.Name, "TITLE", SIZE=50, /STRING)
    BuildBaseType, Base, Obj, Foci, 1, RowColId, Btns
    BuildXY, Base, Obj, Foci, 1+4, /SIZE
    Dummy   = WIDGET_BUTTON(Base, VALUE='More Attributes', $
                UVALUE='ATTR', EVENT_PRO='MAIN_Event')

    ;   Top level dialog has quite a bit of information associated with it.

    DlgInfo = { $
        Foci:       Foci, $
        CutId:      _CutId, $
        CopyId:     _CopyId, $
        PasteId:    _PasteId, $
        EditId:     _EditId, $
        RowColId:   RowColId, $
        ToolBar:    ToolBar, $
        ToolId:     ToolId, $
        Btns:       Btns, $
        ObjPtr:     Ptr $
    }

    Obj.Dialog  = Base

    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE

    WIDGET_CONTROL, Btns(1), /SET_BUTTON    ; Start life as a row
    WIDGET_CONTROL, RowColId, SENSITIVE=1

    Obj2Ptr, Obj, Ptr
END

;
;   There is no MAIN_Save (see FileSave)
;


;
;  MAIN_Restore
;   Read in an widget tree.  Restores the top base and all of its
;   children and all dependent bases as well.
;
PRO MAIN_Restore, Unit, Parent, Ptr

    Value   = 0
    READU, Unit, Value      ; Part of file format
    urBASE_Restore, Unit, Parent, Ptr, "MAIN"

    ;   Restore Dep Bases

    Last    = Ptr
    WHILE 1 DO BEGIN
        Value   = 0
        READU, Unit, Value
        IF Value NE 1 THEN RETURN           ; Exit here

        ClearVar, Dep
        DEP_Restore, Unit, 0L, Dep
        SetTag, Last, "Next", Dep
        Last    = Dep
    ENDWHILE
END


;
;   MAIN_BuildAttr
;
;   Code for this is so similar to DEP_BuildAttr that I combined them
;   They differed by 1 line
;
PRO MAIN_BuildAttr, Leader, Ptr, Obj
    DEP_BuildAttr, Leader, Ptr, Obj
END

;
;  ShowTree
;   Write an ASCII description of the current object tree
;   Indentation indicates parenting.
;
PRO ShowTree, Unit, Ptr, Level

    ; ASCII Assumption: 32b is a space

    ;   Indentation shows hierarchy

    PRINTF, Unit, STRING(REPLICATE(32b,1 + Level * 3)), $
        VarId(Ptr) + "  ( " + GetId(Ptr) + " )"

    ;   look inside objects with children

    Ptr2Obj, Ptr, Obj
    IF TAG_NAMES(Obj, /STRUCTURE) EQ 'WE_BASE' THEN BEGIN
        Child       = Obj.Children
        NewLevel    = Level + 1
        WHILE Child NE 0L DO BEGIN
            ShowTree, Unit, Child, NewLevel
            Child   = NextPtr(Child)
        ENDWHILE
    ENDIF
    Obj2Ptr, Obj, Ptr
END

;
;  ShowObjTree
;   Create a temp file. Write a description of the object tree
;   to that file and display that file.
;
PRO ShowObjTree, Ptr

    ;   Try to make a filename that will work on all
    ;   operating systems, including special ones
    ;   which only allow 8 character file names. (MS-DOS)
    ;   and ones which require a '.' in every filename (VMS)

    ;   This is the low 5 digits of the system time (to the nearest 1/100th sec)
    ;   Names only collided if:
    ;       They are using the same directory and
    ;       they happen to call ShowObjTree in the same time. (t < 1/100 sec)

    Magic       = LONG( (SysTime(1) * 100.0) MOD 100000.0)

    FileName    = 'wid'+STRTRIM(Magic, 2)+'.tmp'
    OPENW, Unit, FileName, /GET_LUN

    ;   Build file contents

    WHILE Ptr NE 0 DO BEGIN
        Showtree, Unit, Ptr, 0
        Next    = NextPtr(Ptr)
        IF Next NE 0L THEN BEGIN
            PRINTF,Unit
            PRINTF,Unit,'---------------'
            PRINTF,Unit
        ENDIF
        Ptr = Next
    ENDWHILE

    FLUSH, Unit
    CLOSE, Unit

    XDisplayFile, FileName, TITLE='Current Widget Tree'     ; Show it

    ;   Reopen the file just to delete it

    OPENW, Unit, FileName, /DELETE
    CLOSE, Unit
    FREE_LUN, Unit
END

PRO WidMain
END
