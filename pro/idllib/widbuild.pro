;
; $Id: widbuild.pro,v 1.9 1994/06/01 23:08:48 ali Exp $
;
;  WidBuild
;   Widget Editor common routines related to building dialog boxes. Mostly.
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;


;
;  MakeBaseObj
;       Create a base object of the given type.  All use the same structure
;   but are different object types (MAIN, DEP, BASE)
;
PRO MakeBaseObj, Parent, Obj, Type

    Obj = {                     $
        WE_BASE,                $
        Type:           Type,   $ ; Set Type
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        TB_Showing:     1,      $ ; Is Tool Bar Showing?
        Dialog:         0L,     $
        AttrDlg:        0L,     $
        Next:           0L,     $
        Name:           '',     $ ; Title or object name
        Children:       0L,     $ ; linked list of children
        LastChild:      0L,     $ ; Last child (makes adding children easier)
        FrameSize:      0,      $
        EventFunc:      '',     $ ; UNUSED.
        EventProc:      '',     $
        GetFunc:        '',     $ ; func_get_value
        SetProc:        '',     $ ; pro_set_value
        KillProc:       '',     $ ; kill_notify
        XSize:          0,      $
        YSize:          0,      $
        XOffset:        0,      $
        YOffset:        0,      $
        UValue:         '',     $
        Space:          0,      $
        XPad:           0,      $
        YPad:           0,      $
        BaseType:       1,      $ ; Enum { BBS, Row, Col } (Row is dflt)
        NRowCol:        1,      $ ; Number of Rows/Columns
        BaseMapped:     1,      $ ; Is Base Visible?
        TLBEvents:      0,      $ ; Base accepts TLB size events?
        XScrollSize:    0,      $
        YScrollSize:    0       $
    }
END


;
;  MakeAddMenu
;   Create a series of pulldown menu items based on the class
;   definition list
;
FUNCTION MakeAddMenu

  COMMON WidEd_Comm

    Dummy       = { CW_PDMENU_S, flags:0, name:'' }
    Menu        = REPLICATE(Dummy, N_ELEMENTS(AddList))
    Menu.name   = AddList.Menu
    RETURN, Menu
END


;
;  ToolBar_Event
;       Repackage a button press to look like a menu press
;
PRO ToolBar_Event, Event

  COMMON WidEd_Comm

    ;   We will need the pointer for the Object associated with this
    ;   dialog box (the parent for the children we are about to create)

    ;   Can't use /NO_COPY because AddChild assumed TopDlg's UVALUE
    ;   will always be valid and Event.Top may be TopDlg

    WIDGET_CONTROL, Event.top, GET_UVALUE=Binfo

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Build

    ;   Allocate object and create a dialog box as well
    CALL_PROCEDURE, Build + '_Build', Ptr, Binfo.ObjPtr

    ;   Add child to our child list.  Note that Base object
    ;   do not get added to the active dialog box list
    AddChild, Binfo.ObjPtr, Ptr, NO_CANCEL=(Build EQ 'Base')

    ;   Restore dialog box information
    WIDGET_CONTROL, Event.top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  BuildToolBar
;   Create a series of bitmapped buttons
;
PRO BuildToolBar, Base, ToolBase

  COMMON WidEd_Comm

    I=0
    N=N_ELEMENTS(AddList)
    WHILE I LT N DO BEGIN
        Icon    = CALL_FUNCTION(Addlist(I).Class + "_Icon")
        IF N_ELEMENTS(Icon) NE 1 THEN BEGIN
            ToolBase    = WIDGET_BASE(Base, /ROW, /FRAME, $
                                SPACE=1, EVENT_PRO='ToolBar_Event')
            FOR I=0,N-1 DO BEGIN
                Icon    = CALL_FUNCTION(Addlist(I).Class + "_Icon")
                IF N_ELEMENTS(Icon) NE 1 THEN BEGIN
                     Button     = WIDGET_BUTTON(ToolBase, VALUE=Icon, $
                                                UVALUE=AddList(I).Class)
                ENDIF
            ENDFOR
        ENDIF

    ENDWHILE

END



;
;  Field
;       Common entry point for creation of a dialog box information entry
;   object (CW_FIELD) for user entry of a value.  Simpler to use
;   layer because we can change options in a single location (here)
;   for (near) global effect.
;
;   Returns Widget Id.
;
FUNCTION Field, Base, Title, Value, Uvalue, $
    SIZE=Size, INT=Int, STRING=String, LONG=Long, FLOAT=Float

  COMMON WidEd_Comm

    RETURN, CW_FIELD(Base, TITLE=Title, VALUE=Value, UVALUE=Uvalue, $
                XSIZE=Size, /ALL_EVENTS, TEXT_FRAME=OnAPC, $
                INT=Int, STRING=String, LONG=Long, FLOAT=Float)
END


;
;  BuildBaseType
;   All base objects have within them a sub base used to set base
;   type information (row v. column, child spacing, etc.).
;
;   Create that information.  Builder provides object information,
;
;   The fields created are used to fill in Focus information starting
;   FOff.  Btns are saved for later showing user object state.
;   RowColId is needed by the parent dialog for enabling/disabling
;   Row-Column information as user changed base type selection.
;
PRO BuildBaseType, Base, Obj, Foci, FOff, RowColId, Btns, ROW=Row

    Btns    = LONARR(3)

    ;   Events get sent to object's event routine (should be called XXX_Event
    Base1   = WIDGET_BASE(Base, /COLUMN, /FRAME, EVENT_PRO=Obj.Type+'_Event')

    ;   Support different widget layouts

    IF KEYWORD_SET(Row) THEN BEGIN
        BBase   = WIDGET_BASE(Base1, /ROW)
        LabVal  = "Base Type:"
    ENDIF ELSE BEGIN
        BBase   = Base1
        LabVal  = "Base Type"
    ENDELSE

    ;   Create subbase contents

    Label   = WIDGET_LABEL(BBase, VALUE=LabVal)
    Base2   = WIDGET_BASE(BBase, /ROW, /EXCLUSIVE)
    Btns(1) = WIDGET_BUTTON(Base2, VALUE='Row', UVALUE='Row', /NO_RELEASE)
    Btns(2) = WIDGET_BUTTON(Base2, VALUE='Column', UVALUE='Column', /NO_REL)
    Btns(0) = WIDGET_BUTTON(Base2, VALUE='Bulletin Board',  $
                    UVALUE='Bbs', /NO_RELEASE)

    RowColId        = WIDGET_BASE(Base1, /COLUMN)
    Foci(FOff)      = Field(RowColId,"Number of Rows/Columns", $
                        Obj.NRowCol, "NROW", /INT, SIZE=5)
    RCBase          = WIDGET_BASE(RowColId, /ROW)
    Foci(FOff+1)    = Field(RcBase, 'Child Spacing:', $
                        Obj.Space, "SPACE", /INT, SIZE=8)
    Foci(FOff+2)    = Field(RcBase, 'X Padding:', $
                        Obj.XPad, "XPAD", /INT, SIZE=8)
    Foci(FOff+3)    = Field(RcBase, 'Y Padding:', $
                        Obj.YPad, "YPAD", /INT, SIZE=8)
END


;
;  BuildXY
;   Every one of the dialog boxes have some fields which control
;   object size, position and/or scroll size.  This routine creates
;   the fields for controlling that information.
;
;   If only 1 keyword is specified, put the X & Y fields in the same row,
;   otherwise, put all the X information in a top row and all the Y
;   information in a lower row.
;
;   The implementation of this routine is obscure. Sorry.
;   However, the usage is pretty straight forward. (c.f. XXX_Build routines)
;
PRO BuildXY, Base, Obj, Foci, FOff, $
    SCROLL=DoScroll, SIZE=DoSize, OFFSET=DoOffset, ALL=All

    ;   Make all of the flags have a value of 0 or 1

    DoSize      = KEYWORD_SET(DoSize)
    DoOffset    = KEYWORD_SET(DoOffset)
    DoScroll    = KEYWORD_SET(DoScroll)

    IF KEYWORD_SET(All) THEN BEGIN
        DoSize      = 1
        DoOffset    = 1
        DoScroll    = 1
    ENDIF

    ;   Y entries will be after X entries
    YOff = DoSize + DoOffset + DoScroll

    ;   Single request?  Make a single row

    IF YOff EQ 1 THEN BEGIN
        Base1   = WIDGET_BASE(Base, /ROW, EVENT_PRO=Obj.Type+'_Event')
        Base2   = Base1
        Base3   = Base1

    ;   Multiple types? Make an X and a Y row
    ENDIF ELSE BEGIN
        Base1   = WIDGET_BASE(Base, /COLUMN, EVENT_PRO=Obj.Type+'_Event')
        Base2   = WIDGET_BASE(Base1, /ROW)
        Base3   = WIDGET_BASE(Base1, /ROW)
    ENDELSE

    ;   Create Size entries
    IF DoSize THEN BEGIN
        Foci(Foff)      = Field(Base2, "X Size:", Obj.XSize, "XSIZE", $
                                /INT, SIZE=8)
        Foci(Foff+YOff) = Field(Base3, "Y Size:", Obj.YSize, "YSIZE", $
                                /INT, SIZE=8)
    ENDIF

    ;   Create Offset Entries
    IF DoOffset THEN BEGIN
        Off = FOff + DoSize
        Foci(Off)       = Field(Base2, "X Offset:", Obj.XOffset, "XOFFSET", $
                                /INT, SIZE=8)
        Foci(Off+YOff)  = Field(Base3, "Y Offset:", Obj.YOffset, "YOFFSET", $
                                    /INT, SIZE=8)
    ENDIF

    ;   Create Scrollsize Entries
    IF DoScroll THEN BEGIN
        Off = FOff + DoSize + DoOffset
        Foci(Off)       = Field(Base2, "X Scroll Size:", $
                                    Obj.XScrollSize, "XSCROLL", /INT, SIZE=8)
        Foci(Off+YOff)  = Field(Base3, "Y Scroll Size:", $
                                    Obj.YScrollSize, "YSCROLL", /INT, SIZE=8)
    ENDIF
END


;
;  BuildOkCancel
;
;   Create Done and Cancel buttons.  Common to the dialog of
;   many object classes.
;
PRO BuildOkCancel, Base, Obj

    Dummy       = WIDGET_LABEL(Base, VALUE=' ')
    Base1       = WIDGET_BASE(Base, /ROW, EVENT_PRO=Obj.Type+'_Event')
    DoneTxt     = '          Done         '
    CancelTxt   = '         Cancel        '
    Dummy       = WIDGET_BUTTON(Base1, VALUE=DoneTxt, UVALUE='DONE' )
    Dummy       = WIDGET_BUTTON(Base1, VALUE=CancelTxt, UVALUE='CANCEL' )
END


;
;  BuildOther
;
;   Many dialogs have a Name, Frame and UVALUE entry.  Common support
;   for creating those items.
;
PRO BuildOther, Base, Obj, Foci, FOff, FRAME=Frame

    Base1           = WIDGET_BASE(Base, FRAME=KEYWORD_SET(Frame), /COLUMN)
    Lab             = WIDGET_LABEL(Base1, VALUE="Other Controls")
    Foci(Foff)      = Field(Base1, "Name:", Obj.Name, 'NAME', SIZE=50, /STRING)
    Base2           = WIDGET_BASE(Base1, /ROW)
    Foci(Foff+1)    = Field(Base2, "Frame Size:", Obj.FrameSize, $
                            "FRAME", /INT, SIZE=8)
    Foci(Foff+2)    = Field(Base2, "User Value:", Obj.UValue, "UVALUE", $
                            SIZE=20, /STRING)
END

;
;  BuildEdit
;   Dialogs for objects which have a STRARR for a value (LIST, BGROUP, etc.)
;   need a Edit Control for entering said text.  Common code for
;   building that portion of the dialog
;
;   Commented portions should be uncommented to add support for using
;   IDL code to generate VALUE (v. Literal strings).
;   This cannot work until EXECUTE won't stop the editor if the user
;   has a typo
;
PRO BuildEdit, Base, Obj, Edit, SINGLE=Single

    Base1       = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Label       = WIDGET_LABEL(Base1, VALUE=Obj.Type + " Widget Value")
    Base2       = WIDGET_BASE(Base1, /ROW)
;   Base3       = WIDGET_BASE(Base2, /COLUMN, /EXCLUSIVE)
;   Button1     = WIDGET_BUTTON(Base3, VALUE="Use Literal Text", $
;                           UVALUE="LITERAL")
;   WIDGET_CONTROL, Button1, /SET_BUTTON
;   Button2     = WIDGET_BUTTON(Base3, VALUE="Use IDL Code", $
;                           UVALUE="CODEBASED");
;   WIDGET_CONTROL, Button2, SENSITIVE=0

    Base3       = WIDGET_BASE(Base2, /COLUMN)
    WIDGET_CONTROL, Obj.Value1, GET_UVALUE=StrValue
    IF KEYWORD_SET(Single) THEN YSize=1 ELSE YSize=5
    MainText    = CW_FIELD(Base3, XSIZE=50, YSIZE=YSize, /ALL, TITLE=" ", $
                        VALUE=StrValue, UVALUE='MAINTEXT')
;   Edit        = CW_FIELD(Base3, XSIZE=44, /ALL, TITLE="VALUE=", $
;                       VALUE=Obj.Value2, UVALUE='VALUETEXT')
;   WIDGET_CONTROL, Edit, SENSITIVE=Obj.ValueType
    Edit        = 0L
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   General Event Routines
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;  DoXFont
;   Run the XFont utility to allow the user to choose a font.
;   Set the 'Font:' field to correspond to the user's choice.
;   Only runs on a system running X.
;
PRO DoXFont, Obj, Id
    WIDGET_CONTROL, /HourGlass
    NewFontName = XFont()
    IF NewFontName NE '' THEN BEGIN
        WIDGET_CONTROL, Id, SET_VALUE=NewFontName
        Obj.Font    = NewFontName
    ENDIF
END

;
;  MISC_Kill
;   Called whenever an active dialog box is destroyed (except for
;   base objects [DEP and BASE]).  IF the dialog still has
;   information attached to it AND the object has not been destroyed
;   THEN clear the dialog widget id contained in the object -- indicating
;   that it no longer has an active dialog box associated with it.
;
PRO MISC_Kill, Dlg

  COMMON WidEd_Comm

    ;   Get dialog info
    WIDGET_CONTROL, Dlg, GET_UVALUE=Binfo, /NO_COPY

    IF KEYWORD_SET(Binfo) THEN BEGIN        ; Dialog info valid?

        IF WIDGET_INFO( BInfo.ObjPtr, /VALID) THEN BEGIN    ; ObjPtr valid?

            Ptr2Obj, Binfo.ObjPtr, Obj      ; Get Object

            ;   We may not have any object associated with the
            ;   dialog -- if we here as a result of the object
            ;   being destroyed programmatically.  In that case,
            ;   just don't do anything

            IF N_ELEMENTS(Obj) NE 0 THEN BEGIN  ; Object Valid?
                Obj.Dialog = 0L                 ; Clear Dialog Id
                Obj2Ptr, Obj, Binfo.ObjPtr      ; put object back into pointer
            ENDIF

            ;   See if we have a copy of this object (allows cancel to work)
            ;   If we do, delete extra copy of the object and update the
            ;   active list

            Idx = WHERE(NewDialogs.ObjPtr EQ Binfo.ObjPtr, Count)
            IF Count EQ 1 THEN BEGIN

                Idx     = Idx(0)    ; Where it is in the active list

                ;   Destroy the copy.  BASE objects should never be in
                ;   NewDialogs. Copy (oldptr) may have a valid pointer to
                ;   the dialog. This is bad. If there is a copy and it is
                ;   destroyed, the dialog would be destroyed twice (once
                ;   by user action, once by this routine. The second
                ;   deletion would fail and crashes the widget editor)

                IF NewDialogs(Idx).OldPtr NE 0L THEN BEGIN
                    Ptr = NewDialogs(Idx).OldPtr    ; Get Copy Pointer
                    Ptr2Obj, Ptr, Obj               ; Get copy object

                    ;  There may exist a point in destroying a dialog
                    ;  where we are in the process of destroying the
                    ;  object associated with this dialog and hence
                    ;  things are half destroyed so we check for that
                    IF N_ELEMENTS(Obj) NE 0 THEN BEGIN
                        Obj.Dialog      = 0         ; Clear dialog box pointer
                        Obj2Ptr, Obj, Ptr           ; Put obj back into pointer
                        Destroy, Ptr            ; Destroy it (but not dialog)
                    ENDIF
                ENDIF

                ;   Now we have to remove the deleted object from the list
                ;   of objects with active dialog boxes.

                N   = N_ELEMENTS(NewDialogs)
                IF Idx EQ N-1 THEN $                    ; Last active object?
                    NewDialogs  = NewDialogs(0:N-2) $
                ELSE $
                    NewDialogs  = [ NewDialogs(0:Idx-1), NewDialogs(Idx+1,*) ]
            ENDIF
        ENDIF
    ENDIF

END

;
;  MISC_Event
;   Event processing for simple dialog boxes.  Used by many
;   of the simple object classes.
;
PRO MISC_Event, Event, FontIdx

  COMMON WidEd_Comm 

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev                 ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY   ; Get Dialog info
    Ptr2Obj, Binfo.ObjPtr, Obj                              ; Get Object

    CASE Ev OF

    'VALUE':    Obj.Value       = Event.Value               ; Generic Fields
    'FONT':     Obj.Font        = Event.Value
    'NAME':     Obj.Name        = Event.Value
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value
    'XSIZE':    Obj.XSize       = Event.Value
    'YSIZE':    Obj.YSize       = Event.Value
    'XOFFSET':  Obj.XOffset     = Event.Value
    'YOFFSET':  Obj.YOffset     = Event.Value
    'XSCROLL':  Obj.XScrollSize = Event.Value
    'YSCROLL':  Obj.YScrollSize = Event.Value

    'XFONT':    DoXFont, Obj, Binfo.Foci(FontIdx)

    'DO_BUTTON':    Obj.DrawBtnEv   = 1 - Obj.DrawBtnEv     ; DRAW Widget only
    'DO_MOTION':    Obj.DrawMoEv    = 1 - Obj.DrawMoEv
    'NO_STORE':     Obj.DrawRetain  = 0
    'SVR_STORE':    Obj.DrawRetain  = 1
    'IDL_STORE':    Obj.DrawRetain  = 2

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

PRO WidBuild
END
