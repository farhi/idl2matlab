;
; $Id: fslid_alloc.pro,v 1.7 1994/06/01 23:08:48 ali Exp $
;
;  WidFslider
;   Widget Fslider class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  FSLID_Icon
;       Return the floating point slider toolbar icon
;
FUNCTION FSLID_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 4b, 192b, 1b ], $
    [ 0b, 6b, 224b, 3b ], $
    [ 0b, 7b, 48b, 6b ], $
    [ 128b, 7b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 24b, 12b ], $
    [ 0b, 6b, 48b, 6b ], $
    [ 128b, 31b, 227b, 3b ], $
    [ 192b, 63b, 195b, 1b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 12b, 248b, 127b, 16b ], $
    [ 12b, 8b, 67b, 32b ], $
    [ 12b, 8b, 67b, 16b ], $
    [ 12b, 8b, 67b, 32b ], $
    [ 12b, 8b, 67b, 16b ], $
    [ 12b, 8b, 67b, 32b ], $
    [ 12b, 248b, 127b, 16b ], $
    [ 172b, 170b, 170b, 42b ], $
    [ 84b, 85b, 85b, 21b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  FSLID_Copy
;   Copy a floating point slider.
;
PRO FSLID_Copy, Ptr, Copy
    GenCopy, Ptr, Copy
END


;
; FSLID_Destroy
;   Release resources for the given floating point slider
;
PRO FSLID_Destroy, Ptr
    GenDestroy, Ptr
END


;
;  FSLID_Event
;   Event handling routine for floating point slider dialog
;
PRO FSLID_Event, Event
    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY
    Ptr2Obj, Binfo.ObjPtr, Obj

    CASE Ev OF

    'TITLE':    Obj.Title       = Event.Value
    'SMIN':     Obj.FslidMin    = Event.Value
    'SINIT':    Obj.FslidStart  = Event.Value
    'SMAX':     Obj.FslidMax    = Event.Value
    'NAME':     Obj.Name        = Event.Value
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value
    'FORMAT':   Obj.LabelFormat = Event.Value
    'XSIZE':    Obj.XSize       = Event.Value
    'YSIZE':    Obj.YSize       = Event.Value

    'DO_DRAG':      Obj.FslidDrag       = 1 - Obj.FslidDrag
    'DO_VERTICAL':  Obj.FslidVertical   = 1 - Obj.FslidVertical
    'DO_SUPPRESS':  Obj.FslidSuppress   = 1 - Obj.FslidSuppress

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

    Dirty   = 1
    ;   Goto next input field on a <CR>
    SetNextFocus, Binfo, Event
    Obj2Ptr, Obj, Binfo.ObjPtr
    WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  FSLID_Build
;   Create a dialog box a floating point slider object.  If ptr is nil then
;   create the object as well.
;
PRO FSLID_Build, Ptr, ParPtr
  COMMON WidEd_Comm

    FSLID_Alloc, ParPtr, Ptr
    MgrName = 'WE_FSLID' + STRTRIM(Ptr, 2)
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
    Foci    = LONARR(10)

    ;   Event Related Info

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
    Foci(0) = Field(Base1, "Slider Title:",Obj.Title,'TITLE',SIZE=50,/STRING)
    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(1) = Field(Base2, "Minimum:", Obj.FslidMin, 'SMIN', SIZE=15, /FLOAT)
    Foci(2) = Field(Base2, "Initial:",Obj.FslidStart,'SINIT',SIZE=15, /FLOAT)
    Foci(3) = Field(Base2, "Maximum:", Obj.FslidMax, 'SMAX', SIZE=15, /FLOAT)

    Base2   = WIDGET_BASE(Base1, /ROW, /NONEXCLUSIVE)
    Button  = WIDGET_BUTTON(Base2, VALUE='Generate Drag Events', $
                            UVALUE='DO_DRAG')
    IF Obj.FslidDrag THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Vertical Slider', $
                            UVALUE='DO_VERTICAL')
    IF Obj.FslidVertical THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Suppress display of current value', $
                            UVALUE='DO_SUPPRESS')
    IF Obj.FslidDrag THEN WIDGET_CONTROL, Button, /SET_BUTTON

    BuildOther, Base1, Obj, Foci, 4, /FRAME

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Slider Appearance Controls")
    Foci(7) = Field(Base1, "Label Format:", Obj.LabelFormat, "FORMAT", $
                    SIZE=30, /STRING)
    BuildXY, Base1, Obj, Foci, 8, /SIZE
    BuildOkCancel, Base, Obj

    DlgInfo     = { $
        Foci:       Foci, $
        ObjPtr:     Ptr $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='FSLID_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  FSLID_Save
;   Save floating point slider information to a file.
;   This is a simple object to save.
;
PRO FSLID_Save, Unit, Ptr
    GenWrite, Unit, Ptr
END


;
;  FSLID_Restore
;   Read in a floating point slider object from a file
;
PRO FSLID_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "FSLID", 0
END


;
;  FSLID_Generate
;   Create a floating point slider object for previewing
;
PRO FSLID_Generate, Base, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Value may not be outside of min/max
    Value   = Obj.FSlidMin > Obj.FSlidStart < Obj.FSlidMax

    Cmd = 'Id = CW_FSLIDER(Base'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IF Obj.FslidMin NE Obj.FslidMax THEN BEGIN
        IAddCmd, Cmd, Obj.FslidMin, 'MINIMUM', /FORCE
        IAddCmd, Cmd, Obj.FslidMax, 'MAXIMUM', /FORCE
    ENDIF
    IAddCmd, Cmd, Obj.FslidSuppress, 'SUPPRESS_VALUE'
    SAddCmd, Cmd, Obj.Title, 'TITLE'
    IAddCmd, Cmd, Value, 'VALUE', /FORCE
    IAddCmd, Cmd, Obj.FslidVertical, 'VERTICAL'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'

    Obj2Ptr, Obj, Ptr

    ; Create fslid
    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Flt Slider ' + VarName(Ptr)
    ENDIF
END

;
;  FSLID_GenWid
;   Create IDL code for creating a floating point slider (CW_FSLIDER)
;
PRO FSLID_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)
    Ptr2Obj, Ptr, Obj

    ;   Value may not be outside of min/max
    Value   = Obj.FSlidMin > Obj.FSlidStart < Obj.FSlidMax

    XPRINTF, Unit, FORMAT='("  ",A," = CW_FSLIDER( ",A)', $
        Name, Parent, /NO_EOL
    ISaveCmd, Unit, Obj.FSlidDrag, "DRAG"
    SSaveCmd, Unit, Obj.LabelFormat, "FORMAT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    IF Obj.FslidMin NE Obj.FslidMax THEN BEGIN
        ISaveCmd, Unit, Obj.FSlidMax, "MAXIMUM", /FORCE
        ISaveCmd, Unit, Obj.FSlidMin, "MINIMUM", /FORCE
    ENDIF
    ISaveCmd, Unit, Obj.FSlidSuppress, "SUPPRESS"
    SSaveCmd, Unit, Obj.Title, "TITLE"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Value, "VALUE", /FORCE
    ISaveCmd, Unit, Obj.FSlidVertical, "VERTICAL"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  FSLID_Alloc
;       Allocate a floating point slider object.  Don't allocate if ptr
;   is non-nil
;
PRO FSLID_Alloc, Parent, Ptr
  COMMON WidEd_Comm    

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN

    Ptr = WIDGET_BASE(GROUP=TopDlg)

    Obj = {                     $
        WE_FSLID,               $
        Type:           'FSLID',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; object name
        Title:          '',     $ ; Fslid title
        FrameSize:      0,      $
        LabelFormat:    '',     $
        XSize:          0,      $
        YSize:          0,      $
        UValue:         '',     $
        FslidDrag:      0,      $ ; slider flags
        FslidVertical:  0,      $
        FslidSuppress:  0,      $
        FslidMin:       0.0,    $ ; slider basic info
        FslidStart:     0.0,    $
        FslidMax:       0.0     $
    }
    Obj2Ptr, Obj, Ptr
END
