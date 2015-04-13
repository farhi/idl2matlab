;
; $Id: slider_alloc.pro,v 1.8 1995/01/20 19:41:01 tonyh Exp $
;
;  WidSlider
;   Widget Slider class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  SLIDER_Icon
;       Return the slider toolbar icon
;
FUNCTION SLIDER_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 2b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 128b, 3b, 0b ], $
    [ 0b, 192b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 0b, 3b, 0b ], $
    [ 0b, 192b, 15b, 0b ], $
    [ 0b, 224b, 31b, 0b ], $
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
;  SLIDER_Copy
;   Copy a slider.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO SLIDER_Copy, Ptr, Copy
    GenCopy, Ptr, Copy
END


;
;  SLIDER_Destroy
;   Release resources for the given slider
;
PRO SLIDER_Destroy, Ptr
    GenDestroy, Ptr
END


;
;  SLIDER_Event
;   Event handling routine for slider dialog
;
PRO SLIDER_Event, Event
    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev                 ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY   ; Get Dialog Info
    Ptr2Obj, Binfo.ObjPtr, Obj                              ; Get Object

    CASE Ev OF

    'NAME':     Obj.Name        = Event.Value
    'TITLE':    Obj.Title       = Event.Value
    'SMIN':     Obj.SliderMin   = Event.Value
    'SINIT':    Obj.SliderStart = Event.Value
    'SMAX':     Obj.SliderMax   = Event.Value
    'FONT':     Obj.Font        = Event.Value
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value
    'XSIZE':    Obj.XSize       = Event.Value
    'YSIZE':    Obj.YSize       = Event.Value
    'XOFFSET':  Obj.XOffset     = Event.Value
    'YOFFSET':  Obj.YOffset     = Event.Value

    'XFONT':    DoXFont, Obj, Binfo.Foci(4)

    'DO_DRAG':      Obj.SliderDrag      = 1 - Obj.SliderDrag
    'DO_VERTICAL':  Obj.SliderVertical  = 1 - Obj.SliderVertical
    'DO_SUPPRESS':  Obj.SliderSuppress  = 1 - Obj.SliderSuppress

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
;  SLIDER_Build
;   Create a dialog box a slider object.  If ptr is nil then
;   create the object as well.
;
PRO SLIDER_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    SLIDER_Alloc, ParPtr, Ptr               ; Allocate object if necessary
    MgrName = 'WE_SLIDER' + STRTRIM(Ptr, 2) ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN   ; See if it already exists

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
    Foci    = LONARR(12)

    ;   Event Related Info

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
    Foci(0) = Field(Base1, "Slider Title:",Obj.Title,'TITLE',SIZE=50,/STRING)
    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(1) = Field(Base2, "Minimum:", Obj.SliderMin, 'SMIN', SIZE=8, /LONG)
    Foci(2) = Field(Base2, "Initial:", Obj.SliderStart, 'SINIT', SIZE=8, /LONG)
    Foci(3) = Field(Base2, "Maximum:", Obj.SliderMax, 'SMAX', SIZE=8, /LONG)

    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(4) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")

    Base2   = WIDGET_BASE(Base1, /ROW, /NONEXCLUSIVE)
    Button  = WIDGET_BUTTON(Base2, VALUE='Generate Drag Events', $
                            UVALUE='DO_DRAG')
    IF Obj.SliderDrag THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Vertical Slider', $
                            UVALUE='DO_VERTICAL')
    IF Obj.SliderVertical THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Suppress display of current value', $
                            UVALUE='DO_SUPPRESS')
    IF Obj.SliderDrag THEN WIDGET_CONTROL, Button, /SET_BUTTON

    BuildOther, Base1, Obj, Foci, 5, /FRAME

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Slider Appearance Controls")
    BuildXY, Base1, Obj, Foci, 8, /SIZE, /OFFSET
    BuildOkCancel, Base, Obj

    DlgInfo     = { $
        Foci:       Foci, $
        ObjPtr:     Ptr $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='SLIDER_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  SLIDER_Save
;   Save slider information to a file.
;   This is a simple object to save.
;
PRO SLIDER_Save, Unit, Ptr
    GenWrite, Unit, Ptr
END


;
;  SLIDER_Restore
;   Read in a slider object from a file
;
PRO SLIDER_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "SLIDER", 0
END


;
;  SLIDER_Generate
;   Create a slider object for previewing
;
PRO SLIDER_Generate, Base, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Value may not be outside of min/max

    Value   = Obj.SliderMin > Obj.SliderStart < Obj.SliderMax

    Cmd = 'Id = WIDGET_SLIDER(Base'
    SAddCmd, Cmd, Obj.Font, 'FONT'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IF Obj.SliderMin NE Obj.SliderMax THEN BEGIN
        IAddCmd, Cmd, Obj.SliderMin, 'MINIMUM', /FORCE
        IAddCmd, Cmd, Obj.SliderMax, 'MAXIMUM', /FORCE
    ENDIF
    IAddCmd, Cmd, Obj.SliderSuppress, 'SUPPRESS_VALUE'
    SAddCmd, Cmd, Obj.Title, 'TITLE'
    IAddCmd, Cmd, Value, 'VALUE', /FORCE
    IAddCmd, Cmd, Obj.SliderVertical, 'VERTICAL'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'

    Obj2Ptr, Obj, Ptr

    ; Create slider

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Slider ' + VarName(Ptr)
    ENDIF
END


;
;  SLIDER_GenWid
;   Create IDL code for creating a SLIDER
;
PRO SLIDER_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)            ; Get name for slider
    Ptr2Obj, Ptr, Obj               ; Get object information

    ;   Value may not be outside of min/max

    Value   = Obj.SliderMin > Obj.SliderStart < Obj.SliderMax

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_SLIDER( ",A)', $
        Name, Parent, /NO_EOL
    ISaveCmd, Unit, Obj.SliderDrag, "DRAG"
    SSaveCmd, Unit, Obj.Font, "FONT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    IF Obj.SliderMin NE Obj.SliderMax THEN BEGIN
        ISaveCmd, Unit, Obj.SliderMax, "MAXIMUM", /FORCE
        ISaveCmd, Unit, Obj.SliderMin, "MINIMUM", /FORCE
    ENDIF
    ISaveCmd, Unit, Obj.SliderSuppress, "SUPPRESS"
    SSaveCmd, Unit, Obj.Title, "TITLE"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Value, "VALUE", /FORCE
    ISaveCmd, Unit, Obj.SliderVertical, "VERTICAL"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  SLIDER_Alloc
;       Allocate a slider object.  Don't allocate if ptr is non-nil
;
PRO SLIDER_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr = WIDGET_BASE(GROUP=TopDlg)         ; Make a pointe

    Obj = {                     $
        WE_SLIDER,              $
        Type:           'SLIDER',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; object name
        Title:          '',     $ ; Slider title
        FrameSize:      0,      $
        Font:           '',     $
        XSize:          0,      $
        YSize:          0,      $
        XOffset:        0,      $
        YOffset:        0,      $
        UValue:         '',     $
        SliderDrag:     0,      $ ; Slider flags
        SliderVertical: 0,      $
        SliderSuppress: 0,      $
        SliderMin:      0L,     $ ; Slider basic info
        SliderStart:    0L,     $
        SliderMax:      0L      $
    }
    Obj2Ptr, Obj, Ptr
END
