;
; $Id: button_alloc.pro,v 1.8 1995/01/20 19:41:01 tonyh Exp $
;
;  WidButton
;   Widget Button class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;


;
;  BUTTON_Icon
;       Return the button toolbar icon
;
FUNCTION BUTTON_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 168b, 170b, 170b, 42b ], $
    [ 84b, 85b, 85b, 53b ], $
    [ 168b, 170b, 170b, 58b ], $
    [ 20b, 0b, 0b, 56b ], $
    [ 8b, 0b, 0b, 56b ], $
    [ 20b, 0b, 0b, 56b ], $
    [ 8b, 128b, 1b, 56b ], $
    [ 20b, 128b, 1b, 56b ], $
    [ 8b, 128b, 1b, 56b ], $
    [ 20b, 64b, 3b, 56b ], $
    [ 8b, 64b, 3b, 56b ], $
    [ 20b, 64b, 3b, 56b ], $
    [ 8b, 32b, 6b, 56b ], $
    [ 20b, 32b, 6b, 56b ], $
    [ 8b, 32b, 6b, 56b ], $
    [ 20b, 16b, 12b, 56b ], $
    [ 8b, 240b, 15b, 56b ], $
    [ 20b, 16b, 12b, 56b ], $
    [ 8b, 8b, 24b, 56b ], $
    [ 20b, 8b, 24b, 56b ], $
    [ 8b, 8b, 24b, 56b ], $
    [ 20b, 28b, 60b, 56b ], $
    [ 8b, 0b, 0b, 56b ], $
    [ 20b, 0b, 0b, 56b ], $
    [ 8b, 0b, 0b, 56b ], $
    [ 244b, 255b, 255b, 63b ], $
    [ 248b, 255b, 255b, 63b ], $
    [ 252b, 255b, 255b, 63b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  BUTTON_Copy
;   Copy a button.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO BUTTON_Copy, Ptr, Copy
    GenCopy, Ptr, Copy
END


;
;  BUTTON_Destroy
;   Release resources for the given button
;
PRO BUTTON_Destroy, Ptr
    GenDestroy, Ptr
END


;
;  BUTTON_Event
;   Event handling routine for a button dialog.  Shares common code
;   (c.f. widbuild.pro)
;
PRO BUTTON_Event, Event
    MISC_Event, Event, 1    ; constant is Font Offset in Foci
END


;
;  BUTTON_Build
;   Create a dialog box a button object.  If ptr is nil then
;   create the object as well.
;
PRO BUTTON_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    BUTTON_Alloc, ParPtr, Ptr               ; Allocate object if necessary
    MgrName = 'WE_BUTTON' + STRTRIM(Ptr, 2) ; Create dialog box name
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
    Foci    = LONARR(9)

    ;   Event Related Info

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
    Foci(0) = Field(Base1, "Button Text:", Obj.Value, 'VALUE', SIZE=50, /STRING)
    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(1) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")


;   Base1   = WIDGET_BASE(Base, /FRAME)
    BuildOther, Base, Obj, Foci, 2, /FRAME
;   Base2   = WIDGET_BASE(Base1, /ROW, /NONEXCLUSIVE)
;   Button  = WIDGET_BUTTON(Base2, VALUE='Generate Release Events', $
;                           UVALUE='DO_RELEASE')
;   IF Obj.ButtonRelease THEN WIDGET_CONTROL, Button, /SET_BUTTON

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Button Appearance Controls")
    BuildXY, Base1, Obj, Foci, 5, /SIZE, /OFFSET
    BuildOkCancel, Base, Obj

    DlgInfo     = { $
        Foci:       Foci, $
        ObjPtr:     Ptr $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='BUTTON_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  BUTTON_Save
;   Save button information to a file.
;   This is a simple object to save.
;
PRO BUTTON_Save, Unit, Ptr
    GenWrite, Unit, Ptr
END


;
;  BUTTON_Restore
;   Read in a button object from a file
;
PRO BUTTON_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "BUTTON", 0
END


;
;  BUTTON_Generate
;   Create a button object for previewing
;
PRO BUTTON_Generate, Base, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Build a command string

    Cmd = 'Id = WIDGET_BUTTON(Base'
    SAddCmd, Cmd, Obj.Font, 'FONT'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    SAddCmd, Cmd, Obj.Value, 'VALUE'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'

    Obj2Ptr, Obj, Ptr

    ; Create button by executing the command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Button ' + VarName(Ptr)
    ENDIF
END


;
;  BUTTON_GenWid
;   Create IDL code for creating a BUTTON
;
PRO BUTTON_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)            ; Get name for button
    Ptr2Obj, Ptr, Obj               ; Get object information

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_BUTTON( ",A)', $
        Name, Parent, /NO_EOL
    SSaveCmd, Unit, Obj.Font, "FONT"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    SSaveCmd, Unit, Obj.Value, "VALUE"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.XSize, "XSIZE"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    ISaveCmd, Unit, Obj.YSize, "YSIZE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  BUTTON_Alloc
;       Allocate a button group object.  Don't allocate if ptr is non-nil
;
PRO BUTTON_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr = WIDGET_BASE(GROUP=TopDlg)         ; Make a pointer

    ;   Make a Button object

    Obj = {                     $
        WE_BUTTON,              $
        Type:           'BUTTON',$
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; object name
        FrameSize:      0,      $
        Font:           '',     $
        XSize:          0,      $
        YSize:          0,      $
        XOffset:        0,      $
        YOffset:        0,      $
        UValue:         '',     $
        Value:          ''      $
    }

;   Buttons support the 'NO_RELEASE' option but we don't
;       ButtonRelease:  1       $ ; 1 = NO_RELEASE (default)

    Obj2Ptr, Obj, Ptr
END
