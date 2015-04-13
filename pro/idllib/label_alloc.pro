;
; $Id: label_alloc.pro,v 1.8 1995/01/20 19:41:01 tonyh Exp $
;
;  WidLabel
;   Widget Label class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  LABEL_Icon
;       Return the label toolbar icon
;
FUNCTION LABEL_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 128b, 1b, 0b ], $
    [ 0b, 128b, 1b, 0b ], $
    [ 0b, 128b, 1b, 0b ], $
    [ 0b, 64b, 3b, 0b ], $
    [ 0b, 64b, 3b, 0b ], $
    [ 0b, 64b, 3b, 0b ], $
    [ 0b, 32b, 6b, 0b ], $
    [ 0b, 32b, 6b, 0b ], $
    [ 0b, 32b, 6b, 0b ], $
    [ 0b, 16b, 12b, 0b ], $
    [ 0b, 240b, 15b, 0b ], $
    [ 0b, 16b, 12b, 0b ], $
    [ 0b, 8b, 24b, 0b ], $
    [ 0b, 8b, 24b, 0b ], $
    [ 0b, 8b, 24b, 0b ], $
    [ 0b, 28b, 60b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  LABEL_Copy
;   Copy a label.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO LABEL_Copy, Ptr, Copy
    GenCopy, Ptr, Copy
END


;
;  LABEL_Destroy
;   Release resources for the given label
;
PRO LABEL_Destroy, Ptr
    GenDestroy, Ptr
END


;
;  LABEL_Event
;   Event handling routine for a label dialog.  Shares common code
;   (c.f. widbuild.pro)
;
PRO LABEL_Event, Event
    MISC_Event, Event, 1    ; constant is Font Offset in Foci
END



;
;  LABEL_Build
;   Create a dialog box a label object.  If ptr is nil then
;   create the object as well.
;
PRO LABEL_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    LABEL_Alloc, ParPtr, Ptr                ; Allocate object if necessary
    MgrName = 'WE_LABEL' + STRTRIM(Ptr, 2)  ; Create dialog box name
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

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
    Foci(0) = Field(Base1, "Label Text:", Obj.Value, 'VALUE', SIZE=50, /STRING)
    Base2   = WIDGET_BASE(Base1,/ROW)
    Foci(1) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base2, VALUE="XFont", UVALUE="XFONT")


    BuildOther, Base, Obj, Foci, 2, /FRAME

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Label Appearance Controls")
    BuildXY, Base1, Obj, Foci, 5, /SIZE, /OFFSET
    BuildOkCancel, Base, Obj

    DlgInfo     = { $
        Foci:       Foci, $
        ObjPtr:     Ptr $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='LABEL_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  LABEL_Save
;   Save label information to a file.
;   This is a simple object to save.
;
PRO LABEL_Save, Unit, Ptr
    GenWrite, Unit, Ptr
END


;
;  LABEL_Restore
;   Read in a label object from a file
;
PRO LABEL_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "LABEL", 0
END


;
;  LABEL_Generate
;   Create a label object for previewing
;
PRO LABEL_Generate, Base, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Build a command string

    Cmd = 'Id = WIDGET_LABEL(Base'
    SAddCmd, Cmd, Obj.Font, 'FONT'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    SAddCmd, Cmd, Obj.Value, 'VALUE'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'

    Obj2Ptr, Obj, Ptr

    ; Create label by executing the command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Label ' + VarName(Ptr)
    ENDIF
END


;
;  LABEL_GenWid
;   Create IDL code for creating a LABEL
;
PRO LABEL_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)            ; Get name for label
    Ptr2Obj, Ptr, Obj               ; Get object information

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_LABEL( ",A)', $
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
;  LABEL_Alloc
;       Allocate a label object.  Don't allocate if ptr is non-nil
;
PRO LABEL_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr = WIDGET_BASE(GROUP=TopDlg)         ; Make a pointer

    Obj = {                     $
        WE_LABEL,               $
        Type:           'LABEL',$
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
    Obj2Ptr, Obj, Ptr
END
