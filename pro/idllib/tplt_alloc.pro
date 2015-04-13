;
; $Id: tplt_alloc.pro,v 1.6 1995/01/20 19:41:01 tonyh Exp $
;
;  WidTemplate
;   Sample Class definition file
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  TPLT_Icon
;
FUNCTION TPLT_Icon
  ;  One should either return 0 (indicates no toolbar icon)
  ;  or a BYTARR(4,32) which will be added to the toolbar.

  RETURN, 0
END

;
;  TPLT_Copy
;   Copy a tmplt.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO TPLT_Copy, Ptr, Copy
    GenCopy, Ptr, Copy
END


;
;  TPLT_Destroy
;   Release resources for the given tmplt
;
PRO TPLT_Destroy, Ptr
    GenDestroy, Ptr
END


;
;  TPLT_Event
;   Event handling routine for a tmplt dialog.   Shares common code
;   (c.f. widbuild.pro)
;
PRO TPLT_Event, Event
    MISC_Event, Event, 1    ; constant is Font Offset in Foci (see TPLT_Build)
END


;
;  TPLT_Build
;   Create a dialog box a tmplt object.  If ptr is nil then
;   create the object as well.
;
PRO TPLT_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    TPLT_Alloc, ParPtr, Ptr               ; Allocate object if necessary
    MgrName = 'WE_TPLT' + STRTRIM(Ptr, 2) ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN    ; See if it already exists

    Title   = GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'
    Ptr2Obj, Ptr, Obj

    ;   Create dialog box

    Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)

;****************OBJECT SPECIFIC CODE TO BUILD DIALOG BOX**************
;   Foci    = LONARR(6)
;   Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
;   Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
;   Foci(0) = Field(Base1, "Tmplt Text:", Obj.Value, 'VALUE', SIZE=50, /STRING)
;    Base2   = WIDGET_BASE(Base1,/ROW)

;***************See TPLT_Event for use of the Font Index***************
;    Foci(1) = Field(Base2, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
;    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
;       XFontBtn    = WIDGET_TPLT(Base2, VALUE="XFont", UVALUE="XFONT")
;
;   Base1   = WIDGET_BASE(Base, /FRAME)
;   BuildOther, Base, Obj, Foci, 2, /FRAME
;   Lab     = WIDGET_LABEL(Base1, VALUE="Tmplt Appearance Controls")
;   BuildXY, Base1, Obj, Foci, 3, /SIZE, /OFFSET
;   BuildOkCancel, Base, Obj

;****************DECLARE DIALOG INFORMATION**********************
    DlgInfo     = { $
        Foci:       Foci, $     ; Required
        ObjPtr:     Ptr, $      ; Required
        SpecialData: 0b $       ; Data for Dialog Box (widget ids, etc.)
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='TPLT_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  TPLT_Save
;   Save tmplt information to a file.
;   This is a simple object to save.
;
PRO TPLT_Save, Unit, Ptr
    GenWrite, Unit, Ptr
END


;
;  TPLT_Restore
;   Read in a tmplt object from a file
;
PRO TPLT_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "TPLT", 0
END


;
;  TPLT_Generate
;   Create a tmplt object for previewing
;
PRO TPLT_Generate, Base, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Build a command string

    Cmd = 'Id = WIDGET_TPLT(Base'
    SAddCmd, Cmd, Obj.TemplateStringData, 'STRING_KEYWORD'
    IAddCmd, Cmd, Obj.TemplateIntData, 'ANOTHER_KEYWORD'
    SAddCmd, Cmd, Obj.TemplateFont, 'FONT'

    ; Create tmplt by executing the command string we just built

    Obj2Ptr, Obj, Ptr

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Tmplt ' + VarName(Ptr)
    ENDIF
END


;
;  TPLT_GenWid
;   Create IDL code for creating a TPLT
;
PRO TPLT_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)            ; Get name for tmplt
    Ptr2Obj, Ptr, Obj               ; Get object information

    XPRINTF, Unit, FORMAT='("  ",A," = CW_TPLT( ",A )', /NO_EOL, $
        Name, Parent
    SSaveCmd, Unit, Obj.TemplateFont, "FONT"
    SSaveCmd, Unit, Obj.TemplateStringData, "STRING_KEYWORD"
    ISaveCmd, Unit, Obj.TemplateIntData, "ANOTHER_KEYWORD"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END

;  Alloc function must be last function in file

;
;  TPLT_Alloc
;       Allocate a <THING> object.
;
PRO TPLT_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr = WIDGET_BASE(GROUP=TopDlg)         ; Make a pointer

    ;   Make a Tmplt object

    Obj = {                     $
        WE_TPLT,               $ ; Nice but not required
        Type:           'TPLT',$ ; Set to function prefix
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Ptr to Control Panel (Base widget ID)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; object name
        UValue:         '',     $ ; Required.
        TemplateStringData: '', $ ; Per class information <Sample>
        TemplateIntData: 0,     $ ; Per class information <Sample>
        TemplateFont:   ''      $ ; Per class information <Font Sample>
    }

    Obj2Ptr, Obj, Ptr
END
