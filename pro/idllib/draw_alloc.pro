;
; $Id: draw_alloc.pro,v 1.7 1994/06/01 23:08:48 ali Exp $
;
;  WidDraw
;   Widget Draw class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  DRAW_Icon
;       Return the draw toolbar icon
;
FUNCTION DRAW_Icon
  RETURN, [ $
    [ 255b, 255b, 255b, 255b ], $
    [ 31b, 0b, 0b, 128b ], $
    [ 31b, 0b, 0b, 128b ], $
    [ 159b, 219b, 185b, 157b ], $
    [ 153b, 255b, 249b, 159b ], $
    [ 153b, 255b, 255b, 159b ], $
    [ 25b, 255b, 255b, 143b ], $
    [ 153b, 255b, 255b, 159b ], $
    [ 153b, 255b, 255b, 159b ], $
    [ 31b, 255b, 255b, 143b ], $
    [ 159b, 255b, 255b, 135b ], $
    [ 153b, 255b, 255b, 147b ], $
    [ 153b, 127b, 255b, 153b ], $
    [ 25b, 62b, 254b, 132b ], $
    [ 25b, 158b, 124b, 134b ], $
    [ 153b, 207b, 57b, 159b ], $
    [ 159b, 231b, 147b, 159b ], $
    [ 159b, 243b, 199b, 159b ], $
    [ 153b, 249b, 239b, 143b ], $
    [ 153b, 252b, 255b, 159b ], $
    [ 25b, 254b, 255b, 159b ], $
    [ 25b, 255b, 255b, 143b ], $
    [ 153b, 255b, 255b, 159b ], $
    [ 159b, 255b, 249b, 159b ], $
    [ 159b, 219b, 185b, 157b ], $
    [ 31b, 0b, 0b, 128b ], $
    [ 31b, 0b, 0b, 128b ], $
    [ 255b, 255b, 255b, 255b ], $
    [ 255b, 255b, 255b, 255b ], $
    [ 255b, 193b, 96b, 240b ], $
    [ 255b, 193b, 96b, 240b ], $
    [ 255b, 255b, 255b, 255b ]  $
  ]
END


;
;  DRAW_Copy
;   Copy a draw.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO DRAW_Copy, Ptr, Copy
    GenCopy, Ptr, Copy
END


;
;  DRAW_Destroy
;   Release resources for the given draw
;
PRO DRAW_Destroy, Ptr
    GenDestroy, Ptr
END


;
;  DRAW_Event
;   Event handling routine for a draw dialog.  Shares common code
;   (c.f. widbuild.pro)
;
PRO DRAW_Event, Event
    MISC_Event, Event, -1   ; constant is Font Offset in Foci
END


;
;  DRAW_Build
;   Create a dialog box a draw object.  If ptr is nil then
;   create the object as well.
;
PRO DRAW_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    DRAW_Alloc, ParPtr, Ptr                ; Allocate object if necessary
    MgrName = 'WE_DRAW' + STRTRIM(Ptr, 2)  ; Create dialog box name
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
    BuildXY, Base1, Obj, Foci, 0, /SIZE

    ;   Event Related Info

    Base2   = WIDGET_BASE(Base1,/NONEXCLUSIVE, /ROW)
    Button  = WIDGET_BUTTON(Base2, VALUE='Accept Button Events', $
                    UVALUE='DO_BUTTON')
    IF Obj.DrawBtnEv THEN WIDGET_CONTROL, Button, /SET_BUTTON
    Button  = WIDGET_BUTTON(Base2, VALUE='Accept Motion Events', $
                    UVALUE='DO_MOTION')
    IF Obj.DrawMoEv THEN WIDGET_CONTROL, Button, /SET_BUTTON

    Base2   = WIDGET_BASE(Base1, /FRAME, /ROW)
    Lab     = WIDGET_LABEL(Base2, VALUE="Backing Store:")
    Base3   = WIDGET_BASE(Base2, /ROW, /EXCLUSIVE)
    Btns    = LONARR(3)
    Btns(0) = WIDGET_BUTTON(Base3, VALUE='None', UVALUE='NO_STORE', /NO_REL)
    Btns(1) = WIDGET_BUTTON(Base3, VALUE='Server', UVALUE='SVR_STORE', /NO_REL)
    Btns(2) = WIDGET_BUTTON(Base3, VALUE='IDL', UVALUE='IDL_STORE', /NO_REL)
    WIDGET_CONTROL, Btns(Obj.DrawRetain), /SET_BUTTON

    BuildOther, Base, Obj, Foci, 2, /FRAME

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Draw Appearance Controls")
    BuildXY, Base1, Obj, Foci, 5, /OFFSET, /SCROLL
    BuildOkCancel, Base, Obj

    DlgInfo     = { $
        Foci:       Foci, $
        ObjPtr:     Ptr $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='DRAW_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  DRAW_Save
;   Save draw information to a file.
;   This is a simple object to save.
;
PRO DRAW_Save, Unit, Ptr
    GenWrite, Unit, Ptr
END


;
;  DRAW_Restore
;   Read in a draw object from a file
;
PRO DRAW_Restore, Unit, Parent, Ptr
    MISC_Restore, Unit, Parent, Ptr, "DRAW", 0
END



;
;  DRAW_Generate
;   Create a draw object for previewing
;
PRO DRAW_Generate, Base, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Build a command string

    Cmd = 'Id = WIDGET_DRAW(Base'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IAddCmd, Cmd, Obj.DrawRetain, 'RETAIN'
    IAddCmd, Cmd, Obj.XSize, 'XSIZE'
    IAddCmd, Cmd, Obj.YSize, 'YSIZE'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'
    IAddCmd, Cmd, Obj.XScrollSize, 'X_SCROLL_SIZE'
    IAddCmd, Cmd, Obj.YScrollSize, 'Y_SCROLL_SIZE'

    ; Create draw by executing the command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        Obj2Ptr, Obj, Ptr
        MESSAGE,'Could not create Draw ' + VarName(Ptr)
    ENDIF

    Obj.DrawId  = Id    ; Save widget id for TestDraw

    Obj2Ptr, Obj, Ptr
END


;
;  DRAW_GenWid
;   Create IDL code for creating a DRAW
;
PRO DRAW_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)            ; Get name for draw
    Ptr2Obj, Ptr, Obj               ; Get object information

    XPRINTF, Unit, FORMAT='("  ",A," = WIDGET_DRAW( ",A)', $
        Name, Parent, /NO_EOL
    ISaveCmd, Unit, Obj.DrawBtnEv, "BUTTON_EVENTS"
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    ISaveCmd, Unit, Obj.DrawMoEv, "MOTION_EVENTS"
    ISaveCmd, Unit, Obj.DrawRetain, "RETAIN", /FORCE
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    SSaveCmd, Unit, Obj.Value, "VALUE"
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
;  DRAW_Alloc
;       Allocate a draw object.  Don't allocate if ptr is non-nil
;
PRO DRAW_Alloc, Parent, Ptr
  COMMON WidEd_Comm    

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return

    Ptr = WIDGET_BASE(GROUP=TopDlg)         ; Allocate a pointer

    ;   Create a draw object

    Obj = {                     $
        WE_DRAW,                $
        Type:           'DRAW', $
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; Title or object name
        FrameSize:      0,      $
        XSize:          0,      $
        YSize:          0,      $
        XOffset:        0,      $
        YOffset:        0,      $
        XScrollSize:    0,      $
        YScrollSize:    0,      $
        UValue:         '',     $
        Value:          '',     $
        DrawId:         0L,     $
        DrawBtnEv:      0,      $
        DrawMoEv:       0,      $
        DrawRetain:     0       $
    }
    Obj2Ptr, Obj, Ptr           ; Store object in pointer
END
