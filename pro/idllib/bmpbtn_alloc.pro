;
; $Id: bmpbtn_alloc.pro,v 1.12 1995/01/20 19:41:01 tonyh Exp $
;
;  WidBmpBtn
;   Widget Bitmap Button class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;


;
;  BMPBTN_Icon
;       Return the bitmapped button toolbar icon
;
FUNCTION BMPBTN_Icon
    RETURN, [ $
        [000B, 000B, 000B, 000B],           $
        [000B, 000B, 000B, 000B],           $
        [000B, 000B, 000B, 000B],           $
        [128B, 255B, 001B, 000B],           $
        [224B, 255B, 007B, 000B],           $
        [240B, 000B, 015B, 000B],           $
        [056B, 000B, 028B, 000B],           $
        [024B, 000B, 024B, 000B],           $
        [028B, 000B, 056B, 000B],           $
        [012B, 000B, 048B, 000B],           $
        [012B, 000B, 048B, 000B],           $
        [012B, 000B, 048B, 000B],           $
        [012B, 000B, 048B, 000B],           $
        [012B, 000B, 240B, 031B],           $
        [012B, 000B, 240B, 031B],           $
        [012B, 000B, 112B, 029B],           $
        [028B, 000B, 184B, 026B],           $
        [024B, 000B, 088B, 029B],           $
        [056B, 000B, 188B, 026B],           $
        [240B, 000B, 095B, 029B],           $
        [224B, 255B, 175B, 026B],           $
        [128B, 255B, 085B, 029B],           $
        [000B, 176B, 170B, 026B],           $
        [000B, 112B, 085B, 029B],           $
        [000B, 176B, 170B, 026B],           $
        [000B, 112B, 085B, 029B],           $
        [000B, 176B, 170B, 026B],           $
        [000B, 240B, 255B, 031B],           $
        [000B, 240B, 255B, 031B],           $
        [000B, 000B, 000B, 000B],           $
        [000B, 000B, 000B, 000B],           $
        [000B, 000B, 000B, 000B]            $
        ]
END


;
;  BMPBTN_Copy
;   Copy a button.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO BMPBTN_Copy, Ptr, Copy
    GenCopy, Ptr, Copy, /HASVALUE
END


;
;  BMPBTN_Destroy
;   Release resources for the given button
;
PRO BMPBTN_Destroy, Ptr
    GenDestroy, Ptr, /HASVALUE
END


PRO BMPBTN_DrawBits, DlgInfo, Obj

    WSET, DlgInfo.WindowId
    ERASE

    Ptr2Obj, Obj.Value1, Bits

    ;   Fill in preview

    IF DlgInfo.Preview NE 0 THEN $
    WIDGET_CONTROL, DlgInfo.Preview, SET_VALUE=Bits

    ;   Fill in Drawable based on size

    NBits   = DlgInfo.NBits
    Delta   = DlgInfo.Delta
    Last    = 389               ; NBits * Delta + 5

    Pix     = BYTARR(NBits, NBits)
    White   = !D.N_COLORS-1
    FOR I=NBits-1,0,-1 DO BEGIN
        FOR J=0,NBits - 1 DO BEGIN
            Mask    = ISHFT(1, J AND 7)
            Offset  = ISHFT(J, -3)
            IF (Bits(Offset, I) AND Mask) EQ 0 THEN Pix(J,I) = White
        ENDFOR
    ENDFOR

    TV, REBIN(Pix, 384, 384, /SAMPLE), 5, 5, /ORDER

    ;   Draw lines

    Gray    = !D.N_COLORS / 2
    FOR I=0,NBits DO BEGIN
        Offset  = I * Delta + 5
        PLOTS, [ 5, Last ], [ Offset, Offset ], /DEVICE, COLOR=Gray
        PLOTS, [ Offset, Offset ], [ 5, Last ], /DEVICE, COLOR=Gray
    ENDFOR

    Obj2Ptr, Bits, Obj.Value1
END


;
;  BMPBTN_Event
;   Event handling routine for a button dialog.
;
PRO BMPBTN_Event, Event

  COMMON WidEd_Comm

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev               ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY ; Get Dialog Info
    Ptr2Obj, Binfo.ObjPtr, Obj                            ; Get Object

    IDirty  = 1

    CASE Ev OF

    'NAME':     Obj.Name        = Event.Value
    'FRAME':    Obj.FrameSize   = Event.Value
    'UVALUE':   Obj.Uvalue      = Event.Value
    'XOFFSET':  Obj.XOffset     = Event.Value
    'YOFFSET':  Obj.YOffset     = Event.Value
    'DRAW':     BEGIN
        IDirty  = 0
        IF Event.Press NE 0 AND Binfo.Press EQ 0 THEN BEGIN

            IF Event.Press EQ 1 THEN            Binfo.Clear = -1 $
            ELSE IF Event.Press EQ 2 THEN       Binfo.Clear = 1 $
            ELSE                                Binfo.Clear = 0

            Binfo.Press     = 1;
            Binfo.CurrX     = -1;
            Binfo.CurrY     = -1;
            WSET, BInfo.WindowId
        ENDIF

        IF Event.Release NE 0 THEN BEGIN
            Binfo.Press     = 0;
            Ptr2Obj, Obj.Value1, Bits
        IF Binfo.Preview NE 0 THEN $
            WIDGET_CONTROL, Binfo.Preview, SET_VALUE=Bits
            Obj2Ptr, Bits, Obj.Value1
        ENDIF

        IF Binfo.Press AND $
           Event.X GE 5 AND Event.X LT 389 AND $
           Event.Y GE 5 AND Event.Y LT 389 THEN BEGIN

            X       = (Event.X - 5) / Binfo.Delta;
            Y       = Binfo.NBits - 1 - (Event.Y - 5) / Binfo.Delta;

            ;       Has to be a new square
            IF X NE Binfo.CurrX OR Y NE Binfo.CurrY THEN BEGIN

                Binfo.CurrX     = X
                Binfo.CurrY     = Y

                ;       convert back to device coords

                Ptr2Obj, Obj.Value1, Bits

                Mask    = ISHFT(1, X AND 7)
                Offset  = ISHFT(X, -3)

                ;       Determine what we are doing if initial event
                IF Binfo.Clear EQ -1 THEN $
                    Binfo.Clear = (Bits(Offset, Y) AND Mask) NE 0

                IF Binfo.Clear THEN BEGIN
                    Color           = 255
                    Bits(Offset,Y)  = Bits(Offset, Y) AND (NOT Mask)
                ENDIF ELSE BEGIN
                    Color           = 0
                    Bits(Offset,Y)  = Bits(Offset, Y) OR Mask
                ENDELSE

                X1      = X * Binfo.Delta + 6
                X2      = X1 + Binfo.Delta - 1
                Y1      = (Binfo.NBits - 1 - Y) * Binfo.Delta + 5
                Y2      = Y1 + Binfo.Delta - 1
                POLYFILL, /DEVICE, [X1, X2, X2, X1], [Y1, Y1, Y2, Y2], $
                    COLOR=Color
                Obj2Ptr, Bits, Obj.Value1
                IDirty  = 1
            ENDIF
        ENDIF
        END
    'BMP16':        BEGIN
        Obj.Size        = 0
        Binfo.NBits     = 16
        Binfo.Delta     = 24
        Obj2Ptr, BYTARR(2,16), Obj.Value1
        BMPBTN_DrawBits, Binfo, Obj
        ENDIF
    'BMP32':        BEGIN
        Obj.Size        = 1
        Binfo.NBits     = 32
        Binfo.Delta     = 12
        Obj2Ptr, BYTARR(4,32), Obj.Value1
        BMPBTN_DrawBits, Binfo, Obj
        ENDIF
    'BMP64':        BEGIN
        Obj.Size        = 2
        Binfo.NBits     = 64
        Binfo.Delta     = 6
        Obj2Ptr, BYTARR(8,64), Obj.Value1
        BMPBTN_DrawBits, Binfo, Obj
        ENDIF
    'INVERT':   BEGIN
        Ptr2Obj, Obj.Value1, Bits
        Obj2Ptr, (NOT Bits), Obj.Value1
        BMPBTN_DrawBits, Binfo, Obj
        END
    'DONE':     BEGIN
        Accept, Obj, Binfo.ObjPtr
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        WIDGET_CONTROL, Event.Top, /DESTROY
        RETURN
        END
    'IGNORE':   ; Ignore

    'CANCEL':   BEGIN
        Cancel, Obj, Binfo.ObjPtr
        RETURN
        END
    ELSE:           MESSAGE, 'Unprocessed event: ' + Ev
    ENDCASE

    Dirty   = Dirty OR IDirty

    SetNextFocus, Binfo, Event      ; Set next keyboard focus as necessary
    Obj2Ptr, Obj, Binfo.ObjPtr      ; Put object back into pointer
    WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  BMPBTN_Build
;   Create a dialog box a bitmap button object.  If ptr is nil then
;   create the object as well.
;
PRO BMPBTN_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    BMPBTN_Alloc, ParPtr, Ptr               ; Allocate object if necessary
    MgrName = 'WE_BMPBTN' + STRTRIM(Ptr, 2) ; Create dialog box name
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
    Foci    = LONARR(5)

    ;   Basic information Related Info

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Basic Information")
    Base2   = WIDGET_BASE(Base1, /ROW, SPACE=30 )
    Draw    = WIDGET_DRAW(Base2, RETAIN=2, /BUTTON_EVENTS, /MOTION_EVENTS, $
                              UVALUE='DRAW', XSIZE=64*6+11, YSIZE=64*6+11 )
    Base3   = WIDGET_BASE(Base2)

    ;  No preview on Mac/PC because can't resize button

;
;   Preview works fine on a Mac now. (It ought to pork on Windows too, so we'll
;   build the button on all platforms.
;

 ;   IF !Version.OS EQ 'MacOS' OR !Version.OS EQ 'Win32' THEN BEGIN
 ;   Preview = 0
 ;   ENDIF ELSE BEGIN
    Preview = WIDGET_BUTTON(Base3, VALUE=BYTARR(8,64),YOFFSET=160, $
                        UVALUE='IGNORE')
 ;   ENDELSE
    Base2   = WIDGET_BASE(Base1, /ROW)
    Base3   = WIDGET_BASE(Base2, /FRAME, /ROW)
    Label   = WIDGET_LABEL(Base3, VALUE="Bitmap Size:")
    Btns    = LONARR(3)
    Btns(0) = WIDGET_BUTTON(Base3, VALUE='16x16', UVALUE="BMP16", /NO_REL)
    Btns(1) = WIDGET_BUTTON(Base3, VALUE='32x32', UVALUE="BMP32", /NO_REL)
    Btns(2) = WIDGET_BUTTON(Base3, VALUE='64x64', UVALUE="BMP64", /NO_REL)

    Dummy   = WIDGET_LABEL(Base2, VALUE="  ")
    Dummy   = WIDGET_BUTTON(Base2, VALUE="Invert", UVALUE="INVERT" )

    ;       Fill in Drawable based on size
    NBits   = ISHFT(1, 4 + Obj.Size)

    BuildOther, Base, Obj, Foci, 0, /FRAME

    Base1   = WIDGET_BASE(Base, /FRAME, /COLUMN)
    Lab     = WIDGET_LABEL(Base1, VALUE="Button Appearance Controls")
    BuildXY, Base1, Obj, Foci, 3, /OFFSET
    BuildOkCancel, Base, Obj

    DlgInfo     = {         $
        Foci:       Foci,   $
        ObjPtr:     Ptr,    $
        Delta:      6*64/NBits, $
        NBits:      NBits,  $
        Preview:    Preview,$
        WindowId:   0,      $
        Press:      0,      $
        Clear:      0,      $
        CurrX:      -1,     $
        CurrY:      -1      $
    }
    Obj.Dialog  = Base
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='BMPBTN_Event', $
        CLEANUP='MISC_Kill'
    WIDGET_CONTROL, Btns(Obj.Size), /SET_BUTTON ; Set button size
    WIDGET_CONTROL, Draw, GET_VALUE=WindowId
    DlgInfo.WindowId        = WindowId

    BMPBTN_DrawBits, DlgInfo, Obj

    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    Obj2Ptr, Obj, Ptr
END


;
;  BMPBTN_Save
;   Save button information to a file.
;   This is a simple object to save.
;
PRO BMPBTN_Save, Unit, Ptr
  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Ptr2Obj, Obj.Value1, Value

    ON_IOERROR, BadWrite

    WRITEU, Unit, Obj
    WRITEU, Unit, Value
    Obj2Ptr, Value, Obj.Value1
    Obj2Ptr, Obj, Ptr
    RETURN

  BadWrite:
    Dirty   = 2
    Obj2Ptr, Value, Obj.Value1
    Obj2Ptr, Obj, Ptr
END


;
;  BMPBTN_Restore
;   Read in a button object from a file
;
PRO BMPBTN_Restore, Unit, Parent, Ptr

    BMPBTN_Alloc, Parent, Ptr
    Ptr2Obj, Ptr, Obj

    SaveV1  = Obj.Value1        ; Save value pointer (crushed by read)
    READU, Unit, Obj            ; Read in object

    NBits   = ISHFT(1, 4+Obj.Size)
    Value   = BYTARR(NBits/8, NBits)
    READU, Unit, Value

    Obj.Value1  = SaveV1        ; Restore saved value pointer
    Obj2Ptr, Value, Obj.Value1  ; Store names in value pointer

    Obj.Dialog  = 0             ; Clear old (and invalid) value
    Obj.Next    = 0             ; Clear old (and invalid) value
    Obj.Parent  = Parent        ; Parent changes every run
    Obj2Ptr, Obj, Ptr           ; Save what we've read
END


;
;  BMPBTN_Generate
;   Create a button object for previewing
;
PRO BMPBTN_Generate, Base, Ptr

  COMMON WidEd_Comm

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Build a command string

    Cmd = 'Id = WIDGET_BUTTON(Base'
    IAddCmd, Cmd, Obj.FrameSize, 'FRAME'
    IAddCmd, Cmd, Obj.XOffset, 'XOFFSET'
    IAddCmd, Cmd, Obj.YOffset, 'YOFFSET'
    Ptr2Obj, Obj.Value1, Bits, /COPY
    Cmd = Cmd + ',VALUE=Bits'
    Obj2Ptr, Obj, Ptr

    ; Create button by executing the command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Button ' + VarName(Ptr)
    ENDIF
END


;
;  BMPBTN_GenWid
;   Create IDL code for creating a BUTTON
;
PRO BMPBTN_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)            ; Get variable name of object
    Ptr2Obj, Ptr, Obj               ; Get Object info

    ValueName       = 'BMP' + STRTRIM(Ptr,2)    ; Create value name


    Ptr2Obj, Obj.Value1, Bits
    XPRINTF, Unit, '  ', ValueName, ' = [ $'

    Sz      = SIZE(Bits)
    FOR I=0,Sz(2)-2 DO BEGIN
        XPRINTF, Unit, FORMAT='("    [ ",100(I0,"b, "))', $
            Bits(0:Sz(1)-2,I), /NO_EOL
        XPRINTF, Unit, FORMAT='(I0, "b ], $")', Bits(Sz(1)-1,I)
    ENDFOR

    XPRINTF, Unit, FORMAT='("    [ ",100(I0,"b, "))', $
        Bits(0:Sz(1)-2,Sz(2)-1), /NO_EOL

    ; Can't do multiple lines using XPRINTF
    XPRINTF, Unit, FORMAT='(I0, "b ]  $")', Bits(Sz(1)-1, Sz(2)-1)
    PRINTF, Unit, "  ]"

    XPRINTF, FORMAT='("  ", A, " = WIDGET_BUTTON( ",A,",VALUE=",A)', $
        Unit, Name, Parent, ValueName, /NO_EOL
    ISaveCmd, Unit, Obj.FrameSize, "FRAME"
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    ISaveCmd, Unit, Obj.XOffset, "XOFFSET"
    ISaveCmd, Unit, Obj.YOffset, "YOFFSET"
    XPRINTF, Unit, ')'

    Obj2Ptr, Bits, Obj.Value1
    Obj2Ptr, Obj, Ptr
END


;
;  BMPBTN_Alloc
;       Allocate a bitmap button object. Don't allocate if ptr is non-nil
;
PRO BMPBTN_Alloc, Parent, Ptr
  COMMON WidEd_Comm

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Ptr     = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer
    ValueId = WIDGET_BASE(GROUP=TopDlg)     ; Make a pointer for value too
    Value   = BYTARR(2,16)
    Obj2Ptr, Value, ValueId                 ; Default value is all zeros

    ;   Make a Bitmap Button object

    Obj = {                     $
        WE_BMPBTN,              $
        Type:       'BMPBTN',   $
        Parent:     Parent,     $ ; Pointer to parent
        Id:         NewId(),    $ ; Permanent Id
        Dialog:     0L,         $ ; Save Dialog ID (need for Cut consistency)
        Next:       0L,         $ ; index of next child/free/top
        Name:       '',         $ ; object name
        FrameSize:  0,          $
        XOffset:    0,          $
        YOffset:    0,          $
        UValue:     '',         $
        Size:       0,          $ ; Dflt: 16x16.  Enum Size {16x16,32x32,64x64}
        Value1:     ValueId     $
    }

    Obj2Ptr, Obj, Ptr
END
