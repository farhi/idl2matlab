;
; $Id: wided.pro,v 1.16 1995/01/26 03:08:47 billo Exp $
;
;
;  WidEd
;   Entry point for widget builder.
;   Reads in all related functions and procedures.
;
; Copyright (c) 1993, 1994, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; Version   = Beta 0.9
;
;+
; NAME:
;       WIDED
;
; PURPOSE:
;       An IDL dialog Editor.  Lets one graphically produce a widget
;       hierarchy.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       WIDED [, File ]
;
; OPTIONAL INPUTS:
;       File:   Widget builder will read this file.  File should
;               be a widget file (.WID)
;
; KEYWORD PARAMETERS:
;       None.
;
; COMMON BLOCKS:
;
;       FMagic_Comm     -- Store global variable, NoCheck.
;       TestDraw_Comm   -- Saves a seed number used in random plot generation
;       WidDirty_Comm   -- Save Yes/No/Cancel information in Dirty
;       WidEd_Comm      -- Global variables used through the Widget Builder
;                       Definition of this common block in in wided.com
;       XPrintf_Comm    -- Save I/O state to allow multiple I/O operations
;                       to write a single line.  FORMAT=(..., $) doesn't
;                       work on VMS and this needs to work *EVERYWHERE*
;
; SIDE EFFECTS:
;       Builds widget trees.  Writes IDL Code.  Probably more side
;       effects than you can shake a stick at.
;
; RESTRICTIONS:
;       You must have IDL 3.5 or later.  This is a graphical program
;       so widgets must be available.
;
; PROCEDURE:
;       Add, delete, cut, copy and paste widgets to your hearts delight.
;       Save the results if you want to.
;
; EXAMPLE:
;       Please see the "Using the IDL Widget Builder" document.
;
; MODIFICATION HISTORY:
;       Written by:     Joshua Goldstein,       Spring '93
;
;       Fall '93        Complete rewrite using widget UVALUEs instead
;                       of uniform structures to store hierarchy. Added
;                       bitmapped buttons, pull down menus, embedded
;                       comments in .PRO files to save user mods.
;                       Made adding user classes even easier (add 1 line
;                       to widdef.dat).  Still need to write class library.
;                       Added comments to wided.  Hope they help.
;
;-

;


pro wided_common
   ; The only  purpose of this routine is that it should be
   ; the first thing compiled in the widget builder. It defines
   ; WidEd_Comm COMMON block, and allows the other routines to
   ; simply ask for the block name without providing the variable names.

  COMMON WidEd_Comm,  $
    Bases,          $       ; Top level bases in preview
	BaseXY,			$		; Remember if user moves bases
    Dirty,          $       ; TRUE if object tree has been modified
    InDestroy,      $       ; TRUE during parts of destroy processing.
    FileName,       $       ; Current file name
    FileDir,        $       ; Current directory
    NewDialogs,     $       ; Active Dialog information
    TopPtr,         $       ; MAIN Object. Root of object tree
    TopDlg,         $       ; Main dialog box id. Group leader to all
    CutList,        $       ; List of last 10 objects cut
    LastId,         $       ; Last Id used so far.
    AddList,        $       ; class list
    CutId, CopyId,  $       ; Ids for the 4 dialog boxes which need to be
    PasteId, EditId, $      ; updated whenever an object is added/delete
    OnAPC,	    $	    ; PC's have no visible text border
    SmallScreen		    ; MAC's (maybe others) need scrolling bases

end




;
;   We need to predeclare some functions so that things compile properly
;

FUNCTION NextPtr, Ptr & RETURN,0L & END
FUNCTION VarName, Ptr & RETURN,'' & END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;  Init
;   Initialize global variables.  Destroy the old object tree (if it
;   exists).  Create a new object tree with only one object: the MAIN base.
;
PRO Init, COMPILE_ONLY=NoRealize

  COMMON WidEd_Comm

    COMMON  XPrintf_Comm, OutputBuffer, ArgList

    ;   Support and library routines.

    WidUtil
    WidAlloc
    WidBuild
    WidEdit
    WidFile
    WidMain
    WidDep


    ;  PC's need frames on their text fields.

    OnAPC =  (!Version.OS EQ 'Win32')


    IF N_ELEMENTS(TopPtr) EQ 0 THEN TopPtr = 0L
    TopDlg      = 0L

    ;   Destroy the old object tree.

    IF WIDGET_INFO(TopPtr, /VALID) THEN BEGIN

        ;   We can get into a state where TopPtr is valid but
        ;   does not contain anything. So we check everything

        IF WIDGET_INFO(TopPtr, /VALID_ID) THEN BEGIN    ; is TopPtr valid
            Ptr2Obj, TopPtr, Obj
            IF KEYWORD_SET(Obj) THEN BEGIN  ; Does it contain an obj?
                Obj2Ptr, Obj, TopPtr        ; Put obj back into pointer
                Destroy, TopPtr             ; Destroy it
            ENDIF
        ENDIF
    ENDIF ELSE BEGIN
        TopPtr  = 0L
    ENDELSE

    Bases       = LONARR(100)       ; #define MAXTOP 100
    BaseXY      = INTARR(2,100)     ; BaseXY[MAXTOP][2]
    Dirty       = 0                 ; no changes yet
    InDestroy   = 0                 ; not destroying anything right now
    FileName    = "unknown"         ; no current file
    FileDir     = ""                ; no current directory

    ;   declare the different structure types used

    Dummy       = { CW_PDMENU_S, flags:0, name:'' }
    Dummy       = { WE_MENUITEM, Str: '', SubMenuPtr: 0L }
    NewDialogs  = { WE_NEWOBJ, ParPtr:0L, ObjPtr:0L, OldPtr:0L }
    AddList     = { WE_ADDLIST, Menu:'', Class: '' }

    CutId       = 0     ; No Cut Dialog
    CopyId      = 0     ; No Copy Dialog
    EditId      = 0     ; No Edit Child dialog
    PasteId     = 0     ; No Paste Dialog

    LastId      = 1 ; No widgets created yet.

    ;   Assume that there are valid items on the cutlist if it exists
    ;   otherwise make it the nil list.
    IF N_ELEMENTS(CutList) EQ 0 THEN CutList = 0L $
    ELSE IF WIDGET_INFO(CutList, /VALID) EQ 0 THEN CutList = 0L

    ;   Open the definition file
    GET_LUN, Unit
    DefFile     = "widdef.dat"
    OPENR, Unit, DefFile, ERROR = OpenError
    IF OpenError NE 0 THEN BEGIN
        DefFile = FilePath("widdef.dat", SUBDIR="lib")
        OPENR, Unit, DefFile
    ENDIF

    Line    = ''
    Quote   = (BYTE('"'))(0)
    LineNo  = 1

    WHILE NOT EOF(Unit) DO BEGIN
        READF, Unit, Line
        ;   Ignore blank lines and comments
        IF Line NE '' AND STRMID(Line,0,1) NE '#' THEN BEGIN
            ;   Complain about lines which we can't parse
            MenuExt = WHERE(BYTE(Line) EQ Quote, Count)
            IF Count NE 2 THEN BEGIN
                Print, "Error in definition file on line", LineNo
                GOTO, Error
            ENDIF
            Menu    = STRMID(Line, MenuExt(0)+1, MenuExt(1)-MenuExt(0)-1)
            Class   = STRTRIM(STRMID(Line, MenuExt(1)+1, 1000), 2)
            AddList = [ AddList, { WE_ADDLIST, Menu, Class } ]
        ENDIF
    Error:
        LineNo  = LineNo + 1
    ENDWHILE

    FREE_LUN, Unit

    ;   Crash and burn if addlist is empty
    IF N_ELEMENTS(AddList) EQ 1 THEN BEGIN
        MESSAGE, "No classes defined!. Cannot continue."
    ENDIF
    Addlist     = AddList(1:*)

    ;   Load class libraries

    FOR I=0,N_ELEMENTS(AddList)-1 DO BEGIN
        ClearVar, Child
        CALL_PROCEDURE, AddList(I).Class + "_Alloc", 0L, Child
        CALL_PROCEDURE, AddList(I).Class + "_Destroy", Child
    ENDFOR

    IF NOT KEYWORD_SET(NoRealize) THEN BEGIN
        ;   Create primary dialog.
        MAIN_Build, TopPtr, TopDlg
        WIDGET_CONTROL, TopPtr, GROUP=TopDlg
    ENDIF

    LastId          = 2 ; Reset


    ;   Check for small screen size.
    ;   We consider any screen less than 768 pixels tall to be a
    ;   small screen
;
;   Got rid of this hack since the Mac now makes large bases scroll
;   automatically. Besides, this code didn't even work right on the Mac.
;
    DEVICE, GET_SCREEN=ScreenSize
;    IF ScreenSize(1) LT 768 THEN BEGIN
;        SmallScreen     = FIX(ScreenSize * 0.75)
;        SmallScreen(0)  = SmallScreen(0) < 500  ; Hack
;    ENDIF ELSE BEGIN
        SmallScreen = [0,0]
;    ENDELSE

    ;   Set XPRINTF buffer to nil
    ;   Setup Xprintf ArgList (c.f. InternalXprintf in widfile.pro)

    OutputBuffer    = ""
    ArgList         = STRARR(8)
    ArgList(0)      = ""
    ArgList(1)      = "a1"
    ArgList(2)      = "a1,a2"
    ArgList(3)      = "a1,a2,a3"
    ArgList(4)      = "a1,a2,a3,a4"
    ArgList(5)      = "a1,a2,a3,a4,a5"
    ArgList(6)      = "a1,a2,a3,a4,a5,a6"
    ArgList(7)      = "a1,a2,a3,a4,a5,a6,a7"

END


;
;  Gen_Event
;   Generic event handler.  Absorbs events the user has created by
;   playing with the
;
PRO Gen_event, Event

;   Preview event

END

;
;  GenBase_Event
;   A event routine to absorb events handle TLB resize events of
;   preview of the user's GUI. Set XY size/offset/scrollsize in dialogs
;   to reflect change
;
;PRO GenBase_event, Event
;
;   WIDGET_CONTROL, Event.Top, GET_UVALUE=Ptr
;   Ptr2Obj, Ptr, Obj
;
;   WIDGET_CONTROL, Event.Id, TLB_GET_OFFSET=Off, TLB_GET_SIZE=Sz
;   Obj.XOffset = Off(0)
;   Obj.YOffset = Off(1)
;
;   IF Obj.XScrollSize NE 0 OR Obj.YScrollSize NE 0 THEN BEGIN
;       Obj.XScrollSize = Sz(0)
;       Obj.YScrollSize = Sz(1)
;   ENDIF ELSE BEGIN
;       Obj.XSize   = Sz(0)
;       Obj.YSize   = Sz(1)
;   ENDELSE
;
;   IF Obj.Dialog NE 0L THEN BEGIN
;       FociOff = 5 + (Obj.Type EQ "DEP")
;       WIDGET_CONTROL, Obj.Dialog, GET_UVALUE=Binfo, /NO_COPY
;
;       WIDGET_CONTROL, Binfo.Foci(FociOff), SET_VALUE=Obj.XScrollSize
;       WIDGET_CONTROL, Binfo.Foci(FociOff+1), SET_VALUE=Obj.YScrollSize
;
;       WIDGET_CONTROL, Obj.Dialog, SET_UVALUE=Binfo, /NO_COPY
;   ENDIF
;   IF Obj.AttrDlg NE 0L THEN BEGIN
;       FociOff = 5
;       WIDGET_CONTROL, Obj.AttrDlg, GET_UVALUE=Binfo, /NO_COPY
;
;       WIDGET_CONTROL, Binfo.Foci(FociOff), SET_VALUE=Obj.XSize
;       WIDGET_CONTROL, Binfo.Foci(FociOff+1), SET_VALUE=Obj.XOffset
;       WIDGET_CONTROL, Binfo.Foci(FociOff+2), SET_VALUE=Obj.YSize
;       WIDGET_CONTROL, Binfo.Foci(FociOff+3), SET_VALUE=Obj.YOffset
;
;       WIDGET_CONTROL, Obj.AttrDlg, SET_UVALUE=Binfo, /NO_COPY
;   ENDIF
;   Obj2Ptr, Obj, PTr
;END


;
;  Generate
;   Recreate preview to reflect current object tree information
;
PRO Generate

  COMMON WidEd_Comm

    ;   Remove old preview bases

    FOR I=0,N_ELEMENTS(Bases)-1 DO BEGIN
        B   = Bases(I)
        IF B NE 0 AND WIDGET_INFO(B,/VALID_ID) THEN BEGIN
            WIDGET_CONTROL, B, TLB_GET_OFFSET=Off
            BaseXY(*,I) = Off
            WIDGET_CONTROL, B, /DESTROY
        ENDIF ELSE BEGIN
            BaseXY(*,I) = 0
        ENDELSE
        Bases(I)    = 0
    ENDFOR

    ;   Generate new top level bases and save their Id's
    Ptr = TopPtr
    I   = 0
    WHILE Ptr NE 0 DO BEGIN
        DEP_Generate, Ptr, NewBase, I
        Bases(I)    = NewBase
        I           = I + 1
        Ptr         = NextPtr(Ptr)
    ENDWHILE
END


;
;  WidEd
;   Run the widget builder
;
PRO WidEd, File

  COMMON WidEd_Comm

    ; Can only have one widget builder running at a time
    IF XRegistered("Widget_Builder") THEN RETURN

    Init            ; Set up globals.  Build top dialog box
    Generate        ; Show preview of emptywidget tree.
    XManager, 'Widget_Builder', TopDlg, $
        EVENT_HANDLER="MAIN_BarEvent", /JUST_REG

    ;  If user has a file in mind, open it.

    IF N_ELEMENTS(File) NE 0 THEN BEGIN
        InsureExt, File, '.wid'
        InternalFileOpen, File
    ENDIF
    XManager
END
