;
; $Id: widfile.pro,v 1.16 1995/01/26 03:32:11 billo Exp $
;
;  WidFile
;   Widget File class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  Currpath
;   Return the current path. If there is no current path, chose
;   a reasonable default path. (I use the current directory)
;
FUNCTION CurrPath

  COMMON WidEd_Comm

    IF FileDir NE '' THEN RETURN, FileDir       ; FileDir non nil

    CASE !Version.OS OF                         ; Use default (OS dependent)
    'vms':  RETURN,'[]'                         ; VAX VMS
    'Win32':  RETURN,'.'                      ; PC
    'MacOS':  RETURN,''                         ; Macintosh
    ELSE:   RETURN,'.'                          ; UNIX
    ENDCASE
END


;
;  ExtractSimpleName
;   Given a full file name, extract the simple filename without extention
;   or directory path.  See examples below:
;
;   foo.pro                 -> foo
;   /ugh/bar/foo.pro        -> foo
;   c:foo.wid               -> foo
;   a:\b\c\d\e\foo.pro      -> foo
;   xx:[IDL.JOSH]FOO.PRO    -> FOO
;
FUNCTION ExtractSimpleName, File, START=Start

    Idx = WHERE(!Version.OS EQ [ "vms", "Win32", "MacOS" ])
    Idx = Idx(0) + 1

                ;   Unix, VMS, WIN, MAC
    First       = ([ '/', ']', '\', ':'])(Idx)
    Second      = ([ '',  ':', ':', ''])(Idx)
    ExtCh       = ([ '.', '.', '.', '.'])(Idx)
    ExtCh2      = ([ '',  ';', '',  ''])(Idx)

    ;   Find end of path and/or disk/volume descriptor.

    Start       = RSTRPOS(File, First) + 1
    IF Start EQ 0 AND Second NE '' THEN Start = RSTRPOS(File, Second) + 1

    ;   Get filename.
    Simple      = STRMID(File,Start,1000)

    ;   Remove ending if there might be one.
    ;   Most have a '.' ending.  VMS might have a version too.

    IF ExtCh NE '' THEN BEGIN
        Ext = RSTRPOS(Simple, ExtCh)
        IF Ext EQ -1 AND ExtCh2 NE '' THEN Ext = RSTRPOS(Simple, ExtCh2)
        IF Ext NE -1 THEN Simple = STRMID(Simple,0,Ext)
    ENDIF

    RETURN, Simple      ; Return what we found.
END


;
;  InsureExt
;   Make sure a file name has the correct ending
;
PRO InsureExt, File, Ending

    Simple  = ExtractSimpleName( File, START=Start )

    File    = STRMID(File,0,Start) + Simple + Ending
END

;
;  InternalXprintf
;
;   Create a string given a list of arguments and a format string
;
;   Used to write less than a line because PRINTF, FORMAT=(..., $)
;   doesn't work on VMS for the file types we use.
;
FUNCTION InternalXPrintf, n, a1, a2, a3, a4, a5, a6, a7, FORMAT=Fmt

    COMMON  XPrintf_Comm, OutputBuffer, ArgList

    DoFormat    = KEYWORD_SET(Fmt)
    Buffer      = ""

    IF n EQ 0 THEN BEGIN
        IF DoFormat EQ 0 THEN RETURN, ""
        MESSAGE, 'STRING() required a positional argument'
    ENDIF ELSE BEGIN

        StrCmd      = "Buffer = STRING(" + ArgList( n )
        IF KEYWORD_SET(Fmt) THEN BEGIN
            StrCmd  = StrCmd + ', FORMAT=Fmt )'
        ENDIF ELSE BEGIN
            StrCmd  = StrCmd + ')'
        ENDELSE

    ENDELSE

    Dummy   = EXECUTE( StrCmd )
    RETURN, Buffer
END

;
;  XPRINTF
;   VMS can't use FORMAT=(..., $) to have multiple I/O operations
;   write the same line of text.  At least not using the file types
;   we have.  Each I/O operation is its own line
;
PRO XPRINTF, Unit, a1, a2, a3, a4, a5, a6, a7, $
                                FORMAT=Fmt, NO_EOL=NoNewline

    COMMON  XPrintf_Comm, OutputBuffer, ArgList

    n   = N_PARAMS() - 1

    IF KEYWORD_SET(NoNewline) THEN BEGIN
        OutputBuffer    = OutputBuffer + $
                InternalXprintf( n, a1, a2, a3, a4, a5, a6, a7, FORMAT=Fmt )

    ENDIF ELSE BEGIN

        PRINTF, Unit, OutputBuffer + $
                InternalXprintf( n, a1, a2, a3, a4, a5, a6, a7, FORMAT=Fmt )
        OutputBuffer    = ""

    ENDELSE

END

;
;  GenWrite
;   General purpose object save routine.
;   Most objects can be written in one of two ways:
;       1. Just the object.
;       2. The object followed by a string array
;
PRO GenWrite, Unit, Ptr, DEFAULT=Default

  COMMON WidEd_Comm

    ON_IOERROR, BadWrite

    Ptr2Obj, Ptr, Obj
    WRITEU, Unit, Obj               ; Save basic information

    IF N_ELEMENTS(Default) NE 0 THEN BEGIN
        GetValue, Obj, Names, Default   ; Use default if object has no value
        WRITEU, Unit, N_ELEMENTS(Names) ; Save n_elements of value
        WRITEU, Unit, Names             ; Save value
    ENDIF

    Obj2Ptr, Obj, Ptr
    RETURN

  BadWrite:
    Dirty   = 2

END

;
;  MISC_Restore
;   A great many of the objects are stored in a file in the same
;   way.  In fact, except for base objects EVERYTHING is stored
;   in on of 2 ways:  Just the object or an object with a <STRARR>
;   following it.  We handle both (if you tell us which it is)
;
PRO MISC_Restore, Unit, Parent, Ptr, Type, HasValue1

    ; Get an object of the right sort
    CALL_Procedure, Type+'_Alloc', Parent, Ptr
    Ptr2Obj, Ptr, Obj

    IF HasValue1 THEN BEGIN

        SaveV1  = Obj.Value1        ; Save value pointer (crushed by read)
        READU, Unit, Obj            ; Read in object

        N_Str   = 0                 ; Read in STRARR size and then strings
        READU, Unit, N_str
        Names   = STRARR(N_Str)
        READU, Unit, Names

        Obj.Value1  = SaveV1        ; Restore saved value pointer
        Obj2Ptr, Names, Obj.Value1  ; Store names in value pointer

    ENDIF ELSE BEGIN

        READU, Unit, Obj    ; Just object needs to be read

    ENDELSE

    Obj.Dialog  = 0         ; Clear old (and invalid) value
    Obj.Next    = 0         ; Clear old (and invalid) value
    Obj2Ptr, Obj, Ptr       ; Save what we've read
END


;
;  FileNew
;   Function activated by the 'New' option on the File menu.
;   Destroys current widget tree and reinitializes top level base
;   to its starting state.  All dialogs (except main and possibly
;   paste are destroyed)
;
PRO FileNew

  COMMON WidEd_Comm

    ;   Remove old preview bases

    FOR I=0,N_ELEMENTS(Bases)-1 DO BEGIN
        B   = Bases(I)
        IF B NE 0 AND WIDGET_INFO(B,/VALID_ID) THEN WIDGET_CONTROL, B, /DESTROY
        Bases(I)    = 0
    ENDFOR

    ;   Destroy any active dialog box widget copies

    FOR I=1,N_ELEMENTS(NewDialogs)-1 DO BEGIN
        Ptr = NewDialogs(I).OldPtr
        IF Ptr NE 0 THEN Destroy, Ptr
    ENDFOR

    NewDialogs  = { WE_NEWOBJ, 0L, 0L, 0L }     ; No active dialogs
    Dirty       = 0                             ; No change since 'New'

    ;   Close cut/copy/paste/edit boxes

    IF CutId NE 0 THEN BEGIN
        WIDGET_CONTROL, CutId, /DESTROY
        CutId   = 0
    ENDIF

    IF CopyId NE 0 THEN BEGIN
        WIDGET_CONTROL, CopyId, /DESTROY
        CopyId  = 0
    ENDIF

    IF PasteId NE 0 THEN BEGIN
        WIDGET_CONTROL, PasteId, /DESTROY
        PasteId = 0
    ENDIF

    IF EditId NE 0 THEN BEGIN
        WIDGET_CONTROL, EditId, /DESTROY
        EditId  = 0
    ENDIF

    LastId      = 1             ; Reset Id counter

    ;   Destroy widget tree and reinitialize main base to starting
    ;   state.  Show preview of single top base to user.
    IF N_ELEMENTS(TopPtr) THEN Destroy, TopPtr
    Generate
    UpdateEdit
END


;
;  InternalFileOpen
;       Open a file without GUI garbage in front
;
PRO InternalFileOpen, NewFile

  COMMON WidEd_Comm

    TmpLastId   = LastId        ; Save true LastId
    LastId      = 1             ; Reset LastId.

    ;   Try to open the file
    OPENR, Unit, NewFile, /GET_LUN, /XDR, ERROR = OpenError
    IF OpenError NE 0 THEN BEGIN
        ErrorDialog, TopDlg, ['Failed to open file:', NewFile ]
        RETURN
    ENDIF

    ;   Try to recover gracefully from errors encountered
    ;   reading the file

    ON_IOERROR, Bad

    ;   Does it have the right header?

    Header  = 'WidgetEditFile'
    TestHeader  = BYTE(Header)

    READU, Unit, TestHeader
    IF STRING(TestHeader) NE Header THEN BEGIN
        ErrorDialog, TopDlg, [ NewFile + ' is not', 'a valid .WID file']
        RETURN
    ENDIF

    ;   Get LastId

    READU, Unit, TmpLastId

    ;   Get Version Information (which we ignore)
    Version     = 0L
    READU, Unit, Version        ; Get Version
    READU, Unit, Version        ; 4 unused bytes


    ;   Indicate to the user this might take a while

    WIDGET_CONTROL, /HourGlass

    FileNew                     ; Destroy old widget tree
    FileName    = NewFile       ; Set new filename
    MAIN_Restore, Unit, 0L, TopPtr  ; Read in widget tree in file

    ;   Done reading.

    FREE_LUN, Unit      ; Close file.
    Dirty   = 0         ; Clear dirty flag.  Consider this starting state
    Generate            ; Show user preview of what we just read in.
    UpdateEdit
    UpdateMainDlg       ; Show it in main dlg
    LastId  = TmpLastId
    RETURN

Bad:

    ;   Close file.  Show user whatever we read.
    FREE_LUN, Unit
    ErrorDialog, TopDlg, 'File is corrupted or not a Widget Edit file'
    Generate
    UpdateEdit
    LastId  = TmpLastId
END


;
;  FileOpen
;   Get a filename from the user, read object data from that file.
;
PRO FileOpen

  COMMON WidEd_Comm

    ;   Get filename choice from user.

    NewFile = pickfile(GROUP=TopDlg, /READ, $
                PATH=CurrPath(), FILTER='*.wid', $
                /MUST_EXIST, /NOCONFIRM, $
                GET_PATH=FileDir)

    ;   If the user hit Cancel in pickfile then just quit
    IF NewFile EQ '' THEN RETURN

    ;   Certain 'special' operating systems can't handle a
    ;   separator at the end of the pathname.

    IF !Version.OS EQ 'Win32' THEN BEGIN
        FileDir = StrMid(FileDir, 0, StrLen(FileDir) - 1)
    ENDIF

    ;   Add a .WID ending regardless of current ending
    InsureExt, NewFile, ".wid"
    InternalFileOpen, NewFile
END



;
;  FileSave
;   Prompt the user for a file name. Save object data to that file
;   Next/Children pointers saved in a file are ignored. Prefixes and
;   ordering of data in the file is used to show parent/child/peer
;   relationships.
;
PRO FileSave

  COMMON WidEd_Comm

    ; Figure out a default filename
    File    = FileName
    InsureExt,File,''

    ;   Get the user's choice for a filename to save to
    NewFile = pickfile(GROUP=TopDlg, /WRITE, $
                PATH=CurrPath(), FILE=File, GET_PATH=GetPath, $
                FILTER='*.wid')

    ;   Quit if user hit 'Cancel' button
    IF NewFile EQ '' THEN RETURN

    ;   Handle DOS can't handle backslash problem

    IF !Version.OS EQ 'Win32' THEN BEGIN
        GetPath = StrMid(GetPath, 0, StrLen(GetPath) - 1)
    ENDIF

    ;   Add a .WID ending regardless of current ending
    InsureExt, NewFile, ".wid"

    ;   Open the file
    OPENW, Unit, NewFile, /XDR, /GET_LUN, ERROR=OpenError
    IF OpenError NE 0 THEN BEGIN
        ErrorDialog, TopDlg, [ 'Unable to open file', NewFile, 'for output' ]
        RETURN
    ENDIF

    ;   If there is a problem writing the file, tell
    ;   the user there was a problem.

    ON_IOERROR, MsgBad

    ;   Indicate that this can be slow
    WIDGET_CONTROL, /HOURGLASS

    ;   Write the header
    WRITEU, Unit, BYTE('WidgetEditFile')
    WRITEU, Unit, LastId
    WRITEU, Unit, 100L          ; Version Id = 1.00
    WRITEU, Unit, 0L            ; Extra Bytes in case we need them

    Ptr = TopPtr
    WHILE PTR NE 0L DO BEGIN
        WRITEU, Unit, 1
        DEP_Save, Unit, Ptr
        Ptr = NextPtr(Ptr)
    ENDWHILE
    WRITEU, Unit, 0     ; Indicate end of base list
    FREE_LUN, Unit

    ;   Subroutines will indicate I/O error by setting Dirty to 2
    IF Dirty EQ 2 THEN BEGIN
        Dirty   = 1
        GOTO, MsgBad
    ENDIF

    ;   Success. Save new file name as the new default

    FileName    = NewFile
    FileDir     = GetPath
    Dirty       = 0         ; Saved current version.  New clean state.

    RETURN

MsgBad:
    ErrorDialog, TopDlg, 'I/O error. Could not write file.'
Bad:
    Close, Unit
    OPENW, Unit, NewFile, /DELETE, ERROR=IgnoredError   ; Remove bad file
    FREE_LUN, Unit
END


;
;  SSaveCmd
;   Write a string keyword for a widget. Default is to only
;   write the keyword if the keyword is not a null string but
;   the FORCE keyword forces the keyword to always be written.
;
;   It is assumed that there is a previously existing portion
;   to the command which will require a comma and a continuation
;   character to be appended before writing the next keyword.
;
PRO SSaveCmd, Unit, Value, Keyword, FORCE=Force

    IF Value NE '' OR KEYWORD_SET(Force) THEN BEGIN
    XPRINTF, Unit, ", $"
    XPRINTF, FORMAT='("      ",A,"=''",A,"''")', /NO_EOL, $
        Unit, Keyword, Qstring(Value)
    ENDIF
END


;
;  ISaveCmd
;   Write an integer(or long integer) keyword for a widget.
;   Default is to only write the keyword if the keyword is not
;   0 but the FORCE keyword forces the keyword to always be written.
;
;   It is assumed that there is a previously existing portion
;   to the command which will require a comma and a continuation
;   character to be appended before writing the next line of code.
;
PRO ISaveCmd, Unit, Value, Keyword, FORCE=Force

    IF Value NE 0 OR KEYWORD_SET(Force) THEN BEGIN
    XPRINTF, Unit, ", $"
    XPRINTF, FORMAT='("      ",A,"=",A)', /NO_EOL, $
        Unit, Keyword, STRTRIM(Value,2)
    ENDIF
END


;
;  SaveStr
;   Given:
;       A file unit to write to.
;       A name for the variable
;       A default value if the object has no value
;       An object containing a value (*)
;
;   Create IDL code to create a variable whose value is that <STRARR>
;   or the default if none is provided.
;
; (*) actually a widget id whose UVALUE is a <STRARR>
;
PRO SaveStr, Unit, Ptr, Obj, StrName, Default

    GetValue, Obj, Names, Default
    XPRINTF, Unit, '  ', StrName, ' = [ $'

    N   = N_ELEMENTS(Names)
    ;   Every element but the last one is a string followed by ', $'
    FOR I=0,N-2 DO $
        XPRINTF, Unit, "    '", QString(Names(I)), "', $"

    ;   Last item has closing bracket for array we are declaring
    XPRINTF, Unit, "    '", QString(Names(N-1)), "' ]"
END


;
;  BeginMagic
;       Write a magic comment
;
PRO BeginMagic, Unit, Id
    PRINTF, Unit
    PRINTF, Unit, "; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
    PRINTF, Unit, "; DO NOT REMOVE THIS COMMENT: BEGIN " + Id
    PRINTF, Unit
    PRINTF, Unit
END


;
;  EndMagic
;       Write a magic comment
;
PRO EndMagic, Unit, Id
    PRINTF, Unit
    PRINTF, Unit
    PRINTF, Unit, "; DO NOT REMOVE THIS COMMENT: END " + Id
    PRINTF, Unit, "; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.
    PRINTF, Unit
END


;
;  FindMagic
;       Look for magic comments.  Return TRUE if we found them.
;
FUNCTION FindMagic, Id, Unit, OldUnit

    COMMON FMagic_Comm, NoCheck

  COMMON WidEd_Comm

    ;   Can't find old stuff if it doesn't exist or
    ;   forced overwrite.

    IF OldUnit EQ 0 OR NoCheck THEN RETURN, 0


    Point_Lun, -OldUnit, SavePos        ; Remember where we are

    StartMagic  = "; DO NOT REMOVE THIS COMMENT: BEGIN " + Id
    EndMagic    = "; DO NOT REMOVE THIS COMMENT: END " + Id
    Line        = ""

    WHILE NOT EOF(OldUnit) DO BEGIN
        READF, OldUnit, Line
        IF Line EQ StartMagic THEN BEGIN
            PRINTF, Unit
            PRINTF, Unit, "; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
            PRINTF, Unit, Line

            WHILE NOT EOF(OldUnit) DO BEGIN
                READF, OldUnit, Line
                PRINTF, Unit, Line
                IF Line EQ EndMagic THEN BEGIN
                    PRINTF, Unit, "; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.
                    PRINTF, Unit
                    RETURN, 1
                ENDIF
            ENDWHILE

            ;   Its bad if we get here

            ErrorDialog, TopDlg, $
                [ "Could not find END magic comment", $
                  "for " + Id + " section" ]
            EndMagic, Unit, Id  ; Restore magic comment

        ENDIF
    ENDWHILE

    Point_Lun, OldUnit, SavePos
    RETURN, 0
END


;
;  MakeBackup
;       Need to make a backup in a machine independent way (painful,
;       slower, more slower).  VMS does its own backup stuff so we
;       don't have to.
;
PRO MakeBackup, File, Unit

    OPENR, Unit, File, /GET_LUN, ERROR=OpenError
    IF OpenError NE 0 THEN BEGIN
        IF N_ELEMENTS(Unit) NE 0 THEN FREE_LUN, Unit
        Unit = 0
        RETURN
    ENDIF

    IF !VERSION.OS EQ 'vms' THEN RETURN

    ;   On any non VMS system we need to create a backup

    SrcUnit     = Unit
    BackFile    = File
    InsureExt, BackFile, ".bak"

    OPENW, Unit, BackFile, /GET_LUN, ERROR=OpenError
    IF OpenError NE 0 THEN BEGIN
        FREE_LUN, SrcUnit
        FREE_LUN, Unit
        Unit    = 0
        RETURN
    ENDIF

    ;   Read line by line from source file and write it to the backup

    Line    = ''
    WHILE NOT EOF(SrcUnit) DO BEGIN
        READF, SrcUnit, Line
        PRINTF, Unit, Line
    ENDWHILE

    ;   Close files

    FREE_LUN, SrcUnit
    CLOSE, Unit

    ;   Reopen backup file, this time for reading

    OPENR, Unit, BackFile, ERROR=OpenError
    IF OpenError NE 0 THEN BEGIN
        FREE_LUN, Unit
        Unit    = 0
    ENDIF
END


;  FileGenPro
;   Main entry point for writing IDL code to reproduce the current
;   widget tree.  The user may opt to include or not include the
;   standard header document
;
PRO FileGenPro, StdHdr, CheckFlag

    COMMON FMagic_Comm, NoCheck
  COMMON WidEd_Comm

    NoCheck     = CheckFlag

    ;   Get a default filename

    File    = FileName
    InsureExt,File,''

    ;   Ask the user 'which file should I write to?'

    NewFile = pickfile(GROUP=TopDlg, /WRITE, $
            PATH=CurrPath(), FILE=File, GET_PATH=GetPath, $
            FILTER='*.pro')

    ;   If the user hit 'Cancel' quit
    IF NewFile EQ '' THEN RETURN

    IF !Version.OS EQ 'Win32' THEN BEGIN
        GetPath = StrMid(GetPath, 0, StrLen(GetPath) - 1)
    ENDIF

    ;   Add a .PRO ending regardless of current ending
    InsureExt, NewFile, ".pro"

    MakeBackup, NewFile, OldUnit

    ;   Open the file

    OPENW, Unit, NewFile, /GET_LUN, ERROR=OpenError
    IF OpenError NE 0 THEN BEGIN
        ErrorDialog, TopDlg, [ 'Unable to open file', NewFile, 'for output' ]
        RETURN
    ENDIF

    ;   Try to recover from errors
    ;   I/O Error recovery is VERY poor.

    ON_IOERROR, Bad

    WIDGET_CONTROL,/HOURGLASS

    ; Print a header

    PRINTF, Unit, ';'
    PRINTF, Unit, '; Auto Save File For ', NewFile
    PRINTF, Unit, ';'

    IF  !Version.Os EQ 'sunos' OR !Version.Os EQ 'hp-ux' OR $
        !Version.Os EQ 'IRIX' OR !Version.Os EQ 'AIX' OR $
        !Version.Os EQ 'ultrix' OR !Version.Os EQ 'DG/UX' THEN BEGIN
        SPAWN, 'date', time
        PRINTF,Unit,'; ', time & PRINTF,Unit,';'
    ENDIF

    PRINTF, Unit
    PRINTF, Unit

    ; Include the standard header.  Under UNIX we could do
    ; fstat, malloc, readu, writeu -- but under VMS we are hosed so
    ; instead we do while(!eof(fd)) n=read,write(n)

    IF FindMagic("HEADER", Unit, OldUnit) EQ 0 THEN BEGIN
        BeginMagic, Unit, "HEADER"

        IF StdHdr THEN BEGIN
            HeaderFile = FilePath('template.pro', SUBDIR=['help', 'widget'])
            CLOSE,1
            OPENR,1,HeaderFile
            Line    = ''
            WHILE NOT EOF(1) DO BEGIN
                READF,1,Line
                PRINTF,Unit,Line
            ENDWHILE
            CLOSE, 1
        ENDIF

        EndMagic, Unit, "HEADER"
    ENDIF

    DoFList2, TopPtr+0, 'PDMENU_MenuEv', Unit, OldUnit
    DoFList2, TopPtr+0, 'DEP_BaseEv', Unit, OldUnit

    ; Write the widget building/entry point procedure

    Name    = ExtractSimpleName(NewFile)

    PRINTF, Unit, FORMAT='(//"PRO ",A,", GROUP=Group")', Name
    PRINTF, Unit, FORMAT='(//"  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0"/)'

    ; If we have pull down menus, we need to predeclare
    ; the CW_PDMENU_S structure.  We declare them even if
    ; we don't use them

    Cmd = "  junk   = { CW_PDMENU_S, flags:0, name:'' }"
    PRINTF, Unit, FORMAT='(A//)', Cmd

    ;   Generate code to build and realize all other dependent
    ;   top level bases

    DEP_GenWid, Unit, TopPtr
    Ptr = NextPtr(TopPtr)
    DoFList, Ptr, 'DEP_GenWid', Unit

    ;   Generate code to manage each of the dependent
    ;   top level bases. Note we just register these bases
    ;   with the XManager.

    Ptr = NextPtr(TopPtr)
    WHILE Ptr NE 0 DO BEGIN

        PRINTF, Unit

        Name    = VarId(Ptr)
        Ptr2Obj, Ptr, Obj

        IF Obj.EventProc NE '' THEN BEGIN
            PRINTF, Unit, "  XMANAGER, '",Name,"', ",Name,  $
                ", /JUST_REG, EVENT_HANDLER='", QString( Obj.EventProc ), "'"
        ENDIF ELSE BEGIN
            PRINTF, Unit, "  XMANAGER, '",Name,"', ",Name,", /JUST_REG"
        ENDELSE
        Next    = Obj.Next
        Obj2Ptr, Obj, Ptr
        Ptr     = Next
    ENDWHILE

    ;   Finally, generate code to manage the top level base

    PRINTF, Unit

    Name    = VarId(TopPtr)
    Ptr2Obj, TopPtr, Obj
    IF Obj.EventProc NE '' THEN BEGIN
        PRINTF, Unit, "  XMANAGER, '",Name,"', ",Name, $
                ", EVENT_HANDLER='", QString( Obj.EventProc ), "'"
    ENDIF ELSE BEGIN
        PRINTF, Unit, "  XMANAGER, '",Name,"', ",Name
    ENDELSE
    Obj2Ptr, Obj, TopPtr

    PRINTF, Unit, 'END'

    ;   Success

    FREE_LUN, Unit

    ;   Remember this file as the new default file

    FileName    = NewFile
    FileDir     = GetPath

    RETURN

Bad:
    ErrorDialog, TopDlg, [ 'Failed to write file:', NewFile ]
    Close, Unit
    OPENW, Unit, NewFile, /DELETE, ERROR=IgnoredError
    FREE_LUN, Unit
END


;
;  TestDraw
;   Recursivly decend through the widget hierarchy looking for draw
;   widgets.  Put a sample plot in every draw widget.
;
PRO TestDraw, Ptr
    Ptr2Obj, Ptr, Obj

    CASE TAG_NAMES(Obj, /STRUCTURE) OF

    'WE_BASE': BEGIN
        Child       = Obj.Children
        WHILE Child NE 0L DO BEGIN
            TestDraw, Child
            Child   = NextPtr(Child)
        ENDWHILE
        END
    'WE_DRAW': BEGIN
        WIDGET_CONTROL, Obj.DrawId, GET_VALUE=WinId
        WSET, WinId
        Plot,[0,1]
        END
    ELSE:
    ENDCASE
    Obj2Ptr, Obj, Ptr
END

PRO WidFile
END
