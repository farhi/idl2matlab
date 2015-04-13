; $Id: pickfile.pro,v 1.11 1995/04/13 15:03:19 mattr Exp $
;
; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       PICKFILE
;
; PURPOSE:
;       This function allows the user to interactively pick a file.  A file
;       selection tool with a graphical user interface is created.  Files
;       can be selected from the current directory or other directories.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       Result = PICKFILE()
;
; KEYWORD PARAMETERS:
;
;       FILE:   A string value for setting the initial value of the
;               selection. Useful if there is a default file
;
;       GET_PATH: Set to a named variable. Returns the path at the
;               time of selection.
;
;       GROUP:  The widget ID of the widget that calls PICKFILE.  When this
;               ID is specified, a death of the caller results in the death of
;               the PICKFILE widget application.
;
;       READ:   Set this keyword to make the title of the PICKFILE window
;               "Select File to Read".
;
;       WRITE:  Set this keyword to make the title of the PICKFILE window
;               "Select File to Write".
;
;       PATH:   The initial path to select files from.  If this keyword is
;               not set, the current directory is used.
;
;       FILTER: A string value for filtering the files in the file list.  This
;               keyword is used to reduce the number of files to choose from.
;               The user can modify the filter unless the FIX_FILTER keyword
;               is set.  Example filter values might be "*.pro" or "*.dat".
;
;       FIX_FILTER: When this keyword is set, only files that satisfy the
;               filter can be selected.  The user has no ability to modify
;               the filter and the filter is not shown.
;
;       TITLE:  A scalar string to be used for the window title.  If it is
;               not specified, the default title is "Select File"
;
;       NOCONFIRM: Return immediately upon selection of a file.  The default
;               behavior is to display the selection and then return the
;               file when the user uses the "ok" button.
;
;       MUST_EXIST: When set, only files that actually exist can be selected.
;
; OUTPUTS:
;       PICKFILE returns a string that contains the name of the file selected.
;       If no file is selected, PICKFILE returns a null string.
;
; COMMON BLOCKS:
;       PICKER: COMMON block that maintains state for the widget.
;
; SIDE EFFECTS:
;       This function initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;       This routine is known to work on Suns (OPEN LOOK), MIPS, RS/6000,
;       DEC Ultrix, HP/700, VAX/VMS and SGI machines.
;
;       Only one instance of the PICKFILE widget can be running at one time.
;
;       PICKFILE does not recognize symbolic links to other files in UNIX.
;
; PROCEDURE:
;       Create and register the widget and then exit, returning the filename
;       that was picked.
;
; EXAMPLE:
;       Create a PICKFILE widget that lets users select only files with
;       the extensions 'pro' and 'dat'.  Use the 'Select File to Read' title
;       and store the name of the selected file in the variable F.  Enter:
;
;               F = PICKFILE(/READ, FILTER = '*.pro *.dat')
;
; MODIFICATION HISTORY:
;       Written by:     Steve Richards, April, 1991
;       July, 1991      Added a FILTER keyword to allow users
;                       to select files with a given extension or
;                       extensions.
;       August, 1991    Fixed bugs caused by differences between
;                       spawned ls commands on different machines.
;       September, 1991 Made Myfindfile so only one pass was
;                       necessary to find files and directories.
;       3/92 - ACY      Corrected initialization of dirsave, change spawn
;                       command to "ls -lL" and added case for links
;                       add NOCONFIRM keyword for auto exiting on selection
;       8/92 - SMR      Rewrote pickfile as a compound widget.
;       10/92 - SMR     Fixed a bug where extremely large file namess didn't
;                       show up properly in the file list or as return
;                       values.
;       12/92 - JWG     Add better machine dependency code
;       1/93 - JWG      Added FILE, GET_PATH keywords.
;       1/93 - TAC      Added Windows Common dialog pickfile code
;       2/93 - SMR      Fixed the documentation example for multiple extensions
;       1/94 - KDB      If directory had no execute permission on Unix
;                       platforms, CD fails and causes error. Added check
;                       for this. Increased spawn speed by using /sh for unix.
;                       Added -a switch to ls so that all files can be found
;                       on unix machines.
;       2/94 - KDB	Values passed to CD cannot end in a '\' on DOS
;			platforms. Program would crash if the PATH keyword
;			was supplied a value that ended with a "\". Added
;		        a check for this.
;	3/94 - BMH	Deleted the reference here to OS_PICKFILE for the
;			Unix platforms and created an IDL routine to
;			to call the Mac and Windows specific OS_PICKFILE
;			routines.  This solved the saving and restoring on
;	 		different platforms problem.
;	4/94 - KDB      The vms call to lib$findfile in valid_dir was
;		        commented out. This caused errors when path was
;			changed by user. Uncommented. In Valid_Dir, with
;			vms the type of directory specification was not
;		        checked (directory can be a path or a filename):
;			Fixed this. In dirlist section of event handler,
;		        a "[-]" would get trimmed to "" and cause error:
;			Fixed.
;	8/94 - ACY      Change the spawn command in getdirs to send error
;			output to /dev/null.
;	12/94 - DJE	Fix the FIX_FILTER option for the MacOS.
;-
;

FUNCTION valid_dir, dir
  WIDGET_CONTROL, /HOUR
  CASE !VERSION.OS OF

  'vms': BEGIN
            CD, current = here   ; get pwd

       ; VMS directories can be files, NAME.DIR, or paths DEVICE:[NAME.NAME].
       ; If the "[]" method is used, tack on a wildcard spec (*.*), otherwise
       ; the value of dir is a filename and it can remain the same.

            if(strpos(dir,']') gt -1)then dir = dir + "*.*"
            context = 0L
            resultant = STRING(BYTARR(256)+32B)

       ; See if either Name.dir file exists in the current directory or
       ; if a path specified see if there is any files in that dir.
       ; Use vms LIB$ routines via Call External

            result = CALL_EXTERNAL("LIBRTL", "LIB$FIND_FILE", dir, resultant,$
		 context, here, 0L, 0L, 0L, VALUE = [0, 0, 0, 0, 1, 1, 1])
            toss = CALL_EXTERNAL("LIBRTL", "LIB$FIND_FILE_END", context)

            RETURN, (result EQ 65537)
         END
  'Win32': BEGIN
            RETURN,1    ; Hook into common dialogs for windows
                        ; when this really works.
         END
  ELSE:  BEGIN
      ; Can't CD to a directory unless the user has execute permission.
      ; Use the unix command test to check this. Have to use sh5 on ultrix
      ; Test sets the shell status variable and echo prints it out. This is
      ; then captured by spawn and placed in result

   if(!version.os ne 'ultrix')then $
     spawn, ['test -d "'+dir +'" -a -x "'+dir+'" ; echo $?'], result, /sh $
   else $
     spawn, ['/bin/sh5 -c "test -d '''+dir+''' -a -x '''+dir+''' ";echo $?'], $
            result, /sh

        return, (not fix(result(0)) )  ;convert result to int and NOT it.

      END
  ENDCASE
END

;------------------------------------------------------------------------------
;       procedure GETDIR
;------------------------------------------------------------------------------
; This routine finds the files or directories at the current directory level.
; It must be called with either files or directories as a keyword.
;------------------------------------------------------------------------------

function getdirs

WIDGET_CONTROL, /HOUR

IF (!VERSION.OS EQ "vms") THEN BEGIN                    ;version is VMS who's
  retval = ['[-]']
  results = findfile("*.DIR")                           ;directories have an
  IF(KEYWORD_SET(results)) THEN BEGIN                   ;extension of ".dir"
    endpath = STRPOS(results(0), "]", 0) + 1
    results = strmid(results, endpath, 100)
    dirs = WHERE(STRPOS(results, ".DIR", 0) NE -1, found)
    IF (found GT 0) THEN BEGIN
      results = results(dirs)
      retval = [retval, results]
    ENDIF
  ENDIF
ENDIF ELSE IF !VERSION.OS EQ 'Win32' THEN BEGIN
  message,"Unsupported on this platform"
ENDIF ELSE BEGIN
  retval = ['../']
  ;added -a switch to get .* dirs
  ;change to /noshell, send errors to /dev/null
  SPAWN, ["/bin/sh", "-c", "ls -laL 2> /dev/null"], /NOSHELL, results
  numfound = N_ELEMENTS(results)
  IF(KEYWORD_SET(results)) THEN BEGIN                   ;extension of ".dir"
    firsts = STRUPCASE(STRMID(results, 0, 1))
    dirs = (where(firsts EQ "D", found))
    IF (found GT 0) THEN BEGIN
      results = results(dirs)
      spaceinds = WHERE(BYTE(results(0)) EQ 32)
      spaceindex = spaceinds(N_ELEMENTS(spaceinds)-1)
      retval = [retval, STRMID(results, spaceindex + 1, 100)]

    ; get rid of "." and ".." that ls -laL picks up
      retval = retval(WHERE( (retval ne '.')and(retval ne '..')) )

    ENDIF
  ENDIF
ENDELSE
RETURN, retval
END ; function getdirs

;------------------------------------------------------------------------------

FUNCTION getfiles, filter

WIDGET_CONTROL, /HOUR

IF (!VERSION.OS EQ "vms") THEN BEGIN
  results = findfile(filter)
  IF (KEYWORD_SET(results)) THEN BEGIN
    endpath = STRPOS(results(0), "]", 0) + 1
    results = strmid(results, endpath, 100)
    dirs = WHERE(STRPOS(results, ".DIR", 0) EQ -1, found)
    IF (found GT 0) THEN BEGIN
      results = results(dirs)
      return, results
    ENDIF
  ENDIF
ENDIF ELSE IF !VERSION.OS EQ 'Win32' THEN BEGIN
  message,"Unsupported on this platform"
ENDIF ELSE BEGIN
  SPAWN, ["/bin/sh", "-c", "ls -laL " + filter + $
          " 2> /dev/null"], results, /NOSHELL     ;added -a to get all files
  IF(KEYWORD_SET(results)) THEN BEGIN
    firsts = STRUPCASE(STRMID(results, 0, 1))
    fileinds = (WHERE(((firsts EQ "F") OR (firsts EQ "-") OR $
                       (firsts EQ "l")), found))
    IF (found GT 0) THEN BEGIN
      results = results(fileinds)
      FOR i = 0, N_ELEMENTS(results) - 1 DO BEGIN
        spaceinds = WHERE(BYTE(results(i)) EQ 32)
        spaceindex = spaceinds(N_ELEMENTS(spaceinds) - 1)
        results(i) = STRMID(results(i), spaceindex + 1, 100)
      ENDFOR
      RETURN, results
    ENDIF
  ENDIF
ENDELSE
RETURN, ""
END

;------------------------------------------------------------------------------
;       procedure Pickfile_ev
;------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
;------------------------------------------------------------------------------
PRO Pickfile_ev, event

COMMON newpicker, pathtxt, filttxt, dirlist, filelist, selecttxt, $
        ok, cancel, help, here, thefile, separator

WIDGET_CONTROL, filttxt, GET_VALUE = filt
filt = filt(0)

CASE event.id OF

  cancel: BEGIN
      thefile = ""
      WIDGET_CONTROL, event.top, /DESTROY
    END

  filttxt: BEGIN
      files = getfiles(filt)
      WIDGET_CONTROL, filelist, SET_VALUE = files
      WIDGET_CONTROL, filelist, SET_UVALUE = files
    END

  dirlist: BEGIN
      WIDGET_CONTROL, dirlist, GET_UVALUE = directories
      IF (event.index GT N_ELEMENTS(directories) - 1) THEN RETURN

   ;  Check an see if the directory is valid

      if(not valid_dir(directories(event.index)) ) then return

      IF (!version.os EQ "vms") THEN BEGIN
      ; Fixed logic error. If the users selects [-], the strpos/mid
      ; combo would return a null string. Added a check for [-],index=0

        if(event.index eq 0)then   $
           found = 3		   $ ; len of [-]
        else			   $
	   found = STRPOS(directories(event.index), ".", 0)

        CD, STRMID(directories(event.index), 0, found)
        CD, CURRENT = here   ;get pwd

      ENDIF ELSE IF !version.os EQ 'Win32' THEN BEGIN
        message,"Unsupported on this platform"
      ENDIF ELSE BEGIN
        CD, directories(event.index)
        CD, CURRENT = here
        here = here + separator
      ENDELSE
      WIDGET_CONTROL, pathtxt, SET_VALUE = here
      directories = getdirs()
      files = getfiles(filt)
      WIDGET_CONTROL, filelist, SET_VALUE = files
      WIDGET_CONTROL, filelist, SET_UVALUE = files
      WIDGET_CONTROL, dirlist, SET_VALUE = directories
      WIDGET_CONTROL, dirlist, SET_UVALUE = directories
    END

  pathtxt: BEGIN
      WIDGET_CONTROL, pathtxt, GET_VALUE = newpath
      newpath = newpath(0)
      len = STRLEN(newpath) - 1
      IF STRPOS(newpath, '/', len) NE -1 THEN $
        newpath = STRMID(newpath, 0, len)
      IF (valid_dir(newpath(0))) THEN BEGIN
        here = newpath(0) + separator
        CD, here
        directories = getdirs()
        files = getfiles(filt)
        WIDGET_CONTROL, filelist, SET_VALUE = files
        WIDGET_CONTROL, filelist, SET_UVALUE = files
        WIDGET_CONTROL, dirlist, SET_VALUE = directories
        WIDGET_CONTROL, dirlist, SET_UVALUE = directories
      ENDIF ELSE $
        WIDGET_CONTROL, pathtxt, SET_VALUE = here
    END

  filelist: BEGIN
      WIDGET_CONTROL, filelist, GET_UVALUE = files
      IF (KEYWORD_SET(files)) THEN BEGIN
        thefile = here + files(event.index)
        WIDGET_CONTROL, selecttxt, SET_VALUE =  thefile
        WIDGET_CONTROl, ok, GET_UVALUE = auto_exit
        IF (auto_exit) THEN GOTO, checkfile
      ENDIF
    END

  ok: GOTO, checkfile

  selecttxt: GOTO, checkfile

  help: XDISPLAYFILE, "", $
                GROUP = event.top, $
                TITLE = "File Selection Help", $
                WIDTH = 50, $
                HEIGHT = 12, $
                TEXT = ["    This file selection widget lets you pick a ", $
                        "file.  The files are shown on the right.  You can", $
                        "select a file by clicking on it with the mouse.", $
                        "Pressing the 'OK' button will accept the choice", $
                        "and the Cancel button will not.  To move into a ", $
                        "subdirectory, click on its name in the directory", $
                        "list on the left.  The path can also be modified", $
                        "to view files from a different directory.  The ", $
                        "full file name can also be typed in directly", $
                        "in the Selection area.  The list of files can be", $
                        "modified by typing in a filter."]

ENDCASE
RETURN

checkfile:
  WIDGET_CONTROL, selecttxt, GET_VALUE = temp
  WIDGET_CONTROL, cancel, GET_UVALUE = existflag
  IF existflag THEN BEGIN
    ON_IOERROR, print_error
    OPENR, unit, temp(0), /GET_LUN
    FREE_LUN, unit
  ENDIF
  thefile = temp(0)
  WIDGET_CONTROL, event.top, /DESTROY
  RETURN

  print_error:
    WIDGET_CONTROL, selecttxt, SET_VALUE = "!!! Invalid File Name !!!"
    thefile = ""

END ;============= end of Pickfile event handling routine task ================



;------------------------------------------------------------------------------
;       procedure Pickfile
;------------------------------------------------------------------------------
;  This is the actual routine that creates the widget and registers it with the
;  Xmanager.  It also determines the operating system and sets the specific
;  file designators for that operating system.
;------------------------------------------------------------------------------
FUNCTION Pickfile, GROUP = GROUP, PATH = PATH, READ = READ, WRITE = WRITE, $
                FILTER = FILTER, TITLE = TITLE, NOCONFIRM = NOCONFIRM, $
                MUST_EXIST = MUST_EXIST, FIX_FILTER = FIX_FILTER, $
                FILE=FILE, GET_PATH=GET_PATH

COMMON newpicker, pathtxt, filttxt, dirlist, filelist, selecttxt, $
        ok, cancel, help, here, thefile, separator

IF(XRegistered("Pickfile")) THEN RETURN, 0

thefile = ""
existflag = 0

CASE !VERSION.OS OF
'vms':          separator       = ''
; WINDOWS does NOT want a \ at the end of the directory
'Win32':      separator       = ''
'MacOS': separator = ""
ELSE:           separator       = '/'
ENDCASE

CD, CURRENT = dirsave

IF (N_ELEMENTS(PATH) EQ 0) THEN BEGIN
  PATH = dirsave + separator
  here = PATH
ENDIF ELSE BEGIN

  ;; When on a Dos platform the argument to CD cannot end in a '\' unless
  ;; it is a root directory of a drive (ie C:\). Because of this, check
  ;; If we must remove the last character of PATH. -KDB 2/4/94

  IF((!version.os eq 'Win32')and  $
       (Strpos(path,'\', Strlen(PATH)-1)ne -1))THEN  BEGIN
         IF(strlen(path) gt 3)THEN  $ ; Root dirs are 3 chars long.
             path = Strmid( path, 0, Strlen(path)-1)
  ENDIF

  IF(STRPOS(PATH, separator,STRLEN(PATH)- 1) EQ -1)AND(PATH NE separator)THEN $
    PATH = PATH + separator
  CD, PATH                                              ;if the user selected
  here = PATH                                           ;a path then use it
ENDELSE

IF (KEYWORD_SET(NOCONFIRM))     THEN auto_exit = 1      ELSE auto_exit = 0
IF (KEYWORD_SET(MUST_EXIST))    THEN existflag = 1      ELSE existflag = 0
IF (KEYWORD_SET(FIX_FILTER))    THEN mapfilter = 0      ELSE mapfilter = 1

IF (N_ELEMENTS(FILE) EQ 0)      THEN FILE = ""

IF (NOT (KEYWORD_SET(TITLE))) THEN $                    ;build up the title
  TITLE = "Please Select a File"                        ;based on the keywords

IF (KEYWORD_SET(READ)) THEN TITLE = TITLE + " for Reading" $
ELSE IF (KEYWORD_SET(WRITE)) THEN TITLE = TITLE + " for Writing"

CASE !VERSION.OS OF

'Win32':      BEGIN
        ; Windows common dialog pickfile
        ; currently does NOT support NOCONFIRM or FIX_FILTER

        ; default FILTER needs to be forced to *.* if none set
        IF (KEYWORD_SET(FILTER))        THEN filt = FILTER ELSE filt = "*.*"

        IF (N_ELEMENTS(GROUP) EQ 0)     THEN GROUP=0

        thefile = OS_PICKFILE( GROUP = GROUP, FILTER = filt, TITLE = TITLE, $
                     MUST_EXIST = existflag, FILE = FILE, GET_PATH = here)
        END

'MacOS':        BEGIN
        ; Mac Standard File dialog pickfile
        ; currently does NOT support FIX_FILTER

        ; default FILTER is set to "*" if none set
        IF (KEYWORD_SET(FILTER))        THEN filt = FILTER ELSE filt = "*"

        IF (N_ELEMENTS(GROUP) EQ 0)     THEN GROUP=0

        IF (KEYWORD_SET(WRITE)) THEN wr = 1 else wr = 0

        IF (KEYWORD_SET(PATH)) THEN pth = PATH else cd, current = pth

	IF (KEYWORD_SET(FIX_FILTER))    THEN mapfilter = 1      ELSE mapfilter = 0

        thefile = OS_PICKFILE( GROUP = GROUP, FILTER = filt, TITLE = TITLE, $
                MUST_EXIST = existflag, FILE = FILE, FIX_FILTER = mapfilter, $
                                GET_PATH = here, WRITE = wr, PATH = pth)

        END

ELSE:   BEGIN
        ; Widget pickfile for the rest of IDL

        IF (KEYWORD_SET(FILTER))        THEN filt = FILTER ELSE filt = ""

        directories = getdirs()
        files = getfiles(filt)

        version = WIDGET_INFO(/VERSION)
        IF (version.style EQ 'Motif') THEN osfrm = 0 ELSE osfrm = 1

        Pickfilebase =  WIDGET_BASE(TITLE = TITLE, /COLUMN)
        widebase =      WIDGET_BASE(Pickfilebase, /ROW)
        label =         WIDGET_LABEL(widebase, VALUE = "Path:")
        pathtxt =       WIDGET_TEXT(widebase, VAL = here, /EDIT, FR = osfrm, XS = 50)
        filtbase =      WIDGET_BASE(Pickfilebase, /ROW, MAP = mapfilter)
        filtlbl =       WIDGET_LABEL(filtbase, VALUE = "Filter:")
        filttxt =       WIDGET_TEXT(filtbase, VAL = filt, /EDIT, XS = 10, FR = osfrm)
        selections =    WIDGET_BASE(Pickfilebase, /ROW, SPACE = 30)
        dirs =  WIDGET_BASE(selections, /COLUMN, /FRAME)
        lbl =   WIDGET_LABEL(dirs, VALUE = "Subdirectories          ")
        dirlist =       WIDGET_LIST(dirs, VALUE = directories, YSIZE = 8, $
                        UVALUE = directories)
        fls =   WIDGET_BASE(selections, /COLUMN, /FRAME)
        lbl =   WIDGET_LABEL(fls, VALUE = "Files                   ")
        filelist =      WIDGET_LIST(fls, VALUE = files, YSIZE = 8, $
                        UVALUE = files)
        widebase =      WIDGET_BASE(Pickfilebase, /ROW)
        label =         WIDGET_LABEL(widebase, VALUE = "Selection:")
        selecttxt =     WIDGET_TEXT(widebase, VAL = FILE, XS = 42,      $
                        FRAME = osfrm, /EDIT)
        rowbase =       WIDGET_BASE(Pickfilebase, SPACE = 20, /ROW)
        ok =            WIDGET_BUTTON(rowbase, VALUE = "     Ok     ", $
                        UVALUE = auto_exit)
        cancel =        WIDGET_BUTTON(rowbase, VALUE = "   Cancel   ", $
                        UVALUE = existflag)
        help =  WIDGET_BUTTON(rowbase, VALUE = "    Help    ")

        WIDGET_CONTROL, Pickfilebase, /REALIZE

        XManager, "Pickfile", Pickfilebase, EVENT_HANDLER = "Pickfile_ev", $
                GROUP_LEADER = GROUP, /MODAL
        END
ENDCASE

CD, dirsave
filt = ""
GET_PATH=here
RETURN, thefile

END ;====================== end of Pickfile routine ===========================

