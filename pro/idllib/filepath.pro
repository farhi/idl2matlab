; $Id: filepath.pro,v 1.9 1995/03/09 17:34:56 davee Exp $

FUNCTION FILEPATH, FILENAME, ROOT_DIR=root_dir, SUBDIRECTORY=subdir, $
	TERMINAL = TERMINAL, TMP = TMP
;+
; NAME:
;	FILEPATH
;
; PURPOSE:
;	Given the name of a file in the IDL distribution,
;	FILEPATH returns the fully-qualified path to use in
;	opening the file. Operating system dependencies
;	are taken into consideration. This routine is used by RSI to
;	make the User Library portable.
;
; CATEGORY:
;	File Management.
;
; CALLING SEQUENCE:
;	Result = FILEPATH('filename' [, SUBDIRECTORY = subdir])
;
; INPUTS:
;    filename:	The lowercase name of the file to be opened. No device
;		or directory information should be included.
;
; KEYWORDS:
;    ROOT_DIR: The name of the directory from which the resulting path
;	should be based. If not present, the value of !DIR is used.
;	This keyword is ignored if TERMINAL or TMP are specified.
;
;    SUBDIRECTORY:	The name of the subdirectory in which the file
;		should be found. If this keyword is omitted, the main
;		directory is used.  This variable can be either a scalar
;		string or a string array with the name of each level of
;		subdirectory depth represented as an element of the array.
;
;		For example, to get a path to the file DETERM in the "userlib"
;		subdirectory to the IDL "lib" subdirectory, enter:
;
;		path = FILEPATH("determ", SUBDIRECTORY = ["lib", "userlib"])
;
;    TERMINAL:	Return the filename of the user's terminal.
;
;	  TMP:	The file is a scratch file.  Return a path to the
;		proper place for temporary files under the current operating
;		system.
;
; OUTPUTS:
;	The fully-qualified file path is returned.  If one of the subdirectory
;	keywords is not specified, the file is assumed to exist in the
;	main distribution directory.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	Don't specify more than one of the keywords in a single call.
;
;
; EXAMPLE:
;	To get a path to the file DETERM in the "userlib" subdirectory to the
;	IDL "lib" subdirectory, enter:
;
;		path = FILEPATH("determ", SUBDIRECTORY = ["lib", "userlib"])
;
;	The variable "path" contains a string that is the fully-qualified file
;	path for the file DETERM.
;
; MODIFICATION HISTORY:
;	December, 1989, AB, RSI (Formalized from original by DMS)
;	October, 1990, SG, RSI (added support for MSDOS)
;	February, 1991, SMR, RSI (added string array support for multi-level
;				  directories)
;	21 April 1993, AB, Added ROOT_DIR keyword.
;       14 July  1994, KDB, RSI - Corrected logic error in VMS section
;                                 of the ROOT_DIR keyword. Any
;                                 sub-directory specification was
;                                 being ignored when using ROOT_DIR. Fixed.
;	March, 1995, DJE, Add a ':' if root_dir is specified on the Mac.
;-


ON_ERROR,2		                        ;Return to caller if an error
						;occurs

do_tmp = KEYWORD_SET(TMP)			;get temporary path if existing
path = ''

IF (KEYWORD_SET(TERMINAL)) THEN BEGIN
  CASE !VERSION.OS OF
    'vms': RETURN,'SYS$OUTPUT:'
    'Win32': RETURN,'con'
    'MacOS' : BEGIN
        PRINT, "No terminal device for the Mac"
        RETURN, ''
      END
    ELSE: RETURN,'/dev/tty'
  ENDCASE
ENDIF

IF (do_tmp) THEN BEGIN
  CASE !VERSION.OS OF
    'vms': root_dir = 'SYS$LOGIN:'
    'Win32': root_dir = '\tmp'
    'MacOS': begin
	root_dir = !DIR
	if (n_params() EQ 0) then filename = "IDL Temp File"
      end
    ELSE: root_dir = '/tmp'
  ENDCASE
ENDIF ELSE BEGIN
  IF (not KEYWORD_SET(ROOT_DIR)) THEN root_dir = !DIR
  IF (KEYWORD_SET(SUBDIR)) THEN BEGIN		;if the SUBDIR keyword is set
    SUBDIR = STRLOWCASE(SUBDIR)			;then include each level in the
    CASE !VERSION.OS OF				;path separated by the correct
      'vms': divider = "."			;directory level delimiter for
      'Win32': divider = "\"			;the current O.S.
      'MacOS' : divider = ":"
      ELSE: divider = "/"
    ENDCASE
    FOR i = 0, N_ELEMENTS(SUBDIR) - 1 DO BEGIN
	path = path + SUBDIR(i)
	IF(i NE N_ELEMENTS(SUBDIR) - 1) THEN $
	  path = path + divider
    ENDFOR
    if !VERSION.OS EQ 'MacOS' THEN path = path + divider
  ENDIF
ENDELSE


CASE !VERSION.OS OF
  'vms': BEGIN
      IF (NOT do_tmp) THEN BEGIN
        IF (path EQ '') THEN path = '000000'
        path = '[' + path + ']'

      ; check for a ".]" at the end of our root directory

        IF(( strmid(root_dir, strlen(root_dir)-2, 2) ne ".]") and    $
           ( strmid(root_dir, strlen(root_dir)-1, 1) eq "]") )then   $
           root_dir = strmid(root_dir,0,strlen(root_dir)-1) +'.]'
      ENDIF
    END
  'Win32': BEGIN
      path = '\' + path
      IF (path NE '\') THEN path = path + '\'
    END
  'MacOS': BEGIN
      ; make sure the root dir ends with a separator
      IF (STRMID(root_dir, STRLEN(root_dir) - 1, 1) NE ':') THEN $
	root_dir = root_dir + ':'
    END
  ELSE: BEGIN
      path = '/' + path
      IF (path NE '/') THEN path = path + '/'
    END
ENDCASE
RETURN, root_dir + path + filename

END
