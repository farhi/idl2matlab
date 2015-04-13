; $Id: doc_library.pro,v 1.3 1995/01/20 19:41:01 tonyh Exp $

pro doc_library, name, print=printflg, directory = direct, multi = multi, $
	PATH = path, FILE=file
;+
; NAME:
;	DOC_LIBRARY
;
; PURPOSE:
;	Extract the documentation template of one or more IDL modules
;	(procedures or functions).  This command provides a standard interface
;	to the operating-system specific DL_DOS, DL_UNIX, and
;	DL_VMS procedures.
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	DOC_LIBRARY		;For prompting.
;
;	DOC_LIBRARY, Name 	;Extract documentation for procedure Name using
;				the current !PATH.
;
; INPUTS:
;	Name:	The string containing the name of the procedure.
;		Under Unix, Name may be "*" to get information on all routines.
;
; KEYWORDS:
;	PRINT:	If set, this keyword sends the output of DOC_LIBRARY to the
;		default printer.  Under Unix, if PRINT is a string, it is
;		interpreted as a shell command used for output with
;		the documentation from DOC_LIBRARY providing standard input
;		(i.e. PRINT="cat > junk").
;
; UNIX KEYWORDS:
;   DIRECTORY:	The directory to search.  If omitted, the current directory
;		and !PATH are used.
;
;	MULTI:	If set, this flag allows printing of more than one file if the
;		requested module exists in more than one directory in the path
;		and the current directory.
;
; VMS KEYWORDS:
;	FILE:	If this keyword is set, the output is left in the file
;		"userlib.doc", in the current directory.
;
;	PATH:	An optional directory/library search path.  This keyword uses
;		the same format and semantics as !PATH.  If omitted, !PATH is
;		used.
;
; OUTPUTS:
;	Documentation is sent to the standard output unless /PRINT
;	is specified.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Output is produced on terminal or printer.
;
; RESTRICTIONS:
;	The DIRECTORY and MULTI keywords are ignored under VMS. The
;	FILE and PATH keywords are ignored under Unix.
;
; EXAMPLE:
;	To obtain documentation for the User's Library function DIST, enter:
;		DOC_LIBRARY, 'DIST'
;
;	For a graphical interface to DOC_LIBRARY, see the procedure XDL.
;
; MODIFICATION HISTORY:
;	Written, DMS, Sept, 1982.
;	Added library param, Jul 1987.
;	Unix version, DMS, Feb, 1988.
;	New VMS version, DMS, Dec. 1989
;	Wrapper procedure to call the correct version
;		under Unix and VMS, AB, Jan 1990
;       Added support for DOS, SNG, Dec, 1990
;	Added support for Macintosh, DJE, Nov, 1994
;-

on_error,2                        ;Return to caller if an error occurs
case !version.os of
  'vms':	DL_VMS, NAME, PRINT=printflg, FILE=file, PATH=path
  'Win32':	DL_DOS, NAME, DIRECTORY=direct, PRINT=printflg
  'MacOS':	DL_MAC, NAME, DIRECTORY=direct, PRINT=printflg
  else:		DL_UNIX, NAME, DIRECTORY=direct, PRINT=printflg, MULTI = multi
endcase
end
