; $Id: dl_unix.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro dl_unix, name, print=printflg, directory = direct, multi = multi
;+NODOCUMENT
; NAME:
;	DL_UNIX
;
; PURPOSE:
;	Extract the documentation template of one or more IDL modules
;	(procedures or functions).
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	DL_UNIX		;For prompting.
;	DL_UNIX, Name 	;Extract documentation for procedure Name using
;				the current !PATH.
;
; INPUTS:
;	Name:	The string containing the name of the procedure or "*" for all.
;	
; KEYWORDS:
;	PRINT:	If set, this keyword sends output of DL_UNIX to lpr.  If
;		PRINT is a string, it is interpreted as a shell
;		command used for output with the documentation from 
;		DL_UNIX providing standard input 
;		(i.e., PRINT="cat > junk").
;
;   DIRECTORY:	The directory to search.  If omitted, the current directory
;		and !PATH are used.
;
;	MULTI:	If set, this flag allows printing of more than one file if the
;		requested module exists in more than one directory in the path
;		and the current directory.
;
; OUTPUTS:
;	No explicit outputs.  Documentation is piped through "more" unless
;	/PRINT is specified.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Output is produced on terminal or printer.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	DMS, Feb, 1988.
;	AB, 21 September 1992, renamed from DOC_LIB_UNIX to DL_UNIX to
;		avoid DOS filename limitations.
;-

on_error,2              ;Return to caller if an error occurs
if n_elements(name) eq 0 then begin	;Interactive query?
	name = ''
	printflg = 0	
	read,'Name of procedure or * for all: ',name
	read,'Enter 1 for printer, 0 for terminal: ',printflg
	endif
name = strlowcase(name)		;make it always lower case

if n_elements(direct) eq 0 then path = ".:" + !path $	;Directories to search
	else path = direct
if n_elements(printflg) eq 0 then output = " more " $
else if strtrim(printflg,2) eq '1' then output = " lpr " $
else if strtrim(printflg,2) eq '0' then output = " more " $
else output = "'"+printflg+"' "

if n_elements(multi) le 0 then multi = 0	;Only print once
if strpos(name,"*") ge 0 then begin	;Wild card?
	multi = 1		;allow printing of multiple files
	endif

cmd = !dir + "/bin/doc_library "+output+strtrim(multi,2)+' ' ;Initial cmd
	
while strlen(path) gt 0 do begin ; Find it
	i = strpos(path,":")
	if i lt 0 then i = strlen(path)
	file = strmid(path,0,i)+ "/" + name + ".pro"
;	print,"File: ",file
	path = strmid(path,i+1,strlen(path))
	cmd = cmd + ' ' + file
	endwhile
;print,cmd
spawn,cmd
end
