; $Id: dl_mac.pro,v 1.1 1994/11/14 23:41:27 davee Exp $

pro doc_file, name, outunit     ; print documentation for file 'name'
  printf, outunit
  printf, outunit, '----- Documentation for ',name
  printf, outunit

  openr, unit, name, /GET_LUN

  line = ""
  outflag = 0
  readf, unit, line
  while not eof(unit) and strpos(line, ";-") ne 0 do begin
    if outflag then $
      printf, outunit, strmid(line, 1, 132)

    ; output lines after line which starts with ";+"
    if strpos(line, ";+") eq 0 then $
      outflag = 1
    readf, unit, line
  endwhile

  close, unit
end

pro DL_MAC, NAME, PRINT=printflag, DIRECTORY = direct

;+NODOCUMENT
; NAME:
;	DL_MAC
;
; PURPOSE:
;	Extract the documentation template of one or more procedures (Macintosh
;	version).
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	DL_MAC		;For prompting.
;	DL_MAC, Name 	;Extract documentation for procedure Name using
;				the current !PATH.
;
; INPUTS:
;	Name:	A string containing the name of the procedure or "*" for all.
;
; OPTIONAL INPUT PARAMETERS:
;
;   DIRECTORY:	The directory to search.  If omitted, the current directory
;		and !PATH are used.
;
;       PRINT:  Name of a file to print to.
;
; OUTPUTS:
;	No explicit outputs.  Documentation is output using 'more' format 
;	unless /PRINT is specified.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Output is produced on terminal or to a file.  If the current directory
;	is also one of the directories specified in !PATH or DIRECTORY,
;	documentation will be output twice for the specified module(s).
;
; RESTRICTIONS:
;	??
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	DJE, 14 Nov 1994, adapted from DL_DOS.PRO
;-

  on_error,2                            ;Return to caller if an error occurs

  ;Interactive query?
  if n_elements(name) eq 0 then begin
    name = ''
    read, 'Name of procedure or * for all: ',name
  endif

  name = strlowcase(name)               ;make it always lower case

  ;
  ; if DIRECTORY not specified, use !path
  ;
  if n_elements(direct) eq 0 then begin
    cd, current=curr

    ; add the current directory to the search path if it is not already there 
    if strpos(!path, curr) eq -1 then $
      path = curr + "," + !path $
    else $
      path = !path
  endif else begin
    path = direct                       ; otherwise use DIRECTORY 
  endelse

  ;
  ; output always goes to the log window
  ;
  outunit=-1
  if n_elements(printflag) ne 0 then $
    openw, outunit, printflag, /GET_LUN

  ;
  ; loop for every directory in path
  ;
  while strlen(path) gt 0 do begin ; Find it
    i = strpos(path, ",")
    if i lt 0 then i = strlen(path)
    name_path = strmid(path, 0, i)
    cd, name_path

    ;
    ;  file_list contains all file(s) to document
    ;
    file_list = findfile(name_path + name + '.pro', count=n_files)
    ; document every file
    for n = 0, n_files-1 do begin
      doc_file, file_list(n), outunit
      printf, outunit
    endfor
    path = strmid(path, i + 1, 1000)
  endwhile

  if n_elements(printflag) ne 0 then $
    close, outunit
  cd, curr

end
