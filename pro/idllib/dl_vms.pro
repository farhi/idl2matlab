; $Id: dl_vms.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro DL_VMS, NAME, FILE=file, PRINT=printflg, PATH = path, $
	NOFILEMSG=nofilemsg
;+NODOCUMENT
; NAME:
;	DL_VMS
;
; PURPOSE:
;	Extract the documentation template of one or more procedures.
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	DL_VMS		;For prompting.
;	DL_VMS, Name	;Extract documentation for procedure Name using
;				the current !PATH.
;
; INPUTS:
;	Name:   A string containing the name of the procedure.
;
; KEYWORDS:
;	FILE:   If this keyword is set, the output is sent to the file
;		"userlib.doc", in the current directory.
;
;	PRINT:  If set, this keyword sends the  output of DL_VMS to lpr.
;
;	PATH:   An optional directory/library search path.  This keyword uses
;		the same format and semantics as !PATH.  If omitted, !PATH is
;		used.
; OUTPUTS:
;	Documentation is output to terminal or printer.
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
;	Written, DMS, Sept, 1982.
;	Added library param, Jul 1987.
;	Unix version, Feb, 1988.
;	Revised for VMS Version 2, 15 December 1989
;	Modified by Jim Pendleton, GRO/OSSE NU, July 30, 1992 to handle
;		!PATH's gt 255 characters.
;	AB, 21 September 1992, renamed from DOC_LIB_VMS to DL_VMS to
;		avoid DOS filename limitations.
;	ACY, 25 January 1993, file should be written with stream mode
;-

on_error,2			 ;Return to caller if an error occurs
params = n_params()
if n_elements(path) eq 0 then path = !path
do_print = keyword_set(printflg)
to_file = keyword_set(file)
do_filemsg = not keyword_set(nofilemsg)
if (to_file) then del_file = 0 else del_file = 1

if n_elements(name) eq 0 then begin	;Interactive query?
  name = ''
  printflg = 0
  read,'Name of procedure: ',name
endif

wild = name eq '*'		 ;True for all modules
;  name = strlowcase(name)	 ;make it always lower case
name = strupcase(name)	  ;make it always upper case

setlog,'_DOCUMENT',' ',table='LNM$JOB'  ;Logical name used for communic
cmd_file = !dir + '[bin]doc_library' ;Name & locn of cmd proc

;;cmd = '@'+ cmd_file + '  "[],'+path+'" '+name  ;To search curr directory
;;;; cmd = '@'+ cmd_file + '  "'+path+'" '+name  ;To omit current directory
;;spawn, cmd

; Convert path into a string array, one path section per array element
bpath = byte(path)
comma = (byte(','))(0)
loc = where(bpath eq comma, n_seg)
if (n_seg eq 0) then begin
  path_arr = path
endif else begin
  path_arr = strarr(n_seg + 1)
  last = 0
  for i = 0, n_seg-1 do begin
    path_arr(i) = string(bpath(last:loc(i)-1))
    last = loc(i)+1
  endfor
  path_arr(n_seg) = string(bpath(last:*))
endelse


; Define the logical IDL_DL_PATH to be a multi-valued logical containing
; the value of path_arr
for i = 0, n_elements(path_arr) - 1 do begin
	setlog, 'IDL_DL_PATH_' + strtrim(i, 2), path_arr(i)
endfor

; Spawn the command file to do the searching
spawn, '@'+ cmd_file + '  ' + name
for i = 0, n_elements(path_arr) - 1 do begin
	dellog, 'IDL_DL_PATH_' + strtrim(i, 2)
endfor
i = trnlog('_DOCUMENT', table='LNM$JOB', status)
if status eq ' ' then message, 'Error executing command file ' + cmd_file

if status eq "?" then message, 'Module ' + name + ' not found in library.'

i = strpos(status,' ')	  ;Found module, process
in_name = strmid(status, 0, i)  ;Input file name
to_delete = strmid(status, i+1,1) eq '1'  ;Delete input file flag
source = strmid(status, i+3, 100)	;Source file

if (to_file or do_print) then begin
  openw,outunit,'userlib.doc', /GET_LUN, PRINT=do_print, DELETE=del_file, $
     /STREAM
  if (to_file and do_filemsg) then $
    message, 'Documentation is in file "userlib.doc".', /INFORMATIONAL
endif else begin
  openw,outunit,'sys$output', /GET_LUN, /MORE
endelse

if not wild then begin
	printf, outunit, '---- Module: ', STRUPCASE(name), ' ----'
	printf, outunit, '---- From:   ', source,'  -----'
	endif

openr,inunit,in_name, /GET_LUN, delete=to_delete ;Read text file, delete when done
A=''
WHILE NOT EOF(inunit) DO BEGIN
  READF,inunit,A
  IF STRMID(A,0,2) EQ ';+' THEN BEGIN
    READF,inunit,A
    WHILE STRMID(A,0,2) NE ';-' DO BEGIN
	 PRINTF,outunit,STRMID(A,1,100) & READF,inunit,A
	 ENDWHILE
    if not wild then goto, done else printf, outunit, ""
    ENDIF
ENDWHILE

done: FREE_LUN, inunit, outunit
END
