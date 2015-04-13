; $Id: mk_library_help.pro,v 1.5 1995/07/07 17:47:49 griz Exp $

;+
; NAME:
;	MK_LIBRARY_HELP
;
; PURPOSE:
;	Given a directory or a VMS text library containing IDL user library
;	procedures, this procedure generates a file in the IDL help format
;	that contains the documentation for those routines that contain
;	a DOC_LIBRARY style documentation template.  The output file is
;	compatible with the IDL "?" command.
;
;	A common problem encountered with the DOC_LIBRARY user library
;	procedure is that the user needs to know what topic to ask for
;	help on before anything is possible.  The XDL widget library
;	procedure is one solution to this "chicken and egg" problem.
;	The MK_LIBRARY_HELP procedure offers another solution.
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	MK_LIBRARY_HELP, Source, Outfile
;
; INPUTS:
;      SOURCE:	The directory or VMS text library containing the
;		.pro files for which help is desired.  If SOURCE is
;		a VMS text library, it must include the ".TLB" file
;		extension, as this is what MK_LIBRARY_HELP uses to
;		tell the difference.
;
;     OUTFILE:	The name of the output file which will be generated.
;		If you place this file in the help subdirectory of the
;		IDL distribution, and if it has a ".help" extension, then
;		the IDL ? command will find the file and offer it as one
;		of the availible help categories. Note that it uses the
;		name of the file as the name of the topic.
;
; KEYWORDS:
;     TITLE:	If present, a string which supplies the name that
;		should appear on the topic button for this file
;		in the online help. This title is only used on
;		systems that allow very short file names (i.e. MS DOS),
;		so it won't always have a visible effect. However,
;		it should be used for the broadest compatibility. If
;		a title is not specified, IDL on short name systems
;		will use a trucated copy of the file name.
;     VERBOSE:	Normally, MK_LIBRARY_HELP does its work silently.
;		Setting this keyword to a non-zero value causes the procedure
;		to issue informational messages that indicate what it
;		is currently doing.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A help file with the name given by the OUTFILE argument is
;	created.
;
; RESTRICTIONS:
;	If you put the resulting file into the "help" subdirectory of
;	the IDL distribution, it will be overwritten when you install a
;	new release of IDL.  To avoid this problem, keep a backup copy of
;	your created help files in another directory and move them into
;	the new distribution.
;
;	Since this routine copies the documentation block from the
;	functions and procedures, subsequent changes will not be
;	reflected in the help file unless you run MK_LIBRARY_HELP
;	again.
;
;	The following rules must be followed in formating the .pro
;	files which are searched.
;		(a) The first line of the documentation block contains
;		    only the characters ";+", starting in column 1.
;		(b) The last line of the documentation block contains
;		    only the characters ";-", starting in column 1.
;		(c) Every other line in the documentation block contains
;		    a ";" in column 1.
;
;	No reformatting of the documentation is done.
;
; MODIFICATION HISTORY:
;	17 April, 1991, Written by AB, RSI.
;	1 December 1992, Reorganized internally. No functionality change.
;	31 December 1992, Added the TITLE keyword as part of the effort
;		towards releasing the Microsoft Windows version of IDL.
;	20 January 1993,  Corrected for VMS, W. Landsman HSTX
;	17 March 1993, Corrected for MSDOS and VMS, AB.
;	13 January 1994, Added the VERSION attribute and modified format to
;		accomodate extended identifier names (the old format was
;		limited to 15 characters.
;       7 July 1995, Added support for MacOS, DD, RSI.
;-
;
;


pro mlh_grab_header, in_file, num, id_chars, subject, idx_file, $
	txt_file, verbose
;
; Searches in_file for all text between the ;+ and ;- comments, and
; updates the index and text files appropriately.
;
; entry:
;	in_file - LUN for .PRO file containing documentation header.
;	num - Named variable containing # of entries added to idx_file.
;	id_chars - Named variable containing number of characters used
;		by all identifier names in the table of contents.
;	subject - Name of subject represented by in_file
;	idx_file - Scratch file to which table of contents entries will
;		be written.
;	txt_file - Scratch file to which the documentation header will
;		be written.
;	verbose - TRUE if the routine should output a descriptive message
;		when it finds the documentation header.
;
; exit:
;	in_file - Closed via FREE_LUN
;	num - Incremented if a documentation header was found.
;	id_chars - Updated to reflect new entry added to table of contents.
;	idx_file, txt_file - Updated as necessary. Both are positioned at EOF.
;

  ; Under DOS, formatted output ends up with a carriage return linefeed
  ; pair at the end of every record. The resulting file would not be
  ; compatible with Unix. Therefore, we use unformatted output, and
  ; explicity add the linefeed, which has ASCII value 10.
  LF=10B

  ; Find the opening line
  tmp = ''
  found = 0
  while (not eof(in_file)) and (not found) do begin
    readf, in_file, tmp
    if (strmid(tmp, 0, 2) eq ';+') then found = 1
  endwhile

  if (found) then begin
    if (verbose ne 0) then message, /INFO, subject
    num = num + 1
    id_chars = id_chars + strlen(subject)
    point_lun, -txt_file, txt_pos
    writeu, idx_file, string(txt_pos, subject, format='(I0, ":", A)'), LF
    writeu, txt_file, ';+', LF
    txt_pos = txt_pos + 3
    found = 0
    while (not found) do begin
      readf, in_file, tmp
      if (strmid(tmp, 0, 2) eq ';-') then begin
        found = 1
      endif else begin
	len = strlen(tmp)
	if (len eq 0) then len = 1
	txt_pos = txt_pos + len
        writeu, txt_file, strmid(tmp, 1, 1000), LF
      endelse
    endwhile

    writeu, txt_file, ';-', LF
    txt_pos = txt_pos + 3
  endif

  FREE_LUN, in_file
end







pro MLH_GEN_FILE, num, id_chars, idx_file, txt_file, outfile, verbose, title
;
; Build a .HELP file from the consituent parts.
;
; entry:
;	num - # of subjects
;	id_chars - # of characters used by identifiers in table of contents
;		not counting any possible NULL termination.
;	idx_file - Scratch file containing the table of contents.
;	txt_file - Scratch file containing the documentation text.
;	outfile - NAME of final HELP file to be generated.
;	verbose - TRUE if the routine should output a descriptive message
;		when it finds the documentation header.
;	title - Scalar string. If non-null,  contains the optional %TITLE
;		name to go at the top of the file.
;
; exit:
;	outfile has been created.
;	idx_file and txt_file have been closed via FREE_LUN


  ; Rewind the scratch files.
  point_lun, idx_file, 0
  point_lun, txt_file, 0

  ; Open the final file.
  openw, final_file, outfile, /stream, /get_lun
  if (verbose ne 0) then message, /INFO, 'building ' + outfile

  ; Under DOS, formatted output ends up with a carriage return linefeed
  ; pair at the end of every record. The resulting file would not be
  ; compatible with Unix. Therefore, we use unformatted output, and
  ; explicity add the linefeed, which has ASCII value 10.
  LF=10B

  writeu, final_file, '%VERSION:2', LF
  if (title ne '') then writeu, final_file, '%TITLE:', STRUPCASE(title), LF

  writeu, final_file, string(num, format='(I0)'), LF	; The subject count
  writeu, final_file, string(id_chars, format='(I0)'), LF  ; # of subject chars

  tmp = ''
  ON_IOERROR, IDX_DONE
  while 1 do begin                              ; Breaks out via IOERROR
    readf, idx_file, tmp
    writeu, final_file, tmp, LF
  endwhile
IDX_DONE:
  free_lun, idx_file				; This deletes the index file

  tmp = ''
  ON_IOERROR, TXT_DONE
  while 1 do begin				; Breaks out via IOERROR
    readf, txt_file, tmp
    writeu, final_file, tmp, LF
  endwhile
TXT_DONE:
  ON_IOERROR, NULL
  free_lun, txt_file, final_file		; Deletes the text file


end







pro MK_LIBRARY_HELP, source, outfile, verbose=verbose, title=title

  if (not keyword_set(verbose)) then verbose = 0
  if (not keyword_set(title)) then title=''

  ; Open two temporary files, one for the index and one for the text.
  openw, idx_file, filepath('userlib.idx', /TMP), /stream, /get_lun, /delete
  openw, txt_file, filepath('userlib.txt', /TMP), /stream, /get_lun, /delete

  ; This is the number of files that actually contain a documentation block
  num = 0

  ; This is the number of characters used by all the identifiers in the
  ; table of contents, not counting any possible NULL termination.
  id_chars = 0

  if (verbose ne 0) then message, /INFO, 'searching ' + source

  ; Is it a VMS text library?
  vms_tlb = 0				; Assume not
  if (!version.os eq 'vms') then begin
    ; If this is VMS, decide if SOURCE is a directory or a text library.
    if ((strlen(SOURCE) gt 4) and $
        (strupcase(strmid(SOURCE, strlen(SOURCE)-4,4)) eq '.TLB')) then $
      vms_tlb = 1
  endif

  ; If it is a text library, get a list of routines by spawning
  ; a LIB/LIST command. Otherwise, just search for .pro files.
  if (vms_tlb) then begin
    spawn,'LIBRARY/TEXT/LIST ' + SOURCE, FILES
    count = n_elements(files)
    i = 0
    while ((i lt count) and (strlen(files(i)) ne 0)) do i = i + 1
    count = count - i - 1
    if (count gt 0) then files = files(i+1:*)
  endif else begin		  ; Search for .pro files
    case !version.os_family of
     'unix': tmp = '/'
     'vms': tmp = ''
     'Windows': tmp = '\'
     'MacOS': tmp = ':'
    endcase 
    files = findfile(source + tmp + '*.pro', count=count)
    if (!version.os eq 'vms') then begin
      if (count ne 0) then prefix_length = STRPOS(files(0), ']')+1
    endif else begin
      prefix_length = strlen(source) + 1
    endelse
  endelse

  for i = 0, count-1 do begin
    if (vms_tlb) then begin
      ; We do a separate extract for each potential routine. This is
      ; pretty slow, but easy to implement. This routine is generally
      ; run once in a long while, so I think it's OK
      subject = files(i)
      name = filepath('mklibhelp.scr', /tmp)
      spawn, 'LIBRARY/TEXT/EXTRACT='+subject+'/OUT='+name+' '+SOURCE
      openr, in_file, name, /get_lun, /delete
    endif else begin
      name = files(i)
      if (!version.os eq 'vms') then begin
        subject = STRMID(name, prefix_length, 1000)
        subject = STRMID(subject, 0, STRPOS(subject, '.'))
      endif else begin
        subject = strupcase(strmid(name, prefix_length, $
	  strlen(name) - prefix_length-4))
      endelse
      openr, in_file, name, /get_lun
    endelse

    ; Grab the header
    MLH_GRAB_HEADER, in_file, num, id_chars, subject, idx_file, $
		txt_file, verbose


  endfor

  ; Produce the final product
  MLH_GEN_FILE, num, id_chars, idx_file, txt_file, outfile, verbose, title

end
