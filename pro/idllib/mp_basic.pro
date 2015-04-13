; $Id: mp_basic.pro,v 1.6 1995/01/20 19:41:01 tonyh Exp $

;+
; NAME:
;	MP_BASIC
;
; PURPOSE:
;	Provide online documentation for IDL topics. The style
;	is a cross between Unix man pages and VMS online help.  The
;	help is organized in a two level hierarchy --- Level 1 is the
;	global subject, and Level 2 supplies help on subjects within
;	each global subject.  If !D.WINDOW is not -1, (window system in use)
;	the mouse is used to prompt for subjects, otherwise, the normal tty
;	interface is used.
;
;	THIS ROUTINE IS TO BE CALLED ONLY BY MAN_PROC.  Users can obtain
;	online help by using the "?" command.
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	MP_BASIC [, Request]
;
; INPUTS:
;     REQUEST:	A scalar string containing the item on which help is desired.
;		This string can contain 1 or 2 (whitespace separated) words.
;		The first word is taken as the global topic and the second
;		as the topic within the scope of the first.  The procedure
;		prompts for missing words.
;
; OUTPUTS:
;	Help text is sent to the standard output.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	The help text is derived from the LaTeX files used to produce
;	the reference manual.  However, it is not possible to produce
;	exactly the same output as found in the manual due to limitations
;	of text oriented terminals.  The text used is therefore considerably
;	abbreviated.  Always check the manual if the online help is
;	insufficient.
;
; MODIFICATION HISTORY:
;	3 November 1988, AB
;
;	January, 1989, AB
;	Added ambiguity resolution, ability to handle multiple levels,
;	and support for mouse.
;
;       SNG, December, 1990	Added support for MS-DOS.
;
;	3 January 1991, AB
;	Renamed from MAN_PROC to make room for the widget version.
;	
;       Bobby Candey, Atlantic Research         30 January 1991
;       Added looping in VMS version to extract multiple "]" from filenames
;
;       31 December 1992, AB    Modified to ignore the optional %TITLE
;                               line at the top of the file. There's no
;                               reason to handle it, since this routine is
;                               obsolete. The builtin online help system
;                               *does* handle it.
;
;	11 February 1993, SMR	Added support for the Mac Version
;	21 April 1993, AB, Added ability to use new !HELP_PATH system
;		variable to locate help files.
;	13 January 1994, AB	Added ability to understand new version
;				2 help files with extended identifier names.
;
;-
;

function MPB_SELTOPIC, SUBJECT, TOPIC_ARRAY, INITIAL
; Given a subject header and an array of topics, returns a string with
; the requested topic (which may or may not be in TOPIC_ARRAY).
; Initial is the index of the initial selection to be highlighed *if*
; a window system menu is used.
on_error,2                      ;Return to caller if an error occurs
target = ''
if((!d.name eq 'X') or (!d.name eq 'SUN')) then begin
    index = wmenu([SUBJECT, TOPIC_ARRAY], title=0, initial=initial)
    if (index ne -1) then target = TOPIC_ARRAY(index-1)
  endif else begin				; Use tty
    if (!VERSION.OS NE 'MacOS') then $
      openw, outunit, filepath(/TERMINAL), /MORE, /GET_LUN $
    else outunit = -1
    printf, outunit, format = '(/,A,":",/)', SUBJECT
    printf, outunit, TOPIC_ARRAY
    if (!VERSION.OS NE 'MacOS') then close, outunit
    outunit = 0
    print, format='(/,/)'
    read, 'Enter topic for which help is desired: ', target
  endelse

return, STRCOMPRESS(STRUPCASE(target),/REMOVE_ALL)    ; Up case & no blanks
end



function MPB_TM, KEY, TOPIC_ARRAY, FOUND, OUTUNIT
; Topic MAtch. Given a string, MPB_TM returns an array of indicies into
; TOPIC_ARRAY that match into FOUND. If there is an exact match
; only its index is returned, otherwise all elements with the same prefix
; match. The number of elements that matched is returned.
; OUTUNIT is the file unit to which output should be directed.
on_error,2                      ;Return to caller if an error occurs
l_topic_array = strupcase(topic_array)
found = [ where(STRTRIM(L_TOPIC_ARRAY) eq KEY, count) ] ; Match exact string
if (count le 0) then begin	; No exact match, try to match the prefix
  FOUND = [ where(strpos(L_TOPIC_ARRAY, KEY) eq 0, count) ]
  if ((count le 0) or (KEY eq '')) then begin		;Found it?
    count = 0
    printf,outunit, !MSG_PREFIX, 'Nothing matching topic "',KEY,'" found."
    printf,outunit, !MSG_PREFIX, 'Enter "?" for list of available topics.'
  endif else begin
    if (count ne 1) then begin
      printf, outunit, format = "(A,'Ambiguous topic ""', A, '"" matches:')",$
	     !MSG_PREFIX, KEY
      printf, OUTUNIT, TOPIC_ARRAY(FOUND)
      endif
  endelse
endif
return, count
end



PRO MP_BASIC, REQUEST

  on_error,1                      ; Return to main level if error occurs
  outunit = (inunit = 0)
  lv1_topic = (lv2_topic = '')

  if (N_ELEMENTS(REQUEST)) then begin
    temp = size(request)
    if (temp(0) NE 0) then begin
      MSG = 'Argument must be scalar.'
      goto, fatal
      endif
    if (temp(1) NE 7) then begin
      MSG = 'Argument must be of type string.'
      goto, FATAL
      endif
    ; Parse into 1 or two strings
    lv1_topic = STRUPCASE(STRTRIM(STRCOMPRESS(REQUEST), 2))
    if (((blank_pos = STRPOS(lv1_topic, ' '))) ne -1) then begin
	lv2_topic = STRMID(lv1_topic, blank_pos+1, 10000L)
	lv1_topic = STRMID(lv1_topic, 0, blank_pos)
      endif
  endif


  ; lv1_files recieves all help files found through !HELP_PATH.
  lv1_dirs = EXPAND_PATH(!HELP_PATH, /ARRAY, COUNT=cnt)
  if (cnt eq 0) then begin
    MSG = 'No online help files found.'
    goto, fatal
  endif
  for i = 0, cnt-1 do begin
    tmp = STRLOWCASE(findfile(filepath('*.help', root_dir=lv1_dirs(i))))
    if (i eq 0) then lv1_files = TEMPORARY(tmp) $
    else lv1_files=[lv1_files, TEMPORARY(tmp)]
  endfor

  ; lv1_topics gets uppercase version of just the names.
  lv1_topics = STRUPCASE(lv1_files)
  if !version.os ne 'Win32' then begin
    tail = STRPOS(lv1_topics, '.HELP')
  endif else begin
    tail = STRPOS(lv1_topics, '.HEL')
  endelse
  n = n_elements(lv1_topics)
  for i = 0, n-1 do $
	lv1_topics(i) = strmid(lv1_topics(i), 0, tail(i))
  for i = 0, n-1 do begin	; Strip path part off lv1_topics
    case !version.os of
      'vms': begin
           j = STRPOS(lv1_topics(i), ']')
           while (j ne -1) do begin
             lv1_topics(i) = strmid(lv1_topics(i), j+1, 32767)
             j = STRPOS(lv1_topics(i), ']')
           endwhile
      end
      'Win32': begin
        j = STRPOS(lv1_topics(i), '\')
        while (j ne -1) do begin
  	  lv1_topics(i) = strmid(lv1_topics(i), j+1, 32767)
          j = STRPOS(lv1_topics(i), '\')
        endwhile
      end
      'MacOS': begin
        j = STRPOS(lv1_topics(i), ':')
        while (j ne -1) do begin
  	  lv1_topics(i) = strmid(lv1_topics(i), j+1, 32767)
          j = STRPOS(lv1_topics(i), ':')
        endwhile
      end
      else:  begin      ; Unix otherwise
        j = STRPOS(lv1_topics(i), '/')
        while (j ne -1) do begin
  	  lv1_topics(i) = strmid(lv1_topics(i), j+1, 32767)
          j = STRPOS(lv1_topics(i), '/')
        endwhile
      end
    endcase
  endfor

  ; Sort the topics into alphabetical order.
  tmp = sort(lv1_topics)
  lv1_files = lv1_files(tmp)
  lv1_topics = lv1_topics(tmp)

  initial = where(lv1_topics eq 'ROUTINES')
  if (lv1_topic eq '') then $
    lv1_topic = MPB_SELTOPIC('Help categories', lv1_topics, initial(0)+1)
  if (!VERSION.OS NE 'MacOS') then $
    openw, outunit, filepath(/TERMINAL), /MORE, /GET_LUN $
  else outunit = -1
  if (((count=MPB_TM(lv1_topic,lv1_topics,found,outunit))) eq 0) then $
    goto, done
  IF (!VERSION.OS NE 'MacOS') THEN free_lun, outunit
  outunit = 0
  lv2_subject = lv1_topics(found(0))		; Use the first element

  ; At this point, a global subject exists, process the specific subject
  lv2_topics = ''
  offset = 0L
  openr, inunit, lv1_files(found(0)), /GET_LUN
  outunit = 0;
  n = 0L
  tmp=''
  readf,inunit,tmp				; Read first line.
  ; If it's the version tag, parse it.
  version = 1L					; Assume old format
  if (strmid(tmp, 0, 9) eq '%VERSION:') then begin
    reads, tmp, version, format='(9X, I0)'
    readf,inunit,tmp				; Read next line.
  endif
  if (strmid(tmp, 0, 7) eq '%TITLE:') then readf, inunit, tmp   ; Skip title
  n = long(tmp)					;# of records
  if (version ne 1) then begin
    ; Version 2 format has the number of characters used by all the
    ; subtopics on the next line. We don't use it, but have to read it
    readf,inunit,tmp				; Read next line.
  endif
  lv2_topics = strarr(n)			;Make names
  readf,inunit,lv2_topics			;Read entire string to inunit
  if (version eq 1) then begin
    offsets = long(strmid(lv2_topics, 15, 30))	;Extract starting bytes
    lv2_topics = strmid(lv2_topics,0,15)		;Isolate names
  endif else begin
    offsets = lonarr(n)
    for i = 0, n-1 do begin
      tmp = lv2_topics(i)
      colon = strpos(tmp, ':') + 1		; Find delimiter
      offsets(i) = long(strmid(tmp, 0, colon))
      lv2_topics(i) = strmid(tmp, colon, 10000000)
    endfor
  endelse

  ; Determine the base of the help text
  tmp = fstat(inunit)
  text_base = tmp.cur_ptr

  ; If no topic is supplied, prompt for one
  if lv2_topic eq '' then $
    lv2_topic = MPB_SELTOPIC(STRUPCASE(lv2_subject), lv2_topics, 1)

  if (!VERSION.OS NE 'MacOS') THEN $
    openw, outunit, filepath(/TERMINAL), /MORE, /GET_LUN $
  else outunit = -1
  if (((count=MPB_TM(lv2_topic,lv2_topics,found,outunit))) eq 0) then $
    goto, done
  str = ''
  for i = 0, count-1 do begin
    index = found(i)
    if (count ne 1) then $
      printf, outunit, lv2_topics(index), $
	      format='("***************",/,A,/,"***************")'
      POINT_LUN, inunit, text_base + offsets(index)
      readf, inunit, str		; Skip the ";+"
      readf, inunit, str
      !err = 0
      while (str NE ";-") do begin
	printf, outunit, str, ' '
	if !err ne 0 then goto, DONE
	readf, inunit, str
	endwhile
      endfor

  goto, DONE
FATAL:		; The string MSG must be set
  message, MSG, /RETURN
DONE:
  if (outunit ne 0) then FREE_LUN, outunit
  if (inunit ne 0) then FREE_LUN, inunit
end
