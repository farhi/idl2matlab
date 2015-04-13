; $Id: mp_widgets.pro,v 1.3 1995/01/20 19:41:01 tonyh Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	MP_WIDGETS
;
; PURPOSE:
;	Provide a graphical user interface to the online documentation.
;	The topic is selected by pressing a button at the top.
;	Subtopics a displayed in a scrolling list on the left side.  Pressing
;	a mouse button while pointing at a subtopic causes the information
;	on that topic to be displayed in the large text region on the right.
;
;	It is expected that this routine will only be called by MAN_PROC.
;
; CATEGORY:
;	Help, documentation, widgets.
;
; CALLING SEQUENCE:
;	MP_WIDGETS [, Request]
;
; INPUTS:
;     Request:	A scalar string containing the item on which help is desired.
;		This string can contain 1 or two (whitespace separated) words.
;		The first word is taken as the global topic and the second
;		as the topic within the scope of the first.
;
; OUTPUTS:
;	None.  A widget interface is used to allow reading the help text.
;
; COMMON BLOCKS:
;	MPW_COM: This common block is private to MP_WIDGETS, and
;		 should not be referenced by other modules.
;
; RESTRICTIONS:
;	The basic version of the help facility (MP_BASIC) can accept
;	ambiguous requests, and if a request maches more than a single
;	subtopic, they are all shown.  This version can also accept
;	ambiguous requests, but only the first subtopic matched is shown.
;	This feature is not as important as it was in MP_BASIC because the
;	widget interface allows multiple subjects to be viewed easily.
;
;	This routine uses a COMMON block to keep its internal state, so only
;	one copy of this routine can run at a time.
;
; MODIFICATION HISTORY:
;	AB, August, 1990
;	28 December 1990	Rewritten to take advantages in changes to
;				the widget facility, to use XMANAGER, and to
;				accept the REQUEST argument.
;	31 December 1992	Modified to ignore the optional %TITLE
;				line at the top of the file. There's no
;				reason to handle it, since this routine is
;				obsolete. The builtin online help system
;				*does* handle it.
;-




function mpw_set_lv1, topic_idx, lv2_topics
; Open a new level 1 help file. Close the old one if one exists.
; Update the common block to reflect the change. lv2_topics is the
; array of level 2 topics found in the file. Returns TRUE if the topic
; was changed, FALSE otherwise.

common mpw_com, cur_lv1_topic_idx, cur_lv2_idx, offsets, text_base, list, $
	title, text, unit, lv1_topics, lv1_files

  if (topic_idx ne cur_lv1_topic_idx) then begin   ; Only if the topic changed.
    if (unit ne 0) then FREE_LUN, unit
    openr, unit, lv1_files(topic_idx), /get_lun
    cur_lv1_topic_idx = topic_idx
    n = 0L
    tmp = ''
    readf, unit, tmp
    ; If it's the version tag, parse it.
    version = 1L					; Assume old format
    if (strmid(tmp, 0, 9) eq '%VERSION:') then begin
      reads, tmp, version, format='(9X, I0)'
      readf,unit,tmp				; Read next line.
    endif
    if (strmid(tmp, 0, 7) eq '%TITLE:') then readf, unit, tmp	; Skip title
    n = long(tmp)				; # of records
    if (version ne 1) then begin
      ; Version 2 format has the number of characters used by all the
      ; subtopics on the next line. We don't use it, but have to read it
      readf,unit,tmp				; Read next line.
    endif
    lv2_topics = strarr(n)			;Make names
    readf, unit, lv2_topics			;Read entire string from unit
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
    tmp = fstat(unit)
    text_base = tmp.cur_ptr
    cur_lv2_idx=-1
    r = 1
  endif else r = 0

  return, r
end







pro mpw_update_display, lv2_topics
; Update the topic label and list elements to reflect the current state
; as determined by mpw_set_lv1. lv2_topics is the array of level 2 topics
; returned by mpw_set_lv1.

  common mpw_com, cur_lv1_topic_idx, cur_lv2_idx, offsets, text_base, list, $
	title, text, unit, lv1_topics, lv1_files

  WIDGET_CONTROL, list, set_value=lv2_topics
  WIDGET_CONTROL,title,set_value='Current Topic: ' $
	+ lv1_topics(cur_lv1_topic_idx)
  WIDGET_CONTROL,text,set_value='', /NO_NEWLINE

end







pro mpw_set_lv2, index
; Given an index, display the text associated with it in the current
; help file.

  common mpw_com, cur_lv1_topic_idx, cur_lv2_idx, offsets, text_base, list, $
	title, text, unit, lv1_topics, lv1_files

  if (cur_lv2_idx ne index) then begin
    str = ''
    POINT_LUN, unit, text_base + offsets(index)
    readf, unit, str                ; Skip the ";+"
    ; Remember this position
    start = fstat(unit)
    start = start.cur_ptr
    ; Find the end
    while (str NE ";-") do readf, unit, str
    ; How long is the selection?
    tmp = fstat(unit)
    len = tmp.cur_ptr - start - 3
    ; Read the text using binary I/O into a single byte array for efficiency
    str = bytarr(len)
    point_lun, unit, start
    readu, unit, str
    WIDGET_CONTROL, text, SET_VALUE=string(str), /NO_NEWLINE
    cur_lv2_idx = index
  endif

end







pro mpw_event, ev

common mpw_com, cur_lv1_topic_idx, cur_lv2_idx, offsets, text_base, list, $
	title, text, unit, lv1_topics, lv1_files

  case (tag_names(ev, /STRUCTURE_NAME)) of
  "WIDGET_BUTTON": begin
	  WIDGET_CONTROL, get_uvalue=uv, ev.id
	  if (uv eq -2) then begin
	    WIDGET_CONTROL, /DESTROY, ev.top
	    if (unit ne 0) then FREE_LUN, unit
	    return
	  endif else begin
	    if (mpw_set_lv1(uv,lv2_topics)) then mpw_update_display,lv2_topics
	  endelse
	end
  "WIDGET_LIST": mpw_set_lv2, ev.index
  endcase


end







function MPW_TM, KEY, TOPIC_ARRAY
; Topic Matcher. Given KEY, this routine returns an index into TOPIC_ARRAY
; that matches KEY. If there is an exact match, that index is returned.
; otherwise the first element with the same prefix is returned.
; On error, MESSAGE is used to report the problem and terminate execution.

  found = where(STRTRIM(TOPIC_ARRAY) eq KEY, count) ; Match exact string
  if (count le 0) then begin	; No exact match, try to match the prefix
    FOUND = where(strpos(TOPIC_ARRAY, KEY) eq 0, count)
    if (count le 0) then begin
      message, /NONAME, 'Unknown topic: ' + KEY
    endif else begin
      if (count ne 1) then begin
	message, /INFO, /NONAME, 'Ambiguous topic "' + KEY $
		+ '" matches: ' + string(format='(i0, " ")', count) $
		+ 'items. ' + strcompress(topic_array(found(0)), /remove) $
		+ ' used.'
      endif
    endelse
  endif

  return, found(0)
end







pro MP_WIDGETS, request

  common mpw_com, cur_lv1_topic_idx, cur_lv2_idx, offsets, text_base, list, $
	title, text, unit, lv1_topics, lv1_files

  if (XREGISTERED('MP_WIDGETS')) then return	; Only one copy at a time

  on_error, 1		; On error, return to main level
  unit = 0		; No help file is open yet

  ; lv1_files recieves all help files found through !HELP_PATH.
  lv1_dirs = EXPAND_PATH(!HELP_PATH, /ARRAY, COUNT=cnt)
  if (cnt eq 0) then message, 'No online help files found.'
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


  ; Check the request (if any) for validity before any widgets are created.
  ; If it is empty, act as if it isn't present.
  REQ_PRESENT = N_ELEMENTS(REQUEST) ne 0
  if (REQ_PRESENT) then begin
    temp = size(request)
    if (temp(0) NE 0) then message, 'Argument must be scalar.'
    if (temp(1) NE 7) then message, 'Argument must be of type string.'
    if (STRLEN(STRCOMPRESS(REQUEST, /REMOVE_ALL)) eq 0) then REQ_PRESENT = 0
  endif

  ; Choose initial display using first option in this list that fits:
  ;  - Request on the command line.
  ;  - The topic used last time this routine was run.
  ;  - Topic is ROUTINES, subtopic is empty
  if (REQ_PRESENT) then begin
    ; Parse into 1 or two strings
    lv1_topic_idx = STRUPCASE(STRTRIM(STRCOMPRESS(REQUEST), 2))
    if (((blank_pos = STRPOS(lv1_topic_idx, ' '))) ne -1) then begin
      lv2_topic = STRMID(lv1_topic_idx, blank_pos+1, 10000L)
      lv1_topic_idx = STRMID(lv1_topic_idx, 0, blank_pos)
    endif else begin
      lv2_topic=''
    endelse
    ; Make sure its legitimate.
    lv1_topic_idx = MPW_TM(lv1_topic_idx, lv1_topics)
    cur_lv1_topic_idx = -1
    junk = mpw_set_lv1(lv1_topic_idx, lv2_topics)
    if (lv2_topic ne '') then lv2_index = MPW_TM(lv2_topic, lv2_topics) $
      else lv2_index = -1
  endif else begin			; No request is present
    if (n_elements(cur_lv1_topic_idx) eq 0) then begin	; Default to ROUTINES
      lv1_topic_idx=MPW_TM("ROUTINES", lv1_topics)	; No sub-topic
      lv2_index = -1
    endif else begin					; Use previous state
      lv1_topic_idx=cur_lv1_topic_idx
      lv2_index = cur_lv2_idx
    endelse
    cur_lv1_topic_idx = -1
    junk = mpw_set_lv1(lv1_topic_idx, lv2_topics)
  endelse
  cur_lv2_idx = -1				; Forget the old state

  ; The request (if any) is OK. Create and realize the widgets.
  base = WIDGET_BASE(title='Help', /COLUMN)
  cntl1 = WIDGET_BASE(base, /FRAME, /ROW, space=30)
  if (!version.os eq 'sunos') then tmp = 10 else tmp = 5
  cntl2 = WIDGET_BASE(base, column=tmp, /FRAME, /exclusive)
    pb_quit = WIDGET_BUTTON(value='Quit', cntl1, uvalue = -2)
    topic_but_ids = lonarr(n, /nozero)
    for i = 0, n-1 do $
      topic_but_ids(i) = WIDGET_BUTTON(value=lv1_topics(i), cntl2, $
				    uvalue = i, /NO_RELEASE)
  title=WIDGET_LABEL(cntl1, value='Current Topic:')
  bottom=WIDGET_BASE(base, /ROW)
  list = WIDGET_LIST(bottom,ysize=30,value = string(bytarr(20) + 45B),/frame)
  text = WIDGET_TEXT(bottom, /SCROLL, xsize = 80, ysize=45)

  WIDGET_CONTROL, base, /REALIZE

  mpw_update_display, lv2_topics

  ; Set the proper button
  if (cur_lv1_topic_idx ne -1) then $
    WIDGET_CONTROL, topic_but_ids(cur_lv1_topic_idx), /SET_BUTTON

  lv2_topics = 0		; Free the dynamic memory
  if (lv2_index ne -1) then mpw_set_lv2, lv2_index

  XMANAGER, 'MP_WIDGETS', base, event_handler='MPW_EVENT'
end
