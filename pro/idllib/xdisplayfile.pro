
; $Id: xdisplayfile.pro,v 1.5 1994/10/10 16:25:08 hilary Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
PRO XDispFile_evt, event

WIDGET_CONTROL, GET_UVALUE = retval, event.id

IF(retval EQ "EXIT") THEN WIDGET_CONTROL, event.top, /DESTROY

END


PRO XDisplayFile, FILENAME, TITLE = TITLE, GROUP = GROUP, WIDTH = WIDTH, $
		HEIGHT = HEIGHT, TEXT = TEXT, FONT = font
;+
; NAME: 
;	XDISPLAYFILE
;
; PURPOSE:
;	Display an ASCII text file using widgets and the widget manager.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XDISPLAYFILE, Filename
;
; INPUTS:
;     Filename:	A scalar string that contains the filename of the file
;		to display.  The filename can include a path to that file.
;
; KEYWORD PARAMETERS:
;	FONT:   The name of the font to use.  If omitted use the default
;		font.
;	GROUP:	The widget ID of the group leader of the widget.  If this 
;		keyword is specified, the death of the group leader results in
;		the death of XDISPLAYFILE.
;
;	HEIGHT:	The number of text lines that the widget should display at one
;		time.  If this keyword is not specified, 24 lines is the 
;		default.
;
;	TEXT:	A string or string array to be displayed in the widget
;		instead of the contents of a file.  This keyword supercedes
;		the FILENAME input parameter.
;
;	TITLE:	A string to use as the widget title rather than the file name 
;		or "XDisplayFile".
;
;	WIDTH:	The number of characters wide the widget should be.  If this
;		keyword is not specified, 80 characters is the default.
;
; OUTPUTS:
;	No explicit outputs.  A file viewing widget is created.
;
; SIDE EFFECTS:
;	Triggers the XMANAGER if it is not already in use.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Open a file and create a widget to display its contents.
;
; MODIFICATION HISTORY:
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;-
                                                        ;use the defaults if
IF(NOT(KEYWORD_SET(HEIGHT))) THEN HEIGHT = 24		;the keywords were not
IF(NOT(KEYWORD_SET(WIDTH))) THEN WIDTH = 80		;passed in

IF(NOT(KEYWORD_SET(TEXT))) THEN BEGIN
  IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = FILENAME     
  OPENR, unit, FILENAME, /GET_LUN, ERROR=i		;open the file and then
  if i lt 0 then begin		;OK?
	a = [ !err_string, ' Can not display ' + filename]  ;No
  endif else begin
	  a = strarr(1000)				;Maximum # of lines
	  i = 0
	  c = ''
	  while not eof(unit) do begin
		readf,unit,c
		a(i) = c
		i = i + 1
		endwhile
	  a = a(0:(i-1)>0)  ;Added empty file check -KDB
	  FREE_LUN, unit				;free the file unit.
  endelse
ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'XDisplayFile'
    a = TEXT
ENDELSE

filebase = WIDGET_BASE(TITLE = TITLE, $			;create the base
		/COLUMN, $
		SPACE = 20, $
		XPAD = 20, $
		YPAD = 20)

filequit = WIDGET_BUTTON(filebase, $			;create a Done Button
		VALUE = "Done with " + TITLE, $
		UVALUE = "EXIT")

IF n_elements(font) gt 0 then $
 filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, FONT = font, $
		VALUE = a) $
ELSE filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, $
		VALUE = a)


WIDGET_CONTROL, filebase, /REALIZE			;instantiate the widget

Xmanager, "XDisplayFile", $				;register it with the
		filebase, $				;widget manager
		GROUP_LEADER = GROUP, $
		EVENT_HANDLER = "XDispFile_evt" 

END  ;--------------------- procedure XDisplayFile ----------------------------

