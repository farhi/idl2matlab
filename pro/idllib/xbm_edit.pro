; $Id: xbm_edit.pro,v 1.3 1993/11/10 17:02:49 beth Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	XBM_EDIT
;
; PURPOSE:
;	This routine allows users to create and edit bitmaps for use with IDL
;	widgets as icons.
;
;	The icons can be saved in two different file formats.  IDL "array 
;	definition files" are text files that can be inserted into IDL 
;	programs.  "Bitmap array files" are data files that can be read into 
;	IDL programs.  Bitmap array files are to be used temporarily until 
;	the final icon design is determined and then they can be saved as 
;	IDL array definitions for inclusion in the final code.  This routine 
;	does not check the file types of the files being read in and assumes 
;	that they are of the correct size and type for reading.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XBM_EDIT
;
; KEYWORD PARAMETERS:
;     FILENAME:	A scalar string that contains the file name to be used.  If 
;		this argument is not specified, the name "idl.bm" is used.
;
;	GROUP:	The widget ID of the widget that calls XBM_EDIT.  When this
;		ID is specified, the death of the caller results in the death
;		of XBM_EDIT.
;
;	XSIZE:	The number of pixels across the bitmap is in the horizontal
;		direction.  The default value is 16 pixels.
;
;	YSIZE:	The number of pixels across the bitmap is in the vertical
;		direction.  The default value is 16 pixels.
;
; SIDE EFFECTS:
;	Initiates the XManager if it is not already running.
;
; RESTRICTIONS:
;	XBM_EDIT maintains its state in a common block so it is restricted
;	to one working copy at a time.
;
; PROCEDURE:
;	Create and register the widget and then exit.
;
; MODIFICATION HISTORY:
;	Created from a template written by: Steve Richards, January, 1991
;-



;------------------------------------------------------------------------------
;	procedure update_display
;------------------------------------------------------------------------------
; This procedure redraws both the drawing area and the small icon example 
; displays.
;------------------------------------------------------------------------------

PRO update_display

COMMON bitedit, bytemap, drawwin, showwin, thexsize, theysize, multiplier, $
		nameid, drawmode

wset, showwin
tv, 255 - bytemap, 6, 6
tv, bytemap, thexsize + 12 , 6
wset, drawwin
grid = 255 - REBIN(bytemap, $
		thexsize * multiplier, $
		theysize * multiplier, /SAMPLE)
FOR i = 0, thexsize - 1 do grid(i * multiplier, *) = 150
FOR i = 0, theysize - 1 do grid(*, i * multiplier) = 150
tv, grid

END ;============= end of xbm_edit display update routine  =============


;------------------------------------------------------------------------------
;	procedure draw_pixel
;------------------------------------------------------------------------------
; This routine draws a pixel on the drawing area of the widget.  When the MARK
; keyword is set, a pixel with a mark is drawn to be used when drawing lines,
; circles, and rectangles.
;------------------------------------------------------------------------------

PRO draw_pixel, x, y, color, MARK = MARK

COMMON bitedit, bytemap, drawwin, showwin, thexsize, theysize, multiplier, $
		nameid, drawmode

IF(KEYWORD_SET(MARK))THEN BEGIN
  little = BYTARR(multiplier-1, multiplier-1)
  FOR i = 0, multiplier-2 DO BEGIN
    little(i,i) = 255
    little(i,multiplier-i-2) = 255
  END
  WSET, drawwin
  tv, little, multiplier * x + 1, multiplier * y + 1
ENDIF ELSE BEGIN
  little = bytarr(multiplier-1, multiplier-1) + color
  wset, drawwin
  tv, little, multiplier * x + 1, multiplier * y + 1
  wset, showwin
  tv, 255 - bytemap, 6, 6
  tv, bytemap, thexsize + 12, 6
ENDELSE

END ;============= end of xbm_edit pixel draw routine  =============


;------------------------------------------------------------------------------
;	procedure xbm_edit_ev
;------------------------------------------------------------------------------
; This procedure processes the events being sent from the xmanager.
;------------------------------------------------------------------------------

PRO xbm_edit_ev, event

COMMON bitedit, bytemap, drawwin, showwin, thexsize, theysize, multiplier, $
		nameid, drawmode
COMMON eventstuff, lastx, lasty

WIDGET_CONTROL, event.id, GET_UVALUE = eventval		;find the user value
							;of the widget where
							;the event occured
CASE eventval OF

  "XLOADCT": XLoadct, GROUP = event.top

  "XPALETTE": XPalette, GROUP = event.top

  "XMANTOOL": XMTool, GROUP = event.top

  "DRAW": BEGIN
	event.x = 0 > event.x < (thexsize * multiplier - 1)
	event.y = 0 > event.y < (theysize * multiplier - 1)
	xpos = (event.x / multiplier)
	ypos = (event.y / multiplier)
	IF((event.press NE 0)) THEN BEGIN
	  CASE drawmode OF

	    0: IF(bytemap(xpos, ypos) NE 0) THEN BEGIN
		 bytemap(xpos, ypos) = 0
		 draw_pixel, xpos, ypos, 255
		 drawmode = 2
	       ENDIF ELSE BEGIN
		 bytemap(xpos, ypos) = 255
		 draw_pixel, xpos, ypos, 0
		 drawmode = 1
	       ENDELSE

	    3: BEGIN
		 draw_pixel, xpos, ypos, 0, /MARK
		 lastx = xpos
		 lasty = ypos
	       END

	    4: BEGIN
		 draw_pixel, xpos, ypos, 0, /MARK
		 lastx = xpos
		 lasty = ypos
	       END

	    5: BEGIN
		 draw_pixel, xpos, ypos, 0, /MARK
		 lastx = xpos
		 lasty = ypos
	       END
	    ELSE:
	  ENDCASE
	ENDIF ELSE IF((event.press EQ 0) AND (event.release EQ 0)) THEN BEGIN
	  CASE drawmode OF

	    0: BEGIN
		 lastx = -1
		 lasty = -1
	       END

	    1: IF(NOT((xpos EQ lastx) AND (ypos EQ lasty))) THEN BEGIN
		 fill = 0
		 lastx = xpos
		 lasty = ypos
		 bytemap(xpos, ypos) = 255 - fill
		 draw_pixel, xpos, ypos, 0
	       ENDIF

	    2: IF(NOT((xpos EQ lastx) AND (ypos EQ lasty))) THEN BEGIN
		 lastx = xpos
		 lasty = ypos
		 bytemap(xpos, ypos) = 0
		 draw_pixel, xpos, ypos, 255
		ENDIF

	    ELSE:
	  ENDCASE
	ENDIF ELSE IF(event.release EQ 1) THEN BEGIN
	  CASE drawmode OF

	    1: drawmode = 0

	    2: drawmode = 0

	    3: BEGIN
		 smallx = lastx < xpos
		 bigx = lastx > xpos
		 smally = lasty < ypos
		 bigy = lasty > ypos
		 xrad = (bigx - smallx)/2.
		 yrad = (bigy - smally)/2.
		 cy = smally + yrad
		 cx = smallx + xrad
		 samples = xrad * yrad * 4.0
                 IF samples NE 0 THEN BEGIN
                    FOR i = 0, samples DO BEGIN
                        a = (i/samples)*!pi*2
                        bytemap(cx + cos(a)*xrad, cy + sin(a)*yrad) = 255
                    ENDFOR
                 ENDIF ELSE BEGIN
                    a=0
                    bytemap(cx + cos(a)*xrad, cy + sin(a)*yrad) = 255
                 ENDELSE
		 update_display
		 drawmode = 0
	       END

	    4: BEGIN
		 smallx = lastx < xpos
		 bigx = lastx > xpos
		 smally = lasty < ypos
		 bigy = lasty > ypos
		 FOR i = smallx, bigx DO BEGIN
		   bytemap(i,smally) = 255
		   bytemap(i,bigy) = 255
		 ENDFOR
		 FOR i = smally, bigy DO BEGIN
		   bytemap(smallx, i) = 255
		   bytemap(bigx, i) = 255
		 ENDFOR
		 update_display
		 drawmode = 0
	       END

	    5: BEGIN					;Generalized Bresenhams
		 x = lastx				;Line Drawing Algorithm
		 y = lasty
		 deltax = ABS(xpos - lastx)
		 deltay = ABS(ypos - lasty)
		 so = -1 > (xpos - lastx) < 1
		 st = -1 > (ypos - lasty) < 1
		 IF(deltay GT deltax) THEN BEGIN
		   temp = deltax
		   deltax = deltay
		   deltay = temp
		   interchange = 1
		 ENDIF ELSE interchange = 0
		 ebar = 2 * deltay - deltax
		 FOR i = 1, deltax + 1 DO BEGIN
		   bytemap(x,y) = 255
		   WHILE(ebar GE 0) DO BEGIN
		     IF(interchange EQ 1) THEN x = x + so ELSE y = y + st
		     ebar = ebar - 2 * deltax
		   ENDWHILE
		   IF(interchange EQ 1) THEN y = y + st ELSE x = x + so
		   ebar = ebar + 2 * deltay
		 ENDFOR
		 update_display
		 drawmode = 0
	       END
		 
	  ELSE:
	  ENDCASE
	ENDIF
	  END

  "SVBM": BEGIN
	    WIDGET_CONTROL, nameid, GET_VALUE = filename
	    xdim = thexsize / 8
	    IF((thexsize MOD 8) NE 0) THEN xdim = xdim + 1
 	    data = BYTARR(xdim, theysize)
	    FOR y = 0, theysize - 1 DO $
	      FOR x = 0, thexsize - 1 DO BEGIN
		IF((bytemap(x,y) AND 2^(x mod 8)) NE 0) THEN $
	          data(x / 8, theysize-y-1) = data(x/8,theysize-y-1) OR $
						2^(x mod 8)
	      ENDFOR
	    OPENW, unit, filename(0), /GET_LUN
	    WRITEU, unit, data
	    FREE_LUN, unit
	  END

  "RDBM": BEGIN
	    WIDGET_CONTROL, nameid, GET_VALUE = filename
	    OPENR, unit, filename(0), /GET_LUN
	    fsize = FSTAT(unit)
	    dim = SQRT(fsize.size * 8)
	    IF (dim mod 8) NE 0 THEN $
	      data = BYTARR(dim/8 + 1, dim) $
	    ELSE $
	      data = BYTARR(dim/8, dim)
	    READU, unit, data
	    CLOSE, unit
	    bytemap = BYTARR(thexsize, theysize)
	    FOR y = 0, theysize - 1 DO BEGIN
	      FOR x = 0, thexsize - 1 DO BEGIN
		IF((data(x/8, y) AND (2^(7 - (x mod 8)))) NE 0) THEN $
		  bytemap((x/8)*8 + (7-(x mod 8)), theysize-y-1) = 255
	      ENDFOR
	    ENDFOR
	    update_display
	  END

  "SVIA": BEGIN
	    WIDGET_CONTROL, nameid, GET_VALUE = filename
	    xdim = thexsize / 8
	    IF((thexsize MOD 8) NE 0) THEN xdim = xdim + 1
 	    data = BYTARR(xdim, theysize)
	    FOR y = 0, theysize - 1 DO $
	      FOR x = 0, thexsize - 1 DO BEGIN
		IF((bytemap(x,y) AND 2^(x mod 8)) NE 0) THEN $
	          data(x/8, theysize-y-1) = data(x/8,theysize-y-1) OR $
						2^(x mod 8)
	      ENDFOR
	    IF !VERSION.OS NE "MacOS" THEN newline = string(10B) $
	    ELSE newline = string(13B)
	    OPENW, unit, filename(0), /GET_LUN
	    WRITEU, unit, + $
		"							;" + $
		filename(0) + " bitmap" + newline + $
		"							;" + $
		"definition" + newline
	    WRITEU, unit, filename(0) + $
		" = 	[				$"
	    WRITEU, unit, newline
	    FOR y = 0, theysize - 1 DO BEGIN
	      line = string(data(*,y), $
		format = '("		[",(i3.3,"B",:,", "))')
	      WRITEU, unit, line
	      WRITEU, unit, "]"
	      IF(y LT theysize-1) THEN $
		WRITEU, unit ,",			$" + newline $
	      ELSE WRITEU, unit, "			$" + newline
	    ENDFOR
	    WRITEU, unit, "		]"
	    FREE_LUN, unit
	  END

  "erase.bm": BEGIN
		bytemap = bytarr(thexsize, theysize)
		update_display
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "cw.bm": BEGIN
		bytemap = rotate(bytemap, 3)
		update_display
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	   END

  "ccw.bm": BEGIN
		bytemap = rotate(bytemap, 1)
		update_display
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	    END

  "flip.bm":BEGIN
		bytemap = rotate(bytemap, 5)
		update_display
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	    END

  "invert.bm": BEGIN
		 bytemap = 255 - bytemap
		 update_display
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	       END

  "circle.bm": BEGIN
		drawmode = 3
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	       END

  "rect.bm": BEGIN
		drawmode = 4
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	     END

  "line.bm": BEGIN
		drawmode = 5
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	     END

  "FILENAME": 

  "EXIT": WIDGET_CONTROL, event.top, /DESTROY

  ELSE: MESSAGE, "Event User Value Not Found"

ENDCASE

END ;============= end of xbm_edit event handling routine task =============


;------------------------------------------------------------------------------
;	procedure xbm_edit
;------------------------------------------------------------------------------
; This is the main routine, it creates and then registers the widget with the
; XManager.
;------------------------------------------------------------------------------

PRO xbm_edit, XSIZE = XSIZE, YSIZE = YSIZE, GROUP = GROUP, FILENAME = FILENAME

COMMON bitedit, bytemap, drawwin, showwin, thexsize, theysize, multiplier, $
		nameid, drawmode

common eventstuff, lastx, lasty

IF(XRegistered("xbm_edit")) THEN RETURN			;only one instance of
							;the xbm_edit widget
							;is allowed.  If it is
							;already managed, do
							;nothing and return

IF(NOT(KEYWORD_SET(FILENAME)))THEN FILENAME = "idl.bm"
IF(NOT(KEYWORD_SET(XSIZE)))THEN XSIZE = 16
IF(NOT(KEYWORD_SET(YSIZE)))THEN YSIZE = 16

IF(XSIZE LT 17) THEN mult = 14 ELSE mult = 8

bytemap = bytarr(XSIZE,YSIZE)
drawwin = 0
showwin = 0
thexsize = XSIZE
theysize = YSIZE
multiplier = mult
nameid = 0L
drawmode = 0L

xbm_editbase = WIDGET_BASE(TITLE = "xbm_edit", $
		/COLUMN)				;create the main base

XPdMenu, [	'"Done"					EXIT',		$
		'"File"	{',						$
		'"Save BitMap Array File"		SVBM',		$
		'"Save IDL Array Definition File" 	SVIA',		$
		'"Read BitMap Array File"		RDBM',		$
		'}',							$
		'"Tools"	{',					$
			'"XLoadct"			XLOADCT',	$
			'"XPalette"			XPALETTE',	$
			'"XManagerTool"			XMANTOOL',	$
		'}'],							$
	xbm_editbase

xbm_topbase = WIDGET_BASE(xbm_editbase, $
		/ROW)

version = WIDGET_INFO(/version)
IF VERSION.STYLE EQ 'OPEN LOOK' THEN $
  xbm_palette = WIDGET_BASE(xbm_topbase, $
		COLUMN = 2, $
		/FRAME, $
		/EXCLUSIVE) $
ELSE $
  xbm_palette = WIDGET_BASE(xbm_topbase, $
		COLUMN = 2, $
		/FRAME)

controls = [	"erase.bm",		$
		"line.bm",		$
		"rect.bm",		$
		"circle.bm",		$
		"cw.bm",		$
		"ccw.bm",		$
		"flip.bm",		$
		"invert.bm"		$
	]

controlicons = [						$
		;eraser icon
	 	[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 252B, 255B, 255B, 031B],			$
		[000B, 003B, 000B, 000B, 022B],			$
		[192B, 000B, 000B, 128B, 017B],			$
		[224B, 255B, 255B, 127B, 008B],			$
		[032B, 000B, 000B, 064B, 008B],			$
		[032B, 000B, 000B, 064B, 008B],			$
		[016B, 000B, 000B, 032B, 004B],			$
		[016B, 000B, 000B, 032B, 004B],			$
		[016B, 000B, 000B, 032B, 004B],			$
		[008B, 000B, 000B, 016B, 002B],			$
		[008B, 000B, 000B, 016B, 002B],			$
		[008B, 000B, 000B, 016B, 003B],			$
		[004B, 000B, 000B, 200B, 000B],			$
		[004B, 000B, 000B, 056B, 000B],			$
		[252B, 255B, 255B, 015B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;line icon
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 004B, 000B, 000B, 000B],			$
		[000B, 004B, 000B, 000B, 000B],			$
		[000B, 004B, 000B, 000B, 000B],			$
		[000B, 004B, 000B, 000B, 000B],			$
		[000B, 002B, 000B, 000B, 000B],			$
		[000B, 002B, 000B, 000B, 000B],			$
		[000B, 002B, 000B, 000B, 000B],			$
		[000B, 002B, 000B, 000B, 000B],			$
		[000B, 001B, 000B, 000B, 000B],			$
		[000B, 001B, 000B, 000B, 000B],			$
		[000B, 001B, 000B, 192B, 015B],			$
		[000B, 001B, 224B, 063B, 000B],			$
		[128B, 240B, 031B, 000B, 000B],			$
		[128B, 000B, 000B, 000B, 000B],			$
		[128B, 000B, 000B, 000B, 000B],			$
		[128B, 000B, 000B, 000B, 000B],			$
		[064B, 000B, 000B, 000B, 000B],			$
		[064B, 000B, 000B, 000B, 000B],			$
		[064B, 064B, 000B, 000B, 000B],			$
		[064B, 128B, 000B, 000B, 000B],			$
		[032B, 000B, 001B, 000B, 000B],			$
		[032B, 000B, 002B, 000B, 000B],			$
		[032B, 000B, 004B, 000B, 000B],			$
		[032B, 000B, 008B, 000B, 000B],			$
		[016B, 000B, 016B, 000B, 000B],			$
		[016B, 000B, 032B, 000B, 000B],			$
		[016B, 000B, 064B, 000B, 000B],			$
		[016B, 000B, 128B, 000B, 000B],			$
		[008B, 000B, 000B, 001B, 000B],			$
		[008B, 000B, 000B, 002B, 000B],			$
		[008B, 000B, 000B, 004B, 000B],			$
		[008B, 000B, 000B, 008B, 000B],			$
		[000B, 000B, 000B, 016B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;rect icon
	 	[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[224B, 255B, 255B, 031B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 016B, 000B],			$
		[224B, 255B, 255B, 031B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 063B, 000B, 000B, 000B],			$
		[000B, 033B, 000B, 000B, 000B],			$
		[000B, 033B, 000B, 000B, 000B],			$
		[000B, 033B, 000B, 000B, 000B],			$
		[000B, 033B, 000B, 252B, 015B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 004B, 008B],			$
		[000B, 033B, 000B, 252B, 015B],			$
		[000B, 063B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;circle icon
	 	[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 192B, 031B, 000B, 000B],			$
		[000B, 056B, 224B, 000B, 000B],			$
		[000B, 006B, 000B, 003B, 000B],			$
		[000B, 001B, 000B, 004B, 000B],			$
		[128B, 000B, 000B, 008B, 000B],			$
		[064B, 000B, 000B, 016B, 000B],			$
		[032B, 000B, 000B, 032B, 000B],			$
		[032B, 000B, 000B, 032B, 000B],			$
		[016B, 000B, 000B, 064B, 000B],			$
		[016B, 000B, 000B, 064B, 000B],			$
		[016B, 000B, 000B, 064B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[016B, 000B, 000B, 064B, 000B],			$
		[016B, 000B, 000B, 064B, 000B],			$
		[016B, 000B, 000B, 064B, 000B],			$
		[032B, 000B, 000B, 032B, 000B],			$
		[032B, 000B, 000B, 032B, 000B],			$
		[064B, 000B, 000B, 016B, 000B],			$
		[128B, 000B, 000B, 008B, 000B],			$
		[000B, 001B, 000B, 004B, 000B],			$
		[000B, 006B, 000B, 227B, 000B],			$
		[000B, 056B, 224B, 016B, 001B],			$
		[000B, 192B, 031B, 008B, 002B],			$
		[000B, 000B, 000B, 008B, 002B],			$
		[000B, 000B, 000B, 008B, 002B],			$
		[000B, 000B, 000B, 016B, 001B],			$
		[000B, 000B, 000B, 224B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;clockwise rotation icon
	 	[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[192B, 127B, 000B, 000B, 000B],			$
		[064B, 128B, 007B, 000B, 000B],			$
		[064B, 000B, 024B, 000B, 000B],			$
		[064B, 000B, 096B, 000B, 000B],			$
		[064B, 000B, 128B, 001B, 000B],			$
		[064B, 000B, 000B, 002B, 000B],			$
		[064B, 000B, 000B, 004B, 000B],			$
		[064B, 000B, 000B, 008B, 000B],			$
		[064B, 000B, 000B, 016B, 000B],			$
		[192B, 063B, 000B, 016B, 000B],			$
		[000B, 192B, 001B, 032B, 000B],			$
		[000B, 000B, 002B, 032B, 000B],			$
		[000B, 000B, 004B, 064B, 000B],			$
		[000B, 000B, 008B, 064B, 000B],			$
		[000B, 000B, 016B, 064B, 000B],			$
		[000B, 000B, 016B, 064B, 000B],			$
		[000B, 000B, 016B, 128B, 000B],			$
		[000B, 000B, 032B, 128B, 000B],			$
		[000B, 000B, 032B, 128B, 000B],			$
		[000B, 000B, 032B, 128B, 000B],			$
		[000B, 000B, 063B, 128B, 031B],			$
		[000B, 000B, 002B, 000B, 008B],			$
		[000B, 000B, 004B, 000B, 004B],			$
		[000B, 000B, 008B, 000B, 002B],			$
		[000B, 000B, 016B, 000B, 001B],			$
		[000B, 000B, 032B, 128B, 000B],			$
		[000B, 000B, 064B, 064B, 000B],			$
		[000B, 000B, 128B, 032B, 000B],			$
		[000B, 000B, 000B, 017B, 000B],			$
		[000B, 000B, 000B, 010B, 000B],			$
		[000B, 000B, 000B, 004B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;counter clockwise rotation icon
	 	[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 254B, 003B],			$
		[000B, 000B, 224B, 001B, 002B],			$
		[000B, 000B, 024B, 000B, 002B],			$
		[000B, 000B, 006B, 000B, 002B],			$
		[000B, 128B, 001B, 000B, 002B],			$
		[000B, 064B, 000B, 000B, 002B],			$
		[000B, 032B, 000B, 000B, 002B],			$
		[000B, 016B, 000B, 000B, 002B],			$
		[000B, 008B, 000B, 000B, 002B],			$
		[000B, 008B, 000B, 252B, 003B],			$
		[000B, 004B, 128B, 003B, 000B],			$
		[000B, 004B, 064B, 000B, 000B],			$
		[000B, 002B, 032B, 000B, 000B],			$
		[000B, 002B, 016B, 000B, 000B],			$
		[000B, 002B, 008B, 000B, 000B],			$
		[000B, 002B, 008B, 000B, 000B],			$
		[000B, 001B, 008B, 000B, 000B],			$
		[000B, 001B, 004B, 000B, 000B],			$
		[000B, 001B, 004B, 000B, 000B],			$
		[000B, 001B, 004B, 000B, 000B],			$
		[248B, 001B, 252B, 000B, 000B],			$
		[016B, 000B, 064B, 000B, 000B],			$
		[032B, 000B, 032B, 000B, 000B],			$
		[064B, 000B, 016B, 000B, 000B],			$
		[128B, 000B, 008B, 000B, 000B],			$
		[000B, 001B, 004B, 000B, 000B],			$
		[000B, 002B, 002B, 000B, 000B],			$
		[000B, 004B, 001B, 000B, 000B],			$
		[000B, 136B, 000B, 000B, 000B],			$
		[000B, 080B, 000B, 000B, 000B],			$
		[000B, 032B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;flip icon
	 	[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 128B, 000B, 000B],			$
		[000B, 000B, 192B, 000B, 000B],			$
		[000B, 000B, 160B, 000B, 000B],			$
		[000B, 000B, 144B, 000B, 000B],			$
		[000B, 000B, 136B, 000B, 000B],			$
		[000B, 000B, 132B, 000B, 000B],			$
		[000B, 000B, 130B, 000B, 000B],			$
		[000B, 000B, 129B, 000B, 000B],			$
		[000B, 128B, 128B, 000B, 000B],			$
		[000B, 064B, 128B, 000B, 000B],			$
		[000B, 032B, 128B, 000B, 000B],			$
		[000B, 032B, 128B, 000B, 000B],			$
		[000B, 063B, 128B, 255B, 000B],			$
		[224B, 032B, 128B, 000B, 007B],			$
		[024B, 032B, 128B, 000B, 024B],			$
		[004B, 032B, 128B, 002B, 032B],			$
		[028B, 032B, 128B, 006B, 032B],			$
		[228B, 032B, 128B, 010B, 032B],			$
		[004B, 255B, 255B, 019B, 032B],			$
		[004B, 000B, 000B, 032B, 032B],			$
		[004B, 000B, 000B, 192B, 032B],			$
		[004B, 000B, 000B, 128B, 039B],			$
		[004B, 000B, 000B, 000B, 057B],			$
		[004B, 000B, 000B, 000B, 033B],			$
		[008B, 000B, 000B, 128B, 000B],			$
		[048B, 000B, 000B, 064B, 000B],			$
		[192B, 001B, 000B, 032B, 000B],			$
		[000B, 254B, 255B, 019B, 000B],			$
		[000B, 192B, 128B, 010B, 000B],			$
		[000B, 128B, 128B, 006B, 000B],			$
		[000B, 000B, 129B, 002B, 000B],			$
		[000B, 000B, 130B, 000B, 000B],			$
		[000B, 000B, 132B, 000B, 000B],			$
		[000B, 000B, 136B, 000B, 000B],			$
		[000B, 000B, 144B, 000B, 000B],			$
		[000B, 000B, 160B, 000B, 000B],			$
		[000B, 000B, 192B, 000B, 000B],			$
		[000B, 000B, 128B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;invert icon
	 	[						$
		[000B, 000B, 000B, 000B, 128B],			$
		[224B, 127B, 000B, 000B, 192B],			$
		[024B, 128B, 001B, 000B, 224B],			$
		[004B, 000B, 002B, 000B, 240B],			$
		[004B, 000B, 002B, 000B, 248B],			$
		[098B, 096B, 004B, 000B, 252B],			$
		[098B, 096B, 004B, 000B, 254B],			$
		[002B, 000B, 004B, 000B, 255B],			$
		[002B, 000B, 004B, 128B, 255B],			$
		[002B, 006B, 004B, 192B, 255B],			$
		[002B, 006B, 004B, 224B, 255B],			$
		[002B, 000B, 004B, 240B, 255B],			$
		[002B, 000B, 004B, 248B, 255B],			$
		[034B, 064B, 004B, 252B, 255B],			$
		[098B, 096B, 004B, 254B, 255B],			$
		[196B, 057B, 002B, 255B, 255B],			$
		[004B, 015B, 130B, 255B, 255B],			$
		[024B, 128B, 193B, 255B, 255B],			$
		[224B, 127B, 224B, 255B, 255B],			$
		[000B, 000B, 240B, 255B, 255B],			$
		[000B, 000B, 248B, 000B, 248B],			$
		[000B, 000B, 060B, 255B, 231B],			$
		[000B, 000B, 222B, 255B, 223B],			$
		[000B, 000B, 223B, 255B, 223B],			$
		[000B, 128B, 239B, 252B, 185B],			$
		[000B, 192B, 239B, 252B, 185B],			$
		[000B, 224B, 239B, 255B, 191B],			$
		[000B, 240B, 239B, 255B, 191B],			$
		[000B, 248B, 239B, 255B, 191B],			$
		[000B, 252B, 239B, 159B, 191B],			$
		[000B, 254B, 239B, 159B, 191B],			$
		[000B, 255B, 239B, 255B, 191B],			$
		[128B, 255B, 239B, 255B, 191B],			$
		[192B, 255B, 239B, 253B, 187B],			$
		[224B, 255B, 239B, 249B, 185B],			$
		[240B, 255B, 223B, 099B, 220B],			$
		[248B, 255B, 223B, 015B, 223B],			$
		[252B, 255B, 063B, 255B, 231B],			$
		[254B, 255B, 255B, 000B, 248B],			$
		[255B, 255B, 255B, 255B, 255B]			$
		]						$
		]

FOR i = 0,N_ELEMENTS(controls)-1 DO BEGIN
  toss = WIDGET_BUTTON(xbm_palette, $
		VALUE = controlicons(*,*,i), $
		UVALUE = controls(i))
ENDFOR

xbm_draw = WIDGET_DRAW(xbm_topbase, $
		XSIZE = thexsize * multiplier, $
		YSIZE = theysize * multiplier, $
		/BUTTON_EVENTS, $
		/MOTION_EVENTS, $
		RETAIN = 2, $
		UVALUE = "DRAW", $
		/FRAME)

showbase = WIDGET_BASE(xbm_editbase, $
		/ROW)

xbm_show = WIDGET_DRAW(showbase, $
		XSIZE = thexsize * 2 + 18, $
		YSIZE = theysize + 12, $
		RETAIN = 2, $
		/FRAME)

tempbase = WIDGET_BASE(xbm_editbase, $
		/ROW, $
		/FRAME)
label = WIDGET_LABEL(tempbase, $
		VALUE = "Filename:")
nameid = WIDGET_TEXT(tempbase, $
		VALUE = FILENAME, $
		XSIZE = 40, $
		YSIZE = 1, $
		/EDITABLE, $
		UVALUE = "FILENAME")

WIDGET_CONTROL, xbm_editbase, /REALIZE			;create the widgets
							;that is defined

WIDGET_CONTROL, xbm_draw, GET_VALUE = test1		;drawwin
WIDGET_CONTROL, xbm_show, GET_VALUE = test2		;showwin

drawwin = test1
showwin = test2

WSET, showwin
ERASE, 149

update_display

lastx = 0
lasty = 0

XManager, "xbm_edit", xbm_editbase, $			;register the widgets
		EVENT_HANDLER = "xbm_edit_ev", $	;with the XManager
		GROUP_LEADER = GROUP

END ;================ end of xbm_edit background task =====================





