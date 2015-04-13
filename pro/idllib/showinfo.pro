; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

PRO ShowInfoEventHndlr, event

   WIDGET_CONTROL, GET_UVALUE = state, event.top, /NO_COPY

   CASE event.id OF

      state.wInfoWindow: BEGIN

           ; Get the new size of the window
         WIDGET_CONTROL, state.wInfoWindow, TLB_GET_SIZE=windowSize
         
           ; Determine the change in the window size
         deltaX = windowSize(0) - state.windowSize(0)
         deltaY = windowSize(1) - state.windowSize(1)

           ; Store the new size in the state structure for later comparisons
         state.windowSize = windowSize

           ; Get the pixel size of the text widget
         textEditGeometry = WIDGET_INFO(state.wInfoText, /GEOMETRY)
         
           ; Determine the new size based on the amount the window grew
         newTextEditXSize = textEditGeometry.scr_xsize + deltaX
         newTextEditYSize = textEditGeometry.scr_ysize + deltaY

           ; Resize the text widget accordingly
         WIDGET_CONTROL, state.wInfoText, SCR_XSIZE=newTextEditXSize, $
           SCR_YSIZE=newTextEditYSize

         ENDCASE

      ENDCASE

   WIDGET_CONTROL, SET_UVALUE = state, event.top, /NO_COPY

END


FUNCTION GetInfoText, fileName, WIDTH=width, HEIGHT=height

   OPENR, unit, fileName, /GET_LUN, ERROR=error		;open the file and then

     ; If an error occurred when opening the file
   IF error LT 0 THEN $ 
      buttonPushed = WIDGET_MESSAGE(/ERROR, $
                          [!err_string, ' Can not get information from "' + $
                           fileName + '"']) $
      
   ELSE BEGIN
      maxLines = 1000
      lineInc = 250
      infoText = STRARR(MAXLINES)	;Maximum # of lines
      lineNumber = 0
      lineOfText = '' 
      lineLength  = 0
      longestLength  = 0
   	WHILE NOT EOF(unit) DO BEGIN

   		READF, unit, lineOfText

                lineLength = lineLength > STRLEN(lineOfText)
            
   		infoText(lineNumber) = lineOfText

   		lineNumber = lineNumber + 1

                ;If the maximum number of lines is hit,
                ;increase the array size by lineInc
                IF (lineNumber EQ maxLines) THEN BEGIN
                   infoText = [infoText, STRARR(lineInc)]
                   maxLines = maxLines + lineInc
                ENDIF

   	   ENDWHILE

   	infoText = infoText(0:(lineNumber-1) > 0)

      IF KEYWORD_SET(height) NE 0 THEN $
         height = lineNumber-1
      IF KEYWORD_SET(width) NE 0 THEN $
         width = lineLength

   	FREE_LUN, unit				; Free the file unit.

      RETURN, infoText
      ENDELSE

   RETURN, ""

END


PRO ShowInfo, fileName, TITLE = title, GROUP = group, WIDTH = width, $
		HEIGHT = height, INFOTEXT = infoText, FONT = font, $
                XOFFSET = xOffset, YOFFSET=yOffset, NO_NEWLINE=noNewLine

   IF (NOT(KEYWORD_SET(noNewLine))) THEN $
      noNewLine = 0
   IF (NOT(KEYWORD_SET(infoText))) THEN BEGIN

      IF (NOT(KEYWORD_SET(fileName))) THEN BEGIN
         buttonPushed = WIDGET_MESSAGE(/ERROR, $
            'Filename missing in call to the procedure "ShowInfo"')
         RETURN
         ENDIF

      IF (NOT(KEYWORD_SET(title))) THEN $
         title = fileName

      WIDGET_CONTROL, /HOURGLASS
      
      infoText = GetInfoText(fileName)

      textSize = SIZE(infoText)
      IF (textSize(0) EQ 0) THEN $
         RETURN

      ENDIF $
   ELSE BEGIN
      IF (NOT(KEYWORD_SET(title))) THEN $
         title = "Info"
      ENDELSE

   IF (NOT(KEYWORD_SET(width))) THEN $
      width = 80
   IF (NOT(KEYWORD_SET(height))) THEN $
      height = 30

   IF (KEYWORD_SET(xOffset)) AND (KEYWORD_SET(yOffset)) THEN $
      wInfoWindow = WIDGET_BASE(TITLE=title, UVALUE="INFO_WINDOW", $
        /TLB_SIZE_EVENTS, XOFFSET=xOffset, YOFFSET=yOffset) $
   ELSE $
      wInfoWindow = WIDGET_BASE(TITLE=title, UVALUE="INFO_WINDOW", $
        /TLB_SIZE_EVENTS)
   
   IF N_ELEMENTS(font) GT 0 THEN $
		  ; Create a text widget with the desired font
      wInfoText = WIDGET_TEXT(wInfoWindow, XSIZE=width, YSIZE=height, $
   		           /SCROLL, FONT=font, VALUE=infoText, $
                           NO_NEWLINE=noNewLine, /WRAP) $
   ELSE $
		  ; Create a text widget when no font is specified
      wInfoText = WIDGET_TEXT(wInfoWindow, XSIZE=width, YSIZE=height, $
   		           /SCROLL, VALUE=infoText, NO_NEWLINE=noNewLine,$
                           /WRAP)

   WIDGET_CONTROL, wInfoWindow, /REALIZE			; Display the Window

   WIDGET_CONTROL, wInfoWindow, TLB_GET_SIZE=windowSize

   state = { $
             wInfoWindow : wInfoWindow, $
             wInfoText : wInfoText, $
             windowSize : windowSize $
           }

   WIDGET_CONTROL, wInfoWindow, SET_UVALUE=state

	  ; Register it with the widget manager
   Xmanager, "ShowInfo", wInfoWindow, GROUP_LEADER=group, $
   		EVENT_HANDLER="ShowInfoEventHndlr" 

END  ;--------------------- procedure ShowInfo ----------------------------
