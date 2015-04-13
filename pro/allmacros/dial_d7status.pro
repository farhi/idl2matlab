;-------------------------------------------------------------------------------
;*******************************************************************************

	PRO dial_d7status_macro, D
;
; 	Interactive dynamic display and control of D7
;
;						JRS 22/5/02
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp
	COMMON c_lamp_font	
	COMMON local, nparams

	nparams = 18
	bobfac  = 1.84

; read in the instrument motor/current values
	mall = DIALNEWVALUE(TYPE = 'motors', /SETVALUE)
	ball = DIALNEWVALUE(TYPE = 'currents', /SETVALUE)
	para = DIALNEWVALUE(TYPE = 't_para', /SETVALUE)
	res  = DIALNEWVALUE(TYPE = 't_res', /SETVALUE)

	ball(0:1) = ball(0:1)*bobfac			;Flipper Currents
	param = FLTARR(nparams)
	param(0:4)   = mall(0:4)			;bank + rotation
	param(5)     = mall(5)				;omega
	param(6:10)  = ball(0:4)			;Currents
	param(11:16) = mall(8:13)			;monos
	param(17)    = mall(14)				;polariser angle
	
; Setup parameter value strings
	parstr = STRTRIM(STRING(param),2)
	FOR i = 0, nparams-1 DO BEGIN
		IF (i EQ 6) OR (i EQ 7) THEN BEGIN
			dummy = FLOAT(ROUND(100. * FLOAT(parstr(i))))/100.
			parstr(i) = STRTRIM(STRING(dummy),2)
		ENDIF
		posdot = STRPOS(parstr[i],'.')
		pos00  = STRPOS(STRMID(parstr(i),posdot),'00')
		IF pos00 NE -1 THEN parstr(i) = STRMID(parstr(i),0,pos00+posdot)
		last = STRLEN(parstr(i))
		IF(STRPOS(parstr(i),'.') EQ last - 1)  THEN parstr(i) = parstr(i)+'0'
		IF RSTRPOS(parstr(i),'0') EQ last - 1 THEN $
		IF STRPOS(parstr[i],'.') NE -1 THEN parstr(i) = STRMID(parstr(i),0,last-1)

	ENDFOR

; Read header values
	stat     = DIALNEWVALUE(TYPE = 'status', /SETVALUE)
	stat     = STRTRIM(stat,2)
	status   = 'D7 is ' + stat
	numor    = STRTRIM(STRING(ROUND(res.numor_to_send) + 1),2)
	title    = STRTRIM(STRING(para.C_TXT),2)
	subtitle = STRTRIM(STRING(para.SUB_TITLE),2)
	user     = STRTRIM(STRING(para.C_USER),2)
	lc       = STRTRIM(STRING(para.C_LC),2)
	label = STRARR(4)
	label(0) = title
	label(1) = 'User: ' + user + '  Local Contact: ' + lc
	label(2) = status
	label(3) = 'Run #' + numor + '   ' + subtitle

; initiallise the dial
    	IF  (D.init EQ 0) THEN BEGIN
		D.init=1

; initiallise the text fields
		param_text = STRARR(nparams)
		param_text(0)  = 'Bank 1'
		param_text(1)  = 'Bank 2'
		param_text(2)  = 'Bank 3'
		param_text(3)  = 'Bank 4'
		param_text(4)  = 'Rotation'
		param_text(5)  = 'Omega'
		param_text(6)  = 'Flip.'
		param_text(7)  = 'Corr.'
		param_text(8)  = 'Z coils'
		param_text(9)  = 'X coils'
		param_text(10) = 'Y coils'
		param_text(11) = 'Mono1'
		param_text(12) = 'Mono2'
		param_text(13) = 'Mono3'
		param_text(14) = 'Mono4'
		param_text(15) = 'Mono5'
		param_text(16) = 'Mono6'
		param_text(17) = 'Polariser'

; Set up GUI
		D7S_BASE    = WIDGET_BASE(UNAME = 'D7S_BASE_4', $
			      TITLE = "D7 Status", /COLUMN)
		D7S_BASE_0  = WIDGET_BASE(D7S_BASE, UNAME = 'D7S_BASE_0', $
			      /ROW)
		D7S_BASE_1  = WIDGET_BASE(D7S_BASE_0, UNAME = 'D7S_BASE_1', $
			      /COLUMN)
		D7S_LABEL    = LONARR(4)
		D7S_LABEL(0) = WIDGET_LABEL(D7S_BASE_1, UNAME = 'D7S_LABEL_0', $
			      VALUE = label(0), /ALIGN_CENTER, $
			      SCR_XSIZE=500, FONT = ft_bigger)
		D7S_LABEL(1) = WIDGET_LABEL(D7S_BASE_1, UNAME = 'D7S_LABEL_1', $
			      VALUE = label(1), /ALIGN_CENTER, $
			      SCR_XSIZE=500, FONT = ft_bigger)
		D7S_LABEL(2) = WIDGET_LABEL(D7S_BASE_1, UNAME = 'D7S_LABEL_2', $
			      VALUE = label(2), /ALIGN_CENTER, $
			      SCR_XSIZE=500, FONT = ft_biggest)
		D7S_LABEL(3) = WIDGET_LABEL(D7S_BASE_1, UNAME = 'D7S_LABEL_3', $
			      VALUE = label(3), /ALIGN_CENTER, $
			      SCR_XSIZE=500, FONT = ft_bigger)
		D7S_DRAW_0  = WIDGET_DRAW(D7S_BASE_1, UNAME = 'D7S_DRAW_0', $
			      SCR_XSIZE=500, SCR_YSIZE=560)
		D7S_BASE_2  = WIDGET_BASE(D7S_BASE_0, UNAME = 'D7S_BASE_2', $
			      /COLUMN)
		D7S_BASE_3  = WIDGET_BASE(D7S_BASE_0, UNAME = 'D7S_BASE_3', $
			      /COLUMN)
		D7_BOX = LONARR(nparams)
		FOR i = 0, nparams - 1 DO BEGIN 
  			lname = 'D7S_LABEL_' + STRTRIM(STRING(i+1),2) 
  			dummy = WIDGET_LABEL(D7S_BASE_2, UNAME = lname, $
				XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, $
				/ALIGN_LEFT, VALUE = param_text(i), $
				FONT = ft_normal)
  	  		bname = 'D7S_VALUE_' + STRTRIM(STRING(i+1),2) 
  			D7_BOX(i) = WIDGET_TEXT(D7S_BASE_3, UNAME = bname, $
				FRAME = 0, $ 
     			        XOFFSET = 1, YOFFSET = 0, SCR_XSIZE = 100, $
				SCR_YSIZE = 30, /EDITABLE, VALUE = parstr(i), $
				FONT = ft_propor) 
		ENDFOR
		D7_BUT      = WIDGET_BUTTON(D7S_BASE_3, UNAME = 'D7S_BUT', $
			      /ALIGN_LEFT, FONT = ft_bigger, FRAME = 2, $
			      VALUE = 'Print', UVALUE = 0)

		WIDGET_CONTROL, /REALIZE, D7S_BASE
		WIDGET_CONTROL, D7S_DRAW_0, GET_VALUE = win_id
		D.mywin = win_id
		D.heads = D7S_LABEL
		D.boxes = D7_BOX
		D.but = D7_BUT
		XMANAGER, 'dial_d7status_macro', D7S_BASE, /JUST_REG
    	ENDIF
	WIDGET_CONTROL, D.but, GET_UVALUE = printfl

; update headers
	FOR i = 0, 3 DO WIDGET_CONTROL, D.heads(i), SET_VALUE = label(i)	

; reinspect parameters and update boxes and plot
        IF (TOTAL(D.ang) EQ TOTAL(param)) AND (printfl EQ 0) THEN GOTO, finished
	
	FOR i = 0, nparams - 1 DO WIDGET_CONTROL, D.boxes(i), SET_VALUE = parstr(i)
	ang      = param(0:3)*!pi/180.
	omega    = param(5)*!pi/180.
	rotation = param(4)*!pi/180.
	mono4    = param(10)*!pi/180.
	mono5    = param(11)*!pi/180.
	mono6    = param(12)*!pi/180.

; Setup diagram

	IF printfl EQ 0 THEN WSET, D.mywin
	IF printfl EQ 1 THEN BEGIN
		SET_PLOT, 'PS'
		DEVICE, FILENAME = 'd7_status.ps', /COLOR, YSIZE = 20, XSIZE = 15
	ENDIF
	box1x = [-0.05,-0.05,0.05,0.05,-0.05]		;montior 2 arm
	box1y = [-0.5,-1.5,-1.5,-0.5,-0.5]
	box2x = [-0.3,-0.3,0.3,0.3,-0.3]		;chopper box
	box2y = [0.45,0.55,0.55,0.45,0.45]
	box3x = [-0.1,0.1,0.1,-0.1,-0.1]		;polarizer/flipper box
	box3y = [1.2,1.2,0.8,0.8,1.2]
	flipx = -0.1 + FINDGEN(15)/75.			;flipper coil
	flipy = [0.57,0.77]
	circle = INDGEN(200)*2*!pi/180.			;sample position
	rcirc  = FLTARR(200)+0.1
	PLOT, rcirc,circle, /POLAR, BACKGROUND = 0, COLOR = 10, XMARGIN = [3,3], $
	      YMARGIN = [5,5], XRANGE = [-1.2,1.2], YRANGE = [-1.2,1.6], $
	      XSTY = 2, YSTY = 2, TITLE = 'D7 Status - ' + SYSTIME(0), $
	      SUBTITLE = title+'  '+label(3), TICKLEN = 0.0, $
	      XTICKNAME = [' ',' ',' ',' ',' ',' ',' '], $
	      YTICKNAME = [' ',' ',' ',' ',' ',' ',' ',' '], XTICKS = 6, $
	      YTICKS = 7
	OPLOT, box1x, box1y, COLOR=10
	OPLOT, box2x, box2y, COLOR=10
	FOR i = 0,14 DO OPLOT,[flipx(i),flipx(i)], flipy, COLOR = 60
        OPLOT, box3x, [0.78,0.78,0.56,0.56,0.78], COLOR = 10
	OPLOT, box3x, box3y, COLOR=200	
	seg = !pi*22.5/180.				;half-angle of banks
	r1 = 0.6					;inner bank radius
	r2 = 1.5					;outer bank radius
	r1arc = FLTARR(100) + r1
	r2arc = FLTARR(100) + r2
	arc   = INDGEN(100)*2*seg/100.
	r     = [r1,r2arc,r1arc,r1]
	OPLOT, [0,0.3], [0,0.5*!pi - rotation], /POLAR, COLOR=200
	OPLOT, [0,0.2], [0,0.5*!pi - omega],    /POLAR, COLOR=200
	ang(1) = ang(1) + 8.5*!pi/180.			;bank 2 offset
	ang(2) = ang(2) - 1.*!pi/180.
	FOR i=0, 3 DO BEGIN
		theta=(!pi/2.)-[ang(i)-seg,ang(i)-seg+arc,ang(i)+seg-arc,ang(i)-seg]
		OPLOT,r,theta,/POLAR,COLOR=10
	ENDFOR
	monolen = 0.15
	monoyc = 1.8
	monoxc = [monolen*1.5,0,-monolen*1.5]
	monoang = param(14:16)
	monoxmin = monoxc - monolen*cos(monoang*!pi/180.)
	monoxmax = monoxc + monolen*cos(monoang*!pi/180.)
	monoymin = monoyc - monolen*sin(monoang*!pi/180.)
	monoymax = monoyc + monolen*sin(monoang*!pi/180.)
	FOR i=0,2 DO OPLOT, [monoxmin(i),monoxmax(i)],[monoymin(i),monoymax(i)], $
			    COLOR = 255

; Annotations
	xco = 1.0*sin(ang)
	yco = 1.0*cos(ang)
	!P.FONT = 0
	IF(printfl EQ 0) THEN DEVICE, FONT = ft_smaller
	wc = 255
	FOR i = 0, 3 DO $
	XYOUTS,xco(i), yco(i), 'Bank' + STRTRIM(STRING(i + 1),2)+' = '+ $
		 parstr(i), COLOR=wc, ALIGNMENT = 0.5
	xrot = 0.35*cos(0.5*!pi - rotation)
	yrot = 0.35*sin(0.5*!pi - rotation)
	XYOUTS, xrot, yrot, 'Rot = '+parstr(4), COLOR = wc, ALIGNMENT = 0.5
	xom  = 0.25*cos(0.5*!pi - omega)
	yom  = 0.25*sin(0.5*!pi - omega)
	XYOUTS, xom, yom, 'Omega = '+parstr(5), COLOR = wc, ALIGNMENT = 0.5
	XYOUTS, 0.15, 0.70, 'Flip. = '+parstr(6), COLOR = wc, ALIGNMENT = 0
	XYOUTS, 0.15, 0.60, 'Corr. = '+parstr(7), COLOR = wc, ALIGNMENT = 0
	XYOUTS, 0.15, 1.0, 'Polariser = '+parstr(17), COLOR = wc, ALIGNMENT = 0
	XYOUTS, 0.6, 1.7, 'Mono6 = '+parstr(16), COLOR = wc, ALIGNMENT = 0
	XYOUTS, 0.6, 1.8, 'Mono5 = '+parstr(15), COLOR = wc, ALIGNMENT = 0
	XYOUTS, 0.6, 1.9, 'Mono4 = '+parstr(14), COLOR = wc, ALIGNMENT = 0

; Return to X device and print postscript file
	IF printfl EQ 1 THEN BEGIN
		printfl = 0
		WIDGET_CONTROL, D.but, SET_UVALUE = 0
		DEVICE, /CLOSE
		SET_PLOT, 'X'
		cmd = 'lp -d'+lamp_devps+' d7_status.ps'
		PRINT, cmd
		SPAWN, cmd
	ENDIF

	D.ang=param

finished:	
	END

pro dial_d7status_macro_event, event
;** ************
;**
	COMMON local

	WIDGET_CONTROL,event.id,bad_id=ii
	IF (ii NE 0) THEN BEGIN 
		D.init=0
		DialStop 
		RETURN
	ENDIF

      	wWidget =  Event.top

      	FOR i=1,nparams DO BEGIN
      	name='D7S_VALUE_'+STRTRIM(STRING(i),2)
      	IF (Event.id EQ WIDGET_INFO(wWidget, FIND_BY_UNAME=name)) THEN $
      	    IF (TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH' ) THEN $
      	    	send_command, event,i
      	ENDFOR
	IF (Event.id EQ WIDGET_INFO(wWidget, FIND_BY_UNAME='D7S_BUT')) THEN $
      	    WIDGET_CONTROL, event.id, SET_UVALUE = 1

	

end
pro send_command, event,i
;** ************
;**
	iprint = 0

	Widget_Control, event.id, GET_VALUE=gv
	gv=STRTRIM(gv,2)
	IF(i LE 4) THEN command='bank'+STRTRIM(STRING(i),2)+' '+gv
	IF(i EQ 5) THEN command='rotation'+' '+gv 
	IF(i EQ 6) THEN command='omega'+' '+gv
	IF((i GT 6) AND (i LE 11)) THEN command='b'+STRTRIM(STRING(i-6),2)+' '+gv  
	IF((i GT 11) AND (i LE 17)) THEN command='mono'+STRTRIM(STRING(i-11),2)+' '+gv
	IF(i EQ 18) THEN command='polar'+' '+gv
	C=Dial_mad_send('',0,command)
	IF(iprint GT 0) THEN PRINT,'Sent command: '+command
end
function dial_d7status
;******* ***********
;**

    return, {NAME:'d7status',GENERIC:'mad',TYPE:'log',VALUE:'',FREQUENCY:1,$
	     INIT:0L,MYWIN:0,ANG:FLTARR(18),HEADS:LONARR(4),$
	     BOXES:LONARR(18),BUT:0L}
    end
