; Programme to calculate and display the intensity profile of neutrons incident
; on a sample trough two slits.

;===============================================================================
;shows error message window
;===============================================================================
PRO errormessage,messy

; common block holding the base's id
COMMON base_id,base

; desensitise base
	WIDGET_CONTROL,base,SENSITIVE=0
;create floating message widget
	msg_base=WIDGET_BASE(group_leader=base,/FLOATING,TITLE='Error Message',/COLUMN)

; label and give widget a button with 'ok' on
	mess=WIDGET_LABEL(msg_base,VALUE=messy)
	errbut=WIDGET_BUTTON(msg_base,VALUE='OK',UVALUE='ok')

; realise and pass control to event handler
	WIDGET_CONTROL,msg_base,/REALIZE
	XMANAGER,'message',msg_base
END

;===============================================================================
;handles events from errormessage
;===============================================================================
PRO message_event,event

; common block for base widget ID
COMMON base_id,base

; get user value from button that caused event
	WIDGET_CONTROL,event.id,GET_UVALUE=ev,GET_VALUE=val

; If Okay button was pressed then resensitise the base widget and destroy
; the message widget
IF ev EQ 'ok' THEN BEGIN
	WIDGET_CONTROL,event.top,/DESTROY
	WIDGET_CONTROL,base,SENSITIVE=1
ENDIF

END

;===============================================================================
; Function to extract a floating point from a text widget specified by
; widget_id. Returns a floating point or an error if incorrectly formatted.
;===============================================================================
FUNCTION getfield, widget_id

; Common block used to pass errors
COMMON eflags,eflag

; initialise: no error
	eflag=0 
	
; get string from text widget and remove white space
	WIDGET_CONTROL,widget_id,GET_VALUE=text
	text=STRCOMPRESS(text[0],/REMOVE_ALL)
; convert to a float
	number = FLOAT(text)
; convert back to string to compare to original string. If they're not the same
; then the string isn't a correctly formatted floating point number so signal error
	checkstr = STRCOMPRESS(STRING(number),/REMOVE_ALL)
; check to see if one is substring of the other (either will do). Can't just check
; that, e.g. checkstr is a substring of text 'coz if text is '1.' then checkstr
; ='1.000000' and vice vera
	IF (STRPOS(checkstr,text) EQ -1) AND  (STRPOS(text,checkstr) EQ -1)THEN BEGIN
		eflag = 1
		RETURN,0.0
; otherwise return the number
	ENDIF ELSE RETURN,number
END

;===============================================================================
; Procedure that does the calculations.
; No comments because I'm not really sure how it works.
;===============================================================================
PRO calculate,d1,d2,s1,s2,sam,theta

COMMON base_id,base
COMMON plot_base,plotbase,illum,delth

; convert theta to rad.
	theta=!pi*theta/180.0
; calc. footprint of sample (projection onto slit)
	footprint=sam*sin(theta)
	
	IF s1 GT s2 THEN fwhm=s1*180.0/(d1*!pi)	ELSE fwhm=s2*180.0/(d1*!pi)
	
	arange=2.0*ATAN((s1+s2)/(2.0*d1))
	
	
	del=fwhm*!pi/(theta*180.0)
	arange=arange*180.0/!pi
	
	IF s1 NE s2 THEN BEGIN
		x=d2-((d1*s2)/(s1-s2))
		w2=ABS((s2*x)/(d2-x))
	ENDIF ELSE BEGIN
		w2=s1
		x=9999.0
	END
	
	w1 = s2+((d2*(s1+s2))/d1)
		
; "Convert to and intensity distribution on sample" (apparently)
	th1=ATAN((s1+s2)/(2.0*d1))
	IF s1 EQ s2 THEN th2 = 0 ELSE th2 = ATAN(s2/(2.0*(d2-x)))
	
	r1=(sam/2.0)+(w2/(2.0*COS(theta)*(TAN(theta)+TAN(th2))))
	r2=(sam/2.0)+(w1/(2.0*COS(theta)*(TAN(theta)+TAN(th1))))
	l1=(sam/2.0)-(w1/(2.0*COS(theta)*(TAN(theta)-TAN(th1))))
	l2=(sam/2.0)-(w2/(2.0*COS(theta)*(TAN(theta)-TAN(th2))))
	
	nt=100
	
	xx=FLTARR(nt+1)
	yy=FLTARR(nt+1)

	FOR i=0,nt DO BEGIN
		xx[i]=(FLOAT(i)/nt)*sam
		y=(FLOAT(i)/nt)*sam
		IF y LT l1 THEN z=0
		IF (y GE l1) AND (y LT l2) THEN z=(y-l1)/(l2-l1)
		IF (y GE l2) AND (y LT r1) THEN z=1
		IF (y GE r1) AND (y LT r2) THEN z=(r2-y)/(r2-r1)
		IF y GE r2 THEN z=0
		yy[i]=z
	END

	dth_by_th = 2*ATAN((s1+s2)/(2.0*d1))/((theta/180.0)*!pi)
	illumination = 100*(r2-l1)/sam
	
	IF illumination GT 100 THEN $ 
	illumination = 'More than 100% ! (sample footprint smaller than beam)' $
	ELSE illumination = STRCOMPRESS(STRING(illumination),/REMOVE_ALL)+'%'
	
; check that plot base exists if it doesn't then create the widget
	WIDGET_CONTROL,plotbase,BAD_ID=check
	IF check NE 0 THEN BEGIN
; Create a drawing widget with the plot in
		plotbase=WIDGET_BASE(GROUP_LEADER=base,TITLE='Intensity across sample (mm)',/COLUMN)
		illum=WIDGET_LABEL(plotbase,/ALIGN_CENTER,/DYNAMIC_RESIZE,VALUE='Illumination = '+ illumination)
		delth=WIDGET_LABEL(plotbase,/ALIGN_CENTER,SCR_YSIZE=35,VALUE='d(theta)/theta = '+STRCOMPRESS(STRING(dth_by_th),/REMOVE_ALL))		
		graph=WIDGET_DRAW(plotbase,XSIZE=700,YSIZE=500,RETAIN=2)

; give it an ok button
		butt=WIDGET_BUTTON(plotbase,VALUE='OK',UVALUE='ok')
		WIDGET_CONTROL,plotbase,/REALIZE
		PLOT,xx,yy,YRANGE=[0,1.1]
	
; handle click of OK button using general message event handler
		XMANAGER,'message',plotbase
; If plotbase does exist then just replot
	ENDIF ELSE BEGIN
		PLOT,xx,yy,YRANGE=[0,1.1]
		WIDGET_CONTROL,illum,SET_VALUE=	'Illumination = '+illumination
		WIDGET_CONTROL,delth,SET_VALUE='d(theta)/theta = '+STRCOMPRESS(STRING(dth_by_th),/REMOVE_ALL)
	END
	
END

;===============================================================================
; Event handler for slitwit
;===============================================================================
PRO slitwit_event, ev

; common block for error flags
COMMON eflags,eflag

; common block containing ID's for text widgets
COMMON editfields, fields


; get user value (text string) to determine whihc button was pressed
	WIDGET_CONTROL, ev.id, GET_UVALUE=button_id

; If quit button was pressed then end
	IF (ev.select) AND (button_id eq 'QUIT') THEN WIDGET_CONTROL, ev.top, /DESTROY

; If do button was pressed perform calcs.
	IF (ev.select) AND (button_id eq 'DO') THEN BEGIN
		
		d1=getfield(fields[0])
		IF eflag NE 0 THEN BEGIN
			errormessage,'Inter Slit distance must be floating point!'
			RETURN
		END
		d2=getfield(fields[1])
		IF eflag NE 0 THEN BEGIN
			errormessage,'Sample-Slit distance must be floating point!'
			RETURN
		END
		sam=getfield(fields[2])
		IF eflag NE 0 THEN BEGIN
			errormessage,'Sample length must be floating point!'
			RETURN
		END
		theta=getfield(fields[3])
		IF eflag NE 0 THEN BEGIN
			errormessage,'Theta must be floating point!'
			RETURN
		END
		s2=getfield(fields[4])
		IF eflag NE 0 THEN BEGIN
			errormessage,'Sample slit width must be floating point!'
			RETURN
		END
		s1=getfield(fields[5])
		IF eflag NE 0 THEN BEGIN
			errormessage,'1st slit width must be floating point!'
			RETURN
		END
		
; If everything checks ok then do calculations
		calculate,d1,d2,s1,s2,sam,theta
	END
END

;===============================================================================
PRO slitwit

COMMON base_id,base

; common block to pass ID's of field widgets
COMMON editfields, fields

COMMON plot_base,plotbase,illum,delth

; Initialise plotbase: widget ID
	plotbase=-1
	
; base widget
	base = WIDGET_BASE(TITLE='SlitWit',/COLUMN)
	
; arrays for the id numbers of the label and field widgets
	labels = LONARR(6)
	fields = LONARR(6)
	
; base containing all of the fields to be enterd
	edit_base = WIDGET_BASE(base,COLUMN=2)
	
; column of labels
	labels[0] = WIDGET_LABEL(edit_base,/ALIGN_RIGHT,SCR_YSIZE=35,VALUE='Inter Slit distance (mm)')
	labels[1] = WIDGET_LABEL(edit_base,/ALIGN_RIGHT,SCR_YSIZE=35,VALUE='Sample-slit distance (mm)')
	labels[2] = WIDGET_LABEL(edit_base,/ALIGN_RIGHT,SCR_YSIZE=35,VALUE='Sample length (mm)')
	labels[3] = WIDGET_LABEL(edit_base,/ALIGN_RIGHT,SCR_YSIZE=35,VALUE='Theta (degrees)')
	labels[4] = WIDGET_LABEL(edit_base,/ALIGN_RIGHT,SCR_YSIZE=35,VALUE='Sample slit	width,s3w (mm)')
	labels[5] = WIDGET_LABEL(edit_base,/ALIGN_RIGHT,SCR_YSIZE=35,VALUE='1st slit width,s2w (mm)')

; column of field entry text widgets
	fields[0] = WIDGET_TEXT(edit_base,SCR_YSIZE=35,/EDITABLE,VALUE='3500.0')
	fields[1] = WIDGET_TEXT(edit_base,SCR_YSIZE=35,/EDITABLE,VALUE='200.0')
	fields[2] = WIDGET_TEXT(edit_base,SCR_YSIZE=35,/EDITABLE,VALUE='100.0')
	fields[3] = WIDGET_TEXT(edit_base,SCR_YSIZE=35,/EDITABLE,VALUE='0.5')
	fields[4] = WIDGET_TEXT(edit_base,SCR_YSIZE=35,/EDITABLE,VALUE='0.5')
	fields[5] = WIDGET_TEXT(edit_base,SCR_YSIZE=35,/EDITABLE,VALUE='0.5')
			
; Do and Quit buttons. UVALUE='...' allows the button to be indetified in the
; event handler without knowing which button the id corresponds to
	quit_button = WIDGET_BUTTON(base, VALUE='QUIT', UVALUE='QUIT')
	do_button = WIDGET_BUTTON(base, VALUE='DO IT!', UVALUE='DO')
	
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'SlitWit', base
END
