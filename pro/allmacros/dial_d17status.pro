	PRO dial_d17status_macro, D
;
; Interactive dynamic display of d17 setup
;						JRS 18/5/00
;
	COMMON c_lamp_font	
	COMMON local, nparams

	nparams=24
	bobfac=1.84

; read in the values

	D.type='motors'   & mall=DialNewValue()
;	D.type='currents' & ball=DialNewValue()
;	D.type='t_para'   & para=DialNewValue()
;	D.type='t_res'    & res=DialNewValue()
;	ball(0:1)=ball(0:1)*bobfac
	param=FLTARR(nparams)
	param(0:23)=mall(0:23)
;	param(5)=mall(6)
;	param(6:10)=ball(0:4)

	stat=DialNewValue(type='status',/SETVALUE)
	stat=STRTRIM(stat,2)
	status='d17 is '+stat
;	numor=STRTRIM(STRING(ROUND(res.numor_to_send)),2)
;	title=STRTRIM(STRING(para.C_TXT),2)
;	subtitle=STRTRIM(STRING(para.SUB_TITLE),2)
;	user=STRTRIM(STRING(para.C_USER),2)
;	lc=STRTRIM(STRING(para.C_LC),2)
;	label1=title+'    User: '+user+'  Local Contact: '+lc
;	label2='Run #'+numor+'   '+subtitle

; initiallise the dial

    	IF  (D.init EQ 0) THEN BEGIN

		D.init=1
		command='mall' & C=DialControl(command)

; initiallise the text fields

		param_text=STRARR(nparams)
		param_text(21) ='collimation'
		param_text(16) ='Detector Angle'
		param_text(2) ='Sample Angle'
		param_text(15) ='Detector Distance'
;		param_text(4) ='Omega'
;		param_text(5) ='Rotation'
;		param_text(6) ='Flip.'
;		param_text(7) ='Corr.'
;		param_text(8) ='Z coils'
;		param_text(9) ='X coils'
;		param_text(10)='Y coils'

print,param_text(21),param(21)
print,param_text(16),param(16)
print,param_text(2),param(2)
print,param_text(15),param(15)

; Set up GUI
;		d17S_BASE_4=Widget_Base(UNAME='d17S_BASE_4',TITLE="d17 Status" $
;			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)
;		d17S_BASE_0=Widget_Base(d17S_BASE_4,UNAME='d17S_BASE_0' $
;			,SPACE=3,XPAD=3,YPAD=3,/ROW)
;		d17S_BASE_1=Widget_Base(d17S_BASE_0,UNAME='d17S_BASE_1' $
;			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)
;		d17S_LABEL_0=Widget_Label(d17S_BASE_1,UNAME='d17S_LABEL_0' $
;			,VALUE=label1,/ALIGN_CENTER $
;			,SCR_XSIZE=500,FONT=ft_normal)
;		d17S_LABEL_1=Widget_Label(d17S_BASE_1,UNAME='d17S_LABEL_1' $
;			,VALUE=status,/ALIGN_CENTER $
;			,SCR_XSIZE=500,FONT=ft_biggest)
;		d17S_LABEL_2=Widget_Label(d17S_BASE_1,UNAME='d17S_LABEL_2' $
;			,VALUE=label2,/ALIGN_CENTER $
;			,SCR_XSIZE=500,FONT=ft_normal)
;		d17S_DRAW_0=Widget_Draw(d17S_BASE_1,UNAME='d17S_DRAW_0' $
;			,SCR_XSIZE=500,SCR_YSIZE=500)
;		d17S_BASE_2=Widget_Base(d17S_BASE_0,UNAME='d17S_BASE_2' $
;			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)
;		d17S_BASE_3=Widget_Base(d17S_BASE_0,UNAME='d17S_BASE_3' $
;			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)
;		FOR i=0,nparams-1 DO BEGIN 
;  			name='d17S_LABEL_'+STRTRIM(STRING(i+1),2) 
; 			dummy = Widget_Label(d17S_BASE_2, UNAME=name,XOFFSET=3   $ 
;     	 		,YOFFSET=3, SCR_YSIZE=33,/ALIGN_LEFT ,VALUE=param_text(i) $
;			,FONT=ft_smaller)
;  	  	ENDFOR
;		FOR i=0,nparams-1 DO BEGIN 
;  			name='d17S_VALUE_'+STRTRIM(STRING(i+1),2) 
;  			dummy = Widget_Text(d17S_BASE_3, UNAME=name ,FRAME=1  $ 
;     			,XOFFSET=3 ,YOFFSET=3 ,SCR_XSIZE=100 ,SCR_YSIZE=29 $
;			,/EDITABLE ,VALUE=STRTRIM(STRING(param(i)),2) $
;			,FONT=ft_smaller) 
;  	  	ENDFOR


;		Widget_Control,/REALIZE,d17S_BASE_3
;		Widget_Control,d17S_DRAW_0,GET_VALUE=win_id
;		D.mywin=win_id
;		D.head0=d17S_LABEL_0
;		D.head1=d17S_LABEL_1
;		D.head2=d17S_LABEL_2
;		loadct, 5
;		XMANAGER, 'dial_d17status_macro',d17S_BASE_0,/JUST_REG

    	ENDIF

; update headers

;	Widget_Control,D.head0,SET_VALUE=label1	
;	Widget_Control,D.head1,SET_VALUE=status
;	Widget_Control,D.head2,SET_VALUE=label2

; reinspect parameters

        IF(TOTAL(D.ang) EQ TOTAL(param)) THEN GOTO, finished

	ang=param(0:3)*!pi/180
	omega=param(4)*!pi/180
	rotation=param(5)*!pi/180

; offset on bank2
	ang(1)=ang(1)+0.14

; Setup diagram

	WSET, D.mywin
	box1x=[1.5,1.5,1.3,1.3,1.5]
	box1y=[-0.05,.05,0.05,-0.05,-0.05]
;	box2x=[-0.3,-0.3,0.3,0.3,-0.3]
;	box2y=[0.45,0.55,0.55,0.45,0.45]
;	box3x=[-0.1,0.1,0.1,-0.1,-0.1]
;	box3y=[1.2,1.2,0.8,0.8,1.2]
;	flipx=-0.1+FINDGEN(15)/75.
;	flipy=[0.57,0.77]
	circle=INDGEN(200)*2*!pi/180.
	rcirc=FLTARR(200)+0.1
	PLOT,rcirc,circle,/POLAR,BACKGROUND=255,COLOR=100,XMARGIN=[3,3] $
		,YMARGIN=[1,1],XRANGE=[-1.5,1.5],YRANGE=[-1.5,1.5],XSTY=4,YSTY=4
	OPLOT,box1x,box1y,COLOR=1
;	OPLOT,box2x,box2y,COLOR=1

	r1=0.6
	r2=1.5
	r1arc=FLTARR(100)+r1
	r2arc=FLTARR(100)+r2
;	arc=INDGEN(100)*2*seg/100.
	r=[10,0,0,0]
        ppmm=1.5/3400.
        OPLOT,[-1.5,1.5],[0,0],/POLAR,COLOR=0
;	OPLOT,[-1.3,0],[param(21)*!pi/180.,param(21)*!pi/180.],/POLAR,COLOR=18
        dan=(param(16)+180)*!pi/180.
        san=(param(2)*2+180)*!pi/180.
        det=param(15)*ppmm
        
        OPLOT,[0,det],[dan,dan],/POLAR,COLOR=20
        OPLOT,[0,det],[san,san],/POLAR,COLOR=25
        xdet=211.
        xo=param(15)*tan(param(16)*!pi/180.)
        xr=param(15)*tan(2*param(2)*!pi/180.)

        if xo lt xdet/2. and xo gt -xdet/2. then begin
           OPLOT,[sqrt(det^2+(xdet*ppmm/2)^2),sqrt(det^2+(xdet*ppmm/2)^2)],[dan+atan(xdet*ppmm/(2*det)),dan-atan(xdet*ppmm/(2*det))],/POLAR,COLOR=20
           db=1
        endif else begin
           OPLOT,[sqrt(det^2+(xdet*ppmm/2)^2),sqrt(det^2+(xdet*ppmm/2)^2)],[dan+atan(xdet*ppmm/(2*det)),dan-atan(xdet*ppmm/(det*2))],/POLAR,COLOR=21
           db=0
        endelse

        if san lt dan+atan(xdet*ppmm/(2*det)) and san gt dan-atan(xdet*ppmm/(2*det)) then begin
           rb=1
        endif else begin
           rb=0
        endelse







        detl=1.6
        detd=0.5
        
        

; Annotations
	

	!P.FONT=0

	DEVICE,FONT=ft_smaller

	XYOUTS,0.15,0.70,'Collimation angle: '+STRTRIM(STRING(param(21)),2)+' deg.',COLOR=0,ALIGNMENT=0
        XYOUTS,0.15,0.80,'Sample angle:       '+STRTRIM(STRING(param(2)),2)+' deg.',COLOR=0 ,ALIGNMENT=0
        XYOUTS,0.15,0.90,'Detector angle:     '+STRTRIM(STRING(param(16)),2)+' deg.',COLOR=0 ,ALIGNMENT=0
        XYOUTS,0.15,1.00,'Detector distance: '+STRTRIM(STRING(param(15)),2)+' mm',COLOR=0 ,ALIGNMENT=0

        if db eq 1 then begin
          XYOUTS,0.15,1.10,'Direct beam is ON the detector',COLOR=1 ,ALIGNMENT=0
        endif else begin
          XYOUTS,0.15,1.10,'Direct beam is OFF the detector ',COLOR=0 ,ALIGNMENT=0 
        endelse

        if rb eq 1 then begin
          XYOUTS,0.15,1.20,'Reflected beam is ON the detector',COLOR=1 ,ALIGNMENT=0
        endif else begin
          XYOUTS,0.15,1.20,'Reflected beam is OFF the detector ',COLOR=0 ,ALIGNMENT=0 
        endelse


	D.ang=param
finished:	
	END

;pro dial_d17status_macro_event, event
;** ************
;**
;	COMMON local
;
;	WIDGET_CONTROL,event.id,bad_id=ii
;	IF (ii NE 0) THEN BEGIN 
;		D.init=0
;		DialStop 
;		RETURN
;	ENDIF
;
;      	wWidget =  Event.top
;
;     	FOR i=1,nparams DO BEGIN
;      	name='d17S_VALUE_'+STRTRIM(STRING(i),2)
;      	IF (Event.id EQ WIDGET_INFO(wWidget, FIND_BY_UNAME=name)) THEN $
;      	    IF (TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH' ) THEN $
;      	    	send_command, event,i
;      	ENDFOR
;	
;
;end
;pro send_command, event,i
;** ************
;**
;	Widget_Control, event.id, GET_VALUE=gv
;	gv=STRTRIM(gv,2)
;	IF(i LE 4) THEN command='bank'+STRTRIM(STRING(i),2)+' '+gv
;	IF(i EQ 5) THEN command='omega'+' '+gv 
;	IF(i EQ 6) THEN command='rotation'+' '+gv
;	IF((i GT 6) AND (i LE 11)) THEN command='b'+STRTRIM(STRING(i-6),2)+' '+gv  
;	print, command
;	;C=DialControl(command)
;	C=Dial_mad_send('',0,command)
;end
function dial_d17status
;******* ***********
;**

    return, {NAME:'d17status',GENERIC:'mad',TYPE:'log',VALUE:'',FREQUENCY:1,$
	     INIT:0L,MYWIN:0,ANG:FLTARR(24),HEAD1:0L,HEAD0:0L,$
	     HEAD2:0L}
    end
