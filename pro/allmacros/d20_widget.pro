PRO int_ev,ID
  IF ID.SELECT EQ 1 THEN flag,/int,/nop ELSE flag,/noint,/nop
END

PRO eff_ev,ID
  common d20widget, text,mon
  common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
  IF ID.SELECT EQ 1 THEN BEGIN
    flag,/eff,/nop 
    IF N_ELEMENTS(inf_d20) GT 0 THEN WIDGET_CONTROL,text,SET_VALUE=inf_d20(0),/APPEND
    IF N_ELEMENTS(inf_d20) GT 1 THEN WIDGET_CONTROL,text,SET_VALUE=inf_d20(1),/APPEND
    IF N_ELEMENTS(inf_d20) GT 2 THEN WIDGET_CONTROL,text,SET_VALUE=inf_d20(2),/APPEND
  ENDIF ELSE flag,/noeff,/nop
END

PRO nor_ev,ID
  common d20widget, text,mon
  common d20  , bad_d20 ,flag_d20, wav_d20, psd_d20   
  IF ID.SELECT EQ 1 THEN BEGIN
    str=''
    WIDGET_CONTROL,mon, bad_id=ii, get_value=str
    ;PRINT,str(0)
    flag,nor=FLOAT(str(0)),/nop 
    WIDGET_CONTROL,mon,SET_VALUE=STRING(flag_d20(4))
  ENDIF ELSE flag,/nonor,/nop
END

PRO ang_ev,ID
  common d20widget, text,mon
  common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
  IF ID.SELECT EQ 1 THEN BEGIN
    flag,/ang,/nop 
    IF N_ELEMENTS(inf_d20) GT 0 THEN WIDGET_CONTROL,text,SET_VALUE=inf_d20(0),/APPEND
    IF N_ELEMENTS(inf_d20) GT 1 THEN WIDGET_CONTROL,text,SET_VALUE=inf_d20(1),/APPEND
    IF N_ELEMENTS(inf_d20) GT 2 THEN WIDGET_CONTROL,text,SET_VALUE=inf_d20(2),/APPEND
  ENDIF ELSE flag,/noang,/nop
END

PRO bad_ev,ID
  common d20widget, text,mon
  common d20  , bad_d20 ,flag_d20, wav_d20, psd_d20   
  IF ID.SELECT EQ 1 THEN BEGIN
    flag,/bad,/nop 
    WIDGET_CONTROL,text,SET_VALUE=STRING(N_ELEMENTS(bad_d20))+' bad cells',$
      /APPEND
  ENDIF ELSE flag,/nobad,/nop
END

PRO cal_ev,ID
  calibration
END

PRO mon_ev,ID
  common d20  , bad_d20 ,flag_d20, wav_d20, psd_d20   
  common d20widget, text,mon
  IF flag_d20(4) NE 0 THEN BEGIN
    str=''
    WIDGET_CONTROL,mon, bad_id=ii, get_value=str
    ;PRINT,str(0)
    flag,nor=FLOAT(str(0)),/nop 
  ENDIF
  ;HELP,ID,/STR
END

PRO d20_widget
  common d20widget, text,mon
  common d20  , bad_d20 ,flag_d20, wav_d20, psd_d20   
  common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
  IF XREGISTERED('d20_widget') THEN RETURN
  result=0
  widget=WIDGET_BASE(title='D20 Options Widget',/ROW)
  text=widget_text(widget,ysize=10,xsize=40)
  flag=WIDGET_BASE   (widget,/nonexclusive)
  eff=WIDGET_BUTTON (flag,value='Efficiency Correction' ,uvalue=[-88,379,8,3],EVENT_PRO='eff_ev')
  IF flag_d20(6) EQ 1 THEN WIDGET_CONTROL,eff,/Set_Button
  ang=WIDGET_BUTTON (flag,value='Angle Calibration' ,uvalue=[-88,379,8,3],EVENT_PRO='ang_ev')
  IF flag_d20(2) EQ 1 THEN WIDGET_CONTROL,ang,/Set_Button
  nor=WIDGET_BUTTON (flag,value='Monitor Normalisation' ,uvalue=[-88,379,8,3],EVENT_PRO='nor_ev')
  IF flag_d20(4) GT 0 THEN WIDGET_CONTROL,nor,/Set_Button
  bad=WIDGET_BUTTON (flag,value='Exclusion of bad and zero counting cells' ,uvalue=[-88,379,8,3],$
    EVENT_PRO='bad_ev')
  IF flag_d20(0) EQ 1 THEN WIDGET_CONTROL,bad,/Set_Button
  int=WIDGET_BUTTON (flag,value='Interpolation to equidistant data' ,uvalue=[-88,379,8,3],EVENT_PRO='int_ev')
  IF flag_d20(1) EQ 1 THEN WIDGET_CONTROL,int,/Set_Button
  right=WIDGET_BASE(widget,/COL)
  mon=WIDGET_TEXT(right,value=strtrim(string(flag_d20(4)),2),xsize=20,ysize=1,$
    /EDITABLE,/ALL_EVENTS,$
    EVENT_PRO='mon_ev')
  cal=WIDGET_BUTTON (right,value='Calibration files' ,uvalue=[-88,379,8,3],EVENT_PRO='cal_ev')
  WIDGET_CONTROL,widget,/REALIZE
  IF flag_d20(4) LE 0 THEN WIDGET_CONTROL,mon,SET_VALUE='100000'
  WIDGET_CONTROL,text,SET_VALUE='D20 specific options'
  WIDGET_CONTROL,text,SET_VALUE='All changes will only be applied to next data read in!',/APPEND
  WIDGET_CONTROL,text,SET_VALUE='Click on "DATA Access ..." for accessing instrument data (numors)',/APPEND
  WIDGET_CONTROL,text,SET_VALUE='To change the calibration file manually, call "calibration"',/APPEND
  HELP,inf_d20,/stru 
  XMANAGER,'d20_widget',widget,Event_Handler='LAMP_EVENT_PARSER',/just_reg
END
