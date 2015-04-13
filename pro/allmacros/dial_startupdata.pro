;**************************
;TO RUN THIS DIAL START IDL, THEN ENTER:  
;Idl>.run dial_startupdata
;Idl> startupdata    
;**************************



pro test_empty, inf, i, ok
;******* *****************
;**
common startup_error, error4

if strlen(strtrim(inf[0],2)) le 0 then begin
  case i of
    1:begin
      temp='user name'
    end
    2:begin
      temp='proposal number'
    end
    3:begin
      temp='experiment title'
    end
    4:begin
      temp='starting date'
    end
    5:begin
      temp='logbook number'
    end
    6:begin
      temp='parameter description control'
    end
    7:begin
      temp='sample title'
    end
  end
;--------------------------------- ERROR ---------------------------------------
  ok=ok+1
  widget_control,error4,set_value='*** enter value for '$
  	+string(temp)+' field ***'
;-------------------------------------------------------------------------------
endif

return
end


pro startupdata
;** *********
;Used to run with "byGeorge"
FORWARD_FUNCTION DialControl, DialNewValue, DialOn
if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

dial_bygeorge,'startupdata'	;<-------- Name of your Dial !!!
END                      	;          ~~~~~~~~~~~~~~~~~ !!!



;*******************************************************************************
;*******************************************************************************
pro startupdata_event,ev
;*******************************************************************************
;*******************************************************************************

common startup_base, base0
common startup_error, error4

widget_control,ev.id,get_uvalue=uva

case uva(0) of

2001:	begin 
	widget_control,ev.id,get_value=user0 
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, user0, 1, ok
	;tabb[1]=user0
end

2002:	begin widget_control,ev.id,get_value=prop0
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, prop0, 2, ok
	;tabb[2]=prop0
end

2003:	begin widget_control,ev.id,get_value=title0
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, title0, 3, ok
	;tabb[3]=title0
end

2004:	begin widget_control,ev.id,get_value=date0
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, date0, 4, ok
	;tabb[4]=date0
end

2005:	begin widget_control,ev.id,get_value=book0
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, book0, 5, ok
	;tabb[5]=book0
end

2006:	begin widget_control,ev.id,get_value=pardes0
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, pardes0, 6, ok
	;tabb[6]=pardes0
end

2007:	begin widget_control,ev.id,get_value=sample0
	widget_control,uva(2),set_value=''
	ok=0
	test_empty, sample0, 7, ok
	;tabb[7]=sample0
end

2011:	begin 
;------ OKK1 is the condition that allows to write to MAD (0) or not (>0) ----
	okk1=0
	widget_control,uva(8),set_value=''
	for i=1,7 do begin
	  widget_control,uva(i),get_value=inf
	  test_empty, inf, i, okk1
	  ;print,"ok: ",okk1
	endfor
	V1 =DialNewValue(TYPE='t_nother',NAME='startupdata')
	activity=fix(V1.cstate)
	print,'Current status is ',activity
	if activity eq 0 then begin
	  widget_control,uva(1),get_value=user0 
	  widget_control,uva(2),get_value=prop0
	  widget_control,uva(3),get_value=title0
	  widget_control,uva(4),get_value=date0
	  widget_control,uva(5),get_value=book0
	  widget_control,uva(6),get_value=pardes0
	  widget_control,uva(7),get_value=sample0
	  if okk1 eq 0 then begin
	    arg=strtrim(user0,2)
	    R =DialControl ('par user '+arg,NAME='startupdata')	
	    arg=strtrim(prop0,2)
	    R =DialControl ('par prop '+arg,NAME='startupdata')	
	    arg=strtrim(title0,2)
	    R =DialControl ('par title '+arg,NAME='startupdata')	
	    arg=strtrim(date0,2)
	    R =DialControl ('par date '+arg,NAME='startupdata')	
	    arg=strtrim(book0,2)
	    R =DialControl ('par log '+arg,NAME='startupdata')	
	    arg=strtrim(pardes0,2)
	    R =DialControl ('par pardes '+arg,NAME='startupdata')	
	    arg=strtrim(sample0,2)
	    R =DialControl ('par sub '+arg,NAME='startupdata')
	    widget_control,uva(8),$
	    	set_value='*** Values have been sent to MAD ***'
	  endif
	endif else begin
	  widget_control,uva(8),set_value='*** instrument isn''t idle ***'
	endelse
end

;------- GET VALUE OF DATE, TITLE, USER, PROP, LOG BOOK FROM INSTRUMENT --------
2012:	begin
	para =DialNewValue(TYPE='t_para',NAME='startupdata')

	user_n=string(para.c_user[0])
	for i=1,9 do begin
		user_n=user_n+string(para.c_user[i])
	endfor

	prop_n=string(para.proposal_number[0])
	for i=1,7 do begin
		prop_n=prop_n+string(para.proposal_number[i])
	endfor

	title_n=string(para.c_txt[0])
	for i=1,31 do begin
		title_n=title_n+string(para.c_txt[i])
	endfor

	date_n=string(para.exp_start_time[0])
	for i=1,10 do begin
		date_n=date_n+string(para.exp_start_time[i])
	endfor

	book_n=string(para.log_book_n[0])+string(para.log_book_n[1])$
		+string(para.log_book_n[2])

	sample_n=string(para.sub_title[0])
	for i=1,19 do begin
		sample_n=sample_n+string(para.sub_title[i])
	endfor
	
	pardes_n=string(format='(I1)',para.pardesc)
	if pardes_n eq '1' then pardes_n='on'
	if pardes_n eq '0' then pardes_n='off'

	widget_control,uva(1),set_value=user_n;		user
	widget_control,uva(2),set_value=prop_n;		prop
	widget_control,uva(3),set_value=title_n;	title
	widget_control,uva(4),set_value=date_n;		date
	widget_control,uva(5),set_value=book_n;		book
	widget_control,uva(6),set_value=pardes_n;	pardes
	widget_control,uva(7),set_value=sample_n;	sample
	widget_control,uva(8),set_value=' ';		error field
end
;-------------------------------------------------------------------------------

2013:	begin 
	widget_control,base0,/destroy
	DialClear, 'startupdata'
end

end
end


;*******************************************************************************
;*******************************************************************************
pro startupdata_gui
;** ****************
;**

common startup_base, base0
common startup_error, error4

if xregistered('startupdata_gui') eq 0 then begin

	base0  =widget_base(group_leader=changer_based0,$
		title='Experiment Info',/column)

;	base01 =widget_label(base0,frame='5',$
;		value='fill in user information')
	err4   =string(format='(I80)','')
	error4 =widget_label(base0,value=err4)

	base1  =widget_base(base0, /row)
	base10 =widget_base(base1, /column)
	base11 =widget_base(base10,/column)
	base111=widget_base(base10,/row)
	base12 =widget_base(base1,/column)

	base13 =widget_base(base0,/column,/align_center)
	base14 =widget_base(base0,/column)
	base15 =widget_base(base0,/column)

;-------------------------------- BASE CREATION --------------------------------
	user=  cw_field(base11,xsize=10,title='user (10 char)',/return_events)
	prop=  cw_field(base11,xsize=8,title='proposal #:    ',/return_events)
	title= cw_field(base11,xsize=32,title='title (32 char):',/return_events)
	date=  cw_field(base11,xsize=11,title='starting date (dd-mmm-yyyy):',$
		/return_events)
	book=  cw_field(base11,xsize=2,title='log book #:    ',/return_events)
	pardes=cw_field(base11,xsize=3,title='Parameter description {on/off):',$
		/return_events)
	sample=cw_field(base11,xsize=20,title='sample (20 char):',$
		/return_events)

	but01=widget_button(base13,frame=10,xsize=250,value='send to MAD')
	but02=widget_button(base13,xsize=250,value='reset to MAD values')
	but03=widget_button(base13,xsize=250,value='done')

	widget_control,base0,/realize

;------------------------------------- EVENTS ----------------------------------

	widget_control,user,  set_uvalue=[2001,user,error4]
	widget_control,prop,  set_uvalue=[2002,prop,error4]
	widget_control,title, set_uvalue=[2003,title,error4]
	widget_control,date,  set_uvalue=[2004,date,error4]
	widget_control,book,  set_uvalue=[2005,book,error4]
	widget_control,pardes,set_uvalue=[2006,pardes,error4]
	widget_control,sample,set_uvalue=[2007,sample,error4]

	widget_control,but01, set_uvalue=[2011,user,prop,title,date,book,$
		pardes,sample,error4]
	widget_control,but02, set_uvalue=[2012,user,prop,title,date,book,$
		pardes,sample,error4]
	widget_control,but03, set_uvalue=[2013]

	widget_control,error4,set_uvalue=[2014,error4]

;------------ fill the widgets with the values from MAD ----------------
	err4=' '
	widget_control,error4,set_value=string(err4)

	para =DialNewValue(TYPE='t_para',NAME='startupdata')
	;help,para,/struc

	user_n=string(para.c_user[0])
	for i=1,9 do begin
		user_n=user_n+string(para.c_user[i])
	endfor

	prop_n=string(para.proposal_number[0])
	for i=1,7 do begin
		prop_n=prop_n+string(para.proposal_number[i])
	endfor

	title_n=string(para.c_txt[0])
	for i=1,38 do begin
		title_n=title_n+string(para.c_txt[i])
	endfor

	date_n=string(para.exp_start_time[0])
	for i=1,10 do begin
		date_n=date_n+string(para.exp_start_time[i])
	endfor

	book_n=string(para.log_book_n[0])+string(para.log_book_n[1])$
		+string(para.log_book_n[2])

	sample_n=string(para.sub_title[0])
	for i=1,19 do begin
		sample_n=sample_n+string(para.sub_title[i])
	endfor
	
	pardes_n=string(format='(I1)',para.pardesc)
	if pardes_n eq '1' then pardes_n='on'
	if pardes_n eq '0' then pardes_n='off'

	widget_control,user,   set_value=user_n;	user
	widget_control,prop,   set_value=prop_n;	prop
	widget_control,title,  set_value=title_n;	title
	widget_control,date,   set_value=date_n;	date
	widget_control,book,   set_value=book_n;	book
	widget_control,pardes, set_value=pardes_n;	pardes
	widget_control,sample, set_value=sample_n;	sample

	xmanager,'startupdata',base0,/just_reg,group_leader=changer_based0
endif
end

pro dial_startupdata_macro, D
;** *************************
;**

if D.init eq 0 then begin
	D.init=1
	D.frequency=0.
	startupdata_gui

endif else print,'ok'

end

function dial_startupdata
;******* *****************
;**

return, {init:0}
end
