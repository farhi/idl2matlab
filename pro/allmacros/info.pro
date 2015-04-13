;*******************************************************************************
;*******************************************************************************
pro info_event,ev
;*******************************************************************************
;*******************************************************************************

widget_control,ev.id,get_uvalue=uva
common tablo,tabb
common nb,n
common erreur2,error4
common bases,pre,t,rep,sav
common sa,sam
common vale,val
common check,chec
common top,changer_based0
common total,ti,m,ti2,tx,txx,time_result_string
;common totaltime,ttime			;to calculate total time

case uva(0) of

1020: begin 
	widget_control,ev.id,get_value=inf 
	tabb[1020]=inf
end

1021:begin widget_control,ev.id,get_value=date0
	tabb[1021]=date0
end

1022:begin widget_control,ev.id,get_value=user0
	tabb[1022]=user0
end

1023:begin widget_control,ev.id,get_value=prop0
	tabb[1023]=prop0
end

1024:begin widget_control,ev.id,get_value=set0
	tabb[1024]=set0
end

1025:begin widget_control,ev.id,get_value=data0
	tabb[1025]=data0
end


1026:begin widget_control,ev.id,get_value=book0
	tabb[1026]=book0
end


1011:begin 
	widget_control,ev.top,/destroy
end

1012:begin 
	widget_control,ev.top,/destroy
	info
end

1013:begin 
;------ OK is the condition that allows to write the file (=1) or not (=0) -----
	okk=1
	for i=1,7 do begin
		widget_control,uva(i),get_value=inf
		tabb[1019+i]=inf
		if i ne 5 then begin
;--------------------------------- FIELD CHECKING ------------------------------
			if strlen(inf[0]) le 0 then begin
				case i of
					1:begin
						temp='title'
					end
					2:begin
						temp='date'
					end
					3:begin
						temp='user'
					end
					4:begin
						temp='prop'
					end
					5:begin
					end
					6:begin
						temp='file name'
					end
					7:begin
						temp='book number'
					end
				end
;--------------------------------- ERROR ---------------------------------------
				err4='*** enter value for '
				widget_control,error4,set_value=string(err4) $
					+string(temp)+' field ***'
;-------------------------------------------------------------------------------
				okk=0
			endif	
		endif
	endfor
;-------------------------------------------------------------------------------
;---------------------------- AFFICHAGE ----------------------------------------
	if okk eq 1 then begin
;		print,'par user ',tabb[1022]
;		print,'par prop ',tabb[1023]
;		print,'par title ',tabb[1020]
;		print,'par date ',tabb[1021]
;		print,'par log ',tabb[1026]
;		print,'!'
;		print,'date'
;		print,'!'
		if strlen(tabb[1024]) ge 1 then begin
			;print,'start setting_',tabb[1024]
		endif
;		print,'!'
;		for d=1,tabb[1081] do begin
	  	  for i=1,n[0] do begin
;		    if tabb[i] eq d then begin
		      if (tabb[i+500] eq 's') or (tabb[i+500] eq 'n') then begin
;			print,'cha '+string(d)
;			print,'cha '+string(tabb[i])
			strsub='par sub '
			if strlen(tabb[1024]) ge 1 then begin
			  strsub=strsub+tabb[1024]+'_'
			endif
;			print,strsub,tabb[i+100]
;			print,'run ',tabb[i+200],' ', $
;			tabb[i+300],' ',tabb[i+400],' ',tabb[i+500]
;			print,'!'
		      endif
;	  	    endif
		  endfor
;		endfor
;------------------------------------------- WRITE THE FILE --------------------
		filesaved=string(format='(A30)','')
		filesaved=tabb[1025]+string('.cmd')	
		close,1
		u=1

		openw,u,filesaved
		printf,u,'par user ', tabb[1022]
		printf,u,'par prop ',tabb[1023]
		printf,u,'par title ',tabb[1020]
		printf,u,'par date ',tabb[1021]
		printf,u,'par prop ',tabb[1023]
		printf,u,'par log ',tabb[1026]
		printf,u,'! '
		printf,u,'date '
		printf,u,'! '
		if strlen(tabb[1024]) ge 1 then begin
		  printf,u,'start setting_',tabb[1024]
		endif
		printf,u,'! '
;		for d=1,tabb[1081] do begin
		  for i=1,n[0] do begin
;		    if tabb[i] eq d then begin
		      if (tabb[i+500] eq 's') or (tabb[i+500] eq 'n') then begin
;			printf,u,'cha '+ string(d)
			printf,u,'cha '+ string(tabb[i])
			strsub='par sub '
			if strlen(tabb[1024]) ge 1 then begin
			  strsub=strsub+tabb[1024]+'_'
			endif
			printf,u,strsub,tabb[i+100]
			printf,u,'run ',tabb[i+200],' ',tabb[i+300],' ',$
			  tabb[i+400],' ',tabb[i+500]
			printf,u,'!'
		      endif
;		    endif
		  endfor
;		endfor 
		close,1
;--------------------------------- INFORMATION ON THE FILE----------------------
			err4='*** The file is called '
			widget_control,error4,set_value=string(err4) $
				+string(filesaved)+string(' ***')
;-------------------------------------------------------------------------------
	endif 
end

1014:begin 
	widget_control,changer_based0,/destroy
	DialClear, 'changer_file'
end

;------- GET VALUE OF DATE, TITLE, USER, PROP, LOG BOOK FROM INSTRUMENT --------
1015:begin
	para =DialNewValue(TYPE='t_para',NAME='changer_file')

	user_n=string(para.c_user[0])
	for i=1,9 do begin
		user_n=user_n+string(para.c_user[i])
	endfor

	prop_n=string(para.proposal_number[0])
	for i=1,7 do begin
		prop_n=prop_n+string(para.proposal_number[i])
	endfor
;	print,para.log_book_n[0], '   ',string(para.log_book_n[0])
;	print,para.log_book_n[1], '   ',string(para.log_book_n[1])

	book_n=string(para.log_book_n[0])+string(para.log_book_n[1])$
		+string(para.log_book_n[2])

	date_n=string(para.exp_start_time[0])
	for i=1,10 do begin
		date_n=date_n+string(para.exp_start_time[i])
	endfor

	title_n=string(para.c_txt[0])
	for i=1,38 do begin
		title_n=title_n+string(para.c_txt[i])
	endfor
	
	tabb[1022]=user_n  
	tabb[1021]=date_n
	tabb[1023]=prop_n
	tabb[1026]=book_n
	tabb[1020]=title_n

	widget_control,uva(1),set_value=tabb[1020];	title
	widget_control,uva(2),set_value=tabb[1021];	date
	widget_control,uva(3),set_value=tabb[1022];	user
	widget_control,uva(4),set_value=tabb[1023];	prop
	widget_control,uva(5),set_value=tabb[1026];	book
end
;-------------------------------------------------------------------------------

1016:begin
	filewritten=string(format='(A30)','')
	filewritten=tabb[1025]+'.cmd'
	; print, "new version 2"
	; print, "filewritten: [",filewritten,"]"
	xdisplayfile,filewritten,group=ev.top,height=30,width=60
end

end
end


;*******************************************************************************
;*******************************************************************************
pro info
;*******************************************************************************
;*******************************************************************************



common tablo,tabb
common nb,n
common erreur2,error4
common vale,val
common check,chec
common top,changer_based0
common total,ti,m,ti2,tx,txx,time_result_string
;common totaltime,ttime			;to calculate total time

base0=widget_base(group_leader=changer_based0,title='Command File (5)',/column)

base01=widget_label(base0,frame='5',value='fill in user and file information')
getinst=widget_button(base0,frame='5',value='get from instrument')
;base03=widget_label(base0,value='*********************')

err4=string(format='(I80)','')
error4=widget_label(base0,value=err4)


base1=widget_base(base0, /row)
base10=widget_base(base1, /column)
base11=widget_base(base10,/column)
base111=widget_base(base10,/row)
base12=widget_base(base1,/column)

base13=widget_base(base0,/column,/align_center)
base14=widget_base(base0,/column)
base15=widget_base(base0,/column)

;-------------------------------- BASE CREATION --------------------------------


user=cw_field(base11,xsize=10,title='user:',/return_events)
prop=cw_field(base11,xsize=8,title='proposal #:',/return_events)
title=cw_field(base11,xsize=32,title='title:',/return_events)
date=cw_field(base11,xsize=11,title='starting date:',/return_events)
set=cw_field(base11,xsize=2,title='setting:',/return_events)
book=cw_field(base11,xsize=2,title='log book #:',/return_events)
data=cw_field(base111,xsize=10,title='file name:',value='',/return_events)
data2=widget_label(base111,value='.cmd')


but33=widget_button(base13,frame=10,xsize=250,value='write command file')
but55=widget_button(base13,xsize=250,value='check this file')
but22=widget_button(base13,xsize=250,value='reset')
but11=widget_button(base13,xsize=250,value='close')
but44=widget_button(base13,xsize=250,value='done')

ttime1=widget_label(base15,value=err4)
;-------------------------------------------------------------------------------
;---------------- TO DISPLAY TIME ----------------------------------------------

	time_display


;-------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------



widget_control,base0,/realize

;------------------------------------- EVENTS ----------------------------------

widget_control,user,set_uvalue=[1022,user]
widget_control,prop,set_uvalue=[1023,prop]
widget_control,title,set_uvalue=[1020,title]
widget_control,date,set_uvalue=[1021,date]
widget_control,set,set_uvalue=[1024,set]
widget_control,book,set_uvalue=[1026,book]
widget_control,data,set_uvalue=[1025,data]

;++++++++++++++++++++++ VALUE READ ON D22 OR IN FILE +++++++++++++++++++++++++++

widget_control,title,set_value=tabb[1020]
widget_control,date,set_value=tabb[1021]
widget_control,user,set_value=tabb[1022]
widget_control,prop,set_value=tabb[1023]
widget_control,book,set_value=tabb[1026]
widget_control,ttime1,set_value=string(time_result_string)


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


widget_control,error4,set_uvalue=[1027,error4]

widget_control,but11,set_uvalue=[1011,but11]
widget_control,but22,set_uvalue=[1012,but22]
widget_control,but33,set_uvalue=[1013,title,date,user,prop,set,data,book]
widget_control,but44,set_uvalue=[1014,but44]
widget_control,getinst,set_uvalue=[1015,title,date,user,prop,book]
widget_control,but55,set_uvalue=[1016,data]

;-------------------------------------------
err4=' '
widget_control,error4,set_value=string(err4)
;--------------------------------------------

;------------ fill the widgets with the value of the saved file ----------------
if chec eq 1 then begin
widget_control,title,set_value=val[0]
widget_control,date,set_value=val[1]
widget_control,user,set_value=val[2]
widget_control,prop,set_value=val[3]
endif

xmanager,'info',base0,/just_reg,group_leader=changer_based0

end
