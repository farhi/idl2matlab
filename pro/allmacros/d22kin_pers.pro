pro d22kin_pers_event,ev
;** *****************
;**

widget_control,ev.id,get_uvalue=uv
case uv(0) of

;*****TIME CONTROL*****
1:begin widget_control,ev.id,type=1,get_value=times & times=times(0)
	ok=0
	on_ioerror,mistime
	time=float(times)
	ok=1
	mistime: if not ok then widget_control,uv(1),set_value='Error Time' $
			else widget_control,uv(1),set_value=''


  end

;*****DO*****
2:begin t=1

	widget_control,uv(1),get_value=times 
	ok=0
	on_ioerror,mistime1
	time=byte(times)
	ok=1
	mistime1: if not ok then t=0
	widget_control,uv(2),get_value=file & file=file(0)
	if (t eq 0) then widget_control,uv(3),set_value='Error Time' $
		else widget_control,uv(3),set_value=''

if (t eq 1) then begin

	widget_control,uv(1),get_value=time
	tick=90.9e-9
	x=n_elements(time)
	if (time(x-1) eq 0) then x=x-1
if not (x eq 0) then begin
	t  =lonarr(x)
	sec=fltarr(x)
	sum=fltarr(x+1)
	if (x lt 450) then begin
		for i=0,x-1 do begin
		tic=time(i)/tick
		t(i)=tic
		sec(i)=time(i)
		endfor
	sum(0)= sec(0)
		for i=1,x-1 do 	sum(i)=sum(i-1)+sec(i)

	if not (file eq '') then begin
	openw,u,file+'.tmp',/get_lun
	printf,u,'      NB       TICKS      SECONDS          SUM  '
	for i=0,x-1 do begin
	printf,u,i+1,t(i),sec(i),sum(i)
	endfor
	close,u
	free_lun,u
	total='total time :'+strtrim((sum(x-1)),2)
	frame=' frames :'+strtrim((string(x)),2)
	widget_control,uv(4),set_value=total
	widget_control,uv(5),set_value=frame
	xdisplayfile,file+'.tmp',group=ev.top,height=10,width=50
	endif else begin
	openw,u,'kin.tmp',/get_lun
	printf,u,'      NB       TICKS      SECONDS          SUM  '
	for i=0,x-1 do begin
	printf,u,i+1,t(i),sec(i),sum(i)
	endfor
	close,u
	free_lun,u
	total='total time :'+strtrim((sum(x-1)),2)
	frame=' frames :'+strtrim((string(x)),2)
	widget_control,uv(4),set_value=total
	widget_control,uv(5),set_value=frame
	xdisplayfile,'kin.tmp',group=ev.top,height=10,width=50
	endelse
	endif else widget_control,uv(3),set_value='too many frames'
endif
endif
  end

;*****SAVE*****
3:begin	widget_control,uv(1),get_value=time
	widget_control,uv(2),get_value=file & file=file(0)

if not (file eq '') then begin
	tick=90.9e-9
	x=n_elements(time)
	if (time(x-1) eq 0) then x=x-1
if not (x eq 0) then begin
	t  =lonarr(x)
	if (x lt 450) then begin
		for i=0,x-1 do begin
		tic=time(i)/tick
		t(i)=tic
		endfor
	openw,u,file+'.tic',/get_lun
	for i=0,x-1 do begin
	printf,u,t(i)
	endfor
	close,u
	free_lun,u
	endif
	endif
	endif

  end

;*****EXIT*****
4:begin widget_control,ev.top,/destroy

  end

else:

endcase

end

pro d22kin_pers
;** ***********
;**

base   = widget_base  (title='D22 KIN Personal time acquisition',/column,frame=5)
title  = widget_label (base,value='Personal time definiton')

base0 = widget_base   (base ,/column,frame=5)

base1 = widget_base   (base0,/row)
time  = cw_field      (base1,title='Time preset (s) :',value='',xsize=5,ysize=3,uvalue=1)
do0   = widget_button (base1,value='Do',uvalue=2)

base2 = widget_base   (base0,/row)
label = widget_label  (base2,value='Saved in file :')
file  = widget_text   (base2,value='',/editable,xsize=10)
save  = widget_button (base2,value='Save',uvalue=3)

base4 = widget_base  (base,/row)
total = widget_label (base4,value='',/dynamic_resize)
frame = widget_label (base4,value='',/dynamic_resize)

base3 = widget_base   (base ,/row)
exit  = widget_button (base3,value='Exit',uvalue=4)
mesg  = widget_label  (base3,value='',/dynamic_resize)

widget_control,time ,set_uvalue=[1,mesg]
widget_control,do0  ,set_uvalue=[2,time,file,mesg,total,frame]
widget_control,save ,set_uvalue=[3,time,file,mesg]


widget_control,base,/realize
xmanager,'d22kin_pers',base,/just_reg
end
