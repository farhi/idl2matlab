pro d22kin_read_event,ev
;** *****************
;**

widget_control,ev.id,get_uvalue=uv
case uv(0) of

;*****READ*****
1:begin widget_control,uv(1),get_value=file & file=file(0)
	if not (file eq '') then begin
	x=0
	tick=90.9e-9
	openr,u,file+'.tic',/get_lun,error=err
	if (err ne 0) then widget_control,uv(2),set_value=file+' : no such file' else begin
	widget_control,uv(2),set_value=''
	while not eof(u) do begin
	readf,u,a
	x=x+1
	endwhile
	close,u
	free_lun,u
	t=lonarr(x)
	sec=fltarr(x)
	sum=strarr(x)
	openr,u,file+'.tic',/get_lun
	for i=0,x-1 do begin
	readf,u,a
	t(i)=a
	b=t(i)*tick
	sec(i)=b
	endfor
	close,u
	free_lun,u
	sum(0)=sec(0)
	for i=1,x-1 do sum(i)=sum(i-1)+ sec(i)
	openw,u,file+'.tmp',/get_lun
	printf,u,'      NB       TICKS     SECONDS          SUM  '
	for i=0,x-1 do begin
	printf,u,i+1,t(i),sec(i),sum(i)
	endfor
	close,u
	free_lun,u
	total='total time :'+strtrim((sum(x-1)),2)
	frame=' frames :'+strtrim((string(x)),2)
	widget_control,uv(3),set_value=total
	widget_control,uv(4),set_value=frame
	xdisplayfile,file+'.tmp',group=ev.top,height=10,width=50
	endelse
	endif
  end

;*****EXIT*****
2:begin widget_control,ev.top,/destroy
  end

else:

endcase

end

pro d22kin_read
;** ***********
;**

base  = widget_base  (title='D22KIN Reading file',/column)
base1 = widget_base  (base,/row,frame=5)
label = widget_label (base1,value='Read KIN file :')
file  = widget_text  (base1,value='',/editable,xsize=10)
label = widget_label (base1,value='.tic')
read  = widget_button(base1,value='Read',uvalue=1)

base3 = widget_base  (base,/row)
total = widget_label (base3,value='',/dynamic_resize)
frame = widget_label (base3,value='',/dynamic_resize)

base2 = widget_base   (base ,/row)
exit  = widget_button (base2,value='Exit',uvalue=2)
mesg  = widget_label  (base2,value='',/dynamic_resize)

widget_control,read,set_uvalue=[1,file,mesg,total,frame]

widget_control,base,/realize

xmanager,'d22kin_read',base,/just_reg

end
