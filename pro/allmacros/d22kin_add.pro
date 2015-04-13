pro d22kin_add_event,ev
;** ****************
;**

widget_control,ev.id,get_uvalue=uv
case uv(0) of

;*****ADD*****
1:begin widget_control,uv(1),get_value=file1 & file1=file1(0)
	widget_control,uv(2),get_value=file2 & file2=file2(0)
	widget_control,uv(3),get_value=file3 & file3=file3(0)
	widget_control,uv(4),get_value=filen & filen=filen(0)

	if not (file1 eq '') then begin
	x=0
	openr,u,file1+'.tic',/get_lun,error=err
	if (err ne 0) then widget_control,uv(5),set_value=file1+' : no such file' else begin
	widget_control,uv(5),set_value=''
	while not eof(u) do begin
	readf,u,a
	x=x+1
	endwhile
	close,u
	free_lun,u
	t1=lonarr(x)
	openr,u,file1+'.tic',/get_lun
	for i=0,x-1 do begin
	readf,u,a
	t1(i)=a
	endfor
	close,u
	free_lun,u

	if not (file2 eq '') then begin
	x=0
	openr,u,file2+'.tic',/get_lun,error=err
	if (err ne 0) then widget_control,uv(5),set_value=file2+' : no such file' else begin
	widget_control,uv(5),set_value=''
	while not eof(u) do begin
	readf,u,a
	x=x+1
	endwhile
	close,u
	free_lun,u
	t2=lonarr(x)
	openr,u,file2+'.tic',/get_lun
	for i=0,x-1 do begin
	readf,u,a
	t2(i)=a
	endfor
	close,u
	free_lun,u

	if not (file3 eq '') then begin	
	x=0
	openr,u,file3+'.tic',/get_lun,error=err
	if (err ne 0) then widget_control,uv(5),set_value=file3+' : no such file' else begin
	widget_control,uv(5),set_value=''
	while not eof(u) do begin
	readf,u,a
	x=x+1
	endwhile
	close,u
	free_lun,u
	t3=lonarr(x)
	openr,u,file3+'.tic',/get_lun
	for i=0,x-1 do begin
	readf,u,a
	t3(i)=a
	endfor
	close,u
	free_lun,u
	endelse
endif
	endelse
endif
	endelse
endif
	a=n_elements(t1)+n_elements(t2)
	if not (file3 eq '') then a=a+n_elements(t3)
	if a lt 450 then begin
	if not (filen eq '') then begin
	openw,u,filen+'.tic',/get_lun
	for i=0,n_elements(t1)-1 do begin
	printf,u,t1(i)
	endfor
	for i=0,n_elements(t2)-1 do begin
	printf,u,t2(i)
	endfor
	if not (file3 eq '') then begin
	for i=0,n_elements(t3)-1 do begin
	printf,u,t3(i)
	endfor
	endif
	close,u
	free_lun,u
	x=n_elements(t1)+n_elements(t2)
	if not (file3 eq '') then x=x+n_elements(t3)
	tick=90.9e-9
	t=lonarr(x)
	sec=fltarr(x)
	sum=strarr(x)
	openr,u,filen+'.tic',/get_lun
	for i=0,x-1 do begin
	readf,u,c
	t(i)=c
	d=t(i)*tick
	sec(i)=d
	endfor
	close,u
	free_lun,u
	sum(0)=sec(0)
	for i=1,x-1 do sum(i)=sum(i-1)+ sec(i)
	total='total time :'+strtrim((sum(x-1)),2)
	frame='frames :'+strtrim((string(x)),2)	
	widget_control,uv(6),set_value=total
	widget_control,uv(7),set_value=frame
endif
endif

  end
2:begin widget_control,ev.top,/destroy
  end

else:

endcase

end

pro d22kin_add
;** **********
;**

base0 = widget_base   (title='D22 KIN Concatenation of files',/column,frame=5)
title = widget_label  (base0,value='Concatenation of files')

base  = widget_base   (base0,/column,frame=5)

base1 = widget_base   (base ,/row)
label = widget_label  (base1,value='First  file :')
file1 = widget_text   (base1,value='',/editable,xsize=7)
label = widget_label  (base1,value='.tic')

base2 = widget_base   (base ,/row)
label = widget_label  (base2,value='Second file :')
file2 = widget_text   (base2,value='',/editable,xsize=7)
label = widget_label  (base2,value='.tic')

base3 = widget_base   (base ,/row)
label = widget_label  (base3,value='Third  file :')
file3 = widget_text   (base3,value='',/editable,xsize=7)
label = widget_label  (base3,value='.tic')

base5 = widget_base   (base ,/row)
label = widget_label  (base5,value='New file :')
filen = widget_text   (base5,value='',/editable,xsize=7)
label = widget_label  (base5,value='.tic')
add   = widget_button (base5,value='Add',uvalue=1)

base6 = widget_base  (base,/row)
total = widget_label (base6,value='',/dynamic_resize)
frame = widget_label (base6,value='',/dynamic_resize)

base4 = widget_base   (base0,/row)
exit  = widget_button (base4,value='Exit',uvalue=2)
mesg  = widget_label  (base4,value='',/dynamic_resize)

widget_control,add,set_uvalue=[1,file1,file2,file3,filen,mesg,total,frame]

widget_control,base0,/realize
xmanager,'d22kin_add',base0,/just_reg
end
