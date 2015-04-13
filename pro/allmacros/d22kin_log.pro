pro d22kin_log_event,ev
;** ****************
;**

widget_control,ev.id,get_uvalue=uv
case uv(0) of

;*****FIRST TIME CONTROL*****
1:begin widget_control,ev.id,get_value=firsts & firsts=firsts(0)
	ok=0
	on_ioerror,misfirst
	first=float(firsts)
	ok=1
	misfirst: if not ok then widget_control,uv(1),set_value='Error First time'$
			else widget_control,uv(1),set_value=''
  end

;*****INCREMENT CONTROL*****
2:begin widget_control,ev.id,get_value=incres & incres=incres(0)
	ok=0
	on_ioerror,misincre
	incre=float(incres)
	ok=1
	misincre: if not ok then widget_control,uv(1),set_value='Error Increment'$
			else widget_control,uv(1),set_value=''
  end

;*****TIME CONTROL******
3:begin widget_control,ev.id,get_value=times & times=times(0)
	ok=0
	on_ioerror,mistime
	time=float(times)
	ok=1
	mistime: if not ok then widget_control,uv(1),set_value='Error Time' $
			else widget_control,uv(1),set_value=''
  end

;*****DO*****
4:begin t0=1 & t1=1 & t2=1 & t3=1
	e =0 & e1=0 & e2=0 & e3=0	

	widget_control,uv(1),get_value=firsts & firsts=firsts(0)
	ok=0								
	on_ioerror,misfirst1
	first=float(firsts)
	ok=1
	misfirst1: if not ok then t0=0

	widget_control,uv(2),get_value=incres & incres=incres(0)
	ok=0								
	on_ioerror,misincre1
	incre=float(incres)
	ok=1
	misincre1: if not ok then t1=0

	widget_control,uv(3),get_value=times & times=times(0)
	ok=0								
	on_ioerror,mistime1
	time=float(times)
	ok=1
	mistime1: if not ok then t2=0

	widget_control,uv(4),get_value=file & file=file(0)

	if ((first ne 0) and (incre ne 0) and (time ne 0)) then begin

	if t0 eq 0 then widget_control,uv(5),set_value='error first time'$
		else begin
			if first gt 390 then begin
			widget_control,uv(5),set_value='first time less than 390s'
			e=1
			endif else begin
	if t1 eq 0 then widget_control,uv(5),set_value='error increment'$
		else begin
			if first gt 390 then begin
			widget_control,uv(5),set_value='incrment less than 390s'
			e1=1
			endif else begin
	if t2 eq 0 then begin
		widget_control,uv(5),set_value='error total time'
		e2=1
		endif else widget_control,uv(5),set_value=''
endelse
endelse
endelse
endelse

if ((t0 eq 1) and (t1 eq 1) and (t2 eq 1)) then begin
if ((e  eq 0) and (e1 eq 0) and (e2 eq 0)) then begin 

	tick=90.9e-9
	a=first
	b=first
	x=0
	while ((b le time) and (x le 450)) do begin
		a=a*incre
		x=x+1
		b=b+a
	endwhile
print,x
	if x  gt 450 then begin
		widget_control,uv(5),set_value='too many frames'
		t3=0
		endif else widget_control,uv(5),set_value=''		

if (t3 eq 1)  then begin

	tic=lonarr(x)
	sec=strarr(x)
	sum=strarr(x)
	a=first
	tic(0)=a/tick
	sec(0)=a
	sum(0)=first
	for i=1,x-1 do begin
		a=a*incre
		sum(i)=a+sum(i-1)
		t=a/tick
		tic(i)=t
		sec(i)=a
	endfor
	for i=0,x-1 do begin
		if (sec(i) gt 390) then begin
		widget_control,uv(5),set_value='time must less than 390s'
		e3=1
		endif else widget_control,uv(5),set_value=''
	endfor
if (e3 eq 0)  then begin

	if not (file eq '') then begin
	openw,u,file+'.tmp',/get_lun
	printf,u,'      NB       TICKS      SECONDS         SUM  '
	for i=0,x-1 do begin
	printf,u,i+1,tic(i),sec(i),sum(i)
	endfor
	close,u
	free_lun,u
	total='total time :'+strtrim((sum(x-1)),2)
	frame=' frames :'+strtrim((string(x)),2)
	widget_control,uv(6),set_value=total
	widget_control,uv(7),set_value=frame
	xdisplayfile,file+'.tmp',group=ev.top,height=10,width=50
	endif else begin
	openw,u,'kin.tmp',/get_lun
	printf,u,'      NB       TICKS      SECONDS         SUM  '
	for i=0,x-1 do begin
	printf,u,i+1,tic(i),sec(i),sum(i)
	endfor
	close,u
	free_lun,u
	total='total time :'+strtrim((sum(x-1)),2)
	frame=' frames :'+strtrim((string(x)),2)
	widget_control,uv(6),set_value=total
	widget_control,uv(7),set_value=frame
	xdisplayfile,'kin.tmp',group=ev.top,height=10,width=50
	endelse

endif
endif
endif
endif
endif
  end

;*****SAVE*****
5:begin widget_control,uv(1),get_value=firsts & firsts=firsts(0)
	first=float(firsts)
	widget_control,uv(2),get_value=incres & incres=incres(0)
	incre=float(incres)
	widget_control,uv(3),get_value=times  & times =times (0)
	time=float(times)
	widget_control,uv(4),get_value=file   & file  =file  (0)
	if ((first ne 0) and (incre ne 0) and (time ne 0)) then begin
	tick=90.9e-9
	a=first
	b=first
	x=0
	while (b le time) do begin
		a=a*incre
		x=x+1
		b=b+a
	endwhile
	tic=lonarr(x)
	a=first
	tic(0)=a/tick
	for i=1,x-1 do begin
		a=a*incre
		t=a/tick
		tic(i)=t
	endfor
	openw,u,file+'.tic',/get_lun
	for i=0,x-1 do begin
	printf,u,tic(i)
	endfor
	close,u
	free_lun,u
	endif
 end

;*****EXIT*****
6:begin widget_control,ev.top,/destroy
  end

else:

endcase

end

pro d22kin_log
;** **********
;**

base  = widget_base   (title='D22 KIN logarithmic acquisition',/column,frame=5)
title = widget_label  (base,value='Logarithmic increase of the acquisition times')

base0 = widget_base   (base ,/column,frame=5)

base1 = widget_base   (base0 ,/row)
label = widget_label  (base1,value='First time (s) :')
first = widget_text   (base1,value='',/editable,xsize=5,uvalue=1)
label = widget_label  (base1,value='Increment (s) :')
incre = widget_text   (base1,value='',/editable,xsize=5,uvalue=2)

base2 = widget_base   (base0 ,/row)
label = widget_label  (base2,value='Roughly total time (s) :')
time  = widget_text   (base2,value='',/editable,xsize=5,uvalue=3)
do0   = widget_button (base2,value='Do',uvalue=4)

base3 = widget_base   (base0 ,/row)
label = widget_label  (base3,value='Saved in file :')
file  = widget_text   (base3,value='',/editable,xsize=10)
label = widget_label  (base3,value='.tic')
save  = widget_button (base3,value='Save',uvalue=5)

base5 = widget_base  (base,/row)
total = widget_label (base5,value='',/dynamic_resize)
frame = widget_label (base5,value='',/dynamic_resize)

base4 = widget_base   (base ,/row)
exit  = widget_button (base4,value='Exit',uvalue=6)
mesg  = widget_label  (base4,value='',/dynamic_resize)

widget_control,first,set_uvalue=[1,mesg]
widget_control,incre,set_uvalue=[2,mesg]
widget_control,time ,set_uvalue=[3,mesg]
widget_control,do0  ,set_uvalue=[4,first,incre,time,file,mesg,total,frame]
widget_control,save ,set_uvalue=[5,first,incre,time,file,mesg]


widget_control,base,/realize
xmanager,'d22kin_log',base,/just_reg
end
