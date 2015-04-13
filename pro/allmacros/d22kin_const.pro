pro d22kin_const_event,ev
;** ******************
;**

widget_control,ev.id,get_uvalue=uv
case uv(0) of

;*****TIME PRESET CONTROL*****
1:begin widget_control,ev.id,get_value=presets & presets=presets(0)
	ok=0								
	on_ioerror,mispreset
	preset=float(presets)
	ok=1
	mispreset: if not ok then widget_control,uv(1),set_value='Error Preset'$
			else widget_control,uv(1),set_value=''
  end

;*****TIME CONTROL*****
2:begin widget_control,ev.id,get_value=times & times=times(0)
	ok=0								
	on_ioerror,mistime
	time=float(times)
	ok=1
	mistime: if not ok then widget_control,uv(1),set_value='Error Time'$
			else widget_control,uv(1),set_value=''
  end

;*****DO********
3:begin t=1 & t1=1 & t2=1
	e=0 & e1=0	

	widget_control,uv(1),get_value=presets & presets=presets(0)
	ok=0								
	on_ioerror,mispreset1
	preset=float(presets)
	ok=1
	mispreset1: if not ok then t=0

	widget_control,uv(2),get_value=times & times=times(0)
	ok=0								
	on_ioerror,mistime1
	time=float(times)
	ok=1
	mistime1:   if not ok then t1=0

	widget_control,uv(3),get_value=file & file=file(0)

if (not (preset eq 0) and not (time eq 0)) then begin

	if t  eq 0 then widget_control,uv(4),set_value='error preset' $
		else begin
			if preset gt 390 then begin
			widget_control,uv(4),set_value='preset less than 390s'
			e=1
			endif else begin
	if t1 eq 0 then begin
		widget_control,uv(4),set_value='error total time'
		e1=1
		endif else widget_control,uv(4),set_value=''
endelse
endelse

if ((t eq 1) and (t1 eq 1)) then begin
if ((e eq 0) and (e1 eq 0)) then begin 

	tick=90.9e-9
	tick=preset/tick	;*transforme s en tick
	x=time/preset		;*nombre de frames
	x=fix(x)

	if x  gt 450 then begin
		widget_control,uv(4),set_value='too many frames'
		t2=0
		endif else begin
		widget_control,uv(4),set_value=''
		endelse

if (t2 eq 1)  then begin
	tic=lonarr(x)
	sec=strarr(x)
	sum=strarr(x)
	s=0
	for i=0,x-1 do begin
		tic(i)=tick
		sec(i)=preset
	endfor
	sum(0)=preset
	total=0
	for i=1,x-1 do begin
		sum(i)=sum(i-1)+preset
	endfor
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
	widget_control,uv(5),set_value=total
	widget_control,uv(6),set_value=frame
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
	widget_control,uv(5),set_value=total
	widget_control,uv(6),set_value=frame
	xdisplayfile,'kin.tmp',group=ev.top,height=10,width=50
	endelse
endif
endif
endif
endif
  end

;*****SAVE*********
4:begin widget_control,uv(1),get_value=presets & presets=presets(0)
	preset=float(presets)
	widget_control,uv(2),get_value=times & times=times(0)
	time=float(times)
	widget_control,uv(3),get_value=save & save=save(0)
if (not (preset eq 0) and not (time eq 0)) then begin
	tick=90.9e-9
	tick=preset/tick ;*transforme s en tick
	x=time/preset    ;*nombre de frames
	x=fix(x)
	tic=lonarr(x)
	for i=0,x-1 do begin
		tic(i)=tick
	endfor
	openw,u,save+'.tic',/get_lun
	for i=0,x-1 do begin
	printf,u,tic(i)
	endfor
	close,u
	free_lun,u
	endif
  end

;*****EXIT*********
5:begin widget_control,ev.top,/destroy
  end

else:

endcase

end

pro d22kin_const
;** ************
;**

base   = widget_base  (title='D22 KIN Constant acquisition',/column,frame=5)
title  = widget_label (base,value='Constant acquisition time')

base0  = widget_base  (base,/column,frame=5)

base1  = widget_base  (base0,/row)
label  = widget_label (base1,value='Preset (s) :')
preset = widget_text  (base1,value='',/editable,xsize=5,uvalue=1)
label  = widget_label (base1,value='Total time (s) :')
time   = widget_text  (base1,value='',/editable,xsize=5,uvalue=2)
do0    = widget_button(base1,value='Do',uvalue=3)

base2  = widget_base  (base0,/row)
label  = widget_label (base2,value='Saved in file :')
file   = widget_text  (base2,value='',/editable,xsize=10)
label  = widget_label (base2,value='.tic ')
save   = widget_button(base2,value='Save',uvalue=4)

base4  = widget_base  (base,/row)
total  = widget_label (base4,value='',/dynamic_resize)
frame  = widget_label (base4,value='',/dynamic_resize)

base3  = widget_base  (base,/row)
exit   = widget_button(base3,value='Exit',uvalue=5)
mesg   = widget_label (base3,value='',/dynamic_resize)

widget_control,preset,set_uvalue=[1,mesg]
widget_control,time  ,set_uvalue=[2,mesg]
widget_control,do0   ,set_uvalue=[3,preset,time,file,mesg,total,frame]
widget_control,save  ,set_uvalue=[4,preset,time,file,mesg]

widget_control,base,/realize
xmanager,'d22kin_const',base,/just_reg
end
