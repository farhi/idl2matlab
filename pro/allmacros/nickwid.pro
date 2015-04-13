

pro nickwid_event,event

widget_control,event.id,get_uvalue=ev,get_value=val

case ev of
	int: if int  	
	
	
endcase
;if ev= then begin
;	data=widget_base(title='Data entered')
;	ok=widget_button(data,value='OK')
;	widget_control,data,/realise
;	;widget_control,ev.top,/destroy
;endif	
end





pro nickwid

;common var,int,real

int=intarr(20)
real=fltarr(20)


;defaults
fac1='fac1'
fac2='fac2'
fac3='fac3'
r='r'
bg='bg'

base=widget_base(title='Data for Analysis',uval='base')

dbl1=widget_label(base,value='Direct beam run nos',xoffset=0,yoffset=5,uval='dbl')
int(1)=widget_text(base,xsize=4,xoffset=200,yoffset=0,/editable)
to=widget_label(base,value='to',xoffset=270,yoffset=5,uval='')
int(2)=widget_text(base,xsize=4,xoffset=300,yoffset=0,/editable)

ref1l1=widget_label(base,value='Reflection run nos (1)',xoffset=0,yoffset=95,uval='')
int(3)=widget_text(base,xsize=4,xoffset=200,yoffset=90,/editable)
to=widget_label(base,value='to',xoffset=270,yoffset=95,uval='')
int(4)=widget_text(base,xsize=4,xoffset=300,yoffset=90,/editable)

ref2l1=widget_label(base,value='Reflection run nos (2)',xoffset=0,yoffset=135,uval='')
int(5)=widget_text(base,xsize=4,xoffset=200,yoffset=130,/editable)
to=widget_label(base,value='to',xoffset=270,yoffset=135,uval='')
int(6)=widget_text(base,xsize=4,xoffset=300,yoffset=130,/editable)

ref3l1=widget_label(base,value='Reflection run nos (3)',xoffset=0,yoffset=175,uval='')
int(7)=widget_text(base,xsize=4,xoffset=200,yoffset=170,/editable)
to=widget_label(base,value='to',xoffset=270,yoffset=175,uval='')
int(8)=widget_text(base,xsize=4,xoffset=300,yoffset=170,/editable)

facl1=widget_label(base,value='Normalisation',xoffset=400,yoffset=45)
facl2=widget_label(base,value='factor',xoffset=430,yoffset=65)

real(1)=widget_text(base,xsize=2,value=fac1,xoffset=440,yoffset=90,/editable)

real(2)=widget_text(base,xsize=2,value=fac2,xoffset=440,yoffset=130,/editable)

real(3)=widget_text(base,xsize=2,value=fac3,xoffset=440,yoffset=170,/editable)

waterl1=widget_label(base,value='Water run nos',xoffset=0,yoffset=265,uval='')
int(9)=widget_text(base,xsize=4,xoffset=200,yoffset=260,/editable)
to=widget_label(base,value='to',xoffset=270,yoffset=265,uval='')
int(10)=widget_text(base,xsize=4,xoffset=300,yoffset=260,/editable)

lamdal1=widget_label(base,value='Useful wavelength',xoffset=0,yoffset=345,uval='')
lamdal2=widget_label(base,value='range (Angstroms)',xoffset=0,yoffset=365,uval='')
real(4)=widget_text(base,xsize=4,xoffset=200,yoffset=350,/editable)
to=widget_label(base,value='to',xoffset=270,yoffset=355,uval='')
real(5)=widget_text(base,xsize=4,xoffset=300,yoffset=350,/editable)

rl1=widget_label(base,value='Foreground range',xoffset=0,yoffset=435,uval='')
rl1=widget_label(base,value='(pixels)',xoffset=40,yoffset=455,uval='')
int(11)=widget_text(base,xsize=4,value=r,xoffset=160,yoffset=440,/editable)

bgl1=widget_label(base,value='Backgroung range',xoffset=270,yoffset=435,uval='')
bgl1=widget_label(base,value='(pixels)',xoffset=310,yoffset=455,uval='')
int(12)=widget_text(base,xsize=4,value=bg,xoffset=430,yoffset=440,/editable)

done=widget_button(base,value='DONE',xoffset=470,yoffset=0)

widget_control,base,/realize

xmanager,'nickwid',base

end
