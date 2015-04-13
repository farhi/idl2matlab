;*******************************************************************************
;*******************************************************************************
pro eve_event,ev
;*******************************************************************************
;*******************************************************************************

widget_control,ev.id,get_uvalue=u


common nb,n
common nb2,m
common tablo,tabb
common wavelth,wav

for i=1,m[0] do begin
	if u(0) eq i then begin
	widget_control,u(1),get_value=ech
	samp=strtrim(ech,2)
	tabb[i+100]=samp
	tabb[i+600]=samp
	endif
endfor




;---------------- CLOSE BUTTON ---------------
if u(0) eq 23 then begin
widget_control,ev.top,/destroy
endif

;---------------- NEW BUTTON ----------------
if u(0) eq 24 then begin
widget_control,/reset
rack
endif




;-------------------------------------------------------------------------------
;---------------------------------------- OPEN GRILLE.PROC ---------------------

if u(0) eq 25 then begin

common tablo,tabb


cpt=0
for i=1,m[0] do begin
	widget_control,u(i+1),get_value=ech
	if strlen(strtrim(string(ech[0]),2)) gt 0 then begin
	cpt=cpt+1
	tabb[cpt+100]=strtrim(ech,2)
;	tabb[cpt]=strtrim(string(i),2)
	tabb[cpt]=i
	tabb[i+600]=tabb[cpt+100]
	endif

endfor
tabb[1080]=cpt


;--------- INITIALISATION -----------
common tablo2,tabb2
tabb2=strarr(701)
;------------------------------------

grille

endif


end








;*******************************************************************************
;*******************************************************************************
pro ev_event,ev
;*******************************************************************************
;*******************************************************************************

widget_control,ev.id,get_uvalue=uv
common nb,n
common nb2,m
common tablo,tabb
common top,changer_based0

common base,base0,error

if uv(0) eq 4 then begin
  widget_control,uv(1),get_value=m
;  print, "m: ",m
  tabb[1081]=m
endif

if uv(0) eq 3 then begin
  ;widget_control,nb_rack,set_uvalue=[3,rack_nb,nb_rack,pos0,incr]
  para =DialNewValue(TYPE='t_para',NAME='changer_file')
  ;help,para,/struc
  ;print,para
  widget_control,uv(2),get_value=nr
  nr1=nr-1
  tabb[1082]=nr
  pos_0=para.changer_one(nr1)
  incr=para.changer_inc(nr1)
  m=long(para.changer_nb(nr1))
;for test only: m=17
;m=17
  tabb[1081]=m
  widget_control,uv(1),set_value=m
  ;widget_control,uv(3),set_value=pos_0
  ;widget_control,uv(4),set_value=inc
endif


if uv(0) eq 2 then begin
  widget_control,ev.top,/destroy
endif


if uv(0) eq 1 then begin    
	widget_control,uv(2),get_value=nr
	tabb[1082]=nr
	widget_control,uv(1),get_value=m
	tabb[1080]=m

	if (m[0] gt 0) and (m[0] le 100) then begin
;-------------------------------------------------------------------------------
;------------------------------------ BASE CREATION ----------------------------

	;---------------- ERROR ------------------
	err=' '
	widget_control,error,set_value=string(err)
	;-----------------------------------------

par=strarr(23)

base0=widget_base(group_leader=changer_based0,title='Command File (3)',/column,/align_center)

base1=widget_base(base0,/column)
base11=widget_base(base1,/align_center)	
base12=widget_base(base1,/align_center)
base13=widget_base(base1)
	
base2=widget_base(base0,/row)
base21=widget_base(base2,/column)	
base22=widget_base(base2,/column)
base23=widget_base(base2,/column)

base3=widget_base(base1,/align_center)

lab1=widget_label(base11,frame=5,value='enter names of samples')	
;lab2=widget_label(base12,value='*****************')
		
		for i=1,m[0] do begin
	
	txt=string(format='(I2)',i)
	pos=widget_text(base21,xsize=2,value=txt)
	par(i)=widget_text(base22,xsize=18)	
	widget_control,par(i),/editable
	
		endfor

but3=widget_button(base23,frame=10,value='measurement table',uvalue=[25])
but2=widget_button(base23,value='new')
but1=widget_button(base23,value='close')

;lambda=cw_field(base3,xsize=2,title='wavelength:',value='8')

		
	widget_control,base0,/realize
	
	
;-------------------------------------------------------------------------------
;-------------------------------- EVENTS ---------------------------------------

for i=1,m[0] do begin
	
	txt=string(format='(I2)',i)
	widget_control,par(i),set_uvalue=[i,par(i)]

endfor
;widget_control,lambda,set_uvalue=[26,lambda]

widget_control,but1,set_uvalue=[23,but1]
widget_control,but2,set_uvalue=[24,but2]
widget_control,but3,set_uvalue=[25,par]

;++++++++++++++++++++++++++++++++++++++++++++++++++++++
common wavelth,wav
wav=0

common check,chec
if chec eq 1 then begin
	nam[0]=tabb[1098]+'.cmd'
	openr,roro,nam[0],/get_lun

	while not (eof(roro)) do begin
		point_lun,-roro,deb
		readf,roro,read1
	
		wave=strpos(read1,'wavelength')
			if wave ge 0 then begin
			lamb=strmid(read1,wave+2,100)
			tabb[1099]=strtrim(lamb,2)
			wav=1
			widget_control,lambda,set_value=tabb[1099]

		
			endif
	endwhile
	close,roro
endif

;++++++++++++++++++++++++++++++++++++++++++++++++++++++

xmanager,'eve',base0,/just_reg

	
	
	
	
	
	
	
;---------- CONDITION TO QUIT IF n>22 ou n<1 -----------------------------------
	
	endif else begin
;------------------------ ERROR ------------------------------	
	err='*** out of range ***'
	widget_control,error,set_value=string(err)
;------------------------------------------------------------

	endelse




endif

end





;*******************************************************************************
;*******************************************************************************
pro rack
;*******************************************************************************
;*******************************************************************************

common tablo, tabb


;---------------------------- INITIALISATION -----------------------------------
tabb=strarr(1100)

;-------------------------------------------------------------------------------

common base,base0,error
common sa,sam
common top,changer_based0

base0=widget_base(group_leader=changer_based0,title='Command File (2)',/column)

base01=widget_label(base0,frame='5',value='choice of the rack')
;base02=widget_label(base0,value='*********************')

base1=widget_base(base0, /column)


err=string(format='(A25)','')
error=widget_label(base1,value=err)

;**** modified by roland *************************************************
nb_rack=cw_field(base1,xsize=2,title='rack #: ',/return_events)
rack_nb=cw_field(base1,xsize=2,title='# of positions on the rack: ', $
	/return_events)
;************************************************************************

base2=widget_base(base0,/align_center,/column)
;lab=widget_label(base2,value='***')

create=widget_button(base2,frame=10,xsize=240,value='sample table')

but=widget_button(base2,xsize=200,value='close')

;--------------- REALIZE THE WIDGETS ------------------------
widget_control,base0,/realize
;------------------------------------------------------------

pos0=0
incr=-10.
widget_control,rack_nb,set_uvalue=[4,rack_nb]
widget_control,nb_rack,set_uvalue=[3,rack_nb,nb_rack] ;,pos0,incr]
widget_control,error,set_uvalue=[6,error]

widget_control,but,set_uvalue=[2,but]
widget_control,create,set_uvalue=[1,rack_nb,nb_rack]  ;,pos0,incr]






;------------------ (DISPLAY NOTHING WHEN NO ERROR) ----------------------------
err=' '								
widget_control,error,set_value=string(err)
;-------------------------------------------------------------------------------





xmanager,'ev',base0,/just_reg,group_leader=changer_based0

end
