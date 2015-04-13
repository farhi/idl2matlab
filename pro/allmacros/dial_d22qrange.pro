;**************************
;HOW TO RUN THIS DIAL !!!
;------------------------
;START idl THEN ENTER:  
;Idl>.run dial_d22qrange
;Idl> d22qrange    
;**************************


pro d22qrange
;** *********
;Used to run with "byGeorge"
FORWARD_FUNCTION DialControl, DialNewValue, DialOn
if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

dial_bygeorge,'d22qrange'   ;<-------- Name of your Dial !!!
END                         ;          ~~~~~~~~~~~~~~~~~ !!!


pro qrange_gui_event,ev
;** ****************
;**

widget_control,ev.id,get_uvalue=uv
	;print,'dial_d22qrange widget # ',uv(0)


case uv(0) of

;****************************************
;*	WAVELENGTH CONTROL		*
;****************************************
1:begin widget_control,ev.id,get_value=bwls & bwls=bwls(0)		
	ok=0								
	on_ioerror,miswl
	bwl=float(bwls)
	ok=1
	miswl: if not ok then widget_control,uv(1),set_value='Type conversion error' $
                         else widget_control,uv(1),set_value=''
  end
			
;****************************************
;*	BEAMSTOP CONTROL		*
;****************************************
2:begin widget_control,uv(2),get_uvalue=bex			
	bex(0)=uv(1)				
	widget_control,uv(2),set_uvalue=bex
  end
		
;****************************************
;*	DETECTOR DISTANCE CONTROL	*
;****************************************
3:begin widget_control,ev.id,get_value=lasdds & lasdds=lasdds(0)		
	ok=0								
	on_ioerror,missdd
	lasdd=float(lasdds)
	ok=1
	missdd: if not ok then widget_control,uv(1),set_value='Type conversion error' $
                         else widget_control,uv(1),set_value=''
  end

;****************************************
;*	DETECTOR OFFSET CONTROL		*
;****************************************
4:begin widget_control,ev.id,get_value=txdofs & txdofs=txdofs(0)	
	ok=0								
	on_ioerror,misdof
	txdof=float(txdofs)
	ok=1
	misdof: if not ok then  widget_control,uv(1),set_value='Type conversion error' $
			 else widget_control,uv(1),set_value=''
  end

;****************************************
;*	CALCULATE BUTTON		*
;****************************************
5:begin 
	eps =0.002 ;**epsilon for DET **
	eps1=0.02  ;**epsilon for DTR DAN BX BY  **
	e=0
	e1=0
	e2=0
	bs=0
	xo=63.5
	t =1 
	t1=1 
	t2=1
	v1=DialNewValue(TYPE='t_para'  ,NAME='d22qrange')
	nb=v1.speed_nb
	h =v1.speed_h
	l =v1.speed_l
	x=0
	cste  = v1.sel_const(0)
	offset= v1.wave_offset(0)
									;********************************
	widget_control,uv(2),get_uvalue=bexcls & bexcls=bexcls(0)	;*	beamstop		*
	bexcl=float(bexcls)						;********************************
	if (bexcl eq 1) then bs=4.					
	if (bexcl eq 2) then bs=5.
	if (bexcl eq 3) then bs=5.5
	if (bexcl eq 4) then bs=7.
	if (bexcl eq 5) then bs=8.
									;********************************
	widget_control,uv(1),get_value=bwls    & bwls=bwls(0)		;*	wavelength		*
	ok=0								;********************************
	on_ioerror,misenter1 
	bwl=float(bwls)
	ok=1
	misenter1: if not ok then t=0		
									;********************************
	widget_control,uv(3),get_value=sdds    & sdds=sdds(0)		;*	detector distance	*
	ok1=0								;********************************
	on_ioerror,misenter2 
	sdd=float(sdds)
	ok1=1
	misenter2:if not ok1 then t1=0
	
									;********************************
	widget_control,uv(4),get_value=dofs    & dofs=dofs(0)		;*	detector offset		*
	ok2=0								;********************************
	on_ioerror,misenter3 
	dof=float(dofs)
	ok2=1
	misenter3:if not ok2 then t2=0
										;************************
	widget_control,uv(12),get_uvalue=coll  & coll=coll(n_elements(coll)-1)	;*	collimation	*
										;************************
	speed=cste/(bwl-offset)

	if t eq 0 then widget_control,uv(10),set_value='Type conversion error in Wavelength' $
			else begin
				for i=0,nb do begin
				if ((speed gt l(i)) and (speed lt h(i))) then x=1
				endfor
				if x eq 1 then begin
				widget_control,uv(10),set_value='? Wavelength forbidden'
				e=1
				endif else begin
	if t1 eq 0 then widget_control,uv(10),set_value='Type conversion error in Distance' $
			else begin
				if ((sdd lt (uv(13)-eps)) or (sdd gt (uv(14)+eps))) then begin
				Distance='? Distance: range from '+strtrim((string(uv(13))),2)+' to '+strtrim((string(uv(14))),2)
				widget_control,uv(10),set_value=Distance
				e1=1
				endif else begin
	if t2 eq 0 then widget_control,uv(10),set_value='Type conversion error in Offset' $
			else begin
				if ((dof lt (uv(15)-eps1)) or (dof gt (uv(16)+eps1))) then begin
				Offset='? Offset: range from '+strtrim((string(uv(15))),2)+' to '+strtrim((string(uv(16))),2)
				widget_control,uv(10),set_value=Offset
				e2=1
				endif else begin
	if coll lt (sdd-eps) then widget_control,uv(10),set_value='! caution: collimation smaller than detector distance !'$
			else widget_control,uv(10),set_value=''
								endelse
							endelse
						endelse
					endelse
				endelse
			endelse

	if ((t eq 1) and (t1 eq 1) and (t2 eq 1)) then begin
;	if ((e eq 0) and (e1 eq 0) and (e2 eq 0)) then begin	

							;************************			
	rmin    =sdd/(sdd-0.3)*bs/2			;*	rmin		*	
	rmin=(round(rmin/0.75+0.5)+1)*0.75		;*			*
	arg=xo*0.75+dof/10				;*		/ all	*
   	rmax    =sqrt(arg*arg+61*61*0.75*0.75)		;*	rmax		*
	rmax=round(rmax)				;*		/ in	*
  	tetamin =0.5*atan(rmin/(sdd*100))		;*			*
	qmin    =(4*!pi)/(bwl)*sin(tetamin)		;*	qmin	/ cm	*
	tetamax =0.5*atan(rmax/(sdd*100))		;*			*
	qmax    =(4*!pi)/(bwl)*sin(tetamax)		;*	qmax		*
	dmax    =!pi/qmin				;*	dmax		*
	dmin    =!pi/qmax				;*	dmin		*
							;************************
							;************************	
	widget_control,uv(5),set_value=string(rmin) 	;*	rmin		*	
	widget_control,uv(6),set_value=string(rmax) 	;*	rmax		*
	widget_control,uv(7),set_value=string(qmin) 	;*	qmin		*
	widget_control,uv(8),set_value=string(qmax) 	;*	qmax 		*
	widget_control,uv(9),set_value=string(dmax) 	;*	dmax		*
	widget_control,uv(11),set_value=string(dmin) 	;*	dmin		*
	endif						;************************
;	endif
end

;****************************************
;*	COLLIMATION LIST		*
;****************************************
6:begin coll=uv(ev.index+1)
	uv(n_elements(uv)-1)=coll		;* puts value of coll in last element of list!	*
	widget_control,ev.id,set_uvalue=uv	;************************************************
	widget_control,uv(10),get_value=sdds & sdds=sdds(0)
	sdd=float(sdds)
	if coll lt sdd then widget_control,uv(11),set_value='caution: collimation smaller than detector distance'$
	else widget_control,uv(11),set_value=''
		
  end

;****************************************
;*	EXIT BUTTON			*
;****************************************
7:begin widget_control,ev.top,/destroy
	DialClear, 'd22qrange'

  end

;****************************************
;*	HELP BUTTON			*
;****************************************
8:begin xdisplayfile,'/users/d22/hlp/dial_d22qrange.hlp',title='Q-Range Help',group=ev.top,height=52,width=65
	

  end

;****************************************
;*	SEND BUTTON			*
;****************************************
9:begin
	e=0 & e1=0 & e2=0 & e3=0 & e4=0
	t=1 & t1=1 & t2=1 & t3=1 & t4=1
	V1=DialNewValue(TYPE='t_nother',NAME='d22qrange')
	V2=DialNewValue(TYPE='t_para'  ,NAME='d22qrange')

	nb=v2.speed_nb
	h =V2.speed_h
	l =V2.speed_l
	x=0
	cste  = v2.sel_const(0)
	offset= v2.wave_offset(0)
									;********************************
	widget_control,uv(1),get_value=bwls    & bwls=bwls(0)		;*	wavelength		*  	
	ok=0								;********************************
	on_ioerror,missend 
	bwl=float(bwls)							
	ok=1
	missend: if not ok then t=0
									;********************************
	widget_control,uv(2),get_uvalue=bexcls & bexcls=bexcls(0)	;*	beamstop		*
									;********************************
	widget_control,uv(3),get_value=sdds    & sdds=sdds(0)		;*	detector distance	*
	ok1=0								;********************************
	on_ioerror,missend1 
	sdd=float(sdds)
	ok1=1
	missend1: if not ok1 then t1=0
									;********************************
	widget_control,uv(4),get_value=dofs    & dofs=dofs(0)		;*	offset			*
	ok2=0								;********************************
	on_ioerror,missend2 
	dof=float(dofs)
	ok2=1
	missend2: if not ok2 then t2=0
								       	;********************************
	widget_control,uv(6),get_value=bxs     & bxs=bxs(0)		;*	Bx			*
	ok3=0								;********************************
	on_ioerror,missend3 
	bx=float(bxs)
	ok3=1
	missend3: if not ok3 then t3=0
									;********************************
	widget_control,uv(7),get_value=bys     & bys=bys(0)		;*	By			*
	ok4=0								;********************************
	on_ioerror,missend4 
	by=float(bys)
	ok4=1
	missend4: if not ok4 then t4=0
									;********************************
	widget_control,uv(9),get_value=x0s     & x0s=x0s(0)		;*	X0			*
	x0=float(x0s)							;********************************

									;********************************
	widget_control,uv(10),get_value=y0s     & y0s=y0s(0)		;*	Y0			*
	y0=float(y0s)							;********************************
	
									;********************************
	widget_control,uv(5),get_uvalue=coll				;*	collimation		*
	coll=coll(n_elements(coll)-1)					;********************************

									;********************************
	activity=V1.cstate						;*	instrument status	*
									;********************************
	speed=cste/(bwl-offset)

	if t  eq 0 then widget_control,uv(8),set_value='Type conversion error in Wavelength' $	;************************
			else begin								;*	test values	*
				for i=0,nb do begin						;************************
				if ((speed gt l(i)) and (speed lt h(i))) then x=1
				endfor
				if x eq 1 then begin
				widget_control,uv(8),set_value='? Wavelength forbidden'
				e=1
				endif else begin
	if t1 eq 0 then widget_control,uv(8),set_value='Type conversion error in Distance' $
			else begin
				if ((sdd lt (uv(11))) or (sdd gt (uv(12)))) then begin
				Distance='? Distance: range from '+strtrim((string(uv(9))),2)+' to '+strtrim((string(uv(10))),2)
				widget_control,uv(8),set_value=Distance
				e1=1
				endif else begin
	if t2 eq 0 then widget_control,uv(8),set_value='Type conversion error in Offset' $
			else begin
				if ((dof lt (uv(13))) or (dof gt (uv(14)))) then begin
				Offset='? Offset: range from '+strtrim((string(uv(11))),2)+' to '+strtrim((string(uv(12))),2)
				widget_control,uv(8),set_value=Offset
				e2=1
				endif else begin
	if t3 eq 0 then widget_control,uv(8),set_value='Type conversion error in Bx' $
			else begin
				if ((bx lt (uv(17))) or (bx gt (uv(18)))) then begin
				Bx='? Bx: range from '+strtrim((string(uv(13))),2)+' to '+strtrim((string(uv(14))),2)
				widget_control,uv(8),set_value=Bx
				e3=1
				endif else begin
	if t4 eq 0 then widget_control,uv(8),set_value='Type conversion error in By' $
			else begin
				if ((by lt (uv(19))) or (by gt (uv(20)))) then begin
				By='? By: range from '+strtrim((string(uv(15))),2)+' to '+strtrim((string(uv(16))),2)
				widget_control,uv(8),set_value=By
				e4=1
				endif else begin
				widget_control,uv(8),set_value=''
										endelse
									endelse
								endelse
							endelse
						endelse
					endelse
				endelse
			endelse
		endelse
	endelse
	print,'Current status is ',activity
	if activity eq 0 then begin
	if ((t eq 1) and (t1 eq 1) and (t2 eq 1) and (t3 eq 1) and (t4 eq 1)) then begin
	if ((e eq 0) and (e1 eq 0) and (e2 eq 0) and (e3 eq 0) and (e4 eq 0)) then begin	
										;********************************
	R =DialControl ('wave '+string(bwl)               ,NAME='d22qrange')	;*	wave		/MAD	*	
	R =DialControl ('att c 3'                ,check=.5,NAME='d22qrange')	;*	attenuator		*
	R =DialControl ('bx ' +bxs+' by '+bys    ,check=.5,NAME='d22qrange')	;*	bx by			*
	cmd='det '+sdds+' coll '+string(coll)+' dtr '+dofs			;*				*
	R =DialControl (cmd                      ,check=.5,NAME='d22qrange')	;*	det coll dtr		*
	R =Dialcontrol ('par centre '+x0s+' '+y0s,check=.5,NAME='d22qrange')	;*	x0 y0			*
	R =DialControl ('att o'                  ,check=.5,NAME='d22qrange')	;*	attenuator out		*
										;********************************
	endif
	endif
	endif

	;* NB: beam command is  n o t  being sent by pushing the SEND button ! *
	;***********************************************************************
  end

;****************************************
;*	GET SETTING BUTTON		*
;****************************************
10:begin a=strtrim(string(uv(1)),2)
		on_ioerror,misfil
		u=-1
		if (a eq 0) then a='t'
		get_lun,u
		openr,u,'setting_'+a+'.cmd'
		a=' ' & wav=0. & beam=0 & det=0. & coll=0. & dtr=0. & n=0 &
		bx=0. & by=0. & x0=0. & y0=0. & dan=0.
	while not eof(u) do begin						;********************************
		readf,u,a							;*				*
		a=strlowcase(a)							;*	reading setting file	*
		i=strpos(a,'wave')						;*				*
		if i ge 0 then begin						;********************************
		reads,strmid(a,i+5,10),wav
		endif
		i=strpos(a,'beam')
		if i ge 0 then begin
		reads,strmid(a,i+5,17),beam
		endif
		i=strpos(a,'det')
		if i ge 0 then begin
		reads,strmid(a,i+4,10),det
		endif
		i=strpos(a,'coll')
		if i ge 0 then begin
		reads,strmid(a,i+5,10),coll
		endif
		i=strpos(a,'dtr')
		if i ge 0 then begin
		reads,strmid(a,i+4,10),dtr
		endif 
		i=strpos(a,'dan')
		if i ge 0 then begin
		reads,strmid(a,i+4,10),dan
		endif
		i=strpos(a,'bx')
		if i ge 0 then begin
		reads,strmid(a,i+3,10),bx
		reads,strmid(a,i+23,10),by
		endif
		;i=strpos(a,'by')
		;if i ge 0 then begin
		;reads,strmid(a,i+3,10),by
		;endif
		i=strpos(a,'par centre')
		if i ge 0 then begin
		reads,strmid(a,i+11,10),x0
		reads,strmid(a,i+25,10),y0
		endif &
	endwhile
		close,u
		free_lun,u
		col=0
		if coll eq 2    then col=1
		if coll eq 2.8  then col=2
		if coll eq 4    then col=3
		if coll eq 5.6  then col=4
		if coll eq 8    then col=5
		if coll eq 11.2 then col=6
		if coll eq 14.4 then col=7
		if coll eq 17.6 then col=8
									;********************************
		widget_control,uv(3) ,set_value =strtrim(string(wav),2) ;*	wavelength		*
		widget_control,uv(5) ,set_value =strtrim(string(det),2) ;* sample to detector distance  *
		widget_control,uv(6) ,set_value =strtrim(string(dtr),2) ;*	detector offset		*
		widget_control,uv(8) ,set_value =strtrim(string(bx),2) 	;*          bx                  *
		widget_control,uv(9) ,set_value =strtrim(string(by),2) 	;*          by                  *
		widget_control,uv(10),set_value =strtrim(string(x0),2)	;*          x0                  *
		widget_control,uv(11),set_value =strtrim(string(y0),2)	;*          y0                  *
		widget_control,uv(12),set_value =strtrim(string(dan),2)	;*	    dan			*
		widget_control,uv(4) ,get_uvalue=bex			;*	beamstop		*
		bex(0)=beam						;*				*
		widget_control,uv(4) ,set_uvalue=bex			;*				*
		widget_control,bex(beam),/set_button			;*				*
		widget_control,uv(7) ,set_list_select =col		;*	collimation		*		
		widget_control,uv(7) ,get_uvalue=cuv			;*				*	
		cuv(n_elements(cuv)-1)=coll				;*				*
		widget_control,uv(7) ,set_uvalue=cuv			;********************************

		if coll lt det then widget_control,uv(13),set_value='! caution: collimation smaller than detector distance !'$
		else widget_control,uv(13),set_value=''
		a=uv(1)
		widget_control,uv(14),set_value=strtrim(string(a),2)

		misfil:if u gt 0 then free_lun,u
 end

;****************************************
;*	SAVE SETTING BUTTON		*
;****************************************				
11:begin 
	e =0 & e1=0 & e2=0 & e3=0 & e4=0 & e5=0 & e6=0 & e7=0 & e8=0
	t0=1 & t1=1 & t2=1 & t3=1 & t4=1 & t5=1 & t6=1 & t7=1 & t8=1
	V1=DialNewValue(TYPE='t_para'  ,NAME='d22qrange')
	nb=v1.speed_nb
	h =V1.speed_h
	l =V1.speed_l
	x=0
	cste  = v1.sel_const(0)
	offset= v1.wave_offset(0)

	widget_control,uv(7),get_value=setnums & setnums=setnums(0)	;********************************
	ok8=0								;*	setting number		*
	on_ioerror,missave8						;********************************
	setnum=fix(setnums)
	ok8=1
	missave8: if not ok8 then t8=0

	if t8 eq 1 then begin

	widget_control,uv(1), get_value=bwls & bwls   =bwls(0)		;********************************
	ok=0								;*	wavelength		*
	on_ioerror,missave						;********************************
	bwl=float(bwls)
	ok=1
	missave: if not ok then t0=0
									;********************************
	widget_control,uv(2), get_uvalue=bexcl & bexcl=bexcl(0)		;*	beamstop		*
									;********************************
	widget_control,uv(3), get_value=sdds & sdds  =sdds(0)		;*	detector distance	*
	ok1=0								;********************************
	on_ioerror,missave1
	sdd=float(sdds)
	ok1=1
	missave1: if not ok1 then t1=0
		
	widget_control,uv(4), get_value=dofs & dofs  =dofs(0)		;********************************
	ok2=0								;*	offset			*
	on_ioerror,missave2						;********************************
	dof=float(dofs)
	ok2=1
	missave2: if not ok2 then t2=0
		
	widget_control,uv(8), get_value=dans & dans  =dans(0)		;********************************
	ok3=0								;*	angle			*
	on_ioerror,missave3						;********************************
	dan=float(dans)
	ok3=1
	missave3: if not ok3 then t3=0

	widget_control,uv(9), get_value=bxs & bxs    =bxs(0)		;********************************
	ok4=0								;*	Bx			*
	on_ioerror,missave4						;********************************
	bx=float(bxs)
	ok4=1
	missave4: if not ok4 then t4=0

	widget_control,uv(10),get_value=bys & bys    =bys(0)		;********************************
	ok5=0								;*	By			*
	on_ioerror,missave5						;********************************
	by=float(bys)
	ok5=1
	missave5: if not ok5 then t5=0

	widget_control,uv(11),get_value=x0s & x0s    =x0s(0)		;********************************
	ok6=0								;*	X0			*
	on_ioerror,missave6						;********************************
	x0=float(x0s)
	ok6=1
	missave6: if not ok6 then t6=0

	widget_control,uv(12),get_value=y0s & y0s   =y0s(0)		;********************************
	ok7=0								;*	Y0			*
	on_ioerror,missave7						;********************************
	y0=float(y0s)
	ok7=1
	missave7: if not ok7 then t7=0
										
	widget_control,uv(6),get_uvalue=coll   & coll=coll(n_elements(coll)-1)	
										
	widget_control,uv(7),get_value=setnums & setnums=setnums(0)

	speed=cste/(bwl-offset)

	if t0 eq 0 then widget_control,uv(13),set_value='Type conversion error in Wavelength' $	;************************
			else begin								;*	test values	*
				for i=0,nb do begin						;************************
				if ((speed gt l(i)) and (speed lt h(i))) then x=1
				endfor
				if x eq 1 then begin
				widget_control,uv(10),set_value='? Wavelength forbidden'
				e=1
				endif else begin
	if t1 eq 0 then widget_control,uv(13),set_value='Type conversion error in Distance' $
			else begin
				if ((sdd lt (uv(14))) or (sdd gt (uv(15)))) then begin
				Distance='? Distance: range from '+strtrim((string(uv(14))),2)+' to '+strtrim((string(uv(15))),2)
				widget_control,uv(13),set_value=Distance
				e1=1
				endif else begin
	if t2 eq 0 then widget_control,uv(13),set_value='Type conversion error in Offset' $
			else begin
				if ((dof lt (uv(16))) or (dof gt (uv(17)))) then begin
				Offset='? Offset: range from '+strtrim((string(uv(16))),2)+' to '+strtrim((string(uv(17))),2)
				widget_control,uv(13),set_value=Offset
				e2=1
				endif else begin
	if t3 eq 0 then widget_control,uv(13),set_value='Type conversion error in Angle' $
			else begin
				if ((dan lt (uv(18))) or (dan gt (uv(19)))) then begin
				Angle='? Angle: range from '+strtrim((string(uv(18))),2)+' to '+strtrim((string(uv(19))),2)
				widget_control,uv(13),set_value=Angle
				e3=1
				endif else begin
	if t4 eq 0 then widget_control,uv(13),set_value='Type conversion error in Bx' $
			else begin
				if ((bx lt (uv(20))) or (bx gt (uv(21)))) then begin
				Bx='? Bx: range from '+strtrim((string(uv(20))),2)+' to '+strtrim((string(uv(21))),2)
				widget_control,uv(13),set_value=Bx
				e4=1
				endif else begin
	if t5 eq 0 then widget_control,uv(13),set_value='Type conversion error in By' $
			else begin
				if ((by lt (uv(22))) or (by gt (uv(23)))) then begin
				By='? By: range from '+strtrim((string(uv(22))),2)+' to '+strtrim((string(uv(23))),2)
				widget_control,uv(13),set_value=By
				e5=1
				endif else begin
;	if t6 eq 0 then widget_control,uv(13),set_value='Type conversion error in X0' $
;			else begin
;				if ((x0 lt (0)) or (x0 gt (127))) then begin
;				widget_control,uv(13),set_value='? X0 : range from 0 to 127'
;				e6=1
;				endif else begin
;	if t7 eq 0 then widget_control,uv(13),set_value='Type conversion error in Y0' $
;			else begin
;				if ((y0 lt (0)) or (y0 gt (127))) then begin
;				widget_control,uv(13),set_value='? Y0 : range from 0 to 127'
;				e7=1
;				endif else begin
				widget_control,uv(13),set_value=''
;						endelse
;						endelse
;						endelse
;						endelse
				endelse
				endelse
				endelse
				endelse
		endelse
		endelse
		endelse
		endelse
endelse
endelse
endelse
endelse

	b=systime(0)
	setnum = fix(setnums)
	a=strtrim(setnum,2)
print,a
if ((t0 eq 1) and (t1 eq 1) and (t2 eq 1) and (t3 eq 1) and (t4 eq 1) and (t5 eq 1)) then begin
if ((e  eq 0) and (e1 eq 0) and (e2 eq 0) and (e3 eq 0) and (e4 eq 0) and (e5 eq 0)) then begin
if ((x0 lt (0)) or (x0 gt (127))) then begin
		widget_control,uv(13),set_value='? X0 : range from 0 to 127'
endif
if ((y0 lt (0)) or (y0 gt (127))) then begin
		widget_control,uv(13),set_value='? Y0 : range from 0 to 127'
endif
if ((a gt 0.9)  and (a lt 9.1)) then begin
		get_lun,u						;********************************
		openw ,u,'setting_'+a+'.cmd'				;*				*
		printf,u,'! written by GEORGE dial d22qrange on ',b	;*				*
		printf,u,'att c 3'					;*	writing setting file	*
		printf,u,'wave',bwl					;*				*	
        	printf,u,'! beam',bexcl					;********************************	
         	printf,u,'det',sdd,' coll',coll,' dtr',dof,' dan',dan
		printf,u,'bx',bx,' by',by
		printf,u,'par centre',x0,y0
		printf,u,'att o'
		close ,u
		free_lun,u
endif else begin
		if (a eq 0) then begin
		get_lun,u						;********************************
		openw,u,'setting_t.cmd'					;*				*	
		printf,u,'! written by GEORGE dial d22qrange on ',b	;*				*	
		printf,u,'att c 3'					;*	writing setting t file	*
		printf,u,'wave',bwl					;*				*	
        	printf,u,'! beam',bexcl					;********************************	
         	printf,u,'det',sdd,' coll',coll,' dtr',dof,' dan',dan
		printf,u,'bx',bx,' by',by
		printf,u,'par centre',x0,y0
		printf,u,'att c 3'
		close ,u
		free_lun,u
		endif
endelse

endif
endif

endif else widget_control,uv(7),set_value=''

  end
;****************************************
;*	INSTRUMENT SETTING BUTTON	*
;****************************************
12:begin 
	 V1 =DialNewValue(TYPE='t_nother',NAME='d22qrange')
	 V2 =DialNewValue(TYPE='t_para'  ,NAME='d22qrange')
	 Lim=DialNewValue(TYPE='limits'  ,NAME='d22qrange')	 
	 ;help,V1,/struc
	 ;help,V2,/struc
	 ;help,lim,/struc				;********************************
	 wave=V1.act_wave				;*	wavelength		*
	 coll=V1.act_coll				;*	collimation		*
	 beam=V2.beam_stop_unit				;*	beamstop		*
	 det =V1.actang(0)				;* sample to detector distance	*
	 dtr =V1.actang(1)				;*	detector offset		*
	 dan =V1.actang(2)				;*	     dan		*
	 bx  =V1.actang(3)				;*	     bx			*
	 by  =V1.actang(4)				;*	     by			*
	 X0  =V2.x0					;*	     x0			*
	 y0  =V2.y0					;*	     y0			*
	 						;********************************

	 if coll eq 1.4  then col=0
	 if coll eq 2    then col=1
	 if coll eq 2.8  then col=2
	 if coll eq 4    then col=3
	 if coll eq 5.6  then col=4
	 if coll eq 8    then col=5
	 if coll eq 11.2 then col=6
	 if coll eq 14.4 then col=7
	 if coll eq 17.6 then col=8
									;********************************
	 widget_control,uv(1) ,set_value = strtrim(string(wave),2)     	;*	wavelength		*
	 widget_control,uv(3) ,set_value = strtrim(string(det) ,2)     	;* sample to detector distance	*
	 widget_control,uv(4) ,set_value = strtrim(string(dtr) ,2)	;*	detector offset		*
	 widget_control,uv(6) ,set_value = strtrim(string(dan) ,2)	;*	     dan		*
	 widget_control,uv(7) ,set_value = strtrim(string(bx)  ,2)	;*	     bx			*
	 widget_control,uv(8) ,set_value = strtrim(string(by)  ,2)	;*	     by			*
	 widget_control,uv(9) ,set_value = strtrim(string(x0)  ,2)	;*	     x0			*
	 widget_control,uv(10),set_value = strtrim(string(y0)  ,2)	;*	     y0			*
	 widget_control,uv(2) ,get_uvalue= bex     			;*	beamstop		*
	 widget_control,bex(beam),/set_button				;*				*
	 widget_control,uv(5) ,set_list_select =col 			;*	collimation		*
	 widget_control,uv(5) ,get_uvalue=cuv				;*				*  
	 cuv(n_elements(cuv)-1)=coll					;*				*
	 widget_control,uv(5),set_uvalue=cuv				;*				*
									;********************************
   end

;****************************************
;*	SETTING NUMBER CONTROL		*
;****************************************
13:begin widget_control,ev.id,get_value=sns & sns=sns(0)
	 ok=0
	 on_ioerror,missn
	 sn=float(sns)
	 ok=1
	 missn: if not ok then widget_control,uv(1),set_value=''
   end

;****************************************
;*	X0 CONTROL			*
;****************************************
14:begin widget_control,ev.id,get_value=x0s & x0s=x0s(0)
	 ok=0
	 on_ioerror,misx0
	 x0=float(x0s)
	 ok=1
	 misx0: if not ok then widget_control,uv(1),set_value='' 
   end

;****************************************
;*	Y0 CONTROL			*
;****************************************
15:begin widget_control,ev.id,get_value=y0s & y0s=y0s(0)
	 ok=0
	 on_ioerror,misy0
	 y0=float(y0s)
	 ok=1
	 misy0: if not ok then widget_control,uv(1),set_value='' 
   end

;****************************************
;*	BX CONTROL			*
;****************************************
16:begin widget_control,ev.id,get_value=bxs & bxs=bxs(0)
	 ok=0
	 on_ioerror,misbx
	 bx=float(bxs)
	 ok=1
	 misbx: if not ok then widget_control,uv(1),set_value='' 
   end

;****************************************
;*	BY CONTROL			*
;****************************************
17:begin widget_control,ev.id,get_value=bys & bys=bys(0)
	 ok=0
	 on_ioerror,misby
	 by=float(bys)
	 ok=1
	 misby: if not ok then widget_control,uv(1),set_value=''
   end

;****************************************
;*	DAN CONTROL			*
;****************************************
18:begin widget_control,ev.id,get_value=dans & dans=dans(0)
	 ok=0
	 on_ioerror,misdan
	 dan=float(dans)
	 ok=1
	 misdan: if not ok then widget_control,uv(1),set_value='' 

   end


else:
endcase

end
;**************

pro qrange_gui, Lim
;** **********
;**

if xregistered('qrange_gui') eq 0 then begin


base=widget_base(title='D22 Q-RANGE',/COLUMN,resource_name='geo')

;******************************************************************************	;********************************
										;*	Get Setting #		*
base0  =widget_base  (base   ,/column,frame=5);,resource_name='lamp')		;*	Get Instrument Setting	*
										;*	Save Setting #		*
base01 =widget_base  (base0  ,/row)						;********************************
setting=widget_label (base01 ,value='Get Setting #')

base02 =widget_base  (base0  ,/row)
bid    =widget_label (base02 ,value='     ')
curset =widget_button(base02 ,value='Get Instrument Setting',uvalue=12,frame=5,resource_name='lamp')
bid    =widget_label (base02 ,value='        ')

base022=widget_base  (base02 ,/row,frame=5)
save   =widget_button(base022,value='Save Setting #',uvalue=[11],resource_name='lamp')
bid    =widget_label (base022,value='0 (for t)-9:')
sn1    =widget_text  (base022,value='',xsize=3,ysize=1,/editable,uvalue=13,resource_name='lamp')
widget_control,sn1,set_uvalue=[13,sn1]

;******************************************************************************	;********************************
										;*	Wavelength		*
base1  =widget_base  (base ,/column,frame=5)					;*	Beamstop #		*
										;*	Detector Distance	*
base11 =widget_base  (base1 ,/row)						;*	Offset			*
labwl  =widget_label (base11,value='Wavelength ('+string("305B)+') :')		;********************************
bwl    =widget_text  (base11,value='',xsize=5,ysize=1,/editable,uvalue=1,resource_name='lamp')
labbst =widget_label (base11,value='   Beamstop #')
def    =4
bst    =lonarr(5)
bexcl  =widget_base  (base11,/exclusive,/row)
	bst(0)=widget_button(bexcl,value='1',/no_release,uvalue=[2,1,bexcl])
	bst(1)=widget_button(bexcl,value='2',/no_release,uvalue=[2,2,bexcl])
	bst(2)=widget_button(bexcl,value='3',/no_release,uvalue=[2,3,bexcl])
	bst(3)=widget_button(bexcl,value='4',/no_release,uvalue=[2,4,bexcl])
	bst(4)=widget_button(bexcl,value='5',/no_release,uvalue=[2,5,bexcl])

widget_control,bst(def-1),/set_button
widget_control,bexcl,set_uvalue=[def,bst]

mesg   =widget_label (base1,value=string(''),/dynamic_resize,resource_name='lamp')

base12 =widget_base  (base1 ,/row)
lasdd  =widget_label (base12,value='Detector Distance (m) :')
txsdd  =widget_text  (base12,value='',xsize=6,ysize=1,/editable,uvalue=3,resource_name='lamp')
ladof  =widget_label (base12,value='         Offset (mm) :')
txdof  =widget_text  (base12,value='',xsize=6,ysize=1,/editable,uvalue=4,resource_name='lamp')

widget_control,bwl,  set_uvalue=[1,mesg]
widget_control,txsdd,set_uvalue=[3,mesg]
widget_control,txdof,set_uvalue=[4,mesg]

;******************************************************************************		;************************
											;*	Radius		*
											;*	Calculate	*
base2     =widget_base  (base ,/column,frame=5)						;*	Dmax		*
calculate =widget_button(base2,value='Calculate',uvalue=5,frame=5,resource_name='lamp')	;*	Q		*
											;************************	
base21    =widget_base  (base2 ,/row)							
labvide   =widget_label (base21,value='               ')
txrmin    =widget_label (base21,value='',/dynamic_resize)
labuni1   =widget_label (base21,value=' cm')
labrmin   =widget_label (base21,value='<  Radius  <')
txrmax    =widget_label (base21,value='',/dynamic_resize)
labuni2   =widget_label (base21,value=' cm')
bid       =widget_label (base21,value='                         ')

base22    =widget_base  (base2 ,/row)
labvide   =widget_label (base22,value='         ')
txqmin    =widget_label (base22,value='',/dynamic_resize)
labuni3   =widget_label (base22,value=+string("305B)+'-1')
labqmin   =widget_label (base22,value='<     Q        <')
txqmax    =widget_label (base22,value='',/dynamic_resize)
labuni5   =widget_label (base22,value=+string("305B)+'-1')
bid       =widget_label (base22,value='                         ')
 
base23    =widget_base  (base2 ,/row)
labvide   =widget_label (base23,value='            ')
labdmax   =widget_label (base23,value='Dmax =')
txdmax    =widget_label (base23,value='',/dynamic_resize)
labuni4   =widget_label (base23,value=+string("305B))
labdmin   =widget_label (base23,value='Dmin =')
txdmin    =widget_label (base23,value='',/dynamic_resize)
labuni4   =widget_label (base23,value=+string("305B))
bid       =widget_label (base23,value='                         ')

;******************************************************************************	;************************
										;*	Collimation	*
base3    =widget_base  (base ,/column,frame=5)					;*      X0		*
basea    =widget_base  (base3,/row)						;*      Y0		*
										;*      Bx		*
baseb    =widget_base  (basea,/row)						;*      By		*
labcolli =widget_label (baseb,value='Collimation (m) : ')			;*      angle		*
										;*	setting button	*
col	 =fltarr(9)								;************************
col	 =['1.4','2','2.8','4','5.6','8','11.2','14.4','17.6']
colli	 =widget_list  (baseb,value=col,ysize=3,uvalue=[6.,float(col),txsdd,mesg,0.],resource_name='lamp')

basec    =widget_base  (basea,/column)

basee    =widget_base  (basec,/row)
labvide  =widget_label (basee,value='              ')
labbx    =widget_label (basee,value='Bx (mm) =')
txtbx    =widget_text  (basee,value='',  /editable,xsize=5,uvalue=16,resource_name='lamp')

based    =widget_base  (basec,/row)
labvide  =widget_label (based,value='              ')
labby    =widget_label (based,value='By (mm) =')
txtby    =widget_text  (based,value='',  /editable,xsize=5,uvalue=17,resource_name='lamp')

base33   =widget_base  (base3 ,/row)
labx0    =widget_label (base33,value='X0 (pix.) =')
txtx0    =widget_text  (base33,value='', /editable,xsize=5,uvalue=14,resource_name='lamp')
labvide  =widget_label (base33,value='    ')
laby0    =widget_label (base33,value='Y0 (pix.) =')
txty0    =widget_text  (base33,value='', /editable,xsize=5,uvalue=15,resource_name='lamp')
labvide  =widget_label (base33,value='    ')
labdan   =widget_label (base33,value='angle (deg.) =')
txtdan   =widget_text  (base33,value='0',/editable,xsize=5,uvalue=18,resource_name='lamp')

widget_control,txtx0, set_uvalue =[14,txtx0]
widget_control,txty0, set_uvalue =[15,txty0]

widget_control,txtbx, set_uvalue =[16,txtbx]
widget_control,txtby, set_uvalue =[17,txtby]
widget_control,txtdan,set_uvalue =[18,txtdan]

set      =lonarr(11)
excl     =widget_base   (base01,/exclusive,/row)
	set(1) =widget_button(excl,value='1',  /no_release,$
		uvalue=[10,1,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
  	set(2) =widget_button(excl,value='2',  /no_release,$
		uvalue=[10,2,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(3) =widget_button(excl,value='3',  /no_release,$
		uvalue=[10,3,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(4) =widget_button(excl,value='4',  /no_release,$
		uvalue=[10,4,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(5) =widget_button(excl,value='5',  /no_release,$
		uvalue=[10,5,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(6) =widget_button(excl,value='6',  /no_release,$
		uvalue=[10,6,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(7) =widget_button(excl,value='7',  /no_release,$
		uvalue=[10,7,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(8) =widget_button(excl,value='8',  /no_release,$
		uvalue=[10,8,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(9) =widget_button(excl,value='9',  /no_release,$
		uvalue=[10,9,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])
	set(10)=widget_button(excl,value='0:t',/no_release,$
		uvalue=[10,0,excl,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,txtx0,txty0,txtdan,mesg,sn1])

widget_control,save,     set_uvalue =[11,bwl,bexcl,txsdd,txdof,excl  ,colli ,sn1   ,txtdan,txtbx ,txtby,txtx0 ,txty0,mesg,lim]
widget_control,curset,   set_uvalue =[12,bwl,bexcl,txsdd,txdof,colli ,txtdan,txtbx ,txtby ,txtx0 ,txty0,lim]
widget_control,calculate,set_uvalue =[5 ,bwl,bexcl,txsdd,txdof,txrmin,txrmax,txqmin,txqmax,txdmax,mesg ,txdmin,colli,lim]

;******************************************************************************			;************************
												;*	Send		*
base4    =widget_base   (base ,/row,frame=5)							;*	Help		*
												;*	Exit		*
send     =widget_button (base4,value='Send Command',uvalue=[9],frame=10,resource_name='lamp')	;************************
bid      =widget_label  (base4,value=' ')
bid      =widget_label  (base4,value=' ')
help     =widget_button (base4,value='Help',uvalue=[8],frame=5,resource_name='lamp')
bid      =widget_label  (base4,value=' ')
bid      =widget_label  (base4,value='                                                        ')
exit     =widget_button (base4,value='Exit',uvalue=[7],frame=5,resource_name='lamp')

widget_control,send,set_uvalue =[9,bwl,bexcl,txsdd,txdof,colli,txtbx,txtby,mesg,txtx0,txty0,lim]

;******************************************************************************

widget_control,base,/realize

xmanager,'qrange_gui',base,/just_reg
endif
end

pro dial_d22qrange_macro, D
;** ********************
;**

if D.init eq 0 then begin
	D.init=1
	D.frequency=0.
	Lim=DialNewValue(type='limits')
	qrange_gui,Lim

endif else print,'init was 1'

end

function dial_d22qrange
;******* **************
;**

return, {init:0}
end
