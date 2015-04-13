; written by Roland May, August 2001
; modifications to also run it on D11 by Roland May, October 2001

;**************************
;HOW TO RUN THIS DIAL:
;------------------------
;START idl THEN ENTER:  
;Idl>.run dial_d22centre
;Idl> d22centre    
;**************************


pro d22centre
;** *********
;Used to run with "byGeorge"
FORWARD_FUNCTION DialControl, DialNewValue, DialOn
if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

dial_bygeorge,'d22centre'	;<-------- Name of the Dial !!!
END				;         ~~~~~~~~~~~~~~~~~ !!!



pro d22centre_gui_event, ev
;** *******************
;**

widget_control,ev.id,get_uvalue=uv

print,'dial_d22centre widget # ',uv(0)


case uv(0) of

1:begin	

;****************************************
;* CALCULATE CENTRE FROM DATA OR RUN	*
;****************************************

;print, "uv: ", uv
	widget_control,uv(1),get_value=larun 
	numor=larun(0)
;print, "numor: ",numor	

	widget_control,uv(2),get_uvalue=bex & bex=bex(0)
;print, "bex: ", bex
	choice=bex(0)
;print, "choice: ", choice
;	choice=2	

	widget_control,uv(3),get_value=lx0c 
;print, "lx0c: ", lx0c

	machine=strlowcase(getenv("HOST"))
	;print,"Instrument: ",machine
	id=strpos(machine,".")
	if id gt 0 then machine=strmid(machine,0,id)
	;print,"Instrument: ",machine

	if ((numor le 0) or (choice eq 1)) then begin
		V  =DialNewValue(TYPE='data',NAME='d22centre',/SETVALUE)
		V1 =DialNewValue(TYPE='t_nother',NAME='d22centre')
	  if machine eq 'd22' then begin
	 	dtr =V1.actang(1)	;*	detector offset		*
	 	dan =V1.actang(2)	;*	detector angle		*
	  endif else if machine eq 'd11' then begin
		dtr=0.
		dan=0.
	  endif
	endif else begin
	  if machine eq 'd22' then begin
		rdset,inst='D22',base='d22' & rdp=1 & V=rdrun(numor,datp=rdp)
		dan=rdp.p(18)
		dtr=rdp.p(19)
	  endif else if machine eq 'd11' then begin
		rdset,inst='D11',base='d11' & rdp=1 & V=rdrun(numor,datp=rdp)
		dan=0.
		dtr=0.
	  endif
	endelse
;print, dtr, dan
	
;*	FIND CENTRE OF GRAVITY (OF DIRECT-BEAM RUN).
; first round, rough centre
	Xsum    =TOTAL(V,2)     &  idx  =FINDGEN (n_elements(Xsum))
	Ysum    =TOTAL(V,1)     &  idy  =FINDGEN (n_elements(Ysum))
	Sumt=TOTAL(V)
	Sum1=TOTAL(Xsum)
	Xgravity=TOTAL(Xsum*idx)/Sum1
	Xgravity=fix(100.*(Xgravity+0.005))
	Xgravity=float(Xgravity)/100.
	Sum2=TOTAL(Ysum)
	Ygravity=TOTAL(Ysum*idy)/Sum2
	Ygravity=fix(100.*(Ygravity+0.005))
	Ygravity=float(Ygravity)/100.
print, "calulated centre (1): X0= ",Xgravity,"; Y0= ", Ygravity
print, "Sum of counts on detector: ",Sumt
print, "Sums over X and Y(1): X   ",Sum1,"; Y   ", Sum2
;second round
	myarr=lonarr(2)
	myarr(0)=0
	myarr(1)=fix(Xgravity-7.5)
	minx=max(myarr,i)
	minx=i
	if machine eq 'd22' then begin
	  myarr(0)=127
	  myarr(1)=fix(Xgravity+8.5)
	  maxx=min(myarr,i)
	  myarr(0)=0
	  myarr(1)=fix(Ygravity-7.5)
	  miny=max(myarr,i)
	  myarr(0)=127
	  myarr(1)=fix(Ygravity+8.5)
	  maxy=min(myarr,i)
	  Xsum=TOTAL(V(0:127,miny:maxy),2) & idx  =FINDGEN (n_elements(Xsum))
	  Ysum=TOTAL(V(minx:maxx,0:127),1) & idy  =FINDGEN (n_elements(Ysum))
	endif else if machine eq 'd11' then begin
	  myarr(0)=63
	  myarr(1)=fix(Xgravity+8.5)
	  maxx=min(myarr,i)
	  myarr(0)=0
	  myarr(1)=fix(Ygravity-7.5)
	  miny=max(myarr,i)
	  myarr(0)=63
	  myarr(1)=fix(Ygravity+8.5)
	  maxy=min(myarr,i)
	  Xsum=TOTAL(V(0:63,miny:maxy),2) & idx  =FINDGEN (n_elements(Xsum))
	  Ysum=TOTAL(V(minx:maxx,0:63),1) & idy  =FINDGEN (n_elements(Ysum))
	endif
	Sum1=TOTAL(Xsum(minx:maxx))
	Xgravity=TOTAL(Xsum(minx:maxx)*idx(minx:maxx))/Sum1
	Xgravity=fix(100.*(Xgravity+0.005))
	Xgravity=float(Xgravity)/100.
	Sum2=TOTAL(Ysum(miny:maxy))
	Sumc=TOTAL(V(minx:maxx,miny:maxy))
	Ygravity=TOTAL(Ysum(miny:maxy)*idx(miny:maxy))/Sum2
	Ygravity=fix(100.*(Ygravity+0.005))
	Ygravity=float(Ygravity)/100.
	X0S=STRMID(STRTRIM(Xgravity,2),0,5)
	Y0S=STRMID(STRTRIM(Ygravity,2),0,5)

print, "calulated centre (2): X0= ",X0S,"; Y0= ", Y0S
print, "Sums over X and Y(2): X   ",Sum1,"; Y   ", Sum2
print, "Sum around centre: ",Sumc

	widget_control,uv(3),set_value=X0S
	widget_control,uv(4),set_value=Y0S
		
	if machine eq 'd22' then begin
	  BXOFFSET=-3
	  BYOFFSET=0
	  BX	=FIX((Xgravity-63.5)*7.5+dtr+0.5)+BXOFFSET
	  BY	=FIX((Ygravity-63.5)*7.5+0.5)+BYOFFSET
	endif else if machine eq 'd11' then begin
	  BXOFFSET=0
	  BYOFFSET=0
	  BX	=FIX((Xgravity-31.5)*10.+dtr+0.5)+BXOFFSET
	  BY	=FIX((Ygravity-31.5)*10.+0.5)+BYOFFSET
	endif
	BXS	=STRTRIM(BX,2)	
	BYS	=STRTRIM(BY,2)	
print, "calculated beamstop: BX= ",BXS,"; BY= ", BYS

	widget_control,uv(5),set_value=BXS
	widget_control,uv(6),set_value=BYS

	widget_control,uv(7),set_value=strtrim(long(Sumt),2)
	widget_control,uv(8),set_value=strtrim(long(Sumc),2)

	widget_control,uv(9),set_value=''; erase message
end

2:begin 

;****************************************
;*         GET DATA/RUN CHOICE		*
;****************************************

;print, "uv: ", uv
	widget_control,uv(2),get_uvalue=bex			
;print, "bex: ", bex
	bex(0)=uv(1)				
	widget_control,uv(2),set_uvalue=bex
;print, "bex: ", bex
	choice=fix(bex(0))
	print, "data/run = ", choice

end

3:begin

;****************************************
;*           GET RUN NUMBER		*
;****************************************

	widget_control,ev.id,get_value=laruns & laruns=laruns(0)		
	ok=0								
	on_ioerror,missdd
	larun=long(laruns)
	numor=long(laruns)
print, "numor : ", numor
	ok=1
	missdd: if not ok then widget_control,uv(2),$
		set_value='Type conversion error' $
                else widget_control,uv(2),set_value=''

end

4:begin

;****************************************
;*           GET X0			*
;****************************************

	widget_control,ev.id,get_value=lax0cs & lax0c=lax0cs(0)		
	ok=0								
	on_ioerror,misx0c
	lax0c=float(lax0cs)
	nx0c=float(lax0cs)
;	print, "x0c : ", nx0c
	ok=1
	misx0c: if not ok then widget_control,uv(2),$
		  set_value='Type conversion error' $
                else widget_control,uv(2),set_value=''

end

5:begin

;****************************************
;*           GET Y0			*
;****************************************

	widget_control,ev.id,get_value=lay0cs & lay0c=lay0cs(0)		
	ok=0								
	on_ioerror,misy0c
	lay0c=float(lay0cs)
	ny0c=float(lay0cs)
;	print, "x0c : ", ny0c
	ok=1
	misy0c: if not ok then widget_control,uv(2),$
		  set_value='Type conversion error' $
		else widget_control,uv(2),set_value=''

end

6:begin

;****************************************
;*           GET BX			*
;****************************************

	widget_control,ev.id,get_value=labxs & labx=float(labxs(0))		
	ok=0								
	on_ioerror,misbx
;	lax0c=float(lax0cs)
	nbx=float(lax0cs)
	print, "bxc : ", nbx
	ok=1
	misbx:	if not ok then widget_control,uv(2),$
		  set_value='Type conversion error' $
		else widget_control,uv(2),set_value=''

end

7:begin
		
;****************************************
;*           GET BY			*
;****************************************

	widget_control,ev.id,get_value=labys & laby=float(labys(0))		
	ok=0								
	on_ioerror,misby
;	laby=float(labys)
	nby=float(labys)
	print, "byc : ", nby
	ok=1
	misby:	if not ok then widget_control,uv(2),$
		  set_value='Type conversion error' $
		else widget_control,uv(2),set_value=''

end

8:begin
 
;****************************************
;*      SEND CENTRE VALUES TO MAD	*
;****************************************

;print, "uv: ", uv
	widget_control,uv(1),get_value=lax0cs & lax0c=lax0cs(0)			
;print, "x0c: ", lax0c
	x0s=strtrim(lax0c,2)				
	widget_control,uv(2),get_value=lay0cs & lay0c=lay0cs(0)			
;print, "y0c: ", lay0c
	y0s=strtrim(lay0c,2)				
	txt='par centre '+X0S+' '+Y0S
	print, txt
;	R =Dialcontrol (txt,check=.5,NAME='d22centre')
	R =Dialcontrol (txt,NAME='d22centre')

end

9:begin 

;****************************************
;*      SEND BEAMSTOP VALUES TO MAD	*
;****************************************

;print, "uv: ", uv

	widget_control,uv(1),get_value=lax0cs & lax0c=lax0cs(0)			
	x0s=strtrim(lax0c,2)				
	widget_control,uv(2),get_value=lay0cs & lay0c=lay0cs(0)			
	y0s=strtrim(lay0c,2)				
	txt='BX '+X0S+' BY '+Y0S
	print, txt
;	R =Dialcontrol (txt,check=.5,NAME='d22centre')
	R =Dialcontrol (txt,NAME='d22centre')

end

12:begin 

;****************************************
;*      REFRESH MAD CENTRE VALUES	*
;****************************************

	para	=DialNewValue(TYPE='t_para',NAME='d22centre')
	X0      =para.x0
	Y0      =para.y0
print, "X0: ", X0, "   Y0: ", Y0
	X0tr	=strtrim(X0,2)
	Y0tr	=strtrim(Y0,2)
print, "X0tr: ", X0tr, "   Y0tr: ", Y0tr
	widget_control,uv(1),set_value=X0tr			
	widget_control,uv(2),set_value=Y0tr			
	V1 =DialNewValue(TYPE='t_nother',NAME='d22centre')
	BX  =V1.actang(3)
	BY  =V1.actang(4)
	BXM=strmid(strtrim(string(BX),2),0,5)
	BYM=strmid(strtrim(string(BY),2),0,5)
	widget_control,uv(3),set_value=BXM			
	widget_control,uv(4),set_value=BYM			

end

13:begin

;****************************************
;*	EXIT BUTTON			*
;****************************************

	widget_control,ev.top,/destroy
	DialClear, 'd22centre'

end

14:begin

;****************************************
;*           MINUS BUTTON		*
;****************************************

	widget_control,uv(1),get_value=laruns 
	numor=long(laruns(0))-1
print, "numor : ", numor
	laruns=strtrim(string(numor),2)
	widget_control,uv(1),set_value=laruns 

end


15:begin

;****************************************
;*           PLUS BUTTON		*
;****************************************

	widget_control,uv(1),get_value=laruns 
	numor=long(laruns(0))+1
print, "numor : ", numor
	laruns=strtrim(string(numor),2)
	widget_control,uv(1),set_value=laruns 

end

else:
; **************************************
endcase

end

;**************

pro d22centre_gui, para
;** *************
;**

if xregistered('d22centre_gui') eq 0 then begin
										
	para=DialNewValue(TYPE='t_para',NAME='d22centre')
;help, para, /struc
	X0      =para.x0
	Y0      =para.y0
	X0M=strmid(strtrim(string(X0),2),0,5)
	Y0M=strmid(strtrim(string(Y0),2),0,5)

	V1 =DialNewValue(TYPE='t_nother',NAME='d22centre')
	BX  =V1.actang(3)
	BY  =V1.actang(4)
	BXM=strmid(strtrim(string(BX),2),0,5)
	BYM=strmid(strtrim(string(BY),2),0,5)
	X0S	=''
	Y0S	=''
	BXS	=''
	BYS	=''

base=widget_base(title='BEAM CENTRE',/COLUMN,resource_name='geo')

;******************************************************************************	
										
base0  =widget_base  (base   ,/column,frame=5);,resource_name='lamp')		
 
base01	=widget_base  (base0 ,/row)
labvide	=widget_label (base01,value='      MAD centre values:       X0 = ')
lax0M	=widget_label (base01,value='')
txx0M   =widget_text  (base01,value=X0M,xsize=4,ysize=1)
labvide	=widget_label (base01,value='Y0 = ')
lay0M	=widget_label (base01,value='')
txy0M	=widget_text  (base01,value=Y0M,xsize=4,ysize=1)
base02	=widget_base  (base0 ,/row)
labvide	=widget_label (base02,value='      MAD beamstop values: BX = ')
labxM	=widget_label (base02,value='')
txbxM   =widget_text  (base02,value=BXM,xsize=4,ysize=1)
labvide	=widget_label (base02,value='BY = ')
labyM	=widget_label (base02,value='')
txbyM	=widget_text  (base02,value=BYM,xsize=4,ysize=1)
base03	=widget_base  (base0 ,/row)
bid	=widget_label (base03,$
	value='                                              ')
refresh	=widget_button(base03 ,value='Refresh',frame=5,resource_name='lamp')

base1  =widget_base  (base   ,/column,frame=5);,resource_name='lamp')		
mesg   =widget_label (base1,value=string(''),/dynamic_resize,$
	resource_name='lamp')

widget_control,txx0M,set_uvalue 	=[10,txx0M]
widget_control,txy0M,set_uvalue 	=[11,txy0M]
widget_control,refresh,set_uvalue	=[12,txx0M,txy0M,txbxM,txbyM]

base12 =widget_base  (base1  ,/row)
bid    =widget_label (base12 ,value='     ')
calcul =widget_button(base12 ,value='Calculate centre',uvalue=1,frame=5,$
	resource_name='lamp')
bid    =widget_label (base12 ,value=' from ')
set    =lonarr(3)
excl   =widget_base  (base12,/exclusive,/row)
set(1) =widget_button(excl,value=' data' ,  /no_release,uvalue=[2,1,excl])
set(2) =widget_button(excl,value=' run #',  /no_release,uvalue=[2,2,excl])
larun  =widget_label (base12,value='')
minus  =widget_button(base12 ,value='-',uvalue=14,frame=5,resource_name='lamp')
txrun  =widget_text  (base12,value='0',/editable,frame=5,xsize=5,uvalue=3,$
	resource_name='lamp')
plus   =widget_button(base12 ,value='+',uvalue=15,frame=5,resource_name='lamp')

widget_control,set(1),/set_button
widget_control,excl,set_uvalue=[2,set]
widget_control,txrun,set_uvalue=[3,txrun,mesg]
widget_control,minus,set_uvalue=[14,txrun]
widget_control,plus,set_uvalue=[15,txrun]

base17	=widget_base  (base1 ,/row)
lasumt	=widget_label (base17,value='      Counts on detector: ')
txsumt	=widget_text  (base17,value=' ',xsize=6,ysize=1)
lasumc	=widget_label (base17,value=' around centre: ')
txsumc	=widget_text  (base17,value=' ',xsize=6,ysize=1)

base13	=widget_base  (base1 ,/row)
labvide	=widget_label (base13,value='      calculated values: ')
lax0c	=widget_label (base13,value='X0 = ')
txx0c	=widget_text  (base13,value=+strtrim(X0S,2),frame=5,xsize=4,ysize=1,$
	/editable)
lay0c	=widget_label (base13,value='Y0 = ')
txy0c	=widget_text  (base13,value=+strtrim(Y0S,2),frame=5,xsize=4,ysize=1,$
	/editable)
bid	=widget_label (base13,value=' (editable)')

base14	=widget_base  (base1 ,/row)
labv	=widget_label (base14,value='      beamstop values: ')
labx	=widget_label (base14,value='BX = ')
txbx	=widget_text  (base14,value=+strtrim(BXS,2),frame=5,xsize=4,ysize=1,$
	/editable)
laby	=widget_label (base14,value='BY = ')
txby	=widget_text  (base14,value=+strtrim(BYS,2),frame=5,xsize=4,ysize=1,$
	/editable)
bid	=widget_label (base14,value=' (editable)')

widget_control,txx0c,set_uvalue =[4,txx0c,mesg]
widget_control,txy0c,set_uvalue =[5,txy0c,mesg]
widget_control,txbx,set_uvalue  =[6,txbx,mesg]
widget_control,txby,set_uvalue  =[7,txby,mesg]
widget_control,calcul,set_uvalue=[1,txrun,excl,txx0c,txy0c,txbx,txby,$
	txsumt,txsumc,mesg]

base15	=widget_base  (base1 ,/row)
bid	=widget_label (base15 ,value='     ')
censet	=widget_button(base15 ,value='Send centre values to MAD',uvalue=1,$
	frame=5,resource_name='lamp')
bid	=widget_label (base15 ,value='     ')
beamset	=widget_button(base15 ,value='  Set beamstop with MAD  ',uvalue=1,$
	frame=5,resource_name='lamp')

widget_control,censet,set_uvalue =[8,txx0c,txy0c]
widget_control,beamset,set_uvalue=[9,txbx,txby]

base16	=widget_base  (base1 ,/row)
bid	=widget_label (base16,$
	value='                                              ')
exit	=widget_button(base16,value='     Exit     ',uvalue=[13],frame=5,$
	resource_name='lamp')

widget_control,base,/realize

xmanager,'d22centre_gui',base,/just_reg

endif
end

;*********************
PRO dial_d22centre_macro, D, Lim, para
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function 
;** dial_d22centre

print, " "
print, "********************"
print, "dial_d22centre_macro"

if D.init eq 0 then begin
;	print, "init was 0"
	D.init=1
	D.frequency=0.
	Lim=DialNewValue(type='limits')
	para=DialNewValue(type='t_para')
;help, para, /struc
	d22centre_gui, para

endif else begin
;	print,'init was 1'
endelse

end


;*********************
FUNCTION dial_d22centre
;*********************
;**
;** The dial initialisation

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
    GENERIC='mad'         ;connect to the mad-idl interface
    TYPE='data'           ;when DialNewValue() is used, get the data
    ONOFF=0               ;state of the Dial 1=running
    VALUE=fltarr(128,128) ;value you assign to the Dial. 
                          ;This value is automaticaly plotted. 
			  ;put errors in ERROR var.
;   PLOT=0                ;-2=no plot 0=image 1=surface 2=contour 
                          ;n>2 means show vector of last n values of the scalar
;   UPPERLIM=0.           ;upper limit of a 1dim. plot (LOWERLIM for lower)
    HISTORY=0             ;=1 to record values in file .his
    DURATION=0            ;if >0 then Dial is stopped after "duration" seconds
    WUPDATE=1             ;update corresponding workspace
    FREQUENCY=0.          ;the Dial macro is executed each frequency seconds. 
    			  ;if =0 then the general frequency is used

   ;User Variables (Must be present in return statement)
   ;--------------
;    LOG =0	 ;  Detectors  not in log.
;    DLOG=0	 ;  Diagram    not in log.
;    NUMOR_sim=0L ;Used for simulation
;    NUMOR_cal=0L ;Used for calibration numor
;    WATER=0	 ;Used for calibration data
;    DIAGUPD=0	 ;1:Diagram may be updated

;return, {generic:GENERIC,type:TYPE,value:VALUE,FREQUENCY:FREQUENCY] ;,$
;         PLOT:PLOT,init:0,log:LOG,dlog:DLOG,tof:0L,channel:1L,$
;         xcenter:0.,ycenter:0.,diagupd:DIAGUPD,$
;	 numor_sim:NUMOR_sim,numor_cal:NUMOR_cal,water:WATER}
return, {init:0}
end
