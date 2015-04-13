; written by Floris Andre, August 2001

;**************************
;HOW TO RUN THIS DIAL:
;------------------------
;START idl THEN ENTER:  
;Idl>.run dial_lecture
;Idl> lecture   
;**************************


pro lecture
;** *********
;Used to run with "byGeorge"
FORWARD_FUNCTION DialControl, DialNewValue, DialOn
if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

dial_bygeorge,'lecture'	;<-------- Name of the Dial !!!
END				;         ~~~~~~~~~~~~~~~~~ !!!

;*********************
PRO dial_lecture_macro, D, V1
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_d22centre

print, " "
print, "********************"
print, "dial_lecture_macro"

if D.init eq 0 then begin
;	print, "init was 0"
	D.init=1
	D.frequency=0.
	Lim=DialNewValue(type='limits')
	para=DialNewValue(type='t_para')
;help, para, /struc
	lecture_gui

endif else begin
;	print,'init was 1'
endelse

end

;*************************************************************************************************************************************
;*************************************************************************************************************************************
pro ev01_event,ev
;*************************************************************************************************************************************
;*************************************************************************************************************************************
common check,chec
common tablo,tabb
tabb=strarr(1100)
widget_control,ev.id,get_uvalue=uu




;-------------- TO KNOW IF THE FILE EXISTS OR NOT ---------------
common errr,err2
common nofile,exist_file,ex
exist_file=0
ex=0
;-----------------------------------------------------------------



;------------ close button ----------------
if uu(0) eq 11114 then begin
widget_control,ev.top,/destroy
endif 
;------------------------------------------

;--------------- wavelength ---------------
;if uu(0) eq 11116 then begin
;	widget_control,uu(1),get_value=l
;	tabb[1099]=l[0]
;
;endif
;--------------------------------------------

;---------------- name of file ----------------
if uu(0) eq 11115 then begin
	widget_control,uu(1),get_value=nam
	print,"nam: ",nam
	nam=strtrim(string(nam))+string('.cmd')
	print,"nam: ",nam
	tabb[1098]=nam[0]
endif

;---------------------------------------------



;------------ open file button -----------------
if uu(0) eq 11113 then begin

widget_control,uu(2),get_value=nam
tabb[1098]=nam[0]+'.cmd'
nam=tabb[1098]
print,"nam: ",nam


common nb,n
common vale,val

;-------------------------- VARIABLES ---------------------------
read1='   '
deb=0
cpt=0
common var,j,p,sn
j=0
val=strarr(10)
po=strarr(23)
pa=strarr(23)
ru=strarr(23)
rut=strarr(23)
sa=strarr(23)
re=strarr(23)



;----------------------------------------------------------------

;------------------------ READ THE FILE -------------------------


on_ioerror,label
openr,roro,nam[0],/get_lun

;----- THE FILE EXISTS --------
exist_file=1
err2=string(format='(A80)','')
;------------------------------

while not (eof(roro)) do begin
	point_lun,-roro,deb
	readf,roro,read1
	
	if strmid(read1,4,5) eq 'title' then begin
	val[0]=strmid(read1,9,80)
	val[0]=strmid(strtrim(val[0],2),0,32)
	endif
		
	if strmid(read1,4,4) eq 'date' then begin
	val[1]=strmid(read1,9,80)
	val[1]=strmid(strtrim(val[1],2),0,10)
	;print,val[1]
	endif
	
	if strmid(read1,4,4) eq 'user' then begin
	val[2]=strmid(read1,9,80)
	val[2]=strmid(strtrim(val[2],2),0,10)
	;print,val[2]
	endif
	
	if strmid(read1,4,4) eq 'prop' then begin
	val[3]=strmid(read1,9,20)
	val[3]=strmid(strtrim(val[3],2),0,10)
	;print,val[3]
	endif

	;----------------- FOR POSITION ---------------
	
	ch=strpos(read1,'cha')
	;print,'cha= ',ch
	if ch ge 0 then begin
		cpt=cpt+1
		j=cpt
		po[j]=strmid(read1,ch+4,80)
		po[j]=strtrim(po[j],2)	
		;print,"po= ",po[j]
		nsa=po[j]
	endif	
	ps=strpos(read1,'par sub')
	;print,'ps= ',ps
	if ps ge 0 then begin
		;---------- FOR PAR SUB --------------
		und=strpos(read1,'_')
		
		if und gt 0 then begin
		pa[nsa]=strmid(read1,ps+10,20)
		pa[nsa]=strtrim(pa[nsa])
		;print,pa[nsa]
		endif else begin
		pa[nsa]=strmid(read1,ps+8,20)
		pa[nsa]=strtrim(pa[nsa])
		endelse
		
		;---------------------------------------
	endif
	
	if strmid(read1,0,3) eq 'run' then begin
		r=strpos(read1,'run')
		rf=r+3
		
		;----------- FOR RUN VALUE ---------------
		;if strmid(read1,4,50) eq 'm' then begin
			p1=strpos(read1,'m')
			if p1 gt 0 then begin
			;print,'p=',p1,cpt
			rut[j]='m'
			;print,'rut= ',rut[j]
			p=p1
			endif
		;if strmid(read1,4,50) eq 't' then begin
			p2=strpos(read1,'t')
			if p2 gt 0 then begin
			;print,'p=',p2,cpt
			rut[j]='t'
			;print,'rut= ',rut[j]
			p=p2
			endif else begin
			rut[j]='t'
			endelse
		;endif
		
		
			ru[j]=strmid(read1,rf+1,p-(rf+1))
			ru[j]=strtrim(ru[j],2)
			;print,'ru= ',ru[j]
		
		
		
		;---------- FOR S/N VALUE -----------------
		
		s1=strpos(read1,'s',rf)
			if s1 gt 0 then begin
			
			sa[j]='s'
			;print,'sav= ',sa[j]
			sn=s1
			endif else begin
			sa[j]='s'
			endelse
		
		n1=strpos(read1,'n',rf)
			
			if n1 gt 0 then begin
			sa[j]='n'
			;print,'sav= ',sa[j]
			sn=n1
			;print,'sn= ',sn
			
		
		;-----------------------------------------
		
		;---------- FOR REP VALUE -----------------
		
		re[j]=strmid(read1,p+1,sn-(p+1))
		re[j]=strtrim(re[j],2)
		;print,'re= ',re[j]
		;-----------------------------------------
			
			endif else begin
			re[j]='1'
			endelse
			
	endif
	
	print,''
	
	
	
	
	
	
endwhile

close,roro



;------------------------------ end of reading -----------------------


;if exist_file eq 1 then begin
;openr,roro,nam[0],/get_lun

;while not (eof(roro)) do begin
;	point_lun,-roro,deb
;	readf,roro,read1

;		if strpos(read1,'wavelength') gt 0 then begin
;		tabb[1099]=strmid(read1,11,50)
;		ex=1	
;		print,'l= ',tabb[1099]
;		endif

;endwhile
;close,roro
;endif


label:err2=string('no such file has been found')




;--------- INITIALISATION -----------
common tablo2,tabb2
tabb2=strarr(601)
;------------------------------------



;--------------------------- FILL GRILLE.PRO WITH THE OPENED FILE ------------


for i=1,cpt do begin
tabb[i]=po[i]		;position
tabb[i+100]=pa(po[i])	;par sub
tabb[i+200]=ru[i]	;value of preset
tabb[i+300]=rut[i]	;t/m
tabb[i+400]=re[i]	;repetition
tabb[i+500]=sa[i]	;save

endfor

;----- nb max of samples = 100 ----------
tabb[1081]=100
;----------------------------------------
tabb[1080]=cpt


grille
endif


end

















;*************************************************************************************************************************************
;*************************************************************************************************************************************
pro ev0_event,ev
;*************************************************************************************************************************************
;*************************************************************************************************************************************

common check,chec			;to know whitch button has been selected
common nofile,exist_file
exist_file=1
widget_control,ev.id,get_uvalue=u

;---------- close button ---------------
if u(0) eq 11114 then begin
	widget_control,ev.top,/destroy
endif 

;------------ create new file ----------
if u(0) eq 11111 then begin
	chec=0
	ex=0
	rack
	
endif 

;----------- use a saved file -----------
if u(0) eq 11112 then begin
	chec=1
common basedeb,based0,based1

;----------------- DEFINITION OF WIDGETS ---------------------
common basefin,base2d,base3d
common basedeb,based0,based1
common b,b1,b2
common sortie,exit
base3d=widget_base(base2d,/row)
q3=cw_field(base3d,title='enter the saved file to open:',xsize=10,/return_events)
q4=widget_label(base3d,value='.cmd')

b3=widget_button(base2d,xsize=200,value='open file')

widget_control,b1,sensitive=0
widget_control,b2,sensitive=0

;--------------------- REALIZE THE WIDGETS ------------------------
widget_control,based0,/realize

;-------------------------- EVENTS ----------------------------
widget_control,q3,set_uvalue=[11115,q3]
widget_control,b3,set_uvalue=[11113,b3,q3]
widget_control,exit,set_uvalue=[11114,exit]
xmanager,'ev01',based0,/just_reg
	
endif
end



;*******************************************************************************
;*****************************************************************************











;*************************************************************************************************************************************
;*************************************************************************************************************************************
pro lecture_gui
;*************************************************************************************************************************************
;*************************************************************************************************************************************

;----------------- DEFINITION OF WIDGETS ---------------------

common basedeb,based0,based1
common basefin,base2d,base3d
common b,b1,b2
common sortie,exit
common newl,newline

;------------ for new line button in grille.pro------------
newline=0
;----------------------------------------------------------

based0=widget_base(title='Command File (1)',/column)

q1=widget_label(based0,frame=5,value='     select one button     ')
q2=widget_label(based0,value='*****')


based1=widget_base(based0,/column)
base2d=widget_base(based0,/column)
base3d=widget_base(based0,/column,/align_center)

b1=widget_button(based1,value='   create new file   ')
b2=widget_button(based1,value='   use saved file    ')


q3=widget_label(base3d,value='*****')
exit=widget_button(base3d,value='close')


;--------------------- REALIZE THE WIDGETS ------------------------
widget_control,based0,/realize
;------------------------------------------------------------

;-------------------------- EVENTS ----------------------------

widget_control,b1,set_uvalue=[11111,b1]
widget_control,b2,set_uvalue=[11112,b2]
widget_control,exit,set_uvalue=[11114,exit]


xmanager,'ev0',based0,/just_reg


end



;*********************
FUNCTION dial_lecture
;*********************
;**
;** The dial initialisation

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
    GENERIC='mad'         ;connect to the mad-idl interface
    TYPE='data'           ;when DialNewValue() is used, get the data
    ONOFF=0               ;state of the Dial 1=running
    HISTORY=0             ;=1 to record values in file .his
    DURATION=0            ;if >0 then Dial is stopped after running duration seconds
    WUPDATE=1             ;update corresponding workspace
    FREQUENCY=0.          ;the Dial macro is executed each frequency seconds. 
    			  ;if =0 then the general frequency is used

 return, {init:0,generic:GENERIC}
end


