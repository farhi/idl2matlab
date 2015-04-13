;procedures in comment for pv-wave:
;*********************************
;RDID_D7
;RDID_D20
;RDID_D17
;GROUPY
;CATCH       now is dummy
;EXPAND_PATH now is dummy
;P_LAMBDA p_did_calib p_did_calev p_did_calod p_did_caldo (dummy for calibration)
;READS as been redefined
;## is changed by #
;structure {...} now is {,...} for DATP

FUNCTION P_LAMBDA, dummy
;******* ********
;**
@lamp.cbk

rst=lamp_macro
idx=strpos(lamp_macro,'macros')
if  idx gt 0 then begin rst= strmid(lamp_macro,0,idx-1) +lamp_dvd
			if   lamp_dvd eq "" then rst=rst+"]"
             endif else if   lamp_dvd ne "" then rst=rst+lamp_dvd

if strmid       (rst,strlen(rst)-1,1) ne lamp_dvd then rst=rst+lamp_dvd
bid=findfile    (rst+'*',count=n)
if n eq 0 then rst=''
return, rst
end


pro p_did_calib, inst, lamp_b1
;** ***********
;**
common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 , inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
common calibrotion         , cal_in4 , idx_in4 , shf_in4 , inf_in4

common calib	  , base0,lirlist,listcal,comment,curfil,bacur,nwork,swork,filen,lodfil,minst,ytext
common c_lamp_font

if (!D.flags and 65536) eq 0 then RETURN
IF xregistered('CALIB') gt 0 then widget_control,bad_id=ii,base0,map=1 $
ELSE BEGIN

CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN & endif
      MINST   = strlowcase(inst)
	PATHCAL = sys_dep('INSUB',P_LAMBDA(0),'CALIBRATION')
		    bid=findfile(pathcal+'*',count=nn) & if nn eq 0 then pathcal=''
      CD,PATHCAL,current=mee & lirlist = findfile('*'+MINST+'*') & CD,mee
      IF n_elements(CURFIL) eq 0 THEN CURFIL  =  MINST+'.cal'
      IF n_elements(SWORK)  eq 0 THEN swork   =' 0 '
      LODFIL=CURFIL

base0	=  widget_base  (title = 'CALIBRATION',resource_name='lamptouch',/column)
base0a	=  widget_base  (base0  ,/row   ,frame=4)
base0a1 =  widget_base  (base0a ,/column)
bid	=  widget_label (base0a1,value = PATHCAL,font=ft_b_bigger)
listcal	=  widget_list  (base0a1,value = lirlist,ysize = 7	,uvalue = [-88,394,1])

base0a1 =  widget_base  (base0a ,/column)
bid     =  widget_label (base0a1,value = 'for '+inst ,font=ft_b_bigger)
	   put_logo	,widget_base(base0a1,/row)
bid	=  widget_button(base0a1,value = 'Load selected file'	,uvalue = [-88,394,2])
bid	=  widget_button(base0a1,value = 'NO Calibration'	,uvalue = [-88,394,3])
bid	=  widget_button(base0a1,value = 'EXIT'			,uvalue = [-88,399  ])

base03	=  widget_base  (base0  ,/row)
bid	=  widget_label (base03 ,value = 'Current Calibration: '  ,font=ft_b_normal)
bacur	=  widget_label (base03 ,value = string(replicate(45b,30)),font=ft_b_normal)
comment	=  widget_label (base0  ,value = string(replicate(45b,50)),font=ft_b_normal)

base0b	=  widget_base  (base0  ,/column,frame=4)
bid	=  widget_label (base0b ,value = 'Make a Calibration file from raw data',font=ft_b_bigger)
base06	=  widget_base  (base0b ,/row)
bid	=  widget_label (base06 ,value='Wk_space #')
NWORK	=  widget_text  (base06 ,/editable,xsize =3 ,value= SWORK	,font=ft_propor)
bid	=  widget_label (base06 ,value='Comment:')
YTEXT	=  widget_text  (base06 ,/editable,xsize =15,value= 'no comment',font=ft_propor)
base07	=  widget_base  (base0b ,/row)
bid	=  widget_label (base07 ,value ='Filename:')
FILEN	=  widget_text  (base07 ,/editable,xsize =20,value='new'+MINST+'.cal',font=ft_propor)
bid	=  widget_button(base07 ,value = 'MAKE'			,uvalue = [-88,394,4])
bid	=  widget_label (base07 ,value ='(Put "'+MINST+'" in filename)')

widget_control  ,base0 , group_leader = lamp_b1,/realize  & put_logo
widget_control  ,bacur , set_value    = curfil
XMANAGER,'CALIB',base0 , event_handler='LAMP_EVENT_PARSER',/just_reg
ENDELSE
RETURN
END

pro p_did_calev, event,uv
;** ***********
;**
common calibration
common calibrotion
common calib

widget_control,comment,bad_id=ii,set_value=' '
CASE uv(2) of
;**SELECT
   1:	   lodfil = lirlist(event.index)
;**LOAD
   2:begin P_DID_CALOD, minst,lodfil, flg ,COMMENT=line
	   IF flg THEN begin curfil=lodfil & com= line+' is accepted ...' & endif $
		  ELSE begin curfil='None' & com= 'Bad Calibration file !!!' & endelse
	   widget_control,bacur  ,bad_id=ii, set_value=curfil
	   widget_control,comment,bad_id=ii, set_value=com
     end
;**NONE
   3:begin curfil='None' &  P_DID_CALOD, minst & widget_control,bacur,bad_id=ii,set_value=curfil & END
;**MAKE
   4:begin widget_control,nwork,bad_id=ii,get_value=swork & swork=strtrim(swork(0),2)
	   on_ioerror,mis_w & flg=0 & wn=fix(swork) & if (wn gt 0) and (wn le 20) then flg=1 & mis_w:
	   widget_control,filen,bad_id=ii,get_value=filnm & filnm=strtrim(filnm(0),2)
	   widget_control,ytext,bad_id=ii,get_value=text  & text =strtrim(text (0),2)
	   if flg eq 0 then com='Bad workspace number !!!' $
	   else begin	    P_DID_CALOD, minst,filnm,flg,wn , text
	   		    if flg eq  0 then com='Problem writting ' +filnm  +' !!!' else $
			    if flg eq -1 then com='Access denied:'    +pathcal+' !!!' $
					 else com= filnm+' write success ...'
      			    CD,PATHCAL,current=mee & lirlist = findfile('*'+MINST+'*') & CD,mee
			    widget_control,listcal , bad_id=ii,set_value=lirlist
	   endelse  &  widget_control,comment,bad_id=ii, set_value=com
     end
ELSE:
ENDCASE
RETURN
END

pro p_did_calod,minst,file, OK ,wn ,text ,LIST=list, COMMENT=line
;** ***********
;**
@lamp.cbk
common calibration
common calibrotion

IF n_elements(wn)   eq 1 then mot='MAKE'  else $
IF n_elements(file) eq 1 then mot='READ'  else $
IF keyword_set(list)     then mot='LIST'  else mot='CLEAR'

OK=-1 & in=0 & out=0 & line=''
IF mot eq 'READ' then begin on_ioerror,mis_read & OPENR,in ,pathcal+file,/get_lun
			    mis_read:  if in le 0 then mot='CLEAR'			& endif
IF mot eq 'MAKE' then begin on_ioerror,mis_writ & OPENW,out,pathcal+file,/get_lun
			    tmp=1 & ii=execute('tmp=float(W'+strtrim(string(wn),2)+')')	& endif
ok=0

IF mot eq 'LIST' then begin tmp=FINDFILE(pathcal+'*'+minst+'*' ,count=cc)
			    j  =strlen  (pathcal) & k=strpos(tmp(0),pathcal)
			    if cc gt 0 then begin   print,'Note:  CALIBRATION [,file="myfile"] [,/nocal] [,/list]'
						    print,'Actual calibration files:'
						    for i=0,cc-1  do if k lt 0 then print,tmp(i) $
						    else    print,strmid(tmp(i),j,30)
			    endif
endif else $
CASE  minst of
'd16':  IF mot eq 'READ' then begin
	   on_ioerror,misd16
	   inf_d16 =[file]
	   READF,  in,line
	   i =strpos(line,'(')   & j=strpos(line,'*')   & k=strpos(line,')')
	   sx=long(strmid(line,i+1,j-i-1)) & sy=long(strmid(line,j+1,k-j-1))
	   cal_d16 =fltarr(sx,sy)
 	   READF,in,cal_d16	&   ok=1
	   misd16: IF ok eq 0 THEN  cal_d16 =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikd16
	   sz=SIZE(tmp) & sx=strtrim(string(sz(1)),2) & sy=strtrim(string(sz(2)),2)
	   PRINTF, out,'Calib('+sx+'*'+sy+') ' +systime()+' '+text
	   PRINTF, out, tmp/(total(tmp)/n_elements(tmp)) & ok=1 & mikd16:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_d16 =0

'd1a':  IF mot eq 'READ' then begin cal_d1a =fltarr(25)  & ang_d1a=findgen(25)
	   on_ioerror,misd1a
	   inf_d1a =[file]
	   READF,  in,line  & READF,in,ang_d1a,cal_d1a	 & ok=1
	   misd1a: IF ok eq 0 THEN  cal_d1a =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikd1a
	   PRINTF, out,'Calib(25)    ' +systime()+' '+text
	   TAKE_DATP,p,w=wn
	   PRINTF, out, P.x
	   PRINTF, out, tmp/(total(tmp)/n_elements(tmp)) & ok=1 & mikd1a:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_d1a =0

'd2b':  IF mot eq 'READ' then begin cal_d2b =fltarr(64)  & ang_d2b=findgen(64)
	   on_ioerror,misd2b
	   inf_d2b =[file]
	   READF,  in,line  & READF,in,format='(64F8.3)',ang_d2b
			      READF,in,format='(64F8.5)',cal_d2b	 & ok=1
	   misd2b: IF ok eq 0 THEN  cal_d2b =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikd2b
	   PRINTF, out,'Calib(64)    ' +systime()+' '+text
	   TAKE_DATP,p,w=wn
	   PRINTF, out, P.x
	   PRINTF, out, tmp/(total(tmp)/n_elements(tmp)) & ok=1 & mikd2b:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_d2b =0

'd20': IF mot eq 'READ' then begin cal_d20 =fltarr(1600)& ang_d20=findgen(1600)
 	   on_ioerror,misd20
     	IF file EQ ' default' THEN BEGIN
       		inf_d20=[file,'not loaded','autod20.cal']
     	ENDIF ELSE BEGIN
 	     	READF,  in,line 
       		cal_d20 =fltarr(1600,long(strmid(line,6,4))/1600)
       		READF,in,ang_d20,cal_d20	 & ok=1
		IF N_ELEMENTS(inf_d20) EQ 0 THEN inf_d20=[file,line,'manualchoice'] ELSE inf_d20(0:1)=[file,line]
       		print,'New Calibration file loaded: ', inf_d20(0)
       		print,inf_d20(1)
       		if N_ELEMENTS(inf_d20) GE 3 THEN IF inf_d20(2) EQ 'autochoice' THEN BEGIN
         		PRINT,'D20 automatic default calibration by the right file - attention to wavelength!' 
       		ENDIF ELSE BEGIN
         		PRINT,'D20 manual choice of calibration file - pay attention to wavelength!' 
         		inf_d20(2) = 'manualchoice'
       		ENDELSE
 		misd20: IF ok eq 0 THEN  cal_d20 =0
     	ENDELSE
       ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikd20
	IF N_ELEMENTS(inf_d20) GE 1 THEN inf_d20=inf_d20(0:2)
 	PRINTF, out,'Calib('+strtrim(string(N_ELEMENTS(tmp)),2)+')  ' +systime()+' '+text
 	TAKE_DATP,p,w=wn
 	PRINTF, out, P.x(0:1599,0,0)
 	PRINTF, out, tmp & ok=1 & mikd20:
       ENDIF ELSE BEGIN
	cal_d20 =0
	inf_d20(0:2)=['','','']
	print,'NO Calibration',STRLEN(inf_d20(0))
       ENDELSE
	
'in13': IF mot eq 'READ' then begin cal_in13=fltarr(70)
	   on_ioerror,misin13
	   inf_in13=[file]
	   READF,  in,line  & READF,in,cal_in13		 & ok=1
	   misin13:IF ok eq 0 THEN  cal_in13=0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikin13 & mikin13:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_in13=0

'in6':  IF mot eq 'READ' then begin
	   on_ioerror,misin6
	   inf_in6 =[file]
	   READF,in,line & sz=1 & READS,line,sz
	   cal_in6 =fltarr(sz)  & idx_in6=intarr(sz) & shf_in6=intarr(sz)
	   READF,in,cal_in6	& READF,in,idx_in6 & READF,in,shf_in6 & ok=1
	   idx_in6=where(idx_in6 ge 0)
	   misin6: IF ok eq 0 THEN  cal_in6 =0
        ENDIF ELSE IF mot eq 'MAKE' then begin  on_ioerror,mikin6
	   s_e=-1
	   if (size(tmp))(0) eq 2   then begin  win=lineup(tmp,pos,s_i,s_e,shf,.2)
						tmp=total (tmp,1)
				    endif else  shf=tmp*0
	   np=n_elements(tmp) & sz =strtrim(np,2)
	   tmp=(tmp/(total(tmp)/np))>.25
	   idb=where( (tmp le .25) or (tmp ge 4.) )
	   idx=indgen(np)
	   if s_e(0) ge 0 then idx(s_e)=-1 else if idb(0) ge 0 then idx(idb)=-1

	   PRINTF, out,sz+' Angles ,goods ,lineup ' +systime()+' '+ text
	   PRINTF, out, tmp & PRINTF, out, idx & PRINTF, out, shf*0 & ok=1 & mikin6:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_in6 =0

'in5':  IF mot eq 'READ' then begin
	   on_ioerror,misin5
	   inf_in5 =[file]
	   READF,in,line & sz=1 & READS,line,sz
	   cal_in5 =fltarr(sz)  & idx_in5=intarr(sz) & shf_in5=intarr(sz)
	   READF,in,cal_in5	& READF,in,idx_in5 & READF,in,shf_in5 & ok=1
	   idx_in5=where(idx_in5 ge 0)
	   misin5: IF ok eq 0 THEN  cal_in5 =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikin5
	   s_e=-1
	   if (size(tmp))(0) eq 2   then begin  win=lineup(tmp,pos,s_i,s_e,shf,.2)
						tmp=total (tmp,1)
				    endif else  shf=tmp*0
	   np=n_elements(tmp) & sz =strtrim(np,2)
	   tmp=(tmp/(total(tmp)/np))>.25
	   idb=where( (tmp le .25) or (tmp ge 4.) )
	   idx=indgen(np)
	   if s_e(0) ge 0 then idx(s_e)=-1 else if idb(0) ge 0 then idx(idb)=-1

	   PRINTF, out,sz+' Angles ,goods ,lineup ' +systime()+' '+ text
	   PRINTF, out, tmp & PRINTF, out, idx & PRINTF, out, shf & ok=1 & mikin5:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_in5 =0

'in4':  IF mot eq 'READ' then begin
	   on_ioerror,misin4
	   inf_in4 =[file]
	   READF,in,line & sz=1 & READS,line,sz
	   cal_in4 =fltarr(sz)  & idx_in4=intarr(sz) & shf_in4=intarr(sz)
	   READF,in,cal_in4	& READF,in,idx_in4 & READF,in,shf_in4 & ok=1
	   idx_in4=where(idx_in4 ge 0)
	   misin4: IF ok eq 0 THEN  cal_in4 =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikin4
	   s_e=-1
	   if (size(tmp))(0) eq 2   then begin  win=lineup(tmp,pos,s_i,s_e,shf,.2)
						tmp=total (tmp,1)
				    endif else  shf=tmp*0
	   np=n_elements(tmp) & sz =strtrim(np,2)
	   tmp=(tmp/(total(tmp)/np))>.25
	   idb=where( (tmp le .25) or (tmp ge 4.) )
	   idx=indgen(np)
	   if s_e(0) ge 0 then idx(s_e)=-1 else if idb(0) ge 0 then idx(idb)=-1

	   PRINTF, out,sz+' Angles ,goods ,lineup ' +systime()+' '+ text
	   PRINTF, out, tmp & PRINTF, out, idx & PRINTF, out, shf & ok=1 & mikin4:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_in4 =0
ELSE:
ENDCASE
mis_writ:
IF in  gt 0 THEN FREE_LUN,in
IF out gt 0 THEN FREE_LUN,out
RETURN
END

pro p_did_caldo,minst,WOUT,YOUT,ok
;** ***********
;**
common calibration
common calibrotion
ok=0
CASE  minst of
'in4':begin	IF n_elements(cal_in4) eq 0 THEN P_DID_CALOD, minst,minst+'.cal', flg
		IF n_elements(cal_in4) gt 1 THEN BEGIN WOUT = float(WOUT)
			sz=(size(WOUT))(2)   & sc= n_elements(cal_in4)
			if sz ne sc then begin cal_in4=0 & print,' # of Angles in calibration file is different !!!' & endif
			if sz ge sc then begin sw=sz-sc & sc=0
			endif	    else begin sc=sc-sz & sw=0 & endelse
			FOR i=sw,sz-1 DO WOUT(*,i)=SHIFT(WOUT(*,i),shf_in4(i-sw+sc)) $
								  /cal_in4(i-sw+sc)
			ok=1
			if sw gt 0 then  idx=[indgen(sw),idx_in4+sw]   else $
			if sc gt 0 then  idx=idx_in4(where((idx_in4-sc) ge 0))-sc $
				   else  idx=idx_in4
			WOUT=WOUT(*,idx) & YOUT=YOUT(idx)
		ENDIF
	end
'in5':begin	IF n_elements(cal_in5) eq 0 THEN P_DID_CALOD, minst,minst+'.cal', flg
		IF n_elements(cal_in5) gt 1 THEN BEGIN WOUT = float(WOUT)
			sz=(size(WOUT))(2)   & sc= n_elements(cal_in5)
			if sz ge sc then begin sw=sz-sc & sc=0
			endif	    else begin sc=sc-sz & sw=0 & endelse
			FOR i=sw,sz-1 DO WOUT(*,i)=SHIFT(WOUT(*,i),shf_in5(i-sw+sc)) $
								  /cal_in5(i-sw+sc)
			ok=1
			if sw gt 0 then  idx=[indgen(sw),idx_in5+sw]   else $
			if sc gt 0 then  idx=idx_in5(where((idx_in5-sc) ge 0))-sc $
				   else  idx=idx_in5
			WOUT=WOUT(*,idx) & YOUT=YOUT(idx)
		ENDIF
	end
'in6':begin	IF n_elements(cal_in6) eq 0 THEN P_DID_CALOD, minst,minst+'.cal', flg
		IF n_elements(cal_in6) gt 1 THEN BEGIN WOUT = float(WOUT)
			sz=(size(WOUT))(2)   & sc= n_elements(cal_in6)
			if sz ne sc then begin cal_in6=0 & print,' # of Angles in calibration file is different !!!' & endif
			if sz ge sc then begin sw=sz-sc & sc=0
			endif	    else begin sc=sc-sz & sw=0 & endelse
			FOR i=sw,sz-1 DO WOUT(*,i)=SHIFT(WOUT(*,i),shf_in6(i-sw+sc)) $
								  /cal_in6(i-sw+sc)
			ok=1
			if sw gt 0 then  idx=[indgen(sw),idx_in6+sw]         else $
			if sc gt 0 then  idx=idx_in6(where((idx_in6-sc) ge 0))-sc $
				   else  idx=idx_in6
			WOUT=WOUT(*,idx) & YOUT=YOUT(idx)
		ENDIF
	end
ELSE:
ENDCASE
RETURN
END


pro skipline , in,line,cnt,nvers=nvers
;** ********
;**
	ON_IOERROR,nocnt
	cnt=0L & bid=0L & READS,line+' 0 0 0',cnt,bid,nvers
	IF bid gt 0 THEN FOR i=1,bid DO READF,in,line
	nocnt:
RETURN
END

pro skipvpar ,vpb,vpara,in
;** ********
;**
IF vpb gt 0 THEN BEGIN deco=''
		 FOR I=0,vpb-1 DO BEGIN
			 READF,in,deco
			 READF,in,deco     & SKIPLINE,in,deco,vpb
			 vpara=fltarr(vpb) & READF,in,vpara
		 ENDFOR
ENDIF
RETURN
END


FUNCTION rdid , INST_GRP,PATH,FILENAME,STATUS,DATP ,COMP=comp
;******* ****
;**
common calibration
common calibrotion
common c_rdid , dzap, pzap, pzip ,pzup

;help, INST_GRP,PATH,FILENAME,STATUS,DATP ,COMP
;print,INST_GRP,PATH,FILENAME,STATUS,DATP

IF n_elements(PATHCAL )   eq 0 then begin PATHCAL =sys_dep('INSUB',P_LAMBDA(0),'CALIBRATION')
					  bid=findfile(pathcal+'*',count=nn) & if nn eq 0 then pathcal='' & endif
IF n_elements(INST_GRP)   le 1 then return,1
CATCH,stat & if stat ne 0 then begin CATCH,/cancel & print,!err_string
				     FREE_LUN,in   & RETURN, WOUT & endif

WOUT=0 & WT='' & DATE='' & OT='' & XT='' & YT='' & ZT='' & PP=0 & PTXT=''
WOU =0 & XX=0  & YY  =0  & ZZ=0  & NN=0  & PV=0  & EE=0

scan=0 & Vri=-1 & text='' & exper='' & partx='' & param=0  & parai=0 & vparm=0
pthv=PATH
INST=strlowcase(INST_GRP)
IF n_elements(FILENAME) gt 1 THEN BEGIN NIMG=fix(FILENAME(1)) & FILENAME=FILENAME(0)
			     ENDIF ELSE NIMG=0

IF (INST(0) EQ 'd16') OR   (INST(0) EQ 'db21') THEN INST(1) ='dif'
IF  INST(3) EQ '1'  THEN IF INST(1) EQ 'dif'   THEN IF INST(0) NE 'd1a' THEN  $
						    IF INST(0) NE 'd2b' THEN  $
						    IF INST(0) NE 'd10' THEN  $
						    IF strpos(pthv,INST(0)) gt 0 THEN $
		    pthv=pthv+INST(0)+'_'+strmid(FILENAME,1,1)+sys_dep('DIVIDER')
IF  INST(0) EQ 'd11tof' THEN BEGIN P=strpos(pthv,'tof')
				   if P gt 0 THEN pthv=strmid(pthv,0,P)+'/' & ENDIF
;Compressed or not!
;---------- -- ---
form=findfile(pthv+FILENAME+'.Z',count=cprs)
IF cprs GT 0  THEN BEGIN
	IF pthv NE '' THEN bid=sys_dep      ('COPY',FILENAME+'.Z',pthv) ELSE cprs=0
			   bid=sys_dep      ('UN_Z',FILENAME+'.Z')       &   pthv=''
ENDIF

ON_IOERROR,misopen
STATUS=11
in=125
GET_LUN,in & if in ge 125 then for i=125,128 do free_lun,i
OPENR,in,pthv+FILENAME

	STATUS=13
	line  =''   & deco='' & form ='' & cnt =0L & bid=0L & numor=0L & vpb=0L
	linet =[''] & nvers=0 & formb='' & cntb=0L
	ON_IOERROR,misread
	
	READF,in,line     & READF,in,deco   & READF,in,line
	SKIPLINE,in,deco,numor,nvers=nvers

;	Contact and date
;	------- --- ----	
	READF,in,deco  & cnt=80
	SKIPLINE,in,deco,cnt
	nbl  =cnt/80      & IF nbl*80 lt cnt  THEN  nbl=nbl+1
	text =strarr(nbl) & READF,in,text   & READF,in,form
	form =strmid(form,0,2)
     	insv =strlowcase(strtrim(strmid(text(0),0,4),2))
	if (inst(0) eq 'd9') or (inst(0) eq 'd10') then insv=inst(0)  ;!!!!!!! to comment in some days
	if (inst(0) eq 'd11tof') then insv=inst(0)

	IF  form ne 'VV'  THEN BEGIN
	 WHILE form ne 'AA' DO BEGIN READF,in,form & form=strmid(form,0,2)
	 ENDWHILE

;	 Experiment
;	 ----------	
	 READF,in,deco     & SKIPLINE,in,deco,cnt
	 nbl  =cnt/80      & IF nbl*80 lt cnt  THEN  nbl=nbl+1
	 exper=strarr(nbl) & READF,in,exper  & READF,in,form
	 form =strmid(form,0,2)
	 WHILE form lt 'AA' DO BEGIN READF,in,form & form =strmid(form,0,2) & ENDWHILE
	ENDIF

;	Parameters
;	----------
	WHILE (form ne 'SS') AND (form ne 'VV') DO BEGIN
		
		READF,in,deco     & SKIPLINE,in,deco,cnt
		nbl  =cnt/80      & IF nbl*80 lt cnt  THEN  nbl=nbl+1
		IF form eq 'FF'  THEN BEGIN
 				      IF n_elements(par4)  gt 0 THEN par5=par4
 				      IF n_elements(par3)  gt 0 THEN par4=par3
 				      IF n_elements(par2)  gt 0 THEN par3=par2
 				      IF n_elements(par1)  gt 0 THEN par2=par1
				      IF n_elements(param) gt 1 THEN par1=param
				      param=fltarr(cnt) & ENDIF ELSE $
		IF form eq 'II'  THEN parai=lonarr(cnt)   ELSE  $
		IF form eq 'AA'  THEN bidon=strarr(nbl)
	
		IF form eq 'AA'  THEN READF,in,bidon ELSE $
		IF form eq 'II'  THEN READF,in,parai ELSE $
		IF form eq 'FF'  THEN READF,in,param
		READF,in,form
		form=strmid(form,0,2)
	
	ENDWHILE
	
	IF form eq 'SS' THEN BEGIN
;***	** **** ** **** **** ***** ---> DIF TOF
;***	** **** ** **** **** ***** ---> DIF TOF
;***	** **** ** **** **** ***** ---> DIF TOF

;	# scans
;	-------
	
	READF,in,deco     & READS,deco+' 0 0 0 0 0 0',bid,scan,bid,bid,bid,vpb
	SKIPVPAR,vpb,vpara, in
	READF,in,form     & form=strmid(form,0,2)
	IF form eq 'JJ' THEN form='II'
	IF scan lt  1   THEN scan=1 ELSE scan=scan+1

	READF,in,deco     & SKIPLINE,in,deco,cnt
	
	forcl=1 & forcd=0 & m19=0
	
	CASE insv OF
	'd19' : begin   vp=10 & forcl=0
		IF (scan gt 1) and (cnt eq 512.*16) THEN BEGIN  forcd=1
		    bad_d=intarr(512) & m19=2*512 & cnt=cnt-m19 & ENDIF
	        end
	'd9'  : begin   vp=10 & forcl=0 & end
	'db21': begin   vp=10 & forcl=0 & end
	'd16' : begin   vp=10 & end
	'd1b' : begin   vp=3  & end
	else  : begin   vp=0  & end
	ENDCASE

	IF vpb gt 0 THEN vp=0

	cnt  =  cnt-vp
	IF cnt  ge 1 THEN BEGIN
	IF vp   gt 0 THEN BEGIN vparm=lonarr(vp ,scan) & vpara=lonarr(vp) & ENDIF
	IF vpb  gt 0 THEN	vparm=fltarr(vpb,scan)
	IF scan gt   1    THEN $
	IF form eq 'II'   THEN buf=lonarr(cnt) $
	                  ELSE buf=fltarr(cnt)

	IF form ne 'II'   THEN WOUT=fltarr(cnt,scan) $
	                  ELSE IF   forcl  eq 0 THEN $
			           WOUT=intarr(cnt,scan) $
	                  ELSE WOUT=lonarr(cnt,scan)
;	Read data
;	---- ----
	ib=0 & scanv=scan
	FOR i=0,scanv-1 DO   BEGIN
	    IF  i gt 0  THEN BEGIN
	    	READF,in,line   & READF,in,deco
		READS,deco+' 0 0 0 0 0 0',bid,bid,bid,bid,bid,vpb
		SKIPVPAR,vpb,vpara,in
		READF,in,formb	& formb=strmid(formb,0,2) & IF formb eq 'JJ' THEN formb='II'
	    	READF,in,deco 	& SKIPLINE,in,deco,cntb & cntb=cntb-m19
		IF   cntb ne cnt+1 THEN $
		IF ((cntb ne cnt) or (formb ne form)) and (vp eq 0) then begin
				cnt=cntb & form=formb & WOU=WOUT(*,0:i-1) & ib=i  & scan=(scan-i)>1
				IF form eq 'II' THEN buf =lonarr(cnt)      ELSE buf =fltarr(cnt)
				IF form eq 'II' THEN WOUT=lonarr(cnt,scan) ELSE WOUT=fltarr(cnt,scan)
				ENDIF
	    ENDIF
	    IF vpb  gt 0 THEN   vparm(0,i)=vpara
	    IF scan le 1 THEN   BEGIN
	    			IF vp eq 0 THEN READF, in,         WOUT  $
	    			ELSE      BEGIN READF, in, vpara , WOUT
	    			                vparm(0,i)=vpara & ENDELSE
	    ENDIF	 ELSE   BEGIN
	    			IF vp eq 0 THEN BEGIN
				  IF forcd eq 1 THEN READF,in,bad_d,buf,bad_d $
				                ELSE READF,in,buf
				ENDIF      ELSE BEGIN
				  IF forcd eq 1 THEN READF,in,vpara,bad_d,buf,bad_d $
				                ELSE READF,in,vpara,buf
	    			  vparm(0,i)=vpara
				ENDELSE
	    		      WOUT(0,i-ib)=buf
	    ENDELSE
	    Vri=i+1
	ENDFOR
	STATUS=0
	ENDIF
	ENDIF ELSE $
	IF form eq 'VV' THEN BEGIN
;***	** **** ** **** **** ***** --->  TAS
;***	** **** ** **** **** ***** --->  TAS
;***	** **** ** **** **** ***** --->  TAS

	NP=0
	READF,in,line & partx=[line]   &  READF,in,line
	linec='' & step='!!'
	WHILE strpos(line,'DATA_:') lt 0 DO BEGIN
	      IF strpos(line,'COMND:') eq 0 THEN BEGIN
	      			i=strpos(strlowcase(line),' np ')
	      			IF i  gt 0 THEN READS,strmid(line,i+4,5)+' 0' ,NP
	      			IF NP eq 0 THEN NP=100
	      			linec=strmid(strtrim(strcompress(line),2),7,50)
	      			ENDIF
	      IF strpos(line,'STEPS:') eq 0 THEN $
				step=strmid(strtrim(strmid(line,6,6),2),1,6)
	      partx=[partx,line]
	      READF,in,line
	ENDWHILE
	READF,in,line & line=strtrim(strcompress(line),2)+' ' & partx=[partx,line]
			l=strlen(line) & j=0
			FOR i=0,l-1 DO IF strmid(line,i,1) eq ' ' THEN BEGIN
					  linet=[linet,strmid(line,j,i-j)] & j=i+1 & ENDIF
			linet=linet(1:*)
	partx=strtrim(partx,2)

	IF NP  GT 0 THEN BEGIN
	   pos=0L & POINT_LUN,-in,pos
	   	    READF,in,line & line=strtrim(strcompress(line),2)
	   	    POINT_LUN, in,pos
	   cnt=0 & i=0 & WHILE i ge 0 DO BEGIN cnt=cnt+1 & i=strpos(line,' ',i+1)  & ENDWHILE
	
	   ON_IOERROR,eofread
	   vparm=fltarr(cnt,NP)
	   READF,in,vparm
	   
	   IF vparm(0,NP-1) eq 100 THEN BEGIN   buf=fltarr(cnt)
	   					WHILE (1) DO BEGIN READF,in,buf
	   						     vparm=[[[vparm]],[buf]]
	   						     NP=NP+1 & ENDWHILE & ENDIF
	eofread:STATUS=0
	IF n_elements(vparm) gt 1 THEN BEGIN i=NP-1 & WHILE (i gt 0) and (vparm(0,i) eq 0) do i=i-1
						vparm=vparm(*,0:i>0)               & ENDIF
	ENDIF
	ENDIF
	
misread:FREE_LUN,in
	IF Vri gt 0 then IF Vri lt scan THEN begin scan=Vri & WOUT=WOUT(*,0:scan-1) & STATUS=0
					IF vp+vpb gt 0 THEN vparm=vparm(*,0:scan-1) & endif
IF cprs gt 0 THEN bid=sys_dep ('DELET', FILENAME)

misopen:FREE_LUN,in

IF STATUS eq 11 THEN print,'File "'+pthv+FILENAME+'" Not found  !!!'
IF STATUS eq 13 THEN print,'File "'+pthv+FILENAME+'" Read error !!!'
IF STATUS eq 0  THEN BEGIN

   CASE INST(1) OF
   'dif':  BEGIN
   	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
   	   WT  =	    strmid(text,4,10) +'  '+strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
     	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' Run ' + FILENAME    ;+' User '     +strmid(text,4,10)  
           XT  ='Detector X .'
           YT  ='Detector Y .'
           IF    scan gt 1 THEN ZT ='Points' ELSE  ZT ='Counts'
	   NN  = 1
	   ntype=0 & kctrl=0 & manip=0 & nbang=0 & icdes=[0] & ub=fltarr(3,3)
	   if n_elements(parai) gt 25 then begin
	   	ntype=parai(1)
	   	kctrl=parai(2)
	   	manip=parai(3)
	   	nbang=parai(4)<6
	   	icdes=parai(24:24+nbang+1)
	   	ub   =reform(param(8:16),3,3)
	   endif
	   IF   n_elements(param) ge 50      THEN  BEGIN
	   	   PP  = [param(3:6),param(17)   ,param(45:47),param(29),param(35:36),param(38),$
		          param(0:2),param(21:23),param(8 :16),manip ,kctrl]
	   	   PTXT= ['Phi    ','Chi    ','Omega  ','2*Theta',$
	   	   	  'Wave lenght',$
	   	   	  'Requested  Temperature  ',$
	   	   	  'Regulation Temperature  ',$
	   	   	  'Sample (K) Temperature  ',$
	   	   	  'Sample-Detector Distance',$
		   	  'Starting Angle  ',$
		   	  'Angle Variation ',$
		   	  'Preset (monitor)',$
			  'Hmin','Kmin','Lmin','Hmax or 0','Kmax or 0','Lmax or 0',$
			  'ub(1,1)','ub(1,2)','ub(1,3)','ub(2,1)','ub(2,2)','ub(2,3)','ub(3,1)','ub(3,2)','ub(3,3)',$
			  'Manip 1:gamma 2:omega 3:chi 4:phi','Kctrl 1:around hkl']
		   NN  = param(38)
	   	   ENDIF
    	   IF   n_elements(vparm) ge scan*2  THEN  NN =[vparm(0,0:scan-1),vparm(1,0:scan-1)]
    	   IF  (n_elements(vparm) ge scan*4) and (scan gt 1) and ((nbang ge 1) or (icdes(0) eq -1)) THEN BEGIN
    	           ZZ  =[ vparm(3,0:scan-1)]/1000.
     	           PV  =[ vparm(3:2+(nbang>1),0:scan-1)]/1000.
		   XT='X' & YT='Z' & ZT='Scan'
		   AX_g=fltarr(scan)+param(6)*!pi/180. & AX_o=fltarr(scan)+param(5)*!pi/180.
		   AX_c=fltarr(scan)+param(4)*!pi/180. & AX_p=fltarr(scan)+param(3)*!pi/180.
		   FOR i=0,nbang-1 do begin
		     case icdes(i) of
		     -1:begin if i eq 0 then ZT='Temperature'                           & end
		      1:begin if i eq 0 then ZT='Gamma' & AX_g=reform(PV(i,*))*!pi/180. & end
		      2:begin if i eq 0 then ZT='Omega' & AX_o=reform(PV(i,*))*!pi/180. & end
		      3:begin if i eq 0 then ZT='Chi'   & AX_c=reform(PV(i,*))*!pi/180. & end
		      4:begin if i eq 0 then ZT='Phi'   & AX_p=reform(PV(i,*))*!pi/180. & end
		      else:
		      endcase
		   ENDFOR
		   UB1=invert((ub),statub)
		   IF  statub eq 0 then begin
		                        SIN_g=2*sin(AX_g/2.)/param(17)
		      if insv eq 'd10' then COS_o=cos(AX_o-AX_g/2.) else COS_o=cos(AX_o)
		      if insv eq 'd10' then SIN_o=sin(AX_o-AX_g/2.) else SIN_o=sin(AX_o)
		      COS_c=cos(AX_c) & SIN_c=sin(AX_c)
		      COS_p=cos(AX_p) & SIN_p=sin(AX_p)
		      HMU  =[[(COS_o * COS_c * COS_p - SIN_o * SIN_p)* SIN_g] , $
		             [(COS_o * COS_c * SIN_p + SIN_o * COS_p)* SIN_g] , $
		             [(COS_o * SIN_c)* SIN_g]]
		      HKL  =transpose((UB1)##(HMU))
		      d=10000. & HKL=round(HKL*d) & HKL=HKL/d
		      PV   =[PV,HKL]
		   ENDIF
     	   ENDIF ELSE IF scan gt 1 then ZZ=indgen(scan)+1 else ZZ=0

     	   CASE insv of
     	   'd19':  BEGIN IF n_elements(cal_d19) eq 0 THEN BEGIN
     	   		 ENDIF
			 IF n_elements(WOUT) eq 64.*64*scan then $
			      IF scan le 1 THEN WOUT=reform(WOUT,64,64     ,/overwrite) $
			                   ELSE WOUT=reform(WOUT,64,64,scan,/overwrite) $
     	   		 ELSE WOUT=reform(WOUT,512  ,n_elements (WOUT)/512 ,/overwrite)
		   END
	   'd9':   BEGIN IF scan gt 1 THEN WOUT=reform   (WOUT,32,32,n_elements(WOUT)/32/32,/overwrite)$
	    	   	      	      ELSE WOUT=reform   (WOUT,32,32, /overwrite)
			 
			 IF sys_dep('VERSION') ge 4.0 then $
			 IF scan gt 1 THEN ii=execute('WOUT=transpose(WOUT,[1,0,2])') $
				      ELSE WOUT=transpose(WOUT)
		   END
	   'd10':  BEGIN IF scan gt 1 THEN WOUT=reform   (WOUT,32,32,n_elements(WOUT)/32/32,/overwrite)$
	    	   	      	      ELSE WOUT=reform   (WOUT,32,32, /overwrite)
				
			 IF sys_dep('VERSION') ge 4.0 then $
			 IF scan gt 1 THEN ii=execute('WOUT=transpose(WOUT,[1,0,2])') $
				      ELSE WOUT=transpose(WOUT)
		   END
	   'd15':  BEGIN IF scan gt 1 THEN WOUT=reform   (WOUT,32,32,n_elements(WOUT)/32/32,/overwrite)$
	    	   	      	      ELSE WOUT=reform   (WOUT,32,32, /overwrite)
			 
			 IF sys_dep('VERSION') ge 4.0 then $
			 IF scan gt 1 THEN ii=execute('WOUT=transpose(WOUT,[1,0,2])') $
				      ELSE WOUT=transpose(WOUT)
		   END
	   'd16':  BEGIN nel=n_elements(WOUT) & nxy=nel/scan & sy=nxy/64 & sx=nxy/sy
	   		 IF nxy ge 64.*64 then begin  & sy=sqrt(nxy) & sx=nxy/sy & endif
			 IF scan gt 1 THEN WOUT=reform   (WOUT,sx,sy,nel/sx/sy,/overwrite) $
	    	   	      	      ELSE WOUT=reform   (WOUT,sx,sy, /overwrite)
					   IF sy eq 16 then begin
					    WOUT=reverse  (WOUT(1:sx-2,1:sy-2,*),1) & sx=sx-2 & sy=sy-2
					   ENDIF
			 sw=size(WOUT) & if sw(0) eq 3 then sz=sz(sw(3)) else sz=1
			 IF n_elements(cal_d16) eq 0 THEN $
			    P_DID_CALOD, insv,insv+'.cal', flg

			 IF n_elements(cal_d16) gt 1 THEN BEGIN WOUT=float(WOUT)
				FOR i=0,sz-1 DO WOUT(0,0,i)=WOUT(*,*,i)/cal_d16
				OT=OT+' /'+inf_d16(0) & ENDIF
		   ZZ  =numor
		   
		   if n_elements(pzip) eq 0 then pzip=0.
		   D2TH=pzip

		   PTXT= ['2*Theta   ','Omega     ','Chi       ','Phi       ',$
	   	   	  'Trans_X   ','Trans_Y   ','Rot       ','Beamstop  ',$
                          'Wave lenght             ','Requested  Temperature  ',$
	   	   	  'Regulation Temperature  ','Sample (K) Temperature  ',$
	   	   	  'Sample-Detector Distance','Starting Angle          ',$
                          'Angle Variation         ','Angle Range             ',$
			  'PRESET                  ','Coupling factor         ',$
                          'Motor scanned           ','Nb points requested     ','Nb points saved         ',$ 
			  'Count:Monitor or Time   ','Type of T-regulation    ','Type of Multi-meter     ',$
                          'Delta2th                ']
		   PP  =[param(6),param(5),param(4),param(3),0,0,0,0,param(17),param(45:47),$
                         param(29),param(35:38),param(42),parai(3),parai(5:7),parai(12:13),pzip]
                        
                   scan_t=parai(3)
                   nbang =parai(4)
                   FOR i=1,nbang DO IF parai(23+i) eq 0 THEN parai(23+i)=24

;                  ***Lecture pour les differents motors scans*****
	 	   PP(parai(24)-1)=vparm(3)/1000.
                   IF nbang eq 1 THEN BEGIN ZZ=PP(2) & ENDIF
		   IF nbang eq 2 THEN BEGIN PP(parai(25)-1)=vparm(4)/1000. & ENDIF
                   IF nbang eq 3 THEN BEGIN PP(parai(25)-1)=vparm(4)/1000. & PP(parai(26)-1)=vparm(5)/1000. & ENDIF
                   IF parai(3) gt 0 THEN BEGIN ZZ=round(vparm(3)*100/1000.) 
		       IF ZZ   eq 0 THEN ZZ=1
                       IF parai(3) eq 1  THEN  ZT='gamma*100'   & IF parai(3) eq 2 THEN ZT='omega*100'
                       IF parai(3) eq 3  THEN  ZT='chi*100'     & IF parai(3) eq 4 THEN ZT='phi*100'
                       IF parai(3) eq 5  THEN  ZT='Trans_X*100' & IF parai(3) eq 6 THEN ZT='Trans_Y*100'
                       IF parai(3) eq 7  THEN  ZT='Rot*100'
		   ENDIF
                   IF PP(12) le 0 THEN PP(12)=100.
		   if sy gt 16 then reso=.3 else reso=.254
		   if sy gt 64 then reso=.2
		   step = 180./!pi * reso/PP(12)
		   ste  = fix(step*1000) & ste = ste/1000.
		   set_tolerance,tt,/get & if tt eq 0 then set_tolerance,ste
		   offs = sx/2 -.5
		   XX   = findgen(sx)*step + PP(0) - offs*step
		   YY   =  indgen(sy)
                   NN   = vparm(1)

;                  ****Write the file in a regular grid determined by the delta2th parameter***
                   IF D2TH gt 0.001 THEN BEGIN
                     S=ROUND((XX(n_elements(XX)-1)-XX(0))/D2TH)+2
                     WINT=fltarr(S,sy) & FRAC=0 & XR=fltarr(S) & NN=lonarr(S)
;                    ***XR contains the 2th's rounded to the nearest step size**
		     tent=round(XX(0)/D2TH)*D2TH
                     if XX(0) gt tent then XR(0)=tent else XR(0)=tent-D2TH

                     for i=1,S-1   do XR(i)=XR(i-1)+D2TH  
;                    ****How to write the numor o the regular grid XR****
                     FOR i=0,sx-1 do begin 
                      FOR J=0,S-2 do begin
                       IF (XX(i) gt XR(j)) and (XX(i) lt XR(j+1)) then begin
			   FRAC=(XX(i)-XR(j))/D2TH & FRAC1=1-FRAC
                           WINT(j,*)=FRAC1*WOUT(i,*)+WINT(j,*)	& WINT(j+1,*)=FRAC*WOUT(i,*)+WINT(j+1,*) 
                             NN(j)  =FRAC1*vparm(1) +  NN(j)	&   NN(j+1)  =FRAC*vparm(1) +  NN(j+1) 
                       ENDIF
                      ENDFOR 
                     ENDFOR
		     XX=XR & WOUT=WINT 
                     EE = SQRT(WOUT)		; *** EE the error
                   ENDIF
                   XX=round(XX*10000) & XX=XX/10000.
           	   XT   ='2*theta'
		   END
	   'db21': BEGIN WOUT=reform(WOUT,128 ,128,/overwrite)
	   	   END
	   'd1b':  BEGIN
           	   XT  ='2*Theta'
           	   YT  ='Temperature'
           	   XX  =findgen(n_elements(WOUT)) * 79.8/(n_elements(WOUT)-1)+PP(3)
           	   YY  =param(46)
	   	   END
	   'd1a':  BEGIN nd=25. & IF n_elements(cal_d1a) eq 0 THEN $
				     P_DID_CALOD, insv,insv+'.cal', flg
		   nj  = n_elements(WOUT)/(nd+4)
		   if long(nj) ne nj then RETURN,0
	   	   WOUT= reform(WOUT,(nd+4),nj ,/overwrite)
		   NN  = WOUT(0,0)
		   YY  = reform(WOUT(2,*))/1000.
   	   	   YT  ='Counts'
		   WOUT= WOUT(4:*,*)
		   IF n_elements(cal_d1a) eq nd THEN BEGIN wout=float(wout) &  FOR i=0,nj-1 $
						        DO wout(0,i)=wout(*,i)/cal_d1a & fct=1.
							OT=OT+' /'+inf_d1a(0)
		   ENDIF ELSE fct = -6.

		   XX  =fct*ang_d1a+YY(0)
		   WOUT= reform(WOUT,nd*nj,/overwrite)
		   IF nj gt 1 then BEGIN FOR i=1,nj-1 do  XX=[XX , fct*ang_d1a+YY(i)]
		   			 idx=sort (XX) &  XX =XX(idx) & WOUT=WOUT(idx)
		   			 XX =round(XX/0.05) & XX=XX*0.05
		   			 YY =param(46)
		   		   ENDIF
           	   XT  ='2*Theta'
	   	   END
	   'd2b':  BEGIN
;			*** initialize parameters ***
			nd=64. & D2TH = param(36) 
		        if n_elements(dzap) eq 0 then dzap=0
		        if n_elements(pzap) eq 0 then pzap=0
;			*** read calibration file (if present)
			IF n_elements(cal_d2b) eq 0 THEN $
			   P_DID_CALOD, insv,insv+'.cal', flg

;		       *** nj = number of points ***
		   vp  = parai(4)+2
		   nj  = n_elements(WOUT)/(nd+vp)
;		       *** rewrite WOUT in matrix form (69 cols, nj rows) ***
	   	   WOUT= reform(WOUT,(nd+vp),nj ,/overwrite)
		   NN  = WOUT(0,0)
;			*** YY = 2th values for detector 1 ***
		   YY  = reform(WOUT(2,*))/1000.
;			*** set titles ***
   	   	   YT  ='Counts'
		   XT  =' 2*Theta' 
;			*** reform WOUT to eliminate first vp values (not data) ***
		   WOUT= WOUT(vp:*,*)

;		   *** detector zapping algorithm ***
;		   *** definition of new calibration arrays (default: equal to old)
		   newcal_d2b=cal_d2b
		   newang_d2b=ang_d2b
		   newd=nd
;		   *** test if dzap contains real detectors **
		   idz=where(dzap ge 1 and dzap le 64)
;		   *** in case, eliminate spurious values ***
		   if idz(0) ne -1 then dzap=dzap(idz) else dzap=0
;		   *** is there something left? ***
		   SD=SIZE(dzap)

;		   *** test if pzap contains real points **
		   idz=where(pzap ge 1 and pzap le nj)
;		   *** in case, eliminate spurious values ***
		   if idz(0) ne -1 then pzap=pzap(idz) else pzap=0
;		   *** is there something left? ***
		   SP=SIZE(pzap)

;		   *** if dzap has something, do the following ***
		   if SD(0) gt 0 then BEGIN
;		     *** set all dud values and calibration to -999 ***
		     WOUT(dzap-1,*)=-999
		     newcal_d2b(dzap-1)=-999
;		     *** good values are those which do not contain -999 ***
		     GOOD=where(WOUT ne -999)
		     if GOOD(0) eq -1 then return, GOOD(0)

;		     *** cut out dud values from WOUT and calibrations ***
		     WOUT=WOUT(GOOD)
		     newang_d2b=ang_d2b(where(newcal_d2b ne -999))
		     newcal_d2b=cal_d2b(where(newcal_d2b ne -999))
;		     *** redefine the detector number
		     newd=(nd-n_elements(dzap))
;		     *** reform WOUT to proper format
		     WOUT=reform(WOUT,newd,nj ,/overwrite)
		   ENDIF

;		   *** if pzap has something, do the following ***
		   if SP(0) gt 0 then BEGIN
;		     *** set all dud values -999 ***
		     WOUT(*,pzap-1)=-999
;		     *** good values are those which do not contain -999 ***
		     GOOD=where(WOUT ne -999)
;		     *** cut out dud values from WOUT ***
		     WOUT=WOUT(GOOD)
;		     *** redefine the number of points
		     nj=(nj-n_elements(pzap))
;		     *** reform WOUT to proper format
		     WOUT=reform(WOUT,newd,nj ,/overwrite)
		   ENDIF
;			*** if calibration available, calibrate by dividing ***
		   
		   IF n_elements(cal_d2b) eq nd THEN BEGIN wout=float(wout) &  FOR i=0,nj-1 $
							DO wout(0,i)=wout(*,i)/newcal_d2b & fct=1.
							OT=OT+' /'+inf_d2b(0)
;		        *** fct*newang_d2b always newd values spaced by ~ -2.5 ***
		   ENDIF ELSE fct = -2.5
;			*** XX array of initial 2th for all detectors ***
		   XX  =fct*newang_d2b+YY(0)
;			*** reform WOUT to be a 1-line vector ***
		   WOUT= reform(WOUT,newd*nj,/overwrite)
;			*** create a XX array by adding up all 2th for all dets **
;			*** in the same order as in WOUT


;			*** do the following if there are at least 2 points ***
		   IF nj gt 1 then BEGIN FOR i=1,nj-1 do  XX=[XX , fct*newang_d2b+YY(i)]
;			*** sort the XX and WOUT arrays (idx is the index array) ***
		   			 idx=sort (XX) &  XX =XX(idx) & WOUT=WOUT(idx)
;			*** XR contains the 2th's rounded to the nearest step size ***
		   			 XR =round(XX/D2TH) & XR=XR*D2TH
;			*** GRID contains the fractional indices of XR into XX ***					 
					 GRID=(XR-XX)/D2TH+findgen(n_elements(XX))				
;			*** WINT is WOUT interpolated onto the fractional indices *** 
					 WINT=interpolate(WOUT,GRID)
;			*** reassign XX and WOUT
					 XX=XR
				         WOUT=WINT
;			*** YY is the temperature, EE the error
		   			 YY =param(46)
;No error for Emanuelle			 EE = SQRT(WOUT)
		   		   ENDIF
	   	   END
		   
	'd20': rdid_d20, INST,numor,nvers,text,exper,scan,cnt,WOUT,vparm,param,par1,par2,par3,par4,par5,$
	                 WT ,XT ,YT ,ZT ,OT ,DATE ,PP ,PTXT ,XX ,YY ,ZZ ,NN ,PV ,EE

	
	   else:
	   ENDCASE
	   END

   '3axes':BEGIN
   	   DATE=strmid   (partx(5),7,9)
   	   WT  =strmid(partx(2),7,11) +' '+linec
   	   OT  =insv +' '+partx(5)+' '+partx(2)+' Run ' + FILENAME
   	   XT  =strcompress(strmid(partx(6),7,69))
   	   YT  ='CNTS'
   	   PP  =fltarr(n_elements(partx))
   	   PTXT=partx
   	   PV  =vparm
   	   
   	   idx =where(linet eq 'CNTS') & idx=idx(0)
   	   IF idx ge 0 THEN IF idx lt cnt THEN WOUT=reform(vparm(idx,*)) ELSE WOUT=reform(vparm)
	   
   	   idx =where(linet eq 'M1')   & idx=idx(0)
   	   IF idx ge 0 THEN IF idx lt cnt THEN NN  =reform(vparm(idx,*))

   	   idx =where(linet eq step)   & idx=idx(0)
   	   IF idx ge 0 THEN IF idx lt cnt THEN BEGIN XX  =reform(vparm(idx,*))
   	   					     id  =sort(XX)
   	   					     XX  =XX  (id)
   	   					     WOUT=WOUT(id)
   	   					     IF n_elements(NN) gt 1  THEN  NN=NN(id)
   	   					     XT  =XT+' unit= '+step  &  ENDIF
	   EE  =sqrt(WOUT)
   	   END
   'tof':  BEGIN nd =0
   	   nb_chn=(SIZE(WOUT))(1)
   	   nb_spc=(SIZE(WOUT))(2)
	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
	   WT  =	                            strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' User '     +strmid(text,4,10)  +' Run ' + FILENAME
	   XT  ='Channels'
	   YT  ='Spectrum'
	   ZT  ='Numor' & ZZ=numor

     	   CASE insv of
	   
           'd7':   rdid_d7, INST,numor,nvers,text,exper,scan,cnt,WOUT,vparm,param,par1,par2,par3,par4,par5,$
	                    WT ,XT ,YT ,ZT ,OT ,DATE ,PP ,PTXT ,XX ,YY ,ZZ ,NN ,PV ,EE
     	   'in16': BEGIN IF n_elements(WOUT) gt 1 THEN BEGIN Helas=0
			        IF n_elements(WOU) eq 1 THEN WOU=WOUT else Helas=1
				nb_chn=par1(6)          & nb_spc=par1(7)
				nb_mon=par1(8)          & md_pos=par1(64)
				n_md  =20               & ndind =nb_spc
				YY    =fltarr(nb_spc)
				NN    =WOU (0:nb_chn-1,nb_spc:nb_spc+nb_mon-1)>1
				PP    =[par1 (14),par1 (6 ),par1 (7 ),par1 (8 ),par1 (15),0.       ,$
				        0.       ,par1 (69),par1 (79),par1 (9 ),par1 (20),par1 (83),$
				        par1 (59),par1 (60),par1 (13),0.       ,0.       ,0.       ,$
				        0.       ,par1 (64),param(20),param(21),param(22),param(23),$
				        param(24),param(25),param(26),param(27),param(0 ),$
				        param(1 )-param(0 ),n_md]
				PTXT  =['Type of scan (index)            ','Number of channels              ',$
				        'Number of detectors             ','Number of monitors              ',$
				        'Mesuring time per step (seconds)','Not used                        ',$
				        'Not used                        ','Monochromator d-spacing (ang.)  ',$
				        'Analyser      d-spacing (ang.)  ','Average sample temperature  (K) ',$
				        'Deflector Chopper frequency (Hz)','Number of dead channels         ',$
				        'T1 (microsec.)                  ','T2 (microsec.)                  ',$
				        '1 if diffraction detector used  ','Not used                        ',$
				        'Not used                        ','Not used                        ',$
				        'Not used                        ','MD position                     ',$
				        'Single scattering angle 1 (deg.)','Single scattering angle 2 (deg.)',$
				        'Single scattering angle 3 (deg.)','Single scattering angle 4 (deg.)',$
				        'Single scattering angle 5 (deg.)','Single scattering angle 6 (deg.)',$
				        'Single scattering angle 7 (deg.)','Single scattering angle 8 (deg.)',$
				        'First angle MD-tube       (deg.)','Angle increment MD        (deg.)',$
				        'Number of MD-tubes              ']
				IF (nb_spc ge n_md)  THEN BEGIN
				  ndind = nb_spc - n_md
				  if ndind   gt   0  THEN YY(0    :ndind-1)     = param(n_md:n_md+ndind-1)
				  OFS=((md_pos-1)>0)*.5 & YY(ndind:ndind+n_md-1)= param(0:n_md-1)+OFS
				ENDIF   ELSE              YY(0    :ndind-1)     = param(n_md:n_md+ndind-1)

				IF par1(14) eq 0         THEN BEGIN		;*** DOPLER **
				  PP(2)=par1(2) & PP(3)=par1(7) & PP(4)=par1(8)
				  PP(5)=par1(0) & PP(6)=par1(1)
				  PTXT(2)='Average Doppler frequency       ' & PTXT(3)='Number of detectors             '
				  PTXT(4)='Number of monitors              ' & PTXT(5)='Duration of scan (seconds)      '
				  PTXT(6)='Counts in Monitor 1             '
				  XX=indgen(nb_chn)+1
				  ENDIF
				IF par1(14) eq 1         THEN BEGIN		;*** ELASTIC SCAN sample_t **
				  XX=WOU (0:nb_chn-1,nb_spc+nb_mon)/1000.
				  ENDIF
				IF par1(14) ge 2         THEN BEGIN		;*** ANGLE SCAN **
				  XX=WOU (0:nb_chn-1,nb_spc+nb_mon)/100.
				  ENDIF
				WOU =WOU (0:nb_chn-1,0:nb_spc-1)
				IF (nb_spc gt n_md)      THEN BEGIN
				  WOT =WOU
				  WOU (*,0:ndind-1)=WOT(*,n_md:n_md+ndind-1)	; Normal detectors
				  WOU (*,ndind:n_md+ndind-1)=WOT(*,0:n_md-1)	; Multi  detectors  
				  ENDIF
				if (INST(2) eq '0')      THEN BEGIN
				  OT     =OT + " Normalized"
				  moni   =round(total(NN(*,0)))/nb_chn & mona=moni/(NN(*,0)>1)
				  FOR  i =0,nb_spc-1 do WOU (*,i)=WOU (*,i)*mona
				  NN(*,0)=moni
				  ENDIF
				if (Helas)  THEN BEGIN ;par1(13)=1
				  OT = OT + " (Diff. on)"
				  PTXT(15) = 'Angle increment between detect. ' & PP(15)=par1(18)
				  PTXT(16) = 'Vert. angle wrt scattering plane' & PP(16)=par1(19)
				  PTXT=[PTXT,'Zero point (deg) of 1st det. Blk',$
				             'Zero point (deg) of 2nd det. Blk',$
				             'Zero point (deg) of 3rd det. Blk',$
				             'Zero point (deg) of 4th det. Blk',$
				             'Zero point (deg) of 5th det. Blk']
				  PP  =[PP  , par1(120),par1(121),par1(122),par1(123),par1(124)]
				  ENDIF
				YT='2-Theta (deg.)'
				CASE par1(14) of
				0:  BEGIN XT='Channels'                 & END
				1:  BEGIN XT='T/K'                      & END
				2:  BEGIN XT='Gamma-1 deg.'             & END
				3:  BEGIN XT='Gamma-2 deg.'             & END
				4:  BEGIN XT='THETA-S deg.'             & END
				5:  BEGIN XT='Sample Height'            & END
				6:  BEGIN XT='Theta-ANALYSER'           & END
				7:  BEGIN XT='2*Theta-ANALYSER'         & END
				8:  BEGIN XT='Theta-Monochromator deg.' & END
				9:  BEGIN XT='Gamma-Monochromator'      & END
				10: BEGIN XT='Theta-D1 deg.'            & END
				11: BEGIN XT='CD1'                      & END
				12: BEGIN XT='Theta-D2 deg.'            & END
				ELSE:
				ENDCASE
			
				IF n_elements(dzap) ne 1 THEN dzap=0 & if dzap eq 0 then Helas=0
				IF not Helas then WOUT=float(WOU) $	;*** INELASTIC **
				ELSE BEGIN			        ;*** MULTI DETECTOR **
				  sz  =size(WOUT)
				  XT  ='Angles' & YT='Counts'
				  a   =par1(18)
				  XX  =[findgen(32)*a+par1(120),findgen(32)*a+par1(121),findgen(32)*a+par1(122),$
				        findgen(32)*a+par1(123),findgen(32)*a+par1(124)]
				  IF  sz(0) ge 2 THEN BEGIN YT ='Scan set'
					YY=indgen(sz(2))+1 & ENDIF
				  PV  =WOU
				  WOUT=REVERSE(WOUT,1)
				ENDELSE
	                 ENDIF
	           END
     	   'in10': BEGIN
			mon =par1 (20)>1
			chn =par1 (23)
			spc =par1 (19)
			NN  =WOUT (0:chn-1,spc:spc+mon-1)
			YY  =param (0:spc-1)
			
				CASE par1(21) of
				0:  BEGIN XT='Channels'           & XX  =indgen(chn)+1                    & END ; DOPLER **
				1:  BEGIN XT='T/K'                & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END ; ELASTIC SCAN **
				2:  BEGIN XT='T/K'                & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END ; ELASTIC SCAN **
				3:  BEGIN XT='CHI_M  (deg.)'      & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END ; ANGLE SCAN **
				4:  BEGIN XT='THETA_M2  (deg.)'   & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END ; ""
				5:  BEGIN XT='THETA_M1  (deg.)'   & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				6:  BEGIN XT='CHI_E1  (deg.)'     & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				7:  BEGIN XT='CHI_E2  (deg.)'     & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				8:  BEGIN XT='THETA_G  (deg.)'    & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				9:  BEGIN XT='CHI_G  (deg.)'      & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				10: BEGIN XT='2THETA_G  (deg.)'   & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				11: BEGIN XT='OMEGA_E1  (deg.)'   & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				12: BEGIN XT='OMEGA_E2  (deg.)'   & XX  =WOUT (0:chn-1,nb_spc-1)/par1(22) & END
				13: BEGIN XT='Spectrum number'    & XX  =WOUT (0:chn-1,nb_spc-1)/1000.0   & END ; Monochromator-T **
				ELSE:
				ENDCASE
				
			WOUT=FLOAT(WOUT (0:chn-1,0:spc-1))
			PTXT=['Type of scan (index)                 =' , 'Duration of scan (seconds)           =']
			
			IF par1(21) eq 0 then begin
			 PTXT=[ PTXT,$
			'Max. Doppler frequency (Hz)          =' , 'Min. Doppler frequency (Hz)          =', +$
			'Chopper frequency (RPM)              =' , 'Lattice param. Monochromator (ang.)  =', +$
			'Lattice param. Analyser      (ang.)  =' , 'Lattice param. Deflector     (ang.)  =', +$
			'TOF Sample-Det. (microsec.)          =' , 'TOF Sample-M1   (microsec.)          =', +$
			'TOF Sample-M2   (microsec.)          =' , 'T1              (microsec.)          =', +$
			'T2              (microsec.)          =' , 'Number of detectors in use           =', +$
			'Number of monitors  in use           =' , 'Channel limit                        =', +$
			'Number of points in spectrum         =' , 'Scattering angle 1 (2*theta) (deg.)  =', +$
			'Scattering angle 2 (2*theta) (deg.)  =' , 'Scattering angle 3 (2*theta) (deg.)  =', +$
			'Scattering angle 4 (2*theta) (deg.)  =' , 'Scattering angle 5 (2*theta) (deg.)  =', +$
			'Scattering angle 6 (2*theta) (deg.)  =' , 'Scattering angle 7 (2*theta) (deg.)  =', +$
			'Scattering angle 8 (2*theta) (deg.)  =' , 'Deflector  angle Theta-g     (deg.)  =', +$
			'Not used =' , 'Not used =' , 'Not used =' , 'Not used =' , 'Not used =']
			 PP=[ $
			 par1 (21),par1 (0 ),par1 (1 ),par1 (2 ),par1 (3 ),par1 (81),par1 (83),par1 (82), +$
			 par1 (84),par1 (85),par1 (86),par1 (87),par1 (88),par1 (19),par1 (20),par1 (6 ), +$
			 par1 (23),param(0 ),param(1 ),param(2 ),param(3 ),param(4 ),param(5 ),param(6 ), +$
			 param(6 ),par1 (50),0. ,0. ,0. ,0. ,0. ]
			ENDIF
			
			IF (par1(21) ge 1) and (par1(21) le 12) then begin
			 PTXT=[ PTXT,$
			'Chopper frequency (RPM)              =' , 'Lattice param. Monochromator (ang.)  =', +$
			'Lattice param. Analyser      (ang.)  =' , 'Lattice param. Deflector     (ang.)  =', +$
			'TOF Sample-Det. (microsec.)          =' , 'TOF Sample-M1   (microsec.)          =', +$
			'TOF Sample-M2   (microsec.)          =' , 'T1              (microsec.)          =', +$
			'T2              (microsec.)          =' , 'Number of detectors in use           =', +$
			'Number of monitors  in use           =' , 'Number of points in spectrum         =', +$
			'Not used =' , 'Not used =' , 'Not used =' , 'Not used =' , 'Not used =']
			 PP=[ $
			 par1 (21),par1 (0 ),par1 (3 ),par1 (81),par1 (83),par1 (82),par1 (84),par1 (85), +$
			 par1 (86),par1 (87),par1 (88),par1 (19),par1 (20),par1 (23),0. ,0. ,0. ,0. ,0. ]
			ENDIF
			
			IF par1(21) eq 13 then begin
			 PTXT=[ PTXT,$
			'Chopper frequency (RPM)              =' , 'Monochromator coeff. A0              =', +$
			'Monochromator coeff. A1              =' , 'Monochromator coeff. A2              =', +$
			'Monochromator coeff. A3              =' , 'Monochromator coeff. B0              =', +$
			'Monochromator coeff. B1              =' , 'Monochromator coeff. B2              =', +$
			'Monochromator coeff. B3              =' , 'Coeff. transition  temperature (K)   =', +$
			'Max. monochromator temperature (K)   =' , 'Lattice param. Analyser      (ang.)  =', +$
			'Lattice param. Deflector     (ang.)  =' , 'TOF Sample-Det. (microsec.)          =', +$
			'TOF Sample-M1   (microsec.)          =' , 'TOF Sample-M2   (microsec.)          =', +$
			'T1              (microsec.)          =' , 'T2              (microsec.)          =', +$
			'Number of detectors in use           =' , 'Number of monitors  in use           =', +$
			'Number of points in spectrum         =' , 'Scattering angle 1 (2*theta) (deg.)  =', +$
			'Scattering angle 2 (2*theta) (deg.)  =' , 'Scattering angle 3 (2*theta) (deg.)  =', +$
			'Scattering angle 4 (2*theta) (deg.)  =' , 'Scattering angle 5 (2*theta) (deg.)  =', +$
			'Scattering angle 6 (2*theta) (deg.)  =' , 'Scattering angle 7 (2*theta) (deg.)  =', +$
			'Scattering angle 8 (2*theta) (deg.)  =' ]
       			 PP=[ $
			 par1 (21),par1 (0 ),par1 (3 ),par1 (66),par1 (67),par1 (68),par1 (69),par1 (70), +$
			 par1 (71),par1 (72),par1 (73),par1 (74),par1 (75),par1 (83),par1 (82),par1 (84), +$
			 par1 (85),par1 (86),par1 (87),par1 (88),par1 (19),par1 (20),par1 (23),param(0 ), +$
			 param(1 ),param(2 ),param(3 ),param(4 ),param(5 ),param(6 ),param(7 )]
			ENDIF
			if (INST(2) eq '0')  THEN BEGIN
				OT     =OT + " Normalized"
				moni   =round(total(NN(*,0))/chn) & mona=moni/(NN(*,0)>1)
				FOR  i =0,spc-1 do WOUT (*,i)=WOUT (*,i)*mona
				NN(*,0)=moni
				ENDIF
	           END
     	   'in4':  BEGIN an=strmid(DATE,6,3) & if strpos(an,'-') eq 0 then an=strmid(an,1,2)
	   		if (an gt '75') and (an lt '91') then off=2 else off=16
			NN  =      WOUT(0:nb_chn-2,0:1)
			WOUT=FLOAT(WOUT(0:nb_chn-2,off:nb_spc-1))
			XX  =indgen(nb_chn-1)+1
			YY  =par1  (31+off:31+nb_spc-1)
			PTXT=[ $
			'Run duration (seconds)             =' , 'Time in 0.1 seconds                =', +$
			'Number of reserved channels        =' , 'Counts in M1                       =', +$
			'Counts in M2                       =' , 'Not used                           =', +$
			'Total counts                       =' , 'Not used                           =', +$
			'Sample angle (deg.)                =' , 'Elastic peak position (channel)    =', +$
			'Numor                              =' , 'Sample temperature (K)             =', +$
			'Digital Voltmeter Reading          =' , 'Repetition period (microsec.)      =', +$
			'Multiplier for repetition period   =' , 'Not used                           =', +$
			'Not used                           =' , 'Not used                           =', +$
			'Channel width (microsec.)          =' , 'Number of channels used            =', +$
			'TOF delay (microsec.)              =' , 'Wavelength (angstroms)             =', +$
			'Distance CH4 - M1     (meter)      =' , 'Distance CH4 - Sample (meter)      =', +$
			'Not used                           =' , 'Distance M1  - M2     (meter)      =', +$
			'Not used                           =' , 'Distance Det - Sample (meter)      =', +$
			'Contents scaler 1                  =' , 'Contents scaler 2                  =', +$
			'Number of angles                   =' ]
       			 PP=[ $
			 par1 (2 ),par1 (5 ),param(1 ),par1 (3 ),par1 (4 ),  0.     ,par1 (7 ),  0.     , +$
			 par1 (17),param(8 ),param(9 ),param(10),par1 (15),param(12),param(13),  0.     , +$
			   0.     ,   0.    ,param(17),nb_chn-1 ,param(19),param(20),param(21),param(22), +$
			   0.     ,param(24),   0.    ,param(26),par1 (0 ),par1 (1 ),nb_spc-off]
			   
			if (INST(2) eq '0') THEN begin	P_DID_CALDO, insv,WOUT,YY,ok
							if ok then begin  XT=XT+' pre-Calibrated'
								monu= 500000.
								tot = monu/total(NN(*,0))
								WOUT= WOUT*tot
								NN(*,0) =0 & NN(0,0)=monu & endif & endif
	           END
     	   'in5':  BEGIN off=8
	   		IF n_elements(par2) eq 0 then begin par2=par1 & par1=param & off=6 & endif
			NN  =      WOUT(0:nb_chn-2,0:2)
			WOUT=FLOAT(WOUT(0:nb_chn-2,off:nb_spc-1))
			XX  =indgen(nb_chn-1)+1
			YY  =par2  (31+off:31+nb_spc-1)
			PTXT=[ $
			'Run duration (seconds)             =' , 'Time in 0.1 seconds                =', +$
			'Number of reserved channels        =' , 'Counts in M1                       =', +$
			'Counts in M2                       =' , 'Counts in M3                       =', +$
			'Total counts                       =' , 'Not used                           =', +$
			'Sample angle (deg.)                =' , 'Elastic peak position (channel)    =', +$
			'Numor                              =' , 'Sample temperature (K)             =', +$
			'Digital Voltmeter Reading          =' , 'Repetition period (microsec.)      =', +$
			'Multiplier for repetition period   =' , 'Not used                           =', +$
			'Not used                           =' , 'Not used                           =', +$
			'Channel width (microsec.)          =' , 'Number of channels used            =', +$
			'TOF delay (microsec.)              =' , 'Wavelength (angstroms)             =', +$
			'Distance CH4 - M1     (meter)      =' , 'Distance CH4 - Sample (meter)      =', +$
			'Not used                           =' , 'Distance M1  - M2     (meter)      =', +$
			'Distance M1  - M3     (meter)      =' , 'Distance Det - Sample (meter)      =', +$
			'Contents scaler 1                  =' , 'Contents scaler 2                  =', +$
			'Number of angles                   =' ]
       			 PP=[ $
			 par2 (2 ),par2 (5 ),par1 (1 ),par2 (3 ),par2 (4 ),par2 (6 ),par2 (7 ),  0.     , +$
			 par2 (17),par1 (8 ),par1 (9 ),par1 (10),par2 (15),par1 (12),par1 (13),  0.     , +$
			   0.     ,   0.    ,par1 (17),nb_chn-1 ,par1 (19),par1 (20),par1 (21),par1 (22), +$
			   0.     ,par1 (24),par1 (25),par1 (26),par2 (0 ),par2 (1 ),nb_spc-off]
			   
			if (INST(2) eq '0') THEN begin	P_DID_CALDO, insv,WOUT,YY,ok
							if ok then begin  XT=XT+' pre-Calibrated'
								monu= 500000.
								tot = monu/total(NN(*,0))
								WOUT= WOUT*tot
								NN(*,0) =0 & NN(0,0)=monu & endif & endif
	           END
     	   'in6':  BEGIN if nb_spc gt 300 then begin off=3  & mom=3
	                            endif else begin off=21 & mom=4 & endelse
			 if nb_spc lt 100 then off=6
			NN  =      WOUT(0:nb_chn-2,0:mom-1)
			WOUT=FLOAT(WOUT(0:nb_chn-2,off:nb_spc-1))
			XX  =indgen(nb_chn-1)+1
			YY  =par1  (31+off:31+nb_spc-1)
			PTXT=[ $
			'Run duration (seconds)             =' , 'Time in 0.1 seconds                =', +$
			'Number of reserved channels        =' , 'Counts in M1                       =', +$
			'Counts in M2                       =' , 'Counts in M3                       =', +$
			'Total counts                       =' , 'Not used                           =', +$
			'Sample angle (deg.)                =' , 'Elastic peak position (channel)    =', +$
			'Numor                              =' , 'Sample temperature (K)             =', +$
			'Digital Voltmeter Reading          =' , 'Repetition period (microsec.)      =', +$
			'Multiplier for repetition period   =' , 'Not used                           =', +$
			'Not used                           =' , 'Not used                           =', +$
			'Channel width (microsec.)          =' , 'Number of channels used            =', +$
			'TOF delay (microsec.)              =' , 'Wavelength (angstroms)             =', +$
			'Distance CH4 - M1     (meter)      =' , 'Distance CH4 - Sample (meter)      =', +$
			'Not used                           =' , 'Distance M1  - M2     (meter)      =', +$
			'Distance M1  - M3     (meter)      =' , 'Distance Det - Sample (meter)      =', +$
			'Contents scaler 1                  =' , 'Contents scaler 2                  =', +$
			'Number of angles                   =' ]
       			 PP=[ $
			 par1 (2 ),par1 (5 ),param(1 ),par1 (3 ),par1 (4 ),par1 (6 ),par1 (7 ),  0.     , +$
			 par1 (17),param(8 ),param(9 ),param(10),par1 (15),param(12),param(13),  0.     , +$
			   0.     ,   0.    ,param(17),nb_chn-1 ,param(19),param(20),param(21),param(22), +$
			   0.     ,param(24),param(25),param(26),par1 (0 ),par1 (1 ),nb_spc-off]
			   
			if (INST(2) eq '0') THEN begin	P_DID_CALDO, insv,WOUT,YY,ok
							if ok then begin  XT=XT+' pre-Calibrated'
								GROUPY,WOUT ,YY ,/average
								monu= 500000.
								tot = monu/total(NN(*,0))
								WOUT= WOUT*tot
								NN(*,0) =0 & NN(0,0)=monu & endif & endif
	           END
     	   'in13': BEGIN wavel=2.23
		    IF (size(WOUT))(2) eq 74 THEN BEGIN  nd=70
			  ;inelastic scan old format
			  ;*************************
			  IF n_elements(cal_in13) eq 0 THEN $
			     P_DID_CALOD, insv,insv+'.cal', flg
			  YY=[round(param(0 :34)*100)/100.      ,.1,.2,$
			      round(param(0 :34)*100)/100.+0.001,.3,.4]
			  sx=(size(WOUT))(1)
			  NN=lonarr(sx,3)
			  NN(*,1)=WOUT(*,36)>1 & NN(*,2)=WOUT(*,73)>1
			  WOUT=float(WOUT)
			  FOR i=0 ,34 do WOUT(*,i)=WOUT(*,i)/NN(*,1)
			  FOR i=37,71 do WOUT(*,i)=WOUT(*,i)/NN(*,2)
			  NN(*,0)=round(total(NN)/sx)

			  idx =sort(YY) & WOUT=WOUT(*,idx) & YY=YY(idx)
					  WOUT=WOUT(*,4:*) & YY=YY(4:*)
			  WOUT=WOUT*NN(0,0)
			  IF n_elements(cal_in13) eq nd THEN BEGIN
				FOR i=0,nd-1 DO wout(0,i)=wout(*,i)/cal_in13(i)
				OT=OT+' /'+inf_in13(0)
			  ENDIF
			  XX  =findgen (sx)*par1(11)+par1(1)-par1(2)
			  YY  =4*!pi/wavel*sin(YY/2*!pi/180.)
           		  XT  ='Energy micro eV   (normalized)'
           		  YT  ='Q(0) pairs'
			  IF par1(8) gt 0 THEN BEGIN ZZ=(par1(9)+par1(10))/2
						     ZT='Temperature' & ENDIF
		    ENDIF ELSE $
		    IF (par1(8) eq 1 or par1(8) eq 2)  THEN BEGIN  nd=par1(7)-2 & sx=(size(WOUT))(1)
			  ;inelastic scan
			  ;**************
			  IF n_elements(cal_in13) eq 0 THEN $
			     P_DID_CALOD, insv,insv+'.cal', flg
			  YY  =round(param(0 :nd-1)*100)/100.
			  WOUT=float(WOUT)
			  XX  =WOUT(*,nd+2) & XI=(XX(1)-XX(0))/2
			  XX  =(XX+Xi)/100.
			  if XI lt 0 then BEGIN
				XX=REVERSE(XX) & WOUT=REVERSE(WOUT,1)
			  ENDIF
		     	  NN  =WOUT(*,nd)>1
			  WOUT=WOUT(*,0 :nd-1)
			  idx =sort(YY) & WOUT=WOUT(*,idx) & YY=YY(idx)
			  if (INST(2) eq '0') then BEGIN
			  	OT   =OT + " Normalized"
				moni =round(total(NN)/sx) & mona=moni/(NN>1)
				FOR i=0,nd-1 do WOUT(*,i)=WOUT(*,i)*mona
				NN(*)=moni
			  ENDIF
		    ENDIF ELSE $
		    IF (scan    eq 1)  THEN BEGIN
			  ;elastic scan old format
			  ;***********************
		          nd=35 & sx=1
		     	  NN  =WOUT(36)
			  WOUT=float(WOUT(0:34))
			  XX  =round(param(0 :34)*100)/100.
			  idx =sort(XX) & XX  =XX(idx) & WOUT=WOUT(idx)
			  XX  =4*!pi/wavel*sin(XX/2*!pi/180.)
           		  XT  ='Q(0)'
			  YY=(par1(9)+par1(10))/2
			  YT  ='Counts'
		    ENDIF ELSE $
		    IF (par1(8) eq 0)  THEN BEGIN  nd=par1(7)-2 & sx=(size(WOUT))(1)
			  ;elastic scan
			  ;************
		     	  NN  =WOUT(*,nd)
			  YY  =WOUT(*,nd+3)/100.
			  WOUT=float(WOUT(*,0:nd-1))
			  WOUT=reform(TRANSPOSE(WOUT))
			  if (sx gt 1) and (INST(2) eq '0') then BEGIN
			  	OT   =OT + " corrected"
				moni =total(NN)/sx
				FOR i=0,sx-1 do WOUT(*,i)=WOUT(*,i)*moni/NN(i)
				NN   =moni
			  ENDIF
			  XX  =round(param(0 :nd-1)*100)/100.
			  idx =sort(XX) & XX  =XX(idx) & WOUT=WOUT(idx,*)
			  XX  =4*!pi/wavel*sin(XX/2*!pi/180.)
           		  XT  ='Q(0)'
			  YT  ='Counts'
		    ENDIF
		    IF nd gt 0 THEN BEGIN
			  EE  =sqrt(WOUT)
			  PP  =[par1(8),par1(0),par1(3),par1(1),par1(2),par1(11),wavel]
			  PTXT=['Type  of scan      (index)   ','Duration of scan   (second)  ',$
				'Chopper frequency            ','Energy   center    (micro eV)',$
				'Energy half range  (micro eV)','Channel  width     (micro eV)',$
				'Wave length        (angstrom)']
		    ENDIF
		  END
	   else:
	   ENDCASE
	   END
   'lss':  BEGIN
   	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
   	   WT  =	    strmid(text,4,10) +'  '+strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
     	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' User '     +strmid(text,4,10)  +' Run ' + FILENAME
	   ZZ  = param(64)
	   NN  = param(0)
	   PP  = [0.0] & PTXT= [" "]
	   
	   dim= LONG(SQRT(n_elements(WOUT)))
	   IF dim*dim  eq n_elements(WOUT) then BEGIN
	   	WOUT=reform(WOUT,dim,dim,/overwrite)
	   	WOUT=float(WOUT)
	   	sw=size(WOUT)
		XX  = INDGEN(sw(1))
		YY  = INDGEN(sw(2))
		XT  ='X detector'
		YT  ='Y detector'
		ZT  ='Sample Angle'

	     PP=[param(0) ,param(1) ,param(2) ,param(3) ,param(14),param(15),param(16),param(17),param(18),$
	         param(25),param(30),param(32),param(33),param(50),param(51),param(52),param(53),param(57),$
		 param(60),param(61),param(62),param(63),param(64),param(65),param(66),param(80),numor,0,0,0,0]

	     PTXT=['PRESET 1                             ','PRESET 2                             ',$
		   'Run duration (1/10 sec.)             ',$
		   'Total detector counts                ','Detector offset angle (deg.)         ',$
		   'Coder 1: By (mm)                     ','Coder 2: Bx (mm)                     ',$
		   'Coder 3: Sample changer transl. (mm) ','Coder 4: Detector distance (set) (m) ',$
		   'Sample-Detector distance (calc.) (m) ','Sample Temperature (K)               ',$
		   'Value of IEEE-1 at start             ','Value of IEEE-1 at end               ',$
		   'Beam centre adress X0 (mm)           ','Beam centre adress Y0 (mm)           ',$
		   'Wavelength (angstroms)               ','Wavelength resolution                ',$
		   'Collimation  (m)                     ','Detector angle (set) (deg.)          ',$
		   'Detector translation (set) (mm)      ','Selector angle (deg.)                ',$
		   'Sample distance (mm)                 ','Sample rotation (deg.)               ',$
		   'Changer position                     ','Sample height (mm)                   ',$
		   'Shear speed (1/min.)                 ','Numor                                ',$
		   'Not used  ', 'Not used  ','Not used  ','Not used  ']
	   ENDIF
	   CASE insv of

           'd17': if nvers gt 0 then BEGIN
		   rdid_d17,INST,numor,nvers,text,exper,scan,cnt,WOUT,vparm,param,par1,par2,par3,par4,par5,$
	                    WT ,XT ,YT ,ZT ,OT ,DATE ,PP ,PTXT ,XX ,YY ,ZZ ,NN ,PV ,EE
		   WOUT=reform(WOUT)
		  ENDIF
	   'd22':
	   'd11': BEGIN PTXT(2) ='Run duration (sec.)                  '
			PTXT(21)='Sample distance (mm)                 '
			PTXT(22)='Sample rotation (deg.)               ' & WOUT(0,0)=1 & END
	   'd11tof': BEGIN
			WOUT=reform(WOUT,n_elements(WOUT),/overwrite)
			XT='T.O.F.' & YT='Counts' & XX=INDGEN(n_elements(WOUT))+1 & YY=0
			PP(*)=0.    & PTXT(*)='Not used  '  & END
	   else:
	   ENDCASE

	   END

   'test': BEGIN nd =0
   	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
   	   WT  =	    strmid(text,4,10) +'  '+strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
     	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' User '     +strmid(text,4,10)  +' Run ' + FILENAME
           XT  ='OMEGA'
           YT  ='Counts'
	   ZT  ='Numor' & ZZ=numor

     	   CASE insv of
     	   't13a':BEGIN nd =n_elements(WOUT)/2
	   	  WOUT =reform(WOUT,2,nd)
		  XX   =reform(WOUT(0,*))
		  WOUT =reform(WOUT(1,*))
		  END
     	   't13c':BEGIN nd =n_elements(WOUT)/2
	   	  WOUT =reform(WOUT,2,nd)
		  XX   =reform(WOUT(0,*))
		  WOUT =reform(WOUT(1,*))
		  END
 	   else:
	   ENDCASE
	   END
		
   'lon':  BEGIN
           CASE insv OF
	   't3':BEGIN
			add = 1
			IF (STRMID(exper(2),0,3) EQ 'End') THEN add = 0 
	   		text = text(0)
	   		DATE = STRMID(text,14,18)
			i = STRPOS(exper(3),' ')
	   		XT = STRMID(exper(3),0,i)
			YT = 'Phase'
			ZT = 'Counts'
			OT = STRTRIM(text,2)
			IF (add EQ 0 AND STRPOS(exper(5),' ') EQ 0) THEN $
				bl = 1 ELSE bl = 0
			i = STRPOS(exper(8 + add + bl),',')
			jj = STRPOS(exper(7 + add  + bl),':')
			kk = RSTRPOS(exper(7 + add + bl),' ')
			WT = STRMID(exper(8 + add + bl),8,i-8)+STRMID(exper(7 + add + bl),jj+1,kk-jj)
                        WT = STRTRIM(WT,2)
			READS, STRMID(exper(5 + add + bl),13,3), nd
			nphases = (cnt/nd) - 3 - add
			WOUT = REFORM(WOUT,nphases + 3 + add,nd)
			WOUT = TRANSPOSE(WOUT)
			XX = WOUT(*,0+add)
			NN = WOUT(*,1+add:2+add)
			CASE nphases OF
			1: BEGIN
				WOUT = WOUT(*,3+add)
				YY = 0
			   END
			2: BEGIN
				WOUT = WOUT(*,3+add:4+add)
				YY = INDGEN(2)
			   END
			ENDCASE
			READS, STRMID(exper(4 + add),STRPOS(exper(4 + add),':')+1,5), b1
			READS, STRMID(exper(4 + add),RSTRPOS(exper(4 + add),':')+1,5), b2
			READS, STRMID(exper(8 + add + bl),RSTRPOS(exper(8 + add + bl),':')+1,5), lam
			i = RSTRPOS(exper(6 + add + bl),'=')
			READS, STRMID(exper(6 + add + bl),i+1,10), dbc1
			i = RSTRPOS(exper(6 + add + bl),',')
			READS, STRMID(exper(6 + add + bl),i+1,10), dbc2
			PP = [LONG(numor),lam,b1,b2,dbc1,dbc2]
			PTXT = ['0) Numor:                    ',$
				'1) Lambda:                   ',$
				'2) Flipper Current:          ',$
				'3) Correction Current:       ',$
				'4) Direct Beam (Cts/Sec):    ',$
				'5) Direct Beam (CtsMon/Sec): ']
		END
	   ENDCASE
	   END
   else:
   ENDCASE

   DATP={w_tit:WT ,x_tit:XT ,y_tit:YT ,z_tit:ZT ,other_tit:OT ,time:DATE,p:PP,par_txt:PTXT,$
         x:XX     ,y:YY     ,z:ZZ     ,n:NN     ,pv:PV        ,e:EE}
   
ENDIF ELSE begin print,!err_string, string(7b)
   ssz=SIZE(WOUT)
   if ssz(0) ge 1 then XX=indgen(ssz(1))+1 else XX=0
   if ssz(0) ge 2 then YY=indgen(ssz(2))+1 else YY=0
   DATP={x:XX ,y:YY}
ENDELSE

RETURN, WOUT
END
