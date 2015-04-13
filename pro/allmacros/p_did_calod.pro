pro p_did_calib, inst, lamp_b1
;** ***********
;**
common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
common calib,base0,lirlist,listcal,comment,curfil,bacur,nwork,swork,filen,lodfil,minst,ytext
common c_lamp_font

if (!D.flags and 65536) eq 0 then RETURN
IF xregistered('CALIB') gt 0 then widget_control,bad_id=ii,base0,map=1 $
ELSE BEGIN

CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN & endif
      MINST   = strlowcase(inst)
      CD,PATHCAL,current=mee & lirlist = findfile() & CD,mee
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
comment	=  widget_label (base0  ,value = string(replicate(45b,60)),font=ft_b_normal)

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
common calib

widget_control,comment,bad_id=ii,set_value=' '
CASE uv(2) of
;**SELECT
   1:	   lodfil = lirlist(event.index)
;**LOAD
   2:begin P_DID_CALOD, minst,lodfil, flg
	   IF flg THEN begin curfil=lodfil & com= curfil+ ' is accepted ...' & endif $
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
      			    CD,PATHCAL,current=mee & lirlist = findfile() & CD,mee
			    widget_control,listcal , bad_id=ii,set_value=lirlist
	   endelse  &  widget_control,comment,bad_id=ii, set_value=com
     end
ELSE:
ENDCASE
RETURN
END

pro p_did_calod,minst,file, OK ,wn ,text ,LIST=list
;** ***********
;**
@lamp.cbk
common calibration

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
'd16':  IF mot eq 'READ' then begin cal_d16 =fltarr(62,14)
	   on_ioerror,misd16
	   inf_d16 =[file]
	   READF,  in,line  & READF,in,cal_d16		 & ok=1
	   misd16: IF ok eq 0 THEN  cal_d16 =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikd16
	   PRINTF, out,'Calib(62*14) ' +systime()+' '+text
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
			      READF,in,format='(64F8.3)',cal_d2b	 & ok=1
	   misd2b: IF ok eq 0 THEN  cal_d2b =0
        ENDIF ELSE IF mot eq 'MAKE' then begin on_ioerror,mikd2b
	   PRINTF, out,'Calib(64)    ' +systime()+' '+text
	   TAKE_DATP,p,w=wn
	   PRINTF, out, P.x
	   PRINTF, out, tmp/(total(tmp)/n_elements(tmp)) & ok=1 & mikd2b:
        ENDIF ELSE IF mot eq 'CLEAR' then cal_d2b =0

'd20': IF mot eq 'READ' then begin cal_d20 =fltarr(1600)& ang_d20=findgen(1600)
 	   on_ioerror,misd20
     ;flag_d20(6)=1
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
        ;flag_d20(6)=0
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
	   PRINTF, out, tmp & PRINTF, out, idx & PRINTF, out, shf & ok=1 & mikin6:
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
ELSE:
ENDCASE
mis_writ:
IF in  gt 0 THEN FREE_LUN,in
IF out gt 0 THEN FREE_LUN,out
RETURN
END
