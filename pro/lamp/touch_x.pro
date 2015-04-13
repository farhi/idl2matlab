;===============================================================================
;                         BEGIN TOUCH_X_EVENT PROCEDURE
;===============================================================================
PRO TOUCH_X_EVENT,event
;** *************
common up1_dat, upd,sdir,stor,linst,total_data,comment,base0,pth1,me,inpath,fnumor,$
		lnumor,id_path,fpath,lpath,cicle,annee, which_cycle, which_year,form

stat=0 &CATCH,stat                                      ;TRAP ERRORS IF CHANGE DIRECTORY FAILED....
IF stat ne 0 then begin
  catch,/cancel
  WIDGET_CONTROL,bad_id=i,comment,set_value=strtrim(strmid(!err_string,0,80),1)
  cd,me & return
ENDIF

WIDGET_CONTROL, event.id, get_uvalue=uv                 ; Control of events

IF n_elements(uv) eq 0  then return   			; <cr>in input fields ?                 
IF uv(0) eq 1 then upd (uv(1)) = event.select           ; BUTTON UPDATE
IF uv(0) eq 2 then sdir(uv(1)) = event.select           ; BUTTON SUB_DIR
IF uv(0) eq 3 then stor(uv(1)) = event.select           ; BUTTON STORE_DATA
IF uv(0) eq 4 then WIDGET_CONTROL,event.top,/destroy    ; BUTTON ABORT

IF uv(0) eq 5 THEN BEGIN                                ; BUTTON APPLY :  
    widget_control,/hourglass
    FOR kl = 0, total_data-1 DO BEGIN                   ; For all input path fields... 
      IF upd(kl) eq 1 then begin
	WIDGET_CONTROL,id_path(kl),get_value = idp      ; get input path  
	idp = strtrim(idp(0),2)
	IF (idp eq '') THEN BEGIN		        ; Equal blank ?                                      
	    WIDGET_CONTROL,comment,set_value='!!! COMPLETE PATH for '+linst(kl)+'!!!'
	    return
	    ENDIF
	CD,idp                                          ; change directory (error traped by catch)
	CD,me                                           ; return home
	inpath(kl)=idp                                  ; store input  path

	WIDGET_CONTROL,fpath(kl),get_value = idp        ; get FIRST NUMORS  
	idp = strtrim(idp(0),2)
	on_ioerror,bad_fnum & t=-1
	t   =long(idp)
	bad_fnum:if t lt 0 then begin
	    	    WIDGET_CONTROL,comment,set_value='!!! BAD FIRST NUMOR VALUE for '+linst(kl)+'!!!'
	    	    return & endif
	fnumor(kl)=strtrim(string(t),2)                 ; store first numor

	WIDGET_CONTROL,lpath(kl),get_value = idp        ; get LAST NUMORS  
	idp = strtrim(idp(0),2)
	on_ioerror,bad_lnum & t=-1
	t   =long(idp)
	bad_lnum:if t lt 0 then begin
	    	    WIDGET_CONTROL,comment,set_value='!!! BAD LAST NUMOR VALUE for '+linst(kl)+'!!!'
	    	    return & endif
	lnumor(kl)=strtrim(string(t),2)                 ; store last numor
      endif
    ENDFOR

    WIDGET_CONTROL,which_cycle,Get_Value=idc            ; get which_cycle
    on_ioerror,bad_cic & t=-1
    t = fix(idc(0))
    bad_cic:IF (t lt 0) or (t gt 9) then begin
		WIDGET_CONTROL,comment,Set_Value='!!! BAD CYCLE!!!'
		return
		ENDIF
    cicle=t
        
    WIDGET_CONTROL,which_year ,get_value=idc            ; get which_year
    on_ioerror,bad_yer & t=-1
    t = fix(idc(0))
    bad_yer:IF (t lt 1000) or (t gt 3000) then begin
		WIDGET_CONTROL,comment,Set_Value='!!! BAD YEAR!!!'
		return
		ENDIF
    annee=t
 
    on_ioerror, end_read & in=-1
    valid=0
    totf=['']
    OPENR,in,pth1,/Get_lun
    	line='' & WHILE (1) DO BEGIN READF,in,line & totf=[totf,line] & ENDWHILE
    end_read:if in gt 0 then FREE_LUN,IN
    
    on_ioerror, no_write & in=-1
    OPENW,in,pth1,/get_lun                              ; WRITE NEW FILE
    	valid=1
	totp =strmid(totf,1,10)
    	FOR jlist =0, total_data -1 DO BEGIN
    	    idx=strpos(totp,' '+linst(jlist)+' ') & idx=where(idx ge 0)
    	    if idx(0) ge 0 then totf(idx)=''
	    printf,in,upd(jlist),linst(jlist),fnumor(jlist),lnumor(jlist),cicle,$
	              annee,sdir(jlist),stor(jlist),inpath(jlist),format=form
        ENDFOR
        
        FOR i=0,n_elements(totf)-1 DO IF totf(i) ne '' THEN PRINTF,in,totf(i) 

    no_write:if in gt 0 then FREE_LUN,IN                ; End of Write

    IF valid eq 0 then begin                            ; error writing ?
	WIDGET_CONTROL,comment,set_value='!!! ERROR WRITING FILE touch.up ....'
	return
    ENDIF

    WIDGET_CONTROL,EVENT.TOP,/DESTROY                   ; DESTROY MAIN BASE..........
ENDIF
return
;===============================================================================
END;                             END TOUCH_X_EVENT PROCEDURE
;===============================================================================


;===============================================================================
PRO TOUCH_RP,iinst,paath,f_sub	;READ path & sub_dir  only
;===============================================================================
common up1_dat

on_ioerror, end_file & in=-1
OPENR,in,pth1,/Get_lun
	up = 0 & inst = '' & fnu = '' & lnu = ''
	sd = 0 & st   = 0  & ip  = '' & cy  = '' & ye = ''
	WHILE (1) DO begin
	    READF,in,up,inst,fnu,lnu,cy,ye,sd,st,ip,format=form
	    
	    idx=where(iinst eq strtrim(inst,2)) & idx=idx(0)
	    if idx ge 0 then paath(idx)=strtrim(ip,2)
	    if idx ge 0 then f_sub(idx)=sd
	ENDWHILE
end_file :if in gt 0 then FREE_LUN,in
return
end

;===============================================================================
PRO TOUCH_R,t_up,t_inst,t_fnu,t_lnu,c_y,y_e,t_sd,t_st,t_ip,exist ;READ IN TOUCH.UP
;===============================================================================
common up1_dat

on_ioerror, end_file
t_up = 0 & in=-1                                                            
exist= 1 & j = 0                                             
OPENR,in,pth1,/Get_lun
    t_up = 0 & t_inst = '' & t_fnu = '' & t_lnu = ''
    t_sd = 0 & t_st   = 0  & t_ip  = '' & cy    = '' & ye = ''
	WHILE (1) DO begin
	    up = 0 & inst = '' & fnu = '' & lnu = ''
	    sd = 0 & st   = 0  & ip  = '' 
	    READF,in,up,inst,fnu,lnu,cy,ye,sd,st,ip,format=form
	    t_inst=[t_inst,strtrim(inst,2)]
	    t_fnu =[t_fnu ,strtrim(fnu ,2)]
	    t_lnu =[t_lnu ,strtrim(lnu ,2)]
	    t_ip  =[t_ip  ,strtrim(ip  ,2)]
	    t_up  =[t_up  ,up]
	    t_sd  =[t_sd  ,sd]
	    t_st  =[t_st  ,st]
	    if j eq 0 then c_y = strtrim(cy  ,2)         
	    if j eq 0 then y_e = strtrim(ye  ,2)
	    j= j+1
	ENDWHILE
end_file :if in gt 0 then FREE_LUN,in
if n_elements(t_up) eq 1 then exist =0
return
end

;===============================================================================
PRO TOUCH_X,xlinst,pth0,manage ;       BEGIN TOUCH_X PROCEDURE 
;===============================================================================
; Parameters recieved : XLINST(DATA ARRAY), PTH0(Path to store "touch.up")

common c_lamp_font
common up1_dat                                           
                                                   ; TOUCH_X yet on screen ?
IF XREGISTERED('TOUCH_X') gt 0 THEN WIDGET_CONTROL,bad_id=i,base0,map = 1 ELSE BEGIN 

form	   ='(i1,x,a8,x,a6,x,a6,x,i1,x,i4,x,i1,x,i1,x,a)'
pth1       =  pth0 + 'touch.up'                    ; Directory+file
total_data = n_elements(xlinst)
IF total_data eq 0 THEN return                     ; Recieved empty data

linst      =  strtrim(xlinst,2)

CD, current= me                                    ; KEEP MY WORKING DIRECTORY 
cicle    = 1                                       ; cycle
annee    = 1789                                    ; Year
id_path  = intarr(total_data)                      ; array paths_id
fpath    = intarr(total_data)                      ; array firstnum_id 
lpath    = intarr(total_data)                      ; array lastnum_id 
inpath   = strarr(total_data)                      ; array for input paths            
upd      = intarr(total_data)                      ; array button up        (state)
sdir     = intarr(total_data)                      ; array button subdir    (state)
stor     = intarr(total_data)                      ; array button storedata (state) 
fnumor   = strarr(total_data)                      ; first numor
lnumor   = strarr(total_data)                      ; last numor
fnumor(*)= '0'                                     ; default first numor
lnumor(*)= '999999'                                ; default last numor

;.............................READ TOUCH_UP file ................................. 


TOUCH_R,t_up,t_inst,t_fnu,t_lnu,cy,ye,t_sd,t_st,t_ip,exist

IF (exist eq 1) THEN BEGIN
		       
 FOR i = 0, total_data-1 DO BEGIN
    idx=where(t_inst eq linst(i)) & idx=idx(0)
    IF idx ge 0 THEN BEGIN
		    upd     (i)  = t_up (idx)          ; keep button up_date status             
		    sdir    (i)  = t_sd (idx)          ; keep button sub_dir status                 
		    stor    (i)  = t_st (idx)          ; keep button store data status 
		    inpath  (i)  = t_ip (idx)          ; keep button store data status
		    fnumor  (i)  = t_fnu(idx)          ; keep first numor
		    lnumor  (i)  = t_lnu(idx)          ; keep last numor
    ENDIF
 ENDFOR
 cicle  = cy                                  
 annee  = ye
                                                  
ENDIF ELSE IF (manage ne 1) then begin print,string(7b)+pth1+' not found ...' & return & endif

;======================================= WIDGETS INSTALL===========================

base0        = widget_base  (/column,title = 'Lamp Touch update list',resource_name='lamp')
base01       = widget_base  (base0,/row,resource_name='don')
bid          = widget_label (base01,value  = 'Cycle',font=ft_propor)
WHICH_CYCLE  = widget_text  (base01,/editable,xsize=1,value=strtrim(string(cicle),2),font=ft_propor)
bid          = widget_label (base01,value  = 'Year',font=ft_propor)
WHICH_YEAR   = widget_text  (base01,/editable,xsize=4,value=strtrim(string(annee),2),font=ft_propor)
bid          = widget_label (base01,value  = 'Custumized Touch_Base location: '+pth0,font=ft_propor)
if manage ne 1 then widget_control,base01,sensitive=0

base1        = widget_base  (base0,/row,resource_name='did')
titr1        = widget_label (base1,value   = ' Update Base',font=ft_propor,/frame)
titr10       = widget_label (base1,value   = 'First Numor' ,font=ft_propor,/frame)
titr11       = widget_label (base1,value   = 'Last Numor ' ,font=ft_propor,/frame)
titr2        = widget_label (base1,value   = 'Sub_Dir'     ,font=ft_propor,/frame)
titr3        = widget_label (base1,value   = 'Store Data'  ,font=ft_propor,/frame)
titr4        = widget_label (base1,value   = '        Input Path'+STRING(REPLICATE(32B, 26)),font=ft_propor,/frame)

base11       = widget_base  (base0,/scroll,/Column, Y_Scroll_Size=(total_data*50)<410,$
				    resource_name='did')   ; SCROLLABLE BASE

;.......................... LOOP TO INSTALL BUTTONS AND FIELDS...............................

FOR I1 = 0,TOTAL_DATA - 1 DO BEGIN                         
      base2  = widget_base  (base11,/row)                             
      base3  = widget_base  (base2,/nonexclusive)
      cinst  = linst(i1)                                       			 ; BUTTON UPDATE                                  
      IF strlen(cinst) lt 8 then cinst=string(replicate(32B,(8 -strlen(cinst))))$
	    +cinst else cinst = strmid(cinst,0,8)                                ; Name of button
      up     = widget_button(base3,Uvalue=[1,i1],value=cinst,font=ft_propor)
      WIDGET_CONTROL,up,set_button=upd(i1)     ; button pushed ?                     
      bid    = widget_label (base2,value=' ',font=ft_propor)              
      firstn = widget_text  (base2,/editable,xsize=6,value=fnumor(i1),font=ft_propor) 
      fpath (i1) = firstn
      bid    = widget_label (base2,value='     ',font=ft_propor)              
      lastn  = widget_text  (base2,/editable,xsize=6,value=lnumor(i1),font=ft_propor) 
      lpath (i1) = lastn
      bid    = widget_label (base2,value='  ',font=ft_propor)              
      bid    = widget_label (base2,value='  ',font=ft_propor)              
      base30 = widget_base  (base2,/nonexclusive) 
      SUBDIR = widget_button(base30,Uvalue=[2,i1],font=ft_propor,value='    ')   ; BUTTON SUB_DIR
      WIDGET_CONTROL,subdir,set_button=sdir(i1)
      bid    = widget_label (base2,value=' ',font=ft_propor)                
      base31 = widget_base  (base2,/nonexclusive) 
      STORE  = widget_button(base31,Uvalue=[3,i1],font=ft_propor,value='')       ; BUTTON STORE
      WIDGET_CONTROL,store,set_button=stor(i1)
      bid    = widget_label (base2,value='  ',font=ft_propor)                   
      IPATH  = widget_text  (base2,/editable,xsize=40,value=inpath(i1),font=ft_propor)
      id_path(i1)  =  ipath                                                      ; INPUT  PATH 

      IF TOTAL_DATA   eq 1   then   WIDGET_CONTROL, SUBDIR,sensitive=0
      IF TOTAL_DATA   eq 1   then   WIDGET_CONTROL, IPATH ,sensitive=0
ENDFOR
;........................... END LOOP 

base4   = widget_base  (base0,/column,resource_name='don')
COMMENT = widget_label (base4,value  = string(replicate(32b,80)),font=ft_propor)
bid	= widget_label (base4,value  ='INPUT PATH is the directory containing the directory name '+$
				      'of the concerned Instrument (ex: /hosts/server/data).'$
				      ,font=ft_smaller)
bid	= widget_label (base4,value  ='NUMORS are the Magic char.length data filenames '+$
				      '(ex: /hosts/server/data/demo/000453 [.Z]).',font=ft_smaller)
bid	= widget_label (base4,value  ='STORE DATA means keep in Touch_Base even large data.' $
				      ,font=ft_smaller)
bid	= widget_label (base4,value  ='SUB_DIR    means 000852 is in .../demo/demo_0/   but   050852 is in .../demo/demo_5/'$
				      ,font=ft_smaller)

base5   = widget_base  (base0,/row,resource_name='don')
but1    = widget_button(base5,value  = 'Abort',font=ft_propor,uvalue=[4,0])
bid     = widget_label (base5,value  = ' '    ,font=ft_propor)
but2    = widget_button(base5,value  = 'Apply',font=ft_propor,/frame,uvalue=[5,0])

;======================================= END WIDGETS INSTALL===========================

WIDGET_CONTROL,/REALIZE,BASE0
if exist lt 1 then widget_control,comment,set_value= 'file '+pth0+'touch.up is unreadable NEW FILE...'
XMANAGER,'TOUCH_X',base0,/just_reg

ENDELSE                                                                         ; XREGISTRED ?
RETURN
END
;======================== END TOUCH_X  ==============================================
