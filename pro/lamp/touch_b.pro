pro TOUCH_KILL, event
;** **********
;**
@lamp.cbk
common tuch, tu_id  , tu_err  , tu_br   , tu_sn   , tu_acc , tu_raw , tu_sub, tu_fct ,$
	     tu_dir , tu_catal, tu_nelmt, tu_index, tu_rep , tu_fi  , tu_li , tu_fall,$
	     tu_wrun, tu_repb , tu_one  , tu_two  , tu_all , tu_list, tu_mid, tu_wall,$
	     tu_uvk , tu_sdir , tu_mod  , tu_wh   , tu_wcat, tu_tuch, tu_pth, tu_6   ,$
	     tu_p21 , tu_t21  , tu_forc , tu_3D   , tu_cylc, tu_bic , tu_prefx

    if n_elements(tu_list) gt 1 then $
    if tu_acc(2) ne '0' then bid=sys_dep      ('DELIST',tu_list(1:*))
    tu_list=['']
    tu_raw =[0]
    tu_mid =['']
    if (lamp_b1 gt 0) and (lamp_b1 ne tu_id) then widget_control,bad_id=i,tu_id,map=0 $
    else begin  widget_control,bad_id=i,tu_id,/destroy
		if lamp_b1 gt 0     then P_DYING,event
    endelse
return
end

pro TOUCH_DONE, event, uv
;** **********
;**
@lamp.cbk
common tuch

    DON_WRITE_PROG_MAC ,0
    TOUCH_KILL, event
    if n_elements(uv) ge 3 then if uv(2) eq 1 then lamp_b1=-100
return
end
	     
function TOUCH_EXT_RUN , text
;******* *************
;**
;** Extract the run number
	on_ioerror,misrun
	run=long(0)
	i  =strpos(text,'#RUN#')
	if i gt 0 then reads,strmid(text,i+5,8) ,run
misrun: return, run
end

pro TOUCH_LIST, event,uv
;** **********
;**
;** Get the correct Catalog when event occurs.

common tuch	     
	     
    if uv(7) gt 0 then begin
    widget_control,bad_id=i,event.id ,get_value =vvv
    widget_control,bad_id=i,event.id ,get_uvalue=tmpuv
    widget_control,bad_id=i,uv(uv(6)),set_value =vvv
    endif

    widget_control,bad_id=i,uv(3)    ,get_value =p_inst
    widget_control,bad_id=i,uv(4)    ,get_value =p_year
    widget_control,bad_id=i,uv(5)    ,get_value =p_trim & p_trim=strmid(p_trim(0),0,1)
    
    if uv(7) ge 0 then begin
      if n_elements(tu_list) gt 1 then $
      if tu_acc(2) ne '0' then bid=sys_dep      ('DELIST',tu_list(1:*))
      tu_list=['']
      tu_raw =[0]
      tu_mid =['']

      for n=0,n_elements(tu_sn)-1 do begin wset,tu_sn(n) & erase,150 & endfor
      wset,tu_wall & erase,150 & tu_all(*,*)=180

;?      if fix(p_trim) lt tu_cylc then tu_forc=4 else tu_forc=1
	tu_forc=1
    endif
    pp_forc=tu_forc
    
    TOUCH_GET_CATAL ,p_inst(0),p_year(0),p_trim
    
    tmp = tu_catal(tu_index) & ntm=n_elements(tmp)
    
    if tu_wh gt 10 then tmp=strmid(tmp,0,tu_wh) $
    else begin
    	 tmp(0)=strmid(strtrim(string(TOUCH_EXT_RUN(tmp(0))),2)+'        ' , 0,tu_wh-3)
    	 for i =1,ntm-1 do begin
    	 	  tmp(i)=strtrim(string(TOUCH_EXT_RUN(tmp(i))),2)
    	 	  if tu_index(i) ne tu_index(i-1)+1 then tmp(i-1)=tmp(i-1)+' ->'
    	 endfor
    	 if  tu_index(ntm-1) ne tu_nelmt-1 then tmp(ntm-1)= tmp(ntm-1)+' ->'
    endelse
    if  tu_forc eq 3  then s2=''		else s2='Catalog'
    if  tu_forc ne 1  then s1='TouchList'	else s1='OtherList'
    if (tu_forc ne 1) and (pp_forc eq 1) 	then s1=''
    tmp=[tmp,s1,s2]

    TOUCH_SHO_CAT ,0
    widget_control,bad_id=i,uv(2),set_value=tmp,SET_LIST_SELECT=ntm-1
return
end

pro TOUCH_GET_CATAL ,p_INST ,p_YEAR ,p_TRIM
;** ***************
;**
;** Read the catalog
;** **** *** *******
@lamp.cbk
common tuch

GRP =''
for i=0,n_elements(lamp_ins)-1 do if p_INST eq lamp_ins(i) then GRP=lamp_grp(i)

INST=		strtrim(p_INST,2)
YEAR=		strtrim(p_YEAR,2)
TRIM=		strtrim(p_TRIM,2)

MOUNTED =lamp_touch
tu_tuch =lamp_touch+lamp_dvd
tu_6	=lamp_6
if INST eq 'demo' then begin
	 MOUNTED=sys_dep      ('NEWSUB',lamp_dir,'demo')
	 MOUNTED=sys_dep      ('INSUB' ,MOUNTED ,'TOUCH_BASE')
	 tb_dir =sys_dep      ('INSUB' ,MOUNTED ,YEAR)
	 tu_forc=1
	 tu_6   =6
endif else $
	 tb_dir =sys_dep      ('NEWSUB',MOUNTED ,YEAR)
	 
form	='(i'+strtrim(string(tu_6),2)+')'
ins	=strlowcase(INST)
ins_t	=ins+'_'+TRIM
tu_dir  =sys_dep      ('INSUB' ,tb_dir ,ins_t)
tu_acc  =[INST,YEAR,TRIM,GRP,'']
tu_catal=['No Runs..']
tail	=' #FMT# S ' & run='     #RUN# '
tu_nelmt=0   & tu_fi=0 & tu_li=0
tu_index=[0] & tu_wrun(*,*)=-1
tu_prefx=['','']

time	=!stime & YEAC=strmid(!stime,7,4)
YT	= strmid (YEAR,2,2)+TRIM

TOUCH_CUS,idx,pathb
alter=pathb

if  TRIM eq '0' then begin tu_bic='On_Line' & tu_forc=3
endif		else begin tu_bic='Cycle'
;			   *************
		 ptin=strpos (strlowcase(pathb) ,ins)
		 if  (strpos (strlowcase(tu_bic),'cycle') ge 0) and (ptin lt 0) $
		  then pathd =sys_dep('NEWSUB',pathb,ins) else pathd=pathb
		 alter=pathd
		 if (strpos(pathb,'serdon') ge 0) or (strpos(pathb,'illdata') ge 0) then $
		 if (YEAR ne YEAC) or (fix(TRIM) lt tu_cylc) then begin alter='/usr/'
			bid=FINDFILE(  alter      +           'illdata/'+YT,count=n)
			if n gt 0 then alter=alter+           'illdata/'+YT+'/'+ins+'/' $
				  else alter='/hosts/serarch/arch/data/'+YT+'/'+ins+'/'
		 endif
		endelse
;**	Open catalogue
;**	---- ---------
	if tu_forc eq 4 then begin
	   on_ioerror,end_d & OPENR,in,tb_dir+ins_t+'d',/get_lun & free_lun,in & tu_forc=1 & end_d:
	   endif
	catal='catalog' & list=[''] & i=sys_dep('POT',catal)
	in=-1
	if tu_forc eq 1 then begin
	   forfil=INST+catal
	   bid=findfile(tu_dir+forfil,count=n)
	   if  n le 0 then forfil=ins+'_'+YT
	   on_ioerror,end_io
	   OPENR,in,tu_dir+forfil,/get_lun
	   while (1) do begin
		list=strarr(1500)
		readf   ,in,list
		tu_catal=[tu_catal,list]
	   endwhile
	endif
	end_io: if in gt 0 then begin free_lun,in  & tu_cat=['1'] & pathd=tu_dir
		      idx =where(list ne '')
		      if idx(0) ge 0 then begin list    =list(idx)
				  tu_catal=[tu_catal,list] & endif
		      if forfil eq ins+'_'+YT  then begin
			 tu_forc=2
			 tu_cat=tu_catal & tu_catal=[' '] & i3='          '
			 for i=0,n_elements(tu_cat)-1 do begin
			    on_ioerror,mis_ran
			    r1  =long(strmid(tu_cat(i),0     ,tu_6))
			    r2  =long(strmid(tu_cat(i),tu_6+1,tu_6))
			    head=i3 + strmid(tu_cat(i),2*tu_6+2,90)+run
			    tu_catal=[tu_catal,head+string(r1,format=form)+tail]
			    if r2 gt r1 then $
			    tu_catal=[tu_catal,head+string(r2,format=form)+tail]
			    mis_ran:
			 endfor
		      endif
		endif else begin  tu_cat  =['']
				  pathd   =alter & endelse

		if tu_cat(0) eq '' then  begin
		         if ((strpos(pathb,'serdon' ) ge 0) or $
			     (strpos(pathb,'illdata') ge 0)) and ( TRIM gt '0') then begin
			     SPAWN,'cat '+pathb+  '/DATA_CATALOG | grep " '+YT + $
						             ' " | grep " '+ins+' "' , tu_cat
			    ;LAST CHANCE!
			     if tu_cat(0) eq '' then $
			     SPAWN,'cat '+pathb+'-1/DATA_CATALOG | grep " '+YT + $
						             ' " | grep " '+ins+' "' , tu_cat

			     i1=14 & i2=22 & i3=30 & i4=64
			 endif else begin
			     i1=0 & i2=tu_6+1 & i3=2*tu_6+2 & i4=i3+10 & modcy=1
			     catalog, pathb,YT,ins,tu_cat,modcy,tu_prefx
			     if modcy eq 0 then begin pathd=pathb & alter=pathb & tu_bic='' & endif
			 endelse
			 if tu_cat(0) ne '' then begin
			  tu_forc=2
			  for i=0,n_elements(tu_cat)-1 do begin
			    on_ioerror,mis_run
			    r1  =long(strmid(tu_cat(i),i1,tu_6))
			    r2  =long(strmid(tu_cat(i),i2,tu_6))
			    head=strmid(tu_cat(i),i3,10) + strmid(tu_cat(i),i4,10)+run
			    tu_catal=[tu_catal,head+string(r1,format=form)+tail]
			    if r2 gt r1 then $
			    tu_catal=[tu_catal,head+string(r2,format=form)+tail]
			    mis_run:
			  endfor
			 endif
		endif

		TOUCH_KP, 1  ,pathd,tu_acc(0),tu_bic,tu_acc(3)

		if tu_cat(0) eq '' then begin
			  catch,stat & if stat eq 0 then begin nc=0 & CD,current=mee
			    tu_forc=3
			    if TRIM gt '0' then begin
			       CD,pathd
			       tu_cat=findfile('*')
			       if n_elements(tu_cat) eq 1 then tu_cat=findfile()
;			       SPAWN,'cd '+pathd+' ; ls *' , tu_cat
			       CD,mee
			       nc=n_elements(tu_cat)
			       if nc gt 1 then begin
				  tu_cat   = tu_cat(where(tu_cat ne ''))
			          tu_catal =[tu_catal ,strmid(tu_cat,0,3)   +'                 '  +$
						   run+strmid(tu_cat,0,tu_6)+tail] & endif
			    endif else begin
				w_numor (20)='0'
				P_DID_GETRUN, 0 ,20
				FORCPLOT,20
				ru2=long(w_numor(20))>1
				if nc gt 1 then ru1=(long(strmid(tu_cat(nc-1),0,tu_6))+1)>1<ru2  $
					   else ru1=(ru2-101)>1
				tail=' #FMT# L '
				tu_catal =[tu_catal ,'On_Line                 '+run+$
						string(indgen(ru2-ru1+1)+ru1,format=form)+tail,$
						     'The last                '+run+$
						string(ru2 		    ,format=form)+tail]
			    endelse
			  endif else begin catch,/cancel & CD,mee & endelse
		endif
	tu_acc(4)=alter
	if tu_mod eq '' then TOUCH_KP, 0

;**	Make index
;**	---- -----
	tu_nelmt=n_elements(tu_catal)
	if tu_nelmt gt 1 then begin
	   tu_catal(0)=''
	   tu_index=[1]
	   cut=strpos(tu_catal,' ',12)+14
	   exper=strmid(tu_catal(1),0,cut(1))
	   for i=long(2),tu_nelmt-1 do begin
	   	extmp=strmid(tu_catal(i),0,cut(i))
	   	if extmp ne exper then begin exper=extmp
	   				     tu_index=[tu_index,i] & endif
	   endfor
	endif
return
end

pro TOUCH_SHO_CAT , flg
;** *************
;**
@lamp.cbk
common tuch

    i=xregistered('SHO_CAT')
    if  i le 0 then begin
    	bas    =widget_base(title='Catalog from Touch_Base',resource_name='lamptouch')
    	tu_wcat=widget_text(bas,xsiz=60,ysize=14,font=ft_propor,/scroll)
	widget_control,bad_id=ii,bas,group_leader=lamp_b1,map=0,/realize
	XMANAGER,'SHO_CAT',bas,/just_reg
    endif
    if flg eq 0 then widget_control,bad_id=ii,tu_wcat,map=0 $
		else widget_control,bad_id=ii,tu_wcat,map=1,set_value=tu_catal
return
end

pro TOUCH_EXT_VAL , text ,mini,maxi
;** *************
;**
;** Extract the mini and maxi values

	mini=1. & maxi=1.
	i  =strpos(text,'#MIN#')
	if i gt 0 then reads,strmid(text,i+5,15) ,mini
	i  =strpos(text,'#MAX#')
	if i gt 0 then reads,strmid(text,i+5,15) ,maxi
return
end

pro TOUCH_EXT_FMT , text ,fmt
;** *************
;**
;** Extract the format of files

	fmt=[0L,0,0]
	i  =strpos(text,'#FMT#')
	if i gt 0 then begin x= strmid(text,i+6 ,1) & if  x eq 'X'   then fmt(0)=1 else $
						      if  x eq 'S'   then fmt(0)=2 else $
						      if  x eq 'L'   then fmt(0)=2
			     y= strmid(text,i+7 ,1) & if fmt(0) eq 2 then fmt(1)=2 else $
						      if  y eq 'R'   then fmt(1)=1
			     x= strmid(text,i+13,1) & if  x eq '#'   then begin
			     	x=strmid(text,i+14,9)
			     	j=strpos(x   ,'#')
			     	if j gt 0 then fmt(2)=long(strmid(x,0,j))
			     endif
	endif
return
end

function TOUCH_EXT_FIL , run
;******* *************
;**
;** Construct the filename.
@lamp.cbk
common tuch

    	fil=strtrim(string(run),2) & ln=strlen(fil)
    	while ln lt tu_6 do begin fil='0'+fil & ln=ln+1 & endwhile
return,	tu_prefx(0)+fil+tu_prefx(1)
end

pro TOUCH_EXPER, uv , index
;** ***********
;**
;** One entry is selected in the experiment_list.

common tuch

if (index ge 0) and (tu_nelmt gt 0) then begin
    if index lt n_elements(tu_index) then begin
	tu_fi  =tu_index(index)
	if index eq n_elements(tu_index)-1 	then tu_li= tu_nelmt-1     $
					   	else tu_li= tu_index(index+1)-1
	if (uv(1) eq 338) or (tu_acc(2) eq '0') then tu_mi=(tu_li-1) >tu_fi $
						else tu_mi=(tu_li-2) >tu_fi
;**	Set first and last Run

	first=tu_catal(tu_fi)
	last =tu_catal(tu_li)
	run_f=TOUCH_EXT_RUN(first)
	run_l=TOUCH_EXT_RUN(last)
	widget_control,bad_id=i,uv(3),SET_VALUE='First Run '+strtrim(string(run_f),2)
	widget_control,bad_id=i,uv(4),SET_VALUE='Last Run ' +strtrim(string(run_l),2)

	if tu_forc eq 2 then begin
	   run_m  =(run_l-2)>run_f
	   tu_fall=bytarr(run_l-run_f+1)
	endif else begin
	   middl  =tu_catal(tu_mi)
	   run_m  =TOUCH_EXT_RUN(middl)
	   tu_fall=bytarr(tu_li-tu_fi+1)
	endelse
	if run_f eq run_l then run_l=run_l+1
	widget_control,bad_id=i,uv(2),SET_SLIDER_MIN=run_f,SET_SLIDER_MAX=run_l
	widget_control,bad_id=i,uv(2),GET_VALUE =j
	widget_control,bad_id=i,uv(2),SET_VALUE =run_m
	
	if n_elements(tu_list) gt 1 then $
	if tu_acc(2) ne '0' then bid=sys_dep      ('DELIST',tu_list(1:*))
	tu_list=['']
	tu_raw =[0]
	tu_mid =['']
        for n=0,n_elements(tu_sn)-1 do begin wset,tu_sn(n) & erase,150 & endfor
    	wset,tu_wall & erase,150 & tu_all(*,*)=180
    
;**	Show the middle Run

	tu_wrun(*,4)=-1
	TOUCH_RUN, uv , run_m ,0
	
	if tu_mod ne '' then begin
	   TOUCH_LOCALISE , cnt,pathd,TOUCH_EXT_FIL(run_m)
	   TOUCH_KP, 1  ,pathd,tu_acc(0),tu_bic,tu_acc(3)
	endif
 	widget_control,bad_id=i,tu_id,/clear_events
 		
    endif else begin
	if index eq n_elements(tu_index)+1 then TOUCH_SHO_CAT ,1
	if index eq n_elements(tu_index)   then begin
		    if tu_forc eq 1  then  tu_forc=2  else tu_forc=1
		    uvu=tu_uvk & uvu(7)=-1 &  TOUCH_LIST ,0,uvu
	endif
    endelse
endif

return
end

pro TOUCH_RUN,   uv , run ,drag
;** *********
;**
;** Show the selected Run
@lamp.cbk
common tuch

nb_snap = n_elements(tu_br)
z	=(nb_snap-1)/2

if drag eq 1 then begin
   widget_control,bad_id=i,tu_br(z),set_value='---> '+string(run)
   return
endif

if run gt 0 then begin

  if tu_forc ne 2 then begin  i_li=tu_li & i_fi=tu_fi		  & endif $
		  else begin  i_li=TOUCH_EXT_RUN(tu_catal(tu_li)) & text=tu_catal(tu_fi)
			      i_fi=TOUCH_EXT_RUN(text)		  & endelse
  if tu_wrun(z,0) ne run then begin
	
	if tu_forc eq 2 then n=run $
	else begin
	   n=tu_fi & stp=0
	   while ((n le tu_li) and (stp eq 0)) do begin
	
		text =tu_catal(n)
		run_m=TOUCH_EXT_RUN(text)
		if (run_m ne run) then n=n+1 else stp=1
	   endwhile
	endelse
	tu_wrun(z,1)=n
	
	if n le i_li then begin
	
;**	Which Runs around
	     tu_wrun(*,0)=-1
	     kf=n-z

	     for j=0,nb_snap-1 do begin
		 k=kf+j
		 if (k ge i_fi) and (k le i_li)  then begin
			    if tu_forc ne 2  then begin
		 	       text	=    tu_catal(k)
			       run_m	=    TOUCH_EXT_RUN(text)
			    endif else	     run_m=k
			    tu_wrun(j,0)=    run_m
			    tu_wrun(j,1)=    k
	     				     TOUCH_EXT_VAL,text ,mini,maxi
	     				     TOUCH_EXT_FMT,text ,fmt
	     		    tu_wrun(j,2)=mini
	     		    tu_wrun(j,3)=maxi
	     		    tu_wrun(j,6)=fmt(0)
	     		    tu_wrun(j,7)=fmt(1)
	     		    tu_wrun(j,8)=fmt(2)
	     		    texx=  strtrim(string(long(tu_wrun(j,0))),2)+' '+strmid(text,20,100)
	     		    widget_control,bad_id=i,tu_br(j),set_value=texx
		 endif else widget_control,bad_id=i,tu_br(j),set_value=' '
	     endfor
	     tu_wrun(z,0)=-1
	 endif
  endif
  if (tu_wrun(z,0) ne run) or (tu_rep ne tu_repb) then begin

	tu_wrun(z,0)=run
	nn	=long(tu_wrun(z,1))

	if nn le i_li then begin
	
;**	     Transfer the files not present
;**	     -------- --- ----- --- -------
	     filmid='' & dirmid=''
	     filist=''
	     filisa=['']
	     filisz=['']
	     filisx=[0]
	     filisr=[0]
	     runame=['']
	     hyst  =''
	     for j=0,nb_snap-1 do begin
	         if tu_wrun(j,0) ge 0 then begin

		    fil=TOUCH_EXT_FIL(long(tu_wrun(j,0)))
		    if tu_wrun(j,6) ne 2 then fil=fil+'_LAMP'
		    runame=[runame,fil]
		    yes   = strpos(tu_list,runame(j+1))
		    yes   = max(yes)
		    if yes lt 0 then begin
		    		     if tu_wrun(j,6) eq 2 then  ext=''       else $
		    		     if tu_wrun(j,6) eq 0 then  ext='png*'   else $
		    		     if tu_wrun(j,7) eq 1 then  ext='pngR.Z' else $
		    		     				ext='png'
		    		     filist= filist+runame(j+1)+ext+' '
		    		     
				     
		    		     if tu_wrun(j,7) eq 1 then  ext='pngR'    else $
				     if tu_wrun(j,7) eq 0 then  ext='png'
		    		     filisa=[filisa,       runame(j+1)+ ext ]
		    		     
		    		     filisx=[filisx,tu_wrun(j,6)]
		    		     filisr=[filisr,tu_wrun(j,7)]
		    		     endif
		    if j   eq z then if tu_wrun(j,6) ne 2 then begin
		    		     yes   = strpos(tu_mid,runame(z+1))
		    		     yes   = max(yes)
		    		     if yes lt 0 then begin
		    		     	    filmid = runame(z+1)
		    		     	    ii=sys_dep      ('POT',filmid)
		    		            filist = filist+filmid+' '
		    endif    &	     endif
		 endif else begin
		    runame=[runame,'']
		 endelse
	     endfor
	     
	     if (filist ne '') and (tu_rep gt 0) then begin
		    n_e=n_elements(filisa)
		    if tu_wrun(z,6) eq 2 then begin zip=0
					      TOUCH_LOCALISE ,cnt,pathf,filisa(1),zip
					      if zip eq 1 then begin filist=''
						    for j=1,n_e-1 do filist=filist+filisa(j)+'.Z '
						    filisr(*)=1   &  endif
		    endif else pathf = tu_dir
;		    					   *******************
;		    **********************************************************
		    if tu_acc(2) ne '0' then bid=sys_dep ('COPY',filist,pathf)
;		    **********************************************************
;		    					   *******************
		    if n_e gt 1 then begin
		    		for j=1,n_e-1 do begin
		    			if filisx(1) eq 0 then begin
					      pngr=findfile(filisa(j)+'R*',count=cnt)
					      if cnt gt 0 then filisa(j)=filisa(j)+'R'
					      filisz=[filisz,filisa(j)+'.Z']
					endif else if filisr(j)  eq 1 then $
					      filisz=[filisz,filisa(j)+'.Z']
				endfor

				if n_elements(filisz) gt 1 then $
				   bid=sys_dep      ('UN_ZLIS',filisz(1:*),lamp_dir)
				   
		    		tu_list=[tu_list,filisa(1:*)]
		    endif
		    if filmid ne '' then  tu_list=[tu_list,filmid]
		    if filmid ne '' then  tu_mid =[tu_mid ,filmid]
		    		    
	     endif else if filmid eq '' then begin filmid= runame(z+1)
	     					   ii    = sys_dep      ('POT',filmid)
	     				endif else dirmid= tu_dir
		
;**	     Display snapshots
;**	     ------- ---------
	     if tu_rep gt 0 then begin
	     
	      worder=!order & !order=0
	      if tu_wrun(z,0) gt tu_wrun(z,4) then begin
	      			 i1=0 & i2=nb_snap-1 & pas= 1
	      endif else begin   i1=nb_snap-1 & i2=0 & pas=-1 & endelse
	      
	      runlst=tu_wrun(*,4)
	      bitall=0
	      nf    =nn-i_fi-z

	      for j=i1,i2,pas do begin
	      
	       wset,tu_sn(j)
	       
	       if runame (j+1) ne '' then begin
	       
		yes   = where( runlst eq tu_wrun(j,0) )
;**	     	Already in buffer
;**	     	------- -- ------
	        if (yes(0) ge 0) and (tu_rep eq tu_repb) then begin
	        
		  device,copy=[0,0,192,192,0,0,tu_sn(yes(0))]
		  tu_wrun(j,5)=tu_wrun(yes(0),5) & runlst(j)=tu_wrun(j,0)
		  if tu_wrun(j,6) eq 2 then tu_p21(j,*)=tu_p21(yes(0),*)
		  if tu_wrun(j,6) eq 2 then tu_t21(j)=tu_t21(yes(0))
	        
;**	     	Must read image
;**	     	---- ---- -----
	        endif else begin
	         
		  mini=tu_wrun(j,2)
		  maxi=tu_wrun(j,3)
		  vect=0
		
		  fil=runame(j+1)
		  if tu_wrun(j,7) eq 0 then begin fil=fil+'png' & pngr=-1 & endif else pngr=1
		  if tu_wrun(j,7) eq 1 then 	  fil=fil+'pngR'
		  tu_wrun(j,5)=pngr
		  
		  if (tu_wrun(j,6) eq 1) and (tu_wrun(j,7) eq 0) then begin   buf=0
		  						 READ_KIF,fil,buf
		  						 tu_one(0,0) =buf
		  endif else begin

	            if pngr lt 0 then tu_one(*,*)=0
	          
		    ON_IOERROR,misopen
		    ii=sys_dep      ('POT',fil)  &  in=0
		    if tu_wrun(j,6) eq 1 then OPENR,in,fil,/GET_LUN,/XDR else $
		    if tu_wrun(j,6) eq 0 then OPENR,in,fil,/GET_LUN
			ON_IOERROR,miseof
			if pngr lt 0 then READU   , in,tu_one
			if pngr ge 0 then begin     xsiz= fix(0) & ysiz=fix(0) & tip=fix(0)
		  	   if   tu_wrun(j,6) eq 2   then  begin
				if tu_acc(2) eq '0' then  CY=tu_bic else CY=''
				TOUCH_KP, 1 ,'',tu_acc(0),CY,tu_acc(3)
				p_did_getrun, long(tu_wrun(j,0)),21    & tu_raw=w21
				TOUCH_KP, 0
				tu_p21(j,*)=''
				for i=0,n_elements(p21)-1 do tu_p21(j,i)=par_txt(21,i)+string(p21(i))
				tu_t21(j)  =strcompress(w_tit(21)+" *** "+other_tit(21))
				sv=size(tu_raw) & if sv(0) eq 0 then tu_raw=[0,1]
				xsiz=sv(1)      & if sv(0) le 1 then vect=1
				if   sv(0) eq 3 then tu_raw=total(tu_raw,tu_3D)
			   endif else begin
				READU   ,in,xsiz,ysiz,tip
				if ysiz le 1  then vect=1
				if vect eq 1  then tu_raw=make_array(xsiz,type=tip) $
					      else tu_raw=make_array(xsiz,ysiz,type=tip) 
				READU   ,in,       tu_raw
			   endelse

			   if vect eq 0  then begin
			      tu_one=congrid(bytscl(tu_raw),192,192)
			      if (tu_rep ne 2) or  (tu_fall(nf+j) eq 0) or $
				 (tu_fct eq 3) or  (tu_fct eq 4)      then $
					  	    tu_one=bytscl (alog(tu_one>1))
			   endif
			endif
		
		     miseof: if in gt 0 then FREE_LUN,in
		     misopen:
		  endelse
		
;**	     	  Update small icons
;**	     	  ------ ----- -----
		  if tu_fall(nf+j) eq 0 then begin
		     tu_fall(nf+j)=1
		     nix=(i_li-i_fi+1)
		     pix=(192+18)*(nb_snap-2)
		     gap=(pix-nix*25*(nb_snap-2))>0
		     gap=float(gap) / nix
		     pix=float(pix) / nix  -  gap
		     idx=fix((gap+pix)*(nf+j)+gap/2)
		     pix=fix(pix)>1
		     bitall=1
		     if pix  ge 6 then  begin
		     	  if vect eq 0  then tu_all(idx,0)=255-congrid(       tu_one ,pix-1,25) $
		     	  else tu_all(idx,0)=congrid(reform(bytscl(tu_raw),xsiz,1),pix-1,25)
		     endif else 	     tu_all(idx:idx+pix-1,*)=0
		  endif
		  
;**	     	  Show snapshot
;**	     	  ---- --------
		  if pngr lt 0 then $
		     if ((tu_fct ne 0)  and (tu_fct ne 3)  and (tu_fct ne 4) and (tu_fct ne 8)) or $
		     	((tu_fct eq 0)  and (tu_rep eq 2)) then begin
			  if mini le 0  then begin minl=alog(0.1)  & maxl=alog(maxi-mini+0.1)
			  endif 	else begin minl=alog(mini) & maxl=alog(maxi) & endelse
			  tu_raw=maxl*tu_one /255
			  tu_raw=exp (tu_raw)
			  if mini le 0 then tu_raw=tu_raw+mini-0.1
		     endif
		  if tu_fct ne 0 then begin
;		     0=Det.Counts 1=Xproj(I) 2=Yproj(I) 3=LOG(Xproj) 4=LOG(Yproj) 5=AVERAGE(I) 6=DISTRIB(I)
;		     7=Count-Mean
		     vector=0 & mt=''
		     if vect eq 1 then begin
		        if tu_fct eq 1 then begin vector=tu_raw					    & endif
		        if tu_fct eq 2 then begin vector=total(tu_raw)		  & mt='Total= '    & endif
		        if tu_fct eq 3 then begin vector=alog (tu_raw>0.1)	  & mt='Log(I)'     & endif
		        if tu_fct eq 4 then begin vector=alog (total(tu_raw)>0.1) & mt='Log(Total)' & endif
		        if tu_fct eq 5 then begin vector=total(tu_raw)/n_elements(tu_raw)
		        							    mt='AVG = '     & endif
		        if tu_fct eq 6 then begin vector=histogram(tu_raw)
						  mt='DISTRIB('+strtrim(string(mini),2)+'->'+       $
								strtrim(string(maxi),2)+')'	    & endif
			if tu_fct eq 7 then begin vector=total(tu_raw)/n_elements(tu_raw)
						  vector=     (tu_raw -vector) > 0		    & endif
		     endif else begin
			if tu_fct eq 1 then begin vector=total(tu_raw,2) 	  & mt='Xproj(I)'   & endif
			if tu_fct eq 2 then begin vector=total(tu_raw,1) 	  & mt='Yproj(I)'   & endif
			if tu_fct eq 3 then begin vector=total(tu_one,2)	  & mt='LOG(Xproj)' & endif
			if tu_fct eq 4 then begin vector=total(tu_one,1)	  & mt='LOG(Yproj)' & endif
			if tu_fct eq 5 then begin vector=total(tu_raw)/n_elements(tu_raw)
					          if pngr lt 0 then mt='AVG ~ ' else mt='AVG = '    & endif
			if tu_fct eq 6 then begin vector=alog (histogram(tu_raw)>1)
						  mt='DISTRIB('+strtrim(string(mini),2)+'->'+       $
								strtrim(string(maxi),2)+')'	    & endif
			if tu_fct eq 7 then begin vector=total(tu_raw)/n_elements(tu_raw)
						  vector=congrid((tu_raw -vector) > 0.1 ,192,192)   & endif
			if tu_fct eq 8 then begin elas=-1
						  if pngr lt 0 then $
						  ii=execute('vector=LINEUP(tu_one,elas)') else $
						  ii=execute('vector=LINEUP(tu_raw,elas)')
						  sv	=size  ( vector)
						  if elas lt 0 then vector=0 else $
						  vector=congrid(vector((elas-40)>0:(elas+40)<sv(1)-1,*) $
						  		,81,20<sv(2))			    & endif
		     endelse
		     if n_elements(vector) gt 1 then begin svec=size(vector)
		     		if svec(0) eq 1 then       plot, vector,xmargin=[0,0],ymargin=[0,2],$
		     					   xstyle=4,ystyle=4,xtitle='',ytitle='',title=mt,$
			  	    			   background=255,color=0,font=-1,charsize=.4 $
				else if tu_fct eq 8 then   surface,vector,/HORIZONTAL,AX=55.,AZ=0.  ,$
									  XSTYLE=4,YSTYLE=4,ZSTYLE=4,$
									  XMARGIN=[0,0],YMARGIN =[0,0],$
									  BACKGROUND=255 ,COLOR =0  ,$
									  XTICKLEN=1.,XGRIDSTYLE=1   $
			  	else	   		   tvscl,255-bytscl(alog(vector))

		     endif
		     if n_elements(vector) eq 1 then begin erase,255
		     					   xyouts,2,192/2,mt+strtrim(string(vector),2)   ,$
		     					   charsize=1.7,/device,color=0
		     					   endif
		  endif else if vect eq 1 then begin
		      	plot,tu_raw,xmargin=[6,0],ymargin=[3,0],xtitle='',ytitle='',title='',$
			  	    background=255,color=0,font=-1,charsize=.4
			  	   
			  	   
		  endif else if tu_rep eq 1 then begin
			tvscl,255-tu_one
			
			
		  endif else if tu_rep eq 2 then begin
		     !order=0
		     tmp   =maxi/(mini>1)
		     if tmp ge 1000 then tmp=(mini+(maxi-mini)/(tmp/1000)) else tmp=maxi
		     zrnge =[ mini , tmp ]
		     if pngr lt 0 then begin
			shade_surf,tu_raw ,xmargin=[0,0],ymargin=[0,0],zrange=zrnge,$
					   xstyle=4,ystyle=4,zstyle=4,ax=45,az=30,shades=tu_one
		     endif else begin
		        s=size(tu_raw)
		        if ((s(2) le 15)) or $
		           ((s(1) le 15) and (s(2) lt 50)) then $
			surface   ,tu_raw, xmargin=[0,0],ymargin=[0,0],zrange=zrnge,/horizontal,color=0,$
					   zstyle=4,background=255   ,ax=70,az=45  else    $
			shade_surf,tu_raw, xmargin=[0,0],ymargin=[0,0],zrange=zrnge,$
					   xstyle=4,ystyle=4,zstyle=4,ax=55,az=30
		     endelse
		  endif else if tu_rep eq 3 then begin
			contour,smooth(tu_one,3),xmargin=[0,0],ymargin=[0,0],$
						 xstyle=4,ystyle=4,/fill
		  endif
		
		endelse
		tu_wrun(j,4) =tu_wrun(j,0)
		widget_control,bad_id=i,tu_br(j),set_value=tu_t21(j)

	       endif
	       	       
	      endfor
	      if bitall eq 1 then begin wset,tu_wall & tvscl,tu_all & endif
	      !order=worder
	     endif
	     
;**	     Show parameters of middle Run
;**	     ---- ---------- -- ------ ---
	     if tu_acc(2) ne '0'   then begin idx=0
	      if tu_wrun(z,6) ne 2 then begin p_did_restore_wrk,filmid,dirmid,'0',hyst,-2
				              idx=where(hyst eq ' PARAMETERS:') & idx=idx(0)
				   endif else hyst=tu_p21(z,*)
	      if uv(6) gt 0 then begin	
	         widget_control,bad_id=i,uv(6),set_value=hyst,SET_TEXT_TOP_LINE=idx>0
	         widget_control,bad_id=i,uv(5),set_value=strtrim(string(tu_wrun(z,0)),2)
	         if tu_wrun(z,5) ge 0 then rors=' R ' else rors='   '
	         widget_control,bad_id=i,uv(7),set_value=  rors

	      endif else begin	if tu_wrun(z,6) ne 2 then if idx gt 0 then hyst=hyst(idx+2:*)
				FORCPAR,hyst
	      endelse
	     endif
	endif
	tu_repb=tu_rep
  endif
endif

return
end

pro TOUCH_MODE,	 uv
;** **********
;**
;** Change representation event

common tuch

tu_repb=tu_rep
tu_rep =uv(2)
z      =(n_elements(tu_br)-1)/2

if tu_repb ne tu_rep then TOUCH_RUN,   uv , long(tu_wrun(z,0)) ,0

return
end

function TOUCH_moni
;******* **********
;**
@lamp.cbk
	return,monimon
end

pro TOUCH_MORE,	 uv , event
;** **********
;**
common tuch
	     
    if uv(2) eq -1   then begin uvu=tu_uvk & uvu(7)=-1
    			if n_elements(tu_acc) ge 3 then begin
    			   TOUCH_LIST ,0,uvu
    			   TOUCH_EXPER, [uv(0:1),uv(3:*)] , n_elements(tu_index)-1
    			endif
    endif  else $
    if uv(2) eq  0   then begin
			 i=xregistered('xloadct')
			 if i lt 1 then xloadct,group=tu_id,/use_current
    endif  else $
    if uv(2) eq  1   then begin
    			 widget_control,bad_id=i,event.id,get_value=text & text=text(0)
    			 run_comd,text,tbl
    			 if text ne '' then begin
    			    widget_control,bad_id=i,event.id,set_value=text
			    TOUCH_SELECTOR,tbl,uv(3),text,(n_elements(tu_br)-1)/2
    			 endif else $
    			    widget_control,bad_id=i,tu_err,set_value='Syntax error ...!'
    endif  else $
    if uv(2) eq  2   then begin INX
    endif  else $
    if uv(2) eq  3   then begin if n_elements(tu_acc) ge 3 then TOUCH_X,tu_acc(0),tu_tuch,0 & tu_pth=''
    endif  else $
    if uv(2) eq  4   then begin RDFILTER ,1
    endif  else $
    if uv(2) ge 100  then begin
    			 if tu_fct ne uv(2)-100 then begin
    			    tu_repb=-1
			    if uv(2) ge 120 then tu_3D=(uv(2)-120)<3 else tu_fct =uv(2)-100

    			    widget_control,bad_id=i,event.id,get_value=tit
    			    widget_control,bad_id=i,uv(3)   ,set_value=tit(0)
			    z=(n_elements(tu_br)-1)/2
			    TOUCH_RUN,   uv , long(tu_wrun(z,0)) ,0
			 endif
    endif  else $
    if uv(2) ge 10   then if event.type eq 0 then begin
    			 n  =uv(2)-10
    			 run=long(tu_wrun(n,0))
    			 tit=strtrim(string(run),2)
    			 if (event.press eq 1) then begin
    			   bas=widget_base  (title=tit)
			   bdr=widget_draw  (bas  ,retain=2  ,xsize=192,ysize=192)
			   widget_control,bad_id=i,bas ,group_leader=tu_id ,/realize
			   widget_control,bad_id=i,bdr ,get_value=j & wset,j
		  	   device,copy=[0,0,192,192,0,0,tu_sn(n)]
		  	 endif else begin
		  	   wi=1 & ws='1'
			   if tu_mod eq '' then TOUCH_WNUMB, uv(3),wi,ws
			   if event.press eq 2  then $
			   	        TOUCH_SELECTOR,[0,run,1],wi,tit,n $
			   else begin   TOUCH_SELECTOR,[0,run,1],21,tit,n
					set_tolerance,tt,/get
			   		if TOUCH_moni() lt 0 then W_ACCU, accu=wi, add=21 ,tol=tt ,/raw $
							     else W_ACCU, accu=wi, add=21 ,tol=tt
					set_tolerance,tol=tt
			   		widget_control,bad_id=i,tu_err,set_value=tit+' added to W'+ws
			   		if tu_mod ne '' then FORCPLOT ,wi
					to_don_history, wi,0,'w'+ws+'=w'+ws+'+w21'
			   endelse
		  	 endelse
    endif
return
end

pro TOUCH_SELECTOR,tbl,wi,text,z
;** **************
;**
@lamp.cbk
common tuch

nn=size(tbl)
if nn(0) lt 2 then nn=1 else nn=nn(2)

if (tu_wrun(z,0) gt 0)  then begin
 
 if (tu_wrun(z,7) eq 1) or ((tu_wrun(z,7) eq 0) and (tu_wrun(z,8) gt 0)) then begin
	pathd=tu_dir & inst='lamp' & grp='' & cnt=1
        
 endif else begin
 	cnt=0
 	run=tbl(1,0)
 	if (run eq 0) and (nn gt 1) then run=tbl(1,1)
	fil=TOUCH_EXT_FIL(run)
	TOUCH_LOCALISE , cnt,pathd,fil ,ptin=ptin
	outext ='? '+pathd+fil+' is not accessible ...'
	inst=tu_acc(0) & grp=tu_acc(3)
 endelse
 
 if cnt gt 0 then begin
	TOUCH_KP, 1 ,pathd,inst,tu_bic,grp
	RDMULTI, text,status,tu_err,wi
	TOUCH_KP, 0
	if status eq 0 then begin
			to_don_history, wi,0,'w'+strtrim(string(wi),2)+'=RDOPR("'+text+'") ;'+tu_acc(0)
			if tu_mod ne '' then if wi le 20 then FORCPLOT ,wi
	endif
 endif else widget_control,bad_id=i,tu_err,set_value=outext
endif

return
end

pro TOUCH_KP, flag ,path,inst,cycl,grou
;** ********
;**
@lamp.cbk
common tuch
common t_kp, keepath,keepins,keepcyc,keepgrp

if flag eq 0 then begin PATH_FOR_ONLINE=keepath & INST_VALUE=keepins
			CYCLE=keepcyc		& INST_GROUP=keepgrp   & endif else $

if flag eq 1 then begin keepath=PATH_FOR_ONLINE & keepins=INST_VALUE
			keepcyc=CYCLE		& keepgrp=INST_GROUP
			PATH_FOR_ONLINE=path 	& INST_VALUE=inst
			CYCLE=cycl		& INST_GROUP=grou      & endif
return
end

pro TOUCH_WNUMB, bidx , wi , ws
;** ***********
;**
	widget_control,bad_id=i,bidx,get_value=ws
	i  =strpos(ws,'W')
	ws=strtrim(strmid(ws,i+1,4),2)
	wi =fix(ws)
return
end
pro TOUCH_RESTORE, uv
;** *************
;**
;** Restore a data file.

@lamp.cbk
common tuch
	     
	TOUCH_WNUMB, uv(2),wi,wnumber
	z  =(n_elements(tu_br)-1) /2
	raw=tu_wrun(z,5)
	run=long(tu_wrun(z,0))
	if run gt 0 then begin
	   fil=TOUCH_EXT_FIL(run) & fir=fil

;**	   Restore from Snapshot or data in current D.
;**	   ------- ---- -------- -- ---- -- ---------   
	   if ((uv(3) eq 2) or (raw ge 0)) and (tu_wrun(z,7) ne 2) then begin
	       pp2=-1 & hyst='' & fil=fil+'_LAMP'
	       i=findfile(fil+'pngR',count=cnt)
	       if cnt eq 0 then i=findfile(fil+'png',count=cnt)
	       if cnt eq 0 then i=findfile(fil+'jpg',count=cnt)
	       if cnt gt 0 then begin
	        widget_control,bad_id=i,tu_err,set_value='Restoring ...'
	        
		bid=sys_dep      ('POT',fil)
		p_did_restore_wrk, fil ,'',wnumber,hyst,pp2
		
	        widget_control,bad_id=i,tu_err,set_value=' '
		
		if (pp2 gt 0) then w_numor(wi)=fir
		if (pp2 gt 0) and (raw lt 0) then begin
		  	mini=tu_wrun(z,2)
		  	maxi=tu_wrun(z,3)
			if mini le 0 then begin
				          minl=alog(0.1)  & maxl=alog(maxi-mini+0.1)
			endif else begin  minl=alog(mini) & maxl=alog(maxi) & endelse
			maxsn=0
			i=execute( 'maxsn=max(w'+wnumber+')' )
			i=execute( 'w'+wnumber+'=maxl*w'+wnumber+'/maxsn' )
			i=execute( 'w'+wnumber+'=exp (w'+wnumber+')' )
			if mini le 0 then $
			i=execute( 'w'+wnumber+'=     w'+wnumber+'+mini-0.1' )
		endif
		
		if (pp2 gt 0) then P_MUS,'mus_shot'
		p_did_after_read,  wnumber,tu_err, fil ,pp2
		if (raw lt 0)			then outext='Snapshot '+fir+' is re_formed in W'+wnumber
		if (raw ge 0)			then outext='raw data '+fil+' are restored in W'+wnumber
		if (raw ge 0) and (uv(3) eq 2)  then outext='In fact, '+outext
		if (pp2 gt 0) then widget_control,bad_id=i,tu_err,set_value=outext
	       endif
		
;**	   Restore from Data Base
;**	   ------- ---- ---------	   
	   endif else begin
		 if tu_wrun(z,7) eq 2 then begin cnt=1 & pathd='' & ptin=0 & endif $
	   	 else  TOUCH_LOCALISE , cnt,pathd,fil ,ptin=ptin
		 if (tu_wrun(z,7) eq 0) and (tu_wrun(z,8) gt 0)   then   begin
					pathd=tu_dir & inst='lamp'    & grp='' & cnt=1
		 endif else begin		       inst=tu_acc(0) & grp=tu_acc(3) & endelse

		 outext ='? '+pathd+fil+' is not accessible ...'
		 if cnt gt 0 then begin
		 
	   	 	widget_control,bad_id=i,tu_err,set_value='Reading ...'
			i=execute( 'w'+wnumber+'=0' )

			TOUCH_KP, 1,pathd,inst,tu_bic,grp
	     	 	status =  0 & P_DID_GETRUN, run ,wi, status
			TOUCH_KP, 0	     	 	
			to_don_history, wi,0,'w'+wnumber+'=RDRUN('+fir+') ;'+inst_value
			
	     	 	if status ne 0  then outext='% Restore '+pathd+fil+' failed ...' $
	     	 			else begin P_MUS,'mus_shot'
					     outext='raw data '+fil+' are restored in W'+wnumber
	     				endelse
		 endif
	   	 widget_control,bad_id=i,tu_err,set_value=outext
	   endelse
	endif    else widget_control,bad_id=i,tu_err,set_value='% Restore failed ...'  
return
end
pro TOUCH_CUS,idx,pathb
;** *********
@lamp.cbk
common tuch
		 if n_elements(tu_pth) ne n_elements(lamp_ins) then begin
		    tu_pth=strarr(n_elements(lamp_ins)) & bb='/usr/illdata/data'
		    if n_elements(findfile(bb)) gt 1 then tu_pth(*) = bb
		    tu_sub=intarr(n_elements(lamp_ins))
		    TOUCH_X , nothing  ,lamp_touch+lamp_dvd ,0
		    TOUCH_RP, lamp_ins ,tu_pth ,tu_sub
		 endif
		 idx  =where(lamp_ins eq tu_acc(0)) & idx=idx(0)>0
		 pathb=tu_pth(idx)
end
pro TOUCH_LOCALISE , cnt,pathd,fil ,zip ,ptin=ptin
;** **************
;**
@lamp.cbk
common tuch
		if n_elements(zip) eq 1 then zop=1 else zop=0
		zip=0
		ins=strlowcase(tu_acc(0))
		 
		TOUCH_CUS,idx,pathb
		f_sub=tu_sub(idx)
		pathd=tu_acc(4)
		ptin =strpos(strlowcase(pathd),ins)
		if  (strpos (strlowcase(tu_bic),'cycle') ge 0) and (ptin lt 0) $
		then pathd  =sys_dep      ('NEWSUB',pathd,ins)
	    	sub=ins+'_'+strmid(fil,1,1)
		if f_sub eq 0 then pathf=pathd else pathf=sys_dep      ('INSUB',pathd,sub)

	   	if zop eq 0 then widget_control,bad_id=i,tu_err,set_value='Searching ...'

		if (zop eq 1) and (tu_acc(2) ne '0') then begin
		 i=findfile ( pathf+fil,count=cnt)			 &    zip=1
		 if cnt eq 0 then i=findfile ( pathf+fil+'.Z',count=cnt) else zip=0
		 if cnt eq 0 then zip=0
		endif else cnt=1

		if (cnt le 0) or (zop eq 1) then pathd=pathf
return
end

pro TOUCH_B , flg ,instru ,xmod ,GROUP=gbase
;** *******
;**
;** Create interface.

@lamp.cbk
common tuch

i=xregistered('TOUCH')
if i le 0 then begin
	P_MUS,'mus_harp2'
	if n_elements(gbase) eq 1 then lamp_b1=gbase
	if n_elements(xmod)  gt 0 then tu_mod=xmod else tu_mod=''
	if (lamp_siz ge 800) or (tu_mod ne '') then nb_snap=5 else nb_snap=3
	
	if n_elements(lamp_don) eq 0 then lamp_don=[0]
	tu_catal=[' ']
	tu_list =['']
	tu_raw  =[0]
	tu_mid  =['']
	tu_dir  = ''
	tu_sdir = ''
	tu_nelmt=0 & tu_fi=0 & tu_li=0
	tu_index=[0]
	tu_repb	=0
	tu_rep	=0
	tu_fct	=0
	tu_forc =1
	tu_3D   =3
	tu_sn	=lonarr(nb_snap)
	tu_br	=lonarr(nb_snap)	
	tu_wrun =fltarr(nb_snap,9)
	b2_mid  =lonarr(nb_snap-2)
	tu_p21  =strarr(nb_snap,npars)
	tu_t21  =strarr(nb_snap)
	tu_one	=bytarr(192,192)
	tu_two	=bytarr(192,192)
	tu_all	=bytarr((192+18)*3,25) & tu_all(*,*)=180
	time	=!stime
	month	=strmid (time,3,3)
	tu_cylc	=round((strpos('JanFebMarAprMayJunJulAugSepOctNovDec',month) /3 +1)/2.3) >1
	year	=strmid (time,7,4)
	list_ins=lamp_ins
	list_cyc=['1 Jan-Mar','2 Mar-May','3 May-Jul','4 Jul-Sep','5 Sep-Dec','6 Noel','0 On_Line ']
	an	=fix(year)
	an2	=  1995+1
	list_an =['1995']
	while an2 le an do begin list_an =[strtrim(string(an2),2),list_an] & an2=an2+1 & endwhile

	on_ioerror,nocc & openr,lu,'/usr/illdata/data/CURRENT_CYCLE',/get_lun & trim=''
			  readf,lu,trim & yr=strmid(trim,0,2)
			  if yr gt strmid(year,2,2) then tu_cylc=(tu_cylc+1)<6 $
						    else tu_cylc=fix(strmid(trim,2,1))>1<5
			  free_lun,lu & nocc:
	cycls	=list_cyc(tu_cylc-1)

	if n_elements(instru) le 0 then instru=''
	if instru eq '' then instrument='Data' else instrument=instru
	
	if lamp_siz lt 900 then minu =7 		   else minu =0
	if tu_mod   ne ''  then tu_wh=9			   else tu_wh=45
	if tu_mod   eq ''  then vsel ='Select your Instrument to get the Experiments list' $
			   else vsel ='EXPERIMENT'
	if tu_mod   ne ''  then resrc='lamp'		   else resrc='lamptouch'
	if tu_mod   ne ''  then arrow=''		   else $
	if lamp_siz ge 800 then arrow=' ----> ' 	   else arrow='->'
	if lamp_siz ge 800 then pfor ='Parameters for '    else pfor ='N'
	if lamp_siz ge 800 then intow='into W_space'	   else intow='in'
	if lamp_siz ge 800 then tutub='... TOUCH BASE ...' else tutub='T.B'
	
	tit     ='Lamp Touch Base  (anonymous@ftp.ill.fr /pub/cs/) (email:lamp@ill.fr)'
	if ( tu_mod  ne '') and (lamp_siz lt 800) then $
	     tu_id =widget_base  (title=tit,/column,resource_name=resrc,x_scroll=950,y_scroll=lamp_siz-50)$
	else tu_id =widget_base  (title=tit,/column,resource_name=resrc,ypad=1)
	b_1	=widget_base  (tu_id ,/row)
	if  tu_mod  eq '' then b_2     = widget_base  (tu_id ,/row) $
			  else b_2     = widget_base  (tu_id ,/row,resource_name='mic')
	
;**	LIST OF EXPERIMENTS
;**	---- -- -----------
	if tu_mod   eq '' then b_list  = widget_base  (b_1   ,/column) $
			  else b_list  = widget_base  (b_1   ,/column,resource_name='don')
	b_inst  =widget_base  (b_list,/row)
	
	if arrow ne ''  then   $
	b_lab	=widget_label (b_inst ,value=arrow	 ,font=ft_b_normal)
	b_instr =widget_button(b_inst ,value=Instrument  ,font=ft_b_bigger,menu=2)
	b_lab	=widget_label (b_inst ,value=arrow	 ,font=ft_b_normal)
	b_year	=widget_button(b_inst ,value=Year	 ,font=ft_b_bigger,menu=2)
	b_lab	=widget_label (b_inst ,value=arrow	 ,font=ft_b_normal)
	b_cycl	=widget_button(b_inst ,value=cycls	 ,font=ft_b_bigger,menu=2)

	if tu_mod eq '' then b_exp =widget_list  (b_list,ysize=20-minu ,font=ft_b_normal,value=vsel) $
	else begin	b_row =widget_base  (b_list ,/row)
			b_exp =widget_list  (b_row,ysize=9,xsize= tu_wh,font=ft_propor  ,value=vsel)
			barcol=widget_base  (b_row,row=5)
			
			butf0 =widget_button(barcol,value='Data Reduction',font=ft_propor,menu=2)
			butf01=widget_button(butf0 ,value='GFIT      (GENERAL fitting)' $
								       ,font=ft_propor,uvalue=[-88,580,0,0])
			butf02=widget_button(butf0 ,value='INX       (TOF reduction) ' $
								       ,font=ft_propor,uvalue=[-88,338, 2])
			butf03=widget_button(butf0 ,value='TRIPX     (TAS reduction) ' $
								       ,font=ft_propor,uvalue=[-88,358])
			butf01=widget_button(butf0 ,value='SELECTOR  (Filter on Read)' $
								       ,font=ft_propor,uvalue=[-88,338, 4])
								       
			butf1 =widget_button(barcol,value='Display funct.',font=ft_propor,menu=2)
			butf11=widget_button(butf1 ,value='Superplot'     ,font=ft_b_normal,uvalue=[-88,352])
			P_BEN_CREATE ,butf1,1
			butf13=widget_button(butf1 ,value=lamp_fsite      ,font=ft_b_normal,uvalue=[-88,574,0])
			butf14=widget_button(butf1 ,value='Scan   '       ,font=ft_b_normal,uvalue=[-88,306,0,-1])
			b_labins(2)=butf13
			
			butf2 =widget_button(barcol,value='Import  Export',font=ft_propor,menu=2)
			butf21=widget_button(butf2 ,value='   Import   '  ,font=ft_propor,uvalue=[-88,380])
			butf22=widget_button(butf2 ,value='   Export   '  ,font=ft_propor,uvalue=[-88,370])
			butf2a=widget_button(butf2 ,value=lamp_asite      ,font=ft_propor)
			butf24=widget_button(butf2 ,value=' Save Session ',font=ft_propor,uvalue=[-88,397])
			butf25=widget_button(butf2 ,value=' Start Lamp '  ,font=ft_propor,uvalue=[-88,336,1])
			
			butf3 =widget_button(barcol,value='Macros. Params',font=ft_propor,menu=2)
			butf31=widget_button(butf3 ,value='User Macros'   ,font=ft_propor,uvalue=[-88,203,0,0])
			butf42=widget_button(butf3 ,value='Data Params'   ,font=ft_propor,uvalue=[-88,204])
			
			butf4 =widget_button(barcol,value=' The  Journal ',font=ft_propor,uvalue=[-88,396,0])

			cd,current=path
        		pwd_t =widget_text  (b_list,font=ft_propor  ,value=path,xsize=25,ysize=1,$
        							    /editable,/all_events)
			my_path(0)=path & my_path(2)=string(pwd_t)
			
			bar1_1=widget_base  (b_list,/row)
			btit1 =widget_label (bar1_1,font=ft_b_bigger,value=' FORMULA ENTRY')
        		bhelp =widget_button(bar1_1,font=ft_normal  ,value='?',uvalue=[-88,588,0])
        		
			prog_b=lonarr(6)
			if lamp_siz le 800 then n=4 else if lamp_siz lt 900 then n=5 else n=6
        		for i=0,n-3 do begin
       			    bar1_1   =widget_base  (b_list,/row)
	 		    prog_b(i)=widget_text  (bar1_1,font=ft_propor  ,xsize=24,ysize=1,/editable,$
        				  	           value=' ' ,uvalue=[-88,200])
	 		    bdo      =widget_button(bar1_1,font=ft_normal  ,$
	 		    				   value='do',uvalue=[-88,214,i,prog_b(i)])
			endfor
       endelse
	
	tu_uvk	=[-88,332,b_exp,b_instr,b_year,b_cycl,0,0]
	uval	=[-88,332,b_exp,b_instr,b_year,b_cycl,3,b_instr]
	
	gcur =' ' & entr1=b_instr & b_labins(0)=b_instr
	for i=0,n_elements(lamp_ins)-1 do begin
	      if  gcur ne  lamp_grp(i) then begin
		  gcur  =  lamp_grp(i)
		  if gcur eq ' ' then entr1=b_instr else $
		  entr1 =widget_button(b_instr,font=ft_b_normal,value=gcur,menu=2)
		  endif
	      bidon=widget_button(entr1  ,font=ft_b_normal,value=lamp_ins(i),uvalue=[uval,0,i])
	endfor
	      bidon=widget_button(b_instr,font=ft_bigger  ,value='CUSTOM',uvalue=[-88,560,0,0,0,-1,0,0])
	
	uval	=[-88,332,b_exp,b_instr,b_year,b_cycl,4,b_year]
	for i=0,n_elements(list_an) -1 do $
	   bidon=widget_button(b_year ,value=list_an(i)  ,font=ft_b_normal,uvalue= uval)

	uval	=[-88,332,b_exp,b_instr,b_year,b_cycl,5,b_cycl]
	n=n_elements(list_cyc)-1
	for i=0,n do begin
	   bidon=widget_button(b_cycl ,value=list_cyc(i) ,font=ft_b_normal,uvalue= uval)
;	   if i ge n-1 then widget_control,bidon,sensitive=0
	endfor
   
;**    LIST OF PARAMETERS OR DIDS
;**    ---- -- ---------- -- ----
       b_parm	=widget_base  (b_1   ,/column)
       tu_parm  =0
       tu_run   =0
       wread    =0
       ra	=0

       if tu_mod eq '' then begin
	b_run	=widget_base  (b_parm,/row)
	b_lab	=widget_label (b_run ,value= pfor       ,font=ft_b_normal)
	tu_run	=widget_label (b_run ,value='this Run'	,font=ft_b_normal)
	ra	=widget_label (b_run ,value='   '	,font=ft_b_normal)
	b_run2  =widget_base  (b_run ,/column)
	if sys_dep('MAP') ne -1 then $
	tu_restd=widget_button(b_run2,value='Restore Raw Data',font=ft_propor,$
					     resource_name='discret')    else $
	tu_restd=widget_button(b_run2,value='Restore Raw Data',font=ft_propor )
	if sys_dep('MAP') ne -1 then $
	tu_rests=widget_button(b_run2,value='Re-form SnapShot',font=ft_propor,$
					     resource_name='discret')    else $
	tu_rests=widget_button(b_run2,value='Restore SnapShot',font=ft_propor )
	bidon	=widget_label (b_run ,font=ft_b_normal,value=intow)
	bs1f	=widget_base  (b_run ,/row,/frame)
	if sys_dep('MAP') ne -1 then $
	bs1b1	=widget_button(bs1f  ,font=ft_smaller ,value='<-',resource_name='discret') else $
	bs1b1	=widget_button(bs1f  ,font=ft_smaller ,value='<-')
	wread	=widget_label (bs1f  ,font=ft_b_normal,value='W7')
	if sys_dep('MAP') ne -1 then $
	bs1b2	=widget_button(bs1f  ,font=ft_smaller ,value='->',resource_name='discret') else $
	bs1b2	=widget_button(bs1f  ,font=ft_smaller ,value='->')
	
	tu_parm =widget_text  (b_parm,xsize=40,ysize=17-minu	,font=ft_b_normal,/scroll)
	tu_err	=widget_label (b_parm,xsize=40*8,value='  '	,font=ft_propor)
	
	widget_control,bad_id=i,bs1b1    ,set_uvalue=[-88,310,wread,0]
	widget_control,bad_id=i,bs1b2    ,set_uvalue=[-88,311,wread,0]
	widget_control,bad_id=i,tu_restd ,set_uvalue=[-88,337,wread,1]
	widget_control,bad_id=i,tu_rests ,set_uvalue=[-88,337,wread,2]
	
	b_row	=widget_base  (b_parm ,/row)
       endif else begin
       		  lamp_did=widget_base(b_parm,resource_name='did')
	          P_DID_CREATE ,lamp_did
		  P_DATA_IDOL
		  bar1_1=widget_base  (b_parm,/row,resource_name='did')
		  bid   =widget_label (bar1_1,font=ft_b_bigger,value='RUNS SELECTOR')
        	  bhelp =widget_button(bar1_1,font=ft_normal  ,value='?',uvalue=[-88,592,0])
		  tu_err=widget_label (bar1_1,font=ft_propor  ,value=' ',xsize=(lamp_siz/2)<600>300)
		  bar1_1=widget_base  (b_parm,/row,resource_name='did')
		  bid   =widget_label (bar1_1,font=ft_b_normal,value='W2=')
        	  run1tx=widget_text  (bar1_1,font=ft_propor  ,xsize=20,ysize=1,/editable,$
        				  	               value=' ')
		  bid   =widget_label (bar1_1,font=ft_b_normal,value='W3=')
        	  run2tx=widget_text  (bar1_1,font=ft_propor  ,xsize=20,ysize=1,/editable,$
        				  	               value=' ')
		  bid   =widget_label (bar1_1,font=ft_b_normal,value='W4=')
        	  run3tx=widget_text  (bar1_1,font=ft_propor  ,xsize=20,ysize=1,/editable,$
        				  	               value=' ')
		  widget_control,run1tx,set_uvalue=[-88,338,1,2]
		  widget_control,run2tx,set_uvalue=[-88,338,1,3]
		  widget_control,run3tx,set_uvalue=[-88,338,1,4]
		  
		  b_row	=widget_base  (b_parm ,/row)
       endelse

	b_lab	=widget_label (b_row ,value=tutub       ,font=ft_biggest)
	b_ruw	=widget_base  (b_row ,/row,/exclusive)
	b_none	=widget_button(b_ruw ,value='None'   	,font=ft_b_normal,/no_release)
	b_imag	=widget_button(b_ruw ,value='Image'  	,font=ft_b_normal,/no_release)
	b_surf	=widget_button(b_ruw ,value='Surf'	,font=ft_b_normal,/no_release)
	b_cont	=widget_button(b_ruw ,value='Cont'	,font=ft_b_normal,/no_release)
	b_func	=widget_button(b_row ,value='Det.Counts',font=ft_b_normal,menu=2,resource_name='discret')

	uv=[-88,338,100,b_func,0,tu_run,tu_parm,ra]
	uv(2)=100 & b_f0=widget_button(b_func,value='Det.Counts' ,font=ft_b_normal,uvalue=uv)
	uv(2)=101 & b_f1=widget_button(b_func,value='X_proj(I)'  ,font=ft_b_normal,uvalue=uv)
	uv(2)=102 & b_f2=widget_button(b_func,value='Y_proj(I)'  ,font=ft_b_normal,uvalue=uv)
	uv(2)=103 & b_f3=widget_button(b_func,value='LOG(Xproj)' ,font=ft_b_normal,uvalue=uv)
	uv(2)=104 & b_f4=widget_button(b_func,value='LOG(Yproj)' ,font=ft_b_normal,uvalue=uv)
	uv(2)=105 & b_f5=widget_button(b_func,value='AVERAGE(I)' ,font=ft_b_normal,uvalue=uv)
	uv(2)=106 & b_f6=widget_button(b_func,value='DISTRIB(I)' ,font=ft_b_normal,uvalue=uv)
	uv(2)=107 & b_f7=widget_button(b_func,value='Count-Mean' ,font=ft_b_normal,uvalue=uv)
		    bid =widget_button(b_func,value='   ----   ' ,font=ft_b_normal)
	uv(2)=108 & b_f8=widget_button(b_func,value='TOF'        ,font=ft_b_normal,uvalue=uv)
		    b_fd=widget_button(b_func,value='3D Z_proj  ',font=ft_b_normal,menu=2)
	uv(3)=      b_fd
	uv(2)=121 & b_f =widget_button(b_fd  ,value='3D X_proj'  ,font=ft_b_normal,uvalue=uv)
	uv(2)=122 & b_f =widget_button(b_fd  ,value='3D Y_proj'  ,font=ft_b_normal,uvalue=uv)
	uv(2)=123 & b_f =widget_button(b_fd  ,value='3D Z_proj'  ,font=ft_b_normal,uvalue=uv)

;**	SNAPSHOT
;**	--------
	b_c1	=widget_base  (b_2   ,/column)
	b_c234	=widget_base  (b_2   ,/column)
	b_c5	=widget_base  (b_2   ,/column)
	b_r1	=widget_base  (b_c1  ,/row)
	b_r234	=widget_base  (b_c234,/row)
	b_r5	=widget_base  (b_c5  ,/row)
	
	b_2_1	=widget_base  (b_r1  ,/column)
	for i=0,nb_snap-3 do   b2_mid(i)=widget_base  (b_r234,/column)
	b_2_5	=widget_base  (b_r5  ,/column)
	
	xs=192 & ys=192
	tu_sn(0)	=widget_draw  (b_2_1 ,retain=2  ,xsize=xs,ysize=ys,/button_event)
	for i=0,nb_snap-3 do tu_sn(i+1)=widget_draw  (b2_mid(i) ,retain=2  ,xsize=xs,ysize=ys,/button_event)
	tu_sn(nb_snap-1)=widget_draw  (b_2_5 ,retain=2  ,xsize=xs,ysize=ys,/button_event)
	
	tu_br(0)	=widget_text  (b_2_1 ,value='      '	,font=ft_b_normal)
	for i=0,nb_snap-3 do tu_br(i+1)=widget_text  (b2_mid(i) ,value='      '	,font=ft_b_normal)
	tu_br(nb_snap-1)=widget_text  (b_2_5 ,value='      '	,font=ft_b_normal)

	if minu eq 0 then $
	bidon	=widget_label (b_c1  ,value=' ')
	b_clo	=widget_base  (b_c1  ,/row)
		 put_logo     ,b_clo
	tu_frun =widget_label (b_clo ,value='First Run ________'	,font=ft_b_normal)
	bidon	=widget_base  (b_c1  ,/row)
	done	=widget_button(bidon ,value='EXIT'	,font=ft_b_bigger)
	colo	=widget_button(bidon ,value='Colors'	,font=ft_b_bigger)
	help	=widget_button(bidon ,value='Idl?'	,font=ft_b_bigger,uvalue=[-88,201])
	tu_slid =widget_slider(b_c234,title=''	,xsize=(192+18)*(nb_snap-2),ysize=35-3*minu,font=ft_b_normal,/drag)
	tu_wall =widget_draw  (b_c234,retain=2  ,xsize=(192+18)*(nb_snap-2),ysize=25)
	if minu eq 0 then $
	bidon	=widget_label (b_c5  ,value=' ')
	tu_lrun =widget_label (b_c5  ,value='Last  Run ________'	 ,font=ft_b_normal)
	bidon	=widget_base  (b_c5  ,/row)
	updt	=widget_button(bidon ,value='Upd Last'   ,font=ft_b_bigger)
	manag	=widget_button(bidon ,value='Manage'     ,font=ft_b_bigger)
	
	widget_control,bad_id=i,tu_slid  ,set_uvalue=[-88,334,tu_slid,tu_frun,tu_lrun,tu_run,tu_parm,ra]
	widget_control,bad_id=i,done  	 ,set_uvalue=[-88,336, 0]
	widget_control,bad_id=i,colo  	 ,set_uvalue=[-88,338, 0]
	widget_control,bad_id=i,manag  	 ,set_uvalue=[-88,338, 3]
	widget_control,bad_id=i,updt  	 ,set_uvalue=[-88,338,-1 $
							     ,tu_slid,tu_frun,tu_lrun,tu_run,tu_parm,ra]
	widget_control,bad_id=i,b_exp    ,set_uvalue=[-88,333,tu_slid,tu_frun,tu_lrun,tu_run,tu_parm,ra]
	widget_control,bad_id=i,b_none   ,set_uvalue=[-88,335,0      ,tu_frun,tu_lrun,tu_run,tu_parm,ra]
	widget_control,bad_id=i,b_imag   ,set_uvalue=[-88,335,1      ,tu_frun,tu_lrun,tu_run,tu_parm,ra]
	widget_control,bad_id=i,b_surf   ,set_uvalue=[-88,335,2      ,tu_frun,tu_lrun,tu_run,tu_parm,ra]
	widget_control,bad_id=i,b_cont   ,set_uvalue=[-88,335,3      ,tu_frun,tu_lrun,tu_run,tu_parm,ra]

        bid=sys_dep      ('DYNLAB',tu_id,1)
	if lamp_b1 gt 0 then widget_control  ,tu_id,group_leader=lamp_b1,/realize $
			else widget_control  ,tu_id                     ,/realize
	put_logo

	if (n_elements(flg) lt 1) then flg=0
	if  flg eq 0 then begin
		widget_control,bad_id=i,tu_restd ,sensitive  =0
		widget_control,bad_id=i,tu_rests ,sensitive  =0
	endif
	widget_control,bad_id=i,b_imag ,set_button=1 & tu_rep=1
	loadct,3
	for n=0,nb_snap-1 do begin
		widget_control,bad_id=i,tu_sn(n) ,get_value =j
		widget_control,bad_id=i,tu_sn(n) ,set_uvalue=[-88,338,n+10,wread]
		tu_sn(n)=j & wset,j & erase,150
	endfor
	
	widget_control,bad_id=i,tu_wall,get_value=j & tu_wall=j & wset,j & erase,150

	if tu_mod  ne  ''  then begin
		widget_control,bad_id=i,butf2a,set_uvalue=[-88,575,tu_err]
		widget_control,bad_id=i,pwd_t ,set_uvalue=[-88,576,tu_err],SET_TEXT_SELECT=[strlen(path),0]
		widget_control,bad_id=i,tu_id ,default_font=ft_normal
		widget_control,/hourglass
		lamp_b1=tu_id
		P_DON_INIT_VAR     ,prog_b,tu_err
		P_AFTER_REALIZE_DID,0,0,0
		if instru eq '' then begin id=where(list_ins eq inst_value)
					   if id(0) ge 0 then  begin instru=inst_value
					   		 widget_control,b_instr,set_value=instru	
	endif & endif			&  endif

	if instru  ne  ''  then TOUCH_LIST, 0,tu_uvk

	XMANAGER,'TOUCH',tu_id,event_handler='LAMP_EVENT_PARSER',/just_reg,CLEANUP='TOUCH_KILL'

endif else begin
	widget_control,bad_id=i,tu_id,map=1
	if n_elements(tu_acc) ge 3 then TOUCH_LIST, 0,tu_uvk
endelse
return
end
