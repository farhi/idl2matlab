pro don_init_prog_mac ,flg
;** *****************
;**
@dons.cbk
@lamp.cbk

 common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
		wbeside,vfl,styles,w4d,smoo,vff

;Read in user command list
         on_ioerror, end_fc
	 in=-1
	 fifi='lamp.cds' & ii  = findfile(fifi,count=n)
	 if  n eq 0 then begin
	  CATCH,stat
	  if stat eq 0 then begin
	   hhm= sys_dep('HOME') & cd,hhm,current=mee & cd,mee,current=hmm
	   if strmid(hhm,strlen(hhm)-1,1) ne lamp_dvd then hhm=hhm+lamp_dvd
	   fifi=hmm+fifi & endif
 	 endif
         openr,in,fifi,/get_lun
	 n=n_elements(prog_mac)
         bstr=''
         for k=0,5 do begin
             readf,in,bstr
             prog_mac(k)=bstr
	     if flg eq 1 then $
	        if prog_txt(k) gt 0 then widget_control,prog_txt(k),bad_id=i,set_value=bstr
         endfor
         if flg eq -1 then begin
         	readf,in,bstr & bstr=''
         	readf,in,bstr & if bstr ne '' then lamp_devps=bstr
         	readf,in,bstr & bstr=''
         	readf,in,rx,rz,nlv,bstr
         	readf,in,bstr & bstr='' & s1=0 & s2=0 & s3=0 & s4=0
         	readf,in,s1,s2,s3,s4,bstr
         	styles(0,0)=s1 & styles(1,0)=s2 & styles(2,0)=s3 & !P.psym=s4
         	readf,in,bstr & bstr=''
         	readf,in,bstr & if strcompress(bstr) gt ' ' then begin
         				      inst_value=strtrim(bstr,2)
					      RDSET,inst=inst_value
         				     ;if b_labins(0) gt 0  then widget_control,bad_id=i,b_labins(0),set_value=inst_value
         				      endif
         	readf,in,bstr & bstr='' & s1=-1
         	readf,in,s1,bstr & if (s1 ge 0) and (s1 le 40) then tcol=s1
         	readf,in,bstr & bstr='' & s1=-1
         	readf,in,s1,bstr & if (s1 ge 0) then smoo=s1
		
         	readf,in,bstr
         	readf,in,bstr ;Free
         	readf,in,bstr ;Free
         	readf,in,bstr
		
         endif else for i=1,16 do readf,in,bstr
	 
         for k=6,n-1 do begin
             readf,in,bstr
             prog_mac(k)=bstr
         endfor
	 
end_fc:  if in gt 0 then free_lun,in

for i=0,n_elements(lamp_ins)-1 do if inst_value eq lamp_ins(i) then $
    				     inst_group =  lamp_grp(i)
return
end
;
;
pro p_don_init_var ,prog_base ,mess_base
;** **************
;**       Sets up variables
@lamp.cbk
@dons.cbk
	n=22
	if n_elements(prog_base) eq n then prog_txt =prog_base ;!!! why not
	prog_txt =lonarr(n) ;!!!
	
	if n_elements(mess_base) eq 1 then l_message=mess_base
	last_w  =1
	nwk     =1
	ifixed  =0
	formtxt =0
	his_info=0
	rawmanip=0
	mac_raw =0
	prog_mac=strarr(n_elements(prog_txt))
		prog_mac(0)='setcol,27'
		prog_mac(2)='w2=total(w1,2) ;X projection'
		prog_mac(3)='w3=w2-shift(w2,1) &  w3(0)=0'
		prog_mac(4)='x_tit(3)="Derivative" & x3=findgen(n_elements(w3))'
	don_init_prog_mac  ,1
return
end
;
pro p_don_create ,base
;** ************
;**       Sets up Formula windows etc

@lamp.cbk
@dons.cbk
;
	p_don_init_var

	formu =['Enter formula below','']
	umac='User'
	if GEORGE eq 1 then  begin dou  =" STATUS CONTROL" & formu(0)='' & umac='Dial'
	                     baso =widget_base(base  ,/row)
	                     baso1=widget_base(baso  ,/column)
	                     baso1=widget_base(baso1 ,/column)
	                     baso2=widget_base(baso  ,/column)
	                     baso2=widget_base(baso2 ,/column)
	                     basoo=widget_base(baso  ,/column)
	                     	basii=widget_base(basoo ,/row)
	                     	baso3=widget_base(basii ,/column)
	                     	basoL=widget_base(basoo)
	                     base =widget_base(map=0,group_leader=lamp_b1)
	endif     else       dou  =" MANIPULATIONS"
;
; Help and Macro buttons
;
; Text area for formula entry
;
	bar0        =widget_base(base   ,/row)
	bar001	    =widget_base(bar0   ,/column)
	macro_area_a=widget_base(bar0)

; First Column
; ***** ******
	bar01		=widget_base  (bar001 ,/row)
	bar1		=widget_base  (bar01  ,/column)

	bar1_1	=widget_base  (bar1   ,/row)
	btit1		=widget_label (bar1_1 ,font=ft_biggest,value=dou)
	bar1_x 	=widget_base  (bar1_1 ,/nonexclusive)
	if sys_dep('MAP') ne -1 then $
	mac_raw	=widget_button(bar1_x ,value='raw',font=ft_smaller,resource_name='discret') else $
	mac_raw	=widget_button(bar1_x ,value='raw',font=ft_smaller)
	bhelp		=widget_button(bar1_1 ,font=ft_normal ,value='?')

	if lamp_siz lt 900 then begin nbli1=2 & nbli2=2  & labx =350
	   formtxt	=widget_text  (bar1   ,font=ft_b_bigger,xsize=40,ysize=nbli1,/editable,value=formu)
	endif		   else begin nbli1=3 & nbli2=2  & labx =562
	   formtxt	=widget_text  (bar1   ,font=ft_b_bigger,xsize=40,ysize=nbli1,/editable,value=formu,/scroll)
	endelse
;
	bar2		=widget_base  (bar01 ,/column)
	mac_but	=widget_button(bar2   ,value=umac+' Macros?')
	idlbut	=widget_button(bar2   ,value='The Journal')
	lamp_don	=[lamp_don,idlbut]
	up_button	=widget_button(bar2   ,value='Data Params')
	lamp_don	=[lamp_don,up_button]
; *****
	l_message	=widget_label (bar001 ,font=ft_b_bigger ,xsize=labx,value='                 ')
; *****
	bar01		=widget_base  (bar001 ,/row)
	his_info	=widget_text  (bar01  ,font=ft_b_bigger ,xsize=40,ysize=nbli2,/scroll,$
					  value=lims(1:*))
	bar2		=widget_base  (bar01  ,/column)
	bar2_1	=widget_base  (bar2   ,/column,/exclusive)
	info_but	=widget_button(bar2_1 ,value='W Min,Max ' ,/no_release)
	his_but	=widget_button(bar2_1 ,value='W History ' ,/no_release)

; Second Column
; ****** ******
	prog_buttons
; Third Column
; ****** *****
	machin=sys_dep('MACHINE')
	if ((lamp_siz ge 800) and (lamp_siz le 950) and (machin eq 'win')) or $
	   ((lamp_siz ge 800) and (GEORGE eq 1)) or $
	   ((lamp_siz ge 800) and (lamp_siz lt 900))                       then begin
;	   ((lamp_siz ge 800) and (lamp_siz lt 900) and (machin eq 'mac')) then begin
		   w0=2 & LOGO,w0 & pax1=size(w0)
		   if GEORGE eq 1 then bose=widget_base (basii,/column) $
		   else                bose=widget_base (bar0 ,/column)
		                       bose=widget_base (bose ,/frame,/row)
		                lamp_ben(6)=widget_draw (bose ,retain=2,xsize=pax1(1),ysize=pax1(2),/button_event)
	endif
;
; Controls
        widget_control,bhelp     ,bad_id=i,set_uvalue=[-88,588,0,0]
        widget_control,formtxt   ,bad_id=i,set_uvalue=[-88,200,0,0]
        widget_control,idlbut    ,bad_id=i,set_uvalue=[-88,396,0]
        widget_control,his_but   ,bad_id=i,set_uvalue=[-88,202,0,0]
        widget_control,mac_but   ,bad_id=i,set_uvalue=[-88,203,0,0]
        widget_control,up_button ,bad_id=i,set_uvalue=[-88,204,0,0]
        widget_control,info_but  ,bad_id=i,set_uvalue=[-88,207,0,0]
        widget_control,mac_raw   ,bad_id=i,set_uvalue=[-88,212,0]
        widget_control,his_info  ,bad_id=i,set_uvalue=[-88,215,0,0]
;;;;    widget_control,save_but  ,bad_id=i,set_uvalue=[-88,370,0,0]
     
	lamp_focus	=formtxt

	if GEORGE eq 1  then begin widget_control,base,/REALIZE
			XMANAGER, 'Don beside' ,base, event_handler='LAMP_EVENT_PARSER',/just_reg
			for k=0,2 do begin
				if prog_mac(k) eq '' then prog_mac(k)=' '
				baso11=widget_base  (baso1,/row)
				button=widget_button(baso11,value='Do')
				text  =widget_text  (baso11,value=prog_mac(k),font=ft_propor,/editable,xsize=30,ysize=1)
				widget_control,button,set_uvalue=[-88,214,0,text,0]   &   prog_txt(k)=text
				widget_control,text  ,set_uvalue=[-88,214,0,text,0]   &   endfor
			baso21 =widget_base   (baso2 ,/row)
			mac_but=widget_button (baso21,value='Macros' ,uvalue=[-88,203,0,0],font=ft_normal)
			idlbut =widget_button (baso21,value='Journal',uvalue=[-88,396,0,0],font=ft_normal)
			baso31 =widget_base   (baso3 ,/row) & k=3
				if prog_mac(k) eq '' then prog_mac(k)=' '
				text  =widget_text  (baso31,value=prog_mac(k),font=ft_propor,/editable,xsize=30,ysize=1)
				button=widget_button(baso31,uvalue=[-88,214,0,text,0],value='Do')
				widget_control,text ,   set_uvalue=[-88,214,0,text,0] &   prog_txt(k)=text
				;text   =widget_text   (baso21,value='Ctrl:'  ,font=ft_propor,/editable,xsize=30,ysize=1,resource_name="geo")
				;button =widget_button (baso21,value='Send'   ,uvalue=[-88,614,0,text,0])
				;widget_control,text   ,  set_uvalue=[-88,614,0,text,0]

			baso22 =widget_base   (baso2,/row)
			bhelp	 =widget_button (baso22,value='?'      ,uvalue=[-88,588,0,0],font=ft_normal)
			baso32 =widget_base   (baso3 ,/row) & k=4
				if prog_mac(k) eq '' then prog_mac(k)=' '
				text  =widget_text  (baso32,value=prog_mac(k),font=ft_propor,/editable,xsize=30,ysize=1)
				button=widget_button(baso32,uvalue=[-88,214,0,text,0],value='Do')
				widget_control,text ,   set_uvalue=[-88,214,0,text,0] &   prog_txt(k)=text

			baso23 =widget_base   (baso2,/row)
			bact   =widget_base   (baso23,/nonexclusive)
			bact   =widget_button (bact  ,value='Activity ->',uvalue=[-88,660,0,0],font=ft_normal)

			l_message=widget_label(basoL,font=ft_b_bigger ,xsize=labx<500,value='                 ')
	endif
return
end
;
pro prog_buttons
;** ************
;**
@lamp.cbk
@dons.cbk
	macro_area_b  =widget_base(macro_area_a,/column)
	if lamp_siz lt 900 then n=5 else n=6
	dou="Do" & uv=214 & prpt=''
	if (sys_dep('MACHINE') eq 'win') and (sys_dep('VERSION') lt '5.3') then txev=0 else txev=1
	for k=0,n-1 do begin
	    if prpt ne '' then if strpos(prog_mac(k),':') ne 4 then prog_mac(k)=prpt
	    base  =widget_base   ( macro_area_b,/row)
	    if prog_mac(k) eq '' then prog_mac(k)=' '
	    if prpt ne '' then $
 	         text=widget_text( base,value=prog_mac(k),font=ft_propor,/editable,$
 	                           xsize=26,ysize=1,resource_name="geo")           $
	    else text=widget_text( base,value=prog_mac(k),font=ft_propor,/editable,$
 	                           xsize=26,ysize=1)
	    button=widget_button( base,value=dou)

	    widget_control,button,bad_id=i,set_uvalue=[-88,uv,k,text,0]
	    if txev then $
	    widget_control,text  ,bad_id=i,set_uvalue=[-88,uv,k,text,0]
	    prog_txt(k)=text
	endfor
	return
	end

pro don_do_cmd, prox=fiprox
;** **********
;**
@lamp.cbk
@dons.cbk
common c_dondo, bas,nbd,fullist

	nprox=n_elements(fiprox) & if nprox gt 0 then idpx=lonarr(nprox)-1
	if xregistered('DoCmd') ne 0 then if nprox gt 0 then widget_control,bas,/destroy
	if xregistered('DoCmd') eq 0 then begin
		bas=widget_base  (title='DO commands',/column,resource_name="lampmic") & nbd=0
		llg=widget_base  (bas,/row) & put_logo,llg
		bid=widget_button(llg,value='?',font=ft_b_normal,uvalue=[-88,588,0,0])
		bid=widget_label (llg,value='      More....',font=ft_b_normal)
		bid=widget_button(llg,value='+2',font=ft_b_normal,uvalue=[-88,224])
		b_labins(8)=bas
		fullist=[' ']
		MAC_LIST,n_em,fullist,maclist,THISFILE='*.prox'
		moclist=strlowcase(strmid(maclist,0,5))
		idx    =where((moclist ne 'dial_') and (moclist ne 'list_') and (moclist ne 'templ'))
		if idx(0) ge 0 then begin
		                        maclist =maclist(idx) & fullist=fullist(idx)
		   idx=sort(maclist) &  maclist =maclist(idx) & fullist=fullist(idx)
		   widget_control,llg,set_uvalue=maclist
		   bid=widget_label (llg,value='   '       ,font=ft_b_normal)
		   if nprox eq 0 then biron=1 else biron=0
		   bil=widget_button(llg,value='Prox files',font=ft_b_normal,uvalue=fullist,menu=2)
		   bir=widget_button(widget_base(llg,/nonexclusive),value='replace',font=ft_smallest,uvalue=[-88,225,-1,biron])
		   widget_control,bir,set_button=biron
		   for i= 0,n_elements(maclist)-1 do begin   uvprox=[-88,225,i,bil,llg,bas,bir]
		      bid=widget_button(bil,value=maclist(i),uvalue=uvprox,font=ft_b_normal)
		      if nprox gt 0 then begin idxpx=where(strlowcase(fiprox) eq maclist(i)) & if idxpx(0) ge 0 then idpx(idxpx(0))=i & endif
		   endfor
		endif
	endif
	if nbd lt n_elements(prog_txt)-1 then begin
	n=n_elements(prog_mac)
	if (sys_dep('MACHINE') eq 'win') and (sys_dep('VERSION') lt '5.3') then txev=0 else txev=1
	for k=0,1 do begin
	 if nbd ge n  then val=' ' else val=prog_mac(nbd)
	 if val eq '' then val=' '
	 bdo=widget_base  (bas,/row)
	 txt=widget_text  (bdo,value=val,font=ft_propor,/editable,xsize=40,ysize=1)
	 tdo=widget_button(bdo,value='Do',font=ft_b_normal)
	 if txev then $
	 widget_control,txt,bad_id=i,set_uvalue=[-88,214,nbd<(n-1),txt,0]
	 widget_control,tdo,bad_id=i,set_uvalue=[-88,214,nbd<(n-1),txt,0]
	 if (nbd gt 6) and (nbd lt n) then prog_txt(nbd)=txt
	 nbd=nbd+1
	endfor
	endif

	if xregistered('DoCmd') eq 0 then begin
		widget_control  ,bas,group_leader=lamp_b1,/realize & put_logo
		XMANAGER,'DoCmd',bas,event_handler='LAMP_EVENT_PARSER',/just_reg
		if nprox gt 0 then for i=0,nprox-1 do if idpx(i) ge 0 then don_do_prox,0,[-88,225,idpx(i),uvprox(3:6)]
	endif
end

pro don_do_prox, event, uv
;** ***********
;**
@lamp.cbk
@dons.cbk
	if uv(2) lt 0 then begin
		widget_control,event.id,set_uvalue=[-88,225,-1,event.select] & return
		endif
	files=['']
	widget_control,uv(3) ,bad_id =ii,get_uvalue=files
	widget_control,uv(4) ,bad_id =ii,get_uvalue=filx
	
	on_ioerror,misfil
	OPENR,u,files(uv(2)),/get_lun
	on_ioerror,eoffil
	contain=strarr(600) & READF,u,contain
	eoffil: free_lun,u
	idx=where(contain gt ' ')
	if idx(0) eq -1 then return
	contain=strtrim(strcompress(contain(idx)),2)
	
	bax=widget_base  (title=files(uv(2)),/column,resource_name="lampmic")
	
	llg=widget_base  (bax,/row) & put_logo,llg
	widget_control,llg,set_uvalue=filx
	bid=widget_button(llg,value='Play all',font=ft_b_normal,uvalue=[-88,214,-1,0,bax])
	bid=widget_label (llg,value='   '     ,font=ft_b_normal)
	biron=1
	bil=widget_button(llg,value='Prox files',font=ft_b_normal,uvalue=files,menu=2)
	bir=widget_button(widget_base(llg,/nonexclusive),value='replace',font=ft_smallest,uvalue=[-88,225,-1,biron])
	widget_control,bir,set_button=biron
	for i= 0,n_elements(filx)-1 do bid=widget_button(bil,value=filx(i),uvalue=[-88,225,i,bil,llg,bax,bir],font=ft_b_normal)

	if lamp_siz lt 900 then cnt=19 else if lamp_siz lt 1300 then cnt=23 else cnt=27
	if n_elements(contain) le cnt   then $
	box=widget_base  (bax,/column)  else $
	box=widget_base  (bax,/column,y_scroll=long(cnt*31.58))
	
	if (sys_dep('MACHINE') eq 'win') and (sys_dep('VERSION') lt '5.3') then txev=0 else txev=1
	if sys_dep('VERSION') ge 4.0 then begin
		bdu=widget_base  (box,/row)
		txt=widget_text  (bdu,value='Just bidon',font=ft_propor,/editable,xsize=40,ysize=1)
		tdo=widget_button(bdu,value='Do',font=ft_b_normal)
		gml=widget_info(bdu,/geometry) & stx=gml.scr_xsize+2*gml.margin
		widget_control,bdu,/destroy
	endif else stx=350

	FOR i=0,n_elements(contain)-1 do begin
	    if strmid(contain(i),0,1) eq ';' then begin
		bid=widget_label (box,value=contain(i),font=ft_smaller,xsize=stx)
	    endif else begin
		bdo=widget_base  (box,/row)
		txt=widget_text  (bdo,value=contain(i),font=ft_propor,/editable,xsize=40,ysize=1)
		tdo=widget_button(bdo,value='Do',font=ft_b_normal)
		if txev then $
		widget_control,txt,set_uvalue=[-88,214,-1,txt,0]
		widget_control,tdo,set_uvalue=[-88,214,-1,txt,0]
		if n_elements(tot) eq 0 then tot=txt else tot=[tot,txt]
	    endelse
	ENDFOR
	if n_elements(bdo) eq 1 then begin
	   if n_elements(contain) le 10 then stxp=0 else stxp=35
	   widget_control,bax,set_uvalue=tot
	   widget_control,box,scr_xsize=stx+stxp

	   widget_control,uv(6),bad_id=ii,get_uvalue=biroff
	   if biroff(3) then widget_control,uv(5),bad_id=ii,/destroy
	   widget_control,bax,group_leader=lamp_b1,/realize  & put_logo
	   XMANAGER,'Prox_'+strtrim(string(uv(2)),2),bax,event_handler='LAMP_EVENT_PARSER',/just_reg
	endif
	return
misfil: print,!err_string
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro	p_don_event  ,event,uv
;**	***********
;**
;
;  299     Destroy alert windows
       if uv(1) eq 299 then begin wait,.3 & widget_control,event.top,/destroy & endif
;
;  200     CR in formula entry
      if uv(1) eq 200 then form_in,event
;
;  201     IDL help
      if uv(1) eq 201 then MANUAL
;     if uv(1) eq 201 then if sys_dep('MAP') le 0 then man_proc,''
;     if uv(1) eq 201 then if sys_dep('MAP') le 0 then man_proc,'' $
;		      else spawn,'$IDL_DIR/bin/idlhelp&'
;
;  202     History
      if uv(1) eq 202 then history,event
;
;  203     Update macros
      if uv(1) eq 203 then macro_files,event,uv
;
;  204     Display user parameters
      if uv(1) eq 204 then par_disp,event
;
;  205     Update user parameters
      if uv(1) eq 205 then par_mod,event,uv(2),uv(3)
;
;  206     Update NWK
      if uv(1) eq 206 then nwk_mod,event,uv(2),uv(3),uv(4)
;
;  207     Update limits
      if uv(1) eq 207 then limits,event
;
;  210     Fire instrument macro
;     if uv(1) eq 210 then fire_inst_mac,event,uv(2)
;
;  211     Display *.pro file
      if uv(1) eq 211 then pro_list,event
;
;  212
      if uv(1) eq 212 then begin
	if event.select then	  setmanip,/raw	 else	   setmanip
	if event.select then txt="SETMANIP,/raw" else txt="SETMANIP,/noraw"
	to_don_history,-1,0, txt
      endif
;
;  213     Set up programable button window
      if uv(1) eq 213 then prog_buttons
;
;  214     Fire instrument macro
      if uv(1) eq 214 then fire_prog_mac,event,uv(2),uv(3),uv(4)
;
;  215     Set up current workspace
      if uv(1) eq 215 then set_cur_work,event
;
;  216     Create a .pro file
      if uv(1) eq 216 then pro_create ,uv
;
;  217     Compile a .pro file
      if uv(1) eq 217 then pro_compile
;
;  218     Show INTERNALS
      if uv(1) eq 218 then show_internal,uv
;
;  219     Create a .pro file
      if uv(1) eq 219 then pro_creater ,uv
;
;  222     DO not use
;
;  224     Create new DO command
      if uv(1) eq 224 then don_do_cmd
;
;  225     Create new DO prox window
      if uv(1) eq 225 then don_do_prox, event,uv
;
;  226     Create XBU dial window
      if uv(1) eq 226 then DialInit,'xbu'
;
return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro setmanip,raw=raw,noraw=noraw
;** ********
;**
@dons.cbk
if keyword_set(raw) then rawmanip=1 else rawmanip=0
if mac_raw  gt 0    then widget_control,mac_raw,bad_id=ii,set_button=rawmanip
end

pro form_in,event
;** *******
;**
;FORMULA WINDOW
;
@lamp.cbk
@dons.cbk

;       Reads Formula Windows 
;

      widget_control,event.id,get_value=formu

      n        =n_elements (formu)-1
      formu(n) =strtrim    (formu(n),2)
      
      index    =where(formu ne '')
      if n_elements(index)  le n then formu=formu(index)
      nelement =n_elements (formu)-1
      if n gt nelement  then begin 
      			widget_control,event.id,set_value=formu(0:nelement)
      			widget_control,event.id,set_value='',/append,/no_newline,$
						set_text_top_line=nelement-1
			endif
      if nelement gt 20 then begin
      			widget_control,event.id,set_value=formu(nelement-10:nelement)
			widget_control,event.id,set_value='',/append,/no_newline,$
						set_text_top_line=10-1
			endif
      
if (last_form ne formu(nelement)) or  (nelement eq n) then begin
		 ifixed=1 & xfor=formu(nelement) & xicute,xfor & endif
return
end

pro form_out,outxt
;** ********
;**
@dons.cbk
	if formtxt gt 0 then begin
           widget_control,formtxt,bad_id=i,get_value=formu
           n=(n_elements(formu)-1) >0
	     formu=[formu((n-15)>0:n),outxt]
           n=(n_elements(formu)-1) >0
      	   widget_control,formtxt,bad_id=i,set_value=formu
           widget_control,formtxt,bad_id=i,set_value=''   ,/append,/no_newline,$
							   set_text_top_line=n
	endif else print,outxt
end

pro commsi ,file, params, macro=extxt, exec=exec
;** ******
;**
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
@lamp.cbk
on_ioerror,miscom
line='' & extxt='' & u=-1
OPENR,u,file,/get_lun
      while (not EOF(u)) do begin READF,u,line & extxt=[extxt,line] & endwhile
miscom:if u gt 0 then begin free_lun,u
		nn= n_elements(params)<(lamp_sys+3)
		if nn gt 0 then for ii=1,nn do begin
		    jj  =strtrim(string(ii),2)
		    xfor='par'+jj+'='+params(ii-1)
		    kk  =execute(xfor)
		endfor
		extxt= extxt(1:*)
		if keyword_set(exec) then COMMCA,extxt
		if nn gt 0 then for ii=1,nn do begin
		    jj  =strtrim(string(ii),2)
		    xfor=params (ii-1)+'=par'+jj
		    kk  =execute(xfor)
		endfor
       endif
end
pro commca ,extxt, prox
;** ******
;**
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
common c_lamp_par

	i1=0L & i2=n_elements(extxt)-1
	if n_elements(prox) eq 2 then if prox(0) ge 0 then begin i1=prox(0)
	                              i2=prox(1)  &  endif
	for kk=i1,i2 do begin xfor=extxt(kk) & jj=execute(xfor) & endfor
end

pro xicuter,intxt
;** *******
;**
@lamp.cbk
@dons.cbk
	ii=strpos(strlowcase(intxt),'passw')
	if formtxt gt 0 then if ii lt 0 then begin
           widget_control,formtxt,bad_id=i,set_value=intxt,/append
           widget_control,formtxt,bad_id=i,get_value=formu
           n=(n_elements(formu)-1) >0
           widget_control,formtxt,bad_id=i,set_value=''   ,/append,/no_newline,$
							   set_text_top_line=n
	endif
	ifixed=1 & xicute,intxt
return
end

pro xicute,intxt
;** ******
;**
@lamp.cbk
@dons.cbk
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

  on_ioerror, mis
  GEORGEO, COMMAND=intxt
      
  last_form=intxt & lwtxt=strlowcase(intxt)

  pvirg=strpos(intxt,';') & if pvirg lt 0 then pvirg=1000
  
  if pvirg             eq 0 then begin & endif else $

  if strpos(intxt,'$') eq 0 then begin
     len=strlen(intxt)
     if b_labins(3) ne 2 then spawn,strmid(intxt,1,len)

  endif else $
  if (strpos(intxt,'&') gt 0) and (strpos(intxt,'&') lt pvirg) and (strpos(lwtxt,'begin') lt 0) then begin
     com_split,last_form

  endif else $
  if strpos(intxt,'@') eq 0 then begin
     intxtp=strmid(intxt,0,pvirg)
     sep=str_sep(intxtp,',')
     sep=strtrim(sep,2)
     if  strpos (sep(0),'.')  lt 0 then file_name=sep(0)+'.prox' else file_name=sep(0)
     len=strlen (file_name) & file_name=strmid(file_name,1,len)
     if n_elements(sep) eq 1 then  com_file,file_name  $
                             else  begin to_don_history, -1,0, intxt
                                         commsi,file_name, sep(1:*), /EXEC  & endelse
  endif else $
  if strpos(intxt,'?') eq 0 then begin
;    if sys_dep('MAP') le 0 then man_proc,''
     if sys_dep('VERSION') ge 5.0 then online_help else $
     if sys_dep('MAP') gt 0 then spawn,'$IDL_DIR/bin/idlhelp&'

  endif else $
  if strpos(lwtxt,'retall')   eq 0 then begin		 & endif else $
  if       (lwtxt eq 'lamp')       then begin		 & endif else $
  if strpos(lwtxt,'saveses,') eq 0 then begin SaveSession  & endif else $
  if       (lwtxt eq 'save')       then begin SaveSession  & endif else $
  if       (lwtxt eq 'stop')       then begin P_LAMP_STOP  & endif else $
  if strpos(lwtxt,'sho')      eq 0 then begin show,intxt   & endif else begin

      if strpos(lwtxt,'exit')   eq 0 then DON_WRITE_PROG_MAC ,0

;**   Inspect line for "W" or "Dials"
;**   -------------------------------
      ii=strpos(intxt,';') & if ii gt 0 then intxt=strmid(intxt,0,ii)

      find_w1_w2, intxt ,line_2,one,two,three ,alone ,splitxyz ,opp_r

      if (one ge 0) and (one le lamp_sys+3) then begin
	 if (two lt 0) or (two gt lamp_sys+3) then two=0

     	 if one gt 0 then begin
;**	  SAVE one parameters
;**	  ---- --- ----------
	  one_str  =strtrim(string(one)  ,2)
	  two_str  =strtrim(string(two)  ,2)
	  three_str=strtrim(string(three),2)
	  
	  					MOVEPAR, (one),one_str, 0,'0'
	  
;**	  PUT two parameters in one
;**	  --- --- ---------- -- ---
     	  if two ne 0 then begin
     	   sz_two=[0L]
	   iii   =execute('sz_two=size(w' +two_str+')')

	   calc_e=0
	   rawraw=rawmanip
	   if strpos(last_form,';++') gt 0 then rawraw=0 else $
	   if strpos(last_form,';--') gt 0 then rawraw=1
	   if (not rawraw) then $
	   if  sz_two(sz_two(0)+2) gt 1 then $
	   if (three gt 0) and ((opp_r eq '+') or (opp_r eq '-') or (opp_r eq '*') or (opp_r eq '/')) then begin
	      sz_three=[0L] & iii=execute('sz_three=size(w' +three_str+')')
	      if sz_three(sz_three(0)+2) gt 1 then begin

;**		First if third W is here then update n,e
;**		----- -- ----- - -- ---- ---- ------ ---
		tix1=''   & tix2='' & tix3='E'+one_str & tix4='' & kp_n='' & kp_e=''
		nenn=1L   & iii=execute('nenn=n_elements(n'+two_str   +'(*,0))')
		nenk=1L   & iii=execute('nenk=n_elements(n'+three_str +'(*,0))')
		jjn =0.   & iii=execute('jjn =total(n'+two_str  +'(*,0))/nenn')
		jjk =jjn  & iii=execute('jjk =total(n'+three_str+'(*,0))/nenk')

		jje =1L   & iii=execute('jje=2*sz_two(sz_two(0)+2)-n_elements(e'+two_str+')-n_elements(e'+three_str+')')

		tsto=((opp_r eq '-') and (jjn ne jjk))
		tstn=((jjn le 1) and (jjk le 1)) or ((nenn eq 1) and (nenk eq 1))
		tstx=1L   & iii=execute('tstx=(x'+two_str  +'(0) eq x'+three_str+'(0)) and '     + $
					     '(x'+two_str  +'(n_elements(x'+two_str  +')-1)  eq '+ $
					      'x'+three_str+'(n_elements(x'+three_str+')-1)) and'+ $
					     '(sz_two(0) eq sz_three(0))')

;		Monitors are # or X different then W_ACCU is used
;		*************************** **** ****************
		if (( opp_r eq '+') or (opp_r eq '-')) and (splitxyz(0) ne 'yes') $
						       and ((not tstx) or (tsto)) then begin

		   if opp_r eq '+' then ads=      ',add='   else ads=',sub='

		   roaw='' ;if monimon lt 0 then roaw=',/raw'
		   tt  =tolerance
		   tix4=last_form
		   plac=0
		   if (sz_two(0) ne sz_three(0)) then begin
		      plac=1
		      tix4='; Incompatible dimensions ...'
		      if (sz_two(1) eq sz_three(1))   then begin
		      if (sz_two(0) eq 2) and (sz_three(0) eq 1) then $
		      			   tix4='W=w'+two_str+' & for i=0,'+strtrim(string(sz_two(2)-1),2)+ $
		      			        ' do w(*,i)=w'  +two_str+'(*,i) '+opp_r+' w'  +three_str+' & w'+one_str+'=W' $
		      else $
		      if (sz_two(0) eq 3) and (sz_three(0) eq 2) then $
		      			   tix4='W=w'+two_str+' & for i=0,'+strtrim(string(sz_two(3)-1),2)+ $
		      			        ' do w(*,*,i)=w'+two_str+'(*,*,i) '+opp_r+' w'+three_str+' & w'+one_str+'=W'
		      endif
		   endif else begin
		      if one eq two   then tix4='W_ACCU,accu='+  one_str+  ads  +three_str +',tol=tt'+roaw else $
		      if one eq three then begin
					   tix4='W_ACCU,accu='+three_str+  ads  +two_str	+',tol=tt'+roaw
					   if opp_r eq '-'  then tix4=tix4+'& w'+one_str+'=-w'+one_str
			        endif else tix4='W'+one_str+'=0'$
							   +' & W_ACCU,accu='+one_str+',add='+  two_str +',tol=tt' $
							   +' & W_ACCU,accu='+one_str+  ads  +three_str +',tol=tt'+roaw
		   endelse
		   toler=tt
		   oon=one & if one ne two then too=two else too=three
		   laa=last_form & ifixed=0

		   if  (plac) then begin
			 iii=EXECUTE(tix4)
			 if one ne two then MOVEPAR ,(two),two_str , (one),one_str
		   endif else begin
			 XICUTE, tix4
			 if (jjk+jjn gt 1) then tix4='N'+one_str+' average ... ' else tix4=''
			 if (jje eq 0)     then tix4=tix4+'E'+one_str+' evaluated ...'
			 if opp_r eq '-'   then $
			 if (jjk ne jjn)   and (not tstn) then tix4='n'+three_str+' and n'+two_str+' are different ...'
		   endelse
		   if l_message gt 0 then widget_control,bad_id=iii,l_message  ,set_value=tix4 else print,tix4
		   if b_labins(6)    then if b_labins(7) gt 0 then $
		                          widget_control,bad_id=iii,b_labins(7),set_value=tix4

		   last_w=oon & to_don_history, oon , too , laa+';W_ACCU'
		   RETURN
		endif else begin
;**		N..
		   if jjn gt 0 then begin
		      if  one ne two then kp_n='n0' else kp_n='n'+one_str+'(*,0)'
		      if (opp_r ne '-') then begin
			tix0= kp_n      +'=n'+two_str+'(*,0)'+opp_r+'n'+three_str+'(*,0)'
			tix1='n'+one_str+'=n'+two_str        +opp_r+'n'+three_str
			iii=execute(tix0) & if iii ne 1 then tix1=''
		      endif
		   endif
;**		E..
		   if jje eq 0 then begin
		      if one ne two then kp_e='e0' else kp_e=tix3
		      if (opp_r eq '+') then tix2='=SQRT( e'+two_str+'^2+e'+three_str+'^2)'
		      if (opp_r eq '-') then tix2='=SQRT( e'+two_str+'^2+e'+three_str+'^2)'
		      if (opp_r eq '*') then tix2='=SQRT((e'+two_str+'*w'+three_str+')^2+(e'+three_str+'*w'+two_str+')^2)'
		      if (opp_r eq '/') then tix2='=SQRT((e'+two_str+'/w'+three_str+')^2+(e'+three_str+'*w'+two_str+'/w'+three_str+'^2)^2)'
		      xfor=kp_e+tix2
		      iii=execute(xfor) & if iii ne 1 then tix2='' else if tix1 ne '' then tix1=tix1+' & '
		   endif
		endelse
		if tix1+tix2 ne '' then  calc_e=1
	      endif
	   endif
	   
	   if one ne two then begin 		MOVEPAR ,(two),two_str , (one),one_str
						if calc_e ne 0 then if tix1 ne '' then iii=execute('n'+one_str+'(*,0)='+kp_n)
						if calc_e ne 0 then if tix2 ne '' then iii=execute('e'+one_str+     '='+kp_e)
	   					endif
;**	  ELSE CLEAR one parameters
;**	  ---- ----- --- ----------
     	  endif else if strpos(lwtxt,'/compl') eq -1 then	CLEARPAR,(one),one_str

     	 endif
      if l_message gt 0 then widget_control,bad_id=iii,l_message  ,set_value=' '
      if b_labins(6)    then if b_labins(7) gt 0 then $
			     widget_control,bad_id=iii,b_labins(7),set_value=' '
      
      datpon=strpos(strlowcase(strcompress(last_form,/remove_all)),',datp')
      if datpon gt 0 then SETDATP,datp

      stat=0 & jjj=1
      on_error,1 & catch,stat
      xfor=last_form
      if (stat eq 0) and (jjj eq 1) then begin  ok=0
        if (sys_dep('VERSION') lt '5.3') then if (sys_dep('MACHINE') eq 'win') then $
        if (not sys_dep('EMBEDDED'))     then if (not sys_dep('RUNTIME')) then begin
      			on_ioerror,misexc & un=-1
      			openw ,un,'exelamp.pro',/get_lun
      			printf,un,'pro exelamp'		& printf,un,'@lamp.cbk'
      			printf,un,'common for_users'	& printf,un, xfor
      			printf,un,'end'			& free_lun,un & ok=1
      			resolve_routine,'exelamp' & exelamp & misexc:
      			endif
        if (not ok) then jjj=EXECUTE(xfor)     ;<--------------------
      endif
      if jjj ne 1 then print,!err_string
      if n_elements(one_str) eq 0 then one=0 ;Recursivity Problem ...

      if (stat ne 0) or  (jjj ne 1) then begin
      		     catch,/cancel
      		     catch,stat & if stat ne 0 then return
      		     P_MUS,'mus_cannon'
      		     therror=strmid(!err_string,0,65)
		     if l_message gt 0 then $
      			widget_control,bad_id=iii,l_message  ,set_value=therror $
		     else print,!err_string
		     if b_labins(6)    then if b_labins(7) gt 0 then $
			widget_control,bad_id=iii,b_labins(7),set_value=therror
      		     print,string(7b)
;**	  		ERROR RESTORE one parameters
;**	  		----- ------- --- ----------
     	 		if   one gt 0 then 	MOVEPAR, 0,'0' , (one),one_str
			if ((one gt 0) or (alone gt 0)) and (ifixed eq 1) then begin
         		     jou_c=[jou_c,last_form]
         		     jou_w=[jou_w,'??? '+!err_string]
         		endif
		     ifixed=0 & return
      endif else begin
      
      if datpon gt 0 then GETDATP,datp

;	 Place x,y title correctly when nb dimensions change.
;	 ----- --- ----- --------- ---- -- ---------- ------
     	 if (one gt 0) and (two gt 0) then begin
     	 	sz_one=[0L] & sz_err=[0L] & sz_mon=[0L] & sz_x=[0L] & sz_y=[0L]
		iii   =execute('if n_elements(w'+one_str+') gt 1 then w'+one_str+ '=reform(w' +one_str+ ',/overwrite)')
		iii   =execute('if n_elements(x'+one_str+') gt 1 then x'+one_str+ '=reform(x' +one_str+ ',/overwrite)')
		iii   =execute('if n_elements(y'+one_str+') gt 1 then y'+one_str+ '=reform(y' +one_str+ ',/overwrite)')
		iii   =execute('sz_one=size(w' +one_str+')')
		iii   =execute('sz_err=size(e' +one_str+')')
		iii   =execute('sz_mon=size(n' +one_str+')')
		iii   =execute('sz_x  =size(x' +one_str+')')
		iii   =execute('sz_y  =size(y' +one_str+')')
		
		if (sz_one(sz_one(0)+2) eq sz_x  (sz_x  (0)+2)) or $
		   (sz_one(1)		eq sz_x  (1))		then xko=0 else xko=1
		if (sz_one(sz_one(0)+2) eq sz_y  (sz_y  (0)+2)) or $
		   (sz_one(0) eq 1) or  $
		  ((sz_one(0) gt 1) and (sz_one(2) eq sz_y(1)))	then yko=0 else yko=1
		if  sz_one(sz_one(0)+2) eq sz_err(sz_err(0)+2)  then eer=0 else eer=1
		if  sz_one(sz_one(0)+2) eq sz_mon(sz_mon(0)+2)  then mon=0 else mon=1

;**		Then reform x,y,z,e,n with splitxyz
;**		---- ------ --------- ---- --------
		if splitxyz(0) eq 'yes' then begin
;**		X..
		   if splitxyz(1) ne '' then if xko eq 1 then begin
		   	svtwo=[0L] & iii=execute( 'svtwo=size(x' +two_str+')' )
			if sz_two(0) ge 1 then begin
			 if (svtwo(0) eq 0) or (svtwo(1) ne sz_two(1)) then $
			 iii=execute( 'x'+two_str+'=indgen(sz_two(1))+1' )
			 if  svtwo(0) gt 1 then $
			 iii=execute( 'x'+one_str+'=reform(x'+two_str+splitxyz(4)+')' ) else $
			 iii=execute( 'x'+one_str+'=   x'+two_str+'('+splitxyz(1)+')' )
			 iii=execute( 'sz_x  =size(x' +one_str+')')
			 if (sz_one(sz_one(0)+2) eq sz_x  (sz_x  (0)+2)) or $
		   	    (sz_one(1)		 eq sz_x  (1))		 then xko=0 else xko=1
		   	endif
		   endif
;**		Y..
		   if splitxyz(2) ne '' then if yko eq 1 then begin
		   	svtwo=[0L] & iii=execute( 'svtwo=size(y' +two_str+')' )
			if sz_two(0) ge 2 then begin
			 if (svtwo(0) eq 0) or $
			   ((svtwo(0) eq 1) and (svtwo(1) ne sz_two(2))) or $
			   ((svtwo(0) eq 2) and (svtwo(2) ne sz_two(2))) then $
			 iii=execute( 'y'+two_str+'= indgen(sz_two(2))+1' )
			 if  svtwo(0) gt 1 then $
			 iii=execute( 'y'+one_str+'=reform(y'+two_str+splitxyz(4)+')' ) else $
			 iii=execute( 'y'+one_str+'=   y'+two_str+'('+splitxyz(2)+')' )
			 iii=execute( 'sz_y  =size(y' +one_str+')')
			 if (sz_one(sz_one(0)+2) eq sz_y  (sz_y  (0)+2)) or $
		   	   ((sz_one(0) gt 1) and (sz_one(2) eq sz_y(1))) then yko=0 else yko=1
			endif
		   endif
;**		Z..		   
		   if splitxyz(3) ne '' then begin
		   	svtwo=[0L] & iii=execute( 'svtwo=size(z' +two_str+')' )
			if sz_two(0) ge 3 then begin
			 if (svtwo(0) eq 0) or (svtwo(1) ne sz_two(3)) then $
			 iii=execute( 'z'+two_str+'= indgen(sz_two(3))+1' )
			 iii=execute( 'z'+one_str+'= z'+two_str+'('+splitxyz(3)+')' )
			endif
		   endif
;**		E..		   
		   if (eer eq 1) and  (sz_err(0) eq sz_two(0)) then begin
		      ei1=max(sz_err(0:sz_err(0)) - sz_two(0:sz_two(0)),min=ei2)
		      if (ei1 eq 0) and (ei2 eq 0) then begin
		   	  iii=execute( 'e'+one_str+'=reform(e'+two_str+splitxyz(4)+')' )
			  iii=execute( 'sz_err=size( e'+one_str+')')
		          if sz_one(sz_one(0)+2) eq sz_err(sz_err(0)+2) then eer=0 else eer=1
		      endif
		   endif
;**		N..
		   if (mon eq 1) and  (sz_mon(0) eq sz_two(0)) then begin
		      ei1=max(sz_mon(0:sz_mon(0)) - sz_two(0:sz_two(0)),min=ei2)
		      if (ei1 eq 0) and (ei2 eq 0) then begin
		   	  iii=execute( 'n'+one_str+'=reform(n'+two_str+splitxyz(4)+')' )
			  iii=execute( 'sz_mon=size( n'+one_str+')')
		          if sz_one(sz_one(0)+2) eq sz_mon(sz_mon(0)+2) then mon=0 else mon=1
		      endif
		   endif
		endif

;**		Now check if TOTAL or slice was used
;**		--- ----- -- ----- -- ----- --- ----
;		---- X Y Z
		if (strpos(strcompress(lwtxt,/remove_all),'total(') gt 0) or (splitxyz(0) eq 'yes') then begin
		 if (sz_one(0) eq 2) and (sz_two(0) eq 3)  then begin
		     if sz_one(1) eq sz_two(1) then begin
		        if sz_one(2) eq sz_two(2) then begin
		        endif else if (sz_one(2) eq sz_two(3)) and (yko eq 1) then begin
		 	   iii=execute('y' +one_str+'= z' +two_str)
		 	   y_tit(one) = z_tit(two)
		        endif
		     endif else    if (sz_one(1) eq sz_two(2)) and (xko eq 1) then begin
		 	   iii=execute('x' +one_str+'= y' +two_str)
		 	   iii=execute('y' +one_str+'= z' +two_str)
		 	   x_tit(one) = y_tit(two)
		 	   y_tit(one) = z_tit(two)
		     endif
		     z_tit(one) ='Count'
		     iii=execute('z' +one_str+'= 0')
		 endif
;		 ---- X Y
		 if   (sz_one(0) eq 1) and (sz_two(0) gt 1) then begin   ;and (xko+yko ge 1)
		  y_tit(one) = 'Count'
		  if  (sz_x(0)   gt 1)         then iii=execute('x' +one_str+'= x' +one_str+'(*,0)')
		  if  (sz_one(1) eq sz_two(2)) then begin
		   if (sz_one(1) ne sz_two(1)) then begin
		 	 iii=execute('x' +one_str+'= y' +two_str)
			 if(sz_y(0)   gt 1)   then iii=execute('x' +one_str+'= reform(x' +one_str+'(0,*))')
		 	 x_tit(one) = y_tit(two)
		 	;if splitxyz(0) ne 'yes' then y_tit(one) = z_tit(two)
		   endif
		  endif else if sz_two(0) gt 2 then if (sz_one(1) eq sz_two(3)) then begin
		 	 iii=execute('x' +one_str+'= z' +two_str)
		 	 x_tit(one) = z_tit(two)       
		 	 y_tit(one) = 'Count'
		  endif
     	      	  z_tit(one)	=''
		  iii=execute('sz_x =size(x' +one_str+')')
     	      	  iii=execute('y' +one_str+'= [0]')
		 endif       
		endif
		if (strpos(strcompress(lwtxt,/remove_all),'total(') gt 0) then begin
;		 ---- E	
		 if (eer eq 1) and (sz_err(0) eq sz_one(0)+1) then begin
		  if sz_err(0) eq 1 then iii=execute('e'+one_str+'=sqrt(total(e'+one_str+'^2  ))') else $
		  if sz_err(0) ge 2 then if sz_one(1) ne  sz_err(1) then $
		 			iii=execute('e'+one_str+'=sqrt(total(e'+one_str+'^2,1))') else $
		 			if sz_one(1) eq  sz_err(1) then $
		 			iii=execute('e'+one_str+'=sqrt(total(e'+one_str+'^2,2))') else $ 
		  if sz_err(0) eq 3 then if sz_one(2) ne  sz_err(3) then $
		 			iii=execute('e'+one_str+'=sqrt(total(e'+one_str+'^2,3))')
		 endif	
;		 ---- N	
		 if (mon eq 1) and (sz_mon(0) eq sz_one(0)+1) then begin
		  if sz_mon(0) eq 1 then iii=execute('n'+one_str+'=total(n'+one_str+'  )') else $
		  if sz_mon(0) ge 2 then if sz_one(1) ne  sz_mon(1) then $
;		 			iii=execute('n'+one_str+'=total(n'+one_str+',1)') else $
					iii=1 else $
		 			if sz_one(1) eq  sz_mon(1) then $
		 			iii=execute('n'+one_str+'=total(n'+one_str+',2)') else $ 
		  if sz_mon(0) eq 3 then if sz_one(2) ne  sz_mon(3) then $
		 			iii=execute('n'+one_str+'=total(n'+one_str+',3)')
		 endif
		endif
		
;**		Now check if TRANSPOSE was used
;**		--- ----- -- --------- --- ----
		tmptr=strcompress(lwtxt,/remove_all)
		if (strpos(tmptr,'=transpose(') gt 0) then begin
		   if sz_x(0)   gt 1 then iii=execute('x'+one_str+ '=reform(transpose(x' +one_str+ '),/overwrite)')
		   if sz_y(0)   gt 1 then iii=execute('y'+one_str+ '=reform(transpose(y' +one_str+ '),/overwrite)')
		   if sz_err(0) gt 1 then iii=execute('e'+one_str+ '=reform(transpose(e' +one_str+ '),/overwrite)')
		   tmp=0.  &  iii=execute('tmp= x' +one_str)
		   iii=execute('x' +one_str+'= y' +one_str)
		   iii=execute('y' +one_str+'= tmp')
		   tmp=x_tit(one)
		   x_tit(one)=y_tit(one) & y_tit(one) = tmp
		endif
	
		if calc_e ne 0 then begin	
		   if tix2  ne ''    then begin tix2=tix3+' evaluated'  & last_form=last_form+';E'+one_str  &    endif
		   if l_message gt 0 then widget_control,bad_id=iii,l_message  ,set_value=tix1+tix2 else print,tix1+tix2
		   if b_labins(6)    then if b_labins(7) gt 0 then $
		                          widget_control,bad_id=iii,b_labins(7),set_value=tix1+tix2
		endif
	 endif

;        Keep last workspace altered in last_w in common block
;        Update history if "wn=" found
         if (one ge 1) and (one le lamp_sys) then begin last_w=one
         					  his_mod,last_form,line_2
         endif else if alone gt 0 then begin
         		     one=alone & two=-1 & last_w=one
         		     his_mod,last_form,''
         		     one=0     & two= 0
         endif else if (one  ge  0) and (ifixed eq 1) then begin
         		     jou_c=[jou_c,last_form]
         		     jou_w=[jou_w,'']
         		     endif
      endelse
      endif
   endelse
ifixed=0
return
mis:		     therror=strmid(!err_string,0,65)
		     if l_message gt 0 then $
		     widget_control,bad_id=iii,l_message  ,set_value=therror $
		     else print,!err_string
		     if b_labins(6) then if b_labins(7) gt 0 then $
		     widget_control,bad_id=iii,b_labins(7),set_value=therror
		     print,string(7b)
		     ifixed=0
return
end

pro don_me_lastf, ici ,flg
;** ************
@dons.cbk
if flg then last_form=ici else ici=last_form
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
pro com_split,formi
;** *********
;**
@lamp.cbk
	form =formi & pe=strpos(form,'&')   & ifi=ifixed
	xfor =strmid (form,0,pe)	    & xicute,xfor  & ifixed=ifi
	xfor =strtrim(strmid(form,pe+1,strlen(form)-pe),2) & xicute,xfor
return
end

pro com_file,file_name
;** ********
;**
; Executes command file
common c_dondo, bas,nbd,fullist

         bstr=''
         on_ioerror, end_f & in=-1
	 
	 if strpos(strlowcase(file_name),'.prox') ge 0 then begin
	 	fifi=file_name
		bid=findfile(fifi,count=n_em)
		if n_em eq 0 then begin
		   if n_elements(fullist) eq 0 then MAC_LIST,n_em,fullist,maclist,THISFILE='*.prox'
		   idx=where(strpos(fullist,fifi) ge 0)
		   if idx(0) ge 0 then fifi=fullist(idx(0))
		endif
	 endif else fifi=file_name
	 
         openr,in,fifi,/get_lun
	 ok=1
	 CATCH,stat & if stat ne 0 then begin print,!err_string & ok=0 & endif
         while (ok) and (not eof(in)) do begin
          	readf,in,bstr
;		assume xicuter is recursive.
	  	xicuter ,bstr
		RDSTAP,1,50,1,res & if res then ok=0
         endwhile
 end_f:  if in gt 0 then free_lun,in else print,!err_string

return
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro history, event
;** *******
;**
@lamp.cbk
;HISTORY BUTTON     

      if event.select eq 1 then begin
	 widget_control,his_info,set_value=histxt
	 ihis=1
      endif
return
end
pro set_history
;** ***********
;**
@lamp.cbk
;SHOW HISTORY  
	DON_LIM_SENS & nh=n_elements(histxt)
	if his_info  gt 0 then widget_control,his_info,set_value=histxt else $
	if l_message le 0 then if nh gt 1 then for i=0,nh-2 do print,histxt(i)
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro limits, event
;** ******
;**
@lamp.cbk
;Info BUTTON     

      if event.select eq 1 then begin
	 widget_control,his_info,set_value=limtxt
	 ihis=0
      endif
return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
pro macro_files,event, uv
;** ***********
;** Display macros
@lamp.cbk
@dons.cbk

prx =uv(2)
intn=uv(3)
curr_macr=''
n_emacs  =0
if prx then mac_list ,n_emacs, THISFILE='*.prox', /SET $
       else mac_list ,n_emacs, /SET

i=xregistered('mac_page')
if i eq 0 then begin
      script=strarr(1000)
      mac_help = widget_base  (title='Lamp Macro Information',/column,resource_name='lamptouch')

      mc_bar1  = widget_base  (mac_help ,/row)
;***      
      mc_bar11 = widget_base  (mc_bar1  ,/column)
		 put_logo     ,widget_base(mc_bar11,/row)
      header   = widget_label (mc_bar11 ,value='Select macro',font=ft_b_bigger)
      header   = widget_label (mc_bar11 ,value='to view     ',font=ft_b_bigger)
      mc_bidon = widget_label (mc_bar1  ,value='     '	     ,font=ft_b_bigger)
      
      mc_bar11 = widget_base  (mc_bar1  ,/column,/frame)
      mc_bar111= widget_base  (mc_bar11 ,/row)
      
      if GEORGE ne 0 then can='New:' else can='Create a new:'
      mc_bidon = widget_label (mc_bar111,value=can          	     ,font=ft_b_bigger)
	compp    ='Write new file'
      if (sys_dep('RUNTIME') or sys_dep('EMBEDDED')) then $
	  crea_but2= widget_button(mc_bar111,value='Batch file'      ,font=ft_b_bigger,uvalue=[-88,216,2,0]) $
      else begin
	  compp    ='Compile new file'
	  crea_but1= widget_button(mc_bar111,value='Macro'  	     ,font=ft_b_bigger,uvalue=[-88,216,1,0])
	  crea_but2= widget_button(mc_bar111,value='Batch'  	     ,font=ft_b_bigger,uvalue=[-88,216,2,0])
	  if GEORGE ne 0 then $
	  crea_but2= widget_button(mc_bar111,value='Dial'  	     ,font=ft_b_bigger,uvalue=[-88,216,3,0])
      endelse
      
      mac_file = widget_text  (mc_bar11 ,value=' ',xsize=18,ysize=1  ,font=ft_propor,/editable,uvalue=[0])

      mc_bar11 = widget_base  (mc_bar1  ,/column)
      done_but = widget_button(mc_bar11 ,value='Exit'		     ,font=ft_b_bigger)
      comp_but = widget_button(mc_bar11 ,value= compp                ,font=ft_b_bigger)
      mac_labl = widget_label (mc_bar11 ,value='                    ',font=ft_b_normal,xsize=200)
;***
      mc_bar2  = widget_base  (mac_help ,/row)
      if lamp_siz lt 900 then nl=30 else nl=35
	macdid   = widget_base  (mc_bar2  ,/column)
      macdd    = widget_button(macdid   ,value= "Internals"             ,font=ft_propor)
      mack     = widget_list  (macdid   ,ysize=n_emacs <nl ,value=macros,font=ft_propor)
      file_text= widget_text  (mc_bar2  ,xsize=80,ysize=30 ,value=script,font=ft_propor,$
      					/scroll,/editable)
      bid=sys_dep      ('DYNLAB',mac_help,1)
      widget_control,mac_help ,group_leader=lamp_b1,/realize & put_logo
      
      widget_control,mack     ,bad_id=i,set_uvalue=[-88,211,0,0,0,0,0,0,0]  
      widget_control,comp_but ,bad_id=i,set_uvalue=[-88,217,0,0,0,0,0,0,0]  
      widget_control,macdd    ,bad_id=i,set_uvalue=[-88,218,file_text,mac_file]  
      widget_control,done_but ,bad_id=i,set_uvalue=[-88,299,0,0,0,0,0,0,0]  

      XMANAGER, 'mac_page' ,mac_help,event_handler='LAMP_EVENT_PARSER',/just_reg
      widget_control,bad_id=i,file_text,set_value=''
      
      if intn then begin txti=""
	 internal,txti & widget_control,bad_id=ii,file_text,set_value=txti
      endif
      
endif else begin
      if not intn then widget_control,bad_id=i,mack     ,set_value=macros
      if not intn then widget_control,bad_id=i,file_text,set_value=''
      widget_control,bad_id=i,mac_help ,map=1
endelse

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
pro mac_list,n_emacs ,mac_ful_out,macros_out, thisfile=thisfile ,set=setmacros
;** ********
;**
@lamp.cbk
@dons.cbk
; Set up macros

	mic_ful=['']   & macris=['']
	cd,current=mee & home=mee  & pmac=''
	lmac=sys_dep      ('NEWSUB',lamp_dir,'lamp_mac')
	
	hhm=sys_dep      ('HOME')
	cd,hhm
	cd,mee,current=home
		stat=0 & catch,stat
		if stat ne 0 then begin lamp_macro='' & pmac='' & submac=[''] & catch,/cancel
		endif else begin
			if lamp_macro ne '' then pmac=lamp_macro else pmac=lmac ;=sys_dep('NEWSUB',!Dir,'lib')
			bid=FINDFILE(pmac,count=nn)
			if nn eq 0 then pmac=lmac
			cd,pmac   & cd,mee,current=pmac
			submac =expand_path('+'+pmac,/array,count=nsub)
			if strmid(pmac  ,strlen(pmac)-1,1)   ne lamp_dvd then pmac  =pmac  +lamp_dvd
			if strmid(submac(0),strlen(submac(0))-1,1) ne lamp_dvd then submac=submac+lamp_dvd
		endelse

		stat=0 & catch,stat
		if stat ne 0 then begin lmac='' & catch,/cancel
		endif else begin
			cd,lmac  & cd,mee,current=lmac
			if strmid(lmac,strlen(lmac)-1,1) ne lamp_dvd then lmac=lmac+lamp_dvd
		endelse
	if strmid(mee  ,strlen(mee)-1,1)   ne lamp_dvd then mee  =mee  +lamp_dvd
	if strmid(home ,strlen(home)-1,1)  ne lamp_dvd then home =home +lamp_dvd
	
      if (sys_dep('RUNTIME') or sys_dep('EMBEDDED')) then xx='x' else xx='*'
;**	Current macros
	if n_elements(thisfile) eq 1 then file_names=mee + thisfile $
	                             else file_names=mee +'*.pro'+xx
	mac_all=findfile(file_names,count=n_files)
	if n_files gt 0 then begin
					   ln     =strpos(strupcase(mac_all(0)),strupcase(mee))
					   if ln ge 0 then ln=ln+strlen(mee)
					   macris =['-- USER --',strmid(mac_all,ln,30)]
					   mic_ful=['', mac_all]
					   n_files=n_files+1
	endif
	n_emacs=n_files
	
;**	Home macros
	if home ne mee then begin
	if n_elements(thisfile) eq 1 then file_names=home + thisfile $
	                             else file_names=home +'*.pro'+xx
	mac_all=findfile(file_names,count=n_files)
	if n_files gt 0 then begin
					   ln     =strpos(strupcase(mac_all(0)),strupcase(home))
					   if ln ge 0 then ln=ln+strlen(home)
		if n_emacs gt 0 then begin macris =[macris ,'','-- HOME --',strmid(mac_all,ln,30)]
					   mic_ful=[mic_ful,'','', mac_all]
					   n_files=n_files+1
		endif 		else begin macris =[           '-- HOME --',strmid(mac_all,ln,30)]
					   mic_ful=[           '', mac_all]
		endelse
		n_files=n_files+1
	endif
	n_emacs=n_emacs+n_files
	endif

;**	Lamp_macro macros
	if pmac ne '' then if pmac ne home then if pmac ne mee then begin
	 lsub = strlen(pmac)
	 okf  = intarr(nsub>1)
	 ok   =1
	 for i=0,nsub-1 do if (strpos(strlowcase(submac(i)),strlowcase(inst_value)) ge 0) or $
	                      (strpos(strlowcase(submac(i)),strlowcase(inst_group)) ge 0) or $
	                      (strpos(submac(i),lamp_dvd,lsub) lt 0) then begin ok=0 & okf(i)=1 & endif
	 if n_elements(thisfile) eq 1 then if strpos(thisfile,'A_') eq 0 then ok=1
	 for i=0,nsub-1 do begin
	  if (ok) or (okf(i)) then begin
	   if n_elements(thisfile) eq 1 then file_names=submac(i) + thisfile $
	                                else file_names=submac(i) +'*.pro'+xx
	   mac_all=findfile(file_names,count=n_files)
	   if n_files gt 0 then begin
					   ln     =strpos(strupcase(mac_all(0)),strupcase(submac(i)))
					   if ln ge 0 then ln=ln+strlen(submac(i))
					   MACP=strupcase(strmid(submac(i),lsub,18)) & if MACP eq "" then MACP="MACROS"
		if n_emacs gt 0 then begin macris =[macris ,'','-- '+MACP+' --',strmid(mac_all,ln,30)]
					   mic_ful=[mic_ful,'','', mac_all]
					   n_files=n_files+1
		endif           else begin macris =[           '-- '+MACP+' --',strmid(mac_all,ln,30)]
					   mic_ful=[           '', mac_all]
		endelse
		n_files=n_files+1
	   endif
	   n_emacs=n_emacs+n_files
	  endif
	 endfor
	endif
	
	if lmac ne pmac then if lmac ne home then if lmac ne mee then begin
	if n_elements(thisfile) eq 1 then file_names=lmac + thisfile $
	                             else file_names=lmac +'*.pro'+xx
	mac_all=findfile(file_names,count=n_files)
	if n_files gt 0 then begin
					   ln     =strpos(strupcase(mac_all(0)),strupcase(lmac))
					   if ln ge 0 then ln=ln+strlen(lmac)
		if n_emacs gt 0 then begin macris =[macris ,'','-- LAMP --',strmid(mac_all,ln,30)]
					   mic_ful=[mic_ful,'','', mac_all]
					   n_files=n_files+1
		endif 		else begin macris =[           '-- LAMP --',strmid(mac_all,ln,30)]
					   mic_ful=[           '', mac_all]
		endelse
		n_files=n_files+1
	endif
	n_emacs=n_emacs+n_files
	endif
	if n_elements(thisfile) eq 1 then begin
		mac_ful_out=[''] & macros_out=['No MacrosList has been found...']
		idx=where(mic_ful ne '')
		if idx(0) ge 0 then begin
			mic_ful=mic_ful(idx) & macris    =macris(idx)
			idx=uniq(macris,sort(macris)) & mic_ful=mic_ful(idx) & macris=macris(idx)
			mac_ful_out=mic_ful  & macros_out=macris
			for i=0,n_elements(idx)-1 do begin
				t=strpos(macros_out(i),'A_')+1 & if t eq 1 then t=t+1
				p=strpos(macros_out(i),'.') & if p le 0 then p=30
				macros_out(i)=strmid(macros_out(i),t,p-t)
			endfor
		endif
	endif
	if keyword_set(setmacros) then macros =macris
	if keyword_set(setmacros) then mac_ful=mic_ful
return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
pro pro_list,event
;** ********
;**
@dons.cbk

      curr_macr=macros (event.index)
      file_name=mac_ful(event.index)
      
      ln=strpos(curr_macr,';')
      if ln gt 0 then curr_macr=strmid(curr_macr,0,ln)

      if file_name ne '' then begin
        on_ioerror, no_f
	in=-1 & k=0
        openr,in,file_name,/get_lun

        on_ioerror, end_f
        script= strarr(1000)
	bstr  = ''
        for k=long(0),999 do begin
            readf,in,bstr
            script(k)=bstr
        endfor
        on_ioerror, end_m
        while (1) do begin
            mors  = strarr(1000)
            readf , in,mors
            script=[script,mors] & k=k+1000
        endwhile
 end_m:     script=[script,mors] & k=k+1000
 	while script(k-1) eq '' do k=k-1
 end_f:
 no_f:  if in gt 0 then free_lun,in
	if in gt 0 then widget_control,bad_id=i,mac_file ,set_value=curr_macr
        if k  gt 0 then widget_control,bad_id=i,file_text,set_value=script(0:k-1)
	if k  gt 0 then widget_control,bad_id=i,mac_labl ,set_value=' ' $
		   else widget_control,bad_id=i,mac_labl ,set_value='Read error ...!'
      endif
return
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro pro_create ,uv
;** **********
;**
@lamp.cbk
@dons.cbk
	if xregistered('MacName') gt 0 then begin widget_control,macro_area_a,get_uvalue= bazar
	                                          widget_control,bazar,bad_id=ii,/destroy & endif	
	if uv(2) eq 1   then ctxt="macro"
	if uv(2) eq 2   then ctxt="batch"
	if uv(2) eq 3   then ctxt="dial"
	bazar =  widget_base  (title='Lamp Macro Name',/column,resource_name='lamptouch')
	put_logo,widget_base  (bazar,/row)
	bazr1 =  widget_base  (bazar,/row)
	bid   =  widget_label (bazr1,value='Enter the name of the '+ctxt,font=ft_b_normal)
	baz1  =  widget_text  (bazr1,value='my'+ctxt,xsize=15, /editable,font=ft_propor)
	if uv(2) eq 1   then begin
	 bazr2=  widget_base  (bazar,/row)
	 bazrb=  widget_button(bazr2,value='Create a function'          ,font=ft_b_normal,uvalue=[-88,219,0,baz1,bazar])
	 bazrb=  widget_button(bazr2,value='Create a procedure'         ,font=ft_b_normal,uvalue=[-88,219,1,baz1,bazar])
	endif
	if uv(2) eq 2   then begin
	 bazr2=  widget_base  (bazar,/row)
	 bazrb=  widget_button(bazr2,value='Create'                     ,font=ft_b_normal,uvalue=[-88,219,2,baz1,bazar])
	endif
	if uv(2) eq 3  then begin
	 bazr2=  widget_base  (bazar,/row)
	 bazrb=  widget_button(bazr2,value='Create a classical Dial'    ,font=ft_b_normal,uvalue=[-88,219,3,baz1,bazar])
	 bazrb=  widget_button(bazr2,value='Create a Dial for Pad'      ,font=ft_b_normal,uvalue=[-88,219,4,baz1,bazar])
	endif

	widget_control,bazar ,group_leader=lamp_b1,/realize & put_logo
	widget_control,macro_area_a,set_uvalue=bazar
	XMANAGER, 'MacName'  ,bazar,event_handler='LAMP_EVENT_PARSER',/just_reg
return
end

pro pro_creater ,uv
;** ***********
;**
@dons.cbk
	widget_control,uv(3),get_value=curr_macr & curr_macr=strcompress(strlowcase(curr_macr(0)),/remove_all)
	idx=strpos(curr_macr,'.')
	if idx ge 0 then curr_macr=strmid(curr_macr,0,idx)
	idx=strpos(curr_macr,'dial_')
	if idx ge 0 then curr_macr=strmid(curr_macr,idx+5,30)
	MacName=curr_macr
	if uv(2) le 1 then curr_macr=MacName+'.pro'
	if uv(2) eq 2 then curr_macr=MacName+'.prox'
	if uv(2) eq 3 then curr_macr='dial_'+MacName+'.pro'
	if uv(2) eq 4 then curr_macr='dial_'+MacName+'_send.pro'

	widget_control,bad_id=i,mac_file ,set_value= curr_macr ,set_uvalue=uv(2)
	widget_control,bad_id=i,file_text,set_value='',/input_focus
	widget_control,bad_id=i,mac_labl ,set_value=' '

	if uv(2) eq 0 then mactxt=["FUNCTION "+MacName+" ; , p1 , p2 ,p3 ...",";********",";**" $
	                          ,";** The call is w6="+MacName+"(...)","","Wout=0","return, Wout","end"]

	if uv(2) eq 1 then mactxt=["PRO "+MacName+" ; , p1 , p2 ,p3 ...",";***",";**" $
	                          ,";** The call is "+MacName+",...","","print,!stime","end"]

	if uv(2) eq 2 then mactxt=[";Enter lines of IDL commands below. THE CALL will be @"+MacName $
	                          ,";Use one line loops (ie FOR i=0,n DO BEGIN ... & ... & ENDFOR" $
	                          ,";Use variables a-z only (and w1,x1,y1,z1,n1,e1,p1,pv1,w_tit(1),x_tit(1)...)"]

	if uv(2) eq 3 then begin
			   mactxt=[";*********************","PRO dial_"+MacName+"_macro, D" $
	                          ,";*********************",";**" $
	                          ,";** Input D is the dial structure as defined by the function dial_"+MacName $
	                          ,";** This macro procedure is called by George every D.frequency seconds","","" $
	                          ,"    IF D.init eq 0 THEN BEGIN D.init=1 & ENDIF   ;Do your cooking","" $
	                          ,"    V=DialNewValue(TYPE='status',/setvalue)      ;Get status from Mad" $
				  ,"    IF V eq 'Burning' then C=DialControl('stop') ;Stop Mad command" $
				  ,"    IF V eq 'StandBy' then D.frequency=3         ;Change frequency","end","","","" $
	                          ,";*********************","FUNCTION dial_"+MacName $
				  ,";*********************",";**",";** The dial constructor" $
				  ,"","    return,{FREQUENCY:1.5}","end","","","" $
				  ,";****************************************** THAT's ALL ****************","","","" $
				  ,"","   ;Dial Variables (Defaulted if not present in return statement)","   ;--------------" $
				  ,"   ;GENERIC='mad'    ;connect to the mad-idl interface" $
				  ,"   ;TYPE='monitor'   ;then V=DialNewValue() stands for V=DialNewValue(TYPE='monitor')" $
				  ,"   ;ONOFF=0          ;state of the Dial 1=running" $
				  ,"   ;FREQUENCY=1.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used" $
				  ,"   ;VALUE=fltarr(64) ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var." $
				  ,"   ;PLOT=0           ;-2=none 0=plot 1=surface 2=contour n>2 means show vector of last n scalar values" $
				  ,"   ;INIT=0           ;may be used in "+MacName+"_macro when started or on reset" $
				  ,"   ;UPPERLIM=0.      ;upper limit of the plot (LOWERLIM for lower limit)" $
				  ,"   ;HISTORY=0        ;=1 to record values in file "+MacName+".his" $
				  ,"   ;DURATION=0       ;if >0 then Dial is stopped after running duration seconds" $
				  ,"   ;WUPDATE=0        ;=1 to automaticaly update corresponding workspace, =-1 silent!" $
				  ,"                     ;=2 to automaticaly update and plot workspace to the main window" $
				  ,"                     ;   0,1,2 are set by pressing the left,middle,right mouse button on the dial snapshot" $
				  ,"","   ;User Variables (Must be present in return statement to be available)","   ;-------------" $
				  ,"   ;XVALUE=fltarr(64);Abscissa of VALUE  (ordinates go in YVALUE)" $
				  ,"   ;X_TIT='I am X'   ;X axis title       (Y axis title go in Y_TIT)" $
				  ,"   ;...              ;etc" $
				  ,"","   ;return, {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,init:INIT}",""]
		mactxt=[mactxt $
		,";***************************************************************************************" $
		,";** Usefull calls to be used in procedure dial_"+MacName+"_macro :" $
		,";** *************" $
		,";** V=DialNewValue([/SETVALUE],[COMMENT=txt]   ;Get a new value from DIAL_'generic'_READ" $
		,";**                [TYPE='monitor'])           (a request is made to the instrument)" $
		,";**                                            (/SETVALUE means D.value is set to V)" $
		,";** C=DialControl ('command syntax',[CHECK=.5]);Send a command to the instrument control" $
		,";**                                            (CHECK means check every .5 sec till the" $
		,";**                                             command  is complete)" $
		,";** DialTag   ,    'temp2',TAG='VALUE',GET=V   ;Return V,the value for the tag  'VALUE'" $
		,";**                                                                of  the dial 'temp2'" $
		,";** DialTag   ,    'temp3',TAG='ONOFF',SET=1   ;Set to 1 the value of  the tag  'ONOFF'" $
		,";** DialStart ,    'temp3'                     ;A short  for previous call" $
		,";** DialStop  ,    'temp3'                     ;A short  too" $
		,";**" $
		,";** DialModValue,   V ,[tag='VALUE']           ;Set the new value for current dial or" $
		,";** D.value   =     V                          ;modify yourself the tag Value if type &" $
		,";**                                            ;dimensions don't change.(same for Error)" $
		,";** D.upperlim=   150.                         ;Set upper limit for plotting." $
		,";**" $
		,";** R=DialOn ()                                ;Return 0 if Dial has been interrupted" $
		,";**                                            (To use inside loops)" $
		,";** DialInit,      'template4',[NEW='tmp4']    ;Initiate dial  'template4' from file:" $
		,";**                           ,[PATH=path ]                dial_template4.pro" $
		,";**                                            (You may change its name to 'tmp4' and" $
		,";**                                            (use DialStart,'tmp4' to activate it)" $
		,";** DialMacro,     'template4'                 ;Force execution of DIAL_TEMPLATE4_MACRO" $
		,";**                                            ('template4'  is keept inactive, ONOFF=0)" $
		,";** DialClear,     'template4'                 ;Suppress dial  'template4' from memory" $
		,";** DialWSet                                   ;Reserve central draw window for next plot" $
		,";**" $
		,";** DialsFrequency,[GET=freq],[SET=.5],[/STOP] ;Set  or Get the general frequency value" $
		,";**                [DURATION=90.] ,   [/START] ;              (time is in seconds)" $
		,";**                                            ;Stop or Start the general process" $
		,";**                                            ;Set  Time  limit for the active process"]
		endif
	if uv(2) eq 4 then mactxt=["FUNCTION dial_"+MacName+"_send, dummy1,dummy2, text, button",";*******",";**" $
	                          ,";** Called from the Pad, generaly to start and stop Dials","" $
                                  ,"Dials=strlowcase(strtrim(str_sep(text(0),'~'),2))","" $
				  ,"FOR i=0,n_elements(Dials)-1 DO BEGIN" $
                                  ,"    DialInit ,Dials(i)" $
	                          ,"    DialTag  ,Dials(i),tag='FREQUENCY',set=1.0" $
	                          ,"    DialStart,Dials(i)" $
				  ,"ENDFOR" $
	                          ,"","return,0","end","","","","","","" $
		,";***************************************************************************************","" $
		,";** Usefull calls to be used in procedure dial_"+MacName+"_send :" $
		,";** *************" $
		,";** DialTag   ,    'temp2',TAG='VALUE',GET=V   ;Return V,the value for the tag  'VALUE'" $
		,";**                                                                of  the dial 'temp2'" $
		,";** DialTag   ,    'temp3',TAG='ONOFF',SET=1   ;Set to 1 the value of  the tag  'ONOFF'" $
		,";** DialStart ,    'temp3'                     ;A short  for previous call" $
		,";** DialStop  ,    'temp3'                     ;A short  too" $
		,";**" $
		,";** DialInit,      'template4',[NEW='tmp4']    ;Initiate dial  'template4' from file:" $
		,";**                                                               dial_template4.pro" $
		,";**                                            (You may change its name to 'tmp4' and" $
		,";**                                            (use DialStart,'tmp4' to activate it)" $
		,";** DialMacro,     'template4'                 ;Force execution of DIAL_TEMPLATE4_MACRO" $
		,";**                                            ('template4'  is keept inactive, ONOFF=0)" $
		,";** DialClear,     'template4'                 ;Suppress dial  'template4' from memory" $
		,";** DialWSet                                   ;Reserve central draw window for next plot" $
		,";**" $
		,";** DialsFrequency,[GET=freq],[SET=.5],[/STOP] ;Set  or Get the general frequency value" $
		,";**                [DURATION=90.] ,   [/START] ;              (time is in seconds)" $
		,";**                                            ;Stop or Start the general process" $
		,";**                                            ;Set  Time  limit for the active process"]

widget_control,bad_id=ii,uv(4),/destroy
widget_control,bad_id=ii,file_text,set_value=mactxt
return
end

pro pro_compile
;** ***********
;**
@dons.cbk

widget_control,bad_id=i,mac_file ,get_value=curr_macr ,get_uvalue=typ
curr_macr=strlowcase(strtrim(curr_macr(0),2))

if curr_macr ne '' then begin
	widget_control,bad_id=i,file_text,get_value=new_macro
	
	bat=strmid (curr_macr,strpos(curr_macr,'.'),5)
	ran=long((systime(1)-(long(systime(1)/10000))*double(10000))*100)
	ran=strtrim(string(randomu(ran)),2) & ran=strmid(ran,strpos(ran,'.')+1,4)
	poc='n'+ran+curr_macr
	pac=strmid (poc,0,strpos(poc,'.'))

	!Error=0
	
	ON_IOERROR,mis_open & out2=-1
	OPENW ,out2,        curr_macr,/get_lun
	ON_IOERROR,mis_io
	for i=0,n_elements(new_macro)-1 do PRINTF,out2,new_macro(i)
	FREE_LUN,out2 & out2=-1
	
	DON_COMP, new_macro, bat

	P_MUS,'mus_shot'
	mac_list ,n_emacs, /set
	widget_control,bad_id=i,mack     ,set_value=macros
	
mis_io: if out2 gt 0 then free_lun,out2
	
mis_open:if !Error ne 0 then widget_control,bad_id=i,mac_labl ,set_value=!err_string  $
			else widget_control,bad_id=i,mac_labl ,set_value=curr_macr+' Created'

endif
return
end

pro don_comp, new_macro, bat
;** ********
;**
	resol =0
	if (not sys_dep("EMBEDDED")) and (not sys_dep("RUNTIME")) then resol=1
	if (resol eq 1)  and (sys_dep ('VERSION') ge 4.0)         then resol=2
	poc='lamp_tmp.pro'
	pac='lamp_tmp'
	if resol gt 0 then begin out1=-1
	   ON_IOERROR,mis_cmp
	   OPENW ,out1,poc,/get_lun
	   if bat ne '.pro' then PRINTF,out1,'pro '+pac
	   for i=0,n_elements(new_macro)-1 do PRINTF,out1,new_macro(i)
	   PRINTF,out1,''
	   if bat ne '.pro' then PRINTF,out1,'end' $
	   else begin	PRINTF,out1,'pro '+pac & PRINTF,out1,'end'
			endelse
	   PRINTF  ,out1,''
 	   FREE_LUN,out1 & out1=-1

	  if resol eq 1 then iii=EXECUTE   ( pac )
	  if resol eq 2 then RESOLVE_ROUTINE,pac

	  mis_cmp:if out1 gt 0 then free_lun,out2
	 ;OPENR ,out1,poc,/get_lun,/DELETE & FREE_LUN,out1 & out1=-1
	endif
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro par_disp, event
;** ********
;**compile
@lamp.cbk
@dons.cbk
;USERPAR BUTTON     
;
; Construct the text
      if nwk le 0 then nwk=1

      par_txt_all(*)=''
      npa=0L
      bb=execute('npa=n_elements(p'+strtrim(string (nwk),2)+')' )
      i =0
      bb=execute('for i=0,npa-1 do par_txt_all(i)=strtrim(par_txt(nwk,i))+string(p' $
      						   +strtrim(string (nwk),2) + '(i))')

      up_t    =widget_base  (title='LAMP Instrument Parameters',/column,resource_name='lampdon')
      bar1    =widget_base  (up_t,/row)
	       put_logo	    ,widget_base(bar1,/column)
      donebut =widget_button(bar1,value='Write')
      abortbut=widget_button(bar1,value='Quit')
      up_slid =widget_slider(bar1,value=nwk,title='Workspace #',maximum=lamp_sys,$
								minimum=1,xsize=200)
      up_labl =widget_label (bar1,value='Numor #',xsize=8*10)
      mc_bidon=widget_label (bar1,value=' '	 ,xsize=4)
      up_win  =widget_text  (up_t,xsize=51,ysize=31,/scroll,/editable,font=ft_propor)
      bid=sys_dep      ('DYNLAB',up_t,0)
      widget_control,up_t,group_leader=lamp_b1,/realize & put_logo
      widget_control,up_win   ,bad_id=i,set_value =  par_txt_all
      widget_control,up_slid  ,bad_id=i,set_uvalue=[-88,206,up_win,up_slid,up_labl,0,0,0,0]
      widget_control,donebut  ,bad_id=i,set_uvalue=[-88,205,up_win,up_slid,up_labl,0,0,0,0]
      widget_control,abortbut ,bad_id=i,set_uvalue=[-88,299,0,0,0,0,0,0,0]

      XMANAGER, 'ups' ,up_t,event_handler='LAMP_EVENT_PARSER',/just_reg
      return
      end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro par_mod,event,up_win,up_slid
;** *******
;**
@lamp.cbk
@dons.cbk
; DONE button in USERPAR display

      i=0
      widget_control,bad_id=i,up_slid,get_value=nwk
      if i eq 0 then begin
      	widget_control,bad_id=i,up_win, get_value=par_txt_all
      	n   =n_elements(par_txt_all)
      	junk=fltarr(n)
      	for j=0,n-1 do begin
          sht_txt=strtrim (par_txt_all(j))
          lnth=strlen(sht_txt)
;
;         Pick out number after '='

          npos   =strpos(sht_txt,'=')
          par_len=lnth-npos
          par_val=strmid(sht_txt,npos+1,par_len)
          junk(j)=float(par_val)
      	endfor

      	bb=execute('p' + strtrim(string(nwk),2) + '=junk' ) 

;	Destroy the evidence
      	wait,.3 & widget_control,event.top,/destroy
      endif
return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro nwk_mod,event,up_win,up_slid,up_labl
;** *******
;**
@lamp.cbk
@dons.cbk
;
;Changes current workspace
    widget_control,bad_id=i,up_slid,get_value=nwk
    widget_control,bad_id=i,up_labl,set_value=w_numor(nwk)

      par_txt_all(*)=''
      npa=0L
      bb=execute('npa=n_elements(p'+strtrim(string (nwk),2)+')' )
      i =0
      bb=execute('for i=0,npa-1 do par_txt_all(i)=strtrim(par_txt(nwk,i))+string(p' $
      						   +strtrim(string (nwk),2) + '(i))')
;
;   Update window text

    widget_control,bad_id=i,up_win,set_value=par_txt_all
return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function str_focc, s,c1,c2,c,f
;******* ********
;**
l=strlen(s) & occ=0
for i=f,l-1 do begin ch=strmid(s,i,1)
    if ch eq c  then if occ le 0 then return,i
    if ch eq c1 then occ=occ+1 else if ch eq c2 then occ=occ-1
endfor
return,-1
end

pro find_w1_w2, line,line_2,one,two,three ,alone ,splitxyz ,opp_r
;** **********
;**
common eme456,	fourth,fifth,sixth

;find first two workspaces on line
;must be an "="
;must be one "w" each side of "="

splitxyz=['no']
line_2  =''
opp_r   =''
one     =0
two     =0
three   =0 & fourth=0 & fifth=0 & sixth=0
alone   =0
ok3	=0

;find first "w"
;--------------
lnth  =strlen(line)
pos_w=-1
ch1  ='a'
while ((ch1 lt '0') or (ch1 gt '9')) do begin
	pos_w=strpos(strlowcase(line),'w',pos_w+1)
	if pos_w lt 0 then RETURN
	ch1=strmid(line,pos_w+1,1)
endwhile

;find  ","
;--------
pos_v=strpos(line,',')

;find  "="
;--------
pos_e=strpos(line,'=')
if pos_v ge 0 then if pos_v lt pos_w then pos_e=-1
		   if pos_e lt pos_w then pos_e=-1

;find a lone "w"
;---- ---------
if pos_e le 0 then begin
	ch1=strmid(line,pos_w+1,1)
	if (ch1 ge '0') and (ch1 le '9') then begin
		ch2=strmid(line,pos_w+2,1)
		if (ch2 lt '0') or (ch2 gt '9') then alone=fix(ch1)   $
			   			else alone=fix(ch1+ch2)
	endif
	RETURN
endif

; is there a second "w" before the "="?
;--------------------------------------
line_t=strmid(line,pos_w+1,pos_e-(pos_w))
pos_t =strpos(strlowcase(line_t),'w')
if pos_t gt 0 then RETURN

;what follows first "w";Is it a number?
;--------------------------------------
ch1=strmid(line,pos_w+1,1)
if (ch1 lt '0') or (ch1 gt '9') then RETURN

;next;Try a case       (care wi(m,n)=x --> assume two=one to keep history)
;---------------
ch2=strmid(line,pos_w+2,1)
case 1 of
  (ch2 eq ' '): one=fix(ch1)
  (ch2 eq '='): one=fix(ch1)
  (ch2 eq '('): begin one=fix(ch1) & two=one & end
  (ch2 eq '['): begin one=fix(ch1) & two=one & end
  (ch2 lt '0')  or (ch2 gt '9'): RETURN
  else:         begin one=fix(ch1+ch2) 
		      ch3=strmid(line,pos_w+3,1)
		      if (ch3 eq '(') or (ch3 eq '[') then two=one & end
endcase

; is there a "w" after the "="
;-----------------------------
;This is repeated until a "w" if followed by an number or eol
iquit=0 & pos_e1=0
while (iquit eq 0) do begin
	line_2=strmid(line,pos_e,lnth-pos_e)
;	find second or third "w"
	pos_w =strpos(strlowcase(line_2),'w')
	if pos_w lt 0 then begin
		iquit=1
		RETURN
		endif

;	what follows second or third "w"
	ch1=strmid(line_2,pos_w+1,1)
;	Is it a number?
	if (ch1 ge '0') and (ch1 le '9') then begin

		ch2=strmid(line_2,pos_w+2,1)
		po3=pos_w+3
		case 1 of
	  	(ch2 eq ''):  if two    eq 0 then two   =fix(ch1) else $
		              if three  eq 0 then three =fix(ch1) else $
		              if fourth eq 0 then fourth=fix(ch1) else $
		              if fifth  eq 0 then fifth =fix(ch1) else $
			      if sixth  eq 0 then sixth =fix(ch1)
	  	(ch2 eq ' '): if two    eq 0 then two   =fix(ch1) else $
		              if three  eq 0 then three =fix(ch1) else $
		              if fourth eq 0 then fourth=fix(ch1) else $
		              if fifth  eq 0 then fifth =fix(ch1) else $
			      if sixth  eq 0 then sixth =fix(ch1)
		(ch2 eq '(') or (ch2 eq '['): begin po3=po3-1
			      if two    eq 0 then two   =fix(ch1) else $
		              if three  eq 0 then three =fix(ch1) else $
		              if fourth eq 0 then fourth=fix(ch1) else $
		              if fifth  eq 0 then fifth =fix(ch1) else $
			      if sixth  eq 0 then sixth =fix(ch1)
			      end
	  	else: 	      if (ch2 ge '0') and (ch2 le '9')  then begin
			      if two    eq 0 then two   =fix(ch1+ch2) else $
		              if three  eq 0 then three =fix(ch1+ch2) else $
		              if fourth eq 0 then fourth=fix(ch1+ch2) else $
		              if fifth  eq 0 then fifth =fix(ch1+ch2) else $
			      if sixth  eq 0 then sixth =fix(ch1+ch2)
			      endif else begin
			      if two    eq 0 then two   =fix(ch1) else $
		              if three  eq 0 then three =fix(ch1) else $
		              if fourth eq 0 then fourth=fix(ch1) else $
		              if fifth  eq 0 then fifth =fix(ch1) else $
			      if sixth  eq 0 then sixth =fix(ch1)
			      endelse
		endcase

;	Check for splitxyz
		pe=po3
		lp=pos_w+1
		chbrk=strmid(line_2,po3,1) & if chbrk eq '(' then chprbk=')'
		                             if chbrk eq '[' then chprbk=']'
		if (chbrk eq '(') or (chbrk eq '[') then $
		 if three gt 0 then pe=(str_focc(line_2,chbrk,chprbk,chprbk,po3+1)+1)>po3 else $
		 if ok3   eq 0 then begin
		   lp =str_focc(line_2,chbrk,chprbk,chprbk,po3+1)
		   
		   if (lp gt po3+1) then splitxyz=['yes','','','',strmid(line_2,po3,lp-po3+1)]
		    		   
		   fv =str_focc(line_2,'(',')',',',po3+1)
		    
;**		     SPLIT X
;**		     -------	    
		     if (fv lt 0) or (fv gt lp)   then ib=lp else  ib=fv
		     splitxyz(1)=strmid(line_2,po3+1,ib-po3-1)
		    
		     if fv gt 0 then begin
		        sv =str_focc(line_2,'(',')',',',fv+1)
		      
;**		        SPLIT Y
;**		        -------		    
		     	if (sv lt 0) or (sv gt lp) then ib=lp else  ib=sv
		     	splitxyz(2)=strmid(line_2,fv+1,ib-fv-1)
		      
		        if sv gt 0 then begin
		      	   tv =str_focc(line_2,'(',')',',',sv+1)
		      
;**		      	   SPLIT Z
;**		    	   -------		    
		     	   if (tv lt 0) or (tv gt lp) then ib=lp else  ib=tv
		     	   splitxyz(3)=strmid(line_2,sv+1,ib-sv-1)
		       endif
		    endif
;if splitxyz(0) eq 'yes' then print,splitxyz(1),'_',splitxyz(2),'_',splitxyz(3),'_',splitxyz(4)
		 endif
; is there a third "w" ?
;-----------------------
		if ok3 lt 0  then begin   if three gt 0 then begin
					    pe_l  =strcompress(strmid(line_2,pe,10),/remove_all)
					    if pe_l eq '' then begin
					       line_2=strmid(line,pos_e1,pos_e+pos_w-pos_e1+1)
					       if strpos(line_2,'+') ge 0 then opp_r=opp_r+'+'
					       if strpos(line_2,'-') ge 0 then opp_r=opp_r+'-'
					       if strpos(line_2,'/') ge 0 then opp_r=opp_r+'/'
					       if strpos(line_2,'*') ge 0 then opp_r=opp_r+'*'
					       if strpos(line_2,'#') ge 0 then opp_r=opp_r+'#'
					    endif
					    ok3=1
					  endif
		endif
		if ok3 eq 0 then ok3 =-1
		pos_e1=pos_e+lp
	endif
	pos_e=pos_e+pos_w+1
	if pos_e ge lnth then RETURN
endwhile
RETURN
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro to_don_history, did_one , did_two , line ,nojournal=nojournal
;** **************
;**
@lamp.cbk
@dons.cbk
	kp_one=one & kp_two=two
	one=did_one
	two=did_two
	if (not keyword_set(nojournal)) then ifixed=1 else ifixed=0
 	his_mod,  line , ''
	ifixed=0
	if did_one lt 0 then begin one=kp_one & two=kp_two & endif
 return
end

pro his_mod,line,line_2
;** *******
;**
;Modify history of one
@lamp.cbk
@dons.cbk

IF one ge 0 THEN BEGIN
dim=[0L,0L]
wkspce='w'+strtrim(string(one),2)
wkspac='w'+        string(one,format='(i2)')
i=execute('dim=size('+wkspce+')')
tipe=dim(dim(0)+1)
case tipe of 		;+newtypes*****
   1: lims(one)='Byte   '
   2: lims(one)='Integer'
   3: lims(one)='Long   '
   4: lims(one)='Float  '
   5: lims(one)='Double '
   6: lims(one)='Complex'
   7: lims(one)='String '
   8: lims(one)='Struct '
   9: lims(one)='Dcomplx'
   10:lims(one)='Pointer'
   11:lims(one)='Object '
   12:lims(one)='Uintegr'
   13:lims(one)='Ulong  '
   14:lims(one)='Int64  '
   15:lims(one)='Uint64 '
else: lims(one)='Undef  '
endcase
lims(one)=wkspac+': '+lims(one)

if dim(0) ge 1 then lims(one)=lims(one)+' dim = '   + strtrim(string(dim(1)),2)
if dim(0) ge 2 then lims(one)=lims(one)+' * '       + strtrim(string(dim(2)),2)
if dim(0) ge 3 then lims(one)=lims(one)+' * '       + strtrim(string(dim(3)),2)
if dim(0) ge 4 then lims(one)=lims(one)+' * '       + strtrim(string(dim(4)),2)

miny=0.
maxy=0.	;+newtypes*****
if ((tipe gt 0) and (tipe lt 7)) or (tipe eq 9) or ((tipe ge 12) and (tipe le 15)) then begin
  if dim(0) gt 0 then begin
    i=execute('maxy=max('+wkspce+',min=miny)')
    if sys_dep('MATLAB') then $
    i=execute('miny=min('+wkspce+')')
    if tipe eq 1 then begin miny=fix(miny) & maxy=fix(maxy) & endif

    lims(one)=lims(one)+' min='+strtrim(string(miny),2)+$
			' max='+strtrim(string(maxy),2)
  endif else begin
    i=execute('miny='+wkspce)
    if tipe eq 1 then miny=fix(miny)

    if miny ne 0 then lims(one)=lims(one)+' Scalar='+  strtrim(string(miny),2) $
		 else lims(one)=' '
  endelse
endif

w_min(one)=miny
w_max(one)=maxy

DON_LIM_SENS

if ihis eq 0 then begin
		  n=n_elements(limtxt)
		  i=0 & chk='w'+string(last_w,format='(i2)')
		  if n gt 1 then $
			for j=1,n-1 do if strmid(limtxt(j),0,3) eq chk then i=j
		  if his_info gt 0 then $
		  widget_control,bad_id=ii,his_info ,set_value=limtxt,   $
						     set_text_top_line=i $
		  else if l_message gt 0 then $
		  widget_control,bad_id=ii,l_message,set_value=lims(one) $
		  else print,lims(one)
		  endif
if two  lt 0 then begin
; update info only
endif else begin
;
; search for another wkspce after the =
   pos2=strpos(line_2,wkspce)
;
; when wkspce appears again append history
   if pos2 ge 0 then begin
      his(one)=line+' ... '+his(one)

; when wkspce appears again but not the same
   endif else if two gt 0 then begin
      his(one)=line+' ... '+his(two)

; when no second workspace just use line as history
   endif else begin
	his(one)=line
      if ((tipe  gt 0) and (tipe lt 7)) or (tipe eq 9) or ((tipe ge 12) and (tipe le 15)) then $
	if (dim(0) eq 0) and (miny eq 0) then his(one)=' '
   endelse
endelse

wtb(one)=0
if (one gt 0) and (one le lamp_sys) then begin i=execute('Sna'+strtrim(string(one),2)+'=0')
                                    if abs(sys_dep('MAP')) ne 1 then $
				      if ifixed ne 0 then to_did_cur , wkspce
endif
histxt=his(where(his ne ' '))
if ihis eq 1 then if his_info gt 0 then $
		  widget_control,bad_id=i,his_info ,set_value=histxt   $
		  else if l_message gt 0 then $
		  widget_control,bad_id=i,l_message,set_value=his(one) $
		  else print,his(one)
ENDIF

if ifixed eq 1 then begin
		  DID_WRITE_JOURNAL,/check & j=n_elements(jou_c)-1
		  if one lt  0 then begin one=0
		   i=strpos(strlowcase(line),'see')
		   if i ge 0 then i=strpos(strlowcase(jou_c(j)),'see') $
		             else i=strpos(strlowcase(line)    ,'passw')
		   if i lt 0 then begin jou_c=[jou_c,line]
					j=n_elements(line)  & lines=strarr(j)
					jou_w=[jou_w,lines]
		   endif else begin lines='' & line='' & endelse
		   
		  endif else begin
		   if b_labins(6) then if b_labins(7) gt 0 then if one gt 0 then begin
			     widget_control,bad_id=iii,b_labins(7),set_value=lims(one)
		   endif
                   if (one gt 0) and (two gt 0) then line=his(one)
		   jou_c=[jou_c,line]  & lines=';'+lims(one)
		   jou_w=[jou_w,lines] & endelse
		   
    i=xregistered('JOURNAL')
    if line ne ''  then  $
    if i gt 0 then begin widget_control,bad_id=i,lamp_don(0),get_uvalue=basc
    			 linn=n_elements(jou_c) & lnew=line+'            '+lines
			 if linn lt 10 then begin
			 	widget_control,bad_id=i,basc,set_value=jou_c+'            '+jou_w
			 endif else $
			 	widget_control,bad_id=i,basc,set_value=lnew,$
                                /append,SET_TEXT_TOP_LINE=(n_elements(jou_c)-18)>0
    endif
endif
end

pro DON_LIM_SENS
;** ************
;**
@lamp.cbk

limtxt=lims(where(lims ne ' '))

if n_elements(lamp_don) gt 2 then begin
 nl=n_elements(limtxt) & j=-1
 if (nl gt 1) and (lamp_don(1) eq 1) then begin lamp_don(1)=0 & j=1 & endif
 if (nl le 1) and (lamp_don(1) eq 0) and (n_elements(w1) lt 2) then begin lamp_don(1)=1 & j=0 & endif

 if GEORGE ne 1 then $
 if  j ge 0 then begin  for i=2,n_elements(lamp_don)-1 do $
		     widget_control,bad_id=ii,lamp_don(i),sensitive=j
 endif
endif
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
pro fire_prog_mac,event,num1,num2, prox
;** *************
;**
@dons.cbk
;
; Put programmed key macro command into formula window for last workspace

comy=[' ']
if prox eq 0 then begin
	widget_control,num2  ,bad_id=ii,get_value=comy & comy=comy(0)
	jj=strpos(strlowcase(comy),'passw')
	if jj ge 0 then widget_control,num2  ,bad_id=ii,set_value=""
	if jj lt 0 then if num1 ge 0 then prog_mac(num1)=comy
	xfor=strtrim(comy,2)  & xicuter,xfor
endif else begin
      widget_control,prox    ,bad_id=ii,get_uvalue=tot
;;    lab='Play all'
;;    widget_control,event.id,bad_id=ii, set_value='Stop!'
      nl=n_elements(tot)  & k=0
      CATCH,stat & if stat ne 0 then begin print,!err_string & k=nl & endif
      while (k lt nl) do begin
	widget_control,tot(k),bad_id=ii,get_value=comy
	k=k+1
;;	evv=widget_event(evv.top,/nowait) & widget_control,/hourglass
;;	if  evv.id ne 0 then k=nl else $          ; if RDSTOP(1,nl,(k)) then k=nl
	xfor=strtrim(comy(0),2) & xicuter,xfor
	RDSTAP,1,nl,(k),res & if res then k=nl
      endwhile
;;    widget_control,event.id,bad_id=ii, set_value=lab
endelse
return
end
;

pro don_write_prog_mac ,flg
;** ******************
;**
;** Write user command list and begood options
@dons.cbk
@lamp.cbk

 common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
		wbeside,vfl,styles,w4d,smoo,vff
    
	 n=n_elements(prog_mac)

	 if flg eq 0 then begin
	    for k=0,n-1 do begin
	     comy=[' ']
	     if prog_txt(k) gt 0 then begin
	     	widget_control,prog_txt(k),bad_id=i,get_value=comy
		
		if i ne 0 then prog_txt(k)=0 else prog_mac(k)=comy(0)
	     endif
	    endfor
	 endif

         on_ioerror, end_fc
	 bid=findfile('lamp.cds',count=cnt)
         if cnt gt 0 then bid=sys_dep('DELET','lamp.cds')
	 out=-1 & openw,out ,'lamp.cds',/get_lun
         for k=0,5 do printf,out,prog_mac(k)
         printf,out,' '
         printf,out,lamp_devps
         printf,out,' '
         printf,out,rx,rz,nlv,'   For rx ry nlv'
         printf,out,' '
         printf,out,styles(0,0),styles(1,0),styles(2,0),!P.psym,'   For styles !P.psym'
         printf,out,' '
         printf,out,inst_value
         printf,out,' '
         printf,out,tcol,'   For color table #'
         printf,out,' '
         printf,out,smoo,'   For smooth image '
         printf,out,' '
         printf,out,'Free line'
         printf,out,'Free line'
         printf,out,' '
	 
         for k=6,n-1 do printf,out,prog_mac(k)
	 
	 end_fc: if out gt 0  then free_lun,out
return
end

pro set_cur_work,event
;** ************
;**
@lamp.cbk
	w =event.index
	wk=strmid(limtxt(w),0,3)
	to_did_cur , wk
return
end

pro show,string_in
;** ****
;**
;
;Handles sho command
;
@lamp.cbk
@dons.cbk
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

     	rhs =' '
	moan='Cant show '
	stat=0 & jjj=1
	catch,stat

        if (stat ne 0) or  (jjj ne 1) then begin
		     catch,/cancel
		     P_MUS,'mus_cannon'
		     if l_message le 0 then print,moan+rhs else $
      		     widget_control,bad_id=iii,l_message,set_value=moan+rhs
      		     print,string(7b)
		     return
        endif else begin

	   string_in=strtrim(string_in,2)
	   coma=strpos(string_in,',')
	   if (coma le 0) then coma=strpos(string_in,' ')
	   fin =strlen(string_in)
	   lstr=fin-coma
	   if (coma le 0) or (coma eq fin-1) then return
	   rhs =strmid(string_in,coma+1,lstr)

	   abc=' '
	   xfor='abc='+rhs & jjj=execute(xfor)

	   icheck=n_elements(abc)
	   if icheck gt 1  then abc=reform(abc,icheck,/overwrite)
	   if icheck gt 10 then abc=abc(0:9)
 	   ans=string(abc)
	   
	   if GEORGE gt 0 then begin
		if icheck gt 10 then ans=['!first elements printed ...',ans]
		to_don_history,-1,0,ans
		WebDo,'log',['SHOW',ans],12
	   endif
	   if formtxt le 0 then begin
              if icheck eq 1 then  if l_message le 0 then print,ans else $
					widget_control,l_message,bad_id=iii,set_value=ans $
              else begin print,ans & ans='!first elements printed ...'
			 if l_message le 0 then print,ans else $
              		 widget_control,bad_id=iii,l_message,set_value=ans
              endelse
           endif else begin
	      if l_message le 0 then begin
		print,ans
		if icheck gt 10 then print,'Woops - Only first 10 elements given'
	      endif else begin
              	if (b_labins(6) and b_labins(7) gt 0) then $
              	widget_control,b_labins(7),bad_id=iii,set_value=ans(0) else $
              	widget_control,formtxt    ,bad_id=iii,set_value=ans,/append
              	widget_control,formtxt    ,bad_id=iii,set_value='' ,/append
	      	if icheck gt 10 then begin
		         i_bust='Woops - Only first 10 elements given'
      		         widget_control,bad_id=iii,l_message,set_value=i_bust
	      	endif else widget_control,bad_id=iii,l_message,set_value=' '
	      endelse
           endelse
endelse

return
end

pro show_internal,uv
;** *************
;**
txti="" & internal,txti & widget_control,bad_id=ii,uv(2),set_value=txti
                          widget_control,bad_id=ii,uv(3),set_value="internal.prox"
end

pro internal,txti
;** ********
;**
txti=[$
";       INTERNAL PROCEDURES AND FUNCTIONS accessible from command lines",$
";       -------- ---------- --- ---------         as from macros.",$
" ",$
";       DRAWIND     Draw    procedure         (window device)",$
";       SEE         Draw    procedure         (gif, png,  ps)",$
";       SEEM        Draw    procedure",$
";       P_SET_FONT  Draw    procedure         (window device)",$
";       SUPRPLOT    Draw    interface         (window device)",$
";       GFIT        Draw    interface         (window device)",$
";       DON_DO_CMD  Do      interface         (window device)",$
" ",$
";       POSITIVE    Convert procedure",$
";       W_ACCU      Add     procedure",$
" ",$
";       RDSET       Read    procedure",$
";       RDRUN       Read    function",$
";       RDSUM       Read    function",$
";       RDAND       Read    function",$
";       RDOPR       Read    function",$
";       CALIBRATION Read    function",$
";       RDFILTER    Read    interface",$
" ",$
";       READ_LAMP   Read    procedure",$
";       WRITE_LAMP  Write   procedure",$
";       SAVESESSION Write   procedure",$
" ",$
";       SHOW        Print   procedure",$
";       SETCOL      Color   procedure         (window device)",$
" ",$
";       TRAP        Scale   procedure         (window device)",$
";       TRAPX       Scale   procedure         (window device)",$
";       TRAPY       Scale   procedure         (window device)",$
";       TRAPT       Total   procedure         (window device)",$
" ",$
";       XICUTE      Lamp    procedure",$
";       SETMANIP    Lamp    procedure",$
" ",$
";       TAKE_DATP   param   procedure",$
";       GIVE_DATP   param   procedure",$
";       MOD_DATP    param   procedure",$
";       TAKE_W      param   procedure",$
";       GIVE_W      param   procedure",$
";       W_STORE     param   procedure",$
";       W_RESTORE   param   procedure",$
";       W_EXCHANGE  param   procedure",$
" ",$
";       MYINIT      init    procedure",$
";       MYHELP      init    procedure",$
";       P_LAYOUT    init    procedure",$
" ",$
" ",$
";pro    DRAWIND [,xsize] [,ysize] [,DrawId=drawid]",$
";***    *******",$
";**",$
";**     Creates a drawing window for plotting in user_macros (default size is 512*512).",$
";**     The  call is DRAWIND,600,600 ,DrawId=drawid         (drawid is returned).",$
" ",$
" ",$
";pro    SEE, W=i [,/Contour ,/Surface ,/Beside ,/Replot,/Screen,/Gif,/Png,/Ps ,XYpixels=[xsz,ysz] ]",$
";***    ***  ",$
";**",$
";**     Has the same function as the 'Plot Wi' button.",$
" ",$
" ",$
";pro    SEEM  [,/Beside ,/Below ,/Image ,/Contour ,/Surface ,Rot=angle",$
";***    ****   ,/Regular, Xrange=[x1,x2], Yrange=[y1,y2] ,/Log ,Zlim=max ]",$
";**",$
";**     SEEM replaces all buttons within DISPLAY WORKSPACE area.",$
" ",$
" ",$
";pro    P_SET_FONT ,size",$
";***    **********",$
";**",$
";**     Changes the look of next graphics interfaces. size is 0,1,2 for large,medium,small ",$
";**     The  call is P_SET_FONT ,2",$
" ",$
" "] & txti=[txti,$
";pro    SUPERPLOT",$
";***    *********",$
";**",$
";**     Interface for over_plotting workspaces.",$
" ",$
" ",$
";pro    GFIT",$
";***    ****",$
";**",$
";**     Interface for fitting.",$
" ",$
" ",$
";pro    DON_DO_CMD",$
";***    **********",$
";**",$
";**     Creates new Do command buttons.",$
" ",$
" ",$
";pro    POSITIVE, w_in",$
";***    ********",$
";**",$
";**     Transforms an integer*2 unsigned array in a long positive one.",$
";**     The  call is POSITIVE ,W1",$
" ",$
" ",$
";pro    W_ACCU, accu=i , add=j [, tolerance=t]",$
";***    ******",$
";**",$
";**     This procedure adds workspace number j to workspace accumulator number i.",$
";**     using the monitors Ni,Nj and propagating the errors EI,Ej.",$
";**     The tolerance is used for vectors; the default is MIN([step(Xi),step(Xj)])/3",$
";**     The call is   W_ACCU, accu=1         (for W1=0)",$
";**                   W_ACCU, accu=1 , add=3 (for W1=W1+W3  using N1,N3,X1,X3,E1,E3)",$
" ",$
" "] & txti=[txti,$
";pro    RDSET  , INST=inst  ,BASE=base [,CYCLE=965] [,STEP=.14553]  [,TOLERANCE=.2]",$
";***    *****              [,/RAW] [,/DEF] [,/DIR] [,/DIF] [,LAST=procedureName]",$
";**                        [,/SUFFIX=string] [,/PREFIX=string] ",$
";**",$
";**     This procedure changes the current instrument name and (or)",$
";**                            the current base access (see Customize).",$
";**     This procedure can also be used to change specific read-variables [STEP,TOL]." ,$
";**                              [/RAW] means do not use treatment in read operations.",$
";**                              [/DEF] means use specific treatment  during the read.",$
";**                              [/DIR] [DIF] [/SUF] [/PREF] specific use at ILL,ESRF.",$
";**                              [LAST=proc]  proc is the name of a procedure called by",$
";**                                           Lamp just after a multiple read (proc,j,data)",$
";**                                           j is worksp number & data is Wj values",$
";**     The  call is RDSET, INST='D20' , BASE='Current Path'",$
" ",$
" ",$
";function RDRUN  , runnumber [,DATP=datp] [,W=i]",$
";******** *****",$
";**",$
";**       This function reads the run number from the currently selected instrument.",$
";**       If datp is set and non 0, all parameters are returned in datp (see READ_tmp.pro).",$
";**       If W is set to 1<i<20 then reading is made in workspace wi",$
";**       The  call is  W1 = RDRUN(1280)",$
" ",$
" ",$
";function RDSUM  , firstrun,lastrun [,DATP=datp] [,W=i] [,/COMPLEMENT]",$
";******** ***** ",$
";**",$
";**       This function reads and adds the run numbers range from the selected instrument.",$
";**       If datp is set and non 0, all parameters are returned in datp (see READ_tmp.pro).",$
";**       If W is set to 1<i<20 then reading is made in workspace wi",$
";**       If COMPLEMENT is set then Wi is not erased before read",$
";**       The  call is  W1 = RDSUM(1280,1295)",$
" ",$
" ",$
";function RDAND  , firstrun,lastrun [,DATP=datp] [,W=i] [,/COMPLEMENT]",$
";******** ***** ",$
";**",$
";**       This function reads and joins the run numbers range from the selected instrument.",$
";**       If datp is set and non 0, all parameters are returned in datp (see READ_tmp.pro).",$
";**       If W is set to 1<i<20 then reading is made in workspace wi",$
";**       If COMPLEMENT is set then Wi is not erased before read",$
";**       The  call is  W1 = RDAND(1280,1295)",$
" ",$
" "] & txti=[txti,$
";function RDOPR  , text [,DATP=datp] [,W=i]",$
";******** ***** ",$
";**",$
";**       This function reads specified runs and performs basic operations on them.",$
";**       If datp is set and non 0, all parameters are returned in datp (see READ_tmp.pro).",$
";**       If W is set to 1<i<20 then reading is made in workspace wi",$
";**       The  call is  W1 = RDOPR( ' 624 + 627>629 - 630>631 - 633 ' )",$
";**                     W2 = RDOPR( ' 624 : 627 ' )",$
";**",$
";**       Result of W1  is   624+ (627+628+629) - (630+631) -633",$
";**       Result of W2  is   [[624],[625],[626],[627]] the runs are joined..!",$
" ",$
" ",$
";pro      CALIBRATION [,FILE=file ,NOCAL=nocal ,LIST=list]",$
";**       ***********",$
";**",$
";**       This procedure sets or unsets calibrations.",$
";**       In   window mode the call is  CALIBRATION",$
";**       In   batch  mode the call is  CALIBRATION, file='calibration_file'",$
";**                                 or  CALIBRATION, /nocal [,/list]",$
" ",$
" ",$
";pro      RDFILTER  [,XRANGE =xrg   ,YRANGE=yrg ,ZRANGE=zrg ,XMASK=xtext ,YMASK=ytext $",$
";***      ********   ,MONIMOD=momod ,XPROJ=xproj ,YPROJ=yproj,ZPROJ=zproj,WKSP =wksp  $",$
";**                  ,MONIVAL=value ,SIGMA=sigma ,TOLERANCE=latol  , SELECTION =selec ]",$
";**",$
";**       This procedure creates a general window interface for the use of",$
";**       the  above function RDOPR.",$
";**       In batch mode, use the keywords. The call is:",$
";**       RDFILTER   ,XRANGE=[10,60] ,YMASK='0 12 27' ,/XPROJ ,WKSP=20 $",$
";**                  ,MONIMOD=3 ,MONIVAL=100000 ,SELECTION='104>122 + 127'",$
" ",$
" ",$
";pro      READ_LAMP ,  filename ,W=i, path=pth",$
";***      *********",$
";**",$
";**       This procedure reads the file 'filename' into workspace number i.",$
";**                                File  filename  is a LAMP_Format file.",$
" ",$
" ",$
";pro      WRITE_LAMP , filename ,W=i [,FORMAT='Bin'] [,FORMAT='Ascii']",$
";***      **********",$
";**",$
";**       This procedure writes workspace number i to the file 'filename'",$
";**                                File   is written in a LAMP_Format.",$
" ",$
" "] & txti=[txti,$
";pro      SAVESESSION",$
";***      ***********",$
";**",$
";**       This procedure saves all variables & workspaces in 'lamp.ses'.",$
" ",$
" ",$
";pro      SHOW   , variable",$
";***      **** ",$
";**",$
";**       This procedure prints the contents of the variable. If variable is an",$
";**            array then only first ten elements are shown.",$
";**       the  call is   SHOW, y1 ",$
" ",$
" ",$
";pro      SETCOL , number",$
";***      ******",$
";**",$
";**       This procedure loads the color table number. This number is the index of the",$
";**            table listed when 'Load new Colors' button is pressed.",$
";**       This number is saved when you exit from LAMP.",$
";**       The  call is  SETCOL, 27",$
" ",$
" ",$
";pro      TRAP   , workspace",$
";***      ****",$
";**",$
";**       This procedure loads the last plotted image into the named workspace.",$
";**       To zoom an image you can drag the left button of the mouse.",$
";**       The call is  TRAP,  W3",$
" ",$
" ",$
";pro      TRAPX  , workspace",$
";***      *****",$
";**",$
";**       This procedure loads the last plotted image into the named workspace",$
";**            and does the sum on the  X axis  into  that workspace.",$
";**       The call is  TRAPX, W4",$
" ",$
" ",$
";pro      TRAPY  , workspace",$
";***      *****",$
";**",$
";**       This procedure loads the last plotted image into the named workspace",$
";**            and does the sum on the  Y axis  into  that workspace.",$
";**       The call is  TRAPY, W5",$
" ",$
" ",$
" ",$
";pro      TRAPT  , workspace",$
";***      *****",$
";**",$
";**       This procedure loads the last plotted image into the named workspace",$
";**            and totalizes   all elements into  that workspace.",$
";**       The call is  TRAPT, W6",$
" ",$
" ",$
" "] & txti=[txti,$
";pro      XICUTE  , formula",$
";***      ******",$
";**",$
";**       This procedure executes the string 'formula' using the Lamp passing rule",$
";**            as if you typed    the formula in the Formulae Entry.",$
";**       The call is  XICUTE, formula",$
" ",$
" ",$
" ",$
";pro      SETMANIP  [,/raw] [,/noraw]",$
";***      ********",$
";**",$
";**       When Lamp calculates a single operation between two workspaces (w1=w2+w3),",$
";**           it tries to adjust the result as a function of the monitors (n2 & n3)",$
";**           and coordinates (x2 & x3) then adjusts the resulting errors (e1).",$
";**       To  prevent Lamp from this extra charge, the call is SETMANIP,/raw",$
";**           to  ask Lamp for  an   extra again , the call is SETMANIP",$
";**",$
";**       Regardless  of this  status,  if Lamp sees  ;++ at the end of a command,",$
";**           it goes in extra for that command only, ;-- is for the opposite expect.",$
" ",$
" ",$
";pro      TAKE_DATP , datp [,W=i] [,/third][,/fourth,/fifth,/sixth]",$
";***      *********",$
";**",$
";**       This procedure allows you to access the following data parameters:",$
";**       datp.X , datp.Y , datp.Z , datp.E , datp.N , datp.PV , datp.W_TIT ....",$
";**       If called from a macro specified  by W3=MACRO(w6,w7)  then:",$
";**                 returned datp is that of w6 by default",$
";**                 returned datp is that of w7 if third is set",$
";**                 returned datp is that of Wi if W is set.",$
";**",$
";**       You can   modify all parameters ex: datp.X_TIT='this is the X axis'",$
";**       You can't modify the size of an array parameter.",$
" ",$
";pro      GIVE_DATP , datp [,W=i] [,/second][,/third,/fourth,/fifth,/sixth]",$
";***      *********",$
";**",$
";**       This procedure allows you to give data parameters back to lamp.",$
";**       If called from a macro specified  by W3=MACRO(w6,w7)  then:",$
";**                      given datp is for w3 by default",$
";**                      given datp is for w6 if second is set",$
";**                      given datp is for w7 if third  is set",$
";**                      given datp is for wi if W  is set.",$
" ",$
";pro      MOD_DATP , datp , tag , value",$
";***      ********",$
";**",$
";**       This procedure allows you to change the size of a parameter.",$
";**       The call is MOD_DATP, datp , 'X' , alog(indgen(50)+1)",$
";**                   MOD_DATP, datp , 'E' , sqrt(w)",$
" ",$
" ",$
" "] & txti=[txti,$
";pro      TAKE_W , variable ,W=i",$
";***      ******",$
";**",$
";**       This procedure gets workspace number i into variable.",$
" ",$
" ",$
" ",$
";pro      GIVE_W , variable ,W=i",$
";***      ******",$
";**",$
";**       This procedure puts variable into workspace number i.",$
" ",$
" ",$
" ",$
";pro      W_STORE    , W=i [,/ALL]",$
";***      *******",$
";**",$
";**       This procedure duplicates workspace number i and its parameters into memory.",$
";**       The call is W_STORE, W=3           (use /ALL to duplicate all workspaces)",$
" ",$
" ",$
";pro      W_RESTORE  , W=i [,/ALL]",$
";***      *********",$
";**",$
";**       This procedure retrieves  workspace number i and its parameters from memory.",$
";**       The call is W_RESTORE, W=3         (use /ALL to retrieve  all workspaces)",$
" ",$
" ",$
";pro      W_EXCHANGE , W=i [,/ALL]",$
";***      **********",$
";**",$
";**       This procedure exchanges  workspace number i and its duplicate   in  memory.",$
";**       The call is W_EXCHANGE, W=3        (use /ALL to exchange  all workspaces)",$
" ",$
" ",$
";pro      MYINIT",$
";***      ******",$
";**",$
";**       This procedure executes your code after lamp is loaded.",$
";**       You may also put some code in the file 'myinit.prox'.",$
" ",$
";pro      MYHELP",$
";***      ******",$
";**",$
";**       This procedure associates your help-text to the upper left ? of lamp.",$
" ",$
" "] & txti=[txti,$
";pro      P_LAYOUT , flag (or a Help-macros-file) [,prox=proxfile]",$
";***      ********",$
";**",$
";**       This procedure should be used as commands in MYINIT procedure.",$
";**       flag='george' , 'lamp' , 'light' , 'full' , 'hide' , 'show' FOR LAMP LAYOUT",$
";**       flag='List_oldTOF' TO LOAD 'A_List_oldTOF.prox' FOR MACROS LAYOUT",$
";**       prox=['file1','file2'] TO LOAD 'file1.prox' & 'file2.prox' IN DO COMMAND GUI",$
" ",$
" ",$
" "]
end


pro dons
;** ****
return
end
