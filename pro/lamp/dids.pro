pro P_DID_SETVAR
;** ************
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
;**
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

    common c_drow,	did_wc,did_we,did_win1,did_4dM,did_etc
    
    if n_elements(lamp_wrd) ne 1 then lamp_wrd=''
    did_repr=lonarr(20)  & did_repr(*)= 0
    did_inib=lonarr(3)   & did_inib(*)= 0
    did_surf=lonarr(2)	 & did_surf(*)= 0
    styles  =intarr(4,2)
    vfl     =lonarr(4)	 & vfl(*)     =-1
    vff     =fltarr(3)
    did_scan=0
    did_tio =0
    did_fu  =0
    flgsurf =0
    wbeside =0
    rx=60 & rz=20 & nlv=24 & smoo=1
    if (sys_dep('STUDENT') or sys_dep('RUNTIME')) then tcol=3 else tcol=1
    if (sys_dep('MACHINE') eq 'win')   then  tcol=3
    tcol=27
   !p.font  =0

    if sys_dep('VERSION') ge 5.0 then LIV_LAMP

    if n_elements(did_wd) eq 0 then begin did_wd=0 & did_we=0 & did_win0=0 & endif

    if b_labins(3) ne 0 then begin did_repr(2)=1 & did_x=512 & did_y=320 & vff(2)=1
	 if  b_labins(3) eq 2  then begin
	  styles(0,0)=3  & styles(1,0)= 1        & did_x=350 & did_y=230
	  if (did_wd eq 0) and (!D.name ne 'Z')  then begin
	    did_wb  =widget_base  (map=0)
	    did_wd  =widget_draw  (did_wb ,retain=2,xsize=did_x,ysize=did_y,colors=-30)
	    widget_control,bad_id=ii  ,did_wb ,/realize
	  endif
	 endif
	 P_AFTER_REALIZE_DID ,0,0,0
	 if  b_labins(3) eq 1 then begin styles(0,0)= 6 & styles(1,0)= 2 & endif
    endif     else begin did_repr(0)=1 & styles(0,0)= 3 & styles(1,0)= 0 & vff(2)=0  & endelse

    vff=[vff,did_repr(2),rz,did_fu,0,0,0,0,0,0,0,rx]
return
end

pro P_DID_CREATE ,base ,ready
;** ************
;**
;** Workspace display unit.
;**
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
;**
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

    common c_drow
    common c_plog,  win,sicon,pixlog
    
    P_DID_SETVAR

    if n_elements(ready) le 0 then begin
	bs0   =widget_base   (base   ,/column)
	bs1   =widget_base   (bs0   ,/row)
    endif else bs1 = ready(0)

	if (lamp_siz ge 800) and (GEORGE ne 1) then text=' DISPLAY WORKSPACE' else text=' DISPLAY'
	if sys_dep('MACHINE') eq 'win' then cap=1 else cap=0

	bs1_1  =widget_label (bs1   ,font=ft_biggest,value=    text)
	bhelp  =widget_button(bs1   ,font=ft_normal ,value='?',$
				     uvalue=[-88,587,0,0,0,0,0,0,0])

	bs1_2  =widget_label (bs1   ,font=ft_biggest ,value='  ')

	bs1b1  =widget_button(bs1   ,font=ft_smaller ,value='<-')
	did_wsc=widget_button(bs1   ,font=ft_propor  ,value='Plot W 1',$
				     uvalue=[-88,301,0,0,0,0,0,0,0])
	bs1b2  =widget_button(bs1   ,font=ft_smaller,value='->')
	widget_control,bad_id=i,bs1b1,set_uvalue=[-88,310,did_wsc,1,0,0,0,0,0]
	widget_control,bad_id=i,bs1b2,set_uvalue=[-88,311,did_wsc,1,0,0,0,0,0]

	bs1_3  =widget_label (bs1   ,font=ft_biggest,value=' ')

	if (GEORGE ne 1) then begin
	bs1_3  =widget_base  (bs1   ,/exclusive,/row)
	bs1_3e =widget_button(bs1_3 ,font=ft_b_normal,value='Below' ,/no_release,$
				     uvalue=[-88,302,0,0,0,0,0,0,0])
	did_inib(0)=bs1_3e
	bs1_3e =widget_button(bs1_3 ,font=ft_b_normal,value='Beside',/no_release,$
				     uvalue=[-88,303,0,0,0,0,0,0,0])
;jock	bs1_3  =widget_base  (bs1   ,/nonexclusive)
;jock	bs1_3e =widget_button(bs1_3 ,font=ft_normal  ,value='Be good',$
;jock				     uvalue=[-88,360,0,0,0,0,0,0,0])
	if sys_dep('MAP') ne -1 then $
	bs1_3  =widget_button(bs1   ,font=ft_normal,value='Options...',/menu,$
				     resource_name='discret') else $
	bs1_3  =widget_button(bs1   ,font=ft_normal,value='Options...',/menu)
	bs1_3t =widget_button(bs1_3 ,font=ft_normal,value='Titles...' ,uvalue=[-88,360,0,0,0,0,0,0])
	did_4dM=widget_button(bs1_3 ,font=ft_normal,value='4D object' ,/menu)
	did_4vM=widget_button(did_4dM,font=ft_normal,value='Vrml File',uvalue='Vrml')
	widget_control,did_4dM,sensitive=0
	if sys_dep('VERSION') ge 5.0 then $
	   bid =widget_button(bs1_3 ,font=ft_normal,value='Need Coffee!' ,uvalue=[-88,304,5,0])

	if lamp_siz ge 800 then text='Be print' else text='Print'
	if sys_dep('MAP') ne -1 then $
	bs1b   =widget_button(bs1   ,font=ft_normal,value=text,uvalue=[-88,350,0,0,0,0,0,0,0],$
				     resource_name='discret') else $
	bs1b   =widget_button(bs1   ,font=ft_normal,value=text,uvalue=[-88,350,0,0,0,0,0,0,0])

	endif
	if GEORGE ne 0 then GEORGEO, TIMER=bs1, freq=duduch1 ,lim=duduch2
	if GEORGE eq 1 then did_fu=1

;*******
	if n_elements(ready) le 0 then bsrow  =widget_base  (bs0   ,/row) $
				  else bsrow  =ready(1)
;*******
	if n_elements(ready) le 0 then bsopt  =widget_base  (bsrow ,/column) $
				  else bsopt  =ready(2)

	if abs(sys_dep('MAP')) eq 1 then option=1 else option=1
	bsoptff=bsopt
	if option then $
	bsoptf =widget_base  (bsoptff,/column,/frame) else bsoptf=bsoptff

	bsopt0 =widget_base  (bsoptf ,/row)
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive)
	bsimage=widget_button(bsopt1 ,font=ft_b_normal ,value='Image  ',$
				      uvalue=[-88,320,0,0,0,0,0,0,0])
	did_inib(1)=bsimage
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive)
	bslevel=widget_button(bsopt1 ,font=ft_b_normal ,value='Contour',$
				      uvalue=[-88,321,0,0,0,0,0,0,0])

	bsopt0 =widget_base  (bsoptf ,/row)
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive)
	bsurfac=widget_button(bsopt1 ,font=ft_b_normal ,value='Surface',$
				      uvalue=[-88,322,0,0,0,0,0,0,0])
	if sys_dep('MAP') ne -1 then $
	bsura1 =widget_button(bsopt0 ,font=ft_smaller  ,value='<',resource_name='discret') else $
	bsura1 =widget_button(bsopt0 ,font=ft_smaller  ,value='<')
	bsuraz =widget_text  (bsopt0 ,font=ft_b_normal ,value='+20',xsize=4+cap,ysize=1,/editable)
	if sys_dep('MAP') ne -1 then $
	bsura2 =widget_button(bsopt0 ,font=ft_smaller  ,value='>',resource_name='discret') else $
	bsura2 =widget_button(bsopt0 ,font=ft_smaller  ,value='>')
	did_repr(4) =bsuraz
	widget_control,bad_id=i,bsura1,set_uvalue=[-88,326,bsuraz,0,0,0,0,0,0]
	widget_control,bad_id=i,bsura2,set_uvalue=[-88,327,bsuraz,0,0,0,0,0,0]
;------
	bsopt0 =widget_base  (bsoptf  ,/row)
	bsopt1 =widget_base  (bsopt0  ,/nonexclusive)
	bsxy   =widget_button(bsopt1  ,font=ft_b_normal ,value='Range etc...')

	widget_control,bad_id=i  ,bsxy,set_uvalue=[-88,319 , bsxy,bsoptf,bsopt0]

	if (option) then begin
	  bsoptf =widget_base  (bsoptff,map=0,/column,/frame)
	  P_DID_CREATE_MORE , bsxy,bsoptf,bsopt0
	  widget_control,bad_id=i,bsxy,set_uvalue=[-88,319 , bsxy,bsoptf,0]
	endif
;*******allow restarting lamp after /reset.
	if n_elements(did_lamp) gt 1 then if (not execute('wset,did_pix>1')) then begin
	   did_lamp=0
	   did_pio =0
	   did_buf =0
	   sicon   =0
	   Snapix  =0
	   did_zoom_reset
	   p_did_save_reset
	endif
;*******
	did_x  =512
	if (sys_dep('MACHINE') eq 'win') then did_y=256 else did_y=288
	if lamp_siz gt  950 then did_y  =320
	if lamp_siz lt  800 then did_x  =300
	if lamp_siz lt  800 then did_y  =230
	if n_elements(ready) le 0 then begin
	   junky   =widget_base  (bsrow)
	   did_wb  =widget_base  (junky,map=1)
	   did_wc  =widget_base  (junky,map=0)
	   did_wd  =widget_draw  (did_wb ,retain=2,xsize=did_x,ysize=did_y,colors=-30,$
					 /button_events,/motion_events)
	   did_we  =0L
	   etxt="did_we  =widget_draw  (did_wc , GRAPHICS_LEVEL=2,UVALUE='DRAW', RETAIN=0,"+ $
	                               "XSIZE=did_x, YSIZE=did_y,/BUTTON_EVENTS,/EXPOSE_EVENTS)"
	   if sys_dep('VERSION') ge 5.0 then ii=EXECUTE(etxt)
	   
	   if GEORGE ne 1 then widget_control,did_wd,draw_motion_events=1
	endif else $
	   did_wd  =ready(3)
	did_curw=10
return
end

pro P_DID_CREATE_MORE, bsxy,bsoptf,bsopt0
;** *****************
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

	tmpbase=0    & P_messi , tmpbase,(lamp_b1+0)
	
	if GEORGE eq 1 then $
	bid = widget_button(widget_base(bsopt0  ,/nonexclusive) $
						,font=ft_normal   ,value='Be good' $
						,uvalue=[-88,360,0,0,0,0,0,0,0])   $
	else begin
	  bsbs   =widget_base  (bsopt0,/nonexclusive)
	  if sys_dep('MAP') ne -1 then $
	  bid    =widget_button(bsbs  ,font=ft_smallest,value='bg',uvalue=[-88,344,0],resource_name='discret') else $
	  bid    =widget_button(bsbs  ,font=ft_smallest,value='bg',uvalue=[-88,344,0])
	endelse
;------
	if sys_dep      ('MACHINE') eq 'win' then cap=1 else cap=0

	bsopt0 =widget_base  (bsoptf ,/row)
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive)
	bstretc=widget_button(bsopt1 ,font=ft_normal ,value='X range',$
				      uvalue=[-88,325,0,0,0,0,0,0,0])
	bsminx =widget_text  (bsopt0 ,font=ft_propor ,value=' Min'  ,xsize=4+cap,ysize=1,$
				     /all_events,/editable)
	bsmaxx =widget_text  (bsopt0 ,font=ft_propor ,value=' Maxi ',xsize=6+cap,ysize=1,$
				     /all_events,/editable)
	widget_control,bad_id=i,bsminx,set_uvalue=[-88,330,bstretc,7,1,0,0,0,0]
	widget_control,bad_id=i,bsmaxx,set_uvalue=[-88,330,bstretc,7,1,0,0,0,0]

	bsopt0 =widget_base  (bsoptf ,/row)
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive)
	bstretc=widget_button(bsopt1 ,font=ft_normal ,value='Y range',$
				      uvalue=[-88,329,0,0,0,0,0,0,0])
	bsminy =widget_text  (bsopt0 ,font=ft_propor ,value=' Min'  ,xsize=4+cap,ysize=1,$
				     /all_events,/editable)
	bsmaxy =widget_text  (bsopt0 ,font=ft_propor ,value=' Maxi ',xsize=6+cap,ysize=1,$
				     /all_events,/editable)
	widget_control,bad_id=i,bsminy,set_uvalue=[-88,330,bstretc,8,1,0,0,0,0]
	widget_control,bad_id=i,bsmaxy,set_uvalue=[-88,330,bstretc,8,1,0,0,0,0]

	bsmins=0
	bsmaxs=0
	if lamp_siz ge 800 then begin
	bsopt0 =widget_base  (bsoptf ,/row,map=0)
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive)
	bstretc=widget_button(bsopt1 ,font=ft_normal ,value='S range',$
				      uvalue=[-88,318,0,0,0,0,0,0,0])
	bsmins =widget_text  (bsopt0 ,font=ft_propor ,value=' Min'  ,xsize=4+cap,ysize=1,$
				     /all_events,/editable)
	bsmaxs =widget_text  (bsopt0 ,font=ft_propor ,value=' Maxi ',xsize=6+cap,ysize=1,$
				     /all_events,/editable)
	widget_control,bad_id=i,bsmins,set_uvalue=[-88,330,bstretc,17,1,0,0,0,0]
	widget_control,bad_id=i,bsmaxs,set_uvalue=[-88,330,bstretc,17,1,0,0,0,0]
	did_inib(2)=bsopt0
	endif

	bsopt0 =widget_base  (bsoptf ,/row)
	bsopt1 =widget_base  (bsopt0 ,/nonexclusive,/row)
	bslog  =widget_button(bsopt1 ,font=ft_normal ,value='W log',$
				      uvalue=[-88,323,0,0,0,0,0,0,0])
	bsmaxb =widget_button(bsopt1 ,font=ft_normal ,value='W lim')
	bsmaxv =widget_text  (bsopt0 ,font=ft_propor ,value=' Maxi ',xsize=6+cap,ysize=1,$
				     /all_events,/editable)
	widget_control,bad_id=i,bsmaxb,set_uvalue=[-88,328,bsmaxv,0,0,0,0,0,0]
	widget_control,bad_id=i,bsmaxv,set_uvalue=[-88,330,bsmaxb,9,bsmaxv,0,0,0,0]
	P_messi , tmpbase,(lamp_b1+0)
	did_repr(10)=bsminx
	did_repr(11)=bsmaxx
	did_repr(12)=bsminy
	did_repr(13)=bsmaxy
	did_repr(15)=bsmins
	did_repr(16)=bsmaxs
return
end

function tvrdd,a,b,c,d
;******* *****
;**
	if !D.Name eq 'WIN' then begin
		if n_elements(d) eq 1 then w=tvrd(a,b,c,d,/true) else w=tvrd(/true)
		w=color_Quan(w,1,a,b,c)
	endif else begin
		if n_elements(d) eq 1 then w=tvrd(a,b,c,d) else w=tvrd() & a=0
	endelse
return, w
end

;*************************************** Restore         *********************************
;*************************************** Restore         *********************************
;*************************************** Restore         *********************************
function P_ICK_LIST, pk_pthv, pk_flt,cnt
;******* **********
;**
	pk_list=''
	stat=0 & cnt=0
	catch,stat
	if stat eq 0 then begin
		cd,pk_pthv,current=mee
		pk_list=findfile(pk_flt,count=cnt)
		if strpos(pk_flt,'.hdf' ) ge 0 then begin
		   pk_more=findfile('*.nxs',count=cn2)
		   if cn2 gt 0 then pk_list=[pk_list,pk_more] & cnt=cnt+cn2
		   if cnt gt 0 then begin idx=where(strpos(pk_list,'.hdfimg') eq -1)
		                          if idx(0) ge 0 then begin pk_list=pk_list(idx) & cnt=n_elements(idx) & endif
		                          idx=where(strpos(pk_list,'.nxsimg') eq -1)
		                          if idx(0) ge 0 then begin pk_list=pk_list(idx) & cnt=n_elements(idx) & endif
		   endif
		endif
		if strpos(pk_flt,'_LAMP') ge 0 then begin
		   pk_more=findfile(pk_flt+'.hdf',count=cn2)
		   if cn2 gt 0 then pk_list=[pk_list,pk_more] & cnt=cnt+cn2
		   pk_more=findfile('*.xml',count=cn2)
		   if cn2 gt 0 then pk_list=[pk_list,pk_more] & cnt=cnt+cn2

		   pk_more=findfile('*.htm',count=cn2)
		   if cn2 gt 0 then $
		   for i=0L,cn2-1 do begin tmp=pk_more(i)
				j=    strpos(tmp,'.htm') & tmp=strmid(tmp,0,j)+'.zip'
				re=   findfile(tmp,count=cn3)
				if cn3 eq 0 then begin
				   tmp=strmid(tmp,0,j)+'.xdr'
				   re=findfile(tmp,count=cn3) & endif
				if cn3 gt 0 then begin
				   pk_list=[pk_list,pk_more(i)] & cnt=cnt+1 & endif
		   endfor
		endif
		cd,mee
	endif else begin
		catch,/cancel
		P_MUS,'mus_cannon'
	endelse
	if cnt gt 0 then begin
			 ln=strpos(strupcase(pk_list(0)),strupcase(pk_pthv))
			 if ln ge 0 then ln=ln+strlen(pk_pthv)
			 for i=long(0),cnt-1 do pk_list(i)=strmid(pk_list(i),ln,35)
	endif  else pk_list='No file '+pk_flt
return,pk_list
end

pro P_ICK_INIT,frm
;**
@lamp.cbk
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

    i=xregistered('PICKDATA')
    if i le 0 then begin

	P_MUS,'mus_harp'
    	if n_elements(pk_sli) le 0 then pk_sli=1
    	if n_elements(pk_frm) le 0 then pk_frm=1
    	if pk_frm eq 0 then pk_frm =1
	pe='' & ii=sys_dep('POT',pe)
    	pk_hyst=''

    	pk_bxt =[ 'Lamp' , 'XY (Z)' , 'gel tif'  , 'Mar'   , 'Scan' , 'NeXus' , 'Other']
    	pk_ext =['_LAMP' , '*.*'    ,'.gel'      ,'.image' ,'.WIND' , '.hdf'  ,  pe+'*']

	IF (GEORGE ne 0) or (sys_dep('VERSION') lt 4.0) then begin j=5 & if GEORGE ne 0 then j=3
							     if n_elements(b_labins) ge 6 then $
							        if b_labins(5) eq 1 then pk_frm=j+1
							     pk_bxt(j)='Dial'  & pk_ext(j)='dial_*.pro*' & endif
    	pk_flt ='*' + pk_ext(pk_frm-1)

	pk_stak=lonarr(14+4)
    	pk_idx =-1
    	pk_img = 0
    	P_GET_DATAPATH ,pk_pthv

	pk_list=P_ICK_LIST(pk_pthv, pk_flt,cnt)

	if cnt eq 0 then pk_list='No '+pk_ext(pk_frm-1)+ ' Data in Path'

	if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0

	pk_base=widget_base  (title='Lamp Pick Data',resource_name='lamptouch',/column)

	pk_p0  =widget_base  (pk_base,/row)
	pk_plab=widget_label (pk_p0  ,value='PATH:' ,font=ft_b_bigger)
	pk_path=widget_text  (pk_p0  ,value=pk_pthv ,font=ft_b_bigger,xsize=40,ysize=1,/editable)
	pk_butu=widget_button(pk_p0  ,value='Update',font=ft_b_normal) & put_logo,pk_p0

	if frm le 0 then begin
	   nxb=n_elements(pk_ext)
	   pk_x   =lonarr(nxb)
	   pk_p0b =widget_base  (pk_base,/row)
	   pk_plab=widget_label (pk_p0b ,value='FORMAT:',font=ft_b_bigger)
	   pk_p0b1=widget_base  (pk_p0b ,/row,/exclusive)
	   for j=0,nxb-1 do $
	   pk_x(j)=widget_button(pk_p0b1,value=pk_bxt(j),font=ft_b_normal)
	   endif

	pk_p1  =widget_base  (pk_base,/row)
	pk_p11 =widget_base  (pk_p1  ,/column)
	pk_root=widget_base  (pk_p11)
	pk_stak(14) =widget_base  (pk_root,map=1)
	 pk_lab     =widget_label (pk_stak(14),value='Files',font=ft_b_bigger)
	pk_stak(15) =widget_base  (pk_root,map=0,/row,uvalue='')
	 pk_lab     =widget_label (pk_stak(15),value='File',font=ft_b_bigger)
	 pk_lab     =widget_label (pk_stak(15),value='{'   ,font=ft_propor)
	 pk_stak(16)=widget_text  (pk_stak(15),value='1'   ,font=ft_propor,xsize=2+cap,ysize=1,/editable)
	 pk_lab     =widget_label (pk_stak(15),value='.'   ,font=ft_propor)
	 pk_stak(17)=widget_text  (pk_stak(15),value='1'   ,font=ft_propor,xsize=2+cap,ysize=1,/editable)
	 pk_lab     =widget_label (pk_stak(15),value='}'   ,font=ft_propor)
	pk_blis=widget_list  (pk_p11 ,value=pk_list,font=ft_b_normal ,ysize=10,xsize=13)
	pk_lab =widget_label (pk_p11 ,value='SnapShot' ,font=ft_b_bigger)
	pk_draw=widget_draw  (pk_p11 ,retain=2  ,xsize=192,ysize=192)

	pk_root    =widget_base  (pk_p1)
	pk_stak(3)=1
	pk_stak(0) =widget_base  (pk_root,/column,map=0)
	pk_lab     =widget_label (pk_stak(0),value='Ascii XY Organisation' ,font=ft_b_bigger)
	pk_stk     =widget_base  (pk_stak(0),/column,/frame)
	pk_lab     =widget_label (pk_stk    ,value=' ')
	pk_bid     =widget_base  (pk_stk    ,/row)
	 pk_lab    =widget_label (pk_bid    ,value=' Headers'      ,font=ft_b_normal)
	 pk_stak(2)=widget_text  (pk_bid    ,value=' 0 ' ,xsize=3+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' Step'         ,font=ft_b_normal)
	 pk_stak(3)=widget_text  (pk_bid    ,value=' 1 ' ,xsize=3+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' max Pair'     ,font=ft_b_normal)
	 pk_stak(13)=widget_text (pk_bid    ,value='2000',xsize=5+cap,ysize=1,font=ft_propor,/editable)
	pk_bid     =widget_base  (pk_stk    ,/row)
	 pk_lab    =widget_label (pk_bid    ,value='Contents:'     ,font=ft_b_normal)
	 pk_bid    =widget_base  (pk_bid    ,/column,/exclusive)
	   pk_bi   =widget_button(pk_bid    ,value='X , Y'         ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,0]) & widget_control,pk_bi,set_button=1
	   pk_bi   =widget_button(pk_bid    ,value='X , Y , E'     ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,1])
	   pk_bi   =widget_button(pk_bid    ,value='X , Y , Z'     ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,2])
	   pk_bi   =widget_button(pk_bid    ,value='X , Y , Z , V' ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,3])
	   pk_bi   =widget_button(pk_bid    ,value='V , X , Y , Z' ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,4])
	   pk_bi   =widget_button(pk_bid    ,value='W (m,n)'       ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,5])
	   pk_bi   =widget_button(pk_bid    ,value='W (m,n,f)'     ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,0,6])
	pk_bid     =widget_base  (pk_stk    ,/row)
	 pk_lab    =widget_label (pk_bid    ,value='Read XY'       ,font=ft_b_normal)
	 pk_bid    =widget_base  (pk_bid    ,/row,/exclusive)
	   pk_bi   =widget_button(pk_bid    ,value='by pairs'      ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,1,0]) & widget_control,pk_bi,set_button=1
	   pk_bi   =widget_button(pk_bid    ,value='by n elements' ,font=ft_b_normal,/no_release,$
	                                    uvalue=[-88,387,1,1])
	pk_bid     =widget_base  (pk_stk    ,/row)
	 pk_lab    =widget_label (pk_bid    ,value=' m='           ,font=ft_b_normal)
	 pk_stak(4)=widget_text  (pk_bid    ,value='  64  ',xsize=6+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' n='           ,font=ft_b_normal)
	 pk_stak(5)=widget_text  (pk_bid    ,value='  64  ',xsize=6+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' f='           ,font=ft_b_normal)
	 pk_stak(6)=widget_text  (pk_bid    ,value=' 1 ',xsize=3+cap,ysize=1,font=ft_propor,/editable)
	pk_bid     =widget_base  (pk_stk    ,/row)
	 pk_lab    =widget_label (pk_bid    ,value='String_# between XY pairs: (a X b Y c Z d V)',font=ft_b_normal)
	pk_bid     =widget_base  (pk_stk    ,/row)
	 pk_lab    =widget_label (pk_bid    ,value=' a='           ,font=ft_b_normal)
	 pk_stak(7)=widget_text  (pk_bid    ,value=' 0 ',xsize=3+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' b='           ,font=ft_b_normal)
	 pk_stak(8)=widget_text  (pk_bid    ,value=' 0 ',xsize=3+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' c='           ,font=ft_b_normal)
	 pk_stak(9)=widget_text  (pk_bid    ,value=' 0 ',xsize=3+cap,ysize=1,font=ft_propor,/editable)
	 pk_lab    =widget_label (pk_bid    ,value=' d='           ,font=ft_b_normal)
	 pk_stak(10)=widget_text (pk_bid    ,value=' 0 ',xsize=3+cap,ysize=1,font=ft_propor,/editable)

	pk_stak(1) =widget_base  (pk_root,/column)
	pk_lab     =widget_label (pk_stak(1),value='Header Contents',font=ft_b_bigger)
	pk_hed     =widget_text  (pk_stak(1),value=[ ' ',' ']       ,font=ft_b_normal,xsize=40,ysize=24,/scroll)

	pk_p2  =widget_base  (pk_base,/row)
	if GEORGE eq 1 then titi='DIAL to be used' else titi='WK_Space to use'
	pk_slid=widget_slider(pk_p2,title=titi               ,font=ft_b_normal,$
				    minimum=1,maximum=20,value=pk_sli)
	pk_butg=widget_button(pk_p2,value='GET THE FILE'     ,font=ft_b_normal)
	pk_butc=widget_button(pk_p2,value=' DONE '           ,font=ft_b_normal)
	bidon  =widget_label (pk_p2,value='     '            ,font=ft_b_normal)
	pk_butd=widget_button(pk_p2,value='DELETE THE FILE'  ,font=ft_b_normal)
	pk_p3  =widget_label (pk_p2,value='                 ',font=ft_b_normal,xsize=120)
        bid=sys_dep      ('DYNLAB',pk_base,1)
	widget_control,bad_id=i,pk_base,group_leader=lamp_b1 ,/realize & put_logo

	widget_control,bad_id=i,pk_draw,get_value =  pixw
	if frm le 0 then begin
	   for j=0,nxb-1 do widget_control,bad_id=i,pk_x(j),set_uvalue=[-88,386,pk_blis,j+1]
	   widget_control,bad_id=i,pk_x(pk_frm-1),set_button=1
	   if pk_ext(pk_frm-1) eq '*.*'  then begin i1=1  & i2=0  & endif else begin i1=0  & i2=1  & endelse
	   widget_control,bad_id=i,pk_stak(i1),map=0
	   widget_control,bad_id=i,pk_stak(i2),map=1
	   if pk_ext(pk_frm-1) eq '.hdf' then begin i1=14 & i2=15 & endif else begin i1=15 & i2=14 & endelse
	   widget_control,bad_id=i,pk_stak(i1),map=0
	   widget_control,bad_id=i,pk_stak(i2),map=1
	   endif
	widget_control,bad_id=i,pk_path,set_uvalue=[-88,381,pk_blis]
	widget_control,bad_id=i,pk_blis,set_uvalue=[-88,382,pk_hed ,pixw,192,192]
	widget_control,bad_id=i,pk_butg,set_uvalue=[-88,383,pk_hed ,pk_p3,pk_slid,pixw,192,192,pk_stak(15)]
	widget_control,bad_id=i,pk_butc,set_uvalue=[-88,384,0]
	widget_control,bad_id=i,pk_butd,set_uvalue=[-88,385,pk_blis,pk_p3]
	widget_control,bad_id=i,pk_butu,set_uvalue=[-88,381,pk_blis]

	XMANAGER, 'PICKDATA'  ,pk_base ,event_handler='LAMP_EVENT_PARSER',/just_reg

    endif else begin widget_control,bad_id=i,pk_base,map=1
		     P_ICK_PTH ,[-88,381,pk_blis]
    endelse
return
end

pro P_ICK_FRM,ev,uv
;** *********
;**
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

	if ev.select eq 1 then begin
	   pk_frm=uv(3)
	   pk_flt='*' + pk_ext(pk_frm-1)
	   P_ICK_PTH,uv
	   if pk_ext(pk_frm-1) eq '*.*'  then begin i1=1  & i2=0  & endif else begin i1=0  & i2=1  & endelse
	   widget_control,bad_id=i,pk_stak(i1),map=0
	   widget_control,bad_id=i,pk_stak(i2),map=1
	   if pk_ext(pk_frm-1) eq '.hdf' then begin i1=14 & i2=15 & endif else begin i1=15 & i2=14 & endelse
	   widget_control,bad_id=i,pk_stak(i1),map=0
	   widget_control,bad_id=i,pk_stak(i2),map=1
	endif
return
end
pro P_ICK_LST,ev,uv
;** *********
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

    pk_idx=ev.index
    if pk_idx ge 0 then begin
	fil =pk_list(pk_idx)
	pk_hyst='' & w_buf=0
	pp2    =-2 & NXe='1' & NXd='1' & NXed=''

	if (pk_ext(pk_frm-1) eq '.hdf') or (pk_ext(pk_frm-1) eq '.xml') then begin
	 widget_control,pk_stak(16),bad_id=ii,get_value=NXe  &  NXe=strtrim(NXe(0),2)
	 widget_control,pk_stak(17),bad_id=ii,get_value=NXd  &  NXd=strtrim(NXd(0),2)
	 if (NXe gt '1') or (NXd gt '1') then NXed='{' +NXe+'.'+NXd+'}'
	endif
	 widget_control,pk_stak(15),bad_id=ii,set_uvalue=NXed

	if pk_ext(pk_frm-1) eq '_LAMP'  then p_did_restore_wrk,fil     ,pk_pthv,'0',pk_hyst,pp2

	if pk_ext(pk_frm-1) eq '.hdf'   then p_did_res_hdf    ,fil+NXed,pk_pthv,'0',pk_hyst,pp2
	if pk_ext(pk_frm-1) eq '.xml'   then p_did_res_hdf    ,fil+NXed,pk_pthv,'0',pk_hyst,pp2

	if (pk_ext(pk_frm-1) eq '.gel') or (pk_ext(pk_frm-1) eq '.image') or (pk_ext(pk_frm-1) eq '.WIND') $
	then if did_scan eq -1 then SL_SCANLOAD,did_scan,lamp_dir

	if pk_ext(pk_frm-1) eq '.gel'   then sl_lampscan, '.gel'   ,w_buf,pp2,pk_hyst,0, pk_pthv+fil

;	if pk_ext(pk_frm-1) eq '.image' then sl_lampscan, '.image' ,w_buf,pp2,pk_hyst,0, pk_pthv+fil

	if pk_ext(pk_frm-1) eq '.WIND'  then sl_lampscan, 'restore',w_buf,pp2,pk_hyst,0, pk_pthv+fil

	if !D.name ne 'Z' then wset,uv(3) else device,set_resolution=[uv(4),uv(5)] & erase
	u=-1
	if pk_ext(pk_frm-1) eq 'dial_*.pro*' then begin on_ioerror,misdial & str=''
					openr,u,pk_pthv+fil,/get_lun
					while (1) do begin str    = strarr(10)   & readf,u,str
					                   pk_hyst=[pk_hyst,str] & endwhile
					misdial: if u gt 0 then free_lun,u
					pk_hyst=[pk_hyst,str]
	endif
	if pk_ext(pk_frm-1) eq '*.*'    then begin on_ioerror,misread & str=strarr(10)
					openr,u,pk_pthv+fil,/get_lun
					readf,u,str
					misread: if u gt 0 then free_lun,u
					for i=0,9 do xyouts,2,173-(19*i),str(i),charsize=1.,/device,color=255

	endif else widget_control,bad_id=i,uv(2),set_value=pk_hyst

	in=-1
	pk_img=0
	on_ioerror,mispixf
	ext='img'

	i =findfile(pk_pthv+fil+'.Z',count=cnt)
	if cnt eq 1 then bid=sys_dep      ('UN_Z',pk_pthv+fil+'.Z',lamp_dir)

	i=  strpos(fil,'.htm') & ordur=0
	if i lt 0 then i=strpos(fil,'.xml')
	if i ge 0 then begin            fil=strmid(fil,0,i)
	                                res=findfile(pk_pthv+fil+'_s.png',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'_i.png',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'-1.png',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'_s.jpg',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'_i.jpg',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'-1.jpg',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'_s.gif',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'_i.gif',count=cnt)
	                                if  cnt eq 0 then $
	                                res=findfile(pk_pthv+fil+'-1.gif',count=cnt)
	                                if  cnt gt 0 then fil=res(0)
	endif     else begin            fii=fil & ii =sys_dep('POT+',fil,ext,1)
	                                res=findfile(pk_pthv+fil         ,count=cnt)
	                                if  cnt eq 0 then begin ext='png' & fil=fii
	                                                        ii =sys_dep('POT+',fii,ext,1)
	                                res=findfile(pk_pthv+fii         ,count=cnt) & endif
	                                if  cnt eq 0 then begin ext='jpg'
	                                                        ii =sys_dep('POT+',fil,ext,1)
	                                res=findfile(pk_pthv+fil         ,count=cnt) & endif
	                                if  cnt gt 0 then begin fil=res(0) & ordur=1 & endif
	endelse
	if NXed ne '' then cnt=0                          ;**Snap already in w_buf or other dataset
	if cnt  gt  0 then if pp2 eq 10 then READ_KIF,fil,w_buf $
	                                else READ_KIF,fil,w_buf,uv(4),uv(5) ;**pp2=10 -> (png,gif) else (img)
	mispixm:
	s=size(w_buf)
	if (s(1) eq uv(4)) and (s(2) eq uv(5)) then begin
		 worder=!order & !order=ordur
		 tvscl,w_buf   & !order=worder
		 pk_img=1
		 endif
	mispixf:if in gt 0 then free_lun,in
	p_did_setwin0
	w_buf=0
    endif
return
end
pro P_ICK_MICO, wkstring,xx,yy,uv,pp2
;** **********
;**
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

	if pk_img eq 0 then begin
	   fil =pk_list(pk_idx)

	   if pp2 eq 10 then ext='png' else ext='img'

	   i=  strpos(fil,'.htm')
	   if i lt 0 then i  =strpos(fil,'.xml')
	   if i ge 0 then fil=strmid(fil,0,i)+'-1.png' else ii =sys_dep('POT+',fil,ext,1)

	   kpDname=!D.name	; or pixmap in uv(5) but problems with true colors...
	   set_plot, 'Z' & device,set_resolution=[uv(6),uv(7)] & erase & w0=0
	   p_did_makeicon, wkstring,xx,yy, uv(6),uv(7) ,0 ,w0 ,'s'
	   worder =!order & !order=0
	   if strpos(fil,'img') gt 0 then !order=1
	   w0=tvrd(0,0,uv(6),uv(7))
	   !order =worder
	   set_plot,kpDname

	   if pp2 ne -10 then WRITE_KIF,pk_pthv+fil,w0,transparent=[0] ;**pp2=10 -> (png,gif) else (img)

	   p_did_setwin0
	endif
return
end

pro P_ICK_PTH ,uv
;** *********
;**
@lamp.cbk
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

	widget_control,bad_id=i,pk_path,get_value=pth
	pk_pthv=sys_dep      ('BLANKS',pth(0))
	if pk_pthv ne '' then begin
	   car=strmid(pk_pthv,strlen(pk_pthv)-1,1)
	   if (car ne lamp_dvd) then begin
			   pk_pthv=pk_pthv+lamp_dvd
			   widget_control,bad_id=i,pk_path,set_value=pk_pthv
			   endif
	endif
	pk_idx =-1

	pk_list=P_ICK_LIST(pk_pthv, pk_flt,cnt)

	widget_control ,bad_id=i,uv(2),set_value=pk_list

return
end
pro P_ICK_GET ,pth,filin,frm,info,uv
;** *********
;**
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

    filin=''
    if pk_idx ge 0 then begin
	pth  =pk_pthv
	filin=pk_list(pk_idx)
	frm  =pk_ext(pk_frm-1)
	info =pk_hyst
	widget_control,bad_id=i,uv(4),get_value=pk_sli
    endif
return
end
pro P_ICK_DEL,uv
;** *********
;**
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

    if pk_idx ge 0 then begin
    	fild=pk_list(pk_idx)
	if fild ne '' then begin
		j=strpos(fild,'.htm') &  if j lt 0 then j=strpos(fild,'.xml')
		if j gt 0 then begin     tmp=strmid(fild,0,j)
		  res =FINDFILE (pk_pthv+tmp+'*.*',count=cnt)
		  for i=0L,cnt-1 do begin
			if strpos(res(i),tmp+'.htm')   ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'.xml')   ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'.xdr')   ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'.zip')   ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'_i.gif') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'_s.gif') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'-1.gif') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'_i.png') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'_s.png') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'-1.png') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'_i.jpg') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'_s.jpg') ge 0 then bid=sys_dep('DELET',res(i))
			if strpos(res(i),tmp+'-1.jpg') ge 0 then bid=sys_dep('DELET',res(i))
		  endfor
		endif else begin
		  bid =sys_dep ('POT+' ,fild,'*' , 2)
		  fild=FINDFILE(pk_pthv + fild)
		  for i=0,n_elements(fild)-1 do bid=sys_dep('DELET',fild(i))
		endelse
	endif
	P_ICK_PTH,uv
    endif
return
end

pro P_GET_DATAPATH, pk_pthv
;** **************
;**
@lamp.cbk
    if  n_elements(pk_pthv) le 0 then pk_pthv=''
    if  pk_pthv eq '' then begin
	pk_pthv=getenv('LAMP_DATAPATH')
	if pk_pthv eq '' then cd,current = pk_pthv
	pk_pthv=sys_dep      ('BLANKS',pk_pthv)
	car=strmid(pk_pthv,strlen(pk_pthv)-1,1)
	if (car ne lamp_dvd) then pk_pthv=pk_pthv+lamp_dvd
    endif
return
end

pro P_ICK_SCAN, pp2,frm,wnumber,uv,hyst,fname,pth
;** **********
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
    common c_ick_scan,  c_pp2,c_frm,c_w,c_uv,c_hyst,tbl

if n_elements(tbl) le 1 then  tbl =[512,512,1, 5,0, 0, 0,1]
c_pp2=pp2 & c_frm=frm & c_w=wnumber & c_uv=uv & c_hyst=hyst
if did_scan eq -1 then SL_SCANLOAD, did_scan,lamp_dir
if did_scan ge  0 then ii=execute('descript,pth+fname,tbl')
return
end

pro P_ICK_RETURN,ok,filename,xtbl
;** ************
;**
@lamp.cbk
common c_ick_scan,c_pp2,c_frm,c_w,c_uv,c_hyst,tbl

if ok eq 1 then begin
	tbl=xtbl & xtbl=[xtbl,0]
	if  xtbl(3) eq 5 then begin xtbl(3)=4 & xtbl(8)=1 & endif
	flg='pass'
	i =execute( 'sl_lampscan, flg ,w' + c_w + ',c_pp2,0,xtbl, filename' )
	comhis='P_ICK_SCAN,-1,"'+c_frm+'","'+c_w+'",0,h,"'+filename+'",""'
	p_did_just_read, c_pp2,c_frm,c_w,c_uv,c_hyst,filename ,bidon ,comhis
endif
return
end

pro P_ICK_XY, ev, uv
;** ********
;**
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

if ev.select eq 1 then pk_stak(11+uv(2))=uv(3)
end
pro READ_LXY, pthfil,w=wi,status=pp2
;** ********
;**
@lamp.cbk
    common c_pick, pk_base,pk_path,pk_pthv,pk_flt ,pk_list,pk_idx,pk_sli,pk_frm,pk_ext,$
                   pk_hyst,pk_img ,pk_blis,pk_stak

head=0 & step=1 & mxi =2000 & m=1 & n=1 & f=1 & a=0 & b=0 & c=0 & d=0 & org=pk_stak(11) & pair=pk_stak(12)
ws  =strtrim(string(wi),2)  & strtit=['']

widget_control,bad_id=i,pk_stak(2) ,get_value=heas & on_ioerror,mish & head=long(heas(0))>0 & mish:
widget_control,bad_id=i,pk_stak(3) ,get_value=stes & on_ioerror,miss & step=long(stes(0))>1 & miss:
widget_control,bad_id=i,pk_stak(13),get_value=mxi  & on_ioerror,misx & mxi =long(mxi (0))>0 & misx:

widget_control,bad_id=i,pk_stak(4) ,get_value=ms   & on_ioerror,mism &    m=long(ms(0))>1   & mism:
widget_control,bad_id=i,pk_stak(5) ,get_value=ns   & on_ioerror,misn &    n=long(ns(0))>1   & misn:
widget_control,bad_id=i,pk_stak(6) ,get_value=fs   & on_ioerror,misf &    f=long(fs(0))>1   & misf:

widget_control,bad_id=i,pk_stak(7) ,get_value=as   & on_ioerror,misa &    a=long(as(0))>0   & misa:
widget_control,bad_id=i,pk_stak(8) ,get_value=bs   & on_ioerror,misb &    b=long(bs(0))>0   & misb:
widget_control,bad_id=i,pk_stak(9) ,get_value=cs   & on_ioerror,misc &    c=long(cs(0))>0   & misc:
widget_control,bad_id=i,pk_stak(10),get_value=ds   & on_ioerror,misd &    d=long(ds(0))>0   & misd:

ii=execute('READ_ORG, pthfil, w'+ws+',x'+ws+',y'+ws+',z'+ws+',e'+ws+', head,step,m,n,f,a,b,c,d' + $
                                    ',org,pair,mxi, status=pp2,ws=ws, strtit=strtit')
if ii eq 1 then begin nt=n_elements(strtit)
                      if nt gt 0 then w_tit(wi)    =strmid(strtit(0),0,80)
                      if nt gt 1 then x_tit(wi)    =strmid(strtit(1),0,80) else x_tit(wi)    ='X'    & y_tit(wi)='Y'
                      if nt gt 2 then other_tit(wi)=strmid(strtit(2),0,80) else other_tit(wi)=pthfil & endif
end
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************

pro	P_AFTER_REALIZE_DID ,sepben,sepdon,sepdid
;**	*******************
@lamp.cbk
	common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
;**
        common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

	common c_drow
    
    	oo=n_elements(did_wd)
    	if oo eq 0 then P_DID_SETVAR
    	if oo eq 0 then P_DON_INIT_VAR

	if did_inib(0) gt 0 then widget_control,bad_id=i,did_inib(0),set_button=1
	if did_inib(1) gt 0 then widget_control,bad_id=i,did_inib(1),set_button=1
	i =0
	if sepdon gt 0 then begin widget_control,bad_id=i,sepdon ,get_value=j
				  if i eq 0 then begin wset,j & erase,50 & endif & endif

	if sepdid gt 0 then begin widget_control,bad_id=i,sepdid ,get_value=j
				  if i eq 0 then begin wset,j & erase,50 & endif & endif

	if (did_wd gt 0) or (!D.name eq 'Z') then loadct  , tcol
	ii=1
	if  did_wd gt 0 then widget_control,bad_id=ii,did_wd  ,get_value =did_win0
	if  did_we gt 0 then widget_control,bad_id=ii,did_we  ,get_value =did_win1

;	   Load Icones
;	   ---- ------
			   logo,3 & logo,4
	   if ii eq 0 then logo,1

	if did_wd  gt 0 then begin   catch,stat
	   if stat eq 0 then device ,font=sys_dep      ('FONTD') else catch,/cancel
	endif
	if lamp_focus gt 0 then widget_control,bad_id=i,lamp_focus,set_value='',/append,/no_newline,$
										/input_focus
	tso=0 & did_scan=-1
	if (!D.flags and 65536) ne 0 then sl_lampscan, 'test' ,did_scan,tso

	did_surf(0)= (tso and 1)

;	Set history
;	--- -------
	set_history
	kcol=tcol
	catch,stat
	if stat eq 0 then DON_INIT_PROG_MAC  ,-1 else catch,/cancel
	
	if kcol ne tcol then if (did_wd gt 0) or (!D.name eq 'Z') then loadct, tcol
	catch,stat
	if stat eq 0 then begin
	 if (not sys_dep('RUNTIME')) and (not sys_dep('EMBEDDED')) then begin
	  if  b_labins(3) ne 2 then CALL_PROCEDURE,'myinit'
	  if  b_labins(3) ne 2 then CALL_PROCEDURE,'myinit_'+strlowcase(inst_value)
	 endif
	endif
	catch,stat
	if stat eq 0 then begin
         na=FINDFILE('myinit.prox',count=nn)
         if nn gt 0 then begin
          on_ioerror, misini & in=-1
	  OPENR,in,'myinit.prox',/get_lun
	  line=''
	  WHILE (not eof(in)) DO begin readf,in,line & iii=EXECUTE(line) & ENDWHILE
	  misini: if in gt 0 then FREE_LUN,in
	  CD,current=mee
	  P_SET_PATH,mee
	 endif
	endif else catch,/cancel

	styles(2,0)=1

	!order  = 0
	!x.style= 1  &  !y.style= 1  &  !z.style= 1
	!x.range= 0  &  !y.range= 0  &  !z.range= 0
	!p.font = 0
	!p.color= 255
	!p.background= 0
	!p.position=[0,0,0,0]
return
end

pro logo ,flg
;** ****
@lamp.cbk
	common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
			
	scan_dir= lamp_dir & if scan_dir ne "" then scan_dir= scan_dir + sys_dep('DIVIDER')

;	Load Image (0) (1)show
;	---- -----
	if flg lt 2 then begin
	   if n_elements(did_lamp) le 1 then begin
		if (!D.flags and 65536) ne 0 then begin
      		did_pix=widget_base  (title='',map=0)
      		did_pix=widget_draw  (did_pix,retain=2,xsize=512,ysize=256,colors=-30)
       		widget_control,did_pix,bad_id=i ,/realize
      		widget_control,did_pix,bad_id=i ,get_value=did_pix
		endif else did_pix=0
		u=-1
		on_ioerror,misimg
		gif='lampIMG.'
		if n_elements(GEORGE) eq 1   then if (GEORGE eq 1) then gif='geoIMG.'
		if sys_dep('VERSION') ge 5.4 then gif=gif+'png' else gif=gif+'gif'
		wor=!order

		if sys_dep('STUDENT') then did_lamp=bytscl(sin(bytscl(indgen(300,150))/10.)) $
		else begin
		   did_lamp=bytarr(512-1,256)
		   bid=findfile(scan_dir+gif,count=cnt)
		   if cnt gt 0  then begin
		    READ_KIF,scan_dir+gif,did_lamp
		   endif else begin
		    if n_elements(did_icon) lt 128 then LAMPICO,did_icon
		    did_lamp=smooth (rebin(did_icon,512,256),3)
		    mx=max(did_lamp)
		    did_lamp=mx-did_lamp
		   endelse
		endelse
		p_screen & mini=0
        	if n_elements(b_labins) ge 4 then if b_labins(3) eq 2 then mini=1
		if lamp_siz lt 800 then mini=1
		if mini then did_lamp=congrid(did_lamp,300,150)

                LAMPICO,idlicon,ldi=35

		if (!D.flags and 65536) ne 0 then begin wset,did_pix & tvscl,did_lamp & endif
		if (!D.name   eq 'Z') then  begin device,set_resolution=[512,256] & tvscl,did_lamp & endif
;		if (sys_dep('RUNTIME') or sys_dep('EMBEDDED')) then $
		               tvscl,idlicon,0,(size(did_lamp))(2) - (size(idlicon))(2)
		if  sys_dep('STUDENT') then xyouts,100,75,'STUDENT Version',/device,color=0
		if n_elements(GEORGE)  eq 1 then if GEORGE eq 3 then begin
			xyouts,5  ,25,'GEORGE inside' ,/device,color=57,font=0
			xyouts,6  ,24,'GEORGE inside' ,/device,color=54,font=0 & endif

		misimg:if u gt 0 then free_lun,u
		!order=wor
		did_lamp=size(did_lamp)
	   endif
	   if flg eq 1 then begin
		p_did_setwin0 & erase,0
		s=did_lamp(2)
		k= (did_x/32)>1
		j= (did_y-s) /2
		if n_elements(GEORGE) eq 1 then if did_pix gt 0 then begin
		for i=did_x-k,0 ,-k  do device,copy=[0,0,did_x-i,s,i,j,did_pix]
					device,copy=[0,0,did_x-i,s,k,j,did_pix]
		for i=k-1,0 ,-1	     do begin  wait,.01
					device,copy=[0,0,did_x-i,s,i,j,did_pix] & endfor
		endif
	   endif
	endif

;	Load Icone (0) (2)export
;	---- -----
	if (n_elements(did_pio) eq 0) then did_pio=0
	if (did_pio(0) eq 0)  and (flg ne 4) then begin

		if (!D.flags and 65536) ne 0 then begin
      		did_pio=widget_base  (title='',map=0)
      		did_pio=widget_draw  (did_pio,retain=2,xsize=128,ysize=64,colors=-30)
       		widget_control,did_pio,bad_id=i ,/realize
      		widget_control,did_pio,bad_id=i ,get_value=did_pio
		endif else did_pio=0
		u=-1
		on_ioerror,misicon
		if n_elements(did_icon) lt 128 then begin
		 did_icon=bytarr(128,64)
		 did_icon(*,*)=255
		 cnt=0 ;bid=findfile(scan_dir+'lampICO.jpg',count=cnt)
		 if cnt eq 0  then LAMPICO,did_icon  else $
		 READ_KIF,scan_dir+'lampICO.jpg',did_icon
		endif
		if (!D.flags and 65536) ne 0 then begin wset,did_pio & tvscl,did_icon,0,0 & endif
		misicon:if u gt 0 then free_lun,u
		did_icon(0,0)=max(did_icon)
	endif
	if  flg eq 0  then flg=did_pix  else $
	if  flg eq 2  then flg=did_icon else $
	if  flg eq 3  then begin  if n_elements(lamp_ben) ge 7 then if lamp_ben(6) gt 0 then begin
      			 	  widget_control, bad_id=ii ,lamp_ben(6),get_value=did_tio
				  did_pio=[did_pio,lamp_ben(6)] & endif
				  did_o=0
				  endif else $
	if (flg eq 3) or (flg eq 4) then if did_tio gt 0 then if did_o ne 1 then begin did_o=1 & keepw=!Window
					 wset,did_tio & device,copy=[0,0,128,64,0,0,did_pio(0)]
					 if keepw gt 0 then wset,keepw
	endif
return
end

pro put_logo, wid ,TIO=tio ,file=icofil
;** ********
;**
	common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
	common c_plog,  win,sicon,pixlog

	if !D.name ne 'Z' then begin
		if n_elements(wid) eq 1 then win=widget_Draw(wid,retain=2,xsize=64,ysize=32,/motion_event) $
		else begin
      		     widget_control,win,bad_id=i, get_value = wis
      		     widget_control,win,bad_id=i, set_uvalue=[-88,391,wis]
		     if n_elements(sicon) le 1   then begin
			if n_elements(did_icon) lt 128 then did_icon=bytarr(128,64)
			sicon=congrid(did_icon,64,32)
      			bid=widget_base  (title='',map=0)
      			bid=widget_draw  (bid,retain=2,xsize=64,ysize=64,colors=-30)
       			widget_control   ,bid,bad_id=i , /realize
      			widget_control   ,bid,bad_id=i , get_value=pixlog
			wset,pixlog  &    tvscl,sicon,0,0 & tvscl,sicon,0,32
		     endif
		     ok=0
		     if n_elements(icofil) eq 1   then begin
			bid=findfile(icofil,count=ok)
			if ok gt 0 then begin	bicon=bytarr(64,32) & READ_KIF,icofil,bicon
						bicon=congrid(bytscl(bicon),64,32)
						widget_control,win,bad_id=i,draw_motion_events=0
			endif
		     endif
		     keepw=!Window & wset,wis & if ok gt 0 then tvscl,bicon else tvscl,sicon
		     if keepw gt 0 then  wset,keepw
		     if keyword_set(tio) then did_tio=-wis
		endelse
	endif
end

pro p_did_mvlog, ev,uv
;** ***********
;**
	common c_plog,  win,sicon,pixlog

	if !D.name ne 'Z' then begin
	keepw=!Window    &  wset,uv(2)
	for i=2,32,2 do begin device,copy=[0,i,64,32,0,0,pixlog] & wait,.05 & endfor
	if keepw gt 0 then  wset,keepw
	endif
end

pro p_tremble
;** *********
;**
@lamp.cbk
	common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

if (lamp_b1 gt 0) and (n_elements(did_x) gt 0) and (!D.name ne 'Z') then begin
	if n_elements(did_buf) eq 0 then did_buf=0
	if did_buf eq 0  then begin

      		did_buf=widget_base  (title='',map=0)
      		did_buf=widget_draw  (did_buf,retain=2,xsize=did_x,ysize=did_y)
       		widget_control,did_buf,bad_id=i ,/realize
      		widget_control,did_buf,bad_id=i ,get_value=did_buf

	endif

	keepd=!D.window
	if keepd gt 0 then begin
	   wset , did_buf
	   device,copy= [0,0,did_x,did_y,0,0,did_win0]
	   k=did_y/20 &  wset,did_win0 & erase
	   for i=k,0,-1  do device,copy=[0,0,did_x,did_y-i,0,i,did_buf]
	   wset , keepd
	endif
endif
return
end

function RDSTOP, a,b,ic, win=wit
;******* ******
;**
	common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
	stp=0
	if (n_elements(wit) eq 4) or (did_tio gt 0) then begin
	   if n_elements(wit) ne 4 then wit=[did_pio(1),did_tio,128,64]

	   if  ic  ne b then p=round(abs(b-a+1)/64.)>1 else p=1
	   t=long(ic-a+1)  & s=t/p & s=s*p
	   if  a eq ic  then begin
	     if a ne b then begin
	   	keepw=!Window   & if !D.name ne 'Z' then wset,wit(1)
		widget_control,wit(0),bad_id=ii,/clear_events & erase,255
		xyouts,(wit(2)-64)>1,(wit(3)/2-17)>1,'STOP',font=-1,charsize=2.,charthick=3.,/device,color=0
		if keepw gt 0 then if !D.name ne 'Z' then wset,keepw
	     endif
	   endif else begin keepw=0
		evv=widget_event(wit(0),/nowait,bad_id=ii) & widget_control,/hourglass
	   	if evv.id eq wit(0) then begin
	   	   keepw=!Window   & if !D.name ne 'Z' then wset,wit(1)
		   stp=1
		   erase ,255
		   device,copy=[0,0,128,64,0,0,did_pio(0)]
		   widget_control,wit(0),bad_id=ii,/clear_events
		   P_MUS,'mus_cannon'
		endif else if s eq t then begin
	   	   keepw=!Window   & if !D.name ne 'Z' then wset,wit(1)
		   t=fix(128.*(ic-a+1)/(b-a+1))>1
		   t=long(t*wit(2)/128.)
		   device,copy=[(128-t)>0,0,t<128,64,(t-128)>0,(wit(3)/2-32)>0,did_pio(0)]
		endif
		if keepw gt 0 then if !D.name ne 'Z' then wset,keepw
	   endelse
	endif

return, stp
end

pro RDSTAP, a,b,ic, res
;** ******
res=RDSTOP( a,b,ic)
end

pro p_did_getw_cur, widx, wnumb
;** **************
;**
	common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

	if n_elements(did_wsc) eq 1 then begin
		widget_control,bad_id=i,did_wsc,get_value=wnumb
		if i eq 0 then wnumb=strupcase(strtrim(wnumb(0),2))
		i =strpos(wnumb,'W')
		wnumb=strtrim(strmid(wnumb,i+1,4),2)
		widx   =fix(wnumb)
	endif else begin widx=1 & wnumb='1' & endelse
return
end

function sl_zoom, x,y,xd,yd
;******* *******
;**
	common	my_geto,go_v7,go_v2,go_v3,go_rql,go_rqm,go_x5,go_y5
;**
;**	Return -1 for none, 1 for left, -2 for middle, -3 for right button.

	ok=0  & ok2=0
	;wiwi=(sys_dep('MACHINE') eq 'win')
	if n_elements(go_v7) eq 0 then begin
	   go_v7= lonarr(7) & go_v2= lonarr(2) & go_y5= intarr(5)
	   go_v3= intarr(3) & go_x5= intarr(5) & endif

	go_x5(0)=x & go_x5(3)=x & go_x5(4)=x
	go_y5(0)=y & go_y5(1)=y & go_y5(4)=y
	xd=x & yd=y
	xp=x & yp=y

	device,set_graphics =6

	but=1 & bat=1
	while (xd ge 0) and (but ne 0) do begin
	   bat=but
	   if  (xd ne xp) or (yd ne yp) then begin
		if ok eq 1 then begin
		    if bat eq 2 then plots   ,go_x5,go_y5,/device, color=255 $
		    		else polyfill,go_x5,go_y5,/device, color=255
		    ok=0 &  endif
		xp=xd & yp=yd
		if bat eq 2 then sz=15 else sz=15
		if (xd-x gt sz) or (xd-x lt -sz) and  $
		   (yd-y gt sz) or (yd-y lt -sz) then begin
		    go_x5(1)=xd  &  go_x5(2)=xd
		    go_y5(2)=yd  &  go_y5(3)=yd
		    if bat eq 2 then plots   ,go_x5,go_y5,/device, color=255 $
		    		else polyfill,go_x5,go_y5,/device, color=255
		    ok=1 & ok2=1
		endif
	   endif
	   cursor  ,xd,yd,0,/device
	   but=!err & if  but lt 0 then but=-but & if (but eq 1) or (but eq 4) then but=5-but
	  ;if wiwi then if but eq 1 then but=2
	endwhile

	if ok eq 1 then if bat eq 2 then plots   ,go_x5,go_y5,/device, color=255 else $
			if bat eq 4 then polyfill,go_x5,go_y5,/device, color=255 else $
			if xd  lt 0 then polyfill,go_x5,go_y5,/device, color=255
	if xd lt 0 then ok=-1
	if ok eq 0 then if ok2 eq 1 then ok=-1
	if ok eq 1 then if bat eq 1 then ok=-3 else $
			if bat eq 2 then ok=-2

	device,set_graphics =3

return, ok
end

pro did_zoom_reset
;** **************
;**
    common c_titi,viex,xxs,yys,wt,xo,yo,zn,xn,but,redo,ok,w_suf
viex=0
end

pro did_zoom ,event,uv
;** ********
;** Draw event (zoom turn)
;**
;**uv:	 2:base	 3:wi	 4:draw	 5:wind	 6:x0	 7:y0	 8:bstx	 9:bsty	10:rgx	11:rgx	12:rgy	13:rgy
;**	14:surf	15:!ord	16:axy	17:labl	18:xof	19:yof	20:log	(12=-1 ->vector)

;**	type 0:pressed	 1:released	 2:motion
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff
    common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current

    common c_titi,viex,xxs,yys,wt,xo,yo,zn,xn,but,redo,ok,w_suf

	if (!D.name ne 'Z') then $
	if (event.type eq 0) or (event.type eq 2) or (event.type eq 1) then begin
		x=event.x & y=event.y
		if (uv(14) eq 0) or (uv(12) lt 0) then begin	;no surf

		 ws =strtrim(string(uv(3)),2)
		 ssz=[0] & ii=execute( 'ssz=size(w'+ws+')' ) & if ssz(0) gt 2 then RETURN
		 if (trap_current ne uv(5)) and (uv(16) ne 1)  then  RETURN

		 if (event.type ne 1) then begin
		  bu=event.press & xd=0 & yd=0
		  wset,uv(5)
		  if (uv(17) eq l_message) and (did_tio gt 0) then oki=1 else oki=0
		  XV=0 & YV=0 & XG=0 & YG=0
		  if event.type eq 0 then begin if uv(12) lt 0 then begin
						  tmXYZ=CONVERT_COORD(x,y,/dev,/to_data)
						  XV=tmXYZ(0) & YV=tmXYZ(1) ;CURSOR,XV,YV ,/nowait,/data
						endif
						p_zoom, x,y,xd,yd,bb & wset,uv(5)
					  endif
		  if event.type eq 2 then begin   tmXYZ=CONVERT_COORD(x,y,/dev,/to_data)
						  XV=tmXYZ(0) & YV=tmXYZ(1) ;CURSOR,XV,YV ,/nowait,/data
						  bb=-9 & xd=x & yd=y & endif
;**		  Activate zoom
;**		  -------- ----
		  if (bb eq 1) or (bb eq -2) or (bb eq -3) or (bb eq -9) then begin ole=1
		  	vft=vfl
		  	xo =uv(6)
		  	xb =uv(18)
			vft(0)=round(uv(10) + (uv(11)-uv(10)) * (float((min([x,xd])-xo)) / (uv(8)-(xo+xb))) )
			flut  =      uv(10) + (uv(11)-uv(10)) * (float((max([x,xd])-xo)) / (uv(8)-(xo+xb)))
			if (vft(0) lt uv(10)) or (flut gt uv(11)) then ole=0
			vft(0)=vft(0)>0
			vft(1)=round(flut)
			if uv(16) eq 1 then begin XV=vft(0)-uv(10) & XG=vft(1)-uv(10) & endif
			vft(3)=0
			if  uv(12) ge 0 then begin
		  	 yo =uv(7)
		  	 yb =uv(19)
		  	 mx =max([y,yd]) & mn =min([y,yd])
		  	 if uv(15) eq 0 then begin i=uv(9)-mn & mn=uv(9)-mx & mx=i & endif $
		  		        else begin i=yo	      & yo=yb       & yb=i & endelse

			 vft(2)=round(uv(12) + (uv(13)-uv(12)) * (float((uv(9)-mx-yo)) / (uv(9)-(yo+yb))) )
			 flut  =      uv(12) + (uv(13)-uv(12)) * (float((uv(9)-mn-yo)) / (uv(9)-(yo+yb)))
			 if (vft(2) lt uv(12)) or (flut gt uv(13)) then ole=0
			 vft(2)=vft(2)>0
			 vft(3)=round(flut)
			 if uv(16) eq 1 then begin YV=vft(2)-uv(12) & YG=vft(3)-uv(12) & endif

			endif else if (trap_current eq uv(5)) and (uv(16) eq 0) then begin
				tmXYZ=CONVERT_COORD(xd,yd,/dev,/to_data)
				XG=tmXYZ(0) & YG=tmXYZ(1) ;CURSOR,XG,YG ,/nowait,/data
		    		if uv(20) eq 1 then begin YV=exp(YV) & YG=exp(YG) & endif
				XS =[0L] &  iii=execute( 'XS=n_elements(x'+ws+')-1' )
			      	ivf= 0L  &  iii=execute('ivf= where(X'+ws+' ge XV )') & ivf=ivf(0)>0<XS
			      	ivl= 0L  &  iii=execute('ivl= where(X'+ws+' ge XG )') & ivl=ivl(0)>0<XS
			      	if ivl  eq 0     then   ivl= uv(11)
				vft (0) =  min([ivf,ivl])   & vft (1) = max([ivf,ivl])
			endif	else begin XV=vft(0)-uv(10) & iii=execute('YV=w'+ws+'(XV)')
				           XG=vft(1)-uv(10) & iii=execute('YG=w'+ws+'(XG)')
			endelse
;**			Total
;**			-----
			if bb eq -3 then begin
			      trap_x1=vft(0) & trap_x2=vft(1) & trap_y1=vft(2) & trap_y2=vft(3)
			      trap_ws=ws
			      trapp  ,tot
			      avg= tot / ((vft(1)-vft(0))*(vft(3)-vft(2)))
			      tx1='  Total :  '   +strtrim(string(tot),2)
			      tx2=' Average : '   +strtrim(string(avg),2)
		     	      if oki eq 0 then   widget_control ,bad_id=i,uv(17),set_value=tx1+tx2 $
			      else begin wset,did_tio & erase,255 & xyouts,3,20, tx1,charsize=1.2,/device,color=120
						      	xyouts,3,3, tx2,charsize=1.2,/device,color=120 & endelse

;**			Motion
;**			------
			endif else if bb eq -9  then begin
			   if (uv(12) lt 0) and (ssz(2) ne 6) then if (YV lt w_min(uv(3))) or (YV gt w_max(uv(3))) then ole=0
			   if ole eq 1 then begin
			      IV=0
			      if uv(16) eq 1 then begin    XI=round(XV>0)+uv(10)
				    iii=execute('XV=X'+ws+'(XI)')
				    if uv(12) ge 0 then begin YI=round(YV>0)+uv(12)
		    				YS =[0] & ii=execute( 'YS=size(y'+ws+')' )
						if YS(0) eq 2 then begin XI=XI+ XI*YI & YI=XI & endif
						iii=execute('XV=X'+ws+'(XI)')
						iii=execute('YV=Y'+ws+'(YI)')
				    endif else  iii=execute('YV=W'+ws+'(XI)')

			      endif
			      tx1=' X='+strtrim(string(XV),2) & tx2=' Y='+strtrim(string(YV),2)
		     	      if oki eq 0 then   widget_control ,bad_id=i,uv(17),set_value=tx1+tx2 $
			      else begin did_o=0 & wset,did_tio & d =32
					 xd=(x+d*2-1) & if xd gt (did_x-1) then a=xd-(did_x-1)  else a=0
					 yd=(y+d-1)   & if yd gt (did_y-1) then b=yd-(did_y-1)  else b=0
					 xe=(x-d*2)   & if xe lt  0        then c=xe		else c=0
					 ye=(y-d-1)   & if ye lt  0        then d=ye		else d=0
					 xe=xe-a-c    & ye=ye-b-d & off=10
	   				 device,copy= [xe,ye,128,64 ,0,0,did_win0]
					 device,get_graphics=oldg,set_graphics=6
	   				 device,copy= [xe,ye,128,off,0,0,did_win0]
					 plots,[64,64]+a+c,[63,32+b+d],color=255,/device,psym=0
					 device,set_graphics=oldg
					 xyouts,2,1, tx1+tx2,/device,color=255
					 endelse
			   endif else LOGO,4
;**			Zoom
;**			----
			endif else begin
			      if (uv(12) lt 0) and (y lt yd) then begin vft(2)=0 & vff(1)=YG & endif
			      vfl= vft
			      uv =[-88,301,uv(2),uv(3)]
			endelse
;**		  Activate cursor position
;**		  -------- ------ --------
		  endif else begin
		    cursor,X,Y ,/nowait,/device
		    if (X ge 0) and (Y ge 0) then begin
		     xdv=x & ydv=y
		     XS =[0L] & ii=execute( 'XS=size(x'+ws+')' )
		        xo =uv(6)
		        yo =uv(7)
		  	xb =uv(18)
		  	yb =uv(19)
			XI =round( uv(10) + (uv(11)-uv(10)) * (float(X-xo) / (uv(8)-(xo+xb))) ) >0
			YI =0. & ii =execute( 'YI=w'+ws+'(XI)' )
;**		     For a vector
;**		     --- - ------
		     if (uv(12) lt 0) then begin
		    	if (trap_current eq uv(5)) and (uv(16) eq 0) then cursor,X,Y ,/nowait,/data $
		    	else begin	X=XI & Y=YI & endelse
		    	YC =Y
		  	D  =uv(10)
		  	YV =0. & XV =X
		  	IDX=[0L]
		  	if (XS(0) le 0) or (uv(16) eq 1) then begin
		  				X =round(X)>0
		  				if XS(0) gt 0 then ii=execute( 'XV=x'+ws+'(X)' ) $
		  				else XV=X
		  				ii=execute( 'YV =w'+ws+'(X)' )
		  	endif else begin 	ii=execute( 'IDX=where(x'+ws+' ge X)' )
		  		if ii eq 1 then ii=execute( 'X  =x'+ws+'(IDX(0))' )
		  		if ii eq 1 then ii=execute( 'YV =w'+ws+'(IDX(0))' )
		  		XV=X
		  	endelse
		  	if ii eq 1 then Y=YV
		  	Ym=w_min(uv(3)) & Ymx=w_max(uv(3))
		  	ii=execute( 'Ym =min(w'+ws+'(uv(10):uv(11)),max=Ymx)' )
		  	if uv(20) eq 1 then begin Ym=alog10(temporary(Ym) > 1E-9) & Y=alog10(temporary(Y) > 1E-9) & endif
			if (trap_current eq uv(5)) and (uv(16) eq 0) then begin
		  	   oplot,[X,X],[Ym,Y],linestyle=1,color=0
		  	   if bu ne 4 then $
		  	   xyouts,X,YC,'_x='+strtrim(string(XV),2),charsize=1.2,/data,color=0 else $
		  	   xyouts,X,YC,'_y='+strtrim(string(YV),2),charsize=1.2,/data,color=0
			endif else begin
			   XR =round( (XI-uv(10))*float((uv(8)-(xo+xb)))/(uv(11)-uv(10)) +xo ) >0
		    	   YR =round( (YV-Ym    )*float((uv(9)-(yo+yb)))/(Ymx   -Ym)     +yo ) >0
		  	   plots,[XR,XR],[yo,YR],linestyle=1,color=0,/device
		  	   if bu ne 4 then $
		  	   xyouts,XR,ydv,'_x='+strtrim(string(XV),2),charsize=1.2,/device,color=0 else $
		  	   xyouts,XR,ydv,'_y='+strtrim(string(YV),2),charsize=1.2,/device,color=0
			endelse
;**		     For an image
;**		     --- -- -----
		     endif else begin
		        if (uv(16) eq 0) then begin
					tmXYZ=CONVERT_COORD(x,y,/dev,/to_data)
					XG=tmXYZ(0) & YG=tmXYZ(1) & endif ;CURSOR,XG,YG ,/nowait,/data
		  	yo =uv(7)
		  	yb =uv(19)
		  	if uv(15) eq 0 then YI=Y else YI=uv(9)-Y
		  	if uv(15) eq 1 then begin i=yb & yb=yo & yo=i & endif
			YI =round (uv(12) + (uv(13)-uv(12)) * (float(YI-yo)/ (uv(9)-(yo+yb))) ) >0

			if (uv(16) eq 0) then begin  IDX=[XI] & IDY=[YI]
				if XS(0) eq 2 then ii=execute( 'IDX=where((x'+ws+' ge XG) and (y'+ws+' ge YG))')$
					      else ii=execute( 'IDX=where (x'+ws+' ge XG)' )
				if XS(0) ne 2 then ii=execute( 'IDY=where (y'+ws+' ge YG)' ) else IDY=IDX
				XI =IDX(0)>0
				YI =IDY(0)>0
			endif else if XS(0) eq 2 then begin XI=XI+ XI*YI & YI=XI & endif
			XV=XI & ii=execute( 'XV=x'+ws+'(XI)' )
			YV=YI & ii=execute( 'YV=y'+ws+'(YI)' )
			IV=0.
			if XS(0) eq 2   then ii=execute( 'IV =w'+ws +'(XI)' ) $
					else ii=execute( 'IV =w'+ws +'(XI,YI)' )
			tx1=' X='+strtrim(string(XV),2) & tx2=' Y=' +strtrim(string(YV),2)
							  tx3=' I=' +strtrim(string(IV),2)
			tx4=' in W'+ws+'('+strtrim(string(XI),2)+','+strtrim(string(YI),2)+')'

			;if trap_current ne uv(5) then begin tx1=' ' & tx2=' ' & tx3=' ' & endif
		     	if oki ne 0 then begin	wset,did_tio & erase,200
						xyouts,3,37, tx1,charsize=1.2,/device,color=50
					   	xyouts,3,20, tx2,charsize=1.2,/device,color=50 & did_o=0
					   	xyouts,3, 3, tx3,charsize=1.2,/device,color=50 & endif
			widget_control ,bad_id=i,uv(17),set_value=tx1+tx2+tx3+tx4
		     endelse ; End image and vector
		    endif    ; End X,Y >= 0
		  endelse    ; End cursor position
		 endif       ; End not type=1 (not release)
		endif else begin ;zoom_surf,event,uv, rx,rz,did_repr(4)
;Surface rotation
;****************
if n_elements(viex) eq 0 then  viex=0
if viex eq 0 then begin ok=0 & xxs=192 & yys=128
	bas=widget_base(map=0)
	pix=widget_draw(bas,xsize=xxs,ysize=yys,retain=2)
	widget_control, bas,/realize
	widget_control, pix, get_value=viex
	wt=dist(19)  &  wt(9,9)=wt(9,9)*2
	endif
wset,uv(5)
case event.type of
0:begin ;	button pressed
		but=event.press
		if but eq 1 then begin ok=1 & redo=0
			Xo=x & Yo=y   ;cursor, Xo,Yo ,/nowait,/device
			ws='Sna'+strtrim(string(uv(3)),2)   & wsz  =0L
			ii=execute('wsz=size('+ws+')') & w_suf=0L
			if (wsz(0) eq 2) or (wsz(wsz(0)+1) eq 8) then ii=execute('w_suf='+ws) ;2D or struc
			endif
		if but eq 2 then ok=0
		if but eq 4 then begin ok=0
			tmXYZ=CONVERT_COORD(x,y,/dev,/to_data)
			XV=tmXYZ(0) & YV=tmXYZ(1) & ZV=tmXYZ(2) ;CURSOR,XV,YV ,/nowait,/data
			endif
  end
1:begin ;	button released
		ok=0
		if but eq 1 then if redo then begin
			rx=long(xn) & rz=long(zn)
			if did_repr(4) gt 0 then widget_control,bad_id=i,did_repr(4),$
						       set_value=strtrim(string(rz),2)
			uv =[-88,301,uv(2),uv(3)]
		endif
  end
2:if ok then begin ;motion
		Xc=x & yc=y ;cursor,Xc,Yc ,/nowait,/device
		if (Xc ge 0) and (Yc ge 0) then begin
		  xt= (xc-xo)/1.5
		  yt=-(yc-yo)/1.5
		  if (xt ne 0) or (yt ne 0) then begin redo=1
			zn= (rz+xt)-long((rz+xt)/360)*360
			xn= (rx+yt)-long((rx+yt)/360)*360
			wset,viex & trap_current=viex
			if (size(w_suf))(0) gt 0 then begin
			   if (size(w_suf))(0) eq 2 then $
			           shade_surf,w_suf,az=zn,ax=xn,xstyle=4,ystyle=4,zstyle=4,subtitle='X--->' $
			   else begin ;->3D
			     if thresh ne w_suf.thresh then begin
				shade_volume,w_suf.w,thresh,vvv,ppp
				if n_elements(vvv) gt 1 then $
				w_suf={w:w_suf.w,thresh:thresh,v:vvv,p:ppp}
			     endif
			     if n_elements(w_suf.p) gt 3 then begin sz=size(w_suf.w)
				   scale3, xrange=[0,sz(1)-1], yrange=[0,sz(2)-1], zrange=[0,sz(3)-1],ax=0.,az=0.
				   t3d, tr=[-.5,-.5,-.5] ,rot=[ 0. , zn  , 0. ]
				   t3d,                   rot=[ xn , 0.  , 0. ]
				   t3d, tr=[+.5,+.5,+.5]
				   set_shading,reject=0 & tvscl,polyshade(w_suf.v,w_suf.p,/t3d) & set_shading,reject=1

				   if sys_dep('VERSION') lt 4.0 then begin xtit='x' & ytit='y'
				   endif                        else begin xtit='scan' & ytit='x' & endelse

				   surface,w_suf.v,xrange=[0,sz(1)-1], yrange=[0,sz(2)-1], zrange=[0,sz(3)-1],$
					   az=zn,ax=xn,xticks=1,yticks=1,zticks=1,/nodata,/noerase,xtitle=xtit,ytitle=ytit,$
					   title='rx:'+strtrim(round(xn),2)+' rz:'+strtrim(round(zn),2)
			     endif
			   endelse
			endif else shade_surf,wt   ,az=zn,ax=xn,xstyle=4,ystyle=4,zstyle=4,subtitle='X--->'
			wset,uv(5)
			DEVICE,copy=[0,0,xxs,yys,xo<(uv(8)-xxs),yo<(uv(9)-yys),viex]
		  endif
		endif
  endif
else:
endcase
		endelse
	endif
return
end
;*************************************** Process Events **********************************
;*************************************** Process Events **********************************
;*************************************** Process Events **********************************
pro P_DID_EVENT	,event,uv
;** ***********
;**
;** Event parser.
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff
    common c_drow

    if uv(1) eq 390 then did_zoom ,event,uv

;** Are there enought LUN left !!!!
;** -------------------------------
    fee=0 & catch,stat
    if stat ne 0  then fee=128 else begin get_lun,fee & free_lun,fee & endelse
    catch,/cancel & if fee ge 120 then for i=1,120 do   free_lun,i

    on_ioerror,mis
    on_error  ,1
    ab=1

    if uv(0) eq -87 then begin
;**	Scan event
;**	---- -----
	if uv(1) eq 21 then begin
		windn=-1
		sl_lampscan, 'w_to_wind' , uv(2) , windn
		if windn ge 0 then begin
		   uv(1)=305
		   wintb(0)=-1
		   index=where(wintb eq windn , i )
		   if i ge 1 then scanum='w'+strtrim(string(index(0)),2) else begin scanum='w0'
		   							 w0=[0,0]
		   							 wintb(0)=  windn
		   							 wtb(0)=1 & endelse
		   endif
	endif else uv(1)=0

    endif

    caze=uv(1)
    if caze  eq 305 then caze=301
    if caze  eq 306 then caze=301
    if caze  eq 308 then begin ;**Tlb_size_events
		 widget_control,bad_id=ii ,event.id ,tlb_get_size=v2 
		 if (abs(uv(7)-v2(0)) gt 50) or (abs(uv(8)-v2(1)) gt 50) then begin
		 				 caze=301
						 uv=[-88,301,uv(2),uv(13)]  & endif
    endif


    case caze of

;**	Disactivate scan
;**	----------- ----
	300:begin
		sl_lampscan, 'send_event',-88
		p_did_setwin0
		set_xy
		!p.font = 0
	    end

;**	Display a workspace
;**	------- - ---------
	301:begin
		ab=0 & i=0
		wnumber='W'
		if (uv(1) eq 301) or (uv(1) eq 306)  then $
			if uv(3) eq -1 then widget_control,bad_id=i,did_wsc ,get_value=wnumber else $
			if uv(3) eq  0 then widget_control,bad_id=i,event.id,get_value=wnumber $
				       else wnumber='W'+strtrim(string(uv(3)),2)
		if uv(1) eq 305 then 	    wnumber=scanum
		if i eq 0 then wnumber=strupcase(strtrim(wnumber(0),2))
		i =strpos(wnumber,'W')
		wnumber=strtrim(strmid(wnumber,i+1,4),2)
		wi=fix(wnumber)
		if (wi ge 0) and (wi le 20) then begin
		    s =[0L,0L]
		    bb=execute( 's=size(w' + wnumber + ')' ) ;+newtypes*****
		    if  bb eq 1 then if (s(0) ge 1) and (s(0) le 3) and $
			                 ((s(s(0)+1) le 6) or (s(s(0)+1) ge 12) or (s(s(0)+1) eq 9)) then begin
			ab=1
			turn=0
			fu_out  =did_fu
			if (uv(1) eq 301) and (uv(2) eq  0 ) then p_did_makeSnap,wi
			if (uv(1) eq 305)  or (uv(1) eq 306) then fu_out=2 $
				      else if  uv(2) ne 0 then begin fu_out=1 & turn=uv(2) & endif
			if fu_out eq 2  then begin
			 if (lamp_act eq 0) then begin
			  if (did_scan eq -1) then begin SL_SCANLOAD,did_scan,lamp_dir
			                                 if (did_scan ge 0) then loadct,tcol & endif
			  if (did_scan ge  0) then begin
;			   call scan
;			   ---- ----
			   lamp_act=1
			   data='w' + wnumber
			   flg='scan'
			   pp2=wintb(wi)
			   pp3=wtb  (wi)
			   pp4=0
			   while pp4 ge 0 do begin
			     pp4=-1
			     sl_lampscan, 'set_size', 0,0,0, lamp_b1 ,wi

			     stat=0 & i=1
      			     catch,stat
      			     if (stat eq 0) and (i eq 1) then begin
      			         pp5=[w_tit(wi),x_tit(wi),y_tit(wi),z_tit(wi),other_tit(wi)]

			      if s(s(0)+1) le 6 then $ ;+newtypes*******
				 i =execute( 'sl_lampscan, flg ,'      +data+ ',pp2,pp3,pp4,pp5 ' ) else $
				 i =execute( 'sl_lampscan, flg ,float('+data+'),pp2,pp3,pp4,pp5 ' )

		    		 bb=execute( 's=size('+data+')' )
		    		 if s(0) eq 2 then if s(2) eq 1 then bb=execute(data+'=total('+data+',2)')
				 w_tit(wi)=strtrim(pp5(0),2) & x_tit(wi)=strtrim(pp5(1),2)
				 y_tit(wi)=strtrim(pp5(2),2) & z_tit(wi)=strtrim(pp5(3),2)
				 other_tit(wi)=strtrim(pp5(4),2)
			     endif
      			     if  i  ne 1 or stat ne 0 then begin
      			             catch,/cancel
			   	     if pp2 ge 0 then sl_lampscan,'purge',pp2
      			             pp2= -1
      			             pp4= -1
      		     		     print,string(7b),!err_string
			     endif
			     if wi gt 0 then begin
			        to_don_history, wi , -1 , ''
			        wintb(wi)=pp2
			        wtb  (wi)=1
			     endif
			     if pp4 ge 0 then begin
			   		pp2=pp4 & pp3=1 & wi=0 & w0='[0,0]' & data='w0'
			   		index=where(wintb eq pp2 , i )
			   		if i ge 1 then begin wi=index(0)
			   				     pp3=wtb(wi)
							     data='w'+ strtrim(string(wi),2)
							     endif
			     endif
			   endwhile
			   !p.font = 0
			   set_xy
			   p_did_setwin0
			 endif &   endif
			endif else begin
;			   call below or beside views
;			   ---- ----- -- ------ -----
				string_w=wnumber
				vff(2)=0
				p_did_drawidl,turn, string_w, s
				if did_repr(0) eq 1 then txt=' ,/image'    else $
				if did_repr(1) eq 1 then txt=' ,/contour'  else $
				if did_repr(2) eq 1 then txt=' ,/surface'  else txt=' '
				if did_fu      eq 0 then txt=txt+',/below' else txt=txt+',/beside'
				to_don_history,-1,0,'SEE ,w='+string_w    +txt
			endelse
			lamp_act=0
		    endif else if did_fu eq 0 then logo,1
		endif
	    end
;**	Below button
;**	----- ------
	302:begin
		did_fu =0
		p_did_setwin0
	    end
;**	Beside button
;**	------ ------
	303:begin
		did_fu = event.select
		p_did_setwin0
	    end

;**	Need tea or coffee!
;**	------------------
	304:begin l_mess=l_message
		pth=sys_dep('NEWSUB',lamp_dir,'lamp_mac')
		if b_labins(6) then if b_labins(7) gt 0 then l_mess=b_labins(7)
		if uv(2) eq  2  then wi=21 else wi=0
		if uv(2) eq  2  then READ_LAMP,'teapot.hdf',w=wi, path=pth
		txt='liv_objet,wdraw=did_we,dim=[did_x,did_y],menubas=did_4dM,alter=did_wb,l_mess=l_mess,mytem=uv(2),wi=wi'
		if uv(2) eq  2  then WIDGET_CONTROL,event.id,set_value='Need Coffee!',set_uvalue=[-88,304,5,0]
		if uv(2) eq  5  then begin
		  t = findfile (pth + 'teapot.hdf',count =exist)
		  if exist gt 0 then WIDGET_CONTROL,event.id,set_value='Have Tea!'   ,set_uvalue=[-88,304,2,0]
		endif
		ii =EXECUTE(txt)
	    end

;**	Menu-bar
;**	---- ---
	307:case uv(2) of
		11: CALIBRATION
		12: begin SAVESESSION & p_did_event,0,[-88,398,0]    & end
		61: begin widget_control,bad_id=ii,lamp_b1    ,scr_xsize=uv(5)
		          widget_control,bad_id=ii,lamp_ben(0),map=1
			  widget_control,bad_id=ii,lamp_b1    ,scr_ysize=uv(6)
			  widget_control,bad_id=ii,lamp_don(0),map=1
			  widget_control,bad_id=ii,uv(7)      ,sensitive=0
			  widget_control,bad_id=ii,uv(8)      ,sensitive=1
			  b_labins(6)=0
			  end
		62: begin widget_control,bad_id=ii,lamp_don(0),map=0
			  widget_control,bad_id=ii,lamp_b1    ,scr_ysize=uv(4)
		          widget_control,bad_id=ii,lamp_ben(0),map=0
			  widget_control,bad_id=ii,lamp_b1    ,scr_xsize=uv(3)
			  widget_control,bad_id=ii,uv(7)      ,sensitive=1
			  widget_control,bad_id=ii,uv(8)      ,sensitive=0
			  b_labins(6)=1
			  end
		else:
	    endcase
	    
;**	Decrement Wsc
;**	--------- ---
	310:	begin
		 widget_control,bad_id=i,uv(2),get_value=wnumber & wnumber=wnumber(0)
		 i =strpos(wnumber,'W')
		 if i gt 0 then wf=strmid(wnumber,0,i) else wf=''
		 wn=strtrim(strmid(wnumber,i+1,4),2)
		 wi=fix(wn)-1
		 if (wi lt  1) then wi=20
		 if (wi gt 20) then wi=1
		 wnumber=strtrim(string(wi),2)
		 wn=wf+'W'+wnumber
		 if wi le 9 then wn=wn+' '
;		 if uv(3) eq 1 then wn='Plot '+wn
;		 if uv(3) eq 2 then wn='LOAD '+wn
;		 if uv(3) eq 3 then wn='SCAN '+wn
		 widget_control,bad_id=i,uv(2),set_value=wn
		 p_did_makeSnap,wi
		 if b_labins(6) then if b_labins(7) gt 0 then widget_control,bad_id=iii,b_labins(7),set_value=HIS(wi)
 		end
;**	Increment Wsc
;**	--------- ---
	311:	begin
		 widget_control,bad_id=i,uv(2),get_value=wnumber & wnumber=wnumber(0)
		 i =strpos(wnumber,'W')
		 if i gt 0 then wf=strmid(wnumber,0,i) else wf=''
		 wn=strtrim(strmid(wnumber,i+1,4),2)
		 wi=fix(wn)+1
		 if (wi lt  1) then wi=20
		 if (wi gt 20) then wi=1
		 wnumber=strtrim(string(wi),2)
		 wn=wf+'W'+wnumber
		 if wi le 9 then wn=wn+' '
		 widget_control,bad_id=i,uv(2),set_value=wn
		 p_did_makeSnap,wi
		 if b_labins(6) then if b_labins(7) gt 0 then widget_control,bad_id=iii,b_labins(7),set_value=HIS(wi)
		end
;**	Button  Raw
;**	------  ---
	312:	begin	monimon=-(event.select)
			if event.select then txt='/raw' else txt='/noraw'
			to_don_history,-1,0,'RDSET,'+txt
		end

;**	Button  More...
;**	------  -------
	319:	begin
		if uv(4) gt 0 then P_DID_CREATE_MORE, uv(2),uv(3),uv(4) $
		else widget_control,bad_id=i,uv(3) ,map=1
		widget_control,bad_id=i,uv(2) ,set_value='Regular Grid',set_uvalue=0
		widget_control,bad_id=i,uv(2) ,set_button=0,set_uvalue=[-88,324,0]
		end

;**	Button  Image
;**	------  -----
	320:	did_repr(0) =event.select
;**	Button  Contour
;**	------  -------
	321:	did_repr(1) =event.select
;**	Button  Surface
;**	------  -------
	322:	did_repr(2) =event.select

;**	Button  Log aspect
;**	------  ----------
	323:	did_repr(5) =event.select
;**	Button  Use Xi,Yi
;**	------  --- -----
	324:	did_repr(6) =event.select
;**	Button  X Stretch
;**	------  - -------
	325:	did_repr(7) =event.select
;**	Button  Y Stretch
;**	------  - -------
	329:	did_repr(8) =event.select

;**	Button  S Stretch
;**	------  - -------
	318:	did_repr(17)=event.select

;**	Button  Turn -angle
;**	------  -----------
	326:	p_did_drawidl, -1, 0,0
;**	Button  Turn +angle
;**	------  -----------
	327:	p_did_drawidl,  1, 0,0

;**	Button  Maxi value
;**	------  ---- -----
	328:	begin
		did_repr(9)=event.select
		if did_repr(9) eq 1 then did_repr(9)=uv(2)
		end

;**	Scale text imply set button
;**	----- ---- ----- --- ------
	330:	begin
		if did_repr(uv(3)) eq 0 then if event.type lt 3 then begin
			widget_control,bad_id=i,uv(2),set_button=1
			did_repr(uv(3))=uv(4)
		endif
		end


;**	Touch Base
;**	**********
	331:	begin p_set_font,1 & lamp_siz=lamp_siz<800
		      TOUCH_B   ,1 ,inst_value
		      p_set_font,0
		end
;**	Touch_base get catalog
;**	---------- --- -------
	332:	touch_list, event,uv
;**	Touch_base select experiment
;**	---------- ------ ----------
	333:	touch_exper,uv,event.index
;**	Touch_base select run
;**	---------- ------ ---
	334:	touch_run,  uv,event.value ,event.drag
;**	Touch_base representation
;**	---------- --------------
	335:	touch_mode, uv
;**	Touch_base done
;**	---------- ----
	336:	touch_done, event ,uv
;**	Touch_base done
;**	---------- ----
	337:	touch_restore, uv
;**	Touch_base color , draw_event ...
;**	---------- -----   ----------
	338:	touch_more, uv,event

;**	didline
;**	-------
	340:	pho_event, event ,uv
;**	inx
;**	---
	341:	inx_event, event ,uv

;**	342:    Do not use
;**	343:    Do not use

;**	Background
;**	----------
	344:    begin if event.select eq 1 then begin tvlct,255,255,255,0  & tvlct,0,0,0,255
	              endif		   else begin tvlct,0,0,0,0        & tvlct,255,255,255,255 & endelse
		end

;**	Print format
;**	------------
	345:    widget_control,bad_id=i,uv(3),set_uvalue=uv(2)

;**	Annotate
;**	--------
	346:	begin keepfont=!p.font & !p.font=-1 & if !D.name ne 'Z' then wset,uv(2)
		      ANNOTATE,COLOR_INDICES=shift(indgen(10)*22,5)
		     ;trap_current=uv(2)
		      that=lamp_don(0) & if b_labins(8) gt 0 then that=[that,b_labins(8)]
		      widget_control,bad_id=i,that,/clear_events
		      !p.font=keepfont & end

;**	Load colors
;**	---- ------
	347:	begin
		i=xregistered('xloadct')
		if i lt 1 then xloadct,group=lamp_b1,/use_current
		end

;**	Remove_event
;**	------------
	349:	begin wait,.3 & widget_control,bad_id=i,uv(2),/destroy & end

;**	Print_event
;**	-----------
	350:begin l_mess=l_message
		if b_labins(6) then if b_labins(7) gt 0 then l_mess=b_labins(7)

		keep_w  =!D.window
		keep_d  =!D.name
		keep_o  =!order
		keep_u  =did_fu
		modop   =1
		err	=1
		if (uv(3) eq 0) and (uv(4) gt 0) then begin
		           widx=uv(4) & wnumber=strtrim(string(widx),2) & modop=uv(6)>1
		           if modop eq 1 then out_file='lamp_w' +wnumber + '_cp.ps' $
		                         else out_file='lamp_w' +wnumber + '_cp.gif'
		endif else begin   p_did_getw_cur, widx, wnumber
		           l='_' & if did_repr(0) eq 1 then l=l+'i'
		                   if did_repr(1) eq 1 then l=l+'c'
		                   if did_repr(2) eq 1 then l=l+'s'
		           out_file='lamp_w' +wnumber +l+ '.ps'
			   ;Next line in comment means print screen
			   ;***************************************
			   modop=3  & w_numor(0)=out_file
			   if uv(3) le 0 then begin did_fu=0 & uv(6)=0 & uv(4)=widx & endif
			   endelse
		if !D.name ne 'Z' then $
		if uv(2)  gt 0 then wset,uv(2) else p_did_setwin0
		if uv(3)  gt 0 then begin widget_control,bad_id=i,uv(3),get_value= out_file
					  out_file=strtrim(out_file(0),2)
					  id      =strpos (out_file,'_h.')
					  if id lt 0 then id=strpos (out_file,'.')
					  if id gt 0 then  out_file=strmid (out_file,0,id)
					  widx    =uv(4)
					  if sys_dep('VERSION') ge 5.4 then kif='.png' else kif='.gif'
					  widget_control,bad_id=i,uv(7),get_uvalue=modop
					  if modop eq 1 then out_file = out_file+'_h.ps'
					  if modop eq 2 then out_file = out_file+  kif
					  if modop eq 3 then out_file = out_file+'.ps'
					  w_numor(0) =  out_file
					  did_fu=1
					  widget_control,bad_id=i,uv(3),set_value= out_file
				    endif
		if  (modop ne 3) and (uv(2) lt 0) then begin ;An object for GIF or Screen copy
		  if modop eq 2 then ps='GIF' else ps='SCREEN'
		  txt='liv_objet,WDRAW=-uv(2),PS=ps'
		  ii=EXECUTE(txt)
		endif else if modop ne 3 then begin
		  !order=0 & r=0
		  w0=tvrdd(r,g,b)
		  s =size(w0)
		  on_ioerror,misps_open

		  if modop eq 2 then begin if n_elements(r) le 1 then tvlct,r,g,b,/get
		  	WRITE_KIF,out_file,w0,r,g,b & err=0
			if l_mess gt 0 then widget_control,bad_id=i,l_mess  ,set_value= out_file+' updated ...'
		  endif
		  if modop eq 1 then begin

		    tvlct, cur_r, cur_g, cur_b, /get
		    if n_elements(r) le 1 then begin r=cur_r & g=cur_g & b=cur_b & endif
		    set_plot,'PS'
		    device,filename=out_file,bits_per_pixel=8,/color

		    pos_r=bytarr(256) & pos_g=bytarr(256) & pos_b=bytarr(256)
		    pos_r(0)=r    & pos_g(0)=g    & pos_b(0)=b
		    tvlct ,  pos_r    ,          pos_g    ,          pos_b
		    covec=indgen(n_elements(cur_r))

			 sx  =7.21 & sy=10.6 & bpi=300. & pi=90 & fx=1. & fy=1.
			 bord=0.5
			 lup =0.3
			 if s(1) le s(2) then begin
			    ix=float(s(1))/pi & iy=float(s(2))/pi
			 endif else begin
			    ix=float(s(2))/pi & iy=float(s(1))/pi & endelse
			 if ix gt sx then fx=sx/ix & if iy gt sy then fy=sy/iy
			 if fy lt fx then fx=fy    & ix=ix*fx  & iy=iy*fx

		    on_ioerror,misps_write

		    xo  =(sx-ix)/2 & yo=(sy-iy)/2

		    if s(1)  le s(2) then begin
			 if yo lt bord then yo=bord & if yo lt 0.8 then lup=0.1
			 if xo lt bord then xo=bord
			 device,/portrait       ,/inches,xoffset=xo       ,yoffset=yo
			 tv,w0      ,0    ,0        ,xsize=ix,ysize=iy    ,/inches
			 tvlct, cur_r, cur_g, cur_b
			 if uv(5) gt 1 then $
			 tv,covec   ,ix-1.,iy+lup   ,xsize=1.,ysize=0.25  ,/inches
			 p_did_ps_header, iy+lup , widx ,out_file
		    endif else begin
			 if xo lt bord then xo=bord & if xo lt 0.8 then lup=0.1
			 device,/landscape      ,/inches,xoffset=xo       ,yoffset=sy-yo
			 tv,w0      ,0    ,0        ,xsize=iy,ysize=ix    ,/inches
			 tvlct, cur_r, cur_g, cur_b
			 if uv(5) gt 1 then $
			 tv,covec   ,iy-1.,ix+lup   ,xsize=1.,ysize=0.25  ,/inches
			 p_did_ps_header, ix+lup , widx ,out_file
		    endelse
		    if l_mess gt 0 then widget_control,bad_id=i,l_mess  ,set_value= out_file+' updated ...'
		    err=0
		    misps_write: if err eq 1 then device,/close_file
		    set_plot,keep_d
		    tvlct ,  cur_r    ,          cur_g    ,          cur_b
		  endif

		  misps_open:   set_plot,keep_d
		  !order  =keep_o
		  if keep_w ge 0  then if !D.name ne 'Z' then wset,keep_w
		  if err    eq 1  then begin
				widget_control,bad_id=i,l_mess  ,set_value='Print write error !!!'
				P_MUS,'mus_cannon' & endif
		endif else begin
		;** POSTSCRIPT REPLOT
		  if uv(8) eq 0 then uv(8)=did_wd
		  widget_control,bad_id=ii,uv(8),get_uvalue=lov
		  if ii eq 0 then begin
		  ;** RESTORE ZOOM... LIMITS
			;lov: [-88,390, b_cur, fix(string_w), d_cur, w_cur, xo, yo,$
			;	bst_x, bst_y, vfl(0), vfl(1),vfl(2),vfl(3), isurf ,$
			;	w_order, axy, baslb, xof, yof, did_repr(5)]
		  	vfl(0)=lov(10:13)
		  endif	  
		  styles(3,0)=1
		  p_did_event,0,[-88,301,uv(6),uv(4)]
		endelse
		did_fu=keep_u
	    end

;**	Multi_plot create
;**	---------- ------
	352:begin
		p_did_getw_cur, widx, wnumber
		suprplot, widx
	    end
;**	Multi_plot event (353:slider_w_x_y 354:slider_range 355:keep etc. 356:buttons)
;**	---------- -----
	353:	p_rom_super_event, event,uv
	354:	p_rom_super_event, event,uv
	355:	p_rom_super_event, event,uv
	356:	p_rom_super_event, event,uv

;**	TRIPX
;**	-----
	357:	ii=execute('tripx_event, event,uv')
	358:	ii=execute('tripx')

;**	TOMOGRAPHY
;**	----------
	359:    tomo_event_parser, event,uv

;**	Begood button
;**	------ ------
	360:begin p_did_getw_cur, widx, wnumber
		  p_set_font,1
		  p_did_create_begood, widx ,rx ,nlv ,smoo ,styles ,did_repr
		  p_set_font,0
		  widget_control,bad_id=i,event.id,set_button=0
	    end
;**	Begood updat
;**	------ -----
	361:	p_did_begood_updat
;**	Begood updat
;**	------ -----
	362:	p_did_begood_slide,event
;**	Begood done
;**	------ ----
	363:	p_did_begood_done, rx ,nlv
;**	Begood device for PS
;**	------ ------ --- --
	364:	p_did_begood_devps
;**	Begood view angle
;**	------ ---- -----
	365:	if uv(2) eq 0 then p_did_begood_ax, rx else p_did_begood_nlv, nlv
;**	Begood Surface style
;**	------ ------- -----
	366:	if uv(2) le 6 then styles(0,0)=uv(2) $
		else begin
		     if uv(2) eq 7 then if   event.select eq 1 then !P.psym=10 else !P.psym=0
		     if uv(2) eq 8 then smoo=event.select
		     if uv(2) eq 9 then did_repr(14)=event.select
		endelse
;**	Begood Contour style
;**	------ ------- -----
	367:	styles(1,0)=uv(2)
;**	Begood Scan preference
;**	------ ---- ----------
	368:	styles(uv(2),1)=event.select
;**	Begood Projection style
;**	------ ---------- -----
	369:	if event.select eq 1 then styles(2,0)=uv(2) else styles(2,0)=1

;**	Save workspace
;**	---- ---------
	370:begin
		p_did_getw_cur, widx, wnumber
		p_did_save_menu,widx
	    end
	371:p_did_save_list,event
	372:p_did_save_format,uv(2)
	373:p_did_save_filename,event
	374:p_did_save_work,event,uv ,0

;**	Diag UI.
;**	---- --
	378:wdiag_event, event,uv

;**	Phil filter UI.
;**	---- ------ --
	379:P_FIL_EVENT ,event,uv

;**	Restore workspace create widget
;**	------- --------- ------ ------
	380:P_ICK_INIT,0
;**	Restore workspace change path
;**	------- --------- ------ ----
	381:P_ICK_PTH,uv
;**	Restore workspace select
;**	------- --------- ------
	382:P_ICK_LST,event,uv
;**	Restore done button
;**	------- ---- ------
	384:	widget_control,bad_id=i,event.top,map=0
;**	Restore workspace remove
;**	------- --------- ------
	385:P_ICK_DEL ,uv
;**	Restore workspace change format
;**	------- --------- ------ ------
	386:P_ICK_FRM ,event,uv
;**	Restore XY change format
;**	------- --------- ------
	387:P_ICK_XY  ,event,uv
;**	Restore workspace read
;**	------- --------- ----
	383:begin P_ICK_GET,pth,fname,frm ,info ,uv
	      if fname ne '' then begin NXed=''
		     widget_control,bad_id=i,uv(8)   ,get_uvalue=NXed
		     widget_control,bad_id=i,uv(4)   ,get_value =iw
		     lamp_wrd='W'+strtrim(string(iw),2)
		     p_did_before_read, wnumber,uv(3)
		     pp2=-1 & hyst=''
		     comhis=''
;		Workspaces
		     if frm eq '_LAMP'  then begin
			comhis='READ_LAMP,"'+pth+fname+'",w='+wnumber
		        p_did_restore_wrk, fname,pth,wnumber,hyst,pp2
		     endif
;		NeXus
		     if (frm eq '.hdf') or (frm eq '.xml') then begin
			comhis='READ_LAMP,"'+pth+fname+NXed+'",w='+wnumber
		        p_did_res_hdf    , fname+NXed,pth,wnumber,hyst,pp2
		     endif
;		XY ascii
		     if frm eq '*.*'  then begin
			comhis='READ_LXY,"'+pth+fname+'",w='+wnumber
		        read_lxy, pth+fname,w=wnumber,status=pp2
		     endif
;		.gel
		     if frm eq '.gel'   then begin
			flg='.gel'
			comhis='sl_lampscan,"'+flg+'",w'+wnumber+'-1,0,0,"'+pth+fname+'"'
			i =execute( 'sl_lampscan, flg ,w' + wnumber + ',pp2,0,0, pth+fname' )
		     endif
;		.image
		     if frm eq '.image' then begin
			flg='.image'
			comhis='sl_lampscan,"'+flg+'",w'+wnumber+'-1,0,0,"'+pth+fname+'"'
			i =execute( 'sl_lampscan, flg ,w' + wnumber + ',pp2,0,0, pth+fname' )
		     endif
;		.SCAN
		     if frm eq '.WIND'  then begin
			flg='restore'
			comhis='sl_lampscan,"'+flg+'",w'+wnumber+'-1,0,0,"'+pth+fname+'"'
			i =execute( 'sl_lampscan, flg ,w' + wnumber + ',pp2,0,0, pth+fname' )
		     endif
;		.PICT
		     if frm eq '.PICT'  then begin
			colr=0 & colg=0 & colb=0
			comhis='READ_PICT,'+pth+fname+',w'+wnumber+'r,g,b'
			i =execute( 'READ_PICT, pth+fname, w' + wnumber + ', colr,colg,colb')
			tvlct,colr,colg,colb
		     endif
;		DIAL
		     if frm eq 'dial_*.pro*' then begin
			dname=strmid(fname,5,strpos(fname,'.')-5)
			comhis='DialInit, "'+dname+'", d='+wnumber+', path="'+pth+'"'
			i =execute( comhis )
			if i then pp2=0
		     endif
;		Others
		     if (frm eq '*') or (frm eq '.*')  then $
			    P_ICK_SCAN     , pp2,frm,wnumber,uv,hyst,fname ,pth $

		     else   p_did_just_read, pp2,frm,wnumber,uv,hyst,fname ,info ,comhis
	      endif
	    end

;**	LOGO event
;**	**********
	391:	  p_did_mvlog,  event,uv

;**	Calibration
;**	***********
	394:	  p_did_calev,  event,uv

;**	Journal
;**	*******
	395:	  p_did_journal_print,uv
	396:begin p_set_font,1
		  p_did_journal,event,uv
		  p_set_font,0 & end
;**	Save all
;**	********
	397:p_did_save_session

;**	Exit
;**	****
	398:begin
;	    DON_WRITE_PROG_MAC ,0
	    if lamp_b1 gt 0 then begin
	       if sys_dep ('MAP') gt 1 then if lamp_siz gt 1000 then begin
		  widget_control,bad_id=i,lamp_don(0),/destroy
		  widget_control,bad_id=i,lamp_ben(0),/destroy
		  widget_control,bad_id=i,lamp_ben(9),/destroy
		  LOGO,1 & endif
	       P_MUS,'mus_cannon'                       & l_message=0
	       widget_control,bad_id=i,lamp_b1,/destroy & lamp_b1  =0

;**	       In case P_DYING did'nt destroyed all the group at time.
	       rout=[0]
	       if  sys_dep('VERSION') ge 5.1 then ii=execute('rout=widget_info(/managed)')
	       if  rout(0)  gt 0 then exok=0 else exok=1
	       if (exok) then EXIT
;	       widget_control,/reset
	    endif else exit
	    end

;**	Destroy
;**	*******
	399:begin wait,.3 & widget_control,bad_id=i,event.top,/destroy & end

	else:
	endcase

mis:
;if ab eq 0 then print,string(7b)

return
end

pro p_did_before_read, wnumber,laber
;** *****************
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

		wnumber=lamp_wrd
		wnumber=strtrim(strmid(wnumber,1,2),2)
		did_curw=fix(wnumber)
		widget_control,bad_id=i,laber, set_value='Reading in '+'# '+wnumber
return
end
pro p_did_just_read, pp2,frm,wnumber,uv,hyst,fname ,info ,comhis
;** ***************
@lamp.cbk
 common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    		did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

	if pp2 ge 0 then  begin
		            if frm eq '_LAMP'  then wintb(did_curw)=-1 else wintb(did_curw)=pp2
		            if frm eq '.hdf'   then wintb(did_curw)=-1 else wintb(did_curw)=pp2
		            if frm eq '.xml'   then wintb(did_curw)=-1 else wintb(did_curw)=pp2
	                    if frm ne 'dial_*.pro*' then $
	                    if frm ne '*.*'         then $
			           ii=execute('P_ICK_MICO, wnumber,x'+wnumber+',y'+wnumber+',uv,pp2') $
	                    else   ii=execute('P_ICK_MICO, wnumber,x'+wnumber+',y'+wnumber+',uv,-10')
	endif

	if hyst ne '' then hyst=fname+' <-- '+hyst else hyst=fname
	p_did_after_read,  wnumber,uv(3), hyst ,pp2 ,comhis

	if (pp2 ge 0) and (frm ne '_LAMP') and (frm ne '.hdf') and (frm ne '*.*') $
			  and (frm ne 'dial_*.pro*') and (frm ne '.xml') then begin siz=0L
			i=execute('siz=size(w'+wnumber+')')
			siz1=siz(1) & if siz(0) gt 1 then siz2=siz(2) else siz2=1
			fixw=fix(wnumber)
			clearpar,fixw,wnumber
			x_tit    (fixw)=' X,Y -> '+strtrim(string(siz1),2)+','   + $
						   strtrim(string(siz2),2)
			z_tit    (fixw)=' Min='+strtrim(string(w_min(fixw)),2)+ $
					' Max='+strtrim(string(w_max(fixw)),2)
			other_tit(fixw)  =fname
			head_tit (fixw,2)=frm

		        if (frm eq '.gel')  or (frm eq '.image') or (frm eq '.WIND') or $
		           (frm eq '.PICT') or (frm eq '') then begin

			   other_tit(fixw) =other_tit(fixw) + x_tit(fixw) + z_tit(fixw)
			   n=n_elements(info)

			   if (frm eq '.gel')  then if n gt 1 then begin
			   	other_tit(fixw)=other_tit(fixw)+' created '+info(0)
			   	w_tit    (fixw)=info(1)
			   	endif
			   if (frm eq '.WIND') then if n ge 5 then begin
			   	tmp=strtrim(info(0),2) & if tmp ne '' then w_tit(fixw)=tmp
			   	tmp=strtrim(info(1),2) & if tmp ne '' then x_tit(fixw)=tmp
			   	tmp=strtrim(info(2),2) & if tmp ne '' then y_tit(fixw)=tmp
			   	tmp=strtrim(info(3),2) & if tmp ne '' then z_tit(fixw)=tmp
			   	tmp=strtrim(info(4),2) & if tmp ne '' then other_tit(fixw)=tmp
			   	endif
			endif
	endif
	if pp2 ge 0 then P_MUS,'mus_shot'
return
end
pro p_did_after_read, wnumber,laber,fname,pp2 ,comhis
;** ****************
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

		if pp2 ge 0 then begin
			   did_curw=fix(wnumber)
			   to_don_history, did_curw , 0 , comhis
			   if strpos(strupcase(comhis),'DIAL') ge 0 then tit='d' else tit='w'
			   widget_control,bad_id=i,laber, set_value=tit+wnumber+' loaded ...'

			   if wintb(did_curw) ge 0 then ii=execute("sl_lampscan,'purge',wintb(did_curw)")
			   wtb  (did_curw)=0
		endif else widget_control,bad_id=i,laber, set_value='No file restored !!!'
		print,string(7b)
return
end

pro write_ps
;** ********
    p_did_event,0,[-88,350,0,0,0,0,0,0,0]
end
pro p_did_ps_header, yo , widx ,out_file
;** ***************
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

		tvlct ,  cur_r,cur_g,cur_b ,/get
		loadct,1,ncolors=did_icon(0,0)+1,/silent
		keep_o  =!order
		!order  =0
		tv,did_icon,0    ,yo   ,xsize=1.,ysize=0.5   ,/inches
		tvlct ,  cur_r,cur_g,cur_b
		icl     =n_elements(cur_r)/2
;		icl     =2
		if widx gt 0 then begin titi=strmid(strtrim(w_tit(widx),2),0,55)
		   on_ioerror,misps
		   device,/helvetica,/bold
		   xyouts,0,-0.20*2540,   other_tit(widx),CHARSIZE =0.65,font=0,/device,color=icl
		   xyouts,1.5*2540 ,yo*2540  ,  titi     ,CHARSIZE =1.5 ,font=0,/device,color=icl
		   misps: print,string(7b)
		endif

		device,/close_file
		!order=keep_o

		txt=out_file+' created'
		if lamp_devps ne '' then begin
						kppw=!D.Name & if sys_dep('MACHINE') eq 'win' then set_plot,'WIN'
						dir=''
						if strpos(out_file,lamp_dvd) lt 0 then begin
							cd,current=dir
							if strmid(dir,strlen(dir)-1,1) ne lamp_dvd then dir=dir+lamp_dvd
						endif
						bid=sys_dep('PRINT',lamp_devps,dir+out_file)  &    set_plot,kppw
						txt=txt+' ,sent to '+lamp_devps
						endif
		if l_message gt 0 then widget_control,bad_id=ii,l_message  ,set_value=txt
		if b_labins(6)    then if b_labins(7) gt 0 then $
		                       widget_control,bad_id=ii,b_labins(7),set_value=txt
return
end

pro Launch, what
;***************
CASE strlowcase(what) of
"colors":	p_did_event,0,[0,347,0]
"scan":		p_did_event,0,[0,306,0,-1]
"superplot":	p_did_event,0,[0,352,0]
"gk_fit":	p_mac_event,0,[0,580,0]
"ben_int":	desk_event ,0,[0,401,0]
"ben_def":	desk_event ,0,[0,402,0]
"ben_rgp":	desk_event ,0,[0,403,0]
"wdiag":	desk_event ,0,[0,401,-1]
"tomo":		desk_event ,0,[0,401,-2]
"calib":	desk_event ,0,[0,307,0]
"inx":		desk_event ,0,[0,571,0]
"exit":		p_did_event,0,[0,398,0]
ELSE:
ENDCASE
end

pro SaveSession
    p_did_save_session & end
pro p_did_save_session
;** ******************
@lamp.cbk
@dons.cbk
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

	save,/variables,filename='lamp.ses'
	txt='Current Lamp Session is SAVED ....'
	if l_message gt 0 then begin  print,string(7b) & widget_control,bad_id=ii,l_message  ,set_value=txt
			  endif else  print,txt
	if b_labins(6)    then if b_labins(7) gt 0 then  widget_control,bad_id=ii,b_labins(7),set_value=txt
	P_MUS,'mus_shot'
	DID_WRITE_JOURNAL
return
end

pro did_set, text
;** *******
@lamp.cbk
@dons.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
;**
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

    i=execute(text)
return
end

pro p_did_setwin0, map=map
;** *************
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

catch,stat & if stat ne 0 then begin catch,/cancel & print,!err_string
		did_wd  =widget_draw  (did_wb ,retain=2,xsize=did_x,ysize=did_y,$
					 /button_events,/motion_events)
		if GEORGE ne 1 then widget_control,did_wd,draw_motion_events=1 & return
             endif
if !D.name eq 'Z' then begin device,set_resolution=[did_x,did_y] & erase
endif else if did_wd gt 0 then begin widget_control,did_wd  ,get_value=did_win0
			             if keyword_set(map) then WIDGET_CONTROL,did_wb ,bad_id=ii,MAP=1
			             WSET,did_win0   &  endif
return
end

pro to_did_cur,	wkspce
;** **********
;**
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

    if strlen(wkspce) le 2 then bb=' ' else bb=''
    if n_elements(did_wsc) eq 1 then widget_control,bad_id=i,did_wsc,$
    						    set_value='Plot '+strupcase(wkspce)+bb

return
end

pro DECOR, cti_,ctj_,a1_,a2_,DIS_,PIXV_,PIXH_,shap,squa,LVu_,LVd_,LHl_,LHr_,FQ_,PHI_,DXT_,DYT_,FCTX_,FCTY_
;** *****
;**
common depli,cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar

WDIAG, /nw

cti=cti_ & ctj=ctj_
if n_elements(a1_)   eq 1 then a1=a1_		else a1=0.
if n_elements(a2_)   eq 1 then a2=a2_		else a2=360.
if n_elements(DIS_)  eq 1 then DIS=DIS_		else DIS=100.
if n_elements(PIXV_) eq 1 then PIXV=PIXV_	else PIXV=150.
if n_elements(PIXH_) eq 1 then PIXH=PIXH_	else PIXH=150.
if n_elements(LVu_)  eq 1 then LVu=LVu_		else LVu=0
if n_elements(LVd_)  eq 1 then LVd=LVd_		else LVd=0
if n_elements(LHl_)  eq 1 then LHl=LHl_		else LHl=0
if n_elements(LHr_)  eq 1 then LHr=LHr_		else LHr=0
if n_elements(FG_)   eq 1 then FQ=FQ_		else FQ=4
if n_elements(PHI_)  eq 1 then PHI=PHI_		else PHI=0
if n_elements(DXT_)  eq 1 then DXT=DXT_		else DXT=0.
if n_elements(DYT_)  eq 1 then DYT=DYT_		else DYT=0.
if n_elements(FCTX_) eq 1 then FCTX=FCTX_	else FCTX=-1.
if n_elements(FCTY_) eq 1 then FCTY=FCTY_	else FCTY=-1.
if n_elements(shap)  eq 1 then shape=shap	else shape=0
if n_elements(squa)  eq 1 then squar=squa	else squar=0

; then DEPLI,  area, ry1,ry2 ,arel,diam,xdiam,b_red, AV,AH
end

;*************************************** Process Display *********************************
;*************************************** Process Display *********************************
;*************************************** Process Display *********************************

pro FORCPLOT ,wi, w=wj ,image=im,contour=co,surface=su,vrml=vr,below=bl,beside=bs,pscript=ps,gif=gf,png=pg,htm=hm
    SEE	     ,wi, w=wj ,image=im,contour=co,surface=su,vrml=vr,below=bl,beside=bs,pscript=ps,gif=gf,png=pg,htm=hm & end
pro SEE      ,wi, w=wj ,image=im,contour=co,surface=su,vrml=vr,below=bl,beside=bs,pscript=ps,gif=gf,png=pg,htm=hm $
                       ,xypixels=sp, screen=sc, replot=rp
;** ***
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

wk=0 & if n_elements(wi) eq 1 then wk=wi else if n_elements(wj)  eq 1     then wk=wj $
					 else if alone gt 0 then wk=alone else wk=one>1
if (wk gt 0) and (wk le 23) then begin
	ws=strtrim(string(wk),2) & s=0L
	bb=execute( 's=size(w' + ws + ')' )
	keep_repr=did_repr & did_repr(0:2)=0
	keep_fu  =did_fu
	keep_st  =styles
	if vff(2) eq 1 then begin
				if vff(3) eq 0 then did_repr(0)=1
				if vff(3) eq 1 then did_repr(1)=1
				if vff(3) eq 2 then did_repr(2)=1
				did_repr(5)= vff(10)
				did_repr(6)= vff(12)
				did_fu	   = vff(5)
				endif
	if keyword_set(vr)	then begin su=1 & bs=1 & endif
	if keyword_set(im)      then did_repr(0)    =im
	if keyword_set(co)      then did_repr(1)    =co
	if keyword_set(su)      then did_repr(2)    =su
	if keyword_set(vr)      then did_repr(2)    =1
	if keyword_set(vr)      then styles(0,1)    =1
	if keyword_set(ps)      then styles(3,0)    =1
	if keyword_set(ps)      then w_numor(0)     ="lamp.ps"
	if keyword_set(gf)      then styles(3,0)    =2
	if keyword_set(pg)      then styles(3,0)    =4
	if keyword_set(hm)      then styles(3,0)    =3
	if keyword_set(sc)      then styles(3,0)    =5
	if keyword_set(bl)      then did_fu	    =0
	if keyword_set(bs)      then did_fu	    =1
	if n_elements (sp) eq 2 then did_repr(18)   =sp
	if keyword_set(rp) and (n_elements(wbeside) gt 1) then turn=wbeside(0) else turn=0
	if (!D.name ne 'Z')     then $
	if (!D.flags and 65536) eq 0 then did_fu=0 else if ((b_labins(3) eq 1) and (!D.name eq "X")) then did_fu=1

;	if ((did_fu eq 0) and (GEORGE ne 0)) then DialWSet

	p_did_drawidl, turn, ws, s

	did_repr=keep_repr  &  styles  =keep_st  &  did_fu  =keep_fu
endif
return
end

pro FORCSET ,image=im,contour=co,surface=su,rot=rot,below=bl,beside=bs,xrange=xr,yrange=yr,$
				 log=lg,zlim=zl,regular=rg,vrml=vr,view=va
    SEEM    ,image=im,contour=co,surface=su,rot=rot,below=bl,beside=bs,xrange=xr,yrange=yr,$
				 log=lg,zlim=zl,regular=rg,vrml=vr,view=va		   & end
pro SEEM    ,image=im,contour=co,surface=su,rot=rot,below=bl,beside=bs,xrange=xr,yrange=yr,$
				 log=lg,zlim=zl,regular=rg,vrml=vr,view=va
;** ****
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o

    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

vff(2)=1 & vff(3)=-1
if keyword_set(vr)	 then begin   su=1 & bs=1 & endif
if keyword_set(im)       then vff(3)  =0
if keyword_set(co)       then vff(3)  =1
if keyword_set(su)       then vff(3)  =2
if n_elements (rot) eq 1 then vff(4)  =rot
if keyword_set(bl)       then vff(5)  =0
if keyword_set(bs)       then vff(5)  =1
if n_elements (xr)  eq 2 then vff(6:7)=xr
if n_elements (yr)  eq 2 then vff(8:9)=yr
if n_elements (lg)  eq 1 then vff(10) =lg
if n_elements (zl)  eq 1 then vff(11) =zl
if n_elements (rg)  eq 1 then vff(12) =rg
if n_elements (va)  eq 1 then vff(13) =va
return
end

pro FORCPAR, param
;** *******
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
h=10 & m=did_y/h
n=n_elements(param)<m -1

if n gt 0 then begin
   y=did_y-h
   k=!window
   p_did_setwin0 & erase
   for i=0,n do begin xyouts,1,y,param(i),/dev,charsize=1.2,font=0  &  y=y-h & endfor
   if k gt 0 then if !D.name ne 'Z' then wset,k
endif
end

pro DRAWIND  ,xsiz,ysiz , DrawId=basw
;** *******
;**
@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
;**
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

if  l_message le 0 then return

if (n_elements(xsiz) ne 1) then xsiz=512
if (n_elements(ysiz) ne 1) then ysiz=512
if (xsiz gt 0) and (ysiz gt 0) and (xsiz le 4000) and (ysiz le 4000)  then begin
	keep_rep =did_repr & did_repr(2)=1 & did_repr(6)=0 & did_repr(7)=0
	keep_fu  =did_fu   & did_fu     =1
	w0	 =[[xsiz,xsiz],[ysiz,ysiz]]

	p_did_drawidl,	0, '0' , size(w0) , DrawId=basw

	did_repr =keep_rep
	did_fu   =keep_fu
endif
return
end

pro p_did_rep, r,spc
;** *********
;**
;		r=0 image
;		r=1 image   +  surface
;		r=2 image   +  contour
;		r=3 image   +  contour   +  surface
;		r=4 contour +  surface
;		r=5 contour
;		r=6 surface

    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
		spc=0
		r  =0
		if (did_repr(0) eq 1) and (did_repr(1) eq 1) and (did_repr(2) eq 1) then r=3 else $
		if (did_repr(0) eq 1) and (did_repr(1) eq 1) then r=2 else $
		if (did_repr(0) eq 1) and (did_repr(2) eq 1) then r=1 else $
		if (did_repr(1) eq 1) and (did_repr(2) eq 1) then r=4 else $
		if (did_repr(1) eq 1) then r=5			      else $
		if (did_repr(2) eq 1) then r=6
		if (r eq 5) then spc=-10
		if (r eq 6) then spc= 10
return
end

pro p_did_drawidl,	turn,  string_w	, isiz , DrawId=basw
;** *************
;**
;** turn= 0	new plot
;** turn= 1	replot +10 degres
;** turn=-1	replot -10 degres
;** turn> 2	replot beside
;** turn<-2	replot LiveTools

@lamp.cbk
    common c_did,	did_x,did_y,did_wb,did_wd,did_wsp,did_fu,did_curw,did_wsc,did_tio,did_pio,$
    			did_repr,did_scan,did_surf,did_inib,did_icon,did_lamp,did_pix,did_buf,did_o
;**
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff
    common c_drow
;**
    common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
;**
    common c_codens,	zz,mx1,mx2,my1,my2,mz1,mz2,mxv2,cdbox,idn,CODENS
    trap_ws='0'
    redraw = 0
    bobol  = 0
    new    = 1
    off60  = 60
    off90  = off60*3/2
    p_did_rep,rrr,spc

    keeprp1=did_repr(1)

    if (turn lt -2)     and (turn gt -10000) then liveT=1  else liveT=0
    if (turn lt -10000) and (turn gt -20000) then liveC=1  else liveC=0
    if (turn lt -20000) and (turn gt -30000) then liveA=1  else liveA=0
    if (turn lt -30000) and (turn gt -40000) then begin rrr  =7 &  did_repr(1)=1  & endif
    if (turn eq  0)     or  (turn gt 2)      then $
                        if  (sys_dep('VERSION') ge 5.0) and (b_labins(3) eq 0) then begin
                        if (rrr eq 3) and (isiz(0) eq 1) then liveT=10
                        if (rrr eq 4) and (isiz(0) eq 1) then liveT=10
                        if (rrr eq 4) and (isiz(0) eq 2) and (did_fu eq 1) then liveC=1
                        if (rrr eq 2) and (isiz(0) eq 3) then liveA=1
                       ;if (rrr eq 3) and (isiz(0) eq 3) then liveT=10
                        endif
    if (turn lt -2)     then turn=0
    if styles(3,0) eq 1 then begin ps_ok =1	& styles(3,0)=0 & endif else ps_ok =0
    if styles(3,0) eq 2 then begin gif_ok=1	& styles(3,0)=0 & endif else gif_ok=0
    if styles(3,0) eq 3 then begin htm_ok=1	& styles(3,0)=0 & endif else htm_ok=0
    if styles(3,0) eq 4 then begin gif_ok=1	& styles(3,0)=0 & png_ok=1 & endif else png_ok=0
    if styles(3,0) eq 5 then begin scr_ok=1	& styles(3,0)=0 & endif else scr_ok=0

    if (turn gt  2) then fu_out=1  else fu_out=did_fu
    if (did_repr(18) gt 50) and (did_repr(19) gt 50) then fu_out=1
    
    if (turn eq  0) and (string_w eq '0') then wonly =[w0(0,0),w0(1,1)]  else wonly=[0,0]
    if (turn eq  0) or  (turn gt 2) then  begin
	wnumber=string_w
        idn    =fix(wnumber)
	if wonly(0) eq 0 then w0 =0
	uxy=0 & axy=0
	siz=isiz
	if (siz(1) eq 1) or (siz(2) eq 1) then  begin
			bb=execute( 'w' + wnumber + '=reform(w' + wnumber + ')' )
			bb=execute( 'siz=size(w' + wnumber + ')' ) & endif
	bb=execute( 'if n_elements(x'+wnumber+') gt 1 then x' + wnumber + ' =reform(x' + wnumber + ')' )
	bb=execute( 'if n_elements(y'+wnumber+') gt 1 then y' + wnumber + ' =reform(y' + wnumber + ')' )
	bb=execute( 'if n_elements(z'+wnumber+') gt 1 then z' + wnumber + ' =reform(z' + wnumber + ')' )
    endif else  if (n_elements(flgsurf) gt 1) and ((did_repr(2) eq 1) or (siz(0) eq 3))  then $
    		if  flgsurf(2) eq  idn then redraw=1
		
;Density points?
;***************
        sizx =[0L]  & i=execute('sizx=size(x'    + wnumber + ')' ) & CODENS= 0
        sizy =[0L]  & i=execute('sizy=size(y'    + wnumber + ')' )
        sizz =[0L]  & i=execute('sizz=size(z'    + wnumber + ')' )
	if (siz (0)  le 2) and (sizx(0) eq 1) and (sizy(0) eq 1) and (sizz(0) eq 1)  then $
        if (siz (1)  eq sizx(1)) and (siz (1)  eq sizy(1)) and (siz (1)  eq sizz(1)) then $ 
	if (siz (0)  eq 1) or (siz (2) le 3) then CODENS=-1
	
    flgsurf=0
    isurf  =0
    if CODENS eq -1 then isurf=1
    if  siz(0) gt 1 then if (did_repr(2)) or ((siz(0) eq 3) and ((did_repr(0) eq 0) and (did_repr(1) eq 1))) then isurf=1

;Z angle !
    if isurf then begin
			if vff(2) eq 1 then rx=vff(13)
			if vff(2) eq 1 then rz=vff(4)  else begin
				srz='+30' & rz=20
				if  l_message gt 0 then $
			    	    widget_control,bad_id=i,did_repr(4),get_value=srz else srz=string(vff(4))
				 srz=strcompress(srz(0),/remove_all)
				 deg=strpos(srz,'^')
				 if deg gt 0 then srz=strmid(srz,0,deg)
				 on_ioerror,misrz
				 rz=fix(srz)
				 if (turn eq 1) or (turn eq -1) then begin
				    rz=rz+ turn*10
				    if rz ge  360 then rz=rz-360
				    if rz le -360 then rz=rz+360
				    srz=strtrim(string(rz),2)+'^'
				    if l_message gt 0 then widget_control,bad_id=i,did_repr(4),set_value=srz
				 endif
				 misrz:
			endelse
    endif
;***
    if (turn eq 0) or (turn gt 2) or (redraw eq 1) then begin
;***
       bst_x=did_x  & bst_y=did_y

;Density points? or object? or standard plot?
;*************** ** ******* ** **************
	IF  b_labins(3) eq 0  then begin
	 if CODENS ne 0       then begin
	    DID_OBJET,idn,monobj
	    if monobj gt 0 then begin CODENS=monobj & liveT=0 & redraw=0 & endif
	 endif
	 if fu_out eq 0 then if (CODENS gt 0)     then $
			WIDGET_CONTROL,did_wb ,bad_id=ii,MAP=0 $ ;Case we come back from liv_objet (below)
	    else begin	WIDGET_CONTROL,did_wb ,bad_id=ii,MAP=1 & WIDGET_CONTROL,did_wc,bad_id=ii,MAP=0
	                WIDGET_CONTROL,did_4dM,bad_id=ii,sensitive=0  & endelse
	ENDIF
;********
       use_scan=0
           if (siz(0) gt 1) and (styles(2,0) eq 3)  then use_scan=1
           if (siz(0) eq 3) and (styles(2,0) eq 4)  then use_scan=1
           if (siz(0) eq 3) and (did_repr(2)+did_repr(1) eq 0)  then use_scan=1
	   if (siz(0) eq 2) and (did_repr(6) eq 0)  then $
			if  did_surf(0)  eq 1 then begin
	   				 if (rrr eq 5) and (styles(1,1)  eq 1) then use_scan=1
	   				 if (rrr eq 6) and (styles(0,1)  eq 1) then $
						    if sys_dep('VIEWER') eq 0  then use_scan=0 ;or = 1 !!!
			endif
       if (use_scan ne  0) then if (did_scan eq -1) then begin SL_SCANLOAD,did_scan,lamp_dir
			                                 if (did_scan ge 0) then loadct,tcol & endif
       if (did_scan lt  0) then use_scan=0

;      if (rrr eq 6) and (styles(0,1) eq 1) then $
       if ((rrr eq 1) or (rrr eq 3) or (rrr eq 4) or (rrr eq 6)) and (styles(0,1) eq 1) then $
		if (b_labins(3) eq 2) or (sys_dep('VIEWER') eq 1) then rrr=7

;Set below or beside !
;*** ***** ** ******
       if (htm_ok) or (liveT ne 0) or (liveC) or (liveA) or (rrr eq 7) then $
        if CODENS  ge 0 then begin
	               fu_out=0 & use_scan =0 & redraw=0 & endif
       if fu_out eq 0 then begin if (redraw eq 0) then LOGO,4
                                 if (liveT  eq 0) then p_did_setwin0
       endif else begin
            if n_elements(wbeside) le 1 then    redraw=0 $
             else if  redraw  eq 1  then begin   widget_control,bad_id=i,wbeside(0),map=1
            					 if i ne 0 then RETURN else if !D.name ne 'Z' then $
            					                              if wbeside(2) gt 0 then wset,wbeside(2)
       endif & endelse

;redraw=0
;******
       if redraw eq 0 then begin

	if use_scan eq 0 then begin
	   !p.background =0
	   !p.noerase =0
	   !p.color =255
	   !p.font =0
	   !order =0
	   !x.style= 1 & !y.style= 1 & !z.style= 1
	   !x.range= 0 & !y.range= 0 & !z.range= 0
	   !p.position=[0,0,0,0]     & !p.multi= 0
	   if (liveT ne 0) or $
	    (((did_repr(2) ne 1) or (siz(0) eq 1)) and (CODENS eq 0) and (siz(0) ne 3)) then begin
		if ps_ok then !p.title   ='' else !p.title   =w_tit(idn)
		if ps_ok then !p.subtitle='' else !p.subtitle=other_tit(idn)
		!x.title   =x_tit(idn)
		!y.title   =y_tit(idn)
		!z.title   =z_tit(idn)
		if (not ps_ok) then $
		if did_repr(5) eq 1 then if siz(0) lt 2 then !p.subtitle=!p.subtitle + ' Y=LOG10(f(x))' else $
						     	     !p.subtitle=!p.subtitle + ' Z=LOGn (z)'
	   endif
	endif
;Use true axis !
;*** **** ****
	xx=[1L] & yy=[1L] & zz=[1L] & ee=0 & nn=1 & vv=0
	if (sizx(0)  eq 0) or  (sizx(1) ne siz(1))   then i=execute('x'+ wnumber+'=lindgen(siz(1))+1')
	if (siz(0)   eq 3) then $
	if (sizz(0)  eq 0) or  (sizz(1) ne siz(3))   then i=execute('z'+ wnumber+'=lindgen(siz(3))+1')
	if  CODENS   eq 0 then begin
	 if  siz(0)  eq 1 then i=execute('y'+ wnumber+'=y'+ wnumber+'(0)') else $
	 if (sizy(0) eq 0) or ((sizy(0) eq 1) and (sizy(1) ne siz(2)))$
			   or ((sizy(0) eq 2) and (sizy(2) ne siz(2)))     then $
			       i=execute('y'+ wnumber+'=lindgen(siz(2))+1')
	endif

	if (sys_dep('STUDENT')) and (rrr eq 0) and (fu_out eq 1)  then begin
	                                                           did_repr(1)=1 & rrr=5 & endif
	i =execute('xx= x'+wnumber) & sizx=size(xx)
	i =execute('yy= y'+wnumber) & sizy=size(yy)
	i =execute('zz= z'+wnumber) & sizz=size(zz)

	if did_repr(6) eq 1 then begin
		uxy=1
		if (rrr eq 0) and (siz(0) eq 2) then begin did_repr(1)=1 & rrr=2 & endif
	endif

	if ( (xx(1) -xx(0))*(siz(1)-1) eq xx(siz(1)-1)-xx(0)) then if (siz(0) gt 1) then $
	if ( (yy(1) -yy(0))*(siz(2)-1) eq yy(siz(2)-1)-yy(0)) then axy=1 else axy=0 else axy=1

;Use errors !
	if siz(0) eq 1 then begin sizee=[0L]
	   i = execute('sizee=size(e' + wnumber + ')' )
	   if (sizee(0) eq 1) and (sizee(1) eq siz(1)) then i=execute('ee= e'+wnumber)
	   endif
	if siz(0) eq 1 then begin siznn=[0L]
	   i = execute('siznn=size(n' + wnumber + ')' )
	   if (siznn(0) eq 1) and (siznn(1) eq siz(1)) then i=execute('nn= n'+wnumber)
	   endif
	if CODENS gt 0 then $
	if siz(0) eq 1 then begin sizvv=[0L]
	   i = execute('sizvv=size(pv'+ wnumber + ')' )
	   if (sizvv(0) eq 2) and (sizvv(1) eq 2)      then i=execute('vv=pv'+wnumber)
	   if (sizvv(0) eq 1) and (sizvv(1) eq siz(1)) then i=execute('vv=pv'+wnumber)
	   endif
;Stretch !
	keyrangx=''  & xl=0 & xf=0 & mx1=0 & mx2=0
	keyrangy=''  & yl=0 & yf=0 & my1=0 & my2=0
	keyrangz=''  & zl=0 & zf=0 & mz1=0 & mz2=0
	if vfl(0) ge 0 then begin xf=vfl(0) & xl=vfl(1) & if xl ge siz(1) then xl=siz(1)-1 & endif
	if vfl(2) ge 0 then begin yf=vfl(2) & yl=vfl(3) & if yl ge siz(2) then yl=siz(2)-1 & endif
;Stretch x !
	on_ioerror,misxrange
	if (((did_repr(7) eq 1) and (vfl(0) lt 0) and (did_repr(10) gt 0)) or (vff(6) ne vff(7))) then begin

	    if  sizx(0) le 1 then xw=0   else xw=siz(2)/2

	    mx2  =max(xx(*,xw),min=mx1) ;or xx(siz(1)-1) & xx(0)
	    kpmx1=mx1  & kpmx2=mx2

	    if vff(2) eq 1 then begin
		if vff(6) ne vff(7) then begin mx1=vff(6) & mx2=vff(7) & endif
	    endif else begin
	    	widget_control,bad_id=i,did_repr(10),get_value=smxr
				 smxr=STRLOWCASE(strcompress(smxr(0),/remove_all))
				 if   STRPOS(smxr,'min') lt 0   then mx1=float(smxr)
	    	widget_control,bad_id=i,did_repr(11),get_value=smxr
				 smxr=STRLOWCASE(strcompress(smxr(0),/remove_all))
				 if   STRPOS(smxr,'max') lt 0   then mx2=float(smxr)
	    endelse

	    if CODENS ne 0 then begin
	    	idx=where((xx ge mx1) and (xx le mx2)) & nidx=n_elements(idx)
	    	if (nidx gt 2) and (nidx lt n_elements(xx)) then begin
	    	    i =execute( 'w0= w'+wnumber +'(idx)') & siz=size(w0) & wnumber= '0'
		    MYREDUCE, idx, xx,yy,zz,ee,nn,vv
	    	    xl=0 & xf=0 & endif
	    endif else begin
		ivf= where(xx(*,xw) ge mx1 ,count1)
		     if count1 gt 0 then xf=ivf(0) else xf=0
		ivl= where(xx(*,xw) ge mx2 , count2)
		     if count2 gt 0 then xl=ivl(0) else xl=siz(1)-1
		if (count1 le 0) or (count2 le 0) or (siz(0) eq 1) then keyrangx=',xrange=[mx1,mx2]'
		if ((kpmx1 gt mx1) or (kpmx2 lt mx2)) and (siz(0) eq 1) then axy=0
	    endelse
	endif
	if (xl gt xf) and (xf ge 0) and (xl lt siz(1)) and ((xf ne 0) or (xl ne siz(1)-1)) then begin
		  if siz(0) eq 1 then i=execute( 'w0= w'+wnumber +'(xf:xl)'     )
		  if siz(0) eq 2 then i=execute( 'w0= w'+wnumber +'(xf:xl,*)'   )
		  if siz(0) gt 2 then i=execute( 'w0= w'+wnumber +'(xf:xl,*,*)' )
		  siz=size(w0)
		  wnumber= '0'
		  if sizx(0) eq 2 then xx=xx(xf:xl,*) else xx=xx(xf:xl)
		  if sizy(0) eq 2 then yy=yy(xf:xl,*)
		  if n_elements(ee) gt 1 then ee=ee(xf:xl)
		  vfl(0)=xf & vfl(1)=xl
		  set_xy
	endif else vfl(0)=-1
	misxrange:
	if vfl(0) lt 0 then begin vfl(0)=0 & vfl(1)=siz(1)-1 & endif

;Stretch y !
	if (((did_repr(8) eq 1) and (did_repr(12) gt 0)) or (vff(8) ne vff(9))) then begin
	    my1=w_min(idn)
	    my2=w_max(idn)
	    if (CODENS ne 0) or (siz(0)  ge 2)  then  begin
	       if sizy(0) le 1 then yw=yy else yw =reform(yy(siz(1)/2,*))
	       my1=min(yw,max=my2)  &    endif

	    if vff(2) eq 1 then begin
		if vff(8) ne vff(9) then begin my1=vff(8) & my2=vff(9) & endif
	    endif else begin
	    	on_ioerror,misyrange
	    	widget_control,bad_id=i,did_repr(12),get_value=smyr
				 smyr=STRLOWCASE(strcompress(smyr(0),/remove_all))
		 		 if   STRPOS(smyr,'min') lt 0 then my1=float(smyr)
	    	widget_control,bad_id=i,did_repr(13),get_value=smyr
				 smyr=STRLOWCASE(strcompress(smyr(0),/remove_all))
	    		 	 if   STRPOS(smyr,'max') lt 0 then my2=float(smyr)
	    	misyrange:
	    endelse
	endif
;	For Image
	if (siz(0) ge 2) or (CODENS ne 0) then begin

	if ((did_repr(8) eq 1) and (did_repr(12) gt 0)) or (vff(8) ne vff(9)) then if vfl(2) lt 0 then begin
	    if CODENS ne 0 then begin
		idx=where((yy ge my1) and (yy le my2)) & nidx=n_elements(idx)
		if (nidx gt 2) and (nidx lt n_elements(yy)) then begin
	    	    i=execute( 'w0= w'+wnumber +'(idx)') & siz=size(w0) & wnumber= '0'
		    MYREDUCE, idx, xx,yy,zz,ee,nn,vv
	    	    yl=0 & yf=0 & endif
	    endif else begin
		ivf= where(yw ge my1 , count)
		     if count gt 0 then yf=ivf(0) else yf=0
		ivl= where(yw ge my2 , count)
		     if count gt 0 then yl=ivl(0) else yl=n_elements(yw)-1
	    endelse
	endif
	if (yl gt yf) and (yf ge 0) and (((yl lt siz(2)) and ((yf ne 0) or (yl ne siz(2)-1))))$
		  then begin
		  if siz(0)  eq 2 then i=execute( 'w0= w'+wnumber +'(*,yf:yl)'   ) else $
		  if siz(0)  gt 2 then i=execute( 'w0= w'+wnumber +'(*,yf:yl,*)' )
		  siz=size(w0)
		  wnumber= '0'
		  if sizy(0) eq 2 then yy=yy(*,yf:yl) else yy=yy(  yf:yl)
		  if sizx(0) eq 2 then xx=xx(*,yf:yl)
		  vfl(2)=yf & vfl(3)=yl
		  set_xy
	endif else vfl(2)=-1
	if vfl(2) lt 0 then begin vfl(2)=0 & vfl(3)=siz(2)-1 & endif

;	For Vector
	endif else begin
	   if ((did_repr(8) eq 1) and (did_repr(12) gt 0)) or (vff(8) ne vff(9)) then begin
	       if (my1 lt w_max(idn)) and (my2 gt w_min(idn)) then keyrangy=',yrange=[my1,my2]'
	   endif else begin my1=w_min(idn)
	   		    my2=w_max(idn) & endelse

	   if vfl(2) ge 0  then begin my2=vff(1) & keyrangy=',yrange=[my1,my2]' &  endif
	   vfl(2)=-1
	   if (my1 lt w_min(idn)) or (my2 gt w_max(idn)) then axy=0
	endelse

;Stretch scan or z!
	on_ioerror,miszrange
	if  (siz(0) eq 3) or (CODENS ne 0) then begin
	 if (did_inib(2)  gt 0) then widget_control,bad_id=ii,did_inib(2),map=1 & did_inib(2)=0
	 if (did_repr(17) eq 1) and (did_repr(15) gt 0) then begin

	    mz2=max(zz,min=mz1)

	    	widget_control,bad_id=i,did_repr(15),get_value=smzr
				 smzr=STRLOWCASE(strcompress(smzr(0),/remove_all))
				 if   STRPOS(smzr,'min') lt 0   then mz1=float(smzr)
	    	widget_control,bad_id=i,did_repr(16),get_value=smzr
				 smzr=STRLOWCASE(strcompress(smzr(0),/remove_all))
				 if   STRPOS(smzr,'max') lt 0   then mz2=float(smzr)

	    if CODENS ne 0 then begin
		idx=where((zz ge mz1) and (zz le mz2)) & nidx=n_elements(idx)
		if (nidx gt 2) and (nidx lt n_elements(zz)) then begin
	    	    i=execute( 'w0= w'+wnumber +'(idx)') & siz=size(w0) & wnumber= '0'
		    MYREDUCE, idx, xx,yy,zz,ee,nn,vv
	    	    zl=0 & zf=0 & endif
	    endif else begin
		ivf= where(zz(*) ge mz1 ,count1)
		     if count1 gt 0 then zf=ivf(0) else zf=0
		ivl= where(zz(*) ge mz2 ,count2)
		     if  count2 gt 0 then zl=ivl(0) else zl=n_elements(zz)-1
	    endelse
	 endif
	endif
	if (zl gt zf) and (zf ge 0) and ((zl lt siz(3)) and ((zf ne 0) or (zl ne siz(3)-1)))$
		  then begin
		  i=execute( 'w0= w'+wnumber +'(*,*,zf:zl)' )
		  siz=size(w0)
		  wnumber= '0'
		  zz=zz(zf:zl)
		  set_xy
	endif
	miszrange:

;Beside construction !
       if (rrr eq 2) and (fu_out eq 1) and (siz(0) eq 2) then bobol=90

       if fu_out eq 1 then begin
	  if turn gt 2 then begin
		i =0 & widget_control,bad_id=i,turn, get_uvalue=uv ,tlb_get_size=v2

		if i eq 0 then begin v2=long(v2) & uv=long(uv)
			new=0
			bst_x=uv(5)-bobol & bst_y=uv(6)
			wbeside=[uv(2),uv(3),uv(4),bst_x,bst_y,uv(9),uv(12)]
			
			widget_control,bad_id=i,uv(10), get_value=rsz_x & rsz_l=bst_x
			widget_control,bad_id=i,uv(11), get_value=rsz_y & rsz_h=bst_y
			on_ioerror,misrsz & rsz_l=fix(rsz_x(0))+off90
			                    rsz_h=fix(rsz_y(0))+off90   & misrsz:

			if (rsz_l ne bst_x) or (rsz_h ne bst_y) then begin ;User changed size texts
				bst_x=rsz_l<3000>100 & xi=bst_x            ;***********************
				bst_y=rsz_h<2000>100 & yi=bst_y
				new=2
				wait,.3
				if (not ps_ok) then widget_control,bad_id=i,turn,/destroy

			endif else if (v2(0) ne uv(7)) or (v2(1) ne uv(8)) then begin ;User resized window
				bst_x=(bst_x+v2(0)-uv(7))>(v2(0)-30)                  ;**** ******* ******
				bst_x=(bst_x-bobol)>100
				bst_y=(bst_y+v2(1)-uv(8))>100
				xi=bst_x & yi=bst_y
				new=2
				wait,.3
				if (not ps_ok) then widget_control,bad_id=i,turn,/destroy
			endif
		endif
	  endif
	  if new ne 0 then begin
	       if new ne 2 then begin ymini=lamp_siz/2
		bst_x=512  & bst_y=ymini
		if wonly(0) ne 0 then begin bst_x=wonly(0) & bst_y=wonly(1)
		endif else $
		if (siz(0) ge 2) and (CODENS eq 0) then begin
		   bst_x=siz(1) & bst_y=siz(2) & maxvol=long(512)*512

		   bty  =bst_x/(bst_y*6) & if bty lt 1 then bty=1
		   btx  =bst_y/(bst_x*6) & if btx lt 1 then btx=1
		   bst_x=bst_x*btx	 & bst_y=bst_y*bty

		   if (did_repr(2)+did_repr(1) eq 0) and (siz(0) eq 3) then begin
		   	f=float(bst_x)/bst_y
		   	fi=round(sqrt(siz(3))/f) & fj=round(sqrt(siz(3))*f)

		   	if fi gt 1 then bst_x=bst_x*fi
		   	if fj gt 1 then bst_y=bst_y*fj
		   	while bst_x gt 640 do bst_x=bst_x-fi
		   	while bst_y gt 640 do bst_y=bst_y-fj
		   endif
		   maxi=4000
		   if bst_x gt maxi then begin bst_x=bst_x/(bst_x/maxi) & bst_y=bst_y/(bst_x/maxi) & endif
		   if bst_y gt maxi then begin bst_y=bst_y/(bst_y/maxi) & bst_x=bst_x/(bst_y/maxi) & endif

		   while bst_x*bst_y lt maxvol do begin bst_x=bst_x+siz(1)
		   					bst_y=bst_y+siz(2) & endwhile
		   while ((bst_x/bst_y gt 4) and (bst_x gt siz(1))) do bst_x=bst_x-siz(1)
		   while ((bst_y/bst_x gt 4) and (bst_y gt siz(2))) do bst_y=bst_y-siz(2)
		   if (did_repr(1) eq 1) or (did_repr(2) eq 1) then begin
		   	if bst_x lt 256 then bst_x=(256/bst_x) * bst_x
		   	if bst_y lt 256 then bst_y=(256/bst_y) * bst_y
		        if isurf then if bst_x lt bst_y then bst_x=bst_y
		   endif
		endif
		if (did_repr(18) gt 50) and (did_repr(19) gt 50) then begin
			bst_x=did_repr(18) & bst_y=did_repr(19)
			did_repr(18)=0 & did_repr(19)=0 & endif
		bst_x=bst_x+off90 & bst_y=bst_y+off90
		if bst_x gt 512  +off90 then xi=min([bst_x,lamp_siz]) else xi=bst_x
		if bst_y gt ymini+off90 then yi=min([ymini,lamp_siz]) else yi=bst_y
	       endif
	       
	       wbeside=[0,0,0,0,0,0,0]
	       if !D.name ne 'Z' then begin
		if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0
		ttl='Lamp ' + strtrim(his(fix(string_w)),2)
		basid=widget_base  (title=ttl,/column,resource_name='lampdid',map=0,/tlb_size_events)
		bas1 =widget_base  (basid    ,/row)
		bas11=widget_button(bas1,value='Remove'    ,font=ft_b_normal,uvalue=[-88,349,basid])
		bas12=widget_button(bas1,value='Color'     ,font=ft_b_normal,uvalue=[-88,347,basid])
		if wonly(0) eq 0 then $
		bas1r=widget_button(bas1,value='Replot'    ,font=ft_b_normal,$
						uvalue=[-88,301, basid,fix(string_w)])
		lue="" & basmo=0L
		if (sys_dep('VERSION') lt 5.0) or (wonly(0) ne 0) then $
		bas13=widget_button(bas1,value='Annotate'  ,font=ft_b_normal) $
		else begin
		basm =widget_button(bas1,value='LiveTools' ,font=ft_b_normal, menu=2)
		bas13=widget_button(basm,value='Annotate'  ,font=ft_b_normal)
		if siz(0) eq 3 then lue='Slicer' else if CODENS eq -1 then lue='Plot 3Dbox' $
		                                 else if CODENS gt  0 then lue='4D object' else lue='LiveTools'
		if (CODENS gt 0) then begin
		  basmo=widget_button(basm ,value=lue,font=ft_b_normal,/menu) & widget_control,basmo,sensitive=0
		  d_4vM=widget_button(basmo,font=ft_normal,value='Vrml File',uvalue='Vrml')
		endif else $
		  basmo=widget_button(basm,value=lue,font=ft_b_normal,uvalue=[-88,301,-basid,fix(string_w)])
		  
		if (siz(0) eq 2) and (CODENS eq 0) then $
		basma=widget_button(basm,value='LiveContour',font=ft_b_normal,$
						uvalue=[-88,301,-basid-10000,fix(string_w)])
		if (siz(0) eq 3) then $
		basma=widget_button(basm,value='Animation'  ,font=ft_b_normal,$
						uvalue=[-88,301,-basid-20000,fix(string_w)])
		if (siz(0) ge 2) and (CODENS eq 0) then $
		basma=widget_button(basm,value='Vrml file'  ,font=ft_b_normal,$
						uvalue=[-88,301,-basid-30000,fix(string_w)])
		endelse
		bas14=widget_button(bas1,value='Print to'  ,font=ft_b_normal)
		ttl='lamp_W'+string_w+'.ps    '
		bas15=widget_text  (bas1,value=ttl,font=ft_b_normal,xsize=11+cap,ysize=1,/editable)

		bas_x=widget_base  (bas1 ,/exclusive,/row,uvalue=3)
		biscr=widget_button(bas_x,value='screen',font=ft_smallest,/no_release,uvalue=[-88,345,1,bas_x])
		if wonly(0) eq 0 then begin
		  if (sys_dep('VERSION') ge 5.4) then kif='png' else kif='gif'
		  bid=widget_button(bas_x,value= kif    ,font=ft_smallest,/no_release,uvalue=[-88,345,2,bas_x])
		  byd=widget_button(bas_x,value='ps'    ,font=ft_smallest,/no_release,uvalue=[-88,345,3,bas_x])
		endif
		bas2 =widget_base  (basid,/row)
		      put_logo	   ,bas2
		bid  =widget_label (bas2,value='Size'				    ,font=ft_smaller)
		pixrx=widget_text  (bas2,value=strtrim(fix(bst_x-off90),2),xsize=6,ysize=1,/editable ,font=ft_smaller)
		bid  =widget_label (bas2,value='x'				    ,font=ft_smaller)
		pixry=widget_text  (bas2,value=strtrim(fix(bst_y-off90),2),xsize=6,ysize=1,/editable ,font=ft_smaller)
		baslb=widget_label (bas2,value=string(replicate(32b,40))                  ,xsize=270 ,font=ft_b_normal)

		if CODENS gt 0 then begin
		  etxt="basd= widget_draw(basid, GRAPHICS_LEVEL=2,UVALUE='DRAW', RETAIN=0 ,"+ $
			                 "XSIZE=bst_x+bobol, YSIZE=bst_y, /BUTTON_EVENTS  ,"+ $
			                 "/EXPOSE_EVENTS,/frame)"
			                                        ii=EXECUTE(etxt) & endif else $
		if (bst_x eq xi) and (bst_y eq yi) then $
	     		basd= widget_draw(basid,retain=2,/frame,xsize=bst_x+bobol,ysize=bst_y,$
	     							/button_event,/motion_event)  $
	     	else	basd= widget_draw(basid,retain=2,/frame, /button_event,/motion_event, $
			   		  xsize=bst_x+bobol,ysize=bst_y,x_scroll_size=xi,y_scroll_size=yi)
   		bid=sys_dep      ('DYNLAB',basid,0)
   		
		widget_control,bad_id=i,   basid,group_leader=lamp_b1, /realize & put_logo
		widget_control,bad_id=i,   basd , get_value=basw

		if CODENS gt 0 then basw=-basd
		if CODENS gt 0 then widget_control,bad_id=i, bas13, sensitive=0
		if wonly(0) eq 0 then $
		widget_control,bad_id=i, byd  , set_button=1
		widget_control,bad_id=i, bas13, set_uvalue=[-88,346,basw]
		widget_control,bad_id=i, bas14, set_uvalue=[-88,350,basw ,bas15,fix(string_w),siz(0),basid,bas_x,basd]
		widget_control,bad_id=i, basid, tlb_get_size=v2  &  v2=long(v2)
		widget_control,bad_id=i, basid, set_uvalue=[-88,308,basid,basd ,basw,bst_x+bobol,bst_y,$
								v2(0),v2(1),baslb,pixrx,pixry,basmo,fix(string_w)]

	        XMANAGER, 'Beside', basid, event_handler='LAMP_EVENT_PARSER',/just_reg
		wbeside=[basid,basd,basw,0,0,baslb,basmo]
	       endif
	       wbeside(3)=bst_x & wbeside(4)=bst_y
	  endif
	  if !D.name eq 'Z' then begin device,set_resolution=[bst_x+bobol,bst_y] & erase
	  endif else if wbeside(2) gt 0 then wset,wbeside(2)
	endif
;Large dimensions
	if (siz(0) ge 2) and (CODENS eq 0) then begin
	   xi=siz(1) & yi=siz(2)
;	   Image...
	   if   did_repr(0) eq 1 then  $
	        if  (xi gt bst_x) or (yi gt bst_y) then begin
			xo= float(xi)/bst_x
			yo= float(yi)/bst_y
			if yo ge xo then fx=yo else fx=xo
	        	if xo gt .5 then xi=fix(xi/fx)
	        	if yo gt .5 then yi=fix(yi/fx)
	        	endif
;	   Contour.
	   if  (did_repr(1) eq 1) or (did_repr(0)*did_repr(6)*uxy eq 1) then begin
			if use_scan eq 1 then maxvol=1000 else maxvol=512
			    xo=xi/maxvol
			    yo=yi/maxvol
			    if xo gt 0 then xi=xi/xo
			    if yo gt 0 then yi=yi/yo
	   endif
;	   Other...
	   if  (xi ge bst_x) and (yi ge bst_y) then begin
		xo=float(xi)/bst_x
		yo=float(yi)/bst_y
		if yo ge xo then fx=yo else fx=xo
		if use_scan eq 1 then maxvol=2000 else maxvol=1000
		if (xi gt maxvol) and (yi gt maxvol) $
				  and (did_repr(1)+did_repr(2) gt 0) then fx=fx*2
	        if xo gt .5 then xi=fix(xi/fx)
	        if yo gt .5 then yi=fix(yi/fx)
		endif

	   xi=xi>2
	   yi=yi>2

	   if  (xi ne siz(1)) or (yi ne siz(2)) then begin
		if wnumber eq '0' then st='temporary(w0)' $
		                  else st='w'+wnumber

	   	if siz(0) eq 2  then i = execute('w0=congrid('+st + ',xi,yi)' )
	   	if siz(0) eq 3  then i = execute('w0=congrid('+st + ',xi,yi,siz(3))' )

		if sizx(0) eq 2 then xx= congrid(temporary(xx),xi,yi) else if xi ne siz(1) then xx= congrid(xx,xi)
		if sizy(0) eq 2 then yy= congrid(temporary(yy),xi,yi) else if yi ne siz(2) then yy= congrid(yy,yi)

		siz=size(w0)
		wnumber= '0'
		endif
;	   Smooth Contour.
	   if  ((did_repr(1) eq 1) and (sizx(0) lt 2)) or (did_repr(0)*did_repr(6)*uxy eq 1) then $
	   	if (xi gt 50) and (yi gt 50) and (use_scan eq 0) then begin
		    if wnumber eq '0' then st='temporary(w0)' $
		                      else st='w'+wnumber
		    if sys_dep('VERSION') lt 4.0 then edg='' else edg=',/edge'
		    i = execute('w0=smooth('+st + ',3'+edg+')' )
		    siz=size(w0)
		    wnumber= '0'
	       endif
	endif
;Max value !
	thresh=-99.
	mxv2  = 0.
	if (did_repr(9) gt 0) or (vff(2) eq 1) then begin mxv2=-99.
				 on_ioerror,mismx
				 if (l_message gt 0) and (vff(2) eq 0) then begin
				     widget_control,bad_id=i,did_repr(9),get_value=smxv & smxv=smxv(0)
				     mxv=0. & READS,smxv+' -99 -99 ' ,mxv,mxv2
				 endif else if vff(11) eq 0 then mxv=w_max(idn) else mxv=vff(11)
				 if CODENS gt 0 then  thresh=mxv else $
				 if mxv ne -99 then begin
				    thresh=mxv
				    if (siz(0) ne 3) or (not isurf) then begin
					if CODENS eq -1  then begin id=0L
					  if mxv2 ne -99 then begin mxv1=mxv
					  endif else begin mxv =(w_max(idn)-w_min(idn))/50.
							   mxv1=thresh-mxv & mxv2=thresh+mxv & endelse
					  i  =execute( 'id=where((w' + wnumber  +' le mxv2)'$
							  + 'and (w' + wnumber  +' ge mxv1))>0' )
					  i  =execute( 'w0=w' + wnumber  +'(id)' )
					  xx =xx(id)    & yy=yy(id) & zz=zz(id)
					  siz=size(w0) & wnumber= '0'
					endif else if siz(0) eq 1 then begin
					  if  keyrangy eq '' then $
					  if  mxv2 eq -99 then keyrangy=',yrange=[my1,mxv]' $
					                  else keyrangy=',yrange=[mxv,mxv2]'
					endif else begin
					  if wnumber eq '0' then st='temporary(w0)' $
					                    else st='w'+wnumber
					  if (mxv2 ne -99)  then $
						i  =execute( 'w0='+st +' > mxv < mxv2' ) $
					  else  i  =execute( 'w0='+st +' < mxv' )
					  if  keyrangz eq '' then begin
					   if mxv2  eq -99 then begin mxv2=mxv & mxv=w_min(idn) & endif
					   if (mxv2 gt w_max(idn)) or (mxv lt w_min(idn)) then  $
					   	!Z.range=[mxv,mxv2]
					  endif
					  siz=size(w0)
					  wnumber= '0'
					endelse
				    endif else if did_repr(5) eq 1 then thresh=alog(thresh)
				 endif
				 mismx:
	endif

;Stretch Z !
	if CODENS eq -1 then begin mx1=min(xx,max=mx2) & my1=min(yy,max=my2) & mz1=min(zz,max=mz2)
	   cdbox=[ [mx2,my2,mz1],[mx2,my1,mz1],[mx2,my1,mz2],[mx2,my2,mz2],[mx2,my2,mz1],[mx1,my2,mz1],$
		   [mx1,my2,mz2],[mx1,my1,mz2],[mx2,my1,mz2],[mx2,my2,mz2],[mx1,my2,mz2]] & endif

;Log !
	if (did_repr(5) eq 1) and (CODENS eq 0) then begin
		if wnumber eq '0'  then st='temporary(w0)' $
		                   else st='w'+wnumber
		if siz(0) eq 1     then i =execute( 'w0=alog10('+st +' > 1E-9)')  else $
		if w_min(idn) gt 0 then i =execute( 'w0=alog  ('+st +' )' )       $
				   else i =execute( 'w0=alog  ('+st +' > 0.001)')
		if n_elements(ee)  gt 1    then      ee=0

		siz=size(w0)
		wnumber= '0'

		if keyrangy ne '' then begin if siz(0) eq 1 then my1=alog10(my1 > 1E-9) else my1=alog(my1 > 0.001)
					     if siz(0) eq 1 then my2=alog10(my2 > 1E-9) else my2=alog(my2 > 0.001)
					     keyrangy=',yrange=[my1,my2]' & endif
	endif
;***
;end redraw=0
;**********
       endif else begin if fu_out eq 0 then bst_x=did_x else bst_x=wbeside(3)
			if fu_out eq 0 then bst_y=did_y else bst_y=wbeside(4) & endelse
;***
       l_mess=l_message
       if b_labins(6) then if b_labins(7) gt 0 then l_mess=b_labins(7)
;***
       if (not htm_ok) and (not liveC) and (not liveA) then begin

	if fu_out eq 0 then b_cur=0         else b_cur=wbeside(0)
	if fu_out eq 0 then d_cur=did_wd    else d_cur=wbeside(1) & if (CODENS gt 0) and (fu_out eq 0) then d_cur=did_we
	if fu_out eq 0 then w_cur=did_win0  else w_cur=wbeside(2)
	if fu_out eq 0 then baslb=l_mess    else baslb=wbeside(5)
	xo=off60 & yo=off60 & xof=xo/2 & yof=yo/2
	w_order=0
;Plot
;----
	poskey  =',position=[xo,yo,bst_x-xof,bst_y-yof]*f_dps,/device'
	poskeyPS='' & f_dps=1
	wplot   =!D.name
	errps   =0
	on_ioerror,IfPsErr
	if (ps_ok) and (CODENS le 0) then begin
		x_sx =7.  & y_sy = 9.5  &  xoff=x_sx/15. & yoff=y_sy*3./40
		x_sps=x_sx & y_sps= y_sy
		if did_fu eq 0 then y_sps=y_sps/2. $
		else begin
			if bst_x+bobol ge bst_y then begin
				y_sps=x_sps*bst_y/bst_x
			endif else begin  tmp  =y_sps*bst_x/bst_y & ttm=(tmp-x_sps)>0
				x_sps=tmp-ttm & y_sps=y_sps-ttm   & endelse
		endelse
		poskeyPS=poskey
		psFile  ='lamp.ps'
		if strpos(w_numor(0),'.ps') gt 0 then psFile=w_numor(0) else psFile='lamp.ps'
		w_numor(0)=''
		errps =1
		nc =256
		keepcol=!P.COLOR
		popo   =!P.COLOR
		popi   =!P.BACKGROUND
		if b_labins(3) ne 1 then tvlct ,  cur_r    ,          cur_g    ,          cur_b ,/get
		set_plot,'PS'
		if b_labins(3) ne 1 then begin
			pos_r=congrid(cur_r,nc) & pos_g=congrid(cur_g,nc) & pos_b=congrid(cur_b,nc)
			popo=min(fix(pos_r)+fix(pos_g)+fix(pos_b))
			popo=!C
			popi=max(fix(pos_r)+fix(pos_g)+fix(pos_b))
			popi=!C
			endif

		!P.COLOR=popo
		!P.BACKGROUND=popi
		lan=0
		if (did_fu ne 0) and (bst_x+bobol ge bst_y) then begin lan=1
		  device,xsize=x_sps,ysize=y_sps, yoffset=y_sy+yoff ,xoffset=xoff ,/inches,/landscape & pdph= x_sps-xoff+.5
		endif else begin
   		  device,xsize=x_sps,ysize=y_sps, yoffset=yoff      ,xoffset=xoff ,/inches,/portrait  & Pdph= y_sps+.5
		endelse
   		device,bits_per_pixel=8,/color
		device,filename=psFile
		if b_labins(3) ne 1 then tvlct ,  pos_r    ,  pos_g    ,  pos_b

		f_dps  =min([x_sps/bst_x , y_sps/bst_y]) *2.4 * !d.x_px_cm
		;fifix  =bst_x*f_dps*2.512/300. & if fifix gt 1 then fifix=fix(fifix)
		;f_dps  =fifix*300. /2.512/bst_x
		dd1    =convert_coord([0,1,1.,0],[0.,0,1,1],/normal,/to_device)
		bst_x  =round(dd1(0,1)/f_dps) ;**bst_x=round((x_sps*2.4 * !d.x_px_cm)/f_dps)
		bst_y  =round(dd1(1,2)/f_dps) ;**bst_y=round((y_sps*2.4 * !d.x_px_cm)/f_dps)
	endif
	if wonly(0) eq 0 then $
	if CODENS ne 0 then begin
	 if (liveT ne 0) then begin  liveT=0
		if wnumber ne '0' then ii=execute('w0=w'+wnumber)
		ii=execute('live_lamp_dens, rrr, xx,yy,zz,w0, thresh=thresh, box=cdbox,'+ $
	                       'xrange=[mx1,mx2], yrange=[my1,my2], zrange=[mz1,mz2],ax=rx,az=rz')
			       
	 endif else if CODENS eq -1 then begin
	   if mx1 eq mx2 then mx2=max (xx ,min=mx1)
	   if my1 eq my2 then my2=max (yy ,min=my1)
	   scale3 ,             xrange=[mx1,mx2], yrange=[my1,my2], zrange=[mz1,mz2],ax=rx,az=rz
			;t3d, tr=[-.5,-.5,-.5] ,rot=[ 0. , rz , 0. ]
			;t3d,                   rot=[ rx , 0. , 0. ]
			;t3d, tr=[+.5,+.5,+.5]
	   surface,fltarr(2,2), xrange=[mx1,mx2], yrange=[my1,my2], zrange=[mz1,mz2],/nodata,/t3d
	   s =(50000./n_elements(xx)/6)>1
	   pk=4   &   if s lt 9 then pk=3   &   s=round(s/2)>1<3
	   if (thresh ne -99) and (mxv2 eq 0.) then plots,xx,yy,zz,psym=pk,symsiz=s,/t3d $
	   else begin
		if thresh ne -99 then begin ji=10. & mxv= (mxv2 - thresh)	     & p=thresh
		endif		 else begin ji=50. & mxv= (w_max(idn)-w_min(idn)) & p=w_min(idn) & endelse
		mxk=mxv /ji & if mxk eq 0 then ji=1
		              if mxk ne 0 then mxv=mxk
		col=220./ji
		for i=1,ji do begin id=0L
		    ii=execute('id=where((w'+wnumber+' ge p) and (w'+wnumber+' le p+mxv))')
		    if id(0) ge 0 then plots,xx(id),yy(id),zz(id),psym=pk,symsiz=s,/t3d,color=col*i
		    p =p+mxv
		    if RDSTOP(1,ji,(i)) then i=ji+1
		endfor
	   endelse
	   plots,cdbox,/t3d
	   
	 endif else begin
	 	if fu_out eq 0 then begin odim=[did_x,did_y]  & omen=did_4dM    & endif
	 	if fu_out ne 0 then begin odim=[bst_x,bst_y]  & omen=wbeside(6) & endif
	 	if rrr    eq 7 then begin ps='VRML' & rrr=6   & endif else ps='' ;but fu_out=0!!!!!
	 	if ps_ok       then begin ps='PS'   & ps_ok=0 & endif
	 	
		txt='liv_objet,WDRAW=d_cur,DIM=odim,MENUBAS=omen,L_MESS=baslb,WI=idn,PS=ps'+$
		             ',XX=xx,YY=yy,ZZ=zz,WW=w'+wnumber+',THRESH=thresh,REPR=did_repr,EE=ee,NN=nn,VV=vv'
		ii = EXECUTE(txt)
	 endelse
	endif else $
	if siz(0)   eq 1 then begin
		keepcol=!P.COLOR
		if not ps_ok then !P.COLOR=0
		keywrd=',yticklen=1.,ygridstyle=1,background=255,charsize=1.2'
		wnumbxy='w'+wnumber & wxx=''
		wxx='xx,'

		if liveT ne 0 then begin
			i=execute('live_lamp_plot,liveT,xx,'+wnumbxy +keyrangx+keyrangy)
		endif else if n_elements(ee) le 1 then begin
			i=execute(' plot,' + wxx+wnumbxy +keywrd+poskey+keyrangx+keyrangy)
		endif else begin
			m=max(ee) & mq=sqrt(w_max(idn))
			if  m*(w_max(idn)+1) eq mq then opp1='*(1+ee)' else opp1='+ee'
			if  m*(w_max(idn)+1) eq mq then opp2='*(1-ee)' else opp2='-ee'
			if bst_x/siz(1) gt 3 then keywrd=keywrd+',psym=2' $
					     else keywrd=keywrd+',psym=3'
			i=execute(' plot,' + wxx+wnumbxy +keywrd+poskey+keyrangx+keyrangy)

			if bst_x/siz(1) ge 6 then begin
			i=execute('oplot,' + wxx+wnumbxy+opp1+',linestyle=1')
			i=execute('oplot,' + wxx+wnumbxy+opp2+',linestyle=1')
			endif
			i=execute('errplot,' + wxx + wnumbxy+opp2+','+wnumbxy+opp1)
		endelse
		!P.COLOR=keepcol
;Display
;-------
	endif else begin
;	   Use Scan
;	   --- ----
	   if use_scan eq 1 then begin
	   			if fu_out eq 0 then $
	   			   sl_lampscan, 'set_size', bst_x     ,bst_y     ,d_cur,lamp_b1 ,0
	   			if fu_out ne 0 then $
	   			   sl_lampscan, 'set_size', wbeside(3),wbeside(4),d_cur,lamp_b1 ,0
	   			f_fg=[-1,-1]
	   			if did_repr(1) eq 1 then begin
	   				if styles(1,0) eq 1 then f_fg(1)=10 else $
	   				if styles(1,0) eq 2 then f_fg(1)=11
	   				endif
	   			if did_repr(2) eq 1 then begin
	   				if styles(0,0) eq 1 then f_fg(0)=1
;	   				if styles(0,0) eq 2 then f_fg(0)=8  else $
	   				if styles(0,0) eq 3 then f_fg(0)=7  else $
	   				if styles(0,0) eq 4 then f_fg(0)=3  else $
	   				if styles(0,0) eq 5 then f_fg(0)=4  else $
	   				if styles(0,0) eq 6 then f_fg(0)=3
	   				endif
				spm=4
	   			if (styles(2,0) eq 3) then if siz(0) eq 3 then spc=-30 $
	   								  else spm= 6  else $
	   			if (styles(2,0) eq 4)  and    siz(0) eq 3 then spc=-20
				if  spc ge 0 then spc=spc+spm else spc=spc-spm
				if  rz eq 0 then rz=-1

			   	sl_lampscan, 'set_params', rx,rz,nlv ,f_fg
				if rz eq -1 then rz=0
				flg='views'
			      if siz(siz(0)+1) le 6 then $ ;+newtypes*******
			   	i =execute( 'sl_lampscan, flg ,w' + wnumber + ',spc') else $
			        i =execute( 'sl_lampscan, flg ,float(w' + wnumber + '),spc')
				xo=0 & yo=0 & xof=0 & yof=0
				!p.font = 0
				w_order=!order
				set_xy
				!p.title=w_tit(idn)
				!p.subtitle=''
				if !D.name ne 'Z' then if w_cur gt 0 then wset,w_cur
				plot,[0,0],/nodata,xstyle=4,ystyle=4,/noerase
	   endif else $
	   if (siz(0) eq 2) or ((siz(0) eq 3) and (did_repr(2)+did_repr(1) eq 0)) then begin
;	   Use Idl
;	   --- ---

		if (siz(0) eq 3) then begin
					    i =execute('w0=total(w'+wnumber+ ',3)' )
					    siz=size(w0)
					    wnumber='0'
					    endif
		axkey=''
		wnumbxy=wnumber+',xx,yy'
	   	if  uxy eq 0  then begin if (rrr eq 0) or (axy ne 1) then wnumbxy=wnumber
					    xs=siz(1)-1. & ys=siz(2)-1.
					    xdx=[0,xs/4.,xs/2.,xs*3./4.,xs] & sxx=strarr(5)
					    ydx=[0,ys/4.,ys/2.,ys*3./4.,ys] & syy=strarr(5)
					    for i=0,4 do begin
						te=fix(xdx(i)) & td=round((xdx(i)-te)*10.)/10.
						a =string( xx(  te)*(1.-td) + xx(  (te+1)<xs)*td)
						j =strlen(a)-1 & while (j gt 0) and (j eq strpos(a,'0',j)) do j=j-1
						sxx(i)=strmid (a,0,j+1)
						sxx   =strtrim(sxx,2)
						te=fix(ydx(i)) & td=round((ydx(i)-te)*10.)/10.
						if (size(yy))(0) le 1 then $
						a =string( yy(  te)*(1.-td) + yy(  (te+1)<ys)*td) else $
						a =string( yy(0,te)*(1.-td) + yy(0,(te+1)<ys)*td)
						j =strlen(a)-1 & while (j gt 0) and (j eq strpos(a,'0',j)) do j=j-1
						syy(i)=strmid (a,0,j+1)
						syy   =strtrim(syy,2)  
					    endfor

					                        scalex=float([sxx(0),sxx(4)])
					    if !order eq 0 then scaley=float([syy(0),syy(4)])  $
					                   else scaley=float([syy(4),syy(0)])

					    if total(long(scalex)) eq total(scalex) then ent=1 else ent=0 ;!!!
					    typ=size(xx) & typ=typ(typ(0)+1)
					    if (typ le 3) and (rrr ne 0) and (axy eq 1) then begin
					    		sxx=round(float(sxx))
							tickx=',xticks=4,xtickv=sxx'
					    endif else	tickx=',xticks=4,xtickname=sxx'

					    if total(long(scaley)) eq total(scaley) then ent=1 else ent=0 ;!!!
					    typ=size(yy) & typ=typ(typ(0)+1)
					    if (typ le 3) and (rrr ne 0) and (axy eq 1) then begin
							syy=round(float(syy))
							ticky=',yticks=4,ytickv=syy'
					    endif else	ticky=',yticks=4,ytickname=syy'
					    
					    wnumbxy=wnumbxy+tickx+ticky
					    axkey  =axkey  +tickx+ticky
		endif

		noaxe=',xstyle=5,ystyle=5,zstyle=5'
		if did_repr(14) eq 1 then begin wnumbxy=wnumbxy+noaxe & noaxe='' & endif
		if did_repr(2)  eq 1 then begin
		 surfstyl='shade_surf' & surfkey=''
		 if styles(0,0) eq 1 then begin if redraw eq 0 then  i=execute('w4d=bytscl(w'+wnumber+')')
		 						     surfkey=',shades=w4d'  & endif
		 if styles(0,0) eq 2 then begin
		 			if redraw eq 0 then begin
		 			 siw=size(w10)
				         if siw(0) ne 2 then i=execute('w10=w'+wnumber) $
				         else if (siw(1) ne isiz(1)) and (siw(2) ne isiz(2)) then $
							 w10=congrid(temporary(w10),isiz(1),isiz(2))
					 w4d=w10(vfl(0):vfl(1) , vfl(2):vfl(3))
		 			 siw=size(w4d)
				         if (siw(1) ne siz(1)) and (siw(2) ne siz(2)) then $
							 w4d=congrid(temporary(w4d), siz(1), siz(2))
					 if (thresh ne -99)    then  w4d=temporary(w4d) < mxv
					 if (did_repr(5) eq 1) then  w4d=alog(temporary(w4d) > 0.001)
					 w4d=bytscl(temporary(w4d))
					endif
								     surfkey=',shades=w4d'  & endif
		 if styles(0,0) eq 4 then begin
		 				surfstyl='surface' & surfkey=''		    & endif
		 if styles(0,0) eq 5 then begin if redraw eq 0 then  i=execute('w4d=bytscl(w'+wnumber+')')
		 				surfstyl='surface' & surfkey=',/lego,shades= w4d' & endif
		 if styles(0,0) eq 6 then begin
		 				surfstyl='surface' & surfkey=',/horizontal' & endif

		 if ps_ok then axsup=',charsize=2. ,zticks=4,font=-1,xtitle=x_tit(idn),ytitle=y_tit(idn),ztitle=z_tit(idn)' $
		          else axsup=',charsize=1.2,zticks=3'
		 surfkey=surfkey+axsup
		 axkey  =axkey  +axsup

		 if siz(1)*siz(2) gt long(128)*64 then horz=',/horizontal' else horz=''

		 if ((rz ge -90.) and (rz le  90.)) or ((rz le -270.)  or (rz ge  270.)) then pvx=0 else pvx=1
		 if ((rz ge   0.) and (rz le 180.)) or ((rz le -180.))                   then pvy=0 else pvy=1
		 if ((rz ge   0.) and (rz le  90.)) or ((rz le -270.))                   then pvz=2 else $
		 if ((rz gt  90.) and (rz le 180.)) or ((rz le -180.) and (rz gt -270.)) then pvz=3 else $
		 if ((rz gt 180.) and (rz le 270.)) or ((rz le - 90.) and (rz gt -180.)) then pvz=0 else $
		 if ((rz gt 270.) and (rz lt 360.)) or ((rz lt    0.) and (rz gt - 90.)) then pvz=1 else pvz=2
		 
		;if ((pvz eq 2) or (pvz eq 1)) and (ps_ok) then axkey=axkey+',font=-1'
		endif
				winx=bst_x	   &   winy=bst_y
				if rrr eq 0 then begin winx=winx-off90 & winy=winy-off90 & endif
				xi= winx/siz(1)    &   yi=winy/siz(2)
				if (xi eq 0) then xi= -(float(siz(1))/winx)
				if (yi eq 0) then yi= -(float(siz(2))/winy)
				if  xi le -1 then fx= -1./xi else fx=xi
				if  yi le -1 then fy= -1./yi else fy=yi
				fm= min([fx,fy])
				if fu_out eq 0 then begin fx=fm & fy=fm & endif
				xi= fix(siz(1)*fx)
				yi= fix(siz(2)*fy)
				if xi/yi gt 4 then yi=xi/4 else $
				if yi/xi gt 4 then xi=yi/4

				if winx lt xi then xi=winx
				if winy lt yi then yi=winy
;		rrr=0 image
;		-----------
		if rrr eq 0 then  begin
				w_order=!order
				xo=(winx-xi)/2 + off60 & xof=bst_x-xi-xo	;prev 55
				yo=(winy-yi)/2 + off60 & yof=bst_y-yi-yo	;prev 45
						     rangex=[vfl(0),vfl(1)]-vfl(0)
				if w_order eq 0 then rangey=[vfl(2),vfl(3)]-vfl(2) $
						else rangey=[vfl(3),vfl(2)]-vfl(3)

				if total(long(scalex)+long(scaley)) eq total(scalex+scaley) then $
				   if axy eq 1 then wnumbxy=wnumber
				axy=1
				if ((xi ne siz(1)) or (yi ne siz(2))) then begin
					     if wnumber eq '0' then st='temporary(w0)' $
					     else st='w'+wnumber
					     i= execute('w0=congrid('+st + ',xi,yi)' )
					     if (fm ge 5) and (smoo eq 1) then begin
						if sys_dep('VERSION') lt 4.0 then edg='' else edg=',/edge'
						i=execute('w0=smooth(temporary(w0),fix(fm-2)<9'+edg+')') & endif
				  if liveT ne 0 then $
				   i= execute('live_lamp_img,liveT,w0,xrange=scalex,yrange=scaley')  $
				  else begin erase
				   if not ps_ok then tvscl,w0,xo,yo $
				   		else tvscl,w0,xo*f_dps,yo*f_dps,xsize=(xi)*f_dps,ysize=(yi)*f_dps
				   i= execute('plot,w'+wnumbxy+ ',charsize=1.2 ,xrange=scalex,yrange=scaley'+  $
					      ',/nodata,position=[xo,yo,xo+xi-1,yo+yi-1]*f_dps ,/device,/noerase')
				  endelse
				endif else   begin
				  if liveT ne 0 then $
				   i= execute('live_lamp_img,liveT,w'+wnumber+',xrange=scalex,yrange=scaley')  $
				  else begin erase
				   if not ps_ok then didi="" else didi=",xsize=(xi)*f_dps,ysize=(yi)*f_dps"
				   i= execute('tvscl,     w'+ wnumber + ',xo*f_dps  , yo*f_dps'+didi )
				   i= execute('plot,w'+wnumbxy+ ',charsize=1.2 ,xrange=scalex,yrange=scaley'+  $
					      ',/nodata,position=[xo,yo,xo+xi-1,yo+yi-1]*f_dps,/noerase,/device')
				  endelse
				endelse
		endif

		if (liveT ne 0) and (rrr ge 1) then begin
				i=execute('live_lamp_surf,liveT,xx,yy,w'+ wnumber +$
					   ',az=rz,ax=rx,rrr=rrr,style=styles')
		endif else begin

;		rrr=1 image + surface
;		---------------------
		if rrr eq 1 then  begin
				i=execute( 'surface,   w' + wnumbxy + ',az=rz,ax=rx,/save,/nodata'+poskeyPS+noaxe )
				i=execute( 'contour   ,w' + wnumbxy + ',/fill' + $
						     ',/noerase,/t3d,zvalue=0.   ,nlevels=nlv'    +poskeyPS+noaxe )
				i=execute( 'surface,   w' + wnumbxy + ',az=rz,ax=rx,/noerase,' +$
									'bottom=80' +horz         +poskeyPS+noaxe )
				if did_repr(14) eq 0 then begin
				  i=execute('AXIS,xaxis=pvx,/t3d'+ axkey)
				  i=execute('AXIS,yaxis=pvy,/t3d'+ axkey)
				  i=execute('AXIS,zaxis=pvz,/t3d'+ axkey)
				endif
				endif
;		rrr=2 image + contour
;		---------------------
		if rrr eq 2 then  begin
				i=execute( 'contour   ,w' + wnumbxy + ',/fill  ,nlevels=nlv' +poskey)
				if (fu_out eq 1) and (did_repr(14) eq 0) then begin
				   wmin =w_min(idn) & wmax=w_max(idn) & i=execute('wmin=min(w'+wnumber+',max=wmax)')
				   if did_repr(5) eq 1 then begin wmin=exp(wmin) & wmax=exp(wmax) & endif
				   bn2  =15 & bn1 =(bst_y-yof-yo)
				   bobo =findgen(bn1)/(bn1-1)*(wmax-wmin)
				   if did_repr(5) eq 1 then bobo=alog(bobo > 0.001)
				   bobox=fltarr(bn1,bn2)  & for i=0,bn2-1 do bobox(0,i)=bobo
				   posx =(bst_x-xof+bn2+bn2)
				   posy =(yo)
				   bobox=transpose(bobox)
				   if (    ps_ok) then tvscl,bobox,posx*f_dps,posy*f_dps,xsize=bn2*f_dps,ysize=bn1*f_dps
				   if (not ps_ok) then tvscl,bobox,posx*f_dps,posy*f_dps
				   wmix=wmax-wmin
				   xyouts,(posx+bn2+3)*f_dps, posy         *f_dps,strtrim(string(wmin)        ,2),/device
				   xyouts,(posx+bn2+3)*f_dps,(posy+  bn1/4)*f_dps,strtrim(string(wmin+wmix/4.),2),/device
				   xyouts,(posx+bn2+3)*f_dps,(posy+  bn1/2)*f_dps,strtrim(string(wmin+wmix/2.),2),/device
				   xyouts,(posx+bn2+3)*f_dps,(posy+3*bn1/4)*f_dps,strtrim(string(wmax-wmix/4.),2),/device
				   xyouts,(posx+bn2+3)*f_dps,(posy+  bn1)  *f_dps,strtrim(string(wmax)        ,2),/device
				endif
				if uxy eq 0 then axy=1
				endif

;		rrr=3 image + contour + surface
;		-------------------------------
		if rrr eq 3 then  begin
				col=(indgen(nlv/2)+1)*10 + 50
				i=execute( 'surface,   w' + wnumbxy + ',az=rz,ax=rx,/save,/nodata'+poskeyPS+noaxe )
				i=execute( 'contour   ,w' + wnumbxy + ',/fill' + $
						     ',/noerase,/t3d,zvalue=0.   ,nlevels=nlv'    +poskeyPS+noaxe )
				i=execute( 'surface,   w' + wnumbxy + ',az=rz,ax=rx,/noerase,' +$
									'/t3d,bottom=80' +horz    +poskeyPS+noaxe )
				if did_repr(14) eq 0 then begin
				  i=execute('AXIS,xaxis=pvx,/t3d'+ axkey)
				  i=execute('AXIS,yaxis=pvy,/t3d'+ axkey)
				  i=execute('AXIS,zaxis=pvz,/t3d'+ axkey)
				endif
				i=execute( 'contour   ,w' + wnumbxy + ',c_colors=col' +$
						     ',/noerase,/t3d,zvalue=1.   ,nlevels=nlv/2'  +poskeyPS+noaxe )
				endif
;		rrr=4 contour +  surface
;		------------------------
		if rrr eq 4 then  begin
				col=(indgen(nlv/2)+1)*10  + 50
				i=execute( surfstyl+ ',w' + wnumbxy + ',az=rz,ax=rx,/save'+surfkey+poskeyPS+noaxe )
				if did_repr(14) eq 0 then begin
				  i=execute('AXIS,xaxis=pvx,/t3d'+ axkey)
				  i=execute('AXIS,yaxis=pvy,/t3d'+ axkey)
				  i=execute('AXIS,zaxis=pvz,/t3d'+ axkey)
				endif
				i=execute( 'contour   ,w' + wnumbxy + ',c_colors=col' +$
						     ',/noerase,/t3d,zvalue=1.   ,nlevels=nlv/2'  +poskeyPS+noaxe )
				endif
;		rrr=5 contour
;		-------------
		if rrr eq 5 then  begin
			if styles(1,0) eq 1 then begin
				c_lab=round(abs(sin(indgen(nlv)*!pi/2.)))
				c_lin=round(findgen(nlv)/(nlv-1)*5)
				c_ll =[1,4,3,2,5,0]
				for i=0,nlv-1 do c_lin(i)=c_ll(c_lin(i))
				i=execute( 'contour   ,w' + wnumbxy + ',/follow,nlevels=nlv,charsize=1.5,c_linestyle=c_lin'+$
					   ',font=-1,charthick=2.,c_labels=c_lab'+poskey)
			endif else begin
				col=(indgen(nlv)+1)*(180/nlv) + 50
				i=execute( 'contour   ,w' + wnumbxy + ',c_colors=col,nlevels=nlv' +poskey)
			endelse
			if uxy eq 0 then axy=1
		endif
;		rrr=6 surface
;		-------------
		if rrr eq 6 then  begin
				i=execute( surfstyl+ ',w' + wnumbxy + ',az=rz,ax=rx,/save'+surfkey +poskeyPS+noaxe )
				if did_repr(14) eq 0 then begin
				  i=execute('AXIS,xaxis=pvx,/t3d'+ axkey)
				  i=execute('AXIS,yaxis=pvy,/t3d'+ axkey)
				  i=execute('AXIS,zaxis=pvz,/t3d'+ axkey)
				endif
			    endif
;		rrr=7 VRML
;		----------
		if rrr eq 7 then  begin if uxy eq 1 then wnumbxy=wnumber+',xx,yy' else wnumbxy=wnumber
					if styles(0,0) eq 1 then pol=',/poly' else pol=''
					i=execute('matovr,w'+wnumbxy+pol)
					if b_labins(3) ne 2 then i=sys_dep('VIEWER','lamp.wrl')
					endif
		endelse
	   endif else if (siz(0) eq 3) and (did_repr(2)+did_repr(1) ge 1)  then begin
		kpcol=!p.color
		isurf =1
		if liveT ne 0 then begin
			i=execute('live_lamp_vol,liveT,w'+wnumber+',thresh=thresh,az=rz,ax=rx,rrr=rrr,name=w'+string_w)
		endif else begin

		if redraw eq 0 then begin
		   mini=w_min(idn) & maxi=w_max(idn)
		   if did_repr(5) eq 1   then begin i=execute( 'W0=w'+wnumber+'>0' )
		                                    wnumber='0' & maxi=max(W0,min=mini) & endif
		   if thresh      eq -99 then thresh=mini + (maxi-mini)/3.
		   v=0 & p=0
		   if rrr eq 7  then begin if styles(0,0) lt 4 then txt=',/poly' else  txt=''
				     i=execute( 'matovr,w' + wnumber + ' , iso=thresh'+txt)
				     if b_labins(3) ne 2 then i=sys_dep('VIEWER','lamp.wrl')
		   endif	else begin
				     if wnumber ne '0' then begin W0 =0 & i=execute( 'W0=float(w'+wnumber+')')
				                       endif else W0 =float(W0)
				     WT = total(W0) & WI = 0.

				     if sys_dep('VERSION') lt 4.0 then edg='' else edg=',/edge'
				     if did_repr(6) eq 1 then begin
					i=execute('W0=smooth(temporary(W0),3'+edg+')')
					W0=W0*(WT/total(W0))
				     endif

				     if edg ne '' then begin
				     	i=execute('W0=transpose(W0,[2,0,1])')
					xtit=z_tit(idn) & ytit=x_tit(idn) & ztit=y_tit(idn) & xxr=zz & yyr=xx & zzr=yy
				     endif else begin
				        xtit=x_tit(idn) & ytit=y_tit(idn) & ztit=z_tit(idn) & xxr=xx & yyr=yy & zzr=zz
				     endelse
				     suz=size(W0)

				     shade_volume,W0,thresh,v,p
				     idx=  where (W0 ge thresh)
				     WN =  n_elements(idx)
				     WE =  n_elements(W0)
				     if WN gt 1 then begin WI=total(W0(idx))
							   if (did_repr(0) eq 0) and (did_repr(2) eq 1) then $
				     			   W0(idx)=-maxi*max([suz(1),suz(2),suz(3)]) & endif
				     WP0= (total(W0,3)>(mini*suz(3)))/suz(3)
				     WP1= (total(W0,2)>(mini*suz(2)))/suz(2)
				     WP2= (total(W0,1)>(mini*suz(1)))/suz(1)
				     W0 ={A:WP0,B:WP1,C:WP2,T:WT,N:WN,I:WI,E:WE,PXZ:total(WP0,2),PYZ:total(WP0,1),$
				          SIZ:suz,XTIT:xtit,YTIT:ytit,ZTIT:ztit,XXR:xxr,YYR:yyr,ZZR:zzr,TH:thresh}
				     endelse
		endif

		if n_elements(p) gt 3 then begin
		   tmp=fltarr(2,2)
		   col=(indgen(nlv/2)+1)*10 + 50
			scale3, xrange=[0,W0.siz(1)-1], yrange=[0,W0.siz(2)-1], zrange=[0,W0.siz(3)-1],ax=0.,az=0.
			t3d, tr=[-.5,-.5,-.5] ,rot=[ 0. , rz , 0. ]
			t3d,                   rot=[ rx , 0. , 0. ]
			t3d, tr=[+.5,+.5,+.5]
			kpn=!D.name
			if kpn ne wplot then set_plot,wplot
			set_shading,reject=0,values=[100,254]
			image=polyshade(v,p,/t3d,xsize=bst_x,ysize=bst_y)
			set_shading,reject=1
			if kpn ne wplot then set_plot,kpn
			!p.color=kpcol
			myproj,image,W0, smoo,f_dps, profile=did_repr(1), surf=did_repr(2)
			if did_repr(5) eq 1 then loo=' (Logn)' else loo=''
			other_tit(idn)='Threshold='+strtrim(string(thresh),2)+loo+'  Total volume:'+strtrim(string(W0.I),2)+loo + $
			               '  Npoints:'+strtrim(string(W0.N),2)
		endif

		endelse
	   endif
	endelse
       endif else begin
	  if wnumber  ne  '0'  then  w0=0
          if (htm_ok) then HtmW,idn, w0, xx, yy
          if (liveC)  then if siz(0) eq 2 then ii=execute('live_lamp_cont,w'+wnumber+',GROUP=lamp_b1,XX=xx,YY=yy,WI=idn'+ $
                                                             ',TIT=w_tit(idn),XTIT=x_tit(idn),YTIT=y_tit(idn)')
          if (liveA)  then if siz(0) eq 3 then ii=execute('live_lamp_anim,w'+wnumber+',GROUP=lamp_b1'+ $
							  ',TIT=w_tit(idn),smoo=smoo,surf=did_repr,az=rz,ax=rx')
       endelse

	if ps_ok then begin
		if (CODENS gt 0) then begin
			txt='liv_objet,WDRAW=d_cur,DIM=odim,MENUBAS=omen,L_MESS=baslb,WI=idn,PS="PS"'
			ii=EXECUTE(txt)
		endif else begin
			if b_labins(3) ne 1 then begin
				P_DID_PS_HEADER, pdph , idn ,psFile
				!P.COLOR=keepcol
				errps=0
			endif
			if errps eq 1 then begin errps=0 & device,/close_file & endif
			set_plot,wplot
			if  b_labins(3)  ne 1 then tvlct , cur_r , cur_g , cur_b
			widget_control,bad_id=i,l_mess,set_value= psFile+' updated ...'
		endelse
	endif
	IfPsErr:if errps eq 1 then begin errps=0 & device,/close_file & set_plot,wplot & endif

	if l_mess gt 0 then $
	if htm_ok	then widget_control,bad_id=i,l_mess,set_value='lamp.htm updated ...' else $
	if rrr eq 7	then widget_control,bad_id=i,l_mess,set_value='lamp.wrl updated ...'

	if (liveT eq 0) and (rrr ne 7) and (not htm_ok) and (not liveC) and (not liveA) then begin
	  if (CODENS eq -1) or (isurf) or (siz(0) eq 3)  then begin
		!p.title   =w_tit(idn)
		if (CODENS ne 0) or (isurf) then !p.subtitle=other_tit(idn)
		if (CODENS le 0) then plot,[0,0],/nodata,xstyle=4,ystyle=4,/noerase
	   	flgsurf    =[0,rz,idn]
		endif

	  if (not ps_ok) then begin
	     if (b_labins(3) ne 1) and (!D.name ne 'Z') then $
	     if (redraw eq 0) and (l_message gt 0) and (CODENS le 0) then begin
	      widget_control,bad_id=i,d_cur,set_uvalue=long ([-88,390,b_cur,fix(string_w),d_cur,w_cur,xo,yo,$
						       bst_x,bst_y,vfl(0),vfl(1),vfl(2),vfl(3),isurf,$
						       w_order,axy,baslb,xof,yof,did_repr(5)])
						       
	      if ((did_fu eq 0) and (GEORGE ne 0)) then WebDo,'dws',1,0,0
	     endif
	  endif

	  trap_current=w_cur
	  trap_x1=vfl(0) & trap_x2=vfl(1) & trap_y1=vfl(2) & trap_y2=vfl(3) & trap_ws=string_w

	  if ((b_labins(3) eq 2) or (gif_ok)) then begin
		if (CODENS gt 0) then begin
			txt='liv_objet,WDRAW=d_cur,DIM=odim,MENUBAS=omen,L_MESS=baslb,WI=idn,PS="GIF"'
			ii=EXECUTE(txt)
		endif else begin
			r=0 & buf=tvrdd(r,g,b)
			if png_ok then giffile='lamp.png' else giffile='lamp.gif'
			if n_elements(r) le 1 then WRITE_KIF,giffile,buf else WRITE_KIF,giffile,buf, r,g,b
			if gif_ok then widget_control,bad_id=i,l_mess  ,set_value=giffile+' updated ...'
		endelse
	  endif
	  if scr_ok then begin
	     if fu_out eq 0 then write_ps $
	     else begin
	     	widget_control,bad_id=i, bas_x,get_uvalue=kuv		;actual modop (screen,gif,ps)
	     	widget_control,bad_id=i, bas_x,set_uvalue= 1		;set to screen
		widget_control,bad_id=i, bas14,get_uvalue=scr_uv	;get uv of "print to" button
		p_did_event,0 ,scr_uv
	     	widget_control,bad_id=i, bas_x,set_uvalue=kuv		;restore modop
	     endelse
	  endif
	  if fu_out eq 1 then begin
	     if  (not ps_ok) and (not gif_ok) and (not scr_ok) and ((b_labins(3) eq 0)   or $
	        ((!D.name eq "X") and  (b_labins(3) eq 1))) or (turn gt 2) then widget_control,bad_id=i,wbeside(0),map=1 $
	     else if (new ne 0)   and (!D.name ne 'Z')                     then widget_control,bad_id=i,wbeside(0),/destroy
	  endif
	endif

	vfl(*)     =-1
	did_repr(1)= keeprp1
	!p.title   =''
	!p.subtitle=''
	!x.title   =''
	!y.title   =''
	!z.title   =''
	!z.range   = 0
    endif
return
end

PRO myreduce, idx, xx,yy,zz,ee,nn,vv
;** ********
		    if n_elements(ee) eq n_elements(xx) then ee=ee(idx)
		    if n_elements(nn) eq n_elements(xx) then nn=nn(idx)
		    if (size(vv))(0)  eq 1 then $
		    if n_elements(vv) eq n_elements(xx) then vv=vv(idx)
	    	    xx=xx(idx) & yy=yy(idx) & zz=zz(idx)

		    if (size(vv))(0)  eq 2 then begin
		        for i=0,(size(vv))(2)-1 do begin idv=where(idx eq vv(0,i)) & vv(0,i)=idv(0)
			       if idv(0) ge 0 then begin idv=where(idx eq vv(1,i)) & vv(1,i)=idv(0)
			                                 if idv(0) lt 0 then vv(0,i)=-1  & endif
			endfor
			idv=where(vv(0,*) ge 0) & if idv(0) ge 0 then vv=vv(*,idv) else vv=0
		    endif
end

PRO myproj , image ,W0 ,smoo, f_dps, profile=profile, surf=surf
;** ******
;**
;**image= byte shaded volume
;**W0.A = total(data,3)
;**W0.B = total(data,2)
;**W0.C = total(data,1)
;**smoo = smooth flag 1 or 0
;**f_dps= 1 or PS factor

sizm= size(image)
x   =(size(W0.A))(1) & y=(size(W0.A))(2) & z=(size(W0.B))(2)
surface,image,xrange=[0,x], yrange=[0,y], zrange=[0,z],/nodata,/t3d,/sav,$
        xmargin=[0,0],ymargin=[0,0]

k=x & i=y & x=x-1 & y=y-1 & z=z-1   & ym1=1 & ym2=1 & ym3=1
M=0 & MM1=0 & MM2=0 & MM3=0 & dd1=0 & dd2=0 & dd3=0

 coom=[[0,0,0],[0,i,0],[k,i,0],[k,0,0]]

 dd1=round(convert_coord([0,0,x,x],[0,y,y,0],[0,0,0,0],/data,/t3d,/to_device)/f_dps)
 ya =dd1(1,2)  + dd1(1,1)
 yb =dd1(1,3)  + dd1(1,0)
 yc =dd1(1,0)  + dd1(1,1)
 yd =dd1(1,2)  + dd1(1,3)
 if (dd1(1,0) eq dd1(1,2)) or (dd1(0,0) eq dd1(0,2)) then $
 if (dd1(1,0) eq dd1(1,1)) or (dd1(0,0) eq dd1(0,1)) then ym1=0

if ya gt yb then $
 dd2=round(convert_coord([0,0,x,x],[i,i,i,i],[0,z,z,0],/data,/t3d,/to_device)/f_dps)
if ya le yb then $
 dd2=round(convert_coord([0,0,x,x],[0,0,0,0],[0,z,z,0],/data,/t3d,/to_device)/f_dps)
if (dd2(1,0) eq dd2(1,2)) or (dd2(0,0) eq dd2(0,2)) then $
if (dd2(1,0) eq dd2(1,1)) or (dd2(0,0) eq dd2(0,1)) then ym2=0
if  dd2(1,0) gt dd2(1,1) then up =1 else up =0 ;We may redo dd1 with z=[z,z,z,z]

if yd gt yc then $
 dd3=round(convert_coord([k,k,k,k],[0,0,y,y],[0,z,z,0],/data,/t3d,/to_device)/f_dps)
if yd le yc then $
 dd3=round(convert_coord([0,0,0,0],[0,0,y,y],[0,z,z,0],/data,/t3d,/to_device)/f_dps)
if (dd3(1,0) eq dd3(1,2)) or (dd3(0,0) eq dd3(0,2)) then $
if (dd3(1,0) eq dd3(1,1)) or (dd3(0,0) eq dd3(0,1)) then ym3=0

if (profile and (not surf))  then begin ym1=0 & ym2=0 & ym3=0 & endif

if ym1 then begin
 XX1=reform(dd1(0,*)) & YY1=reform(dd1(1,*))
 XI =[0,0,x,x]
 YI =[0,y,y,0]
 POLYWARP, XI, YI, XX1, YY1, 1, KX, KY
 MM1= POLY_2D(sqrt(W0.A), KX, KY,smoo,sizm(1),sizm(2),missing=0)
endif

if ym2 then begin
 XX2=reform(dd2(0,*)) & YY2=reform(dd2(1,*))
 XI =[0,0,x,x]
 YI =[0,z,z,0]
 POLYWARP, XI, YI, XX2, YY2, 1, KX, KY
 MM2= POLY_2D(sqrt(W0.B), KX, KY,smoo,sizm(1),sizm(2),missing=0)
endif

if ym3 then begin
 XX3=reform(dd3(0,*)) & YY3=reform(dd3(1,*))
 XI =[0,0,y,y]
 YI =[0,z,z,0]
 POLYWARP, XI, YI, XX3, YY3, 1, KX, KY
 MM3= POLY_2D(sqrt(W0.C), KX, KY,smoo,sizm(1),sizm(2),missing=0)
endif

if ym2 then M=fix(bytscl(MM2))
if ym3 then M=fix(bytscl(MM3))+M

idx=where(image ne 0)>0
if up then begin
	if (size(M))(0) eq 2 then M(idx)=image(idx) else M=image
	idx=where (MM1 ne 0)
	if idx(0) ge 0 then M(idx)=bytscl(MM1 (idx))
endif else begin
	if ym1 then M=M+bytscl(MM1)
	if (size(M))(0) eq 2 then M(idx)=image(idx) else M=image
endelse

if (size(M))(0) eq 2 then TVscl,M, xsize=sizm(1)*f_dps, ysize=sizm(2)*f_dps

!p.color=!p.color>250
if (up) then f=0 else f=z+1
plots,[0,k,k,0,0],[0,0,i,i,0],[f,f,f,f,f],/data,/t3d	;upper plan

bid=min(dd1(1,*),my) & cyc=coom(0:1,my)
oy=(my+1) mod 4      & coc=coom(0:1,oy)
ay=(my+2) mod 4      & cac=coom(0:1,ay)
uy=(my+3) mod 4      & cuc=coom(0:1,uy)
plots,[cyc(0),cyc(0)],[cyc(1),cyc(1)],[z+1,0],/data,/t3d	;three verticales
plots,[coc(0),coc(0)],[coc(1),coc(1)],[z+1,0],/data,/t3d
plots,[cuc(0),cuc(0)],[cuc(1),cuc(1)],[z+1,0],/data,/t3d

if up then f=z+1 else f=0
; plots,[cac(0),cac(0)],[cac(1),cac(1)],[z,0],/data,/t3d,linestyle=1	;back corners
; plots,[coc(0),cac(0),cuc(0)],[coc(1),cac(1),cuc(1)],[f,f,f],/data,/t3d,linestyle=1
; plots,[cuc(0),cyc(0),coc(0)],[cuc(1),cyc(1),coc(1)],[f,f,f],/data,/t3d

if ya gt yb then axis,0,0,f,xax=0,/data,/t3d,xrange=[W0.xxr(0),W0.xxr(x)],font=-1,charsize=2.5,xtitle=W0.xtit $
            else axis,0,i,f,xax=1,/data,/t3d,xrange=[W0.xxr(0),W0.xxr(x)],font=-1,charsize=2.5,xtitle=W0.xtit
if yd gt yc then axis,0,0,f,yax=0,/data,/t3d,yrange=[W0.yyr(0),W0.yyr(y)],font=-1,charsize=2.5,ytitle=W0.ytit $
            else axis,k,0,f,yax=1,/data,/t3d,yrange=[W0.yyr(0),W0.yyr(y)],font=-1,charsize=2.5,ytitle=W0.ytit

vert=coc ; if dd1(1,oy) lt dd1(1,uy) then vert=cuc
verv=[W0.zzr(0),W0.zzr(z/3),W0.zzr(z*2/3),W0.zzr(z)]
if ya gt yb then axis,vert(0),vert(1),0,zax=1,/data,/t3d,zrange=[W0.zzr(0),W0.zzr(z)],font=-1,charsize=2.5,zticks=3,ztickv=verv,ztitle=W0.ztit $
            else axis,vert(0),vert(1),0,zax=0,/data,/t3d,zrange=[W0.zzr(0),W0.zzr(z)],zticks=3,ztickv=verv,ztitle=W0.ztit

if (profile)  then begin
    px=W0.PXZ-min(W0.PXZ) & py=W0.PYZ-min(W0.PYZ)
    px=px/max(px)*z       & py=py/max(py)*z
    if ya gt yb then h=0 else h=i
    plots,indgen(k),intarr(k)+h,px,/t3d,/data
    if yd gt yc then h=0 else h=k
    plots,intarr(i)+h,indgen(i),py,/t3d,/data

    if up then h=0. else h=1.
    nlv=6 & col=(indgen(nlv)+1)*(255/nlv)
    contour,(W0.A<(W0.TH)),/t3d,nlevels=nlv,/noerase,zvalue=h,c_colors=col,xmargin=[0,0],ymargin=[0,0],$
                   xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']
endif
end

pro setcol, n
;** ******
;**
    common c_draw,	w0,xx,yy,axy,uxy,wnumber,v,p,thresh,rx,rz,nlv,tcol,siz,flgsurf,$
    			wbeside,vfl,styles,w4d,smoo,vff

    if (n ge 0) and (n lt 40) then begin loadct,n & tcol=n & endif

return
end
pro trapx,w_res
;** ****  *****
    trap, w_res , 2
return
end
pro trapy,w_res
;** ****  *****
    trap, w_res , 1
return
end
pro trapt,w_res
;** ****  *****
    trap, w_res , 3
return
end
pro trapp,w_res
;** ****  *****
    trap, w_res , 4
return
end
pro trap, w_res ,flag
;** ****  *****  ****
;**
;** Get last zoomed workspace into w_res (c.a.d alone)
;** Flag=0 then return array
;** Flag=1 then return total(array,1)
;** Flag=2 then return total(array,2)
;** Flag=3 then return total(array)
;** Flag=4 then print  total(array)

@lamp.cbk
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current

    if (trap_x1 ge  0) or (trap_y1 ge 0) then $
    if  trap_ws gt '0' then begin
	if trap_x1 ge 0 then sx=strtrim(string(trap_x1),2) + ':' + $
				strtrim(string(trap_x2),2) $
			else sx='*'
	if trap_y1 ge 0 then sy=strtrim(string(trap_y1),2) + ':' + $
				strtrim(string(trap_y2),2) $
			else sy='*'

	siz=[0L] & ii=execute( 'siz=size(w'+trap_ws+')' )
	tx =''
	if siz(0) eq 1 then tx='('+sx+')'
 	if siz(0) eq 2 then tx='('+sx+','+sy+')'

	fl=0
 	nl=n_elements(flag) & if nl eq 1 then fl=flag

	if fl eq 4 then begin w_res=0.
	    ii=execute( 'w_res=total(w'+trap_ws+tx+')' )
	endif else if (alone gt 0) and (alone le 20) then begin
	    ws='W'+strtrim(string(alone),2)
	    xfor=ws +'=W'+trap_ws+tx & XICUTER,xfor
	    if fl gt 0 then begin
		if fl eq 1 then xicuter, ws +'=total('+ws+',1)'
		if fl eq 2 then xicuter, ws +'=total('+ws+',2)'
		if fl eq 3 then xicuter, ws +'=total('+ws+  ')'
	    endif
	endif
;	For xicute recursivity
	one=-1
	two= 0
   endif
return
end

pro positive, w_in
;** ********
;**
;** Transform an integer*2 unsigned array in a long positive one.
;** Call: W1 = POSITIVE ( W1 )

    s=size(w_in)
    if s(s(0)+1) eq 2 then begin

        index=where ( w_in lt 0 )

        w_in       =long  (temporary(w_in))

        if index(0) ge 0 then w_in(index)=65536+ w_in(index)

    endif
end

;*************************************** Process Create Multi  ***************************
;*************************************** Process Create Multi  ***************************
;*************************************** Process Create Multi  ***************************
pro p_did_multi_cre, widx
;** ***************
;**
;** Create the Multi_Plot interface.
return
end

;*************************************** Process Create begood ***************************
;*************************************** Process Create begood ***************************
;*************************************** Process Create begood ***************************

pro p_did_create_begood, widx ,rx ,nlv ,smoo ,styles ,did_repr
;** *******************
;**
;** Make a UI to change titles and general settings.
@lamp.cbk
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

    i=xregistered('BEGOOD')
    if i le 0 then begin
	if widx le 0 then widx=1
	beg_wid=widx
	beg_t  =lonarr(5)
	if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0

	beg_id	=widget_base  (title='Lamp Begood settings',/column,resource_name='lampdon')

;**	TITLES
	btmp0	=widget_base  (beg_id,/column,/frame)
	btmp	=widget_base  (btmp0 ,/row)
	bg_updat=widget_button(btmp  ,value='Update new titles',font=ft_b_normal  ,uvalue=[-88,361,0])
	bg_slid =widget_slider(btmp  ,title='WK_Space titles'  ,font=ft_b_normal,$
				      xsize=200,minimum=1,maximum=20,value=beg_wid,uvalue=[-88,362,0])
	btmp	=widget_base  (btmp0 ,/row)
	beg_t(0)=widget_text  (btmp  ,xsize=30,ysize=1         ,font=ft_b_bigger,/editable,value=' ')
	btmp11	=widget_label (btmp  ,value='Main Title'       ,font=ft_b_normal)
	btmp11	=widget_label (btmp  ,value='w_tit(i)'         ,font=ft_smallest)
	btmp	=widget_base  (btmp0 ,/row)
	beg_t(1)=widget_text  (btmp  ,xsize=30,ysize=1         ,font=ft_b_bigger,/editable,value=' ')
	btmp11	=widget_label (btmp  ,value='Sub Title'        ,font=ft_b_normal)
	btmp11	=widget_label (btmp  ,value='other_tit(i)'     ,font=ft_smallest)
	btmp	=widget_base  (btmp0 ,/row)
	beg_t(2)=widget_text  (btmp  ,xsize=30,ysize=1         ,font=ft_b_bigger,/editable,value=' ')
	btmp11	=widget_label (btmp  ,value='X   Title'        ,font=ft_b_normal)
	btmp11	=widget_label (btmp  ,value='x_tit(i)'         ,font=ft_smallest)
	btmp	=widget_base  (btmp0 ,/row)
	beg_t(3)=widget_text  (btmp  ,xsize=30,ysize=1         ,font=ft_b_bigger,/editable,value=' ')
	btmp11	=widget_label (btmp  ,value='Y  Title'         ,font=ft_b_normal)
	btmp11	=widget_label (btmp  ,value='y_tit(i)'         ,font=ft_smallest)
	btmp	=widget_base  (btmp0 ,/row)
	beg_t(4)=widget_text  (btmp  ,xsize=30,ysize=1         ,font=ft_b_bigger,/editable,value=' ')
	btmp11	=widget_label (btmp  ,value='Z Title'          ,font=ft_b_normal)
	btmp11	=widget_label (btmp  ,value='z_tit(i)'         ,font=ft_smallest)

;**	GENERAL SETTINGS
	btmp0	=widget_base  (beg_id,/column,/frame)
	btmp11	=widget_base  (btmp0 ,/row)

	btmpa0	=widget_base  (btmp11,/column)
	btmp2t	=widget_base  (btmpa0,/row)
	btmp1t	=widget_label (btmp2t,value='GENERAL SETTINGS' ,font=ft_b_bigger)
	btmp10	=widget_base  (btmpa0,/column,/frame)
	btmp	=widget_label (btmp10,value='... SURFACE ...'  ,font=ft_b_normal)
	btmpb0  =widget_base  (btmp10,/column,/exclusive)
	btmps=lonarr(7)
	btmps(1)=widget_button(btmpb0,value='Shading based on intensities'  ,font=ft_normal,/no_release,$
							uvalue=[-88,366,1])
	btmps(2)=widget_button(btmpb0,value='Shading from Wk_Space W10 "'   ,font=ft_normal,/no_release,$
							uvalue=[-88,366,2])
	btmps(3)=widget_button(btmpb0,value='Shading from a light source'   ,font=ft_normal,/no_release,$
							uvalue=[-88,366,3])
	btmps(4)=widget_button(btmpb0,value='Wire mesh' 		    ,font=ft_normal,/no_release,$
							uvalue=[-88,366,4])
	btmps(5)=widget_button(btmpb0,value='Box style' 		    ,font=ft_normal,/no_release,$
							uvalue=[-88,366,5])
	btmps(6)=widget_button(btmpb0,value='Vectors' 			    ,font=ft_normal,/no_release,$
							uvalue=[-88,366,6])
	btmpr	=widget_base  (btmp10,/row)
	btmp	=widget_label (btmpr ,value='View Angle'       ,font=ft_normal)
	beg_view=widget_text  (btmpr ,value=strtrim(string(rx),2),/editable ,font=ft_b_normal,$
							uvalue=[-88,365,0]  ,xsize=4+cap,ysize=1)
	btmpn	=widget_base  (btmpr,/nonexclusive)
	btmpvr	=widget_button(btmpn ,value='VRML' 		,font=ft_normal,$
							uvalue=[-88,368,0])

	btmp10 =widget_base   (btmpa0,/row,/frame)
	btmp	=widget_label (btmp10,value='.. PLOT ..'  ,font=ft_b_normal)
	btmpb0  =widget_base  (btmp10,/nonexclusive,/row)
	btmphi  =widget_button(btmpb0,value='Histogram',font=ft_normal,uvalue=[-88,366,7])
	btmpna  =widget_button(btmpb0,value='noAxes'   ,font=ft_normal,uvalue=[-88,366,9])

	btmp0a	=widget_base  (btmp11,/column)
	btmp0i	=widget_base  (btmp0a,/row,/frame)
	btmp	=widget_label (btmp0i,value='... IMAGE ...'  ,font=ft_b_normal)
	btmpbi  =widget_base  (btmp0i,/nonexclusive)
	btmpsi  =widget_button(btmpbi,value='Smooth',font=ft_normal,uvalue=[-88,366,8])

	btmp01	=widget_base  (btmp0a,/column,/frame)
	btmp	=widget_label (btmp01,value='... CONTOUR ...'  ,font=ft_b_normal)
	btmp0b  =widget_base  (btmp01,/column,/exclusive)
	btmpct1	=widget_button(btmp0b,value='Using annotations ' 	    ,font=ft_normal,/no_release,$
							uvalue=[-88,367,1])
	btmpct2	=widget_button(btmp0b,value='Using colors lines'            ,font=ft_normal,/no_release,$
							uvalue=[-88,367,2])
	btmpr	=widget_base  (btmp01,/row)
	btmp	=widget_label (btmpr ,value='Levels nb'        ,font=ft_normal)
	beg_lev =widget_text  (btmpr ,value=strtrim(string(nlv),2),/editable,font=ft_b_normal,$
							uvalue=[-88,365,1]  ,xsize=4+cap,ysize=1)
	btmpn	=widget_base  (btmpr,/nonexclusive)
	btmpsp	=widget_button(btmpn ,value='Scan pref' 		    ,font=ft_normal,$
							uvalue=[-88,368,1])

	btmp01	=widget_base  (btmp0a,/column,/frame)
	btmp	=widget_label (btmp01,value='... PROJECTIONS ...'  	    ,font=ft_b_normal)
	btmp0b  =widget_base  (btmp01,/column,/exclusive)
	btmpp   =lonarr(5)
	btmpp(1)=widget_button(btmp0b,value='Frames' 		            ,font=ft_normal,$
							uvalue=[-88,369,1])
	btmpp(2)=widget_button(btmp0b,value='Using polygons'                ,font=ft_normal,$
							uvalue=[-88,369,2])
	btmpp(3)=widget_button(btmp0b,value='Sum dimensions (box)'          ,font=ft_normal,$
							uvalue=[-88,369,3])
	btmpp(4)=widget_button(btmp0b,value='Showing maximum values'        ,font=ft_normal,$
							uvalue=[-88,369,4])
	widget_control,btmpp(2),sensitive=0
	widget_control,btmpp(4),sensitive=0
;	btmp11	=widget_label (btmp0 ,value=' ')
	btmp11	=widget_base  (btmp0 ,/row)
	btmp	=widget_label (btmp11,value='Default PostScript DEVICE:'    ,font=ft_b_normal)
	beg_dev =widget_text  (btmp11,xsize=15,ysize=1, uvalue=[-88,364,0]  ,font=ft_b_bigger,/editable,$
							 value=lamp_devps)

	bg_done =widget_button(btmp11,value='Done'    , uvalue=[-88,363,0]  ,font=ft_b_bigger)
		 put_logo     ,btmp11

	p_did_begood_getitle

   	bid=sys_dep      ('DYNLAB',beg_id,0)
	widget_control,beg_id,group_leader=lamp_b1,/realize & put_logo

        if !P.psym eq 10    then widget_control,btmphi   ,bad_id=i,set_button=1
        if smoo    eq  1    then widget_control,btmpsi   ,bad_id=i,set_button=1
        ix=styles(0,0)>1<6    &  widget_control,btmps(ix),bad_id=i,set_button=1
        if did_repr(14)     then widget_control,btmpna   ,bad_id=i,set_button=1
        if styles(1,0) eq 1 then widget_control,btmpct1  ,bad_id=i,set_button=1
        if styles(1,0) eq 2 then widget_control,btmpct2  ,bad_id=i,set_button=1
        if styles(0,1) eq 1 then widget_control,btmpvr   ,bad_id=i,set_button=1
        if styles(1,1) eq 1 then widget_control,btmpsp   ,bad_id=i,set_button=1
        ix=styles(2,0)>1<4    &  widget_control,btmpp(ix),bad_id=i,set_button=1

	XMANAGER, 'BEGOOD' ,beg_id,event_handler='LAMP_EVENT_PARSER',/just_reg

    endif else widget_control,bad_id=i,beg_id,map=1
return
end

pro p_did_begood_getitle
;** ********************
@lamp.cbk
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	widget_control,bad_id=i,beg_t(0),set_value= w_tit	(beg_wid)
	widget_control,bad_id=i,beg_t(1),set_value= other_tit	(beg_wid)
	widget_control,bad_id=i,beg_t(2),set_value= x_tit	(beg_wid)
	widget_control,bad_id=i,beg_t(3),set_value= y_tit	(beg_wid)
	widget_control,bad_id=i,beg_t(4),set_value= z_tit	(beg_wid)
return
end

pro p_did_begood_setitle
;** ********************
@lamp.cbk
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	widget_control,bad_id=i,beg_t(0),get_value= txt & w_tit    (beg_wid)=txt(0)
	widget_control,bad_id=i,beg_t(1),get_value= txt & other_tit(beg_wid)=txt(0)
	widget_control,bad_id=i,beg_t(2),get_value= txt & x_tit	   (beg_wid)=txt(0)
	widget_control,bad_id=i,beg_t(3),get_value= txt & y_tit	   (beg_wid)=txt(0)
	widget_control,bad_id=i,beg_t(4),get_value= txt & z_tit	   (beg_wid)=txt(0)
return
end

pro p_did_begood_updat
;** ******************
@lamp.cbk
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	p_did_begood_setitle
return
end

pro p_did_begood_slide,ev
;** ******************
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	widget_control,bad_id=i,ev.id,get_value=wi
	if beg_wid ne wi then p_did_begood_setitle
	beg_wid=wi
	p_did_begood_getitle
return
end

pro p_did_begood_nlv, nlv
;** ****************
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	level=12
	widget_control,bad_id=i,beg_lev,get_value=level
	slv=strcompress(level(0),/remove_all)
	on_ioerror,mislv
	nlv=fix(slv)
	mislv:
return
end


pro p_did_begood_ax, rx
;** ***************
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	angle=60
	widget_control,bad_id=i,beg_view,get_value=angle
	srx=strcompress(angle(0),/remove_all)
	on_ioerror,misrx
	rx=fix(srx)
	misrx:
return
end

pro p_did_begood_devps
;** ******************
@lamp.cbk
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev

	widget_control,bad_id=i,beg_dev,get_value=txt
	lamp_devps=strcompress(txt(0),/remove_all)
return
end

pro p_did_begood_done, rx ,nlv
;** *****************
@lamp.cbk
common c_begood,	beg_id,beg_wid,beg_t,beg_view,beg_dev,beg_lev
	p_did_begood_setitle
	p_did_begood_ax,  rx
	p_did_begood_nlv, nlv
	p_did_begood_devps
	widget_control,bad_id=i,beg_id,map=0
	widget_control,bad_id=i,beg_id,/destroy
return
end

;
;*************************************** Process Save Wi *********************************
;*************************************** Process Save Wi *********************************
;*************************************** Process Save Wi *********************************

pro p_did_save_reset
;** ****************
;**
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv
sav_uv=0
end

pro p_did_save_menu,widx
;** ***************
;**
@lamp.cbk
;
; Make a menu list to save workspace
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv
    common c_savt,	sav_tab ,sav_tap,sav_forp,sav_m

    if (sys_dep('VERSION') ge 5.4) then kif='Png' else kif='Gif'
    
    sav_tab=['hdf','xml','Ascii','xdr','F77','htm']
    sav_tap=['Tiff','Bmp',kif,'Pict'] ; sav_tap=['Tiff','Bmp','Gif','Jpeg','Pict']
    sav_tab=[sav_tab,lamp_wrti,sav_tap]   &   sav_tap=[lamp_wrtp,sav_tap]
    if n_elements(sav_forp) eq 0 then sav_forp=7
    if widx eq -1 then return

    P_GET_DATAPATH, sav_pthv
    i=xregistered('SAVE_MENU')
    if i gt 0 then widget_control,bad_id=i,sav_b,/destroy
    n= n_elements(limtxt)
    if n gt 0 then begin
      sav_idx=-1
      if n_elements(sav_form) le 0 then sav_form=5
      dat=systime()
      sav_file=strmid(dat,8,2) + strmid(dat,4,3)
      sav_file=strcompress(sav_file,/remove_all)
      num=strcompress(w_numor(widx),/remove_all)
      if num ne '' then sav_file=num
      p_did_save_seq
      if sav_seq gt 0 then seq='_'+strtrim(string(sav_seq),2) else seq=''
      sav_b=widget_base  (title='Lamp Save a Workspace',/column,resource_name='lampdon')
      list =widget_list  (sav_b,font=ft_b_bigger,uvalue=[-88,371,0],$
      					        value=limtxt,ysize=n,/frame)
      btmp =widget_label (sav_b,value='           ')

      btmp =widget_base  (sav_b,/row)
      none =widget_button(btmp,uvalue=[-88,399,0],value= 'DONE ')
      s_ok =widget_button(btmp                   ,value=' SAVE ',font=ft_b_normal)
      bf   =widget_label (btmp,                   value='   Output File :')
      sav_f=widget_text  (btmp,font=ft_b_bigger, $
      			       uvalue=[-88,373,0],value=sav_file+seq,xsize=20,ysize=1,/editable)
      b_ico=widget_base  (sav_b,/row)
	    put_logo	 ,b_ico
      sav_l=widget_label (b_ico,font=ft_b_normal,value=string(replicate(95b,40)))

      form =widget_label (sav_b,value=' LAMP FORMAT (Web is default, hdf is NeXus)',font=ft_b_bigger)
      formb=widget_base  (sav_b,/row)
      form =widget_base  (formb,/row,/exclusive)
      bt   =lonarr(8)
      bt(0)=widget_button(form ,uvalue=[-88,372,0],/no_release,value='hdf'       ,font=ft_b_normal)
      bt(1)=widget_button(form ,uvalue=[-88,372,1],/no_release,value='xml'       ,font=ft_b_normal)
      bt(2)=widget_button(form ,uvalue=[-88,372,2],/no_release,value='Ascii')
      bt(3)=widget_button(form ,uvalue=[-88,372,3],/no_release,value='xdr (Bin)' ,font=ft_b_normal)
      bt(4)=widget_button(form ,uvalue=[-88,372,4],/no_release,value='F77')
      bt(5)=widget_button(form ,uvalue=[-88,372,5],/no_release,value='htm'       ,font=ft_b_normal)
      bt(6)=widget_button(form ,uvalue=[-88,372,6],/no_release,value='-->'       ,font=ft_b_normal)
      sav_m =widget_button(formb,uvalue=[-88,372,7],    menu=2 ,value=sav_tab(sav_forp))
      FOR i=7,n_elements(sav_tab)-1 do $
       ptmp=widget_button(sav_m,uvalue=[-88,372,i]            ,value=sav_tab(i)  ,font=ft_b_normal)

      btmp =widget_base  (sav_b,/row)
      lpth =widget_label (btmp,value=' Save Path:',font=ft_b_normal)
      bpth =widget_text  (btmp,value=sav_pthv,font=ft_b_bigger,xsize=40,ysize=1,/editable)

      bid=sys_dep      ('DYNLAB',sav_b,0)
      widget_control,sav_b,group_leader =lamp_b1,/realize & put_logo
      widget_control,bt(sav_form<6),bad_id=i,set_button=1
      if sys_dep('VERSION') lt 5.0 then widget_control,bt(0),bad_id=i,sensitive =0

      XMANAGER, 'SAVE_MENU' ,sav_b,event_handler='LAMP_EVENT_PARSER',/just_reg

      pixm =widget_base  (title='Save Icon',map=0)
      pixd =widget_draw  (pixm,retain=2,xsize=192,ysize=192)
      widget_control,pixm ,group_leader=sav_b ,/realize
      widget_control,pixd          ,bad_id=i  ,get_value=pixw

      widget_control,s_ok          ,bad_id=i,set_uvalue=[-88,374,bpth,pixw,192,192,pixm]

      if (widx gt 0) and (widx le 20) then begin sav_idx=0
						 for i=0,n_elements(limtxt)-1 do begin
	    						wi=fix(strmid(limtxt(i),1,2))
	    						if wi eq widx then sav_idx=i
	    					 endfor
						 widget_control,list,bad_id=i,SET_LIST_SELECT=sav_idx
      						 endif
    endif
return
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
pro write_kif,file,image,r,g,b ,TRANSPARENT=trans
;** *********
;**
;** Used for compatibilty with Idl versions.

catch,stat & if stat ne 0 then begin catch,/cancel & print,!err_string & return & endif

ver =sys_dep('VERSION') & png=0 & gif=0 & jpg=0 & img=0
fifi=strlowcase(file)
idx =strpos(fifi,'.gif')
if idx gt 0 then gif=1 else begin
	idx=strpos(fifi,'png')  & if idx gt 0 then png=1 else begin
		idx=strpos(file,'img') & if idx gt 0 then img=1 else jpg=1
		endelse & endelse

pnt =strmid(file,idx+3,5)

if (ver lt 5.2) and (png) then begin file=strmid(file,0,idx)+'jpg'+pnt & jpg=1 & png=0 & endif
if (ver ge 5.4) and (gif) then begin file=strmid(file,0,idx)+'.png'    & png=1 & gif=0 & endif

if jpg then write_jpeg,file,image
if png then if (ver lt 5.4) then imag2=reverse(image,2) else imag2=image
if png then if n_elements(trans) gt 0 then if n_elements(r) gt 1 then $
				           write_png ,file,imag2 ,r,g,b ,TRANSPARENT=[trans] else $
				           write_png ,file,imag2        ,TRANSPARENT=[trans] $
				      else if n_elements(r) gt 1 then $
				           write_png ,file,imag2 ,r,g,b else $
				           write_png ,file,imag2
				           
if gif then if n_elements(r)     gt 1 then write_gif ,file,image ,r,g,b $
				      else write_gif ,file,image
if img then begin
	      out=-1 & on_ioerror,mispixf
	      OPENW,out, file,/GET_LUN & WRITEU  ,out,image
	      mispixf:if out gt 0 then   FREE_LUN,out
endif
end

pro read_kif ,file,image ,sx,sy
;** ********
;**
;** Used for compatibilty with Idl versions.

catch,stat & if stat ne 0 then begin catch,/cancel & print,!err_string & return & endif

ver =sys_dep('VERSION') & png=0 & gif=0 & jpg=0 & img=0
fifi=strlowcase(file)
idx =strpos(fifi,'.gif')
if idx gt 0 then gif=1 else begin
	idx=strpos(fifi,'png')  & if idx gt 0 then png=1 else begin
		idx=strpos(file,'img') & if idx gt 0 then img=1 else jpg=1
		endelse & endelse

pnt =strmid(file,idx+3,5)

if (img) and (n_elements(sx) eq 0) then begin gif=1 & img=0 & endif
if (ver lt 5.2) and (png) then begin file=strmid(file,0,idx)+'jpg'+pnt & jpg=1 & png=0 & endif

if (ver ge 5.4) and (gif) then begin image=0 & print,'!! GIF read not implemented...' & return & endif
if (ver lt 5.2) and (png) then begin image=0 & print,'!! PNG read not implemented...' & return & endif

if jpg then read_jpeg,file,image,COLORS=!D.n_colors-1
if png then image=read_png(file)
if png then if (ver lt 5.4) then image=reverse(image,2)
if gif then read_gif ,file,image
if img then begin
	     in=-1 & on_ioerror,mispixm & image=bytarr(sx,sy)
	     OPENR,in,file,/GET_LUN     & READU   ,in,image
	     mispixm:if in  gt 0   then   FREE_LUN,in
endif
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
pro write_lamp,file, w=wi ,fmt=fmt ,format=format, path=patho
;** **********
;**
common c_savt,	sav_tab ,sav_tap, sav_forp,sav_m

auto=0
if (n_elements(wi) eq 1) and (n_elements(file) eq 1)  then $
if (wi gt 0) and (wi le 23)   and (file gt ' ')       then begin auto=1
					if n_elements(fmt)    eq 1 then p_did_save_format,fmt
					if n_elements(patho)  ne 1 then patho=""
					if n_elements(format) eq 1 then begin
					   p_did_save_menu,-1
					   idx=where(strlowcase(sav_tab) eq strlowcase(format))
					   if idx(0)  ge  0	   then p_did_save_format,idx(0)
					endif
					p_did_save_auto,wi,patho,file,auto & endif
if auto eq 0 then print,string(7b)+'file not saved ...!'
return
end

pro HtmW, wi ,w,x,y
;** ****
;**
@lamp.cbk
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv
	if n_elements(w) gt 1 then begin
		kpw=0. & kpx=0. & kpy=0.  & ws=strtrim(string(wi),2)
		ii=execute('kpw=w'+ws)    & ii=execute('kpx=x'+ws)
		ii=execute('kpy=y'+ws)
		ii=execute('w'+ws+'=w') & ii=execute('x'+ws+'=x')
		ii=execute('y'+ws+'=y') & endif
	sav_form=5
	if (GEORGE ne 0) then WebDo,'pth',wpth
	if wpth eq '' then fif='lamp' else fif='geo_d_12htm.web'
	P_DID_SAVE_AUTO, wi,wpth,fif,1

	if n_elements(w) gt 1 then begin
		ii=execute('w'+ws+'=kpw') & ii=execute('x'+ws+'=kpx')
		ii=execute('y'+ws+'=kpy') & endif
end

pro p_did_WebIda, inst,year,cycl,runs,furm, patho, rep
;** ************
;**
@lamp.cbk
    common c_WebTouch , wtweb, wtinst, wtyear, wtcycl, wtfile, wtcn, wtrep, wtrop, wtroot, wtpath
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv

	if n_elements(wtinst) eq 0 then begin	wtinst="" & wtyear="" & wtcycl="" & wtrop=""
	                                        wtfile="" & endif
	if (wtinst ne   inst) or (wtyear ne year) or (wtcycl ne cycl) then begin
		RDSET,  inst=inst, base="C_Year "+year, cycle=cycl
		wtinst =inst & wtyear=year & wtcycl=cycl & wtfile=inst+"_"+cycl+"_"
		endif

	for i=0,n_elements(runs)-1 do begin
		fils  = strtrim(string(runs(i)),2)
		status= 1
		catch,stat & if stat eq 0 then P_DID_GETRUN, runs(i) ,1, status else catch,/cancel
		to_don_history, 1,0,'W1=RDRUN('+fils+') ;'+inst+"_"+cycl
		if (status eq 0) then begin
			status=1
			px=strpos(fils,'.') & if px gt 0 then fils=strmid(fils,0,px)

			WRITE_LAMP,wtfile+fils, w=1 ,format=furm, path=patho
		endif
	endfor
end

pro p_did_WebTouch, inst,year,cycl,runs,prun, wi, rep
;** **************
;**
@lamp.cbk
    common c_WebTouch , wtweb, wtinst, wtyear, wtcycl, wtfile, wtcn, wtrep, wtrop, wtroot, wtpath
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv

	if n_elements(wtinst) eq 0 then begin	wtinst="" & wtyear="" & wtcycl="" & wtrop="" & endif
	if (wtinst ne   inst) or (wtyear ne year) or (wtcycl ne cycl) then begin
		RDSET,  inst=inst, base="C_Year "+year, cycle=cycl
		wtinst =inst & wtyear=year & wtcycl=cycl & wtfile=inst+"_"+cycl+"_"
		wtpath ="/var/www/htdocs/BARNS/UserZone/Commons/TOUCH_BASE"
		wtpath =sys_dep ('NEWSUB',wtpath ,wtyear)	  & wtroot=wtyear
		ii=findfile(wtpath,count=cnt) & if cnt eq 0 then ii=sys_dep('MKDIR',wtpath)
		wtpath =sys_dep ('INSUB' ,wtpath ,wtinst)	  & wtroot=wtroot+"/"+wtinst+"/"
		ii=findfile(wtpath,count=cnt) & if cnt eq 0 then ii=sys_dep('MKDIR',wtpath)
		endif
	wtrep=rep
	path =wtpath
	chang=""
	wci  = wi & if wci le 0 then wci=1
	WS   ="w" + strtrim(string(wci),2) & sizw=[0L]
	tab=runs
	for j=1b,2 do begin
	if  tab(0) gt 0 then $
	for i=0,n_elements(tab)-1 do begin
		fils  = strtrim(string(tab(i)),2) ; or flto6(tab(i))
		file  = wtfile+fils
		status= 0
		cnti  = 0
		iii   = findfile(path+file+".htm",count=wtcn)
		fili  = file+"_"+wtrep
		if wtcn gt 0 then iii= findfile(path+fili+".png",count=cnti)
		if cnti eq 0 then begin
			if wi le 0 then begin 	status= 1
			   catch,stat & if stat eq 0 then P_DID_GETRUN, tab(i) ,wci, status else catch,/cancel
			   to_don_history, wci,0,WS+'=RDRUN('+fils+') ;'+inst+"_"+cycl
			endif
			if (status eq 0) and (wtrep ne "i") then begin
				ii=execute('sizw=size('+WS+')')
				if sizw(0) eq 1 then begin wtrep ="i" & chang =" i" & fili = file+"_"+wtrep & endif
				if sizw(0) eq 2 then if (wtrep eq 'pz') or (wtrep eq 'lz') then begin
				   			    wtrep ="i" & chang =" i" & fili = file+"_"+wtrep & endif
				if (wtrep eq "i") and (wtcn gt 0) then begin
						iii= findfile(path+fili+".png",count=cnti)
						if cnti gt 0 then status=-1 & endif
			endif
			if status eq 0 then begin
				if (wi le 0) and (wtrep ne wtrop) then begin
					if wtrep eq "s" then begin setcol,3  & tvlct,100,100,100,0 & endif else $
					if wtrep eq "c" then begin setcol,5  & tvlct,100,100,100,0 & endif $
					                else begin setcol,27 & tvlct,160,160,160,0 & endelse
					wtrop=wtrep & endif
				sav_form=5
				P_DID_SAVE_AUTO, wci,path,file,1
				endif
		endif
		if status gt 0 then fili=file+"_failed"
		if wi le 0 then print,"WT "+wtroot+fili+" complete ",j,chang
	endfor
	tab=prun
	endfor
	wtcn=0
end

pro p_did_save_auto, widx,path,file, auto
;** ***************
;**
;** auto =-1 auto save without  data (return -2 for 'imgR')
;** auto = 1 auto save with the data
@lamp.cbk
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv

    sav_idx = widx
    for i =0,n_elements(limtxt)-1 do   begin
	wi=fix(strmid(limtxt(i),1,2))
	if wi eq widx then sav_idx=i & endfor
    sav_pthv= path
    sav_file= file
    if auto eq -1		 then sav_form=3
    if n_elements(sav_form) eq 0 then sav_form=5
    sav_seq = 0
    if n_elements(sav_uv) lt 6 then begin
	 sav_uv=[long(0),0,0,0,192,192]
	     if (!D.flags and 65536) ne 0 then begin
      		pixm  =widget_base  (title='Save Icon',map=0)
      		pixd  =widget_draw  (pixm,retain=2,xsize=sav_uv(4),ysize=sav_uv(5))
      		widget_control,pixm ,group_leader=lamp_b1 ,/realize
      		widget_control,pixd ,bad_id=i  ,get_value=pixw
      		sav_uv(3)=pixw
	     endif
    endif
    if !D.name eq 'Z' then device,set_resolution=[192,192]
    p_did_save_work, 0, sav_uv ,auto
return
end

pro p_did_save_work, event,uv ,auto
;** ***************
;**
;** auto = 0 save from interface
;** auto =-1 auto save without  data (return -2 for 'imgR')
;** auto = 1 auto save with the data
@lamp.cbk

    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv
    common c_savt,	sav_tab ,sav_tap,sav_forp,sav_m
    common c_WebTouch , wtweb, wtinst, wtyear, wtcycl, wtfile, wtcn, wtrep, wtrop, wtroot, wtpath
;
; Save a workspace from menu list
on_ioerror,mis
out=-1

	if (sav_idx ge 0) and (sav_idx lt n_elements(limtxt)) then begin
	    wi=fix(strmid(limtxt(sav_idx),1,2))

	    if (wi ge 1) and (wi le 20) then begin
		if auto eq 0  then begin p_did_save_filename,0 & wtrep='-1' & endif
		if n_elements(wtcn) eq 0 then begin wtcn=0     & wtrep='-1' & endif
		wkstring=strtrim(string(wi),2)
		sizw=[0L] & sizx=[0L] & sizy=[0L] & sizp=[0L] & sizi=[0L] & sizn=[0L]
		i  =execute('sizw=size(w' + wkstring + ')' )
		i  =execute('sizx=size(x' + wkstring + ')' )
		i  =execute('sizy=size(y' + wkstring + ')' )
		i  =execute('sizp=size(p' + wkstring + ')' )
		i  =execute('sizi=size(e' + wkstring + ')' )
		i  =execute('sizn=size(n' + wkstring + ')' )

		if sizw(0) gt 0 then begin
		   if sav_form eq 5 then begin sav_form=3 & wtweb=1 & endif else wtweb=0
		   if (sizx(1) ne sizw(1))  then $
		   		i=execute('x'+wkstring+'=lindgen(sizw(1))+1')
		   if (sizy(sizy(0)+2) ne sizw(sizw(0)+2)) then $
		   if (sizy(1) ne sizw(2))  then $
		   if (sizy(2) ne sizw(2))  then $
		   		i=execute('y'+wkstring+'=lindgen(sizw(2))+1')
		   if  sizp(0) ne 1 then $
				i=execute('p'+wkstring+'=[0]')
		   if (sizi(0) lt 1) and (sizi(1) lt 1) then $
				i=execute('e'+wkstring+'=[0]')
		   if (sizn(0) lt 1) and (sizn(1) lt 1) then $
				i=execute('n'+wkstring+'=[0]')

		   fltr=''
		   if (not wtweb and strpos(sav_file,'_LAMP') lt 0) then fltr='_LAMP' else sav_seq=0

		   if auto eq 0 then begin
		      widget_control,/hourglass
		      widget_control,bad_id=i,uv(2),get_value=pth
		      sav_pthv=sys_dep      ('BLANKS',pth(0))
		      if sav_pthv ne '' then begin
		       car=strmid(sav_pthv,strlen(sav_pthv)-1,1)
		       if (car ne lamp_dvd) then sav_pthv=sav_pthv+lamp_dvd
		      endif
		      p_did_save_seq
		   endif
		   if sav_seq gt 0 then seq='_'+strtrim(string(sav_seq),2) else seq=''

;*****		   ***********************************************************Forms 0 1 2 3 4
		   if (sav_form le 4) then begin
			xx=[1L] & yy=[1L] & zz=[1L] & nn=[0L] & pv=[0L] & ee=[0L]
		   	i=execute('xx = x' +wkstring)
		   	i=execute('yy = y' +wkstring)
		   	i=execute('zz = z' +wkstring)
		   	i=execute('nn = n' +wkstring)
		   	sizn= size(nn)
		   	i=execute('pv = pv'+wkstring)
		   	i=execute('ee = e' +wkstring) & sizi=size(ee)
			sx=sizw(1)
			if sizw(0)  gt 1 then sy=sizw(2) else sy=long(1)
			if sizw(0)  gt 2 then sz=sizw(3) else sz=long(1)
			symod=sy

			if sav_form eq 0 then ext =fltr+'.hdf'
			if sav_form eq 1 then ext =fltr+'.xml'
			if sav_form eq 2 then ext =fltr+'ascii'
			if sav_form eq 3 then $
				if wtweb then ext =fltr+'.xdr' $
					 else ext =fltr+'xdr'
			if sav_form eq 4 then ext =fltr+'f77'

			if not wtweb then ii=sys_dep('POT',ext)
			doe=''
			if  sizi(sizi(0)+2) eq sizw(sizw(0)+2)	then begin c=0L & maxw=0.
				i  =execute('maxw=max(w' + wkstring + ',c)' ) & maxe=ee(c)
				sqr=sqrt(maxw)
				if  sqr          eq maxe then doe='sqrt(i)'       else $
				if  sqr/ maxw    eq maxe then doe='sqrt(i)/ i'    else $
				if  sqr/(maxw+1) eq maxe then doe='sqrt(i)/(i+1)' $
				else     if (not wtweb) or (auto eq 0) then doe='file'
			endif
;****			**********************Write data and header
			if wtcn eq 0 then begin

			if auto ge 0 then begin
;***		Data  file
			 if sav_form eq 4 then openw,out,sav_pthv+sav_file+seq+ext  ,/get_lun,/F77 else $
			 if sav_form eq 3 then openw,out,sav_pthv+sav_file+seq+ext  ,/get_lun,/XDR else $
			 if sav_form eq 2 then openw,out,sav_pthv+sav_file+seq+ext  ,/get_lun      else $
		;	 if sav_form eq 1 then openw,out,sav_pthv+sav_file+seq+ext  ,/get_lun      else $
			 if sav_form eq 1 then begin & end
			 if sav_form eq 0 then begin & end

			 if sav_form gt 1 then begin
			 if sav_form ne 2 then i=execute('writeu,out,w' + wkstring)
			 if sav_form eq 2 then begin
			       if sy gt 1 then i=execute('printf,out,w' + wkstring) $
			       else begin      symod=0 &  ww=0.
			       		       if (sizi(1) ne sx) then ee=bytarr(sx)
			      		       i=execute('ww = w' +wkstring)
			      		       for i=0,sx-1 do printf,out,xx(i),ww(i),ee(i)
			       		       endelse
			  endif
			  free_lun,out
			 endif
			 if wtweb then begin  ii=sys_dep('ZIP',sav_file+seq+ext,sav_pthv)
					      if ii then  ext='.zip'   & endif
;***		Error file
			 if   sav_form gt 1 then begin
		   	 if   doe eq 'file' then $
		   	 if ((sav_form ne 2) or (sy gt 1)) $
		   	 		    and (sav_form ne 0) then begin  eet=ext+'_e'
			  if sav_form eq 4 then openw,out,sav_pthv+sav_file+seq+eet  ,/get_lun,/F77 else $
			  if sav_form eq 3 then openw,out,sav_pthv+sav_file+seq+eet  ,/get_lun,/XDR $
					   else openw,out,sav_pthv+sav_file+seq+eet  ,/get_lun
			  if sav_form ne 2 then i=execute('writeu,out,ee')
			  if sav_form eq 2 then i=execute('printf,out,ee')
			  free_lun,out
		   	 endif else doe=''

		   	 endif;sav_form gt 1
			endif ;auto ge 0
			;**********************End Write data
;***		Header file
			machine=sys_dep   ('MACHINE')
			if n_elements(histxt) gt sav_idx then histoire=histxt(sav_idx) else histoire=' '
			limxt  =limtxt(sav_idx)
		   	src    =head_tit  (fix (wkstring),2)

			out=-1
			if wtweb then exss=".htm" $
				 else begin exss=fltr & ii=sys_dep('POT',exss) & endelse

			if sav_form eq 3 then exs='png' else exs='img'

			if wtweb then begin exs='.png'
				if (sys_dep('VERSION') lt 5.4) and (!D.Name eq 'Z') then exs='.gif'
				if wtrep ne '-1' then exs="_"+wtrep+exs else exs=wtrep+exs
			endif else ii=sys_dep  ('POT',exs)

			if sav_form eq 1 then form='Binary'
			if sav_form eq 2 then form='Ascii'
			if sav_form eq 3 then form='XDR'
			if sav_form eq 4 then form='F77 unformatted'
			tiip=sizw(sizw(0)+1)
			case  tiip of	;+newtypes*****
			   1: tip='(1 )Byte'
			   2: tip='(2 )Short Integer'
			   3: tip='(3 )Long Integer'
			   4: tip='(4 )Floating'
			   5: tip='(5 )Double Floating'
			   6: tip='(6 )Complex'
			   7: tip='(7 )String'
			   8: tip='(8 )Structure'
			   9: tip='(9 )Double Complex'
			   10:tip='(10)Pointer'
			   11:tip='(11)Object'
			   12:tip='(12)Unsigned Integ'
			   13:tip='(13)Unsigned Long'
			   14:tip='(14)Long Int64'
			   15:tip='(15)Unsigned Int64'
			else:tip='(0) Undefined'
			endcase
      			npa=0L & npv=[0L]
      			bb=execute('npa=n_elements(p' +wkstring+')' )
      			bb=execute('npv=size      (pv'+wkstring+')' )
     			i  =0
       			bb=execute('for i=0,npa-1 do par_txt_all(i)=strtrim(par_txt(fix(wkstring),i))+string(p' $
      						   +wkstring + '(i))')
			if (sizn(0) eq 2) then if (sizn(1) eq sx) and (n_elements(nn) gt sx) then begin
				j=sizn(2)-1
				k=1
				for i=sizn(2)-1,1,-1 do if (k and max(nn(*,i)) eq 0) then j=j-1 else k=0
				if j lt sizn(2)-1 then  begin nn=nn(*,0:j) & sizn=size(nn) & endif
			endif
		   	ttl=w_tit     (fix (wkstring))
		   	ttx=x_tit     (fix (wkstring))
		   	tty=y_tit     (fix (wkstring))
		   	ttz=z_tit     (fix (wkstring))
		   	tto=other_tit (fix (wkstring))
			A_ac=""
			trf =""
			apl =""
			if wtweb then if (sy gt 1) and (sz le 1) then begin	A_type=strtrim(string(tiip)	,2)
										A_sx  =strtrim(string(sx )	,2)
										A_sy  =strtrim(string(sy )	,2)
										A_x0  =strtrim(string(xx(0))	,2)
										A_x1  =strtrim(string(xx(1))	,2)
										A_xn  =strtrim(string(xx(sx-1))	,2)
										A_y0  =strtrim(string(yy(0))	,2)
										A_y1  =strtrim(string(yy(1))	,2)
										A_yn  =strtrim(string(yy(sy-1))	,2)
										A_fl  =sav_file+seq+ext
									;	A_fl  =strmid(A_fl,strpos(A_fl,"/BARNS"),100)
										A_ac  ="Exec" & if sav_file eq 'lamp' then A_ac ="Start"
										A_wht ='300'  & if sav_file eq 'lamp' then A_wht='30'
			 apl='<APPLET Codebase= "http://barns.ill.fr/BARNS/GRAPH/V3D/classes/"' +$
			 ' Code="V3D.class" archive="V3D.jar" Width= '+A_wht+' Height= 70>'     +$
			 '<PARAM Name="Action"   Value="'+A_ac+'">'  +$
			 '<PARAM Name="User"     Value="$BarnsUser">'+$
			 '<PARAM Name="CGI"      Value="/cgi-bin/barns/nph-barns?Application=#WebLamp+-nws">'+$
 			 '<PARAM Name="File"     Value="'+A_fl+'">'  +$
			 '<PARAM Name="Format"   Value="xdr">'+$
			 '<PARAM Name="Type"     Value="'+A_type+'">'+$
			 '<PARAM Name="Title"    Value="'+ttl+'">'+$
			 '<PARAM Name="TitleX"   Value="'+ttx+'">'+$
			 '<PARAM Name="TitleY"   Value="'+tty+'">'+$
			 '<PARAM Name="SubTitle" Value="'+tto+'">'+$
			 '<PARAM Name="NbX"      Value="'+A_sx+'">'+$
			 '<PARAM Name="X0"       Value="'+A_x0+'">'+$
			 '<PARAM Name="X1"       Value="'+A_x1+'">'+$
			 '<PARAM Name="Xn"       Value="'+A_xn+'">'+$
			 '<PARAM Name="NbY"      Value="'+A_sy+'">'+$
			 '<PARAM Name="Y0"       Value="'+A_y0+'">'+$
			 '<PARAM Name="Y1"       Value="'+A_y1+'">'+$
			 '<PARAM Name="Yn"       Value="'+A_yn+'"></APPLET>'
			endif
			if wtweb then trf='<br>Get vrml !'+ $
					  '<br><a href="'+sav_file+seq+ ext+'" >Get Data (bin zipped) !</a>'+ $
					  apl+'<br><br><pre>'

			if sav_form gt 1 then begin
			openw,out,sav_pthv+sav_file+seq+exss,/get_lun

			printf,out,'<html><head><title>LAMP_FORMAT ' + systime()+'</title></head><body><img align =right src="'+sav_file+seq+fltr+exs+'"><b>'
			printf,out,' HEADER FILE written by the LAMP APPLICATION</b>'+trf
			printf,out,' '

			if auto ge 0 then  begin
			printf,out,' '
			printf,out,' DATA_FILE:      ' + sav_file +seq+ ext
			if src ne '' then $
			printf,out,' SOURCE:         ' + src
			endif

			if doe eq 'file' then if auto ge 0 then doe=sav_file +seq+ ext+'_e' else doe=''
			if doe ne ''     then begin
			printf,out,' ERRO_FILE:      ' + doe
			printf,out,' '
			endif

			printf,out,' HISTORY:        ' + histoire
			printf,out,' '
			printf,out,' X_SIZE:         ' + strtrim(string(sx)   ,2)
			printf,out,' Y_SIZE:         ' + strtrim(string(symod),2)
			printf,out,' Z_SIZE:         ' + strtrim(string(sz)   ,2)
			printf,out,' FORMAT:         ' + form
			printf,out,' TYPE:           ' + tip
			printf,out,' '

			if A_ac ne "Start" then begin

			printf,out,' MIN,MAX VALUES: ' + limxt
			printf,out,' '
			printf,out,' TITLES: ' + ttl
			if ttx ne '' then $
			printf,out,'      X: ' + ttx
			if tty ne '' then $
			printf,out,'      Y: ' + tty
			if ttz ne '' then $
			printf,out,'      Z: ' + ttz
			if tto ne '' then $
			printf,out,'  OTHER: ' + tto
			printf,out,' '
			printf,out,' PARAMETERS:'
			n=npa-1
			if n gt 0 then $
			printf,out,' ----------'
			if n gt 0 then for i=0,n do printf,out,' * '+  par_txt_all(i)
			printf,out,' '

			if npv(0) ge 1 then begin
			    tmp=strtrim(string(npv(1)),2)
			    if npv(0) gt 1 then tmp=tmp+' '+strtrim(string(npv(2)),2) else tmp=tmp+' 0'
			    if npv(0) gt 2 then tmp=tmp+' '+strtrim(string(npv(3)),2) else tmp=tmp+' 0'
			    printf,out,' VAR PARAM: nb='   +tmp
			    printf,out,' ---------                        (hidden)</pre><!--'
			    printf,out, pv
			    printf,out,'                                  --><pre>'
			endif

			if (long(xx(0)) eq xx(0)) and (xx(sx-1)-xx(0) eq sx-1) then begin
			    printf,out,' X_COORDINATES:'+string(xx(0))+' --> X size'
			    printf,out,' '
			endif else begin
			    sizx=size(xx) & if sizx(0) eq 2 then tmp=' bi_dim' else tmp=''
			    printf,out,' X_COORDINATES: '+ tmp
			    printf,out,' -------------                    (hidden)</pre><!--'
			    printf,out, xx
			    printf,out,'                                  --><pre>'
			endelse

			if (long(yy(0)) eq yy(0)) and (yy(sy-1)-yy(0) eq sy-1) and (sizw(0) gt 1) then begin
			    printf,out,' Y_COORDINATES:'+string(yy(0))+' --> Y size'
			    printf,out,' '
			endif else begin
			    sizy=size(yy) & if sizy(0) eq 2 then tmp=' bi_dim' else tmp='nb='+string(n_elements(yy))
			    printf,out,' Y_COORDINATES: '+ tmp
			    printf,out,' -------------                    (hidden)</pre><!--'
			    printf,out, yy
			    printf,out,'                                  --><pre>'
			endelse

			sizz=n_elements(zz)
			if (sizz gt 1) or (zz(0) ne 0) then begin
			 if sizz eq 1 then begin
			    printf,out,' Z_COORDINATES: '+string(zz(0))
			    printf,out,' '
			 endif else begin
			    printf,out,' Z_COORDINATES: nb='+string(sizz)
			    printf,out,' -------------                    (hidden)</pre><!--'
			    printf,out, zz
			    printf,out,'                                  --><pre>'
			 endelse
			endif

			if sizn(0) lt 1 then tmp='1' else tmp=strtrim(string(sizn(1)),2)
			if sizn(0) gt 1 then tmp=tmp+' '+strtrim(string(sizn(2)),2) else tmp=tmp+' 0'
			if sizn(0) gt 2 then tmp=tmp+' '+strtrim(string(sizn(3)),2) else tmp=tmp+' 0'
			printf,out,' MONITORS: nb='   +tmp
			printf,out,' --------                             </pre><!--'
			if (sizn(0) gt 2) or (sizn(0) le 1) then printf,out, nn $
			else		 for i=0,sizn(2)-1 do printf,out, nn(*,i)
			printf,out,'                                      --><pre>'

			if (not wtweb and auto eq 0) then begin

			printf,out,' MACHINE: ' + machine
			printf,out,'                                      </pre><!--'
			printf,out,' HOW TO READ THE DATA (example)'
			printf,out,' --------------------'
			printf,out,' USING IDL'
			printf,out,' ----- ---'
			if  symod    gt 0 then $
			printf,out,'        array = MAKE_ARRAY( xsize,ysize,zsize ,TYPE=code)'
			if  symod    eq 0 then $
			printf,out,'        array = MAKE_ARRAY( 3 , xsize ,TYPE=code)'
			printf,out,' '
			if (sav_form eq 1) or (sav_form eq 2) then $
			printf,out,'        OPENR,  unit, "'+sav_file+seq+ext +'" ,/GET_LUN'
			if  sav_form eq 3 then $
			printf,out,'        OPENR,  unit, "'+sav_file+seq+ext +'" ,/GET_LUN,/XDR'
			if  sav_form eq 4 then $
			printf,out,'        OPENR,  unit, "'+sav_file+seq+ext +'" ,/GET_LUN,/F77_UNFORMATTED'
			printf,out,' '
			if (sav_form ne 2)  then $
			printf,out,'        READU,  unit, array'
			if sav_form eq 2 then $
			printf,out,'        READF,  unit, array'
			printf,out,' '
			printf,out,' USING FORTRAN'
			printf,out,' ----- -------'
			if sav_form eq 2 then begin
			   if sy gt 1    then begin
			   	printf,out,'        REAL array(xsize,ysize,zsize)'
			   	printf,out,'        OPEN(unit=20,status="old",file="'+sav_file+seq+ext+'")'
			   	printf,out,'        READ(20,*) array'
			   endif else begin
			   	printf,out,'        REAL*4 x(xsize) , y(xsize) , e(xsize)'
			   	printf,out,'        OPEN(unit=20,status="old",file="'+sav_file+seq+ext+'")'
			   	printf,out,'        DO i=1,xsize'
			   	printf,out,'           READ(20,*) x(i),y(i),e(i)'
			   	printf,out,'        ENDDO'
			   endelse
			endif
			if (sav_form ne 2)   then begin
			   if tiip eq 1 then printf,out,'        BYTE array(xsize,ysize,zsize)'
			   if tiip eq 2 then printf,out,'        INTEGER*2 array(xsize,ysize,zsize)'
			   if tiip eq 3 then printf,out,'        INTEGER*4 array(xsize,ysize,zsize)'
			   if tiip eq 4 then printf,out,'        REAL*4 array(xsize,ysize,zsize)'
			   if tiip eq 5 then printf,out,'        DOUBLE PRECISION array(xsize,ysize,zsize)'
			   if tiip eq 6 then printf,out,'        COMPLEX array(xsize,ysize,zsize)'
			   if tiip eq 9 then printf,out,'        DOUBLE COMPLEX array(xsize,ysize,zsize)'
			   if sav_form eq 1 then printf,out,'        OPEN(unit=20,status="old",file="' $
			   			+sav_file+seq+ext+'",form="unformatted",' $
			   			+'recl=xsize*ysize*zsize,access="direct")'
			   if sav_form eq 3 then printf,out,'        OPEN(unit=20,status="old",file="' $
			   			+sav_file+seq+ext+'",form="xdr")'
			   if sav_form eq 4 then printf,out,'        OPEN(unit=20,status="old",file="' $
			   			+sav_file+seq+ext+'",form="unformatted")'

			   printf,out,'        READ(20) array'
			endif
			printf,out,'                                  --><pre>'
			endif		;not wtweb and auto eq 0

			endif		;A_ac="Start"

			printf,out,'</body></html>'
			free_lun,out
			out=-1
			endif		;sav_form gt 1
			endif		;wtcn eq 0
;****			**********************End Write data and header

;****			**********************Write Snapshot or HDF or XML file
			if (uv(3) gt 0) or (!D.name eq 'Z') then begin
;			Icone
;			-----
			kpDname=!D.name	;or pixmap in uv(3) but problems with true colors...
			set_plot, 'Z' & device,set_resolution=[uv(4),uv(5)]
			if auto ne -1 then erase
			if sav_form le 1 then wr=1 else  wr=0
			if wtrep eq '-1' then wttr='s' else wttr=wtrep

			p_did_makeicon, wkstring,xx,yy, uv(4),uv(5) ,auto ,wr ,wttr

			if auto ne -1 then begin worder=!order & !order=0
						 if (sav_form gt 1) and (not wtweb) then !order=1
						 wr=tvrd(0,0,uv(4),uv(5))
						 !order=worder & endif
			set_plot,kpDname
			
			if n_elements(wr) eq 1 then exs='imgR' else if sav_form eq 3 then exs='png' else exs='img'
			if wtweb then begin exs='.png'
				if (sys_dep('VERSION') lt 5.4) and (!D.Name eq 'Z') then exs='.gif'
				if wtrep ne '-1' then exs="_"+wtrep+exs else exs=wtrep+exs
			endif    else ii=sys_dep  ('POT',exs)
			ftxt=sav_pthv+sav_file+seq+fltr+exs

			if (sav_form le 1) then  begin fi=sav_file+seq+ext & ptfi=sav_pthv+fi
			    if sav_form eq 0 then pcm='write_hdf' else pcm='write_xml'
			    cmd= pcm +  ',ptfi,w'+wkstring+',xc=xx,yc=yy'              + $
			                ',zc=zz,e=ee,par_txt_all=par_txt_all'          + $
			                ',pr=p'+wkstring+',pv=pv,n=nn,w_tit=ttl'       + $
			                ',x_tit=ttx,y_tit=tty,z_tit=ttz,other_tit=tto' + $
			                ',SRC=src,HIST=histoire,LIM=limxt,MACH=machine'+ $
			                ',DOE=doe,SNAP=wr,DATE=head_tit(wi,4),FIFI=fi'
			    err=execute(cmd)
			endif else begin
			    if n_elements(wr) eq 1 then begin
				OPENW,out,ftxt,/get_lun,/XDR
				i=execute('WRITEU,out,fix(sx),fix(sy),fix(tiip),w' + wkstring)
				FREE_LUN,out   &  out=-1
				if save_form ne 3 then auto=-2
				if auto le -1 then bid=sys_dep('DO_Z',sav_pthv+sav_file+seq+fltr+exs,lamp_dir)
				auto=-2
			    endif else begin
				if (sav_form eq 3) then begin tvlct,r,g,b,/get
				                 WRITE_KIF,ftxt,wr,r,g,b, transparent=[0]
					         if (wtweb) and (strpos(ftxt,'gif') gt 0) then ii=sys_dep('GIFTRANS',ftxt)
				endif else begin WRITE_KIF,ftxt,wr
				if auto le -1 then bid=sys_dep('DO_Z',sav_pthv+sav_file+seq+fltr+exs,lamp_dir)
				endelse
			    endelse
			endelse
		        endif
;****			********************** End Write Snapshot or HDF or XML

		     if (auto eq 0) or ((sav_form ne 3) and (b_labins(3) ne 0)) then begin
			   text='W'+wkstring+ ' saved in '+sav_file+seq+ext
		           if wtweb then text=text + ' (& .htm)'
			   p_did_setwin0
			   if auto eq 0 then widget_control,bad_id=i,sav_l,set_value=text $
					else print ,text
			   text='WRITE_LAMP,"'+sav_pthv+sav_file+seq+'",w='+wkstring
			   to_don_history,-1,-1,text
		     endif
		   endif
;*****		   ***********************************************************End Forms 0 1 2 3 4
		   if (auto ge 0) and (sav_form ge 7)  then begin

		    proced=sav_tap(sav_form-6) & ptfi=sav_pthv+sav_file+seq & s=sizw(0) & err=88

		    if (proced eq 'Tiff') and (s eq 2) then begin ext='.tiff'
			  if sys_dep('VERSION') ge 5.2 then begin
				keywrd=',compression=1'
				tiip  =  sizw(sizw(0)+1) ;+newtypes*****
				if tiip  eq 2 then keywrd=keywrd+',/short'
				if tiip  eq 3 then keywrd=keywrd+',/long'
				if tiip  eq 4 then keywrd=keywrd+',/float'
				err=execute('write_tiff,ptfi+ext,w'+wkstring+keywrd )
			  endif else begin
				err=execute('write_tiff,ptfi+ext,bytscl(w'+wkstring +')')
				if err ne 1 then $
				err=execute('tiff_write,ptfi+ext,bytscl(w'+wkstring +')')
			  endelse
		    endif else $
		    if (proced eq 'Gif')  and (s eq 2) then begin
				if (sys_dep('VERSION') ge 5.4) then ext='.png' else ext='.gif'
				err=execute('WRITE_KIF ,ptfi+ext,bytscl(w'+wkstring +')')
		    endif else $
		    if (proced eq 'Bmp')  and (s eq 2) then begin ext='.bmp'
				err=execute('write_bmp ,ptfi+ext,bytscl(w'+wkstring +')')
		    endif else $
		    if (proced eq 'Jpeg') and (s eq 2) then begin ext='.jpg'
				err=execute('write_jpeg,ptfi+ext,bytscl(w'+wkstring +')')
		    endif else $
		    if (proced eq 'Png')  and (s eq 2) then begin ext='.png'
				err=execute('write_png ,ptfi+ext,bytscl(w'+wkstring +')')
		    endif else $
		    if (proced eq 'Pict') and (s eq 2) then begin ext='.pict'
				err=execute('write_pict,ptfi+ext,bytscl(w'+wkstring +')')
		    endif else  begin ext=''
		     err=execute(proced+',ptfi,w'+wkstring+',xc=x'+wkstring+',yc=y'+wkstring+ $
		                 ',zc=z'+wkstring+',e=e'  +wkstring+',par_txt=par_txt(wi,*)'+ $
		                 ',pr=p'+wkstring+',pv=pv'+wkstring+',n=n'+wkstring         + $
		                 ',w_tit=w_tit(wi),x_tit=x_tit(wi),y_tit=y_tit(wi)'         + $
		                 ',z_tit=z_tit(wi),other_tit=other_tit(wi)')
		    endelse

		    if auto eq 0 then if err eq 1 then $
		    widget_control,bad_id=i,sav_l,set_value='W'+wkstring+' is saved in '+sav_file+seq+ext

		    if err eq 88 then begin
		   		if auto eq 0 then $
				widget_control,bad_id=i,sav_l,set_value=' Workspace not a 2D array!!!'
				print,string(7b)
		    endif
		   endif
		   if wtweb then sav_form=5

		endif else begin
		   		if auto eq 0 then $
				widget_control,bad_id=i,sav_l,set_value=' Workspace is not an array!!!'
				print,string(7b) & endelse
	    endif else begin
		   		if auto eq 0 then $
				widget_control,bad_id=i,sav_l,set_value=' Choose a Workspace !!!'
				print,string(7b) & endelse
	endif else begin
		   		if auto eq 0 then $
				widget_control,bad_id=i,sav_l,set_value=' Choose a Workspace !!!'
				print,string(7b) & endelse
return
mis:				if auto eq 0 then widget_control,bad_id=i,sav_l,set_value=!err_string $
					     else print,!err_string
				print,string(7b)
				if out gt 0 then free_lun,out
				auto=0
				if wtweb then sav_form=5
return
end

pro p_did_makeSnap, wi
;** **************
;**
@lamp.cbk
common c_did
common c_trap
	ws=strtrim(string(wi),2)
		if n_elements(Snapix) eq 0 then Snapix=0
		if Snapix eq 0 then begin
			aa=64L & bb=32L
      			bid=widget_base  (title='',map=0)
      			bid=widget_draw  (bid,retain=2,xsize=aa,ysize=bb * 21)
       			widget_control   ,bid,bad_id=i , /realize
      			widget_control   ,bid,bad_id=i , get_value=Snapix
      			bid=widget_base  (title='',map=0)
      			bid=widget_draw  (bid,retain=2,xsize=aa,ysize=bb)
       			widget_control   ,bid,bad_id=i , /realize
      			widget_control   ,bid,bad_id=i , get_value=Snapil
		endif
	ii=execute('p_did_makSnaps,w'+ws+', Sna'+ws+', Snapix, Snapil, did_tio, wi')
	trap_current=Snapil
end
pro p_did_makSnaps, w, s, Snapix, Snapil, dido, wi
;** **************
;**
aa=64L & bb=32L
kpwin=!window
if n_elements(s) eq 1 then begin
   if !D.name ne 'Z' then wset,Snapil & erase,255
   if n_elements(w) gt 1 then begin
	sz=size(w)
	if sz(0) eq 1 then begin s=congrid(w,sz(1)<100)
	                           plot,s,xmargin=[0,0],ymargin=[0,0],xstyle=4,ystyle=4 & endif
	if sz(0) eq 2 then begin i=sz(1)<aa>(aa/2) & j=sz(2)<bb>(bb/2)
	                         s=congrid(w,i,j)
	                           tvscl,s,(aa-i)/2,(bb-j)/2 & endif
	if sz(0) eq 3 then begin i=sz(1)<aa>(aa/2) & j=sz(2)<bb>(bb/2) & k=sz(3)<40

				 if sys_dep('VERSION') ge 4.0 then ii=execute('s=transpose(congrid(w,i,j,k),[2,0,1])') $
				                              else s =congrid(w,i,j,k)

				 maxi=max (s,min=mini) & thresh=mini+(maxi-mini)/3.
				 shade_volume,s,thresh,v,p
				 if n_elements(p) gt 3 then begin
				   s={w:s,thresh:thresh,v:v,p:p}
				   scale3, xrange=[0,k-1], yrange=[0,i-1], zrange=[0,j-1],ax=0.,az=0.
				   t3d, tr=[-.5,-.5,-.5] ,rot=[ 0. , 20. , 0. ]
				   t3d,                   rot=[ 60., 0.  , 0. ]
				   t3d, tr=[+.5,+.5,+.5]
				   set_shading,reject=0 & tvscl,polyshade(v,p,/t3d) & set_shading,reject=1
				 endif else begin
	                           s=congrid(total(w,3),i,j)
 	                           tvscl,s,(aa-i)/2,(bb-j)/2 & endelse
	endif
  endif
  if !D.name ne 'Z' then begin wset,Snapix & device,copy=[0,0,aa,bb,0,bb*wi,Snapil] & endif
endif
if (dido ne 0) and (!D.name ne 'Z') then begin
	wset,abs(dido) & i=0 & j=0
	if dido gt 0 then begin i=32 & j=16 & erase,255 & endif
	device,copy=[0,bb*wi,aa,bb,i,j,Snapix]
endif
if kpwin gt 0 then if !D.name ne 'Z' then wset,kpwin
end

pro p_did_makefunc, w,w0, rep, ln
;** **************
;**
if (size(w))(0) lt 2 then rep='ln'
if (size(w))(0) eq 2 then if (rep eq 'pz') or (rep eq 'lz') then rep='i'

if rep eq 'px' then w0=      total(w,2)          else $
if rep eq 'py' then w0=      total(w,1)          else $
if rep eq 'pz' then w0=      total(w,3)          else begin ln=1
if rep eq 'lx' then w0=alog (total(w,2)>0 + 0.1) else $
if rep eq 'ly' then w0=alog (total(w,1)>0 + 0.1) else $
if rep eq 'lz' then w0=alog (total(w,3)>0 + 0.1) else $
if rep eq 'ln' then w0=alog (      w   >0 + 0.1) &  endelse
rep='i'
end

pro p_did_makeicon, wkstr,xx,yy, uv4,uv5 ,auto ,wr ,rup
;** **************
;**
;** auto = 0 save from interface
;** auto =-1 auto save without  data (no web)
;** auto = 1 auto save with the data
@lamp.cbk
common c_trap
		trap_current=!D.window
		ln=0   &  rep=rup  &  wkstring=wkstr  &  nlv=11 & sizw=[0L]

		i =execute('sizw=size(w'+wkstring+')') & if sizw(0) eq 1 then rup="i"

		if strlen(rep) gt 1 then begin
			i =execute('p_did_makefunc, w'+wkstring+',w0,rep,ln')
			wkstring='0' & endif
		i =execute('sizw=size(w'+wkstring+')')
		sx=sizw(1)
		if sizw(0)  gt 1 then sy=sizw(2) else sy=long(1)
		if sizw(0)  gt 2 then sz=sizw(3) else sz=long(1)

		xi=sx & yi=sy   & xo=sx*2/uv4 & yo=sy*2/uv5
		if yo ge xo then  fx=xo else fx=yo
		if fx gt 1  then  begin xi=(sx/fx)<uv4 & yi=(sy/fx)<uv5 & endif
		xo=(uv4-xi)/2 & yo=(uv5-yi)/2

		worder=!order & if not wr then !order=1

		if auto ne -1 then begin
		   if sz eq 1    then begin
			if sy eq 1 then begin
			      if n_elements(xx) gt 1 then $
			      	i=execute('plot,xx,w' + wkstring + ',xmargin=[5,0],ymargin=[3,0]') else $
			      	i=execute('plot   ,w' + wkstring + ',xmargin=[5,0],ymargin=[3,0]')
			endif
			if sy gt 1 then begin
			  if rep ne 's'   then begin tmw=0L
			    if rep eq 'c' then begin xi=sx<64  & yi=sy<64  & endif
			    if rep eq 'i' then begin xi=uv4 & yi=uv5
			                             if sy le sx/2 then yi=yi/2
			                             if sx le sy/2 then xi=xi/2
			                             xo=(uv4-xi)/2 & yo=(uv5-yi)/2 & endif
			    i=execute('tmw=congrid(w'+ wkstring + ',xi,yi)')
			    if ln eq 0 then begin
			       minx=min ( tmw ) & if minx le 0 then tmw=tmw-minx+0.1
			       tmw=alog  (temporary(tmw)) & endif
			    if sys_dep('VERSION') lt 4.0 then edg='' else edg=',/edge'
			    i=execute('tmw=smooth(temporary(tmw),4'+edg+')')
			    tmw=bytscl(temporary(tmw))
			  endif
			    if (n_elements(xx) eq sx) and (n_elements(yy) eq sy) and (rep ne 'i') then begin
			      if (sx ne xi) or (sy ne yi) then begin six=size(xx) & siy=size(yy)
				 if six(0)  eq  2  then tmx=congrid(xx,xi,yi) else tmx=congrid(xx,xi)
				 if siy(0)  eq  2  then tmy=congrid(yy,xi,yi) else tmy=congrid(yy,yi)
			      endif
			      if (sx ne xi) or (sy ne yi) then begin
			         if rep eq 'c' then contour,tmw,tmx,tmy,xmargin=[0,0],ymargin=[0,0],xstyle=4,ystyle=4, $
			                                 c_colors=(indgen(nlv)+1)*(180/nlv) + 50 ,nlevels=nlv $
				 else $
			         i=execute('shade_surf,congrid(w' + wkstring + ',xi,yi)' +$
			      		   ',tmx,tmy    ,xmargin=[0,0]'+$
			      		   ',ymargin=[0,0],xstyle=4,ystyle=4,zstyle=4,ax=55.,az=30.' )
			      endif else $
			         if rep eq 'c' then contour,tmw,xx,yy,xmargin=[0,0],ymargin=[0,0],xstyle=4,ystyle=4, $
			                                 c_colors=(indgen(nlv)+1)*(180/nlv) + 50 ,nlevels=nlv $
				 else $
			         i=execute('shade_surf,w' + wkstring + ',xx,yy,xmargin=[0,0]'+$
			      		   ',ymargin=[0,0],xstyle=4,ystyle=4,zstyle=4,ax=55.,az=30.' )

			    endif else begin
			      if rep eq 'c' then contour,tmw,xmargin=[0,0],ymargin=[0,0],xstyle=4,ystyle=4, $
			                                 c_colors=(indgen(nlv)+1)*(180/nlv) + 50 ,nlevels=nlv
			      if rep eq 'i' then tv,tmw,xo,yo else $
			      if (sx ne xi) or (sy ne yi) then $
			         i=execute('shade_surf,congrid(w' + wkstring + ',xi,yi)' +$
			      		   				     ',xmargin=[0,0]'+$
			      		   ',ymargin=[0,0],xstyle=4,ystyle=4,zstyle=4,ax=55.,az=30.' ) $
			      else $
			         i=execute('shade_surf,w' + wkstring + ',      xmargin=[0,0]'+$
			      		   ',ymargin=[0,0],xstyle=4,ystyle=4,zstyle=4,ax=55.,az=30.' )
			    endelse
			endif
		   endif
		   if sz gt 1    then begin
			   if (xi gt 60) and (yi gt 60)   then begin
			      if (sx ne xi) or (sy ne yi) then $
			         i=execute('tvscl,congrid(total(w' + wkstring + ',3),xi,yi),xo,yo') else $
			         i=execute('tvscl,        total(w' + wkstring + ',3)       ,xo,yo')
			   endif else $
			         i=execute('tvscl,congrid(total(w' + wkstring + ',3),uv4,uv5)')
		   endif
		endif else begin
			minx=w_min(fix(wkstring))
			wks ='w' + wkstring
			wr  =0.
			if sz eq 1    then begin
			  tip =sizw(sizw(0)+1) ;+newtypes*****
			  if tip lt  2 then nbyt=sx*sy   else if tip eq  2 then nbyt=sx*sy*2 else $
			  if tip le  4 then nbyt=sx*sy*4 else if tip eq 12 then nbyt=sx*sy*2 else $
			  if tip eq 13 then nbyt=sx*sy*4 else nbyt=sx*sy*8
			  if (nbyt+3*2 ge long(uv4)*uv5) and (sy gt 1) then begin
			   if minx le 0 then i =execute( 'wr=alog('+wks+'-minx+0.1)' ) $
			         	else i =execute( 'wr=alog('+wks+')' )
			   if sy eq 1 then begin
;			         wt=congrid(wr,uv4,/interp)
;			         if n_elements(xx) gt 1 then $
;			      	    plot,congrid(xx,uv4),wt,xmargin=[5,0],ymargin=[3,0],ytitle='log' $
;			      	 else plot,		 wt,xmargin=[5,0],ymargin=[3,0],ytitle='log'
;				 wr     =tvrd(0,0,uv4,uv5)
;			      	 wr(0,0)=bytscl(congrid(wt,uv4,2))
			   endif else wr=bytscl(congrid(temporary(wr),uv4,uv5,/interp))
			  endif
			endif else begin
			   if sx gt sy then begin i=execute('wr=reform(w'+wkstring+',sx,sy*sz)')
		      	        		  mini=w_min(fix(wkstring))
		      	       			  maxi=w_max(fix(wkstring))
			   endif else begin
			        if (sy gt sz) and (sy ne sx) then j=2 else j=3
			        i  =execute('wr=total(w' + wkstring + ',j)')
		  	        maxi =max(wr,min=mini)
		      	        w_min(fix(wkstring))=mini
		      	        w_max(fix(wkstring))=maxi
			   endelse

			   sizw=size(wr) & sx=sizw(1) & sy=sizw(2) & tip=sizw(sizw(0)+1)
			   if tip lt  2 then nbyt=sx*sy   else if tip eq  2 then nbyt=sx*sy*2 else $
			   if tip le  4 then nbyt=sx*sy*4 else if tip eq 12 then nbyt=sx*sy*2 else $
			   if tip eq 13 then nbyt=sx*sy*4 else nbyt=sx*sy*8 ;+newtypes*****

;			   if (nbyt+3*2 ge long(uv4)*uv5) then begin
			       if mini le 0 then wr=alog(temporary(wr)-mini+0.1) $
			         	    else wr=alog(temporary(wr))
			       wr=bytscl(congrid(temporary(wr),uv4,uv5,/interp))
;			   endif
			endelse
		endelse
		!order=worder
return
end

pro p_did_save_format, form
;** *****************
;**
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv
    common c_savt,	sav_tab ,sav_tap,sav_forp,sav_m

    sav_form=form
    if form eq 6 then sav_form=sav_forp
    if form gt 6 then begin sav_forp=form
                      widget_control,bad_id=i,sav_m,set_value=sav_tab(form) & endif
return
end

pro p_did_save_list, event
;** ***************
;**
@lamp.cbk
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv
	sav_idx=event.index
	if (sav_idx ge 0) and (sav_idx lt n_elements(limtxt)) then begin
	    wi=fix(strmid(limtxt(sav_idx),1,2))
	    if (wi ge 1) and (wi le 20) then begin
	    	num=strcompress(w_numor(wi),/remove_all)
	    	if num ne '' then begin
	    		widget_control,sav_f,bad_id=i,set_value=num
	    		sav_file=num
	endif & endif & endif
return
end

pro p_did_save_filename,event
;** *******************
;**
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv

      widget_control,sav_f,bad_id=i,get_value=fname
      sav_file=sys_dep      ('BLANKS',fname(0))
      i=strpos(sav_file,'.')
      if i ge 0 then sav_file=strmid(sav_file,0,i)+'_'+strmid(sav_file,i+1,20)
return
end

pro p_did_save_seq
;** **************
;**
    common c_save,	sav_form,sav_idx,sav_f,sav_file,sav_l,sav_seq,sav_b,sav_pthv,sav_uv

      sav_seq=0
      k=0
      res=findfile(sav_pthv+sav_file+'*',count=k)

      i=0
      while k gt 0 do begin i=i+1 & res=findfile(sav_pthv+sav_file+'_'+strtrim(string(i),2)+'*',count=k)
      		   endwhile
      sav_seq=i
return
end

;*************************************** Process Restore Wi ******************************
;*************************************** Process Restore Wi ******************************
;*************************************** Process Restore Wi ******************************
function read_tmp, INST , PATH , FILENAME , STATUS , DATP
;******* ********
;**
 DATA=dist(40)         & STATUS=0 &  pall=INDGEN(40,6)
 xv  =INDGEN(40)*2 +15 & wt=' Template_read test '
 par =[3.0,5.5]        & xt=' This is the X axis '
 ptxt=['First  parameter','Second parameter' ]
 DATP={X:xv,W_TIT:wt,X_TIT:xt,P:par,PAR_TXT:ptxt,PV:pall}
 return,DATA
end

pro read_lamp,file, w=wi, path=pth
;** *********
;**
auto=-1
if (n_elements(wi) eq 1) and (n_elements(file) gt 0) then $
if (wi gt 0) and (wi le 23)   and (file(0) gt ' ')   then begin
						     if n_elements(pth) ne 1 then pth=''
						     ws=strtrim(string(wi),2) & fil=file
		   				     if (strpos(file(0),'_LAMP') lt 0) and $
						        (strpos(file(0),'.htm' ) lt 0) and $
						        (strpos(file(0),'.xml' ) lt 0) and $
						        (strpos(file(0),'.nxs' ) lt 0) and $
						        (strpos(file(0),'.hdf' ) lt 0)  then fil(0)=fil(0)+'_LAMP'
						     p_did_restore_wrk,fil,pth,ws,'',auto & endif
						     if strpos(fil(0),'{') gt 0 then file(0)=fil(0)
						     if (wi le 20) then $
						     to_don_history, wi,0,'READ_LAMP,"'+pth+file(0)+'",w='+ws
if auto lt 0 then print,string(7b)+'file not read ...!'
return
end

pro read_myGIF,file, w=wi
;** **********
;**
@lamp.cbk
	wstr='w'+strtrim(string(wi),2) & XICUTE, wstr+'=0'
	i=execute('READ_KIF,file,'+wstr)
	to_don_history, wi,0,'READ_KIF,'+file+', '+wstr
end

pro p_did_res_hdf,fil,pth,ws,hyst,rflag
;** *************
;** Read  HDF LAMP format
@lamp.cbk

if rflag eq -2 then look=1 else look=0
if strpos(strlowcase(fil),'.xml') gt 0 then xml=1 else xml=0
rflag=-1
ac   = 0
      i=strpos(fil,'{') & nimg=1
      if i gt 1 then begin j=strpos(fil,'}')
                     ac=1
                     if j gt i+1 then nimg=strmid(fil,i+1,j-i-1)
                     fil=strmid(fil,0,i)
                     endif
if look eq 1 then begin Data=-88
	if xml then read_xml,pth+fil, Data, SNAP=w_buf, IMG=nimg  $
	       else read_hdf,pth+fil, Data, SNAP=w_buf, IMG=nimg
	if n_elements(Data) gt 1 then begin hyst=Data & rflag=0 & endif
endif else  begin
      if xml then proc='read_xml' else proc='read_hdf'
      wi=fix(ws)
      ii=execute(proc+',pth+fil,w'+ws+',XC=x'+ws+',YC= y'+ws+',ZC=z'+ws+',E=e'+ws      + $
                      ',N= n'+ws+',PR=p'+ws+',PV=pv'+ws+',PAR_TXT=p_txt,W_TIT=wt,X_TIT=xt'+ $
                      ',Y_TIT=yt,Z_TIT=zt,OTHER_TIT=ot ,SOURCE=src, HIST=hyst, IMG=nimg')
      if ii eq 1 then begin
         par_txt(wi,*)='' & npar=n_elements(p_txt) & if npar gt npars then npar=npars
         rflag=0
         if npar gt 0 then for i=0,npar-1 do par_txt(wi,i)=p_txt(i)
         head_tit(wi,2)=src
         w_tit(wi)=wt & x_tit(wi)=xt & y_tit(wi)=yt & z_tit(wi)=zt & other_tit(wi)=ot
	 if src ne '' then if strlowcase(inst_value) ne strlowcase(src) then RDSET,inst=src
      endif
endelse
if not ac then fil=fil+'{1.1}'
if xml then if (rflag eq 0) then rflag=10
end

pro p_did_restore_wrk,fil,pth,wnumber,hyst,rflag
;** *****************
;** Read  LAMP  format
;** Incoming:   input filename 		= pth+fil
;**		workspace string number = wnumber

@lamp.cbk
if n_elements(fil) eq 2 then if strpos(fil(0),'{') lt 0 then fil=fil(0)+'{'+fil(1)+'}'

if strpos(strlowcase(fil),'.xml') ge 0 then begin p_did_res_hdf,fil,pth,wnumber,hyst,rflag & return & endif
if strpos(strlowcase(fil),'.hdf') ge 0 then begin p_did_res_hdf,fil,pth,wnumber,hyst,rflag & return & endif
if strpos(strlowcase(fil),'.nxs') ge 0 then begin p_did_res_hdf,fil,pth,wnumber,hyst,rflag & return & endif
;                          ****
if rflag eq -2 then look=1 else look=0
rflag=-1

;**Read Header
;****** ******
   standard=0
   unit=-1
   on_ioerror,pathread		& ok=0
   openr,unit,fil ,/get_lun	& ok=1
   pathread:  on_ioerror,endhead
   if ok eq 0 then openr,unit, pth+fil ,/get_lun
   standard=1

   keyw=['LAMP_FORMAT'   , 'DATA_FILE:'    , 'HISTORY:'   , 'X_SIZE:'    , 'Y_SIZE:'  , $
	 'Z_SIZE:'       , 'FORMAT:'       , 'TYPE:'      , 'PARAMETERS:', 'MONITORS:', $
	 'X_COORDINATES:', 'Y_COORDINATES:', 'TITLES:'    , 'VAR PARAM:' , 'MACHINE:' , $
	 'Z_COORDINATES:', 'ERRO_FILE:'    , 'SOURCE:']
   nkey=n_elements(keyw)-1

   data=''   & wyst='' & xsiz=0  & ysiz=0  & zsiz=0 & frmt=0  & tipe=0
   parm=0    & ttl ='' & moni=0  & cdnx=0  & cdny=0 & cdnz=0  & npar=0
   ttx =''   & tty ='' & ttz ='' & tto ='' & parv=0 & erro='' & sorc=''

   partx =['']
   hyst  =''
   line  =' '
   mach  ='uni'

   while (1) do begin
	readf,unit, line
	if look eq 1 then hyst=[hyst,line]

	n=-1 & r=-1
	while (n lt nkey) and (r lt 0) do begin
	       n=n+1
	       r=STRPOS(line,keyw(n))
	endwhile

	if r ge 0 then begin
	 r=STRPOS(line,':')
	 if r ge 0 then remi=strtrim( strmid(line,r+1,100) ,2)

	 CASE keyw(n) of
	'LAMP_FORMAT':   begin standard=1
			 end
	'DATA_FILE:':	 begin data= strtrim(remi,2)
			 end
	'ERRO_FILE:':	 begin erro= strtrim(remi,2)
			 end
	'HISTORY:':	 begin wyst= strtrim(remi,2)
			 end
	'SOURCE:':	 begin sorc= strtrim(remi,2)
			 end
	'X_SIZE:':	 begin xsiz= long(remi)
			 end
	'Y_SIZE:':	 begin ysiz= long(remi)
			 end
	'Z_SIZE:':	 begin zsiz= long(remi)
			 end
	'FORMAT:':	 begin if remi eq 'Binary' 		then frmt=1
			       if remi eq 'Ascii'  		then frmt=2
			       if remi eq 'XDR'    		then frmt=3
			       if remi eq 'F77 unformatted'	then frmt=4
			 end
	'TYPE:':	 begin if strmid(remi,2,1) eq ')' then r=1 else r=2 ;+newtypes*****
	        	       tipe=fix( strmid(remi,1,r) )
			 end
	'TITLES:':	 begin ttl = remi
			 	r=1
			 	while r ge 0 do begin
				    readf,unit, line
			 	    if look eq 1 then hyst=[hyst,line]
				    r=STRPOS(line,' X:')
				    if r ge 0 then    ttx=strtrim(strmid(line,r+3,80),2) $
				    else begin        r  =STRPOS(line,' Y:')
				     if r ge 0 then   tty=strtrim(strmid(line,r+3,80),2) $
				     else begin       r  =STRPOS(line,' Z:')
				      if r ge 0 then  ttz=strtrim(strmid(line,r+3,80),2) $
				      else begin      r  =STRPOS(line,' OTHER:')
				       if r ge 0 then tto=strtrim(strmid(line,r+7,80),2)
				    endelse & endelse & endelse
				endwhile
			 end
	'PARAMETERS:':	 begin
			 readf,unit, line
			 if look eq 1 then hyst=[hyst,line]
			 r=STRPOS(line,'--')
			 if r ge 0 then begin
			 	r=0 & npar=0
			 	while r ge 0 do begin
				    readf,unit, line
			 	    if look eq 1 then hyst=[hyst,line]
				    r=STRPOS(line,'* ')
				    if r ge 0 then begin   ip=STRPOS(line,'==')
					   if ip lt 0 then ip=STRPOS(line,'=' ) else ip=ip+1
				     if ip gt 0 then begin remi=         strmid(line,r+2,ip-r-1)
				    			   if npar eq 0 then partx=[remi] $
				    			 	        else partx=[partx,remi]
				    			   remi=strtrim(strmid(line,ip+1,15)   ,2)
				    			   if npar eq 0 then parm =[remi] $
				    			 	        else parm =[parm ,remi]
				    			   npar=npar+1
				     endif
				    endif
				endwhile
				on_ioerror,misfloat & parm=float(parm) & misfloat: on_ioerror,endhead
			 endif
			 end
	'VAR PARAM:':	 begin
			 r=STRPOS(remi,'nb=')
			 if r ge 0 then begin
			 	reads, strmid(remi,r+3,10)+'0 0 0' ,da1,da2,da3
			 	if da1 gt 0 then begin
			 	 if da2 gt 0 then if da3 gt 0 then parv=fltarr(da1,da2,da3) $
			 				    else parv=fltarr(da1,da2)    $
			 				    else parv=fltarr(da1)
			 	 readf,unit, line
			 	 readf,unit, parv
			 	 if look eq 1 then begin
					parv= reform(parv,1.*da1*(da2>1)*(da3>1))
					hyst= [hyst,line,string(parv,format='(10G)')]
			 	 endif
			 	endif
			 endif
			 end
	'MONITORS:':	 begin
			 r=STRPOS(remi,'nb=')
			 if r ge 0 then begin
			 	reads, strmid(remi,r+3,10)+'0 0 0' ,da1,da2,da3
			 	if da1 gt 0 then begin
			 	 if da2 gt 0 then if da3 gt 0 then moni=fltarr(da1,da2,da3) $
			 				    else moni=fltarr(da1,da2)    $
			 				    else moni=fltarr(da1)
			 	 readf,unit, line
			 	 readf,unit, moni
			 	 if look eq 1 then begin
					moni= reform(moni,1.*da1*(da2>1)*(da3>1))
					hyst= [hyst,line,string(moni,format='(10G)')]
			 	 endif
			 	endif
			 endif
			 end
	'X_COORDINATES:':if xsiz gt 0 then begin
			 r=STRPOS(remi,'->')
			 if r lt 0 then begin
			        r=STRPOS(remi,'bi_dim')
			 	if r lt 0 then cdnx=fltarr(xsiz) else cdnx=fltarr(xsiz,ysiz)
			 	readf,unit, line
			 	readf,unit, cdnx
			 	if look eq 1 then hyst=[hyst,line,string(cdnx,format='(10G)')]
			 endif else begin  da1=1L
				reads, strmid(remi,0,r-1)+' 1' ,da1
				cdnx=lindgen(xsiz)+da1
			 endelse & endif
	'Y_COORDINATES:':begin
			 r=STRPOS(remi,'->')
			 if r lt 0 then begin
			        r=STRPOS(remi,'bi_dim')
			 	if r lt 0  then begin r=STRPOS(remi,'nb=') & da1=0
				           if r ge 0 then reads,strmid(remi,r+3,20)+' 0',da1
				           if da1 le 0 then da1=ysiz>1
				           cdny=fltarr(da1)
				endif else cdny=fltarr(xsiz,ysiz>1)
			 	readf,unit, line
			 	readf,unit, cdny
			 	if look eq 1 then hyst=[hyst,line,string(cdny,format='(10G)')]
			 endif else begin  da1=1L
				reads, strmid(remi,0,r-1)+' 1' ,da1
				cdny=lindgen(ysiz>1)+da1
			 endelse  & end
	'Z_COORDINATES:':begin
			 r=STRPOS(remi,'nb=')
			 if r ge 0 then begin
			 	reads, strmid(remi,r+3,20)+' 1' ,da1
				cdnz=fltarr(da1>1)
			 	readf,unit, line & readf,unit, cdnz
			 endif else $
				reads,remi+' 0',cdnz
			 	if look eq 1 then hyst=[hyst,line,string(cdnz,format='(10G)')]
			 end
	'MACHINE:':	 mach=strmid(remi,0,3)

	'HOW TO READ':   while (1) do readf,unit, line

	 else:
	 endcase
	endif
   endwhile
   endhead:if unit gt 0 then free_lun,unit else return

;**Test Header
;****** ******
   pngr =-1
   rflag= 0
   zdel = 0
   j=strpos(fil,'.')
   if j gt 0 then jz ='Z' else jz='.Z'

   if (standard eq 0)              then  hyst=[hyst,' ??? Header file not readable ....'] else $
   if (xsiz le 0) and (ysiz le 0)  then  hyst=[hyst,' ??? Data size not specified .....'] else $
   if (frmt eq 0)                  then  hyst=[hyst,' ??? Data format not specified ...'] else $
   if (tipe le 0) or  (tipe ge 16) then  hyst=[hyst,' ??? Data type not specified .....'] else $ ;+newtypes*****
   if (data eq '')                 then  begin rflag=1
;   					hyst=[hyst,' ??? Data file not specified .....']
;					TOUCH_BASE   ???
;				     ***Try imgR
   					pngr=1 & j=strpos(fil,'.')
   					if j gt 0 then data=strmid(fil,0,j)+'imgR.' $
   						  else data=fil+'imgR'
   					if pth ne '' then i=findfile(pth+data+jz,count=cnt) $
   						     else i=findfile(    data   ,count=cnt)
;				     ***Try xdr
   					if (cnt le 0) and (pth gt ' ') then begin
   					   pngr=-1
 					   k   =strlen(pth)
 					   ddir=strmid(pth,0,k-1)+'d'+strmid(pth,k-1,1)
					   data=strmid(fil,0,lamp_6)
   					   if j gt 0  then    data=data+'.'
   					   i   =findfile(ddir+data+jz ,count=cnt)
   					   if cnt gt 0 then frmt=3
   					   if cnt gt 0 then pth =ddir
   					endif
;				     ***Try png
   					if cnt le 0 then begin
   					   pngr=0
   					   if j gt 0 then data=strmid(fil,0,j)+'png.' $
   						     else data=fil+'png'
   					   if pth ne '' then i=findfile(pth+data+jz,count=cnt) $
   						        else i=findfile(    data   ,count=cnt)
   					endif
;				     ***Try jpg
   					if cnt le 0 then begin
   					   pngr=0
   					   if j gt 0 then data=strmid(fil,0,j)+'jpg.' $
   						     else data=fil+'jpg'
   					   if pth ne '' then i=findfile(pth+data+jz,count=cnt) $
   						        else i=findfile(    data   ,count=cnt)
   					endif
;				     ***Try img
   					if cnt le 0 then begin
   					   pngr=0
   					   if j gt 0 then data=strmid(fil,0,j)+'img.' $
   						     else data=fil+'img'
   					   if pth ne '' then i=findfile(pth+data+jz,count=cnt) $
   						        else i=findfile(    data   ,count=cnt)
   					endif
   					if cnt le 0 then rflag=-1
   endif else rflag=1

;**Read the Data
;****** *** ****
     if (look eq 0) and (rflag eq 1) then begin
        if frmt ne 3 then rflag=-1 else rflag=0
	on_ioerror, nofile
	unit=-1
	unet=-1
	ptd =pth

	i=findfile(pth+data,count=cnt)
	if cnt eq 0 then begin
	  i=findfile(data,count=cnt)
	  if cnt gt 0 then ptd='' else begin
	   i=findfile(pth+data+jz,count=cnt)
	   if cnt gt 0 then begin
	      i=strpos(strupcase(pth),'TOUCH')
	      if (i ge 0) or (pth ne '') then begin
		   bid =sys_dep      ('COPY',data+jz,pth)
		   zdel=1
		   ptd =''
	      endif
	      bid=sys_dep      ('UN_Z',ptd+data+jz,lamp_dir)
	   endif
	   if cnt eq 0 then begin
		i=findfile(data+'.',count=cnt)
		if cnt gt 0 then begin data=data+'.' & if erro ne '' then erro=erro+'.'
		endif else begin
			if frmt eq 0 then ext ='hdf'   else if frmt eq 1 then ext ='bin' else $
			if frmt eq 2 then ext ='ascii' else if frmt eq 3 then ext ='xdr' else $
			if frmt eq 4 then ext ='f77'
			i=strpos(fil,'.htm')
			if i gt 0 then begin data=strmid(fil,0,i)+'.xdr' & dat2=strmid(fil,0,i)+'.zip'
			                     if (findfile(pth+data))(0) eq '' then  data=dat2
			endif else data=fil+ext
			if erro ne '' then erro=data+'_e'
		endelse
	   endif
	  endelse
	endif

	i= strpos(data,'.zip')
	if i gt 0 then begin dat2=strmid(data,0,i)+'.xdr'
	                     re =findfile(ptd+dat2,count=cnt)
	                     if  cnt eq 0 then re=sys_dep('UNZIP',data,ptd) & data=dat2 & endif

	if (frmt eq 1) or (frmt eq 2)	then OPENR,  unit, ptd+data ,/GET_LUN
	if (frmt eq 3)			then OPENR,  unit, ptd+data ,/GET_LUN,/XDR
	if (frmt eq 4)			then OPENR,  unit, ptd+data ,/GET_LUN,/F77

	rflag=0
	doe=erro
	if erro ne '' then if strpos(doe,'sqrt(i)') ge 0 then erro='' else doe=''
	if erro ne '' then begin
	on_ioerror, noerro
	flge=0
	if (frmt eq 1) or  (frmt eq 2)	then OPENR,  unet, pth+erro ,/GET_LUN
	if (frmt eq 3) and (pngr ne 0)  then OPENR,  unet, pth+erro ,/GET_LUN,/XDR
	if (frmt eq 4)			then OPENR,  unet, pth+erro ,/GET_LUN,/F77
	flge=1
	noerro:if flge eq 0 then erro=''
	endif

	on_ioerror, enddata
	fil  =data

	if xsiz le 0 then xsiz=1
	if ysiz lt 0 then ysiz=1
	if pngr ne 0 then begin
	   if zsiz gt 1 then i=execute('w'+wnumber+'=MAKE_ARRAY( xsiz,ysiz,zsiz ,TYPE=tipe)' ) else $
	   if ysiz gt 1 then i=execute('w'+wnumber+'=MAKE_ARRAY( xsiz,ysiz      ,TYPE=tipe)' ) else $
	   if ysiz eq 1 then i=execute('w'+wnumber+'=MAKE_ARRAY( xsiz           ,TYPE=tipe)' ) else $
	   if ysiz eq 0 then i=execute('w'+wnumber+'=MAKE_ARRAY(  3  ,xsiz      ,TYPE=tipe)' )
	endif
	i=execute('x'     +wnumber+' =cdnx')
	i=execute('y'     +wnumber+' =cdny')
	i=execute('z'     +wnumber+' =cdnz')
	i=execute('e'     +wnumber+' =0   ')
	i=execute('n'     +wnumber+' =moni')
	i=execute('p'     +wnumber+' =parm')
	i=execute('pv'    +wnumber+' =parv')
	par_txt(fix(wnumber),*)=''
	if npar gt npars then npar=npars
	if npar gt 0 then for i=0,npar-1 do par_txt(fix(wnumber),i)=partx(i)
	w_numor  (fix(wnumber))  =''
	w_tit    (fix(wnumber))  =ttl
	x_tit    (fix(wnumber))  =ttx
	y_tit    (fix(wnumber))  =tty
	z_tit    (fix(wnumber))  =ttz
	other_tit(fix(wnumber))  =tto
	head_tit (fix(wnumber),*)=''
	head_tit (fix(wnumber),2)=sorc
	if sorc ne '' then if strlowcase(inst_value) ne strlowcase(sorc) then RDSET,inst=sorc

	if pngr lt 0 then begin
	   if  frmt ne 2 then i=execute('READU,  unit,w'+wnumber ) else $
	   if  frmt eq 2 then i=execute('READF,  unit,w'+wnumber )
	   if  erro ne '' then begin
	    i=execute('e'+wnumber+ '=w'+wnumber)
	    if frmt ne 2 then i=execute('READU,  unet,e'+wnumber ) else $
	    if frmt eq 2 then i=execute('READF,  unet,e'+wnumber )
	   endif
	endif else begin
	   if pngr eq 0 then begin
	   		     if frmt eq 3 then READ_KIF,pth+data,buf $
	   		                  else READ_KIF,pth+data,buf, 192,192
	   		     buf=float (buf)
	   		     if zsiz gt 1 then begin
	   		        if  xsiz gt ysiz then  ysiz=ysiz*zsiz else $
	   		        if (ysiz gt zsiz) and (ysiz ne xsiz) then ysiz=zsiz
	   		        zsiz=1
	   		     endif
	   		     i=execute('w'+wnumber+'=CONGRID(buf,xsiz,ysiz,/INTER)' )
	   endif else begin  bxs=fix(0) & bys=fix(0) & bts=fix(0)
	   		     READU, unit, bxs,bys,bts
	   		     i=execute('READU,  unit,w'+wnumber )
	   endelse
	endelse

	if doe ne '' then $
	if doe eq 'sqrt(i)'       then i=execute('e'+wnumber+' =sqrt(w'+wnumber+')')                else $
	if doe eq 'sqrt(i)/i'     then i=execute('e'+wnumber+' =sqrt(w'+wnumber+')/ w'+wnumber)     else $
	if doe eq 'sqrt(i)/(i+1)' then i=execute('e'+wnumber+' =sqrt(w'+wnumber+')/(w'+wnumber+'+1)')

	enddata:if  unit gt 0  then free_lun,unit

	if pngr lt 0 then $
	if ysiz eq 0 then if frmt eq 2  then begin
					i=execute('e'+wnumber+'=reform(w'+wnumber+'(2,*))' )
					i=execute('w'+wnumber+'=reform(w'+wnumber+'(1,*))' )
					endif
	res=sys_dep      ('SWAPER',mach)
	if (frmt eq 1) and (res eq 1) then begin ;+newtypes*****
				if tipe eq 2  then i=execute('BYTEORDER,w'+wnumber+',/SSWAP')   else $
				if tipe eq 3  then i=execute('BYTEORDER,w'+wnumber+',/LSWAP')   else $
				if tipe eq 12 then i=execute('BYTEORDER,w'+wnumber+',/SSWAP')   else $
				if tipe eq 13 then i=execute('BYTEORDER,w'+wnumber+',/LSWAP')   else $
				if tipe eq 14 then i=execute('BYTEORDER,w'+wnumber+',/L64SWAP') else $
				if tipe eq 15 then i=execute('BYTEORDER,w'+wnumber+',/L64SWAP')
				if erro ne '' then $
				if tipe eq 2  then i=execute('BYTEORDER,e'+wnumber+',/SSWAP') else $
				if tipe eq 3  then i=execute('BYTEORDER,e'+wnumber+',/LSWAP')
	endif

	nofile: if unit lt 0 then print,string(7b),'% File '+pth+data+' not found ...'
	if zdel eq 1 then bid =sys_dep      ('DELET',data)

     endif

  if rflag ge 0 then if frmt eq 3  then rflag=10
  if look  eq 0 then if hyst eq '' then hyst =strtrim(wyst,2) else print,string(7b),hyst

end

;*************************************** Data access ******************************
;*************************************** Data access ******************************
;*************************************** Data access ******************************

pro P_DATA_ACCESS, laber,b33,bac,butb, flag
;** *************
;**
;** Create Data Access buttons
@lamp.cbk

if b33 gt 0 then begin
   if flag ne 0 then begin
	  tmpbase=0    & P_messi , tmpbase,(lamp_b1+0)

	  P_DATA_IDOL
	  if lamp_siz ge 800 then bid = widget_label (b33     ,font=ft_b_normal,value='Ins:')
	  NEW=inst_value & IF strcompress(NEW) LE ' ' THEN NEW='New !!'
	  if sys_dep('MAP') ne -1 then $
	  b_labins(0)=widget_button(b33     ,font=ft_b_normal,value=NEW,menu=2,$
	  						      resource_name='discret') else $
	  b_labins(0)=widget_button(b33     ,font=ft_b_normal,value=NEW,menu=2)

	 ;if lamp_siz ge 900 then bid = widget_label (b33     ,font=ft_b_normal,value='Pth:')
	  if sys_dep('MAP') ne -1 then $
	  b_labins(1)=widget_button(b33     ,font=ft_b_normal,value=cycle     ,menu=2,$
	  						      resource_name='discret') else $
	  b_labins(1)=widget_button(b33     ,font=ft_b_normal,value=cycle     ,menu=2)

	  to_don_history,-1,0,'RDSET,base="'+cycle   ;+'",inst="'+inst_value+'"'

	  uval =[-88,560,laber,b_labins(0),b_labins(1)]
	  gcur =' '
	  entr1=b_labins(0)
	  for i=0,n_elements(lamp_ins)-1 do begin
	      if  gcur ne  lamp_grp(i) then begin
		  gcur  =  lamp_grp(i)
		  if gcur eq ' ' then entr1=b_labins(0) else $
		  entr1 =widget_button(b_labins(0),font=ft_b_normal,value=gcur,menu=2)
		  endif
	      bidon=widget_button(entr1  ,font=ft_b_normal,value=lamp_ins(i),uvalue=[uval,i,0,0])
	  endfor
	  bidon=widget_button(b_labins(0),font=ft_bigger ,value='CUSTOMIZE',uvalue=[uval,-1,0,0])

	  uval =[-88,561,laber,b_labins(0),b_labins(1)]
	  for i=0,n_elements(lamp_ali)-1 do begin
	      if strpos(strlowcase(lamp_ali(i)),'c_year') ge 0 then begin
		yr =strtrim(strmid(lamp_ali(i),7,15),2) & yr=strmid(yr,2,2)
		didon=widget_button(b_labins(1),font=ft_b_normal,menu=2		    ,value=lamp_ali(i))
		for j=1,5 do begin  yrs=yr+strtrim(string(j),2)
		  bid=widget_button(didon      ,font=ft_b_normal,uvalue=[uval,i,0,long(yrs)],value='Cycle '+yrs)
		endfor
	      endif else $
		bidon=widget_button(b_labins(1),font=ft_b_normal,uvalue=[uval,i,0,0]	    ,value=lamp_ali(i))
	  endfor

	  uvbuti=[-88,575,laber,0,b33,0,0,0,0]
	  P_MAC_COMPLETE, uvbuti ,butb
	  widget_control,bad_id=i,b33, set_uvalue=uvbuti

   	  bid=sys_dep      ('DYNLAB',b33,1)
	  P_messi , tmpbase,(lamp_b1+0)
   endif
   if ((flag eq 0) or (flag eq 2)) and (lamp_data ne 'hostvms') then begin
	  if lamp_siz ge 800 then text='Self...' else text='Self...'
	  widget_control,bad_id=i,b33, get_uvalue=uvbuti
	  widget_control,bad_id=i,bac ,set_value = text, set_uvalue=uvbuti
   endif
endif
return
end
pro	P_MAC_COMPLETE, uv ,butb
;**	**************
;**
@lamp.cbk
if uv(3) eq 0 then begin
	bsup =widget_base  (uv(4),/row)
	if lamp_siz ge 800 then bid = widget_label (bsup,font=ft_b_normal,value='File:')

	bnum =widget_text  (bsup,xsiz=14,ysize=1,font=ft_propor,/editable,value='Name')
	
	if sys_dep('MAP') ne -1 then $
	bget =widget_button(bsup,font=ft_b_normal,value='Read ',resource_name='discret') else $
	bget =widget_button(bsup,font=ft_b_normal,value='Read ')
	
	if sys_dep('MAP') ne -1 then $
	bnex =widget_button(bsup,font=ft_smaller ,value='+1',resource_name='discret') else $
	bnex =widget_button(bsup,font=ft_smaller ,value='+1')
	uv(3)=bnum
	if lamp_siz ge 800 then text='raw ->' else text='raw'
	bidon=widget_base  (bsup,/nonexclusive)
	if sys_dep('MAP') ne -1 then $
	braw =widget_button(bidon,value=text,font=ft_smaller,resource_name='discret') else $
	braw =widget_button(bidon,value=text,font=ft_smaller)
	b_labins(4) =braw

	widget_control,butb,get_uvalue=uvb & wread=uvb(4)

	if uvb(5) ne -2 then begin
	 bs1f =widget_base  (bsup,/row,/frame)
	 if sys_dep('MAP') ne -1 then $
	 bs1b1=widget_button(bs1f,font=ft_smaller ,value='<',resource_name='discret') else $
	 bs1b1=widget_button(bs1f,font=ft_smaller ,value='<')
	 wread=widget_label (bs1f,font=ft_b_normal,value='W1 ',xsize=29)
	 if sys_dep('MAP') ne -1 then $
	 bs1b2=widget_button(bs1f,font=ft_smaller ,value='>',resource_name='discret') else $
	 bs1b2=widget_button(bs1f,font=ft_smaller ,value='>')

	 widget_control, bs1b1  ,bad_id=i,set_uvalue=[-88,310,wread,0   ,0,0,0,0,0]
	 widget_control, bs1b2  ,bad_id=i,set_uvalue=[-88,311,wread,0   ,0,0,0,0,0]
	endif
	
	if (sys_dep('MACHINE') eq 'win') and (sys_dep('VERSION') lt '5.3') then txev=0 else txev=1
	
	widget_control, braw   ,bad_id=i,set_uvalue=[-88,312,0]
	widget_control, butb   ,bad_id=i,set_uvalue=[-88,562,uv(2),bnum,wread,-1,-1,0],sensitive=1
	widget_control, bget   ,bad_id=i,set_uvalue=[-88,577,uv(2),bnum,wread,0,0,0,0]
	if txev then $
	widget_control, bnum   ,bad_id=i,set_uvalue=[-88,577,uv(2),bnum,wread,0,0,0,0]
	widget_control, bnex   ,bad_id=i,set_uvalue=[-88,578,uv(2),bnum,wread,0,0,0,0]

	if n_elements(monimon) eq 0 then monimon=-1
	if monimon lt 0 then    widget_control,braw    ,bad_id=i,set_button=1
endif
return
end

pro P_DATA_IDOL
;** ***********
@lamp.cbk
    	if (lamp_data  eq 'hostvms') then begin
    				if lamp_exec ne '' then tmp=findfile(lamp_exec,count=j) else j=0
				if j le 0 then  lamp_data  =''
				if j gt 0 then  begin cycle='On_Line'
						inst_value = lamp_host
						lamp_exec  ='lamp_exec'
					;	lamp_entry ='r_mic'
						endif
    	endif else begin	j=0
				if lamp_exec gt ' '   then tmp=findfile(lamp_exec,count=j)
				if j le 0 then begin
				  lamp_exec=sys_dep      ('EXEC',lamp_dir)
				  if lamp_exec gt ' ' then tmp=findfile(lamp_exec,count=j)
				endif
				if j gt 0 then if lamp_cyc(0) eq 0 then begin
				  lamp_data ='idol'
				  lc  =where(lamp_ali eq 'On_Line') & lamp_cyc(0)=lc(0)>0
				  cycle     =lamp_ali(lamp_cyc(0))
				  if inst_value eq '' then inst_value='?Inst?'
				 ;lamp_entry=sys_dep      ('ENTRY')
				endif
    	endelse
end

pro	P_MAC_LABINS
;**	************
;**
@lamp.cbk
	   NEW=inst_value & IF strcompress(NEW) LE ' ' THEN NEW='New !!'
	   if b_labins(0) gt 0 then widget_control,b_labins(0),bad_id=i,set_value=NEW
	   if b_labins(1) gt 0 then widget_control,b_labins(1),bad_id=i,set_value=cycle
return
end

function flto6, run
;******* *****
;**
@lamp.cbk
    	file=strtrim(string(run),2) & ln=strlen(file)
    	while ln lt lamp_6 do begin file='0'+file & ln=ln+1 & endwhile
return, file
end

pro CALIBRATION ,FILE=file ,NOCAL=nocal ,LIST=list
;** ***********
@lamp.cbk
    ii=0 & ii=execute('ii=rdid()')
    if keyword_set(file)  then P_DID_CALOD, strlowcase(inst_value) ,file ,flg	else $
    if keyword_set(nocal) then P_DID_CALOD, strlowcase(inst_value)		else $
    if keyword_set(list)  then P_DID_CALOD, strlowcase(inst_value) ,/LIST	else $
    if b_labins(3) ge  1  then P_DID_CALOD, strlowcase(inst_value) ,/LIST	     $
			  else P_DID_CALIB, inst_value+'' , lamp_b1
return
end

function rlamp, inst,path,file,status,datp
;******* *****
;**
status=0
		 pp2=-1
		 p_did_restore_wrk, file,path,datp,'',pp2
		 if pp2 lt 0 then status=11
return,1
end

pro p_did_get_it, run,wi,status,uv ,run_fil
;** ************
;**
@lamp.cbk
    if uv(2) gt 0 then $
       if strpos(path_for_online,strlowcase(inst_value)) gt 0 then $
            widget_control,uv(2),bad_id=i,set_value='Checking '+path_for_online+' ...' $
       else widget_control,uv(2),bad_id=i,set_value='Reading ...'
    ran=run
    if run_fil eq 'run' then p_did_getrun, run,wi,status
    if run_fil eq 'fil' then p_did_getfil, run,wi,status
    if run_fil eq 'opr' then rdmulti     , run   ,status,uv(2),wi

    if status ne 0 then begin
		 	     errtxt=' Un_implemented status '+string(status)
	if status eq 1  then errtxt=' Client/server on local node not established'
	if status eq 2  then errtxt=' Client/server on router node not established'
	if status eq 3  then errtxt=' The local node cannot access the server node'
	if status eq 4  then errtxt=' The router node cannot access the server node'
	if status eq 5  then errtxt=' VME memory read error'
	if status eq 6  then errtxt=' No host defined'
	if status eq 7  then errtxt=' Sequence error in data transfer'
	if status eq 8  then errtxt=' Memory space or buffer too small'
	if status eq 9  then errtxt=' Parameter error'
	if status eq 10 then errtxt=' Router is busy with other transfer'
	if status eq 11 then errtxt=' Cant open the file'
	if status eq 12 then errtxt=' Syntax error'
	if status eq 13 then errtxt=' Data file incomplete'
	if status eq 14 then errtxt=' Bad instrument data definition'
	if status eq 23 then errtxt=' Internal error'
	if status eq 24 then errtxt=' Cant read the file'
	print,string(7b)
	if uv(2) gt 0 then widget_control,uv(2),bad_id=i,set_value=errtxt else print,errtxt
    endif  else begin
	strun=strtrim(string(run),2) & stwi='w'+strtrim(string(wi),2)
	
	if uv(2) gt 0 then widget_control,uv(2),bad_id=i,set_value=strun+' may be plotted from '+stwi
	if uv(3) gt 0 then $
	widget_control,bad_id=i,uv(3),set_value=strun
	if run_fil eq 'run' then cmd="=RDRUN(" +strun+") ;.."  else $
	if run_fil eq 'fil' then cmd="=RDRUN(" +strun+") ;.."  else $
				 cmd="=RDOPR(" +strun+") ;.."
	if run_fil ne 'opr' then RDSET,golast=[wi,wi]
	to_don_history, wi,0,stwi+ cmd+inst_value
    endelse
return
end

pro p_did_getfil, run,wi,full
;** ************
;**
i=strpos(run,'{') & nimg=0
if i ge 1 then begin j=strpos(run,'}') & m=0
                     if j gt i+1 then begin
                     	for k=i+1,j-1 do begin
				c=strmid(run,k,1)
				if ((c lt '0') or (c gt '9')) and (c ne ' ') then m=1
			endfor
                     	if m eq 0 then nimg=long(strmid(run,i+1,j-i-1))
                     endif
                     if m eq 0 then run=strmid(run,0,i) & endif

if nimg le 1 then p_did_getrun , run,wi,full,'file' $
             else p_did_getrun , run,wi,full,'file' ,FRAME=nimg

return
end

pro p_did_getrun, run,wi,full,fifi ,FRAME=nimg
;** ************
;**
@lamp.cbk
common rd_filter,filter_proc

full	=23
status  =14
wstr    =strtrim(string(wi),2)
inst_val=strlowcase(inst_value)

idx=where(lamp_ins eq inst_value) & idx=idx(0) > 0 & proced=lamp_proc(idx)
if (proced  gt ' ') and (proced ne 'ill') and ((proced ne 'mic') or (strpos(strupcase(run),'_LAMP') gt 0)) $
		    and (cycle  ne 'On_Line')  then begin
		 	  if strpos(strlowcase(cycle),'cycle') lt 0 then kc='' else kc='1'
			  if monimon lt 0 then km='1' else km='0'
    			  inst  =[inst_value,inst_group,km,kc]
    			  path  =path_for_online
    			  if n_elements(fifi) eq 0 then begin
    			  	 file  =flto6(run)
    			  	 if  strlowcase(proced) eq 'rlamp' then $
		   		     if (strpos(file,'_LAMP') lt 0) and $
				        (strpos(file,'.htm' ) lt 0) and $
				        (strpos(file,'.xml' ) lt 0) and $
				        (strpos(file,'.nxs' ) lt 0) and $
				        (strpos(file,'.hdf' ) lt 0) then file=file+'_LAMP'
    			  	 bid =sys_dep      ('POT',file)
    			  endif else file=run
			  ac=strpos(file,'{')
    			  if n_elements(nimg) eq 1 then if nimg gt 0 then file=[file,strtrim(string(nimg),2)]

	    		  iii   =execute( 'w'+wstr+'=0') & datp=wstr
	    		  clearpar, wi,wstr
    	    		  head_tit (wi,2)=inst_value
      	    		  w_numor  (wi)  =strtrim(string(run),2)

			  if (proced eq 'rlamp') or (strpos(strupcase(file(0)),'_LAMP') gt 0) then $
			  iii   =rlamp(inst,path,file,status,datp)                  else $
    			  iii   =execute( 'w'+wstr+'=call_function(proced,inst,path,file,status,datp)')

	    		  if iii ne 1 then status=23 else if status  eq 0 then $
    	    		     iii =execute( 'if n_elements(w'+wstr+') eq 1 then if w'+wstr+' eq 0 then status=13' )

	    		 ;if (status eq 0) then begin
      	    		      one=wi
	    		      GETDATP,datp
	    		      if ac lt 0 then if strpos(file(0),'{') gt 0 then run=file(0)
    	    		 ;endif
endif  else $
if inst_val eq 'pn1'  then begin
    			  status=0
    			  file  =strtrim(string(run),2)
    			  one =wi & two=0
    			  iii =execute( 'w'+wstr+'=pn1(file)' )
    			  if iii ne 1 then status=11 else $
    			     iii =execute( 'if n_elements(w'+wstr+') eq 1 then status=w'+wstr )
endif  else $
if inst_val eq 'inx'  then begin
    			  status=0
    			  file  =strtrim(string(run),2)
    			  one =wi & two=0
    			  iii =execute( 'w'+wstr+'=inx_in(file)' )
    			  if iii ne 1 then status=11 else $
    			     iii =execute( 'if n_elements(w'+wstr+') eq 1 then status=w'+wstr )
endif  else $
if (cycle eq 'On_Line') then begin
			     RPCILL, inst_val,run,wi,wstr,status
endif else begin

;*******TOF LSS
;*******TOF LSS
	    mic,1
	    clearpar,  wi,wstr
    	    fil =flto6(run)

    	    pthv=path_for_online
	    bid =findfile(pthv+FIL+'.Z',count=cprs)
	    IF  cprs GT 0     THEN BEGIN
		IF pthv NE '' THEN bid=sys_dep      ('COPY',FIL+'.Z',pthv) ELSE cprs=0
			   bid=sys_dep      ('UN_Z',FIL+'.Z')    &   path_for_online=''
	    ENDIF


    	    bid =sys_dep('POT',fil)
	    file_found =[ path_for_online+fil ]
      	    w_numor(wi)=  strtrim(string(run),2)
	    nwk_select =  wi
	    swk_select =  wstr
	    status     =0
	    iii=execute('w'+wstr+'=0')
	    iii=execute('read_data')
	    if iii ne 1 then status=23 else $
    	       iii=execute( 'if n_elements(w'+wstr+') eq 1 then status=24' )

    	    path_for_online=pthv
	    IF cprs gt 0 THEN bid=sys_dep      ('DELET',FIL)
endelse

n=n_elements(filter_proc)
if (status eq 0) and (n gt 0)	then if filter_proc(0) ne '' $
				then for i=0,n-1 do CALL_PROCEDURE,filter_proc(i),wi
full=status
return
end

pro filterpro, name
;**
common rd_filter,filter_proc
filter_proc=name
end

pro clearpar, wi,wstr
;** ********
@lamp.cbk
    	        iii=execute('p' +wstr+'= 0') & iii=execute('pv'+wstr+'= 0')
    	        iii=execute('e' +wstr+'= 0')
     	        iii=execute('n' +wstr+'= 0') & iii=execute('x' +wstr+'= 0')
     	        iii=execute('y' +wstr+'= 0') & iii=execute('z' +wstr+'= 0')
     	        par_txt  (wi,*)	 ='' & w_numor(wi)	 =''
     	        w_tit    (wi)	 ='' & x_tit  (wi)='' & y_tit(wi)='' & z_tit(wi)=''
     	        other_tit(wi)	 =''
     	        head_tit (wi,*)  =''
return
end
pro movepar, wi,wsti,wo,wsto
;** *******
@lamp.cbk
	   if wi ne wo then begin
    	      iii=execute('p' +wsto+'= p' +wsti)
    	      iii=execute('pv'+wsto+'= pv'+wsti)
     	      iii=execute('e' +wsto+'= e' +wsti)
     	      iii=execute('n' +wsto+'= n' +wsti)
     	      iii=execute('x' +wsto+'= x' +wsti)
     	      iii=execute('y' +wsto+'= y' +wsti)
     	      iii=execute('z' +wsto+'= z' +wsti)

     	      par_txt  (wo,*)   =par_txt  (wi,*)
     	      w_numor  (wo)     =w_numor  (wi)
    	      w_tit    (wo)	=w_tit    (wi)
     	      x_tit    (wo)	=x_tit    (wi)
     	      y_tit    (wo)	=y_tit    (wi)
     	      z_tit    (wo)   	=z_tit    (wi)
     	      other_tit(wo)   	=other_tit(wi)
     	      head_tit (wo,*) 	=head_tit (wi,*)
     	   endif
return
end

pro take_w, wkp , w=wi
;** ******
@lamp.cbk
if n_elements(wi) ne 1  then $
if two gt 0 then wi=two else if alone gt 0 then wi=alone else wi=one
wkp=[0L]
if wi  gt 0 then if wi  lt n_elements(w_tit) then ii=execute('wkp=w'+strtrim(string(wi),2))
return
end

pro new_w , wkp
;** ******
end

pro give_w, wkp , w=wi
;** ******
@lamp.cbk

if n_elements(wi) ne 1  then $
if one gt 0 then wi=one else if alone  gt 0  then wi=alone else wi=two
if n_elements(wkp) eq 0 then wkp=0
if wi  gt 0 then if wi  lt n_elements(w_tit) then begin
		 ii=execute('w'  +strtrim(string(wi),2)+'=wkp')
		 to_don_history, wi,0,'NEW_w,w'+strtrim(string(wi),2) & endif
return
end

pro W_store    ,W=wii ,ALL=all
;** *******
if keyword_set(ALL) then FOR i=1,20 do A_store, W=(i) $
else A_store, W=wii
end
pro W_restore  ,W=wii ,ALL=all
;** *********
if keyword_set(ALL) then FOR i=1,20 do A_restore, W=(i) $
else A_restore, W=wii
end
pro W_exchange ,W=wii ,ALL=all
;** **********
if keyword_set(ALL) then FOR i=1,20 do A_exchange, W=(i) $
else A_exchange, W=wii
end
pro W_clear    ,W=wii ,ALL=all
;** *******
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

if keyword_set(ALL) then begin FOR i=1,20 do A_clear, W=(i)
	a=0 & b=0 & c=0 & d=0 & e=0 & f=0 & g=0 & h=0 & i=0 & j=0 & k=0 & l=0 & m=0
	n=0 & o=0 & p=0 & q=0 & r=0 & s=0 & t=0 & u=0 & v=0 & w=0 & x=0 & y=0 & z=0
endif else A_clear, W=wii
end

pro A_exchange ,W=wii
;** **********
@lamp.cbk
common W_stload, wst0 ,wst1 ,wst2 ,wst3 ,wst4 ,wst5 ,wst6 ,wst7 ,wst8 ,wst9 ,wst10,$
	         wst11,wst12,wst13,wst14,wst15,wst16,wst17,wst18,wst19,wst20
common P_stload, pst0 ,pst1 ,pst2 ,pst3 ,pst4 ,pst5 ,pst6 ,pst7 ,pst8 ,pst9 ,pst10,$
	         pst11,pst12,pst13,pst14,pst15,pst16,pst17,pst18,pst19,pst20

if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then begin
	ws=strtrim(string(wii),2)
	ii=execute('wst0=wst'+ws)
	if n_elements(wst0) le 1 then return
	ii=execute('pst0=pst'+ws)
	A_store,  W=wii
	A_restore,W=wii ,/TEMP & endif
end
pro A_store ,W=wii
;** *******
@lamp.cbk
common W_stload, wst0 ,wst1 ,wst2 ,wst3 ,wst4 ,wst5 ,wst6 ,wst7 ,wst8 ,wst9 ,wst10,$
	         wst11,wst12,wst13,wst14,wst15,wst16,wst17,wst18,wst19,wst20
common P_stload, pst0 ,pst1 ,pst2 ,pst3 ,pst4 ,pst5 ,pst6 ,pst7 ,pst8 ,pst9 ,pst10,$
	         pst11,pst12,pst13,pst14,pst15,pst16,pst17,pst18,pst19,pst20

if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then begin
	ws=strtrim(string(wii),2)
	ii=execute('wst'+ws+'=w'+ws)
	ii=execute('take_datp,pst'+ws+',W=wii') & endif
end
pro A_restore ,W=wii ,TEMP=temp
;** *********
@lamp.cbk
common W_stload, wst0 ,wst1 ,wst2 ,wst3 ,wst4 ,wst5 ,wst6 ,wst7 ,wst8 ,wst9 ,wst10,$
	         wst11,wst12,wst13,wst14,wst15,wst16,wst17,wst18,wst19,wst20
common P_stload, pst0 ,pst1 ,pst2 ,pst3 ,pst4 ,pst5 ,pst6 ,pst7 ,pst8 ,pst9 ,pst10,$
	         pst11,pst12,pst13,pst14,pst15,pst16,pst17,pst18,pst19,pst20

if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then begin
	ws=strtrim(string(wii),2)
	if keyword_set(temp) then wa='0' else wa=ws
	nel=0L & ii=execute('nel=n_elements(wst'+wa+')')
	if nel le 1 then return
	ii=execute('w'+ws+'=wst'+wa)
	ii=execute('give_datp,pst'+wa+',W=wii')
	to_don_history, wii,0,'W_restore ,W='+ws ,/nojournal & endif
end
pro A_clear ,W=wii
;** *******
@lamp.cbk
common W_stload, wst0 ,wst1 ,wst2 ,wst3 ,wst4 ,wst5 ,wst6 ,wst7 ,wst8 ,wst9 ,wst10,$
	         wst11,wst12,wst13,wst14,wst15,wst16,wst17,wst18,wst19,wst20
common P_stload, pst0 ,pst1 ,pst2 ,pst3 ,pst4 ,pst5 ,pst6 ,pst7 ,pst8 ,pst9 ,pst10,$
	         pst11,pst12,pst13,pst14,pst15,pst16,pst17,pst18,pst19,pst20

wst0=0 & pst0=0
if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then begin
	ws=strtrim(string(wii),2)
	ii=execute('wst'+ws+'=0')
	ii=execute('pst'+ws+'=0')
	ii=execute('W'  +ws+'=0')
	CLEARPAR,wii,ws & endif
end

pro   setdatp, datp
    take_datp, datp & return & end
pro take_datp, datp , w=wi ,second=second ,third=third ,fourth=eme4,fifth=eme5,sixth=eme6
;** *********
@lamp.cbk
common eme456,	fourth,fifth,sixth

if n_elements(wi) ne 1  then $
if two gt 0 then wi=two else if alone gt 0 then wi=alone else wi=one
if keyword_set(second)  then wi=two
if keyword_set(third)   then wi=three
if keyword_set(eme4)    then wi=fourth
if keyword_set(eme5)    then wi=fifth
if keyword_set(eme6)    then wi=sixth
datp={empty:0}
if wi  gt 0 then if wi  lt n_elements(w_tit) then begin
		 wstr=strtrim(string(wi),2)
		 ii=execute('datp={x:		x' +wstr  + $
		 		 ',y:		y' +wstr  + $
		 		 ',z:		z' +wstr  + $
		 		 ',e:		e' +wstr  + $
		 		 ',n:		n' +wstr  + $
		 		 ',p:		p' +wstr  + $
		 		 ',pv:		pv'+wstr  + $
		 		 ',w_tit:	w_tit(wi)'+ $
		 		 ',x_tit:	x_tit(wi)'+ $
		 		 ',y_tit:	y_tit(wi)'+ $
		 		 ',z_tit:	z_tit(wi)'+ $
		 		 ',other_tit:	other_tit(wi)'  + $
		 		 ',time:	head_tit (wi,4)'+ $
		 		 ',par_txt:	par_txt  (wi,*) }')
endif
return
end

pro   getdatp, datp
    give_datp, datp & return & end
pro give_datp, datp , w=wi ,second=second ,third=third ,fourth=eme4,fifth=eme5,sixth=eme6
;** *********
@lamp.cbk
common eme456,	fourth,fifth,sixth

if n_elements(wi) ne 1  then $
if one gt 0 then wi=one else if alone  gt 0  then wi=alone else wi=two
if keyword_set(second)  then wi=two
if keyword_set(third)   then wi=three
if keyword_set(eme4)    then wi=fourth
if keyword_set(eme5)    then wi=fifth
if keyword_set(eme6)    then wi=sixth
if wi  gt 0 then if wi  lt n_elements(w_tit) then begin
	    		if n_tags(datp) gt 0 then begin
			     wstr=strtrim(string(wi),2)
	    		     tlist=tag_names(datp)
	    		     for k=0,n_elements(tlist)-1 do begin
	    		         CASE tlist(k) of

	    		         'X':	 iii =execute('x' +wstr+'=datp.x' )
	    		         'Y':	 iii =execute('y' +wstr+'=datp.y' )
	    		         'Z':	 iii =execute('z' +wstr+'=datp.z' )
	    		         'E':	 iii =execute('e' +wstr+'=datp.e' )
	    		         'N':	 iii =execute('n' +wstr+'=datp.n' )
	    		         'PV':	 iii =execute('pv'+wstr+'=datp.pv')

	    		         'W_TIT':    w_tit(wi)      =string(datp.w_tit)
	    		         'X_TIT':    x_tit(wi)      =string(datp.x_tit)
	    		         'Y_TIT':    y_tit(wi)      =string(datp.y_tit)
	   			 'Z_TIT':    z_tit(wi)      =string(datp.z_tit)
	   			 'OTHER_TIT':other_tit(wi)  =string(datp.other_tit)
	   			 'TIME' :    head_tit (wi,4)=string(datp.time)

	   			 'P'	  :begin  j=n_elements(datp.p)       < npars
	   			 		  if j eq 1 then $
	   			 		  iii=execute ('p'+wstr+'=datp.p') else $
	   			 		  iii=execute ('p'+wstr+'=datp.p(0:j-1)')
	   			 	   end
	   			 'PAR_TXT':begin  j=n_elements(datp.par_txt) < npars
	   			 		  if j eq 1 then par_txt(wi,0)=datp.par_txt else $
	   			 		  for i=0,j-1 do begin
	   			 		  		 par_txt(wi,i)=datp.par_txt(i)
	   			 		  		 if strpos(par_txt(wi,i),'=') le 0 then $
	   			 		  		 par_txt(wi,i)=par_txt(wi,i)+'='
	   			 		  endfor
	   			 	   end
	   			  ELSE:
	   			  ENDCASE
	    		     endfor
			endif
endif
return
end

pro mod_datp,datp,tag,val
;** ********
sz=size(tag)
if (n_tags(datp) gt 0) and (sz(1) gt 0) and (sz(2) eq 1) and (n_elements(val) gt 0) then begin

	    		     x=0 & y=0 & z=0 & e=0 & n=0 & pv=0 & p=0      & par_txt=''
	    		     w_tit=''  & x_tit=''  & y_tit=''   & z_tit='' & other_tit='' & time=''
	    		     tlist=tag_names(datp)
	    		     for k=0,n_elements(tlist)-1 do begin
	    		         CASE tlist(k) of

	    		         'X':	 x =datp.x
	    		         'Y':	 y =datp.y
	    		         'Z':	 z =datp.z
	    		         'E':	 e =datp.e
	    		         'N':	 n =datp.n
	    		         'PV':	 pv=datp.pv

	    		         'W_TIT':    w_tit    =datp.w_tit
	    		         'X_TIT':    x_tit    =datp.x_tit
	    		         'Y_TIT':    y_tit    =datp.y_tit
	   			 'Z_TIT':    z_tit    =datp.z_tit
	   			 'OTHER_TIT':other_tit=datp.other_tit
	   			 'TIME' :    time     =datp.time

	   			 'P'	  :  p        =datp.p
	   			 'PAR_TXT':  par_txt  =datp.par_txt
	   			  ELSE:
	   			  ENDCASE
	    		     endfor

	    		     iii=execute(tag+'=val')
      			     DATP={X:x,Y:y,Z:z,E:e,N:n,PV:pv,W_TIT:w_tit,X_TIT:x_tit    ,$
      			     	   Y_TIT:y_tit,Z_TIT:z_tit,OTHER_TIT:other_tit,TIME:time,$
      			     	   P:p,PAR_TXT:par_txt}
endif
return
end

pro did_objet,wi,objout
;** *********
@lamp.cbk

objout=0
if (wi ge 1) and (wi le 23) and (sys_dep('VERSION') ge 5.1) then begin
	if strpos(strupcase(par_txt(wi,0)),'OBJECT:') eq 0  then begin
	   ii=execute('P=P'+strtrim(string(wi),2)+'(0)')
	   if (P ge 1) and (fix(P) eq P) then objout=P
endif & endif
end

pro set_suf ,input & RDSET,suffix=input & END
pro set_pref,input & RDSET,prefix=input & END
;** ********

pro RDSET , INST=inst , BASE=base  ,CYCLE=cyclo , TOLERANCE=tol ,STEP=step ,RAW=raw ,DEFAULT=def $
	  , DIR=dir   , NORAW=noraw,DIF=dif , LAST=lst , GOLAST=gol , PREFIX=prefx   ,SUFFIX=sufx
;** *****
;** set INST_VALUE , PATH_FOR_ONLINE , Other dependent parameters for read-in procedures
@lamp.cbk
common c_rdid , dzap, pzap, pzip, pzup
common c_rdid2, zapa, zape, zipi, zupo, zupe
common c_edf  , pref, suf

if keyword_set(inst) then begin
   ins=strlowcase(strtrim(inst,2))
   idx=where  (strlowcase(lamp_ins) eq ins) & idx=idx(0)
   if  idx ge 0 then P_MAC_EVENT  ,0 ,[-88,560,0,b_labins(0),b_labins(1),idx,0,0]
   endif
if keyword_set(base) then begin
   bas=strtrim(base,2)
   idx=where  (strlowcase(lamp_ali) eq strlowcase(bas)) & idx=idx(0)
   if n_elements(cyclo) ne 1 then cyclo=0
   if  idx ge 0 then P_MAC_EVENT  ,0 ,[-88,561,0,b_labins(0),b_labins(1),idx,0,long(cyclo)]
   endif
if keyword_set(raw)	 then begin if b_labins(4) gt 0 then widget_control,bad_id=ii,b_labins(4),set_button=1
				    monimon=-1 & endif
if (keyword_set(noraw) or $
    keyword_set(def))	 then begin if b_labins(4) gt 0 then widget_control,bad_id=ii,b_labins(4),set_button=0
				    monimon= 0 & endif
				    
if n_elements(lst)   eq 1 then  if (size(lst))(1)    eq 7 then if lst  gt ' ' then zapa=lst

if n_elements(gol)   eq 2 then  if  n_elements(zapa) eq 1 then if zapa gt ' ' then begin
				    ii=execute(zapa+',W'+strtrim(string(gol(1)),2)+',gol(0)') & zapa='' & endif

if n_elements(tol)   eq 1 then set_tolerance,tol
if n_elements(step)  eq 1 then pzip=step<10.>0.
if n_elements(dir)   eq 1 then pzup=dir
if n_elements(dif)   eq 1 then dzap=dif
if n_elements(prefx) eq 1 then pref=prefx
if n_elements(sufx)  eq 1 then suf =sufx
end

function rdsum, run1,run2 ,stati,datp=rdp ,W=wii ,COMPLEMENT=complement
;******* *****
;** read run1 sum to run2 from formula entry	!!! W21 is the temp workspace
@lamp.cbk
    w21=0   & n21=0 & e21=0 & x21=0 & stati=0 & status=-1
    if (n_elements(run1) eq 1) and (n_elements(run2) eq 1) then begin
    	if one le 0 then  one  =19
	if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then one=wii
	wi =one & ws =strtrim(string(wi),2)
	
	if  keyword_set(complement)  then XICUTE,'w21=w'+ws
	
	if abs(run2-run1) ge 10 then begin
		p_did_getrun, run1 ,wi,stati & if stati ne 0 then return,w21
		p_did_getrun, run2 ,wi,stati & if stati ne 0 then return,w21
	endif
	for i=long(run1),run2 do begin
		p_did_getrun, i ,wi,status
		if status eq 0 then begin
			tt=tolerance
			if monimon lt 0 then W_ACCU, accu=21 , add=wi ,tolerance=tt ,/raw $
					else W_ACCU, accu=21 , add=wi ,tolerance=tt
			toler=tt
		endif else stati=i
		if RDSTOP(run1,run2,(i)) then i=run2+1
	endfor
	iii=execute('x'+ws+'=x21')
	iii=execute('n'+ws+'=n21')
	iii=execute('e'+ws+'=e21')
        other_tit(one)=other_tit(one)+' '+strtrim(string(run1),2)+'>'+strtrim(string(run2),2)
	if status eq 0 then RDSET,golast=[wi,21]
   endif
if n_elements(rdp) gt 0 then begin tmp=two & two=one & take_datp,rdp & two=tmp & endif
return,w21
end

function rdrun, run ,stati,datp=rdp ,W=wii
;******* *****
;** read a run from formula entry
@lamp.cbk

   wtm=0L & run2=0
   if n_elements(stati) eq 1 then if stati gt run then begin
      run2=stati
      wtm =rdsum(run,run2,W=wii)
   endif

   if run2 eq 0 then begin
    stati=0
    if n_elements(run) eq 1 then begin
        if one le 0 then  one  =19
	if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then one=wii
	wi =one & ws =strtrim(string(wi),2)
	p_did_getrun, run,wi,status
	if status eq 0 then RDSET,golast=[wi,wi]
	if status eq 0 then iii=execute('wtm=w'+ws) else stati=run
    endif
   endif
if n_elements(rdp) gt 0 then begin tmp=two & two=one & take_datp,rdp & two=tmp & endif
return,wtm
end

function rdand, run1,run2 ,stati ,datp=rdp ,accu=accu ,flip=flip ,mon=monoto ,FRAME=rrun ,W=wii ,COMPLEMENT=complement
;******* *****
;** read run1 and to run2 from formula entry	!!! W21 is the temp workspace
@lamp.cbk
common keep_rd, sz,sn,tkx,tkz

;  Ken"s crack
   IF (inst_group EQ 'ISIS') THEN BEGIN
	ii=EXECUTE('w21=rdand_isis(run1,run2,flip=flip)')
	if ii eq 1 then RETURN, w21 ;if rdand_isis not here then continue...
   ENDIF
   w21=0 & x21=0 & n21=0 & e21=0 & stati=0
   if (n_elements(run1) eq 1) and (n_elements(run2) eq 1) then $
   if  run2 ge run1 then begin
        if one le 0 then  one  =19
	if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then one=wii
	wi =one & ws =strtrim(string(wi),2)
	status=0
	if n_elements(rrun)   eq 0 then rrun   =0
	if n_elements(monoto) ne 1 then monoto =monimon
	if not keyword_set(flip)   then flip   =1
	if     keyword_set(accu)   then begin J=0
			 XICUTE,'w21=w'+ws  & sz21=size(w21)
			 mon= fltarr(sz21(sz21(0)))+n21(0)
	endif else begin J=flip
			 if rrun(0) then begin ruun=rrun(0) & nimg=run1 & endif else ruun=run1
			 if n_elements(rrun) eq 2 then $
				nimg=strtrim(string(rrun(1)),2)+'.'+strtrim(string(nimg),2)
			 p_did_getrun, ruun ,21,status, FRAME=nimg
			 sz =(size(w21))(0)     & sn =(size(n21))(0)  & tkx=0
			 if sz eq 1 then if n_elements(x21) eq n_elements(w21) then tkx=1
			 pv21=p21 & z21=z21(*,0)
			 if n_elements (z21) eq 1 then tkz=1 else tkz=0
			 mon= total  (n21(*,0))
			 if   keyword_set(complement)  then begin J=0
			      MOVEPAR, (wi),ws, 21,'21' & ii=execute('W21=W'+ws)
			      sz21=size(w21)
			      if sz lt sz21(0) then mon=fltarr(sz21(sz21(0))) else mon=0.
			      mon=mon+total(n21(*,0))
			      endif
	endelse

	if status eq 0 then $
	if run2-run1 ge 10 then p_did_getrun, (run2) ,wi,status
	stati=status
		
	if status eq 0 then $
	for i=long(run1)+J,run2,flip do begin
		if rrun(0) then begin ruun=rrun(0) & nimg=i & endif else ruun=i
		if n_elements(rrun) eq 2 then $
				nimg=strtrim(string(rrun(1)),2)+'.'+strtrim(string(nimg),2)
		p_did_getrun, ruun ,wi,status, FRAME=nimg
		if status eq 0 then begin
			if sz  lt 1 then iii=execute('w21=[  w21  ,  w'+ws+'  ]')    else $
			if sz  eq 1 then iii=execute('w21=[ [w21] , [w'+ws+'] ]')    else $
			if sz  ge 2 then iii=execute('w21=[[[w21]],[[w'+ws+']]]')
			if sz  lt 1 then iii=execute('x21=[  x21  ,  x'+ws+'  ]')    else $
			if tkx eq 1 then iii=execute('x21=[ [x21] , [x'+ws+'] ]')
			if sz  eq 1 then iii=execute('y21=[  y21  ,  y'+ws+'  ]')
			if sz  eq 1 then       if n_elements(e21) gt 1 then $
			                 iii=execute('e21=[ [e21] , [e'+ws+'] ]')
			if sn  lt 2 then iii=execute('n21=[ [n21] , [n'+ws+'] ]')    else $
					 iii=execute('n21=[[[n21]],[[n'+ws+']]]')
			if tkz eq 1 then iii=execute('z21 =[ z21  ,  z'+ws+'(*,0)]') else $
					 iii=execute('z21=[ [z21] , [z'+ws+'(*,0)]]')
			iii=execute('mon =[  mon   , total(n'+ws+'(*,0)) ]')
			iii=execute('pv21=[ [pv21] , [p'+ws+'     ] ]')
		endif
		if RDSTOP(run1+J,run2,(i)) then i=run2+1
	endfor

	if monoto ge 0 then RDMONI,0, W21,E21,N21,mon>1,monoto

	iii=execute('n' +ws+'= n21')
	iii=execute('z' +ws+'= z21')
	iii=execute('e' +ws+'= e21')
	iii=execute('pv'+ws+'=pv21')
	if sz  lt  1 then if x21(0) ne x21(n_elements(x21)-1) then iii=execute('x'+ws+'=x21')
	if sz  eq  1 then if y21(0) ne y21(n_elements(y21)-1) then iii=execute('y'+ws+'=y21')
	if tkx eq  1 then begin sid=size (x21) & mid=sid(sid(0))
				zid=total(x21( *,mid-1) - x21(*,0))
				if zid ne 0 then begin	     	   iii=execute('x'+ws+'=x21')
							    	   y21=transpose([[y21],[y21]])
							    	   y21=congrid  (  y21 ,sid(1),mid)
								   iii=execute('y'+ws+'=y21') & endif
	endif
	if sz lt  1 then if x_tit(wi) eq '' then x_tit(wi)='Run number'
	if sz eq  1 then if y_tit(wi) eq '' then y_tit(wi)='Run number'
	if sz eq  2 then if z_tit(wi) eq '' then z_tit(wi)='Run number'
	if (n_elements(accu) eq 0) and (status eq 0) then   RDSET,golast=[wi,21]
   endif

if n_elements(rdp) gt 0 then begin tmp=two & two=one & take_datp,rdp & two=tmp & endif
return,w21
end

function rdopr, text,stati,datp=rdp,W=wii
;******* *****
;**
@lamp.cbk

if one le 0 then  ones =19 else ones=one
if n_elements(wii) eq 1 then if (wii ge 1) and (wii le 20) then ones=wii
txt=text
rdmulti, txt,stati,0,ones
wt =0L
if stati   eq 0 then ii=execute('wt=w'+strtrim(string(ones),2)  )

if n_elements(rdp) gt 0 then begin tmp=two & two=one & take_datp,rdp & two=tmp & endif
return,wt
end

pro rdmulti, txt,status,labid,wi,monoto
;** *******
;**
;	!!! W21 or W22 or W23 is the temp workspace
@lamp.cbk

run_comd,txt,tbl,tbf
status=12
if txt ne '' then begin
	if n_elements(wi)     eq 0 then wi=one
	if n_elements(labid)  eq 0 then labid=0
	if n_elements(monoto) eq 0 then monoto=monimon
	nn=size(tbl)
	if nn(0) lt 2 then nn=1 else nn=nn(2)

        ws=strtrim(string(wi),2)
	ii=execute( 'w'+ws+'=0' )
	status=0
	lo =0
    	if wi ne 21 then begin	w21=0   & n21=0 & e21=0 & x21=0 & was='21' & wac=21 & endif $
    		    else begin  w22=0   & n22=0 & e22=0 & x22=0 & was='22' & wac=22 & endelse
    	accu=',accu=ac,flip=op)' & ac=0
	mltr=(nn gt 1) or (tbl(0,0) gt 0)
	while  (status eq 0) and lo lt nn do begin
        	if (tbl(1,lo) ne 0) and (tbl(2,lo) ne 0) then begin
		    ru0 =tbl(0,lo)
        	    ru1 =tbl(1,lo)
		    rrun=tbl(3,lo) & if (rrun eq 0) and (tbf gt ' ') then rrun=tbf
		    
        	    if ru0 eq 0 then ru0=ru1 else if ru0 gt ru1 then ru1=ru0
        	    if tbl(2,lo) ge 1 then opr='+'  else  begin opr='-'
			if ru1 gt ru0 then begin		opr='-+'
				w23=0   & n23=0 & e23=0 & x23=0 & wat='23' & wau=23 & endif & endelse
		    run =ru0
		    if (ru1-ru0 ge 10) then begin
			p_did_getrun, (ru0) ,wi,status & if status ne 0 then return
			p_did_getrun, (ru1) ,wi,status & if status ne 0 then return
		    endif
        	    while  (status eq 0) and (run le ru1) do begin
	   	        op=abs(tbl(2,lo))
	   	        if op ge 2 then begin
	   	        	if labid gt 0 then $
	   	                widget_control,bad_id=i,labid,set_value='Reading '+string(ru0)+' to '+string(ru1)
	  			one=wi
				op =op-1
				nimg=rrun
				if tbl(4,lo) gt 0 then nimg=[string(rrun),string(tbl(4,lo))]

	   	        	ii =execute('w'+ws+'=RDAND(ru0,ru1,status ,mon=monoto,FRAME=nimg'+accu)
	   	        	run=ru1
				ac=1
	   	        endif else begin
	   	        	if labid gt 0 then $
	   	                widget_control,bad_id=i,labid,set_value='Reading '+string(run)+' ...'
				nimg=0
				if rrun then begin ruun=rrun & nimg=run & endif else ruun=run
						if tbl(4,lo) gt 0 then begin
						   nimg=strtrim(string(tbl(4,lo)),2)+'.'+strtrim(string(nimg),2)
						endif

	   	        	P_DID_GETRUN, ruun ,wi, status, FRAME=nimg

        	        	if  mltr then begin
				    tt=tolerance
				    if status ne 0 then status=0    else $
        	        	    if   opr eq '-'  then W_ACCU, accu=wac ,sub=wi  else begin
				     if monoto(0) lt 0 then $
				      if opr eq '-+' then W_ACCU, accu=wau ,add=wi ,tolerance=tt,/raw else $
        	        	      if opr eq '+'  then W_ACCU, accu=wac ,add=wi ,tolerance=tt,/raw
				     if monoto(0) ge 0 then $
				      if opr eq '-+' then W_ACCU, accu=wau ,add=wi ,tolerance=tt      else $
        	        	      if opr eq '+'  then W_ACCU, accu=wac ,add=wi ,tolerance=tt
				    endelse
				    toler=tt
        	        	endif
				if RDSTOP(ru0,ru1,(run)) then run=ru1+1
	   	        endelse
			if status eq 0 then run=run+1
        	    endwhile
		    if 	opr eq '-+'  then W_ACCU, accu=wac ,sub=wau
		    if  abs(tbl(2,lo)) lt 2 then  $
			if monoto(0) gt 0 then begin moni=monoto
;			   if monoto(0) eq 0 then moni=-(ru1-ru0+1) ;(IF /RAW !!!)
			   if mltr then ii=execute('RDMONI,1,W'+was+',E'+was+',N'+was+',N'+was+'(*,0)>1,moni') $
				   else ii=execute('RDMONI,1,W'+ws +',E'+ws +',N'+ws +',N'+ws +'(*,0)>1,moni')
			endif
        	endif
        	lo=lo+1
	endwhile
	if status ne 0  then begin ii=execute( 'w'+ws+'=0' )
			outext='% Restore '+string(run)+' failed ...'
	endif else begin
        	        if (nn gt 1) or ((tbl(0,0) gt 0) and (abs(tbl(2,0)) eq 1)) then $
			if (abs(tbl(2,0)) ne 2) then begin
        	           ii=execute( 'W'+ws+'=w'+was )
        	           ii=execute( 'X'+ws+'=x'+was )
        	           ii=execute( 'N'+ws+'=n'+was )
        	           ii=execute( 'E'+ws+'=e'+was )
			   if monimon   ge 0 then $
			   ii=execute( 'E'+ws+'=e'+was ) else ii=execute( 'E'+ws+'=0')
        	        endif
        	        other_tit(wi)=other_tit(wi)+' '+txt
			outext='Data restored in W'+ws
			RDSET,golast=[wi,wi]
	endelse
	if labid gt 0 then if wi le 20 then widget_control,bad_id=i,labid,set_value=outext
endif
return
end

pro RDMONI,HZ, W21,E21,N21,moni,mol
;** ******
;** HZ=0 comes  from RDAND   (one count by spectra or frame)
;** HZ=1 comes  from RDMULTI (X monitor)
;** moni is the current monitor while mol is the new monitor.
	mon=moni
	if mol eq 0 then mol=round(total(mon)/n_elements(mon))*1.
	if mol lt 0 then mol=round(total(mon)/n_elements(mon))*1./(-mol)
	if mon(0) ne mol(0) then begin
	   if HZ then n21(*,0)=mol else n21=mol
	   mon=mol/mon & sz21=size(w21)
	   if  n_elements(w21) eq n_elements(E21)  then ero=1 else ero=0
	   if (n_elements(mon) ne sz21(1)) and HZ  then mon=mon(0)

	   if  sz21(0) le 1 then  begin	if ero then E21=E21*mon  else $
					if HZ  then E21=sqrt(w21)*mon & w21=w21*mon & endif

	   if  sz21(0) eq 2 then  begin	for i=0,sz21(2)-1 do if HZ then w21(0,i)  =(w21(*,i)  )*mon $
								   else w21(0,i)  =(w21(*,i)  )*mon(i)
			    if ero then for i=0,sz21(2)-1 do if HZ then E21(0,i)  =(E21(*,i)  )*mon $
								   else E21(0,i)  =(E21(*,i)  )*mon(i) & endif
	   if  sz21(0) eq 3 then  begin	for i=0,sz21(3)-1 do if HZ then w21(0,0,i)=(w21(*,*,i))*mon $
								   else w21(0,0,i)=(w21(*,*,i))*mon(i)
			    if ero then for i=0,sz21(3)-1 do if HZ then E21(0,0,i)=(E21(*,*,i))*mon $
								   else E21(0,0,i)=(E21(*,*,i))*mon(i) & endif
	endif
end

pro run_comd, text,tbl,tbf
;** ********
;**
;** Command analysis

text=text+"&"
ttt =[0L,0L,0L,0L,0L]
tbf =''
tbl =ttt & tbl(2)=1 & j =1 & run=''
on_ioerror,mis     & ok=0 & fil=0
for i=0,strlen(text)-1 do begin

    c =  strmid(text,i,1)
    case 1 of
    (c eq ' ') or (c eq '(') or (c eq ')'):
    (c eq '{') or (c eq '['):begin
			     if run ne ''  then  begin fil=0 & on_ioerror,misfil
			     	a=strmid(run,0,1)
				if (a ge '0') and (a le '9') then fil=long(run)
			     	misfil: if fil eq 0 then tbf=run
			        on_ioerror,mis & endif
			     run=''  & end 
    (c eq '}') or (c eq ']') or (c eq '&'):begin
    			     if run ne ''  then tbl(1  ,j-1)=long(run)
    			     if run ne ''  then tbl(3  ,j-1)=fil
    			     run='' & fil=0 & end
    (c eq '-') or (c eq '+') or (c eq ',') or (c eq ';'):begin
    			     if run ne ''  then tbl(1  ,j-1)=long(run)
    			     if run ne ''  then tbl(3  ,j-1)=fil
    			     run=''
    			     tbl=[[tbl],[ttt]]
    			     if (c eq '-') then tbl(2  ,j)=-1   else tbl(2,j)=1
    			     j  =j+1 & end
    (c eq '<') or (c eq '>') or (c eq ':'):begin
    			     if run ne ''  then tbl(0  ,j-1)=long(run)
    			     if run ne ''  then tbl(3  ,j-1)=fil
    			     if  c  eq ':' then tbl(2  ,j-1)=abs(tbl(2,j-1))+1
    			     run=''  & end
    (c eq '.'):              begin
			     if (fil eq 0) and (tbf eq '') then run=run+c $
			     else begin
    			     if run ne ''  then tbl(4  ,j-1)=long(run) & run=''
			     endelse
			     end
    else:		     run=run + c
    endcase
    endfor
nn=size(tbl)
if nn(0)    lt 2 then nn=1  else nn=nn(2)
if tbl(1,0) eq 0 then if nn gt 1 then begin tbl=tbl(*,1:*) & nn=nn-1 & endif
ok=1
mis:text=''
bo=0
if  ok eq 1 then $
for i=0,nn-1  do $
    if (tbl(1,i) ne 0) and (tbl(2,i) ne 0)  then begin
	if tbl(3,i) ne fil then  if bo eq 1 then text=text+'}'
    	if tbl(2,i) eq  1  then  if i  gt 0 then text=text+'+'
	if tbl(2,i) ge  2  then  if i  gt 0 then text=text+','
    	if tbl(2,i) le -1  then                  text=text+'-'
	if tbl(3,i) ne fil then  begin fil= tbl(3,i) & bo=0
	     if fil gt  0  then  begin bo = 1  & text=text+strtrim(string(fil),2)+'{' & endif
	endif
    	if tbl(0,i) ne  0  then  $
    	if tbl(4,i) eq  0  then text=text+strtrim(string(tbl(0,i)),2) $
    	else text=text+strtrim(string(tbl(4,i)),2)+'.'+strtrim(string(tbl(0,i)),2)
    	
	op=abs(tbl(2,i))
    	if tbl(0,i) ne  0  then  if op eq 1 then text=text+'>' $
					    else text=text+string(replicate(58b,op-1))
					    
    	if tbl(4,i) eq  0  then text=text+strtrim(string(tbl(1,i)),2) $
    	else text=text+strtrim(string(tbl(4,i)),2)+'.'+strtrim(string(tbl(1,i)),2)
    endif
if  bo eq 1  then text=text+'}'
if (tbf gt '') and (strpos(text,'{') lt 0) then text=tbf+'{'+text+'}'
return
end

pro set_tolerance,tt ,get=get ,tol=tol
;** *************
;**
@lamp.cbk
    if keyword_set(get) then   tt=tolerance else $
    if keyword_set(tol) then   toler=tol    else $
    if n_elements (tt)  eq 1 then tolerance=tt
end

pro W_ACCU, accu=wi , add=wj , sub=wk , combine=wl , tolerance=tt , ero=ero ,raw=raw
;** ******
;**
;** Operation on accumulator wi (add or subtract or combine)

@lamp.cbk

;CHECK FOR CONSISTENCIES
;***** *** *************
if n_elements(wi) eq 1 then if (wi ge 1) and (wi le 23) then begin

   wis= strtrim(string(wi),2)	& wiw='w'+wis & wix='x'+wis & win='n'+wis & wie='e'+wis

   wjn= n_elements(wj)		& wkn=n_elements(wk)	    & wln=n_elements(wl)

   if wjn+wkn+wln eq 1 then begin

      if wkn eq 1 then begin opr='-' & wj=wk & endif else $
      if wln eq 1 then begin opr='c' & wj=wl & endif else opr='+'

      if (wj ge 1) and (wj le 23) then begin

   	wjs= strtrim(string(wj),2)	& wjw='w'+wjs & wjx='x'+wjs & wjn='n'+wjs & wje='e'+wjs

   	swi=0L & ii=execute( 'swi=size('+wiw+')' ) & swj=0L & ii=execute( 'swj=size('+wjw+')' )
   	sxi=0L & ii=execute( 'sxi=size('+wix+')' ) & sxj=0L & ii=execute( 'sxj=size('+wjx+')' )
   	sni=0L & ii=execute( 'sni=size('+win+')' ) & snj=0L & ii=execute( 'snj=size('+wjn+')' )
   	sei=0L & ii=execute( 'sei=size('+wie+')' ) & sej=0L & ii=execute( 'sej=size('+wje+')' )

	if swi(swi(0)+2)  gt 1 then begin

;	DEFINE PARAMETERS IF NECESSARY (X,N,E).
;	****** ********** ** ********* *******
;I-x						ii=execute( wix+'=float('+wix+')')
	if swi(0) 	 gt 0 		  then  $
	   if  swi(1)    ne sxi(1)	  then  ii=execute( wix+'=findgen(swi(1))+1' )
;I-e
	ero=1
   	if sei(sei(0)+2) ne swi(swi(0)+2) then  if swi(0) eq 1 then $
   						ii=execute( wie+'=sqrt('+wiw+')' ) else ero=0
;I-n
	if (sni(0) gt 1) and (sni(1) ne swi(1)) then iro=0 else iro=1
	if iro eq 1 then begin
  	if sni(1)	 ne   swi(1)	  then  ii=execute( win+'=fltarr(swi(1))+'+win+'(0)>1' ) else $
        if sni(0)	 gt 1		  then  $
	   if  swi(0)    eq 1		  then  ii=execute( win+'=fltarr(swi(1))+total('+win+'(*,0))>1' )
	endif
;J-x
	if swj(0) 	 gt 0 		  then  $
	   if  swj(1)    ne sxj(1)	  then  ii=execute( wjx+'=findgen(swj(1))+1' )
;J-e
	if ero eq 1 then begin
	   if swj(0) eq 1 then wjee=1  &  ii=execute('wjee='+wje)
   	   if sej(sej(0)+2) ne swj(swj(0)+2) then  if swj(0) eq 1 then $
   						   ii=execute( 'wjee=sqrt('+wjw+')' ) else ero=0
        endif
;J-n
	wjnn=1    &   ii=execute('wjnn=' +wjn)
	if iro eq 1 then begin
   	if snj(snj(0)+2) ne swj(swj(0)+2) then  $
   	   if  snj(1)    ne     swj(1)    then  ii=execute( 'wjnn=fltarr(swj(1))+'+wjn+'(0)>1' ) else $
           if  snj(0)	 gt 1		  then  $
	     if  swj(0)  eq 1		  then  ii=execute( 'wjnn=fltarr(swj(1))+total('+wjn+'(*,0))>1' )
	endif

;	DEFINE A TOLERANCE IF NOT
;	****** * ********* ** ***
   	if n_elements(tt) ne 1 then tt=0
	if tt le 0 then begin
		k1=1L & ii=execute( 'k1=n_elements('+wix+')-1' )
		k2=1L & ii=execute( 'k2=n_elements('+wjx+')-1' )
		ii=execute( 'k1=float(abs('+wix+'(k1-1)-'+wix+'(0)))/k1' )
   		ii=execute( 'k2=float(abs('+wjx+'(k2-1)-'+wjx+'(0)))/k2' )
		tt=min([k1,k2])/3
	endif

;	   MATRIX OPERATION
;	   ****** *********
	   if (swj(0) gt 1) or (not iro)  then begin
	   		if opr eq 'c' then opr='+'
			if keyword_set(raw)  then rw=1 else rw=0
	   		if opr eq '+' then begin
			  tst=1 & iii=execute('tst=('+wix+'(0) eq '+wjx+'(0)) and ('+wix+'(swi(1)-1) eq '+wjx+'(swj(1)-1))')
			  if (sxj(0) ne 1) or (tst) or (swj(2) ne swi(2)) or (not iro) then begin
			   iii=execute( wiw+'=float(' +wjw +')' +opr+wiw )
			   iii=execute( win+'=float(   wjnn  )' +opr+win )
			   iii=execute( wie+'=sqrt(float(' +wje+')^2'+opr+wie+'^2)' )

			   if (iro) and (tst) and (not rw) then begin m1=1.  & iii=execute('m1=total('+win+'(*,0))/swj(1)/2')
			        if m1 le 1 then iii=execute( win+'=0') $
				else      begin iii=execute( wiw+'='+wiw+'/2') & iii=execute( win+'(*,0)='+win+'(*,0)/2')
						iii=execute( wie+'='+wie+'/2') & endelse
			   endif

			  endif else begin
			   if ero eq 1 then myerr=wje+'(*,i)' else begin tmerr=fltarr(swj(1))+1 & myerr='tmerr'
									 tie  =fltarr(swi(1))+1 & endelse
			   tmw=0 & tmx=0 & tmn=0 & rse=0
			   for  i=0,swj(2)-1 do begin
				iii=execute('tmw=' +wiw+'(*,i)')  &  iii=execute('tmx=' +wix)
				iii=execute('tmn=' +win)
				if ero eq 1 then iii=execute('tme=' +wie+'(*,i)') else tme=tie

	   			iii=execute( 'V_ACCU,opr,   tmw       ,  tmx  ,  tmn       ,  tme    ,'   $
	   						   +wjw+'(*,i),'+wjx+',  wjnn(*,0) ,'+myerr+', tt ,raw=raw')
				if i eq 0 then rsw=tmw else rsw=[[rsw],[tmw]]
				if ero eq 1 then $
				if i eq 0 then rse=tme else rse=[[rse],[tme]]
			   endfor
			   iii=execute(wiw+'=rsw') & iii=execute(wix+'=tmx')
			   iii=execute(win+'=tmn') & iii=execute(wie+'=rse')
			  endelse

			endif
	   		if opr eq '-' then begin na=1. & ni =1.
	   		   iii=execute('na   =total(' +win +'(*,0))>1')
					ni   =total(     wjnn(*,0))>1
	   		   m  =na/ni
			   iii=execute( wiw+'='+wiw+opr+wjw+'*m' )
			   if ero eq 1 then iii=execute(wie+'=sqrt('+wie+'^2+'+wje+'^2*m)')
			endif

;	   VECTOR OPERATION
;	   ****** *********
	   endif else begin
	   		iii=execute( 'V_ACCU,opr,' +wiw+','+wix+','+win+'      ,'+wie+','   $
	   					   +wjw+','+wjx+',  wjnn(*,0)  ,  wjee , tt ,raw=raw')
	   endelse

	endif else begin oon=one & too=two & tee=three & alo=alone & ifi=ifixed & don_me_lastf, lfoo ,0
			 ifixed=0
			 XICUTE,wiw+'=float('+wjw+')'
			 one=oon & two=too & three=tee & alone=alo & ifixed=ifi & don_me_lastf, lfoo ,1
	endelse
      endif
   endif else	   begin XICUTE,wiw+'=0'		& one=0		& endelse
endif
return
end

pro V_ACCU, ops ,wa,xa,na,ea, wi,xi,ni,ei , tt ,raw=raw
;** ******
;**
    if  ops eq 'c' then opr='-' else opr=ops

;   ADDITION : Accumulator  --> wa, xa, na, ea = counts, abcissa, monitors, errors
;   -------- : New spectrum --> wi, xi, ni, ei   tolerance is tt.

    if  opr eq '+' then begin
;	JOIN ACCU AND SPECTRUM
;	---- ---- --- --------
	if keyword_set(raw)  then rw=1 else rw=0
	xa =[xa,xi]	   & sa=n_elements(wa) & si=n_elements(wi)
	idx=sort(xa)	   & xa=xa(idx)
	wa =[wa,wi]	   & wa=wa(idx)  & wa=float(temporary(wa))
	na =[na(*,0),ni]>1	   & na=na(idx)  & na=float(temporary(na))
	ea =[ea,ei]	   & ea=ea(idx)  & nn=n_elements(xa)
	if not rw then begin wa=wa/na	 & m1=total (na)/nn & ea=ea/na*m1 & na(*)=1. & endif
	k  = 0

	for  i=1,nn-1 do begin

	     if (xa(i)-xa(k))  le tt  then begin
;	     MERGE TWO POINTS
;	     ----- --- ------
		 wp =wa(i) & ep=ea(i)

;		 INTERPOLATE IF POSSIBLE.
;		 ----------- -- --------
		 if (xa(i) ne xa(k)) and (not rw) then begin ip=idx(i)-sa-1
ip=-1 ;No op
		  if ip    ge  0     then begin   if (xa(k) gt xi(ip)) then begin	   ;i is  new
						  fac  =(xa(i)-xa(k))  /(xa(i)-xi(ip))
						  fac  = fac<.5 & fac1=1.-fac
						  wp   =(fac1)*wa(i)+ fac*wi(ip)/(ni(ip)>1)
						  ep   =(fac1)*ea(i)+ fac*ei(ip)/(ni(ip)>1)*m1
					  	  endif
		  endif else begin ip=idx(k)-sa
ip=-1 ;No op
		   if (ip ge 0) and (ip lt si-1)  then begin ip=ip+1			   ;k was new
						  if (xi(ip) gt xa(i)) then begin
						  fac  =(xa(i)-xa(k))  /(xi(ip)-xa(k))
						  fac  = fac<.5 & fac1=1.-fac
						  wa(k)=(fac1)*wa(k)+ fac*wi(ip)/(ni(ip)>1)
						  ea(k)=(fac1)*ea(k)+ fac*ei(ip)/(ni(ip)>1)*m1
						  xa(k)= xa(i)
						  endif
		 endif & endelse & endif

;!!!!!!		 Not physique or physique ????
		 phy=1       ;or phy=ea(k)*ep

		 idx(k)=0
		 if rw  then begin wa(k)= wa(k)+wp & na(k)=na(k)+na(i) & ea(k)=sqrt(ea(k)^2+ep^2)   ; raw addition !!

		 endif else if phy eq 0 then begin
	     	    na(k)= na(k)+1
	     	    wa(k)= wa(k)+wp			; w = (wa+wi)/2
	     	    ea(k)= sqrt(ea(k)^2 + ep^2)		; e = sqrt(ea^2 + ei^2)/2
		 endif else begin
		    eak  =1./ea(k)^2 &  epk=1./ep^2
		    wa(k)=(wa(k)*eak+wp*epk)/(eak+epk)	; w = ( wa/ea^2 + wi/ei^2 ) / ( 1/ea^2 + 1/ei^2)
		    ea(k)=1./sqrt(eak+epk)		; e = 1/sqrt(1/ea^2 + 1/ei^2)
		 endelse
	     endif else begin
;	     OR MAKE A NEW POINT.
;	     -- ---- - --- -----
	      	 k= k+1
	      	 if k ne i then begin
	            xa(k)= xa(i) & idx(k)=idx(i)
	            wa(k)= wa(i) & ea (k)= ea(i)
		    if rw  then    na (k)= na(i)
	       	 endif
	     endelse
	endfor
	if k lt nn-1 then begin
	   xa= xa(0:k)  & wa=wa(0:k)
	   na= na(0:k)  & ea=ea(0:k)
	   if not rw then begin wa= wa/na & ea= ea/na & endif
	endif
;	NORMALIZE
;	---------
	   if not rw then begin wa= wa*m1 & na(*)= m1 & endif
    endif

;   SUBTRACTION
;   -----------
    if  opr eq '-' then begin
;	SORT
;	----
	idx=sort(xa)  & xa=xa(idx) & wa=wa(idx) & na=na(idx) & ea=ea(idx)
	idx=sort(xi)  & xi=xi(idx) & wi=wi(idx) & ni=ni(idx) & ei=ei(idx)

;	INTERPOLATE MISSING ABSCISSA
;	----------- ------- --------
	if ops ne 'c' then begin

	 m1 =total(na)/ n_elements(wa)
	 wa =float(temporary(wa))/(na>1)*m1
	 wp =float(wi)/(ni>1)*m1   & si=n_elements(wp)

	 yy =INTERPOL(wp, xi,xa)   & ea=ea/(na>1)* m1
	 yer=INTERPOL(ei/(ni>1)*m1 , xi,xa)

	 id1=where(xa lt xi(0))			& i1=n_elements(id1)
	 id2=where(xa gt xi(n_elements(xi)-1))	& i2=n_elements(id2)

	 nn =10<(si/3) & nn=nn>1
	 if i1 gt 1 then begin	ab =LADFIT(xi(0:nn),wp(0:nn))
				yy (id1)=ab(0)+ ab(1)*xa(id1)
				yer(id1)=max(abs(yy(id1)))/2    & endif
	 if i2 gt 1 then begin	ab =LADFIT(xi(si-nn-1:*),wp(si-nn-1:*))
				yy (id2)=ab(0)+ ab(1)*xa(id2)
				yer(id2)=max(abs(yy(id2)))/2    & endif

;	 NORMALIZE
;	 ---------
	 wa   = (wa-yy)
	 na(*)=  m1
	 ea   =  sqrt((ea^2 + yer^2))

;   COMBINE
;   -------
	endif else begin
	 id1=where(xa ge xi(0))			& id1=id1(0)>0<(n_elements(xa)-1)
	 id2=where(xa ge xi(n_elements(xi)-1))  & id2=id2(n_elements(id2)-1)>0<(n_elements(xa)-1)
	 m  =float(na(id1:id2))/(ni>1) & m1=m>1
	 yy =INTERPOL(wi*m1,xi,xa(id1:id2))
	 wa(id1:id2)=(wa(id1:id2)-yy)>0
	endelse
    endif
return
end

;************************************ JOURNAL *************************************
;************************************ JOURNAL *************************************
;************************************ JOURNAL *************************************

pro p_did_journal, event,uv
;** *************
;**
@lamp.cbk
    i=xregistered('JOURNAL')
    if i gt 0 then widget_control,bad_id=i,uv(2),/destroy

    base=widget_base  (title='Lamp Journal',/column,resource_name='lamptouch')
    tit =widget_label (base,value='JOURNAL OF CURRENT SESSION (lamp.jou)',font=ft_biggest)
    basc=widget_text  (base,value=jou_c+'            '+jou_w,font=ft_b_normal,xsize=80,ysize=20,/scroll)
    brow=widget_base  (base,/row,SPACE=30)
    prin=widget_button(brow,value='PRINT' ,uvalue=[-88,395,base,1,basc])
    cler=widget_button(brow,value='CLEAR' ,uvalue=[-88,395,base,2,basc])
    updt=widget_button(brow,value='UPDATE',uvalue=[-88,395,base,3,basc])
    done=widget_button(brow,value='CLOSE' ,uvalue=[-88,399])
	 put_logo     ,brow

    uv(2)=base
    widget_control,event.id   ,bad_id=i,set_uvalue=uv
    widget_control,lamp_don(0),bad_id=i,set_uvalue=basc

    bid=sys_dep      ('DYNLAB',base,0)
    widget_control,base,group_leader=lamp_b1,/realize & put_logo
    widget_control,basc,SET_TEXT_TOP_LINE=(n_elements(jou_c)-18)>0
    XMANAGER, 'JOURNAL' ,base,event_handler='LAMP_EVENT_PARSER',/just_reg
return
end

pro p_did_journal_print, uv
;** *******************
;**
@lamp.cbk

if uv(3) eq 1 then begin
    text=[''] & widget_control,bad_id=ii,uv(4),get_value=text
    ON_IOERROR,misopn
    OPENW ,out,'journal.print',/GET_LUN
    	ON_IOERROR,miswrt
    	for i=0,n_elements(text)-1 do PRINTF,out,text(i)
    	miswrt:FREE_LUN,out
	cd,current=dir
	if strmid(dir,strlen(dir)-1,1) ne lamp_dvd then dir=dir+lamp_dvd
	bid=sys_dep      ('PRT_DEF',dir+'journal.print')
    misopn:
endif
if uv(3) eq 2 then begin
    DID_WRITE_JOURNAL, /clear
    widget_control,bad_id=ii,uv(2),/destroy
endif
if uv(3) eq 3 then begin
    DID_WRITE_JOURNAL
endif
return
end

pro DID_WRITE_JOURNAL, jou ,htm=htm ,clear=clear ,check=check
;** *****************
;**
@lamp.cbk
common c_kpjou, kpjou
	out=-1
	if n_elements(jou) gt 0 then begin
		if n_elements(kpjou) eq 0 then kpjou=[2,0]
		jj=n_elements(jou_c)
		kk=(jj-kpjou(1))<20 & if kk lt 0 then kk=jj
		if kk gt 0 then jou=jou_c(jj-kk-1:jj-1)+'            '+jou_w(jj-kk-1:jj-1)
		kpjou(1)=jj
	endif else if keyword_set(htm) then begin wpth=''
	        if (GEORGE ne 0) then WebDo,'pth',wpth
		if wpth eq '' then wpth='journal.htm' else wpth=wpth+'geo_d_12htm.web'
		on_ioerror, end_wr
		OPENW,out,wpth,/get_lun
		PRINTF,out,'<html><pre>'
		for i= 0,n_elements(jou_c)-1 do PRINTF,out,jou_c(i)+'            '+jou_w(i)
		PRINTF,out,'</pre></html>'
	endif else begin
		j= n_elements(jou_c)-1 & k=j & ok=0
		if n_elements(kpjou) eq 0 then kpjou=[2,0]

		if (keyword_set(check)) and (j-kpjou(0) le 35) then return
		
		while (k gt 0) and (not ok) do begin
	   	if strpos(jou_c(k),'SESSION') eq 0 then ok=1 else k=k-1
		endwhile
		
		on_ioerror, end_wr
		OPENW,out,'lamp.jou',/get_lun,/APPEND
		for i= k-1,j do PRINTF,out,jou_c(i)+'            '+jou_w(i)
		
		if (j gt 300) or (keyword_set(clear)) then begin & kpjou(0)=2
        	                 jou_c=[      '*******','SESSION','*******'] & jou_w=[      ' ',!stime,' ']
        	endif else begin jou_c=[jou_c,'*******','SESSION','*******'] & jou_w=[jou_w,' ',!stime,' ']
        	                 kpjou(0)=j+3 & endelse
	endelse
	end_wr: if out gt 0  then FREE_LUN,out
return
end

pro DID_PARAM_HTM , nwk
;** *************
;**
@lamp.cbk
	on_ioerror, end_wr & out=-1  & wpth=''
	        if (GEORGE ne 0) then WebDo,'pth',wpth
		if wpth eq '' then wpth='param.htm' else wpth=wpth+'geo_d_12htm.web'
		OPENW,out,wpth,/get_lun
		pp =['0']
		bb =execute('pp=string(p'+nwk+')')
		PRINTF,out,'<html><b>Parameters from W'+nwk+'  '+w_numor(fix(nwk))+'</b></br><pre>'
		for i=0,n_elements(pp)-1 do PRINTF,out,par_txt(fix(nwk),i) + pp(i)
		PRINTF,out,'</pre></html>'
	end_wr: if out gt 0  then FREE_LUN,out
return
end

pro p_did_help, flg, formu,formt
;** **********
;**
common dialshare2
	if flg eq 586 then begin
	   	formu=[      'Path is your PATH WORKING DIRECTORY you can change at any time.']
	   	formt=[      '']
	   	formu=[formu,''] & formt=[formt,'']
	   	formu=[formu,'Click the "Data..." button to access your instrument data on site']
	   	formt=[formt,'']
	   	formu=[formu,'        ']
	   	formt=[formt,'you may have to customize the two ins: pull-down menus.']
	   	formu=[formu,'The READ button allows access  to data from several types.']
	   	formt=[formt,'']
	   	formu=[formu,'You have to adjust the Ins format type and its location (second Ins menu)']
	   	formt=[formt,'']
	   	formu=[formu,'then enter the Run number (or file_name{i.j}) and adjust W_space (W1).']
	   	formt=[formt,'']
	   	formu=[formu,''] & formt=[formt,'']
	   	formu=[formu,'The CUSTOMIZE entry in the Ins: menu is what you need to start with Lamp!']
	   	formt=[formt,'']
	   	formu=[formu,'If you click the "Self..." button then you can bring up']
	   	formt=[formt,'']
	   	formu=[formu,'YOUR special interface window for accessing your data.(See CUSTOMIZE)']
	   	formt=[formt,'']
	   	formu=[formu,'This is a programmed button, as an example you may']
	   	formt=[formt,'']
	   	formu=[formu,'associate the RDFILTER procedure which provides:']
	   	formt=[formt,'']
	   	formu=[formu,'        ']
	   	formt=[formt,'- X Y and Z scaling.']
	   	formu=[formu,'        ']
	   	formt=[formt,'- X Y and Z projections, consistencies']
	   	formu=[formu,'        ']
	   	formt=[formt,'- Do simple operations on several Runs as they are read in.']
	   	formu=[formu,'The IMPORT File entry  allows you to get']
	   	formt=[formt,'']
	   	formu=[formu,'files having format such as:']
	   	formt=[formt,'']
	   	formu=[formu,'        ']
	   	formt=[formt,'- LAMP format  - XY ascii files  - NeXus hdf files      .../...']
	   	formu=[formu,'The EXPORT button is the best way to save your WORKSPACES and']
	   	formt=[formt,'']
	   	formu=[formu,'their associated parameters,history,coordinates,titles.']
	   	formt=[formt,'']
	   	formu=[formu,'The LAMP format is quit clear: A header ascii file is produced']
	   	formt=[formt,'']
	   	formu=[formu,'containing the parameters, a litte snapshot file(192*192 bytes),']
	   	formt=[formt,'']
	   	formu=[formu,'and a data file:']
	   	formt=[formt,'BINARY for c & lamp , F77 for fortran , ASCII for suspicious , XDR']
      		formu=[formu,''] & formt=[formt,'']
		formu=[formu,'Choose NeXus standard hdf format for data interchange.']
	   	formt=[formt,'']

	endif else if flg eq 587 then begin
	   	formu=[formu,'To visualize a workspace you must click on the PLOT W n button.']
	   	formt=[formt,'']
	   	formu=[formu,'']
	   	formt=[formt,'You may have to adjust its number by pressing the neighbouring arrows.']
	   	formu=[formu,'']
	   	formt=[formt,'Plot result depends of a combination of Image,Contour,Suface radio buttons.']

	   	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'Data are plotted in the small drawing-window unless you press the BESIDE button.']
	   	formt=[formt,'']
	   	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'A beside-drawing-window can be re_sized using the mouse, then']
	   	formt=[formt,'']
	   	formu=[formu,'when the new size is suitable press the REPLOT button']
	   	formt=[formt,'You may also use the X-Ysize fields.']
	   	formu=[formu,'']
	   	formt=[formt,'Each time you press the PLOT W n button you get a new window.']

	   	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'The PRINT buttons produce a PostScript file.']
	   	formt=[formt,'If a PostScript Device is']
	   	formu=[formu,'']
	   	formt=[formt,'specified in the Titles... interface then print-out is automatic.']

	   	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'The Options... button allows you to change:']
	   	formt=[formt,'Titles, surface aspects ...']

	   	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'The Z LOG button associated with the IMAGE button is useful to check']
	   	formt=[formt,'']
	   	formu=[formu,'for backgrounds and detector problems.']
	   	formt=[formt,'']
	   	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'The axis are labelled according to the values of the Workspaces"s']
	   	formt=[formt,'']
	   	formu=[formu,'coordinates. The REGULAR GRID option may imply an interpolation.']
	   	formt=[formt,'']
	   	formu=[formu,''] & formt=[formt,'----']

	endif else if flg eq 588 then begin
		if GEORGE eq 1 then begin
	   	formu=[formu,'LIST OF FUNCTIONS USED TO MANAGE THE DIALS:']
	   			formt=[formt,' ']
	   	formu=[formu,'DialTag   ,    "temp2",TAG="VALUE",GET=V']
	   			formt=[formt,'    Return V, the value of the tag  "VALUE" of dial "temp2"']
	   	formu=[formu,'DialTag   ,    "temp3",TAG="ONOFF",SET=1']
	   			formt=[formt,'    Set to 1  the value of the tag  "ONOFF" of dial "temp3"']
	   	formu=[formu,'DialStart ,    "temp3"']
	   			formt=[formt,'    A short  for previous call']
	   	formu=[formu,'DialStop  ,    "temp3"']
	   			formt=[formt,'    A short  too']
	   	formu=[formu,'D1.upperlim=   150.']
	   			formt=[formt,'    modify a property of DIAL 1 (Set upper limit for plotting)']
	   	formu=[formu,'DialInit,      "template4",[NEW="tmp4"]']
	   			formt=[formt,'    Initiate dial  "template4" from file:dial_template4.pro']
	   			formu=[formu,'                  ']
	   			formt=[formt,'    (You may change its name to "tmp4" and use DialStart,"tmp4" to activate it)']
	   	formu=[formu,'DialMacro,     "template4"']
	   			formt=[formt,'    Force execution of DIAL_TEMPLATE4_MACRO']
	   			formu=[formu,'                  ']
	   			formt=[formt,'    ("template4"  is keept inactive, ONOFF=0)']
	   	formu=[formu,'DialClear,     "template4"']
	   			formt=[formt,'    Suppress dial  "template4" from memory']
	   	formu=[formu,'WebOn  ,       [PATH="pth"],[PASSWD="pwd"]']
	   			formt=[formt,'    Output to the web (allow input if passwd is set)']
	   	formu=[formu,'WebOff            ']
	   			formt=[formt,'    Output to the web (allow input if passwd is set)']
	   	formu=[formu,'DialsFrequency,[GET=freq],[SET=.5],[/STOP],[DURATION=90.],[/START]']
	   			formt=[formt,' ']
	   			formu=[formu,'                  ']
	   			formt=[formt,'    Set  or Get the general frequency value (time is in seconds)']
	   			formu=[formu,'                  ']
	   			formt=[formt,'    Stop or Start the general process, Set Time limit for the active process']
	   	formu=[formu,' ']
		formt=[formt,' ']
	   	formu=[formu,'FUNCTIONS USED EXCLUSIVELY INSIDE A DIAL-MACRO']
	   			formt=[formt,' ']
	   	formu=[formu,'R=DialOn ()']
	   			formt=[formt,'                 Return 0 if Dial has been interrupted (To be used inside loops)']
	   	formu=[formu,'DialWSet']
	   			formt=[formt,'                        Reserve central draw window for next plot']
	   	formu=[formu,'V=DialNewValue([/SETVALUE, COMMENT=txt])']
	   			formt=[formt,'    Get a new value from DIAL_"generic"_READ']
	   			formu=[formu,'                  ']
				formt=[formt,'    (a request is made to the instrument)(/SETVALUE means D.value is set to V)']
	   	formu=[formu,'C=DialControl ("command syntax",[CHECK=.5])']
	   			formt=[formt,'    Send a command to the instrument control']
	   			formu=[formu,'                  ']
				formt=[formt,'    (CHECK means check every .5 sec till the command is complete)']
	   	formu=[formu,'DialModValue,   V']
	   			formt=[formt,'     Set the new value for current dial if type or dimensions have changed']

		endif else begin

	   	formu=[formu,'RAW MANIPULATIONS']
	   			formt=[formt,'Set this mode to prevent Lamp from adjusting results as a']
	   			formu=[formu,'']
				formt=[formt,'                  function of monitors & operators.(see setmanip in INTERNAL)']
	   	formu=[formu,'INTERNAL MACROS']
	   			formt=[formt,'Access to the list by the "UserMacros" button.']
	   			formu=[formu,'IDL LANGUAGE:'] & formt=[formt,'']
	   	formu=[formu,'W4 = W1(0:35 , 5:40)']
	   	         	formt=[formt,'EXTRACT a sub-array']
	   	formu=[formu,'W4 = W1( *   ,  8)']
	   	           	formt=[formt,'EXTRACT all points at y = 8']
	   			formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'w4 = FLTARR(256,128)']
	   	           	formt=[formt,'CREATE an empty floating matrix']
	   	formu=[formu,'OPENR,L,"DON.DAT",/GET_LUN']
	   	           	formt=[formt,'OPEN the file containing the matrix']
	   	formu=[formu,'READF ,L, W4']
	   	           	formt=[formt,'READ the matrix from ASCII format or']
	   	formu=[formu,'READU ,L, W4']
	   	           	formt=[formt,'READ the matrix from BINARY format']
	   	           	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'W4 = TOTAL ( W1 )']
	   	           	formt=[formt,'Total INTEGRATION of W1 ']
	   	formu=[formu,'W4 = TOTAL ( W1 ,2)']
	   	           	formt=[formt,'Vector INTEGRATION of W1 :SUM the SECOND dimension']
	   	formu=[formu,'W4 = TOTAL ( W1 ,1)']
	   	           	formt=[formt,'Vector INTEGRATION of W1 :Y PROJECTION']
	   	           	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'W4 = CONGRID ( W1 ,100,50)']
	   	           	formt=[formt,'RESIZE W1 to a new matrix sized by 100*50']
	   	formu=[formu,'W1 = W1 > 2']
				formt=[formt,'Force any values in W1 to be >= 2']
	   	formu=[formu,'W4 = ALOG ( W1 + W2 + W3 + 1)']
	   	           	formt=[formt,'The LOGARITHMIC SUM of 3 WKspaces']
	   	           	formu=[formu,''] & formt=[formt,'----']
	   	formu=[formu,'W4 = [  W1  ,  W2  ,  W3  ]']
	   			formt=[formt,'JOIN workspaces into FIRST dimension']
	   	formu=[formu,'W4 = [ [W1] , [W2] , [W3] ]']
	   			formt=[formt,'into SECOND dimension']
	   	formu=[formu,'W4 = [[[W1]],[[W2]],[[W3]]]']
	   			formt=[formt,'into THIRD  dimension']
	   			formu=[formu,''] & formt=[formt,'---- See the IDL user"s guide ...']
		endelse
	endif else if flg eq 589 then begin
	   	formu=[formu,'SCROLL SPECTRA']
	   	formt=[formt,'This interface scrolls individual spectra within a workspace.']
	   	formu=[formu,'                           ']
	   	formt=[formt,'It provides interactive zoom and animation.']
	   	formu=[formu,'RADIAL INTEGRATION']
	   	formt=[formt,'This interface defines sectors of integrations.']
	   	formu=[formu,''] & formt=[formt,'']
	   	formu=[formu,'MASK & GROUP']
	   	formt=[formt,'This interface regroups spectra within a workspace and']
	   	formu=[formu,'                           ']
	   	formt=[formt,'creates a mask which defines defective detectors.']
	   	formu=[formu,'GK_FIT']
	   	formt=[formt,'Calcutates gaussians & lorentz from a spectrum within a workspace.']
	   	formu=[formu,''] & formt=[formt,'']
	   	formu=[formu,'LOAD NEW COLORS']
	   	formt=[formt,'is used for colors adjustements and loading new tables.']
	   	formu=[formu,''] & formt=[formt,'']
	   	formu=[formu,'SCAN Wi']
	   	formt=[formt,'This interface provides interactive facilities such as:']
	   	formu=[formu,'                           ']
	   	formt=[formt,'- SLICING.']
	   	formu=[formu,'                           ']
	   	formt=[formt,'- ZOOMING.']
	   	formu=[formu,'                           ']
	   	formt=[formt,'- INTEGRATING ZONES.']
	   	formu=[formu,'                           ']
	   	formt=[formt,'- FOURIER TRANSFORM.           ..../....']
	   	formu=[formu,'SUPER PLOT']
	   	formt=[formt,'Is usefull to compare spectra within one on several wkp.']
	   	formu=[formu,''] & formt=[formt,'']
	   	formu=[formu,'THE JOURNAL']
	   	formt=[formt,'The journal.']
	   	formu=[formu,'SAVE LAMP SESSION']
	   	formt=[formt,'Workspaces and parameters are saved for next lamp time.']
	   	formu=[formu,''] & formt=[formt,'']

endif else if flg eq 591 then begin

formu=[formu,'SuperPlot']
formt=[formt,' was written by JOUFFREY Romuald, on August 1995. Hope this Helps']
formu=[formu,'What is to be plotted :']
formt=[formt,'']
formu=[formu,'']
formt=[formt,'Adjust workspace number and cutting value with sliders, cutting axis with X or Y button.']

formu=[formu,'Manipulating local workspace plots :']
formt=[formt,'']
formu=[formu,'    "Keep as"']
formt=[formt,' Buttons allow you to keep a workspace in one of the six buffers']
formu=[formu,'']
formt=[formt,'            You can replace any kept workspace by any other, just click !']
formu=[formu,'    "Hide"']
formt=[formt,' temporarily hide a plot without losing it,'+ $
		' data are still processed, without be plotted']
formu=[formu,'    "Scale"']
formt=[formt,' temporarily disactivate scaling of '+ $
		'considered buffer, allowing to scale one plot versus another']
formu=[formu,'    Apply cut to "Current Workspace"']
formt=[formt,' cut is processed only on selected workspace']
formu=[formu,'                          "All Workspace"']
formt=[formt,' cut is processed on all kept workspaces']
formu=[formu,'    ']
formt=[formt,'                           You can change from on mode to the other, cuts are preserved for each']

formu=[formu,'Changing plotting parameters :']
formt=[formt,'']
formu=[formu,'    Bottom horizontal sliders']
formt=[formt,' permit to define X minimum and maximum Range']
formu=[formu,'    Left side slider']
formt=[formt,' defines Y axis scale ratio']
formu=[formu,'    Right side slider']
formt=[formt,' defines Y axis offset values']

formu=[formu,'Integrity of plots versus data :']
formt=[formt,' Beware of errors on plot interpretation']
formu=[formu,'']
formt=[formt,'     - when "normalize all" is set, the Y scale is'+ $
		' from 0 to 1. Each plot is normalized over its own range']
formu=[formu,'']
formt=[formt,'     - when the right side slider' + $
		' isn'+string(39B)+'t at the bottom, each plot has an Y incremental offset.']
formu=[formu,'']
formt=[formt,'     - when the XMin and XMax range sliders are not set to'+ $
		' minimum and maximum respectively.']
formu=[formu,'']
formt=[formt,'     - when the Filter button is set, smooth and median filters are'+ $
		' processed for plotting.']

formu=[formu,'Other Abilities :']
formt=[formt,'']
formu=[formu,'    "PRINT"']
formt=[formt,' Generate a PS file of the plotting window, you get what you see']
formu=[formu,'    "ANNOTATE"']
formt=[formt,' allows you to annotate the plotting window']

endif else if (flg eq 592) or (flg eq 594) then begin
	formu=[formu,'AVAILABLE OPERATORS between runs ']
	formt=[formt,' +   -   >   :']
	formu=[formu,'FRAME OPERATORS for runs or file ']
	formt=[formt,'{ +   -   >   : }']
	formu=[formu,'NO OPERATOR between alphabetic FileName ']
	formt=[formt,'']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'To select a  run  number     ']
	formt=[formt,' Enter only the run number ex: 211 ']
	formu=[formu,'To select and add three runs ']
	formt=[formt,' 211 + 214 + 218']
	formu=[formu,'To add a range of runs       ']
	formt=[formt,' 211 > 300']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'ALLOWED COMBINATIONS ']
	formt=[formt,' 205 + 211>300 + 315 - 316>318 - 321']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'To concatenate runs 211 to 300 and 303 to 314']
	formt=[formt,' 211:300 , 303:314']
	formu=[formu,'To concatenate every third runs  210 to 300']
	formt=[formt,' 210 ::: 300']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'Frame operations in run 211 and 214']
	formt=[formt,' 211{1>5 +7} + 214{1>6}']
	formu=[formu,'Frame operations in Nexus or Spec files ']
	formt=[formt,' File{1>5 +7.3}']
	formu=[formu,'']
	formt=[formt,'']
	IF flg eq 592 then begin
	formu=[formu,'TO STORE A RUN IN W1 USING THE MOUSE ']
	formt=[formt,' Click a SnapShot with the middle button']
	formu=[formu,'TO _ADD_ A RUN TO W1 USING THE MOUSE ']
	formt=[formt,' Click a SnapShot with the right  button']
	endif
	IF flg eq 594 then begin
	formu=[formu,'SCALING AND PROJECTIONS APPLY TO RUNS INDIVIDUALY']
	formt=[formt,' ']
	formu=[formu,'CONSISTENCY IS USED WITH CONCATENATION ']
	formt=[formt,' only']
	endif
endif else if (flg eq 595) then begin
	formu=[formu,'LIKEABLE URL: http://www.ill.fr/YellowBook/D7/home/D7_george_book.html']
	formt=[formt,'']
	formu=[formu,'THE PAD INTERFACE BUTTONS IS DESIGNED FROM A FILE.']
	formt=[formt,'']
	formu=[formu,'DEFAULT FILE:']
	formt=[formt,' lamp/lamp_mac/dial_pad_init.pro']
	formu=[formu,'WHERE TO PLACE YOUR dial_pad_init.pro FILE:']
	formt=[formt,' in the directory where you have your macros.']
	formu=[formu,'']
	formt=[formt,' Otherwise you may have a local dial_pad_init.prox file (see that one in /lamp_mac)']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'SOME WORDS about DIALS:']
	formt=[formt,' Dials are named Objects, designed to perform actions at a frequency time.']
	formu=[formu,'']
	formt=[formt,' A Dial consists of a set of own and general properties, plus a macro procedure.']
	formu=[formu,'']
	formt=[formt,' An object named "model" is placed in a file named "dial_model.pro"']
	formu=[formu,'']
	formt=[formt,' The minimum code for an object is:  (see lamp/lamp_mac/dial_template1.pro  for a more complete Dial)']
	formu=[formu,'    PRO dial_model_macro, Dial']
	formt=[formt,'    ;(The METHOD)']
	formu=[formu,'']
	formt=[formt,'    V=DialNewValue()            &   Dial.value=sqrt(V)']
	formu=[formu,'']
	formt=[formt,'    R=DialControl ("My wish")   &   end']
	formu=[formu,'    FUNCTION dial_model']
	formt=[formt,'    ;(The CONSTRUCTOR)']
	formu=[formu,'']
	formt=[formt,'    return, {NAME:"model", GENERIC:"interface", TYPE:"temperature" }   &   end']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'    DIAL "model" gets its value from function dial_interface_read']
	formt=[formt,' "interface" comes from the GENERIC tag value.']
	formu=[formu,'']
	formt=[formt,'    George hidden call is: v=dial_interface_read("temperature")    if DialNewValue() is used.']
	formu=[formu,'    DIAL "model" sends its command to function dial_interface_send']
	formt=[formt,' "interface" comes from the GENERIC value.']
	formu=[formu,'']
	formt=[formt,'    George hidden call is: errcod=dial_interface_send("temperature",0,"My wish","model")   if DialControl() is used.']
	formu=[formu,'']
	formt=[formt,'']
	formu=[formu,'SOME WORDS about PAD:']
	formt=[formt,' a generic value is associated to each button. George proceeds in the same way']
	formu=[formu,'']
	formt=[formt,'   as for Dials: errcod=dial_myface_send("PAD",0,"My wish","button label")']
	
endif else if (flg eq 596) then begin
	formu=[formu,'Very easy !!!  Check for the template file lamp/lamp_mac/A_List_oldTOF.prox']
	formt=[formt,'']
	formu=[formu,'You will be able to define the tags which describe your macros.']
	formt=[formt,'']
	formu=[formu,'Then put your file A_List_*.prox near your macros or in directory lamp/lamp_mac/']
	formt=[formt,'']
endif
return
end

pro dids
;** ****
return
end
