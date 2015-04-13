pro inx_event, event,uv
;** *********
;**
@lamp.cbk
common c_inx,inx_base,iws,iwv,iwe,iwc,iwr,its,itv,islab,ilo,ith0,ith1,ith2,ialph,ires,ibgmod

if uv(2) eq 1 then begin

   on_ioerror,mis
   text='? Bad workspace number... '
   widget_control,bad_id=i,uv(3) ,get_value=str & str=strlowcase (str(0))
	  iws   =strmid(str,strpos(str,'w')+1,10)  & nws=fix(strtrim(iws   ,2))
   widget_control,bad_id=i,uv(5) ,get_value=str & str=strlowcase (str(0))
	  iwe   =strmid(str,strpos(str,'w')+1,10)  & nwe=fix(strtrim(iwe   ,2))
   widget_control,bad_id=i,uv(6) ,get_value=str & str=strlowcase (str(0))
	  iwr   =strmid(str,strpos(str,'w')+1,10)  & nwr=fix(strtrim(iwr   ,2))
   widget_control,bad_id=i,uv(7) ,get_value=str & str=strlowcase (str(0))
	  iwv(0)=strmid(str,strpos(str,'w')+1,10)  & nwv=fix(strtrim(iwv(0),2))
   widget_control,bad_id=i,uv(15),get_value=str & str=strlowcase (str(0))
	  iwv(1)=strmid(str,strpos(str,'w')+1,10)  & nwd=fix(strtrim(iwv(1),2))
   widget_control,bad_id=i,uv(11),get_value=str & str=strlowcase (str(0))
	  iwc   =strmid(str,strpos(str,'w')+1,10)  & nwc=fix(strtrim(iwc   ,2))
   
   text='? Bad transmission value... '
   widget_control,bad_id=i,uv(4) ,get_value=str & str=str(0) & nts=float(str) 	  & its=str
   widget_control,bad_id=i,uv(8) ,get_value=str & str=str(0) & ntv=float(str) 	  & itv=str

   text='? Bad discrimination factor... '
   widget_control,bad_id=i,uv(10),get_value=str & str=str(0) & nlo=float(str)/100 & ilo=str

   text='? Bad slab angle... ' & nalph=0
   if islab gt 0 then begin widget_control,bad_id=i,uv(14),get_value=str & str=str(0)
			    nalph=float(str) & ialph=strcompress(str) & endif

   text=''
   on_ioerror,norange & n=-400 & nth1=n & nth2=n
   widget_control,bad_id=i,uv(12),get_value=str & str=str(0) & nth1=float(str)    & ith1=strcompress(str)
   widget_control,bad_id=i,uv(13),get_value=str & str=str(0) & nth2=float(str)    & ith2=strcompress(str)
   str=''
   norange:if (nth1 eq n) or (nth2 eq n) or (nth1 ge nth2)  then  begin
   			if ith0 eq 1 then text='? Bad angle range values...'
   			nth1=0 & nth2=0
   	   endif else   if ith0 eq 0 then begin nth1=0 & nth2=0 & endif
   
   if (nws gt 20) or (nwe gt 20) or  (nwr gt 20) or (nwd gt 20) or $
   		     (nwv gt 20) or  (nwc gt 20) then text='? Workspace gt 20... '
   if (nwr le 0 ) then  text='? Resulting workspace must be specified... '
   if (nws le 0 ) then  text='? Sample workspace must be specified... '
   mis:
   if text ne ''  then  widget_control,bad_id=i,uv(9),set_value=text+str  $
   else begin
  	widget_control,bad_id=i,uv(9),set_value='Working ...'
	ibgm=ibgmod(0)+ibgmod(1)*2+ibgmod(2)*4+ibgmod(3)*8
   	wk=[nwr,nws,nwv,nwe,nwc,nwd]     	 	 & wks= strtrim(string(wk),2)
   	rt=[nts,ntv,islab,nlo,nth1,nth2,ires,ibgm,nalph] & rtp= strtrim(string(rt),2)
   	
   	text='w'+wks(0)+'=didline (samp='+wks(1)+',vana='+wks(2)+',empty='+wks(3)+',canva='+wks(4)+ $
   				 ',cadm='+wks(5)+',trans_abs=['+rtp(0)+','+rtp(1)+','+rtp(2)+','+rtp(3)+ $
   				 	    ','+rtp(4)+','+rtp(5)+','+rtp(6)+','+rtp(7)+','+rtp(8)+'])'
   	XICUTE,text
	to_don_history,-1,0,'w'+wks(0)+'=didline(w'+wks(1)+')'
  	widget_control,bad_id=i,event.top,/destroy
   endelse
   
endif
if uv(2) eq 2 then islab  = uv(3)
if uv(2) eq 3 then ith0   = event.select
if uv(2) eq 4 then ires   = event.select
if uv(2) eq 5 then ibgmod(uv(3)) = event.select

return
end

pro inx ,o,t
;** ***
;**
@lamp.cbk
common c_inx

i=xregistered('Inx')
if i le 0 then begin

if n_elements(iws) eq 0 then begin
   iws=' 0  '  & iwv=[' 0  ',' 0  ']   & iwe=' 0  ' & iwc=' 0  ' & iwr=' 0  '    & ires=0  & ialph='      '
   its=' 0.90' & itv=' 0.85' & islab=0 & ilo='15.'  & ith0=0     & ith1='      ' & ith2 ='      '
   ibgmod=intarr(4) & ibgmod(0)=1
endif

if n_elements(o) eq 1 then begin iws=strtrim(string(t),2) & iwr=strtrim(string(o),2) & endif
if lamp_siz ge  900 then p_set_font,1
if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0

inx_base =widget_base  (title='Lamp parameters for DIDLINE',/column,resource_name='lamp')

base =widget_base  (inx_base,/column,resource_name='mic')
bsamp=widget_base  (base  ,/row)
blab =widget_label (bsamp ,value='Sample workspace'  	 ,font=ft_b_bigger)
bsamw=widget_text  (bsamp ,value=iws    ,/editable   	 ,font=ft_propor  ,xsize=4+cap,ysize=1)
blab =widget_label (bsamp ,value='Transmission'      	 ,font=ft_b_bigger)
bsamt=widget_text  (bsamp ,value=its    ,/editable   	 ,font=ft_propor  ,xsize=8,ysize=1)

bempt=widget_base  (base  ,/row)
blab =widget_label (bempt ,value='Empty-C workspace'	 ,font=ft_b_bigger)
bempw=widget_text  (bempt ,value=iwe    ,/editable   	 ,font=ft_propor  ,xsize=4+cap,ysize=1)
blab =widget_label (bempt ,value='Transmission 0.97'     ,font=ft_b_bigger)

bvana=widget_base  (base  ,/row)
blab =widget_label (bvana ,value='Vanadium workspace'	 ,font=ft_b_bigger)
bvanw=widget_text  (bvana ,value=iwv(0) ,/editable   	 ,font=ft_propor  ,xsize=4+cap,ysize=1)
blab =widget_label (bvana ,value='Transmission'      	 ,font=ft_b_bigger)
bvant=widget_text  (bvana ,value=itv    ,/editable   	 ,font=ft_propor  ,xsize=8,ysize=1)

bvaca=widget_base  (base  ,/row)
blab =widget_label (bvaca ,value='Empty-Van workspace'	 ,font=ft_b_bigger)
bvanc=widget_text  (bvaca ,value=iwc    ,/editable    	 ,font=ft_propor  ,xsize=4+cap,ysize=1)
blab =widget_label (bvaca ,value='Transmission 0.97'     ,font=ft_b_bigger)

bresu=widget_base  (base  ,/row)
blab =widget_label (bresu ,value='RESULTING..workspace'  ,font=ft_b_bigger)
bresw=widget_text  (bresu ,value=iwr    ,/editable       ,font=ft_propor  ,xsize=4+cap,ysize=1)
blab =widget_label (bresu ,value='(Cadmium wks'          ,font=ft_b_bigger)
bcadm=widget_text  (bresu ,value=iwv(1) ,/editable   	 ,font=ft_propor  ,xsize=4+cap,ysize=1)
blab =widget_label (bresu ,value=')'                     ,font=ft_b_bigger)

;blab =widget_label (base  ,value='.......'		 ,font=ft_propor)

base =widget_base  (inx_base,/column,resource_name='did')
bslab=widget_base  (base  ,/row)
blab =widget_label (bslab ,value='Self-absorption'   	 ,font=ft_b_bigger)
btog =widget_base  (bslab ,/column,/exclusive)
ntog =4
btg  =lonarr(ntog)
blab =['None','Single slab','Full cylinder','Hollow cylinder']
for i=0,ntog-1 do btg(i)=widget_button(btog  ,value=blab(i),font=ft_propor,/no_release,$
					     uvalue=[-88,341,2,i])
bslab=widget_base  (bslab ,/column)
blab =widget_label (bslab ,value='Angle:',font=ft_b_bigger)
balph=widget_text  (bslab ,value= ialph  ,/editable   ,font=ft_propor  ,xsize=6,ysize=1)

;blab =widget_label (base  ,value='...............'    ,font=ft_propor)

base =widget_base  (inx_base,/column,resource_name='don')
btlow=widget_base  (base  ,/row)
blab =widget_label (btlow ,value='Discrimination spectrum factor (%)' ,font=ft_b_bigger)
bdisc=widget_text  (btlow ,value=ilo    ,/editable   ,font=ft_propor  ,xsize=4+cap,ysize=1)

bteta=widget_base  (base  ,/row)
btete=widget_base  (bteta ,/nonexclusive)
btetb=widget_button(btete ,value='Use 2*Theta range from' ,font=ft_b_bigger,uvalue=[-88,341,3])
btet1=widget_text  (bteta ,value=ith1,/editable   ,font=ft_propor  ,xsize=6+cap,ysize=1)
blab =widget_label (bteta ,value='to',font=ft_b_bigger)
btet2=widget_text  (bteta ,value=ith2,/editable   ,font=ft_propor  ,xsize=6+cap,ysize=1)

;blab =widget_label (base  ,value='.....................'	   ,font=ft_propor)

base =widget_base  (inx_base,/column,resource_name='ben')
baso =widget_base  (base    ,/row)
bid  =widget_label (baso    ,value="Align from: ",font=ft_b_bigger)
bbgmo=widget_base  (baso    ,/row,/nonexclusive)
bbgm1=widget_button(bbgmo   ,value='vana'      ,font=ft_b_normal,uvalue=[-88,341,5,0])
bbgm2=widget_button(bbgmo   ,value='sample'    ,font=ft_b_normal,uvalue=[-88,341,5,1])
bbgm3=widget_button(bbgmo   ,value='empty'     ,font=ft_b_normal,uvalue=[-88,341,5,2])
bbgm4=widget_button(bbgmo   ,value='empty_v'   ,font=ft_b_normal,uvalue=[-88,341,5,3])

breso=widget_base  (base  ,/row,/nonexclusive)
bresb=widget_button(breso ,value='Use vana as instrument resolution' ,font=ft_b_bigger,$
			  uvalue=[-88,341,4])

berr =widget_label (base  ,value='____________________________________________',font=ft_propor)

uv   =[-88,341,1,bsamw,bsamt,bempw,bresw,bvanw,bvant,berr,bdisc,bvanc,btet1,btet2,balph,bcadm]
bgo  =widget_button(base  ,value='NEXT STEP' ,uvalue=uv,font=ft_b_bigger)
bexit=widget_button(base  ,value='CANCEL'    ,uvalue=[-88,399])

widget_control , btg(islab),set_button=1
widget_control , btetb     ,set_button=ith0
widget_control , bresb     ,set_button=ires
widget_control , bbgm1     ,set_button=ibgmod(0)
widget_control , bbgm2     ,set_button=ibgmod(1)
widget_control , bbgm3     ,set_button=ibgmod(2)
widget_control , bbgm4     ,set_button=ibgmod(3)
widget_control , btg(2)    ,sensitive =0
widget_control , btg(3)    ,sensitive =0
widget_control , bresb     ,sensitive =0
widget_control , inx_base  ,group_leader=lamp_b1,/realize
if lamp_siz ge 900 then p_set_font,0

XMANAGER, 'Inx', inx_base  ,event_handler='LAMP_EVENT_PARSER',/just_reg

endif else widget_control,bad_id=i,inx_base,map=1

return
end
