pro phowind, ytp,rawe,vidp,pry,corre, xout,yin,win, yraw,exclu ,elas,ott,wavel ,ERRdat
;** *******
;**
;** TOF SHOW . (D. Richard).

@lamp.cbk
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
common pho_cu,  pho_grp, pho_rax, pho_fct ,pho_rep, pho_oiw, pho_e,pho_w,pho_x,pho_y,$
		pho_t  , pho_mx , pho_psf

sw   = size(win )
maxw = max(win,min=minw) & maxw=[minw,maxw]
rtp  =[wavel(5:*)]
if n_elements(pho_fct) eq 0 then pho_fct=1
if n_elements(pho_rep) eq 0 then pho_rep=6
if n_elements(pho_grp) eq 0 then pho_grp=sw(2)   $
			    else pho_grp=min([pho_grp,sw(2)])
if n_elements(pho_rax) eq 0 then pho_rax=[0,0]
if n_elements(pho_mx)  eq 0 then pho_mx =maxw(1) $
			    else pho_mx =maxw(1)

if lamp_siz lt 800 then yys=15 else yys=0

wko=ott(0) & wkos=strtrim(string(wko),2) & pho_out=wkos & if wko le 9 then wkos=wkos+' '
wkn=ott(1) & wkns=strtrim(string(wkn),2) & pho_in =wkns
pho_oiw=[pho_out,pho_in]
uv =[-88,340]
psf=pho_rep+pho_fct+(pho_grp+pho_rax(0)+pho_rax(1))/10+wkn
psf='DL'+strtrim(string(psf),2)

bshow =	widget_base  (title='didline '+strtrim(his(wkn),2),group_leader=lamp_b1,$
				   /column,resource_name='lamptouch')
bplt1 = widget_base  (bshow,/row)
bdra1 = widget_draw  (bplt1,xsize=512,ysize=60-yys)
bget1 = widget_base  (bplt1,/column)
blabel= widget_label (bget1,value='RAW SAMPLE PEAKS'      ,font=ft_b_normal)
bggg1 = widget_base  (bget1,/row)
bread = widget_button(bggg1,font=ft_b_normal ,value='re_Treat W'+wkns)
brev  = widget_button(bggg1,font=ft_smallest ,value='<',uvalue=[-88,310,bread,4])
bnex  = widget_button(bggg1,font=ft_smallest ,value='>',uvalue=[-88,311,bread,4])

bplt4 = widget_base  (bshow,/row)
bdra4 = widget_draw  (bplt4,xsize=512,ysize=60-yys)
bcorr = widget_label (bplt4,value='SAMPLE P corrected'    ,font=ft_b_normal)

bplt3 = widget_base  (bshow,/row)
bdra3 = widget_draw  (bplt3,xsize=512,ysize=85-yys)
blabel= widget_label (bplt3,value='EFFICIENCIES'                 ,font=ft_b_normal)
valf  =strtrim(string(wavel),2)
val   =strtrim(string(rtp)  ,2)
betis = widget_button(bplt3,value='?',menu=2,uvalue=rtp          ,font=ft_biggest)
binfo = widget_button(betis,value='Wavelenght   (Angs)= '+valf(0),font=ft_propor)
binfo = widget_button(betis,value='Channel width(msec)= '+valf(1),font=ft_propor)
binfo = widget_button(betis,value='Sample->Det (meter)= '+valf(2),font=ft_propor)
binfo = widget_button(betis,value='Vanadium Temp.  (K)= '+valf(3),font=ft_propor)
binfo = widget_button(betis,value='Sample   Temp.  (K)= '+valf(4),font=ft_propor)
binf2 = widget_button(betis,value='F(E) efficiencies  =IN4,5,6 type',font=ft_propor)

binf2 = widget_button(betis,value='Sample transmission= '+val(0) ,menu=2,font=ft_propor)
 seq  =['.10','.15','.20','.25','.30','.35','.40','.45','.50','.55',$
        '.60','.65','.70','.75','.80','.82','.84','.86','.88','.90',$
        '.91','.92','.93','.94','.95','.96','.97','.98','.99','1.0']
 seq  = reverse    (seq)
 for i=0,n_elements(seq)-1 do $
 btrs = widget_button(binf2,value=seq(i),uvalue=[uv,3,0,binf2,betis]	,font=ft_smallest)
binf2 = widget_button(betis,value='Vana   transmission= '+val(1) ,menu=2,font=ft_propor)
 for i=0,n_elements(seq)-1 do $
 btrv = widget_button(binf2,value=seq(i),uvalue=[uv,3,1,binf2,betis]	,font=ft_smallest)

vabs  = strtrim(string(fix(val(2))),2)
binf2 = widget_button(betis,value='Self-absorption    = '+vabs ,menu=2  ,font=ft_propor)
 bsa1 = widget_button(binf2,value='0=None',uvalue=[uv,3,2,binf2,betis,0]	   ,font=ft_propor)
 case vabs of
 '1':bsa2 = widget_button(binf2,value='1=Single slab',uvalue=[uv,3,2,binf2,betis,1]  ,font=ft_propor)
 '2':bsa2 = widget_button(binf2,value='2=Full cylinder',uvalue=[uv,3,2,binf2,betis,2] ,font=ft_propor)
 '3':bsa2 = widget_button(binf2,value='3=Hollow cylinder',uvalue=[uv,3,2,binf2,betis,3],font=ft_propor)
 else:
 endcase
binf2 = widget_button(betis,value='ABOUT THIS TREATMENT'       ,menu=2  ,font=ft_propor)
 seq  =['- Normalise  vanadium , empty_can , sample according to monitor counts' ,$
        '- Determine  peak position of sample and vanadium'                  ,$
        '- Align      sample and vanadium (empty) using a circular shift'    ,$
        '- Remove     empty_can from sample and vanadium using transmissions',$
        '- Remove     noisy , too low and too high spectra (discrimination)' ,$
        '- Correct    the vanadium using a Debye Waller factor'              ,$
        '- Calibrate  the sample   using the elastic peak of the vanadium'   ,$
        '-(Resolution)instrument is taken from vanadium peak at any angles'  ,$
        '- Correct    for the energy dependent efficiency of the detectors'  ,$
        '- Transform  to energy'                                             ,$
        '- Group      angles if many'                                        ,$
        ' ',$
        'IF YOU CHANGE TRANSMISSION OR ABSORBTION TYPE YOU HAVE TO'          ,$
        'PRESS THE "re_Treat W" BUTTON ON YOUR UPPER RIGHT']
 for i=0,n_elements(seq)-1 do $
  bt  = widget_button(binf2,value=seq(i),font=ft_propor)

if rtp(2) ne 1 then titi='--- EMPTY PEAKS ---' else titi='ABSORPTION shape'
bdra2=0L
blra2=0L
if yys eq 0 then begin
 bplt2= widget_base  (bshow,/row)
 bdra2= widget_draw  (bplt2,xsize=512,ysize=85-yys)
 blra2= widget_label (bplt2,value=titi			,font=ft_b_normal)
endif
brow  = widget_base  (bshow,/row)
br1   = widget_base  (brow ,/column)
btext = widget_list  (br1  ,xsize=14 ,ysize=17		,font=ft_propor)
blabel= widget_label (br1  ,value='TOF reduction'	,font=ft_b_normal)
blabel= widget_label (br1  ,value='by'			,font=ft_smaller)
blabel= widget_label (br1  ,value='D.Richard (7307)'	,font=ft_b_normal)
blabel= widget_label (br1  ,value='M.Ferrand       '	,font=ft_b_normal)

br3   = widget_base  (brow ,/column)
bupd  = widget_button(br3  ,value='re-PLOT'         ,font =ft_b_normal)
blabel= widget_label (br3  ,value='....'            ,font =ft_smallest)
bswqe = widget_button(br3  ,value='S(w,Qel)'        ,font =ft_b_normal)
bswq  = widget_button(br3  ,value='S(w,Q)'          ,font =ft_b_normal)
bpab  = widget_button(br3  ,value='P(a,B)'          ,font =ft_b_normal)
bquas = widget_button(br3  ,value='QUASI'           ,font =ft_b_normal)
blabel= widget_label (br3  ,value='....'            ,font =ft_smallest)
blev  = widget_button(br3  ,value='Levels'          ,font =ft_b_normal)
bsurf = widget_base  (br3  ,/row)
bsu1  = widget_button(bsurf,value='S'               ,font =ft_b_normal)
bsu2  = widget_button(bsurf,value='u'               ,font =ft_b_normal)
bsu3  = widget_button(bsurf,value='r'               ,font =ft_b_normal)
bsu4  = widget_button(bsurf,value='f'               ,font =ft_b_normal)
bsu0  = widget_button(br3  ,value='Front'           ,font =ft_b_normal)
blabel= widget_label (br3  ,value='....'            ,font =ft_smallest)
br3r  = widget_base  (br3  ,/row)
bid   = widget_label (br3r ,value='To'              ,font =ft_smallest)
bhard = widget_button(br3r ,value=psf+'.PS'         ,font =ft_b_normal)
br3r  = widget_base  (br3  ,/row)
bid   = widget_label (br3r ,value='To'              ,font =ft_smallest)
bhinx = widget_button(br3r ,value=psf+'.INX'        ,font =ft_b_normal)
br3r  = widget_base  (br3  ,/row)
bid   = widget_label (br3r ,value='To'              ,font =ft_smallest)
bsav  = widget_button(br3r ,font=ft_propor   ,value='W'+wkos)
brev  = widget_button(br3r ,font=ft_smallest ,value='<',uvalue=[-88,310,bsav,5])
bnex  = widget_button(br3r ,font=ft_smallest ,value='>',uvalue=[-88,311,bsav,5])

br2   = widget_base  (brow ,/column)
bsca  = widget_base  (br2  ,/row)
bidon = widget_label (bsca ,value='Max Count '      	    ,font =ft_b_normal)
bidon = widget_label (bsca ,value=strtrim(maxw(1),2)	    ,font =ft_b_normal)
bidon = widget_label (bsca ,value=' cut to '   	          ,font =ft_b_normal)
bscal = widget_text  (bsca ,value=strtrim(pho_mx ,2),xsize=7,font =ft_propor,/editable)
xwin  = 430
br2r  = widget_base  (br2  ,/row)
bdrac = widget_draw  (br2r ,xsize=xwin,ysize=300)
bslid = widget_base  (br2  ,/row)
bsld1 = widget_base  (bslid,/column)
bsld2 = widget_base  (bslid,/column)
bsli1 = widget_slider(bsld1,title='',min=1  ,max=350,xsize=xwin/2-8,/supp,$
				     value=1  ,/drag,ysize=15)
bsli2 = widget_slider(bsld2,title='',min=151,max=500,xsize=xwin/2-8,/supp,$
				     value=500,/drag,ysize=15)
bran1 = widget_label (bsld1,value='_______________' ,font =ft_propor)
bran2 = widget_label (bsld2,value='_______________' ,font =ft_propor)

bxgrp = widget_base  (br2  ,/row)
bslix = widget_slider(bxgrp,title='',min=3  ,max=500,xsize=xwin,/supp,$
				     value=500,/drag,ysize=15)
blabx = widget_label (bxgrp,value='______'	    ,font =ft_propor)

br2rr = widget_base  (br2r ,/column)
blabg = widget_label (br2rr,value=strtrim(string(pho_grp),2),font=ft_b_normal)
bgrp  = widget_slider(br2rr,title='',min=1,max=sw(2),/supp,value=pho_grp,xsize=17,$
		      /drag,/vertical,ysize=300-20,uvalue=[uv,5,blabg,bran1])

if exclu(0) ge 0 then $
widget_control,btext,set_value=[' Excluded  ' , '  Spectra'  ,  ' ' ,   $
				 strtrim(string(yraw(exclu)) ,2)+' -> '+$
				 strtrim(string(     exclu+1),2)]  else $
widget_control,btext,set_value=[' Excluded  ' , '  Spectra']

if pho_rax(0) eq pho_rax(1) then begin
   idx=where(xout ge -2*xout(sw(1)-1))
   idx=min([idx,(sw(1)-1)/2])
   idx=0
   idz=where(xout ge -2*xout(0))
   if idz(0) le idx then idz=sw(1)-1
   idz=min([idz(0),(sw(1)-1)])
   pho_rax=[idx,idz]
endif else begin
   idx=min([pho_rax(0),sw(1)-4])
   idz=max([min([pho_rax(1),sw(1)-1]),sw(1)/2+2])
   pho_rax=[idx,idz]
endelse

rx1    =xout(idx)
rx2    =xout(idz)
str_lab={sx:sw(1) , idi:idx , xval:xout ,ygrp:pho_grp}
widget_control,bran1,set_uvalue=str_lab,set_value=string(rx1)
str_lab.idi=idz
widget_control,bran2,set_uvalue=str_lab,set_value=string(rx2)

uvs=[uv,1,bran1,bran2,bslix,blabx]
     widget_control,bsli1,set_uvalue=uvs,set_value=(idx+1)*500./sw(1)
uvs=[uv,1,bran2,bran1,bslix,blabx]
     widget_control,bsli2,set_uvalue=uvs,set_value=(idz+1)*500./sw(1)

uvs=[uv,6,blabx,bran1,bran2]
     widget_control,bslix,set_uvalue=uvs  &  grpx=idz-idx+1
     widget_control,blabx,set_value =strtrim(string(grpx),2)

uvs=[uv,2,bran1,bran2,bcorr,1,0 ,blabx,bscal]     	& widget_control,bupd ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,-1,blabx,bscal]     	& widget_control,bswqe,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,-2,blabx,bscal]     	& widget_control,bswq ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,-3,blabx,bscal]     	& widget_control,bpab ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,-4,blabx,bscal]     	& widget_control,bquas,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,1 ,blabx,bscal]     	& widget_control,bsu1 ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,2 ,blabx,bscal]     	& widget_control,bsu2 ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,3 ,blabx,bscal]     	& widget_control,bsu3 ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,4 ,blabx,bscal]     	& widget_control,bsu4 ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,5 ,blabx,bscal]     	& widget_control,bsu0 ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,1,6 ,blabx,bscal]     	& widget_control,blev ,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,3,bread,blabx,bscal,bsav] 	& widget_control,bhinx,set_uvalue=uvs
uvs=[uv,2,bran1,bran2,bcorr,2,bread,blabx,bscal]  	& widget_control,bsav ,set_uvalue=uvs
uvs=[uv,4,betis,ott]		            	  	& widget_control,bread,set_uvalue=uvs

bid=sys_dep      ('DYNLAB',bshow,1)
widget_control,bshow,/realize

if bdra2 gt 0 then widget_control,bdra2,get_value=wip2 else wip2=0
widget_control,bdra1,get_value=wip1
widget_control,bdra3,get_value=wip3
widget_control,bdra4,get_value=wip4
widget_control,bdrac,get_value=wic

uvs=[-88,350,wic,bhard,wkn,2]        & widget_control,bhard,set_uvalue=uvs

 widget_control,bpab  ,sensitive=0
 widget_control,bquas ,sensitive=0
;widget_control,bhinx ,sensitive=0

wicp   =[wic,wip2,blra2]
str_lab={cy:yin,w:win,id:wicp,kn:wkn,mx:maxw,hd:[bhard,bhinx],wl:wavel,err:ERRdat}
widget_control,bcorr,set_uvalue=str_lab

XMANAGER, 'Didline', bshow, event_handler='LAMP_EVENT_PARSER',/just_reg

keepd =!D.window
col   =0
bgr   =255
ry1   =min (vidp)
ry2   =max (vidp) & ryt=ry2
ry3   =max (rawe)
if ry3 gt ry2*5 then ry2=ry2*5 else ry2=max([ry2,ry3])

wset,wip1
plot,ytp,rawe        ,color=col,background=bgr,$
		xmargin=[0,0],ymargin=[0,0],ystyle=4 ,xticklen=1.,xgridstyle=1
wset,wip4
plot,ytp,corre       ,color=col,background=bgr,$
		xmargin=[0,0],ymargin=[0,0],ystyle=4 ,xticklen=1.,xgridstyle=1
wset,wip3
plot,ytp,1./pry      ,color=col,background=bgr,$
		xmargin=[0,0],ymargin=[2,0],ystyle=4 ,xticklen=1.,xgridstyle=1

if wip2 gt 0 then begin
wset,wip2
if rtp(2) eq 1 then ry2=ryt
plot,ytp,vidp        ,color=col,background=bgr,yrange=[ry1,ry2],$
		xmargin=[0,0],ymargin=[0,0],ystyle=4 ,xticklen=1.,xgridstyle=1
endif
trap_current=!D.window

wset,keepd
pho_contour,[wic,0],win,xout,yin,ERRdat, [rx1,rx2],maxw, w_tit(wkn) ,pho_grp,grpx,1,wavel,pho_mx

end

pro pho_contour, wicp ,www,xxx,yyy,eee ,rangx ,maxw, wtit ,nsp,nch ,what ,wavel ,maxim
;** ***********
;**
@lamp.cbk
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
common pho_cu

win =www (pho_rax(0):pho_rax(1),*)<maxim
EER =eee (pho_rax(0):pho_rax(1),*)
xout=xxx (pho_rax(0):pho_rax(1),*)
yin =yyy

		 pho_groupx  , nch ,win ,xout,EER
if nsp gt 1 then pho_groupy  , nsp ,win ,yin ,EER $
	    else begin  yin=total(yin)  /n_elements(yyy)
			win=total(win,2)/n_elements(yyy)
			EER=SQRT(total(EER^2,2))/n_elements(yyy)
			endelse

s   =size(win) & if nsp eq 1 then s(2)=1

if pho_fct eq 1 then begin
   xtit='Energy Changes (meV)'
   ytit='Q elastic'
   ztit='S ( Q elas , W )'
   yin = sin(yin*!pi/180/2) *4*!pi / wavel(0)
endif

if pho_fct eq 2 then begin
   xtit='Energy Changes (meV)'
   ytit='Q'
   ztit='S ( Q , W )'
   qc  = cos(yin*!pi/180)
   ymax= sin(max(yin)*!pi/180/2) *4*!pi / wavel(0) *4
   yin = fltarr(s(1),s(2))

   ee    =(1.05457*  2*!pi)^2 / (2*1.67493) / 1.602177 * 10     ;h^2 /2m /f(joules)
   eelast= ee  /  wavel(0) ^2
   mho   = 0.695014*sqrt(eelast+xout) ;+ if energy changes, - if energy transfert
   mhoc  = mho^2
   ki    = 0.695014*sqrt(eelast)
   kic   = ki ^2

   for i=0,s(2)-1 do yin(*,i)= sqrt(kic + mhoc - 2.* ki * mho * qc(i) )

   yin =   yin<ymax
   xout=         [ [xout],[xout] ]
   xout=   congrid (xout ,s(1),s(2))
endif

if pho_fct eq 3 then begin
   rtemp =wavel(4)>5.
   rmass =1.	& radian=57.3
   xtit  ='Energy(meV)  Temp='+strtrim(string(rtemp),2)+' mass=1.'
   ytit  ='PaB'
   ztit  =''
   yin   = sin(yin*!pi/180/2) *4*!pi / wavel(0)

   ei      =81.799/wavel(0)^2  &  temp_inv = 11.61/rtemp  &  temp_x_mass = rtemp*rmass
   tof_fac = 5.227e6/ei        &  tof_in = sqrt(tof_fac)  &  rec_tof2 = 1.0/tof_in^2
   tau     = 1.0/(sqrt((xout)/5.227e6 + 1.0/tof_fac))     &  tau2 = tau^2  &  tau4 = tau^4

   beta      =(xout*temp_inv)<40  &  exp_beta = exp(0.5*beta)>  1.0e-9
   shine_beta= 0.5 *(exp_beta  - 1.0/exp_beta )

   cos_theta= cos(yin / radian)
   q_eff    = ((1.0/tau2)+rec_tof2-((2.0*cos_theta)/(tof_in*tau)))
   alfa     =(((6.07e7)*q_eff)/temp_x_mass) > .00001

   for cpt=0,s(2)-1 do begin
   	yc        = win(*,cpt)*tof_in/((9.681e-8) *tau4)
	sab       =((0.82412e-8)*tau4*exp_beta*yc)/tof_in
        win(*,cpt)=(2.0*beta*shine_beta*sab)/alfa(cpt)
        
   	yc        = EER(*,cpt)*tof_in/((9.681e-8) *tau4)
	sab       =((0.82412e-8)*tau4*exp_beta*yc)/tof_in
        EER(*,cpt)=(2.0*beta*shine_beta*sab)/alfa(cpt)
   endfor
endif

if pho_fct eq 4 then begin
   xtit='Q elastic'
   ytit='Quasi'
   ztit=''
endif

;PLOT
;****
if what eq 1 then begin
   keepd =!D.window
   bg    =255 & col=0

   if wicp(1) gt 0 then begin
      if nsp gt 1 then proj  =total(win,2)/s(2) else proj=win
      proj  =proj -min(proj)
      wset,wicp(1)
      plot,xout,proj,color=col,background=bg,xtitle=xtit,$
		  xmargin=[0,0],ymargin=[2,0] ,xticklen=1.,xgridstyle=1
      widget_control,bad_id=i,wicp(2),set_value=ytit+' projection'
      endif
   wset,wicp(0)
   if (nsp     eq 1) then begin
      plot,xout,win ,color=col,background=bg,xtitle=xtit,xmargin=[7,1],ymargin=[3,2] $
		    ,xticklen=1.,xgridstyle=1,yticklen=1.,ygridstyle=1
   endif  else $
   if (pho_rep eq 6) then begin
;     if maxw(1) gt 50 then win=win<(maxw(1)/10)
      win =alog(win) & xtit=xtit+' (Log display)'
      if s(1)*s(2) gt 80.*50 then if sys_dep('VERSION') lt 4.1 then ii=execute("win=smooth(win,3)") $
                                                               else ii=execute("win=smooth(win,3,/edge)")
      nl  =24
      contour   ,win,xout,yin,/fill,nlevels=nl ,$
                          xtitle=xtit,ytitle=ytit,title=wtit,$
                          xmargin=[7,1],ymargin=[3,2]
;     contour   ,win,xout,yin,c_colors=(indgen(nl)+1)*(180/nl)+50 ,nlevels=nl ,$
;                         xtitle=xtit,ytitle=ytit,title=wtit,background=bg    ,$
;                         xmargin=[7,1],ymargin=[3,2],color=col
   endif  else $
   if (pho_rep ge 1) and (pho_rep le 5) then begin
      maxz=max(win,min=minz)
      mima=maxz -  minz
      szz =strcompress(string([minz, minz+mima/4  ,minz+mima/2,$
				     minz+mima*3/4,maxz],format='(f8.2)'))
      xx=60 &		    zz=   0
      if pho_rep eq 1  then zz= -20 else if pho_rep eq 2 then zz=  20
      if pho_rep eq 3  then zz=-160 else if pho_rep eq 4 then zz= 160
      if pho_rep eq 5  then xx=   0
      if abs(zz) gt 45 then mtit='' else mtit=wtit
      if xx	 eq 0  then begin   mtit=xtit & xtit='' & endif

      res=sys_dep('VIEWER')
      if (res eq 1) and ((pho_rep eq 3) or (pho_rep eq 4)) then begin
	      if pho_rep eq 3 then matovr,win,xout,yin,/row
	      if pho_rep eq 4 then matovr,win,xout,yin,/col
	      res=sys_dep('VIEWER','lamp.wrl')
      endif else $
      if s(2) gt 16 then $
      	      shade_surf,win,xout,yin,ax=xx,az=zz,color=col,zticks=4,ztickname=szz,$
                                      xtitle=xtit,ytitle=ytit,title=mtit,background=bg,$
                                      xmargin=[7,1],ymargin=[3,2]        $
      else    surface   ,win,xout,yin,ax=xx,az=zz,color=col,/horizontal ,$
                                      xtitle=xtit,ytitle=ytit,title=mtit,background=bg,$
                                      xmargin=[7,1],ymargin=[3,2],zticks=4,ztickname=szz
   endif
   trap_current=!D.window
   wset,keepd
endif

;SAVE
;****
if what ge 2 then begin
   pho_tri,win,xout,yin,what ,EER
   pho_t =[wtit,xtit,ytit]
   
   XICUTE,'W'+pho_oiw(0)+'=sav_didline(W'+pho_oiw(1)+',samp='+pho_oiw(1)+')'
   if what eq 3 then $
   XICUTE,'dumpx,W'+pho_oiw(0) +',"'+ pho_psf+'.INX"'
endif

return
end

pro pho_tri, win,xout,yin,what ,EER
;** *******
;**
common pho_cu

s =size(win)
if (pho_fct eq 2) and (what ge 2) and (s(0) eq 2) then begin
   s =size(win)
   sx=size(xout)
   sy=size(yin)
   n =sx(1)-1
   a =max (xout,min=b)   & gsx = (a-b)/n
   n =sy(sy(0))-1
   a =max (yin ,min=c)   & gsy = (a-c)/n

   if sx(0) eq 1 then begin
      xout=         [ [xout],[xout] ]
      xout=   congrid (xout ,s(1),s(2))
      endif
   if sy(0) eq 1 then begin
      yin =transpose([ [yin],[yin] ])
      yin =     congrid(yin ,s(1),s(2))
      endif

   triangulate,  xout,yin, triangles ,bds
   pho_w=     trigrid(xout,yin,win,  triangles ,[gsx,gsy] )
   pho_e=SQRT(trigrid(xout,yin,EER^2,triangles ,[gsx,gsy] ))

   s    =size(pho_w)
   pho_x=b+findgen(s(1))*gsx
   pho_y=c+findgen(s(2))*gsy

endif else begin pho_x=xout & pho_y=yin & pho_w=win & pho_e=EER & endelse

return
end

function sav_didline, w,sample=n
;******* ***********
;**
@lamp.cbk
common pho_cu

   centm=100000.
   iii =execute('X'+pho_oiw(0)+'=pho_x')
   iii =execute('Y'+pho_oiw(0)+'=pho_y')
   iii =execute('E'+pho_oiw(0)+'=pho_e')
   iii =execute('N'+pho_oiw(0)+'=centm')
   w_tit(fix(pho_oiw(0)))=pho_t(0)
   x_tit(fix(pho_oiw(0)))=pho_t(1)
   y_tit(fix(pho_oiw(0)))=pho_t(2)

return,pho_w
end

pro pho_event ,ev ,uv
;** *********
;**
@lamp.cbk
common pho_cu

case uv(2) of

;Range sliders
;***** *******
1:begin widget_control,bad_id=i,uv(3),get_uvalue=str_lab
	idx	   =round(str_lab.sx/500.*ev.value)-1
	str_lab.idi=idx
	strig      =string(str_lab.xval(idx))
	widget_control,bad_id=i,uv(3),set_uvalue=str_lab,set_value=strig
	widget_control,bad_id=i,uv(4),get_uvalue=str_lab & idz=str_lab.idi
	widget_control,bad_id=i,uv(5),set_value =500
	widget_control,bad_id=i,uv(6),set_value =strtrim((abs(idz-idx)+1)>3 ,2)
        end
;Update Save Buttons
;****** **** *******
2:begin widget_control,bad_id=i,uv(3),get_uvalue=str_s1
	widget_control,bad_id=i,uv(4),get_uvalue=str_s2
	widget_control,bad_id=i,uv(5),get_uvalue=str_yw
	da1     =min([str_s1.idi ,str_s2.idi])
	da2     =max([str_s1.idi ,str_s2.idi]) & if da1 eq da2 then da2=da2+2
	pho_rax=[da1,da2]
	pho_grp= str_s1.ygrp
	rangx  =[str_s1.xval(da1),str_s2.xval(da2)]

	if (uv(6) eq 2) or (uv(6) eq 3) then begin
	   if uv(6) eq 2 then iv=ev.id else iv=uv(10)
	   widget_control,iv  ,get_value=wkn  & wkn=wkn(0) & i=strpos(wkn,'W')
	   wkn=strtrim(strmid(wkn,i+1,2),2)   & pho_oiw(0) =wkn
	   widget_control,uv(7),get_value=wkn & wkn=wkn(0) & i=strpos(wkn,'W')
	   wkn=strtrim(strmid(wkn,i+1,2),2)   & pho_oiw(1) =wkn

	endif else if uv(7) gt 0 then pho_rep= uv(7) $
	      else if uv(7) lt 0 then pho_fct=-uv(7)

        psf    =pho_rep+pho_fct+(pho_grp+pho_rax(0)+pho_rax(1))/10+str_yw.kn
        pho_psf='DL'+strtrim(string(psf),2)
	widget_control,bad_id=i,str_yw.hd(0),set_value=pho_psf+'.PS'
	widget_control,bad_id=i,str_yw.hd(1),set_value=pho_psf+'.INX'

	widget_control,bad_id=i,uv(8),get_value=grpx  & grpx =fix(grpx(0))

	widget_control,bad_id=i,uv(9),get_value=maxou & maxou=maxou(0)
			on_ioerror,mismax & maxim=str_yw.mx(1)
			maxim=float(maxou)
			if (maxim le str_yw.mx(0)) then maxim=str_yw.mx(1) $
			else pho_mx=maxim
			mismax:

	pho_contour, str_yw.id ,str_yw.w ,str_s1.xval ,str_yw.cy ,str_yw.err ,rangx ,$
		     str_yw.mx ,w_tit(str_yw.kn),str_s1.ygrp,grpx ,uv(6),str_yw.wl,maxim
	end

;Change parameters
;****** **********
3:begin if uv(3) eq 0 then begin
;	Sample transmision
	   widget_control,ev.id,get_value =val & valf=float(val)
	   widget_control,uv(5),get_uvalue=rtp & rtp(0)=valf
	   widget_control,uv(5),set_uvalue=rtp
	   widget_control,uv(4),set_value ='Next sample trans -> '+val
	endif
	if uv(3) eq 1 then begin
;	Vanadi transmision
	   widget_control,ev.id,get_value =val & valf=float(val)
	   widget_control,uv(5),get_uvalue=rtp & rtp(1)=valf
	   widget_control,uv(5),set_uvalue=rtp
	   widget_control,uv(4),set_value ='Next vanad. trans -> '+val
	endif
	if uv(3) eq 2 then begin
;	Self-absorbtion
	   widget_control,uv(5),get_uvalue=rtp & rtp(2)=uv(6)
	   widget_control,uv(5),set_uvalue=rtp
	   widget_control,uv(4),set_value ='Next absorb. type -> '+strtrim(string(uv(6)),2)
	endif
	end

;Read   button
;****   ******
4:begin
	widget_control,uv(3),get_uvalue=rtp
	widget_control,ev.id,get_value =wkn & wkn=wkn(0)
	i  =strpos(wkn,'W')
	wkn=strtrim(strmid(wkn,i+1,2),2) & pho_oiw(1)=wkn

	cmd='w'+strtrim(string(uv(4)),2)+'=didline(samp='+wkn
	if uv(6) gt 0  then cmd=cmd+        ',vana=' +strtrim(string(uv(6)),2)
	if uv(7) gt 0  then cmd=cmd+        ',empt=' +strtrim(string(uv(7)),2)
	if uv(8) gt 0  then cmd=cmd+        ',canv=' +strtrim(string(uv(8)),2)
	cmd=cmd+',trans_abs=rtp)'
	iii=execute(cmd)
  	end

;Group  slider for angles
;*****  ****** *** ******
5:begin widget_control,uv(3),set_value =strtrim(string(ev.value),2)
	widget_control,uv(4),get_uvalue=str_s1
	str_s1.ygrp=ev.value
	widget_control,uv(4),set_uvalue=str_s1
  	end

;Group  slider for channels
;*****  ****** *** ********
6:begin widget_control,uv(4),get_uvalue=str_lab & da1=str_lab.idi
	widget_control,uv(5),get_uvalue=str_lab & da2=str_lab.idi
	idx=((da2-da1+1)*ev.value/500)>3
	widget_control,uv(3),set_value =strtrim(string(idx),2)
	end
else:

endcase

return
end

pro pho_groupy, nsp,win,yin ,ERR
;** **********
;**
     sw=size(win)
     if nsp ne sw(2) then begin
        gp  =float (sw(2))/nsp
	wtmp=fltarr(sw(1) ,nsp)
	ERRt=fltarr(sw(1) ,nsp) & if n_elements(ERR) eq 0 then ERR=SQRT(win)
	ytmp=fltarr(nsp)

	for  i =nsp -1 ,1,-1  do begin
	     j1=round(i*gp)
	     j2=round(i*gp+gp-1)
	     j3=j2-j1+1
	     if j3 gt 1 then wtmp(*,i)=     total(win(*,j1 : j2)    ,2)  /j3 $
	     		else wtmp(*,i)=           win(*,j1)
	     if j3 gt 1 then ERRt(*,i)=SQRT(total(ERR(*,j1 : j2)^2  ,2)) /j3 $
	     		else ERRt(*,i)=           ERR(*,j1)
	     		     ytmp  (i)=     total(yin(  j1 : j2)      )  /j3
	endfor
	     j3=round(gp)
	     if j3 gt 1 then wtmp(*,0)=     total(win(*, 0 : j3-1)  ,2)  /j3 $
	     		else wtmp(*,0)=           win(*, 0)
	     if j3 gt 1 then ERRt(*,0)=SQRT(total(ERR(*, 0 : j3-1)^2,2)) /j3 $
	     		else ERRt(*,0)=           ERR(*, 0)
	     		     ytmp  (0)=     total(yin(   0 : j3-1)    )  /j3

	win=wtmp & wtmp=0
	ERR=ERRt & ERRt=0
	yin=ytmp & ytmp=0
     endif
return
end

pro pho_groupx, nch,win,xin ,ERR
;** **********
;**
     sw=size(win)
     if nch ne sw(1) then begin
        gp  =float (sw(1))/nch
	wtmp=fltarr(nch,sw(2))
	ERRt=fltarr(nch,sw(2)) & if n_elements(ERR) eq 0 then ERR=SQRT(win)
	xtmp=fltarr(nch)

	for  i =nch -1 ,1,-1  do begin
	     j1=round(i*gp)
	     j2=round(i*gp+gp-1)
	     j3=j2-j1+1
	     if j3 gt 1 then wtmp(i,*)=     transpose(total(win(  j1 : j2,*)  ,1)) /j3 $
	     		else wtmp(i,*)=                     win(  j1     ,*)
	     if j3 gt 1 then ERRt(i,*)=SQRT(transpose(total(ERR(  j1 : j2,*)^2,1)))/j3 $
	     		else ERRt(i,*)=                     ERR(  j1     ,*)
	     		     xtmp  (i)=               total(xin(  j1 : j2) )       /j3
	endfor
	     j3=round(gp)
	     if j3 gt 1 then wtmp(0,*)=     transpose(total(win( 0 : j3-1,*)  ,1)) /j3 $
	     		else wtmp(0,*)=                     win( 0       ,*)
	     if j3 gt 1 then ERRt(0,*)=SQRT(transpose(total(ERR( 0 : j3-1,*)^2,1)))/j3 $
	     		else ERRt(0,*)=                     ERR( 0       ,*)
	     		     xtmp  (0)=               total(xin( 0 : j3-1) )       /j3

	win=wtmp & wtmp=0
	ERR=ERRt & ERRt=0
	xin=xtmp & ytmp=0
     endif
return
end

function didline , radata , sample=sampln  , vanadium=vanadn , empty=emptn , canvan=canva , trans_abs=rtp , cadmium=cadmn
;******* *******
;**
;** Reality show . (D. Richard)
;**
@lamp.cbk

if n_params() eq 1 then begin INX,(one+0),(two+0) & return,radata & endif
instv=strupcase(inst_value)

;****CHECK PARAMETERS
;****
second=two

if n_elements(radata) gt 1 then rodata=radata else $
if n_elements(sampln) eq 1 then if (sampln gt 0)   and (sampln le 20) then  begin second=sampln
   rodata=0 & sn=strtrim(string(sampln),2) & i=execute('rodata=w'+sn) & i=execute('ERRdat=e'+sn)
   endif else sampln=0 else sampln=0

if n_elements(vanadn) eq 1 then if (vanadn gt 0)   and (vanadn le 20) then begin
   vanadi=0 & vn=strtrim(string(vanadn),2) & i=execute('vanadi=w'+vn)
   endif else vanadn=0 else vanadn=0

empti=0
if n_elements(emptn ) eq 1 then if (emptn  gt 0)   and (emptn le 20)  then begin
   empti =0 & en=strtrim(string(emptn) ,2) & i=execute('empti =w'+en) & i=execute('ERRemp=e'+en)
   endif else emptn =0 else emptn =0

if n_elements(canva ) eq 1 then if (canva  gt 0)   and (canva le 20)  then begin
   canvi =0 & ci=strtrim(string(canva) ,2) & if (canva ne emptn) then   i=execute('canvi =w'+ci)
   endif else canva =0 else canva =0

if n_elements(cadmn ) eq 1 then if (cadmn  gt 0)   and (cadmn le 20)  then begin
   cadmi =0 & di=strtrim(string(cadmn) ,2) & i=execute('cadmi =w'+di) & i=execute('ERRcad=e'+di)
   endif else cadmn =0 else cadmn =0

wk1 =strtrim(string(one),2) & wk2=strtrim(string(second),2)

sizs= size(rodata)
siza= size(vanadi)
sizn= size(empti)
sizc= size(canvi)
sizd= size(cadmi)
win = 0
if  n_elements(vanadi) le 1   then  begin v_is= 0 & siza=sizs & endif  else v_is=1
if  n_elements(empti)  le 1   then  begin n_is= 0 & sizn=sizs & endif  else n_is=1
if  n_elements(canvi)  le 1   then  begin c_is= 0 & sizc=sizs & endif  else c_is=1
if  n_elements(cadmi)  le 1   then  begin d_is= 0 & sizd=sizs & endif  else d_is=1
if  emptn  eq sampln		     then n_is=-1
if (emptn  eq canva) and (n_is eq 1) then c_is= 2
;if (c_is   eq 0)     and (n_is eq 1) then c_is= 2

if (sizs(0) eq 2) and (siza(0) eq 2) and (sizn(0) eq 2) and (sizc(0) eq 2) and $
   (sizs(1) eq siza(1)) and (sizs(1) eq sizn(1)) and (sizs(1) eq sizc(1))  and $
   (sizs(2) eq siza(2)) and (sizs(2) eq sizn(2)) and (sizs(2) eq sizc(2)) then begin
    rodata=rodata>0
    if n_elements(ERRdat) ne n_elements(rodata) then ERRdat=SQRT(rodata)
    if n_elements(ERRemp) ne n_elements(empti)  then ERRemp=SQRT(empti)
    if n_elements(ERRcad) ne n_elements(cadmi)  then ERRcad=SQRT(cadmi)
;****
;****
     p_in=0 & p_vn=0 & iii = execute('p_in=p'+wk2)
     if v_is eq 1 then iii = execute('p_vn=p'+vn)

     cwidth= 1. & wavel=1. & dist=1. & kelvis=0. & kelvin=0. & s_elas=sizs(1)/2.+.5 & doppl=1.
     if n_elements(p_in) gt 27 then begin
      doppl = p_in(2)>0.
      cwidth= p_in(18)
      wavel = p_in(21)
      dist  = p_in(27)
      kelvis= p_in(11)
      s_elas= p_in( 9)>0
      if v_is eq 1 then kelvin= p_vn(11)
     endif
     if   (instv eq 'IN10') or (instv eq 'IN16') or $
	  (instv eq 'IN13') then wavel=doppl
     if wavel gt 0 then begin
     if dist  gt 0 then begin

     fe_a  = 1.0      & f5_a  = 0.951
     fe_b  =-0.0565   & f5_b  =-0.0887
     fe_c  =-3.284    & f5_c  =-4.07
     if   (instv eq 'IN4') then begin fe_a=0.951 & fe_b=-.0887 & fe_c=-5.597 & endif
     
     if n_elements(rtp) gt 1 then begin
     		if rtp(0) eq 0 then rtp(0)=.99
     		if rtp(1) eq 0 then rtp(1)=.99
		transs= rtp(0) & Ts=transs
		transv= rtp(1) & Tv=transv
		transc= 0.97   & Tc=transc
		transn= 0.97   & Tn=transn
		slab  = rtp(2)
		tolow = rtp(3)
		ra1   = rtp(4)
		ra2   = rtp(5)
		ires  = rtp(6)
		igg   = fix(rtp(7)) & bgsub=[0,0,0,0] & bgsub(0)=(igg and 1)/1 & bgsub(1)=(igg and 2)/2
		                                        bgsub(2)=(igg and 4)/4 & bgsub(3)=(igg and 8)/8
		sangl = rtp(8)
		test  = long(sangl/180.)
		if test*180 eq sangl then slab=0
      endif  else begin
		transs= 0.90   & Ts=transs
		transv= 0.85   & Tv=transv
		transc= 0.97   & Tc=transc
		transn= 0.97   & Tn=transn
		slab  = 0
		tolow =15./100
		ra1   =0
		ra2   =0
		ires  =0
		igg   =1       & bgsub =[1,0,0,0]
		sangl =0
     endelse

     if strpos(strlowcase(x_tit(second)),'energy')  ge 0   then inergy=1 else inergy=0
     if v_is ne 1 then bgsub(0)=0 & if n_is ne 1 then bgsub(2)=0 & if c_is ne 1 then bgsub(3)=0

;*** ANGLE RANGE
;***
     xout=0 & iii =execute('xout=x'+wk2) & xout=xout(*,0)
     yraw=0 & iii =execute('yraw=y'+wk2)

     if n_elements(xout) ne sizs(1) then xout=findgen(sizs(1))+1.
     if n_elements(yraw) ne sizs(2) then yraw=findgen(sizs(2))+10. & sizy=size(yraw)
     elas = where(xout ge 0) & elas=elas(0)>0
     if elas gt 0 then s_elas= elas
     if sizy(0) eq 2 then yraw=reform(yraw(s_elas,*))

     if ra2 gt ra1 then begin
	id1=where(yraw ge ra1) & id1=id1(0)>0
	id2=where(yraw ge ra2) & id2=id2(0) & if id2 lt id1 then id2=n_elements(yraw)-1
	if id2 gt id1 then begin
				  rodata=rodata(*,id1:id2)
				  yraw  =yraw  (  id1:id2)
				  ERRdat=ERRdat(*,id1:id2)
		if n_is eq 1 then ERRemp=ERRemp(*,id1:id2)
		if n_is eq 1 then empti =empti (*,id1:id2)
		if c_is eq 1 then canvi =canvi (*,id1:id2)
		if d_is eq 1 then cadmi =cadmi (*,id1:id2)
		if d_is eq 1 then ERRcad=ERRcad(*,id1:id2)
		if v_is eq 1 then vanadi=vanadi(*,id1:id2)
		sizs= size(rodata)
	endif
     endif

   if not inergy then begin

;****GROUP SAME ANGLES
;****
     if v_is eq 1  then begin yt=yraw &  groupy,vanadi,yt         ,/average & endif
     if n_is eq 1  then begin yt=yraw &  groupy,empti ,yt  ,ERRemp,/average & endif
     if c_is eq 1  then begin yt=yraw &  groupy,canvi ,yt         ,/average & endif
     if d_is eq 1  then begin yt=yraw &  groupy,cadmi ,yt  ,ERRcad,/average & endif
					 groupy,rodata,yraw,ERRdat,/average
     sizs=size (rodata)

;****NORMALIZE BY MONITORS
;****
        msamp =1 & i=execute('msamp=total(n'+wk2+'(*,0))>1') & centm=100000.
        rodata=(rodata)* ( centm/msamp)
	ERRdat=(ERRdat)* ( centm/msamp)
	msamp =centm

     if v_is eq 1 then begin
        mvana =1 & i=execute('mvana=total(n'+vn +'(*,0))>1')
        vanadi=(vanadi)* ( msamp/mvana)
     endif
     if n_is eq 1 then begin
        mempt =1 & i=execute('mempt=total(n'+en +'(*,0))>1')
        empti =(empti) * ( msamp/mempt)
	ERRemp=(ERRemp)* ( msamp/mempt)
     endif
     if c_is eq 1 then begin
        mcanv =1 & i=execute('mcanv=total(n'+ci +'(*,0))>1')
        canvi =(canvi) * ( msamp/mcanv)
     endif
     if d_is eq 1 then begin
        mcadm =1 & i=execute('mcadm=total(n'+di +'(*,0))>1')
        cadmi =(cadmi) * ( msamp/mcadm)
	ERRcad=(ERRcad)* ( msamp/mcadm)
     endif

;****SELF ABSORPTION CURVE
;****
     if (slab eq 1) and ((n_is eq 1) or (v_is*c_is ge 1)) then begin
	 alpha=sangl*!pi/180.
	 Yr =	 yraw/180.*!pi
	 ida=where(Yr eq alpha)
	 if ida(0) ge 0 then Yr(ida)=Yr(ida)*1.01 ;(Yr((Ida-1)>0)+Yr((ida+1)<(n_elements(yr)-1)))/2.01
	 idt=where(Yr lt alpha) & idr=where(Yr gt alpha)
	 As =	-alog(TRANSs)/   sin(alpha)
	 Bc =	-alog(TRANSc)/(2*sin(alpha))
	 Av =	-alog(TRANSv)/   sin(alpha)
	 Bn =	-alog(TRANSn)/(2*sin(alpha))
	 Mex=85. ;EXP(89)=Inf!

	 if (v_is eq 1) and (c_is eq 2) then begin Bn=Bc & canvi=empti & c_is=1 & endif

	 if idt(0) ge 0 then begin
	    j=idt(0)
	    Rt =    sin(alpha)  / sin(alpha - Yr(idt))
;*	    SAMPLE IN TRANSMISSION
	    vecSt=(exp(-Bc*(Rt+1))*exp(-As*Rt)*(exp((As*(Rt-1))<Mex)-1))/(As*(Rt-1)) ;1/A1

	    if n_is eq 1 then begin ;A2~
	       vectt=(exp(-As*Rt) * exp(-Bc*Rt) +  exp(-As) * exp(-Bc)) / ((1+exp((Bc*(Rt-1))<Mex))*exp(-Bc*Rt))
	    endif
;*	    VANADIUM IN TRANSMISSION
	    vecVt=(exp(-Bn*(Rt+1))*exp(-Av*Rt)*(exp((Av*(Rt-1))<Mex)-1))/(Av*(Rt-1)) ;1/A1

	    if v_is*c_is eq 1 then begin ;A2~
	       vevtt=(exp(-Av*Rt) * exp(-Bn*Rt) +  exp(-Av) * exp(-Bn)) / ((1+exp((Bn*(Rt-1))<Mex))*exp(-Bn*Rt))
	    endif
	 endif
	 if idr(0) ge 0 then begin
	    j=idr(0)
	    Rr =  -sin(alpha)  /  sin(Yr(idr) - alpha)
;*	    SAMPLE IN REFLEXION
	    A1Sr=(exp((Bc*(Rr-1))<Mex)*(exp((As*(Rr-1))<Mex)-1))/(As*(Rr-1)) ;1/A1
	    if idt(0) ge 0 then vecSt=[vecSt,A1Sr] else vecSt=A1Sr

	    if n_is eq 1 then begin ;A2~
	       A2r=(1+exp((Bc*(Rr-1))<Mex)*exp((As*(Rr-1))<Mex)) / (1+exp((Bc*(Rr-1))<Mex))
	       if idt(0) ge 0 then vectt=[vectt,A2r] else vectt=A2r
	    endif
;*	    VANADIUM IN REFLEXION
	    A1Vr=(exp((Bn*(Rr-1))<Mex)*(exp((Av*(Rr-1))<Mex)-1))/(Av*(Rr-1)) ;1/A1
	    if idt(0) ge 0 then vecVt=[vecVt,A1Vr] else vecVt=A1Vr

	    if v_is*c_is eq 1 then begin ;A2~
	       A2r=(1+exp((Bn*(Rr-1))<Mex)*exp((Av*(Rr-1))<Mex)) / (1+exp((Bn*(Rr-1))<Mex))
	       if idt(0) ge 0 then vevtt=[vevtt,A2r] else vevtt=A2r
	    endif
	 endif
	 AA1=vecVt/vecSt
     endif else slab=0

;****REMOVE EMPTY CAN BEFORE ALINE ???
;****
     if bgsub(2) eq 0 then begin
      	 if n_is  eq 1 then                   rodata=(rodata - Ts*empti)
      	 if n_is  eq 1 then                   ERRdat=SQRT(ERRdat^2 + (Ts*ERRemp^2))
      	 if d_is  eq 1 then if n_is eq 1 then rodata=(rodata - (1-Ts)*cadmi)
      	 if d_is  eq 1 then if n_is eq 1 then ERRdat=SQRT(ERRdat^2 + (((1-Ts)*ERRcad^2)))
      	 if v_is  eq 1 then if c_is eq 2 then vanadi=(vanadi - Tv*empti)
      	 if d_is  eq 1 then if c_is eq 2 then vanadi=(vanadi - (1-Ts)*cadmi)
     endif
     if bgsub(3) eq 0 then begin
      	 if v_is  eq 1 then if c_is eq 1 then vanadi=(vanadi - Tv*canvi)
     endif

;****ALINE
;****
     exclu=[-1]   &  s_idx=indgen(sizs(2)) & decal=intarr(sizs(2))       & v_elas=s_elas
     excln=[-1]
     exclv=[-1]
     exclg=[-1]
     if bgsub(3) then cav=lineup(canvi  ,g_elas,g_idx,exclg,bid  ,tolow)
     if bgsub(2) then vid=lineup(empti  ,n_elas,n_idx,excln,bid  ,tolow ,ERRORS=ERRemp)
     if bgsub(0) then van=lineup(vanadi ,v_elas,v_idx,exclv,decal,tolow) & s_elas=v_elas
     if bgsub(1) then win=lineup(rodata ,s_elas,s_idx,exclu,decal,tolow ,ERRORS=ERRdat) else win=rodata

     if bgsub(1) then if not bgsub(0) then for i=0,sizs(2)-1 do win   (*,i)=shift(win   (*,i),decal(i))
     if bgsub(1) then if not bgsub(0) then for i=0,sizs(2)-1 do ERRdat(*,i)=shift(ERRdat(*,i),decal(i))
     if bgsub(0) then if not bgsub(1) then for i=0,sizs(2)-1 do van   (*,i)=shift(van   (*,i),decal(i))
     wro=win

     if  n_is eq 1  then if bgsub(2)  then begin
	 if n_elas gt 0 then $
	 if s_elas ne n_elas then vid   =shift(vid   ,s_elas-n_elas,0)
	 if s_elas ne n_elas then ERRemp=shift(ERRemp,s_elas-n_elas,0)
	endif else vid=empti

     if  v_is eq 1  then if bgsub(0)  then begin
	 if v_elas gt 0 then $
	 if s_elas ne v_elas then van   =shift(van   ,s_elas-v_elas,0)
	endif else van=vanadi

     if  c_is eq 1  then if bgsub(3)  then begin
	 if g_elas gt 0 then $
	 if s_elas ne g_elas then cav   =shift(cav,s_elas-g_elas,0)
	endif else cav=canvi

;****REMOVE EMPTY CAN AFTER ALINE ???
;****
     if bgsub(2) eq 1 then begin              win   =(win - Ts*vid)
                                              ERRdat=SQRT(ERRdat^2 + (Ts*ERRemp^2))
      	 if d_is  eq 1 then if n_is eq 1 then win   =(win - (1-Ts)*cadmi)
      	 if d_is  eq 1 then if n_is eq 1 then ERRdat=SQRT(ERRdat^2 + (((1-Ts)*ERRcad^2)))
         if v_is  eq 1 then if c_is eq 2 then van   =(van - Tv*vid)
      	 if d_is  eq 1 then if c_is eq 2 then van   =(van - (1-Ts)*vid)
     endif
     if bgsub(3) eq 1 then                    van   =(van - Tv*cav)

;****SLAB CORRECTIONS
     if slab then begin
     ;?? if n_is      eq 1 then for i=0,n_elements(vectt)-1 do vid (*,i)=vid (*,i) * vectt(i)
     ;?? if v_is*c_is eq 1 then for i=0,n_elements(vevtt)-1 do cav (*,i)=cav (*,i) * vevtt(i)
	 			for i=0,n_elements(AA1)  -1 do win   (*,i)=win   (*,i) * AA1  (i)
	                        for i=0,n_elements(AA1)  -1 do ERRdat(*,i)=ERRdat(*,i) * AA1  (i)
     endif
     
;****MERGE GOOD INDEX GIVEN BY LINEUP
;****
     idx=s_idx
     if (v_is eq 1) or (n_is eq 1) or (c_is eq 1) then begin
     	 if excln(0) ge 0  then if exclv(0) ge 0  then exclv=[exclv,excln] else exclv=excln
     	 if exclg(0) ge 0  then if exclv(0) ge 0  then exclv=[exclv,exclg] else exclv=exclg
	 if exclv(0) ge 0  then begin
     	    for i= 0,n_elements(s_idx)-1 do begin
	        n= where(exclv eq s_idx(i) ,cnt)
	        if cnt gt 0  then begin
				  exclu   =[exclu,s_idx(i)]
				  s_idx(i)=-1   &   endif
     	    endfor
     	    idx=s_idx(where(s_idx ge 0))
     	    if  n_elements(exclu) gt 1 then  begin
     	    	if exclu(0) eq -1 then exclu=exclu(1:*)
     	    	exclu=exclu(sort(exclu))
	    endif
	 endif
     endif
;****REDUCTION OF BAD DETECTORS
;****
     rr =3    &         win   =win   (rr:sizs(1)-rr-2,idx)
     			ERRdat=ERRdat(rr:sizs(1)-rr-2,idx)
			xout  =xout  (rr:sizs(1)-rr-2)
     if  v_is eq 1 then van   =van   (rr:sizs(1)-rr-2,idx)
     if  n_is eq 1 then vid   =vid   (rr:sizs(1)-rr-2,idx)
     if  slab eq 1 then vecSt =vecSt (idx)
     if  slab eq 1 then vecVt =vecVt (idx)
     sizs  =size (win)

     rawe=total( wro((s_elas-10)>0 : (s_elas+10)<(sizs(1)-1),idx) ,1)/21  &  wro=0

     s_elas=(s_elas-rr)>0
     nsp  =n_elements(idx)
     decal=decal(idx)
     yin  =yraw (idx)
     ytp  =yin

;****REMOVE ELECTR. NOISES IF NO EMPTY GIVEN
;****
     if  n_is eq  0 then vidp=fltarr(nsp)+1 else $
     if  n_is eq -1 then begin
	 pry =total (win,2)
	 pidx=sort  (pry)
	 pidx=pidx  (4:(n_elements(pidx)/4)>5)  &  nch =n_elements(pidx)
	 vch =(nch/10)>2
	 vid =fltarr(vch,nsp)
	 vie =fltarr(vch,nsp)

         for  i= 0,vch-1 do vid(i,*)= win   (pidx(i),*)
         for  i= 0,vch-1 do vie(i,*)= ERRdat(pidx(i),*)
         pry= total(vid  ,1)/vch
	 vie= total(vie^2,1)/vch
         for  i= 0,nsp-1 do win   (*,i)=    (win   (*,i)  -pry(i)) >0
         for  i= 0,nsp-1 do ERRdat(*,i)=SQRT(ERRdat(*,i)^2+vie(i))

         if v_is eq 1 then begin
            for  i= 0,vch-1 do vid(i,*)= van(pidx(i),*)
            pry=(total(vid,1))/vch
            for  i= 0,nsp-1 do van(*,i)=(van(*,i)-pry(i)) >0
         endif

         vidp=pry
     endif else $
         vidp=total(vid((s_elas-10)>0:(s_elas+10)<(sizs(1)-1),*),1)/21

;****NORMALIZE BY EFFICIENCIES
;****
     win=win+.3
     if (v_is eq 0) and (n_is eq -1) then begin
	 van =fltarr(nch,nsp)
	 for  i= 0,nch-1 do van(i,*)=win(pidx(i),*)
         pry =(total(van,1)/nch)+1 & van=0

     endif else if v_is eq 0 then pry=fltarr(nsp)+1 $

     else begin
	 tmp=total(van,2) & tmp=tmp/max(tmp)
	 van=van>0
	 print,'Elastic peak position:',s_elas+rr
         pry=(total(van((s_elas-10)>0:(s_elas+15)<(sizs(1)-1),*),1)/26)+1
;*       DEBYE-WALLER attenuation
;*
         kc   = (sin(yin*!pi/180/2) *4*!pi / wavel) ^2
         dwf  = exp(-.0067*kelvin/300 *kc)
         pry  = pry * dwf
     endelse
     mx   = max ( pry,min=mn )
     pry  = total(pry)/n_elements(pry)/pry

     for i=0,nsp-1 do win   (*,i)=win   (*,i)*pry(i)
     for i=0,nsp-1 do ERRdat(*,i)=ERRdat(*,i)*pry(i)
     if v_is*ires eq 1 then for i=0,nsp-1 do van(*,i)=van(*,i)*dwf(i)
    ;vidp =  vidp*pry
     corre=total(win((s_elas-10)>0:(s_elas+10)<(sizs(1)-1),*),1)/21

;****NEGATIVE VALUES
;****
     if n_is eq 1 then begin
         	idx=where(win   lt 0) & n =n_elements(idx)
		if n gt 1 then begin
			nto=total(win(idx))/n & pto=total(win)/n_elements(win)
			mxx=max  (win,min=mii)
			print,'Transmission Correction Info: E_negative='+string(nto)+' E_positive='+string(pto)
			print,'************ Min='+string(mii)+' Max='+string(mxx)
         		win =win>.3
		endif
     endif

;****Care with multidetector distance
;    --------------------------------
     j1=0  &  j3=0  &  j4=nsp-1

     a =where(yin lt 9.)
     if a(0) ge 0 then begin n=n_elements(a)-1 & j2=a(n) & j3=j2+1 & endif

;****TO S(w,2*theta)
;****
     decad = decal * cwidth/252.781/wavel
     cwidts= cwidth*1e-6
     xgen  = findgen   (sizs(1))

     ee    =(1.05457*  2*!pi)^2 / (2*1.67493) / 1.602177 * 10     ;h^2 /2m /f(joules)
     eelast= ee     /  wavel ^2
     telast= dist   *  wavel /3956.				  ;3956=1e6/252.781
     fltime= telast - (s_elas-xgen)*cwidts
     lam   = fltime *  3956. /dist

     yout =sin(yin*!pi/180/2) *4*!pi / wavel

      if   (instv eq 'IN10') or (instv eq 'IN16') then begin
       if  (instv eq 'IN10') then t2efac= 1.234 else t2efac=1.036
       xgen =xgen+1
       xout =xgen*2.0*t2efac*doppl/max(xgen)
       xout =xout-0.5*max(xout)
      endif else begin

       xout  = eelast -  ee    /lam ^2

;****  F(E) EFFICIENCIES
;****
       eff  = fe_a *exp( fe_b /sqrt(eelast-xout)) * (1-exp( fe_c /sqrt(eelast-xout)))
       ef5  = f5_a *exp( f5_b /sqrt(eelast-xout)) * (1-exp( f5_c /sqrt(eelast-xout)))
       flti = 1
       if instv eq 'IN5' then begin
          j5=where(Yin le 63) & j5=j5(n_elements(j5)-1)
          for i=j1,j5 do begin
		    telas2  =(dist-decad(i))*  wavel /3956.
     		    flti    =(telas2   - (s_elas-xgen)*cwidts) ^4 * 1.0e6 / telas2 *10
		    			   win   (*,i)= win   (*,i) * flti  / ef5
		    			   ERRdat(*,i)= ERRdat(*,i) * flti  / ef5
		    if v_is*ires eq 1 then van   (*,i)= van   (*,i) * flti  / ef5
          endfor
          j1=j5+1
       endif
       for i=j1,j4 do begin
		    telas2  =(dist-decad(i))*  wavel /3956.
     		    flti    =(telas2   - (s_elas-xgen)*cwidts) ^4 * 1.0e6 / telas2 *10
		    			   win   (*,i)= win   (*,i) * flti  / eff
		    			   ERRdat(*,i)= ERRdat(*,i) * flti  / eff
		    if v_is*ires eq 1 then van   (*,i)= van   (*,i) * flti  / eff
       endfor
      endelse

;*****GROUP
;*****
      gp  =1
      if sizs(2) gt 80  then gp=gp+1
      if sizs(2) gt 160 then gp=gp+1
      nsp =nsp/gp
      tms =yin  &	     pho_groupy, nsp,win,yin ,ERRdat
      if v_is*ires eq 1 then pho_groupy, nsp,van,tms
      sizs=size (win)

;*****RESOLUTION FILTER
;*****
      if v_is*ires eq 1 then begin
      tmc=total(van,2)
      tmv=smooth(smooth(median(tmc,3),3),3)
     
	id1=s_elas & m=tmv(s_elas)
	while (id1 gt 0) and (tmv(id1-1) lt m) do begin m=tmv(id1) & id1=id1-1 & endwhile
	id2=s_elas & m=tmv(s_elas) & n=sizs(1)-1
	while (id2 lt n) and (tmv(id2+1) lt m) do begin m=tmv(id2) & id2=id2+1 & endwhile
	
	id =max([s_elas-id1   , id2-s_elas]  )
	tmc=tmc((s_elas-id)>0 :(s_elas+id)<n )
			;tmc=[0,tmc-min(tmc),0]
			; mx=tmc(s_elas) & tmc=smooth(median(tmc,3),3) & tmc(id+1)=mx
	tmc=tmc/total(tmc)
	tmc=tmc>1E-4
	tmc=tmc-1E-4

;      win(*,20)=diconv(win(*,20),tmc,/stat)
      help,tmc
      i=1 & if i then $
      for i=0,nsp-1 do begin print,i,nsp-1
	tms=win(*,i)
;	if ires eq 1 then tms=diconv(tms,tmc,/use,/filt) $
;	             else tms=diconv(tms,tmc,/use,/stat)
	win(*,i)=tms
      endfor
      endif
;*****
      rr=2 & win   = win   (rr:sizs(1)-rr-2,*)
	     ERRdat= ERRdat(rr:sizs(1)-rr-2,*)
	     xout  = xout  (rr:sizs(1)-rr-2,*)
      sizs  =size (win)
      s_elas=(s_elas-rr)>0
      van=0 & vid=0 & cav=0

;*****REVERSE X AXIS (NOT TREATED BY INX FORTRAN)
      P_MUS,'mus_shot'
      win   =reverse( win  )
      ERRdat=reverse(ERRdat)
      xout  =reverse(-xout)
      s_elas=sizs(1) -s_elas -1

;*****WHERE ALREADY IN ENERGIE !!
   endif else begin
      yout=yraw
      yin =asin(yout*wavel/4/!pi)*180*2/!pi
      win =rodata
      ytp =yin & rawe=yin*0+1 & vidp=rawe & pry=rawe & corre=reform(win(s_elas,*))
      exclu=[-1]
   endelse

;*****CALL THE GRAPHICAL SHOW
     if slab eq 1  then vidp=vecSt
     ott   =[one,second,vanadn,emptn,canva]
     wavel =[wavel,cwidth,dist,kelvin,kelvis,transs,transv,slab,tolow,ra1,ra2,$
	     ires ,igg   ,sangl]
     phowind, ytp,rawe,vidp,pry,corre, xout,yin,win, yraw,exclu ,s_elas,ott,wavel ,ERRdat

;****OUTPUT VARIABLES BACK TO LAMP
;****
     iii=execute('y'+wk1+'=yout'  )
     iii=execute('x'+wk1+'=xout'  )
     iii=execute('e'+wk1+'=ERRdat')
     iii=execute('n'+wk1+'=centm' )
     x_tit(one)= 'Energy Changes (meV)'
     y_tit(one)= 'Q elastic'

endif else begin mess='! Sample-Detector distance too small [P'+wk2+'(27)]'
		 widget_control,l_message,bad_id=i,set_value=mess & print,string(7b),mess & endelse
endif else begin mess='! Wave-lenght not defined [P'+wk2+'(21) or P'+wk2+'(2)]'
		 widget_control,l_message,bad_id=i,set_value=mess & print,string(7b),mess & endelse
endif else begin mess='! Workspaces have different sizes ...'
		 widget_control,l_message,bad_id=i,set_value=mess & print,string(7b),mess & endelse
return,win
end
