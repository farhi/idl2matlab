function Unroll, w
;******* ******
;**
@lamp.cbk
common cw_diag, b_rol, b_win, b_t, b_v, b_s, b_new, b_wl, b_ww, b_arel, b_diam, b_xdia,b_p,$
                b_area,b_areu,b_show  , b_z, b_on1, b_on2,b_on3,b_red , b_chg , b_f1, b_f2
common depli  , cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
common deplo  , rr1,rr2, av,ah
common destop , wit
common dexxl  , xxl1,xxl2

	wi=fix(b_ww) & wj=fix(b_wl) & s=size(b_arel)
	sd=(s(1)-1.)/2.

	if squar ne 0 then  begin ii=execute('x'+b_ww+'=(findgen(s(1))-sd)/sd * 3.1416')
			    ii=execute('x'+b_ww+'=x'+b_ww+'(xxl1:xxl2)')
	endif    else       ii=execute('x'+b_ww+'=(findgen(s(1))-sd)/sd * 3.1416*rr2')
	ii=execute('y'+b_ww+'= indgen (s(2))+rr1>1')
	if squar ne 0 then x_tit(wi)   ='UNROLLED RINGS. X axis: -Pi<--->Pi  ('+x_tit(wj)+')'  $
		      else x_tit(wi)   ='UNROLLED RINGS. X axis: -Pi*R<--->Pi*R  ('+x_tit(wj)+')'
	y_tit(wi)   ='RADIUS in pixels'
	
if squar ne 0 then return,b_arel(xxl1:xxl2,*) else return,b_arel
end

function Diagram, w
;******* *******
;**
@lamp.cbk
common cw_diag, b_rol, b_win, b_t, b_v, b_s, b_new, b_wl, b_ww, b_arel, b_diam, b_xdia,b_p,$
                b_area,b_areu,b_show  , b_z, b_on1, b_on2,b_on3,b_red , b_chg , b_f1, b_f2
common depli  , cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
common deplo  , rr1,rr2, av,ah

        wi=fix(b_ww) & wj=fix(b_wl) & s=size(b_arel)
	ii=execute('x'+b_ww+'=b_xdia')
	x_tit(wi)   ='DIAGRAM 2*Theta ('+x_tit(wj)+')'
	y_tit(wi)   ='MEAN VALUES'
return,b_diam
end

pro wdiag_event, event,uv
;** ***********
;**
@lamp.cbk
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
common cw_diag, b_rol, b_win, b_t, b_v, b_s, b_new, b_wl, b_ww, b_arel, b_diam, b_xdia,b_p,$
                b_area,b_areu,b_show  , b_z, b_on1, b_on2,b_on3,b_red , b_chg , b_f1, b_f2
common depli  , cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
common deplo  , rr1,rr2, av,ah

if (uv(2) eq 1) or $
  ((b_chg ne b_on3) and ((uv(2) eq 3) or (uv(2) eq 4) or (uv(2) eq 5))) then begin
    b_chg =  b_on3
        if uv(2) eq 1 then begin
           widget_control,uv(3),get_value=wnum  & wnum=wnum(0)      ;Load
           i =strpos(wnum,'W') & b_wl=strtrim(strmid(wnum,i+1,4),2)
           wsdiag & endif
        s =0 & ii=execute('s=size(w'+b_wl+')')
        if s(0) eq 2 then begin
           sw=b_z(0) & pi=1. & pj=1.
           if PIXH lt PIXV then pi=PIXH/PIXV else pj=PIXV/PIXH
           b_p  =[1+ ((PIXV/PIXH)-1)/2. , 1+ ((PIXH/PIXV)-1)/2.]

           b_f1 =float(sw)/(s(1)) & b_f2=float(sw)/(s(2))
           if b_f1 lt b_f2 then b_f2=b_f1 else b_f1=b_f2
           b_f1=b_f1*pi  & b_f2=b_f2*pj
           si=round(s(1)*b_f1) & sj=round(s(2)*b_f2)
           ii=execute ('b_area=congrid(w'+b_wl+',si,sj)')
           if b_on3 then b_area=alog((temporary(b_area)>0)+1)
           b_area=bytscl(temporary(b_area))
        endif
    if uv(2) eq 1 then begin b_new=1 & uv(2)=3
    endif else if (size(b_arel))(0) eq 2 then begin
           b_areu=congrid( b_arel,b_z(0),b_z(0))
           if b_on3 then b_areu=alog((temporary(b_areu)>0)+1)
           b_areu=bytscl(temporary(b_areu))
    endif
endif

if uv(2) eq 11 then if (event.type eq 0) and (b_show eq 1) then begin;Mouse for Center & Ray
   if event.press eq 1 then begin                                    ;Center
	cti=event.X / b_f1  + 0.5
	ctj=event.Y / b_f2  + 0.5
	if (cti gt long(cti)+ 0.25) and (cti lt long(cti)+ 0.75) then cti=long(cti)+0.5 else cti=round(cti)
	if (ctj gt long(ctj)+ 0.25) and (ctj lt long(ctj)+ 0.75) then ctj=long(ctj)+0.5 else ctj=round(ctj)
	widget_control,b_t(0),set_value=strtrim(string(cti),2)
	widget_control,b_t(1),set_value=strtrim(string(ctj),2)
   endif else begin                                                  ;Ray
	nn1=event.X / (b_f1+b_f2) *2.
	nn2=event.Y / (b_f1+b_f2) *2.
	rr2=round(sqrt((abs(nn1-cti)+1)^2 + (abs(nn2-ctj)+1)^2))
	widget_control,b_t(6),set_value=strtrim(string(rr2),2)
   endelse
   uv(2)=3 & b_new=1
endif

case uv(2) of
2: begin widget_control,event.id,get_value=wnum  & wnum=wnum(0)      ;Write
        i =strpos(wnum,'W') & b_ww=strtrim(strmid(wnum,i+1,4),2)
        if b_show eq 2 then XICUTER,'w'+b_ww+'=Unroll (w'+b_wl+')'
        if b_show eq 3 then XICUTER,'w'+b_ww+'=Diagram(w'+b_wl+')'
   end
3: begin wsdiag  & wset,b_win(2) & erase,255 & tvscl,b_area & b_show=1 ;input Data
        sw=b_z(0) & sh=b_z(1) & sw1=sw-1
        ctx=cti-0.5 & cty=ctj-0.5
        ccx=ctx*b_f1 & ccy=cty*b_f2 & nn1=rr1*(b_f1+b_f2)/2 & nn2=rr2*(b_f1+b_f2)/2

        tx1=round([ccx-nn2,ccx+nn2])>0<sw1 & ty1=round([ccy,ccy])>0<sw1
        tx2=round([ccx,ccx])>0<sw1         & ty2=round([ccy-nn2,ccy+nn2])>0<sw1
        tx3=round([ccx-nn1,ccx+nn1])>0<sw1 & ty3=round([ccy,ccy])>0<sw1
        tx4=round([ccx,ccx])>0<sw1         & ty4=round([ccy-nn1,ccy+nn1])>0<sw1
        plots,tx2,ty2,/device,color=255   & plots,tx1,ty1,/device,color=255
        plots,tx3,ty3,/device,color=0     & plots,tx4,ty4,/device,color=0

        plots,tx1,ty1-1,/device,color=0   & plots,tx1,ty1+1,/device,color=0
        plots,tx2-1,ty2,/device,color=0   & plots,tx2+1,ty2,/device,color=0
        plots,tx3,ty3-1,/device,color=255 & plots,tx3,ty3+1,/device,color=255
        plots,tx4-1,ty4,/device,color=255 & plots,tx4+1,ty4,/device,color=255

	nn2 = nn2-1 & n = round(nn2 * 100./256.)>20
	ceri=(findgen(n)-(n-1)/2.) / ((n-1)/2.) * !pi
	cery=(sin(ceri)*nn2 + ccy) >0<sw1 & cerx=(cos(ceri)*nn2 + ccx) >0<sw1
	plots,cerx,cery,/device,color=0
	nn2 = nn2+1 & n = round(nn2 * 100./256.)>20
	ceri=(findgen(n)-(n-1)/2.) / ((n-1)/2.) * !pi
	cery=(sin(ceri)*nn2 + ccy) >0<sw1 & cerx=(cos(ceri)*nn2 + ccx) >0<sw1
	plots,cerx,cery,/device,color=255

	n = round(nn1 * 100./256.)>20
	ceri=(findgen(n)-(n-1)/2.) / ((n-1)/2.) * !pi
	cery=(sin(ceri)*nn1 + ccy) >0<sw1 & cerx=(cos(ceri)*nn1 + ccx) >0<sw1
	plots,cerx,cery,/device,color=0

	if b_new then begin s=0
	   ii=execute('s =size(w'+b_wl+')')    & td=0    & tr=b_z(1)/10
	   nn2=rr2*b_p(0)
	   si=(cti+nn2-5)>0<(s(1)-1) & sj=(cti+nn2+4)>0<(s(1)-1)
	   ii=execute('td=w'+b_wl+'(si:sj,*)') & wset,b_win(4) & erase,255
	   td=reform(td) & sd=size(td) & if sd(0) eq 2 then begin
	      td=congrid(temporary(td),sd(1)*tr,round(sd(2)*b_f2))
	      if b_on3 then td=alog((temporary(td)>0)+1)
	      td=bytscl(temporary(td)) & tvscl,td,0,0
	      tp1=[0,((ctx+nn2-si)>0<(s(1)-1))*tr]
	      plots,tp1,ty1  ,/device,color=255 & plots,tp1,ty1+1,/device,color=0
	      plots,tp1,ty1-1,/device,color=0
	      endif
	   si=(cti-nn2+5)>0<(s(1)-1) & sj=(cti-nn2-4)>0<(s(1)-1)
	   ii=execute('td=w'+b_wl+'(sj:si,*)') & wset,b_win(0) & erase,255
	   td=reform(td) & sd=size(td) & if sd(0) eq 2 then begin
	      td=congrid(temporary(td),sd(1)*tr,round(sd(2)*b_f2))
	      if b_on3 then td=alog((temporary(td)>0)+1)
	      td=bytscl(temporary(td)) & tvscl,td,b_z(1)-sd(1)*tr,0
	      tp1=[-((si-ctx+nn2)>0<(s(1)-1))*tr,0]+b_z(1)-1
	      plots,tp1,ty1  ,/device,color=255 & plots,tp1,ty1+1,/device,color=0
	      plots,tp1,ty1-1,/device,color=0
	      endif
	   nn2=rr2*b_p(1)
	   si=(ctj+nn2-5)>0<(s(2)-1) & sj=(ctj+nn2+4)>0<(s(2)-1)
	   ii=execute('td=w'+b_wl+'(*,si:sj)') & wset,b_win(1) & erase,255
	   td=reform(td) & sd=size(td) & if sd(0) eq 2 then begin
	      td=congrid(temporary(td),round(sd(1)*b_f1),sd(2)*tr)
	      if b_on3 then td=alog((temporary(td)>0)+1)
	      td=bytscl(temporary(td)) & tvscl,td,0,0
	      tp1=[0,((cty+nn2-si)>0<(s(2)-1))*tr]
	      plots,tx2  ,tp1,/device,color=255 & plots,tx2+1,tp1,/device,color=0
	      plots,tx2-1,tp1,/device,color=0
	      endif
	   si=(ctj-nn2+5)>0<(s(2)-1) & sj=(ctj-nn2-4)>0<(s(2)-1)
	   ii=execute('td=w'+b_wl+'(*,sj:si)') & wset,b_win(3) & erase,255
	   td=reform(td) & sd=size(td) & if sd(0) eq 2 then begin
	      td=congrid(temporary(td),round(sd(1)*b_f1),sd(2)*tr)
	      if b_on3 then td=alog((temporary(td)>0)+1)
	      td=bytscl(temporary(td)) & tvscl,td,0,b_z(1)-sd(2)*tr
	      tp1=[0,-((si-cty+nn2)>0<(s(2)-1))*tr]+b_z(1)-1
	      plots,tx2  ,tp1,/device,color=255 & plots,tx2+1,tp1,/device,color=0
	      plots,tx2-1,tp1,/device,color=0
	      endif
	endif
   end
4: begin wsdiag  & if b_new then begin tAV=AV & tAH=AH & ii=execute( $
       'depli, w'+b_wl+',rr1,rr2, b_arel,b_diam,b_xdia,b_red,tAV,tAH')       ;diagram
                  endif
        b_new=0 & wset,b_win(2) & b_show=3
        titx ='DIAGRAM 2*Theta'
        if b_on3 then tity ='MEAN (Log)' else tity='MEAN'
        trap_current=!D.window
        if b_on3 then $
           plot,b_xdia,alog(b_diam>.5),xtitle=titx,ytitle=tity,background=255,color=0 else $
           plot,b_xdia,     b_diam    ,xtitle=titx,ytitle=tity,background=255,color=0
   end
5: begin wsdiag  & if b_new then begin tAV=AV & tAH=AH & ii=execute( $
                 'depli, w'+b_wl+',rr1,rr2, b_arel,b_diam,b_xdia,b_red,tAV,tAH')       ;unroll
		  yf=(size(b_arel))(2)
		  if yf le b_z(0)/4 then yf=yf*4   else $
		  if yf le b_z(0)/3 then yf=yf*3   else $
		  if yf le b_z(0)/2 then yf=yf*2 & yf=yf<b_z(0)
                  b_areu=congrid( b_arel,b_z(0),yf)
                  if b_on3  then  b_areu=alog((temporary(b_areu)>0)+1)
                  b_areu=bytscl(temporary(b_areu))
		endif else yf= (size(b_areu))(2)
        b_new=0 & wset,b_win(2) & erase,255 & tvscl,b_areu,0,(b_z(0)-yf)/2 & b_show=2
        if b_win(5) gt 0 then begin
           nic  =n_elements(b_diam)
           mind =min(b_diam)
           b_dia=b_diam(2:nic-3)
           idx  =where(b_dia ne 0)
           if idx(0) ge 0 then b_dia=b_dia(idx) else b_dia=[0]
           mine =total(b_dia)/n_elements(b_dia)

           indi1=((b_diam - shift(b_diam, 2)))
           indi2=((b_diam - shift(b_diam,-2)))
           indil= (indi1<0) * (indi2<0)
           indih= (indi1>0) * (indi2>0)
           indil(0:1)=0 & indil(nic-2:*)=0
           indih(0:1)=0 & indih(nic-2:*)=0

           idx  =where(indil>0)
           if idx(0) ge 0 then indil=b_diam(idx) else indil=mine
           idx  =where(indih>0)
           if idx(0) ge 0 then indih=b_diam(idx) else indih=mine
           indih=(total(indih)/n_elements(indih))
           indil=(total(indil)/n_elements(indil))
           indid=1./((indih-indil)>0.01)
           tit="Value to be minimized: "+strtrim(string(indid),2)
           wset,b_win(5)
           trap_current=!D.window
           plot,b_xdia,b_diam,color=0,xstyle=5,ystyle=5,subtitle="",title=tit,background=255,$
                xmargin=[0,0],ymargin=[0,2],charsize=1.5,charthick=2,font=-1,/nodata
           polyfill,[b_xdia(0),b_xdia,b_xdia(nic-1)],[mind,b_diam,mind],color=100,/data
           oplot,b_xdia,b_diam,color=0
        endif
   end
6: begin b_on1=event.select & widget_control,b_t(21),map=b_on1       ;Distortion
   end
7: begin b_on2=event.select & widget_control,b_t(22),map=b_on2       ;Plate angle
   end
8: begin b_on3=event.select                                          ;Log
   end
9: begin b_arel=0 & b_area=0 & b_areu=0 & b_diam=0 & b_xdia=0        ;destroy
         widget_control,event.top,/destroy
   end
10:begin b_red=event.select & b_new=1                                ;Reduce
   end
12:begin                                                             ;Mouse Stop
   end
13:begin shape=uv(3)        & b_new=1                                ;Shapes
   end
14:begin squar=0 & b_new=1  & end                                    ;Raw
15:begin squar=1 & b_new=1  & end                                    ;Squared (keep total intensity)
16:begin squar=2 & b_new=1  & end                                    ;Square  (keep pixel intensity)

else:
endcase

end

pro wgdiag, idx
;** ******
;**
common cw_diag, b_rol, b_win, b_t, b_v, b_s, b_new, b_wl, b_ww, b_arel, b_diam, b_xdia,b_p,$
                b_area,b_areu,b_show  , b_z, b_on1, b_on2,b_on3,b_red , b_chg , b_f1, b_f2
common depli  , cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar

tmp='0' & widget_control,bad_id=ii,b_t(idx),get_value=tmp & tmp=tmp(0)
flg= 1  & on_ioerror,misflt & val=0. & val=float(tmp) & flg=0 & misflt:
if idx eq 2  then val=val>0.1
if idx ge 3  then val=val>0.
if idx eq 6  then val=val>1.
b_v(idx)=val
b_s(idx)=strtrim(string(b_v(idx)),2)
if flg then widget_control,bad_id=ii,b_t(idx),set_value=b_s(idx)
end

pro wsdiag
;** ******
;**
common cw_diag, b_rol, b_win, b_t, b_v, b_s, b_new, b_wl, b_ww, b_arel, b_diam, b_xdia,b_p,$
                b_area,b_areu,b_show  , b_z, b_on1, b_on2,b_on3,b_red , b_chg , b_f1, b_f2
common depli  , cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
common deplo  , rr1,rr2, av,ah

for idx=0,n_elements(b_t)-3 do wgdiag, (idx)

if (not b_new) then  $
if (b_v(0)  ne cti)  or (b_v(1)  ne ctj)  or (b_v(2)  ne DIS)  or (b_v(3)  ne PIXV) or (b_v(4)  ne PIXH) or $
   (b_v(5)  ne rr1)  or (b_v(6)  ne rr2)  or (b_v(7)  ne a1 )  or (b_v(8)  ne a2 )  or (b_v(9)  ne FQ )  or $
                                             (b_v(14) ne PHI)  then b_new=1
if (b_on1)  then     if (b_v(12) ne dxt)  or (b_v(13) ne dyt)  or $
                        (b_v(10) ne FCTX) or (b_v(11) ne FCTY) then b_new=1
if (b_on2)  then     if (b_v(15) ne av )  or (b_v(16) ne ah)   or (b_v(17) ne lhl)  or (b_v(18) ne lhr)  or $
                        (b_v(19) ne lvu)  or (b_v(20) ne lvd)  then b_new=1

	cti =b_v(0)   &  ctj =b_v(1)
	lvu =b_v(19)  &  lvd =b_v(20)
	lhl =b_v(17)  &  lhr =b_v(18)
	PIXH=b_v(3)   &  PIXV=b_v(4)
	FCTX=b_v(10)  &  FCTY=b_v(11) ;** Spacial distortion center %.

	dxt =b_v(12)  &  dyt =b_v(13) ;** Spacial distortion factors.
	a1  =b_v(7)   &  a2  =b_v(8)  ;** Sector to analyse.
	DIS =b_v(2)                   ;** Sample to Detector distance in Cm.
	FQ  =b_v(9)                   ;** Spacial distortion parameters.
	PHI =b_v(14)

	rr1  =b_v(5)  &  rr2  =b_v(6) & av=b_v(15) & ah=b_v(16)

	if (not b_on1) then begin dxt=0 & dyt=0 & endif
	if (dxt eq 0)  and (dyt eq 0) then begin  FCTX=-1. & FCTY=-1.  & endif
	if (not b_on2) then begin lvu=0 & lvd=0 & lhl=0 & lhr=0 & av=0 & ah=0 & endif
end

;pro DECOR, cti_,ctj_,a1_,a2_,DIS_,PIXV_,PIXH_,shap,squa, LVu_,LVd_,LHl_,LHr_,FQ_,PHI_,DXT_,DYT_,FCTX_,FCTY_
;;** *****
;;**
;common depli,cti,ctj, a1,a2, DIS,PIXV,PIXH, shape,squar, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
;
;	cti=cti_ & ctj=ctj_ & a1=a1_   & a2=a2_       & DIS=DIS_   & PIXV=PIXV_ & PIXH=PIXH_
;	LVu=LVu_ & LVd=LVd_ & LHl=LHl_ & LHr=LHr_     & FQ=FQ_     & PHI=PHI_
;	DXT=DXT_ & DYT=DYT_ & FCTX=FCTX_ & FCTY=FCTY_ & shape=shap & squar=squa
;WDIAG, /nw
;end

pro DEPLI,  area, ry1,ry2 ,arel,diam,xdiam,b_red, AV,AH
;** *****
;** INPUT
;** Image plate                 ->	area
;** Radius limits               ->	ry1 ,ry2	(pixels)
;** Known plate angles (else 0) ->	AV,AH		(0  ,0 )

;** OUTPUT
;** Pyramid & diagram & x coord.-> 	arel,diam,xdiam
;** Calculated plate angles     -> 	AV,AH

;** Vertical   decal	LVu ,LVd
;** Horizontal decal	LHl ,LHr
;** Vert. pixel resol.  PIXV
;** Hori. pixel resol.  PIXH
;** Distortion center   FCTX,FCTY
;** Distance sample(cm)	DIS
;** Center		cti ,ctj
;** Sector limits	a1  ,a2
;** Spacial distortion parameters FQ,PHI
;** Spacial distortion factors  DXT,DYT

common depli,cti,ctj,aa1,aa2,DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
common destop , wit
common dexxl  , xxl1,xxl2
common rad_tmp, i1,i2,it,j1,j2,jt,nz,r,ta,tt,ra1,ra2,ii1,ii2,ii3,ii4,$
                jj1,jj2,jj3,jj4,ki,kj
;**
;float jt,r,ra1,ra2
;
vsiz =size(area)
if vsiz(0) eq 2 then begin
	cx   =cti
	cy   =ctj
	ci   =float(cti-1)
	cj   =float(ctj-1)

;**	Allocate memory
;**	-------- ------
	r1   =ry1 & if r1 le 0  then r1= 1
	r2   =ry2 & if r2 lt r1 then r2=r1
	pii  =!pi
	dpi  =pii/2
	pf   =round (r2/2000 + 1)
	if n_elements(b_red) eq 1 then pf=pf*(b_red+1)
	if n_elements(AV)    ne 1 then AV=0
	if n_elements(AH)    ne 1 then AH=0
	sx   =long (pii*r2*2/pf)+ 3
	sx1  =sx-1
	sy   =round(r2-r1+1)
	sy1  =sy-1
	arel =fltarr(sx,sy)
	arec =fltarr(sx,sy)
	diam =fltarr(sy)
	xdiam=fltarr(sy)
	
	rad_6	 = 6.2832
	rad_57	 = 57.2956

	a1=aa1 & a2=aa2
	if ((a1 eq 0.)    and (a2 eq 360.)) or $
	   ((a1 eq 1.)    and (a2 eq 360.)) or $
	   ((a1 eq -180.) and (a2 eq 180.)) or $
	    (a1 eq a2)    then ta=0 else ta=1

;**	Calculate limit index
;**	--------- ----- -----
	ra1=a1*pii/180.
	ra2=a2*pii/180.
	csa1=cos(ra1)
	csa2=cos(ra2)
	sna1=sin(ra1)
	sna2=sin(ra2)
;
	ii1=ci+csa1*r1
	ii2=ci+csa1*r2
	ii3=ci+csa2*r1
	ii4=ci+csa2*r2
	same=1-ta & if a1 ge a2 then if (a1/90 eq a2/90) then same=1

	if ((sna1 gt 0) and (sna2 lt 0)) or (same eq 1) then i1 =long (ci-r2) $
					else i1 =long (min([ii1,ii2,ii3,ii4]))
	if ((sna1 lt 0) and (sna2 gt 0)) or (same eq 1) then i2 =round(ci+r2) $
					else i2 =round(max([ii1,ii2,ii3,ii4]))
	if i1 lt 0       then i1=long(0)
	if i2 ge vsiz(1) then i2=vsiz(1)-1
	i1=long(i1)
;
	jj1=cj-sna1*r1
	jj2=cj-sna1*r2
	jj3=cj-sna2*r1
	jj4=cj-sna2*r2
	if ((csa1 gt 0) and (csa2 lt 0)) or (same eq 1) then j1 =long (cj-r2) $
					else j1 =long (min([jj1,jj2,jj3,jj4]))
	if ((csa1 lt 0) and (csa2 gt 0)) or (same eq 1) then j2 =round(cj+r2) $
					else j2 =round(max([jj1,jj2,jj3,jj4]))

	if j1 lt 0       then j1=long(0)
	if j2 ge vsiz(2) then j2=vsiz(2)-1
	j1=long(j1)

;**	Initialise all correctors
;**	---------- --- ----------
	ra1=-0.01+a1
	ra2= 0.01+a2

;	Correctors for pixel resolution
;	********** *** ***** **********
	DPIX= 1+ ((PIXV/PIXH)-1)/2.
	DPIY= 1+ ((PIXH/PIXV)-1)/2.

;	Correctors for scanner spacial distortion
;	********** *** ******* ******* **********
	cpx  =(vsiz(1)-1)*FCTX			& cpy  =(vsiz(2)-1)*FCTY
	if (FCTX eq -1) and (FCTY eq -1) then begin cpx=ci & cpy=cj & endif
	pci  =ci  -cpx				& pcj  =cpy -  cj

	if pcj ne 0 then ang  =atan(pcj,pci) else ang=0.

	nci  =pci* (1+ DXT*cos(FQ* ang +PHI))	& ncj  =pcj* (1+ DYT*cos(FQ* ang +PHI))

	p_u  =cpy -(cj-LVu) 			& p_d  =cpy -((vsiz(2)-1) - (cj+LVd))
	p_l  =cpx -(ci-LHl) 			& p_r  =cpx -((vsiz(1)-1) - (ci+LHr))

	if p_u ne 0 then angu =atan (p_u,pci)	  else angu=0.
	if p_d ne 0 then angd =atan (p_d,pci)	  else angd=0.
	puj  =p_u* (1+ DYT*cos(FQ*angu +PHI))	& pdj  =p_d* (1+ DYT*cos(FQ*angd +PHI))
	if pcj ne 0 then angl =atan (pcj,p_l)     else angl=0.
	if pcj ne 0 then angr =atan (pcj,p_r)     else angr=0.
	pli  =p_l* (1+ DXT*cos(FQ*angl +PHI))	& pri  =p_r* (1+ DXT*cos(FQ*angr +PHI))
	
	IF LVu gt 0 then begin
	LVu  =sqrt ((LVu + p_u - puj)^2 + (pci-nci)^2)
	LVd  =sqrt ((LVd + p_d - pdj)^2 + (pci-nci)^2)
	LHl  =sqrt ((LHl + p_l - pli)^2 + (pcj-ncj)^2)
	LHr  =sqrt ((LHr + p_r - pri)^2 + (pcj-ncj)^2)
	endif else begin
	LVu=1. & LVd=1. & LHl=1. & LHr=1. & endelse

;	Correctors for plate angle
;	********** *** ***** *****
	P=1.        ;& if vsiz(1) gt 2000 then P=2.
	DISV= 10000.*DIS/PIXV/P			& DISH= 10000.*DIS/PIXH/P   & DISM=(DISV+DISH)/2
	if (AV eq 0) and (AH eq 0) then begin
	 DV  = float (LVd-LVu)			& DH  = float (LHr-LHl)
	 AV  = 1.5   & CSAV=0.07 & SNAV=0.99	& AH  = 1.5   & CSAH=0.07 & SNAH=0.99
	 SNAV= (DISV * DV) / (2  * LVu *LVd  )	& SNAH= (DISH * DH) / (2  * LHl *LHr  )
	 if (SNAV le 1)   and (SNAV ge -1) then   AV  = asin(SNAV)
	 if (SNAH le 1)   and (SNAH ge -1) then   AH  = asin(SNAH)
	endif else begin
	     SNAV=sin(av)
	     SNAH=sin(ah)
	endelse

	if (AV   lt 1.5) then CSAV=cos(AV)
	if (AH   lt 1.5) then CSAH=cos(AH)
	DCSV=  DISV * CSAV			& DCSH=  DISH * CSAH
;
;MAIN LOOP: PYRAMID CONSTRUCTION
;*********
	ci2=round(ci*2) & r2b=r2+.5
	ci1=ci-1
	ab =1
	mid=(sx)/2. -1
	ofx=cx-long(cx) & ofy=cy-long(cy)
	pox=cpx-ofx     & poy=cpy+ofy
	j3 =j2+sy1      & if shape eq 1 then j3=j3+sy1

	FOR j =j1,j2 DO BEGIN
	    pj= poy-j

	    for i=i1,i2 do begin
		pi = i-pox

;	        Scanner distortion
;		------- ----------
		if pj ne 0 then ang=atan(pj,pi) else ang=0.
		tcs=cos(FQ*ang+PHI)

		ki =pi * (1+ DXT*tcs) - nci
		kj =pj * (1+ DYT*tcs) - ncj

;	    	Horizontal,Vertical Correction angle
;		------------------- ---------- -----
	    	ki=DCSH * ki / (DISH+ki*SNAH)
		kj=DCSV * kj / (DISV-kj*SNAV)

;		Pixel size correction
;		----- ---- ----------
		ki=ki*DPIX
		kj=kj*DPIY
;		Shape detector correction (not used)
;		----- -------- ----------

;		Intensity  correction (not used)
;		---------  ----------
		r =sqrt(kj^2 + ki^2)
		
		it=round(r)
		if  (it ge r1)  then begin
		 if (it le r2b) then begin
		     if kj ne 0 then ang=atan(kj,ki) else ang=0.
		     if  ta  eq 1 then begin
		         if ang lt 0 then vtm= (rad_6 + ang)*rad_57 else vtm= ang*rad_57
		         if a2 ge a1 then begin
		           if (vtm ge ra1) and (vtm le ra2) then ab=1 else ab=0
		         endif else $
		           if (vtm ge ra1) or  (vtm le ra2) then ab=1 else ab=0
		     endif
		     if ab eq 1 then begin
		       val=area(i,j)
		       if val ge 0 then begin
			cir=abs(r-r1)
			if ang le dpi	then xir=(it*(ang+dpi    )/pf +mid) $
					else xir=(it*(ang-dpi-pii)/pf +mid)   
			xil=long(xir) & xid=(xil+1)<sx1
			cil=long(cir) & cid=(cil+1)<sy1

			fcx=1.-(xir-xil) & fcy=1.-(cir-cil)

			fc =fcx*fcy
			arel(xil,cil)=arel(xil,cil)+val*fc & arec(xil,cil)=arec(xil,cil)+fc
			fc =(1.-fcx)*(1.-fcy)
			arel(xid,cid)=arel(xid,cid)+val*fc & arec(xid,cid)=arec(xid,cid)+fc
			fc =(1.-fcx)*(fcy)
			arel(xid,cil)=arel(xid,cil)+val*fc & arec(xid,cil)=arec(xid,cil)+fc
			fc =(fcx)*(1.-fcy)
			arel(xil,cid)=arel(xil,cid)+val*fc & arec(xil,cid)=arec(xil,cid)+fc
		       endif
		     endif
		 endif  else if i ge ci  then i=i2+ 1 $
		 	else if i eq i1  then begin if kj ne 0 then ang=atan(kj,ki) else ang=0.
						    i=i1+ (long((r2-it)/cos(ang))-1)>0 & endif
		endif   else if i lt ci1 then i=ci2-i-1

	    endfor 
	    if n_elements(wit) ge 1 then $
	    if RDSTOP(j1,j3,(j),win=wit) then begin j=j2+1 & j3=0 & endif
	ENDFOR

;**	DIAGRAM
;**	*******
	arel =temporary(arel)/(arec>.0001)
	diam =reform (total ( (arec)/(arec>.0001),1))

;		         Pi*d       *             theta/360              /   pixelsize(mm)
;		ii= (!pi*DIS*2*10.) * (atan((i+r1)/DISM)*180/!pi) / 360. / ((PIXV+PIXH)/2./1000)
;		ii=      DIS*2*10.  *  atan((i+r1)/DISM)*180      / 360. /  (PIXV+PIXH)*2.*1000
;		ii=      DIS*2      *  atan((i+r1)/DISM)/2. /  (PIXV+PIXH)*2.*10000.
;		ii=      DIS*2      *  atan((i+r1)/DISM)    /  (PIXV+PIXH)   *10000.

	if shape eq 1 then begin
	   arc  =     DIS*2    / (PIXV+PIXH) *10000.
	   nip  =     arc      * (atan(     r1 /DISM)) 
	   ni   =     arc      * (atan((sy1+r1)/DISM)) - nip
	   fdiam= fltarr(sy)
	   endif

	arex=total(arec,2) & idx=where(arex gt 0)
	xxl1=idx(0)>0 & xxl2=idx(n_elements(idx)-1)>0

	sr1=pii*2./pf & sr2 =sr1*r2
	aret=arel(*,0)
	arev=aret
	mi1=long(mid) & mi2=round(mid)

	if j3 gt 0 then $
	for i =sy1 ,0,-1 do begin 
	    if squar ne 0 then  begin sr=sr2/(sr1*(i+r1)) & srd=(sr-1.)/2.
	                        aret=aret*0. & arev=arev*0. & endif
	    j=mi1
	    while (j gt 0) and (arec(j,i) eq 0) do j=j-1
	    while  j gt 0 do begin
	        if arec(j,i) eq 0 then if (arec(j-1,i) ne 0) and (arec(j+1,i) ne 0) $
	                          then begin arel(j,i)=(arel(j-1,i)+arel(j+1,i) )  / 2.
	                                     diam(i)=diam(i)+1
	                          endif
	        if squar ne 0 then begin
			nwi=mid-(mid-j)*sr
			nw1=round(nwi-srd)>xxl1 & nw2=round(nwi+srd)<xxl2
			for k=nw1,nw2 do begin aret(k)=aret(k)+arel(j,i)
			                       arev(k)=arev(k)+sr & endfor
	        endif
	        j=j-1
	    endwhile

	    j=mi2
	    while (j lt sx1) and (arec(j,i) eq 0) do j=j+1
	    while  j lt sx1 do begin
	        if arec(j,i) eq 0 then if (arec(j-1,i) ne 0) and (arec(j+1,i) ne 0) $
	                          then begin arel(j,i)=(arel(j-1,i)+arel(j+1,i) )  / 2.
	                                     diam(i)=diam(i)+1
	                          endif
	        if squar ne 0 then begin
			nwi=mid+(j-mid)*sr
			nw1=round(nwi-srd)>xxl1 & nw2=round(nwi+srd)<xxl2
			for k=nw1,nw2 do begin aret(k)=aret(k)+arel(j,i)
			                       arev(k)=arev(k)+sr & endfor
	        endif
	        j=j+1
	    endwhile

	    if diam(i) gt 0 then  diam(i)=total(arel(*,i))/diam(i)

	    if squar ne 0 then  begin aret=aret/(arev>1) & totl=total(arel(*,i))
	    			if squar eq 2 then aret=aret*sr
	    			arel(*,i)=aret & from=xxl1 & to=xxl2>xxl1
	                        idx=where(aret(from:to) eq 0)
	                        if idx(0) ge 0 then $
	                        for k=n_elements(idx)-1,0,-1 do begin kd=idx(k)+from
				    if kd gt from then ia=aret(kd-1) else ia=0
				    if kd lt to   then ib=aret(kd+1) else ib=0
				    if ia eq 0 then arel(kd,i)=ib else if ib ne 0 then arel(kd,i)=(ia+ib)/2. else arel(kd,i)=ia
	                            ;  print,'trou=',i,kd
	                        endfor
				if squar eq 1 then begin
					tott=total(arel(*,i)) & arel(*,i)=arel(*,i) * (totl/tott) & endif
	    		  endif

	   ;if squar ne 0 then begin idx=where(arev gt sr) & if idx(0) ge 0 then print,'coli=',i,sr,idx & endif

	    theta=atan((i+r1)/DISM)

	    if shape eq 1 then fdiam(i)= arc * theta -nip

;	    if diam(i) gt 0 then  diam(i)=total(arel(*,i))/diam(i)
	    xdiam(i)=theta
	    if n_elements(wit) ge 1 then $
	    if RDSTOP(j1,j3,(j2+sy1-i),win=wit) then begin i=i-sy1 & j3=0 & endif
	endfor

;	Shape detector correction
;	----- -------- ----------
	if j3    gt 0  then $
	if shape eq 1  then begin
	   arec =arec *0.
	   diat =diam *0.
	   xdiat=xdiam*0. & xdiot=xdiat
	   for i=0,sy1 do begin
		i1=long(fdiam(i)) & i2=(i1+1)<sy1 & b1=1.-(fdiam(i)-i1)
		                                    b2=1.-b1
		arec(*,i1)=arel (*,i)*b1 +  arec (*,i1)
		arec(*,i2)=arel (*,i)*b2 +  arec (*,i2)
		diat(  i1)=diam (  i)*b1 +  diat (  i1)
		diat(  i2)=diam (  i)*b2 +  diat (  i2)
		xdiat( i1)=xdiam(  i)*b1 +  xdiat(  i1) & xdiot(i1)=xdiot(i1)+b1
		xdiat( i2)=xdiam(  i)*b2 +  xdiat(  i2) & xdiot(i2)=xdiot(i2)+b2

	       if n_elements(wit) ge 1 then $
	       if RDSTOP(j1,j3,(j2+sy1+i),win=wit) then begin i=i+sy1 & j3=0 & endif
	   endfor
	   ni=long(ni)
	   arel=arec(*,1:ni) & diam=diat(1:ni) & xdiam=xdiat(1:ni)/(xdiot(1:ni)>.0001)
	   arec=0            & diat=0          & xdiat=0   & xdiot=0   & fdiam=0
	endif
;	-------------------------
	xdiam=xdiam*180/pii * 2.
	if pf ne 1 then arel=temporary(arel)*pf
	if squar ne 0 then begin arex=total(arel,2) & idx=where(arex gt 0)
				 xxl1=idx(0)>0 & xxl2=idx(n_elements(idx)-1)>0 & endif
endif
;
return
end

pro wdiag, NW=nw
;** *****
;**
@lamp.cbk
common cw_diag, b_rol, b_win, b_t, b_v, b_s, b_new, b_wl, b_ww, b_arel, b_diam, b_xdia,b_p,$
                b_area,b_areu,b_show  , b_z, b_on1, b_on2,b_on3,b_red , b_chg , b_f1, b_f2
common depli  , cti,ctj, a1,a2, DIS,PIXV,PIXH, LVu,LVd,LHl,LHr, FQ,PHI,DXT,DYT ,FCTX,FCTY, shape,squar
common destop , wit

if keyword_set(nw) then return

i=xregistered('Unroll')
if i le 0 then begin

 if n_elements(b_rol) eq 0 then begin             ;** SET FIXED PARAMETERS
 ;** Position of beam center                   -> centx,centy [first value in the file is at (1,1)]
 ;** For a ring: constated radius at angle  90 -> rVu	[0]
 ;               constated radius at angle -90 -> rVd	[0]
 ;** For a ring: constated radius at angle   0 -> rHr	[0]
 ;               constated radius at angle 180 -> rVl	[0]
 ;** Vertical    pixel resolution (micron)     -> pixsv  [150 or 75]
 ;** Horizontal  pixel resolution (micron)     -> pixsh  [150 or 75]
 ;** Horizontal  distortion center factor      -> tcx    [ .5]
 ;** Vertical    distortion center factor      -> tcy    [ .5]

	b_win  =lonarr(8) & b_t=lonarr(23) & b_v=float(b_t)
	b_v(2) =100       & b_v(3) =150    & b_v(4) =150
	b_v(5) =1         & b_v(6) =10     & b_v(7) =0        & b_v(8) =360
	b_v(9) =4         & b_v(10)=0.5    & b_v(11)=0.5
	b_s    =strtrim(string(b_v),2)
	b_wl   ='1'       & b_ww   ='2'    & b_new  =1        & b_area=[[0,0],[0,0]]
	b_areu =b_area    & b_show = 0     & b_z    =[512,30] & b_on1 =0 & b_on2=0
	b_on3  =0         & b_chg  =b_on3  & b_red  =0        & shape =0 & squar=0
 endif
 
 if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0
 b_rol=widget_base  (title='Unrolling...',/row,group_leader=lamp_b1,resource_name="lamp")
 sw=b_z(0) & sh=b_z(1)
 b_l0 =widget_base  (b_rol,/column)
 b_l  =widget_base  (b_l0 ,/row)
 b_r  =widget_base  (b_rol,/column)
 b_l1 =widget_base  (b_l  ,/column)
     bmp0    =widget_base(b_l1,map=0)
     bmap    =widget_draw(bmp0,xsize=sh,ysize=sh)
     bmap    =widget_base(b_l1)
     b_win(0)=widget_draw(bmap,xsize=sh,ysize=sw)
     bmp1    =widget_base(b_l1,map=0)
     bmap    =widget_draw(bmp1,xsize=sh,ysize=sh)
 b_l2 =widget_base  (b_l  ,/column)
     bmap    =widget_base(b_l2)
     b_win(1)=widget_draw(bmap,xsize=sw,ysize=sh)
     bmap    =widget_base(b_l2)
     b_win(2)=widget_draw(bmap,xsize=sw,ysize=sw,/button_events,uvalue=[-88,378,11])
     bmap    =widget_base(b_l2)
     b_win(3)=widget_draw(bmap,xsize=sw,ysize=sh)
 b_l3 =widget_base  (b_l  ,/column)
     bmp2    =widget_base(b_l3,map=0)
     bmap    =widget_draw(bmp2,xsize=sh,ysize=sh)
     bmap    =widget_base(b_l3)
     b_win(4)=widget_draw(bmap,xsize=sh,ysize=sw)
     bmp3    =widget_base(b_l3,map=0)
     bmap    =widget_draw(bmp3,xsize=sh,ysize=sh)

 if lamp_siz gt 750 then b_win(5) =widget_draw(b_l0,xsize=595,ysize=160,/button_events,uvalue=[-88,378,12]) $
 else begin bil     =widget_base(title="Diagram",group_leader=b_rol)
            b_win(5)=widget_draw(bil,xsize=595,ysize=160,/button_events,uvalue=[-88,378,12])
            widget_control,bil,/realize & endelse

 b_r0 =widget_base  (b_r  ,/row)
	b_r0l=widget_button(b_r0  ,font=ft_smaller ,value='<-')
	b_r0m=widget_button(b_r0  ,font=ft_propor  ,value='load W ' +b_wl)
	b_r0r=widget_button(b_r0  ,font=ft_smaller,value='->')
	widget_control,bad_id=i,b_r0l,set_uvalue=[-88,310  ,b_r0m,1]
	widget_control,bad_id=i,b_r0m,set_uvalue=[-88,378,1,b_r0m  ]
	widget_control,bad_id=i,b_r0r,set_uvalue=[-88,311  ,b_r0m,1]

	b_don=widget_button(b_r0  ,font=ft_b_normal,value="Done"         ,uvalue=[-88,378,9])

	b_r9l=widget_button(b_r0  ,font=ft_smaller ,value='<-')
	b_r9m=widget_button(b_r0  ,font=ft_propor  ,value='write W '+b_ww,uvalue=[-88,378,2])
	b_r9r=widget_button(b_r0  ,font=ft_smaller,value='->')
	widget_control,bad_id=i,b_r9l,set_uvalue=[-88,310,b_r9m,1]
	widget_control,bad_id=i,b_r9r,set_uvalue=[-88,311,b_r9m,1]

 b_r1 =widget_base  (b_r  ,/column,/frame,resource_name="mic")
     blab    =widget_label (widget_base(b_r1,/row),value="CONSTANTS",font=ft_b_normal)
     bid     =widget_base  (b_r1 ,/row)
     blab    =widget_label (bid  ,value="Cx"                    ,font=ft_propor)
     b_t(0)  =widget_text  (bid  ,value=b_s(0),xsize=7+cap,/editable,uvalue=[-88,378,3],font=ft_propor)
     blab    =widget_label (bid  ,value="Cy"                    ,font=ft_propor)
     b_t(1)  =widget_text  (bid  ,value=b_s(1),xsize=7+cap,/editable,uvalue=[-88,378,3],font=ft_propor)
     blab    =widget_label (bid  ,value="Center in pixel"       ,font=ft_b_normal)
     bid     =widget_base  (b_r1 ,/row)
     blab    =widget_label (bid  ,value="Sd"                    ,font=ft_propor)
     b_t(2)  =widget_text  (bid  ,value=b_s(2),xsize=7+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Sample to Detector distance in Cm.",font=ft_b_normal)
     bid     =widget_base  (b_r1 ,/row)
     blab    =widget_label (bid  ,value="Px"                    ,font=ft_propor)
     b_t(3)  =widget_text  (bid  ,value=b_s(3),xsize=7+cap,/editable,uvalue=[-88,378,1,b_r0m],font=ft_propor)
     blab    =widget_label (bid  ,value="Py"                    ,font=ft_propor)
     b_t(4)  =widget_text  (bid  ,value=b_s(4),xsize=7+cap,/editable,uvalue=[-88,378,1,b_r0m],font=ft_propor)
     blab    =widget_label (bid  ,value="Pixel size in micron"  ,font=ft_b_normal)

 b_r2 =widget_base  (b_r  ,/column,/frame,resource_name="did")
     blab    =widget_label (widget_base(b_r2,/row),value="REPRESENTATION",font=ft_b_normal)
     bid     =widget_base  (b_r2 ,/row)
     blab    =widget_label (bid  ,value="Ra"                    ,font=ft_propor)
     b_t(5)  =widget_text  (bid  ,value=b_s(5),xsize=7+cap,/editable,uvalue=[-88,378,3],font=ft_propor)
     blab    =widget_label (bid  ,value="Rb"                    ,font=ft_propor)
     b_t(6)  =widget_text  (bid  ,value=b_s(6),xsize=7+cap,/editable,uvalue=[-88,378,3],font=ft_propor)
     blab    =widget_label (bid  ,value="in,out Radius in pixel",font=ft_b_normal)
     bid     =widget_base  (b_r2 ,/row)
     blab    =widget_label (bid  ,value="Sa"                    ,font=ft_propor)    ;    270
     b_t(7)  =widget_text  (bid  ,value=b_s(7),xsize=7+cap,/editable,font=ft_propor);180     0,360
     blab    =widget_label (bid  ,value="Sb"                    ,font=ft_propor)    ;     90
     b_t(8)  =widget_text  (bid  ,value=b_s(8),xsize=7+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="deg. Sector"           ,font=ft_b_normal)
     widget_control,b_t(7),sensitive =0,set_value='0.'
     widget_control,b_t(8),sensitive =0,set_value='360.'
     bido   =widget_base   (b_r2 ,/row)
     bid     =widget_button(bido ,value="input data"    ,font=ft_b_normal,uvalue=[-88,378,3])
     bid     =widget_button(bido ,value="diagram"       ,font=ft_b_normal,uvalue=[-88,378,4])
     blab    =widget_label (bido ,value="   ",font=ft_b_normal) & put_logo,bido
     lilu    =widget_base  (bido,/nonexclusive,/row)
     bidu    =widget_button(lilu ,value="log"    ,uvalue=[-88,378,8] ,font=ft_b_normal)
     bid     =widget_button(lilu ,value="reduce" ,uvalue=[-88,378,10],font=ft_b_normal)
     bido    =widget_base  (b_r2 ,/row)
     bid     =widget_button(bido ,value="show unrolled rings",font=ft_b_normal,uvalue=[-88,378,5])
     blab    =widget_label (bido ,value="   ",font=ft_b_normal)
     lilu    =widget_base  (bido,/exclusive,/row)
     bid1    =widget_button(lilu ,value="raw"     ,uvalue=[-88,378,14],font=ft_b_normal,/no_release)
     bid2    =widget_button(lilu ,value="dilated" ,uvalue=[-88,378,15],font=ft_b_normal,/no_release)
     bid3    =widget_button(lilu ,value="squared"  ,uvalue=[-88,378,16],font=ft_b_normal,/no_release) & bid123=[bid1,bid2,bid3]
     widget_control,bidu         ,set_button=b_on3
     widget_control,bid123(squar),set_button=1

 b_r3 =widget_base  (b_r  ,/column,/frame,resource_name="don")
     blab    =widget_label (widget_base(b_r3,/row),value="DETECTOR SHAPE",font=ft_b_normal)
     bid     =widget_base  (b_r3  ,/row,/exclusive)
     bidu    =widget_button(bid   ,value="sphere"               ,font=ft_b_normal,/no_release,uvalue=[-88,378,13,0])
     bid1    =widget_button(bid   ,value="plate"                ,font=ft_b_normal,/no_release,uvalue=[-88,378,13,1])
     bida    =widget_button(bid   ,value="y cylinder"           ,font=ft_b_normal,/no_release,uvalue=[-88,378,13,2])
     bidb    =widget_button(bid   ,value="x cyl."               ,font=ft_b_normal,/no_release,uvalue=[-88,378,13,3])
     widget_control,bidu,set_button=1
     widget_control,bida,sensitive =0
     widget_control,bidb,sensitive =0
 b_r4 =widget_base  (b_r  ,/column,/frame)
     bid     =widget_base  (b_r4 ,/row)
     blab    =widget_label (bid  ,value="SCANNER DISTORTION",font=ft_b_normal)
     bid1    =widget_button(widget_base(bid,/nonexclusive),value="on/off",font=ft_b_normal,$
                                                          uvalue=[-88,378,6])
     blab    =widget_button(bid  ,value="!",font=ft_smaller)
     b_r44   =widget_base  (b_r4 ,/column,resource_name="ben")
     bid     =widget_base  (b_r44,/row)
     blab    =widget_label (bid  ,value="Tx"                    ,font=ft_propor)
     b_t(10) =widget_text  (bid  ,value=b_s(10),xsize=5+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Ty"                    ,font=ft_propor)
     b_t(11) =widget_text  (bid  ,value=b_s(11),xsize=5+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Frequ"                 ,font=ft_propor)
     b_t(9)  =widget_text  (bid  ,value=b_s(9) ,xsize=3+cap,/editable,font=ft_propor)
     bid     =widget_base  (b_r44,/row)
     blab    =widget_label (bid  ,value="Fx"                    ,font=ft_propor)
     b_t(12) =widget_text  (bid  ,value=b_s(12),xsize=5+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Fy"                    ,font=ft_propor)
     b_t(13) =widget_text  (bid  ,value=b_s(13),xsize=5+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Phase"                 ,font=ft_propor)
     b_t(14) =widget_text  (bid  ,value=b_s(14),xsize=3+cap,/editable,font=ft_propor)
     widget_control,b_r44,map=0

 b_r5 =widget_base  (b_r  ,/column,/frame)
     bid     =widget_base  (b_r5 ,/row)
     blab    =widget_label (bid  ,value="PLATE DELTA ANGLE",font=ft_b_normal)
     bid1    =widget_button(widget_base(bid,/nonexclusive),value="on/off",font=ft_b_normal,$
                                                          uvalue=[-88,378,7])
     blab    =widget_button(bid  ,value="!",font=ft_smaller)
     b_r55   =widget_base  (b_r5 ,/column,resource_name="ben")
     bid     =widget_base  (b_r55,/row)
     blab    =widget_label (bid  ,value="Av"                    ,font=ft_propor)
     b_t(15) =widget_text  (bid  ,value=b_s(15),xsize=4+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Ah"                    ,font=ft_propor)
     b_t(16) =widget_text  (bid  ,value=b_s(16),xsize=4+cap,/editable,font=ft_propor)
     bid     =widget_base  (b_r55,/row)
     blab    =widget_label (bid  ,value="Lr"                    ,font=ft_propor)
     b_t(17) =widget_text  (bid  ,value=b_s(17),xsize=4+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Rr"                    ,font=ft_propor)
     b_t(18) =widget_text  (bid  ,value=b_s(18),xsize=4+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Ur"                    ,font=ft_propor)
     b_t(19) =widget_text  (bid  ,value=b_s(19),xsize=4+cap,/editable,font=ft_propor)
     blab    =widget_label (bid  ,value="Dr"                    ,font=ft_propor)
     b_t(20) =widget_text  (bid  ,value=b_s(20),xsize=4+cap,/editable,font=ft_propor)
     widget_control,b_r55,map=0
 
 b_t(21)=b_r44 & b_t(22)=b_r55
 widget_control,b_rol,/realize & put_logo
 wsdiag
 b_win(6)=b_win(2)
 b_win(7)=b_win(5)
 for  i=0,4       do   begin widget_control,b_win(i),get_value=j & b_win(i)=j & endfor
 if b_win(5) gt 0 then begin widget_control,b_win(5),get_value=j & b_win(5)=j & endif

 wit=[b_win(7),b_win(5),592,160]

 Xmanager,'Unroll',b_rol,event_handler='LAMP_EVENT_PARSER',/just_reg

endif else widget_control,bad_id=i,b_rol,map=1
end
