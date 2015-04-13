function rec_tom, data,angles ,DISTANCE=dist ,OUT_SIN=dataf ,TRACE=widg  ,CENTER=cent,$
                               PIXSIZE=pxsz  ,STRIPES =stri ,FILTER=filt ,FILPAR=filp,$
                               DIVERGENCE=divg ,MODCENTER=mcen, AIR=air  ,FILSIZ=filk
;******* *******
;**
sw=SIZE(data) & sa=SIZE(angles) & sz=1 & sx=sw(1) & sy=sw(2) & trc=0 & sxv=sx
wout=0
if (sw(0) lt 2) or (sw(0) gt 3) then begin print,'Data dimensions incorrect !!!'  & return,0 & endif
if (sy ne sa(1))                then begin print,'Angle dimension incorrect !!!'  & return,0 & endif
if (n_elements(widg) eq 6)      then if widg(0)*widg(1)*widg(3)*widg(4) gt 0 then trc=1
if (n_elements(stri) ne 1)      then stri=0
if (n_elements(cent) ne 1)      then cent=0
if (n_elements(filt) ne 1)      then filt=0
if (n_elements(dist) ne 1)      then dist=0
if (n_elements(pxsz) ne 1)      then pxsz=0
if (n_elements(divg) ne 1)      then divg=0
if (n_elements(air)  ne 1)      then air =0
if (n_elements(filp) ne 1)      then filp=1.
if (n_elements(filk) ne 1)      then filk=32
if (sw(0) eq 3)                 then begin sz=sw(3) & wout=fltarr(sx,sy,sz) & endif

Cx   =sx/2 & if Cx*2 eq sx then Cx=Cx-0.5 & kpCX=Cx
if mcen eq 3 then Cx=cent  else cent=Cx
Anglr=angles*!Pi/180.
Rei  =fltarr (sx,sx)
rho  =findgen(sx)

if dist*pxsz*divg ne 0   then begin divb=1
				 np=rho-Cx
				 db=ATAN(np*pxsz/dist/10000.)*180./!Pi
			 endif else divb=0

if mcen eq 1  then begin ling=rho+1
                         grav=fltarr (sy) & weight=fltarr(sy)+1.   & endif
if filt gt 0  then begin
                         filk=filk>3<(sx/3) & half = fix(filk)/2   & zfil=2*half+1 
                         x  = findgen(zfil) - half & filp=filp>0.1 & filter=[1.0]
                         x(half) = .01
                         tmp= fltarr (sx+2*zfil)                   & endif

FOR j=0,sz-1  do  begin
   ;FILTERS
   ;*******
   	dataf=fltarr(sx,sy)
	if sz   gt 1 then dataf(0,0)=data(*,*,j) else dataf(0,0)=data

	;LINEARITY SLICES
	;********* ******
	if air gt 0 then begin
		tat =reform(total(dataf(0:air-1,*),1)+total(dataf(sx-air:sx-1,*),1))/(2.*air)
		avg =tat/(total(tat)/sy)
		FOR i=0,sy-1 DO dataf(0,i)=dataf(*,i)/avg(i)
	endif

	;LINEARITY DETECTOR
	;********* ********
	if air gt 0  then begin
		totL = total(dataf(0:air-1    ,*)) / (sy*air)
		totR = total(dataf(sx-air:sx-1,*)) / (sy*air)
		incr =(totR-totL)/(sx-air)
		airN = totL-incr*(air/2.) + rho*incr + 1
		airF = total(airN)/sx/airN
		FOR i=0,sy-1 do dataf(0,i)=dataf(*,i)*airF
	endif

	;STRIPES
	;*******
	if stri ge 3 then begin
		tot =total(dataf,2)/sy & if !Version.release lt '4.1' then edg='' else edg=',/edge'
		ii  =execute('smo=smooth(tot,stri'+edg+')') & dif=(tot-smo)>0
		for i=0,sy-1 do dataf(0,i)=dataf(*,i)-dif
	endif

	;CENTER
	;******
	if mcen eq 1 then begin
		FOR i=0,sy-1 do begin temp=dataf(*,i)
			grav(i) = total(temp*ling)/total(temp)
		ENDFOR
		mxx=max(grav,min=mii)
		A=[Cx,(mxx-mii)/2.,0.] ;estimate center,amplitude,phase
		fit=curvefit(Anglr, grav, weight, A, sigm, function_name='wavoid')
		Cx = a(0)-1.5 & Cent=Cx
	endif
		kpCX=fix(kpCX-Cx)

	;DIVERGENCE
	;**********
	if divb ne 0 then begin
		for i=0,sx-1 do dataf(i,*)=INTERPOL(reform(dataf(i,*)),angles,angles-db(i))
	endif

	;CONVOLUTION
	;***********
	if filt gt 0 then begin
	   if filt eq 1 then begin a=0.5
		filtrA=-sin(!pi*x/2)^2 / (!pi^2 * x^2 * filp)           ; *** Gen-Hamming ***
		filtrA(half)  = 1./(4.*filp)                            ; *** Gen-Hamming ***
		filtrB=-sin(!pi*(x-1.)/2)^2 / (!pi^2 * (x-1.)^2 * filp) ; *** Gen-Hamming ***
		filtrB(half+1)= 1./(4.*filp)                            ; *** Gen-Hamming ***
		filtrC=-sin(!pi*(x+1.)/2)^2 / (!pi^2 * (x+1.)^2 * filp) ; *** Gen-Hamming ***
		filtrC(half-1)= 1./(4.*filp)                            ; *** Gen-Hamming ***
		filter= a * filtrA + ((1.-a)/2) * (filtrB+filtrC)       ; *** Gen-Hamming ***
	   endif
	   if filt eq 2 then begin
		d = !pi^2 * filp * (1.-4.*x^2)                          ; *** Shepp_Logan ***
		filter=2./d                                             ; *** Shepp_Logan ***
	   endif
	   if filt eq 3 then begin
		filter=-sin(!pi*x/2)^2 / (!pi^2 * x^2 * filp)           ; ***    Ramlak   ***
		filter(half)  = 1./(4.*filp)                            ; ***    Ramlak   ***
	   endif
	   if filt eq 4 then begin
		filtrA=-sin(!pi*(x-.5)/2)^2 / (!pi^2 * (x-.5)^2 * filp) ; ***  lp-cosine  ***
		filtrB=-sin(!pi*(x+.5)/2)^2 / (!pi^2 * (x+.5)^2 * filp) ; ***  lp-cosine  ***
		filter=0.5*(filtrA+filtrB)                              ; ***  lp-cosine  ***
	   endif
	   FOR i=0, sy-1 do begin
		tmp(0 :zfil-1) = dataf(0,i)
		tmp   (zfil)   = dataf(*,i)
		tmp(sx+zfil-1 :sx+2*zfil-1) = dataf(sx-1,i)
		tmp = convol(tmp,filter)
		dataf(0,i) = tmp(zfil : zfil+sx-1)
	   ENDFOR
	endif

   ;SHOW SINOGRAM
   ;**** ********
	if trc then begin Vei=dataf
			if air gt 0 then begin mxx=max(Vei,min=mii) & mxi=mii+(mxx-mii)/3.
				Vei(0:air-1    ,*)=Vei(0:air-1    ,*) + mxi
				Vei(sx-air:sx-1,*)=Vei(sx-air:sx-1,*) + mxi & endif
			wset,widg(0) & tvscl,congrid(Vei,widg(3),widg(4)) & endif

   ;RECONSTRUCTION
   ;**************
	if sys_dep('VERSION') ge 5.4 then begin rho=rho-cx
	     ii=execute('Rei=RADON (transpose(dataf),theta=anglr,/BACKPROJECT,nx=sx,ny=sx,/LINEAR,rho=rho)')
	endif else $
	if sys_dep('VERSION') lt 4.0 then begin if kpCX ne 0 then dataf=shift(dataf,kpCX)
	   for i=0,sy-1 do RIEMANN, dataf, Rei, anglr(i), /BACKPROJECT, ROW=i, /BILINEAR
	endif else $
	   for i=0,sy-1 do RIEMANN, dataf, Rei, anglr(i), /BACKPROJECT, ROW=i, /BILINEAR, CENTER=Cx

   ;AFTER FILTER
   ;**** *******
	if filt gt 0 then begin
	endif
	if sz gt 1  then  wout(0,0,j)=Rei else wout=Rei

   ;SHOW RESULT
   ;**** ******
	if trc then begin wset,widg(1) & tvscl,congrid(Rei ,widg(3),widg(3))
			if (sz eq 1) and (widg(5) gt 0) then jj=widg(5) else jj=j
			widget_control,widg(2),bad_id=ii,set_value=jj & endif
ENDFOR
return,wout
end

pro wavoid, xx, a, fy, pder
;** ******
;** by Mark Rivers.(Chicago)
;** a(0) = rotation center  ; a(1) = amplitude  ; a(2) = phase
	fy = a(0) + a(1)*sin(xx + a(2))
	pder = fltarr(n_elements(xx), n_elements(a))
	pder(*,0) = 1.
	pder(*,1) = sin(xx + a(2))
	pder(*,2) = a(1)*cos(xx + a(2))
end

function strst,st & return,strtrim(string(st),2) & end
;******* *****

function Wuval,uv1, FIX=fix
;******* *****
val=0.
	widget_control,bad_id=ii, uv1, get_value = sval
	on_ioerror,misval & val=float(sval(0))   & misval:
	if keyword_set(fix) then  val=fix(val)
	widget_control,bad_id=ii, uv1, set_value = strst(val)
return, val
end

pro tomo_event_parser, ev,uv
;** *****************
;**
@lamp.cbk
common cw_tomo, b_tom, b_sinr, b_slir, b_sinf, b_rcon, b_slic, b_siz , b_all ,$
                w_idx, w_ang,  w_numr, w_numf, w_numc, w_sinr, w_sinf, w_rcon, w_all, $
                f_dis, f_psiz, f_cent, f_stri, f_filt, f_filp, f_air , f_linr, f_trans, $
                f_div, f_stp , f_logc, f_filk, f_fils

case uv(2) of
1:	begin	kp_w=!D.window & wset ,b_rcon & erase
		if uv(4) ge 0 then begin
		  widget_control,uv(3),get_value=wnum  & wnum=wnum(0)        ;Load Sinogram
		  i =strpos (wnum,'W') & w_numr=strtrim(strmid(wnum,i+1,4),2);*************
		  ii=execute('w_sinr=float(w'+w_numr+')') & sw=SIZE(w_sinr)
		  ii=execute('w_ang =float(y'+w_numr+')') & sy=SIZE(w_ang)
		endif else begin
		  n =101L & m =101L & nv=101 & k =ceil(sqrt(n^2+m^2))        ;Load a square test
		  w_rcon = FLTARR(N, M) & w_rcon(N/2:N/2+25,M/2:M/2+25)= 10. ;******************
		  w_rcon(N/2-15:N/2-10, M/2-15:M/2-10)=11.
		  w_sinr = FLTARR(K, nv)       & sw=SIZE(w_sinr)
		  w_ang  = findgen(nv)*180/nv  & sy=SIZE(w_ang)
		  r_rand = randomu(s,nv) +0.5
		  r_linr = findgen(k)/(k-1) +0.5
		  
		  if sys_dep('VERSION') ge 5.4 then begin    w_ang=w_ang*!Pi/180.
	             ii=execute('w_sinr=RADON (w_rcon, nrho=K, theta=w_ang, /LINEAR)')
		         w_sinr=transpose(w_sinr)   &        w_ang=w_ang/!Pi*180.
		  endif else $
		  FOR I=0, nv-1   DO  RIEMANN, w_sinr, w_rcon, w_ang(i)*!Pi/180., ROW=i

		  w_sinr=shift(w_sinr,5,0) +1.
		  FOR I=0, k-1,10 DO w_sinr(i,*)  =w_sinr(i,*)+50.
		  FOR I=0, nv-1   DO w_sinr(0,i)  =w_sinr(*,i)*r_linr
		  FOR I=0, nv-1   DO w_sinr(0,i)  =w_sinr(*,i)*r_rand(i)
		  tvscl,congrid(w_rcon,b_siz(0),b_siz(0))
		endelse
		if (sw(0) lt 2) or (sw(0) gt 3) then begin w_sinr=dist(32) & sw=SIZE(w_sinr) & endif
		if (sw(2) ne sy(1)) then w_ang=findgen(sw(2))/(sw(2)-1)*180.
		if (sw(0) eq 3)     then begin w_idx=w_idx<(sw(3)-1)
				widget_control,bad_id=ii,b_slir,set_slider_max=sw(3)-1
				widget_control,bad_id=ii,b_slic,set_slider_max=sw(3)-1
				widget_control,bad_id=ii,b_slir,set_value=w_idx,sensitive=1
				widget_control,bad_id=ii,b_slic,set_value=w_idx,sensitive=1
				widget_control,bad_id=ii,b_all ,                sensitive=1
				w_sinf=w_sinr(*,*,w_idx) & w_rcon=fltarr(sw(1),sw(2),sw(3))
		endif	else	begin  w_idx=0
				widget_control,bad_id=ii,b_slir,sensitive=0
				widget_control,bad_id=ii,b_slic,sensitive=0
				widget_control,bad_id=ii,b_all ,sensitive=0
				w_sinf=w_sinr            & if uv(4) ge 0 then w_rcon=fltarr(sw(1),sw(2))
				endelse
		wset ,b_sinr
		tvscl,congrid(w_sinr(*,*,w_idx),b_siz(0),b_siz(1))
		if kp_w gt 0 then wset ,kp_w
	end
2:	begin
		widget_control,uv(3),get_value=wnum  & wnum=wnum(0)      ;Write Reconstruction
		i=strpos(wnum,'W') & w_numc=strtrim(strmid(wnum,i+1,4),2);********************
		XICUTER,'w'+w_numc+'=get_tom(w'+w_numr+',/back_pro)'
	end
3:	begin
		widget_control,uv(3),get_value=wnum  & wnum=wnum(0)      ;Write Filtered Signal
		i=strpos(wnum,'W') & w_numf=strtrim(strmid(wnum,i+1,4),2);*********************
		XICUTER,'w'+w_numf+'=get_tom(w'+w_numr+',/out_sin)'
	end
4:	begin	f_dis = Wuval(uv(3))                                     ;Do the work ......
		                                                         ;******************
		f_psiz= Wuval(uv(4))

		f_stp = Wuval(uv(5),/fix) & if f_stri then f_ker=f_stp else f_ker=0

		r_cent= Wuval(uv(6))

		f_air = Wuval(uv(7),/fix) & if f_linr then f_are=f_air else f_are=0

		f_filk= Wuval(uv(8),/fix)

		f_filp= Wuval(uv(9))

		kp_w=!D.window    & trace=[b_sinf,b_rcon,b_slic,b_siz,w_idx]

		if ((SIZE(w_sinr))(0) eq 3) and (not w_all) $
		 then w_rcon(w_idx)=REC_TOM(w_sinr(*,*,w_idx),w_ang, DIV=f_div, TRACE=trace, OUT=w_sinf,$
		                      CENTER =r_cent, STRIP=f_ker, FILTER=f_filt, FILPAR=f_filp,FILSIZ=f_filk ,$
					    PIXSIZE=f_psiz, DISTANCE=f_dis, MODCENTER=f_cent, AIR=f_are)      $
		 else w_rcon       =REC_TOM(w_sinr           ,w_ang, DIV=f_div, TRACE=trace, OUT=w_sinf,$
		                      CENTER =r_cent, STRIP=f_ker, FILTER=f_filt, FILPAR=f_filp,FILSIZ=f_filk ,$
					    PIXSIZE=f_psiz, DISTANCE=f_dis, MODCENTER=f_cent, AIR=f_are)
					    
		widget_control,uv(6),set_value=strst(r_cent)
		if kp_w gt 0 then wset ,kp_w
	end
5:	begin w_sinr=0 & w_sinf=0 & w_rcon=0 & widget_control,ev.top,/destroy
	end
6:	begin w_all=ev.select                                          ;for all
	end                                                            ;*******
7:	begin widget_control,b_slir,get_value=w_idx                    ;sinogram index
		kp_w=!D.window  & wset ,b_sinr                         ;**************
		tvscl,congrid(w_sinr(*,*,w_idx),b_siz(0),b_siz(1))
		if kp_w gt 0 then wset ,kp_w
	end
8:	begin widget_control,b_slic,get_value=rc_idx                   ;reconstruction index
		kp_w=!D.window  & wset ,b_rcon                         ;********************
		tvscl,congrid(w_rcon(*,*,rc_idx),b_siz(0),b_siz(0))
		if kp_w gt 0 then wset ,kp_w
	end
9:	begin f_cent=uv(3)                                             ;for center
	end                                                            ;**********
10:	begin f_stri=ev.select                                         ;for stripes
	end                                                            ;***********
11:	begin f_filt=uv(3)                                             ;for filter
	      widget_control,ev.id,get_value=sval                      ;**********
	      widget_control,uv(4),set_value=sval
	end
12:	begin f_div =ev.select                                         ;for divergence
	end                                                            ;**************
13:	begin f_linr=ev.select                                         ;for linearity
	      widget_control,uv(3),sensitive=f_linr                    ;*************
	end
14:	begin f_log =ev.select                                         ;for log convertion
	end                                                            ;******************
else:
endcase
end

function get_tom, W,  out_sin=out_sin  ,back_pro=back_pro
;******* *******
;**
@lamp.cbk
common cw_tomo
	s=SIZE(W)
	if keyword_set(back_pro) then begin id=fix(w_numc)
		other_tit(id)='BACK PROJECTION'
		  x_tit(id)  ='X direction (angle '+string(w_ang(0))
		  y_tit(id)  ='Y'
		ii=execute('x'+w_numc+'=indgen(s(1))+1')
		ii=execute('y'+w_numc+'=indgen(s(2))+1')
		return,w_rcon
	endif else begin                    id=fix(w_numf)
		other_tit(id)='FILTERED SINOGRAM'
		ii=execute('x'+w_numf+'=indgen(s(1))+1')
		ii=execute('y'+w_numf+'=w_ang')
		return,w_sinf
	endelse
end

pro tomography, NW=nw
;** **********
;**
@lamp.cbk
common cw_tomo

if keyword_set(nw) then return

i=xregistered('Tomogra')
if i le 0 then begin

 if n_elements(b_tom) eq 0 then begin             ;** SET FIXED PARAMETERS
	w_idx =0  & w_rcon=0 & w_numr='1'  & w_numf='3' & w_numc='2' & b_siz=[256,360]
	w_sinr=dist(32) &  w_sinf=w_sinr  & w_rcon=findgen(32,32)   & w_ang=findgen(32)/31*180.
	w_all =0  & f_dis=720. & f_cent=0 & f_stri=0 & f_filt=0 & f_filp=1. & f_air=12 & f_linr=0
	f_trans=0 & f_psiz=360 & f_div =0 & f_stp=9  & f_logc=0 & f_filk=40
	f_fils=[" no-filter ","Gen-Hamming","Shepp-Logan","Ramlak","LP_Cosine"]
 endif
 if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0
 b_tom =widget_base  (title='Tomography... V1.02',/row,group_leader=lamp_b1,resource_name="lamp")
 bas1  =widget_base  (b_tom ,/column)
 bas2  =widget_base  (b_tom ,/column)
 bas3  =widget_base  (b_tom ,/column)
 basx  =[bas1,bas2,bas3]
 labx  =["Sinogram","Re-construct","Filtered Signal",$
         "load W "+w_numr,"write W "+w_numc,"write W "+w_numf]
 For j =0,2 do begin
	b_r0  =widget_base (basx(j),/row)
	lab  =Widget_label (b_r0   ,font=ft_b_normal,value=labx(j))
	b_r0l=widget_button(b_r0   ,font=ft_smaller ,value='<-')
	b_r0m=widget_button(b_r0   ,font=ft_propor  ,value=labx(j+3))
	b_r0r=widget_button(b_r0   ,font=ft_smaller,value='->')
	widget_control,b_r0l,set_uvalue=[-88,310    ,b_r0m,1]
	widget_control,b_r0m,set_uvalue=[-88,359,j+1,b_r0m,0]
	widget_control,b_r0r,set_uvalue=[-88,311    ,b_r0m,1]
 ENDFOR
 b_sinr=widget_draw  (bas1  ,xsize=b_siz(0) ,ysize=b_siz(1))
 b_slir=widget_slider(bas1  ,xsize=b_siz(0) ,Font=ft_b_normal,title=''  ,uvalue=[-88,359,7])
 bid   =widget_label (bas1  ,value='Input has a background subtracted'      ,Font=ft_b_normal)
 bid   =widget_label (bas1  ,value='and is calibrated by the white signal.' ,Font=ft_b_normal)
  bido  =widget_base (bas1  ,/row)
  bid   =widget_label(bido  ,value='Source -> Detector (Cm)'           ,Font=ft_b_normal)
  b_dis =widget_text (bido  ,value=strst(f_dis) ,xsize=6+cap,/editable ,Font=ft_propor)
  bido  =widget_base (bas1  ,/row)
  bid   =widget_label(bido  ,value='Detect. pixel size (Micron)'       ,Font=ft_b_normal)
  b_psz =widget_text (bido  ,value=strst(f_psiz),xsize=6+cap,/editable ,Font=ft_propor)

  b_11 =widget_base  (bas1  ,/row) & put_logo,b_11
  bid  =widget_button(b_11  ,value='Test',Font=ft_b_normal,uvalue=[-88,359,1,0L,-1])
  bid  =widget_button(b_11  ,value='Done',Font=ft_b_normal,uvalue=[-88,359,5])

 b_rcon=widget_draw  (bas2  ,xsize=b_siz(0) ,ysize=b_siz(0))
 b_slic=widget_slider(bas2  ,xsize=b_siz(0) ,Font=ft_b_normal,title=''  ,uvalue=[-88,359,8])
 bas22 =widget_base  (bas2  ,/column,frame=2)
  lab  =widget_label (bas22 ,value='... Filters ...' ,Font=ft_b_bigger)
  b_21 =widget_base  (bas22 ,/row)
  b_2b =widget_button(widget_base(b_21,/nonexclusive),value="linearity using air values->",Font=ft_b_normal,uvalue=[-88,359,13])
  b_air=widget_text  (b_21  ,value=strst(f_air),xsize=2+cap ,Font=ft_propor,/editable)
  widget_control,b_2b,set_button=f_linr ,set_uvalue=[-88,359,13,b_air]
  if not f_linr then widget_control,b_air,sensitive=0

  b_22 =widget_base  (bas22 ,/row,/nonexclusive)
   but0=widget_button(b_22  ,value='Intensities -log(transmit$/air)',Font=ft_b_normal,uvalue=[-88,359,14])
   widget_control,b_22,set_button=f_logc,sensitive=0
 
  b_23 =widget_base  (bas22 ,/row)
  b_231=widget_base  (b_23 ,/nonexclusive)
   but1=widget_button(b_231 ,value='remove Stripes. Kern='   ,Font=ft_b_normal  ,uvalue=[-88,359,10])
  b_stp=widget_text  (b_23  ,value=strst(f_stp),xsize=3+cap,Font=ft_propor,/editable)

  b_23 =widget_base  (bas22 ,/row)
   bid =widget_label (b_23  ,value='Cx'         ,Font=ft_b_normal)
  b_231=widget_base  (b_23  ,/row,/exclusive)
   bct1=widget_button(b_231 ,value='find'       ,Font=ft_b_normal  ,uvalue=[-88,359,9,1])
   bct2=widget_button(b_231 ,value='mid'        ,Font=ft_b_normal  ,uvalue=[-88,359,9,2])
   bct3=widget_button(b_231 ,value='set'        ,Font=ft_b_normal  ,uvalue=[-88,359,9,3])
  b_cen=widget_text  (b_23  ,value=' 0 ',xsize=4+cap,Font=ft_propor,/editable)
   if f_cent eq 1 then widget_control,bct1 ,set_button=1 else $
   if f_cent eq 3 then widget_control,bct3 ,set_button=1      $
                  else widget_control,bct2 ,set_button=1
 
  b_24 =widget_base  (bas22 ,/row,/nonexclusive)
  b_div=widget_button(b_24  ,value='divergent Beam'   ,Font=ft_b_normal  ,uvalue=[-88,359,12])

  b_25 =widget_base  (bas22 ,/row)
  b_25m=widget_button(b_25  ,value=f_fils(f_filt) ,Font=ft_b_normal  ,menu=2)
   FOR i=0,n_elements(f_fils)-1 do $
   bid =widget_button(b_25m ,value=f_fils(i)      ,Font=ft_b_normal  ,uvalue=[-88,359,11,i,b_25m])
   bid =widget_label (b_25  ,value='Kern:'        ,Font=ft_b_normal)
  b_fik=widget_text  (b_25  ,value=strst(f_filk)  ,Font=ft_propor,xsize=2+cap,/editable)
   bid =widget_label (b_25  ,value='Step:'       ,Font=ft_b_normal)
  b_fip=widget_text  (b_25  ,value=strst(f_filp)  ,Font=ft_propor,xsize=3+cap,/editable)

 b_sinf=widget_draw  (bas3  ,xsize=b_siz(0),ysize=b_siz(1))
 bas32 =widget_base  (bas3  ,/row)
 bid   =widget_button(bas32 ,value='... Proceed ...',Font=ft_b_normal,$
                            uvalue=[-88,359,4,b_dis,b_psz,b_stp,b_cen,b_air,b_fik,b_fip])
 b_all =widget_button(widget_base(bas32,/nonexclusive),value='for all'  ,Font=ft_b_normal,uvalue=[-88,359,6])

 widget_control,b_slir,sensitive=0 
 widget_control,b_slic,sensitive=0 
 widget_control,b_all ,sensitive=0 , set_button=w_all
 widget_control,but0 ,set_button=f_trans
 widget_control,but1 ,set_button=f_stri
 widget_control,b_div,set_button=f_div
 widget_control,b_tom ,/realize    & put_logo
 widget_control,b_sinr,get_value=j & b_sinr=j
 widget_control,b_sinf,get_value=j & b_sinf=j
 widget_control,b_rcon,get_value=j & b_rcon=j

Xmanager,'Tomogra',b_tom,event_handler='LAMP_EVENT_PARSER',/just_reg

endif else widget_control,bad_id=i,b_tom,map=1
end
