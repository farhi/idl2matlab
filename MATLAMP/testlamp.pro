;*******************************************************************************************
;*******************************************************************************************
;*******************************************************************************************

pro spec_wplot,event, runtxt
;** **********
;**
common c_rdspec, c_hand, c_fil, c_siz, c_inor, c_imcd, c_ot, c_list, c_lz, c_fifi, c_bas

runtxt=''
index =event.index
txt   = c_list(index)
i     = strpos(txt,'.')
if (i gt 0) and (i lt 8) then begin    ;runtxt=c_fifi+'{'+strtrim(strmid(txt,0,i),2)+'}'
	r1 = strtrim(string(c_hand.scan(index).scan_n ),2)
	r2 = strtrim(string(c_hand.scan(index).scan_ap),2)
	runtxt=c_fifi+'{'+r1+'.'+r2+'}'
endif
if n_elements(c_list) ne c_lz then SPEC_WLIST
end

pro spec_wlist,lbase
;** **********
;**
common c_rdspec, c_hand, c_fil, c_siz, c_inor, c_imcd, c_ot, c_list, c_lz, c_fifi, c_bas

c_lz =n_elements(c_list)
if n_elements(lbase) eq 1 then $
     c_bas=widget_list(lbase,xsize=20,ysize=c_lz,value=c_list,uvalue=[-88,569,0,-1,0]) $
else if c_bas gt 0 then widget_control,bad_id=ii,c_bas,set_value=c_list
end

function read_spec, INST , PATH , FILENAME , STATUS , DATP
;******* *********
;**
;**	Standard call for a data-read function interfacing LAMP.

common c_rdspec, c_hand, c_fil, c_siz, c_inor, c_imcd, c_ot, c_list, c_lz, c_fifi, c_bas

if n_elements(INST) eq 0 then return,1

STATUS=11
DATA  =0

CATCH,stat & if stat ne 0 then begin CATCH,/cancel & print,!err_string & return,DATA & endif

;Check for scan number
;---------------------
FileN=FILENAME(0)
if n_elements(FILENAME) gt 1 then immg=FILENAME(1) else immg=1
ac=0
i =strpos(FileN,'{')                     & if i lt 0 then i=strpos(FileN,'[')
if i gt 1 then begin j=strpos(FileN,'}') & if j lt 0 then j=strpos(FileN,']')
                     ac=1
                     if j gt i+1  then immg=strmid(FileN,i+1,j-i-1)
		     FileN=strmid(FileN,0,i) & endif
c_fifi=FileN
FileN =PATH+FileN
img= 1 & imm=1   & simg = str_sep(string(immg),'.')
ON_IOERROR,mismg &  img = long(simg(0))>1
if n_elements(simg) eq 2 then imm=long(simg(1))>1
mismg:
imgs=strtrim(string(img),2)+'.'+strtrim(string(imm),2)
if not ac then FILENAME(0)=FILENAME(0)+'{'+imgs+'}'

;Check for file
;--------------
if n_elements(c_fil) eq 0 then begin c_fil='' & c_bas=0L & endif
if c_fil eq FileN         then new=0 else begin new=1 & c_fil=FileN & endelse

ON_IOERROR ,misfil
OPENR, Unit, FileN, /get_lun & S=fstat(Unit) & free_lun,unit
STATUS=13

if not new then if c_siz ne S.size then new=1

;Initialize Handel
;-----------------
if new  then begin print,'Initialising input file ....'
   n=spec_access(c_hand,FileN)
   if n le 0 then  return,DATA
   c_inor = spec_scan(c_hand,'-',/no_empty)
   c_imcd = spec_scan(c_hand,'-',/mcdata)
   c_fil  = FileN
   c_siz  = S.size
   HEAD   = spec_headers(c_hand,1,/ALL,/INDEX)
   F='' & D='' & C=''
   for i=n_elements(HEAD)-1,0,-1 do begin HD=strcompress(HEAD(i))
	a = strmid(HEAD(i),0,2)
	if a eq '#F' then F=' Origin:...'+strmid(HD,(strlen(HD)-20)>3,20)
	if a eq '#D' then D=' Start:'    +strmid(HD,3,25)
	if a eq '#C' then C=' Sample:'   +strmid(HD,3,50)
   endfor
   c_ot   = C+D+F

   inor   = spec_scan(c_hand,'-',/return_index)-1
   imcd   = spec_scan(c_hand,'-',/return_index,/mcdata)-1
   c_list = '<none>'
   if  inor(0) ge 0 then c_list= strtrim(string(c_hand.scan(inor).scan_n),2)+'. '   + $
                                 c_hand.scan(inor).name
   if  imcd(0) ge 0 then begin
    if inor(0) ge 0 then c_list=[c_list,'  ----','MCdata-> '+strtrim(string(c_hand.scan(imcd).scan_n),2)+' '+ $
                                 c_hand.scan(imcd).name]
    if inor(0) lt 0 then c_list= strtrim(string(c_hand.scan(imcd)),2)+'. MC '+ $
                                 c_hand.scan(imcd).name
   endif
   c_lz =0
   P_MAC_EVENT,0,[-88,570,0,-1]
   if n_elements(c_list) ne c_lz then SPEC_WLIST
endif
STATUS=9

;Check if scan number is there
;-----------------------------
mcd=0 & nor=0
idx=where(c_inor eq img) & if idx(0) ge 0 then nor=1
idx=where(c_imcd eq img) & if idx(0) ge 0 then mcd=1
if mcd+nor eq 0 then return,DATA
STATUS=14

;Prepare all variables
;---------------------
P_TXT=spec_headers(c_hand,imgs)+'  '
P_TXT=[P_TXT,FileN+'{'+imgs+'} points--> in PVi variable of Lamp  ']
P    =intarr(n_elements(P_TXT))
PV=0
X =0 & Y =img & Z =img
E =0
N =0
WT='' & XT='' & YT='' & ZT='' & OT=c_ot

T=0. & M=0. & Q='' & Tp=''
For i=n_elements(P_TXT)-1,0,-1 do begin
	a = strmid(P_TXT(i),0,2)
	if a eq '#S' then WT=strmid(strcompress(P_TXT(i)),3,100)
	if a eq '#Q' then Q =' (hkl:' +strmid(strcompress(P_TXT(i)),3,50)+')'
	if a eq '#X' then Tp=' Temp:'+strmid(strcompress(P_TXT(i)),3,50)
	if a eq '#T' then reads,strmid(P_TXT(i),2,20), T
	if a eq '#M' then reads,strmid(P_TXT(i),2,20), M
endfor
WT=WT+Tp     ;+Q
N =[[M],[T]]

;Read the data
;-------------
STATUS=0
if nor then begin
		PV   =spec_data(c_hand,imgs,LABELS=LABL)
		
		if not mcd then begin
		 YT   =LABL(n_elements(LABL)-1)
		 XT   =LABL(0)
		 sz   =SIZE(PV)
		 if sz(0) eq 2 then begin
		   DATA=reform(PV(sz(1)-1,*))
		   MONI=reform(PV(sz(1)-2,*))
		   X  =0 & ok=-1
		   for i=0,sz(1)-3 do begin
			if ok lt 0 then if PV(i,0) ne PV(i,sz(2)-1) then begin
				X=reform(PV(i,*)) & ok=i & endif
		   endfor
		   if ok ge 0 then XT=LABL(ok)
		   E=sqrt(DATA)
		   if N(0,1) gt 0 then begin N=fltarr(sz(2),2) & N(*,0)=MONI & N(*,1)=T
		   endif else $
		   if N(0,0) gt 0 then begin N=fltarr(sz(2),2) & N(*,0)=M & N(*,1)=MONI & endif
		 endif else begin
		   DATA=PV
		 endelse
		endif
endif
if mcd then begin
		DATA =spec_data(c_hand,imgs,/MCDATA,CHANNELS=X,CALIBRATED=calib)
		sz=SIZE(DATA)
		XT='Channels'
		Nt=N
		N =fltarr(sz(1),3) & N(*,0)=M & N(*,1)=T & N(*,2)=calib
		Y =indgen(sz(2))
		if nor then begin
		 sz   =SIZE(PV)
		 YT   =LABL(0)
		 if sz(0) eq 2 then begin
		   ok=-1
		   for i=0,sz(1)-2 do begin
			if ok lt 0 then if PV(i,0) ne PV(i,sz(2)-1) then begin
				Y=reform(PV(i,*)) & ok=i & endif
		   endfor
		   if ok ge 0 then YT=LABL(ok)
		 endif
		endif
endif

;Return every thing
;------------------
DATP={X:      x   , Y:y , Z:z , E:e , N:n   ,$
      W_TIT:  wt  , X_TIT:xt  , Y_TIT:yt    ,$
      Z_TIT:  zt  , OTHER_TIT:ot            ,$
      P:      p   ,    $
      PAR_TXT:p_txt,   $
      PV:     pv       }


misfil: RETURN, DATA
;       ************
 END
pro setpar_txt, inst_val,wi
;** **********
@lamp.cbk
     		par_txt(wi,0) ='X Dectector size (pixels)         ='
     		par_txt(wi,1) ='Y Dectector size (pixels)         ='

		if (inst_val eq 'in15') then begin
     		   par_txt(wi,2) ='Time of Flight if 1               ='
     		   par_txt(wi,3) ='Preset Monitor if 1 , time if 2   ='
     		   par_txt(wi,4) ='Wave lenght                       ='
     		   par_txt(wi,5) ='TOF Channel resolution            ='
     		   par_txt(wi,6) ='TOF Channel width                 ='
     		   par_txt(wi,7) ='TOF Delay                         ='
     		   par_txt(wi,8) ='???                               ='
;**
		 endif else if (inst_val eq 'd19')  then begin
     		   par_txt(wi,2) ='Acquisition number (>0 for scan ) ='
     		   par_txt(wi,3) ='Starting points                   ='
     		   par_txt(wi,4) ='Scan angle (2=omega 3=chi 4=phi)  ='
     		   par_txt(wi,5) ='Scan angle 1 value                ='
     		   par_txt(wi,6) ='Non  Scan  angle phi              ='
     		   par_txt(wi,7) ='Non  Scan  angle chi              ='
     		   par_txt(wi,8) ='Non  Scan  angle omega            ='
     		   par_txt(wi,9) ='Non  Scan  angle 2 theta          ='
     		   par_txt(wi,10)='Monitor 1                         ='
     		   par_txt(wi,11)='Time 1                            ='
;**
  		 endif else if (inst_val eq 'db21') then begin
     		   par_txt(wi,2) ='Acquisition number (>0 for scan ) ='
     		   par_txt(wi,3) ='Starting points                   ='
     		   par_txt(wi,4) ='Scan angle (2=omega 3=chi 4=phi)  ='
     		   par_txt(wi,5) ='Scan angle 1 value                ='
     		   par_txt(wi,6) ='Non  Scan  angle phi              ='
     		   par_txt(wi,7) ='Non  Scan  angle chi              ='
     		   par_txt(wi,8) ='Non  Scan  angle omega            ='
     		   par_txt(wi,9) ='Non  Scan  angle 2 theta          ='
     		   par_txt(wi,10)='Monitor 1                         ='
     		   par_txt(wi,11)='Time 1                            ='
;**
		 endif
return
end

pro rpcill, inst_val,run,wi,wstr,status
;** ******
;   Used by ILL only
@lamp.cbk

	instru =long(0)
	ttp    ='=lonarr'
	
	if inst_val eq 'espace'  then inst_val='d19'  else $
	if inst_val eq 'in5sgi'  then inst_val='in5'  else $
	if inst_val eq 'in6sgi'  then inst_val='in6'  else $
	if inst_val eq 'in10sgi' then inst_val='in10' else $
	if inst_val eq 'd7sgi'   then inst_val='d7'   else $
	if inst_val eq 'd19sgi'  then inst_val='d19'  else $
	if inst_val eq 'db21sgi' then inst_val='db21'
	
;	if inst_val eq 'in4'     then instru=long(1)    else $
;	if inst_val eq 'in5'     then instru=long(2)    else $
;	if inst_val eq 'in6'     then instru=long(3)    else $
;	if inst_val eq 'in15'    then instru=long(5)    else $
;	if inst_val eq 'in10'    then instru=long(4)    else $
;	if inst_val eq 'in16'    then instru=long(6)    else $
;	if inst_val eq 'd7'      then instru=long(7)    else $
	if inst_val eq 'd9'      then instru=long(8)    else $
;	if inst_val eq 'd11'     then instru=long(9)    else $
;	if inst_val eq 'd17'     then instru=long(10)   else $
	if inst_val eq 'd19'     then begin
				      instru=long(11) & ttp='=intarr' & endif else $
	if inst_val eq 'd22'     then instru=long(12)   else $
	if inst_val eq 'db21'    then begin
				      instru=long(13) & ttp='=intarr' & endif
	
    if  (instru    gt   0) and $
       ((lamp_data eq 'hostvms') or (lamp_data eq 'idol')) then begin

        status =3
	run    =long(run)
	channel=long(0) & spect=long(0) & np=long(0) & text=bytarr(512) & param=fltarr(512)

	entri=sys_dep('ENTRY')   ;lamp_entry
	com1='status=call_external(lamp_exec,entri,instru,get,run,channel,spect,np,text,param,0,0)'
	com2='status=call_external(lamp_exec,entri,instru,get,run,channel,spect,np,text,param,nppar,'$
				   +'w'+wstr+')'
	stat=0 & iii=1
      	catch,stat
      	if (stat eq 0) and (iii eq 1) then begin
;**	Get the parameters
;**	*** *** **********
	    get=long(0)
      	    iii=execute(com1)
      	    status=get
      	    if iii ne 1 then status=23
      	    if (status eq 0) then begin 
      	      w0  =0
      	      full=0
      	      if (wi gt 0) and (wi le 20+3) then iii=execute('w'+wstr+'=0') else full=-1

	      if (np gt 201) then np=201
      	      if (full ge 0) and (np ge 0) then begin
;**	Allocate all spaces
;**	******** *** ******
		if (inst_val eq 'd11') or (inst_val eq 'd17') or (inst_val eq 'd22') then $
		    if (channel ge 64.*64.) and (spect  le 1) then begin channel=long(sqrt(channel))
		    							 spect  =channel & endif
      	    	if np gt 0 then nppar=fltarr(15,2,np) else nppar=0
      	    	if spect le 1 then iii=execute('w'+wstr+ttp+'(channel)')     else $
      	    	if np    le 1 then iii=execute('w'+wstr+ttp+'(channel,spect)'   ) $
      	    		      else iii=execute('w'+wstr+ttp+'(channel,spect,np)')

;**	Get the data
;**	*** *** ****
      	    	get    =long(1)
      	    	iii=execute(com2)
      	    	status=get
      	        if iii ne 1 then status=23
	      endif 	    else status=8
;**	Must be positive
;**	**** ** ********
	      if (status eq 0) and (ttp eq '=intarr') then begin
	      	  mini=0 & iii=execute('mini=min(w'+wstr+')')
	      	  if mini lt 0 then begin
	      	  	hhs=0 & iii=execute('hhs=where(w'+wstr+' lt 0)')
	      	  	iii=execute('w'+wstr+'=long(w'+wstr+')')
	      	  	iii=execute('w'+wstr+'(hhs)=65536+w'+wstr+'(hhs)')
	      endif & endif

;**	Place all parameters
;**	***** *** **********
		clearpar, wi,wstr
		
		npar=n_elements(param) & if npar gt npars then npar=npars
		iii =execute('p' +wstr+'= param(0:npar-1)')       
      	    	w_numor(wi)	 = strtrim(string(run),2)

     	        w_tit(wi)	 =strcompress(string(text(0:59)))
     	        x_tit(wi)	 ='Channels'   
     	        y_tit(wi)	 ='Spectra'   
     	        if np gt 1 then z_tit(wi) ='Points' else  z_tit(wi) ='Counts' 
     	        other_tit(wi)	 =w_numor(wi)+' '+strcompress(string(text(60:79))) $
     	        			     +' '+strcompress(string(text(80:159)))
     	        head_tit (wi,*)  =''
     	        head_tit (wi,2)  =inst_value
;**
		if (inst_val eq 'in15') or (inst_val eq 'd19') or (inst_val eq 'db21') then begin
     	         w_tit(wi)	=strcompress(string(text(36:107)))
     	         head_tit (wi,4)=				string(text(4:13))
     	         other_tit(wi)  =string(text(0:3))   +' Date '+ string(text(4:13))  +' Time '+ $
     	        		 string(text(14:21)) +' User '+ string(text(24:29)) +' '     + $
     	        		 string(text(30:35)) +' Run ' + w_numor(wi)
                 x_tit(wi)	='Detector X'   
     	         y_tit(wi)	='Detector Y'
		 setpar_txt, inst_val,wi
		  
		 if (inst_val eq 'in15') then begin
    	           iii=execute('p' +wstr+'= [param(0:3),param(33:37)]')       
    	           iii=execute('pv'+wstr+'=  nppar')       
;**
		 endif else if (inst_val eq 'd19')  then begin
     		   ptmp= [param(0:1),param(6),param(17),param(26),nppar(0,1,0),$
     		   	  param(36),param(37),param(38),param(39),nppar(0,0,0),$
     		   	  nppar(1,0,0)]
    	           iii=execute('p' +wstr+'= ptmp')       
    	           iii=execute('n' +wstr+'=[[nppar(0,0,0:np-1)],[nppar(1,0,0:np-1)]]')       
    	           iii=execute('z' +wstr+'=[ nppar(0,1,0:np-1)]')       
;     	           iii=execute('pv'+wstr+'=[ nppar(1,1,0:np-1)]')       
;** 	
  		 endif else if (inst_val eq 'db21') then begin
     		   ptmp= [param(0:1),param(6),param(17),param(26),nppar(0,1,0),$
     		   	  param(36),param(37),param(38),param(39),nppar(0,0,0),$
     		   	  nppar(1,0,0)]
    	           iii=execute('p' +wstr+'= ptmp')       
    	           iii=execute('n' +wstr+'=[[nppar(0,0,0:np-1)],[nppar(1,0,0:np-1)]]')       
    	           iii=execute('z' +wstr+'=[ nppar(0,1,0:np-1)]')
    	         endif       

 		endif else begin
 		   mic,1
		   P_MIC_SETRUN, run,wi,text,param
		endelse
      	      endif
      	    endif else begin catch,/cancel
			     P_MUS,'mus_cannon'
      	    		     status=23 & endelse
      	endif else begin
		bz   ='Current Cycle'
		idx=strpos(lamp_ali,inst_val)
		idx=where (idx ge 0)
		if  idx(0) ge 0 then bz=lamp_ali(idx(0))
		CYCLE=''
		RDSET , BASE=bz
		p_did_getrun, run,wi,status
	endelse
return
end

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
pro write_inx, o_file,w_buf ,XC=x_buf, YC=y_buf, PR=pmt_buf, E=e_buf ,$
                             ZC=z_buf,  N=n_buf, PV=pv,PAR_TXT=p_txt ,$
                             W_tit=wt  , X_tit=xt , Y_TIT=yt         ,$
                             Z_tit=zt  , OTHER_TIT=ot
;** *********
;**
; Dumps an INX file - parameters may be a bit odd

pi=!pi

if n_elements(pmt_buf) lt 22 then pmt_buf=fltarr(22)+1

; Prepare zones
   nzone=intarr(7)
   nzone(1)=1
   nzone(2)=2
   nzone(3:6)=0

   my_check=size(w_buf)

;
; What dimensions for the data?
      case my_check(0) of
         0: return
         1: begin
               nchans=my_check(1)
               nspecs=1
            end
         2: begin
               nchans=my_check(1)
               nspecs=my_check(2)
            end
	 else: return
      endcase
   
   if n_elements(w_buf) ne n_elements(e_buf) then e_buf=w_buf*0.
   if (size(y_buf))(0) eq 2 then y_buf=reform(y_buf(0,*))
   sx=(size(x_buf))(0)
   
; Get the parameters
      temp	=pmt_buf(11) 
      einc	=81.799/pmt_buf(21)/pmt_buf(21) 
      qinc	=2*pi/pmt_buf(21) 
      amass	=1.0
      nlines	=nchans+3
      deltatau	=pmt_buf(18)
      deltaen	=0.0
      deltak	=0.0
      isym	=0
      dumme	=0.0

      on_ioerror,pb_wrt  
      get_lun,ifile
      openw,ifile,o_file
				frmv='(5x,f10.5,e13.5,e12.4)'
       if max(y_buf) ge 10 then frmt='(x,f6.2,f8.3,f8.4,f9.3,f6.1,i2)' $
			   else frmt='(x,f6.4,f8.3,f8.4,f9.3,f6.1,i2)'
       for i=0,nspecs-1 do begin
	 printf ,ifile,nlines,nzone(1:6),nchans          ,format='(8i5)'
	 printf ,ifile,wt
	 printf ,ifile,y_buf(i),einc,qinc,temp,amass,isym,format=frmt
	 printf ,ifile,deltaen,deltatau,deltak           ,format='(16x,3f8.4)'
	 if sx eq 1 then for j=0,nchans-1 do $
	  printf,ifile,x_buf(j)  ,w_buf(j,i),e_buf(j,i)  ,format=frmv

	 if sx eq 2 then for j=0,nchans-1 do $
	  printf,ifile,x_buf(j,i),w_buf(j,i),e_buf(j,i)  ,format=frmv

       endfor
       pb_wrt:free_lun,ifile
    return
    end	    
      
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
PRO rdid_d2b, INST,numor,nvers,text,exper,scan,cnt,nd,WOUT,vparm,param,par1,par2,par3,par4,par5,$
	                 WT ,XT ,YT ,ZT ,OT ,DATE ,PP ,PTXT ,XX ,YY ,ZZ ,NN ,PV ,EE


common calibration
common calibrotion
common c_rdid

                   print, 'RDID_D2B on pcmrjohnson'

;                      *** data will be (number of detectors, height, number of points) ***
;                      *** WOUT will be 2D (nt*nj,nh), ZZ contains angles               ***
;                      *** for calibration ...                                          ***

;                      *** set physical height of detector (30cm)                       ***
		   h=30.0

		   D2TH = param(36) ;& print, '   D2TH  ', D2TH
		   nj  = ROUND(n_elements(WOUT)/(nd)) ; number of points in scan
		   nt  = 128                          ; number of detector tubes
                   nh  = nd/nt                        ; number of height pixels for 128 detector tubes
;		       *** rewrite WOUT in matrix form (nh, nt, nj) ***
	   	   WOUT= reform(WOUT, nh, nt ,nj ,/overwrite) ;& print, size(WOUT,/dimensions)
                   print, 'first reform'
;		   NN  = WOUT(0,0) ; set monitor
;			*** set titles ***
   	   	   YT  ='height'
		   XT  ='2*Theta'

;		   *** definition of new calibration arrays (default: equal to old)
		   newzon_d2b=zon_d2b ;& print, zon_d2b
		   newcal_d2b=cal_d2b
		   newang_d2b=ang_d2b ;& print, newang_d2b
		   newd=nd

		      print, 'linear interpolation of data'
;		   *** now we have risers and falls for the detector ***
;		   *** interpolate data onto 128 (nh) channels       ***
;		   *** reverse the data in the second tube           ***
		   rise1=intarr(nt/2) & fall1=intarr(nt/2) & rise2=intarr(nt/2) & fall2=intarr(nt/2)
		   rise1(*)=newzon_d2b(0,*) & fall1(*)=newzon_d2b(1,*) & rise2(*)=newzon_d2b(2,*) & fall2(*)=newzon_d2b(3,*)
;		   rise1(*)=0 & fall1(*)=127 & rise2(*)=128 & fall2(*)=255 ; default values - commented!!!
		   FOR j=0,nj-1 DO BEGIN  ; loop thro scan points nj
		      FOR i=0,nt/2-1 DO BEGIN ;
			 WONE2 = [WOUT(*,i*2,j),WOUT(*,i*2+1,j)] ; put pairs of tubes together again
		         WOUT(*,i*2,  j)=CONGRID(WONE2(rise1(i):fall1(i)),nh,/INTERP)
		         WOUT(*,i*2+1,j)=REVERSE(CONGRID(WONE2(rise2(i):fall2(i)),nh,/INTERP))
		      ENDFOR
		   ENDFOR

;		   *** if calibration available, calibrate by MULTIPLYING ***
		   IF n_elements(cal_d2b) eq nh*nt THEN BEGIN             ;if calibration file exists
                     print, 'CALIBRATING'
                     WOUT=float(WOUT)
                     FOR i=0,nj-1 DO WOUT(*,*,i)=WOUT(*,*,i)*newcal_d2b   ;newcal has dimension nh*nt i.e. resolution per pixel
                     fct=1.                                               ;factor set to 1 to preserve angles in ang_d2b
		     OT=OT+' /'+inf_d2b(0) ; set sub-title
;		        *** fct*newang_d2b always newd values spaced by ~ -1.25 ***
		   ENDIF ELSE BEGIN                                       ;calibration does not exist, generate angles
		     if n_elements(newang_d2b) ne nt then newang_d2b=127.0-findgen(nt)
                     fct = -1.25
		   ENDELSE

;                  *** find and remove dud detectors
;                  *** find detectors with efficiency too different from 1, work with detector number=index+1
		   cal_1d=cal_d2b(0,*) ;& print, cal_1d; 1 efficiency per tube
		   if n_elements(pzip) eq 1 then seuil=pzip else seuil=1.5 & print,'cutoff=',seuil
		   ezap=0
		   ;for i=0,nt-1 do begin
		   ;  if (cal_1d(i) gt seuil) or (cal_1d(i) lt 1/seuil) then ezap=[ezap,i+1]
		   ;endfor
		   rseuil=1./seuil
		   idx=where((cal_1d gt seuil) or (cal_1d lt rseuil)) & if idx(0) ge 0 then ezap=idx+1

;		   *** combine dzap and ezap
		   zap=[dzap,ezap]
		   zap=zap[uniq(zap,sort(zap))]

;		   *** test if zap contains real detectors **
		   idz=where(zap ge 1 and zap le 128)
;		   *** in case, eliminate spurious values ***
		   if idz(0) ne -1 then zap=zap(idz) else zap=0
;		   *** is there something left? ***
		   SD=SIZE(zap)


;		   *** if zap has something, remove corresponding detector tubes from WOUT ***
		   if SD(0) gt 0 then BEGIN
		     print, 'removing dud detectors'
;		     *** set all dud values and angles to -999 ***
		     WOUT(*,zap-1,*)=-999
		     newang_d2b(zap-1)=-999
;		     *** good values are those which do not contain -999 ***
		     GOOD=where(WOUT ne -999)
;		     if GOOD(0) eq -1 then return, GOOD(0)

;		     *** cut out dud values from WOUT and angles, not calibration as already done ***
		     WOUT=WOUT(GOOD)
		     newang_d2b=ang_d2b(where(newang_d2b ne -999))
;		     *** redefine the number of detectors
		     nt=(nt-n_elements(zap)) & print,n_elements(zap),'  detectors removed   '
;		     *** reform WOUT to proper format
		     WOUT=reform(WOUT, nh, nt, nj ,/overwrite) ;& print, size(WOUT,/dimensions)
		   ENDIF

;                  *** set tubes positions for first detector position
;                   newang_d2b=nt-1-newang_d2b ;turn angles around
		   if nj gt 1 then XX=fct*newang_d2b+ZZ(0) else XX=1.25*findgen(nh) ; angle not picked up for one point scan

;		   *** reform WOUT to be 2D (nt*nj,nh), transpose first to put nt&nj together ***
		   if nj gt 1 then WOUT= transpose(WOUT,[1,2,0]) else WOUT= transpose(WOUT,[1,0]) ; WOUT looses 3rd dimension in one point scan
		   WOUT= reform(WOUT,nt*nj,nh,/overwrite)
                   print, 'second reform'

                   print, 'concatenating detector positions '
;			*** do the following if there are at least 2 points in scan ***
;			*** create a XX array by adding up all 2th for all dets **
;			*** in the same order as in WOUT
		   IF nj gt 1 then BEGIN                                     ; i.e. not single point scan
					 FOR i=1,nj-1 do  XX=[XX , fct*newang_d2b+ZZ(i)]
;			*** sort the XX and WOUT arrays, preserve 2D of latter (idx is the index array) ***
		   			 idx=sort (XX) &  XX =XX(idx) & WOUT=WOUT(idx,*)
;			*** XR contains the 2th's rounded to the nearest step size ***
		   			 XR =round(XX/D2TH) & XR=XR*D2TH
;			*** GRID contains the fractional indices of XR into XX ***
					 GRID=(XR-XX)/D2TH+findgen(n_elements(XX))
;			*** for each height do a 1D interpolation using WOUT_1D and reassign WOUT***
					 FOR i=0,nh-1 DO BEGIN
					   WOUT_1D=WOUT(*,i)
					   WOUT(*,i)=interpolate(WOUT_1D,GRID,CUBIC=-0.5)
					 ENDFOR
;			*** reassign XX and WOUT
					 XX=XR
;			*** YY is the height, EE the error
					 YY = findgen(nh)*h/(nh-1) - h/2.0
		   			 ZZ = param(46)
					 WOUT=WOUT>0
                 	 EE = SQRT(WOUT)
		   ENDIF

END
;*********** D2B => Paolo Radaelli ******************

pro dud, dets=d, pts=p, cutoff=c


common c_rdid, dzap, pzap, pzip

if n_elements(d) gt 0 then dzap=d
if n_elements(p) gt 0 then pzap=p
if n_elements(c) gt 0 then pzip=c


end
;-----------------------------------------------------------------------------	
	
	PRO FR_calculate, n_buf , j, ratio, dRatio

	IF n_buf(2,j + 1) EQ 0. THEN BEGIN
		ratio = 999. & dratio = 0.
		RETURN
	ENDIF

	nonflip  = n_buf(2,j)     / n_buf(1,j)
	flip     = n_buf(2,j + 1) / n_buf(1,j + 1)
	ratio    = nonflip/flip
	dnonflip = SQRT(n_buf(2,j)/n_buf(1,j)^2 + n_buf(2,j)^2/n_buf(1,j)^3)
	dflip    = SQRT(n_buf(2,j + 1)/n_buf(1,j + 1)^2 + n_buf(2,j + 1)^2/n_buf(1,j + 1)^3)
	dratio   = SQRT((dnonflip/flip)^2 + (nonflip*dflip/flip^2)^2)
	
	RETURN
	END

;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO rdid_d7, INST,numor,nvers,text,exper,scan,cnt,WOUT,vparm,param, $
		     par1,par2,par3,par4,par5,WT ,XT ,YT ,ZT ,OT,DATE,PP,PTXT,$
		     XX ,YY ,ZZ ,NN ,PV ,EE

;LAMP data-read interface for D7 data (ILL format) called from rdid.pro
;**********************************************************************
;
; Reads 3 versions of D7 data:
;	Version 0: Up to and including cycle 961
;	Version 1: 2nd half of cycle 965 up to and including cycle 024
;	Version 2: From cycle 031 onwards
;
; Doesn't read D7 data in cycles 962-964 and the beginning of cycle
; 965 corresponding to run numbers #7002 - #9583 which were 
; badly formatted.  This data may be read with the macro "read_d7_96.pro"
;
;
;Parameters to return
;********************
;WOUT             (modified data)
;WT,XT,YT,ZT,OT   (titles)
;PP,PTXT          (parameter floatting table and textarea)
;XX,YY,ZZ         (coordinates)
;NN               (monitors)
;PV               (supplement parameter table of any dimension)
;EE               (errors from WOUT)
;
;							JRS 4/4/03
;-------------------------------------------------------------------------------
;*******************************************************************************
;
	iprint = 0

;Useful input parameters from rdid
;*********************************

	IF iprint THEN BEGIN
		HELP, INST 	& PRINT, INST
		HELP, numor 	& PRINT, numor
		HELP, nvers 	& PRINT, nvers
		HELP, text 	& PRINT, text
		HELP, exper 	& PRINT, exper
		HELP, scan 	& PRINT, scan
		HELP, cnt 	& PRINT, cnt
		HELP, WOUT
		HELP, vparm 	& PRINT, vparm
		HELP, param 	& PRINT, param   
		HELP, par1 	& PRINT, par1
		HELP, par2	& IF (N_ELEMENTS(par2) GT 0) THEN PRINT, par2
	ENDIF

	IF iprint THEN PRINT,'rdid_d7: starting'
	PRINT,'Reading run',numor

;-------------------------------------------------------------------------
;*************************************************************************

	CASE nvers OF

;-------------------------------------------------------------------------

	0: BEGIN

		IF iprint THEN PRINT,'D7:  Old data format -> 1996'

		inst   = STRMID(text,0 ,4)
		numexp = STRMID(text,4 ,10)
		date   = STRMID(text,14,18)
		main_title = numexp
		sub_title  = main_title
		start_time = date

	;      -------------------------------
	;      MONITOR VALUES in n
	;      -------------------------------      

		n_buf = FLTARR(4,7)

		n_buf(*,0) = 0.			; first line is zeros
		n_buf(0,1) = par1(14)		; Time      Z up-up    
		n_buf(0,2) = par1(114)	 	; Time      Z up-down
		n_buf(0,3) = param(0)  	 	; Time      X up-up    
		n_buf(0,4) = param(12)     	; Time      X up-down
		n_buf(0,5) = param(24)		; Time      Y up-up 
		n_buf(0,6) = param(36)		; Time      Y up-down
		n_buf(1,1) = par1(3)    	; Monitor 1 Z up-up
		n_buf(1,2) = par1(103) 		; Monitor 1 Z up-down 
		n_buf(1,3) = par1(117) 		; Monitor 1 X up-up
		n_buf(1,4) = param(1)		; Monitor 1 X up-down
		n_buf(1,5) = param(13)		; Monitor 1 Y up-up 
		n_buf(1,6) = param(25)		; Monitor 1 Y up-down
		n_buf(2,1) = par1(12)    	; Monitor 2 Z up-up
		n_buf(2,2) = par1(112)	 	; Monitor 2 Z up-down 
		n_buf(2,3) = par1(126) 		; Monitor 2 X up-up
		n_buf(2,4) = param(10)		; Monitor 2 X up-down
		n_buf(2,5) = param(22)		; Monitor 2 Y up-up 
		n_buf(2,6) = param(34)		; Monitor 2 Y up-down
		n_buf(3,1) = par1(13)		; Handshake Z up-up
		n_buf(3,2) = par1(113)		; Handshake Z up-down
		n_buf(3,3) = par1(127)		; Handshake X up-up
		n_buf(3,4) = param(11)		; Handshake X up-down
		n_buf(3,5) = param(23)		; Handshake Y up-up
		n_buf(3,6) = param(35)		; Handshake Y up-down

		IF n_buf(0,2) EQ 0. THEN BEGIN
			ratioZ = 0.
			ratioX = 0.
			ratioY = 0.
		ENDIF ELSE IF n_buf(0,3) EQ 0. THEN BEGIN
			IF n_buf(2,2) EQ 0. THEN ratioZ = 999. ELSE $
			ratioZ = (n_buf(2,1)/n_buf(1,1)) / (n_buf(2,2)/n_buf(1,2))
			ratioX = 0.
			ratioY = 0.
		ENDIF ELSE BEGIN
			IF n_buf(2,2) EQ 0. THEN ratioZ = 999. ELSE $
			ratioZ = (n_buf(2,1)/n_buf(1,1)) / (n_buf(2,2)/n_buf(1,2))
			IF n_buf(2,4) EQ 0. THEN ratioX = 999. ELSE $
			ratioX = (n_buf(2,3)/n_buf(1,3)) / (n_buf(2,4)/n_buf(1,4))
			IF n_buf(2,6) EQ 0. THEN ratioY = 999. ELSE $
			ratioY = (n_buf(2,5)/n_buf(1,5)) / (n_buf(2,6)/n_buf(1,6))
		ENDELSE

		nspectra = 64
		IF scan EQ 1 THEN BEGIN					; No TOF
			TOF       = 0
			nchannels = 1
			IF n_buf(0,2) EQ 0. THEN BEGIN			; no PA
				nphases = 1
			ENDIF ELSE IF (n_buf(0,3) EQ 0.) THEN BEGIN 	; only Z-PA
				nphases = 2
			ENDIF ELSE BEGIN				; full XYZ-PA
				nphases = 6
			ENDELSE

			x_buf = par1(23:86)	; x-axis is scattering angle
			y_buf = INDGEN(nphases)
			z_buf = x_buf
			w_buf = FLTARR(nspectra,nphases)
			FOR iphase=0, nphases - 1 DO $
				w_buf(*,iphase) = WOUT(80*iphase : 80*iphase + nspectra - 1)
			chel   = 0.
			nb_chn = 1
			nb_spc = nspectra
		ENDIF ELSE BEGIN
			TOF       = 1
			nchannels = cnt
			nphases   = scan/nspectra
			x_buf     = INDGEN(nchannels)+1 		; x-axis is channel number
			y_buf     = FLTARR(scan)	  		; y-axis is scattering angle
			FOR iphase=0, nphases - 1 DO $
				y_buf(iphase*nspectra : iphase*nspectra + nspectra - 1) = par1(23:86)
			z_buf     = y_buf
			w_buf     = FLTARR(nchannels,scan)
			ispec     = INDGEN(scan)
			w_buf(*,ispec) = WOUT(*,ispec)
			w_tmp     = FLTARR(nchannels)
			FOR i = 1, scan, 2 DO w_tmp = w_tmp + w_buf(*,i)
			Imax = MAX(w_tmp,ichel)
			chel = FLOAT(ichel)
		ENDELSE

		lambda = par1(15)
		IF (lambda GT 4.5) AND (lambda LE 5.0) THEN lambda=4.8 $
		ELSE IF (lambda LT 3.5) THEN lambda=3.1
		IF (lambda GT 5.0) THEN lambda=5.7

	;      -------------------------------
	;      PARAMETER ASSIGNMENT IN Pn
	;      -------------------------------      
		npar        = 34
		p_buf       = FLTARR(npar)
		par_txt_buf = STRARR(npar)

		p_buf(0)  = numor	& par_txt_buf(0)  = ' 0 Numor			='
		p_buf(1)  = nspectra	& par_txt_buf(1)  = ' 1 Number of spectra	='
		p_buf(2)  = nphases	& par_txt_buf(2)  = ' 2 Number of phases	='
		p_buf(3)  = 1.		& par_txt_buf(3)  = ' 3 Number of runs  	='
		p_buf(4)  = lambda	& par_txt_buf(4)  = ' 4 Wavelength (A)  	='
		p_buf(5)  = 0.		& par_txt_buf(5)  = ' 5 Chopper Speed (rpm)	='
		p_buf(6)  = nchannels	& par_txt_buf(6)  = ' 6 Number of Time Channels ='
		p_buf(7)  = par1(17)	& par_txt_buf(7)  = ' 7 Channel Width (mcs)	='
		p_buf(8)  = TOF		& par_txt_buf(8)  = ' 8 TOF mode (0/1)  	='
		p_buf(9)  = chel	& par_txt_buf(9)  = ' 9 Elastic Peak Channel	='
		p_buf(10) = 0.		& par_txt_buf(10) = '10 Set-pt Temp Start	='
		p_buf(11) = 0.		& par_txt_buf(11) = '11 Set-pt Temp End 	='
		p_buf(12) = 0.		& par_txt_buf(12) = '12 Regul Temp Start	='
		p_buf(13) = 0.		& par_txt_buf(13) = '13 Regul Temp End  	='
		p_buf(14) = 0.		& par_txt_buf(14) = '14 Sample Temp Start	='
		p_buf(15) = par1(16)	& par_txt_buf(15) = '15 Sample Temp End 	='
		p_buf(16) = 0.		& par_txt_buf(16) = '16 Bank1 Position  	='
		p_buf(17) = 0.		& par_txt_buf(17) = '17 Bank2 Position  	='
		p_buf(18) = 0.		& par_txt_buf(18) = '18 Bank3 Position  	='
		p_buf(19) = 0.		& par_txt_buf(19) = '19 Bank4 Position  	='
		p_buf(20) = 0.		& par_txt_buf(20) = '20 Lower Sample Rotation	='
		p_buf(21) = par1(22)	& par_txt_buf(21) = '21 Upper Sample Rotation	='
		p_buf(22) = 0.		& par_txt_buf(22) = '22 Z Flipper Current	='
		p_buf(23) = 0.		& par_txt_buf(23) = '23 Z Correction Current	='
		p_buf(24) = 0.		& par_txt_buf(24) = '24 X Flipper Current	='
		p_buf(25) = 0.		& par_txt_buf(25) = '25 X Correction Current	='
		p_buf(26) = 0.		& par_txt_buf(26) = '26 Y Flipper Current	='
		p_buf(27) = 0.		& par_txt_buf(27) = '27 Y Correction Current	='
		p_buf(28) = ratioZ	& par_txt_buf(28) = '28 Z Flipping Ratio	='
		p_buf(29) = ratioX	& par_txt_buf(29) = '29 X Flipping Ratio	='
		p_buf(30) = ratioY	& par_txt_buf(30) = '30 Y Flipping Ratio	='
		p_buf(31) = 0.		& par_txt_buf(31) = '31 Run Time (seconds)	='
		p_buf(32) = 0.		& par_txt_buf(32) = '32 Start time (hours)	='
		p_buf(33) = 0.		& par_txt_buf(33) = '33 End Time (hours)	='

	END

;------------------------------------------------------------------------
	
	1: BEGIN

	datasource=0	; 0 => take data from appropriate counters
			; 1 => take data from hytec counters always
			; 2 => take data from tof counters always

	IF iprint THEN PRINT,'D7: Data format 1 -> 1996 - 2002'
	
	inst         = STRMID(text,0,4)
	save_time    = STRMID(text,14,18)
	main_title   = STRTRIM(exper(0),2)
	sub_title    = STRTRIM(exper(1),2)
	IF (numor GE 9609 AND numor LE 10318) OR $
	   (numor GE 10327 AND numor LE 11062) THEN BEGIN	
		user         = STRMID(exper(3),0,8)
		local_contact= STRMID(exper(3),8,8)
		start_time   = ''
		stop_time    = ''
		scan_type    = ''
		pol_mode     = ''
		Time_mode    = ''	
	ENDIF ELSE BEGIN
		user         = STRMID(exper(2),0,8)
		local_contact= STRMID(exper(2),8,8)
		start_time   = STRMID(exper(3),0,18)
		stop_time    = STRMID(exper(3),20,18)
		scan_type    = STRMID(exper(3),38,9)
		pol_mode     = STRMID(exper(3),47,5)
		Time_mode    = STRMID(exper(3),52,6)
	ENDELSE

	IF iprint THEN BEGIN
		PRINT,'inst          = ',inst
		PRINT,'main_title    = ',main_title
		PRINT,'sub_title     = ',sub_title
		PRINT,'user          = ',user
		PRINT,'local_contact = ',local_contact
		PRINT,'start_time    = ',start_time
		PRINT,'stop_time     = ',stop_time
		PRINT,'save_time     = ',save_time
		PRINT,'scan_type     = ',scan_type
		PRINT,'pol_mode      = ',pol_mode
		PRINT,'Time_mode     = ',Time_mode
	ENDIF

;------------------------------------
;	Monitor Values in n
;------------------------------------

	n_buf      = FLTARR(4,7)		
	n_buf(*,0) = 0.			; first line is zeros
	n_buf(0,1) = par1(13)		; Time      Z up-up    
	n_buf(0,2) = par1(22)	 	; Time      Z up-down
	n_buf(0,3) = par1(31)   	; Time      X up-up    
	n_buf(0,4) = par1(40)     	; Time      X up-down
	n_buf(0,5) = par1(49)		; Time      Y up-up 
	n_buf(0,6) = par1(58)		; Time      Y up-down
	n_buf(1,1) = par1(15)    	; Monitor 1 Z up-up
	n_buf(1,2) = par1(24) 		; Monitor 1 Z up-down 
	n_buf(1,3) = par1(33) 		; Monitor 1 X up-up
	n_buf(1,4) = par1(42)		; Monitor 1 X up-down
	n_buf(1,5) = par1(51)		; Monitor 1 Y up-up 
	n_buf(1,6) = par1(60)		; Monitor 1 Y up-down
	n_buf(2,1) = par1(16)    	; Monitor 2 Z up-up
	n_buf(2,2) = par1(25)	 	; Monitor 2 Z up-down 
	n_buf(2,3) = par1(34) 		; Monitor 2 X up-up
	n_buf(2,4) = par1(43)		; Monitor 2 X up-down
	n_buf(2,5) = par1(52)		; Monitor 2 Y up-up 
	n_buf(2,6) = par1(61)		; Monitor 2 Y up-down
	n_buf(3,1) = par1(14)		; Handshake Z up-up
	n_buf(3,2) = par1(23)		; Handshake Z up-down
	n_buf(3,3) = par1(32)		; Handshake X up-up
	n_buf(3,4) = par1(41)		; Handshake X up-down
	n_buf(3,5) = par1(50)		; Handshake Y up-up
	n_buf(3,6) = par1(59)		; Handshake Y up-down

	IF iprint THEN PRINT,'monitors assigned OK'

;--------------------------------
;	Calculate Flipping Ratios
;--------------------------------

	IF n_buf(0,2) EQ 0. THEN BEGIN				;NOP
		ratioZ = 0.	& dratioZ = 0.
		ratioX = 0.	& dratioX = 0.
		ratioY = 0.	& dratioY = 0.
	ENDIF ELSE IF n_buf(0,3) EQ 0. THEN BEGIN		;ZPO
		FR_calculate, n_buf, 1, ratioZ, dratioZ
		ratioX = 0. & dratioX = 0.
		ratioY = 0. & dratioY = 0.
	ENDIF ELSE BEGIN					;XYZ
		FR_calculate, n_buf, 1, ratioZ, dratioZ
		FR_calculate, n_buf, 3, ratioX, dratioX
		FR_calculate, n_buf, 5, ratioY, dratioY
	ENDELSE
	IF iprint THEN PRINT,'Flipping Ratios OK'

;------------------------------
;	Calculate Running Times
;------------------------------

	h1=LONG(0)	& m1=LONG(0)	& s1=LONG(0)
	h2=LONG(0)	& m2=LONG(0)	& s2=LONG(0)
	IF (start_time NE '') AND (stop_time NE '') THEN BEGIN
		READS, start_time, FORMAT = '(I2,8X,I2,1X,I2,1X,I2)', da1, h1, m1, s1
		READS, stop_time,  FORMAT = '(I2,8X,I2,1X,I2,1X,I2)', da2, h2, m2, s2
		time1 = FLOAT(3600*h1+60*m1+s1)/3600.+24.*da1
		time2 = FLOAT(3600*h2+60*m2+s2)/3600.+24.*da2
		IF (h2 GE h1) THEN run_time = FLOAT(3600*(h2-h1)+60*(m2-m1)+(s2-s1)) $
			      ELSE run_time = FLOAT(3600*(24+h2-h1)+60*(m2-m1)+(s2-s1))
	ENDIF ELSE BEGIN
		time1     = 0.
		time2     = 0.
		run_time  = 0.
	ENDELSE

;--------------------------------
;	Counts
;--------------------------------

	nspectra  = 64
	shytec    = SIZE(param)
	c_hytec   = param
	nsp_hytec = shytec(1)
	nph_hytec = nsp_hytec/nspectra
	nphases   = nph_hytec

	IF (scan EQ 1) THEN TOFformat=0 ELSE TOFformat=1

	IF (NOT TOFformat) AND (datasource NE 1) THEN BEGIN	; no TOF
		IF iprint THEN PRINT,'non-TOF data'
		TOF       = 0
		nsp_tof   = cnt
		nph_tof   = cnt/nspectra
		nchannels = 1
		c_tof     = WOUT
	ENDIF ELSE IF (datasource EQ 1) THEN TOF=0 ELSE BEGIN	; with TOF
		IF iprint THEN PRINT,'TOF data'

; mon1 and mon2 tof channels added as dets 65 and 66 from #42356 (23/11/99)

		IF (numor GE 42356) THEN nspectra=66 
		TOF       = 1
		nsp_tof   = scan
		nph_tof   = scan/nspectra
		nchannels = FIX(par2(2))
		c_tof     = WOUT
	ENDELSE

	IF iprint THEN PRINT,'nsp=',nsp_tof,' nph=',nph_tof,' nchannels=',nchannels
	IF iprint THEN PRINT,'nphases=',nphases, 'nspectra=',nspectra

	TOFfactor = FLTARR(6)	& TOFfactor(*) = 1.
	IF (TOF EQ 0) THEN BEGIN
		IF iprint THEN PRINT,'TOF = 0'
		IF (datasource EQ 2) THEN w_buf=reform(c_tof,nspectra,nphases) $
			ELSE w_buf=reform(c_hytec,nspectra,nphases)
		x_buf = par2(6:69)
		IF (nphases EQ 1) THEN y_buf = LONG(numor) $
				  ELSE y_buf = INDGEN(nphases)
		z_buf = x_buf
		chel  = 0.
		norm  = 1.
	ENDIF ELSE BEGIN
		IF iprint THEN PRINT,'TOF=1'
		w_buf = c_tof
		x_buf = INDGEN(nchannels)+1
		y_buf = FLTARR(nsp_tof)
		FOR iph = 0, nph_tof - 1 DO y_buf(iph*nspectra:iph*nspectra + 63) = par2(6:69)
		z_buf = y_buf
		w_tmp = LONARR(nchannels)	& w_tmp(*) = 0
		FOR i = 1, nsp_tof, 2 DO w_tmp(*) = w_tmp(*) + w_buf(*,i)
		Imax = MAX(w_tmp,ichel)
		chel = FLOAT(ichel)
		FOR iphase = 0, nphases - 1 DO BEGIN
			lowlim  =  iphase*64
			highlim = (iphase*63) + 63 + iphase
			sumh    = TOTAL(c_hytec(lowlim:highlim))
			tott    = TOTAL(FLOAT(c_tof),1)
			lowlim  = iphase*66
			highlim = (iphase*65) + 63 + iphase
			sumt    = TOTAL(tott(lowlim:highlim))
			IF (sumh EQ 0) THEN sumh = sumt
			TOFfactor(iphase) = sumt/sumh
		ENDFOR
		IF iprint THEN PRINT,'sumh=',sumh,' sumt=',sumt
	ENDELSE

	IF iprint THEN PRINT,'scattering angles and chel OK'

;-------------------------
;	Parameters
;-------------------------

	lambda = par2(0)
	IF (lambda GT 3.5) AND (lambda LE 5.0) THEN lambda=4.838 $
	ELSE IF (lambda LT 3.5) THEN lambda=3.12
	IF (lambda GT 5.0) THEN lambda=5.9
	IF (numor GE 8434 AND numor LT 73568) THEN lambda=3.02
	IF iprint THEN PRINT,'lambda=',lambda

	p_buf=FLTARR(50)
	par_txt_buf=STRARR(50)
	p_buf(0)  = numor	  & par_txt_buf(0)  = ' 0 Numor 		  ='
	p_buf(1)  = nspectra	  & par_txt_buf(1)  = ' 1 Number of spectra	  ='
	p_buf(2)  = nph_hytec	  & par_txt_buf(2)  = ' 2 Number of phases	  ='
	p_buf(3)  = 1.  	  & par_txt_buf(3)  = ' 3 Number of runs	  ='
	p_buf(4)  = lambda	  & par_txt_buf(4)  = ' 4 Wavelength (A)	  ='
	p_buf(5)  = par2(1)	  & par_txt_buf(5)  = ' 5 Chopper Speed (rpm)	  ='
	p_buf(6)  = par2(2)	  & par_txt_buf(6)  = ' 6 Number of Time Channels ='
	p_buf(7)  = par2(3)	  & par_txt_buf(7)  = ' 7 Channel Width (mcs)	  ='
	p_buf(8)  = TOF 	  & par_txt_buf(8)  = ' 8 TOF mode (0/1)	  ='
	p_buf(9)  = chel	  & par_txt_buf(9)  = ' 9 Elastic Peak Channel    ='
	p_buf(10) = par1(0)	  & par_txt_buf(10) = '10 Set-pt Temp Start	  ='
	p_buf(11) = par1(3)	  & par_txt_buf(11) = '11 Set-pt Temp End	  ='
	p_buf(12) = par1(1)	  & par_txt_buf(12) = '12 Regul Temp Start	  ='
	p_buf(13) = par1(4)	  & par_txt_buf(13) = '13 Regul Temp End	  ='
	p_buf(14) = par1(2)	  & par_txt_buf(14) = '14 Sample Temp Start	  ='
	p_buf(15) = par1(5)	  & par_txt_buf(15) = '15 Sample Temp End	  ='
	p_buf(16) = par2(76)	  & par_txt_buf(16) = '16 Bank1 Position	  ='
	p_buf(17) = par2(77)	  & par_txt_buf(17) = '17 Bank2 Position	  ='
	p_buf(18) = par2(78)	  & par_txt_buf(18) = '18 Bank3 Position	  ='
	p_buf(19) = par2(79)	  & par_txt_buf(19) = '19 Bank4 Position	  ='
	p_buf(20) = par2(80)	  & par_txt_buf(20) = '20 Upper Sample Rotation   ='
	p_buf(21) = par2(81)	  & par_txt_buf(21) = '21 Lower Sample Rotation   ='
	p_buf(22) = par1(17)	  & par_txt_buf(22) = '22 Z Flipper Current	  ='
	p_buf(23) = par1(18)	  & par_txt_buf(23) = '23 Z Correction Current    ='
	p_buf(24) = par1(35)	  & par_txt_buf(24) = '24 X Flipper Current	  ='
	p_buf(25) = par1(36)	  & par_txt_buf(25) = '25 X Correction Current    ='
	p_buf(26) = par1(53)	  & par_txt_buf(26) = '26 Y Flipper Current	  ='
	p_buf(27) = par1(54)	  & par_txt_buf(27) = '27 Y Correction Current    ='
	p_buf(28) = ratioZ	  & par_txt_buf(28) = '28 Z Flipping ratio	  ='
	p_buf(29) = ratioX	  & par_txt_buf(29) = '29 X Flipping ratio	  ='
	p_buf(30) = ratioY	  & par_txt_buf(30) = '30 Y Flipping ratio	  ='
	p_buf(31) = run_time	  & par_txt_buf(31) = '31 Run Time (seconds)	  ='
	p_buf(32) = time1	  & par_txt_buf(32) = '32 Start time (hours)	  ='
	p_buf(33) = time2	  & par_txt_buf(33) = '33 End Time (hours)	  ='
	p_buf(34) = par2(70)	  & par_txt_buf(34) = '34 Mono1 (psiA)  	  ='
	p_buf(35) = par2(71)	  & par_txt_buf(35) = '35 Mono2 (psiB)  	  ='
	p_buf(36) = par2(72)	  & par_txt_buf(36) = '36 Mono3 (psiC)  	  ='
	p_buf(37) = par2(73)	  & par_txt_buf(37) = '37 Mono4 (thetaA)	  ='
	p_buf(38) = par2(74)	  & par_txt_buf(38) = '38 Mono5 (thetaB)	  ='
	p_buf(39) = par2(75)	  & par_txt_buf(39) = '39 Mono6 (thetaC)	  ='
	p_buf(40) = TOFfactor(0)  & par_txt_buf(40) = '40 Z  TOFfactor  	  ='
	p_buf(41) = TOFfactor(1)  & par_txt_buf(41) = '41 Z1 TOFfactor  	  ='
	p_buf(42) = TOFfactor(2)  & par_txt_buf(42) = '42 X  TOFfactor  	  ='
	p_buf(43) = TOFfactor(3)  & par_txt_buf(43) = '43 X1 TOFfactor  	  ='
	p_buf(44) = TOFfactor(4)  & par_txt_buf(44) = '44 Y  TOFfactor  	  ='
	p_buf(45) = TOFfactor(5)  & par_txt_buf(45) = '45 Y1 TOFfactor  	  ='
	p_buf(46) = dratioZ	  & par_txt_buf(46) = '46 Z dFlipping ratio	  ='
	p_buf(47) = dratioX	  & par_txt_buf(47) = '47 X dFlipping ratio	  ='
	p_buf(48) = dratioY	  & par_txt_buf(48) = '48 Y dFlipping ratio	  ='
	p_buf(49) = par2(82)	  & par_txt_buf(49) = '49 Polariser Angle	  ='
	
	IF (iprint GT 0) THEN PRINT,'parameters assigned OK'

; 22/9/98 Problem with bank4 - recabled using coder 5 cables. Result is 10.21 
; degree shift in coder reading
; 15/10/98 Looking at magnetic Bragg peak in UCu5, another 1.2 degree shift is
; required to bring bank4 peak into agreement with bank1

	IF (numor GE 30721 AND numor LE 31064) THEN p_buf(19) = p_buf(19) + 10.21 - 1.2

	END

;-----------------------------------------------------------------------------

	2: BEGIN

	IF (iprint GT 0) THEN PRINT,'D7: Data format 2 -> 2003 ->'

	inst          = STRMID(text,0,4)
	save_time     = STRMID(text,14,18)
	main_title    = STRTRIM(exper(0),2)
	sub_title     = STRTRIM(exper(1),2)
	user          = STRMID(exper(2),0,8)
	local_contact = STRMID(exper(2),8,8)
	start_time    = STRMID(exper(3),0,18)
	stop_time     = STRMID(exper(3),20,18)
	scan_type     = STRMID(exper(3),38,9)
	pol_mode      = STRMID(exper(3),47,5)
	Time_mode     = STRMID(exper(3),52,6)


	IF iprint THEN BEGIN
		PRINT, 'inst         = ', inst
		PRINT, 'main_title   = ', main_title
		PRINT, 'sub_title    = ', sub_title
		PRINT, 'user         = ', user
		PRINT, 'local_contact= ', local_contact
		PRINT, 'start_time   = ', start_time
		PRINT, 'stop_time    = ', stop_time
		PRINT, 'save_time    = ', save_time
		PRINT, 'scan_type    = ', scan_type
		PRINT, 'pol_mode     = ', pol_mode
		PRINT, 'Time_mode    = ', Time_mode
	ENDIF

	CASE pol_mode OF
		' NOP ': nphases = 1
		'  ZP ': nphases = 2
		'XYZP ': nphases = 6
	ENDCASE

	nspectra = cnt/nphases

	IF (scan EQ 1) THEN TOFformat=0 ELSE TOFformat=1

	IF (NOT TOFformat) THEN BEGIN			; no TOF
		IF iprint THEN PRINT,'non-TOF data'
		TOF       = 0
		nspectra  = cnt/nphases
		nchannels = 1
		c_tof     = WOUT
	ENDIF ELSE BEGIN	; with TOF
		TOF       = 1
		nsp_tof   = scan
		nph_tof   = nphases
		nspectra  = nsp_tof/nphases
		nchannels = FIX(par1(2))
		c_tof     = WOUT
	ENDELSE

	IF iprint THEN PRINT,'nspectra=',nspectra,' nphases=',nphases,' nchannels=',nchannels
	IF iprint THEN PRINT,'Data read OK from ', nspectra, 'detectors'

;------------------------------------
;	Monitor Values in n
;------------------------------------

	n_buf      = FLTARR(4,7)
	n_buf(*,0) = 0.			; first line is zero
	n_buf(0,1) = param(14)		; Time      Z up-up    
	n_buf(0,2) = param(24)	 	; Time      Z up-down
	n_buf(0,3) = param(34)   		; Time      X up-up    
	n_buf(0,4) = param(44)     	; Time      X up-down
	n_buf(0,5) = param(54)		; Time      Y up-up 
	n_buf(0,6) = param(64)		; Time      Y up-down
	n_buf(1,1) = param(16)    	; Monitor 1 Z up-up
	n_buf(1,2) = param(26) 		; Monitor 1 Z up-down 
	n_buf(1,3) = param(36) 		; Monitor 1 X up-up
	n_buf(1,4) = param(46)		; Monitor 1 X up-down
	n_buf(1,5) = param(56)		; Monitor 1 Y up-up 
	n_buf(1,6) = param(66)		; Monitor 1 Y up-down
	n_buf(2,1) = param(17)    	; Monitor 2 Z up-up
	n_buf(2,2) = param(27)	 	; Monitor 2 Z up-down 
	n_buf(2,3) = param(37) 		; Monitor 2 X up-up
	n_buf(2,4) = param(47)		; Monitor 2 X up-down
	n_buf(2,5) = param(57)		; Monitor 2 Y up-up 
	n_buf(2,6) = param(67)		; Monitor 2 Y up-down
	n_buf(3,1) = param(15)		; Handshake Z up-up
	n_buf(3,2) = param(25)		; Handshake Z up-down
	n_buf(3,3) = param(35)		; Handshake X up-up
	n_buf(3,4) = param(45)		; Handshake X up-down
	n_buf(3,5) = param(55)		; Handshake Y up-up
	n_buf(3,6) = param(65)		; Handshake Y up-down

	IF iprint THEN PRINT,'monitor assigned OK'

;--------------------------------
;	Calculate Flipping Ratios
;--------------------------------

	IF n_buf(0,2) EQ 0. THEN BEGIN				;NOP
		ratioZ = 0.	& dratioZ = 0.
		ratioX = 0.	& dratioX = 0.
		ratioY = 0.	& dratioY = 0.
	ENDIF ELSE IF n_buf(0,3) EQ 0. THEN BEGIN		;ZPO
		FR_calculate, n_buf, 1, ratioZ, dratioZ
		ratioX = 0. & dratioX = 0.
		ratioY = 0. & dratioY = 0.
	ENDIF ELSE BEGIN					;XYZ
		FR_calculate, n_buf, 1, ratioZ, dratioZ
		FR_calculate, n_buf, 3, ratioX, dratioX
		FR_calculate, n_buf, 5, ratioY, dratioY
	ENDELSE
	IF iprint THEN PRINT,'Flipping Ratios OK'

;------------------------------
;	Calculate Running Times
;------------------------------

	h1=LONG(0)	& m1=LONG(0)	& s1=LONG(0)
	h2=LONG(0)	& m2=LONG(0)	& s2=LONG(0)
	IF (start_time NE '' AND stop_time NE '') THEN BEGIN
		READS, start_time, FORMAT='(I2,8X,I2,1X,I2,1X,I2)', da1, h1, m1, s1
		READS, stop_time, FORMAT='(I2,8X,I2,1X,I2,1X,I2)', da2, h2, m2, s2
		time1=FLOAT(3600*h1+60*m1+s1)/3600.+24.*(da1 - 1)
		time2=FLOAT(3600*h2+60*m2+s2)/3600.+24.*(da2 - 1)
		IF (h2 GE h1) THEN run_time=FLOAT(3600*(h2-h1)+60*(m2-m1)+(s2-s1)) $
			      ELSE run_time=FLOAT(3600*(24+h2-h1)+60*(m2-m1)+(s2-s1))
	ENDIF ELSE BEGIN
		time1=0.
		time2=0.
		run_time=0.
	ENDELSE

;------------------------------

	IF (TOF EQ 0) THEN BEGIN
		IF iprint THEN PRINT,'TOF = 0'
		w_buf = reform(c_tof,nspectra,nphases) 
		x_buf = par1(6:nspectra + 5)
		IF (nphases EQ 1) THEN y_buf = LONG(numor) $
				  ELSE y_buf = INDGEN(nphases)
		z_buf = x_buf
		chel  = 0.
		norm  = 1.
	ENDIF ELSE BEGIN
		IF iprint THEN PRINT,'TOF = 1'
		w_buf = c_tof
		x_buf = INDGEN(nchannels) + 1
		y_buf = FLTARR(nsp_tof)
		FOR iph = 0, nphases - 1 DO $
		y_buf(iph*nspectra:iph*nspectra + nspectra - 3) = par1(6:nspectra + 3)
		z_buf = y_buf
		w_tmp = LONARR(nchannels)
		FOR i = 0, nspectra - 1 DO w_tmp(*) = w_tmp(*) + w_buf(*,i)
		Imax = MAX(w_tmp,ichel)
		chel = FLOAT(ichel)
	ENDELSE

	IF iprint THEN PRINT,'scattering angles and chel OK'

;-------------------------
;	Parameters
;-------------------------

	lambda=par1(0)
	IF (lambda GT 3.5) AND (lambda LE 5.0) THEN lambda=4.838
	IF (lambda LT 3.5) THEN lambda=3.12
	IF (lambda GT 5.0) THEN lambda=5.98

	IF (iprint GT 0) THEN PRINT,'lambda=',lambda

	p_buf       = FLTARR(47)
	par_txt_buf = STRARR(47)
	
	p_buf(0)  = numor	  & par_txt_buf(0)  = ' 0 Numor 			 ='
	p_buf(1)  = nspectra	  & par_txt_buf(1)  = ' 1 Number of spectra		 ='
	p_buf(2)  = nphases	  & par_txt_buf(2)  = ' 2 Number of phases		 ='
	p_buf(3)  = 1.  	  & par_txt_buf(3)  = ' 3 Number of runs		 ='
	p_buf(4)  = lambda	  & par_txt_buf(4)  = ' 4 Wavelength (A)		 ='
	p_buf(5)  = par1(1)	  & par_txt_buf(5)  = ' 5 Chopper Speed (rpm)		 ='
	p_buf(6)  = par1(2)	  & par_txt_buf(6)  = ' 6 Number of Time Channels	 ='
	p_buf(7)  = par1(3)	  & par_txt_buf(7)  = ' 7 Channel Width (mcs)		 ='
	p_buf(8)  = TOF 	  & par_txt_buf(8)  = ' 8 TOF mode (0/1)		 ='
	p_buf(9)  = chel	  & par_txt_buf(9)  = ' 9 Elastic Peak Channel  	 ='
	p_buf(10) = param(0)	  & par_txt_buf(10) = '10 Set-pt Temp Start		 ='
	p_buf(11) = param(3)	  & par_txt_buf(11) = '11 Set-pt Temp End		 ='
	p_buf(12) = param(1)	  & par_txt_buf(12) = '12 Regul Temp Start		 ='
	p_buf(13) = param(4)	  & par_txt_buf(13) = '13 Regul Temp End		 ='
	p_buf(14) = param(2)	  & par_txt_buf(14) = '14 Sample Temp Start		 ='
	p_buf(15) = param(5)	  & par_txt_buf(15) = '15 Sample Temp End		 ='
	p_buf(16) = par1(86)	  & par_txt_buf(16) = '16 Bank1 Position		 ='
	p_buf(17) = par1(87)	  & par_txt_buf(17) = '17 Bank2 Position		 ='
	p_buf(18) = par1(88)	  & par_txt_buf(18) = '18 Bank3 Position		 ='
	p_buf(19) = par1(89)	  & par_txt_buf(19) = '19 Bank4 Position		 ='
	p_buf(20) = par1(90)	  & par_txt_buf(20) = '20 Upper Sample Rotation 	 ='
	p_buf(21) = par1(91)	  & par_txt_buf(21) = '21 Lower Sample Rotation 	 ='
	p_buf(22) = param(18)	  & par_txt_buf(22) = '22 Z Flip Current		 ='
	p_buf(23) = param(19)	  & par_txt_buf(23) = '23 Z Cor_Z Current		 ='
	p_buf(24) = param(23)	  & par_txt_buf(24) = '24 Z Cor_X Current		 ='
	p_buf(25) = param(38)	  & par_txt_buf(25) = '25 X Flip Current		 ='
	p_buf(26) = param(39)	  & par_txt_buf(26) = '26 X Cor_Z Current		 ='
	p_buf(27) = param(43)	  & par_txt_buf(27) = '27 X Cor_X Current		 ='
	p_buf(28) = param(58)	  & par_txt_buf(28) = '28 Y Flip Current		 ='
	p_buf(29) = param(59)	  & par_txt_buf(29) = '29 Y Cor_Z Current		 ='
	p_buf(30) = param(63)	  & par_txt_buf(30) = '30 Y Cor_X Current		 ='
	p_buf(31) = ratioZ	  & par_txt_buf(31) = '31 Z Flipping ratio		 ='
	p_buf(32) = ratioX	  & par_txt_buf(32) = '32 X Flipping ratio		 ='
	p_buf(33) = ratioY	  & par_txt_buf(33) = '33 Y Flipping ratio		 ='
	p_buf(34) = run_time	  & par_txt_buf(34) = '34 Run Time (seconds)		 ='
	p_buf(35) = time1	  & par_txt_buf(35) = '35 Start (hrs since 1st of month) ='
	p_buf(36) = time2	  & par_txt_buf(36) = '36 End	(hrs since 1st of month) ='
	p_buf(37) = par1(80)	  & par_txt_buf(37) = '37 Mono1 (psiA)  		 ='
	p_buf(38) = par1(81)	  & par_txt_buf(38) = '38 Mono2 (psiB)  		 ='
	p_buf(39) = par1(82)	  & par_txt_buf(39) = '39 Mono3 (psiC)  		 ='
	p_buf(40) = par1(83)	  & par_txt_buf(40) = '40 Mono4 (thetaA)		 ='
	p_buf(41) = par1(84)	  & par_txt_buf(41) = '41 Mono5 (thetaB)		 ='
	p_buf(42) = par1(85)	  & par_txt_buf(42) = '42 Mono6 (thetaC)		 ='
	p_buf(43) = dratioZ	  & par_txt_buf(43) = '43 Z dFlipping ratio		 ='
	p_buf(44) = dratioX	  & par_txt_buf(44) = '44 X dFlipping ratio		 ='
	p_buf(45) = dratioY	  & par_txt_buf(45) = '45 Y dFlipping ratio		 ='
	p_buf(46) = par1(92)	  & par_txt_buf(46) = '46 Polariser Angle		 ='
	
	IF iprint THEN PRINT,'parameters assigned OK'

	END

;-----------------------------------------------------------------------------
	
	ENDCASE

;-----------------------------------------------------------------------------
;*****************************************************************************

	WOUT = w_buf
	PP   = p_buf
	PTXT = par_txt_buf
	NN   = n_buf
	XX   = x_buf
	YY   = y_buf
	ZZ   = z_buf
	WT   = sub_title
	OT   = inst+' '+main_title
	
	IF (TOF EQ 0) THEN BEGIN
		XT = 'Scattering Angle'
		IF (nphases EQ 1) THEN YT = 'Counts' $
		ELSE BEGIN
			YT = 'Phase'
			ZT = 'Counts'
		ENDELSE
	ENDIF ELSE BEGIN
		XT = 'Time Channels'
		YT = 'Scattering Angle'
		ZT = 'Counts'
	ENDELSE

	IF iprint THEN PRINT,'rdid_d7: finished'

	RETURN
	END
	
PRO rdid_d20, INST,numor,nvers,text,exper,scan,cnt,WOUT,vparm,param,par1,par2,par3,par4,par5,$
	                 WT ,XT ,YT ,ZT ,OT ,DATE ,PP ,PTXT ,XX ,YY ,ZZ ,NN ,PV ,EE

common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
  common d20, bad_d20 ,flag_d20, wav_d20, psd_d20  

  if !version.release ge '5.0' then ii=execute('FORWARD_FUNCTION RDDAT')

  WOUT=WOUT>0
  nd=cnt
  DATI     = strmid(exper(5),20,18)
  IF strpos(DATI,'Stop',0) GE 0 THEN BEGIN
    DATI='16-Apr-02 20:00:00'
  ENDIF
  day      = strmid(DATI,0,2)
  month    = strmid(DATI,3,3)
  year     = strmid(DATI,7,2)
  hour     = strmid(DATI,10,2)
  minute   = strmid(DATI,13,2)
  second   = strmid(DATI,16,2)
  charpos=0
  IF  STRMID(year,0,1) GT '9' OR  STRMID(year,0,1) LT '0'  OR STRMID(year,1,1) GT '9' OR  STRMID(year,1,1) LT '0' THEN BEGIN 
			  	charpos=strpos(text(0),':',0)-2
			  	hour=strmid(text(0),charpos,2)
		  		hour=strmid('00'+hour,strlen(hour),2)
				charpos=charpos+3
			  	secpos=strpos(text(0),':',charpos)
				minute=strmid(text(0),charpos,secpos-charpos)
			  	minute=strmid('00'+minute,strlen(minute),2)
			  	second=strmid(text(0),secpos+1,2)
			  	second=strmid('00'+second,strlen(second),2)
			  	charpos=strpos(text(0),' ',secpos)+1
		  		secpos=strpos(text(0),'-',charpos)
				day=strmid(text(0),charpos,secpos-charpos)
			  	day=strmid('00'+day,strlen(day),2)
			  	charpos=secpos+1
		 	        secpos=strpos(text(0),"-",charpos)
			  	month=strmid(text(0),charpos,secpos-charpos)
			  	month=strmid('   '+month,strlen(month),3)
			  	charpos=secpos+1
			  	year=strmid(text(0),charpos,2)
			  	year=strmid('00'+year,strlen(year),2)
				DATI=day+"-"+month+"-"+year+" "+hour+":"+minute+":"+second
  ENDIF ELSE BEGIN
  	hour     = strmid(DATI,10,2)
  	minute   = strmid(DATI,13,2)
  	second   = strmid(DATI,16,2)
  ENDELSE
  IF nd EQ 1600 THEN BEGIN
	IF total(WOUT(128:1599,*)) EQ 0 AND year EQ "96" THEN BEGIN     ; old PSD 128 cells
            	WOUT  =  WOUT(0:127,*)
            	nd=128
            	if n_elements(vparm) lt 26.*scan then vparm=fltarr(26,scan)
  	ENDIF
  ENDIF
  WOUT=float(WOUT)
  IF N_ELEMENTS(vparm) GE  1 THEN vparm=vparm(*,0:N_elements(WOUT(0,*))-1)
  nj = N_ELEMENTS(wout(0,*))
  EE  = WOUT/sqrt(1+WOUT)
  IF n_elements(par5) eq 0 THEN BEGIN                          ; Numor bug fix
	       par5=par4
	       par4=par3
	       par3=0
  ENDIF
  IF year GT 60 THEN par1(51)=par1(51)/1000000.                ; CntTime in sec (raw data before 2000: microsec)
  IF n_elements(par5) eq 25 THEN BEGIN                         ; Numor bug fix
	   	par5=[par5(0:19),0.,0.,0.,0.,0.,par5(20:24)]
       	param=[0.,param]
     	FOR i=0,nj-1 Do wout(*,i)=[wout(nd-1,i),wout(0:nd-2,i)] 
  ENDIF
  CntTime  = par1(51)
  proposal = strmid(exper(2),20 ,9)
  WT  =  ' '+strcompress(strmid(exper(0),20,40)) $
   	    +' '+strcompress(strmid(exper(3),20,40))
  OT = INST(0)  +' '+ DATI $
     	        +' User '+strmid(exper(1),20,8)$
     	  	    +' L.C.' +strmid(exper(4),20,7)$
		        +' Run'  +strcompress(string(numor)) 
  CASE month OF
       'Jan':  MON=1
       'Feb':  MON=2
       'Mar':  MON=3
       'Apr':  MON=4
       'May':  MON=5
       'Jun':  MON=6
       'Jul':  MON=7
       'Aug':  MON=8
       'Sep':  MON=9
       'Oct':  MON=10
       'Nov':  MON=11
       'Dec':  MON=12
  ENDCASE					
  IF par5(25) LT 963 THEN BEGIN                                  ; evt. missing reactor cycle
	  	par5(25)=year*10+ROUND(((mon-1.)*30.5+(day-1.))/366.*5.)
  ENDIF
  CYCLE = long(par5(25))
  IF N_ELEMENTS(inf_d20) EQ 0 THEN BEGIN
      IF CYCLE NE long(year*10+1+ROUND(((mon-1.)*30.5+(day-1.))/366.*4.)) THEN PRINT,"Cycle is",cycle," but probably should be",long(year*10+1+ROUND(((mon-1.)*30.5+(day-1.))/366.*4.))
  ENDIF
  if year le 60 then century=20 else century=19
  jul_day=julday(mon,day,century*100+year)-julday(1,1,1970)
  jul_sec=hour*3600.0+minute*60.0+second
  ;****** CHECK WAVELENGTH ************
  IF ROUND(10*par5(3)) LE 99 AND ROUND(10*par5(3)) GE 25 THEN BEGIN
      wav=2.41
      IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
        IF ROUND(par5(1)) NE   1 THEN PRINT,'2.52A, HOPG-Monochromator, Filters should be in but they are not'
        IF ROUND(par5(4)) NE  44 THEN PRINT,'2.52A, HOPG-Monochromator, TakeOff should be 44deg but it is at',ROUND(par5(4))
        IF ROUND(par5(5)) NE 120 THEN PRINT,'2.52A, HOPG-Monochromator, MonoChanger should be 120deg but it is at',ROUND(par5(5))
      ENDIF
  ENDIF ELSE BEGIN
      IF ROUND(10*par5(3)) LE 24 AND ROUND(10*par5(3)) GE  20 THEN BEGIN
        wav=2.41
        IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
          IF ROUND(par5(1)) NE   1 THEN PRINT,'2.41A, HOPG-Monochromator, Filters should be in but they are not'
          IF ROUND(par5(4)) NE  42 THEN PRINT,'2.41A, HOPG-Monochromator, TakeOff should be 42 deg but it is at',ROUND(par5(4))
          IF ROUND(par5(5)) NE 120 THEN PRINT,'2.41A, HOPG-Monochromator, MonoChanger should be 120 deg but it is at',ROUND(par5(5))
        ENDIF
      ENDIF ELSE BEGIN
        IF ROUND(10*par5(3)) LE 19 AND ROUND(10*par5(3)) GE 14 THEN BEGIN
          wav=1.30
          IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
            IF ROUND(par5(1)) NE   0 THEN PRINT,'1.36A, Cu-Monochromator, Filters should be out but they are not'
            IF ROUND(par5(4)) NE  44 THEN PRINT,'1.36A, Cu-Monochromator, TakeOff should be 44 deg but it is at',ROUND(par5(4))
            IF ROUND(par5(5)) NE 210 THEN PRINT,'1.36A, Cu-Monochromator, MonoChanger should be 210 deg but it is at',ROUND(par5(5))
          ENDIF
        ENDIF ELSE BEGIN
          IF ROUND(10*par5(3)) LE 13 AND ROUND(10*par5(3)) GE 10 THEN BEGIN
            wav=1.30
            IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
              IF ROUND(par5(1)) NE   0 THEN PRINT,'1.30A, Cu-Monochromator, Filters should be out but they are not'
              IF ROUND(par5(4)) NE  42 THEN PRINT,'1.30A, Cu-Monochromator, TakeOff should be 42 deg but it is at',ROUND(par5(4))
              IF ROUND(par5(5)) NE 210 THEN PRINT,'1.30A, Cu-Monochromator, MonoChanger should be 210 deg but it is at',ROUND(par5(5))
            ENDIF
          ENDIF ELSE BEGIN
            IF ROUND(100*par5(3)) LE 99 AND ROUND(100*par5(3)) GE 91 THEN BEGIN
              wav=0.94
              IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
                IF ROUND(par5(1)) NE   0 THEN PRINT,'0.94A, Cu-Monochromator, Filters should be out but they are not'
                IF ROUND(par5(4)) NE  30 THEN PRINT,'0.94A, Cu-Monochromator, TakeOff should be 30 deg but it is at',ROUND(par5(4))
                IF ROUND(par5(5)) NE 210 THEN PRINT,'0.94A, Cu-Monochromator, MonoChanger should be 210 deg but it is at',ROUND(par5(5))
              ENDIF
            ENDIF ELSE BEGIN
              IF ROUND(100*par5(3)) LE 90 AND ROUND(100*par5(3)) GE 85 THEN BEGIN
                wav=0.88
                IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
                  IF ROUND(par5(1)) NE   0 THEN PRINT,'0.88A, Cu-Monochromator, Filters should be out but they are not'
                  IF ROUND(par5(4)) NE  28 THEN PRINT,'0.88A, Cu-Monochromator, TakeOff should be 28 deg but it is at',ROUND(par5(4))
                  IF ROUND(par5(5)) NE 210 THEN PRINT,'0.88A, Cu-Monochromator, MonoChanger should be 210 deg but it is at',ROUND(par5(5))
                ENDIF
              ENDIF ELSE BEGIN
                IF ROUND(100*par5(3)) LE 84 AND ROUND(100*par5(3)) GE 70 THEN BEGIN
                  wav=0.82
                  IF N_ELEMENTS(inf_d20) LE 1 THEN BEGIN
                    IF ROUND(par5(1)) NE   0 THEN PRINT,'0.82A, Cu-Monochromator, Filters should be out but they are not'
                    IF ROUND(par5(4)) NE  26 THEN PRINT,'0.82A, Cu-Monochromator, TakeOff should be 28 deg but it is at',ROUND(par5(4))
                    IF ROUND(par5(5)) NE 210 THEN PRINT,'0.82A, Cu-Monochromator, MonoChanger should be 210 deg but it is at',ROUND(par5(5))
                  ENDIF
                ENDIF ELSE BEGIN
                  IF N_ELEMENTS(inf_d20) LE 1 THEN PRINT,'Do you really have a wavelength of',par5(3),' Angstroem?'
                ENDELSE
              ENDELSE
            ENDELSE
          ENDELSE
        ENDELSE
      ENDELSE
  ENDELSE
  ;WIDGET_CONTROL,wid1,/APPEND,SET_VALUE='Reading Flags'
  ;########VVVVVVV Reading several flags form rdid.flag VVVVVVV######################
  IF N_ELEMENTS(flag_d20) LT 8 THEN flag,/noprint,old=flag_d20
		bad_flag=     flag_d20(0)
		interpol_flag=flag_d20(1)
		ang_flag=     flag_d20(2)
		wav_flag=     flag_d20(3)
		normalize=    flag_d20(4)
		float_flag=   flag_d20(5)
		eff_flag  =   flag_d20(6)
		cor_flag=     flag_d20(7)
  ;########VVVVVV EFFICIENCY/Angle CALIBRATION VVVVVVVV######################
  IF N_ELEMENTS(inf_d20) EQ 0 THEN BEGIN
      inf_d20='autod20.cal'
  ENDIF ELSE IF N_ELEMENTS(inf_d20) EQ 1 THEN IF STRCOMPRESS(inf_d20,/RE) EQ '0' THEN inf_d20 ='autod20.cal'
  IF N_ELEMENTS(cal_d20) EQ 0 THEN BEGIN
      cal_d20=0
  ENDIF  
  default_cal = 0
  IF n_elements(inf_d20) EQ 0 THEN BEGIN
  		default_cal=1 
  ENDIF ELSE BEGIN
  	IF inf_d20(0) EQ 'autod20.cal' THEN BEGIN
		default_cal=1
  	ENDIF ELSE BEGIN
		IF N_ELEMENTS(inf_d20) GE 3 THEN BEGIN
			IF inf_d20(2) EQ 'autod20.cal' THEN BEGIN
				default_cal=1
			ENDIF
		ENDIF
  	ENDELSE
  ENDELSE
  IF default_cal EQ 1 AND (ang_flag EQ 1 OR eff_flag EQ 1) THEN BEGIN
	IF N_ELEMENTS(inf_d20) EQ 0 THEN inf_d20=['autod20.cal','not loaded','autod20.cal']
	count=N_ELEMENTS(inf_d20)
	IF count LT 4 THEN BEGIN
    	PRINT,'Looking up for calibration files in ',PATHCAL
		tmf     =PATHCAL
		current_dir=''
		cd,current=current_dir,tmf
		tmp     =findfile(inst(0)+'_????_??_??????.???',count=tmc) 
		cd,current_dir
		IF tmc GT 0 THEN filelist=strmid(tmp,strpos(tmp(0),inst(0)),22) else print,"Are you really on the right workstation (d20sgi.ill.fr)?"
		IF N_ELEMENTS(filelist) LE 0 THEN BEGIN
			PRINT,'NO auto-calibration file found!'
			inf_d20=['','','']
			default_cal=0
			filelist=''
		ENDIF else print,'Files in ~lambda/CALIBRATION ',filelist
        IF N_ELEMENTS(inf_d20) LT 2 THEN inf_d20=['autod20.cal','']
		inf_d20=[inf_d20(0:1),'autod20.cal',STRING(filelist)]
	ENDIF ELSE filelist= inf_d20(3:count-1)  ; NEW calibration file naming
    tmplist=filelist	
    tmp=where(STRMID(filelist,strlen(inst(0))+16,3) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,3),count)
    IF count LE 0 THEN tmp=where(STRMID(filelist,strlen(inst(0))+16,2) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,2),count)
    IF count le 0 then BEGIN
    	tmp=where(STRMID(filelist,strlen(inst(0))+16,1) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,1),count)
        IF count le 0 then tmp=where(STRMID(filelist,strlen(inst(0))+16,1) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*((wav+1)<9))),1,1),count)
        IF count le 0 then tmp=where(STRMID(filelist,strlen(inst(0))+16,1) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*((wav-1)>0))),1,1),count)
    ENDIF
    IF count gt 0 then begin
    	tmplist=filelist(tmp)
    	tmp=where(tmplist LE inst(0)+'_'+strcompress(string(century),/re)+strcompress(string(year),/re)+'_'+STRMID(STRING(f='(I3)',100+mon),1,2)+'_'+STRMID(STRING(f='(I7)',1000000+numor),1,6)+'.'+STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,3),count)
    ENDIF
	IF count GT 0 THEN tmp=MAX(tmplist(tmp)) ELSE BEGIN
      	PRINT,'Apparently there is no calibration for ',wav,' Angstroem ',STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,3)
		tmp=WHERE(filelist LE inst(0)+'_'+strcompress(string(century),/re)+strcompress(string(year),/re)+'_'+STRMID(STRING(f='(I3)',100+mon),1,2)+'_'+STRMID(STRING(f='(I7)',1000000+numor),1,6)+'.'+STRMID(STRING(f='(I4)',1000+ROUND(100*par5(3))),1,3),count)
		IF count GT 0 THEN tmp=MAX(filelist(tmp)) ELSE tmp=MIN(filelist)
        PRINT,tmp,' will be taken for calibration'
    ENDELSE
	calfile=STRCOMPRESS(tmp,/re)
  ENDIF
  ON_IOERROR,badmisread
		IF bad_flag EQ 1  OR  bad_flag NE 1 THEN BEGIN
			count=N_ELEMENTS(psd_d20)
			;help,psd_d20
			IF count LT 1 THEN psd_d20=['empty']
			IF count LT 2 THEN BEGIN
				tmf     =PATHCAL ; P_LAMBDA(0)+'/BAD_CELLS/'
				cd,current=current_dir,tmf
				tmp     =findfile(tmf+inst(0)+'_????_??_??????.bad',count=tmc) ; NEW  file names
				cd,current_dir
				if tmc gt 0 then badlist=strmid(tmp,strpos(tmp(0),inst(0)),22) else print,"Are you really on the right workstation (d20sgi.ill.fr)?"
				IF N_ELEMENTS(badlist) LE 0 THEN BEGIN
					PRINT,'NO bad cells file found!'
					psd_d20=['empty']
					bad_flag=0
					badlist=''
				ENDIF ELSE BEGIN
            		IF bad_flag EQ 1 THEN print,'Creating new bad cells filelist'
            		IF bad_flag EQ 1 THEN PRINT,' ',badlist
          		ENDELSE
				psd_d20=[string(psd_d20(0)),badlist]
				;help,psd_d20
			ENDIF ELSE badlist=psd_d20(1:count-1)
       		tmp=where(badlist LE inst(0)+'_'+strcompress(string(century),/re)+strcompress(string(year),/re)+'_'+STRMID(STRING(f='(I3)',100+mon),1,2)+'_'+STRMID(STRING(f='(I7)',1000000+numor),1,6)+'.'+'bad',count)
			IF count GT 0 THEN tmp=MAX(badlist(tmp)) ELSE  tmp=MIN(badlist)
 			IF psd_d20(0) NE tmp THEN BEGIN
            	psd_d20(0)=tmp
            	IF bad_flag EQ 1 THEN print,'data       : ',PATHCAL+psd_d20(0)
				bad_d20=rddat('data',PATHCAL,psd_d20(0),rddat_status,rddat_datp)
				PRINT,'Bad_cells file loaded : ',psd_d20(0)
            	IF bad_flag EQ 1 THEN print,'Bad cells: ',STRCOMPRESS(ROUND(bad_d20))
       		ENDIF
		ENDIF
  badmisread:
		IF wav_flag EQ 1 THEN BEGIN
	    count=N_ELEMENTS(wav_d20)
  	 	IF count LT 1 THEN wav_d20=[0.,0.,0.]
  	 	IF count LT 4 THEN BEGIN
					p_lambda,plambda
				    	tmf     =PLAMBDA+'/RIETVELD/'
					cd,current=current_dir,tmf
				    	tmp     =findfile(tmf+'?????????.d20',count=tmc)
					cd,current_dir
				    	if tmc gt 0 then filelist=LONG(strmid(tmp,strpos(tmp(0),'.d20')-9,9))
					    IF N_ELEMENTS(filelist) LE 0 THEN BEGIN
						    		  PRINT,'NO wavelength/zeroshift file found!'
					  	  				wav_d20=[0.,0.,0.]
					  	  				bad_flag=0
										    filelist=''
					  	 ENDIF else print,'Creating newwavelength/zeroshift file list',filelist
					  	 wav_d20=[wav_d20(0:2),filelist]
				ENDIF ELSE filelist=wav_d20(1:count-1)
				tmp=WHERE(filelist LE LONG(cycle)*1000000+numor,count)
				IF count GT 0 THEN tmp=MAX(filelist(tmp)) ELSE tmp=MIN(filelist)
				wav_d20(0)=LONG(tmp)
				OPENR,tmp,tmf+STRING(STRCOMPRESS(wav_d20(0),/REMOVE_ALL))+'.d20',/get_lun
				READF,tmp,line
				READF,tmp,wav_d20(1:2)
				FREE_LUN,tmp
		ENDIF
  ;help,inf_d20
  ;print,inf_d20
		IF N_ELEMENTS(inf_d20) ge 3 THEN IF N_ELEMENTS(calfile) GE 1 THEN IF calfile NE inf_d20(0) THEN IF inf_d20(2) EQ 'autod20.cal' THEN BEGIN
         inf_d20(2) = 'autochoice'
         P_DID_CALOD, INST(0),calfile, flg 
         inf_d20(2) = 'autod20.cal'
  ENDIF
  IF N_ELEMENTS(inf_d20) GE 2 THEN BEGIN
    IF STRMID(inf_d20(1),37,8) LT '3' THEN wavelength=FLOAT(STRMID(inf_d20(1),37,8)) 
  ENDIF ELSE wavelength=0
  ;####### Efficiency Correction by MULTIPLICATION with efficiency-CORRECTION data! ###########
  IF float_flag EQ 1 THEN WOUT=FLOAT(WOUT)
  IF n_elements(cal_d20(*,0)) eq nd AND eff_flag EQ 1 THEN BEGIN 
         OT=OT+'*'+inf_d20(0)
         FOR i=0,nj-1 DO FOR j=0,nd-1 DO BEGIN 
           wout(j,i)=wout(j,i)*cal_d20(j,0) 
           EE(j,i)= EE(j,i)*cal_d20(j,0) 
         ENDFOR 
  ENDIF
  IF n_elements(cal_d20(*,0)) eq 0 THEN cal_d20=0
  IF float_flag NE 1 THEN BEGIN
      IF eff_flag EQ 1 OR normalize GT 0 THEN PRINT,'Attention : Workspace is NOT floating point, but it should be!'
  ENDIF
  SequenceType    = par1(0) 
  StroboType      = par1(40) 
  NbOfSegments    = par1(1)  
	 PV = fltarr(31,scan)
	 NN = fltarr(scan)
	 FOR i=0,scan-1 DO BEGIN
	       PV(0:4,i)   = par1(0:4)	
	       PV(5,i)     = par1(51)		    				; CntTime in sec.
	       PV(6:7,i)   = par5(26:27)	  					; RtrPower,D19
	       PV(8,i)     = par5(14)		    				; OS
	       PV(9:19,i)  = param(0:10) 	 					; SampEnv,TempValues,MotorValues,Voltmeter1
	       PV(20,i)    = (par5(12)<30)+(par5(13)<30) 	 	; hor.  MonoSlits
	       PV(21,i)    = (par5(10)<140)+(par5(11)<140) 	 	; vert. MonoSlits
	       PV(22,i)    = par5(18)+par5(19) 	 				; hor.  SampleSlits
	       PV(23,i)    = par5(16)+par5(17) 	 				; vert. SampleSlits
	       PV(24,i)    = 0.0		         				; TimeStep
	       PV(25:27,i) = par1(45:47)	  					; TimeSlice,TimeDelay,TimeWindow
	       PV(28,i)    = i             						; Slice_No or sub-numor, for scans it will contain the inner stepwidth (later on ...)
	       PV(29,i)    = numor
	       PV(30,i)    = par1(50)      						; monitor
	       NN(i)       = par1(50)
	   ENDFOR
	   IF nj LE 1 THEN NN = par1(50)
	   IF N_ELEMENTS(vparm) GE 1 THEN BEGIN
       IF N_ELEMENTS(vparm(*,0)) GE 26 THEN BEGIN                                 ; NOT Stroboscopie
	    PV(0:4,*)  = vparm(0:4,*) 
        PV(5:20,*) = vparm(6:21,*) 
        PV(5,*)    = PV(5,*)/1000000.                          ; CntTime in sec (not microsec)
		NN(*)= (vparm(5,*) EQ 0.)*NN(*)+vparm(5,*)
		PV(30,*)=nn(*)
      ENDIF ELSE BEGIN 
        IF N_ELEMENTS(vparm(*,0)) GE 5 THEN BEGIN
	         PV(25:27,*)= vparm(0:2,*)  
	         PV(5,*)    = vparm(4,*) /1000000.                          ; time in sec (not microsec)
		        NN(*)= (vparm(3,*) EQ 0.)*NN(*)/PV(5,0)*PV(5,*)+vparm(3,*) ; tot.monitor if slice-mon. zero 
          IF vparm(4,0) LT vparm(3,0) THEN BEGIN                     ; Monitor-time confusion in the reading of the 51st scaler
		          PV(5,*)    = vparm(3,*) /1000000.                        ; time in sec (not microsec)
					       NN(*)      = vparm(4,*)
            IF nvers EQ 2 THEN BEGIN
              brico=indgen(nj)*3
		  						  		brico=brico(WHERE(brico LT nj))
		            PV(26:27,brico)= vparm(0:1,brico)                                ; even more confusions
		            PV(25,brico)   = vparm(2,brico)                                  ; even more confusions
              brico=indgen(nj)*3+1
								    		brico=brico(WHERE(brico LT nj))
		            PV(25:26,brico)= vparm(1:2,brico)                                ; even more confusions
		            PV(27,brico)   = vparm(0,brico)                                  ; even more confusions
					  	    ENDIF
					  	  ENDIF
		        PV(30,*)=nn(*)
				    ENDIF
				  ENDELSE
				ENDIF
    IF cor_flag EQ 1 THEN IF Cycle LE 974 THEN IF cycle GE 973 THEN IF numor LE 20200 THEN IF numor GE 8451 THEN BEGIN
      IF StroboType EQ 0 THEN IF scan EQ 1 OR SequenceType NE 0 THEN BEGIN
	    p_lambda,plambda
        corname =PLAMBDA+'/MONITOR/'+STRCOMPRESS(STRING(LONG(cycle)), /REMOVE_ALL)+'.cor'
        OPENR,cor,corname,/get_lun
        READF,cor,corrections
        IF corrections GT 0 THEN BEGIN
	      cornum=fltarr(3,corrections)
	      READF,cor,cornum
		ENDIF
		FREE_LUN,cor
		corind=WHERE(cornum(0,*) EQ numor,corrections)
		IF corrections EQ 1 THEN BEGIN
			PRINT,'Corrections will be applied: ',corname
			cornum=cornum(1:2,corind)
        	PV(30,*)=PV(30,*)-cornum(0)
        	NN      =NN      -cornum(0)
        	PV(5,*) =PV(5,*) -cornum(1)
		ENDIF ELSE PRINT,'Looked for corrections but found nothing',corrections
      ENDIF
	ENDIF
	MotPar=fltarr(6)
	MotTxt=['2*Theta/deg.     ','Omega  /deg.     ',$
	        'Chi    /deg.     ','Phi    /deg.     ',$
		    'Tr.1   /mm (horz)','Tr.2   /mm (vert)']
	FOR i=0,5 DO BEGIN 
		IF par1(12+i*5) EQ 0.0 THEN BEGIN
	       MotPar(i)=param(4+i) 
	       MotTxt(i)=MotTxt(i)+'       '
	    ENDIF ELSE BEGIN
	       MotPar(i)=par1(12+i*5)
	       MotTxt(i)=MotTxt(i)+'       '
      	ENDELSE
	ENDFOR
    step_or_sub='Sub-Numor or Slice-No.  '
    ScanText=   'Unknown Scan Type       '                        
    IF  SequenceType eq  0 THEN ScanText='Single Data Acquisition '
    IF  SequenceType eq  1 THEN ScanText='Sequential Acquisition  '
    IF  SequenceType eq  2 THEN ScanText='Temperature Scan        '
    IF  SequenceType eq  3 THEN ScanText='2Theta Scan             '
    IF  SequenceType eq  4 THEN ScanText='Omega Scan              '
    IF  SequenceType eq  5 THEN ScanText='Chi Scan                '
    IF  SequenceType eq  6 THEN ScanText='Phi Scan                '
    IF  SequenceType eq  7 THEN ScanText='Translation 1 Scan      '
    IF  SequenceType eq  8 THEN ScanText='Translation 2 Scan      '
    IF  SequenceType eq  9 THEN ScanText='Other Scan              '
				inner=long(SequenceType)-2
    IF  SequenceType gt 10 THEN BEGIN
             ScanTextArray=['Sing.','Sequ.','Temp.','2Th. ','Omega','Chi  ','Phi  ','Tr.1 ','Tr.2 ','Seven','Else ']
             ScanText=ScanTextArray((long(SequenceType) /   10)<10)
             outertext=ScanText
             outer=(long(SequenceType)  /  10) - 2
             ScanText=ScanTextArray((long(SequenceType) mod   10)<10)
             innertext=ScanText
             inner=(long(SequenceType) mod 10) - 2
             ScanText=outerText+'/'+innertext+'coupled Scan '
    ENDIF
    IF inner GE 0 THEN BEGIN
				  PV(28,*)=par1(7+inner*5) 
						step_or_sub='Sub-Numor or Slice-No.  '
						step_or_sub='(inner) Scan Step-Width '
    ENDIF
	   PP  =[PV(0:30),par5(0:1),par5(3),$                   
		        StroboType,$      
		        par1(41),0,0,0,0]
    IF CntTime EQ 0.0 THEN BEGIN            ; IF Real Counting Time not measured
				  CntTime=PV(25,0)*PP(35)/8000000.      ; missing 51st scaler, 1 ACQ = 1 Slice (before modification)
						PV(5,*)=PV(5,*)>(PV(25,*)*PP(35)/8000000.)
				ENDIF
    IF CntTime LT 0.0 THEN BEGIN            ; Real Counting Time GE 1 hour: Value becomes negative
				  CntTime=PV(25,0)*PP(35)/8000000.*2.   ; 51st scaler present, 1 ACQ = 2 Slices (after modification)
						IF StroboType eq 0 THEN PV(5,*)=PV(5,*)>(PV(25,*)*PP(35)/8000000.*2.) ELSE PV(5,*)=PV(5,*)>(PV(25,*)*PP(35)/8000000.)
				ENDIF
				PP(5)=TOTAL(PV(5,*))                    ; Total counting time in fixed parameter 
				CntTime=PP(5)
    IF charpos NE 0 THEN BEGIN
				  day  = STRCOMPRESS(STRING(LONG(day)   -   LONG(CntTime)  /  86400),/remove_all)
				  hour  =STRCOMPRESS(STRING(LONG(hour)  -  (LONG(CntTime) MOD 86400)  /  3600),/remove_all)
				  minute=STRCOMPRESS(STRING(LONG(minute)- ((LONG(CntTime) MOD 86400) MOD 3600)  /  60),/remove_all)
				  second=STRCOMPRESS(STRING(LONG(second)-(((LONG(CntTime) MOD 86400) MOD 3600) MOD 60)),/remove_all)
      IF LONG(second) LT 0 THEN BEGIN
						  second = STRCOMPRESS(STRING(LONG(second)+60),/remove_all)
							 minute = STRCOMPRESS(STRING(LONG(minute)- 1),/remove_all)
						ENDIF
      IF LONG(minute) LT 0 THEN BEGIN
						  minute = STRCOMPRESS(STRING(LONG(minute)+60),/remove_all)
							 hour   = STRCOMPRESS(STRING(LONG(hour  )- 1),/remove_all)
						ENDIF
      IF LONG(hour  ) LT 0 THEN BEGIN
						  hour   = STRCOMPRESS(STRING(LONG(hour  )+60),/remove_all)
							 day    = STRCOMPRESS(STRING(LONG(day   )- 1),/remove_all)
						ENDIF
      IF LONG(day   ) LT 0 THEN BEGIN
						  second = STRCOMPRESS(STRING(0),/remove_all)
						  minute = STRCOMPRESS(STRING(0),/remove_all)
						  hour   = STRCOMPRESS(STRING(0),/remove_all)
							 day    = STRCOMPRESS(STRING(1),/remove_all)
						ENDIF
      day='00'+day
      day=strmid(day,strlen(day)-2,2)
      hour='00'+hour
      hour=strmid(hour,strlen(hour)-2,2)
      minute='00'+minute
      minute=strmid(minute,strlen(minute)-2,2)
      second='00'+second
      second=strmid(second,strlen(second)-2,2)
						DATI=day+"-"+month+"-"+year+""+hour+":"+minute+":"+second
      OT = INST(0)   +' '+ DATI $
     	   +' User '+strmid(exper(1),20,8)$
     	  	+' L.C.' +strmid(exper(4),20,7);$
		       ;+' Run'  +strcompress(string(numor)) 
				ENDIF
    PTXT=[$
                  ScanText   $
                 ,'Number of Segments      ','Segment Number          '$
                 ,'Number of Data Sets     ','Data Set Number         '$
                 ,'TOTAL Counting Time/s   ','Reactor Power / MW      '$
                 ,'D19 State               ','OS closed/open (0/1)    '$
                 ,'Sample Environment      ','Set Temperature         '$
                 ,'Regulation Temperature  ','Sample Temperature      '$
                 , MotTxt(0:5)$
                 ,'Voltmeter1              ','hor. MonoSlits / mm     '$
                 ,'vert. MonoSlits / mm    ','hor. SampleSlits / mm   '$
                 ,'vert. SampleSlits / mm  ','Time Step               '$
                 ,'Time Slice              ','Time Delay              '$
                 ,'Time Window             ', step_or_sub              $
                 ,'Numor                   ','Original Monitor Counts '$
                 ,'Incid. divergence alpha1','HOPG Filters IN/OUT     '$
                 ,'Wavelength / Angstroem  '$
                 ,'Stroboscopic Mode       ','Number of strob. Cycles '$
                 ,'Calibration Wavelength/A','Raw Data TwoTheta Offset'$
                 ,'Correction Eff./Ang./Bad','Normalisation Mon./Time '$
                 ,'Monochromator TakeOff   ','Monochromator Changer   '$
                 ,'Monochromator Rocking   ','Monochromator Tilt      '$
                 ,'Monochromator Bending   ','Lead Blocks             '$
                 ,'Monochromator Slit Top  ','Monochromator Slit Bottm'$
                 ,'Monochromator Slit Left ','Monochromator Slit Right'$
                 ,'Vacuum tube pressure    '$
                 ,'Sample Slit Top         ','Sample Slit Bottom      '$
                 ,'Sample Slit Left        ','Sample Slit Right       '$
                 ,'ReactorCycle            ','Magnetic Field          '$
		 ,'Julian Days since 1970  ','Seconds since noon      ']
           mag=0.0
           IF N_ELEMENTS(param) GE 16 THEN mag=param(15)
	PP=[PP,par5(4:13),par5(15:19),par5(25),mag,jul_day,jul_sec]
	FOR i=0,N_ELEMENTS(PP)-1 DO PTXT(i)=PTXT(i)+' ('+STRMID(STRCOMPRESS(100+i,/RE),1,2)+')'
	IF N_ELEMENTS(PV(0,*)) GT 1 THEN BEGIN
		PV_tmp=PV
		old_dim=N_ELEMENTS(PV(*,0))
		PV=FLTARR(N_ELEMENTS(PP),N_ELEMENTS(PV(0,*)))
		PV(0:old_dim-1,*)=PV_tmp(0:old_dim-1,*)
		FOR i=0,scan-1 DO PV(old_dim:N_ELEMENTS(PP)-1,i)=PP(old_dim:N_ELEMENTS(PP)-1)	
	ENDIF ELSE BEGIN
		PV=[PV,PP(N_ELEMENTS(PV):N_ELEMENTS(PP)-1)]
	ENDELSE
	if n_elements(pv(*,0) ge 55) then IF N_ELEMENTS(vparm(*,0)) GE 35 THEN BEGIN
			pv(51:54,*)=vparm(26:29,*)
			pv(41,*)=vparm(31,*)
			pv(42,*)=vparm(30,*)
			pv(43,*)=vparm(32,*)
			pv(44,*)=vparm(34,*)
			pv(32,*)=vparm(33,*)
	endif
	IF year GT 60 THEN twotheta_offset=-32.2 ELSE twotheta_offset=0.0  
    tmparr=FINDGEN(nd)/10.
	   PP(38)= (n_elements(cal_d20) eq nd AND eff_flag EQ 1) 
	   IF ang_flag NE 0 AND n_elements(ang_d20) EQ nd AND n_elements(cal_d20) EQ nd THEN BEGIN 
	       PP(38)= PP(38)+2 
			     twotheta_offset = ang_d20(0)
        tmparr=ang_d20-twotheta_offset
        PP(38)= PP(38)+2*(n_elements(ang_d20) eq nd) 
        IF wav_flag NE 0 THEN IF Wavelength NE 0 THEN PP(36)=Wavelength
    ENDIF
    IF (TwoTheta_OffSet+PP(13) GE 10 AND nd EQ 1600) THEN TwoTheta_Offset=TwoTheta_Offset-66.666667
    PV(13,*)=TwoTheta_OffSet+PV(13,*)
    PP(13)  =TwoTheta_OffSet+PP(13)
    PP(37)  =TwoTheta_Offset
    XX      =PV(13,0)+tmparr
				tmp     =WHERE([SequenceType,long(SequenceType)/10,long(SequenceType) mod 10] eq 3,count)
	   IF count GE 1 THEN BEGIN
             XX=fltarr(nd,scan)
             FOR i=0,nd-1 DO XX(i,*)=PV(13,*)+tmparr(i)
    ENDIF
    XT  ='2*Theta'
    IF scan gt 1 THEN BEGIN 
             YY=findgen(scan) 
             IF StroboType le 0 THEN BEGIN
                IF (SequenceType) mod 10 GE  2 AND long(SequenceType) mod 10 LE 8 THEN YY(*) = PV(10+long(SequenceType) mod 10,*)
             ENDIF
    ENDIF ELSE YY = round(numor)
	   IF StroboType le 0 THEN YT='Steps' ELSE YT='Slices'
    IF scan le 1 THEN BEGIN
        YT = 'Counts' 
        IF normalize NE 0 THEN YT=YT+'*'+STRCOMPRESS(STRING(ROUND(normalize/PP(30))),/remove_all)+')'
    ENDIF ELSE BEGIN
        IF StroboType le 0 THEN YT=ScanText
	       IF MAX(YY)-MIN(YY) eq 0.0 THEN BEGIN
	         YY=indgen(scan)
	         IF StroboType le 0 THEN YT='Steps' ELSE YT='Slices'
        ENDIF
    ENDELSE
    IF  SequenceType gt 10 THEN BEGIN
             YT='Steps'
             YY=indgen(scan)
    ENDIF
	   IF scan le 1 THEN BEGIN
      ZT = 'Counts' 
      IF normalize NE 0 THEN ZT=ZT+'('+STRCOMPRESS(STRING(normalize),/remove_all)+')'
    ENDIF ELSE ZT  ='Numor' & ZZ=PV
    DATE=DATI

  w=wout

  zerocounters=WHERE(TOTAL(REFORM(wout,N_ELEMENTS(wout(*,0)),N_ELEMENTS(wout(0,*))),2) LE 0)
  tmp=N_ELEMENTS(ZeroCounters)
		NbOfZeroCounters=tmp
;  print,''
;  IF NbOfZeroCounters GE 1 THEN PRINT,' Zero ',STRMID(ZeroCounters(0:13<N_ELEMENTS(ZeroCounters)-1),8,4)
  tmp=0*ZeroCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfZeroCounters GE 1 THEN FOR i=0,NbOfZeroCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ ZeroCounters(i) THEN tmp(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfZeroCounters GE 1 THEN BEGIN
    ind=where(tmp,count)
;    IF count GE 1 THEN PRINT,' excl.', STRMID(ZeroCounters(ind),8,4)
  ENDIF
  tmp=TOTAL(REFORM(w,N_ELEMENTS(w(*,0)),N_ELEMENTS(w(0,*))),2)
  ind=findgen(N_ELEMENTS(tmp)-4)+2

  smoothed=tmp
  smoothed(ind)=2./3.*(tmp(ind-1)+tmp(ind-1))-(tmp(ind-2)+tmp(ind-2))/6.>0

  tmp0=TOTAL(REFORM(EE,N_ELEMENTS(EE(*,0)),N_ELEMENTS(EE(0,*))),2)/SQRT(N_ELEMENTS(EE(0,*)))
  tmp1=tmp-tmp0
  tmp2=tmp+tmp0

  MCounters=2+WHERE(tmp2(ind) LT (tmp1(ind-2)+tmp1(ind+2))/2./1.06 AND tmp1(ind-1)/1.02 GT (3.*tmp2(ind-2)+tmp2(ind+2))/4. AND tmp1(ind+1)/1.02 GT (tmp2(ind-2)+3.*tmp2(ind+2))/4.,NbOfMCounters)
  IF NbOfMCounters GE 2 THEN MCounters=MCounters(SORT((tmp(MCounters+2)+tmp(MCounters+1)+tmp(MCounters)+tmp(MCounters-1)+tmp(MCounters-2))/((tmp(MCounters+1)+tmp(MCounters-1))/2.-tmp(MCounters))))
;  IF NbOfMCounters GE 1 THEN PRINT,' "M"  ', STRMID(MCounters(0:13<N_ELEMENTS(MCounters)-1),8,4)
  tmp0=0*MCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfMCounters GE 1 THEN FOR i=0,NbOfMCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ MCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfMCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
;    IF count GE 1 THEN PRINT,' excl.', STRMID(MCounters(ind0),8,4)
  ENDIF

  WCounters=2+WHERE(tmp1(ind)/1.06 GT (tmp2(ind-2)+tmp2(ind+2))/2. AND tmp2(ind-1) LT (3.*tmp1(ind-2)+tmp1(ind+2))/4./1.02 AND tmp2(ind+1) LT (tmp1(ind-2)+3.*tmp1(ind+2))/4./1.02,NbOfWCounters)
  IF NbOfWCounters GE 2 THEN WCounters=WCounters(SORT((tmp(WCounters+2)+tmp(WCounters+1)+tmp(WCounters)+tmp(WCounters-1)+tmp(WCounters-2))/(tmp(WCounters)-(tmp(WCounters+1)+tmp(WCounters-1))/2.)))
;  IF NbOfWCounters GE 1 THEN PRINT,' "W"  ', STRMID(WCounters(0:13<N_ELEMENTS(WCounters)-1),8,4)
  tmp0=0*WCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfWCounters GE 1 THEN FOR i=0,NbOfWCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ WCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfWCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
 ;   IF count GE 1 THEN PRINT,' excl.', STRMID(WCounters(ind0),8,4)
  ENDIF

  ind=findgen(N_ELEMENTS(tmp)-4)+2

  LowCounters =2+WHERE(tmp(ind) LT (smoothed(ind)-4.*SQRT(smoothed(ind)>0))-ABS(2.*smoothed(ind)-tmp(ind-1)-tmp(ind+1))/1.-ABS(tmp(ind-1)-tmp(ind+1))/2. AND tmp(ind) GT 0,NbOfLowCounters)
  IF NbOfLowCounters GE 2 THEN LowCounters=LowCounters(SORT(smoothed(LowCounters)/(smoothed(LowCounters)-tmp(LowCounters))))
;  IF NbOfLowCounters GE 1 THEN PRINT,' Low  ', STRMID(LowCounters(0:13<N_ELEMENTS(LowCounters)-1),8,4)
  tmp0=0*LoWCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfLoWCounters GE 1 THEN FOR i=0,NbOfLoWCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ LoWCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfLoWCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
;    IF count GE 1 THEN PRINT,' excl.', STRMID(LoWCounters(ind0),8,4)
  ENDIF

  HighCounters =2+WHERE(tmp(ind) GT (smoothed(ind)+4.*SQRT(smoothed(ind)>0))+ABS(2.*smoothed(ind)-tmp(ind-1)-tmp(ind+1))/1.+ABS(tmp(ind-1)-tmp(ind+1))/2.,NbOfHighCounters)
  IF NbOfHighCounters GE 2 THEN HighCounters=HighCounters(SORT(smoothed(HighCounters)/tmp((HighCounters)-smoothed(HighCounters))))
;  IF NbOfHighCounters GE 1 THEN PRINT,' High ', STRMID(HighCounters(0:13<N_ELEMENTS(HighCounters)-1),8,4)
  tmp0=0*HighCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfHighCounters GE 1 THEN FOR i=0,NbOfHighCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ HighCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfHighCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
;    IF count GE 1 THEN PRINT,' excl.', STRMID(HighCounters(ind0),8,4)
  ENDIF

    good_cells           =indgen(nd)
    IF bad_flag EQ 1 AND N_ELEMENTS(bad_d20) GE 1 THEN BEGIN
      PP(38)= PP(38)+4 
      good_cells(bad_d20)=-1
      good_cells           =good_cells(WHERE(good_cells GE 0))
    ENDIF
    IF (bad_flag EQ 1 AND N_ELEMENTS(bad_d20) GE 1) THEN BEGIN
        ZZ=[bad_d20]
    ENDIF ELSE ZZ=[0]
    IF N_ELEMENTS(bad_d20) GE 1 THEN ZZ=[bad_d20] ELSE ZZ=[0]
    IF interpol_flag EQ 1 THEN BEGIN
      IF (bad_flag EQ 1 AND N_ELEMENTS(bad_d20) GE 1) THEN BEGIN
        ;PRINT,'Bad cells will be excluded AND interpolated, numbers of bad cells in Z'
						  IF (N_ELEMENTS(ang_d20) NE nd OR ang_flag EQ 0 OR N_ELEMENTS(cal_d20) NE nd) THEN BEGIN ;noang
          PP(38)= PP(38)+8 
          IF N_ELEMENTS(WOUT(0,*)) GE N_ELEMENTS(XX(0,*)) THEN BEGIN  
            FOR i=0,N_ELEMENTS(WOUT(0,*))-1 DO BEGIN
								      WOUT(bad_d20,i)=interpol(WOUT(good_cells,i),XX(good_cells),XX(bad_d20))
								      EE  (bad_d20,i)=interpol(EE  (good_cells,i),XX(good_cells),XX(bad_d20))
						    		ENDFOR
          ENDIF ELSE BEGIN
						      WOUT(bad_d20,*)=interpol(WOUT(good_cells,*),XX(good_cells),XX(bad_d20))
						      EE  (bad_d20,*)=interpol(EE  (good_cells,*),XX(good_cells),XX(bad_d20))
						    ENDELSE
								ENDIF 
      ENDIF 
	     IF N_ELEMENTS(ang_d20) EQ nd AND N_ELEMENTS(cal_d20) EQ nd AND ang_flag EQ 1 THEN BEGIN
          PP(38)= PP(38)+8 
          IF N_ELEMENTS(WOUT(0,*)) GE N_ELEMENTS(XX(0,*)) THEN BEGIN
										  XXold=XX(good_cells)
												XX=XX(0)+findgen(nd)/10.
            FOR i=0,N_ELEMENTS(WOUT(0,*))-1 DO BEGIN
						    		  WOUT(*,i)=interpol(WOUT(good_cells,i),XXold,XX)
						    		  EE  (*,i)=interpol(EE  (good_cells,i),XXold,XX)
						    		ENDFOR
          ENDIF ELSE BEGIN
										  XXold=XX(good_cells)
												FOR i=0,N_ELEMENTS(WOUT(0,*))-1 DO XX(*,i)=XX(0,i)+findgen(nd)/10.
						      WOUT=interpol(WOUT,XXold,XX)
						      EE  =interpol(EE  ,XXold,XX)
					    	ENDELSE

								ENDIF;ang
				ENDIF ELSE BEGIN ; NO interpolation
				  IF bad_flag EQ 1 AND N_ELEMENTS(bad_d20) GE 1 THEN BEGIN
        ;PRINT,'Bad cells will be excluded and NOT interpolated, numbers of BAD cells in Z!'
        ;PRINT,'Attention, some macros may fail on irregular data (missing cells)!'
						  WOUT=WOUT(good_cells,*)
						  XX  =XX  (good_cells,*)
						  EE  =EE  (good_cells,*)
				  ENDIF
				ENDELSE
			 IF normalize GT 0 AND MIN(NN) GT 0 THEN BEGIN
						FOR i=0,scan-1 DO BEGIN
						  WOUT(*,i)=      WOUT(*,i)/ NN(i)*normalize
						  EE  (*,i)=      EE  (*,i) /NN(i)*normalize								
						ENDFOR
      par1(54)=par1(54)/par1(50)*normalize ; 2nd monitor
      normalization_factor=NN
      normalization_factor=NN/normalize
      normalization_factor=1./normalization_factor
						NN=NN*0+normalize
						PP(39)=normalize
      normalization_factor=NN/normalize
				ENDIF
    IF N_ELEMENTS(pv(0,*)) GT 1 THEN BEGIN
      ;help,NN
	  IF normalize GT 0 AND MIN(NN) GT 0 THEN BEGIN
        NN=[[REFORM(NN,1,1,N_ELEMENTS(NN))],[REFORM(PV(5,*)*normalization_factor,1,1,N_ELEMENTS(PV(5,*)))],[REFORM(PV(5,*)*0.+par1(54)*normalization_factor,1,1,N_ELEMENTS(PV(5,*)))]]
      ENDIF ELSE BEGIN
        NN=[[REFORM(NN,1,1,N_ELEMENTS(NN))],[REFORM(PV(5,*),1,1,N_ELEMENTS(PV(5,*)))],[REFORM(PV(5,*)*0.+par1(54),1,1,N_ELEMENTS(PV(5,*)))]]
      ENDELSE
    ENDIF ELSE BEGIN
	  IF normalize GT 0 AND MIN(NN) GT 0 THEN BEGIN
        NN=[[NN],[PV(5,*)*normalization_factor],[par1(54)*normalization_factor]]
        ; Mod. 4/2/98 Hansen: 2nd monitor
      ENDIF ELSE BEGIN
        NN=[[NN],[PV(5,*)],[par1(54)]] ; Mod. 4/2/98 Hansen: 2nd monitor
      ENDELSE
    ENDELSE
    IF float_flag EQ 0 THEN WOUT=LONG(WOUT)
	;HELP,PTXT,PP,PV
	;PRINT,'CouCou'
  RETURN
END
