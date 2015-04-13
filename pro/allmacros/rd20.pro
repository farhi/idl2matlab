FUNCTION rd20 , INST_GRP,PATH,FILENAME,STATUS,DATP ,COMP=comp
;******* ****
;**
common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
common c_rdid , dzap, pzap, pzip
common d20, bad_d20 ,flag_d20, wav_d20, psd_d20  

IF n_elements(PATHCAL )   eq 0 then BEGIN
  if (sys_dep('MACHINE') eq 'mac') THEN BEGIN
    PATHCAL =P_LAMBDA()+':CALIBRATION:'
    HELP,PATHCAL
  ENDIF ELSE PATHCAL =P_LAMBDA()+'/CALIBRATION/'
ENDIF
IF n_elements(INST_GRP)   le 1 then return,1
CATCH,stat & if stat ne 0 then begin CATCH,/cancel & print,!err_string
				     FREE_LUN,in   & RETURN, WOUT & endif

;help,inst_grp
;print,inst_grp
;HELP,PATH,FILENAME,STATUS,DATP,comp
;PRINT,PATH,FILENAME,STATUS,DATP
WOUT=0 & WT='' & DATE='' & OT='' & XT='' & YT='' & ZT='' & PP=0 & PTXT=''
         XX=0  & YY  =0  & ZZ=0  & NN=0  & PV=0  & EE=0

scan=0 & Vri=-1 & text='' & exper='' & partx='' & param=0  & parai=0 & vparm=0
pthv=PATH
INST=strlowcase(INST_GRP)
IF n_elements(FILENAME) gt 1 THEN BEGIN NIMG=fix(FILENAME(1)) &
FILENAME=FILENAME(0)
			     ENDIF ELSE NIMG=0

remember=0


IF (INST(0) EQ 'd16') OR   (INST(0) EQ 'db21') THEN INST(1) ='dif'
IF  INST(3) EQ '1'  THEN IF INST(1) EQ 'dif'   THEN IF INST(0) NE 'd1a' THEN  $
						    IF INST(0) NE 'd2b' THEN  $
		    pthv=pthv+INST(0)+'_'+strmid(FILENAME,1,1)+sys_dep('DIVIDER')
;Compressed or not!
;---------- -- ---
form=findfile(pthv+FILENAME+'.Z',count=cprs)
IF cprs GT 0  THEN BEGIN
	IF pthv NE '' THEN bid=sys_dep      ('COPY',FILENAME+'.Z',pthv) ELSE cprs=0
			   bid=sys_dep      ('UN_Z',FILENAME+'.Z')       &   pthv=''
ENDIF

ON_IOERROR,misopen
STATUS=11
GET_LUN,in & if in gt 125 then begin free_lun,125 & free_lun,126 & endif
OPENR,in,pthv+FILENAME

	STATUS=13
	line  ='' & deco='' & form='' & cnt=0L & bid=0L & numor=0L & vpb=0L
	linet =['']
	ON_IOERROR,misread
	
	READF,in,line     & READF,in,deco   & READF,in,line
	SKIPLINE,in,deco,numor,nvers=nvers

;	Contact and date
;	------- --- ----	
	READF,in,deco  & cnt=80
	SKIPLINE,in,deco,cnt
	nbl  =cnt/80      & IF nbl*80 lt cnt  THEN  nbl=nbl+1
	text =strarr(nbl) & READF,in,text   & READF,in,form
	form =strmid(form,0,2)
     	insv =strlowcase(strtrim(strmid(text(0),0,4),2))

	IF  form ne 'VV'  THEN BEGIN
	 WHILE form ne 'AA' DO BEGIN READF,in,form & form=strmid(form,0,2)
	 ENDWHILE

;	 Experiment
;	 ----------	
	 READF,in,deco     & SKIPLINE,in,deco,cnt
	 nbl  =cnt/80      & IF nbl*80 lt cnt  THEN  nbl=nbl+1
	 exper=strarr(nbl) & READF,in,exper  & READF,in,form
	 form =strmid(form,0,2)
	ENDIF
;	Parameters
;	----------
	WHILE (form ne 'SS') AND (form ne 'VV') DO BEGIN
		
		READF,in,deco    & SKIPLINE,in,deco,cnt
		nbl  =cnt/80      & IF nbl*80 lt cnt  THEN  nbl=nbl+1
		IF form eq 'FF'  THEN BEGIN
 				      IF n_elements(par4)  gt 1 THEN par5=par4
 				      IF n_elements(par3)  gt 1 THEN par4=par3
 				      IF n_elements(par2)  gt 1 THEN par3=par2
 				      IF n_elements(par1)  gt 1 THEN par2=par1
				       IF n_elements(param) gt 1 THEN par1=param
;**** numor-bug fix Th.Hansen D20 ***********************
  IF n_elements(par1)  EQ 0 AND cnt EQ 30 AND numor GE 6815 AND numor LE 6840 THEN cnt=25 
;**** numor-bug fix Th.Hansen D20 ***********************
				      param=fltarr(cnt) 
		ENDIF ELSE $
		IF form eq 'II'  THEN parai=lonarr(cnt)   ELSE  $
		IF form eq 'AA'  THEN bidon=strarr(nbl)
	
		IF form eq 'AA'  THEN READF,in,bidon ELSE $
		IF form eq 'II'  THEN READF,in,parai ELSE $
;		IF form eq 'FF'  THEN READF,in,param
;**** numor-bug fix Th.Hansen D20 ***********************
		IF form eq 'FF'  THEN BEGIN
    IF numor GE 22274 AND nvers EQ 3 AND n_elements(par4) GT 1 AND n_elements(par5) LE 1 THEN BEGIN
      tmppar=fltarr(10)
      READF,in,tmppar
      param(0:9)=tmppar
      param(10:14) = 0
      READF,in,deco
      tmppar=fltarr(cnt-15)
      READF,in,tmppar
      param(15:cnt-1)=tmppar
    ENDIF ELSE READF,in,param
  ENDIF
;**** numor-bug fix Th.Hansen D20 ***********************
		READF,in,form
		form=strmid(form,0,2)
	
	ENDWHILE
	
	IF form eq 'SS' THEN BEGIN
;***	** **** ** **** **** ***** ---> DIF TOF
;***	** **** ** **** **** ***** ---> DIF TOF
;***	** **** ** **** **** ***** ---> DIF TOF

;	# scans
;	-------
	
	READF,in,deco     & READS,deco+' 0 0 0 0 0 0',bid,scan,bid,bid,bid,vpb
	SKIPVPAR,vpb,vpara, in
;**** numor-bug fix Th.Hansen May 97 D20 ***********************
IF N_ELEMENTS(par5) GT 25 THEN IF par5(25) EQ 972. THEN IF numor EQ 8131 THEN BEGIN
  READF,in,form  
  vpara=[vpara(0:10),vpara(12:25),0.]
ENDIF
;**** numor-bug fix Th.Hansen May 97 D20 ***********************
	READF,in,form     & form=strmid(form,0,2)
	IF scan lt 1 THEN scan=1 ELSE scan=scan+1

	READF,in,deco     & SKIPLINE,in,deco,cnt
	
	forcl=1 & forcd=0
	
	CASE insv OF
	'd19' : begin   vp=10 & forcl=0
		IF scan gt 1 THEN BEGIN bad_d=intarr(512) & forcd=1 & cnt=cnt-1024 &
ENDIF
	        end
	'd9'  : begin   vp=10 & forcl=0 & end
	'db21': begin   vp=10 & forcl=0 & end
	'd16' : begin   vp=10 & end
	'd1b' : begin   vp=3  & end
	else  : begin   vp=0  & end
	ENDCASE

	IF vpb gt 0 THEN vp=0

	cnt  =  cnt-vp
	IF cnt  gt 1 THEN BEGIN
	IF vp   gt 0 THEN BEGIN vparm=lonarr(vp ,scan) & vpara=lonarr(vp) & ENDIF
	IF vpb  gt 0 THEN	vparm=fltarr(vpb,scan)                  ; ##### modified for D20 #######
 IF form eq 'JJ' THEN  form='II'
	IF scan gt   1    THEN $
	IF form eq 'II'   THEN buf=lonarr(cnt) $
			  ELSE buf=fltarr(cnt)

	IF form ne 'II'  THEN WOUT=fltarr(cnt,scan) $
		          ELSE IF   forcl  eq 0 THEN $
			       WOUT=intarr(cnt,scan) $
			  ELSE WOUT=lonarr(cnt,scan)
;	Read data
;	---- ----
	FOR i=0,(scan-1)<1299 DO   BEGIN   ;####### limitation necessary but not usefull: modif D20 ######
;   PRINT,'scan',i
	  IF  i gt 0 THEN BEGIN
     READF,in,line
     ;**** numor-bug fix Th.Hansen July 98 D20 ***********************
	    WHILE strpos(strlowcase(line), 'ssssssss') LT 0 DO BEGIN
;       print,'lin ',line
;       PRINT,strpos(strlowcase(line), 'ssssssss')
       READF,in,line
     ENDWHILE
;     PRINT,strpos(strlowcase(line), 'ssssssss')
     ;**** numor-bug fix Th.Hansen July 98 D20 ***********************
     READF,in,deco
;     print,'line',line
;     print,'deco',deco
		   READS,deco+' 0 0 0 0 0 0',bid,bid,bid,bid,bid,vpb
		   SKIPVPAR,vpb,vpara,in
     ;**** numor-bug fix Th.Hansen May 97 D20 ***********************
     IF N_ELEMENTS(par5) GT 25 THEN IF par5(25) EQ 972. THEN IF numor EQ 8131 THEN BEGIN
       READF,in,form  
;      print,'form',form
       vpara=[vpara(0:10),vpara(12:25),0.]
     ENDIF
     ;**** numor-bug fix Th.Hansen May 97 D20 ***********************
	   	READF,in,line   & READF,in,deco   & SKIPLINE,in,deco,bid
;     print,'line',line
	  ENDIF

	  IF vpb  gt 0 THEN   vparm(0,i)=vpara
;   help,vpb
	  IF scan le 1 THEN   BEGIN
	    IF vp eq 0 THEN BEGIN
       READF, in,         WOUT  
	    ENDIF ELSE BEGIN 
       READF, in, vpara , WOUT
	    		vparm(0,i)=vpara 
     ENDELSE
	  ENDIF	 ELSE  BEGIN
	    IF vp eq 0 THEN BEGIN
				   IF forcd eq 1   THEN BEGIN
         READF,in,bad_d,buf,bad_d 
						 ENDIF ELSE READF,in,buf
				 ENDIF ELSE BEGIN
				   IF forcd eq 1   THEN BEGIN
         READF,in,vpara,bad_d,buf,bad_d 
						 ENDIF ELSE READF,in,vpara,buf
	    		vparm(0,i)=vpara 
     ENDELSE
				
	    WOUT(0,i)=buf
;     plot,buf
 ;    HELP,buf
	  ENDELSE
	  Vri=i+1
	ENDFOR
 IF i LT scan THEN PRINT,'Not the entire Scan could be read in! (',i,' of',scan,' steps)'
	STATUS=0
	ENDIF
	ENDIF ELSE $
	IF form eq 'VV' THEN BEGIN
;***	** **** ** **** **** ***** --->  TAS
;***	** **** ** **** **** ***** --->  TAS
;***	** **** ** **** **** ***** --->  TAS

	NP=0
	READF,in,line & partx=[line]   &  READF,in,line
	linec='' & step='!!'
	WHILE strpos(line,'DATA_:') lt 0 DO BEGIN
	      IF strpos(line,'COMND:') eq 0 THEN BEGIN
	      			i=strpos(strlowcase(line),' np ')
	      			IF i  gt 0 THEN READS,strmid(line,i+4,5)+' 0' ,NP
	      			IF NP eq 0 THEN NP=100
	      			linec=strmid(strtrim(strcompress(line),2),7,50)
	      			ENDIF
	      IF strpos(line,'STEPS:') eq 0 THEN $
				step=strmid(strtrim(strmid(line,6,6),2),1,6)
	      partx=[partx,line]
	      READF,in,line
	ENDWHILE
	READF,in,line & line=strtrim(strcompress(line),2)+' ' & partx=[partx,line]
			l=strlen(line) & j=0
			FOR i=0,l-1 DO IF strmid(line,i,1) eq ' ' THEN BEGIN
					  linet=[linet,strmid(line,j,i-j)] & j=i+1 & ENDIF
			linet=linet(1:*)
	partx=strtrim(partx,2)

	IF NP  GT 0 THEN BEGIN
	   pos=0L & POINT_LUN,-in,pos
	   	    READF,in,line & line=strtrim(strcompress(line),2)
	   	    POINT_LUN, in,pos
	   cnt=0 & i=0 & WHILE i ge 0 DO BEGIN cnt=cnt+1 & i=strpos(line,' ',i+1)  &
ENDWHILE
	
	   ON_IOERROR,eofread
	   vparm=fltarr(cnt,NP)
	   READF,in,vparm
	   
	   IF vparm(0,NP-1) eq 100 THEN BEGIN   buf=fltarr(cnt)
	   					WHILE (1) DO BEGIN READF,in,buf
	   						     vparm=[[[vparm]],[buf]]
	   						     NP=NP+1 & ENDWHILE & ENDIF
	eofread:STATUS=0
	IF n_elements(vparm) gt 1 THEN BEGIN i=NP-1 & WHILE (i gt 0) and (vparm(0,i) eq 0) do i=i-1
						vparm=vparm(*,0:i>0)               & ENDIF
	ENDIF
	ENDIF
	
misread:FREE_LUN,in
	IF Vri gt 0 then IF Vri lt scan THEN begin scan=Vri & WOUT=WOUT(*,0:scan-1) & STATUS=0
					IF vp+vpb gt 0 THEN vparm=vparm(*,0:scan-1) & endif
IF cprs gt 0 THEN bid=sys_dep ('DELET', FILENAME)

misopen:

IF STATUS eq 0 THEN BEGIN
   CASE INST(1) OF
   'dif':  BEGIN
   	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
   	   WT  =	    strmid(text,4,10) +'  '+strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
     	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' Run ' + FILENAME    ;+' User '    +strmid(text,4,10)  
           XT  ='Detector X .'
           YT  ='Detector Y .'
           IF    scan gt 1 THEN ZT ='Points' ELSE  ZT ='Counts'
	   IF   n_elements(param) ge 50      THEN  BEGIN
	   	   PP  = [param(3:6),param(17),param(45:47),param(34:36)]
	   	   PTXT= ['Phi    ','Chi    ','Omega  ','2*Theta',$
	   	   	  'Wave lenght',$
	   	   	  'Requested  Temperature  ',$
	   	   	  'Regulation Temperature  ',$
	   	   	  'Sample (K) Temperature  ',$
	   	   	  'Sample-Detector Distance',$
		   	  'Starting Angle ',$
		   	  'Angle Variation']
	   	   ENDIF     
    	   IF   n_elements(vparm) ge scan*2  THEN  NN=[vparm(0,0:scan-1),vparm(1,0:scan-1)] 
    	   IF   n_elements(vparm) ge scan*4  THEN  BEGIN
    	           ZZ  =[ vparm(2,0:scan-1)]
    	           PV  =[ vparm(3,0:scan-1)]
              	   	   ENDIF

     	   CASE INST(0) of
     	   'd19':  BEGIN IF n_elements(cal_d19) eq 0 THEN BEGIN
     	   		 ENDIF
     	   		 WOUT=reform(WOUT,512,n_elements(WOUT)/512,/overwrite)
		   END
	   'd9':   BEGIN IF scan gt 1 THEN WOUT=reform (WOUT,32,32,n_elements(WOUT)/32/32,/overwrite)$
	    	   	      	      ELSE WOUT=reform  (WOUT,32,32, /overwrite)
		   END
	   'd16':  BEGIN IF scan gt 1 THEN WOUT=reform (WOUT,64,16,n_elements(WOUT)/64/16,/overwrite)$
	    	   	      	      ELSE WOUT=reform  (WOUT,64,16, /overwrite)
					   WOUT=reverse (WOUT(1:62,1:14),1)
			 sw=size(WOUT) & if sw(0) eq 3 then sz=sz(sw(3)) else sz=1
			 IF n_elements(cal_d16) eq 0 THEN $
			    P_DID_CALOD, INST(0),INST(0)+'.cal', flg

			 IF n_elements(cal_d16) gt 1 THEN BEGIN WOUT=float(WOUT)
				FOR i=0,sz-1 DO WOUT(0,0,i)=WOUT(*,*,i)/cal_d16
				OT=OT+' /'+inf_d16(0) & ENDIF
		   ZZ  =numor
		   
		   if n_elements(pzip) eq 0 then pzip=0.
		   D2TH=pzip

		   PTXT= ['2*Theta   ','Omega     ','Chi       ','Phi       ',$
	   	   	  'Trans_X   ','Trans_Y   ','Rot       ','Beamstop  ',$
                          'Wave lenght             ','Requested  Temperature ',$
	   	   	  'Regulation Temperature  ','Sample (K) Temperature  ',$
	   	   	  'Sample-Detector Distance','Starting Angle          ',$
                          'Angle Variation         ','Angle Range            ',$                           
			  'PRESET                  ','Coupling factor         ',$
                          'Motor scanned           ','Nb points requested    ','Nb points saved         ',$ 
			  'Count:Monitor or Time   ','Type of T-regulation    ','Type ofMulti-meter     ',$
                          'Delta2th                ']
		   PP =[param(6),param(5),param(4),param(3),0,0,0,0,param(17),param(45:47),$
                        param(29),param(35:38),param(42),parai(3),parai(5:7),parai(12:13),pzip]
                        
                   scan_t=parai(3)
                   nbang=parai(4)
                   FOR i=1,nbang DO IF parai(23+i) eq 0 THEN parai(23+i)=24
;                  ***Lecture pour les differents motors scans*****
	 	   PP(parai(24)-1)=vparm(3)/1000.
                   IF nbang eq 1 THEN BEGIN ZZ=PP(2) & ENDIF
		   IF nbang eq 2 THEN BEGIN PP(parai(25)-1)=vparm(4)/1000. & ENDIF
                   IF nbang eq 3 THEN BEGIN PP(parai(25)-1)=vparm(4)/1000. &PP(parai(26)-1)=vparm(5)/1000. & ENDIF
                   IF parai(3) gt 0 THEN BEGIN ZZ=round(vparm(3)*100/1000.) 
                       IF parai(3) eq 1  THEN  ZT='gamma*100'   & IF parai(3) eq 2 THEN ZT='omega*100'
                       IF parai(3) eq 3  THEN  ZT='chi*100'     & IF parai(3) eq 4 THEN ZT='phi*100'
                       IF parai(3) eq 5  THEN  ZT='Trans_X*100' & IF parai(3) eq 6 THEN ZT='Trans_Y*100'
                       IF parai(3) eq 7  THEN  ZT='Rot*100'
		   ENDIF
                   IF PP(12) le 0 THEN PP(12)=100.
		   step = 180./!pi * .254/PP(12)
		   set_tolerance,tt,/get & if tt eq 0 then set_tolerance,step
         	   XX   = findgen(62)*step + PP(0) - 30.5*step
                   NN   = vparm(1)

;                  ****Write the file in a regular grid determined by the delta2th parameter***
                   IF D2TH gt 0.001 THEN BEGIN
                     S=ROUND((XX(n_elements(XX)-1)-XX(0))/D2TH)+2
                     WINT=fltarr(S,14) & FRAC=0 & XR=fltarr(S) & NN=lonarr(S)
;                    ***XR contains the 2th's rounded to the nearest step size**
		     tent=round(XX(0)/D2TH)*D2TH
                     if XX(0) gt tent then XR(0)=tent else XR(0)=tent-D2TH

                     for i=1,S-1   do XR(i)=XR(i-1)+D2TH  
;                    ****How to write the numor o the regular grid XR****
                     FOR i=0,61 do begin 
                      FOR J=0,S-2 do begin
                       IF (XX(i) gt XR(j)) and (XX(i) lt XR(j+1)) then begin
			   FRAC=(XX(i)-XR(j))/D2TH & FRAC1=1-FRAC
                           WINT(j,*)=FRAC1*WOUT(i,*)+WINT(j,*)	& WINT(j+1,*)=FRAC*WOUT(i,*)+WINT(j+1,*) 
                             NN(j)  =FRAC1*vparm(1) +  NN(j)	&   NN(j+1) =FRAC*vparm(1) +  NN(j+1) 
                       ENDIF
                      ENDFOR 
                     ENDFOR
		     XX=XR & WOUT=WINT 
                     EE = SQRT(WOUT)		; *** EE the error
                   ENDIF
                   XX=round(XX*10000) & XX=XX/10000.
           	   XT   ='2*theta'
		   END
	   'db21': BEGIN WOUT=reform(WOUT,128 ,128,/overwrite)
	   	   END
	   'd1b':  BEGIN
           	   XT  ='2*Theta'
           	   YT  ='Temperature'
           	   XX  =findgen(n_elements(WOUT)) * 79.8/(n_elements(WOUT)-1)+PP(3)
           	   YY  =param(46)
	   	   END
	   'd1a':  BEGIN nd=25. & IF n_elements(cal_d1a) eq 0 THEN $
				     P_DID_CALOD, INST(0),INST(0)+'.cal', flg
		   nj  = n_elements(WOUT)/(nd+4)
		   if long(nj) ne nj then RETURN,0
	   	   WOUT= reform(WOUT,(nd+4),nj ,/overwrite)
		   NN  = WOUT(0,0)
		   YY  = reform(WOUT(2,*))/1000.
   	   	   YT  ='Counts'
		   WOUT= WOUT(4:*,*)
		   IF n_elements(cal_d1a) eq nd THEN BEGIN wout=float(wout) &  FOR i=0,nj-1 $
						        DO wout(0,i)=wout(*,i)/cal_d1a & fct=1.
							OT=OT+' /'+inf_d1a(0)
		   ENDIF ELSE fct = -6.

		   XX  =fct*ang_d1a+YY(0)
		   WOUT= reform(WOUT,nd*nj,/overwrite)
		   IF nj gt 1 then BEGIN FOR i=1,nj-1 do  XX=[XX , fct*ang_d1a+YY(i)]
		   			 idx=sort (XX) &  XX =XX(idx) & WOUT=WOUT(idx)
		   			 XX =round(XX/0.05) & XX=XX*0.05
		   			 YY =param(46)
		   		   ENDIF
           	   XT  ='2*Theta'
	   	   END
	   'd2b':  BEGIN
;			*** initialize parameters ***
			nd=64. & D2TH = param(36) 
		        if n_elements(dzap) eq 0 then dzap=0
		        if n_elements(pzap) eq 0 then pzap=0
;			*** read calibration file (if present)
			pp=[pp,d2th, param(38)]
			ptxt=[ptxt, 'Delta 2*theta', 'Monitor']

			IF n_elements(cal_d2b) eq 0 THEN $
			   P_DID_CALOD, INST(0),INST(0)+'.cal', flg

;		       *** nj = number of points ***
		   vp  = parai(4)+2
		   nj  = n_elements(WOUT)/(nd+vp)
;		       *** rewrite WOUT in matrix form (69 cols, nj rows) ***
	   	   WOUT= reform(WOUT,(nd+vp),nj ,/overwrite)
		   NN  = WOUT(0,0)
;			*** YY = 2th values for detector 1 ***
		   YY  = reform(WOUT(2,*))/1000.
;			*** set titles ***
   	   	   YT  ='Counts'
		   XT  =' 2*Theta' 
;			*** reform WOUT to eliminate first vp values (not data) ***
		   WOUT= WOUT(vp:*,*)

;		   *** detector zapping algorithm ***
;		   *** definition of new calibration arrays (default: equal to old)
		   newcal_d2b=cal_d2b
		   newang_d2b=ang_d2b
		   newd=nd
;		   *** test if dzap contains real detectors **
		   idz=where(dzap ge 1 and dzap le 64)
;		   *** in case, eliminate spurious values ***
		   if idz(0) ne -1 then dzap=dzap(idz) else dzap=0
;		   *** is there something left? ***
		   SD=SIZE(dzap)

;		   *** test if pzap contains real points **
		   idz=where(pzap ge 1 and pzap le nj)
;		   *** in case, eliminate spurious values ***
		   if idz(0) ne -1 then pzap=pzap(idz) else pzap=0
;		   *** is there something left? ***
		   SP=SIZE(pzap)

;		   *** if dzap has something, do the following ***
		   if SD(0) gt 0 then BEGIN
;		     *** set all dud values and calibration to -999 ***
		     WOUT(dzap-1,*)=-999
		     newcal_d2b(dzap-1)=-999
;		     *** good values are those which do not contain -999 ***
		     GOOD=where(WOUT ne -999)
		     if GOOD(0) eq -1 then return, GOOD(0)

;		     *** cut out dud values from WOUT and calibrations ***
		     WOUT=WOUT(GOOD)
		     newang_d2b=ang_d2b(where(newcal_d2b ne -999))
		     newcal_d2b=cal_d2b(where(newcal_d2b ne -999))
;		     *** redefine the detector number
		     newd=(nd-n_elements(dzap))
;		     *** reform WOUT to proper format
		     WOUT=reform(WOUT,newd,nj ,/overwrite)
		   ENDIF

;		   *** if pzap has something, do the following ***
		   if SP(0) gt 0 then BEGIN
;		     *** set all dud values -999 ***
		     WOUT(*,pzap-1)=-999
;		     *** good values are those which do not contain -999 ***
		     GOOD=where(WOUT ne -999)
;		     *** cut out dud values from WOUT ***
		     WOUT=WOUT(GOOD)
;		     *** redefine the number of points
		     nj=(nj-n_elements(pzap))
;		     *** reform WOUT to proper format
		     WOUT=reform(WOUT,newd,nj ,/overwrite)
		   ENDIF
;			*** if calibration available, calibrate by dividing ***
		   
		   IF n_elements(cal_d2b) eq nd THEN BEGIN wout=float(wout) &  FOR i=0,nj-1 $
							DO wout(0,i)=wout(*,i)/newcal_d2b & fct=1.
							OT=OT+' /'+inf_d2b(0)
;		        *** fct*newang_d2b always newd values spaced by ~ -2.5 ***
		   ENDIF ELSE fct = -2.5
;			*** XX array of initial 2th for all detectors ***
		   XX  =fct*newang_d2b+YY(0)
;			*** reform WOUT to be a 1-line vector ***
		   WOUT= reform(WOUT,newd*nj,/overwrite)
;			*** create a XX array by adding up all 2th for all dets **
;			*** in the same order as in WOUT


;			*** do the following if there are at least 2 points ***
		   IF nj gt 1 then BEGIN FOR i=1,nj-1 do  XX=[XX , fct*newang_d2b+YY(i)]
;			*** sort the XX and WOUT arrays (idx is the index array) ***
		   			 idx=sort (XX) &  XX =XX(idx) & WOUT=WOUT(idx)
;			*** XR contains the 2th's rounded to the nearest step size ***
		   			 XR =round(XX/D2TH) & XR=XR*D2TH
;			*** GRID contains the fractional indices of XR into XX ***					 
					 GRID=(XR-XX)/D2TH+findgen(n_elements(XX))				
;			*** WINT is WOUT interpolated onto the fractional indices *** 
					 WINT=interpolate(WOUT,GRID)
;			*** reassign XX and WOUT
					 XX=XR
				         WOUT=WINT
;			*** YY is the temperature, EE the error
		   			 YY =param(46)
					 EE = SQRT(WOUT)
		   		   ENDIF
	   	   END
'd20':BEGIN ;############### D20 Specifications ###########################
	WOUT=WOUT>0
    nd=cnt
	DATI     = strmid(exper(5),20,18)
    day      = strmid(DATI,0,2)
    month    = strmid(DATI,3,3)
    year     = strmid(DATI,7,2)
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
    ENDIF
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
    par1(51)=par1(51)/1000000.                                   ; CntTime in sec (raw data: microsec)
	   IF n_elements(par5) eq 25 THEN BEGIN                         ; Numor bug fix
	     par5=[par5(0:19),0.,0.,0.,0.,0.,par5(20:24)]
      param=[0.,param]
      FOR i=0,nj-1 Do wout(*,i)=[wout(nd-1,i),wout(0:nd-2,i)] 
	   ENDIF
 	  CntTime  = par1(51)
    proposal = strmid(exper(2),20 ,9)
  	 WT  =  ' '+strcompress(strmid(exper(0),20,40)) $
   	      +' '+strcompress(strmid(exper(3),20,40))
    OT = insv +' '+ DATI $
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
    ;****** CHECK WAVELENGTH ************
    IF ROUND(10*par5(3)) LE 26 AND ROUND(10*par5(3)) GE 25 THEN BEGIN
      wav=2.52
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
        IF ROUND(10*par5(3)) LE 19 AND ROUND(10*par5(3)) GE 15 THEN BEGIN
          wav=1.36
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
              ;IF ROUND(par5(4)) NE  42 THEN PRINT,'1.30A, Cu-Monochromator, TakeOff should be 42 deg but it is at',ROUND(par5(4))
              ;IF ROUND(par5(5)) NE 210 THEN PRINT,'1.30A, Cu-Monochromator, MonoChanger should be 210 deg but it is at',ROUND(par5(5))
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
    ENDIF ELSE IF N_ELEMENTS(inf_d20) EQ 1 THEN IF inf_d20 EQ 0 THEN inf_d20 ='autod20.cal'

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
				    tmp     =findfile(tmf+inst(0)+'_????_??_??????.???',count=tmc) ; NEW calibration file names
				    if tmc gt 0 then filelist=strmid(tmp,strpos(tmp(0),inst(0)),22) else print,"Are you really on the right workstation (d20sgi.ill.fr)?"
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
      if count le 0 then tmp=where(STRMID(filelist,strlen(inst(0))+16,2) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,2),count)
      if count le 0 then BEGIN
        tmp=where(STRMID(filelist,strlen(inst(0))+16,1) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,1),count)
        if count le 0 then tmp=where(STRMID(filelist,strlen(inst(0))+16,1) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*((wav+1)<9))),1,1),count)
        if count le 0 then tmp=where(STRMID(filelist,strlen(inst(0))+16,1) EQ STRMID(STRING(f='(I4)',1000+ROUND(100*((wav-1)>0))),1,1),count)
      ENDIF
      if count gt 0 then begin
        tmplist=filelist(tmp)
        tmp=where(tmplist LE inst(0)+'_'+strcompress(string(century),/re)+strcompress(string(year),/re)+'_'+STRMID(STRING(f='(I3)',100+mon),1,2)+'_'+STRMID(STRING(f='(I7)',1000000+numor),1,6)+'.'+STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,3),count)
      endif
						IF count GT 0 THEN tmp=MAX(tmplist(tmp)) ELSE BEGIN
        print,'Apparently there is no calibration for ',wav,' Angstroem ',STRMID(STRING(f='(I4)',1000+ROUND(100*wav)),1,3)
					   tmp=WHERE(filelist LE inst(0)+'_'+strcompress(string(century),/re)+strcompress(string(year),/re)+'_'+STRMID(STRING(f='(I3)',100+mon),1,2)+'_'+STRMID(STRING(f='(I7)',1000000+numor),1,6)+'.'+STRMID(STRING(f='(I4)',1000+ROUND(100*par5(3))),1,3),count)
				  		IF count GT 0 THEN tmp=MAX(filelist(tmp)) ELSE tmp=MIN(filelist)
         print,tmp,' will be taken for calibration'
      ENDELSE
						calfile=STRCOMPRESS(tmp,/re)
			 ENDIF
ON_IOERROR,badmisread
;help,psd_d20
				IF bad_flag EQ 1  OR  bad_flag NE 1 THEN BEGIN
				   count=N_ELEMENTS(psd_d20)
							IF count LT 1 THEN psd_d20=['empty']
			  	 IF count LT 2 THEN BEGIN
				    	tmf     =PATHCAL ; P_LAMBDA()+'/BAD_CELLS/'
				    	tmp     =findfile(tmf+inst(0)+'_????_??_??????.bad',count=tmc) ; NEW  file names
				     if tmc gt 0 then badlist=strmid(tmp,strpos(tmp(0),inst(0)),22) else print,"Are you really on the right workstation (d20sgi.ill.fr)?"
					    IF N_ELEMENTS(badlist) LE 0 THEN BEGIN
						    	  PRINT,'NO bad cells file found!'
					  	  			psd_d20=['empty']
					  	  			bad_flag=0
									    badlist=''
					  	  ENDIF else print,'Creating new bad cells filelist',badlist
					  	  psd_d20=[string(psd_d20(0)),badlist]
					  ENDIF ELSE badlist=psd_d20(1:count-1)
       tmp=where(badlist LE inst(0)+'_'+strcompress(string(century),/re)+strcompress(string(year),/re)+'_'+STRMID(STRING(f='(I3)',100+mon),1,2)+'_'+STRMID(STRING(f='(I7)',1000000+numor),1,6)+'.'+'bad',count)
				  IF count GT 0 THEN tmp=MAX(badlist(tmp)) ELSE  tmp=MIN(badlist)
 						 IF psd_d20(0) NE tmp THEN BEGIN
            psd_d20(0)=tmp
            print,'data : ',PATHCAL,psd_d20(0)
								    bad_d20=rddat('data',PATHCAL,psd_d20(0),rddat_status,rddat_datp)
            print,bad_d20
								    PRINT,'New bad_cells file loaded : ',psd_d20(0)
       ENDIF
				ENDIF
badmisread:
				IF wav_flag EQ 1 THEN BEGIN
				      count=N_ELEMENTS(wav_d20)
			  	    IF count LT 1 THEN wav_d20=[0.,0.,0.]
			  	    IF count LT 4 THEN BEGIN
				    	tmf     =P_LAMBDA()+'/RIETVELD/'
				    	tmp     =findfile(tmf+'?????????.d20',count=tmc)
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
      IF STRMID(inf_d20(1),37,8) LT '3' THEN wavelength=FLOAT(STRMID(inf_d20(1),37,8)) else wavelength=0
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
	       PV(5,i)     = par1(51)		    ; CntTime in sec.
	       PV(6:7,i)   = par5(26:27)	  ; RtrPower,D19
	       PV(8,i)     = par5(14)		    ; OS
	       PV(9:19,i)  = param(0:10) 	 ; SampEnv,TempValues,MotorValues,Voltmeter1
	       PV(20,i)    = (par5(12)<30)+(par5(13)<30) 	 ; hor.  MonoSlits
	       PV(21,i)    = (par5(10)<140)+(par5(11)<140) 	 ; vert. MonoSlits
	       PV(22,i)    = par5(18)+par5(19) 	 ; hor.  SampleSlits
	       PV(23,i)    = par5(16)+par5(17) 	 ; vert. SampleSlits
	       PV(24,i)    = 0.0		         ; TimeStep
	       PV(25:27,i) = par1(45:47)	  ; TimeSlice,TimeDelay,TimeWindow
	       PV(28,i)    = i             ; Slice_No or sub-numor, for scans it will contain the inner stepwidth (later on ...)
	       PV(29,i)    = numor
	       PV(30,i)    = par1(50)      ; monitor
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
        corname =P_LAMBDA()+'/MONITOR/'+STRCOMPRESS(STRING(LONG(cycle)), /REMOVE_ALL)+'.cor'
        OPENR,cor,corname,/get_lun
        READF,cor,corrections
        IF corrections GT 0 THEN BEGIN
	      	  cornum=fltarr(3,corrections)
	      			READF,cor,cornum
	      	ENDIF
        CLOSE,cor
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
	       MotTxt(i)=MotTxt(i)+'     '
	     ENDIF ELSE BEGIN
	       MotPar(i)=par1(12+i*5)
	       MotTxt(i)=MotTxt(i)+'     '
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
      OT = insv   +' '+ DATI $
     	   +' User '+strmid(exper(1),20,8)$
     	  	+' L.C.' +strmid(exper(4),20,7);$
		       ;+' Run'  +strcompress(string(numor)) 
				ENDIF
    PTXT=[$
                  ScanText,   $
                 'Number of Segments      ','Segment Number          ',$
                 'Number of Data Sets     ','Data Set Number         ',$
                 'TOTAL Counting Time/s   ','Reactor Power / MW      ',$
                 'D19 State               ','OS closed/open (0/1)    ',$
                 'Sample Environment      ','Set Temperature         ',$
                 'Regulation Temperature  ','Sample Temperature      ',$
                  MotTxt(0:5),$
                 'Voltmeter1              ','hor. MonoSlits / mm     ',$
                 'vert. MonoSlits / mm    ','hor. SampleSlits / mm   ',$
                 'vert. SampleSlits / mm  ','Time Step               ',$
                 'Time Slice              ','Time Delay              ',$
                 'Time Window             ', step_or_sub              ,$
                 'Numor                   ','Original Monitor Counts ',$
                 'Incid. divergence alpha1','HOPG Filters IN/OUT     ',$
                 'Wavelength / Angstroem  ',$
                 'Stroboscopic Mode       ','Number of strob. Cycles ',$
                 'Calibration Wavelength/A','Raw Data TwoTheta Offset',$
                 'Correction Eff./Ang./Bad','Normalisation Mon./Time ']
    twotheta_offset=-32.2 ; Changed 7/11, before : -32.1876  
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
    
    

	 END

	   else:
	   ENDCASE
	   END

   '3axes':BEGIN
   	   DATE=strmid   (partx(5),7,9)
   	   WT  =strmid(partx(2),7,11) +' '+linec
   	   OT  =insv +' '+partx(5)+' '+partx(2)+' Run ' + FILENAME
   	   XT  =strcompress(strmid(partx(6),7,69))
   	   YT  ='CNTS'
   	   PP  =fltarr(n_elements(partx))
   	   PTXT=partx
   	   PV  =vparm
   	   
   	   idx =where(linet eq 'CNTS') & idx=idx(0)
   	   IF idx ge 0 THEN IF idx lt cnt THEN WOUT=reform(vparm(idx,*)) ELSE WOUT=reform(vparm)
	   
   	   idx =where(linet eq 'M1')   & idx=idx(0)
   	   IF idx ge 0 THEN IF idx lt cnt THEN NN  =reform(vparm(idx,*))

   	   idx =where(linet eq step)   & idx=idx(0)
   	   IF idx ge 0 THEN IF idx lt cnt THEN BEGIN XX  =reform(vparm(idx,*))
   	   					     id  =sort(XX)
   	   					     XX  =XX  (id)
   	   					     WOUT=WOUT(id)
   	   					     IF n_elements(NN) gt 1  THEN  NN=NN(id)
   	   					     XT  =XT+' unit= '+step  &  ENDIF
	   EE  =sqrt(WOUT)
   	   END
   'tof':  BEGIN nd =0
   	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
   	   WT  =	    strmid(text,4,10) +'  '+strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
     	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' User '     +strmid(text,4,10)  +' Run ' + FILENAME
           XT  ='Channels'
           YT  ='Angles'
	   ZT  ='Numor' & ZZ=numor

     	   CASE INST(0) of
     	   'in13':BEGIN wavel=2.23
		    IF (size(WOUT))(2) eq 74 THEN BEGIN  nd=70
			  IF n_elements(cal_in13) eq 0 THEN $
			     P_DID_CALOD, INST(0),INST(0)+'.cal', flg
			  YY=[round(param(0 :34)*100)/100.      ,.1,.2,$
			      round(param(0 :34)*100)/100.+0.001,.3,.4]
			  sx=(size(WOUT))(1)
			  NN=lonarr(sx,3)
			  NN(*,1)=WOUT(*,36)>1 & NN(*,2)=WOUT(*,73)>1
			  WOUT=float(WOUT)
			  FOR i=0 ,34 do WOUT(*,i)=WOUT(*,i)/NN(*,1)
			  FOR i=37,71 do WOUT(*,i)=WOUT(*,i)/NN(*,2)
			  NN(*,0)=round(total(NN)/sx)

			  idx =sort(YY) & WOUT=WOUT(*,idx) & YY=YY(idx)
					  WOUT=WOUT(*,4:*) & YY=YY(4:*)
			  WOUT=WOUT*NN(0,0)
			  IF n_elements(cal_in13) eq nd THEN BEGIN
				FOR i=0,nd-1 DO wout(0,i)=wout(*,i)/cal_in13(i)
				OT=OT+' /'+inf_in13(0)
			  ENDIF
			  XX  =findgen (sx)*par1(11)+par1(1)-par1(2)
			  YY  =4*!pi/wavel*sin(YY/2*!pi/180.)
           		  XT  ='Energy micro eV   (normalized)'
           		  YT  ='Q(0) pairs'
			  IF par1(8) gt 0 THEN BEGIN ZZ=(par1(9)+par1(10))/2
						     ZT='Temperature' & ENDIF
		    ENDIF ELSE $
		    IF (size(WOUT))(0) eq 1  THEN BEGIN  nd=35 & sx=1
		     	  NN  =WOUT(36)
			  WOUT=float(WOUT(0:34))
			  XX  =round(param(0 :34)*100)/100. & XX  =XX(sort(XX))
			  XX  =4*!pi/wavel*sin(XX/2*!pi/180.)
           		  XT  ='Q(0)'
			  YY=(par1(9)+par1(10))/2
			  YT  ='Counts'
		    ENDIF
		    IF nd gt 0 THEN BEGIN
			  EE  =sqrt(WOUT)
			  PP  =[13.,par1(0),par1(3),par1(1),par1(2),par1(11),wavel,par1(8),$
				par1(9),par1(10)]
			  PTXT=['Type  of scan      (index)   ','Duration of scan   (second)  ',$
				'Chopper frequency            ','Energy   center    (micro eV)',$
				'Energy half range  (micro eV)','Channel  width     (micro eV)',$
				'Wave lenght        (angstrom)','Requested  Temperature (Kelv)',$
				'Temperature at start         ','Temperature at end           ']
		    ENDIF
		  END
	   else:
	   ENDCASE
	   END
   'lss':  BEGIN
   	   text=text(0) & expar=exper(0)
	   DATE=strmid(text,14,9)
   	   WT  =	    strmid(text,4,10) +'  '+strcompress(strmid(expar,0 ,69))$
   	  				      + ' '+strcompress(strmid(expar,70,79))
     	   OT  =insv +' Date '+ DATE +' Time '     +strmid(text,24,8)$
     	  			     +' User '     +strmid(text,4,10)  +' Run ' + FILENAME
           XT  ='X detector'
           YT  ='Y detector'
	   END
   else:
   ENDCASE
   
   DATP={w_tit:WT ,x_tit:XT ,y_tit:YT ,z_tit:ZT ,other_tit:OT ,time:DATE,p:PP,par_txt:PTXT,$
         x:XX     ,y:yy     ,z:ZZ     ,n:NN     ,pv:PV        ,e:EE}
   
ENDIF

RETURN, WOUT
END
