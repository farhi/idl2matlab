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
