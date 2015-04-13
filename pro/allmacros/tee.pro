function tee,in_wk
;
;converts array w_in  to energy in w_out.....GJK 1994
;
;Example call: w2=t2e(w1)
;         convert tof spectrum in w1 to energy and output in w2

;
; Sets up and copies work-space stuff into simple arrays
@lamp.cbk
      nwk=''
      x=0.0
      y=0.0
      w=0.0
      if one ne 0 then out_wk=one
      if two ne 0 then begin
        n_wk=strtrim(string(two),2)
        iii=execute('w_in = w' +n_wk)      
        iii=execute('n_in = n' +n_wk)
        iii=execute('pv_in= pv'+n_wk)       
        iii=execute('p_in = p' +n_wk)       
        iii=execute('x_in = x' +n_wk)
        iii=execute('y_in = y' +n_wk)
        iii=execute('z_in = z' +n_wk)
        iii=execute('e_in = e' +n_wk)
        
;       set defaults            
        n_out =n_in
        p_out =p_in
        pv_out=pv_in
        x_out =x_in
        y_out =y_in
        z_out =z_in
        e_out =e_in
        endif
;
;done @mac.in

my_check=size(in_wk)

  if my_check(0) lt 1 then  begin
     P_MUS,'mus_cannon'
     mess='Workspace empty'
     widget_control,bad_id=iii,l_message,set_value=mess
     return,in_wk
   endif

  if strpos(his(two),'t2e') gt 0 then begin
     P_MUS,'mus_cannon'
     mess='Workspace already in energy'
     widget_control,bad_id=iii,l_message,set_value=mess
     return,in_wk
  endif

  P_MUS,'mus_shot'
  nch=my_check(1)
  if my_check(0) gt 1 then nang=my_check(2) else nang=1
;
; stof or bs
  if (inst_value eq 'IN10') or (inst_value eq 'IN16') then begin
       x_in     = INDGEN(nch)+1
       t2efac   = 1.234
       if (inst_value eq 'IN16') then t2efac=1.036
  x_out=x_in*2.0*t2efac*p_in(2)/max(x_in)
  x_out=x_out-0.5*max(x_out)

  x_tit(one)='Energy Transfer (ueV)'
; Copies work-space stuff from simple arrays back to main LAMP arrays
      n_wk=string(out_wk)
      n_wk=strtrim(n_wk,2)

      jjj=execute('p' +n_wk+'=p_out' )
      jjj=execute('pv'+n_wk+'=pv_out')
      jjj=execute('x' +n_wk+'=x_out' )
      jjj=execute('y' +n_wk+'=y_out' )
      jjj=execute('z' +n_wk+'=z_out' )
      jjj=execute('e' +n_wk+'=e_out' )
;
;done @mac.out
return,in_wk
  endif

  	coef=fltarr(nch)
	x_out=fltarr(nch)
        w_out=fltarr(nch,nang)
        w_buf=fltarr(nch,nang)
	if n_elements(e_in)  ne n_elements(w_in) then e_out=0
	if n_elements(e_out) gt 1 then e_buf=w_buf
;
	y_out=y_in

  ;
; Convert to Energy
	eelast=81.799/p_in(21)^2                  ; Ei in meV
	telast=p_in(27)*p_in(21)/3956.0           ; TOF in sec
	cwidth=p_in(18)*1e-6                      ;chw in sec
        epp=p_in(9)

  ;
; Any channels to shift?
        norg=nch
        nxtra=0
        elimit=1000.0
        tlimit=p_in(27)*sqrt(81.8/elimit)/3956.0
        for j=0,nch-1 do begin
         time_diff=(p_in(9)-j)*cwidth        
         flt_time=telast-time_diff
         if flt_time le tlimit then begin
          epp=epp-1
          w_buf(nxtra,*)=in_wk(j,*)
	  if n_elements(e_out) gt 1 then e_buf(nxtra,*)=e_in(j,*)
          nxtra=nxtra+1
          norg=norg-1
         endif
        endfor

        if nxtra gt 0 then begin
           w_out(0:norg-1,*)  =in_wk(nxtra:nch-1,*)
           w_out(norg:nch-1,*)=w_buf(0:nxtra-1,*)
	  if n_elements(e_out) gt 1  then begin
           e_out(0:norg-1,*)  =e_in (nxtra:nch-1,*)
           e_out(norg:nch-1,*)=e_buf(0:nxtra-1,*)
	  endif
        endif
        if nxtra le 0 then w_out=in_wk

  
        bodge=1.0e6/telast
	for j=0,nch-1 do begin
	    time_diff=(epp-j)*cwidth
	    flt_time=telast-time_diff
	    lam=flt_time*3956.0/p_in(27)
	    x_out(j)=eelast-(81.8/lam^2)
	    w_out(j,*)=w_out(j,*)*flt_time^4*bodge
	    if n_elements(e_out) gt 1 then e_out(j,*)=e_out(j,*)*flt_time^4*bodge
	    endfor
	p_out=p_in
	x_tit(one)='Energy Transfer (meV)'
; Copies work-space stuff from simple arrays back to main LAMP arrays
      n_wk=string(out_wk)
      n_wk=strtrim(n_wk,2)

      jjj=execute('p' +n_wk+'=p_out' )
      jjj=execute('pv'+n_wk+'=pv_out')
      jjj=execute('x' +n_wk+'=x_out' )
      jjj=execute('y' +n_wk+'=y_out' )
      jjj=execute('z' +n_wk+'=z_out' )
      jjj=execute('e' +n_wk+'=e_out' )
;
;done @mac.out
	return,w_out
	end
