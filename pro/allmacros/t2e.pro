;-------------------------------------------------------------------------------
;*******************************************************************************
;
;     FUNCTION t2e, w_in, in5multi = idet, ken = abs, id
;
;For IN4, IN5, IN6, HET and D7 data. Calls Don Kearley's old t2e for other instruments.
;
;Converts tof data in w_in to energy transfer in w_out. Includes the
;factor of ki/kf.
;
;KEYWORDS:
; /in5multi	: use data in IN5 multidetector
;		- otherwise use IN5 high angle detectors (default)
; /ken		: use Ken's expression for dEdt in t2e_tof.pro (not recommended)
;
; (id is obsolete and should not be used)
;
;DIMENSIONS:
; w_in(nchannels,nspectra) -> w_out(nE,spectra)
;
;COMMAND SYNTAX
; w10=t2e(w9[,/in5multi][,/norm])
;                                         KHA, JRS 16/7/02
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION t2e_tof, w_in, idet, ken

	COMMON c_lamp_access, inst
	COMMON printing, iprint, outstring
	COMMON constants, const1, const2, const3, const4

	IF iprint THEN PRINT,'Start t2e_tof:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy parameters

	par = datp.p

; ** One case to perform the transformation for MiBeMol data
; ** S. Rols 11/01 srols@anl.gov
; ** *****************************************************

	CASE inst OF
	'DCSasc':BEGIN
		lambda = FLOAT(par(8))
		print,'lambda=',lambda
      		chw    = FLOAT(par(14))
      		print,'ch_width=',chw
      		chel   = FLOAT(par(10))
      		print,'el. channel=',chel
      		L2     = FLOAT(par(6))*1.        ; sample-detector distance (mm)
      		print,'SD distance (mm)=',L2
		END
	ELSE:BEGIN
		lambda = par(21)
      		chw    = par(18)
      		chel   = par(9)
      		L2     = par(27)*1000.        ; sample-detector distance (mm)
		END
	ENDCASE

	det = 1
	IF N_ELEMENTS(idet) NE 0 THEN det=idet
	IF det EQ 0 THEN BEGIN
		IF inst NE 'IN5' THEN BEGIN
			s = 't2e: Error - "/in5multi" can only be used for IN5 data'
			j = DIALOG_MESSAGE(s, /ERROR)
  			GOTO, finished
		ENDIF
		L2 = L2 - 300.
	ENDIF
	IF inst EQ 'IN6' THEN L2 = 2470.

	IF iprint THEN PRINT,'lambda=',lambda,'A, chw=',chw,'mcs, chel=',chel
	IF iprint THEN PRINT,' L2=',L2,'mm'

	sw = SIZE(w_in)
	nchannels = sw(1)
	IF sw(0) EQ 1 THEN nspectra = 1 ELSE nspectra = sw(2)

	IF iprint THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	x_in = datp.x
	y_in = datp.y
	e_in = datp.e

	se = SIZE(e_in)
	IF (se(0) NE sw(0)) OR (se(1) NE sw(1)) THEN e_in = SQRT(w_in)

	IF iprint THEN PRINT, 't2e: End of check dimensions, etc. section'

;-------------------------------------------------------------------------------------
;     Set constants and prepare output arrays

	IF (inst EQ 'IN5') AND (nspectra GT 1) THEN BEGIN
		IF det EQ 0 THEN i = WHERE(y_in LT 10.,nspectra) $
			ELSE i = WHERE(y_in GT 10.,nspectra)
 		w_buf = w_in(*,i)
 		e_buf = e_in(*,i)
 		y_out=y_in(i)
	ENDIF ELSE BEGIN
		w_buf = w_in
		e_buf = e_in
		y_out = y_in
	ENDELSE

	Ei   = const4/lambda^2
	Vi   = SQRT(Ei/const1)
	Tel  = L2/Vi
	time = chw*FLOAT(x_in - chel) + Tel
	Ef   = const1*(L2/time)^2
	Eps  = Ei - Ef

;-------------------------------------------------------------------------------------
;     Transform to energy transfer

	IF N_ELEMENTS(ken) EQ 0 THEN dtdE = SQRT(const1/(4.*Ef^3)) $
		ELSE dtdE = SQRT(const1/Ef^3)/SQRT(const1/Ei^3)	;Ken's expression (!?)
	kikf = SQRT(Ei/Ef)

	IF iprint THEN BEGIN
            PRINT,'channel    time    Eps       dtdE      kikf'
            FOR i=0,nchannels-1 DO PRINT, FORMAT='(I4,2F10.3,6F10.6)', $
                  i+1, time(i), Eps(i), dtdE(i), kikf(i)
	ENDIF

	fact=(dtdE*kikf) # (FLTARR(1,nspectra) + 1.)

	x_out = Eps
	w_out = w_buf*fact
	e_out = e_buf*fact

	IF iprint THEN PRINT,'t2e: End of t2e-ing section'

;-------------------------------------------------------------------------------------
; Prepare data and return

	s = STRTRIM(STRING(Ei),2) & i = RSTRPOS(s,'.') & Ei = STRMID(s,0,i + 3)
	outstring = 'Ei=' + Ei + 'meV'
	IF inst EQ 'IN5' THEN BEGIN
  		IF det EQ 0 THEN dstring = 'multidetector data' $
			ELSE dstring = 'high-angle detector data'
		outstring = outstring + ', ' + dstring
	ENDIF

	bit=' -t2e(Ei='+Ei
	IF inst EQ 'IN5' THEN bit = bit + ',' + STRTRIM(STRING(det),2) + ')' $
		ELSE bit = bit + ')'
	datp.other_tit = datp.other_tit + bit
	datp.x_tit = 'Energy Transfer (meV)'

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out
	IF inst EQ 'IN5' THEN mod_datp, datp, "y", y_out
	give_datp, datp

finished:
      RETURN, w_out
      END


;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION t2e_d7, w_in, abs

	COMMON printing, iprint, outstring
	COMMON constants, const1, const2, const3, const4

	IF iprint THEN PRINT,'Start t2e_d7:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy parameters

	par    = datp.p
	lambda = par(4)
	chw    = par(7)
	chel   = par(9)

	IF iprint THEN PRINT,'lambda=',lambda,'A, chw=',chw,'mcs, chel=',chel

	TOF = FIX(par(8))
	IF NOT TOF THEN BEGIN
            j = DIALOG_MESSAGE(' t2e: Error - workspace data is not in TOF', /ERROR)
            GOTO, finished
	ENDIF

	nspectra  = FIX(par(1))
	nphases   = FIX(par(2))
	nchannels = FIX(par(6))
	nspectra  = nspectra*nphases
	sw        = SIZE(w_in)

	IF iprint THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	y_in = datp.y
	e_in = datp.e
	se = SIZE(e_in)
	IF (se(0) NE sw(0)) OR (se(1) NE sw(1)) THEN BEGIN
		j = DIALOG_MESSAGE('t2e: no error bars defined', /ERROR)
		GOTO, finished
	ENDIF

	IF iprint THEN PRINT, 't2e: End of check workspaces, etc. section'

;-------------------------------------------------------------------------------------
;     Set constants and prepare output arrays

	w_out = w_in
	e_out = e_in
	x_out = FLTARR(nchannels)
	y_out = datp.y

	L2 = 1500.0         ; sample-detector distance (mm)

	Ei = const4/lambda^2
	Vi = SQRT(Ei/const1)
	Tel = L2/Vi

;-------------------------------------------------------------------------------------
;     Transform to energy transfer and correct data

	time  = chw*FLOAT(INDGEN(nchannels) - chel) + Tel
	Ef    = const1*(L2/time)^2
	Eps   = Ei - Ef
	dtdE  = time/(2.*Ef)
	kikf  = SQRT(Ei/Ef)
	x_out = Eps

	IF iprint  THEN BEGIN
		PRINT,'channel    time    Eps       dtdE      kikf'
 		FOR i=0,nchannels-1 DO $
			PRINT, FORMAT='(I4,2F10.3,6F10.6)', i, time(i), $
			Eps(i), dtdE(i), kikf(i)
	ENDIF

	fact=(dtdE*kikf) # (FLTARR(1,nspectra) + 1.)
	w_out = w_in*fact
	e_out = e_in*fact

	zeroed = WHERE(e_in LT -0.9,nz)
	IF nz NE 0 THEN BEGIN
		w_out(zeroed) = 0.
		e_out(zeroed) = -1.
	ENDIF

	IF iprint THEN PRINT, 't2e: End of t2e-ing section'

;------------------------------------------------------------------------------------
; Prepare output arrays and return

	s = STRTRIM(STRING(Ei),2) & i = RSTRPOS(s,'.') & Ei = STRMID(s,0,i+3)
	outstring = 'Ei=' + Ei + 'meV'
	datp.x_tit = 'Energy Transfer (meV)'
	IF datp.z_tit EQ 'X-section (b/ster/f.u./mcs)' THEN $
		datp.z_tit='S(omega,E)'
	datp.other_tit=datp.other_tit+' -t2e(Ei='+Ei+')'

	mod_datp, datp, "x", x_out
	datp.e = e_out

	give_datp, datp

finished:
	RETURN, w_out
	END

;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION t2e_HET, w_in

	COMMON printing, iprint, outstring
	COMMON constants, const1, const2, const3, const4

	IF iprint THEN PRINT,'Start t2e_HET:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy parameters

	par = datp.p

	lambda = par(21)
	tel2p5 = par(9)
	tel4   = par(8)
	L2     = datp.z*1000.       ; sample-detector distance (mm)

	IF iprint THEN PRINT,'A, tel(2.5m)=',tel2p5,'mcs, tel(4m)=',tel4

	sw = SIZE(w_in)
	nchannels = sw(1)
	IF sw(0) EQ 1 THEN nspectra = 1 ELSE nspectra = sw(2)

	IF iprint THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	x_in = datp.x
	y_in = datp.y
	e_in = datp.e

	se = SIZE(e_in)
	IF (se(0) NE sw(0)) OR (se(1) NE sw(1)) THEN e_in = SQRT(w_in)

	IF iprint THEN PRINT, 't2e: End of check workspaces, etc. section'

;-------------------------------------------------------------------------------------
;     Set constants and prepare output arrays

	Ei   = const4/lambda^2
	Vi   = SQRT (Ei/const1)
	Tel  = L2/Vi
	time = x_in

	Ef = FLTARR(nchannels,nspectra)

	i = WHERE(L2 GT 2300 AND L2 LT 2700, ni)	; 2,5m bank
	FOR ii = 0, ni - 1 DO $
		Ef[*,i[ii]] = const1*(L2[i[ii]]/(time - tel2p5 + Tel[i[ii]]))^2
	i = WHERE(L2 GT 3800 AND L2 LT 4200, ni)	; 4m bank
	FOR ii = 0, ni - 1 DO $
		Ef[*,i[ii]] = const1*(L2[i[ii]]/(time - tel4 + Tel[i[ii]]))^2
	Eps = Ei - Ef
	p = MAX(TOTAL(Ef,2),imax)			;find t = 0 channel

	x_out =  Eps[imax+1:nchannels-1,*]
	Ef    =   Ef[imax+1:nchannels-1,*]
	w_buf = w_in[imax+1:nchannels-1,*]
	e_buf = e_in[imax+1:nchannels-1,*]

;-------------------------------------------------------------------------------------
; Transform to energy transfer

	dtdE = SQRT(const1/(4.*Ef^3))
	kikf = SQRT(Ei/Ef)
	fact = (dtdE*kikf)

	w_out = w_buf*fact
	e_out = e_buf*fact

	IF iprint GT 0 THEN PRINT,'End of t2e-ing section'

;-------------------------------------------------------------------------------------
; Prepare data and return

	s = STRTRIM(STRING(Ei),2) & i = RSTRPOS(s,'.') & Ei = STRMID(s,0,i+3)
	outstring = 'Ei=' + Ei + 'meV'

	bit=' -t2e(Ei='+ Ei + ')'
	datp.other_tit = datp.other_tit + bit
	datp.x_tit = 'Energy Transfer (meV)'

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

	give_datp, datp

finished:
	RETURN, w_out
	END


;-------------------------------------------------------------------------------
;*******************************************************************************

function t2e_don,in_wk
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

;       set defaults
        n_out =n_in
        p_out =p_in
        pv_out=pv_in
        x_out =x_in
        y_out =y_in
        z_out =z_in
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
;
;done @mac.out

return,in_wk
  endif

      coef=fltarr(nch)
      x_out=fltarr(nch)
        w_out=fltarr(nch,nang)
        w_buf=fltarr(nch,nang)
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
        nxtra=nxtra+1
        norg=norg-1
        endif
        endfor

        if nxtra gt 0 then begin
        w_out(0:norg-1,*)=in_wk(nxtra:nch-1,*)
        w_out(norg:nch-1,*)=w_buf(0:nxtra-1,*)
        endif
        if nxtra le 0 then w_out=in_wk


        bodge=1.0e6/telast
      for j=0,nch-1 do begin
          time_diff=(epp-j)*cwidth
          flt_time=telast-time_diff
          lam=flt_time*3956.0/p_in(27)
          x_out(j)=eelast-(81.8/lam^2)
          w_out(j,*)=w_out(j,*)*flt_time^4*bodge
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
;
;done @mac.out
      return,w_out
      end

;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION t2e, w_in, in5multi = idet, ken = abs, id

	COMMON c_lamp_access, inst
	COMMON printing, iprint, outstring
	COMMON constants, const1, const2, const3, const4

	iprint = 0    ; if iprint>0, show debugging messages

	const1 = 5.22697          ; E(meV)=const1*V(m/ms)^2 for neutron
	const2 = 2.07193571       ; E(meV)=const2*k(A^-1)^2 for neutron
	const3 = 3.956076         ; V(m/ms)=const3/lambda(A) for neutron
	const4 = 81.8066          ; E(meV)=const4/lambda(A)^2 for neutron

	IF iprint THEN PRINT,'Start t2e:'

	IF KEYWORD_SET(in5multi) THEN idet=0
	IF N_ELEMENTS(id) GT 0 THEN idet=id

;-------------------------------------------------------------------------------
;Check instrument name and call appropriate function

	IF (inst EQ 'IN4') OR (inst EQ 'IN5') OR (inst EQ 'IN6') OR $
	   (inst EQ 'MiBeMol') OR (inst EQ 'DCSasc') THEN $
		w_out = t2e_tof(w_in, idet, abs) $
	ELSE IF inst EQ 'D7' THEN $
		w_out = t2e_d7(w_in) $
	ELSE IF inst EQ 'HET' THEN $
		w_out = t2e_HET(w_in) $
	ELSE w_out = t2e_don(w_in)

;-------------------------------------------------------------------------------
;Return parameters and exit

	IF N_ELEMENTS(outstring) GT 0 THEN PRINT,'t2e: '+outstring

	RETURN, w_out
	END


