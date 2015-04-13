;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION vnorm, w_in, w_van0, min=ch1, max=ch2, tv=tv, Ns=Ns, $
		 ts=ts, cb1, cb2, ierr=ierr

;For IN4, IN5, IN6 and D7 data
;
; Normalises data in w_in vanadium data in w_van0 integrated between given limits.
;
;ARGUMENTS
; min	: lower time channel limit of integration over vanadium elastic peak
; max	: upper time channel limit
; tv	: thickness of vanadium sample
; Ns	: number density of sample (*10^22 per cm^3)
; ts	: effective sample thickness
; (cb1 and cb2 are obsolete and kept for backwards compatability)
;
;DIMENSIONS
; w_in=w_out(nchannels, nspectra)
;
;COMMAND SYNTAX
; w8=vnorm(w7,w20,min=<min>,max=<max>[,tv=<tv>,Ns=<Ns>,ts=<ts>])
;      - normalises data in w7 to vanadium data in w20,
; (optional arguments shown in square brackets)
;
;							JRS,KHA  15/7/02
;
;LAST MODIFICATION
; Modified by S. Rols 14/03/02 to perform vanadium normalisation of MiBeMol
; (LLB-CEA SACLAY), DCS (NIST USA) and QENS (IPNS USA) data.
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp_access, inst

	iprint = 0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start vnorm:'

	take_datp, datp
	take_datp, datvan, /third
	IF N_ELEMENTS(cb1) GT 0 THEN ch1 = cb1
	IF N_ELEMENTS(cb2) GT 0 THEN ch2 = cb2
;-------------------------------------------------------------------------------
;Check workspace sizes

	sw=SIZE(w_in)
	IF iprint THEN PRINT,'SIZE(w_in)=',sw
	nchannels=sw(1)
	IF sw(0) EQ 1 THEN nspectra = 1 ELSE nspectra = sw(2)

; Part added by SR on 14/03/02
; ** Decide if include the vanadium error or not (default=included)
; *****************************************************************

	ierrtype = 1
	IF N_ELEMENTS(ierr) GT 0 THEN ierrtype = 2
	CASE inst OF
		'QENS_raw':BEGIN
			   w_out = vnorm_qens(w_in, w_van0, ch1, ch2)
			   GOTO, finished
			   END
		'QENS':BEGIN
			   w_out = vnorm_qens(w_in, w_van0, ch1, ch2)
			   GOTO, finished0
			   END
		'MiBeMol':BEGIN
				par  = datp.p
				e_in = datp.e
				se   = SIZE(e_in)
				chw  = FLOAT(par(7))
				L2   = 3.58*1000.
			   END
		'DCSasc':BEGIN
				par  = datp.p
				e_in = datp.e
				se   = SIZE(e_in)
				chw  = FLOAT(par(14))
				L2   = FLOAT(par(6))
			   END
		ELSE:BEGIN
			 par  = datp.p
			 e_in = datp.e
			 se   = SIZE(e_in)
			 chw  = par(18)
			 L2   = par(27)*1000.
			 END
	ENDCASE
;-------------------------------------------------------------------------------


	IF inst EQ 'D7' THEN nphases = par(2)

	se = SIZE(e_in)
	IF iprint THEN PRINT,'SIZE(e_in) = ',se
	IF (se(0) NE sw(0)) OR (se(1) NE sw(1)) OR (se(2) NE sw(2)) THEN e_in = SQRT(w_in)

	w_van = w_van0	& sv = SIZE(w_van)

	e_van = datvan.e
	se = SIZE(e_van)
	IF (se(0) NE sv(0)) OR (se(1) NE sv(1)) OR (se(2) NE sv(2)) THEN e_van = SQRT(w_van)

	IF inst EQ 'D7' THEN BEGIN
		nvs = sv(2)
		IF iprint THEN PRINT,'D7: nspectra=',nspectra,' nvs=',nvs
		IF nspectra EQ 2*nvs THEN BEGIN
			w_van=[[w_van],[w_van]]
			e_van=[[e_van],[e_van]]
		ENDIF ELSE IF nspectra EQ 6*nvs THEN BEGIN
			w_van=[[w_van],[w_van],[w_van],[w_van],[w_van],[w_van]]
			e_van=[[e_van],[e_van],[e_van],[e_van],[e_van],[e_van]]
		ENDIF ELSE IF nvs NE nspectra THEN BEGIN
			s = 'vnorm: Error - no. of V spectra must be 1,2 or 6 times no. of w_in spectra'
			i = WIDGET_MESSAGE(s, /ERROR)
			GOTO, fin
		ENDIF
		zeroed = WHERE(e_in LE -1.,nz) ; zeroed spectrum numbers
		IF iprint THEN PRINT,nz/nchannels,' zeroed spectra
	ENDIF ELSE IF (sv(0) NE sw(0)) OR (sv(1) NE sw(1)) OR (sv(2) NE sw(2)) THEN BEGIN
		s = 'vnorm: Error - w_in and V data not same format'
		i = WIDGET_MESSAGE(s, /ERROR)
		GOTO, fin
	ENDIF

	IF iprint THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra

;-------------------------------------------------------------------------------------
;	Perform normalisation


;	FOR D7: reform vanadium into (nchannels, nspectra, nphases)
;	and sum over phases before normalisation

        IF inst EQ 'D7' THEN BEGIN
		wv_buf = REFORM(w_van,nchannels,nspectra/nphases,nphases)
		ev_buf = REFORM(e_van,nchannels,nspectra/nphases,nphases)
		V_buf  = TOTAL(wv_buf(ch1-1:ch2-1,*,*),1)
		dV_buf = SQRT(TOTAL(ev_buf(ch1-1:ch2-1,*,*)^2,1))
		V1  = TOTAL(V_buf,2)
		dV1 = SQRT(TOTAL(dV_buf^2,2))
		IF iprint THEN PRINT, 'nphases = ',nphases
		IF nphases EQ 6 THEN BEGIN
			V  = [V1,V1,V1,V1,V1,V1]
			dV = [dV1,dV1,dV1,dV1,dV1,dV1]
		ENDIF ELSE IF nphases EQ 2  THEN BEGIN
			V  = [V1,V1]
			dV = [dV1,dV1]
		ENDIF
	ENDIF ELSE BEGIN
		V  = TOTAL(w_van(ch1-1:ch2-1,*),1)
		dV = SQRT(TOTAL(e_van(ch1-1:ch2-1,*)^2,1))
	ENDELSE

	w_buf=FLTARR(nchannels,nspectra)
	e_buf=FLTARR(nchannels,nspectra)

	IF nspectra GT 1 THEN BEGIN
		V  = REFORM(V,1,nspectra)
		dV = REFORM(dV,1,nspectra)
		V  = (FLTARR(nchannels)+1.) # V
		dV = (FLTARR(nchannels)+1.) # dV

		nz=WHERE(V GT 0)
		w_buf(nz) = w_in(nz)/V(nz)
		IF ierrtype EQ 1 THEN $
			e_buf(nz) = SQRT((e_in(nz)/V(nz))^2 + (w_in(nz)*dV(nz)/V(nz)^2)^2) $
		ELSE BEGIN
			e_buf(nz) = e_in(nz)/V(nz)
			PRINT, 'vnorm:  Absolute errors not calcuated'
		ENDELSE
	ENDIF ELSE BEGIN
		w_buf = w_in/V
		IF ierrtype EQ 1 THEN $
			e_buf = SQRT((e_in/V)^2 + (w_in*dV/V^2)^2) $
		ELSE BEGIN
			e_buf = e_in/V
			PRINT, 'vnorm:  Absolute errors not calcuated'
		ENDELSE
	ENDELSE

	w_buf = REFORM(w_buf,nchannels,nspectra)
	e_buf = REFORM(e_buf,nchannels,nspectra)

	IF(N_ELEMENTS(Ns) GT 0) THEN BEGIN
		Ns = Ns*1.E22
		Nv = 7.197E22
		tv = FLOAT(tv)
		Ns = FLOAT(Ns)
		ts = FLOAT(ts)
                IF iprint THEN BEGIN
			PRINT, 'Absoulte Normalisation:'
			PRINT, 'Ns = ',Ns
			PRINT, 'Nv = ',Nv
			PRINT, 'ts = ',ts
			PRINT, 'tv = ',tv
		ENDIF
		w_out = ((Nv*tv)/(Ns*ts))*w_buf*5.08*L2/(4*!pi*chw)
		e_out = ((Nv*tv)/(Ns*ts))*e_buf*5.08*L2/(4*!pi*chw)
	ENDIF ELSE BEGIN
		w_out = w_buf
		e_out = e_buf
	ENDELSE

	novan = WHERE(V LE 0., n)
	IF n GE 1 THEN BEGIN
		V(novan)=1.
		w_out(novan) = 0.
		e_out(novan) = -1.
	ENDIF

	IF inst EQ 'D7' THEN BEGIN
		IF N_ELEMENTS(zeroed) GT 2 THEN BEGIN
			w_out(zeroed) = 0.
			e_out(zeroed) = -1.
		ENDIF
	ENDIF

	IF iprint THEN PRINT,'End of main section'
;-------------------------------------------------------------------------------------

	if n_elements(datp.e) eq n_elements(e_out) then datp.e=e_out

finished0:
	PRINT,'vnorm: normalised to V data, channels',ch1,' to',ch2

	title=datvan.other_tit
	IF (inst EQ 'D7') THEN i=3 ELSE i=4
	IF (STRMID(title,i,1) EQ '#') THEN BEGIN
		n=STRLEN(title)		& title=STRMID(title,i+1,n-i-1)
		i=STRPOS(title,' ')	& numor=STRMID(title,0,i)
	ENDIF ELSE BEGIN
		i=RSTRPOS(title,' ')	& n=STRLEN(title)
		numor=STRMID(title,i+1,n)
		IF (STRPOS(numor,'>') EQ -1) THEN $
			numor=STRTRIM(STRING(LONG(datvan.p(10))),2)
	ENDELSE
	numor = '#'+numor
	IF (N_ELEMENTS(Ns) EQ 0) THEN BEGIN
		s=' -vn('+numor+','+STRTRIM(STRING(ch1),2)+','+STRTRIM(STRING(ch2),2)+')'
	ENDIF ELSE BEGIN
		tvs=STRTRIM(STRING(tv),2)	& n=STRPOS(tvs,'.')+3	& tvs=STRMID(tvs,0,n)
		Nss=STRTRIM(STRING(Ns),2)	& n=STRPOS(Nss,'.')+3	& Nss=STRMID(Nss,0,n)
		tss=STRTRIM(STRING(ts),2)	& n=STRPOS(tss,'.')+3	& tss=STRMID(tss,0,n)
		abss=tvs+','+Nss+','+tss
		s=' -vn('+numor+','+STRTRIM(STRING(ch1),2)+','+STRTRIM(STRING(ch2),2)+','+abss+')'
	ENDELSE

	datp.other_tit=datp.other_tit+s

	give_datp, datp

finished:
	RETURN, w_out
fin:
	END


