;-------------------------------------------------------------------------------
;*******************************************************************************
;
;	FUNCTION corr_tof, w_in, det_eff=efficiency,frameoverlap=frameoverlap, $
;		 bkgd_trans=bkgd_trans, ana_eff=ana_eff, ef1, fr1, bg1
;
;For IN4, IN5, IN6 and D7 data.
;
;KEYWORDS:
; /det_eff	 : corrects for energy-variation of detector efficiency
; /frameoverlap: subtracts a t^-4 tail from the beginning
;			of the time frame
; /bkgd_trans  : subtracts a flat background in each detector,
;			found using a moving filter (For IN4, IN5 and IN6 only)
; /ana_eff	 : correct for energy dependence of analyser transmission
;			(For D7 only)
; (ef1, fr1, and bg1 are obsolete and kept for backwards compatability)	
;
;DIMENSIONS
;   w_in=w_out(nchannels,nphases*nspectra)
;
;COMMAND SYNTAX
;   w6=corr_tof(w5[,/det_eff][,/frameoverlap][,/bkgd_trans][,/ana_eff])
;
;   (optional keywords shown in square brackets)
;
;						KHA,JRS 7/8/00
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION corr_tof_tof, w_in, efficiency, frameoverlap, bkgd

	COMMON c_lamp_access, inst
	COMMON printing, iprint, outstring

	IF (iprint GT 0) THEN PRINT,'Start corr_tof_tof:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy parameters

	par=datp.p

	lambda=par(21)
	chw=par(18)
	chel=par(9)	   ; elastic channel (if IN5, for high-angle detectors)
	L2=par(27)*1000.	  ; sample-detector distance (mm)
	IF (inst EQ 'IN5') THEN BEGIN
		L2det=L2	  ; sample-detector distance (mm)
		L2mul=L2det-300.	; sample-multidetector distance
	ENDIF ELSE IF (inst EQ 'IN6') THEN $
		par(14)=1.	; rep period multiplier is 1 on IN6

	period=par(13)*par(14)	; repetition period (mcs)

	IF (N_ELEMENTS(efficiency) NE 0) THEN corr_e=efficiency ELSE corr_e=0
	IF (N_ELEMENTS(frameoverlap) NE 0) THEN corr_f=frameoverlap ELSE corr_f=0
	IF (N_ELEMENTS(bkgd) NE 0) THEN corr_b=bkgd ELSE corr_b=0

	IF (iprint GT 0) THEN BEGIN
		PRINT,'lambda=',lambda,'A, chw=',chw,'mcs, chel=',chel
		PRINT,' L2=',L2,'mm period=',period,'mcs'
		PRINT,' corr_e=',corr_e,' corr_f=',corr_f,' corr_b=',corr_b
	ENDIF

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	nchannels=sw(1)
	IF (sw(0) EQ 1) THEN nspectra=1	 ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,w_in

	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	x_in=datp.x
	y_in=datp.y
	e_in=datp.e

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1) OR se(2) NE sw(2)) THEN e_in=SQRT(w_in)

	IF(iprint GT 0) THEN PRINT,'corr_tof: End of check dimensions section'

;-------------------------------------------------------------------------------------
;   Set constants and prepare output arrays

	const1=5.22697	; E(meV)=const1*V(m/ms)^2 for neutron
	const2=2.07193571   ; E(meV)=const2*k(A^-1)^2 for neutron
	const3=3.956076	 ; V(m/ms)=const3/lambda(A) for neutron
	const4=81.8066	; E(meV)=const4/lambda(A)^2 for neutron

	w_out=w_in
	e_out=e_in
	x_out=FLTARR(nchannels)
	y_out=y_in

	Ei=const4/lambda^2
	Vi=SQRT(Ei/const1)

	Tsam=chel*chw-L2/Vi

	IF (inst EQ 'IN5') THEN BEGIN
		cheldet=chel	  & Teldet=L2det/Vi
		timedet=chw*FLOAT(x_in-cheldet)+Teldet
		Efdet=const1*(L2det/timedet)^2
		Epsdet=Ei-Efdet
		chelmul=chel-300./Vi	& Telmul=L2mul/Vi
		timemul=chw*FLOAT(x_in-chelmul)+Telmul
		Efmul=const1*(L2mul/timemul)^2
		Epsmul=Ei-Efmul
	ENDIF ELSE BEGIN
		Tel=L2/Vi
		time=chw*FLOAT(x_in-chel)+Tel
		Ef=const1*(L2/time)^2
		Eps=Ei-Ef
		wave=SQRT(const4/Ef)
	ENDELSE

;-------------------------------------------------------------------------------------
;   Perform background correction

	IF (corr_b EQ 1) THEN BEGIN
		iw=10
		FOR ispec=0,nspectra-1 DO BEGIN
			S=w_out(*,ispec)
			Ss=SMOOTH(S,2*iw+1)
			bkgd=MIN(Ss(iw:nchannels-iw-1))>0.
			IF (iprint GT 0) THEN PRINT,'ispec=',ispec,' bkgd=',bkgd
			w_out(*,ispec)=S-bkgd
		ENDFOR
	ENDIF

	IF (iprint GT 0) THEN PRINT,'corr_tof_tof: End of background correction section'
;-------------------------------------------------------------------------------------
;   Prepare for frame overlap correction

	IF (corr_f EQ 1) THEN BEGIN
		nfit=10
		ifit1=nchannels-nfit
		ifit2=nchannels-1
		IF (inst EQ 'IN5') THEN BEGIN
			Tdet=chw*(FLOAT(x_in(ifit1)+x_in(ifit2))/2.-cheldet)+Teldet
			time1det=timedet+period
			Tmul=chw*(FLOAT(x_in(ifit1)+x_in(ifit2))/2.-chelmul)+Telmul
			time1mul=timemul+period
		ENDIF ELSE BEGIN
			T=chw*(FLOAT(x_in(ifit1)+x_in(ifit2))/2.-chel)+Tel ; mean time of integrating region
			time1=time+period   ; frame-overlap time-of-flight
		ENDELSE
	ENDIF ELSE BEGIN
		T=0.	& time1=0.
		Tdet=0. & time1det=0.
		Tmul=0. & time1mul=0.
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'corr_tof_tof: End of frame overlap correction section'
;-------------------------------------------------------------------------------------
;   Prepare for detector efficiency correction

	IF (corr_e EQ 0) THEN BEGIN
		eff=1.
		effdet=1.
		effmul=1.
	ENDIF ELSE BEGIN
		IF (inst EQ 'IN5') THEN BEGIN   ; use expressions for D7
			effdet=(1.-exp(-5.6/SQRT(Efdet)))/(1.-exp(-5.6/SQRT(Ei)))
			effmul=(1.-exp(-5.6/SQRT(Efmul)))/(1.-exp(-5.6/SQRT(Ei)))
		ENDIF ELSE IF (inst EQ 'IN6') THEN BEGIN ; use expressions from Y. Blanc's report
			eff=0.94*(1.-EXP(-0.363*wave)) & i=WHERE(wave GE 4., n)
			IF (n GT 0) THEN eff(i)=EXP(-0.00625*wave(i))*(1.-EXP(-0.363*wave(i)))
			eff0=EXP(-0.00625*lambda)*(1.-EXP(-0.363*lambda))
			eff=eff/eff0	; normalise to 1 at elastic peak
		ENDIF ELSE $		; use expressions for D7
		eff=(1.-exp(-5.6/SQRT(Ef)))/(1.-exp(-5.6/SQRT(Ei)))
	ENDELSE

;-------------------------------------------------------------------------------------
;   Perform frame-overlap and detector efficiency corrections

	FOR ispec=0,nspectra-1 DO BEGIN
		S=w_out(*,ispec)	& dS=e_out(*,ispec)
	  	IF (inst EQ 'IN5') THEN BEGIN
			IF (y_in(ispec) LE 10.) THEN BEGIN
				T=Tmul & time1=time1mul & eff=effmul
			ENDIF ELSE BEGIN
				T=Tdet & time1=time1det & eff=effdet
			ENDELSE
		ENDIF
		IF (corr_f EQ 1) THEN BEGIN
			Y=TOTAL(S(ifit1:ifit2),1)/FLOAT(nfit)
			A=Y*T^4 & S=S-A/time1^4
		ENDIF
		w_out(*,ispec)=S/eff
		e_out(*,ispec)=dS/eff
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'corr_tof_tof: End of main section'
;-------------------------------------------------------------------------------------

	datp.other_tit=datp.other_tit+' -ct('+STRTRIM(STRING(corr_e),2) $
	  +','+STRTRIM(STRING(corr_f),2)+','+STRTRIM(STRING(corr_b),2)+')'

	IF (corr_e EQ 0) THEN estring='no det eff corr' ELSE estring='with det eff corr'
	IF (corr_f EQ 0) THEN fstring='no frame overlap corr' ELSE fstring='with frame overlap corr'
	IF (corr_b EQ 0) THEN bstring='no bkgd corr' ELSE bstring='with bkgd corr'
	outstring=estring+', '+fstring+', '+bstring

	give_datp, datp

finished:
	IF (iprint GT 0) THEN PRINT,'End corr_tof_tof:'

	RETURN, w_out
	END


;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION corr_tof_d7, w_in, ieff, iover, itrans

	COMMON c_lamp
	COMMON printing, iprint, outstring

	IF (iprint GT 0) THEN PRINT,'Start corr_tof_d7:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input file and energy parameters
	
	trans_file=FILEPATH('transmission.dat',ROOT_DIR=lamp_macro+'/D7')

;analyser transmission from H2 experiment of Mompean

	rmlp=0	; rmlp=0 => leave last point as it is
		; rmlp=1 => remove last point (set = neighbouring point)
	par=datp.p

	lambda=par(4)
	freq=par(5)
	chw=par(7)
	chel=par(9)

	IF (iprint GT 0) THEN PRINT,'lambda=',lambda,' chel=',chel
	IF (iprint GT 0) THEN PRINT,'freq=',freq,'rpm   chw=',chw,'mcs'

	TOF=FIX(par(8))
	IF (TOF NE 1) THEN BEGIN
		PRINT,' corr_tof_d7: Error - workspace data is not in TOF'
		return,w_in
	ENDIF

	IF (freq LE 0. OR freq GT 12000.) THEN BEGIN
		PRINT,'corr_tof_d7: freq=0 in workspace parameters. Assume 8000rpm'
		freq=8000.
	ENDIF

	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nchannels=FIX(par(6))

	sw=SIZE(w_in)

	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	y_in=datp.y
	e_in=datp.e

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in)=',sw
		PRINT,'	  SIZE(e_in)=',se
		PRINT,'	- must be the same size'
		return,w_in
	ENDIF

	zeroed=WHERE(e_in LE -1.,n) ; zeroed channels
	IF (iprint GT 0) THEN PRINT,n/nchannels,' zeroed spectra

	eff0=0
	IF (N_ELEMENTS(ieff) NE 0) THEN eff0=ieff
	trans0=0
	IF (N_ELEMENTS(itrans) NE 0) THEN trans0=itrans
	over0=0
	IF (N_ELEMENTS(iover) NE 0) THEN over0=iover

	IF (iprint GT 0) THEN BEGIN
		PRINT,'efficiency=',eff0
		PRINT,'transmission=',trans0
		PRINT,'overlap=',over0
	ENDIF

	IF (eff0 EQ 0 AND trans0 EQ 0 AND over0 EQ 0) THEN BEGIN
		return,w_in
	ENDIF

	IF (iprint GT 0) THEN PRINT,'corr_tof_d7: End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------------
;   Set constants and prepare arrays

	eff=FLTARR(nchannels)
	wave=FLTARR(nchannels)

	const1=5.22697	; E(meV)=const1*V(m/ms)^2 for neutron
	const2=2.07193571   ; E(meV)=const2*Q(A^-1)^2 for neutron
	const3=3.956076	 ; V(m/ms)=const3/lambda(A) for neutron
	const4=81.8066	; E(meV)=const4/lambda(A)^2 for neutron

	L2=1500.0	 ; sample-detector distance (mm)

	Ei=const4/lambda^2
	Vi=SQRT(Ei/const1)
	Tel=L2/Vi

	time=chw*FLOAT(INDGEN(nchannels)-chel)+Tel
	Ef=const1*(L2/time)^2

	eff=(1.-exp(-5.6/SQRT(Ef)))/(1.-exp(-5.6/SQRT(Ei)))

	wave=SQRT(const4/Ef)

	IF (over0 EQ 1) THEN BEGIN
		IF (freq LE 0.) THEN BEGIN
			PRINT,'Corr_tof_d7: Error - freq =< 0'
			return,w_in
		ENDIF
		freq=freq/60.	 ; convert to Hz
		period=1.E+06/freq  ; period in mcs
		period=period/4.	; since there are four slits in chopper disk
		nfit=10	   ; no.of points for fit of y=A*t^-4
		ifit1=nchannels-nfit
		ifit2=nchannels-2
		T=chw*(FLOAT(ifit1+ifit2)/2.-chel)+Tel ; mean time of integrating region
		time1=time+period   ; frame-overlap time-of-flight
	ENDIF

	IF (iprint GT 0) THEN BEGIN
		PRINT,'lambda=',lambda,' Ei=',Ei,' freq=',freq,' chw=',chw
		PRINT,'Ef=',Ef
		PRINT,'eff=',eff
		PRINT,'wave=',wave
	ENDIF

	IF (iprint GT 0) THEN PRINT,'corr_tof_d7: End of "Set constants etc." section'

;-------------------------------------------------------------------------------
;Open and read from input file

	line=''

	npts=136
	A=FLTARR(3,npts)
	OPENR, 1, trans_file, ERROR=err
	IF (err NE 0) THEN PRINT, !ERR_STRING
	READF, 1, A
	CLOSE, 1
	Wax=FLTARR(npts)	& Trx=Wax   & dTrx=Wax
	Wax(*)=A(0,*)	 & Trx(*)=A(1,*) & dTrx(*)=A(2,*)
	coeff=POLY_FIT(Wax,Trx,9)
	trans1=INTERPOL(Trx,Wax,wave)
	trans2=FLOAT(POLY(wave,coeff))
	i1=0	& i2=0
	FOR i=1,nchannels-1 DO BEGIN
		IF (wave(i-1) LT 2.4) AND (wave(i) GE 2.4) THEN i1=i
		IF (wave(i-1) LT 3.5) AND (wave(i) GE 3.5) THEN i2=i
		IF (wave(i-1) LE lambda) AND (wave(i) GT lambda) THEN i0=i
	ENDFOR
	trans=trans2
	trans(i1:i2)=trans1(i1:i2)	; interpolate to get bump and dip
			  		; at lambda=2.5 to 3 Angstrom
	transm=trans(i0)+(lambda-wave(i0)) $
			*(trans(i0+1)-trans(i0))/(wave(i0+1)-wave(i0))
	trans=trans/transm

	IF (iprint GT 0) THEN BEGIN
		PRINT,' ichannel  wavelength  transmission  efficiency'
		FOR i=0,nchannels-1 DO PRINT, i, wave(i), trans(i), eff(i)
	ENDIF

	IF (iprint GT 0) THEN PRINT,'corr_tof_d7: End of "read from input files" section'

;-------------------------------------------------------------------------------
;Perform correction


	S=FLTARR(nchannels) & dS=S
	w_out=w_in
	e_out=e_in

	IF (rmlp EQ 1) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'Corr_tof: Setting last pt = next-last point'
		w_out(nchannels-1,*)=w_out(nchannels-2,*)
		e_out(nchannels-1,*)=e_out(nchannels-2,*)
	ENDIF

	Ecorr=FLTARR(nchannels) & Ecorr(*)=1.

	IF (eff0 EQ 1) THEN Ecorr=Ecorr/eff

	IF (trans0 EQ 1) THEN Ecorr=Ecorr/trans
	IF (iprint NE 0) THEN print, 'nphases*nspectra-1',nphases*nspectra-1

	FOR ispec=0,nphases*nspectra-1 DO BEGIN
		S=w_out(*,ispec)	& dS=e_out(*,ispec)
		IF (over0 EQ 1) THEN BEGIN
			Y=TOTAL(S(ifit1:ifit2),1)/FLOAT(nfit)
			A=Y*T^4
			IF (iprint GT 0 AND ispec EQ 0) THEN BEGIN
				PRINT,'channel	time	wave	S	fr_ovlp	 eff	 trans'
				FOR i=0,nchannels-1 DO $
				PRINT, FORMAT='(I4,2F10.3,4F10.6)', i, time(i), wave(i), $
				S(i), A/time1(i)^4, eff(i), trans(i)
			ENDIF
			S=S-A/time1^4
	  	ENDIF
		w_out(*,ispec)=S*Ecorr
		e_out(*,ispec)=dS*Ecorr
	ENDFOR

	IF(n GT 0) THEN BEGIN
		w_out(zeroed)=0.	& e_out(zeroed)=-1.
		ENDIF

	IF (iprint GT 0) THEN PRINT,'corr_tof_d7: End of Correction section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.e=e_out

	datp.other_tit=datp.other_tit+' -ct('+STRTRIM(STRING(eff0),2) $
	  +','+STRTRIM(STRING(over0),2)+','+STRTRIM(STRING(trans0),2)+')'

	IF (eff0 EQ 0) THEN estring='no det eff corr' ELSE estring='with det eff corr'
	IF (over0 EQ 0) THEN fstring='no frame ovlp corr' ELSE fstring='with frame ovlp corr'
	IF (trans0 EQ 0) THEN tstring='no ana trans corr' ELSE tstring='with ana trans corr'
	outstring=estring+', '+fstring+', '+tstring

finished:
	IF (iprint GT 0) THEN PRINT,'End corr_tof_d7:'

	give_datp, datp

	RETURN, w_out
	END

;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION corr_tof, w_in, det_eff=efficiency,frameoverlap=frameoverlap, $
		 bkgd_trans=bkgd_trans, ana_eff=ana_eff, ef1, fr1, bg1

	COMMON c_lamp_access, inst
	COMMON c_lamp
	COMMON printing, iprint, outstring

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start corr_tof:'
	IF(N_ELEMENTS(ef1) GT 0) THEN efficiency=ef1
	IF(N_ELEMENTS(fr1) GT 0) THEN frameoverlap=fr1
	IF(N_ELEMENTS(bg1) GT 0) THEN BEGIN
		ana_eff=bg1
		bkgd_trans=1
	ENDIF

;-------------------------------------------------------------------------------
;Check instrument name and call appropriate function

	IF (inst EQ 'IN4' OR inst EQ 'IN5' OR inst EQ 'IN6') THEN $
		w_out=corr_tof_tof(w_in,efficiency,frameoverlap,bkgd_trans) $
	ELSE IF (inst EQ 'D7') THEN $
		w_out=corr_tof_d7(w_in,efficiency,frameoverlap,ana_eff) $
	ELSE BEGIN
		PRINT,'corr_tof: Error - instrument must be IN4, IN5, IN6 or D7'
		return,w_in
	ENDELSE

;-------------------------------------------------------------------------------
;Return parameters and exit

	PRINT,'corr_tof: '+outstring

finished:
	RETURN, w_out
	END
