	FUNCTION norm_water, w_in0, Vrun, Mrun, AS, bd, wt, st, sd, TW
;
; Normalise intensities using water data. 
; Water integrals are in a file with number given by Vrun. 
; For absolute intensity normalisation, additional arguments 
; are required:
;		AS = atomic mass per f.u.	(amu)
;		bd = beam diameter		(mm)
;		wt = water thickness		(mm)
;		st = sample thickness		(mm)
;		sd = sample density		(g/cm^3)
;		TW = water transmission 	 
;
;							JRS 16/6/00
	iprint=0	; if iprint>0, show debugging messages

	ON_IOERROR, finished

	IF (iprint GT 0) THEN PRINT,'Start norm_water:'

	take_datp, datp
;-------------------------------------------------------------------------------
;Check dimensions of input workspaces
	sw=SIZE(w_in0)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in0)=',sw

	par=datp.p
	parv=datp.pv
	IF (sw(0) EQ 3) THEN nruns=sw(3) ELSE nruns=1
	IF (nruns EQ 1) THEN parv=par
	nspectra=sw(1)
	x_in0=datp.x
	y_in0=datp.y
	e_in0=datp.e

	se=SIZE(e_in0)

	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in0)=',sw
		PRINT,'        SIZE(e_in0)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	w_in=w_in0
	e_in=e_in0

	IF (iprint GT 0) THEN BEGIN
		PRINT,'nspectra=',nspectra
		PRINT,'nruns=',nruns
	ENDIF

	IF (N_ELEMENTS(Vrun) EQ 1) THEN BEGIN
		nVs=1
		V_file='water_'+STRTRIM(STRING(Vrun),2)+'.dat'
		PRINT,'Norm_water: Normalise to water file ',V_file
	ENDIF ELSE BEGIN
		PRINT,'norm_water: Error - must include water file number'
	ENDELSE
	IF (N_ELEMENTS(Mrun) EQ 1) THEN BEGIN
		M_file='mask_'+STRTRIM(STRING(Mrun),2)+'.dat'
		PRINT,'Norm_water: Using mask file ',M_file
	ENDIF

	absol=0
	IF (N_ELEMENTS(AS) NE 0) THEN absol=1

	IF (iprint GT 0) THEN PRINT,'absol=',absol

	absnorm=1.
	IF (absol EQ 1) THEN BEGIN
		sv=!pi*((bd/2)^2)*st	; sample volume (mm^3)
		sv=sv/1000.		; sample volume (cm^3)
		mS=sv*sd		; sample mass (g)
		wt=wt/10.		; water thickness (cm)
		amu=1.66E-24		; amu in g
		NS=mS/(AS*amu)		; number of sample formula units
		wv=!pi*((bd/2)^2)*wt	; water volume (mm^3)
		wv=wv/1000.		; water volume (cm^3)
		AV=3.34E22		; number density of water at RT
		NV=AV*wv		; number of water moleules
		sigmaV=-ALOG(TW)/(4.*!pi*wt*AV*1E-24) 	; water cross-section in b/ster
		absnorm=sigmaV*NV/NS
		PRINT, 'Sigma of Water is ',sigmaV*4*!pi
		PRINT,'Sample: m=',mS,'g,     A=',AS,'amu  N.f.u.=',NS
		PRINT,' Water: t=',wt,'cm, diam=',bd,'mm   N.f.u.=',NV,'  => absnorm=',absnorm
	ENDIF

	IF (iprint GT 0) THEN PRINT,'absnorm=',absnorm

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open and read from input files

	line=''
	Varray=FLTARR(5,nspectra^2)
	V_buf=FLTARR(nspectra^2)		& dV_buf=V_buf
	IF (iprint GT 0) THEN PRINT,'Opening water file: ',V_file(0)
	OPENR, 1, V_file(0), ERROR=err
	IF (err NE 0) THEN PRINT, !ERR_STRING
	READF, 1, line	& READF, 1, line
	READF, 1, Varray
	CLOSE, 1
	V_buf(*)=Varray(3,*)
	dV_buf(*)=Varray(4,*)
	V=FLTARR(nspectra^2,nruns)	& dV=V
	FOR irun=0,nruns-1 DO BEGIN
		V(*,irun)=V_buf(*)
		dV(*,irun)=dV_buf(*)
	ENDFOR

	IF (N_ELEMENTS(Mrun) NE 0) THEN BEGIN
		line=''
		Marray=FLTARR(5,nspectra^2)
		M_buf=FLTARR(nspectra^2)	& dM_buf=M_buf
		IF (iprint GT 0) THEN PRINT,'Opening mask file: ',M_file(0)
		OPENR, 2, M_file(0), ERROR=err
		IF (err NE 0) THEN PRINT, !ERR_STRING
		READF, 2, line	& READF, 2, line
		READF, 2, Marray
		CLOSE, 2
		M_buf(*)=Marray(3,*)
		dM_buf(*)=Marray(4,*)
		M=FLTARR(nspectra^2,nruns)	& dM=M
		FOR irun=0,nruns-1 DO BEGIN
			M(*,irun)=M_buf(*)
			dM(*,irun)=dM_buf(*)
		ENDFOR
	ENDIF


	IF (iprint GT 0) THEN PRINT,'End of "read from input files" section'

;-------------------------------------------------------------------------------
;Perform Normalisation

	normf=w_in*0.	& dnormf=normf

	FOR irun=0,nruns-1 DO BEGIN
		FOR ispec=0,(nspectra)-1 DO BEGIN
			FOR jspec=0,(nspectra-1) DO BEGIN
				normf(ispec,jspec,irun)=V(((ispec*nspectra)+jspec),irun)
				dnormf(ispec,jspec,irun)=dV(((ispec*nspectra)+jspec),irun)
			ENDFOR
		ENDFOR
	ENDFOR
	IF (N_ELEMENTS(Mrun) NE 0) THEN BEGIN
		maskf=w_in*0.	& dmaskf=maskf
		FOR irun=0,nruns-1 DO BEGIN
			FOR ispec=0,(nspectra)-1 DO BEGIN
				FOR jspec=0,(nspectra-1) DO BEGIN
					maskf(ispec,jspec,irun)=M(((ispec*nspectra)+jspec),irun)
					dmaskf(ispec,jspec,irun)=dM(((ispec*nspectra)+jspec),irun)
				ENDFOR
			ENDFOR
		ENDFOR
	ENDIF ELSE BEGIN
		maskf=w_in*0.	& dmaskf=maskf
		FOR irun=0,nruns-1 DO BEGIN
			FOR ispec=0,(nspectra)-1 DO BEGIN
				FOR jspec=0,(nspectra-1) DO BEGIN
					maskf(ispec,jspec,irun)=1
					dmaskf(ispec,jspec,irun)=0
				ENDFOR
			ENDFOR
		ENDFOR
	ENDELSE
	w_out=w_in
	e_out=(w_out*0)-1
        i=WHERE(normf NE 0.0, nez)
	w_out(i)=w_in(i)*maskf(i)/normf(i)
	j=WHERE(normf EQ 0.0, nz)
	IF(N_ELEMENTS(j) GT 1) THEN BEGIN
		w_out(j)=0.0
	ENDIF
	i=WHERE(w_out NE 0.0, nez)
	term1=e_in(i)/normf(i)
	term2=(e_in(i)*dnormf(i))/(normf(i)^2)
	e_out(i)=SQRT(term1^2+term2^2)

	w_out=w_out*absnorm
	e_out=e_out*absnorm

	IF (iprint GT 0) THEN PRINT,'End of "Perform Normalisation" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.e=e_out

	IF (absol EQ 1) THEN $
		IF (nruns EQ 1) THEN datp.y_tit='X-section (b/ster/f.u.)' $
				ELSE datp.z_tit='X-section (b/ster/f.u.)'

	Vruns=STRTRIM(STRING(Vrun),2)
	IF (N_ELEMENTS(Mrun) NE 0) THEN BEGIN
		Mruns=STRTRIM(STRING(Mrun),2)
	ENDIF ELSE BEGIN
		Mruns='no mask'
	ENDELSE

	IF (absol EQ 0) THEN datp.other_tit=datp.other_tit+' -nw('+Vruns+','+Mruns+',0)' $
	ELSE BEGIN
		mSs=STRTRIM(STRING(mS),2)
mS0:		n=STRLEN(mSs)	& i=RSTRPOS(mSs,'0')
		IF (i EQ n-1) THEN BEGIN
			mSs=STRMID(mSs,0,n-1)
			GOTO, mS0
		ENDIF
		ASs=STRTRIM(STRING(AS),2)
AS0:		n=STRLEN(ASs)	& i=RSTRPOS(ASs,'0')
		IF (i EQ n-1) THEN BEGIN
			ASs=STRMID(ASs,0,n-1)
			GOTO, AS0
		ENDIF
		mVs=STRTRIM(STRING(NV),2)
mV0:		n=STRLEN(mVs)	& i=RSTRPOS(mVs,'0')
		IF (i EQ n-1) THEN BEGIN
			mVs=STRMID(mVs,0,n-1)
			GOTO, mV0
		ENDIF
		datp.other_tit=datp.other_tit+' -nw('+Vruns+','+Mruns $
			+','+mSs+','+ASs+','+mVs+')'
	ENDELSE

finished:
	IF (iprint GT 0) THEN PRINT,'End norm_water:'

	give_datp, datp

	RETURN, w_out
	END


