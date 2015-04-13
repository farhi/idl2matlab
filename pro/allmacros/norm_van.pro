;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION norm_van, w_in0, filenum=Vrun, S_mass=mS, Fwt=AS, V_mass=mV, $
		 fn, op1, op2, op3

;For D7 data only
;
;Correct for "apparent" detector efficiencies using vanadium data.  If desired,
;normalise intensity to an absolute scale. Must be run after components.pro!!!
;
;ARGUMENTS:
; filenum	:number of vanadium integral file - "vanadium_<filenum>.dat"
;		 created by vanadium.pro.  Is possible also to input multiple
;		 vanadium file numbers for use with theta_scan data -
;		 e.g. filenum=[13487,13491] - use 5 vanadium files numbered
;					      13487,13488,13489,13490,13491
;					      i.e. sequential order
;		filenum=[13487,13490,13499] - use the three specified files
;					      i.e. non-sequential order
;
; S_mass	:sample mass (g) -optional
; V_mass	:V mass (g)	 -optional
; Fwt		:formula weight (amu) per f.u. -optional
;
; (fn, op1, op2 and op3 are obsolete, kept for backwards compatability)
;
;DIMENSIONS:
; w_in=w_out(nspectra,nphases,nruns) unless
;
;COMMAND SYNTAX
;Simple Example:
;	w2=norm_van(w1,filenum=[13478,13499])
;	- correct det efficiencies using 12 vanadium files vanadium_13478.dat
;	  to vanadium_13499.dat taken at different scan positions 
;
;Absolute Normalisation example:
;	w2=norm_van(w1,filenum=13487,S_mass=13.45,V_mass=92.3,Fwt=6.29)
;	- normalise to vanadium_13487.dat, using a sample mass of 13.45g 
;	  and a formula weight of 92.3 amu. V mass is 6.29g. 
;
;							KHA,JRS 14/4/01
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp_access, inst

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start norm_van:'

	take_datp, datp
	
	IF(N_ELEMENTS(fn) GT 0) THEN Vrun=fn
	IF(N_ELEMENTS(op1) GT 0) THEN mS=op1
	IF(N_ELEMENTS(op2) GT 0) THEN mV=op2
	IF(N_ELEMENTS(op3) GT 0) THEN AS=op3

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	IF (inst NE 'D7') THEN BEGIN
		PRINT,'norm_van: Error - instrument must be D7'
		GOTO, finished
	ENDIF

	sw=SIZE(w_in0)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in0)=',sw

	par=datp.p
	nspectra=FIX(par(1))
	ncomps=FIX(par(2))
	nruns=FIX(par(3))
	TOF=FIX(par(8))
	nchannels=FIX(par(6))
	chw=par(7)

	e_in0=datp.e

	se=SIZE(e_in0)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in0)=',sw
		PRINT,'        SIZE(e_in0)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF (TOF EQ 0) THEN BEGIN
		IF (ncomps EQ 1) THEN BEGIN
			w_in=FLTARR(nspectra,ncomps,nruns)	& e_in=w_in
			w_in(*,0,*)=w_in0(*,*)
			e_in(*,0,*)=e_in0(*,*)
		ENDIF ELSE BEGIN
			w_in=w_in0
			e_in=e_in0
		ENDELSE
	ENDIF ELSE BEGIN
		w_in=REFORM(w_in0,nchannels,nspectra,ncomps)
		e_in=REFORM(e_in0,nchannels,nspectra,ncomps)
	ENDELSE

	IF (iprint GT 0) THEN BEGIN
		PRINT,'TOF=',TOF
		PRINT,'nspectra=',nspectra
		PRINT,'ncomps=',ncomps
		PRINT,'nchannels=',nchannels
		PRINT,'nruns=',nruns
	ENDIF

	IF (N_ELEMENTS(Vrun) EQ 1) THEN BEGIN
		nVs=1
		V_file='vanadium_'+STRTRIM(STRING(Vrun),2)+'.dat'
		junk=FINDFILE(V_file,COUNT=co)
		IF(co EQ 0) THEN BEGIN
			V_file='/home/vis/d7/lambda/VANFILES/'+V_file
			junk=FINDFILE(V_file,COUNT=co)
			IF(co EQ 0) THEN PRINT, !ERR_STRING
		ENDIF
		PRINT,'Norm_van: Normalise to vanadium file ',V_file
	ENDIF ELSE BEGIN
		nVs=N_ELEMENTS(Vrun)
		IF(nVs EQ 2) THEN BEGIN		;consecutive sequence
			subv=Vrun(1)-Vrun(0)
			nVs=subv+1
			Vrun=INDGEN(nVs)+Vrun(0)
		ENDIF
		V_file=STRARR(nVs)
		PRINT,'Norm_van: Normalise to vanadium files
		FOR iV=0,nVs-1 DO BEGIN
			V_file(iV)='vanadium_'+STRTRIM(STRING(Vrun(iV)),2)+'.dat'
			junk=FINDFILE(V_file(iV),COUNT=co)
			IF(co EQ 0) THEN BEGIN
				V_file(iV)='/home/vis/d7/lambda/VANFILES/'+V_file(iV)
				junk=FINDFILE(V_file(iV),COUNT=co)
				IF(co EQ 0) THEN PRINT, !ERR_STRING
			ENDIF
			PRINT,'        ',V_file(iV)
		ENDFOR
	ENDELSE

	absol=0
	IF (N_ELEMENTS(mS) NE 0) THEN absol=1

	IF (iprint GT 0) THEN PRINT,'absol=',absol

	absnorm=1.
	IF (absol EQ 1) THEN BEGIN
		amu=1.66E-24	; amu in g
		NS=mS/(AS*amu)	; number of formula units
		AV=50.9414	; atomic mass of V
		NV=mV/(AV*amu)	; number of V atoms
		sigmaV=5.08/(4.*!pi) ; V incoherent cross-section in b/ster
		absnorm=sigmaV*NV/NS
		IF (iprint GT 0) THEN PRINT,'  Sample: m=',mS,'g  A=',AS,'amu  N.f.u.=',nS
		IF (iprint GT 0) THEN PRINT,'Vanadium: m=',mV,'g  A=',AV,'amu  N.f.u.=',nV,'  => absnorm=',absnorm
	ENDIF
		
	IF (iprint GT 0) THEN PRINT,'absnorm=',absnorm

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open and read from input file

	itest=12

	line=''
	Varray=FLTARR(4,nspectra)
	V=FLTARR(nspectra,nruns)	& dV=V
	IF (nVs EQ 1) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'Opening Vanadium file: ',V_file(0)
		OPENR, 1, V_file(0), ERROR=err
		IF (err NE 0) THEN PRINT, !ERR_STRING
		READF, 1, line	& READF, 1, line
		READF, 1, Varray
		CLOSE, 1
		FOR irun=0,nruns-1 DO BEGIN
			V(*,irun)=Varray(2,*)
			dV(*,irun)=Varray(3,*)
		ENDFOR
	ENDIF ELSE BEGIN
		Vin=FLTARR(nspectra,nVs)	& dVin=Vin	& Vx=Vin
		FOR iV=0,nVs-1 DO BEGIN
			IF (iprint GT 0) THEN PRINT,'Opening Vanadium file: ',V_file(iV)
			OPENR, 1, V_file(iV), ERROR=err
			IF (err NE 0) THEN PRINT, !ERR_STRING
			READF, 1, line	& READF, 1, line
			READF, 1, Varray
			CLOSE, 1
			Vin(*,iV)=Varray(2,*)
			dVin(*,iV)=Varray(3,*)
			Vx(*,iV)=Varray(1,*)
			IF (iprint GT 0) THEN PRINT,'Vin(',itest,')=',Vin(itest,iV)
		ENDFOR
	ENDELSE

	IF (nruns GT 1 AND nVs GT 1) THEN BEGIN
		IF (nruns EQ nVs) THEN BEGIN
			IF (iprint GT 0) THEN PRINT,'identical V and sample positions'
			V=Vin & dV=dVin
		ENDIF ELSE BEGIN	; interpolate between V positions
			IF (iprint GT 0) THEN PRINT,'interpolating V positions'
			x_in=datp.z
			FOR ispec=0,nspectra-1 DO BEGIN
				V0=Vin(ispec,*)	   & dV0=dVin(ispec,*)
				xI0=x_in(ispec,*)  & xV0=Vx(ispec,*)
				V(ispec,*)=INTERPOL(V0,xV0,xI0)
				dV(ispec,*)=INTERPOL(dV0,xV0,xI0)
				IF (iprint GT 0 AND ispec EQ itest) THEN BEGIN
					PRINT,'In s',itest,' :'
					PRINT,'V0=',V0
					PRINT,'xV0=',xV0
					PRINT,'xI0=',xI0
					PRINT,'V=',V(ispec,*)
				ENDIF
			ENDFOR
		ENDELSE
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "read from input file" section'

;-------------------------------------------------------------------------------
;Perform Normalisation

	normf=w_in*0.	& dnormf=normf

	IF (TOF EQ 0) THEN BEGIN
		FOR irun=0,nruns-1 DO BEGIN
			FOR ispec=0,nspectra-1 DO BEGIN
				normf(ispec,*,irun)=V(ispec,irun)
				dnormf(ispec,*,irun)=dV(ispec,irun)
			ENDFOR
		ENDFOR
	ENDIF ELSE BEGIN
		FOR ispec=0,nspectra-1 DO BEGIN
			normf(*,ispec,*)=V(ispec)*chw
			dnormf(*,ispec,*)=dV(ispec)*chw
		ENDFOR
	ENDELSE

	w_out=w_in/normf
	e_out=SQRT((e_in/normf)^2+(w_in*dnormf/(normf^2))^2)

	zeroed=WHERE(e_in LT -0.9, nz)

	IF (TOF EQ 1) THEN BEGIN
		w_out=REFORM(w_out,nchannels,nspectra*ncomps)
		e_out=REFORM(e_out,nchannels,nspectra*ncomps)
	ENDIF

	w_out=w_out*absnorm
	e_out=e_out*absnorm

	IF (nz GT 0) THEN BEGIN
		w_out(zeroed)=0.
		e_out(zeroed)=-1.
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "Perform Normalisation" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.e=e_out

	IF (TOF EQ 0 AND absol EQ 1) THEN $
		IF (nruns EQ 1 AND ncomps EQ 1) THEN datp.y_tit='X-section (b/ster/f.u.)' $
				ELSE datp.z_tit='X-section (b/ster/f.u.)'
	IF (TOF EQ 1 AND absol EQ 1) THEN datp.z_tit='X-section (b/ster/f.u./mcs)'

	IF (nVs EQ 1) THEN Vruns=STRTRIM(STRING(Vrun),2) $
	ELSE BEGIN
		Vruns=STRTRIM(STRING(Vrun(0)),2)+':'+STRTRIM(STRING(Vrun(nVs-1)),2)
	ENDELSE

	IF (absol EQ 0) THEN datp.other_tit=datp.other_tit+' -nv('+Vruns+',0)' $
	ELSE BEGIN
		mSs=STRTRIM(STRING(mS),2)
mS0:		n=STRLEN(mSs)	& i=RSTRPOS(mSs,'0')
		IF (i EQ n-1) THEN BEGIN
			mSs=STRMID(mSs,0,n-1)
			GOTO, ms0
		ENDIF
		ASs=STRTRIM(STRING(AS),2)
AS0:		n=STRLEN(ASs)	& i=RSTRPOS(ASs,'0')
		IF (i EQ n-1) THEN BEGIN
			ASs=STRMID(ASs,0,n-1)
			GOTO, AS0
		ENDIF
		mVs=STRTRIM(STRING(mV),2)
mV0:		n=STRLEN(mVs)	& i=RSTRPOS(mVs,'0')
		IF (i EQ n-1) THEN BEGIN
			mVs=STRMID(mVs,0,n-1)
			GOTO, mV0
		ENDIF
		datp.other_tit=datp.other_tit+' -nv('+Vruns $
			+','+mSs+','+ASs+','+mVs+')'
	ENDELSE

finished:
	IF (iprint GT 0) THEN PRINT,'End norm_van:'

	give_datp, datp

	RETURN, w_out
	END
