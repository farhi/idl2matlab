;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION norm_inc, w_in0, inc_xs=sigmaI, debye=DWfac, ini, dwf

;For D7 data only
;
; Normalise intensities to the incoherent scattering of the sample. 
; Must be called after components and theta_scan and phi2q. 
;
;ARGUMENTS:
; inc_xs	:incoherent cross-section of the sample (required)
; debye		:Debye-Waller factor (optional)
;
;DIMENSIONS
;x-axis must be in Q  -> w_in=w_out(nQ) 
;
;COMMAND SYNTAX:
;eq -	w2=norm_inc(w1,inc_xs=4.5[,debye=0.06])
;	- normalise to an incoherent scattering cross-section of 4.5barns
;	- Debye-Waller factor of exp(-0.06*Q^2)
; (optional arguments shown in square brackets)
;
;							KHA,JRS 14/9/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start norm_inc:'

	take_datp, datp
	IF(N_ELEMENTS(ini) GT 0) THEN sigmaI=ini
	IF(N_ELEMENTS(dwf) GT 0) THEN DWfac=dwf

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw=SIZE(w_in0)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in0)=',sw
	npts=sw(1)
	par=datp.p
	nspectra=FIX(par(1))
	ncomps=FIX(par(2))
	nruns=FIX(par(3))
	TOF=FIX(par(8))
	nchannels=FIX(par(6))
	chw=par(7)
	lambda=par(4)
	q=datp.x

	e_in0=datp.e

	se=SIZE(e_in0)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in0)=',sw
		PRINT,'        SIZE(e_in0)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF (ncomps EQ 1) THEN BEGIN
		PRINT,'norm_inc: Error - Workspace contains only one component'
		GOTO, finished
	ENDIF

	IF (TOF EQ 0) THEN BEGIN
		w_in=w_in0
		e_in=e_in0
	ENDIF ELSE BEGIN
		w_in=REFORM(w_in0,nchannels,nspectra,ncomps)
		e_in=REFORM(e_in0,nchannels,nspectra,ncomps)
	ENDELSE

	IF (iprint GT 0) THEN BEGIN
		PRINT,'TOF=',TOF
		PRINT,'nspectra=',nspectra
		PRINT,'nchannels=',nchannels
		PRINT,'nruns=',nruns
		PRINT,'ncomps=',ncomps
	ENDIF

	absnorm=sigmaI/(4.*!pi)

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Perform Normalisation

	normf=w_in*0.	& dnormf=normf

	IF(N_ELEMENTS(DWfac) GT 0) THEN BEGIN
		dw=exp(-DWfac*q^q)
	ENDIF ELSE BEGIN
		dw=0*q+1
	ENDELSE

	IF (TOF EQ 0) THEN BEGIN
		FOR ipt=0,npts-1 DO BEGIN
			FOR icomp=0,ncomps-1 DO BEGIN
				normf(ipt,icomp,*)=w_in(ipt,1,*)/dw(ipt)
				dnormf(ipt,icomp,*)=e_in(ipt,1,*)/dw(ipt)
			ENDFOR
		ENDFOR
		avinc=TOTAL(w_in(*,1,*)/e_in(*,1,*)^2,1)/TOTAL(1./e_in(*,1,*)^2,1)
		davinc=SQRT(1./TOTAL(1./e_in(*,1,*)^2,1))
		FOR irun=0,nruns-1 DO BEGIN
			normf(*,1,irun)=avinc(irun)
			dnormf(*,1,irun)=davinc(irun)
		ENDFOR
	ENDIF ELSE BEGIN
		FOR ispec=0,nspectra-1 DO BEGIN
			sum=TOTAL(w_in(*,ispec,1),1)
			dsum=SQRT(TOTAL(e_in(*,ispec,1)^2,1))
			normf(*,ispec,*)=sum*chw
			dnormf(*,ispec,*)=dsum*chw
		ENDFOR
		avinc=TOTAL(normf(0,*,1)/dnormf(0,*,1)^2)/TOTAL(1./dnormf(0,*,1)^2)
		davinc=SQRT(1./TOTAL(1./dnormf(0,*,1)))
		normf(*,*,1)=avinc	& dnormf(*,*,1)=davinc
	ENDELSE

	zeroed=WHERE(e_in LT -0.9, nz)
	IF (nz GT 0) THEN normf(zeroed)=-1.

	w_out=w_in/normf
	e_out=SQRT((e_in/normf)^2+(w_in*dnormf/normf^2)^2)
	e_out(*,1,*)=0.

	IF (nz GT 0) THEN BEGIN
		w_out(zeroed)=0.
		e_out(zeroed)=-1.
	ENDIF

	IF (TOF EQ 1) THEN BEGIN
		w_out=REFORM(w_out,nchannels,nspectra*ncomps)
		e_out=REFORM(e_out,nchannels,nspectra*ncomps)
	ENDIF

	w_out=w_out*absnorm
	e_out=e_out*absnorm

	IF (iprint GT 0) THEN PRINT,'End of "Perform Normalisation" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.e=e_out

	IF (TOF EQ 0) THEN $
		IF (nruns EQ 1 AND ncomps EQ 1) THEN datp.y_tit='X-section (b/ster/f.u.)' $
				ELSE datp.z_tit='X-section (b/ster/f.u.)'
	IF (TOF EQ 1) THEN datp.z_tit='X-section (b/ster/f.u./mcs)'


	s=STRTRIM(STRING(sigmaI),2)
trys:	n=STRLEN(s)	& i=RSTRPOS(s,'0')
	IF (i EQ n-1) THEN BEGIN
		s=STRMID(s,0,n-1)
		GOTO, trys
	ENDIF
	IF(N_ELEMENTS(DWfac) GT 0) THEN BEGIN
		p=STRTRIM(STRING(DWfac),2)
tryp:		n=STRLEN(p)	& i=RSTRPOS(p,'0')
		IF (i EQ n-1) THEN BEGIN
			p=STRMID(p,0,n-1)
			GOTO, tryp
		ENDIF
	ENDIF ELSE p=''

	PRINT,'Norm_inc: normalise to spin-incoherent X-section of '+s+' b/ster/f.u.'
	datp.other_tit=datp.other_tit+' -ni('+s+','+p+')'

finished:
	IF (iprint GT 0) THEN PRINT,'End norm_inc:'

	give_datp, datp

	RETURN, w_out
	END
