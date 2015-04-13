;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION theta_scan, w_in, all_angles=all_angles, $
		 pos_angles=pos_angles, neg_angles=neg_angles, coder=coder, $
		 detector=detector,ib,ic

;For D7 data only
;
;Puts all detector angles onto a common x-axis. 
;Removes zeroed data points. 
;
;ARGUMENTS:
; detector	:optional argument specifying an individual detector to use
;
;KEYWORDS:
; /neg_angles	:only use data with negative angles
; /pos_angles	:only use data with positive angles
; /all_angles	:use all detector banks (default)
; /coder	:only used if an individual detector is specified
;		 If set:
;		 	use coder angle as x-axis, 
;		 otherwise:
;			use calculated detector angle as x-axis
; 
;
;DIMENSIONS:
;input format: w(nspectra,nphases,nruns)	- unless nphases or nruns is 1
;output:       w(nruns*nspectra,nphases)	- unless nphases is 1
;
;COMMAND SYNTAX:
; w10=theta_scan(w9[,/all_angles][,/pos_angles][,/neg_angles][detector=<det>][,/coder])
;
;							KHA,JRS 21/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start theta_scan:'

	w_in0=w_in

	take_datp, datp

	ibank=2
	icoder=0

	IF(N_ELEMENTS(ib) GT 0) THEN ibank=ib
	IF(N_ELEMENTS(ic) GT 0) THEN icoder=ic

	IF KEYWORD_SET(all_angles) THEN ibank=2
	IF KEYWORD_SET(neg_angles) THEN ibank=0
	IF KEYWORD_SET(pos_angles) THEN ibank=1
	IF KEYWORD_SET(coder) THEN icoder=1

	IF(N_ELEMENTS(detector) GT 0) THEN ibank=-detector

;-------------------------------------------------------------------------------
;Check arguments and dimensions of w_in
	
	par=datp.p
	e_in0=datp.e
	
	IF (iprint GT 0) THEN BEGIN
		PRINT,'w_in0: '
		HELP, w_in0
		PRINT,'e_in0: '
		HELP, e_in0
		PRINT,'par: '
		HELP, par
	ENDIF

	sw=SIZE(w_in0)
	se=SIZE(e_in0)
	IF(sw(1) EQ 32) THEN ana=1 ELSE ana=0	;define whether analysers are in
	IF (se(0) EQ 0) THEN BEGIN
		PRINT,'theta_scan: Error - no error bars; normalise must be called first'
		GOTO, finished
	ENDIF

	nchannels=FIX(par(6))
	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nruns=FIX(par(3))
	tof=FIX(par(8))
	
	IF (tof EQ 1) THEN BEGIN
		nchannels=sw(1)
		w_in0=REFORM(w_in0,nchannels,nspectra,nphases,nruns)
		e_in0=REFORM(e_in0,nchannels,nspectra,nphases,nruns)
	ENDIF
	
	IF (nruns EQ 1 AND icoder EQ 1) THEN BEGIN
		PRINT,'theta_scan: Error - can`t use coder angles for only 1 run'
		GOTO, finished
	ENDIF
	IF (nphases EQ 1 AND tof EQ 0) THEN BEGIN
		w_in1=FLTARR(nspectra,1,nruns)	& e_in1=w_in1
		w_in1(*,0,*)=w_in0(*,*)
		e_in1(*,0,*)=e_in0(*,*)
	ENDIF ELSE BEGIN
		w_in1=w_in0	& e_in1=e_in0
	ENDELSE

	IF (nruns EQ 1 AND tof EQ 0) THEN x_in1=datp.x 
        IF (nruns EQ 1 AND tof EQ 1) THEN x_in1=datp.y
	IF (nruns GT 1) THEN x_in1=datp.z

	IF (iprint GT 0) THEN PRINT,' nphases=',nphases,'nruns=',nruns,' size(w_in)=',sw

	IF (iprint GT 0) THEN PRINT,'End of "check arguments and dimensions" section'

;-------------------------------------------------------------------------------
;Set up parameters relating to ibank and output arrays

	IF (nruns EQ 1) THEN BEGIN
		i=WHERE(x_in1 LT 0.,count)

		IF (ibank EQ 0) THEN BEGIN
			istart=0
			nspectra=count
		ENDIF ELSE IF (ibank EQ 1) THEN BEGIN
			istart=count
			nspectra=nspectra-count
		ENDIF ELSE IF (ibank EQ 2) THEN BEGIN
			istart=0
		ENDIF ELSE IF (ibank GE -64 AND ibank LE -1) THEN BEGIN
			nspectra=1
			IF(ana EQ 0) THEN istart=-ibank-1 ELSE istart=-(ibank/2)-1
		ENDIF ELSE BEGIN
			PRINT,'Error - ibank=',ibank
			GOTO, finished
		ENDELSE
	
		IF (tof EQ 0) THEN BEGIN
			x_buf=FLTARR(nspectra)	& x_out=x_buf	& y_out=INDGEN(nphases)
			w_buf=FLTARR(nspectra,nphases)	& w_out=w_buf
			e_buf=FLTARR(nspectra,nphases)	& e_out=e_buf
		ENDIF ELSE BEGIN
			y_buf=FLTARR(nspectra)	& y_out=y_buf	& x_out=INDGEN(nchannels)
			w_buf=FLTARR(nchannels,nspectra,nphases)	& w_out=w_buf
			e_buf=FLTARR(nchannels,nspectra,nphases)	& e_out=e_buf
		ENDELSE

		nspect=INTARR(1)	& istar=nspect
		nspect(0)=nspectra
		istar(0)=istart

	ENDIF ELSE BEGIN
		nspect=INTARR(nruns)	& istar=nspect
		FOR irun=0,nruns-1 DO BEGIN	
			i=WHERE(x_in1(*,irun) LT 0.,count)

			IF (ibank EQ 0) THEN BEGIN
				istar(irun)=0
				nspect(irun)=count
			ENDIF ELSE IF (ibank EQ 1) THEN BEGIN
				istar(irun)=count
				nspect(irun)=nspectra-count
			ENDIF ELSE IF (ibank EQ 2) THEN BEGIN
				istar(irun)=0
				nspect(irun)=nspectra
			ENDIF ELSE IF (ibank GE -64 AND ibank LE -1) THEN BEGIN
				nspect(irun)=1
				IF (ana EQ 0) THEN istar(irun)=-ibank-1 ELSE  istar(irun)=-(ibank/2)-1
			ENDIF ELSE BEGIN
				PRINT,'Error - ibank=',ibank
				GOTO, finished
			ENDELSE
		ENDFOR

		IF (tof EQ 0) THEN BEGIN
			x_buf=FLTARR(TOTAL(nspect))	& x_out=x_buf	& y_out=INDGEN(nphases)
			w_buf=FLTARR(TOTAL(nspect),nphases)	& w_out=w_buf
			e_buf=FLTARR(TOTAL(nspect),nphases)	& e_out=e_buf
		ENDIF ELSE BEGIN
			y_buf=FLTARR(TOTAL(nspect))	& y_out=y_buf	& x_out=INDGEN(nchannels)
			w_buf=FLTARR(nchannels,TOTAL(nspect),nphases)	& w_out=w_buf
			e_buf=FLTARR(nchannels,TOTAL(nspect),nphases)	& e_out=e_buf
		ENDELSE
	ENDELSE

	coder=0

	IF (N_ELEMENTS(icoder) NE 0) THEN coder=icoder
	IF (coder EQ 1) THEN BEGIN
		IF (istar(0) LT 16/(ana+1)) THEN x_buf=datp.pv(16,*) $
		ELSE IF (istar(0) LT 32/(ana+1)) THEN x_buf=datp.pv(17,*) $
		ELSE IF (istar(0) LT 48/(ana+1)) THEN x_buf=datp.pv(18,*) $
					 ELSE x_buf=datp.pv(19,*)
	ENDIF
		
	IF (iprint GT 0) THEN PRINT,'End of "ibank and iPA setup" section'

;-------------------------------------------------------------------------------
;Rearrange w_in into w_buf

	FOR irun=0,nruns-1 DO BEGIN
		IF (irun EQ 0) THEN BEGIN
			is1=0
			is2=nspect(irun)-1
		ENDIF ELSE BEGIN
			is1=is2+1
			is2=is1+nspect(irun)-1
		ENDELSE
		FOR iphase=0,nphases-1 DO BEGIN
			IF (iprint GT 0) THEN BEGIN 
				PRINT,'irun=',irun,': Putting w_in(',istar(irun),':',istar(irun)+nspect(irun)-1,',',iphase,',',irun,')
				PRINT,'                 into w_buf(',is1,':',is2,',',iphase,')'
			ENDIF
			IF (tof EQ 0) THEN BEGIN
				w_buf(is1:is2,iphase)=w_in1(istar(irun):istar(irun)+nspect(irun)-1,iphase,irun)
				e_buf(is1:is2,iphase)=e_in1(istar(irun):istar(irun)+nspect(irun)-1,iphase,irun)
			ENDIF ELSE BEGIN
				w_buf(*,is1:is2,iphase)=w_in1(*,istar(irun):istar(irun)+nspect(irun)-1,iphase,irun)
				e_buf(*,is1:is2,iphase)=e_in1(*,istar(irun):istar(irun)+nspect(irun)-1,iphase,irun)
			ENDELSE
		ENDFOR
		IF (coder EQ 0) THEN BEGIN
			IF (tof EQ 0) THEN BEGIN
				x_buf(is1:is2)=ABS(x_in1(istar(irun):istar(irun)+nspect(irun)-1,irun))
			ENDIF ELSE BEGIN
				y_buf(is1:is2)=ABS(x_in1(istar(irun):istar(irun)+nspect(irun)-1,irun))
			ENDELSE
		ENDIF

	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of "rearrange w_in into w_buf" section'

;-------------------------------------------------------------------------------
;Rearrange w_buf into w_out

	IF (tof EQ 0) THEN BEGIN
		i=SORT(x_buf)
		x_out=x_buf(i)
        	xlim=SIZE(x_buf)
	ENDIF ELSE BEGIN
		i=SORT(y_buf)
		y_out=y_buf(i)
        	xlim=SIZE(y_buf)
	ENDELSE

	w_out=w_buf
	e_out=e_buf

	FOR inc=0,xlim(1)-1 DO BEGIN
	   	FOR iphase=0,nphases-1 DO BEGIN
			IF (tof EQ 0) THEN BEGIN
				w_out(inc,iphase)=w_buf(i(inc),iphase)
				e_out(inc,iphase)=e_buf(i(inc),iphase)
			ENDIF ELSE BEGIN
				w_out(*,inc,iphase)=w_buf(*,i(inc),iphase)
				e_out(*,inc,iphase)=e_buf(*,i(inc),iphase)
			ENDELSE
		ENDFOR
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of "rearrange w_buf into w_out" section'

;-------------------------------------------------------------------------------
;Remove zeroed spectra

	IF (tof EQ 0) THEN x_buf=x_out ELSE y_buf=y_out
	w_buf=w_out
	e_buf=e_out

	IF (tof EQ 0) THEN BEGIN
		i=WHERE(w_out(*,0) NE 0. OR e_out GE 0.,count)
	ENDIF ELSE BEGIN
		i=WHERE(w_out(0,*,0) NE 0. OR e_out GE 0.,count)
	ENDELSE
	
	nspectra=count
	
	IF (tof EQ 0) THEN BEGIN
		x_out=FLTARR(nspectra)		& x_out=x_buf(i)
		w_out=FLTARR(nspectra,nphases)	& w_out=w_buf(i,*)
		e_out=FLTARR(nspectra,nphases)	& e_out=e_buf(i,*)
	ENDIF ELSE BEGIN
		y_out=FLTARR(nspectra*nphases)		& y_out=y_buf(i)
		w_out=FLTARR(nchannels,nspectra,nphases)	& w_out=w_buf(*,i,*)
		e_out=FLTARR(nchannels,nspectra,nphases)	& e_out=e_buf(*,i,*)
		w_out=REFORM(w_out,nchannels,nspectra*nphases)
		e_out=REFORM(e_out,nchannels,nspectra*nphases)
		IF (nphases EQ 2) THEN y_out=[y_out,y_out]
		IF (nphases EQ 3) THEN y_out=[y_out,y_out,y_out]
		IF (nphases EQ 6) THEN y_out=[y_out,y_out,y_out,y_out,y_out,y_out]		
	ENDELSE
	
	IF (iprint GT 0) THEN PRINT,'End of "remove zeroed spectra" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "x", x_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	datp.p(1)=nspectra
	datp.p(3)=1

	datp.x_tit='Scattering Angle'
	IF (coder EQ 1) THEN datp.x_tit='Bank Angle'
	IF (tof EQ 1) THEN datp.x_tit='Time channel'
	datp.y_tit=datp.z_tit
	IF (tof EQ 1) THEN datp.y_tit='Scattering Angle'

	IF (ibank EQ 0) THEN s='negative angle detectors' $
	ELSE IF (ibank EQ 1) THEN s='positive angle detectors' $
	ELSE IF (ibank EQ 2) THEN s='all detectors' $
	ELSE BEGIN
		s='detector number '+STRTRIM(STRING(-ibank))
		IF (coder EQ 0) THEN s=s+' with detector angle as x-axis' $
				 ELSE s=s+' with coder angle as x-axis'
	ENDELSE
	PRINT,'Theta_scan: keep '+s

	datp.other_tit=datp.other_tit+' -ts('+STRTRIM(STRING(ibank),2)+')'
finished:

	IF (iprint GT 0) THEN PRINT,'End theta_scan:'

	give_datp, datp

	RETURN, w_out
	END
