;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION vanadium, w_in, nosave=nosave, sk

;For D7 data only
;
;Takes 2-D or 3-D workspace containing one or more normalised non-tof z-PA 
;or xyz-PA background-subtracted vanadium runs.  For quartz and multiple 
;scattering corrections, data should be run through corr_xyz first. Vanadium.pro
;adds the flip and non-flip intensities and puts the sums into w_out and also 
;into the file 'vanadium_<numor>.dat'
;
;KEYWORDS:
; /nosave	:Don't save in file format
; (sk is obsolete and should not be used)
;
;DIMENSIONS:
; input format: w_in(32,nphases,nruns)
; output format: w_out(32,nruns) (saved in <nrun> individual files)
;
;COMMAND SYNTAX:
; w5=vanadium(w4[,/nosave])
; (optional keyword shown in square brackets)
;
;							KHA,JRS 18/4/02
;-------------------------------------------------------------------------------
;*******************************************************************************
	COMMON c_lamp
        
	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start vanadium:'

	ON_IOERROR, finished

	take_datp, datp

	IF(N_ELEMENTS(sk) GT 0) THEN skip=sk
	IF KEYWORD_SET(nosave) THEN skip=1

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	x_in=datp.x
	e_in=datp.e
	par=datp.p

	TOF=FIX(par(8))
	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nruns=FIX(par(3))
	sw=SIZE(w_in)

	IF (TOF EQ 1) AND (sw(1) EQ 128) THEN BEGIN
		PRINT,'Vanadium: Error - TOF data must be integrated before calling vanadium.pro'
		GOTO, finished
	ENDIF

	IF (nspectra NE 32 AND nspectra NE 64) THEN BEGIN
		PRINT,'Vanadium: Error - nspectra=',nspectra,' sw=',sw
		GOTO, finished
	ENDIF

	IF (N_ELEMENTS(skip) EQ 0) THEN skip=0

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Add up vanadium counts

	IF (nruns EQ 1) THEN x_out=x_in ELSE x_out=datp.z

	IF (iprint GT 0) THEN PRINT,'nphases=',nphases

	IF (nspectra EQ 32) THEN ispec=INDGEN(32)*2+2 ELSE ispec=INDGEN(64)+1

	IF (nphases EQ 1) THEN BEGIN
		w_out=w_in
		e_out=e_in
	ENDIF ELSE BEGIN
		rnorm=FLOAT(nphases/2)
		w_out=TOTAL(w_in,2)/rnorm
		e_out=SQRT(TOTAL(e_in^2,2))/rnorm
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'End of "Sum V counts" section'

;-------------------------------------------------------------------------------
;Write vanadium intensities to 'vanadium_file'

	numor=STRTRIM(STRING(LONG(datp.p(0))),2)
	title='Vanadium Intensities from #'+numor

	IF (skip NE 0) THEN GOTO, dontsave

	FOR irun=0,nruns-1 DO BEGIN
		IF (nruns NE 1) THEN BEGIN
			numor=STRTRIM(STRING(LONG(datp.pv(0,irun))),2)
			title='Vanadium Intensities from #'+numor
		ENDIF
		out_file='vanadium_'+numor+'.dat'
		IF (iprint GT 0) THEN PRINT,'out_file=',out_file
		OPENW, 1, out_file
		PRINTF, 1, title
		PRINTF, 1, 'Spectrum     Angle     Vana       dVana'
		FOR i=0,nspectra-1 DO PRINTF, 1, FORMAT='(I7,F11.3,2F11.6)', $
					ispec(i), x_out(i,irun), w_out(i,irun), e_out(i,irun)
		CLOSE, 1
                IF(lamp_host EQ 'd7' OR lamp_host EQ 'd7sgi' OR lamp_host EQ 'd7lin') THEN $
		   spawn, 'cp '+out_file+' /home/vis/d7/lambda/VANFILES/'
	ENDFOR

dontsave:
	IF (iprint GT 0) THEN PRINT,'End of "Write to vanadium_file" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.p(2)=1.

	mod_datp, datp, "e", e_out

	datp.w_tit=title
	datp.other_tit=datp.other_tit+' -va'
	datp.y_tit='Intensity'

finished:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End vanadium:'

	give_datp, datp

	RETURN, w_out
	END
