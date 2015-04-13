	FUNCTION water, w_in0, skip

;
;takes 2-D workspace containing normalised background-subtracted water run.
;Save water integrals in file 
;
; if skip NE 0, Don't save in file
;
; input format: w_in(ndet,ndet)
; output format: w_out(ndet,ndet)
;							JRS 10/9/99




	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start water:'

	ON_IOERROR, finished

	take_datp, datp

;-------------------------------------------------------------------------------
;Check dimensions of input workspace

	x_in0=datp.x
	y_in0=datp.y
	z_in0=datp.z
	e_in0=datp.e
	par=datp.p
	parv=datp.pv
	se=SIZE(e_in0)
	sw=SIZE(w_in0)

	IF (sw(0) EQ 3) THEN nruns=sw(3) ELSE nruns=1
	IF (nruns EQ 1) THEN parv=par
	nspectra=sw(1)

	IF (N_ELEMENTS(skip) EQ 0) THEN skip=0

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'


;-------------------------------------------------------------------------------
;Write vanadium intensities to 'vanadium_file'

	w_out=w_in0
	x_out=x_in0
	y_out=y_in0
	e_out=e_in0

	ispec=INDGEN(nspectra^2)+1

	numor=STRTRIM(STRING(LONG(datp.p(26))),2)
	title='Water Intensities from #'+numor

	IF (skip NE 0) THEN GOTO, dontsave

	out_file='water_'+numor+'.dat'
	PRINT,'Writing water file: ',out_file
	OPENW, 1, out_file
	PRINTF, 1, title
	PRINTF, 1, 'Spectrum     X	Y     Vana       dVana'
	FOR i=0,nspectra-1 DO BEGIN
		FOR j=0,nspectra-1 DO BEGIN
			PRINTF, 1, FORMAT='(I7,1X,F5.1,1X,F5.1,2F12.6)', $
				ispec((j+1)+(i*nspectra)-1), x_out(i), y_out(j), w_out(i,j), e_out(i,j)
		ENDFOR
	ENDFOR
	CLOSE, 1


dontsave:
	IF (iprint GT 0) THEN PRINT,'End of "Write to water_file" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "e", e_out

	datp.w_tit=title
	datp.other_tit=datp.other_tit+' -wa'

finished:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End water:'

	give_datp, datp

	RETURN, w_out
	END
