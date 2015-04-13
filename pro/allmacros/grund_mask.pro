	FUNCTION grund_mask, w_in0, skip

; takes a 2-d SANS workspace and creates a mask, removing points 
; greater than 2 standard devs from mean.  Should be run on back-
; ground substracted water runs.
; Skip stops from saving to file


;						JRS 13/6/00

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start mask:'

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
	IF(iprint GT 0) THEN PRINT, 'nspectra = ',nspectra

	IF (N_ELEMENTS(skip) EQ 0) THEN skip=0

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'


;-------------------------------------------------------------------------------
;       Find low/high points and make mask

	av=MEAN(w_in0)
	sd3=3*MEANABSDEV(w_in0)
	sd2=2*MEANABSDEV(w_in0)
	sd1=MEANABSDEV(w_in0)
	min=av-sd2
	max=av+sd2

	i=WHERE(w_in0 LT min, nmin)
	j=WHERE(w_in0 GT max, nmax)

	w_out=FLTARR(nspectra,nspectra)+1
	e_out=FLTARR(nspectra,nspectra)
	IF(N_ELEMENTS(i) GT 1) THEN BEGIN
		w_out(i)=0	& e_out(i)=-1
	ENDIF
	IF(N_ELEMENTS(j) GT 1) THEN BEGIN	
		w_out(j)=0	& e_out(j)=-1
	ENDIF
	x_out=x_in0
	y_out=y_in0

	IF (iprint GT 0) THEN PRINT,'End of "Create Mask" section'

;-------------------------------------------------------------------------------
;Write to mask file

	ispec=INDGEN(nspectra^2)+1

	numor=STRTRIM(STRING(LONG(datp.p(26))),2)
	title='Mask created from #'+numor

	IF (skip NE 0) THEN GOTO, dontsave

	out_file='mask_'+numor+'.dat'
	PRINT,'Writing mask file: ',out_file
	OPENW, 1, out_file
	PRINTF, 1, title
	PRINTF, 1, 'Spectrum     X	Y     1/0       0/-1'
	FOR i=0,nspectra-1 DO BEGIN
		FOR j=0,nspectra-1 DO BEGIN
			PRINTF, 1, FORMAT='(I7,1X,F5.1,1X,F5.1,2I4)', $
				ispec((j+1)+(i*nspectra)-1), x_out(i), y_out(j), w_out(i,j), e_out(i,j)
		ENDFOR
	ENDFOR
	CLOSE, 1


dontsave:
	IF (iprint GT 0) THEN PRINT,'End of "Write to mask file" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "e", e_out

	datp.w_tit=title
	datp.other_tit=datp.other_tit+' -ma'
	datp.y_tit='Intensity'

finished:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End mask:'

	give_datp, datp

	RETURN, w_out
	END

