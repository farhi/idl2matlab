	FUNCTION man_mask, w_in0, box, xlines, ylines, add 

; creates a mask with manual input of beamstop limits and bad lines.

;						JRS 13/6/00

	iprint=1	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start man_mask:'

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
;       Make mask

	IF(N_ELEMENTS(box) NE 4) THEN BEGIN
		PRINT, 'man_mask: Central beamstop position needs to be specified'
		GOTO, finished
	ENDIF

        boxxmin=box(0)
	boxxmax=box(1)
	boxymin=box(2)
	boxymax=box(3)

	IF (N_ELEMENTS(add) GT 0 AND add EQ 1) THEN BEGIN
		m_out=w_in0
		me_out=e_in0
	ENDIF ELSE BEGIN
		m_out=FLTARR(nspectra,nspectra)+1
		me_out=FLTARR(nspectra,nspectra)
	ENDELSE

	m_out(boxxmin:boxxmax,boxymin:boxymax)=0	& me_out(boxxmin:boxxmax,boxymin:boxymax)=-1
	IF(N_ELEMENTS(xlines) NE 0) THEN BEGIN
		m_out(xlines,*)=0	& me_out(xlines,*)=-1
	ENDIF
	IF(N_ELEMENTS(ylines) NE 0) THEN BEGIN
		m_out(*,ylines)=0	& me_out(*,ylines)=-1	
	ENDIF
	mx_out=x_in0
	my_out=y_in0

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
				ispec((j+1)+(i*nspectra)-1), mx_out(i), my_out(j), m_out(i,j), me_out(i,j)
		ENDFOR
	ENDFOR
	CLOSE, 1


dontsave:
	IF (iprint GT 0) THEN PRINT,'End of "Write to mask file" section'

;-------------------------------------------------------------------------------
;mask input data
	w_out=w_in0*m_out
	e_out=e_in0
	i=WHERE(w_out EQ 0, nz)
	e_out(i)=-1
	x_out=x_in0
	y_out=y_in0

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "e", e_out

	datp.w_tit=title
	datp.other_tit=datp.other_tit+' -manma'
	datp.y_tit='Intensity'

finished:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End man_mask:'

	give_datp, datp

	RETURN, w_out
	END

