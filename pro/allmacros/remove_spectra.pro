;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION remove_spectra, w_in, badspecs, nolog=nolog
;
; For IN4, IN5, IN6 and HET
; Removes spectra with numbers given in array badspecs. Spectrum numbers
; run from 1 to nspectra.
;
;KEYWORDS
; /nolog	: if set, supresses output of badspecs in other_tit
;		  This is necessary in cases where other_tit exceeds 160 characters
;
;ARGUMENTS
; badspecs	: array of spectrum numbers to be deleted
;
;DIMENSIONS
; w_in(nchannels,nspectra) -> w_out(nchannels,nspectra-n) - n=number of bad specs
;
;COMMAND SYNTAX
;  e.g. w2=remove_spectra(w1,[3,90,202])	-remember square brackets
;
;							KHA,JRS 29/6/02
;-------------------------------------------------------------------------------
;*******************************************************************************
;
	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start remove_specs:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace

	par=datp.p

	x_in=datp.x
	y_in=datp.y
	e_in=datp.e
	z_in=datp.z

	sw=size(w_in)
	nchannels=sw(1)
	IF (sw(0) EQ 1) THEN nspectra=1     ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,w_in
	npts=nchannels

	se=size(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1) OR se(2) NE sw(2)) THEN e_in=SQRT(w_in)

	nbad=N_ELEMENTS(badspecs)
	IF (nbad EQ 0) THEN badspecs=1
	bad=badspecs-1

	IF (iprint GT 0) THEN PRINT,'npts=',npts,' nspectra=',nspectra,' nbad=',nbad
	IF (iprint GT 0) THEN PRINT,'End of input setup section'

;-------------------------------------------------------------------------------
;Remove bad spectra
;
	ispec=INDGEN(nspectra)
	FOR ibad=0,nbad-1 DO BEGIN
		i=WHERE(ispec NE bad(ibad), n)
		IF (n EQ nspectra) THEN $
			PRINT,'remove_specs: Warning - spectrum',bad(ibad)+1,' does not exist'
		ispec=ispec(i)
	ENDFOR

	w_out=w_in(*,ispec)
	e_out=e_in(*,ispec)
	y_out=y_in(ispec)
	z_out=z_in(ispec)
	IF (iprint GT 0) THEN PRINT,'End of "remove bad spectra" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out
	mod_datp, datp, "z", z_out
	IF (nbad EQ 0) THEN badstring='(0)' ELSE BEGIN $
		bad=STRTRIM(badspecs+1,2)	& badstring=bad(0)
		FOR i=1,nbad-1 DO badstring=badstring+','+bad(i)
		badstring='('+badstring+')'
	ENDELSE

	PRINT,'remove_specs: Removed spectra '+badstring
	IF NOT KEYWORD_SET(nolog) THEN $
		datp.other_tit=datp.other_tit+' -rs'+badstring

	give_datp, datp

finished:
	RETURN, w_out

	END
