;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION input_vanadium, numor=numor, number=number, angle=angle,$
		 nspectra=nsp, nm, nix, np

;Reads the vanadium file 'vanadium_<numor>.dat', and puts it into a 
;1-D workspace.  Also reads in multiple numors (e.g. numor=[47324,47336]) and 
;puts them into a 2-d workspace.
; 
;ARGUMENTS:
; numor		:numor(s) corresponding to vanadium file(s) to be read in
; nspectra	:number of spectra to read in from file
; (nix and np are obsolete, kept for backwards compatability)
;
;KEYWORDS:
; /number	:x-axis is spectrum number (default)
; /angle	:x-axis is scattering angle
;
;COMMAND SYNTAX:
; w1=input_vanadium(<numor>[,nspectra=<nspectra>][,/number][,/angle])
; (optional keywords/arguments are shown in square brackets)
;
;							KHA,JRS  30/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start input_vanadium:'

	take_datp, datp
	
	ix=0
	IF(N_ELEMENTS(nm) GT 0) THEN numor=nm
	IF(N_ELEMENTS(nix) GT 0) THEN ix=nix
	IF(N_ELEMENTS(np) GT 0) THEN nsp=np
	IF KEYWORD_SET(number) THEN ix=0
	IF KEYWORD_SET(angle) THEN ix=1

;-------------------------------------------------------------------------------
;Open and read from input file

	IF (N_ELEMENTS(ix) EQ 0) THEN ix=0
	IF (N_ELEMENTS(nsp) EQ 0) THEN nsp=32

	nruns=1
	IF(N_ELEMENTS(numor) GT 0) THEN BEGIN
		IF(N_ELEMENTS(numor) EQ 2) THEN BEGIN
			nruns=numor(1)-numor(0)+1
			nm=LONARR(nruns)
			FOR i=0,nruns-1 DO nm(i)=numor(0)+i
			numor=nm
		ENDIF ELSE BEGIN
			nruns=N_ELEMENTS(numor)
		ENDELSE
	ENDIF

	x_out=FLTARR(nsp)
	w_out=FLTARR(nsp,nruns)	& e_out=w_out & y_out=w_out
	
	FOR irun=0,nruns-1 DO BEGIN
		in_file='vanadium_'+STRTRIM(numor(irun),2)+'.dat'
		line=' '
		buf=FLTARR(4,nsp)
		IF (iprint GT 0) THEN PRINT,'Opening input file ',in_file
		OPENR, 1, in_file, ERROR=err	& IF (err NE 0) THEN BEGIN
			PRINT, !ERR_STRING
			GOTO, finished
		ENDIF
		READF, 1, line	& READF, 1, line
		READF, 1, buf
		CLOSE, 1

		IF(irun EQ 0) THEN x_out(*)=buf(ix,*)
		y_out(*,irun)=buf(ix,*)	& w_out(*,irun)=buf(2,*) & e_out(*,irun)=buf(3,*)
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of "Read from input file" section'

;-------------------------------------------------------------------------------
;Return and exit

	datp.x_tit='Spectrum Number'
	IF(ix EQ 1) THEN datp.x_tit='2-theta (degrees)'
	
	IF(nruns EQ 1) THEN datp.y_tit='Vanadium Integral' ELSE $
		datp.z_tit='Vanadium Integral'

	IF(nruns EQ 1) THEN nums='#'+STRTRIM(STRING(numor(0)),2) ELSE $
		nums='#'+STRTRIM(STRING(numor(0)),2)+':'+STRTRIM(STRING(numor(nruns-1)),2)

	datp.w_tit='Vanadium Intensities from '+nums	

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out
	mod_datp, datp, "y", y_out

	give_datp, datp

finished:
	IF (iprint GT 0) THEN PRINT,'End input_vanadium:'

	RETURN, w_out
	END
