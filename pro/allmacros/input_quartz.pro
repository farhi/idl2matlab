;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION input_quartz, numor=numor, temp=temp, nm, tm

;Read a quartz flipping ratio file 'quartz_<numor>_<temp>.dat' and loads it
;into desired workspace
;
;ARGUMENTS:
; numor	: numor corresponding to quatzfile of interest
; temp	: temperature parameter of corrected quartzfile (see quartz.pro)
; (nm and tm are obsolete, kept for backwards compatability)
;
;COMMAND SYNTAX:
; w1=input_quartz(numor=<numor>[,temp=<temp>])
; (optional arguments shown in square brackets)
;
;							KHA,JRS 14/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start input_quartz:'

	take_datp, datp

	IF(N_ELEMENTS(nm) GT 0) THEN numor=nm
	IF(N_ELEMENTS(tm) GT 0) THEN temp=tm
;-------------------------------------------------------------------------------
;Open and read from input file

	IF (N_ELEMENTS(TEMP) GT 0) THEN BEGIN
		in_file='quartz_'+STRTRIM(numor,2)+'_'+STRTRIM(temp,2)+'.dat'
	ENDIF ELSE BEGIN
		in_file='quartz_'+STRTRIM(numor,2)+'.dat'
	ENDELSE	
	line=' '
	ispec=0	& iangle=0
	flip=0.	& dflip=0.	& test=0.
	buf=FLTARR(4,32)
	IF (iprint GT 0) THEN PRINT,'Opening input file'
	OPENR, 1, in_file, ERROR=err	& IF (err NE 0) THEN BEGIN
		PRINT, !ERR_STRING
		GOTO, finished
	ENDIF
	READF, 1, line	& READF, 1, line	& READF, 1, line
	line=line+' -1'
	READS, line, ispec, iangle, flip, dflip, test
	IF (FIX(test) EQ -1) THEN xyz=0 ELSE xyz=1

	datp.x_tit='Spectrum Number'
	IF (xyz EQ 0) THEN BEGIN
		buf=FLTARR(4,32)
		w_in=FLTARR(32)
		datp.y_tit='Flipping Ratio'
	ENDIF ELSE BEGIN
		buf=FLTARR(8,32)
		w_in=FLTARR(32,3)
		y_in=INDGEN(3)
		datp.y_tit='Phase'
		datp.z_tit='Flipping Ratio'
	ENDELSE
	x_in=FLTARR(32)
	e_in=w_in

	POINT_LUN, 1, 0
	READF, 1, line	& datp.w_tit=line	& READF, 1, line
	READF, 1, buf
	CLOSE, 1

	x_in(*)=buf(0,*)
	w_in(*,0)=buf(2,*)	& e_in(*,0)=buf(3,*)
	IF (xyz EQ 1) THEN BEGIN
		w_in(*,1)=buf(4,*)	& e_in(*,1)=buf(5,*)
		w_in(*,2)=buf(6,*)	& e_in(*,2)=buf(7,*)
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "Read from input file" section'

;-------------------------------------------------------------------------------
;Return and exit

	mod_datp, datp, "x", x_in
	IF (xyz EQ 1) THEN mod_datp, datp, "y", y_in
	mod_datp, datp, "e", e_in

	give_datp, datp

finished:
	IF (iprint GT 0) THEN PRINT,'End input_quartz:'

	RETURN, w_in
	END
