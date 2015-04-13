;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO output, w_in, file=out_file, opf

;takes a workspace and writes it to a file. 1-D workspaces are saved 
;in a standard (x,y,e) 3-column format. Larger workspaces saved differently. 
;
;COMMAND SYNTAX
; output, w10, file=<filename> 
;							KHA,JRS 14/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start output:'

	take_datp, datp

	IF(N_ELEMENTS(opf) GT 0) THEN out_file=opf

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	x_in=datp.x
	y_in=datp.y
	z_in=datp.z
	e_in=datp.e

	sw=SIZE(w_in)
	npts=sw(1)
	IF (sw(0) EQ 1) THEN oned=1 ELSE oned=0
		
	IF (N_ELEMENTS(out_file) EQ 0) THEN BEGIN
		PRINT,'output: Error - output file name must be specified'
		GOTO, finished
	ENDIF

	se=SIZE(e_in)
	FOR i=0,se(0) DO IF (se(i) NE sw(i)) THEN e_in=w_in*0.

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open and write to output file

	IF (iprint GT 0) THEN PRINT,'Opening output file'
	ON_IOERROR, close_file

	OPENW, 1, out_file, ERROR=err	& IF (err NE 0) THEN PRINT, !ERR_STRING

	IF (oned EQ 1) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'1-D data format'
		FOR i=0,npts-1 DO IF (w_in(i) NE 0.) OR (e_in(i) GT 0.) THEN $
			PRINTF, 1, FORMAT='(3F14.6)', x_in(i), w_in(i), e_in(i)
		IF (datp.y_tit EQ '') THEN datp.y_tit='Intensity (arb. units)'
		PRINTF, 1, datp.x_tit
		PRINTF, 1, datp.y_tit
		PRINTF, 1, datp.w_tit
		PRINTF, 1, datp.other_tit
	ENDIF ELSE BEGIN
		IF (iprint GT 0) THEN PRINT,'More than 1-D data format'
		PRINTF, 1, 'SIZE(workspace) =',SIZE(w_in)
		PRINTF, 1, w_in
		PRINTF, 1, e_in
		PRINTF, 1, 'SIZE(xarray) =',SIZE(x_in)
		PRINTF, 1, x_in
		PRINTF, 1, 'SIZE(yarray) =',SIZE(y_in)
		PRINTF, 1, y_in
		PRINTF, 1, 'SIZE(zarray) =',SIZE(z_in)
		PRINTF, 1, z_in
		PRINTF, 1, datp.x_tit
		PRINTF, 1, datp.y_tit
		PRINTF, 1, datp.z_tit
		PRINTF, 1, datp.w_tit
		PRINTF, 1, datp.other_tit
	ENDELSE

close_file:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End of "Write to output file" section'

;-------------------------------------------------------------------------------
;Return and exit

	give_datp, datp

finished:
	PRINT,'output: workspace data saved in '+out_file

	RETURN
	END
