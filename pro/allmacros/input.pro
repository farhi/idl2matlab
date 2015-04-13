;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION input, file=in_file, fin

;Reads a file previously saved by output.pro (with titles), or any XYE 
;formatted file.
;
;ARGUMENTS
; file	:name of file to input
; (the argument fin is obsolete, kept for backwards compatability)
;
;COMMAND SYNTAX:
; w1=input(file='<in_file>')
; NB - don't forget to include single quotes
;
;							KHA,JRS 14/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start input:'

	take_datp, datp

	IF(N_ELEMENTS(fin) GT 0) THEN in_file=fin

;-------------------------------------------------------------------------------
;Open and read from input file

	line=' '
	IF (iprint GT 0) THEN PRINT,'Opening input file'
	OPENR, 1, in_file, ERROR=err	& IF (err NE 0) THEN BEGIN
		PRINT, !ERR_STRING
		GOTO, finished
	ENDIF

	READF, 1, line
	IF (STRPOS(line,'SIZE(workspace)') EQ -1) THEN oned=1 ELSE oned=0

	IF (oned) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'1-D data format'
		POINT_LUN, 1, 0
		ON_IOERROR, nomoredata
		m=10000	& n=0
		x_in=FLTARR(m)	& w_in=x_in	& e_in=x_in
		FOR i=0,m-1 DO BEGIN
			READF, 1, x, w, e
			x_in(i)=x	& w_in(i)=w	& e_in(i)=e
		ENDFOR
nomoredata:
		x_in=x_in(0:i-1)
		w_in=w_in(0:i-1)
		e_in=e_in(0:i-1)
		n=i

		ON_IOERROR, endoffile
		POINT_LUN, 1, 0
		FOR i=0,n-1 DO READF, 1, x
		READF, 1, line	& datp.x_tit=line
		READF, 1, line	& datp.y_tit=line
		READF, 1, line	& datp.w_tit=line
		READF, 1, line	& datp.other_tit=line

	ENDIF ELSE BEGIN
		IF (iprint GT 0) THEN PRINT,'More than 1-D data format'

		i=STRPOS(line,'=') & n=STRLEN(line) & sw=STRMID(line,i+1,n-i-1)
		ndims=FIX(STRMID(sw,0,12))	& dim=INTARR(ndims)
		FOR i=0,ndims-1 DO dim(i)=FIX(STRMID(sw,12*(i+1),12))
		IF (ndims EQ 2) THEN w_in=FLTARR(dim(0),dim(1)) $
			ELSE IF (ndims EQ 3) THEN w_in=FLTARR(dim(0),dim(1),dim(2)) $
			ELSE w_in=FLTARR(dim(0),dim(1),dim(2),dim(3))
		READF, 1, w_in
		IF (iprint GT 0) THEN PRINT,'w_in read OK'
		e_in=w_in*0.
		READF, 1, e_in
		IF (iprint GT 0) THEN PRINT,'e_in read OK'
		READF, 1, line
		i=STRPOS(line,'=') & n=STRLEN(line) & sx=STRMID(line,i+1,n-i-1)
		ndims=FIX(STRMID(sx,0,12))
		IF (ndims GE 1) THEN BEGIN
			dim=INTARR(ndims) & FOR i=0,ndims-1 DO dim(i)=FIX(STRMID(sx,12*(i+1),12))
			IF (ndims EQ 1) THEN x_in=FLTARR(dim(0)) $
				ELSE IF (ndims EQ 2) THEN x_in=FLTARR(dim(0),dim(1)) $
				ELSE x_in=FLTARR(dim(0),dim(1),dim(2))
		ENDIF ELSE x_in=0
		READF, 1, x_in
		IF (iprint GT 0) THEN PRINT,'x_in read OK'
		READF, 1, line
		i=STRPOS(line,'=') & n=STRLEN(line) & sy=STRMID(line,i+1,n-i-1)
		ndims=FIX(STRMID(sy,0,12))
		IF (ndims GE 1) THEN BEGIN
			dim=INTARR(ndims) & FOR i=0,ndims-1 DO dim(i)=FIX(STRMID(sy,12*(i+1),12))
			IF (ndims EQ 1) THEN y_in=FLTARR(dim(0)) $
				ELSE IF (ndims EQ 2) THEN y_in=FLTARR(dim(0),dim(1)) $
				ELSE y_in=FLTARR(dim(0),dim(1),dim(2))
		ENDIF ELSE y_in=0
		READF, 1, y_in
		IF (iprint GT 0) THEN PRINT,'y_in read OK'
		READF, 1, line
		i=STRPOS(line,'=') & n=STRLEN(line) & sz=STRMID(line,i+1,n-i-1)
		ndims=FIX(STRMID(sz,0,12))
		IF (ndims GE 1) THEN BEGIN
			dim=INTARR(ndims) & FOR i=0,ndims-1 DO dim(i)=FIX(STRMID(sz,12*(i+1),12))
			IF (ndims EQ 1) THEN z_in=FLTARR(dim(0)) $
				ELSE IF (ndims EQ 2) THEN z_in=FLTARR(dim(0),dim(1)) $
				ELSE IF (ndims EQ 3) THEN z_in=FLTARR(dim(0),dim(1),dim(2)) $
				ELSE z_in=FLTARR(dim(0),dim(1),dim(2),dim(3))
		ENDIF ELSE z_in=0
		READF, 1, z_in
		IF (iprint GT 0) THEN PRINT,'z_in read OK'
		READF, 1, line	& datp.x_tit=line
		READF, 1, line	& datp.y_tit=line
		READF, 1, line	& datp.z_tit=line
		READF, 1, line	& datp.w_tit=line
		READF, 1, line	& datp.other_tit=line
		IF (iprint GT 0) THEN PRINT,'titles read OK'

	ENDELSE

endoffile:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End of "Read from input file" section'

;-------------------------------------------------------------------------------
;Return and exit

	mod_datp, datp, "x", x_in
	mod_datp, datp, "e", e_in
	mod_datp, datp, "p", par
	IF (oned NE 1) THEN BEGIN
		mod_datp, datp, "y", y_in
		mod_datp, datp, "z", z_in
	ENDIF

	give_datp, datp

finished:
	PRINT,'input: workspace data read from '+in_file

	RETURN, w_in
	END
