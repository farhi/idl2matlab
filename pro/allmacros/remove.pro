;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION remove, w_in, min=xmin, max=xmax, xin, xax

; Removes points from 1-d data within x-region specified by xmin, xmax. 
;
;ARGUMENTS:
; min	: min of x-region
; max	: max of x-region
;
;DIMENSIONS
; 1-d data only
;
;COMMAND SYNTAX:
; w2=remove(w1,min=<min>,max=<max>)
;
;						KHA, 28/11/97
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, datp

	IF(N_ELEMENTS(xin) GT 0) THEN xmin=xin
	IF(N_ELEMENTS(xax) GT 0) THEN xmax=xax

	x_in=datp.x
	e_in=datp.e

	s=SIZE(w_in)
	IF (s(0) NE 1) THEN BEGIN
		PRINT,'remove: Error - workspace must be 1-dimensional'
		GOTO, finished
	ENDIF

	iOK=WHERE(x_in LE xmin OR x_in GE xmax, nOK)
	IF (nOK LE 0) THEN BEGIN
		PRINT,'remove: Error - no data points will be kept'
		GOTO, finished
	ENDIF

	x_out=x_in(iOK)
	w_out=w_in(iOK)
	e_out=e_in(iOK)

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

finished:
	give_datp, datp

	RETURN, w_out
	END
