;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION remove_zeros, w_in

;removes points with y=0 and e=-1 from 1-D workspace
;
;DIMENSIONS:
; 1d workspace only
;
;COMMAND SYNTAX
; w2=remove_zeroes(w1)
;
;							KHA, 8/12/96
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start remove_zeros:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	x_in=datp.x
	e_in=datp.e

	writedata=1
	sw=SIZE(w_in)
	IF (sw(0) NE 1) THEN BEGIN
		PRINT,'Remove_zeros: Error - w_in must be 1-D'
		GOTO, finished
	ENDIF

	se=SIZE(e_in)
	FOR i=0,se(0) DO IF (se(i) NE sw(i)) THEN GOTO, noerrors
	GOTO, errorsOK
noerrors:
	PRINT,'remove_zeros: Error - no error bars set'
	GOTO, finished
errorsOK:
	PRINT,'SIZE(w_in)=',sw
	npts=sw(1)

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Remove zeroed points

	x_out=x_in	& w_out=w_in	& e_out=e_in

	PRINT,'OK1'
	nout=0-1
	PRINT,'OK2'
	FOR i=0,npts-1 DO BEGIN
		PRINT,'OK3'
		IF (w_in(i) NE 0.) OR (e_in(i) GE 0.) THEN BEGIN
			nout=nout+1
			x_out(nout)=x_in(i)
			w_out(nout)=w_in(i)
			e_out(nout)=e_in(i)
		ENDIF
		PRINT,'OK4'
	ENDFOR
	nout=nout-1

	PRINT,'OK5'
	x_out=x_out(0:nout-1)
	w_out=w_out(0:nout-1)
	e_out=e_out(0:nout-1)
	PRINT,'OK6'

	IF (iprint GT 0) THEN PRINT,'End of "Remove zeroed points" section'

;-------------------------------------------------------------------------------
;Return and exit

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

	give_datp, datp

finished:
	IF (iprint GT 0) THEN PRINT,'End remove_zeros:'

	RETURN, w_out
	END
