;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION bin, w_in, ibin

; Bins points in the x-axis of a 1-d data set by an integer multiple, ibin. 
;
;						KHA, 1/12/97
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, datp

	x_in=datp.x
	e_in=datp.e

	s=SIZE(w_in)
	IF (s(0) NE 1) THEN BEGIN
		PRINT,'remove: Error - workspace must be 1-dimensional'
		return,w_in
	ENDIF

	nin=s(1)
	nout=nin/ibin
	rbin=FLOAT(ibin)

	w_out=FLTARR(nout)
	x_out=FLTARR(nout)
	e_out=FLTARR(nout)
	iout=0
	FOR i=0,nin-ibin,ibin DO BEGIN
		w_out(iout)=TOTAL(w_in(i:i+ibin-1))/rbin
		x_out(iout)=TOTAL(x_in(i:i+ibin-1))/rbin
		e_out(iout)=SQRT(TOTAL(e_in(i:i+ibin-1)^2))/rbin
		iout=iout+1
	ENDFOR

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

finished:
	give_datp, datp

	RETURN, w_out
	END
