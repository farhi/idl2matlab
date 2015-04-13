;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION average, w_in1, w_in2

; takes the weighted average of two workspaces
;						KHA, 25/2/97
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, dat1
	take_datp, dat2, /third

	e_in1=dat1.e
	e_in2=dat2.e

	z1=WHERE(e_in1 EQ 0.,nz1)
	IF (nz1 NE 0) THEN e_in1(z1)=1.
	z2=WHERE(e_in2 EQ 0.,nz2)
	IF (nz2 NE 0) THEN e_in2(z2)=1.

	w_out=(w_in1/e_in1^2+w_in2/e_in2^2)/(1./e_in1^2+1./e_in2^2)
	e_out=1./sqrt(1./e_in1^2+1./e_in2^2)

	IF (nz1 NE 0) THEN e_in1(z1)=0.
	IF (nz2 NE 0) THEN e_in2(z2)=0.

	IF (nz1 NE 0) THEN BEGIN
		w_out(z1)=w_in2(z1)
		e_out(z1)=e_in2(z1)
	ENDIF
	IF (nz2 NE 0) THEN BEGIN
		w_out(z2)=w_in1(z2)
		e_out(z2)=e_in1(z2)
	ENDIF

	datp=dat1

	mod_datp, datp, "e", e_out

	give_datp, datp

	RETURN, w_out
	END
