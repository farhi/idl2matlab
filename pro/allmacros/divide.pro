;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION divide, w_in1, w_in2

; Divides two workspaces with correct error propogation (any dimension)
;
;COMMAND SYNTAX
; w3=divide(w1,w2)
;
;						KHA, 6/11/97
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, dat1
	take_datp, dat2, /third

	e_in1=dat1.e
	e_in2=dat2.e

	zero=WHERE(w_in2 EQ 0.,nz)
	zeroed1=WHERE(e_in1 LT -0.9,nz1)
	zeroed2=WHERE(e_in2 LT -0.9,nz2)

	w_out=w_in1/w_in2
	e_out=SQRT((e_in1/w_in2)^2+(w_in1*e_in2/w_in2^2)^2)

	IF (nz NE 0) THEN BEGIN
		w_out(zero)=0.
		e_out(zero)=-1.
	ENDIF
	IF (nz1 NE 0) THEN BEGIN
		w_out(zeroed1)=0.
		e_out(zeroed1)=-1.
	ENDIF
	IF (nz2 NE 0) THEN BEGIN
		w_out(zeroed2)=0.
		e_out(zeroed2)=-1.
	ENDIF

	datp=dat1

	mod_datp, datp, "e", e_out

	give_datp, datp

	RETURN, w_out
	END
