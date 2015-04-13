;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION unfold, w_in

;Copies one quadrant of the Qx-Qy plane (from fold.pro or qrebin.pro) into 
;all four quadrants
;
;DIMENSIONS:
; w_in(nQ,nQ) -> w_out(2*nQ,2*nQ)
;
;COMMAND SYNTAX:
; w10=unfold(w9)
;
;						KHA, 15/7/98
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	take_datp, datp

	sw=SIZE(w_in)

	nx=sw(1)
	ny=sw(2)
	IF (nx NE ny) THEN BEGIN
		PRINT,'Unfold: Error - x and y dimensions must be equal'
		GOTO, finished
	ENDIF
	n=nx

	x_in=datp.x
	y_in=datp.y
	e_in=datp.e

	x_out=[-REVERSE(x_in),x_in(1:n-1)]
	y_out=[-REVERSE(y_in),y_in(1:n-1)]

	w_out=FLTARR(2*n-1,2*n-1)	& e_out=FLTARR(2*n-1,2*n-1)
	n1=n-1
	n2=2*n-2

;	help, w_out(n1:n2,n1:n2)
;	help, w_in
	w_out(n1:n2,n1:n2)=w_in
	e_out(n1:n2,n1:n2)=e_in

	w_out(n1:n2,0:n1)=REVERSE(w_in,2)
	e_out(n1:n2,0:n1)=REVERSE(e_in,2)

	w_out(0:n1,0:n2)=REVERSE(w_out(n1:n2,0:n2),1)
	e_out(0:n1,0:n2)=REVERSE(e_out(n1:n2,0:n2),1)

	mod_datp, datp, "x", x_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	PRINT,'Unfold: one quadrant unfolded into four'
	datp.other_tit=datp.other_tit+' -uf'

finished:
	give_datp, datp

	RETURN, w_out
	END
