;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION fit_poly, w_in, degree=npoly, np

; Fits a polynomial of degree npoly in the x-dimension to one- or 
; two-dimensional data
;
;ARGUMENTS:
; degree	:degree of polynomial to fit the data with
;
;DIMENSIONS:
; 1-d or 2d data
;
;COMMAND SYNTAX:
; w3=fit_poly(w2,degree=<degree>)
;
;						KHA,JRS 21/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	w_out=w_in

	take_datp, datp
	IF(N_ELEMENTS(np) GT 0) THEN npoly=np

	x_in=datp.x
	e_in=datp.e

	sw=SIZE(w_in)
	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN e_in=SQRT(w_in)

	IF (sw(0) EQ 1) THEN BEGIN
		poly=POLYFITW(x_in,w_in,1./e_in^2,npoly,w_out)
	ENDIF ELSE IF (sw(0) EQ 2) THEN BEGIN
		nx=sw(1)
		ny=sw(2)
		x0=x_in
		FOR iy=0,ny-1 DO BEGIN
			w0=w_in(*,iy)
			e0=e_in(*,iy)
			poly=POLYFITW(x0,w0,1./e0^2,npoly,w0)
			w_out(*,iy)=w0
		ENDFOR
	ENDIF ELSE BEGIN
		PRINT,'Fit_poly: Error - workspace dimensions cannot be greater than 2'
		GOTO, finished
	ENDELSE

finished:
	mod_datp, datp, "e", 0
	give_datp, datp

	RETURN, w_out
	END
