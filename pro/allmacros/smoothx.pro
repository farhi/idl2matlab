;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION smoothx, w_in, nsmooth

; Smoothes using a moving filter in the x-dimension to one- or 
; two-dimensional data. smoothes over -nsmooth to +nsmooth
;
;						KHA, 6/5/98
;-------------------------------------------------------------------------------
;*******************************************************************************

	w_out=w_in

	take_datp, datp

	x_in=datp.x
	e_in=datp.e
	e_out=e_in

	sw=SIZE(w_in)
	nx=sw(1)
	PRINT,'nx=',nx
	IF (sw(0) EQ 1) THEN BEGIN
		FOR i=0,nx-1 DO BEGIN
			i1=(i-nsmooth)>0
			i2=(i+nsmooth)<(nx-1)
			r=FLOAT(i2-i1+1)
			w=TOTAL(w_in(i1:i2))
			e=SQRT(TOTAL(e_in(i1:i2)^2))
			w_out(i)=w/r	& e_out(i)=e/r
		ENDFOR
	ENDIF ELSE IF (sw(0) EQ 2) THEN BEGIN
		FOR i=0,nx-1 DO BEGIN
			i1=(i-nsmooth)>0
			i2=(i+nsmooth)<(nx-1)
			r=FLOAT(i2-i1+1)
			w=TOTAL(w_in(i1:i2,*),1)
			e=SQRT(TOTAL(e_in(i1:i2,*)^2,1))
			w_out(i,*)=w/r	& e_out(i,*)=e/r
		ENDFOR
	ENDIF ELSE BEGIN
		PRINT,'Smooth: Error - workspace dimensions cannot be greater than 2'
		GOTO, finished
	ENDELSE

	datp.e=e_out
	datp.other_tit=datp.other_tit+' -sx'
finished:
	give_datp, datp

	RETURN, w_out
	END
