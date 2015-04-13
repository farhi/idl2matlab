;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION smoothy, w_in, nsmooth

; smoothes using a moving filter in the y-dimension to 
; two-dimensional data. smoothes over -nsmooth to +nsmooth
;
;						KHA, 6/5/98
;-------------------------------------------------------------------------------
;*******************************************************************************

	w_out=w_in

	take_datp, datp

	y_in=datp.y
	e_in=datp.e
	e_out=e_in

	sw=SIZE(w_in)
	ny=sw(2)
	PRINT,'ny=',ny
	IF (sw(0) EQ 2) THEN BEGIN
		FOR i=0,ny-1 DO BEGIN
			i1=(i-nsmooth)>0
			i2=(i+nsmooth)<(ny-1)
			r=FLOAT(i2-i1+1)
			w=TOTAL(w_in(*,i1:i2),2)
			e=SQRT(TOTAL(e_in(*,i1:i2)^2,2))
			w_out(*,i)=w/r	& e_out(*,i)=e/r
		ENDFOR
	ENDIF ELSE BEGIN
		PRINT,'Smooth: Error - workspace dimension must be 2'
		GOTO, finished
	ENDELSE

	datp.e=e_out
	datp.other_tit=datp.other_tit+' -sy'
finished:
	give_datp, datp

	RETURN, w_out
	END
