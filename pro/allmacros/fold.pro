;-----------------------------------------------------------------------------
;*****************************************************************************
;
	FUNCTION fold, w_in

;For single-crystal D7 data (omega scan mode)
;Folds the four quadrants of the Qx-Qy plane, output from QxQy.pro, into one.
;
;DIMENSIONS:
; w_in(nQ.nQ) -> w_out(nQ/2,nQ/2)
;
;COMMAND SYNTAX:
; w10=fold(w9)
;
;							KHA, 15/7/98
;------------------------------------------------------------------------------
;******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	take_datp, datp

	check=SIZE(w_in)

	nx=check(1)
	ny=check(2)
	x_in=datp.x
	y_in=datp.y

	se=SIZE(datp.e)
	IF (se(0) NE 2) THEN BEGIN
; If datp.e is not set, then assume w_in(ix,iy)=0 => outside the measured region
		e_in=FLTARR(nx,ny)
		FOR ix=0,nx-1 DO FOR iy=0,ny-1 DO $
		  IF (w_in(ix,iy) EQ 0.) THEN e_in(ix,iy)=-1. ELSE e_in(ix,iy)=1.
	ENDIF ELSE e_in=datp.e

	w_buf=w_in
	e_buf=e_in^2
	n_buf=w_in*0.

	xeven=0
	xodd=0
	IF (2*(nx/2) EQ nx) THEN xeven=1 ELSE xodd=1
	yeven=0
	yodd=0
	IF (2*(ny/2) EQ ny) THEN yeven=1 ELSE yodd=1

	IF (iprint GT 0) THEN BEGIN
		PRINT,'nx=',nx,' xeven=',xeven,' xodd=',xodd
		PRINT,'ny=',ny,' yeven=',yeven,' yodd=',yodd
	ENDIF

	ixL1=0
	IF (xeven EQ 1) THEN ixL2=nx/2-1 ELSE ixL2=nx/2
	ixR1=nx/2
	ixR2=nx-1

	iyB1=0
	IF (yeven EQ 1) THEN iyB2=ny/2-1 ELSE iyB2=ny/2
	iyT1=ny/2
	iyT2=ny-1

	n_buf(*)=0.
	i=WHERE(e_in GE 0., n)
	IF (n GE 1) THEN n_buf(i)=1.

	i=WHERE(e_in LT 0., n)
	IF (n GE 1) THEN e_buf(i)=0.

;	PRINT,'w_buf=',w_buf
;	PRINT,'e_buf=',e_buf
;	PRINT,'n_buf=',n_buf

	w_buf1=REVERSE(w_buf,1)	& w_buf=w_buf+w_buf1
	e_buf1=REVERSE(e_buf,1)	& e_buf=e_buf+e_buf1
	n_buf1=REVERSE(n_buf,1)	& n_buf=n_buf+n_buf1

	w_buf1=REVERSE(w_buf,2)	& w_buf=w_buf+w_buf1
	e_buf1=REVERSE(e_buf,2)	& e_buf=e_buf+e_buf1
	n_buf1=REVERSE(n_buf,2)	& n_buf=n_buf+n_buf1

	nx=ixR2-ixR1+1	& x_out=x_in(ixR1:ixR1+nx-1)

	ny=iyT2-iyT1+1	& y_out=y_in(iyT1:iyT1+ny-1)

	w_out=w_buf(ixR1:iXR1+nx-1,iyT1:iyT1+ny-1)
	e_out=SQRT(e_buf(ixR1:iXR1+nx-1,iyT1:iyT1+ny-1))
	n_out=n_buf(ixR1:iXR1+nx-1,iyT1:iyT1+ny-1)

	i=WHERE(n_out GT 0., n)
	IF (n GE 1) THEN w_out(i)=w_out(i)/n_out(i)
	IF (n GE 1) THEN e_out(i)=e_out(i)/n_out(i)

	i=WHERE(n_out LE 0., n)
	IF (n GE 1) THEN w_out(i)=0.
	IF (n GE 1) THEN e_out(i)=-1.

	mod_datp, datp, "x", x_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	PRINT,'Fold: four quadrants folded into one'
	datp.other_tit=datp.other_tit+' -fo'
finished:
	give_datp, datp

	RETURN, w_out
	END
