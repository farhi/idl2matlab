;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION binq, w_in, dQ=dQ, qb

;Interpolates data along the x-axis to regular steps with point spacing dQ. 
;
;ARGUMENTS:
; dQ	:desired constant Q-bin width
; (qb is obsolete, kept for backwards compatability)
;
;DIMENSIONS:
; w_in(nspectra) -> w_out(nQ)
;
;COMMAND SYNTAX:
; w2=binq(w1,dQ=<dQ>)
;						KHA,JRS 14/8/00
;------------------------------------------------------------------------------
;******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start binq:'

	take_datp, datp

	IF(N_ELEMENTS(qb) GT 0) THEN dQ=qb
	
;-------------------------------------------------------------------------------
;Check dimensions of input workspace and setup output workspace

	sw=SIZE(w_in)
	if (sw(0) ne 1) and (sw(0) ne 2) then return,w_in
	par=datp.p

	nx_in=sw(1)
	IF (sw(0) EQ 2) THEN ny=sw(2) ELSE ny=1

	e_in=datp.e
	x_in=datp.x
	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'
;-------------------------------------------------------------------------------
;Rebin to constant Q

	Qmax=x_in(nx_in-1)
	nx_out=Qmax/dQ+1
	x_out=INDGEN(nx_out)*dQ

	w_out=FLTARR(nx_out,ny)	& w_out(*,*)=0.
	e_out=FLTARR(nx_out,ny)	& e_out(*,*)=-1.
	wsum=FLTARR(ny)
	e2sum=FLTARR(ny)

	i0=0
	FOR iQ=0,nx_out-1 DO BEGIN
		Q0=x_out(iQ)
		wsum(*)=0.
		e2sum(*)=0.
		n=0
		FOR i=i0,nx_in-1 DO BEGIN
			Q=x_in(i)
			IF (ABS(Q-Q0) LT dQ/2.) THEN BEGIN
				wsum(*)=wsum(*)+w_in(i,*)
				e2sum(*)=e2sum(*)+e_in(i,*)^2
				n=n+1
			ENDIF ELSE IF (Q GE Q0+dQ/2.) THEN GOTO, endloop
		ENDFOR
endloop:	i0=i
		IF (n GT 0) THEN BEGIN
			w_out(iQ,*)=wsum/n
			e_out(iQ,*)=sqrt(e2sum)/n
		ENDIF
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of "rebin to constant Q" section'

;-------------------------------------------------------------------------------
;Remove undefined points

	buf=e_out(*,0)
	i=WHERE(e_out GE 0.,n)

	x_out=x_out(i)
	w_buf=FLTARR(n,ny)
	e_buf=FLTARR(n,ny)
	FOR iy=0,ny-1 DO BEGIN
		w_buf(*,iy)=w_out(i,iy)
		e_buf(*,iy)=e_out(i,iy)
	ENDFOR
	w_out=w_buf
	e_out=e_buf

	IF (iprint GT 0) THEN PRINT,'End of "remove undefined points" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	dQs=STRTRIM(STRING(dQ),2) & np=RSTRPOS(dQs,'.')+3 & dQs=STRMID(dQs,0,np)
	PRINT,'binq: data binned to dQ='+dQs
finished:

	mod_datp, datp, "e", e_out
	mod_datp, datp, "x", x_out

	datp.other_tit=datp.other_tit+' -bq('+dQs+')'

	IF (iprint GT 0) THEN PRINT,'End binq:'

	give_datp, datp

	RETURN, w_out
	END
