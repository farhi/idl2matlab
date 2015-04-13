	FUNCTION radial, w_in0, lim1, lim2, bins
;
; Produces radial integration of isotropic SANS data (with proper errors)

; lim1 and lim2 	define radial limits of integration
; bins 			defines number of points in outfile

;							JRS 30/3/00

	iprint=0	;shows debugging messages

	IF (iprint GT 0) THEN PRINT, 'Start radial:'
	
	take_datp, datp

;--------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw=SIZE(w_in0)
	se=size(w_in0)
	IF (sw(0) EQ 3) THEN nruns=sw(3) ELSE nruns=1

	z_in0=datp.z
	e_in0=datp.e
	par=datp.p
	parv=datp.pv

	x0=par(13)
	y0=par(14)
	x_in0=datp.x-x0
	y_in0=datp.y-y0

	p_out=par
	p_out(13)=0.0
	p_out(14)=0.0	;write (0,0) beamcentre to output parameter block
	
	IF (nruns GT 1) THEN BEGIN
		pv_out=parv
		pv_out(13,*)=0.0
		pv_out(14,*)=0.0
	ENDIF
		
	IF (iprint GT 0) THEN PRINT,sw(1),sw(2)
	IF (iprint GT 0) THEN PRINT,'radial: End of "check dimensions" section'

;----------------------------------------------------------------------------
; Sort data

	w_buf=FLTARR(sw(1)*sw(2),nruns)
	e_buf=FLTARR(sw(1)*sw(2),nruns)
	r_buf=FLTARR(sw(1)*sw(2))
	ir=0
	FOR ix=0,sw(1)-1 DO BEGIN
		FOR iy=0,sw(2)-1 DO BEGIN
			r_buf(ir)=SQRT((x_in0(ix)^2)+(y_in0(iy)^2))
			w_buf(ir,*)=w_in0(ix,iy,*)
			e_buf(ir,*)=e_in0(ix,iy,*)
			ir=ir+1
		ENDFOR
	ENDFOR

	nx_in=sw(1)*sw(2)

	i=SORT(r_buf)
	w_buf1=w_buf
	e_buf1=e_buf
	r_buf1=w_buf
	FOR j=1,nruns DO BEGIN
		r_buf1(*,j-1)=r_buf(i)
		w_buf1(*,j-1)=w_buf(i,j-1)
		e_buf1(*,j-1)=e_buf(i,j-1)
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'radial: End of "sort data" section'

;-------------------------------------------------------------------------
;Perform radial average

	IF (N_ELEMENTS(bins) EQ 0) THEN bins=30.
	IF (N_ELEMENTS(lim2) EQ 0) THEN lim2=FIX(sw(1)-x0)
	IF (N_ELEMENTS(lim1) EQ 0) THEN lim1=5.

	bw=FLOAT((lim2-lim1)/FLOAT(bins))
	IF (iprint GT 0) THEN PRINT,'radial: bin width= ',bw

	rmax=r_buf1(nx_in-1,0)
	nx_out=bins
	r_out=(INDGEN(nx_out)*bw)+lim1

	w_out=FLTARR(nx_out,nruns)	& w_out(*,*)=0.
	e_out=FLTARR(nx_out,nruns)	& e_out(*,*)=-1.

	FOR g=1,nruns DO BEGIN
		i0=0
		FOR iQ=0,nx_out-1 DO BEGIN
			Q0=r_out(iQ)
			wsum=0.
			e2sum=0.
			n=0
			FOR i=i0,nx_in-1 DO BEGIN
				Q=r_buf1(i,g-1)
				IF (ABS(Q-Q0) LT bw/2.) THEN BEGIN
					IF(e_buf1(i,g-1) NE -1) THEN BEGIN
						wsum=wsum+w_buf1(i,g-1)
						e2sum=e2sum+e_buf1(i,g-1)^2
						n=n+1
					ENDIF 
				ENDIF ELSE IF (Q GE Q0+bw/2.) THEN GOTO, endloop
			ENDFOR
endloop:		i0=i
			IF (n GT 0) THEN BEGIN
				w_out(iQ,g-1)=wsum/n
				e_out(iQ,g-1)=sqrt(e2sum)/n
			ENDIF
		ENDFOR
	ENDFOR

; for multiple runs, define temperature as y-axis and arrange in ascending order
	
	IF (nruns GT 1) THEN BEGIN
		y_out=FLTARR(nruns)
		y_out(*)=parv(10,*)
		datp.y_tit='Temperature (K)'
		i=SORT(y_out)
		y_out=y_out(i)
		pv_out(*,*)=pv_out(*,i)
		w_out(*,*)=w_out(*,i)
		e_out(*,*)=e_out(*,i)
	ENDIF
	
	IF (iprint GT 0) THEN PRINT,'radial: End of "rebin to constant R" section'

;-------------------------------------------------------------------------------
; return data

	IF (nruns EQ 1) THEN BEGIN
		w_out=REFORM(w_out)
		e_out=REFORM(e_out)
		datp.y_tit='Intensity'
	ENDIF ELSE BEGIN
		mod_datp, datp, "pv", pv_out
		mod_datp, datp, "y", y_out
	ENDELSE
	
	datp.x_tit='Radius (pixels)'
	
	slim1=STRTRIM(STRING(lim1),2)
lim10:	n=STRLEN(slim1)	& i=RSTRPOS(slim1,'0')
	IF (i EQ n-1) THEN BEGIN
		slim1=STRMID(slim1,0,n-1)
		GOTO, lim10
	ENDIF
	slim2=STRTRIM(STRING(lim2),2)
lim20:	n=STRLEN(slim2)	& i=RSTRPOS(slim2,'0')
	IF (i EQ n-1) THEN BEGIN
		slim2=STRMID(slim2,0,n-1)
		GOTO, lim20
	ENDIF
	sbins=STRTRIM(STRING(bins),2)
bins0:	n=STRLEN(sbins)	& i=RSTRPOS(sbins,'0')
	IF (i EQ n-1) THEN BEGIN
		sbins=STRMID(sbins,0,n-1)
		GOTO, bins0
	ENDIF

	datp.other_tit=datp.other_tit+' -ra('+slim1+','+slim2+','+sbins+')'
		
	mod_datp, datp, "x", r_out
	mod_datp, datp, "e", e_out
	mod_datp, datp, "p", p_out	

finished:
	give_datp, datp

	RETURN, w_out
	END

	
