;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION qrebin, w_in, omegashift=omegashift, fold=fold, $
	                 nQ=nQ, oms

;For D7 data only
;
;takes output workspace of omega_scan.pro containing omega-scan, converts to
;Q-space and rebins to a constant Qx-Qy grid in a single quadrant, giving 
;sensible error bars! Deals with only one spin phase.
;
;ARGUMENTS:
; omegashift	: offset in degrees of omega=0 position
; (oms is obsolete, kept for backwards compatability)
;
;KEYWORDS:
; fold		: folds quadrants into +,+
;
;DIMENSIONS:
; w_in(nspectra,nruns)	-> w_out(nQ,nQ) (folded), w_out(2*nQ,2*nQ) (unfolded)
;
;COMMAND SYNTAX:
; w10=qrebin(w9,[nQ=<nQ>][,omegashift=<omegashift>][,/fold])
;						KHA,JRS 31/3/01
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, datp

	iprint=0	; if iprint>0, show debugging messages

	fld=1		; don't fold data (default)
	IF(N_ELEMENTS(oms) GT 0) THEN omegashift=oms
	IF KEYWORD_SET(fold) THEN fld=0

;-------------------------------------------------------------------------------
;Set constants

	par=datp.p
	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nruns=FIX(par(3))
	lambda=par(4)
	k=2.*!pi/lambda

	IF (iprint GT 0) THEN PRINT,'End of "set constants" section'

;-------------------------------------------------------------------------------
;Set up arrays

	ntheta=nspectra
	nomega=nruns

	e_in=datp.e

	phi=-datp.x		; detector angle: anticlockwise is +ve
	IF (N_ELEMENTS(omegashift) EQ 0) THEN omegashift=0.
	omega=-datp.y+omegashift	; sample rotation angle

	theta=FLTARR(ntheta)
	FOR itheta=0,ntheta-1 DO $
		IF (phi(itheta) LE 0.) THEN $
			theta(itheta)=phi(itheta)/2.+90. $
		ELSE theta(itheta)=phi(itheta)/2.-90.

	Qmag=k*SQRT(2.*(1.-COS(phi*!pi/180.)))

	Qx=FLTARR(ntheta,nomega)
	Qy=FLTARR(ntheta,nomega)
	FOR iomega=0,nomega-1 DO BEGIN
		Qx(*,iomega)=Qmag*COS((theta-omega(iomega))*!pi/180.)
		Qy(*,iomega)=Qmag*SIN((theta-omega(iomega))*!pi/180.)
	ENDFOR

	IF (lambda GT 4.5) THEN BEGIN
		IF(N_ELEMENTS(nQ) EQ 0) THEN BEGIN
			nQ=26
			dQ=0.1
		ENDIF ELSE BEGIN
			dQ=2.6/FLOAT(nQ)
		ENDELSE
	ENDIF ELSE BEGIN
		IF(N_ELEMENTS(nQ) EQ 0) THEN BEGIN
			nQ=21
			dQ=0.2
		ENDIF ELSE BEGIN
			dQ=4.2/FLOAT(nQ)
		ENDELSE
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'omega=',omega
	IF (iprint GT 0) THEN FOR itheta=0,ntheta-1 DO $
		PRINT,'itheta=',itheta,' phi=',phi(itheta), $
		' theta=',theta(itheta),' Qmag=',Qmag(itheta)
	IF (iprint GT 0) THEN PRINT,'End of "Set up arrays" section'

;-------------------------------------------------------------------------------
;Rebin data

	w_out=FLTARR((fld+1)*nQ,(fld+1)*nQ)	& w_out(*,*)=0.
	e_out=FLTARR((fld+1)*nQ,(fld+1)*nQ)	& e_out(*,*)=0.
	n_out=FLTARR((fld+1)*nQ,(fld+1)*nQ)	& n_out(*,*)=0.

	FOR itheta=0,ntheta-1 DO BEGIN
		FOR iomega=0,nomega-1 DO BEGIN
			IF (fld EQ 1) THEN BEGIN
				ix=FIX(((Qx(itheta,iomega)+dQ/2.)/dQ)+nQ)
				iy=FIX(((Qy(itheta,iomega)+dQ/2.)/dQ)+nQ)
			ENDIF ELSE BEGIN
				ix=FIX(ABS((Qx(itheta,iomega))+dQ/2.)/dQ)
				iy=FIX(ABS((Qy(itheta,iomega))+dQ/2.)/dQ)
			ENDELSE
			w_out(ix,iy)=w_out(ix,iy)+w_in(itheta,iomega)
			e_out(ix,iy)=e_out(ix,iy)+e_in(itheta,iomega)^2
			n_out(ix,iy)=n_out(ix,iy)+1.
		ENDFOR
	ENDFOR

	FOR ix=0,(fld+1)*nQ-1 DO BEGIN
		FOR iy=0,(fld+1)*nQ-1 DO BEGIN
			IF (n_out(ix,iy) GT 0.) THEN BEGIN
				w_out(ix,iy)=w_out(ix,iy)/n_out(ix,iy)
				e_out(ix,iy)=SQRT(e_out(ix,iy))/n_out(ix,iy)
			ENDIF ELSE BEGIN
				w_out(ix,iy)=0.
				e_out(ix,iy)=-1.
			ENDELSE
		ENDFOR
	ENDFOR

	x_out=FLOAT((INDGEN((fld+1)*nQ)-(fld*nQ))*dQ)	& y_out=x_out

	IF (iprint GT 0) THEN PRINT,'End of "rebin data" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "x", x_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	datp.x_tit='Qx'
	datp.y_tit='Qy'

	d=STRTRIM(STRING(dQ),2)
tryd:	n=STRLEN(d)	& i=RSTRPOS(d,'0')
	IF (i EQ n-1) THEN BEGIN
		d=STRMID(d,0,n-1)
		GOTO, tryd
	ENDIF
	s=STRTRIM(STRING(omegashift),2)
trys:	n=STRLEN(s)	& i=RSTRPOS(s,'0')
	IF (i EQ n-1) THEN BEGIN
		s=STRMID(s,0,n-1)
		GOTO, trys
	ENDIF
	PRINT,'Qrebin: rebinned to Qx-Qy grid with dQ='+d+'A-1 and omega-shift='+s+'degrees'
	datp.other_tit=datp.other_tit+' -qr('+d+','+s
	IF(fld=0) THEN datp.other_tit=datp.other_tit+',/fold)' ELSE $
		       datp.other_tit=datp.other_tit+')'

finished:
	give_datp, datp

	RETURN, w_out
	END
