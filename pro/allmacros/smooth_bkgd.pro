;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION smooth_bkgd, w_in, ismooth
;
;For IN5 data only
;
; Performs a smoothing of a background measurement. Assumes that the signal
; consists of an elastic plus broad background scattering. ismooth must be odd. 
;
;ARGUMENTS
; ismooth<0 :	Replaces the elastic peak by a best-fit gaussian and performs a
;		moving filter smoothing (ABS(ismooth)) on the rest. 
; ismooth>0 :	Performs a moving filter smoothing (ismooth) everywhere. 
;
; ismooth=0 :	No smoothing (default)
;
;DIMENSIONS
; w_in=w_out(nhcannels,nspectra)
;
;COMMAND SYNTAX
; w5=smooth_bkgd(w4,<ismooth>)
;							KHA 17/7/98
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start smooth_bkgd:'

	IF (N_ELEMENTS(ismooth) EQ 0) THEN ismooth=0
	IF (ismooth EQ 0) THEN return,w_in

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy parameters


	par=datp.p
	lambda=par(21)
	cheldet=par(9)	; elastic channel for high-angle detectors
	chw=par(18)
	const=3.956076
	Vi=const/lambda
	chelmul=cheldet-300./(Vi*chw)

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	nchannels=sw(1)
	IF (sw(0) EQ 1) THEN nspectra=1     ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,w_in

	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	phi=datp.y
	x_in=datp.x
	e_in=datp.e

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN e_in=SQRT(w_in)

	IF (iprint GT 0) THEN PRINT,'End of setup section'

;-------------------------------------------------------------------------------------
;	Fit Gaussians

	w_out=w_in	& e_out=e_in*0.
	gauss=FLTARR(4)	& dgauss=gauss
	x=x_in
	FOR ispec=0,nspectra-1 DO BEGIN
		y=w_in(*,ispec)
		e=e_in(*,ispec)
		IF (ismooth LT 0) THEN BEGIN
			IF (phi(ispec) LT 10) THEN x0=chelmul ELSE x0=cheldet
			IF (nspectra EQ 1) THEN x0=cheldet
			PRINT,'Fitting Gaussian to s',ispec
			fitgauss, x, y, e, x0-30., x0+30., gauss, dgauss, 1
			gaussian, x, gauss, ygauss
			centre=gauss(2)	& FWHM=gauss(3)*2.355
			ysmooth=SMOOTH(y,ABS(ismooth))
			i1=WHERE(x LT centre-3.*FWHM, n1)
			i2=WHERE(x GE centre-3.*FWHM AND x LE centre+3.*FWHM, n2)
			i3=WHERE(x GT centre+3.*FWHM, n3)
			yall=[ysmooth(i1),ygauss(i2),ysmooth(i3)]
		ENDIF ELSE yall=SMOOTH(y,ismooth)
		w_out(*,ispec)=yall
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of fitting section'

	datp.e=e_out

	give_datp, datp

finished:
	RETURN, w_out
	END


