;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION elastic, w_in, min=xmin, max=xmax, save=save
;
;For IN4, IN5, IN6 and D7 data. 
; 
;Finds the elastic peak position in each spectrum by fitting a Gaussian
;and a flat background. Looks for peak in region xmin<x<xmax 
;
;ARGUMENTS:
;	min:	minimum x-value for peak search
;	max:	maximum x-value for peak search
;
;KEYWORDS:
; 	/save     : Write out results to file 'elastic.dat'
;
;DIMENSIONS:
;w_in(nchannels,nspectra) OR w_in(nchannels)
;
;COMMAND SYNTAX:
;w3=elastic(w2[,min=<min>][,max=<max>][,/save])
;
;(optional arguments and keywords shown in square brackets)
;
;							KHA,JRS 13/7/00
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp_access, inst

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start elastic:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy gaussameters

	par=datp.p

	IF (N_ELEMENTS(xmin) EQ 0) THEN BEGIN
		xmin=par(9)-30.
		xmax=par(9)+30.
	ENDIF

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	nchannels=sw(1)
	IF (sw(0) EQ 1) THEN nspectra=1     ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,0

	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	phi=datp.y
	x_in=datp.x
	e_in=datp.e

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN e_in=SQRT(w_in)

	IF (iprint GT 0) THEN PRINT,'End of setup section'

;-------------------------------------------------------------------------------------
;	Fit Gaussians

	x_out=phi
	w_out=FLTARR(nspectra)	& e_out=w_out

	gauss=FLTARR(4)	& dgauss=gauss
	x=x_in
	FOR ispec=0,nspectra-1 DO BEGIN
		y=w_in(*,ispec)
		e=e_in(*,ispec)
		IF (iprint GT 0) THEN PRINT,'Fitting Gaussian to s',ispec
		fitgauss, x, y, e, xmin, xmax, gauss, dgauss, ispec
		w_out(ispec)=gauss(2)	& e_out(ispec)=dgauss(2)
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of fitting section'

;-------------------------------------------------------------------------------------
;	Save to file

	IF KEYWORD_SET(save) THEN BEGIN
		OPENW, 1, 'elastic.dat'
		FOR i=0,nspectra-1 DO PRINTF, 1, phi(i), w_out(i), e_out(i)
		PRINTF, 1, 'Elastic channels'
		PRINTF, 1, 'Scattering Angle'
		PRINTF, 1, 'Elastic Channel'
		CLOSE, 1
		PRINT,'elastic: Elastic channels svaed to file "elastic.dat"'
	ENDIF
;-------------------------------------------------------------------------------------

	datp.x_tit='Scattering Angle'
	datp.y_tit='Elastic Channel'

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

	x1=STRTRIM(STRING(FIX(xmin)),2) & x2=STRTRIM(STRING(FIX(xmax)),2)
	datp.other_tit=datp.other_tit+' -el('+x1+','+x2+')'

	PRINT,'elastic: Gaussians fitted from channels '+x1+' to '+x2

	give_datp, datp

finished:
	RETURN, w_out
	END


