	FUNCTION t2l, w_in

;converts from cts/TOF to cts/lambda
;
; input=output format: w(128,nspectra)
;							KHA, 30/10/96


	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start t2l:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input file and energy parameters

	par=datp.p

	lambda=par(3)
	freq=par(4)
	chw=par(6)
	chel=par(8)

	IF (iprint GT 0) THEN PRINT,'lambda=',lambda,' chel=',chel
	IF (iprint GT 0) THEN PRINT,'freq=',freq,'rpm   chw=',chw,'mcs'

	TOF=FIX(par(7))
	IF (TOF NE 1) THEN BEGIN
		PRINT,' Corr_tof: Error - workspace data is not in TOF'
		GOTO, finished
	ENDIF

	nchannels=FIX(par(5))
	nphases=FIX(par(1))

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	IF (sw(0) EQ 1) THEN nspectra=1 $
			ELSE nspectra=sw(2)

	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	y_in=datp.y
	e_in=datp.e

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in)=',sw
		PRINT,'        SIZE(e_in)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------------
;	Set constants and prepare arrays

	eff=FLTARR(nchannels)
	wave=FLTARR(nchannels)

	const1=5.22697		; E(meV)=const1*V(m/ms)^2 for neutron
	const2=2.07193571	; E(meV)=const2*Q(A^-1)^2 for neutron
	const3=3.956076		; V(m/ms)=const3/lambda(A) for neutron
	const4=81.8066		; E(meV)=const4/lambda(A)^2 for neutron

	L2=1500.0		; sample-detector distance (mm)

	Ei=const4/lambda^2
	Vi=SQRT(Ei/const1)
	Tel=L2/Vi

	time=chw*FLOAT(INDGEN(nchannels)-chel)+Tel
	Ef=const1*(L2/time)^2
	wave=SQRT(const4/Ef)
	x_out=wave
	w_out=w_in

	datp.x=x_out
	datp.x_tit='Neutron Wavelength (A)'
	
finished:
	IF (iprint GT 0) THEN PRINT,'End t2l:'

	give_datp, datp

	RETURN, w_out
	END
