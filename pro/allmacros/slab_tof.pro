;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION slab_tof, w_in, angle=alpha, inc_xs=sigmai, abs_xs=sigmaa, $
		 thick=thick, N=N
;For D7, IN4, IN5 and IN6

;Takes a 2-D tof workspace and corrects for sample attenuation of the
;scattered neutrons assuming infinite slab geometry.
;
;ARGUMENTS
; angle	: the angle in degrees of sample normal rel. to incident beam
; inc_xs: inchorent cross-section of sample (barns)
; abs_xs: absorption cross-section for thermal neutrons (lambda=1.8Å) (barns)
; thick	: thickness of sample (mm)
; N	: number density of sample (*10^22 per cm^3)
;
;DIMENSIONS
; w_in(nchannels,nspectra) -> w_out(nchannels,nspectra)
;
;COMMAND SYNTAX
; w7=slab_tof(w6,angle=<angle>,inc_xs=<inc_xs>,abs_xs=<abs_xs>,thick=<thick>,N=<N>)
;
;							JRS, 10/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	COMMON c_lamp_access, inst
	IF (iprint GT 0) THEN PRINT,'Start slab_tof:'

	take_datp, datp

	alpha=FLOAT(alpha)
	sigmai=FLOAT(sigmai)
	sigmaa=FLOAT(sigmaa)
	thick=FLOAT(thick)
	N=FLOAT(N)
	alphar=alpha*!pi/180.
	Nr=N*1.e22

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw

	par=datp.p

	IF(inst NE 'D7') THEN BEGIN
		lambda=par(21)
		chw=par(18)		; channelwidth in microseconds
		chel=par(9)
	ENDIF ELSE BEGIN
		lambda=par(4)
		chw=par(7)
		chel=par(9)
	ENDELSE

	nspectra=sw(2)
	nchannels=sw(1)
	ei=81.8066/(lambda^2)

	IF (iprint GT 0) THEN BEGIN
		PRINT,'lambda=',lambda
		PRINT,'ei=',ei
		PRINT,'nspectra=',nspectra
		PRINT,'nchannels=',nchannels
	ENDIF

	x_in=datp.x	;time-of-flight
	y_in=datp.y	;scattering angle
	e_in=datp.e

	const1=5.22697          ; E(meV)=const1*V(mm/mus)^2 for neutron
	IF (inst EQ 'IN5') THEN L2=par(27)*1000
	IF (inst EQ 'IN6') THEN L2=2470
	IF (inst EQ 'D7') THEN L2=1500
	Vi=SQRT(ei/const1)
	Tel=L2/Vi		; time in microseconds

	time=chw*FLOAT(x_in-chel)+Tel
	Ef=const1*(L2/time)^2
	Eps=Ei-Ef

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in)=',sw
		PRINT,'        SIZE(e_in)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF


	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Perform correction

	w_out=w_in
	e_out=FLTARR(nchannels,nspectra)

	ttheta=y_in*!pi/180.	; anticlockwise is +ve

	sigmaa1=sigmaa*1.E-24
	sigmai1=sigmai*1.E-24
	sigmaar=sigmaa1*lambda/1.8
	mu=Nr*(sigmaar+sigmai1)
	mu2=Nr*sigmaar*(SQRT(ei/(ei-Eps)))
	beta=mu/SIN(alphar)
	t=thick/10.

	IF (iprint EQ 1) THEN BEGIN
		PRINT,'sigmai1=',sigmai
		PRINT,'sigmaa1=',sigmaa
		PRINT,'sigmaar=',sigmaar
		PRINT,'mu=',mu
		PRINT,'t=',t
	ENDIF

	gama=FLTARR(nchannels,nspectra)

	FOR ang=0,nspectra-1 DO BEGIN
		gama(0,ang)=mu2/SIN(alphar-ttheta(ang))
	ENDFOR

	A=(EXP(-beta*t)-EXP(-gama*t))/(gama-beta)

	w_out=w_in*t/A
	e_out=e_in*t/A
	x_out=x_in
	y_out=y_in

	IF (iprint GT 0) THEN PRINT,'End of Correction section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.e=e_out
	
	als=STRTRIM(STRING(alpha),2)	& np=RSTRPOS(als,'.')+0	& als=STRMID(als,0,np)
	sis=STRTRIM(STRING(sigmai),2)	& np=RSTRPOS(sis,'.')+3	& sis=STRMID(sis,0,np)
	aas=STRTRIM(STRING(sigmaa),2)	& np=RSTRPOS(aas,'.')+3	& aas=STRMID(aas,0,np)
	ths=STRTRIM(STRING(thick),2)	& np=RSTRPOS(ths,'.')+3	& ths=STRMID(ths,0,np)
	Ns=STRTRIM(STRING(N),2)		& np=RSTRPOS(Ns,'.')+3	& Ns=STRMID(Ns,0,np)
	
	datp.other_tit=datp.other_tit+' -st('+als+','+sis+','+aas+','+ $
		ths+','+Ns+')'

finished:
	IF (iprint GT 0) THEN PRINT,'End slab_tof:'

	give_datp, datp

	RETURN, w_out
	END
