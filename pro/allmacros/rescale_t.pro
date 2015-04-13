;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION rescale_t, w_in, Told=Told, Tnew=Tnew

;For IN5 and IN6 data
;
;Rescales data taken at a temperature Told to a temperature Tnew, using
;the Bose thermal population factor. Workspace data must be as a function
;of energy transfer. Energies in meV.
;
;ARGUMENTS
; Told	: Temperature of input data
; Tnew	: Desired rescaling temperature
;
;DIMENSIONS
; w_in(dE,nspectra)=w_out
;
;COMMAND SYNTAX
; w10=rescale_t(w9,Told=<Told>,Tnew=<Tnew>)
;
;							KHA,JRS 7/8/00
;
;------------------------------------------------------------------------------
;******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start rescale_t:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set up input workspace and energy parameters

	par=datp.p

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	nchannels=sw(1)
	IF (sw(0) EQ 1) THEN nspectra=1     ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,w_in

	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	x_in=datp.x
	y_in=datp.y
	e_in=datp.e

	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN e_in=SQRT(w_in)

;-------------------------------------------------------------------------------------
;	Perform correction

	kB=0.0861735	; Boltzmann constant in meV/K

	w_out=w_in
	e_out=e_in
	x_out=FLTARR(nchannels)
	y_out=datp.y

	arg=x_in/(kB*Told)	& Bose=arg
	i=WHERE(arg LT -50.,n)
	IF (n GE 1) THEN arg(i)=-50.
	i=WHERE(arg GT 50.,n)
	IF (n GE 1) THEN arg(i)=50.
	i=WHERE(arg NE 0.,n)
	Bose(i)=1./(1.-EXP(-arg(i)))
	i=WHERE(arg EQ 0.,n)
	IF (n GE 1) THEN Bose(i)=10000.
	i=WHERE(ABS(Bose) GE 9999.,n)
	IF (n GE 1) THEN Bose(i)=Told
	Bold=Bose

	arg=x_in/(kB*Tnew)
	i=WHERE(arg LT -50.,n)
	IF (n GE 1) THEN arg(i)=-50.
	i=WHERE(arg GT 50.,n)
	IF (n GE 1) THEN arg(i)=50.
	i=WHERE(arg NE 0.,n)
	Bose(i)=1./(1.-EXP(-arg(i)))
	i=WHERE(arg EQ 0.,n)
	IF (n GE 1) THEN Bose(i)=10000.
	i=WHERE(ABS(Bose) GE 9999.,n)
	IF (n GE 1) THEN Bose(i)=Tnew
	Bnew=Bose

	corr=Bnew/Bold	; temperature correction factor

	IF (iprint GT 0) THEN BEGIN
		PRINT, 'channel      Eps       Bose(old)    Bose(new)   ratio'
		FOR i=0,nchannels-1 DO PRINT, i, x_in(i), Bold(i), Bnew(i), corr(i)
	ENDIF

	corr=corr#(FLTARR(1,nspectra)+1)

	w_out=w_in*corr
	e_out=e_in*corr

	IF (iprint GT 0) THEN PRINT,'End of main section'
;-------------------------------------------------------------------------------------

	datp.e=e_out

	s=STRTRIM(STRING(FLOAT(Told)),2) & i=RSTRPOS(s,'.') & Told=STRMID(s,0,i+2)
	s=STRTRIM(STRING(FLOAT(Tnew)),2) & i=RSTRPOS(s,'.') & Tnew=STRMID(s,0,i+2)
	datp.other_tit=datp.other_tit+' rt('+Told+','+Tnew+')'

	give_datp, datp

	PRINT,'rescale_t: Rescaled from T='+Told+'K to T='+Tnew+'K'

finished:
	RETURN, w_out
	END


