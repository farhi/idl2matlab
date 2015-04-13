;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION sumbank, w_in, dQ=dQ, qb
;
; For IN4, IN5 and IN6 data.
;
;ARGUMENTS
; dQ:	Q-spacing with which to sum detectors. If dQ is not specified, nothing
;	is done for IN5 data. For IN6, adds data in centre, upper and lower
;	detector banks by default.
;
;DIMENSIONS
; w_in=w_out(nchannels,nspectra)
;
;COMMAND SYNTAX
; w2=sumbank(w1,dQ=<dQ>)
;
;							KHA,JRS 8/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp_access, inst

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start sumbank:'

	take_datp, datp
	IF(N_ELEMENTS(qb) GT 0) THEN dQ=qb

;-------------------------------------------------------------------------------
;Set up input workspace

	par=datp.p

	y_in=datp.y
	e_in=datp.e

	sw=size(w_in)
	nchannels=sw(1)
	IF (sw(0) EQ 1) THEN nspectra=1     ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,w_in
	npts=nchannels

	se=size(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1) OR se(2) NE sw(2)) THEN e_in=SQRT(w_in)

	IF (N_ELEMENTS(dQ) EQ 0) THEN dQ=0
	lambda=par(21)

	IF (iprint GT 0) THEN PRINT,'npts=',npts,' nspectra=',nspectra
	IF (iprint GT 0) THEN PRINT,'End of input setup section'

;-------------------------------------------------------------------------------
;add IN6 detectors together

	y_out=y_in
	w_out=w_in	& e_out=e_in

	IF (inst EQ 'IN6') THEN BEGIN
		angle=0.
		iangle=-1
		FOR ispec=0,nspectra-1 DO BEGIN
			IF (y_in(ispec) NE angle) THEN BEGIN
				iangle=iangle+1
				angle=y_in(ispec)
				y_out(iangle)=angle
				w_out(*,iangle)=w_in(*,ispec)
				e_out(*,iangle)=e_in(*,ispec)^2
			ENDIF ELSE BEGIN
				w_out(*,iangle)=w_out(*,iangle)+w_in(*,ispec)
				e_out(*,iangle)=e_out(*,iangle)+e_in(*,ispec)^2
			ENDELSE
		ENDFOR
		w_out=w_out(*,0:iangle)
		e_out=SQRT(e_out(*,0:iangle))
		y_out=y_out(0:iangle)
		nspectra=iangle+1
		IF (iprint GT 0) THEN PRINT,'IN6: nspectra=',nspectra
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of IN6 detector-adding section'

;-------------------------------------------------------------------------------
;add detectors together with dQ spacing

	k=2.*!pi/lambda
	Qarr=2.*k*SIN(y_out*!pi/360.)	; convert scattering angle to Q

	Qmin=-1.
	iout=-1
	FOR ispec=0,nspectra-1 DO BEGIN
		Q=Qarr(ispec)
		IF ((Q-Qmin) GT dQ) THEN BEGIN
			IF (ispec NE 0) THEN y_out(iout)=y_out(iout)/nout
			Qmin=Q	& iout=iout+1	& nout=1
			y_out(iout)=y_out(ispec)
			w_out(*,iout)=w_out(*,ispec)
			e_out(*,iout)=e_out(*,ispec)^2
		ENDIF ELSE BEGIN
			nout=nout+1
			y_out(iout)=y_out(iout)+y_out(ispec)
			w_out(*,iout)=w_out(*,iout)+w_out(*,ispec)
			e_out(*,iout)=e_out(*,iout)+e_out(*,ispec)^2
		ENDELSE
	ENDFOR

	y_out(iout)=y_out(iout)/nout

	y_out=y_out(0:iout)
	w_out=w_out(*,0:iout)
	e_out=SQRT(e_out(*,0:iout))
	nspectra=iout+1

	IF (iprint GT 0) THEN PRINT,'End of other detector-adding section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	IF (dQ EQ 0.) THEN dQ='0' ELSE BEGIN
		s=STRTRIM(STRING(dQ),2) & i=RSTRPOS(s,'.') & dQ=STRMID(s,0,i+3)
	ENDELSE
	Qstring='dQ='+dQ+'A-1'
	IF (inst EQ 'IN6') THEN Qstring='added centre, upper and lower detector bank data, '+Qstring
	PRINT,'sumbank: '+Qstring

	datp.other_tit=datp.other_tit+' -sb('+dQ+')

	give_datp, datp

finished:
	RETURN, w_out

	END
