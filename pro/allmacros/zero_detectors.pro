;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION zero_detectors, w_in0, bad1, bad2, bad3, bad4, bad5, angle=angle

;For D7 polarization analysis data only
;Zeroes non-contributing spectra or angular regions - (I=0, dI=-1)
;Data should already have every other detector removed (i.e. must have
;been put through normalise).
; 
;ARGUMENTS:
; bad1, bad2, ...	:Can either be
;			 a) detector numbers (must be even and between 2 and 64)
;			 b) ranges in scattering angle
;KEYWORD:
; /angle		:specifies that angular ranges are used
;
;DIMENSIONS
; w_in=w_out(nspectra,nphases,nruns)
;
;COMMAND SYNTAX:
; e.g.
; a) w2=zero_detectors(w1,[28,40])
;		-zeroes spectra 28 and 40
;
; b) w2=zero_detectors(w1,[29.5,44.2],/angle)
;		-zeroes spectra with scattering angles between 29.5 and 44.2 
;		 degrees. This alternative format is specified by the additional
;		 "1" after the square brackets. 
;
;						KHA,JRS 11/3/99
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start zero_detectors:'

	take_datp, datp

	np=N_PARAMS()
	angflg=0
	IF KEYWORD_SET(angle) THEN BEGIN
		IF(N_PARAMS() EQ 2) THEN bad2=1
		IF(N_PARAMS() EQ 3) THEN bad3=1
		IF(N_PARAMS() EQ 4) THEN bad4=1
		IF(N_PARAMS() EQ 5) THEN bad5=1
		np=np+1
	ENDIF
;-------------------------------------------------------------------------------
;Check dimensions of input workspace and setup output workspace

	sw=SIZE(w_in)
	par=datp.p

	TOF=FIX(par(8))
	nchannels=FIX(par(6))
	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nruns=FIX(par(3))

	e_in0=datp.e
	IF (TOF EQ 0 AND nphases EQ 1) THEN BEGIN
		w_in=reform(w_in0,nspectra,nphases,nruns)
		e_in=reform(e_in0,nspectra,nphases,nruns)
	ENDIF ELSE BEGIN
		w_in=w_in0
		e_in=e_in0
	ENDELSE

	w_out=w_in
	e_out=e_in
	x_in=datp.x
	z_in=datp.z

	nothing=0
	IF (np EQ 1) THEN BEGIN
		nothing=1
		GOTO, finished
	ENDIF ELSE IF (np EQ 2) THEN BEGIN
		range=0
		bad=bad1
		nbad=N_ELEMENTS(bad)
	ENDIF ELSE BEGIN
		range=1
		IF (np EQ 3 AND N_ELEMENTS(bad2) EQ 1) THEN BEGIN
			IF (N_ELEMENTS(bad1) EQ 2 AND bad2 EQ 1) THEN bad=bad1 $
			ELSE GOTO, argerror
		ENDIF ELSE IF (np EQ 4 AND N_ELEMENTS(bad3) EQ 1) THEN BEGIN
			IF (N_ELEMENTS(bad1) EQ 2 AND N_ELEMENTS(bad2) EQ 2 $
				AND bad3 EQ 1) THEN bad=[[bad1],[bad2]] $
			ELSE GOTO, argerror
		ENDIF ELSE IF (np EQ 5 AND N_ELEMENTS(bad4) EQ 1) THEN BEGIN
			IF (N_ELEMENTS(bad1) EQ 2 AND N_ELEMENTS(bad2) EQ 2 AND N_ELEMENTS(bad3) EQ 2 $
				AND bad4 EQ 1) THEN bad=[[bad1],[bad2],[bad3]] $
			ELSE GOTO, argerror
		ENDIF ELSE IF (np EQ 6 AND N_ELEMENTS(bad5) EQ 1) THEN BEGIN
			IF (N_ELEMENTS(bad1) EQ 2 AND N_ELEMENTS(bad2) EQ 2 AND N_ELEMENTS(bad3) EQ 2 AND N_ELEMENTS(bad4) EQ 2 $
				AND bad5 EQ 1) THEN bad=[[bad1],[bad2],[bad3],[bad4]] $
			ELSE GOTO, argerror
		ENDIF
		s=size(bad)
		IF (s(0) EQ 1) THEN nbad=1 ELSE nbad=s(2)
	ENDELSE
	GOTO, noargerror
argerror:
	PRINT,'zero_detectors: Error in argument format'
	GOTO, finished
noargerror:
		
	IF (iprint GT 0) THEN BEGIN
		PRINT,'size(w_in)=',sw
		PRINT,'TOF=',TOF
		PRINT,'nchannels=',nchannels
		PRINT,'nspectra=',nspectra
		PRINT,'nphases=',nphases
		PRINT,'nruns=',nruns
		PRINT,'nbad=',nbad
		IF (nbad NE 0) THEN PRINT,'bad=',bad
	ENDIF

	IF (range EQ 0) THEN PRINT,'Zero_detectors: zeroing detectors',bad $
	ELSE BEGIN
		phimin=bad(0,*)	& phimax=bad(1,*)
		FOR i=0,nbad-1 DO PRINT, $
			FORMAT='("Zero_detectors: zeroing in range",F7.2," < phi <",F7.2," deg")', $
			phimin(i), phimax(i)
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Zero bad spectra

	IF (iprint GT 0) THEN PRINT,'range=',range

	IF (TOF EQ 0) THEN BEGIN
		IF (range EQ 0) THEN BEGIN
			IF (iprint GT 0) THEN PRINT,'zeroing spectrum',bad
			IF (nspectra EQ 32) THEN BEGIN
				w_out(bad/2-1,*,*)=0.
				e_out(bad/2-1,*,*)=-1.
			ENDIF ELSE BEGIN
				w_out(bad-1,*,*)=0.
				e_out(bad-1,*,*)=-1.
			ENDELSE
		ENDIF ELSE BEGIN
			IF (nruns EQ 1) THEN BEGIN
				FOR i=0,nbad-1 DO BEGIN
					bad=WHERE(x_in GT phimin(i) AND x_in LT phimax(i),n)
					IF (n GE 1) THEN BEGIN
						w_out(bad,*,*)=0.
						e_out(bad,*,*)=-1.
					ENDIF
				ENDFOR
			ENDIF ELSE BEGIN
				FOR irun=0,nruns-1 DO BEGIN
					x=z_in(*,irun)
					FOR i=0,nbad-1 DO BEGIN
						bad=WHERE(x GT phimin(i) AND x LT phimax(i),n)
						IF (n GE 1) THEN BEGIN
							w_out(bad,*,irun)=0.
							e_out(bad,*,irun)=-1.
						ENDIF
					ENDFOR
				ENDFOR
			ENDELSE
		ENDELSE
	ENDIF ELSE BEGIN
		IF (range EQ 0) THEN BEGIN
			FOR i=0,nbad-1 DO BEGIN
				IF (iprint GT 0) THEN PRINT,'zeroing spectrum',bad(i)
				IF (nspectra EQ 32) THEN ispec=bad(i)/2-1
				IF (nspectra EQ 64) THEN ispec=bad(i)-1
				FOR iphase=0,nphases-1 DO BEGIN
					w_out(*,iphase*32+ispec)=0.
					e_out(*,iphase*32+ispec)=-1.
				ENDFOR
			ENDFOR
		ENDIF ELSE BEGIN
			FOR i=0,nbad-1 DO BEGIN
				bad=WHERE(phimin(i) LT x_in AND x_in LT phimax(i),n)
				IF (n GE 1) THEN BEGIN
					w_out(*,bad)=0.
					e_out(*,bad)=-1.
				ENDIF
			ENDFOR
		ENDELSE
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'End of "Zero bad spectra" section'

;-------------------------------------------------------------------------------
;Return parameters and exit


finished:

	IF (TOF EQ 0 AND nphases EQ 1) THEN BEGIN
		w_out=reform(w_out,nspectra,nruns)
		e_out=reform(e_out,nspectra,nruns)
	ENDIF

	IF (nothing EQ 1) THEN s='0' $
	ELSE IF (range EQ 0) THEN BEGIN
		s=STRTRIM(STRING(bad(0)),2)
		IF (nbad GT 1) THEN FOR i=1,nbad-1 DO s=s+','+STRTRIM(STRING(bad(i)),2)
	ENDIF ELSE BEGIN
		s1=STRTRIM(STRING(phimin(0)),2)	& s2=STRTRIM(STRING(phimax(0)),2)
		n=STRPOS(s1,'.')>0+3	& s1=STRMID(s1,0,n)
		n=STRPOS(s2,'.')>0+3	& s2=STRMID(s2,0,n)
		s=s1+'>'+s2
		IF (nbad GT 1) THEN FOR i=1,nbad-1 DO BEGIN
			s1=STRTRIM(STRING(phimin(i)),2)	& s2=STRTRIM(STRING(phimax(i)),2)
			n=STRPOS(s1,'.')>0+3	& s1=STRMID(s1,0,n)
			n=STRPOS(s2,'.')>0+3	& s2=STRMID(s2,0,n)
			s=s+','+s1+'>'+s2
		ENDFOR
	ENDELSE
			
	datp.other_tit=datp.other_tit+' -zd('+s+')'

	IF (iprint GT 0) THEN PRINT,'End zero_detectors:'

	datp.e=e_out

	give_datp, datp

	IF (iprint GT 0) THEN BEGIN
		PRINT,' irun    ispec     phi     w_in(ispec,0,irun)  w_out(ispec,0,irun)'
		FOR irun=0,nruns-1 DO BEGIN
			FOR ispec=0,nspectra-1 DO $
				PRINT, irun, ispec, z_in(ispec,irun), w_in(ispec,0,irun), w_out(ispec,0,irun)
		ENDFOR
	ENDIF

	RETURN, w_out
	END
