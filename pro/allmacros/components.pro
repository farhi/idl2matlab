;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION components, w_in0, Schaerpf=Schaerpf

;For D7 data only
;
;Separates data taken with Z or XYZ polarisation analysis into
;the constituent components: 
;
;index
; 0: total
; 1: incoherent
; 2: coherent + isotope-incoherent
; 3: paramagnetic (average of spin-flip and non-spin-flip paramagnetic)
; 4: non-spin-flip paramagnetic
; 5: spin-flip paramagnetic
;
;KEYWORD:
; /Schaerpf	:use original Schaerpf expressions for component separation
;
;DIMENSIONS:
; No-TOF: w_in(32,nphases,nruns) -> w_out(32,ncomponents,nruns)
; TOF:    w_in(128,nspectra*nphases)   -> w_out(128,nspectra*ncomponents)
;
;COMMAND SYNTAX:
; w3=components(w2,[,/Schaerpf])
; 
; (optional keywords shown in square brackets)
;							KHA,JRS 17/3/00
;-------------------------------------------------------------------------------
;*******************************************************************************


	iprint=0	; if iprint>0, show debugging messages

	IF KEYWORD_SET(Schaerpf) THEN Schaerpf=1 ELSE Schaerpf=0

	IF(iprint GT 0) THEN BEGIN
		IF(Schaerpf EQ 1) THEN PRINT,'Components: using original Schaerpf expressions' $
				  ELSE PRINT,'Components: using standard expressions'
	ENDIF

	IF (iprint GT 0) THEN PRINT,'Start components:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw=SIZE(w_in0)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in0)=',sw

	par=datp.p
	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nruns=FIX(par(3))
	TOF=FIX(par(8))
	nchannels=FIX(par(6))
	chw=par(7)

	IF (nphases EQ 2) THEN ncomps=3 ELSE ncomps=nphases

	IF (nruns EQ 1) THEN x_in=datp.x ELSE x_in=datp.z
	y_in=datp.y
	e_in0=datp.e

	se=SIZE(e_in0)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in0)=',sw
		PRINT,'        SIZE(e_in0)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF (TOF EQ 0) THEN BEGIN
		IF (nphases EQ 1) THEN BEGIN
			w_in=FLTARR(nspectra,nphases,nruns)	& e_in=w_in
			w_in(*,0,*)=w_in0(*,*)
			e_in(*,0,*)=e_in0(*,*)
		ENDIF ELSE BEGIN
			w_in=w_in0
			e_in=e_in0
		ENDELSE
	ENDIF ELSE BEGIN
		w_in=REFORM(w_in0,nchannels,nspectra,nphases)
		e_in=REFORM(e_in0,nchannels,nspectra,nphases)
	ENDELSE

	IF (iprint GT 0) THEN BEGIN
		PRINT,'TOF=',TOF
		PRINT,'nspectra=',nspectra
		PRINT,'nphases=',nphases
		PRINT,'nchannels=',nchannels
		PRINT,'nruns=',nruns
		PRINT,'ncomps=',ncomps
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Separate components

	IF (iprint GT 0) THEN PRINT,'TOF=',TOF

; No-TOF: w_in(32,nphases,nruns) -> w_out(32,ncomponents,nruns)
; TOF:    w_in(128,nspectra*nphases)   -> w_out(128,nspectra*ncomponents)
;
; 0: total
; 1: incoherent
; 2: coherent + isotope-incoherent
; 3: paramagnetic (average of spin-flip and non-spin-flip paramagnetic)
; 4: non-spin-flip paramagnetic
; 5: spin-flip paramagnetic
;
; Schaerpf expressions:
;	 Mnf=2*(2*Znf-Xnf-Ynf) \
;	 Mfl=2*(Xfl+Yfl-2*Zfl) / => M=weighted average of Mnf and Mfl
;	  SI=(3*Zfl-Xfl-Yfl)*3/2
;	C+II=Znf-Zsf+SI/3
;
; Expressions used here:
;	 Mnf, Mfl and M the same
;	 Tnf=Znf+Xnf+Ynf
;	 Tfl=Zfl+Xfl+Yfl
;	  SI=Tfl/2-M
;	C+II=(2*Tnf-Tfl)/6

	IF (TOF EQ 0) THEN BEGIN
		w_out=FLTARR(nspectra,ncomps,nruns)	& e_out=w_out
		IF (nphases EQ 1) THEN w_out=w_in $
		ELSE IF (nphases EQ 2) THEN BEGIN
			w_out(*,0,*)=total(w_in,2)
			e_out(*,0,*)=sqrt(total(e_in^2,2))
			w_out(*,1,*)=1.5*w_in(*,1,*)
			e_out(*,1,*)=1.5*e_in(*,1,*)
			w_out(*,2,*)=w_in(*,0,*)-w_in(*,1,*)/2.
			e_out(*,2,*)=sqrt(e_in(*,0,*)^2+(e_in(*,1,*)/2.)^2)
		ENDIF ELSE BEGIN
			magsf=FLTARR(32,nruns)	& dmagsf=magsf	; spin-flip magnetic
			magsf=2.*(w_in(*,3,*)+w_in(*,5,*)-2.*w_in(*,1,*))
			dmagsf=2.*sqrt(e_in(*,3,*)^2+e_in(*,5,*)^2+(2.*e_in(*,1,*))^2)
			magnf=FLTARR(32,nruns)	& dmagnf=magnf	; non-spin-flip magnetic
			magnf=2.*(2.*w_in(*,0,*)-w_in(*,2,*)-w_in(*,4,*))
			dmagnf=2.*sqrt((2.*e_in(*,0,*))^2+e_in(*,2,*)^2+e_in(*,4,*)^2)
			zsf=WHERE(dmagsf EQ 0.,nzsf)
			IF (nzsf NE 0) THEN dmagsf(zsf)=1.
			znf=WHERE(dmagnf EQ 0.,nznf)
			IF (nznf NE 0) THEN dmagnf(znf)=1.
			mag=(magsf/dmagsf^2+magnf/dmagnf^2)/(1./dmagsf^2+1./dmagnf^2)
			dmag=1./sqrt(1./dmagsf^2+1./dmagnf^2)
			IF (nzsf NE 0) THEN dmagsf(zsf)=0.
			IF (nznf NE 0) THEN dmagnf(znf)=0.
			IF (nzsf NE 0) THEN BEGIN
				mag(zsf)=magnf(zsf)
				dmag(zsf)=dmagnf(zsf)
			ENDIF
			IF (nznf NE 0) THEN BEGIN
				mag(znf)=magsf(znf)
				dmag(znf)=dmagsf(znf)
			ENDIF
			IF (Schaerpf) THEN BEGIN
				inc=(3.*w_in(*,1,*)-w_in(*,3,*)-w_in(*,5,*))*3./2.
				dinc=sqrt((3.*e_in(*,1,*))^2+e_in(*,3,*)^2+e_in(*,5,*)^2)*3./2.
				coh=w_in(*,0,*)-w_in(*,1,*)+inc/3.
				dcoh=sqrt(e_in(*,0,*)^2+e_in(*,1,*)^2+(dinc/3.)^2)
			ENDIF ELSE BEGIN
				Tnf=w_in(*,0,*)+w_in(*,2,*)+w_in(*,4,*)
				dTnf=sqrt(e_in(*,0,*)^2+e_in(*,2,*)^2+e_in(*,4,*)^2)
				Tfl=w_in(*,1,*)+w_in(*,3,*)+w_in(*,5,*)
				dTfl=sqrt(e_in(*,1,*)^2+e_in(*,3,*)^2+e_in(*,5,*)^2)
				inc=Tfl/2.-mag 
				dinc=sqrt((dTfl/2.)^2+dmag^2)
				coh=(2.*Tnf-Tfl)/6.
				dcoh=sqrt((2.*dTnf)^2+dTfl^2)/6.
			ENDELSE
			w_out(*,0,*)=total(w_in,2)/3.
			e_out(*,0,*)=sqrt(total(e_in^2,2))/3.
			w_out(*,1,*)=inc
			e_out(*,1,*)=dinc
			w_out(*,2,*)=coh
			e_out(*,2,*)=dcoh
			w_out(*,3,*)=mag	& e_out(*,3,*)=dmag
			w_out(*,4,*)=magnf	& e_out(*,4,*)=dmagnf
			w_out(*,5,*)=magsf	& e_out(*,5,*)=dmagsf
		ENDELSE
	ENDIF ELSE BEGIN
		w_out=FLTARR(nchannels,nspectra,ncomps)	& e_out=w_out
		IF (nphases EQ 1) THEN w_out=w_in $
		ELSE IF (nphases EQ 2) THEN BEGIN
			w_out(*,*,0)=total(w_in,3)
			e_out(*,*,0)=sqrt(total(e_in^2,3))
			w_out(*,*,1)=1.5*w_in(*,*,1)
			e_out(*,*,1)=1.5*e_in(*,*,1)
			w_out(*,*,2)=w_in(*,*,0)-w_in(*,*,1)/2.
			e_out(*,*,2)=sqrt(e_in(*,*,0)^2+(e_in(*,*,1)/2.)^2)
		ENDIF ELSE BEGIN
			magsf=FLTARR(nchannels,nspectra)	& dmagsf=magsf	; spin-flip paramagnetic
			magsf=2.*(w_in(*,*,3)+w_in(*,*,5)-2.*w_in(*,*,1))
			dmagsf=2.*sqrt(e_in(*,*,3)^2+e_in(*,*,5)^2+(2.*e_in(*,*,1))^2)
			magnf=FLTARR(nchannels,nspectra)	& dmagnf=magnf	; non-spin-flip paramagnetic
			magnf=2.*(2.*w_in(*,*,0)-w_in(*,*,2)-w_in(*,*,4))
			dmagnf=2.*sqrt((2.*e_in(*,*,0))^2+e_in(*,*,2)^2+e_in(*,*,4)^2)
			zsf=WHERE(dmagsf EQ 0.,nzsf)
			IF (nzsf NE 0) THEN dmagsf(zsf)=1.
			znf=WHERE(dmagnf EQ 0.,nznf)
			IF (nznf NE 0) THEN dmagnf(znf)=1.
			mag=(magsf/dmagsf^2+magnf/dmagnf^2)/(1./dmagsf^2+1./dmagnf^2)
			dmag=1./sqrt(1./dmagsf^2+1./dmagnf^2)
			IF (nzsf NE 0) THEN dmagsf(zsf)=0.
			IF (nznf NE 0) THEN dmagnf(znf)=0.
			IF (nzsf NE 0) THEN BEGIN
				mag(zsf)=magnf(zsf)
				dmag(zsf)=dmagnf(zsf)
			ENDIF
			IF (nznf NE 0) THEN BEGIN
				mag(znf)=magsf(znf)
				dmag(znf)=dmagsf(znf)
			ENDIF
			Tnf=w_in(*,*,0)+w_in(*,*,2)+w_in(*,*,4)
			dTnf=sqrt(e_in(*,*,0)^2+e_in(*,*,2)^2+e_in(*,*,4)^2)
			Tfl=w_in(*,*,1)+w_in(*,*,3)+w_in(*,*,5)
			dTfl=sqrt(e_in(*,*,1)^2+e_in(*,*,3)^2+e_in(*,*,5)^2)
			w_out(*,*,0)=total(w_in,3)/3.
			e_out(*,*,0)=sqrt(total(e_in^2,3))/3.
			w_out(*,*,1)=Tfl/2.-mag
			e_out(*,*,1)=sqrt((dTfl/2.)^2+dmag^2)
			w_out(*,*,2)=(2.*Tnf-Tfl)/6.
			e_out(*,*,2)=sqrt((2.*dTnf)^2+dTfl^2)/6.
			w_out(*,*,3)=mag	& e_out(*,*,3)=dmag
			w_out(*,*,4)=magnf	& e_out(*,*,4)=dmagnf
			w_out(*,*,5)=magsf	& e_out(*,*,5)=dmagsf
		ENDELSE
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'End of Component Separation section'

;-------------------------------------------------------------------------------
;Rezero detectors and check dimensions of output data

	IF (TOF EQ 0) THEN BEGIN
		FOR irun=0,nruns-1 DO BEGIN
			zeroed=WHERE(e_in(*,0,irun) LT -0.9, nz)
			IF (nz NE 0) THEN BEGIN
				w_out(zeroed,*,irun)=0.
				e_out(zeroed,*,irun)=-1.
			ENDIF
		ENDFOR
	ENDIF ELSE BEGIN
		w_buf=INTARR(nchannels,nspectra)*0
		FOR i=0,nphases-1 DO BEGIN
			zeroed=WHERE(e_in(*,*,i) LE -1.,n)
			IF (n GT 0) THEN w_buf(zeroed)=1
		ENDFOR
		zeroed=WHERE(w_buf EQ 1,nz)
		IF (iprint GT 0) THEN PRINT,nz/nchannels,' zeroed spectra
		IF (nz GT 0) THEN FOR i=0,nphases-1 DO BEGIN
			w_buf=w_out(*,*,i)	& e_buf=e_out(*,*,i)
			w_buf(zeroed)=0.	& e_buf(zeroed)=-1.
			w_out(*,*,i)=w_buf	& e_out(*,*,i)=e_buf
		ENDFOR
	ENDELSE

	IF (TOF EQ 1) THEN BEGIN
		w_out=REFORM(w_out,nchannels,nspectra*ncomps)
		e_out=REFORM(e_out,nchannels,nspectra*ncomps)
		y_buf=y_in(0:nspectra-1)
		IF (ncomps EQ 3) THEN y_out=[y_buf,y_buf,y_buf]
		IF (ncomps EQ 6) THEN y_out=[y_buf,y_buf,y_buf,y_buf,y_buf,y_buf]
	ENDIF

	w_out=w_out
	e_out=e_out

	IF (iprint GT 0) THEN PRINT,'End of "Rezero detectors" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.p(2)=FLOAT(ncomps)
	IF(TOF EQ 1) THEN mod_datp, datp, "y", y_out
	IF (nphases EQ 2) THEN mod_datp, datp, "e", e_out $
			ELSE datp.e=e_out
	IF (TOF EQ 1 AND nphases EQ 6) THEN mod_datp, datp, "y", y_in(0:ncomps*nspectra-1)

	datp.w_tit='component of '+STRTRIM(datp.w_tit,2)
	datp.other_tit=datp.other_tit+' -co'
	PRINT,'Components: components separated'

finished:
	IF (iprint GT 0) THEN PRINT,'End Components:'

	give_datp, datp

	RETURN, w_out
	END
