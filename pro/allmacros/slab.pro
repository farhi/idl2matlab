;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION slab, w_in, angle=gamma0, muR=Nsigmat, Tmin=Amin, $
		       output_array = output_array

;For D7 data only - (useful for slab geometry single crystal omega scans)
;Corrects for sample attenuation of the scattered neutrons assuming 
;infinite slab geometry. 
;
;ARGUMENTS:
; angle	:angle in degrees of sample normal rel. to incident beam at omega=0. 
;	 Counterclockwise is +ve
; muR	:muR is Nsigmat where sample transmission=EXP(-Nsigmat)
;	 N*sigma=0.037 mm^-1 for vanadium
; Tmin	:Minimum transmission of sample (defined by real thickness and length)
;
;DIMENSIONS
; w_in(32,nphases,nruns) -> w_out(32,nphases,nruns)
;
;COMMAND SYNTAX:
; w5=slab(w4,angle=<angle>,muR=<muR>,Tmin=<Tmin>)
;
;							KHA,JRS 3/6/02
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start slab:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw = SIZE(w_in)
	IF iprint GT 0 THEN PRINT,'SIZE(w_in)=',sw

	par = datp.p
	nspectra = FIX(par[1])
	nphases = FIX(par[2])
	nruns = FIX(par[3])
	nchannels = FIX(par[6])
	TOF = FIX(par[8])

	IF iprint GT 0 THEN BEGIN
		PRINT, 'TOF      = ', TOF
		PRINT, 'nspectra = ', nspectra
		PRINT, 'nphases  = ', nphases
		PRINT, 'nruns    = ', nruns
	ENDIF

	IF nruns EQ 1 THEN BEGIN
		x_in=datp.x
		y_in=0.
	ENDIF ELSE BEGIN
		x_in=datp.z
		y_in=datp.pv[20,*]
		IF y_in[0] EQ y_in[nruns-1] THEN y_in=datp.pv[21,*]
		IF y_in[0] EQ y_in[nruns-1] THEN y_in[*]=0.
	ENDELSE

	e_in=datp.e
	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in)=',sw
		PRINT,'        SIZE(e_in)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF TOF EQ 1 THEN BEGIN
		PRINT,'Slab: eror - data must be non-TOF'
		GOTO, finished
	ENDIF

	IF iprint GT 0 THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Perform correction

	w_out=w_in
	e_out=e_in

	gamma0 = gamma0*!pi/180.
	xbuf = FLTARR(nspectra)
	ybuf = FLTARR(nspectra)
	FOR irun=0,nruns-1 DO BEGIN
		phi  = -x_in[*,irun]*!pi/180.	; anticlockwise is +ve
		xbuf = phi*180./!pi
		omega = y_in[irun]*!pi/180.
		gamma = gamma0 - omega
		IF (gamma LT -!pi/2.) THEN gamma=gamma+!pi $
		ELSE IF (gamma GT !pi/2.) THEN gamma=gamma-!pi
		IF (gamma LT -!pi/2.) THEN gamma=gamma+!pi $
		ELSE IF (gamma GT !pi/2.) THEN gamma=gamma-!pi
		pmin=gamma-!pi/2.
		pmax=gamma+!pi/2.
		IF (pmax GT !pi) THEN BEGIN
			pmin=pmin-!pi
			pmax=pmax-!pi
		ENDIF ELSE IF (pmin LT -!pi) THEN BEGIN
			pmin=pmin+!pi
			pmax=pmax+!pi
		ENDIF
		secg = 1./COS(gamma)
		IF (iprint GT 0) THEN BEGIN
			PRINT
			PRINT,'irun=',irun,' omega=',y_in(irun),' gamma=',gamma*180./!pi
			PRINT,'phimin=',pmin*180./!pi,' phimax=',pmax*180./!pi
			PRINT,'phi(0)=',phi(0)*180./!pi,' phi(max)=',phi(nspectra-1)*180./!pi
		ENDIF
		FOR i=0,nspectra-1 DO BEGIN
			p=phi(i)
			secpg=1./COS(p-gamma)
			IF (p GT pmin AND p LT pmax) THEN $	;transmission
				A=(EXP(-Nsigmat*secg)-EXP(-Nsigmat*secpg))/(Nsigmat*(secpg-secg)) $
			ELSE $		; reflection
				A=(1.-EXP(-Nsigmat*(secg-secpg)))/(Nsigmat*(secg-secpg))
			IF (A LT Amin) THEN A=Amin
			IF (nphases EQ 1) THEN BEGIN
				w_out(i,irun)=w_out(i,irun)/A
				e_out(i,irun)=e_out(i,irun)/A
			ENDIF ELSE BEGIN
				w_out(i,*,irun)=w_out(i,*,irun)/A
				e_out(i,*,irun)=e_out(i,*,irun)/A
			ENDELSE
			ybuf(i)=A
		ENDFOR
		IF iprint GT 0 THEN BEGIN
			DIALWSET
			plot, xbuf, ybuf
		ENDIF
	ENDFOR

	IF KEYWORD_SET(output_array) THEN w_out = ybuf

	IF iprint GT 0 THEN PRINT,'End of Correction section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.e=e_out


finished:
	IF (iprint GT 0) THEN PRINT,'End slab:'

	give_datp, datp

	RETURN, w_out
	END
