;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION QxQy, w_in, omegashift=omegashift, dQ=dQ, os

;Takes output workspace of omega_scan.pro containing omega-scan, converts to
;Q-space and interpolates to a constant Qx-Qy grid (rough and ready version of
;qrebin.pro)  Deals with only one spin phase at a time
;
;ARGUMENTS:
; omegashift	: angle (in degrees) of omega=0 position
; dQ		: Q interpolation bin
; (os is obsolete, kept for backwards compatability)
;
;DIMENSIONS
; w_in(nspectra,nruns) -> w_out(nQx,nQy)
;
;COMMAND SYNTAX:
; w6=QxQy(w5[,/omegashift=<omega>])
;
; (optional arguments shown in square brackets)
;
;						KHA,JRS 21/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, datp

	iprint=0	; if iprint>0, show debugging messages

	IF(N_ELEMENTS(os) GT 0) THEN omegashift=os

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

	IF (iprint GT 0) THEN PRINT,'omega=',omega
	IF (iprint GT 0) THEN FOR itheta=0,ntheta-1 DO $
		PRINT,'itheta=',itheta,' phi=',phi(itheta), $
		' theta=',theta(itheta),' Qmag=',Qmag(itheta)
	IF (iprint GT 0) THEN PRINT,'End of "Set up arrays" section'

;-------------------------------------------------------------------------------
;Check number of banks

	negative=0
	positive=0
	IF (phi(0) LT 0.) THEN BEGIN
		negative=1
		nneg1=0
		FOR iphi=1,ntheta-1 DO $
			IF (phi(iphi) GT 0.) THEN GOTO, pos
		nneg2=ntheta-1
		npos1=0
		npos2=0
		GOTO, endbanks
	pos:	positive=1
		nneg2=iphi-1
		npos1=iphi
		npos2=ntheta-1
	ENDIF ELSE BEGIN
		positive=1
		npos1=0
		FOR iphi=1,ntheta-1 DO $
			IF (phi(iphi) LT 0.) THEN GOTO, neg
		npos2=ntheta-1
		nneg1=0
		nneg2=0
		GOTO, endbanks
	neg:	negative=1
		npos2=iphi-1
		nneg1=iphi
		nneg2=ntheta-1
	ENDELSE
endbanks:

	IF (iprint GT 0) THEN BEGIN
		PRINT,'negative=',negative,' positive=',positive
		PRINT,'nneg1=',nneg1,' nneg2=',nneg2,' npos1=',npos1,' npos2=',npos2
	ENDIF
	IF (iprint GT 0) THEN PRINT,'End of "Check number of banks" section'

;-------------------------------------------------------------------------------
;Interpolate to regular Qx-Qy grid

	Qx=FLTARR(ntheta,nomega)
	Qy=FLTARR(ntheta,nomega)
	FOR iomega=0,nomega-1 DO BEGIN
		Qx(*,iomega)=Qmag*COS((theta-omega(iomega))*!pi/180.)
		Qy(*,iomega)=Qmag*SIN((theta-omega(iomega))*!pi/180.)
	ENDFOR

	TRIANGULATE, Qx, Qy, triangles, b

	IF N_ELEMENTS(dQ) EQ 0 THEN dQ=0.05
	GS=[dQ,dQ]
	Limits=[-2.*k,-2.*k,2.*k,2.*k]

	w_out=TRIGRID(Qx,Qy,w_in,triangles,GS,limits)
	e_out=TRIGRID(Qx,Qy,e_in,triangles,GS,limits)

	sw=size(w_out)

	x_out=FINDGEN(sw(1))-(sw(1)-1)/2. & x_out=x_out*dQ
	y_out=FINDGEN(sw(2))-(sw(2)-1)/2. & y_out=y_out*dQ

	nx=size(x_out)	& nx=nx(1)
	ny=size(y_out)	& ny=ny(1)

	IF (iprint GT 0) THEN PRINT,'End of "Interpolate to regular grid" section'

;-------------------------------------------------------------------------------
;Set points outside measured region to zero

	Qmin=100.
	Qmax=-100.
	FOR iQ=0,ntheta-1 DO BEGIN
		Q=Qmag(iQ)
		IF (Q LT Qmin) THEN Qmin=Q
		IF (Q GT Qmax) THEN Qmax=Q
	ENDFOR

	thetaneg1=theta(nneg2)
	thetaneg2=theta(nneg1)
	thetapos1=theta(npos2)
	thetapos2=theta(npos1)

	omegamin=omega(0)
	omegamax=omega(nomega-1)
	IF (omegamin GT omegamax) THEN BEGIN
		omegabuf=omegamin
		omegamin=omegamax
		omegamax=omegabuf
	ENDIF
	domega=(omegamax-omegamin)/FLOAT(nomega-1)
	fullcircle=0
	IF (360.-(omegamax-omegamin) LT 1.5*domega) THEN fullcircle=1

	IF (iprint GT 0) THEN BEGIN
		PRINT,'Qmin=',Qmin,' Qmax=',Qmax
		PRINT,'thetaneg1=',thetaneg1,' thetaneg2=',thetaneg2
		PRINT,'thetapos1=',thetapos1,' thetapos2=',thetapos2
		PRINT,'omegamin=',omegamin,' omegamax=',omegamax,' fullcircle=',fullcircle
	ENDIF

;	GOTO, skip

	ip=0
	FOR ix=0,nx-1 DO BEGIN
		Qx0=x_out(ix)
		FOR iy=0,ny-1 DO BEGIN
			Qy0=y_out(iy)
			Q=SQRT(Qx0^2+Qy0^2)
			IF (Q LT Qmin) OR (Q GT Qmax) THEN GOTO, zero
			IF (fullcircle EQ 1) THEN GOTO, notzero
			outneg=0
			outpos=0
			psi0=ACOS(Qx0/Q)*180./!pi
			IF (Qy0 LT 0.) THEN psi0=-psi0
			phi0=ACOS(1.-0.5*(Q/k)^2)*180./!pi
			IF (iprint GT 0) THEN ip=ip+1
			IF (ip EQ 10) THEN PRINT,'Qx=',Qx0,' Qy=',Qy0,' psi=',psi0,' phi=',phi0
			IF (negative EQ 1) THEN BEGIN
				IF (ip EQ 10) THEN PRINT,'phi is negative'
				theta0=-phi0/2.+90.
				IF (ip EQ 10) THEN PRINT,'theta0=',theta0
				IF (theta0 LT thetaneg1) OR (theta0 GT thetaneg2) THEN GOTO, outsideneg
				omega0=theta0-psi0
				IF (ip EQ 10) THEN PRINT,'omega0=',omega0
				nom=0
				FOR iom=-2,1 DO $
				IF (omega0 GE omegamin+iom*360.) AND (omega0 LE omegamax+iom*360.) THEN nom=nom+1
				IF (nom EQ 0) THEN GOTO, outsideneg
				IF (nom GE 2) THEN PRINT,'Error in -ve: nom>1'
				IF (ip EQ 10) THEN PRINT,'inside omega region'
			ENDIF ELSE BEGIN
	outsideneg:		outneg=1
				IF (ip EQ 10) THEN PRINT,'outside -ve region'
			ENDELSE
			IF (positive EQ 1) THEN BEGIN
				IF (ip EQ 10) THEN PRINT,'phi is positive'
				theta0=phi0/2.-90.
				IF (ip EQ 10) THEN PRINT,'theta0=',theta0
				IF (theta0 LT thetapos1) OR (theta0 GT thetapos2) THEN GOTO, outsidepos
				omega0=theta0-psi0
				IF (ip EQ 10) THEN PRINT,'omega0=',omega0
				nom=0
				FOR iom=-2,1 DO $
				IF (omega0 GE omegamin+iom*360.) AND (omega0 LE omegamax+iom*360.) THEN nom=nom+1
				IF (nom EQ 0) THEN GOTO, outsidepos
				IF (nom GE 2) THEN PRINT,'Error in +ve: nom>1'
				IF (ip EQ 10) THEN PRINT,'inside omega region'
			ENDIF ELSE BEGIN
	outsidepos:		outpos=1
				IF (ip EQ 10) THEN PRINT,'outside +ve region'
			ENDELSE
			IF (ip EQ 10) THEN ip=0
			IF (outneg EQ 1) AND (outpos EQ 1) THEN GOTO, zero ELSE GOTO, notzero
	zero:		w_out(ix,iy)=0.
			e_out(ix,iy)=-1.
	notzero:	dummy=0.
		ENDFOR
	ENDFOR

skip:

	IF (iprint GT 0) THEN PRINT,'End of "set outside points to zero" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "x", x_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	datp.x_tit='Qx'
	datp.y_tit='Qy'

	s=STRTRIM(STRING(omegashift),2)
try:	n=STRLEN(s)	& i=RSTRPOS(s,'0')
	IF (i EQ n-1) THEN BEGIN
		s=STRMID(s,0,n-1)
		GOTO, try
	ENDIF
	PRINT,'QxQy: converted to Qx-Qy grid with omega-shift='+s+'degrees'
	datp.other_tit=datp.other_tit+' -qx('+s+')'

finished:
	give_datp, datp

	RETURN, w_out
	END
