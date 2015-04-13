;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION isotropic_ms, w_in0, roh=roh, muR=muR, abs_xs=sigmaa, $
		 nuc_xs=sigmanuc, inc_xs=sigmasi, mag_xs=sigmamag
;
; Takes background subtracted D7 data of 1, 2 or 6 phases and corrects for
; multiple scattering using the isotropic approximation of Wells and Cywinski,
; and the secondary scattering coefficients of Blech and Averbach.  At present,
; deals only with cylindrical samples.  Correction of TOF data not yet implemented. 
;
; References
; ----------
; P. Wells and R. Cywinski, Aust. J. Phys. 34 (1981) 193
; T. M. Harders, T. J. Hicks and P. Wells, J. Appl. Cryst. 18 (1985) 131
; J. Mayers and R. Cywinski, Nucl. Inst. Meth. Phys. Res. A241 (1985) 519
; I. A. Blech and B. L. Averbach, Phys. Rev. 137 (1965) A1113
;
;ARGUMENTS:
; roh	: R/h - radius of cylinder over height
; muR	: Nsigmat*R calculated from transmission measurements
; abs_xs: Absorption Cross-section (actual) in barns
; nuc_xs: Nuclear Cross-section in barns
; inc_xs: Spin-Incoherent cross-section in barns
; mag_xs: Magnetic cross-section in barns (optional)
;
;DIMENSIONS
; w_in = w_out(nspectra,nphases,nruns) - unless nruns=1
;
;COMMAND SYNTAX:
; w6=isotropic_ms(w5,roh=<roh>,muR=<muR>,abs_xs=<abs_xs>,nuc_xs=<nuc_xs>,inc_xs=<inc_xs>[,mag_xs=<mag_xs>])
;
; (optional arguments shown in square brackets)
;
;							JRS 18/4/02
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint = 0	; if iprint > 0, show debugging messages
	
	COMMON c_lamp
	IF iprint GT 0 THEN PRINT,'Start isotropic_ms:'	

	take_datp, datp

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw = SIZE(w_in0)
	IF iprint GT 0 THEN PRINT,'SIZE(w_in0)=',sw

	par = datp.p
	nspectra = FIX(par(1))
	nphases = FIX(par(2))
	nruns = FIX(par(3))
	TOF = FIX(par(8))
	nchannels = FIX(par(6))
	chw = par(7)

	IF nruns EQ 1 THEN x_in = datp.x ELSE x_in = datp.z
	y_in = datp.y
	e_in0 = datp.e
	se = SIZE(e_in0)
	IF (se(0) NE sw(0)) OR (se(1) NE sw(1)) THEN BEGIN
		PRINT,'Error - SIZE(w_in0)=',sw
		PRINT,'        SIZE(e_in0)=',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF (TOF EQ 0) THEN BEGIN
		w_in = w_in0
		e_in = e_in0
	ENDIF ELSE BEGIN
		w_in = REFORM(w_in0,nchannels,nspectra,nphases)
		e_in = REFORM(e_in0,nchannels,nspectra,nphases)
	ENDELSE

	IF iprint GT 0 THEN BEGIN
		PRINT,'TOF       =',TOF
		PRINT,'nspectra  =',nspectra
		PRINT,'nphases   =',nphases
		PRINT,'nchannels =',nchannels
		PRINT,'nruns     =',nruns
	ENDIF
        IF (N_ELEMENTS(roh) EQ 0) OR (N_ELEMENTS(muR) EQ 0) OR (N_ELEMENTS(sigmaa) EQ 0) OR $
	   (N_ELEMENTS(sigmanuc) EQ 0) OR (N_ELEMENTS(sigmasi) EQ 0) THEN BEGIN
		PRINT,'Isotropic_ms: Error -  All parameters must be specified'
		GOTO, finished
	ENDIF
	IF N_ELEMENTS(sigmamag) EQ 0 THEN BEGIN 
		PRINT,'Isotropic ms: Assuming zero magnetic cross-section'
		sigmamag=0.0
	ENDIF
	IF (muR LT 0.1) OR (muR GT 0.9) THEN BEGIN
		PRINT,'muR= ',muR,'  No value of delta tabulted for muR= ', muR
		GOTO, finished
	ENDIF 

	IF iprint GT 0 THEN PRINT,'End of "w_in dimensions check etc." section'

;----------------------------------------------------------------------------
; Open blech.dat and read appeopriate delta

	line=''
	delarray = FLTARR(10,18)
        ba_file=FILEPATH('blech.dat',ROOT_DIR=lamp_macro+'/D7')

	IF (iprint GT 0) THEN PRINT,'Opening blech.dat'
	OPENR, 1, ba_file, ERROR=err
	IF (err NE 0) THEN PRINT, !ERR_STRING
	FOR i = 1,5 DO READF, 1, line
	READF, 1, delarray
	CLOSE, 1
	
	del=FLTARR(1)
        FOR i=0,16 DO BEGIN
		IF (delarray(0,i) LE roh) AND (delarray(0,i+1) GT roh) THEN BEGIN
			IF(muR EQ 0.9) THEN j=FLOOR(muR*10.)-1 ELSE j=FLOOR(muR*10.)
			del1=FLTARR(2)
			delx1=[0.1*j,(0.1*j)+0.1]
			del1(0)=INTERPOL(delarray(j,*),delarray(0,*),roh)
			del1(1)=INTERPOL(delarray(j+1,*),delarray(0,*),roh)	
			del=INTERPOL(del1,delx1,muR)
		ENDIF ELSE BEGIN
			IF((i EQ 16 AND del(0) EQ 0) OR (i EQ 16 AND del(0) EQ -1.)) THEN BEGIN
				PRINT,' Isotropic_ms: no value of Delta tabulated for:'
				PRINT,' R/h =',roh
				PRINT,' muR =',mur
				GOTO, finished
			ENDIF
		ENDELSE
	ENDFOR
	PRINT, 'Isotropic_ms: Delta from B-A table =', del

	IF (iprint GT 0) THEN PRINT,'End of "read from Blech-Averbach Table" section'
;------------------------------------------------------------------------------
; Do multiple scattering correction

        zeroed=WHERE(e_in(*,0,0) LT -0.9)
	irun=INDGEN(nruns)
	iphase=INDGEN(nphases)

	CASE nphases OF

	1: BEGIN
	   PRINT, 'Multiple scattering correction for unpolarized neutrons'

	   IF (TOF EQ 0) THEN BEGIN
		sigmas=sigmanuc+simgasi+sigmamag
		IF (iprint GT 0) THEN PRINT, 'SigmaS =', sigmas, ' barns/atom'

		capdelta = del(0)*(sigmas/(sigmas+sigmaa))
		w_out = w_in*(1 - capdelta)
		e_out = e_in*(1 - capdelta)

	   ENDIF ELSE BEGIN
		PRINT, 'Isotropic_ms: TOF option not yet implemented'
	   ENDELSE			
	   END
	2: BEGIN
	   PRINT, 'Multiple scattering correction for z-polarization analysis'

	   IF (TOF EQ 0) THEN BEGIN
		sigmansf = sigmanuc + 0.3333*sigmasi + 0.5*sigmamag
		sigmasf  = 0.66667*sigmasi + 0.5*sigmamag
		sigmas   = sigmansf + sigmasf
		IF (iprint GT 0) THEN BEGIN
			PRINT, 'SigmaS   =', sigmas, ' barns/atom'
			PRINT, 'SigmaNSF =', sigmansf, ' barns/atom'
			PRINT, 'SigmaSF  =', sigmasf, ' barns/atom'
		ENDIF

		alpha  = del(0)*(sigmasf/(sigmas + sigmaa))
		beta   = del(0)*(sigmansf/(sigmas + sigmaa))
		w_out=w_in & e_out=e_in
		inv=REVERSE(iphase)
		w_out(*,iphase,irun) = (1-beta)*w_in(*,iphase,irun)-alpha*w_in(*,inv,irun)
		e_out(*,iphase,irun) = SQRT(((1-beta)*e_in(*,iphase,irun))^2+(alpha*e_in(*,inv,irun))^2)

	   ENDIF ELSE BEGIN
		PRINT, 'Isotropic_ms: TOF option not yet implemented'
	   ENDELSE			
	   END
	6: BEGIN
	   PRINT, 'Multiple scattering correction for xyz-polarization analysis'

	   IF (TOF EQ 0) THEN BEGIN

; these are the Schaerpf equations, averaged over alpha between 0 and pi/2
; so that isotropic approximation may be used.

		sigma = FLTARR(nphases)
		sigma(0) = sigmanuc + 0.3333*sigmasi + 0.5*sigmamag
		sigma(1)  = 0.66667*sigmasi + 0.5*sigmamag
		sigma(2) = sigmanuc + 0.3333*sigmasi + 0.25*sigmamag
		sigma(3)  = 0.66667*sigmasi + 0.75*sigmamag
		sigma(4) = sigmanuc + 0.3333*sigmasi + 0.25*sigmamag
		sigma(5)  = 0.66667*sigmasi + 0.75*sigmamag

		IF (iprint GT 0) THEN BEGIN
			PRINT, 'SigmaZNSF =', sigma(0), ' barns/atom'
			PRINT, 'SigmaZSF  =', sigma(1), ' barns/atom'
			PRINT, 'SigmaXNSF =', sigma(2), ' barns/atom'
			PRINT, 'SigmaXSF  =', sigma(3), ' barns/atom'
			PRINT, 'SigmaYNSF =', sigma(4), ' barns/atom'
			PRINT, 'SigmaYSF  =', sigma(5), ' barns/atom'
		ENDIF
		alp = del(0)*(sigma/(sigma(0)+sigma(1)+sigmaa))
		alpha = w_in
		FOR i = 0,5 DO alpha(*,i,*) = alp(i)

		w_out=w_in & e_out=e_in
		inv=[1,0,3,2,5,4]
		a = [0,0,2,2,4,4]
		b = [1,1,3,3,5,5]
		w_out(*,iphase,irun) = (1-alpha(*,a,irun))*w_in(*,iphase,irun)-alpha(*,b,irun)*w_in(*,inv,irun)
		e_out(*,iphase,irun) = SQRT(((1-alpha(*,a,irun))*e_in(*,iphase,irun))^2+(alpha(*,b,irun)*e_in(*,inv,irun))^2)

	   ENDIF ELSE BEGIN
		PRINT, 'Isotropic_ms: TOF option not yet implemented'
	   ENDELSE
	   END
	ENDCASE

	IF (iprint GT 0) THEN BEGIN
		PRINT,''
		PRINT,'Output for comparison with MSCATT'
		R=w_out/w_in
		angle=x_in
		tarr=FLTARR(nspectra,nphases,nruns)
		a=1.7133			; a1
		b=-0.0368			; b1
		c=-0.0927			; a2
		d=-0.375			; b2
		x_hew=angle*!pi/180.		;convert deg to rad
		t0=EXP(-(a+b*sin(0./2)^2)*muR-(c+d*sin(0./2)^2)*muR^2)
		PRINT,'Transmission of Straight through beam =',t0,'%'
		FOR ispec = 0,nspectra-1 DO BEGIN
			tarr(ispec,*,*)=EXP(-(a+b*sin(x_hew(ispec)/2)^2)*muR-(c+d*sin(x_hew(ispec)/2)^2)*muR^2)
		ENDFOR
		R=R/tarr
		FOR irun=0, nruns-1 DO BEGIN
			CASE nphases OF
			1: BEGIN
			   PRINT,'    ispec      angle       R'
			   FOR i=0,31 DO PRINT, i+1, angle(i,irun), R(i,*,irun)
			   END
			2: BEGIN
			   PRINT,'    ispec      angle       Rnf          Rfl'
			   FOR i=0,31 DO PRINT, 2*(i+1), angle(i,irun), R(i,0,irun), R(i,1,irun)
			   END
			6: BEGIN			
			   PRINT,'    ispec      angle       R(X)nf       R(X)fl   ', $
				 '    R(Y)nf       R(Y)fl       R(Z)nf       R(Z)fl'
			   FOR i=0,31 DO PRINT, 2*(i+1), angle(i,irun), R(i,2,irun),R(i,3,irun),$
					R(i,4,irun),R(i,5,irun),R(i,0,irun),R(i,1,irun)
			   END
			ENDCASE
		ENDFOR
	ENDIF
				
;-------------------------------------------------------------------------------
;rezero the zeroed detectors

	IF (N_ELEMENTS(zeroed) GT 1) THEN BEGIN
		irun = INDGEN(nruns)
		e_out(zeroed,*,irun)=-1.
		w_out(zeroed,*,irun)=0.
	ENDIF

;----------------------------------------------------------------------------------------
; Return Paramters and exit
	
	datp.e = e_out
	param = [roh,muR,sigmaa,sigmanuc,sigmasi,sigmamag]
	parstr = STRTRIM(STRING(param),2)
	pos00 = STRPOS(parstr,'00')
	FOR i=0, 5 DO BEGIN
		IF(pos00(i) NE -1) THEN parstr(i) = STRMID(parstr(i),0,pos00(i))
		last = STRLEN(parstr(i))
		IF(STRPOS(parstr(i),'.') EQ last - 1) THEN parstr(i) = parstr(i)+'0'
		IF(STRPOS(parstr(i),'0') EQ last - 1) THEN parstr(i) = STRMID(parstr(i),0,last-1)
	ENDFOR

	rohs = 'roh='+parstr(0)
	muRs = 'muR='+parstr(1)
	sigmaas = 'abs='+parstr(2)
	sigmanucs = 'nuc='+parstr(3)
	sigmasis = 'si='+parstr(4)
	sigmamags = 'mag='+parstr(5)
	datp.other_tit=datp.other_tit+' -im('+rohs $
		+','+muRs+','+sigmaas+','+sigmanucs $
		+','+sigmasis+','+sigmamags+')'
finished:
	IF (iprint GT 0) THEN PRINT,'End Isotropic_ms:'

	give_datp, datp

	RETURN, w_out
	END
