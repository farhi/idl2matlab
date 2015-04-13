;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION background, sample = sam, empty = emp, cadmium = cdm, $
		 T = T, muR = muR, MS_file = MC_file0

; For D7 data only:
; Performs background subtraction of normalised data.
;
;ARGUMENTS:
; sample	: workspace containing sample runs (required)
; empty		: workspace containing empty runs (required)
; cadmium	: workspace contaoning of cadmium runs (optional)
; T		: sample transmission (default: T = 1)
; muR		: muR parameter required for Hewat-type angular transmission
;		  correction
; MS_file	: Output file from MSCATT for monte-carlo correction
;
; N.B. Specify either T for a normal background subtraction or muR for
;      an angular dependent background subtraction - NOT BOTH
;
;DIMENSIONS:
;  TOF:	w_in = w_out(nchannels,nspectra*nphases) - only deals with 1 run
;NOTOF:	w_in = w_out(nspectra,nphases,nruns)     - unless nruns = 1
;
;COMMAND SYNTAX:
; w4 = background(sample = w1,empty = w2[,cadmium = w3][,T = <T>]
;                     [,muR = <muR>][,MS_file = <filename>])
;
; (optional keywords/arguments shown in square brackets)
; 
;							JRS 21/5/02
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint = 0			;iprint = 1 -> show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start background:'

	IF(N_ELEMENTS(MC_file0) GT 0) THEN mscatt = 1 ELSE mscatt = 0
			

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces and setup output workspace

	IF (N_ELEMENTS(sam) EQ 0 OR N_ELEMENTS(emp) EQ 0) THEN BEGIN
		PRINT,'Background: ERROR - Both sample and background wkspace' $
		     +' numbers must be included'
		GOTO, finished
	ENDIF
	
	w_in1 = sam 
	take_datp, dat1
	w_in2 = emp
	take_datp, dat2, /third	
	IF(N_ELEMENTS(cdm) LT 2) THEN BEGIN
		cad = 0 
		w_in3 = 0.*w_in2
	ENDIF ELSE BEGIN
		cad  =  1
		w_in3  =  cdm
		take_datp, dat3, /fourth
	ENDELSE
	
; check dimensions of input arrays
	
	sw1 = SIZE(w_in1)
	sw2 = SIZE(w_in2)
	sw3 = SIZE(w_in3)
	e_in1 = dat1.e
	e_in2 = dat2.e
	IF(cad GT 0) THEN e_in3 = dat3.e ELSE e_in3 = 0.*e_in2
	se1 = SIZE(e_in1)
	se2 = SIZE(e_in2)
	se3 = SIZE(e_in3)	
	IF((TOTAL(sw1-sw2) NE 0) OR (TOTAL(sw1-sw3) NE 0) OR $
	   (TOTAL(sw2-sw3) NE 0) OR (TOTAL(se1-sw3) NE 0) OR $
	   (TOTAL(se2-sw3) NE 0) OR (TOTAL(se3-sw3) NE 0))THEN BEGIN
		PRINT,'Background: ERROR - Input arrays must be the same size'
		GOTO, finished
	ENDIF
	
	par1 = dat1.p
	par2 = dat2.p
	IF (cad GT 0) THEN par3 = dat3.p
	parv2 = dat2.pv
	IF (cad GT 0) THEN parv3 = dat3.pv
	TOF       = FIX(par1(8))
	nchannels = FIX(par1(6))
	nspectra  = FIX(par1(1))
	nphases   = FIX(par1(2))
	nruns     = FIX(par1(3))

; put all workspaces into 4d array
	
	w_in = FLTARR(3,nspectra,nphases,nruns) & e_in = w_in
	IF TOF EQ 1 THEN BEGIN
		w_in = FLTARR(3,nchannels,nspectra*nphases,nruns)
		e_in  =  w_in
	ENDIF
	w_in(0,*,*,*)  =  w_in1 & e_in(0,*,*,*)  =  e_in1
	w_in(1,*,*,*)  =  w_in2 & e_in(1,*,*,*)  =  e_in2
	w_in(2,*,*,*)  =  w_in3 & e_in(2,*,*,*)  =  e_in3
	
	IF (TOF EQ 0 AND nphases EQ 1) THEN BEGIN
		w_in = reform(w_in,3,nspectra,nphases,nruns)
		e_in = reform(e_in,3,nspectra,nphases,nruns)
	ENDIF

	IF (nruns EQ 1) OR (N_ELEMENTS(dat1.z) LE 1) THEN $
		x_in = dat1.x ELSE x_in = dat1.z
	y_in = dat1.y	

	IF (iprint GT 0) THEN BEGIN
		PRINT,'TOF       = ',TOF
		PRINT,'Nchannels = ',nchannels
		PRINT,'Nspectra  = ',nspectra
		PRINT,'Nphases   = ',nphases
		PRINT,'Nruns     = ',nruns
	ENDIF
	
	IF (iprint GT 0)THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
; Perform Backgound Subtraction

	IF N_ELEMENTS(T) GT 0 AND (N_ELEMENTS(muR) GT 0 OR mscatt EQ 1) THEN BEGIN 
		PRINT,'Background: ERROR - specify only one of T, muR or'
		PRINT,'		   an MSCATT correction file.'
		GOTO, finished
	ENDIF
	IF N_ELEMENTS(muR) GT 0 AND (N_ELEMENTS(T) GT 0 OR mscatt EQ 1) THEN BEGIN 
		PRINT,'Background: ERROR - specify only one of T, muR or'
		PRINT,'		   an MSCATT correction file.'
		GOTO, finished
	ENDIF
	IF N_ELEMENTS(muR) GT 0 THEN BEGIN 
		correct  =  1
		PRINT,'Background: muR = ',muR,' (Hewat)'
	ENDIF
	IF N_ELEMENTS(T) EQ 0 AND N_ELEMENTS(muR) EQ 0 AND mscatt EQ 0 THEN BEGIN
		muR = 1
		correct  =  0
		PRINT,'Background: T = 1 (simple)'
	ENDIF
	IF N_ELEMENTS(T) GT 0 THEN BEGIN 
		muR = T
		correct  =  0
		PRINT,'Background: T = ',muR,' (simple)'
	ENDIF
	IF (mscatt EQ 1) THEN correct  =  2	

        tarr = 0.*w_in1
	IF (TOF EQ 0) AND(nphases EQ 1) THEN tarr = REFORM(tarr, nspectra, 1, nruns)

	CASE correct OF
	0:BEGIN
		muR = FLOAT(muR)
		tarr = tarr+muR
	  END

	1:BEGIN
		muR = FLOAT(muR)
		a = 1.7133			; a1
		b = -0.0368			; b1
		c = -0.0927			; a2
		d = -0.375			; b2
		IF (TOF EQ 0) THEN x_hew = x_in*!pi/180. ELSE x_hew = y_in*!pi/180.
		FOR i = 0,(nspectra*(TOF*nphases + 1 - TOF) -1) DO BEGIN
			IF TOF EQ 1 THEN $
			tarr(*,i,*) = exp(-(a+b*sin(x_hew(i)/2)^2)*muR- $
				           (c+d*sin(x_hew(i)/2)^2)*muR^2) $
			ELSE $
			tarr(i,*,*) = exp(-(a+b*sin(x_hew(i)/2)^2)*muR- $
				           (c+d*sin(x_hew(i)/2)^2)*muR^2)
		ENDFOR
	END

; TOF MSCATT option is not yet implemented

2:	BEGIN	
		nMC = 1
		IF N_ELEMENTS(MC_file0) EQ 1 THEN BEGIN
			corr_MC = 1
			MC_file = STRARR(nMC)
			MC_file(0) = MC_file0
			PRINT,'Background: correction with mscatt file:'
			PRINT,'          ',MC_file(0)
		ENDIF ELSE BEGIN
			corr_MC = 1
			nMC = N_ELEMENTS(MC_file0)
			MC_file = MC_file0
			PRINT,'Background: correction with mscatt files:'
			FOR i = 0,nMC-1 DO PRINT,'          ',MC_file(i)
		ENDELSE
		tarr = FLTARR(nspectra,nphases,nruns)
		IF nphases EQ 2 THEN MC = FLTARR(4,32) ELSE MC = FLTARR(8,32)
		FOR irun = 0,nMC-1 DO BEGIN
			IF iprint GT 0 THEN PRINT,'Opening MC file: ',MC_file(irun)
			OPENR, 1, MC_file(irun), ERROR = err
			IF (err NE 0) THEN BEGIN
				PRINT, !ERR_STRING
				GOTO, finished
			ENDIF
			found = -1
			line = ''
			IF(nphases EQ 6) THEN look = 'R(X)nf' ELSE look = 'Rnf'
			WHILE (found EQ -1) DO BEGIN
				READF, 1, line
				found = STRPOS(line,look)
			ENDWHILE
			READF, 1, MC
			CLOSE, 1
			FOR i = 0,nphases-1 DO tarr(*,i,irun) = 1./MC(i+2,*)
		ENDFOR
		IF nruns GT 1 THEN BEGIN
			IF nMC EQ 1 THEN $
			FOR irun = 1,nruns-1 DO tarr[*,*,irun] = tarr[*,*,0] $
			ELSE IF (nruns GT 2 AND nMC EQ 2) THEN BEGIN	
				FOR irun = nruns-1,1,-1 DO $
				MC0[*,*,irun] = tarr[*,*,0] + (tarr[*,*,1] - $
				tarr[*,*,0])*FLOAT(irun)/FLOAT(nruns-1)
			ENDIF
		ENDIF
		IF iprint GT 0 THEN BEGIN
			PRINT,'MS_Array  =  :'
			PRINT,1./tarr
		ENDIF
	END
	ENDCASE

	w_out  =  (1./tarr)*(w_in[0,*,*,*] - w_in[2,*,*,*]) - $
	                    (w_in[1,*,*,*] - w_in[2,*,*,*])
	e_out  =  SQRT((e_in[0,*,*,*]/tarr)^2 + $
	                e_in[1,*,*,*]^2+((1. - 1./tarr)*e_in[2,*,*,*])^2)

        IF iprint GT 0 THEN PRINT,'End of background subtraction section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	IF (TOF EQ 0) AND (nphases EQ 1) THEN BEGIN
		w_out0 = FLTARR(nspectra,nruns)	& e_out0 = w_out0
		w_out0[*,*] = w_out[*,0,*]
		e_out0[*,*] = e_out[*,0,*]
	ENDIF ELSE BEGIN
		w_out0 = w_out
		e_out0 = e_out
	ENDELSE
	
	dat1.e = e_out0

	s = dat2.other_tit & i = STRPOS(s,' ') & n = RSTRPOS(s,' ')
	bgnumor = STRMID(s,i+1,n-i-1)
	IF nruns NE 1 THEN $
	bgnumor = '#'+STRTRIM(STRING(LONG(parv2[0,0])),2)+':' $
	             +STRTRIM(STRING(LONG(parv2[0,nruns-1])),2)
        IF (nruns EQ 1) AND (STRPOS(bgnumor,'>') EQ -1) THEN $
	bgnumor = 'emp:#'+STRTRIM(STRING(LONG(par2[0])),2)	
	IF cad GT 0 THEN BEGIN
		s = dat3.other_tit & i = STRPOS(s,' ') & n = RSTRPOS(s,' ')
		cdnumor = STRMID(s,i+1,n-i-1)
        	IF (nruns EQ 1 AND STRPOS(cdnumor,'>') EQ -1) THEN $
		cdnumor = '#'+STRTRIM(STRING(LONG(par3[0])),2)
		IF nruns NE 1 THEN $
		cdnumor = 'cad:#'+STRTRIM(STRING(LONG(parv3[0,0])),2)+':' $
		                 +STRTRIM(STRING(LONG(parv3[0,nruns-1])),2)
	ENDIF ELSE BEGIN
		cdnumor = 'no cd'
	ENDELSE
	
	IF mscatt EQ 0 THEN BEGIN
		muRs = STRTRIM(STRING(muR),2)
muR0:		n = STRLEN(muRs)	& i = RSTRPOS(muRs,'0')
		IF i EQ n-1 THEN BEGIN
			muRs = STRMID(muRs,0,n-1)
			GOTO, muR0
		ENDIF
		mss = ''
		IF correct EQ 1 THEN hew  =  'muR=' ELSE hew  =  'T='
		muRs  =  hew + muRs
	ENDIF ELSE BEGIN
		muRs = ''
	 	mss = 'MS:' + MC_file0
	ENDELSE
		params = muRs+mss

	dat1.other_tit = dat1.other_tit+' -bg('+bgnumor+','+cdnumor+','+params+')'

finished:

	IF iprint GT 0 THEN PRINT,'End background:'
	give_datp, dat1
	RETURN, w_out0

	END
