;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION omega_scan, w_in0, all_angles=all_angles, pos_angles=pos_angles, $
		 neg_angles=neg_angles, ib

;For D7 data only
;
;Input: 2-D or 3-D workspace containing a non-tof omega-scan read in with 
;rdand, normalised with normalise.pro and optionally separated into components 
;with components.pro
;Rearranges to a 1-D or 2-D workspace with detector angle theta as x-axis and 
;omega angle as y-axis
;
;KEYWORDS:
; /neg_angles	: only use data with negative angles
; /pos_angles	: only use data with positive angles
; /all_angles	: use all banks (defaults
;
; (ib is obsolete, kept for backwards compatability)
;
;DIMENSIONS:
; input format: w(32_or_64,nphases,nruns)	- unless nphases or nruns is 1
; output:       w(nspectra,nruns,nphases)	- unless nphases is 1
;
;COMMAND SYNTAX
; w5=omega_scan(w4[,/all_angles][,/pos_angles][,/neg_angles])
;
; (optional keywords shown in square brackets)
;
;						KHA,JRS 27/6/02
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=1	; if iprint>0, show debugging messages

	IF iprint GT 0 THEN PRINT,'Start omega_scan:'

	take_datp, datp

	ibank = 2				;all angles -  default
	IF(N_ELEMENTS(ib) GT 0) THEN ibank=ib
	IF KEYWORD_SET(all_angles) THEN ibank=2
	IF KEYWORD_SET(pos_angles) THEN ibank=1
	IF KEYWORD_SET(neg_angles) THEN ibank=0

;-------------------------------------------------------------------------------
;Check arguments and dimensions of w_in

	par   = datp.p
	e_in0 = datp.e

	sw = SIZE(w_in0)
	se = SIZE(e_in0)
	IF se[0] EQ 0 THEN BEGIN
		PRINT,'omega_scan: Error - no error bars; normalise must be called first'
		GOTO, finished
	ENDIF

	nspectra = sw[1]
	nphases  = FIX(par[2])
	nruns    = FIX(par[3])
	IF nphases EQ 1 THEN BEGIN
		w_in = FLTARR(nspectra,1,nruns)	& e_in = w_in
		w_in[*,0,*] = w_in0[*,*]
		e_in[*,0,*] = e_in0[*,*]
	ENDIF ELSE BEGIN
		w_in = w_in0	& e_in = e_in0
	ENDELSE

	x_in = datp.x
	y_in = datp.y

	IF iprint GT 0 THEN PRINT,'End of "check arguments and dimensions" section'

;-------------------------------------------------------------------------------
;Set up parameters relating to ibank and output arrays


	istart = 0
	i = WHERE(x_in GT 0)
	IF ibank EQ 0 THEN nspectra = i[0] ELSE $
	IF ibank EQ 1 THEN BEGIN
		istart=i[0]
		nspectra=nspectra-i[0]
	ENDIF

	IF iprint GT 0 THEN PRINT,'istart=',istart,' nspectra=',nspectra
	IF iprint GT 0 THEN PRINT,'End of "ibank and iPA setup" section'

;-------------------------------------------------------------------------------
;Rearrange w_in into w_out sorted in order of ascending omega

	y_out = FLTARR(nruns)
	y_out[*] = datp.pv[20,*]
	IF y_out[0] EQ y_out[1] THEN BEGIN
		y_out[*] = datp.pv[21,*]
		ws = 'using Lower Sample Rotation'
	ENDIF ELSE ws = 'using Lower Sample Rotation'	
	
	iorder = SORT(y_out)
	w_out  = FLTARR(nspectra,nruns,nphases)	& e_out = w_out

	is1 = istart	& is2 = is1 + nspectra - 1

	x_out = x_in[is1:is2]
	y_out = y_out[iorder]
	iphase = INDGEN(nphases)
	irun = INDGEN(nruns)
	FOR irun = 0, nruns - 1 DO BEGIN
		w_out[*,irun,iphase] = w_in[is1:is2,iphase,iorder[irun]]
		e_out[*,irun,iphase] = e_in[is1:is2,iphase,iorder[irun]]
	ENDFOR

	IF iprint GT 0 THEN PRINT,'End of "rearrange w_in into w_buf" section'

;-------------------------------------------------------------------------------
;Exclude bad spectra and runs

	ibs = INDGEN(nspectra)
	w_tot = TOTAL(w_out[ibs,*,0],2)
	e_tot = TOTAL(e_out[ibs,*,0],2)
	igs = WHERE(w_tot GT 0. AND e_tot GE 0)
	ib  = WHERE(w_tot LE 0. OR e_tot LT 0., nbs)
print, igs, ib
	IF nbs GT 0 THEN BEGIN
		IF iprint GT 0 THEN PRINT,'There is/are '+ $
			STRTRIM(STRING(nbs),2)+' bad spectrum/spectra'
		IF iprint GT 0 THEN PRINT,'Excluding det. at phi ='+ $
			STRTRIM(STRING(x_out[ib]),2)
		x_out = x_out[igs]
		w_out = w_out[igs,*,*]
		e_out = e_out[igs,*,*]
	ENDIF
help, w_out
	ibr=INDGEN(nruns)
	w_tot = TOTAL(w_out[*,ibr,0],1)
	e_tot = TOTAL(e_out[*,ibr,0],1)
	igr = WHERE(w_tot GT 0. AND e_tot GE 0.)
	ib  = WHERE(w_tot LE 0. OR e_tot LT 0.,nbr)
	IF nbr GT 0 THEN BEGIN
		IF iprint GT 0 THEN PRINT,'There is/are '+ $
			STRTRIM(STRING(nbr),2)+' bad run/runs'
		IF iprint GT 0 THEN PRINT,'Excluding run. at omega ='+ $
			STRTRIM(STRING(y_out[ib]),2)
		y_out = y_out[igr]
		w_out = w_out[*,*,igr]
		e_out = e_out[*,*,igr]
	ENDIF
help, w_out
	nspectra = nspectra - nbs
	nruns = nruns - nbr

	datp.p[1]=FLOAT(nspectra)
	datp.p[3]=FLOAT(nruns)

	IF (iprint GT 0) THEN PRINT,'End of "remove zeroed spectra and runs" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "x", x_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	datp.x_tit = '2-theta'
	datp.y_tit = 'Omega'

	CASE ibank OF
	0: BEGIN
		s='negative angle detectors'
		os = '/neg'
	   END
	1: BEGIN
		s='positive angle detectors'
		os = '/pos'
	   END
	2: BEGIN
		s='all detectors'
		os = '/all'
           END
	ENDCASE

	PRINT,'Omega_scan: keep '+s+', '+ws

	datp.other_tit=datp.other_tit+' -os('+os+')'

finished:
	IF iprint GT 0 THEN PRINT,'End omega_scan:'

	give_datp, datp
help, w_out

	RETURN, w_out
	END
