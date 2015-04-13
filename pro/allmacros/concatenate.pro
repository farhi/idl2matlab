;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION concatenate, w1, w2

;For D7 data only
;
;concatenates two workspaces. Must be performed on raw data, i.e.
;before calling normalise.pro
;
;DIMENSIONS
; any - but first n-1 out of n dimensions must be equal in w1 and w2
; eg 	w1(nspectra,nphases,nrun1)
;	w2(nspectra,nphases,nrun2) -> w3(nspectra,nphases,nrun1+nrun2)
;
;COMMAND SYNTAX:
; w3=concatenate(w1,w2)
;
;						KHA,JRS  21/5/02
;------------------------------------------------------------------------------
;******************************************************************************

	COMMON c_lamp_access, inst

	IF (inst NE 'D7') THEN BEGIN
		PRINT,'concatenate: Error - instrument must be D7'
		GOTO, finished
	ENDIF

	iprint = 0

;------------------------------------------------------------------------------
; Inspect and prepare input workspaces

	take_datp, dat1
	take_datp, dat2, /third

	x1=dat1.x	& x2=dat2.x
	y1=dat1.y	& y2=dat2.y
	z1=dat1.z	& z2=dat2.z
	n1=dat1.n	& n2=dat2.n	
	p1=dat1.p	& p2=dat2.p	
	pv1=dat1.pv	& pv2=dat2.pv
	e_in1=dat1.e	& e_in2=dat2.e

	nspectra1=FIX(p1[1])	& nspectra2=FIX(p2[1])
	nphases1=FIX(p1[2])	& nphases2=FIX(p2[2])
	nruns1=FIX(p1[3])	& nruns2=FIX(p2[3])
	TOF1=FIX(p1[8])		& TOF2=FIX(p2[8])

	IF (nspectra1 NE nspectra2) OR $
	   (nphases1 NE nphases2) THEN BEGIN
		PRINT,'Concatenate: Error in dimensions:'
		PRINT,'par(w1)=',p1
		PRINT,'par(w2)=',p2
		GOTO, finished
	ENDIF

	IF (nspectra1 NE 64 AND TOF1 EQ 0) OR (N_ELEMENTS(e_in1) GT 1) $
	OR (nspectra2 NE 64 AND TOF2 EQ 0) OR (N_ELEMENTS(e_in2) GT 1) $
	OR (nspectra1 NE 66 AND TOF1 EQ 1) OR (nspectra2 NE 66 AND TOF2 EQ 1) THEN BEGIN
		PRINT,'Concatenate: Error - data must be raw, i.e. not normalised'
		GOTO, finished
	ENDIF

	nspectra = nspectra1
	nphases  = nphases1
	tof      = tof1
	IF tof EQ 1 THEN nphases=1

	s1 = size(w1)	& s2 = size(w2)
	IF iprint GT 0 THEN PRINT,'size(w1) = ',s1
	IF iprint GT 0 THEN PRINT,'size(w2) = ',s2

	IF (nphases1 EQ 1) AND (s1[0] EQ 2) THEN nruns1 = s1[2] $
		ELSE IF s1[0] EQ 3 THEN nruns1 = s1[3] $
		ELSE nruns1 = 1
	IF (nphases2 EQ 1) AND (s2[0] EQ 2) THEN nruns2 = s2[2] $
		ELSE IF s2[0] EQ 3 THEN nruns2 = s2[3] $
		ELSE nruns2 = 1
	IF tof EQ 1 THEN BEGIN
		IF s1[0] EQ 2 THEN nruns1 = 1 ELSE nruns1 = s1[3]
		IF s2[0] EQ 2 THEN nruns2 = 1 ELSE nruns2 = s2[3]
	ENDIF

	IF iprint GT 0 THEN PRINT,'nruns1=',nruns1,'nruns2=',nruns2
	IF iprint GT 0 THEN PRINT,'Concatenate: End of "check arguments and dimensions" section'

;------------------------------------------------------------------------------
; Perform concatenation

	IF nruns1 EQ 1 THEN BEGIN
		IF nruns2 EQ 1 THEN BEGIN
			IF (tof EQ 0) THEN z_out=[[x1],[x2]] ELSE z_out=[[y1],[y2]]
			pv_out=[[p1],[p2]]
		ENDIF ELSE BEGIN
			IF tof EQ 0 THEN z_out=[[x1],[z2]] ELSE z_out=[[y1],[z2]]
			pv_out=[[p1],[pv2]]
		ENDELSE
	ENDIF ELSE BEGIN
		IF nruns2 EQ 1 THEN BEGIN
			IF (tof EQ 0) THEN z_out=[[z1],[[x2]]] ELSE z_out=[[z1],[y2]]
			pv_out=[[pv1],[p2]]
		ENDIF ELSE BEGIN
			z_out=[[z1],[z2]]
			pv_out=[[pv1],[pv2]]
		ENDELSE
	ENDELSE

	n_out=[[[n1]],[[n2]]]
	IF (tof EQ 0) AND (nphases EQ 1) THEN w_out = [[w1],[w2]] $
					 ELSE w_out = [[[w1]],[[w2]]]

	nruns = nruns1 + nruns2
	datp = dat1
	datp.p(3) = FLOAT(nruns)

	IF (tof EQ 0) AND (nphases EQ 1) THEN BEGIN
		y_out = REFORM(FIX(pv_out(0,*)),nruns)
		mod_datp, datp, "y", y_out
	ENDIF
	IF iprint GT 0 THEN PRINT, 'Concatenate: End of concatenation section'

;-------------------------------------------------------------------------------
; Return data and parameters

	mod_datp, datp, "z", z_out
	mod_datp, datp, "n", n_out
	mod_datp, datp, "pv", pv_out

	IF nruns1 EQ 1 THEN BEGIN
		runs1 = STRTRIM(STRING(LONG(p1[0])),2)
		r1='1 run'
	ENDIF ELSE BEGIN
		runs1 = STRTRIM(STRING(LONG(pv1[0,0])),2)+':'+STRTRIM(STRING(LONG(pv1[0,nruns1-1])),2)
		r1=STRTRIM(STRING(nruns1),2)+' runs'
	ENDELSE
	IF (nruns2 EQ 1) THEN BEGIN
		runs2 = STRTRIM(STRING(LONG(p2[0])),2)
		r2='1 run'
	ENDIF ELSE BEGIN
		runs2 = STRTRIM(STRING(LONG(pv2[0,0])),2)+':'+STRTRIM(STRING(LONG(pv2[0,nruns2-1])),2)
		r2=STRTRIM(STRING(nruns2),2)+' runs'
	ENDELSE

	PRINT,'Concatenate: '+r1+' and '+r2+' concatenated to give '+STRTRIM(STRING(nruns),2)+' runs'
	datp.other_tit='D7 #'+runs1+','+runs2+' -cc'

	give_datp, datp

finished:
	RETURN, w_out
	END
