	FUNCTION s_concatenate, w1, w2

;For SANS (D11,D22) data only
;
;concatenates two workspaces. Must be performed on raw data, i.e.
;before calling s_normalise.pro
;
; e.g. w3=concatenate(w1,w2)
;	w1(nspectra,nspectra,nrun1)
;	w2(nspectra,nspectra,nrun2) -> w3(nspectra,nspectra,nrun1+nrun2)
;
;						KHA,JRS 3/3/00

	COMMON c_lamp_access, inst

	IF (inst NE 'D11' AND inst NE 'D22') THEN BEGIN
		PRINT,'concatenate: Error - instrument must be D11 or D22'
		GOTO, finished
	ENDIF

	iprint=0

	take_datp, dat1
	take_datp, dat2, /third

;-------------------------------------------------------------------------------
; Check dimensions section

	x1=dat1.x	& x2=dat2.x
	y1=dat1.y	& y2=dat2.y
	n1=dat1.n	& n2=dat2.n	
	p1=dat1.p	& p2=dat2.p	
	pv1=dat1.pv	& pv2=dat2.pv
	e_in1=dat1.e	& e_in2=dat2.e

	sn1=SIZE(n1)
	sn2=SIZE(n2)
	IF (sn1(0) EQ 1) THEN n1=FLTARR(1)+n1(0)
	IF (sn2(0) EQ 1) THEN n2=FLTARR(1)+n2(0)

	s1=SIZE(w1)	& s2=SIZE(w2)
	IF (iprint GT 0) THEN PRINT,'size(w1)=',s1
	IF (iprint GT 0) THEN PRINT,'size(w2)=',s2
	nspectra1=s1(1)	& nspectra2=s2(1)
	IF (s1(0) EQ 3) THEN nruns1=s1(3) ELSE nruns1=1
	IF (s2(0) EQ 3) THEN nruns2=s2(3) ELSE nruns2=1

	IF (nspectra1 NE nspectra2) THEN BEGIN
		PRINT,'Concatenate: Error in par dimensions:'
		PRINT,'par(w1)=',p1
		PRINT,'par(w2)=',p2
		GOTO, finished
	ENDIF

	IF (N_ELEMENTS(e_in1) GT 1 OR N_ELEMENTS(e_in2) GT 1) THEN BEGIN
		PRINT,'Concatenate: Error - data must be raw, i.e. not normalised'
		GOTO, finished
	ENDIF

	nspectra=nspectra1

	IF (iprint GT 0) THEN PRINT,'nruns1=',nruns1,' nruns2=',nruns2

	IF (iprint GT 0) THEN PRINT,'s_concatenate: End of "check dimensions" section'

;-------------------------------------------------------------------------------
;Perform Concatenation

	IF (nruns1 EQ 1) THEN BEGIN
		IF (nruns2 EQ 1) THEN BEGIN
			pv_out=[[p1],[p2]]
		ENDIF ELSE BEGIN
			pv_out=[[p1],[pv2]]
		ENDELSE
	ENDIF ELSE BEGIN
		IF (nruns2 EQ 1) THEN BEGIN
			pv_out=[[pv1],[p2]]
		ENDIF ELSE BEGIN
			pv_out=[[pv1],[pv2]]
		ENDELSE
	ENDELSE

	n_out=[[n1],[n2]]
	w_out=[[[w1]],[[w2]]]

	nruns=nruns1+nruns2
	datp=dat1

	z_out=FLTARR(nruns)
	IF (iprint GT 0) THEN PRINT,'s_concatenate: End of "concatenation" section'

;-------------------------------------------------------------------------------
; Return data

	mod_datp, datp, "z", z_out
	mod_datp, datp, "n", n_out
	mod_datp, datp, "pv", pv_out

	s=dat1.other_tit & i=RSTRPOS(s,' ') & n=STRLEN(s)
	runs1=STRMID(s,i+1,n-1)

	s=dat2.other_tit & i=RSTRPOS(s,' ') & n=STRLEN(s)
        runs2=STRMID(s,3,i-1)

	IF (nruns1 EQ 1) THEN BEGIN
		IF (STRPOS(runs1,'>') EQ -1) THEN runs1=STRTRIM(STRING(LONG(p1(26))),2)
		r1='1 run'
	ENDIF ELSE BEGIN
		IF (STRPOS(runs1,'>') EQ -1 AND STRPOS(runs1,',') EQ -1) $
		THEN runs1=STRTRIM(STRING(LONG(pv1(26,0))),2)+':'+STRTRIM(STRING(LONG(pv1(26,nruns1-1))),2)
		r1=STRTRIM(STRING(nruns1),2)+' runs'
	ENDELSE
	IF (nruns2 EQ 1) THEN BEGIN
		IF (STRPOS(runs2,'>') EQ -1) THEN runs2=STRTRIM(STRING(LONG(p2(26))),2)
		r2='1 run'
	ENDIF ELSE BEGIN
		IF (STRPOS(runs2,'>') EQ -1 AND STRPOS(runs2,',') EQ -1) $
		THEN runs2=STRTRIM(STRING(LONG(pv2(26,0))),2)+':'+STRTRIM(STRING(LONG(pv2(26,nruns2-1))),2)
		r2=STRTRIM(STRING(nruns2),2)+' runs'
	ENDELSE

	PRINT,'concatenate: '+r1+' and '+r2+' concatenated to give '+STRTRIM(STRING(nruns),2)+' runs'
	s=datp.other_tit & i=STRPOS(s,' ') & n=STRPOS(s,'Start:') & datp.w_tit=STRTRIM(STRMID(s,i+1,n-4),2)	
	datp.other_tit=inst+'  #'+runs1+','+runs2+' -cc'

	give_datp, datp

finished:
	RETURN, w_out
	END
