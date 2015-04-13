;*******************************************************************************
;
	FUNCTION s_normalise, w_in0, inorm
;
; Normalises raw sans data to monitor or counting time, depending on value of
; inorm. 
; Also finds the position of the elastic peak. ikeep not used. 
;
; inorm=0 => no normalisation
; inorm=1 => 1000 monitor counts (default)
; inorm=2 => time in minutes (incorrect for data added with rdsum or '>')
;
;Output format:
;  		  w_out(xarray,yarray,nruns)
;
;
;							JRS 30/3/00
;
;-------------------------------------------------------------------------------


	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start normalise:'

	COMMON c_lamp_access, inst

	take_datp, datp

	IF (N_ELEMENTS(inorm) EQ 0) THEN inorm=1

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	m_in0=datp.n
	x_in0=datp.x	
	y_in0=datp.y	
	z_in0=datp.z	
	e_in0=datp.e
	par=datp.p
	parv=datp.pv
	se=SIZE(e_in0)
	sw=SIZE(w_in0)
	sm=SIZE(m_in0)
	IF (sm(0) EQ 1) THEN m_in0=FLTARR(1)+m_in0(0)

	FOR i=0,se(0) DO IF (se(i) NE sw(i)) THEN GOTO, seterr
	GOTO, noseterr
seterr:
	IF (iprint GT 0) THEN PRINT,'Normalise: No error bars defined for w_in. Use sqrt'
	e_in0=SQRT(w_in0)
noseterr:

	IF (sw(0) EQ 0) THEN BEGIN
		PRINT,'normalise_d7: Error - workspace is empty'
		GOTO, finished
	ENDIF

	IF (sw(0) EQ 3) THEN nruns=sw(3) ELSE nruns=1
	IF (nruns EQ 1) THEN parv=par
	nspectra=sw(1)

	IF (iprint GT 0) THEN BEGIN
		PRINT,'nspectra=',nspectra
		PRINT,'nruns=',nruns
		PRINT,'inorm=',inorm
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of Check Dimensions Section:'

;-------------------------------------------------------------------------------
; Perform Normalisation

	w_buf=FLTARR(nspectra,nspectra,nruns)
	e_buf=FLTARR(nspectra,nspectra,nruns)

	IF (inorm EQ 1) THEN BEGIN
		FOR irun=0,nruns-1 DO BEGIN
			PRINT,'Normalising run #',FIX(parv(26,irun)),' to ',m_in0(irun),' monitor counts'
			w_buf(*,*,irun)=w_in0(*,*,irun)*1000./m_in0(irun)
			e_buf(*,*,irun)=e_in0(*,*,irun)*1000./m_in0(irun)
		ENDFOR
	ENDIF ELSE IF (inorm EQ 2) THEN BEGIN
		FOR irun=0,nruns-1 DO BEGIN
			PRINT,'Normalising run #',FIX(parv(26,irun)),' to time'
			w_buf(*,*,irun)=w_in0(*,*,irun)/parv(2,irun)
			e_buf(*,*,irun)=e_in0(*,*,irun)/parv(2,irun)
		ENDFOR
	ENDIF ELSE BEGIN
		FOR irun=0,nruns-1 DO BEGIN
			PRINT,'Not normalising run #',FIX(parv(26,irun))
			w_buf(*,*,irun)=w_in0(*,*,irun)
			e_buf(*,*,irun)=e_in0(*,*,irun)
		ENDFOR
	ENDELSE
	
	w_out=FLTARR(nspectra,nspectra,nruns) & w_out=w_buf
	e_out=FLTARR(nspectra,nspectra,nruns) & e_out=e_buf

	IF (iprint GT 0) THEN PRINT,'End of perform normalisation section:'

;-------------------------------------------------------------------------------
;Return data

	s=datp.other_tit & i=RSTRPOS(s,' ') & n=STRLEN(s) & numor=STRMID(s,i+1,n-1)
        IF (nruns EQ 1 AND STRPOS(numor,'>') EQ -1 AND STRPOS(numor,'+') EQ -1) $
		THEN numor=STRTRIM(STRING(LONG(par(26))),2)
	IF (nruns NE 1 AND STRPOS(numor,'>') EQ -1 AND STRPOS(numor,',') EQ -1) $
		THEN numor=STRTRIM(STRING(LONG(parv(26,0))),2)+':'+STRTRIM(STRING(LONG(parv(26,nruns-1))),2)
	
	normnum=STRTRIM(STRING(inorm),2)

	IF (STRPOS(s,' -cc') EQ -1) THEN BEGIN 	; data not previously concatenated
		s=datp.other_tit & i=STRPOS(s,' ') & n=STRPOS(s,'Start:') & datp.w_tit=STRTRIM(STRMID(s,i+1,n-6),2)	
		datp.other_tit=inst+'  #'+numor+' -sn('+normnum+')' 
	ENDIF ELSE datp.other_tit=datp.other_tit+' -sn('+normnum+')'

finished:

	IF (iprint GT 0) THEN PRINT,'SANS Normalise finished'

	mod_datp, datp, "e", e_out
	give_datp, datp

	RETURN, w_out
	END
