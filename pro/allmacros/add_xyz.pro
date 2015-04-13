;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION add_xyz, w_in, zpo=zpo, xyz=xyz, nopo=nopo, ex

;For D7 data only. 
;
;adds the X, Y and Z measurements together. Must be performed on raw data, i.e.
;before calling normalise.pro
;
;KEYWORDS:
; /zpo: (default)
;	three non-flip measurements are added together to give one
;	three flip measurements are also added together. Dimensions:
;		w_in(nspectra,6,nruns) -> w_out(nspectra,2,nruns)
;
; /xyz:
;	after separately adding together the non-flip and flip measurements, 
;	the sums are then copied into the X and Y phases. Dimensions:
;		w_in(nspectra,6,nruns) -> w_out(nspectra,6,nruns)
;
; /nopo:
;	add all flip phases together to obtain a single measurement. Then copy 
;	sum into the Z1, X and Y phases. Dimensions:
;		w_in(nspectra,6,nruns) -> w_out(nspectra,6,nruns)
; (the argument ex is obsolete kept for backwards compatability)
;
;DIMENSIONS: (see above)
;
;COMMAND SYNTAX:
; w2=add_xyz(w1[,/zpo][,/xyz][,/nopo])
;
; (optional keywords shown in square brackets)
;							KHA,JRS 18/4/02
;------------------------------------------------------------------------------
;******************************************************************************


	COMMON c_lamp_access, inst

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start add_xyz:'

	take_datp, datp

	expand=0
	IF(N_ELEMENTS(ex) GT 0) THEN expand=ex
	IF KEYWORD_SET(zpo) THEN expand=0
	IF KEYWORD_SET(xyz) THEN expand=1
	IF KEYWORD_SET(nopo) THEN expand=2

;-------------------------------------------------------------------------------
;Check dimensions of input arrays

	IF (inst NE 'D7') THEN BEGIN
		PRINT,'add_xyz: Error - instrument must be D7'
		GOTO, finished
	ENDIF

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw

	par=datp.p
	nspectra=FIX(par(1))
	nphases=FIX(par(2))
	nruns=FIX(par(3))
	TOF=FIX(par(8))

	IF (N_ELEMENTS(expand) EQ 0) THEN expand=0

	IF (iprint GT 0) THEN BEGIN
		PRINT,'TOF=',TOF
		PRINT,'nspectra=',nspectra
		PRINT,'nphases=',nphases
		PRINT,'nruns=',nruns
		PRINT,'expand=',expand
	ENDIF

	e_in=datp.e
	n_in=datp.n
	y_in=datp.y

	errstring=''
	IF (TOF EQ 1) THEN errstring='data must be no-TOF'
	IF (nphases NE 6) THEN errstring='data must be XYZ'
	IF (expand NE 0 AND expand NE 1 AND expand NE 2) THEN errstring='expand must be 0,1 or 2'
	IF (nspectra NE 64 OR N_ELEMENTS(e_in) GT 1) THEN errstring='data must be raw, i.e. not normalised'
	IF (errstring NE '') THEN BEGIN
		PRINT,'Add_xyz: Error - ',errstring
		GOTO, finished
	ENDIF

	IF (sw(0) EQ 3) THEN nruns=sw(3)
		
;-------------------------------------------------------------------------------
;Perform addition

	IF (expand EQ 0) THEN w_out=FLTARR(nspectra,2,nruns) $
			ELSE w_out=w_in
	n_out=n_in

	IF (expand LE 1) THEN BEGIN
		w_out(*,0,*)=w_in(*,0,*)+w_in(*,2,*)+w_in(*,4,*)
		w_out(*,1,*)=w_in(*,1,*)+w_in(*,3,*)+w_in(*,5,*)
		n_out(*,1,*)=n_in(*,1,*)+n_in(*,3,*)+n_in(*,5,*)
		n_out(*,2,*)=n_in(*,2,*)+n_in(*,4,*)+n_in(*,6,*)
	ENDIF ELSE BEGIN
		w_out(*,0,*)=TOTAL(w_in,2)
		i = INTARR(5)
		w_out(*,1:5,*)=w_out(*,i,*)
		n_out(*,1,*)=TOTAL(n_in,2)
		n_out(*,2:6,*)=n_out(*,i+1,*)
	ENDELSE

	IF (expand EQ 0) THEN BEGIN
		n_out(*,3:6,*)=0.
		y_out=y_in(0:1)
		mod_datp, datp, "y", y_out
		datp.p(2)=2.
		IF (N_ELEMENTS(datp.pv) GT 1) THEN datp.pv(2,*)=2.
	ENDIF ELSE IF (expand EQ 1) THEN BEGIN
		w_out(*,2:3,*)=w_out(*,0:1,*)
		w_out(*,4:5,*)=w_out(*,0:1,*)
		n_out(*,3:4,*)=n_out(*,1:2,*)
		n_out(*,5:6,*)=n_out(*,1:2,*)
	ENDIF

	datp.n=n_out

	IF (iprint GT 0) THEN PRINT,'End of Addition section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	s=datp.other_tit & i=RSTRPOS(s,' ') & n=STRLEN(s) & numor=STRMID(s,i+1,n-1)
        IF (nruns EQ 1) THEN IF (STRPOS(numor,'>') EQ -1) THEN numor=STRTRIM(STRING(FIX(par(0))),2)

	CASE expand OF
		0: key = '/zpo'
		1: key = '/xyz'
		2: key = '/nopo'
	ENDCASE

	IF (STRPOS(s,' -cc') EQ -1) THEN $	; data not previously concatenated
		datp.other_tit='D7 #'+numor+' -ax('+key+')' $
	ELSE datp.other_tit=datp.other_tit+' -ax('+key+')'

	IF (expand EQ 0) THEN s='Z,X and Y added together'
	IF (expand EQ 1) THEN s='Z,X and Y added together and copied to X and Y'
	IF (expand EQ 2) THEN s='all phases added together and expanded back'
	PRINT,'add_xyz: '+s

finished:
	IF (iprint GT 0) THEN PRINT,'End Add_xyz:'

	give_datp, datp

	RETURN, w_out
	END
