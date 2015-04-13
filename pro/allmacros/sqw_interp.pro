;--------------------------------------------------------------------------------
;********************************************************************************
;
	FUNCTION sqw_interp, w_in, dQ=dQ, dE=dE, Emin=Emin, $
		 all_angles=all_angles, pos_angles=pos_angles, $
		 neg_angles=neg_angles, qb, eb, em, ib
;
; For IN4, IN5, IN6 and D7
;
;converts output data from t2e to regular-grid interpolated S(Q,w) data.
;input workspace must be in energy transfer versus scattering angle.
;
;ARGUMENTS:
;	dQ :	Q bin width
;	dE :	energy bin width
;	Emin :	minimum energy (neutron energy gain is defined as negative)
;
;KEYWORDS (- only for D7 data)
; /neg_angles	: use only negative angles
; /pos_angles	: use only positive angles
; /all_angles	: use all angles (default)
;		input workspace must be in energy transfer versus scattering angle,
;		i.e. only one component or spin phase.
; (qb, eb, em and ib are obsolete, kept for backwards compatability)
;
;DIMENSIONS:
; w_in(nE,nphi) -> w_out(nQs,nEs)
;
;COMMAND SYNTAX:
; w10=spw_interp(w9,dQ=<dQ>,dE=<dE>, Emin=<Emin>[,/neg_angles][,/pos_angles][,/all_angles])
;
; (optional keywords shown in square brackets)
;
;							KHA,JRS 10/8/00
;--------------------------------------------------------------------------------
;********************************************************************************

	common c_lamp_access, inst

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start sqw_interp:'

	take_datp, datp

	ibank=2
	IF(N_ELEMENTS(qb) GT 0) THEN dQ=qb
	IF(N_ELEMENTS(eb) GT 0) THEN dE=eb
	IF(N_ELEMENTS(em) GT 0) THEN Emin=em
	IF(N_ELEMENTS(ib) GT 0) THEN ibank=ib

	IF KEYWORD_SET(pos_angles) THEN ibank=1
	IF KEYWORD_SET(neg_angles) THEN ibank=0
	IF KEYWORD_SET(all_angles) THEN ibank=2
;-------------------------------------------------------------------------------
;Set up starting parameters

	par=datp.p

	swap_QE=0	; swap_QE=0 => Q is x-axis, E is y-axis
			; swap_QE=1 => E is x-axis, Q is y-axis

	noneg=1	; noneg=1 => set all negative values to zero

	IF (N_ELEMENTS(Emin) NE 1) THEN BEGIN
		PRINT,'sqw_rebin: Error - dQ, dE, Emin must be specified'
		return,w_in
	ENDIF

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	IF (sw(0) NE 2) THEN BEGIN
		PRINT,'sqw_interp: Error - input workspace must be 2-D: E vs. phi'
		return,w_in
	ENDIF
	nEps=sw(1)
	nphi=sw(2)
	IF (iprint GT 0) THEN PRINT,'nEps=',nEps,' nphi=',nphi
	x_in=datp.x	& sx=SIZE(x_in)
	y_in=datp.y	& sy=SIZE(y_in)
	IF (nEps NE sx(1)) OR (nphi NE sy(1)) THEN BEGIN
		PRINT,'sqw_interp: Error - sx=',sx,' sy=',sy
		return,w_in
	ENDIF

	e_in=datp.e
	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1) OR se(2) NE sw(2)) THEN e_in=w_in*0.

	IF (iprint GT 0) THEN PRINT,'Instrument = ',inst
	IF (inst EQ 'D7') THEN BEGIN
		lambda=par(4)
		IF (N_ELEMENTS(ibank) EQ 0) THEN ibank=2
		TOF=FIX(par(8))
		IF (TOF NE 1) THEN BEGIN
			PRINT,'sqw_rebin: Error - workspace data is not in TOF'
			return,w_in
		ENDIF
	ENDIF ELSE lambda=par(21)

	IF (iprint GT 0) THEN PRINT,'lambda=',lambda,'A'

;-------------------------------------------------------------------------------------
;	Set constants and interpolate to regular Q-E grid

	const1=5.22697		; E(meV)=const1*V(m/ms)^2 for neutron
	const2=2.07193571	; E(meV)=const2*k(A^-1)^2 for neutron
	const3=3.956076		; V(m/ms)=const3/lambda(A) for neutron
	const4=81.8066		; E(meV)=const4/lambda(A)^2 for neutron

	Ei=const4/lambda^2
	ki=SQRT(Ei/const2)

	i=WHERE(x_in GT Emin,nEps)	& Eps=x_in(i)
	w_buf=w_in(i,*)	& e_buf=e_in(i,*)

	phi=y_in*!pi/180.	; convert to radians

	IF (inst EQ 'D7') THEN BEGIN
		IF (ibank EQ 0) THEN BEGIN
			i=WHERE(phi LT 0.,nphi) & phi=ABS(phi(i))
			w_buf=w_buf(*,i)	& e_buf=e_buf(*,i)
		ENDIF ELSE IF (ibank EQ 1) THEN BEGIN
			i=WHERE(phi GT 0.,nphi) & phi=phi(i)
			w_buf=w_buf(*,i)	& e_buf=e_buf(*,i)
		ENDIF ELSE BEGIN
			phi=abs(phi)
			i=SORT(phi)
			phi=phi(i)
			w_buf=w_buf(*,i)	& e_buf=e_buf(*,i)
		ENDELSE
	ENDIF

	Eco=Eps#(FLTARR(1,nphi)+1.)
	COSphi=(FLTARR(nEps)+1.)#COS(phi)
	Qco=SQRT((2.*Ei-Eco-2.*SQRT(Ei*(Ei-Eco))*COSphi)/const2)

	IF(iprint GT 0) THEN help, Eco, Qco, COSphi, w_buf, e_buf

	TRIANGULATE, Qco, Eco, triangles

	Qmin=0.	& Qmax=MAX(Qco)	& Emax=MAX(Eco)
	GS=[dQ,dE]
	Limits=[Qmin,Emin,Qmax,Emax]
	w_out=TRIGRID(Qco,Eco,w_buf,triangles,GS,Limits)
	e_out=TRIGRID(Qco,Eco,e_buf,triangles,GS,Limits)

	sw=SIZE(w_out)
	nQs=sw(1)
	nEs=sw(2)
	Qarr=FINDGEN(nQs)*dQ+Qmin
	Earr=FINDGEN(nEs)*dE+Emin

	IF (iprint GT 0) THEN PRINT,'Qarr=',Qarr
	IF (iprint GT 0) THEN PRINT,'Earr=',Earr

	IF (iprint GT 0) THEN PRINT,'End of "Interpolate to regular grid" section'

;-------------------------------------------------------------------------------
;Set points outside measured region to zero

	phimin=MIN(phi)-2.*!pi/180.	& COSmax=COS(phimin)
	phimax=MAX(phi)+2.*!pi/180.	& COSmin=COS(phimax)
	Q=Qarr#(FLTARR(1,nEs)+1.)
	E=(FLTARR(nQs)+1.)#REFORM(Earr,1,nEs)

	COSphi=(2.*Ei-E-const2*Q^2)/(2.*SQRT((Ei*(Ei-E))>0.))

	outside=WHERE(COSphi GT COSmax OR COSphi LT COSmin)
	w_out(outside)=0.
	e_out(outside)=-1.
	below=WHERE(E LT Eps(0)-dE/2., n)
	IF (n GE 1) THEN w_out(below)=0.
	IF (n GE 1) THEN e_out(below)=-1.
	above=WHERE(E GT Eps(nEps-1)+dE/2., n)
	IF (n GE 1) THEN w_out(above)=0.
	IF (n GE 1) THEN e_out(above)=-1.

;-------------------------------------------------------------------------------------
;	Chop off superfluous bits

checkQ1:
;	iw0=WHERE(w_out(0,*) EQ 0.,nw0) & ie0=WHERE(e_out(0,*) EQ -1.,ne0)
;	IF (nw0 EQ nEs AND ne0 EQ nEs) THEN BEGIN
;		nQs=nQs-1
;		w_out=w_out(1:nQs,*) & e_out=e_out(1:nQs,*) & Qarr=Qarr(1:nQs)
;		GOTO, checkQ1
;	ENDIF

checkQ2:
	iw0=WHERE(w_out(nQs-1,*) EQ 0.,nw0) & ie0=WHERE(e_out(nQs-1,*) EQ -1.,ne0)
	IF (nw0 EQ nEs AND ne0 EQ nEs) THEN BEGIN
		nQs=nQs-1
		w_out=w_out(0:nQs-1,*) & e_out=e_out(0:nQs-1,*) & Qarr=Qarr(0:nQs-1)
		GOTO, checkQ2
	ENDIF

checkEps1:
	iw0=WHERE(w_out(*,0) EQ 0.,nw0) & ie0=WHERE(e_out(*,0) EQ -1.,ne0)
	IF (nw0 EQ nQs AND ne0 EQ nQs) THEN BEGIN
		nEs=nEs-1
		w_out=w_out(*,1:nEs) & e_out=e_out(*,1:nEs) & Earr=Earr(1:nEs)
		GOTO, checkEps1
	ENDIF

checkEps2:
	iw0=WHERE(w_out(*,nEs-1) EQ 0.,nw0) & ie0=WHERE(e_out(*,nEs-1) EQ -1.,ne0)
	IF (nw0 EQ nQs AND ne0 EQ nQs) THEN BEGIN
		nEs=nEs-1
		w_out=w_out(*,0:nEs-1) & e_out=e_out(*,0:nEs-1) & Earr=Earr(0:nEs-1)
		GOTO, checkEps2
	ENDIF

;-------------------------------------------------------------------------------------
;	zero negative counts

	IF (noneg EQ 1) THEN BEGIN
		ineg=WHERE(w_out LT 0., n)
		IF (n GE 1) THEN w_out(ineg)=0.
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "set outside points to zero" section'


;-------------------------------------------------------------------------------------
;	Return parameters and exit

	IF (swap_QE EQ 0) THEN BEGIN
		datp.y_tit=datp.x_tit
		datp.x_tit='Wavevector Transfer (A-1)'
		mod_datp, datp, "x", Qarr
		mod_datp, datp, "y", Earr
	ENDIF ELSE BEGIN
		w_out=TRANSPOSE(w_out)	& e_out=TRANSPOSE(e_out)
		datp.y_tit='Wavevector Transfer (A-1)'
		mod_datp, datp, "x", Earr
		mod_datp, datp, "y", Qarr
	ENDELSE

	mod_datp, datp, "e", e_out

	s=STRTRIM(STRING(FLOAT(dQ)),2)  & i=STRPOS(s,'.') & dQ=STRMID(s,0,i(0)+3)
	s=STRTRIM(STRING(FLOAT(dE)),2)  & i=STRPOS(s,'.') & dE=STRMID(s,0,i(0)+3)
	Emin=STRTRIM(STRING(FIX(Emin)),2)
	s=' -si('+dQ+','+dE+','+Emin
	IF (inst EQ 'D7') THEN s=s+','+STRTRIM(STRING(ibank),2)+')' ELSE s=s+')'
	datp.other_tit=datp.other_tit+s

	PRINT, 'sqw_interp: Interpolated to constant Q-w: dQ='+dQ+'A-1, dE='+dE+'meV'

	give_datp, datp

finished:
	RETURN, w_out
	END


