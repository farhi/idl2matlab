;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION estrip, w_in, E=Evalue, dE=dE, dQ=dQ, $
		 pos_angles=pos_angles, neg_angles=neg_angles, $
		 all_angles=all_angles, ev, eb, qb, ib
;
; For IN4, IN5, IN6 and D7
;
;Rebins output data from t2e and reb to regular-grid S(Q,w) data using the old
;KHA IN6 rebin algorithm. Selects a single E-value. Input workspace must be in
;energy transfer versus scattering angle, i.e. only one component or spin phase.
;
;ARGUMENTS:
;	Evalue: E-value at centre of extracted strip
;	dE :	Width of E-strip
;	dQ :	Q bin width
;
;KEYWORDS: (- only for D7 data:)
; /neg_angles   : use only negative angles
; /pos_angles	: use only positive angles
; /all_angles	: use all angles (default)
;		input workspace must be in energy transfer versus scattering angle,
;		i.e. only one component or spin phase.
; (ev, eb, qb and ib are obsolete, kept for backwards compatability)
;
;DIMENSIONS:
; w_in(nE,nphi) -> w_out(nE,nQ)
;
;COMMAND SYNTAX:
; w10=estrip(w9,E=<Evalue>,dE=<dE>,dQ=<dQ>[,/pos_angles][,/neg_angles][,/all_angles])
;
; (Optional keywords shown in square brackets)
;
;							KHA,JRS 13/7/00
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	common c_lamp_access, inst

	common grid, Qmin, Qmax, Emin, Emax

	iprint=0	; if iprint>0, show debugging messages
			; if iprint>1, plot overlap areas

	IF (iprint GT 0) THEN PRINT,'Start estrip:'

	take_datp, datp

	ibank=2
	IF(N_ELEMENTS(ev) GT 0) THEN Evalue=ev
	IF(N_ELEMENTS(eb) GT 0) THEN dE=eb
	IF(N_ELEMENTS(qb) GT 0) THEN dQ=qb
	IF(N_ELEMENTS(ib) GT 0) THEN ibank=ib

	IF KEYWORD_SET(pos_angles) THEN ibank=1
	IF KEYWORD_SET(neg_angles) THEN ibank=0
	IF KEYWORD_SET(all_angles) THEN ibank=2


;-------------------------------------------------------------------------------
;Set up starting parameters

	IF (N_ELEMENTS(Evalue) NE 1 OR N_ELEMENTS(dE) NE 1 OR N_ELEMENTS(dQ) NE 1) THEN BEGIN
		PRINT,'estrip: Error - Evalue, dE, dQ must be specified'
		return,w_in
	ENDIF

	sw=SIZE(w_in)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	IF (sw(0) NE 2) THEN BEGIN
		PRINT,'estrip: Error - input workspace must be 2-D: E vs. phi'
		return,w_in
	ENDIF
	nx=sw(1)
	ny=sw(2)
	IF (iprint GT 0) THEN PRINT,'nx=',nx,' ny=',ny
	x_in=datp.x	& sx=SIZE(x_in)
	y_in=datp.y	& sy=SIZE(y_in)
	IF (nx NE sx(1)) OR (ny NE sy(1)) THEN BEGIN
		PRINT,'estrip: Error - sx=',sx,' sy=',sy
		return,w_in
	ENDIF

	e_in=datp.e
	se=SIZE(e_in)
	IF (se(0) NE sw(0) OR se(1) NE sw(1) OR se(2) NE sw(2)) THEN e_in=w_in*0.

	par=datp.p

	IF (iprint GT 0) THEN PRINT,'Instrument = ',inst
	IF (inst EQ 'D7') THEN BEGIN
		lambda=par(4)
		IF (N_ELEMENTS(ibank) EQ 0) THEN ibank=2
		TOF=FIX(par(8))
		IF (TOF NE 1) THEN BEGIN
			PRINT,'estrip: Error - workspace data is not in TOF'
			return,w_in
		ENDIF
	ENDIF ELSE lambda=par(21)

	IF (iprint GT 0) THEN PRINT,'lambda=',lambda,'A'

;-------------------------------------------------------------------------------------
;	Set constants and prepare arrays for rebinning to regular Q-E grid

	const1=5.22697		; E(meV)=const1*V(m/ms)^2 for neutron
	const2=2.07193571	; E(meV)=const2*k(A^-1)^2 for neutron
	const3=3.956076		; V(m/ms)=const3/lambda(A) for neutron
	const4=81.8066		; E(meV)=const4/lambda(A)^2 for neutron

	Ei=const4/lambda^2
	ki=SQRT(Ei/const2)
	y_in=y_in*!pi/180.	; convert to radians

	nEps=nx+1	& Eps=FLTARR(nEps)
	Eps(0)=x_in(0)-(x_in(1)-x_in(0))/2.
	Eps(1:nx-1)=(x_in(0:nx-2)+x_in(1:nx-1))/2.
	Eps(nx)=x_in(nx-1)+(x_in(nx-1)-x_in(nx-2))/2.

	IF (iprint GT 0) THEN PRINT,'x=',x_in
	IF (iprint GT 0) THEN PRINT,'Eps=',Eps

	Emin=Evalue-dE/2.	& Emax=Evalue+dE/2.
	iEarr=WHERE(Eps GT Emin AND Eps LT Emax,n)
	iEarr=[iEarr(0)-1,iEarr]
	IF (Emax LT Eps(nx)) THEN iEarr=[iEarr,iEarr(n-1)+1]
	Eps=Eps(iEarr)	& s=SIZE(Eps)	& nEps=s(1)
	Eps(0)=Emin	& Eps(nEps-1)=Emax
	IF (iprint GT 0) THEN PRINT,'Eps=',Eps
	iEarr=iEarr(0:nEps-2)
	Qmin=0.	& Qmax=SQRT((2.*Ei-Emin-2.*SQRT(Ei*(Ei-Emin))*COS(y_in(ny-1)))/const2)
	IF (iprint GT 0) THEN PRINT,'Emin=',Emin,' Emax=',Emax,' meV'
	IF (iprint GT 0) THEN PRINT,'Qmin=',Qmin,' Qmax=',Qmax,' A-1'

	nQ=FIX((Qmax-Qmin)/dQ)+1
	IF (iprint GT 0) THEN PRINT,'nQ=',nQ

	w_out=FLTARR(nQ,nEps-1)*0.	& e_out=w_out-1.
	Q=Qmin+FLOAT(INDGEN(nQ))*dQ
	IF (iprint GT 0) THEN PRINT,'Q=',Q

	IF (iprint GT 0) THEN PRINT,'y_in=',y_in*180./!pi

	IF (inst EQ 'D7') THEN BEGIN
		i=WHERE(y_in GT 0.,n)
		IF (n LE 0 OR n EQ ny) THEN BEGIN
			PRINT,'estrip: Error - For D7 both -ve and +ve angles must be present'
			return,w_in
		ENDIF
		IF (ibank EQ 2) THEN BEGIN
			twice=1
			iphi1=0		& iphi1next=i(0)
			iphi2=i(0)-1	& iphi2next=ny-1
		ENDIF ELSE BEGIN
			twice=0
			IF (ibank EQ 0) THEN BEGIN
				iphi1=0
				iphi2=i(0)-1
			ENDIF ELSE IF (ibank EQ 1) THEN BEGIN
				iphi1=i(0)
				iphi2=ny-1
			ENDIF ELSE BEGIN
				PRINT,'estrip: Error - ibank =',ibank
				return,w_in
			ENDELSE
		ENDELSE
	ENDIF ELSE BEGIN
		twice=0
		iphi1=0	& iphi2=ny-1
	ENDELSE
	IF (iprint GT 0) THEN PRINT,'twice=',twice,' iphi1=',iphi1,' iphi2=',iphi2

start:
	nphi=iphi2-iphi1+2
	phi=FLTARR(nphi)
	phi(0)=y_in(iphi1)-(y_in(iphi1+1)-y_in(iphi1))/2.
	phi(1:nphi-2)=(y_in(iphi1:iphi2-1)+y_in(iphi1+1:iphi2))/2.
	phi(nphi-1)=y_in(iphi2)+(y_in(iphi2)-y_in(iphi2-1))/2.
	COSphi=COS(phi)

	w_buf=w_in(iEarr,iphi1:iphi2)
	e_buf=e_in(iEarr,iphi1:iphi2)
	y_buf=y_in(iphi1:iphi2)
	IF (phi(0) LT 0.) THEN BEGIN ; reverse array direction for negative angles
		w_buf=REVERSE(w_buf,2)
		e_buf=REVERSE(e_buf,2)
		y_buf=ABS(REVERSE(y_buf))
		phi=ABS(REVERSE(phi))
		COSphi=REVERSE(COSphi)
	ENDIF

	IF (iprint GT 0) THEN PRINT,'phi=',phi*180./!pi

	IF (iprint GT 0) THEN PRINT,'End of "prepare arrays" section'

;-------------------------------------------------------------------------------------
;	Rebin to constant Q grid

	a=const2		;	E(meV)=a*Q(A**-1)**2   for neutron

	iprint0=iprint
	oldymin=0.

	IF (iprint GT 1) THEN BEGIN
		b=''
		PRINT,'About to start rebinning. Hit return to continue'
		READ, b
	ENDIF

	FOR iQ=0,nQ-1 DO BEGIN
		IF (iprint0 GT 0) THEN PRINT,'Rebinning at Q =',Q(iQ),' +/-',dQ/2.
		iprint=0
		IF (iprint0 GT 0) AND (10*(iQ/10) EQ iQ) THEN iprint=iprint0
		IF (iprint GT 1) THEN PRINT,'iQ=',iQ
		Qmin=Q(iQ)-dQ/2.	& Qmax=Q(iQ)+dQ/2.
		Q00=[Qmin,Qmin,Qmax,Qmax]
		IF (iprint GT 1) THEN PRINT,Qmin,' < Q <',Qmax
		FOR iEps=0,nEps-2 DO BEGIN
			IF (iprint GT 1) THEN PRINT,'iEps=',iEps
			Emin=Eps(iEps)	& Emax=Eps(iEps+1)
			corrarea=dQ*(Emax-Emin)
			Eps0=[Emin,Emax,Emax,Emin]
			IF (iprint GT 1) THEN PRINT,Emin,' < Eps <',Emax
			COSphi0=(2.*Ei-Eps0-a*Q00^2)/(2.*SQRT(Ei*(Ei-Eps0)))
			IF (iprint GT 1) THEN PRINT,' COS(phi1-4):',COSphi0
			IF (MAX(ABS(COSphi0)) GE 1.) THEN GOTO, outside
			phi0=ACOS(COSphi0)
			IF (iprint GT 1) THEN PRINT,' phi0=',phi0*180./!pi
			phimin=MIN(phi0)	& phimax=MAX(phi0)
			IF (iprint GT 1) THEN PRINT,'Outside if: phimax=',phimax*180./!pi,'< phi(0)=',phi(0)*180./!pi
			IF (iprint GT 1) THEN PRINT,'        or: phimin=',phimin*180./!pi,'> phi(nphi-1)=',phi(nphi-1)*180./!pi,'  nphi=',nphi
			IF (phimax LT phi(0) OR phimin GT phi(nphi-1)) THEN GOTO, outside
			IF (iprint GT 1) THEN PRINT,'phi within range'
			iphi=WHERE(phi GT phimin AND phi LT phimax, nlines)
			iphi0=(iphi(0)-1)>0
			IF (nlines EQ 0) THEN BEGIN
				phimean=(phimin+phimax)/2.
				ip=WHERE(phi LT phimean, np)
				iphi0=ip(np-1)
			ENDIF
startrebin:		Areasum=0.
			wsum=0.
			e2sum=0.
			phiminmeas=7.	& phimaxmeas=0.
			FOR iphi=iphi0,(iphi0+nlines)<(nphi-2) DO BEGIN
				IF (iprint GT 1) THEN BEGIN
					ip1=iphi0
					ip2=(iphi0+nlines)<(nphi-2)
					PRINT,'FOR iphi=',ip1,',',ip2
					PRINT,'phi(',ip1,')=',phi(ip1)*180./!pi
					PRINT,'phi(',ip2+1,')=',phi(ip2+1)*180./!pi
				ENDIF
				COSphi1=COSphi(iphi)	& COSphi2=COSphi(iphi+1)
				COSphi0=[COSphi1,COSphi1,COSphi2,COSphi2]
				IF (iprint GT 1) THEN PRINT,'phi1=',phi(iphi)*180./!pi,$
								' phi2=',phi(iphi+1)*180./!pi
				Q0=SQRT((2.*Ei-Eps0-2.*SQRT(Ei*(Ei-Eps0))*COSphi0)/a)
				area=overlap(Q0,Eps0,iprint-1,oldymin)
				IF (area GT 0.) THEN BEGIN
					IF (iprint GT 1) THEN PRINT,'Area>0'
					w=w_buf(iEps,iphi)	& e=e_buf(iEps,iphi)
					IF (w NE 0. OR e GE 0.) THEN BEGIN
						areasum=areasum+area
						wsum=wsum+area*w
						e2sum=e2sum+(area*e)^2
						phiminmeas=phiminmeas<phi(iphi)
						phimaxmeas=phimaxmeas>phi(iphi+1)
						IF (iprint GT 1) THEN PRINT, $
							'w_buf(',iEps,',',iphi,')=',w
						IF (iprint GT 1) THEN PRINT, $
							'e_buf(',iEps,',',iphi,')=',e
						IF (iprint GT 1) THEN PRINT, $
						phiminmeas,' < phimeas <',phimaxmeas
					ENDIF
				ENDIF ELSE IF (iprint GT 1) THEN PRINT,'Area<=0'
			ENDFOR
			IF (areasum NE 0.) THEN BEGIN
				IF (iprint GT 1) THEN PRINT,'areasum NE 0. - OK'
				w_out(iQ,iEps)=wsum/areasum
				e_out(iQ,iEps)=SQRT(e2sum)/areasum
				GOTO, binned
			ENDIF ELSE IF (iprint GT 1) THEN PRINT,'areasum is zero'
outside:		IF (iprint GT 1) THEN PRINT,'Outside covered Q-w region'
			w_out(iQ,iEps)=0.
			e_out(iQ,iEps)=-1.
			GOTO, nextpoint
binned:
			IF (iprint GT 1) THEN PRINT,'phimin=',phimin,' phimax=',phimax
			IF (iprint GT 1) THEN PRINT,'measur=',phiminmeas,'        ',phimaxmeas
			p1=phimin>phiminmeas
			p2=phimax<phimaxmeas
			IF (p2-p1 LT (phimax-phimin)/2.) THEN BEGIN
				IF (iprint GT 1) THEN PRINT,'Point removed'
				w_out(iQ,iEps)=0.
				e_out(iQ,iEps)=-1.
			ENDIF ELSE IF (iprint GT 1) THEN PRINT,'Point kept'
nextpoint:
			IF (iprint GT 1) THEN PRINT,'*********************************************************'
		ENDFOR
	ENDFOR

	iprint=iprint0

	IF (iprint GT 0) THEN PRINT,'End of rebinning'

	IF (twice EQ 1) THEN BEGIN
		IF (iphi1 EQ 0) THEN BEGIN
			w_out1=w_out	& e_out1=e_out
			iphi1=iphi1next
			iphi2=iphi2next
			GOTO, start
		ENDIF ELSE BEGIN
			w_out2=w_out	& e_out2=e_out
			w_out(*,*)=0.	& e_out(*,*)=0.
			not1=WHERE(e_out1 LE 0.,n1)
			IF (n1 NE 0) THEN e_out1(not1)=1.
			not2=WHERE(e_out2 LE 0.,n2)
			IF (n2 NE 0) THEN e_out2(not2)=1.
			w_out=(w_out1/e_out1^2+w_out2/e_out2^2)/(1./e_out1^2+1./e_out2^2)
			e_out=1./SQRT(1./e_out1^2+1./e_out2^2)
			IF (n1 NE 0) THEN e_out1(not1)=-1.
			IF (n2 NE 0) THEN e_out2(not2)=-1.
			IF (n1 NE 0) THEN BEGIN
				w_out(not1)=w_out2(not1)
				e_out(not1)=e_out2(not1)
			ENDIF
			IF (n2 NE 0) THEN BEGIN
				w_out(not2)=w_out1(not2)
				e_out(not2)=e_out1(not2)
			ENDIF
		ENDELSE
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of rebinning section'

;-------------------------------------------------------------------------------------
;	Add E channels together

	dEps=Eps(1:nEps-1)-Eps(0:nEps-2)	& dEpssum=Eps(nEps-1)-Eps(0)
	dEps=REFORM(dEps,1,nEps-1)	& x=fltarr(nQ)+1.	& dEps=x#dEps

	n_out=0*w_out+1	& i=WHERE(e_out LT 0.)	& n_out(i)=0	& e_out(i)=0.

	w_out=TOTAL(w_out*n_out*dEps,2)
	e_out=SQRT(TOTAL((e_out*n_out*dEps)^2,2))
	n_out=TOTAL(n_out*dEps,2)

	i=WHERE(n_out GE dEpssum/2.)	& z=WHERE(n_out EQ 0)	& n_out(z)=1
	w_out=w_out/n_out	& w_out=w_out(i)
	e_out=e_out/n_out	& e_out=e_out(i)
	Q=Q(i)

	IF (iprint GT 0) THEN PRINT,'End of channel adding section'

;-------------------------------------------------------------------------------------
;	Return parameters and exit

	datp.x_tit='Wavevector Transfer (A-1)'
	datp.y_tit=datp.z_tit

	mod_datp, datp, "x", Q
	mod_datp, datp, "e", e_out

	PRINT, FORMAT='("estrip: Rebinned to constant Q-w: E=",F6.2,"+/-",F4.2,"meV")', Evalue, dE/2.

	s=STRTRIM(STRING(FLOAT(Evalue)),2)  & i=STRPOS(s,'.') & Evalue=STRMID(s,0,i(0)+3)
	s=STRTRIM(STRING(FLOAT(dE)),2)  & i=STRPOS(s,'.') & dE=STRMID(s,0,i(0)+3)
	s=' -es('+Evalue+','+dE
	IF (inst EQ 'D7') THEN s=s+','+STRTRIM(STRING(ibank),2)+')' ELSE s=s+')'
	datp.other_tit=datp.other_tit+s

	give_datp, datp

finished:
	RETURN, w_out
	END


