;-----------------------------------------------------------------------------
;*****************************************************************************
	FUNCTION reb, w_in, dE=dE, force=forcebin, eb, fb
;
;For IN4, IN5, IN6 and D7 data.
;
; rebins output workspace from t2e to constant energy bin width.
;
;ARGUMENTS:
; dE	: required energy bin width
; (eb and fb are obsolete and kept for backwards compatability)
;
;KEYWORDS:
; /force	: rebin over entire energy range
;		  otherwise: rebin only where dE is > point spacing (default)
;
;DIMENSIONS
; w_in=(nE,nspectra) -> w_out(dE, nspectra)
;
;COMMAND SYNTAX:
; w10=reb(w9,dE=<dE>[,/force])
;
; (optional keywords shown in square brackets)
;
;							KHA,JRS 9/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start reb:'

	take_datp, datp
	IF(N_ELEMENTS(eb) GT 0) THEN dE=eb
	IF(N_ELEMENTS(fb) GT 0) THEN forcebin=fb

;-------------------------------------------------------------------------------
;Set up input workspace

	par  =datp.p
	w_out=0

	sw=SIZE(w_in)
	IF (sw(0) EQ 1) THEN nspectra=1     ELSE $
	IF (sw(0) EQ 2) THEN nspectra=sw(2) ELSE return,w_in
	nchannels=sw(1)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in)=',sw
	IF (iprint GT 0) THEN PRINT,'nchannels=',nchannels,' nspectra=',nspectra

	npts=nchannels

	IF (N_ELEMENTS(dE) EQ 0) THEN BEGIN
		PRINT,'reb: error - dE not specified'
		return,w_in
	ENDIF

	IF (N_ELEMENTS(forcebin) EQ 0) THEN forcebin=0

	x_in=datp.x
	e_in=datp.e

        se=size(e_in)
	sw=size(w_in)

	errors=1
	IF (se(0) NE sw(0) OR se(1) NE sw(1) OR se(2) NE sw(2)) THEN errors=0

	IF (iprint GT 0) THEN PRINT,'npts=',npts,' nspectra=',nspectra,' forcebin=',forcebin
	IF (iprint GT 0) THEN PRINT,'End of input setup section'

;-------------------------------------------------------------------------------
;Set up rebin arrays

	IF (forcebin EQ 0) THEN xmin=x_in(0) $
			ELSE xmin=MAX([x_in(0),-50.])
	xmax=x_in(npts-1)

	dx_in=x_in
	dx_in(1:npts-2)=(x_in(2:npts-1)-x_in(0:npts-3))/2.
	dx_in(0)=x_in(1)-x_in(0)	& dx_in(npts-1)=x_in(npts-1)-x_in(npts-2)

	x_in0=FLTARR(npts+1)	; histogram
	x_in0(1:npts-1)=(x_in(0:npts-2)+x_in(1:npts-1))/2.
	x_in0(0)=x_in0(1)-(x_in(1)-x_in(0))
	x_in0(npts)=x_in0(npts-1)+(x_in(npts-1)-x_in(npts-2))

	IF (iprint GT 0) THEN BEGIN
		PRINT,'   channel      x_in        dx_in        x01         x02'
		FOR i=0,npts-1 DO PRINT, i, x_in(i), dx_in(i), x_in0(i), x_in0(i+1)
	ENDIF

	nbin=LONG((xmax-xmin)/dE)+2

	IF (forcebin EQ 1) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'forcebin=1'
neverwider:
		i0=0
		IF (iprint GT 0) THEN PRINT,'rebin everything'
		xstart=dE*FLOAT(FIX(xmin/dE)-1)
		x_bin=FLOAT(INDGEN(nbin))*dE+xstart
	ENDIF ELSE BEGIN
		IF (iprint GT 0) THEN PRINT,'forcebin=0'
		wider=WHERE(dx_in GT dE, nw)
		IF (nw LE 0) THEN GOTO, neverwider
		x_bin=FLTARR(nbin)
		i0=wider(nw-1)	& x0=x_in(i0)
		IF (iprint GT 0) THEN PRINT,'rebin from x=',x0
		x_bin(0:i0)=x_in(0:i0)
		n=FIX((xmax-x0)/dE)
		IF (x0 LT 0.) THEN xstart=dE*FLOAT(FIX(x0/dE)+1) $
				ELSE xstart=dE*FLOAT(FIX(x0/dE)+2)
		x_bin(i0+1:i0+n)=FLOAT(INDGEN(n))*dE+xstart
		x_bin(i0+1)=(x_bin(i0)+x_bin(i0+2))/2.
		nbin=i0+n+1
		x_bin=x_bin(0:nbin-1)
	ENDELSE


	dx_bin=x_bin
	dx_bin(1:nbin-2)=(x_bin(2:nbin-1)-x_bin(0:nbin-3))/2.
	dx_bin(0)=x_bin(1)-x_bin(0)	& dx_bin(nbin-1)=x_bin(nbin-1)-x_bin(nbin-2)

	x_bin0=FLTARR(nbin+1)	; histogram
	x_bin0(1:nbin-1)=(x_bin(0:nbin-2)+x_bin(1:nbin-1))/2.
	x_bin0(0)=x_bin0(1)-(x_bin(1)-x_bin(0))
	x_bin0(nbin)=x_bin0(nbin-1)+(x_bin(nbin-1)-x_bin(nbin-2))

	IF (iprint GT 0) THEN BEGIN
		PRINT,'   bin      x_bin     dx_bin      x01       x02'
		FOR i=0,nbin-1 DO PRINT, i, x_bin(i), dx_bin(i), x_bin0(i), x_bin0(i+1)
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of rebin array setup section'

;-------------------------------------------------------------------------------
;Perform rebinning

	w_out=FLTARR(nbin,nspectra)
	IF (errors EQ 1) THEN e_out=FLTARR(nbin,nspectra)

	IF (i0 GT 0) THEN BEGIN	; do not rebin first points
		w_out(0:i0-1,*)=w_in(0:i0-1,*)
		IF (errors EQ 1) THEN e_out(0:i0-1,*)=e_in(0:i0-1,*)
	ENDIF

	iptmin=0
	ysum=FLTARR(nspectra)	& esum2=ysum

	FOR ibin=i0,nbin-1 DO BEGIN
		xmin=x_bin0(ibin)	& xmax=x_bin0(ibin+1)
		xsum=0.		& ysum(*)=0.	& esum2(*)=0.
		FOR ipt=iptmin,npts-1 DO BEGIN
			x2=x_in0(ipt+1)
			IF (x2 GT xmin) THEN BEGIN
				x1=x_in0(ipt)
				IF (x1 GT xmax) THEN GOTO, nomore
				IF (x2 LT xmax) THEN xhi=x2 ELSE xhi=xmax
				xhi=MIN([x2,xmax])
				xlo=MAX([x1,xmin])
				deltax=xhi-xlo
				IF (deltax LT 0.0) THEN PRINT,'Error in rebinning'
				xsum=xsum+deltax
				ysum=ysum+deltax*w_in(ipt,*)
				IF (errors EQ 1) THEN esum2=esum2+(deltax*e_in(ipt,*))^2
			ENDIF
		ENDFOR
nomore:		IF (xsum EQ 0.) THEN BEGIN
			w_out(ibin,*)=0.
			IF (errors EQ 1) THEN e_out(ibin,*)=-1.
		ENDIF ELSE BEGIN
			w_out(ibin,*)=ysum/xsum
			IF (errors EQ 1) THEN e_out(ibin,*)=SQRT(esum2)/xsum
		ENDELSE
		iptmin=(ipt-1)>0
	ENDFOR

	x_out=x_bin

	IF (iprint GT 0) THEN PRINT,'End of rebinning section'

;-------------------------------------------------------------------------------
;Remove zeroed points

	IF (errors EQ 1) THEN BEGIN
		WHILE (TOTAL(e_out(0,*)) LT -FLOAT(nspectra)+0.01) DO BEGIN
			x_out=x_out(1:nbin-1)
			w_out=w_out(1:nbin-1,*)
			e_out=e_out(1:nbin-1,*)
			nbin=nbin-1
		ENDWHILE
		WHILE (TOTAL(e_out(nbin-1,*)) LT -FLOAT(nspectra)+0.01) DO BEGIN
			x_out=x_out(0:nbin-2)
			w_out=w_out(0:nbin-2,*)
			e_out=e_out(0:nbin-2,*)
			nbin=nbin-1
		ENDWHILE
	ENDIF

	FOR ispec=0,nspectra-1 DO BEGIN
		IF (TOTAL(w_in(*,ispec),1) EQ 0.) THEN BEGIN
			w_out(*,ispec)=0.
			IF (errors EQ 1) THEN e_out(*,ispec)=-1.
		ENDIF
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of remove zeros section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "x", x_out
	IF (errors EQ 1) THEN mod_datp, datp, "e", e_out

	s=STRTRIM(STRING(dE),2) & i=RSTRPOS(s,'.') & dE=STRMID(s,0,i+3)
	datp.other_tit=datp.other_tit+' -rb('+dE+','+STRTRIM(STRING(forcebin),2)+')'

	IF (forcebin EQ 0) THEN BEGIN
		s=STRTRIM(STRING(x_out(i0)),2) & i=RSTRPOS(s,'.') & Elim=STRMID(s,0,i+2)
		range='where E>'+Elim+'meV'
	ENDIF ELSE range='everywhere'

	PRINT,'reb: rebinned '+range+' to constant energy bin width of '+dE+'meV'

	give_datp, datp

finished:
	RETURN, w_out

	END
