;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION splice, w_in1, w_in2, min=xmin, max=xmax, xl

; Splices together two data sets together. Takes w_in1 outside the 
; xmin->xmax region and w_in2 inside. 
;
;ARGUMENTS:
; min	:min of w_in2 region
; max	:max of w_in2 region
;
;DIMENSIONS
; must be 2 1-d arrays input
;
;COMMAND SYNTAX:
; w3=splice(w1,w2,min=<min>,max=<max>)
;
;						KHA,JRS 21/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	IF(N_ELEMENTS(xl) GT 0) THEN xlimits=xl ELSE xlimits=[xmin,xmax]

	take_datp, dat1
	take_datp, dat2, /third

	datp=dat1

	xmin=xlimits(0)
	xmax=xlimits(1)

	x_in1=dat1.x
	x_in2=dat2.x

	e_in1=dat1.e
	e_in2=dat2.e

	s1=SIZE(w_in1)
	s2=SIZE(w_in2)

	IF (s1(0) NE s2(0)) THEN BEGIN
		PRINT,'splice: Error - input workspaces must have same dimensions'
		GOTO, finished
	ENDIF
	nin1=s1(1)	& nin2=s2(1)

	n1=1	& n2=1
	IF (s1(0) NE 1) THEN BEGIN
		IF (s1(0) EQ 2) THEN n1=s1(2) ELSE BEGIN
			n1=s1(2)
			n2=s1(3)
		ENDELSE
	ENDIF
	
	iOK1=WHERE(x_in1 LE xmin OR x_in1 GE xmax, nOK1)
	iOK2=WHERE(x_in2 GE xmin AND x_in2 LE xmax, nOK2)
	IF (nOK1 LE 0 OR nOK2 LE 0) THEN BEGIN
		PRINT,'splice: Error - one of the workspaces has no data points to be kept'
		GOTO, finished
	ENDIF

	nout=nOK1+nOK2

	x_out=FLTARR(nout)
	w_buf=FLTARR(nout)
	e_buf=FLTARR(nout)

	w_out=FLTARR(nout,n1,n2)
	e_out=FLTARR(nout,n1,n2)

	x_out=[x_in1(iOK1),x_in2(iOK2)]
	isort=SORT(x_out)
	x_out=x_out(isort)

	FOR i1=0,n1-1 DO BEGIN
		FOR i2=0,n2-1 DO BEGIN
			w_buf=[w_in1(iOK1,i1,i2),w_in2(iOK2,i1,i2)]
			w_buf=w_buf(isort)
			w_out(*,i1,i2)=w_buf
			e_buf=[e_in1(iOK1,i1,i2),e_in2(iOK2,i1,i2)]
			e_buf=e_buf(isort)
			e_out(*,i1,i2)=e_buf
		ENDFOR
	ENDFOR

	IF (n2 EQ 1) THEN e_out=REFORM(e_out,nout,n1)
	IF (n1 EQ 1) THEN e_out=REFORM(e_out,nout)

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

        smin=STRTRIM(STRING(xmin),2)
trymin:	n=STRLEN(smin)     & i=RSTRPOS(smin,'0')
        IF (i EQ n-1) THEN BEGIN
                smin=STRMID(smin,0,n-1)
                GOTO, trymin
        ENDIF
        smax=STRTRIM(STRING(xmax),2)
trymax:	n=STRLEN(smax)     & i=RSTRPOS(smax,'0')
        IF (i EQ n-1) THEN BEGIN
                smax=STRMID(smax,0,n-1)
                GOTO, trymax
        ENDIF

	PRINT,'Splice: w_in1 taking outside ['+smin+','+smax+'] region, w_in2 outside'
	datp.other_tit=datp.other_tit+' -sp('+smin+','+smax+')'

finished:
	give_datp, datp

	RETURN, w_out
	END
