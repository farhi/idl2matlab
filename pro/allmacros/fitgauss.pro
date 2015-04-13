;----------------------------------------------------------------------------
;****************************************************************************
;
;	PRO fitgauss, x_in, y_in, e_in, xmin, xmax, par, dpar, ispec, $
;	    all=allpoints, plot=plot
;
; Fits a Gaussian plus a flat background, and calculates partial derivatives.
; Normally called from normalise.pro and elastic.pro
;
;ARGUMENTS
; xmin	:min limit of fit range
; xmax	:max limit of fit range
; par	:output of fitted Gaussian parameters :-- par(0) - flat background
;						  par(1) - height
;						  par(2) - centre
;						  par(3) - FWHM (sigma)
; dpar	:errors in fitted parameters
; ispec	:spectrum number (used by elastic.pro)
;
;KEYWORDS
; /all	: keeps all points in range xmin<x<xmax.
; 	  otherwise, Gaussian fitted in range x0-FWHM<x<x0+FWHM (default)
; /plot : plot data and fit while executing
;
;DIMENSIONS
; x_in, y_in and e_in ar all 1D arrays of the same dimension
;
;COMMAND SYNTAX
; fitgauss, x1, y1, e1, ,<min> ,<max> [,<ispec>][,/all][,/plot]
;
; (optional keywords/args shown in sqaure brackets) 
;
;						KHA,JRS 8/8/00
;----------------------------------------------------------------------------
;****************************************************************************

	PRO gaussian, x, par, gauss, PD

	bkgd=par(0)
	height=par(1)
	centre=par(2)
	sigma=par(3)

	peak=EXP(-0.5*((x-centre)/sigma)^2)
	gauss=bkgd+height*peak

	IF (N_PARAMS() GE 4) THEN BEGIN
		dfda=0.*x+1.
		dfdb=peak
		dfdc=height*peak*(x-centre)/sigma^2
		dfdd=height*peak*(x-centre)^2/sigma^3
		PD=[[dfda],[dfdb],[dfdc],[dfdd]]
	ENDIF

	END

;----------------------------------------------------------------------------
;****************************************************************************

	PRO fitgauss, x_in, y_in, e_in, xmin, xmax, par, dpar, ispec, all=all, $
		      plot=plot

	iprint=0
	IF KEYWORD_SET(plot) THEN iplot=1 ELSE iplot = 0

	IF (iprint GT 0) THEN PRINT,'Start fitgauss:'

	sx=SIZE(x_in)
	npts=sx(1)

	IF (N_ELEMENTS(ispec) LT 1) THEN ispec=0
	IF KEYWORD_SET(all) THEN allpoints=1 ELSE allpoints=0
	IF (iprint GT 0) THEN PRINT,'npts=',npts

;---------------------------------------------------------------------

	IF (iprint GT 0) THEN PRINT,'Starting x-range extraction section'

	i=WHERE(x_in GE xmin AND x_in LE xmax,ndat)
	xdat=x_in(i)
	ydat=y_in(i)
	edat=e_in(i)
	i1=WHERE(edat EQ 0.,n1)
	IF (n1 GE 1) THEN BEGIN
		i2=WHERE(edat GT 0.,n2)
		IF (n2 LE 0) THEN BEGIN
			par=[0.,0.,(xmin+xmax)/2.,xmax-xmin]
			dpar=[0.,0.,xmax-xmin,xmax-xmin]
			GOTO, finished
		ENDIF
		emin=MIN(edat(i2))
		edat(i1)=emin
	ENDIF

	bkgd=(ydat(0)+ydat(ndat-1))/2.
	height=MAX(ydat,imax)
	IF (imax EQ ndat-1 OR imax EQ 0) THEN notenough=1 ELSE notenough=0
	centre=xdat(imax)
	hh=(height-bkgd)/2.+bkgd
	hh1=WHERE(ydat(0:imax) LT hh, n)
	IF (n EQ 0) THEN i1=0 ELSE i1=MAX(hh1)
	hh2=WHERE(ydat(imax:ndat-1) LT hh, n)
	IF (n EQ 0) THEN i2=0 ELSE i2=MIN(hh2)	& i2=i2+imax
	FWHM=xdat(i2)-xdat(i1+1)

	IF (allpoints EQ 0) THEN BEGIN
		i=WHERE(ABS(xdat-centre) LE FWHM, n)
		IF (n LE 4) THEN BEGIN
			n=5
			i=[imax-2,imax-1,imax,imax+1,imax+2]
			IF (i(0) LT 0) THEN i=i+1
			IF (i(0) LT 0) THEN i=i+1
			IF (i(4) GE ndat) THEN i=i-1
			IF (i(4) GE ndat) THEN i=i-1
			FWHM=xdat(i(4))-xdat(i(0))
			centre=xdat(i(2))
		ENDIF
		ndat=n
		xdat=xdat(i)
		ydat=ydat(i)
		edat=edat(i)
	ENDIF
	wdat=1./edat^2

	IF (iplot GT 0) THEN BEGIN
		PLOT, xdat, ydat, PSYM=4, SYMSIZE=0.4, $
			TITLE='Spectrum '+STRING(STRTRIM(ispec,2))
		ERRPLOT, xdat, ydat-edat/2., ydat+edat/2.
	ENDIF

;---------------------------------------------------------------------

	IF (iprint GT 0) THEN PRINT,'Starting fitting section'

	par=FLTARR(4)	& dpar=par
	par(0)=bkgd	& par(1)=height	& par(2)=centre	& par(3)=FWHM/2.355

	IF (iplot GT 0) THEN BEGIN
		dx=(xmax-xmin)/1000.
		xplot=xmin+FLOAT(INDGEN(1000))*dx
		gaussian, xplot, par, ystart
		OPLOT, xplot, ystart, LINE=1
	ENDIF

	ymean=TOTAL(ydat,1)/FLOAT(ndat)
	i=WHERE(ABS(ydat-ymean) GT 2.*edat, n)
	IF (n LE 1 OR notenough EQ 1) THEN BEGIN
		PRINT,'fitgauss: Error - peak not found in spectrum',ispec
		dpar(*)=-1.
	ENDIF ELSE yfit=CURVEFIT(xdat,ydat,wdat,par,dpar,FUNCTION_NAME='gaussian')

	IF (iplot GT 0) THEN BEGIN
		gaussian, xplot, par, yfit
		OPLOT, xplot, yfit, LINE=0
		PRINT,'Hit return to continue'	& wait=''
		READ, wait
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of fitting section'

;---------------------------------------------------------------------

	IF (iprint GT 0) THEN BEGIN
		pname=STRARR(4)
		pname(0)='Background'
		pname(1)='Height'
		pname(2)='Centre'
		pname(3)='Sigma'
		FOR i=0,3 DO PRINT, pname(i), par(i), dpar(i), $
			FORMAT='(A10," =",F7.2," +/-",F6.2)'
	ENDIF

finished:
	RETURN
	END
