; $Id: ploterr.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO PLOTERR,X,Y,ERR, psym = PSYM, type = TYPE
;
;+
; NAME:
;	PLOTERR
;
; PURPOSE:
;	Plot data points with accompanying error bars.
;	(See also OPLOTERR.)	
;
; CATEGORY:
;	Plotting, two-dimensional.
;
; CALLING SEQUENCE:
;	PLOTERR, [ X ,]  Y , Err [, PSYM = Psym] [, TYPE = Type]
;
; INPUTS:
;	X:	The array of abcissae.
;
;	Y:	The array of Y values.
;
;	Err:	The array of error-bar values.
;
; OPTIONAL KEYWORD PARAMETERS:
;	PSYM:	The plotting symbol to use.  The default is +7.
;
;	ITYPE:	The type of plot to be produced.  The possible types are:
;			ITYPE = 0 :	X Linear - Y Linear (default)
;			ITYPE = 1 :	X Linear - Y Log
;			ITYPE = 2 :	X Log    - Y Linear
;			ITYPE = 3 :	X Log    - Y Log
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Arrays cannot be of type string.  There must be enough points to
;	plot.
;
; PROCEDURE:
;	A plot of X versus Y with error bars drawn from Y - ERR to Y + ERR
;	is written to the output device.
;
; MODIFICATION HISTORY:
;	William Thompson	Applied Research Corporation
;	July, 1986		8201 Corporate Drive
;				Landover, MD  20785
;
;	DMS, April, 1989	Modified for Unix.
;-
;
;P_SYM = !PSYM		; Save the affected system parameters
;Y_MIN = !YMIN
;Y_MAX = !YMAX
;BANG_C = !C
;LINETYPE = !LINETYPE
;
;  Interpret the input parameters.
;
if n_elements(type) eq 0 then type = 0
if n_elements(psym) eq 0 then psym = 7

ON_ERROR,2
NP = N_PARAMS(0)
IF NP LT 2 THEN BEGIN
	message,'Must be called with 2-5 parameters: [X,] Y, ERR [,PSYM [,ITYPE]]'
	RETURN
ENDIF ELSE IF NP EQ 2 THEN BEGIN	;Only Y and ERR passed.
	YERR = ABS(Y)
	YY = X
	XX = INDGEN(N_ELEMENTS(YY))
ENDIF ELSE BEGIN
	YERR = ABS(ERR)
	YY = Y
	XX = X
ENDELSE
;
N = N_ELEMENTS(XX) < N_ELEMENTS(YY) < N_ELEMENTS(YERR)
IF N LT 2 THEN message, 'Not enough points to plot.'

XX = XX(0:N-1)
YY = YY(0:N-1)
YERR = YERR(0:N-1)
YLO = yy - yerr
YHI = yy + yerr
;	Set yrange if not already set
if !y.range(0) eq !y.range(1) then $	;yrange specified?
    yrange = [ min(ylo), max(yhi) ] $
 else yrange = !y.range
;
plot,xx,yy,xtype = type/2, ytype = type and 1, yrange = yrange, psym=psym
;
;  Plot the error bars.
;
FOR I = 0,N-1 DO plots,[xx(i),xx(i)], [ylo(i), yhi(i)]
RETURN
END

