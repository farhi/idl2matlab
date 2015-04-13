; $Id: oploterr.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO OPLOTERR, X, Y, ERR, PSYM
;
;+
; NAME:
;	OPLOTERR
;
; PURPOSE:
;	Overplot data points with accompanying error bars.
;
; CATEGORY:
;	Plotting, 2-dimensional.
;
; CALLING SEQUENCE:
;	OPLOTERR, [ X ,]  Y , Err  [, Psym ]
;
; INPUTS:
;	Y:	The array of Y values.
;
;	Err:	The array of error bar values.
;
; OPTIONAL INPUT PARAMETERS:
;	X:	An optional array of X values.  The procedure checks whether 
;		or not the third parameter passed is a vector to decide if X 
;		was passed.
;		
;		If X is not passed, then INDGEN(Y) is assumed for the X values.
;
;	PSYM:	The plotting symbol to use (default = +7).
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
;	is written to the output device over any plot already there.
;
; MODIFICATION HISTORY:
;	William Thompson	Applied Research Corporation
;	July, 1986		8201 Corporate Drive
;				Landover, MD  20785
;-
;
P_SYM = !PSYM		; Save the affected system parameters
LINETYPE = !LINETYPE
;
;  Interpret the input parameters.
;
ON_ERROR,2              ; Return to caller if an error occurs
NP = N_PARAMS(0)
IF NP LT 2 THEN $
  message, 'Must be called with 2-4 parameters: [ X ,]  Y , ERR  [, PSYM ]' $
 ELSE IF NP EQ 2 THEN BEGIN			;Only Y and ERR passed.
	!PSYM = 7
	YERR = Y
	YY = X
	XX = INDGEN(n_elements(x))
END ELSE IF NP GE 3 THEN BEGIN
	N = N_ELEMENTS(ERR)
	IF N EQ 1 THEN BEGIN			;X array not passed.
		!PSYM = ERR
		YERR = Y
		YY = X
		XX = INDGEN(Y)
	END ELSE BEGIN				;X array passed.
		IF NP EQ 3 THEN !PSYM = 7 $
			ELSE !PSYM = PSYM
		YERR = ERR
		YY = Y
		XX = X
	END
END
;
;  Plot data and the error bars.
;
N = N_ELEMENTS(XX) < N_ELEMENTS(YY) < N_ELEMENTS(YERR)
IF N LT 1 THEN message, 'No points to plot.' $
ELSE IF N EQ 1 THEN BEGIN		;Double XX and YY arrays to allow
	XX = [XX(0),XX(0)]		;	plotting of single point.
	YY = [YY(0),YY(0)]
	YERR = [YERR(0),YERR(0)]
END ELSE BEGIN
	XX = XX(0:N-1)
	YY = YY(0:N-1)
ENDELSE
OPLOT,XX,YY				;Plot data points.
!PSYM = 0
!LINETYPE = 0
FOR I = 0,N-1 DO BEGIN			;Plot error bars.
	XXX = [XX(I),XX(I)]
	YYY = [YY(I)-YERR(I),YY(I)+YERR(I)]
	OPLOT,XXX,YYY
END
;
!PSYM = P_SYM		; Return the orginal system parameter values.
!LINETYPE = LINETYPE
;
RETURN
END

