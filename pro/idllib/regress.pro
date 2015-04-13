; $Id: regress.pro,v 1.2 1993/10/26 23:29:57 doug Exp $

FUNCTION REGRESS,X,Y,W,YFIT,A0,SIGMA,FTEST,R,RMUL,CHISQ, $
                 RELATIVE_weight=relative_weight
;+
; NAME:
;	REGRESS
;
; PURPOSE:
;	Perform a multiple linear regression fit.
;
;	REGRESS fits the function:
;		Y(i) = A0 + A(0)*X(0,i) + A(1)*X(1,i) + ... + 
;                      A(Nterms-1)*X(Nterms-1,i)
;
; CATEGORY:
;       G2 - Correlation and regression analysis.
;
; CALLING SEQUENCE:
;	Result = REGRESS(X, Y, W, Yfit, A0, Sigma, Ftest, R, Rmul, Chisq)
;
; INPUTS:
;       X:	The array of independent variable data.  X must 
;		be dimensioned as an array of Nterms by Npoints, where 
;		there are Nterms coefficients (independent variables) to be 
;		found and Npoints of samples.
;
;       Y:	The vector of dependent variable points.  Y must have Npoints 
;		elements.
;
;	W:	The vector of weights for each equation.  W must be a vector
;		of Npoints elements.  For instrumental weighting, 
;		W(i) = 1/standard_deviation(Y(i))^2.  For statistical 
;		weighting, w(i) = 1./Y(i).  For no weighting, set w(i)=1,
;		and also set the RELATIVE_WEIGHT keyword.
;
; OUTPUTS:
;	REGRESS returns a column vector of coefficients that has Nterms 
;	elements.
;
; OPTIONAL OUTPUT PARAMETERS:
;	Yfit:	Vector of calculated values of Y with Npoints elements.
;
;	A0:	Constant term.
;
;	Sigma:	Vector of standard deviations for coefficients.
;
;	Ftest:	The value of F for test of fit.
;
;	Rmul:	The multiple linear correlation coefficient.
;
;	R:	Vector of linear correlation coefficients.
;
;	Rmul:   The multiple linear correlation coefficient.
;
;	Chisq:	Reduced, weighted chi squared.
;
; KEYWORDS:
; RELATIVE_WEIGHT:  If this keyword is set, the input weights
;		(W vector) are assumed to be relative values, and not based
;		on known uncertainties in the Y vector.  Set this keyword in 
;		the case of no weighting, W(*) = 1.
;
; PROCEDURE:
;	Adapted from the program REGRES, Page 172, 
;	Bevington, Data Reduction and Error Analysis for the 
;	Physical Sciences, 1969.
;
; MODIFICATION HISTORY:
;	Written, DMS, RSI, September, 1982.
;	Added RELATIVE_WEIGHT keyword    W. Landsman   August 1991
;       Fixed bug in invert  Bobby Candey 1991 April 22
;-
;
On_error,2              ;Return to caller if an error occurs 
SY = SIZE(Y)            ;Get dimensions of x and y.  
SX = SIZE(X)
IF (N_ELEMENTS(W) NE SY(1)) OR (SX(0) NE 2) OR (SY(1) NE SX(2)) THEN $
  message, 'Incompatible arrays.'
;
NTERM = SX(1)           ;# OF TERMS
NPTS = SY(1)            ;# OF OBSERVATIONS
;
SW = TOTAL(W)           ;SUM OF WEIGHTS
YMEAN = TOTAL(Y*W)/SW   ;Y MEAN
XMEAN = (X * (REPLICATE(1.,NTERM) # W)) # REPLICATE(1./SW,NPTS)
WMEAN = SW/NPTS
WW = W/WMEAN
;
NFREE = NPTS-1          ;DEGS OF FREEDOM
SIGMAY = SQRT(TOTAL(WW * (Y-YMEAN)^2)/NFREE) ;W*(Y(I)-YMEAN)
XX = X- XMEAN # REPLICATE(1.,NPTS)      ;X(J,I) - XMEAN(I)
WX = REPLICATE(1.,NTERM) # WW * XX      ;W(I)*(X(J,I)-XMEAN(I))
SIGMAX = SQRT( XX*WX # REPLICATE(1./NFREE,NPTS)) ;W(I)*(X(J,I)-XM)*(X(K,I)-XM)
R = WX #(Y - YMEAN) / (SIGMAX * SIGMAY * NFREE)
ARRAY = (WX # TRANSPOSE(XX))/(NFREE * SIGMAX #SIGMAX)
IF (SX(1) EQ 1) THEN ARRAY = 1 / ARRAY ELSE ARRAY = INVERT(Array)
A = (R # ARRAY)*(SIGMAY/SIGMAX)         ;GET COEFFICIENTS
YFIT = A # X                            ;COMPUTE FIT
A0 = YMEAN - TOTAL(A*XMEAN)             ;CONSTANT TERM
YFIT = YFIT + A0                        ;ADD IT IN
FREEN = NPTS-NTERM-1 > 1                ;DEGS OF FREEDOM, AT LEAST 1.
CHISQ = TOTAL(WW*(Y-YFIT)^2)*WMEAN/FREEN ;WEIGHTED CHI SQUARED
IF KEYWORD_SET(relative_weight) then varnce = chisq $
                                else varnce = 1./wmean
sigma = sqrt(array(indgen(nterm)*(nterm+1))*varnce/(nfree*sigmax^2)) ;Error term
RMUL = TOTAL(A*R*SIGMAX/SIGMAY)         ;MULTIPLE LIN REG COEFF
IF RMUL LT 1. THEN FTEST = RMUL/NTERM / ((1.-RMUL)/FREEN) ELSE FTEST=1.E6
RMUL = SQRT(RMUL)
RETURN,A
END
