; $Id: poly_fit.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION POLY_FIT,X,Y,NDEGREE,YFIT,YBAND,SIGMA,A
;+
; NAME:
;	POLY_FIT
;
; PURPOSE:
;	Perform a least-square polynomial fit with optional error estimates.
;
;	This routine uses matrix inversion.  A newer version of this routine,
;	SVDFIT, uses Singular Value Decomposition.  The SVD technique is more
;	flexible, but slower.
;
;	Another version of this routine, POLYFITW, performs a weighted
;	least square fit.
;
; CATEGORY:
;	Curve fitting.
;
; CALLING SEQUENCE:
;	Result = POLY_FIT(X, Y, NDegree [,Yfit, Yband, Sigma, A] )
;
; INPUTS:
;	X:	The independent variable vector.
;
;	Y:	The dependent variable vector, should be same length as x.
;
;     NDegree:	The degree of the polynomial to fit.
;
; OUTPUTS:
;	POLY_FIT returns a vector of coefficients with a length of NDegree+1.
;
; OPTIONAL OUTPUT PARAMETERS:
;	Yfit:	The vector of calculated Y's.  These values have an error 
;		of + or - Yband.
;
;	Yband:	Error estimate for each point = 1 sigma
;
;	Sigma:	The standard deviation in Y units.
;
;	A:	Correlation matrix of the coefficients.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; MODIFICATION HISTORY:
;	Written by: George Lawrence, LASP, University of Colorado,
;		December, 1981.
;
;	Adapted to VAX IDL by: David Stern, Jan, 1982.
;
;-
	ON_ERROR,2		;RETURN TO CALLER IF ERROR
	XX = X*1.		;BE SURE X IS FLOATING OR DOUBLE
	N = N_ELEMENTS(X) 	;SIZE
	IF N NE N_ELEMENTS(Y) THEN $
	  message,'X and Y must have same # of elements'
;
	M = NDEGREE + 1			;# OF ELEMENTS IN COEFF VEC.
;
	A = DBLARR(M,M)		;COEFF MATRIX
	B = DBLARR(M)		;WILL CONTAIN SUM Y * X^J
	Z = DBLARR(N)+1.
;
	A(0,0) = N
	B(0) = TOTAL(Y)
;
	FOR P = 1,2*NDEGREE DO BEGIN ;POWER LOOP.
		Z=Z*XX			;Z IS NOW X^P
		IF P LT M THEN B(P) = TOTAL(Y*Z) ;B IS SUM Y*XX^J
		SUM = TOTAL(Z)
		FOR J= 0 > (P-NDEGREE), NDEGREE < P DO A(J,P-J) = SUM
	  END			;END OF P LOOP.
;
	A = INVERT(A)		;INVERT MATRIX.
;
;			IF A IS MULTIPLIED BY SIGMA SQUARED, IT IS THE
;			CORRELATION MATRIX.
;
	C = float(b) # a	;Get coefficients

;
	IF (N_PARAMS(0) LE 3) THEN RETURN,C	;EXIT IF NO ERROR ESTIMATES.
;
	YFIT = FLTARR(N)+C(0)	;INIT YFIT
	FOR K = 1,NDEGREE DO YFIT = YFIT + C(K)*(XX^K) ;FORM YFIT.
;
	IF (N_PARAMS(0) LE 4) THEN RETURN,C	;EXIT IF NO ERROR ESTIMATES.
;
	IF N GT M THEN $
		SIGMA = TOTAL((YFIT-Y) ^ 2) / (N-M) $	;COMPUTE SIGMA
	   ELSE	SIGMA = 0.
;
	A=A* SIGMA		;GET CORREL MATRIX
;
	SIGMA = SQRT(SIGMA)
	YBAND = FLTARR(N)+ A(0,0)	;SQUARED ERROR ESTIMATES
;
	FOR P = 1,2*NDEGREE DO BEGIN
	  Z = XX ^ P
	  SUM = 0.
	  FOR J=0 > (P - NDEGREE), NDEGREE < P DO SUM = SUM + A(J,P-J)
	  YBAND = YBAND + SUM * Z ;ADD IN ERRORS.
	END		;END OF P LOOP
	YBAND = SQRT(ABS(YBAND))	;ERROR ESTIMATES
	RETURN,C
END
