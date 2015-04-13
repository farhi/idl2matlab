; $Id: digital_filter.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION DIGITAL_FILTER,FLOW,FHIGH,A,NTERMS
;+
; NAME:
;	DIGITAL_FILTER
;
; PURPOSE:
;	Compute the coefficients of a non-recursive, digital
;	filter.  Highpass, lowpass, bandpass and bandstop
;	filters may be constructed with this function.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Coeff = DIGITAL_FILTER(Flow, Fhigh, A, Nterms)  ;To get coefficients.
;
;	Followed by:
;
;	Yout  = CONVOL(Yin, Coeff)	;To apply the filter.
;
; INPUTS:
;	Flow:	The lower frequency of the filter as a fraction of the Nyquist
;		frequency.
;
;	Fhigh:	The upper frequency of the filter as a fraction of the Nyquist
;		frequency.
;
;	A:	The size of Gibbs phenomenon wiggles in -db.  50 is a good 
;		choice.
;
;	Nterms:	The number of terms in the filter formula.  The order
;		of filter.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;		The following conditions are necessary for various types of
;		filters:
;
;		No Filtering:	Flow = 0, Fhigh = 1.
;		Low Pass:	Flow = 0, 0 < Fhigh < 1.
;		High Pass:	0 < Flow < 1, Fhigh =1.
;		Band Pass:	0 < Flow < Fhigh < 1.
;		Band Stop:	0 < Fhigh < Flow < 1.
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;	Returns a vector of coefficients with (2*nterms + 1) elements.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	This function returns the coefficients of a non-recursive,
;	digital filter for evenly spaced data points.  Frequencies are
;	expressed in terms of the Nyquist frequency, 1/2T, where T
;	is the time between data samples.
;
; MODIFICATION HISTORY:
;	DMS, April, 1985.
;	Adapted from:
;	"Digital Filters", Robert Walraven, 
;	Proceedings of the Digital Equipment User's Society, Fall, 1984.
;	Department of Applied Science, 
;	University of California, Davis, CA 95616.
;	
;-
;
ON_ERROR,2              ;Return to caller if an error occurs
PI = 3.14159265
IF (FHIGH LT FLOW) THEN STOP = 1. ELSE STOP = 0.
;
;	computes Kaiser weights W(N,K) for digital filters.
; W = COEF = returned array of Kaiser weights
; N = value of N in W(N,K), i.e. number of terms
; A = Size of gibbs phenomenon wiggles in -DB.
;
IF (A LE 21.) THEN ALPHA = 0. $
	ELSE IF (A GE 50.) THEN ALPHA = 0.1102 *(A-8.7)  $
	ELSE ALPHA = 0.5842*(A-21.)^0.4 + 0.07886*(A-21.)
;
ARG = (FINDGEN(NTERMS)+1.)/NTERMS
COEF = BESELI(ALPHA*SQRT(1.-ARG^2),0)/BESELI(ALPHA,0)
T = (FINDGEN(NTERMS)+1)*PI
COEF = COEF * (SIN(T*FHIGH)-SIN(T*FLOW))/T
COEF = [REVERSE(COEF),FHIGH-FLOW+STOP,COEF] ;REPLICATE IT
RETURN,COEF
END
