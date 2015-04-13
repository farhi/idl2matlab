; $Id: chebyshev.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION CHEBYSHEV,D,N
;+
; NAME:
;	CHEBYSHEV
;
; PURPOSE:
;	Implements forward and reverse Chebyshev polynomial expansion of
;	a set of data.
;
; CATAGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = CHEBYSHEV(D, N)
;
; INPUT:
;	D:  A vector containing the values at the zeros of Chebyshev
;	    polynomial.
;
;	N:  A flag that, if set to -1, returns a set of Chebyshev polynomials.
;	    If set to +1, the original data is returned.
;
; OUTPUT:
;	Returns either the set of Chebyshev polynomials or the original
;	data depending on the value of N.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Results from this function are subject to roundoff error given
;	discontinuous data.
;
; RESTRICTIONS:                                           
;	Unknown.
;
; PROCEDURE:
;	Straightforward implementation of recursion formula. 
;
; REVISION HISTORY:
;	Jan, 1986  Written by Leonard Kramer, U. of Maryland
;		   University Research Foundation
;-	
	ON_ERROR,2              ;Return to caller if an error occurs	
	IF (N_PARAMS(0) NE 2 ) THEN BEGIN
	  PRINT,'two parameters required'
          STOP
	ENDIF
 	UNITY =(ABS(D(0))+1.)/(ABS(D(0))+1.)
        NL=N_ELEMENTS(D)
	NE2=(NL-1.)/2.
	C=TRANSPOSE([TRANSPOSE(D),TRANSPOSE(D)])
	C(*,0)=1.
	X = cos(!Pi * (findgen(NL) + .5)/Nl)
	C(0,1)=X
	T=D
        CASE N OF 
	-1: BEGIN
          FOR I = 0, 1 DO T(I) = 2 * TOTAL(D * C(*,I))/NL
  	  FOR I=2,NL-1 DO BEGIN
	    SAVE=C(*,1)
	    C(0,1)= 2.*X*C(*,1)-C(*,0)
	    C(0,0)=SAVE
            T(I) = 2 * TOTAL( D * C(*, 1))/NL
	  ENDFOR
	  END
	1: BEGIN
          T = fltarr(NL) - .5 * D(0)
	  FOR I=0,1 DO T = T + D(I) * C(*,I)
	  FOR I=2,NL-1 DO BEGIN
	    SAVE=C(*,1)
	    C(0,1)= 2.*X*C(*,1)-C(*,0)
	    C(0,0)=SAVE
	    T=T+D(I)*C(*,1)
	  ENDFOR
	  END
	ELSE:
	ENDCASE
	RETURN,T
END

