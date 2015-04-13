; $Id: bilinear.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION BILINEAR,P,IX,JY
;+
; NAME:
;	BILINEAR
;
; PURPOSE:
;	Bilinearly interpolate a set of reference points.
;
; CALLING SEQUENCE:
;	Result = BILINEAR(P, IX, JY)
;
; INPUTS:                 
;	P:  A two-dimensional data array.
;
;	IX and JY:  The "virtual subscripts" of P to look up values
;	  for the output.
;  
;	IX can be one of two types:
;	     1)	A one-dimensional, floating-point array of subscripts to look
;		up in P.  The same set of subscripts is used for all rows in
;		the output array.
;	     2)	A two-dimensional, floating-point array that contains both 
;		"x-axis" and "y-axis" subscripts specified for all points in
;		the output array.
;
;	In either case, IX must satisfy the expression,
;		    0 <= MIN(IX) < N0  and 0 < MAX(IX) <= N0
;	where N0 is the total number of subscripts in the first dimension
;	of P.
;
;	JY can be one of two types:
;	     1) A one-dimensional, floating-point array of subscripts to look
;		up in P.  The same set of subscripts is used for all rows in
;		the output array.
;	     2) A two-dimensional, floating-point array that contains both
;               "x-axis" and "y-axis" subscripts specified for all points in
;               the output array.
;
;	    In either case JY must satisfy the expression,
;		    0 <= MIN(JY) < M0  and 0 < MAX(JY) <= M0
;	    where M0 is the total number of subscripts in the second dimension
;	    of P.
;
;  	It is better to use two-dimensional arrays for IX and JY when calling
;  	BILINEAR because the algorithm is somewhat faster.  If IX and JY are 
;  	one-dimensional, they are converted to two-dimensional arrays on
;  	return from the function.  The new IX and JY can be re-used on 
;	subsequent calls to take advantage of the faster, 2D algorithm.  The 
;	2D array P is unchanged upon return.
;
; OUTPUT:
;	The two-dimensional, floating-point, interpolated array.  
;
; SIDE EFFECTS:
;	This function can take a long time to execute.
;
; RESTRICTIONS:
;	None.
;
; EXAMPLE:
;	Suppose P = FLTARR(3,3), IX = [.1, .2], and JY = [.6, 2.1] then
;	the result of the command:
;		Z = BILINEAR(P, IX, JY)
;	Z(0,0) will be returned as though it where equal to P(.1,.6) 
;	interpolated from the nearest neighbors at P(0,0), P(1,0), P(1,1)
;	and P(0,1).
;
; PROCEDURE:
;	Uses bilinear interpolation algorithm to evaluate each element
;	in the result  at virtual coordinates contained in IX and JY with 
;	the data in P.                                                          
;
; REVISION HISTORY:
;       Nov. 1985  Written by L. Kramer (U. of Maryland/U. Res. Found.)
;	Aug. 1990  TJA simple bug fix, contributed by Marion Legg of NASA Ames
;	Sep. 1992  DMS, Scrapped the interpolat part and now use INTERPOLATE
;-
	ON_ERROR,2              ;Return to caller if an error occurs	
	IF((N_ELEMENTS(IX) EQ 0) AND (N_ELEMENTS(JY) EQ 0)) THEN BEGIN
	  I=FIX(IX) & J=FIX(JY) & IP=I+1 & JP=J+1
	  DX=IX-FLOAT(I) & DY=JY-FLOAT(J)
	  DX1=(1.-DX) & DY1=(1.-DY) 
	  RETURN,( P(I,J)*DX1*DY1 + P(I,JP)*DX1*DY $
	  	+ P(IP,J)*DX*DY1 + P(IP,JP)*DX*DY)
	ENDIF

	A=SIZE(IX)  & B=SIZE(JY)
	NX=A(1)
	IF(B(0) EQ 1) THEN BEGIN
	  NY=B(1)
	ENDIF ELSE BEGIN
	  NY=B(2)
	ENDELSE                                                   
        IF(A(0) EQ 1) THEN BEGIN
	  TEMP=IX
	  IX=FLTARR(NX,NY)
	  FOR I=0,NY-1 DO IX(0,I)=TEMP
	ENDIF
	IF(B(0) EQ 1) THEN BEGIN
	  TEMP=JY
	  JY=FLTARR(NY,NX)
	  FOR I=0,NX-1 DO JY(0,I)=TEMP
	  JY=TRANSPOSE(JY)
	ENDIF
	return, interpolate(p, ix, jy)	;Use new interpolate function
;	I=FIX(IX) & J=FIX(JY)
;	IP=I+1   &  JP=J+1
;	DX=IX-I & DY=JY-J
;	DX1=1.-DX & DY1=1.-DY
;	Z=FLTARR(N_ELEMENTS(I(*,0)),N_ELEMENTS(J(0,*)))
;	NUMX=N_ELEMENTS(I)
;	PZ=FLTARR(N_ELEMENTS(P(*,0))+1,N_ELEMENTS(P(0,*))+1)
;	PZ(0,0)=P(0:*,0:*)
;	FOR N=0L,NUMX-1 DO BEGIN
;	  Z(N)=  PZ(I(N), J(N)) *DX1(N)*DY1(N)	$
;	+        PZ(I(N),JP(N)) *DX1(N)*DY(N)	$
;	+        PZ(IP(N),J(N)) *DX(N) *DY1(N)	$
;	+        PZ(IP(N),JP(N))*DX(N) *DY(N)
;	ENDFOR 
;	RETURN,Z
END			
