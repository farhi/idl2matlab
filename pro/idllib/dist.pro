; $Id: dist.pro,v 1.2 1994/04/28 18:14:15 doug Exp $

function dist,n,m  ;Return a rectangular array in which each pixel = euclidian
		;distance from the corner.
;+
; NAME:
;	DIST
;
; PURPOSE:
;	Create a rectangular array in which each element is proportional
;	to its frequency.  This array may be used for a variety
;	of purposes, including frequency-domain filtering and
;	making pretty pictures.
;
; CATEGORY:
;	Signal Processing.
;
; CALLING SEQUENCE:
;	Result = DIST(N [, M])
;
; INPUTS:
;	N = number of columns in result.
;	M = number of rows in result.  If omitted, N is used to return
;		a square array.
;
; OUTPUTS:
;	Returns an (N,M) floating array in which:
;
;	R(i,j) = SQRT(F(i)^2 + G(j)^2)   where:
;		 F(i) = i  IF 0 <= i <= n/2
;		      = n-i  IF i > n/2
;		 G(i) = i  IF 0 <= i <= m/2
;		      = m-i  IF i > m/2
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.  The computation is done a row at a time.
;
; MODIFICATION HISTORY:
;	Very Old.
; 	SMR, March 27, 1991 - Added the NOZERO keyword to increase efficiency.
;				(Recomended by Wayne Landsman)
;	DMS, July, 1992.  - Added M parameter to make non-square arrays.
;-
on_error,2              ;Return to caller if an error occurs
x=findgen(n)		;Make a row
x = (x < (n-x)) ^ 2	;column squares
if n_elements(m) le 0 then m = n

a = FLTARR(n,m,/NOZERO)	;Make array

for i=0L, m/2 do begin	;Row loop
	y = sqrt(x + i^2) ;Euclidian distance
	a(0,i) = y	;Insert the row
	if i ne 0 then a(0, m-i) = y ;Symmetrical
	endfor
return,a
end


