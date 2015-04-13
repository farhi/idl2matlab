; $Id: reverse.pro,v 1.2 1994/09/16 21:12:24 dave Exp $

function reverse, a, subscript	;Reverse a vector or array about the subscript
; subscript = 1 or omitted to reverse 1st dim, =2 for 2nd dim.
; This function simply calls rotate with the correct parameter.
;+
; NAME:
;	REVERSE
;
; PURPOSE:
;	Reverse the order of rows or columns in an array or vector.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = REVERSE(Array [, Subscript_Index])
;
; INPUTS:
;	Array:	The array or vector containing the original data.
;
; OPTIONAL INPUT PARAMETERS:
; Subscript_Index:  If this parameter is omitted or 1, the first subscript is
;		reversed (i.e., rows are reversed).  Set this parameter to
;		2 to reverse columns.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	REVERSE returns a copy of the original array that is reversed about 
;	one of its dimensions.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Only works for 1-, 2-, or 3-dimensional arrays.
;
; PROCEDURE:
;	Uses the ROTATE function.
;
; MODIFICATION HISTORY:
;	Old.
;	Apr, 1991, DMS,	Added 3D reversing.
;       Sept, 1992 Mark L. Rivers, added simple return for scaler argument
;	Sept, 1994. Added default for 3D case.
;-

on_error,2                             ;Return to caller if an error occurs
s = size(a)
ndims = s(0)
if ndims eq 0 then return, a
if ndims eq 3 then begin		;3D?
	b = a
	if n_elements(subscript) le 0 then subscript = 1  ;Default case
	case subscript of
	1: begin			;(x,*,*)
		n = s(1)
		for i=0,(n-1)/2 do begin
			t0 = b(i,*,*) & t1 = b(n-1-i,*,*)
			b(n-i-1,0,0) = t0
			b(i,0,0) = t1
			endfor
		endcase
	2: begin			;(*,x,*)
		n = s(2)
		for i=0,(n-1)/2 do begin
			t0 = b(*,i,*) & t1 = b(*,n-1-i,*)
			b(0,n-i-1,0) = t0
			b(0,i,0) = t1
			endfor
		endcase
	3: begin			;(x,*,*)
		n = s(3)
		for i=0,(n-1)/2 do begin
			t0 = b(*,*,i) & t1 = b(*,*,n-i-1)
			b(0,0,n-i-1) = t0
			b(0,0, i) = t1
			endfor
		endcase
	else: message, "REVERSE: Subscript parameter out of range"
	endcase
	return,b
endif			
if n_params() then return, rotate(a,5) ;default = 1st dim.
if subscript eq 1 then return,rotate(a,5) else $
	return, rotate(a,7)
end
