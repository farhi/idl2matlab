; $Id: swap_endian.pro,v 1.2 1993/10/06 17:44:20 doug Exp $
;
; Copyright (c) 1993, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	SWAP_ENDIAN
;
; PURPOSE:
;	This fucntion reverses the byte ordering of arbitrary scalars,
;	arrays or structures. It may be used, for example, to make little
;	endian numbers big, or big endian numbers little.
;
; CATEGORY:
;	Utility.
;
; CALLING SEQUENCE:
;	Result = SWAP_ENDIAN(A)
;
; INPUTS:
;	A:	The scalar, array, or structure to be swapped.
;
; OUTPUTS:
;	Result:	The same type and structure as the input, with the
;		pertinent bytes reversed.
;
; PROCEDURE:
;	Swap arrays and scalars directly using BYTEORDER.
;	Swap structures recursively.
;
; EXAMPLE:
;	A = SWAP_ENDIAN(A)  ;Reverses the "endianness" of A
;
; MODIFICATION HISTORY:
;	DMS, RSI, May, 1993.	Written.
;-

function swap_endian, in
t = in			;Make a copy
s = size(t)
case s(s(0)+1) of 	;Type code
1: return, t		;don't change bytes
2: byteorder, t, /SSWAP  ;shorts
3: byteorder, t, /LSWAP  ;Longs
4: byteorder, t, /LSWAP  ;single floats
5: BEGIN		;Double, swap longs & then swap even/odds
   n = n_elements(t)
   t = long(temporary(t), 0, 2 * n)  ;Cvt to longs
   byteorder, t, /LSWAP   ;swap each long
   t = t(lindgen(2*n) xor 1L)  ;swap pairs
   return, double(temporary(t), 0, n)  ;make double
   ENDCASE
6: byteorder, t, /LSWAP  ;Complex
7: return, t		;String
8: for i=0, n_tags(t)-1 do t.(i) = swap_endian(t.(i))    ;Structure
ENDCASE
return, t
end

