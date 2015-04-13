;$Id: determ.pro,v 1.6 1994/11/29 18:35:29 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       DETERM
;
; PURPOSE:
;       This function computes the determinant of an N by N array.
;
; CATEGORY:
;       Linear Algebra.
;
; CALLING SEQUENCE:
;       Result = DETERM(A)
;
; INPUTS:
;       A:      An N by N array of type: float, or double.
;
; KEYWORD PARAMETERS:
;       CHECK:  If set to a non-zero value, A is checked for singularity.
;               The determinant of a singular array is returned as zero if
;               this keyword is set. Run-time errors may result if A is
;               singular and this keyword is not set.
;
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
;       ZERO:   Use this keyword to set the value of floating-point
;               zero. A floating-point zero on the main diagonal of
;               a triangular matrix results in a zero determinant.
;               For single-precision inputs, the default value is 
;               1.0e-6. For double-precision inputs, the default value 
;               is 1.0e-12.
;
; EXAMPLE:
;       Define an array (a).
;         a = [[ 2.0,  1.0,  1.0], $
;              [ 4.0, -6.0,  0.0], $
;              [-2.0,  7.0,  2.0]]
;       Compute the determinant.
;         result = determ(a)
;       Note:
;            See CRAMER.PRO, in the same directory as this file, for
;            an application of the determinant function.
;
; PROCEDURE:
;       LU decomposition is used to represent the input array in
;       triangular form. The determinant is computed as the product
;       of diagonal elements of the triangular form. Row interchanges
;       are tracked during the LU decomposition to ensure the correct   
;       sign, + or - .
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, February 1994
;       Modified:    GGS, RSI, November 1994
;                    Added CHECK keyword to check for singular arrays.
;                    Changed NR_LUDCMP to LUDC.
;-

function determ, a, check = check, double = double, zero = zero

  on_error, 2  ;Return to caller if error occurs.

  dim = size(a)
  if dim(1) ne dim(2) then $
    message, 'Input matrix is not square.'

  if dim(3) ne 4 and dim(3) ne 5 then $
    message, 'Input matrix must be float or double.'

  dbl = 0
  if keyword_set(double) ne 0 then dbl = 1
  
  if keyword_set(zero) eq 0 and dim(3) eq 4 then $
    zero = 1.0e-6             ;Single-precision zero.
  if keyword_set(zero) eq 0 and dim(3) eq 5 then $
    zero = 1.0e-12            ;Double-precision zero.

  det = 1.0 ;Initialize determinant.

  ;a = transpose(a) Transposing into column format prior to calling
                    ;nr_ludcmp is unneccesary since the determinant
                    ;is unaffected by the transpose operation.
  
  alud = a ;Make a copy of the array for its LU decomposition.

  if keyword_set(check) then begin ;Return a determinant of zero?
    if cond(a) eq -1 then return, 0.0
  endif

  ;Compute LU decomposition.
  ;nr_ludcmp, alud, index, double = dbl, interchanges = sign
  ludc, alud, index, double = dbl, interchanges = sign

  ;Are there any floating-point zeros on the main diagonal?
  ii = where( abs( alud(indgen(dim(1))*(dim(1)+1)) ) le zero, cnt)

  if cnt ne 0 then return, 0 $ ;A zero on the main diagonal results
  else begin                   ;in a zero determinant.
    for k = 0, dim(1)-1 do $
      det = det * alud(k,k)
    return, sign * det
  endelse

end
