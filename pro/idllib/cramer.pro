;$Id: cramer.pro,v 1.3 1994/11/29 20:44:18 beth Exp $
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CRAMER
;
; PURPOSE:
;       This function solves an n by n linear system of equations 
;       using Cramer's rule.
;
; CATEGORY:
;       Linear Algebra.
;
; CALLING SEQUENCE:
;       Result = CRAMER(A, B)
;
; INPUTS:
;       A:      An N by N array of type: float, or double.
;
;       B:      An N-element vector of type: float, or double.
;
; KEYWORD PARAMETERS:
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
;       ZERO:   Use this keyword to set the value of floating-point
;               zero. A floating-point zero on the main diagonal of
;               a triangular matrix results in a zero determinant.
;               A zero determinant results in a 'Singular matrix'
;               error and stops the execution of CRAMER.PRO.
;               For single-precision inputs, the default value is 
;               1.0e-6. For double-precision inputs, the default value 
;               is 1.0e-12.
;
; EXAMPLE:
;       Define an array (a).
;         a = [[ 2.0,  1.0,  1.0], $
;              [ 4.0, -6.0,  0.0], $
;              [-2.0,  7.0,  2.0]]
;
;       And right-side vector (b).
;         b = [3.0, 10.0, -5.0]
;
;       Compute the solution of the system, ax = b.
;         result = cramer(a, b)
;
; PROCEDURE:
;       CRAMER.PRO uses ratios of column-wise permutations of the array (a)
;       to calculate the solution vector (x) of the linear system, ax = b.
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, February 1994
;       Modified:    GGS, RSI, November 1994
;                    Added support for double precision results.
;-

function cramer, a, b, double = double, zero = zero

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

  det = determ(a, double = dbl, zero = zero)
  if det eq 0 then $
    message, 'Singular matrix.'

  if dim(3) eq 5 or dbl eq 1 then x = dblarr(dim(1)) $
  else x = fltarr(dim(1))

  for k = 0, dim(1)-1 do begin
    col = a(k,*) ;Save the Kth column of a.
    a(k,*) = b   ;Permute the Kth column of a with b.
    x(k) = determ(a, double = dbl, zero = zero) / det
                 ;Solve for the Kth component of the solution x.
    a(k,*) = col ;Restore a to its original state.
  endfor

  return, x

end
