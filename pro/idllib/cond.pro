;$Id: cond.pro,v 1.7 1995/01/05 18:16:55 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       COND
;
; PURPOSE:
;       This function computes the condition number of an N by N array.
;
; CATEGORY:
;       Complex Linear Algebra.
;
; CALLING SEQUENCE:
;       Result = COND(A)
;
; INPUTS:
;       A:      An N by N real or complex array.
;
; KEYWORD PARAMETERS:
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
; EXAMPLE:
;       Define a complex array (a).
;         a = [[complex(1, 0), complex(2,-2), complex(-3,1)], $
;              [complex(1,-2), complex(2, 2), complex(1, 0)], $
;              [complex(1, 1), complex(0, 1), complex(1, 5)]]
;       Compute the condition number of the complex array (a) using 
;       double-precision complex arithmetic.
;         result = cond(a, /double)
;         
; PROCEDURE:
;       This function returns the condition number of an N x N real or
;       complex array (A) by explicitly computing, norm(A)*norm(A_inverse).
;       If A is real and A_inverse is invalid (due to the singularity of A 
;       or floating-point errors in the nr_invert function), the condition 
;       number is returned as a -1. If A is complex and A_inverse is invalid
;       (due to the singularity of A), calling COND.PRO results in floating-
;       point errors.
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, April 1992
;       Modified:    GGS, RSI, February 1994
;                    Accepts complex inputs. Added DOUBLE keyword.
;       Modified:    GGS, RSI, November 1994
;                    Added support for double-precision complex inputs.
;                    Changed NR_INVERT to INVERT.
;-

function cond, a, double = double

  on_error, 2  ; Return to caller if error occurs.

  dimension = size(a)
  if dimension(1) ne dimension(2) then message, $
    'Input array is not square.'

  if keyword_set(double) eq 0 then dbl = 0 $
  else dbl = 1

  if dimension(3) ne 6 and dimension(3) ne 9 then begin ;Real matrix.
    ;inv = nr_invert(transpose(a), s, double = dbl)
    inv = invert(a, s, double=dbl)
    if s ne 0 then return, -1 $ ;Check status of inverse.
    else return, norm(a) * norm(inv)
  endif else $ ;Complex matrix.
    return, norm(a) * norm(lu_complex(a, -1, double = dbl))

  inv = 0

end
