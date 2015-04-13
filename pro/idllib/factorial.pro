;$Id: factorial.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       FACTORIAL
;
; PURPOSE:
;       This function computes the factorial N! as the double-precision 
;       product, (N) * (N-1) * (N-2) * ...... * 3 * 2 * 1. 
;
; CATEGORY:
;       Special Functions.
;
; CALLING SEQUENCE:
;       Result = Factorial(n)
;
; INPUTS:
;       N:    A non-negative scalar of type integer, float or double.
;
; KEYWORD PARAMETERS:
;       STIRLING:    If set to a non-zero value, Stirling's asymptotic 
;                    formula is used to approximate N!. 
;
; EXAMPLE:
;       Compute 20! with and without Stirling's asymptotic formula.
;         result_1 = factorial(20, /stirling)
;         result_2 = factorial(20)
;       
;       Result_1 and result_2 should be 2.4227869e+18 and 2.4329020e+18
;       respectively.
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, November 1994
;-

function factorial, n, stirling = stirling

  ;Computes N! as (N) * (N-1) * (N-2) * ...... * 3 * 2 * 1
  ;Test example: 20! = 2.4329020e+18
  ;Use NR_MACHAR(/DOUBLE) to determine largest floating point number.
  ;Stirling's formula: N! = SQRT(2.0d*!PI*N) * (N/EXP(1.0d))^(N+0.0d)

  on_error, 2

  if n lt 0 then $
    message, 'n must be a non-negative scalar.'

  ;Computes N! as (N) * (N-1) * (N-2) * ...... * 3 * 2 * 1
  if keyword_set(stirling) eq 0 then begin 
    fact = 1.0d
    for k = n+0.0d, 1d, -1d do begin
      fact = fact * k
    endfor
  endif else $ ;Approximate N! using Stirling's formula.
    fact = sqrt(2.0d * !pi * n) * (n / exp(1.0d))^(n+0.0d)

  return, fact

end
