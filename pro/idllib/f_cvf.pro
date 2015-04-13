;$Id: f_cvf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       F_CVF
;
; PURPOSE:
;       This function computes the cutoff value (v) such that:
;                   Probability(X > v) = p
;       where X is a random variable from the F distribution with
;       (dfn) and (dfd) degrees of freedom.
;
; CALLING SEQUENCE:
;     Result = f_cvf(P, DFN, DFD)
;
; INPUTS:
;       P:    A non-negative scalar, in the interval [0.0, 1.0], of 
;             type float or double that specifies the probability of 
;             occurance or success. 
;
;     DFN:    A positive scalar of type integer, float or double that
;             specifies the degrees of freedom of the F distribution
;             numerator.
;
;     DFD:    A positive scalar of type integer, float or double that
;             specifies the degrees of freedom of the F distribution
;             denominator.
;
; EXAMPLE:
;       Compute the cutoff value (v) such that Probability(X > v) = 0.100
;       from the F distribution with (dfn = 10) and (dfd = 6) degrees of 
;       freedom. The result should be 7.87413
;         result = f_cvf(0.01, 10, 6)        
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. New documentation header.
;-

function f_cvf, p, dfn, dfd
 
  on_error, 2  ;Return to caller if error occurs.

  if p lt 0. or p gt 1. then message, $
    'p must be in the interval [0.0, 1.0]'

  case 1 of
    dfd eq 1: up = 300.0 
    dfd eq 2: up = 100.0 
    dfd gt 2 and dfd le 5: up = 30.0
    dfd gt 5 and dfd le 14: up = 20.0
    else: up = 12.0
  endcase
  below = 0
  while f_pdf(up, dfn, dfd) lt (1 - p) do begin
    below = up
    up = 2 * up
  endwhile

  return, bisect_pdf([1-p, dfn, dfd], 'f_pdf', up, below)
end

