;$Id: chisqr_cvf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CHISQR_CVF
;
; PURPOSE: 
;	This function computes the cutoff value (v) such that:
;                   Probability(X > v) = p
;       where X is a random variable from the Chi-square distribution
;       with (df) degrees of freedom. 
;
; CATEGORY:
;       Statistics.
; 
; CALLING SEQUENCE:
;       Result = chisqr_cvf(P, DF)
;
; INPUTS:
;       P:    A non-negative scalar, in the interval [0.0, 1.0], of type
;             float or double that specifies the probability of occurance 
;             or success.
;
;      DF:    A positive scalar of type integer, float or double that 
;             specifies the degrees of freedom of the Chi-square distribution.
;
; EXAMPLE:
;       Compute the cutoff value (v) such that Probability(X > v) = 0.100
;       from the Chi-square distribution with (DF = 3) degrees of freedom. 
;       The result should be 6.25139
;         result = chisqr_cvf(0.100, 3) 
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. New documentation header.
;-

function chisqr_cvf, p, df

  on_error, 2  ;Return to caller if error occurs.

  if p lt 0. or p gt 1. then message, $
    'p must be in the interval [0.0, 1.0]'
  if p eq 0 then return, 1.0e12
  if p eq 1 then return, 0.0
  if df lt 0 then message, $
    'Degrees of freedom must be positive.'

  case 1 of
    df eq 1: up = 300.0 
    df eq 2: up = 100.0 
    df gt 2 and df le 5: up = 30.0
    df gt 5 and df le 14: up = 20.0
    else: up = 12.0
  endcase
  below = 0
  while chisqr_pdf(up, df) lt (1 - p) do begin
      below = up
      up = 2 * up
  endwhile

  return, bisect_pdf([1-p, df], 'chisqr_pdf', up, below)
end

