;$Id: gauss_cvf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       GAUSS_CVF
;
; PURPOSE:
;       This function computes the cutoff value (v) such that:
;                   Probability(X > v) = p
;       where X is a random variable from the standard Gaussian (Normal)
;       distribution with a mean of 0.0 and a variance of 1.0
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Gauss_cvf(P)
;
; INPUTS:
;       P:    A non-negative scalar, in the interval [0.0, 1.0], of type
;             float or double that specifies the probability of occurance
;             or success.
;
; EXAMPLE:
;       Compute the cutoff value (v) such that Probability(X > v) = 0.025
;       from the standard Gaussian (Normal) distribution. The result should 
;       be 1.95997
;         result = gauss_cvf(0.025)
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

function gauss_cvf, p

  on_error, 2  ;Return to caller if error occurs.

  if p lt 0. or p gt 1. then message, $
    'p must be in the interval [0.0, 1.0]'
  if p eq 0 then return,  1.0e12
  if p eq 1 then return, -1.0e12

  if (p gt 0.5) then begin
    p = 1 - p
    adjust = 1
  endif else adjust = 0 

  below = 0
  up = 1.0

  while gauss_pdf(up) lt 1.0 - p do begin
    below = up
    up = 2 * up
  endwhile

  x = bisect_pdf([1.0 - p], 'gauss_pdf', up, below)
  if adjust then begin
    p = 1 - p
    return, -x
  endif else return, x
end

