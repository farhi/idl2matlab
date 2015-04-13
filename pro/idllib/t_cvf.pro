;$Id: t_cvf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       T_CVF
;
; PURPOSE:
;       This function computes the cutoff value (v) such that:
;                   Probability(X > v) = p
;       where X is a random variable from the Student's t distribution
;       with (df) degrees of freedom.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = t_cvf(P, DF)
;
; INPUTS:
;       P:    A non-negative scalar, in the interval [0.0, 1.0], of type
;             float or double that specifies the probability of occurance
;             or success.
;
;      DF:    A positive scalar of type integer, float or double that
;             specifies the degrees of freedom of the Student's t 
;             distribution.
;
; EXAMPLE:
;       Compute the cutoff value (v) such that Probability(X > v) = 0.025
;       from the Student's t distribution with (df = 5) degrees of freedom. 
;       The result should be 2.57058
;         result = t_cvf(0.025, 5)
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

function t_cvf , a1, df
  
  on_error, 2  ;Return to caller if error occurs.

  a = a1 
  if a lt 0. or a gt 1. then message, $
    'p must be in the interval [0.0, 1.0]'
  if (a gt 0.5) then adjust = 1 else begin
    adjust = 0
    a = 1.0 - a 
  endelse
  if a1 eq 0 then return,  1.0e12
  if a1 eq 1 then return, -1.0e12

  case 1 of
    df eq 1: up = 100 > (100 * 0.005/a1)
    df eq 2: up = 10  > (10  * 0.005/a1)
    df gt 2 and df le 5:  up = 5 > (5 * 0.005/a1)
    df gt 5 and df le 14: up = 4 > (4 * 0.005/a1)
    else: up = 3 > (3 * 0.005/a1)				
  endcase

  while t_pdf(up, df) lt a do begin
    below = up
    up = 2 * up
  endwhile
  
  x = bisect_pdf([a, df], 't_pdf', up, 0)
  if (adjust) then return, -x   $
    else return, x
end

