;$Id: binomial.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       BINOMIAL
;
; PURPOSE:
;       This function computes the probabilty (bp) such that:
;                   Probability(X => v) = bp 
;       where X is a random variable from the cumulative binomial distribution
;       (Bernouli distribution).
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Binomial(V, N, P)
;
; INPUTS:
;       V:    A non-negative integer specifying the minimal number of 
;             times an event E occurs in (N) independent performances.
;
;       N:    A non-negative integer specifying the number of performances.
;             If the number of performances exceeds 25, the Gaussian 
;             distribution is used to approximate the cumulative binomial 
;             distribution.
;
;       P:    A non-negative scalar, in the interval [0.0, 1.0],  of type 
;             float or double that specifies the probability of occurance 
;             or success of a single independent performance.
;
; EXAMPLES:
;       Compute the probability of obtaining at least two 6s in rolling a
;       die four times. The result should be 0.131944
;         result = binomial(2, 4, 1./6.)
;
;       Compute the probability of obtaining exactly two 6s in rolling a
;       die four times. The result should be 0.115741
;         result = binomial(2, 4, 1./6.) - binomial(3, 4, 1./6.)
;
;       Compute the probability of obtaining three or fewer 6s in rolling
;       a die four times. The result should be 0.999228
;         result = (binomial(0, 4, 1./6.) - binomial(1, 4, 1./6.)) + $
;                  (binomial(1, 4, 1./6.) - binomial(2, 4, 1./6.)) + $
;                  (binomial(2, 4, 1./6.) - binomial(3, 4, 1./6.)) + $
;                  (binomial(3, 4, 1./6.) - binomial(4, 4, 1./6.))
;
; PROCEDURE:
;       BINOMIAL computes the probability that an event E occurs at least
;       (V) times in (N) independent performances. The event E is assumed
;       to have a probability of occurance or success (P) in a single 
;       performance.
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. Rewrote documentation header.
;-

function N_BANG, n, min, fac1
  ;If min and fac1 are undefined, then N_BANG returns n!.
  ;Otherwise, fac1 * min * (min+1)....n is returned.
  if n_elements(min) eq 0 then min = 2
  if n_elements(fac1) eq 0 then fac = 1. $
    else fac = fac1
  if min gt n then return, fac
  n1 =  n < 10
  if min lt 11 then  $
    for i = min, n1 do fac = i*fac
  if (n lt 11.) then return, fac
  n1 = 11 > min
  ;Use logarithms to preserve precision.
  return, fac * exp(total(alog(findgen(n-n1+1) + n1)))
end

function binomial, v, n, p

  on_error, 2  ;Return to caller if error occurs.

  if p lt 0. or p gt 1. then message, $
    'p must be in the interval [0.0, 1.0]'

  if v lt 0 then message, $
    'v must be nonnegative.'

  if n lt 0 then message, $
    'n must be nonnegative.'

  if v eq 0 then return,  1.0 $
  else if n gt 25 then return, $
                  1.0 - gauss_pdf((v-n*p)/sqrt(n*p*(1-p))) $
  else if v gt n then return,  0.0

  nn = fix(n)
  vv = fix(v)
  n2 = vv < (nn - vv)
  n3 = vv > (nn - vv)
  n1 = N_BANG(nn, n3+1, p)
  n1 =  n1/N_BANG(n2)
  sum = n1 * p^(vv-1) * (1-p)^(nn-vv)
  for i = vv+1, nn do begin
    n1 = (nn-i+1) * n1/float(i)
    sum = sum + n1 * p^(i-1) * (1-p)^(nn-i)
  endfor
  return, sum
end
