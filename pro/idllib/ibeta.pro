;$Id: ibeta.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       IBETA
;
; PURPOSE:
;       This function computes the incomplete beta function, Ix(a, b).
;
; CATEGORY:
;       Special Functions.
;
; CALLING SEQUENCE:
;       Result = Ibeta(a, b, x)
;
; INPUTS:
;       A:    A positive scalar of type integer, float or double that 
;             specifies the parametric exponent of the integrand.
;
;       B:    A positive scalar of type integer, float or double that
;             specifies the parametric exponent of the integrand.
;
;       X:    A scalar, in the interval [0, 1], of type integer, float 
;             or double that specifies the upper limit of integration.
;
; EXAMPLE:
;       Compute the incomplete beta function for the corresponding elements
;       of A, B, and X.
;       Define the parametric exponents.
;         A = [0.5, 0.5, 1.0, 5.0, 10.0, 20.0]
;         B = [0.5, 0.5, 0.5, 5.0,  5.0, 10.0]
;       Define the the upper limits of integration.
;         X = [0.01, 0.1, 0.1, 0.5, 1.0, 0.8]
;       Allocate an array to store the results.
;         result = fltarr(n_elements(A))
;       Compute the incomplete beta functions.
;         for k = 0, n_elements(A)-1 do $
;           result(k) = Ibeta(A(k), B(k), X(k))
;       The result should be:
;         [0.0637686, 0.204833, 0.0513167, 0.500000, 1.00000, 0.950736]
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1994
;                    IBETA is based on the routines: betacf.c, betai.c and
;                    gammln.c described in section 6.2 of Numerical Recipes,
;                    The Art of Scientific Computing (Second Edition), and is
;                    used by permission.
;-

function betacf, a, b, x
  on_error, 2
  eps   = 3.0e-7
  fpmin = 1.0e-30
  maxit = 100
  qab = a + b
  qap = a + 1.0
  qam = a - 1.0
    c = 1.0
    d = 1.0 - qab * x / qap
  if(abs(d) lt fpmin) then d = fpmin
  d = 1.0 / d
  h = d
  for m = 1, maxit do begin
    m2 = 2 * m
    aa = m * (b - m) * x / ((qam + m2) * (a + m2))
     d = 1.0 + aa*d
     if(abs(d) lt fpmin) then d = fpmin
     c = 1.0 + aa / c
     if(abs(c) lt fpmin) then c = fpmin
     d = 1.0 / d
     h = h * d * c
     aa = -(a + m) *(qab + m) * x/((a + m2) * (qap + m2))
     d = 1.0 + aa * d
     if(abs(d) lt fpmin) then d = fpmin
     c = 1.0 + aa / c
     if(abs(c) lt fpmin) then c = fpmin
     d = 1.0 / d
     del = d * c
     h = h * del
     if(abs(del - 1.0) lt eps) then return, h
  endfor
  message, 'Failed to converge within given parameters.'
end

function gammln, xx
  coff = [76.18009172947146d0,   -86.50532032941677d0,  $
          24.01409824083091d0,    -1.231739572450155d0, $
           0.1208650973866179d-2, -0.5395239384953d-5]
  stp = 2.5066282746310005d0
  x = xx
  y = x
  tmp = x + 5.5d0
  tmp = (x + 0.5d0) * alog(tmp) - tmp
  ser = 1.000000000190015d0
  for j = 0, n_elements(coff)-1 do begin
    y = y + 1.d0
    ser = ser + coff(j) / y
  endfor
  return, tmp + alog(stp * ser / x)
end

function ibeta, a, b, x
  on_error, 2
  if (x lt 0 or x gt 1) then message, $
    'x must be in the interval [0, 1].'

  if (a le 0 or b le 0) then message, $
    'a and b must be positive scalars.'

  if (x eq 0  or x eq 1) then bt = 0.0 $
  else $
    bt = exp(lngamma(a + b) - lngamma(a) - lngamma(b) + $
             a * alog(x) + b * alog(1.0 - x))
    ;bt = exp(gammln(a + b) - gammln(a) - gammln(b) + $
    ;         a * alog(x) + b * alog(1.0 - x))
  if(x lt (a + 1.0)/(a + b + 2.0)) then return, $
    bt * betacf(a, b, x) / a $
  else return, 1.0 - bt * betacf(b, a, 1.0-x) / b
end


