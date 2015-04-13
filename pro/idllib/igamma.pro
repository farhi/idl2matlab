;$Id: igamma.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       IGAMMA
;
; PURPOSE:
;       This function computes the incomplete gamma function, Px(a).
;
; CATEGORY:
;       Special Functions.
;
; CALLING SEQUENCE:
;       Result = Igamma(a, x)
;
; INPUTS:
;       A:    A positive scalar of type integer, float or double that
;             specifies the parametric exponent of the integrand.
;
;       X:    A positive scalar of type integer, float or double that 
;             specifies the upper limit of integration.
;
; KEYWORD PARAMETERS:
;       METHOD:  Use this keyword to specify a named variable which returns
;                the method used to compute the incomplete gamma function.
;                A value of 0 indicates that a power series representation
;                was used. A value of 1 indicates that a continued fractions
;                method was used.
;
; EXAMPLE:
;       Compute the incomplete gamma function for the corresponding elements
;       of A and X.
;       Define the parametric exponents.
;         A = [0.10, 0.50, 1.00, 1.10, 6.00, 26.00]
;       Define the the upper limits of integration.
;         X = [0.0316228, 0.0707107, 5.00000, 1.04881, 2.44949, 25.4951]
;       Allocate an array to store the results.
;         result = fltarr(n_elements(A))
;       Compute the incomplete gamma functions.
;         for k = 0, n_elements(A)-1 do $
;           result(k) = Igamma(A(k), X(k))
;       The result should be:
;         [0.742026, 0.293128, 0.993262, 0.607646, 0.0387318, 0.486387]
;
; PROCEDURE:
;       IGAMMA computes the incomplete gamma function, Px(a), using either
;       a power series representation or a continued fractions method. If X
;       is less than or equal to A+1, a power series representation is used.
;       If X is greater than A+1, a continued fractions method is used. 
; 
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1994
;                    IGAMMA is based on the routines: gser.c, gcf.c, and  
;                    gammln.c described in section 6.2 of Numerical Recipes,
;                    The Art of Scientific Computing (Second Edition), and is
;                    used by permission.
;-

function igamma, a, x, itmax = itmax, method = method

  on_error, 2

  if a le 0 or x le 0 then $
    message, 'a and x must be positive scalars.'

  eps = 3.0e-7
  fpmin = 1.0e-30
  if keyword_set(itmax) eq 0 then itmax = 100

  if x le (a + 1) then begin ;Series Representation.
    method = 0
    ap = a
    sum = 1.0 / a
    del = sum
    for k = 1, itmax do begin
      ap = ap + 1.0
      del = del * x / ap
      sum = sum + del
      if abs(del) lt abs(sum)*eps then return, $
        sum * exp(-x + a * alog(x) - lngamma(a))
    endfor 
  endif else begin ;Continued Fractions.
    method = 1
    b = x + 1.0 - a
    c = 1.0 / fpmin
    d = 1.0 / b
    h = d
    for k = 1, itmax do begin
      an = -k * (k - a)
      b = b + 2
      d = an * d + b
      if abs(d) lt fpmin then d = fpmin
      c = b + an / c
      if abs(c) lt fpmin then c = fpmin
      d = 1.0 / d
      del = d * c
      h = h * del
      if abs(del - 1) lt eps then return, $
        1 - (exp(-x + a * alog(x) - lngamma(a)) * h)
    endfor
  endelse

  message, 'Failed to converge within given parameters.'

end
