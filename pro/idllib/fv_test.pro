;$Id: fv_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       FV_TEST
;
; PURPOSE:
;       This function computes the F-statistic and the probability that two 
;       vectors of sampled data have significantly different variances. This
;       type of test is often refered to as the F-variances Test.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = FV_TEST(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An m-element vector of type integer, float or double.
;
; EXAMPLE
;       Define two n-element vectors of tabulated data.
;         X = [257, 208, 296, 324, 240, 246, 267, 311, 324, 323, 263, 305, $
;               270, 260, 251, 275, 288, 242, 304, 267]
;         Y = [201, 56, 185, 221, 165, 161, 182, 239, 278, 243, 197, 271, $
;               214, 216, 175, 192, 208, 150, 281, 196]
;       Compute the F-statistic (of X and Y) and its significance. 
;       The result should be the two-element vector [2.48578, 0.0540116], 
;       indicating that X and Y have significantly different variances.
;         result = fv_test(X, Y)
;
; PROCEDURE:
;       FV_TEST computes the F-statistic of X and Y as the ratio of variances
;       and its significance. X and Y may be of different lengths. The result 
;       is a two-element vector containing the F-statistic and its 
;       significance. The significance is a value in the interval [0.0, 1.0];
;       a small value (0.05 or 0.01) indicates that X and Y have significantly
;       different variances.
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, Aug 1994
;                    FV_TEST is based on the routine: ftest.c described in 
;                    section 14.2 of Numerical Recipes, The Art of Scientific 
;                    Computing (Second Edition), and is used by permission.
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
  if (x eq 0  or x eq 1) then bt = 0.0 $
  else $
    bt = exp(gammln(a + b) - gammln(a) - gammln(b) + $
             a * alog(x) + b * alog(1.0 - x))
  if(x lt (a + 1.0)/(a + b + 2.0)) then return, $
    bt * betacf(a, b, x) / a $
  else return, 1.0 - bt * betacf(b, a, 1.0-x) / b
end

function fv_test, x0, x1

  on_error, 2

  nx0 = n_elements(x0)
  nx1 = n_elements(x1)

  if nx0 le 1 or nx1 le 1 then $
    message, 'x0 and x1 must be vectors of length greater than one.'

  type = size(x0)

  mv0 = moment(x0)
  mv1 = moment(x1)

  if mv0(1) gt mv1(1) then begin
    f = mv0(1) / mv1(1)
    df0 = nx0 - 1
    df1 = nx1 - 1
  endif else begin
    f = mv1(1) / mv0(1)
    df0 = nx1 - 1
    df1 = nx0 - 1
  endelse

  prob = 2.0 * ibeta(0.5*df1, 0.5*df0, df1/(df1+df0*f))

  if type(2) ne 5 then prob = float(prob)  
    
  if prob gt 1 then return, [f, 2.0 - prob] $
  else return, [f, prob]

end
