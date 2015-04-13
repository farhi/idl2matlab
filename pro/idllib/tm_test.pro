;$Id: tm_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       TM_TEST
;
; PURPOSE:
;       This function computes the Student's t-statistic and the probability
;       that two vectors of sampled data have significantly different means. 
;       The default assumption is that the data is drawn from populations with
;       the same true variance. This type of test is often refered to as the 
;       T-means Test.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = TM_TEST(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An m-element vector of type integer, float or double.
;             If the PAIRED keyword is set, X and Y must have the same
;             number of elements.
;
; KEYWORD PARAMETERS:
;       PAIRED:   If set to a non-zero value, X and Y are assumed to be 
;                 paired samples and must have the same number of elements.
;
;       UNEQUAL:  If set to a non-zero value, X and Y are assumed to be from
;                 populations with unequal variances.
;
; EXAMPLE
;       Define two n-element vectors of tabulated data.
;         X = [257, 208, 296, 324, 240, 246, 267, 311, 324, 323, 263, 305, $
;               270, 260, 251, 275, 288, 242, 304, 267]
;         Y = [201, 56, 185, 221, 165, 161, 182, 239, 278, 243, 197, 271, $
;               214, 216, 175, 192, 208, 150, 281, 196]
;       Compute the Student's t-statistic and its significance assuming that 
;       X and Y belong to populations with the same true variance.
;       The result should be the two-element vector [5.5283890, 2.5245510e-06],
;       indicating that X and Y have significantly different means.
;         result = tm_test(X, Y)
;       
; PROCEDURE:
;       TM_TEST computes the t-statistic of X and Y as the ratio;
;       (difference of sample means) / (standard error of differences) and 
;       its significance (the probability that |t| could be at least as large
;       large as the computed statistic). X and Y may be of different lengths.
;       The result is a two-element vector containing the t-statistic and its
;       significance. The significance is a value in the interval [0.0, 1.0]; 
;       a small value (0.05 or 0.01) indicates that X and Y have significantly
;       different means. 
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, Aug 1994
;                    TM_TEST is based on the routines: ttest.c, tutest.c and
;                    tptest.c described in section 14.2 of Numerical Recipes, 
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
  if (x eq 0  or x eq 1) then bt = 0.0 $
  else $
    bt = exp(gammln(a + b) - gammln(a) - gammln(b) + $
             a * alog(x) + b * alog(1.0 - x))
  if(x lt (a + 1.0)/(a + b + 2.0)) then return, $
    bt * betacf(a, b, x) / a $
  else return, 1.0 - bt * betacf(b, a, 1.0-x) / b
end

function tm_test, x0, x1, paired = paired, unequal = unequal

  on_error, 2

  if keyword_set(paired) ne 0 and keyword_set(unequal) ne 0 then $
    message, 'Paired and Unequal keywords cannot be set simultaneously.'

  nx0 = n_elements(x0)
  nx1 = n_elements(x1)

  if nx0 le 1 or nx1 le 1 then $
    message, 'x0 and x1 must be vectors of length greater than one.'

  type = size(x0)

  if keyword_set(paired) ne 0 then begin
    ;x0 and x1 are paired samples with corrected covariance.
    if nx0 ne nx1 then message, $
      'Paired keyword requires vectors of equal size.'
    mv0 = moment(x0)
    mv1 = moment(x1)
    cov = total((x0 - mv0(0)) * (x1 - mv1(0)))
    df = nx0 - 1
    cov = cov / df
    sd = sqrt((mv0(1) + mv1(1) - 2.0 * cov) / nx0)
    t = (mv0(0) - mv1(0)) / sd
    prob = ibeta(0.5*df, 0.5, df/(df+t^2))
    if type(2) eq 4 then return, float([t, prob]) $
    else return, [t, prob]
  endif else if keyword_set(unequal) ne 0 then begin
    ;x0 and x1 are assumed to have different population variances.
    mv0 = moment(x0)
    mv1 = moment(x1)
    t = (mv0(0) - mv1(0)) / sqrt(mv0(1)/nx0 + mv1(1)/nx1)
    df = (mv0(1)/nx0 + mv1(1)/nx1)^2 / $
         ((mv0(1)/nx0)^2/(nx0 - 1.0) + (mv1(1)/nx1)^2/(nx1 - 1.0))
    prob = ibeta(0.5*df, 0.5, df/(df+t^2))
    if type(2) ne 5 then return, float([t, prob]) $
    else return, [t, prob]
  endif else begin
    ;x0 and x1 are assumed to have the same population variance. 
    mv0 = moment(x0)
    mv1 = moment(x1)
    df = nx0 + nx1 - 2
    var = ((nx0 - 1)*mv0(1) + (nx1 - 1)*mv1(1)) / df 
    t = (mv0(0) - mv1(0)) / sqrt(var*(1.0/nx0 + 1.0/nx1))
    prob = ibeta(0.5*df, 0.5, df/(df+t^2))
    if type(2) ne 5 then return, float([t, prob]) $
    else return, [t, prob]
  endelse

end
