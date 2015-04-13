;$Id: md_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       MD_TEST
;
; PURPOSE:
;       This function tests the hypothesis that a sample population
;       is random against the hypothesis that it is not random. The 
;       result is a two-element vector containing the nearly-normal
;       test statistic Z and the one-tailed probability of obtaining
;       a value of Z or greater. This test is often refered to as the
;       Median Delta Test.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = MD_test(X)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;   ABOVE:    Use this keyword to specify a named variable which returns
;             the number of sample population values greater than the
;             median of X.
;
;   BELOW:    Use this keyword to specify a named variable which returns
;             the number of sample population values less than the
;             median of X.
;
;     MDC:    Use this keyword to specify a named variable which returns
;             the number of Median Delta Clusters; sequencial values of
;             X above and below the median.
;
; EXAMPLE:
;       Define a sample population.
;         x = [2.00,  0.90, -1.44, -0.88, -0.24,  0.83, -0.84, -0.74,  0.99, $
;             -0.82, -0.59, -1.88, -1.96,  0.77, -1.89, -0.56, -0.62, -0.36, $
;             -1.01, -1.36]
;
;       Test the hypothesis that X represents a random population against the
;       hypothesis that it does not represent a random population at the 0.05
;       significance level.
;         result = md_test(x, mdc = mdc)
;
;       The result should be the 2-element vector:
;         [0.722745, 0.234918]
;
;       The computed probability (0.234918) is greater than the 0.05
;       significance level and therefore we do not reject the hypothesis 
;       that X represents a random population. 
;
; PROCEDURE:
;       MD_TEST computes the nonparametric Median Delta Test. Each sample
;       in the population is tagged with either a 1 or a 0 depending on
;       whether it is above or below the population median. Samples that
;       are equal to the median are removed and the population size is 
;       corresponding reduced. This test is an extension of the Runs Test 
;       for Randomness. 
;
; REFERENCE:
;       APPLIED NONPARAMETRIC STATISTICAL METHODS
;       Peter Sprent
;       ISBN 0-412-30610-7
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, November 1994
;-

function md_test, x, above = above, below = below, mdc = mdc

  on_error, 2

  nx = n_elements(x)

  if nx le 10 then message, $
    'Not defined for input vectors of 10 or fewer elements.'

  if nx mod 2 eq 0 then $ ;x is of even length.
    ;Average Kth and K-1st medians.
    medx = 0.5 * (median(x(0:nx-2)) + median(x)) $
  else medx = median(x)

  ;Eliminate values of x equal to median(x).
  xx = x(where(x ne medx))

  ;Values of xx above and below median(x).
  ia = where(xx gt medx, above)
  ib = where(xx lt medx, below)

  ;Values above are tagged 1.
  if above ne 0 then xx(ia) = 1

  ;Values below are tagged 0.
  if below ne 0 then xx(ib) = 0

  ;sxx = above + below
  sxx = size(xx)

  h0 = where(xx eq 0, n0)
  if n0 ne 0 then hi = where(h0+1 ne shift(h0, -1), nn0) $
  else nn0 = 0L

  ;above median
  n1 = sxx(1) - n0

  if xx(sxx(1)-1) ne xx(0) then nn1 = nn0 $
  else if xx(0) eq 1 then nn1 = nn0 + 1 $
  else nn1 = nn0 - 1

  if n0 eq 0 or n1 eq 0 then message, $
    'x is a sequence of identically distributed data.'

  ;Formulate the Median Delta test statistic.
  mdc = nn0 + nn1
  if sxx(2) eq 5 then n0 = n0 + 0.0d
  e = 2.0 * n0 * n1 / (n0 + n1) + 1.0
  v = 2.0 * n0 * n1 * (2.0 * n1 * n0 - n0 - n1) / $
      ((n0 + n1 - 1.0) * (n1 + n0)^2)
  z = (mdc - e) / sqrt(v)

  ;Probability from a Gaussian Distribution.
  prob = 1.0 - gauss_pdf(abs(z))

  return, [z, prob]

end
