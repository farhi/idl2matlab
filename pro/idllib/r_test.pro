;$Id: r_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       R_TEST
;
; PURPOSE:
;       This function tests the hypothesis that a binary sequence (a 
;       sequence of 1s and 0s) represents a "random sampling". This
;       test is based on the "theory of runs" and is often refered to
;       as the Runs Test for Randomness. The result is a two-element
;       vector containing the nearly-normal test statistic Z and the 
;       one-tailed probability of obtaining a value of Z or greater.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = R_test(X)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;             Elements not equal to 0 or 1 are removed and the length
;             of X is correspondingly reduced.
;
; KEYWORD PARAMETERS:
;       R:    Use this keyword to specify a named variable which returns
;             the number of runs (clusters of 0s and 1s) in X.       
;
;      N0:    Use this keyword to specify a named variable which returns
;             the number of 0s in X.
;
;      N1:    Use this keyword to specify a named variable which returns
;             the number of 1s in X.
;
; EXAMPLE:
;       Define a vector of 1s and 0s.
;         x = [0,1,1,0,1,0,0,0,1,0,0,1,1,0,1,0,1,0,0,1,0,1,1,0,1,0,0,1,0,1]
;
;       Test the hypothesis that x represents a random sampling against the 
;       hypothesis that it does not represent a random sampling at the 0.05 
;       significance level.
;         result = r_test(x, r = r, n0 = n0, n1 = n1)
;
;       The result should be the 2-element vector:
;         [2.26487, 0.0117604]
;       The keyword parameters should be returned as:
;         r = 22.0000, n0 = 16.0000, n1 = 14.0000 
;
;       The computed probability (0.0117604) is less than the 0.05
;       significance level and therefore we reject the hypothesis that x
;       represents a random sampling. The results show that there are too 
;       many runs, indicating a non-random cyclical pattern.
;
; PROCEDURE:
;       R_TEST computes the nonparametric Runs Test for Randomness. A 
;       "run" is a cluster of identical symbols within a sequence of two
;       distinct symbols. The binary sequence (x) defined in EXAMPLE has 
;       22 runs (or clusters). The first run contains one 0, the second 
;       run contains two 1s, the third run contains one 0, and so on.
;       In general, the randomness hypothesis will be rejected if there
;       are lengthy runs of 0s or 1s or if there are alternating patters
;       of many short runs.
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, August 1994
;-

function r_test, x, r = r, n0 = n0, n1 = n1

  on_error, 2

  nx = n_elements(x)

  if nx le 10 then message, $
    'Not defined for input vectors of 10 or fewer elements.'

  ;Remove any sequence elements that are not 0s or 1s.
  data = where(x eq 0 or x eq 1, nb)
  if nb ne 0 then x = x(data)

  sx = size(x)

  h0 = where(x eq 0, n0)
  if n0 ne 0 then hi = where(h0+1 ne shift(h0, -1), nn0) $
  else nn0 = 0L

  n1 = sx(1) - n0

  if x(sx(1)-1) ne x(0) then nn1 = nn0 $
  else if x(0) eq 1 then nn1 = nn0 + 1 $
  else nn1 = nn0 - 1

  if n0 eq 0 or n1 eq 0 then message, $
    'x is a sequence of identical data.'

  r = nn0 + nn1
  e = 2.0 * n0 * n1 / (n0 + n1) + 1.0
  v = 2.0 * n0 * n1 * (2.0 * n1 * n0 - n0 - n1) / $
                      ((n0 + n1 -1.0) * (n1 + n0)^2)
  z = (r - e) / sqrt(v)

  prob = 1.0 - gauss_pdf(abs(z))

  return, [z, prob]

end


