;$Id: s_test.pro,v 1.4 1994/12/19 19:44:41 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       S_TEST
;
; PURPOSE:
;       This function tests the hypothesis that two sample popultions, 
;       {X(i), Y(i)}, have the same mean of distribution against the 
;       hypothesis that they differ. The result is a two-element vector
;       containing the maximum number of signed differences between 
;       corresponding pairs of X(i) and Y(i) and the one-tailed level of
;       significance. This type of test is often refered to as the Sign
;       Test.
;       
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = S_test(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An n-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;   ZDIFF:    Use this keyword to specify a named variable which returns the
;             number of differences between corresponding pairs of X(i) and 
;             Y(i) resulting in zero. Paired data resulting in a difference 
;             of zero are excluded from the ranking and the sample size is 
;             correspondingly reduced.
;
; EXAMPLE:
;       Define the n-element vectors of sample data.
;         x = [47, 56, 54, 49, 36, 48, 51, 38, 61, 49, 56, 52]
;         y = [71, 63, 45, 64, 50, 55, 42, 46, 53, 57, 75, 60]
;       Test the hypothesis that two sample popultions, {X(i), Y(i)}, have 
;       the same mean of distribution against the hypothesis that they differ
;       at the 0.05 significance level.
;         result = s_test(x, y, zdiff = zdiff)
;       The result should be the 2-element vector:
;         [9.00000, 0.0729981]
;       The keyword parameter should be returned as:
;         zdiff = 0
;       The computed probability (0.0729981) is greater than the 0.05 
;       significance level and therefore we do not reject the hypothesis that
;       X and Y have the same mean of distribution.
;
; PROCEDURE:
;       S_TEST computes the nonparametric Sign Test. Differences between 
;       corresponding pairs of X(i) and Y(i) are ranked as either positive or
;       negative with equal probability of occurance. Differences between 
;       pairs of X(i) and Y(i) that result in zero are excluded from the 
;       ranking and the sample size is correspondingly reduced. The result is 
;       a two-element vector [diff, p] containing the maximum number of signed 
;       differences between corresponding pairs of X(i) and Y(i) and the one-
;       tailed level of significance. Using the Binomial random variable X, 
;       we can accept of reject the proposed hypothesis. If the sample size 
;       exceeds 25, then the Gaussian distribution is used to approximate the 
;       cumulative Binomial distribution. The one-tailed probability of
;       obtaining at least (diff) signed differences in an n-element sample is
;       equal to (p). Prob(X >= diff) = p. 
;       The hypothesis that two sample popultions have the same mean
;       of distribution is rejected if the number of positive ranks and the
;       number of negative ranks differ with statistical significance. 
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, August 1994
;-

function s_test, x, y, zdiff = zdiff

  on_error, 2
  n = n_elements(x)
  if n ne n_elements(y) then message, $
      'x and y must be vectors of equal size.'

  diff = x - y

  ;Number of "ties" (identical data).
  psize = where(diff eq 0, zdiff)

  ;Population sample size. 
  psize = n - zdiff 

  if psize eq 0 then message, $
    'x and y contain identical data.'

  ;Number of positive ranks.
  ipn = where(diff gt 0, npos)

  ;Number of negative ranks.
  nneg = psize - npos

  if npos gt nneg then begin
  ;Probability that the number of positive ranks is at least (npos) with a
  ;population size (psize).     Prob(# of positive ranks >= npos)
    prob = binomial(npos, psize, 0.5)
  endif else if nneg gt npos then begin 
  ;Prob(# of negative ranks >= nneg)
    prob = binomial(nneg, psize, 0.5)
  endif else $
    ;prob = binomial(npos, psize/2, 0.5)
    prob = 0.5

  ;Maximum number of signed differences and the one-tailed probability.
  return, [max([npos, nneg]), prob]

end

