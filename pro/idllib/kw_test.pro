;$Id: kw_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       KW_TEST
;
; PURPOSE:
;       This function tests the hypothesis that three or more sample 
;       popultions have the same mean of distribution against the
;       hypothesis that they differ. The popultions may be of equal
;       or unequal lengths. The result is a two-element vector containing 
;       the test statistic H and the one-tailed probability of obtaining 
;       a value of H or greater from a chi-square distribution. This type 
;       of test is often refered to as the Kruskal-Wallis H-Test.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = KW_test(X)
;
; INPUTS:
;       X:    An array of m-columns (m >= 3) and n-rows of type integer, 
;             float or double. The columns of this two dimensional array 
;             correspond to the sample popultions. If the sample popultions
;             are of unequal length, all vectors must be appended up to a
;             common length of n using a user-specified missing value. This
;             method requires the use of the MISSING keyword.
;
; KEYWORD PARAMETERS:
;      DF:    Use this keyword to specify a named variable which returns
;             the degrees of freedom used to compute the probability of
;             obtaining a value of H or greater from the corresponding 
;             chi-square distribution
;
; MISSING:    Use this keyword to specify a non-zero numeric value which
;             is used to appended popultions of unequal length up to a 
;             common length of n.
;
; EXAMPLE:
;       Test the hypothesis that three sample popultions have the same mean 
;       of distribution against the hypothesis that they differ at the 0.05 
;       significance level.
;         sp0 = [24.0, 16.7, 22.8, 19.8, 18.9]
;         sp1 = [23.2, 19.8, 18.1, 17.6, 20.2, 17.8]
;         sp2 = [18.2, 19.1, 17.3, 17.3, 19.7, 18.9, 18.8, 19.3]
;       Since the sample popultions are of unequal lengths, a missing value
;       must be appended to sp0 and sp1. In this example the missing value
;       is -1.0 and the 3-column, 8-row input array is defined as:
;         x = [[24.0, 23.2, 18.2], $
;              [16.7, 19.8, 19.1], $
;              [22.8, 18.1, 17.3], $
;              [19.8, 17.6, 17.3], $
;              [18.9, 20.2, 19.7], $
;              [-1.0, 17.8, 18.9], $
;              [-1.0, -1.0, 18.8], $
;              [-1.0, -1.0, 19.3]]
;         result = kw_test(x, missing = -1)
;       The result should be the 2-element vector:
;         [1.65862, 0.436351]
;       The computed probability (0.436351) is greater than the 0.05
;       significance level and therefore we do not reject the hypothesis
;       that the three sample popultions s0, s1 and s2 have the same mean 
;       of distribution.
;
; PROCEDURE:
;       KW_TEST computes the nonparametric Kruskal-Wallis H-Test for three or
;       more populations of equal or unequal size. This test is an extension
;       of the Rank Sum Test implemented in the RS_TEST function. When each 
;       sample population contains at least five observations, the H test
;       statistic is approximated very well by a chi-square distribution with
;       DF degrees of freedom. The hypothesis that three of more sample 
;       populations have the same mean of distribution is rejected if two or
;       more populations differ with statistical significance. 
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1994
;-

pro crank, w, s
  ;Replace elements of the sorted array "w" by their rank.
  ;Identical observations ("ties") are ranked according to their means.
  ;s = f^3 - f (f is the number of elements in identical observations.)
  n = n_elements(w)
  w = [0.0, w]  ;operate on elements w(1), ... , w(n) of the shifted
                ;n+1 element float array (w).
  s = 0.0
  j = 1
  while(j lt n) do begin
    if(w(j+1) ne w(j)) then begin
      w(j) = j
      j = j+1
    endif else begin
      for jt = j+1, n do $
        if (w(jt) ne w(j)) then goto, case2
      jt = n + 1
      case2:
      rank = 0.5 * (j + jt - 1)
      for ji = j, jt-1 do $
        w(ji) = rank
      t = jt - j
      s = s + t^3 - t
      j = jt
    endelse
  endwhile
  if(j eq n) then w(n) = n
  w = w(1:*)
end

function kw_test, x, df = df, missing = missing

  on_error, 2

  sx = size(x)
  if sx(0) + 1 lt 3 then $
    message, 'x must be a two-dimensional array with 3 or more columns.'

  ;Redimension x as a column vector.
  xx = reform(transpose(x), 1, sx(4))

  if keyword_set(missing) eq 0 then begin
    ;Equal length samples.
    ixx = sort(xx) ;Sort and rank the combined vector.
    xx = xx(ixx)
    crank, xx
    xx = xx(sort(ixx))
    crs = total(reform(xx, sx(2), sx(1)), 1) ;Compute the individual column 
                                             ;rank sums by reforming xx.
    n = sx(4)
    h = 12.0/(n*(n+1))*total(crs^2 / sx(2)) - 3*(n+1)
    df = sx(1) - 1
    prob = 1 - chisqr_pdf(h, df)
    return, [h, prob]
  endif else begin
    ;Unequal length samples.
    ixx = where(xx ne missing, nxx) ;Eliminate missing data.
    if nxx ne sx(4) then xx = xx(ixx)
    ixx = sort(xx) ;Sort and rank the combined vector.
    xx = xx(ixx)
    crank, xx
    xx = xx(sort(ixx))
    mvi = lonarr(sx(1))
    if sx(3) eq 5 then crs = dblarr(sx(1)) else $
      crs = fltarr(sx(1))
    top = 0L
    for k = 0, sx(1)-1 do begin 
      imiss = where(x(k,*) eq missing, nmiss)
      mvi(k) = nmiss ;Number of missing values per column vector.
      bot = top + sx(2) - mvi(k) - 1L     
      crs(k) = total(xx(top:bot)) ;Column rank sum.
      top = bot + 1L
    endfor
    h = 12.0/(nxx*(nxx+1))*total(crs^2 / (sx(2)-mvi)) - 3*(nxx+1)
    df = sx(1) - 1
    prob = 1 - chisqr_pdf(h, df)
    return, [h, prob]
  endelse

end
