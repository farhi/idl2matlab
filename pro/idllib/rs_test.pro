;$Id: rs_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       RS_TEST
;
; PURPOSE:
;       This function tests the hypothesis that two sample popultions,
;       {X(i), Y(i)}, have the same mean of distribution against the
;       hypothesis that they differ. The result is a two-element vector
;       containing the nearly-normal test statistic Z and the one-tailed
;       probability of obtaining a value of Z or greater. This type of 
;       test is often refered to as the Wilcoxon Rank-Sum Test or Mann-
;       Whitney U-Test.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = RS_test(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An m-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;      UX:    Use this keyword to specify a named variable which returns
;             the Mann-Whitney statistic for X.
;
;      UY:    Use this keyword to specify a named variable which returns
;             the Mann-Whitney statistic for Y.
;
; EXAMPLE:
;       Define the vectors of sample data.
;         x = [-14,   3,   1, -16, -21,   7,  -7, -13, -22, -17, -14, -8, $
;                7, -18, -13,  -9, -22, -25, -24, -18, -13, -13, -18, -5]
;         y = [-18, -9, -16, -14,  -3,  -9, -16, 10, -11, -3, -13, $
;              -21, -2, -11, -16, -12, -13,  -6, -9,  -7, -11, -9]
;       Test the hypothesis that two sample popultions, {X(i), Y(i)},
;       have the same mean of distribution against the hypothesis that they
;       differ at the 0.05 significance level.
;         result = rs_test(x, y, ux = ux, uy = uy)
;       The result should be the 2-element vector:
;         [1.45134, 0.0733429]
;       The keyword parameters should be returned as:
;         ux = 330.000, uy = 198.000
;       The computed probability (0.0733429) is greater than the 0.05
;       significance level and therefore we do not reject the hypothesis
;       that X and Y have the same mean of distribution. 
;
; PROCEDURE:
;       RS_TEST computes the nonparametric Rank Sum Test for populations of
;       equal or unequal size. The populations X(i) and Y(i) are combined
;       and individual elements are ranked based on magnitude. Elements of
;       identical magnitude are ranked using a rank equal to the mean of the
;       ranks that would otherwise be assigned. The Mann-Whitney statistics
;       (Ux and Uy) are computed and used to determine the nearly-normal test
;       statistic (Z) and the one-tailed probability of obtaining a value of 
;       Z or greater. The hypothesis that two sample populations have the same
;       mean of distribution is rejected if Ux and Uy differ with statistical 
;       significance. If either population contains 10 or fewer samples, the
;       test statistic (Z) and the one-tailed probability of obtaining a value
;       of Z or greater are returned as zero. In this case, consult published
;       tables such as the ones available in the REFERENCE given below. 
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, August 1994
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

function rs_test, x, y, ux = ux, uy = uy

  on_error, 2

  nx = n_elements(x)
  ny = n_elements(y)

  ;An alternate method of error handling if Ux and Uy are not required.
  ;if min([nx, ny]) le 10 then message, $
  ;    'Consult statistical tables for samples of 10 or fewer elements.'

  ;Number of "ties" (identical data).
  ties = where(x - y eq 0, zdiff)

  if nx eq ny and zdiff eq nx then message, $
    'x and y contain identical data.'

  ;Sort and rank the combined x and y vectors.
  xy = [x, y]            ;Combine vectors.
  is = sort(xy)          ;Sort xy into ascending order.
  xy = xy(is)
  crank, xy              ;Rank sorted xy vector.
  xy = xy(sort(is))
  
  ;Rank-sum totals.
  wx = total(xy(0:nx-1))
  wy = total(xy(nx:*))

  ;Compute the Mann-Whitney statistics.
  ux = nx * ny + (nx * (nx + 1.0) / 2.0) - wx
  uy = nx * ny + (ny * (ny + 1.0) / 2.0) - wy

  ;Compute the Z statistic with respect to ux.
  z = (ux - (nx*ny/2.0)) / sqrt(nx*ny*(nx+ny+1.0)/12.0)

  ;Probability of obtaining z or something greater.
  prob = 1.0 - gauss_pdf(abs(z)) 

  ;If either sample is <= 10, consult published statistical tables.
  if min([nx, ny]) le 10 then return, [0, 0] $
  else return, [z, prob] 

end
