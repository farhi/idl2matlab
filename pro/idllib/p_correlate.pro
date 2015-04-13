;$Id: p_correlate.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       P_CORRELATE
;
; PURPOSE:
;       This function computes the partial correlation coefficient of a
;       dependent variable and one particular independent variable when
;       the effects of all other variables involved are removed.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE: 
;       Result = P_correlate(X, Y, C)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double that
;             specifies the independent variable data.
;       
;       Y:    An n-element vector of type integer, float or double that
;             specifies the dependent variable data.
;
;       C:    An array of type integer, float or double that specifies the 
;             independent variable data whose effects are to be removed. 
;             The columns of this two dimensional array correspond to the 
;             n-element vectors of independent variable data.
;
; EXAMPLES:
;       Define the data vectors.
;         x0 = [64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68]
;         x1 = [57, 59, 49, 62, 51, 50, 55, 48, 52, 42, 61, 57]
;         x2 = [ 8, 10,  6, 11,  8,  7, 10,  9, 10,  6, 12,  9]
;
;       Compute the partial correlation of x0 and x1 with the effects of
;       x2 removed. The result should be 0.533469
;         result = p_correlate(x0, x1, reform(x2, 1, n_elements(x2)))
;
;       Compute the partial correlation of x0 and x2 with the effects of 
;       x1 removed. The result should be 0.334572
;         result = p_correlate(x0, x2, reform(x1, 1, n_elements(x1)))
;
;       Compute the partial correlation of x1 and x2 with the effects of 
;       x0 removed. The result should be 0.457907
;         result = p_correlate(x1, x2, reform(x0, 1, n_elements(x0)))
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. New documentation header.
;-

function p_correlate, x, y, c

  on_error, 2  ;Return to caller if an error occurs.

  if n_elements(x) ne n_elements(y) then message, $
    'x and y must have the same number of elements.'

  sc = size(c)

  if sc(1) eq 1 then begin
    p = [correlate(x, y), correlate(x, c), correlate(y, c)]
    if (p(1) ne 1 and p(2) ne 1) then $
      return, (p(0) - p(1) * p(2))/sqrt((1 - p(1)^2) * (1 - p(2)^2)) $
      else return, 0
  endif else begin
    w = replicate(1.0, sc(2))  ;Vector of weights.
    cf = regress(c, y, w, yfit, a0, s, f, r, p0, /rel)
    cf = regress([c, transpose(x)], y, w, yfit, a0, s, f, r, p1, /rel)
    p0 = 1 - p0^2
    p1 = 1 - p1^2 
    if p0 eq 0 then $
      return, 0 $
      else return, sqrt((p0 - p1)/p0) 
  endelse

end

