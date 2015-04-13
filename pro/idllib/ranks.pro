;$Id: ranks.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       RANKS
;
; PURPOSE:
;       This function computes the magnitude-based ranks of a sample 
;       population X. Elements of identical magnitude "ties" are ranked 
;       according to the mean of the ranks that would otherwise be assigned.
;       The result is a vector of ranks equal in length to X.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Ranks(X)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;             The elements of this vector must be in ascending order
;             based on their magnitude.
;
; EXAMPLE:
;       Define an n-element sample population.
;         x = [-0.8, 0.1, -2.3, -0.6, 0.2, 1.1, -0.3, 0.6, -0.2, 1.1, -0.7, $
;              -0.2, 0.6, 0.4, -0.1, 1.1, -0.3, 0.3, -1.3, 1.1]
;
;       Allocate a two-column, n-row array to store the results.
;         array = fltarr(2, n_elements(x))
;
;       Sort the sample population and store in the 0th column of ARRAY.
;         array(0, *) = x(sort(x))
;
;       Compute the ranks of the sorted sample population and store in the 
;       1st column of ARRAY.
;         array(1, *) = ranks(x(sort(x)))
;
;       Display the sorted sample population and corresponding ranks with a 
;       two-decimal format.
;         print, array, format = '(2(5x, f5.2))'
;       
;       The result should be:
;         -2.30      1.00
;         -1.30      2.00
;         -0.80      3.00
;         -0.70      4.00
;         -0.60      5.00
;         -0.30      6.50
;         -0.30      6.50
;         -0.20      8.50
;         -0.20      8.50
;         -0.10     10.00
;          0.10     11.00
;          0.20     12.00
;          0.30     13.00
;          0.40     14.00
;          0.60     15.50
;          0.60     15.50
;          1.10     18.50
;          1.10     18.50
;          1.10     18.50
;          1.10     18.50
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, November 1994
;-

function ranks, x

  on_error, 2

  nx = n_elements(x)
  r_vec = [0.0, x] 
  k = 1L
  while(k lt nx) do begin
    if(r_vec(k+1) ne r_vec(k)) then begin
      r_vec(k) = k
      k = k+1
    endif else begin
      for kt = k+1, nx do $
        if (r_vec(kt) ne r_vec(k)) then goto, midrank
      kt = nx + 1
      midrank:
      rank = 0.5 * (k + kt - 1)
      for ki = k, kt-1 do $
        r_vec(ki) = rank
      t = kt - k
      k = kt
    endelse
  endwhile
  if(k eq nx) then r_vec(nx) = nx

  return, r_vec(1:*)

end
