;$Id: ts_diff.pro,v 1.1 1994/12/02 21:24:28 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       TS_DIFF
;
; PURPOSE:
;       This function recursively computes the forward differences, of an 
;       N-element time-series, K times. The result is an N-element differenced 
;       time-series with its last K elements as zeros. 
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = TS_Diff(X, K)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double containing
;             time-series samples.
;
;       K:    A positive scalar of type integer or long integer in the 
;             interval [1, N_ELEMENTS(X) - 1], that specifies the number of 
;             times X is differenced.
;
; EXAMPLE:
;       Define an n-element vector of time-series samples.
;         x = [389, 345, 303, 362, 412, 356, 325, 375, $
;              410, 350, 310, 388, 399, 362, 325, 382, $
;              399, 382, 318, 385, 437, 357, 310, 391]
;
;       Compute the second forward differences of X.
;         result = TS_DIFF(x, 2)
;
;       The result should be:
;         [ 2, 101,   -9,  -106, 25,  81, -15, -95, $
;          20, 118,  -67,   -48,  0,  94, -40, -34, $
;         -47, 131,  -15,  -132, 33, 128,   0,   0]
;
; REFERENCE:
;       The Analysis of Time Series (Fourth Edition)
;       C. Chatfield
;       ISBN 0-412-31820-2
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, December 1994
;-

function ts_diff, x, k

  on_error, 2

  nx = n_elements(x)
  if k gt nx - 1 then $
    message, 'Order of differencing cannot exceed N_ELEMENTS(X) - 1.'

  tsx = [0, x]

  if k gt 0 then begin
    for l = 1, k do begin ;Recursively compute differences.
      ;j = [1, 2, 3, ..., nx-1]
      ;tsx(j) = tsx(j) - tsx(j+1)
      j = lindgen(nx-1L)+1L
      tsx(j) = tsx(j) - tsx(j+1L)
      tsx(nx) = 0
      nx = nx - 1L
    endfor
  ;;endif else if k lt 0 then begin  ;Backward difference operator.
  ;;  for l = 1, abs(k) do begin     ;  ts_diff(x, 1) = -ts_diff(x, -1)
  ;;    a = tsx(1)                   ;  ts_diff(x, k) =  ts_diff(x, -k)
  ;;    for j = 2, nx do begin       ;    for k >= 2
  ;;      b = tsx(j)
  ;;      tsx(j-1) = tsx(j) - a
  ;;      a = b
  ;;    endfor
  ;;    tsx(nx) = 0.0
  ;;    nx = nx - 1L
  ;;  endfor
  ;;endif else $
  endif else $
    message, 'Order of differencing must be greater than zero.'

  return, tsx(1:*)

end
