;$Id: a_correlate.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       A_CORRELATE
;
; PURPOSE:
;       This function computes the autocorrelation Px(L) or autocovariance
;       Rx(L) of a sample population X as a function of the lag (L).
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = A_correlate(X, Lag)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;     LAG:    A scalar or n-element vector, in the interval [-(n-2), (n-2)],
;             of type integer that specifies the absolute distance(s) between 
;             indexed elements of X.
;
; KEYWORD PARAMETERS:
;       COVARIANCE:    If set to a non-zero value, the sample autocovariance
;                      is computed.
;
; EXAMPLE
;       Define an n-element sample population.
;         x = [3.73, 3.67, 3.77, 3.83, 4.67, 5.87, 6.70, 6.97, 6.40, 5.57]
;
;       Compute the autocorrelation of X for LAG = -3, 0, 1, 3, 4, 8
;         lag = [-3, 0, 1, 3, 4, 8]
;         result = a_correlate(x, lag)
;
;       The result should be:
;         [-0.0303311, 1.00000, 0.784758, -0.0303311, -0.350930, -0.112325]
;
; PROCEDURE:
;       See computational formula published in IDL manual.
;
; REFERENCE:
;       INTRODUCTION TO STATISTICAL TIME SERIES
;       Wayne A. Fuller
;       ISBN 0-471-28715-6
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, October 1994
;
;-

function auto_cov, x, m, nx
  ;Sample autocovariance function.

  xmean = total(x) / nx
  t = lindgen(nx - m - 1L)
  return, total((x(t) - xmean) * (x(t + m) - xmean))

end


function a_correlate, x, lag, covariance = covariance

  ;Compute the sample-autocorrelation or autocovariance of (Xt, Xt+l)
  ;as a function of the lag (l).

  on_error, 2

  nx = n_elements(x)

  if nx lt 2 then $
    message, 'x must be a vector of two or more elements.'
  
  nlag = n_elements(lag)

  if nlag eq 1 then lag = [lag] ;Create a 1-element vector.

  type = size(x)
  if type(2) eq 5 then auto = dblarr(nlag) $
  else auto = fltarr(nlag)

  if keyword_set(covariance) eq 0 then begin ;Compute Autocorrelation.
    for k = 0, nlag-1 do $
      auto(k) = auto_cov(x, abs(lag(k)), nx) / auto_cov(x, 0L, nx)
  endif else begin ;Compute Autocovariance.
    for k = 0, nlag-1 do $ 
      auto(k) = auto_cov(x, abs(lag(k)), nx) / nx
  endelse

  return, auto

end
