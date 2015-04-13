;$Id: ts_fcast.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       TS_FCAST
;
; PURPOSE:
;       This function computes future values of a stationary time-series 
;       using a Pth order autoregressive model. The result is an
;       N_FUTURE-element vector whose type is identical to X.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = TS_FCAST(X, P, N_FUTURE)
;
; INPUTS:
;       X:    An n-element vector of type float or double containing time-
;             series samples.
;
;       P:    A scalar of type integer or long integer that specifies the
;             number of past time-series values to be used in the forecast. 
;             In general, a larger number of values results in a more accurate
;             forecast.
;
;N_FUTURE:    A scalar of type integer or long integer that specifies the
;             number of future values to be computed.
;
; EXAMPLE:
;       Define an n-element vector of time-series samples.
;         x = [6.63, 6.59, 6.46, 6.49, 6.45, 6.41, 6.38, 6.26, 6.09, 5.99, $
;              5.92, 5.93, 5.83, 5.82, 5.95, 5.91, 5.81, 5.64, 5.51, 5.31, $
;              5.36, 5.17, 5.07, 4.97, 5.00, 5.01, 4.85, 4.79, 4.73, 4.76]
;
;       Compute five future values of the time-series using a 10th order
;       autoregressive model.
;         result = ts_fcast(x, 10, 5)
;
;       The result should be:
;         [4.65870, 4.58380, 4.50030, 4.48828, 4.46971] 
;
; REFERENCE:
;       The Analysis of Time Series, An Introduction (Fourth Edition)
;       Chapman and Hall
;       ISBN 0-412-31820-2
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, November 1994
;-

function ts_fcast, x, p, n_future, reflect = reflect

  ;This function uses the last P elements of the time-series
  ;[x0, x1, ... , xn-1] to compute the forecast. More coefficients 
  ;correspond to more past time-series data used to make the forecast.

  on_error, 2

  n_future = n_future + 0L

  if n_future le 0 then $
    message, 'n_future must be a scalar greater than 0.'

  nx = n_elements(x)
  sx = size(x)
 
  if p lt 2 or p gt nx-1 then $
    message, 'p must be a scalar in the interval: [2, n_elements(x)-1].'

  ;The last p elements of the time-series.
  data = reverse(x(nx-p:nx-1))

  if sx(2) eq 5 then fcast = dblarr(n_future) $
  else fcast = fltarr(n_future)

  ;Compute coefficients.
  if keyword_set(reflect) eq 0 then $
    ar_coef = ts_coef(x, p) $
  else ar_coef = ts_coef(x, p, /reflect)
  ;Note: The REFLECT keyword implements a reflection algorithm to 
  ;      stabilize the coefficients that lie outside the complex 
  ;      unit circle. This algorithm may be included in the future.

  ;n_elements(ar_coef) must equal p.

  for j = 0, n_future-1 do begin
    yn = total(data * ar_coef)
    ;data = [yn, data(0:n_future-2)]
    data = [yn, data(0:p-2)]
    fcast(j) = yn
  endfor

  return, fcast

end


