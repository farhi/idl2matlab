;$Id: ts_coef.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       TS_COEF
;
; PURPOSE:
;       This function computes the coefficients used in a Pth order
;       autoregressive time-series forecasting model. The result is
;       a P-element vector whose type is identical to X.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = TS_COEF(X, P)
;
; INPUTS:
;       X:    An n-element vector of type float or double containing time-
;             series samples.
;
;       P:    A scalar of type integer or long integer that specifies the
;             number of coefficients to be computed.
;
; KEYWORD PARAMETERS:
;     MSE:    Use this keyword to specify a named variable which returns the 
;             mean square error of the Pth order autoregressive model.
;
; EXAMPLE:
;       Define an n-element vector of time-series samples.
;         x = [6.63, 6.59, 6.46, 6.49, 6.45, 6.41, 6.38, 6.26, 6.09, 5.99, $
;              5.92, 5.93, 5.83, 5.82, 5.95, 5.91, 5.81, 5.64, 5.51, 5.31, $
;              5.36, 5.17, 5.07, 4.97, 5.00, 5.01, 4.85, 4.79, 4.73, 4.76]
;
;       Compute the coefficients of a 5th order autoregressive model.
;         result = TS_COEF(x, 5)
;
;       The result should be:
;         [1.30168, -0.111783, -0.224527. 0.267629, -0.233363]
;
; REFERENCE:
;       The Analysis of Time Series, An Introduction (Fourth Edition)
;       Chapman and Hall
;       ISBN 0-412-31820-2
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, November 1994
;-

function ts_coef, x, p, mse = mse, reflect = reflect

  ;Compute the coefficients of the Pth order autoregressive model
  ;used in time-series forecasting.
  ;AR_COEF = AR_COEF(0, 1, ... , p-1) 

  nx = n_elements(x)

  mse = total(x^2) / nx
  
  sx = size(x)
  ;if sx(2) eq 5 then ar_coef = dblarr(p) $
  ;else ar_coef = fltarr(p)
  ar_coef = dblarr(p)

  ;Do all intermediate calculations in double-precision.
  str1 = [0.0d, x(0:nx-2), 0.0]
  str2 = [0.0d, x(1:nx-1), 0.0]
  str3 = dblarr(nx+1)

  for k = 1, p do begin
    ar_coef(k-1) = 2.0 * total(str1(1:nx-k) * str2(1:nx-k)) / $
                     total(str1(1:nx-k)^2 + str2(1:nx-k)^2)
    mse = mse * (1.0 - ar_coef(k-1)^2)
    for i = 1, k-1 do $
      ar_coef(i-1) = str3(i) - ar_coef(k-1) * str3(k-i)

    ;if k = p then skip the remaining computations.
    if k eq p then goto, return_ar_coef

    str3(1:k) = ar_coef(0:k-1)
    for j = 1, nx-k-1 do begin
      str1(j) = str1(j) - str3(k) * str2(j)
      str2(j) = str2(j+1) - str3(k) * str1(j+1)
    endfor
  endfor

  return_ar_coef:
  ;if keyword_set(reflect) ne 0 then $     ;Reflect the coefficients
  ;  return, reflect(ar_coef) $            ;for stability.
  ;else return, ar_coef
  ;;Note: The REFLECT keyword implements a reflection algorithm to
  ;;      stabilize the coefficients that lie outside the complex
  ;;      unit circle. This algorithm may be included in the future.
 
  if sx(2) eq 5 then return, ar_coef $
  else return, float(ar_coef)

end
