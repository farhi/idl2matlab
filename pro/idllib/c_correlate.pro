;$Id: c_correlate.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       C_CORRELATE
;
; PURPOSE:
;       This function computes the cross correlation Pxy(L) or cross 
;       covariance Rxy(L) of two sample populations X and Y as a function
;       of the lag (L).
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = C_correlate(X, Y, Lag)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An n-element vector of type integer, float or double.
;
;     LAG:    A scalar or n-element vector, in the interval [-(n-2), (n-2)],
;             of type integer that specifies the absolute distance(s) between
;             indexed elements of X.
;
; KEYWORD PARAMETERS:
;       COVARIANCE:    If set to a non-zero value, the sample cross 
;                      covariance is computed.
;
; EXAMPLE
;       Define two n-element sample populations.
;         x = [3.73, 3.67, 3.77, 3.83, 4.67, 5.87, 6.70, 6.97, 6.40, 5.57]
;         y = [2.31, 2.76, 3.02, 3.13, 3.72, 3.88, 3.97, 4.39, 4.34, 3.95]
;
;       Compute the cross correlation of X and Y for LAG = -5, 0, 1, 5, 6, 7
;         lag = [-5, 0, 1, 5, 6, 7]
;         result = c_correlate(x, y, lag)
;
;       The result should be:
;         [-0.448655, 0.915846, 0.628814, -0.393466, -0.350169, -0.282198]
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

function cross_cov, x, y, m, nx
  ;Sample cross covariance function.

  xmean = total(x) / nx
  ymean = total(y) / nx
  t = lindgen(nx - m - 1L)
  return, total((x(t) - xmean) * (y(t + m) - ymean))

end

function c_correlate, x, y, lag, covariance = covariance

  ;Compute the sample cross correlation or cross covariance of 
  ;(Xt, Xt+l) and (Yt, Yt+l) as a function of the lag (l).

  on_error, 2

  nx = n_elements(x)
  ny = n_elements(y)

  if nx ne ny then $
    message, 'x and y must be vectors of equal length.'
  
  nlag = n_elements(lag)

  if nlag eq 1 then lag = [lag] ;Create a 1-element vector.

  xtype = size(x)
  ytype = size(y)
  if xtype(2) eq 5 or ytype(2) eq 5 then cross = dblarr(nlag) $
  else cross = fltarr(nlag)

  if keyword_set(covariance) eq 0 then begin ;Compute Cross Correlation.
    for k = 0, nlag-1 do begin
      if lag(k) ge 0 then $
        cross(k) = cross_cov(x, y, lag(k), nx) / $
                   sqrt(cross_cov(x, x, 0L, nx) * cross_cov(y, y, 0L, ny)) $
      else cross(k) = cross_cov(y, x, abs(lag(k)), ny) / $
                   sqrt(cross_cov(x, x, 0L, nx) * cross_cov(y, y, 0L, ny))
    endfor
  endif else begin ;Compute Cross Covariance.
    for k = 0, nlag-1 do begin
      if lag(k) ge 0 then $
        cross(k) = cross_cov(x, y, lag(k), nx) / nx $
      else cross(k) = cross_cov(y, x, abs(lag(k)), nx) / nx
    endfor
  endelse

  return, cross

end
  
