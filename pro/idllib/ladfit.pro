;$Id: ladfit.pro,v 1.5 1995/07/20 19:35:46 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       LADFIT
;
; PURPOSE:
;       This function fits the paired data {X(i), Y(i)} to the linear model,
;       y = A + Bx, using a "robust" least absolute deviation method. The 
;       result is a two-element vector containing the model parameters, A 
;       and B.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = LADFIT(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An n-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;  ABSDEV:    Use this keyword to specify a named variable which returns the
;             mean absolute deviation for each data-point in the y-direction.
;
; EXAMPLE:
;       Define two n-element vectors of paired data.
;         x = [-3.20, 4.49, -1.66, 0.64, -2.43, -0.89, -0.12, 1.41, $
;               2.95, 2.18,  3.72, 5.26]
;         y = [-7.14, -1.30, -4.26, -1.90, -6.19, -3.98, -2.87, -1.66, $
;              -0.78, -2.61,  0.31,  1.74]
;       Compute the model parameters, A and B.
;         result = ladfit(x, y, absdev = absdev)
;       The result should be the two-element vector:
;         [-3.15191, 0.930440]
;       The keyword parameter should be returned as:
;         absdev = 0.636851
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1994
;       Modified:    GGS, RSI, July 1995
;                    Corrected an infinite loop condition that occured when
;                    the X input parameter contained mostly negative data.
;-

function sign, z, d
  ;if d lt 0 then z = -abs(z) $
  ;else if d eq 0 then z = 0 $
  ;else if d gt 0 then z = abs(z)
  ;return, z
  return, abs(z) * d / abs(d)
end

function mdfunc, b, x, y, arr, aa, absdev, nx
  on_error, 2
  eps = 1.0e-7
  arr = y - b*x
  if nx mod 2 eq 0 then $;X is of even length.
    ;j = nx / 2
    ;Average Kth and K-1st medians.
    aa = 0.5 * (median(arr(0:nx-2)) + median(arr)) $
  else $
    aa = median(arr)
  sum = 0
  absdev = 0

  ;for k = 0L, nx-1 do begin
  ;  d = y(k) - (b * x(k) + aa)
  ;  absdev = absdev + abs(d)
  ;  if y(k) ne 0 then d = d / abs(y(k))
  ;  if abs(d) gt eps then sum = sum + sign(x(k), d)
  ;endfor

  d = y - (b * x + aa)
  absdev = total(abs(d))
  zeros = where(y ne 0)
  d = d(zeros) / abs(y(zeros))  
  zeros = where(abs(d) gt eps, nzeros)
  if nzeros ne 0 then $
    sum = total(x(zeros)*sign(replicate(1.0, nzeros), d(zeros))) 
  return, sum
end

function ladfit, x, y, absdev = absdev
  on_error, 2

  nx = n_elements(x)
  if nx ne n_elements(y) then $
    message, 'x and y must be vectors of equal length.'

  sx = total(x)
  sy = total(y)
  sxy = total(x*y)
  sxx = total(x*x)
  del = nx * sxx - sx^2
  aa = (sxx * sy - sx * sxy) / del
  bb = (nx * sxy - sx * sy) / del
  chisqr = total((y - (aa + bb*x))^2)
  sigb = sqrt(chisqr / del)
  b1 = bb
  f1 = mdfunc(b1, x, y, arr, aa, absdev, nx)
  b2 = bb + sign(3.0 * sigb, f1) 
  f2 = mdfunc(b2, x, y, arr, aa, absdev, nx)
  while f1*f2 gt 0 do begin
    bb = 2.0 * b2 - b1
    b1 = b2
    f1 = f2
    b2 = bb
    f2 = mdfunc(b2, x, y, arr, aa, absdev, nx)
  endwhile
  sigb = 0.01 * sigb
  while abs(b2-b1) gt sigb do begin
    bb = 0.5 * (b1 + b2)
    if bb eq b1 or bb eq b2 then begin
      absdev = absdev / nx
      return, [aa, bb]
    endif else begin
      f = mdfunc(bb, x, y, arr, aa, absdev, nx)
      if f*f1 ge 0 then begin
        f1 = f
        b1 = bb
      endif else begin
        f2 = f
        b2 = bb
      endelse
    endelse
  endwhile
  absdev = absdev / nx
  return, [aa, bb]
end

