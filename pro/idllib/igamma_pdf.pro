;$Id: igamma_pdf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       IGAMMA_PDF
;
; PURPOSE:
;       This function computes the incomplete gamma function using a series
;       representation. It is called by the probability density functions
;       in this directory. See the function IGAMMA() in the "math"
;       subdirectory for the user-callable version of the incomplete gamma 
;       function.
;
; MODIFICATION HISTORY:
;       Modified by:  Jong Yi, Sept 1992
;                     Increased iterations in g_series.pro
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code.
;-

pro g_series, result, x, a
  ;Computes incomplete gamma function using a series representation.
  glog = lngamma(a)
  nsample = long(x/50.) > 1000l
  resarray = 1.0/(findgen(nsample) + a)
  resarray(1:*) = x*resarray(1:*)
  sum = 1.0/a
  for i = 1, nsample-1 do begin
    resarray(0) = resarray(0) * resarray(i)
    sum = sum + resarray(0)
    if (abs(resarray(0)) lt abs(sum)*3.0e-7) then begin
      result = sum * exp(-x + a * alog(x) - glog)
      return
    endif
  endfor
  result = -1
  return
end

pro g_fract, result, x, a
  glog = lngamma(a)
  gd = 0.0 & fc = 1.0 & b1 = 1.0
  bo = 0.0 & ao = 1.0
  a1 = x
  for n = 1,100 do begin
    an = float(n)
    ana = an -a
    ao = (a1 +ao * ana) * fc
    bo = (b1 + bo *ana) * fc
    anf = an * fc
    a1 = x * ao + anf * a1
    b1 = x * bo + anf * b1
    if a1 then begin
      fc = 1.0/a1
      g = b1 * fc
      if abs((g-gd)/g) LT 3.0e-7 then begin
        result = exp(-x + a * alog(x) - glog) * g
        return
      endif
      gd = g
    endif
  endfor
  result = -1
  return
end

function igamma_pdf, a, x
  if x lt a+1.0 then begin
    g_series, result, x, a
    return, result
  endif else begin
    g_fract, result, x, a
    if result ne -1 then return, (1.0 - result) $
    else return, -1
  endelse
end
 

  
     



