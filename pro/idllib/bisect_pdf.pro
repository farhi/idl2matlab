;$Id: bisect_pdf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       BISECT_PDF
;
; PURPOSE:
;       This function computes the cutoff value x such that the probabilty
;       of an observation from the given distribution, less than x, is a(0).
;       u and l are the upper and lower limits for x, respectively.
;       a(1) and a(2) are degrees of freedom, if appropriate.
;       funct is a string specifying the probability density function.
;       BISECT_PDF is not intended to be a user-callable function.
;-

function bisect_pdf, a, funct, u, l, del
  sa = size(a)
  if (n_elements(del) eq 0) then del = 1.0e-6
  p = a(0)
  if (p lt 0 or p gt 1) then return, -1
  up = u
  low = l
  mid = l + (u - l) * p
  count = 1
  while (abs(up - low) gt del*mid) and (count lt 100) do begin
   if n_elements(z) ge 1 then begin 
     if z gt p then  up = mid else low = mid
     mid = (up + low)/2.
   endif
  case n_elements(a) of
    1: z = call_function(funct, mid)
    2: z = call_function(funct, mid, a(1))
    3: z = call_function(funct, mid, a(1), a(2))
    else: return, -1
  endcase
  count = count + 1
  endwhile
  return, mid
end



  
