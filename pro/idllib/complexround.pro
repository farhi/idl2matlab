;$Id: complexround.pro,v 1.3 1994/11/29 18:31:31 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       COMPLEXROUND
;
; PURPOSE:
;       This function rounds a complex scalar or array.
;
; CATEGORY:
;       Numerical Analysis.
;
; CALLING SEQUENCE:
;       Result = Complexround(z)
;
; INPUTS:
;       Z: A complex scalar or array.
;
; RESTRICTIONS:
;       The input argument must be complex.
;
; PROCEDURE:
;       This function rounds the real and imaginary components of the 
;       complex input argument.
;
; EXAMPLE:
;       ;Define a complex array.
; 	  z = [[complex(1.245, 3.880), complex( 1.245, -3.880)], $
;              [complex(1.499, 5.501), complex(-1.355, -2.115)]]
;       ;Round it.
;         result = complexround(z) 
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1992
;       Modified:    GGS, RSI, September 1994
;                    Added support for double-precision complex inputs.
;                    Uses IDL's intrinsic ROUND function.
;-

function complexround, z

  ;dimension = size(input)  ;Size of input array.
  ;output = complexarr(dimension(1), dimension(2))
  ;real = float(input) ;Separate into real and imaginary. 
  ;imag = imaginary(input)

  ;z1 = real + 0.5 ;Round real components.
  ;neg1 = where(real lt 0, count1)
  ;if count1 ne 0 then z1(neg1) = z1(neg1) - 1
  ;z1 = fix(z1)

  ;z2 = imag + 0.5 ;Round imaginary components.
  ;neg2 = where(imag lt 0, count2)
  ;if count2 ne 0 then z2(neg2) = z2(neg2) - 1
  ;z2 = fix(z2)

  ;output = complex(z1,z2)
  ;return, complex(z1,z2)

  on_error, 2
 
  sz = size(z)

  if sz(sz(0)+1) eq 6 then begin 
    return, complex(round(float(z)), round(imaginary(z))) 
  endif else if sz(sz(0)+1) eq 9 then $
    return, dcomplex(round(float(z)), round(imaginary(z))) $
  else message, 'Input must be of complex type.'

end
