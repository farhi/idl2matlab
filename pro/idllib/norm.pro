;$Id: norm.pro,v 1.6 1994/11/29 18:44:16 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       NORM
;
; PURPOSE:
;       1) This function computes the Euclidean norm of a vector.
;          OR
;       2) This function computes the Infinity norm of an array.
;
; CATEGORY:
;       Complex Linear Algebra.
;
; CALLING SEQUENCE:
;       Result = NORM(A)
;
; INPUTS:
;       A:      An N-element real or complex vector.
;               An M by N real or complex array.
;
; KEYWORD PARAMETERS:
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
; EXAMPLE:
;       1) Define an N-element complex vector (a).
;            a = [complex(1, 0), complex(2,-2), complex(-3,1)]
;          Compute the Euclidean norm of (a).
;            result = norm(a)
;
;       2) Define an M by N complex array (a). 
;            a = [[complex(1, 0), complex(2,-2), complex(-3,1)], $
;                 [complex(1,-2), complex(2, 2), complex(1, 0)]]
;          Compute the Infinity norm of the complex array (a).
;            result = norm(a)
;
; PROCEDURE:
;       NORM.PRO computes the Euclidean norm of an N-element vector.
;       NORM.PRO computes the Infinity norm of an M by N array
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, April 1992
;       Modified:    GGS, RSI, February 1994
;                    Computes the Euclidean norm of an N-element vector.
;                    Accepts complex inputs. Added DOUBLE keyword.
;       Modified:    GGS, RSI, September 1994
;                    Added support for double-precision complex inputs.
;-

function NORM, a, double = double
  
  on_error, 2  ;Return to caller if error occurs.

  type = size(a) 

  if type(0) eq 1 then begin ;If vector, compute the Euclidean norm.
    if keyword_set(DOUBLE) ne 0 then begin 
      if type(2) ne 6 and type(2) ne 9 then a = double(a) $
      else a = dcomplex(a) 
    endif
    return, sqrt(total(abs(a)^2)) 
  endif else if type(0) eq 2 then begin ;If matrix, compute the Infinity norm.
    if keyword_set(DOUBLE) ne 0  then begin
      if type(3) ne 6 and type(3) ne 9 then a = double(a) $
      else a = dcomplex(a)
    endif
    return, max(total(abs(a),1))   ;Create the matrix of absolute values,
                                   ;add the elements of each row,
                                   ;and return the maximum.
  endif else message, $
    'Input must be an n-element vector or an M by N array.'

end
