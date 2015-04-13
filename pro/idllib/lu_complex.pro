;$Id: lu_complex.pro,v 1.6 1994/11/29 18:42:55 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       LU_COMPLEX
;
; PURPOSE:
;       This function solves an N by N complex linear system using
;       LU decomposition. The result is an N-element complex vector.
;       Alternatively, this function computes the generalized inverse 
;       of an N by N complex array using LU decomposition. The result 
;       is an N by N complex array.
;
; CATEGORY:
;       Complex Linear Algebra.
;
; CALLING SEQUENCE:
;       Result = LU_COMPLEX(A, B)
;
; INPUTS:
;       A:    An N by N complex array.
;
;       B:    An N-element right-side vector (real or complex).
;     
; KEYWORD PARAMETERS:
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
;      INVERSE: If set to a non-zero value, the generalized inverse of A
;               is computed. In this case the input parameter B is ignored.
;
;       SPARSE: If set to a non-zero value, the input array is converted
;               to row-indexed sparse storage format. Computations are
;               done using the iterative biconjugate gradient method.
;               This keyword is effective only when solving complex linear
;               systems. This keyword has no effect when calculating the
;               generalized inverse.
;
; EXAMPLE:
;       1) Define a complex array (A) and right-side vector (B).
;            A = [[complex(1, 0), complex(2,-2), complex(-3,1)], $
;                 [complex(1,-2), complex(2, 2), complex(1, 0)], $
;                 [complex(1, 1), complex(0, 1), complex(1, 5)]]
;            B =  [complex(1, 1), complex(3,-2), complex(1,-2)]
;
;          Solve the complex linear system (Az = B) for z.
;            z = LU_COMPLEX(a, b)
;
;        2) Compute the generalized inverse of A.
;            inv = LU_COMPLEX(a, b, /inverse)
;
; PROCEDURE:
;       LU_COMPLEX solves the complex linear system Az = b using
;       LU decomposition. If the SPARSE keyword is set, the coefficient
;       array is converted to row-indexed sparse storage format and the 
;       system is solved using the iterative biconjugate gradient method.
;       LU_COMPLEX computes the generalized inverse of the complex
;       array A using LU decomposition if B is supplied as an arbitrary
;       scalar value or if the INVERSE keyword is set.
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, October 1993
;       Modified:    GGS, RSI, February 1994
;                    Transposing the array prior to calling LU_COMPLEX
;                    is no longer necessary. LU_COMPLEX is now able to
;                    compute the generalized inverse of an N by N complex
;                    array using LU decomposition.
;       Modified:    GGS, RSI, June 1994
;                    Included support for sparse complex arrays using the
;                    Numerical Recipes functions NR_SPRSIN and NR_LINBCG.
;       Modified:    GGS, RSI, Decemberber 1994
;                    Added support for double-precision complex inputs.
;                    Reduced internal memory allocation requirements.
;                    Added INVERSE keyword. New documentation header.
;-

function lu_complex, a, b, double = double, inverse = inverse, sparse = sparse

  on_error, 2  ;Return to caller if error occurs.

  if n_params() ne 2 then $ 
    message, 'Incorrect number of input parameters.'

  dimension = size(a)
  if dimension(1) ne dimension(2) then $
    message, 'Input array must be square.'

  if keyword_set(double) eq 0 then double = 0 else $
    double = 1

  ;Double-precision complex.
  if dimension(3) eq 9 or keyword_set(double) ne 0 then begin
    comp = [[double(a), -imaginary(a)], $
            [imaginary(a), double(a)]]

    ;Generalized inverse of A (does not depend upon SPARSE keyword).
    if n_elements(b) eq 1 or keyword_set(inverse) then begin
      vec = dblarr(2L*dimension(1))
      inv = dcomplexarr(dimension(1), dimension(1))
      ;Compute the LU decomp only once and iterate on it!
      ludc, comp, index, double = double
      for k = 0, dimension(1)-1 do begin
        vec(k) = 1.0d
        sol = lusol(comp, index, vec, double = double)
        vec(k) = 0.0d
        inv(k, *) = dcomplex(sol(0:dimension(1)-1), sol(dimension(1):*))
      endfor
      return, inv
      inv = 0 & sol = 0 & vec = 0;Clean up intermediate variables.
    endif else begin ;Solve Az = b
      s = size(b)
      ;Rhs complex?
      if s(s(0)+1) ne 6 and s(s(0)+1) ne 9 then $
        vec = [b, dblarr(n_elements(b))] $ ;No.
      else vec = [double(b), imaginary(b)] ;Yes.
      if keyword_set(SPARSE) eq 0 then begin ;Dense coefficient array.
        ludc, comp, index, double = double
        sol = lusol(comp, index, vec, double = double)
      endif else begin ;Sparse coefficient array.
        sol = linbcg(sprsin(comp), vec, replicate(1.0d, 2L*s(3)), $
                                                           double = double)
      endelse
      return, dcomplex(sol(0:dimension(1)-1), sol(dimension(1):*))
      sol = 0 & vec = 0;Clean up intermediate variables.
    endelse
  ;Single-precision complex.
  endif else if dimension(3) eq 6 then begin
    comp = [[float(a), -imaginary(a)], $
            [imaginary(a), float(a)]]

    ;Generalized inverse of A (does not depend upon SPARSE keyword).
    if n_elements(b) eq 1 or keyword_set(inverse) then begin
      vec = fltarr(2L*dimension(1))
      inv = complexarr(dimension(1), dimension(1))
      ;Compute the LU decomp only once and iterate on it!
      ludc, comp, index, double = double
      for k = 0, dimension(1)-1 do begin
        vec(k) = 1.0
        sol = lusol(comp, index, vec, double = double)
        vec(k) = 0.0
        inv(k, *) = complex(sol(0:dimension(1)-1), sol(dimension(1):*))
      endfor
      return, inv
      inv = 0 & sol = 0 & vec = 0 ;Clean up intermediate variables.
    endif else begin ;Solve Az = b
      s = size(b)
      ;Rhs complex?
      if s(s(0)+1) ne 6 and s(s(0)+1) ne 9 then $
        vec = [b, fltarr(n_elements(b))] $  ;No.
        else vec = [float(b), imaginary(b)] ;Yes.
      if keyword_set(SPARSE) eq 0 then begin ;Dense coefficient array.
        ludc, comp, index, double = double
        sol = lusol(comp, index, vec, double = double)
      endif else begin ;Sparse coefficient array.
        sol = linbcg(sprsin(comp), vec, replicate(1.0, 2L*s(3)), $
                                                         double = double)
      endelse
      return, complex(sol(0:dimension(1)-1), sol(dimension(1):*))
      sol = 0 & vec = 0;Clean up intermediate variables.
    endelse
  endif else message, 'Input array must be of complex type.'

end
