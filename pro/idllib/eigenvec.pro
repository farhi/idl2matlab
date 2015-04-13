;$Id: eigenvec.pro,v 1.1 1994/11/29 18:36:08 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       EIGENVEC
;
; PURPOSE:
;       This function computes the eigenvectors of an N by N real, non-
;       symmetric array using inverse subspace iteration. The result is 
;       a complex array with a column dimension equal to N and a row 
;       dimension equal to the number of eigenvalues.
;
; CATEGORY:
;       Linear Algebra / Eigensystems
;
; CALLING SEQUENCE:
;       Result = Eigenvec(A, Eval)
;
; INPUTS:
;       A:    An N by N nonsymmetric array of type float or double.
;
;    EVAL:    An N-element complex vector of eigenvalues.
;
; KEYWORD PARAMETERS:
;       DOUBLE:  If set to a non-zero value, computations are done in
;                double precision arithmetic.
;
;        ITMAX:  The number of iterations performed in the computation
;                of each eigenvector. The default value is 4.
;
;     RESIDUAL:  Use this keyword to specify a named variable which returns
;                the residuals for each eigenvalue/eigenvector(lambda/x) pair.
;                The residual is based on the definition Ax - (lambda)x = 0
;                and is an array of the same size and type as RESULT. The rows
;                this array correspond to the residuals for each eigenvalue/
;                eigenvector pair. This keyword must be initialized to a non-
;                zero value before calling EIGENVEC if the residuals are 
;                desired.
;
; EXAMPLE:
;       Define an N by N real, nonsymmetric array.
;         a = [[1.0, -2.0, -4.0,  1.0], $
;              [0.0, -2.0,  3.0,  4.0], $
;              [2.0, -6.0, -1.0,  4.0], $
;              [3.0, -3.0,  1.0, -2.0]]
;
;       Compute the eigenvalues of A using double-precision complex arithmetic.
;         eval = HQR(ELMHES(a), /double)
;
;       Print the eigenvalues. The correct solution should be:
;       (0.26366259, -6.1925899), (0.26366259, 6.1925899), $
;       (-4.9384492,  0.0000000), (0.41112397, 0.0000000)
;         print, eval
;
;       Compute the eigenvectors of A. The eigenvectors are returned in the 
;       rows of EVEC. The residual keyword must be initialized as a nonzero
;       value prior to calling EIGENVEC.
;         residual = 1
;         result = EIGENVEC(a, eval, residual = residual)
;
;       Print the eigenvectors.
;         print, evec(*,0), evec(*,1), evec(*,2), evec(*,3)
;
;       The accuracy of each eigenvalue/eigenvector (lamda/x) 
;       pair may be checked by printing the residual array. This array is the
;       same size and type as RESULT and returns the residuals as its rows.
;       The residual is based on the mathematical definition of an eigenvector,
;       Ax - (lambda)x = 0. All residual values should be floating-point zeros.
;         print, residual
;
; PROCEDURE:
;       EIGENVEC computes the set of eigenvectors that correspond to a given 
;       set of eigenvalues using Inverse Subspace Iteration. The eigenvectors 
;       are computed up to a scale factor and are of Euclidean length. The
;       existence and uniqueness of eigenvectors are not guaranteed.
;
; MODIFICATION HISTORY:
;           Written by:  GGS, RSI, December 1994
;-

function eigenvec, a, eval, double = double, itmax = itmax, $
                                             residual = residual

  on_error, 2  ;Return to caller if error occurs.

  if n_params() ne 2 then $
    message, 'Incorrect number of input arguments.'
    
  sa = size(a)
  seval = size(eval)
  if sa(1) ne sa(2) then $
    message, 'Input array must be square.'

  ;Set default values for keyword parameters.
  if keyword_set(double) eq 0 then double =  0
  if keyword_set(itmax)  eq 0 then itmax  =  4

  enum = n_elements(eval)          ;Number of eigenvalues.
  diag = indgen(sa(1)) * (sa(1)+1) ;Diagonal indices.

  ;Double Precision.
  if seval(2) eq 9 or keyword_set(double) ne 0 then begin
    evec = dcomplexarr(sa(1), enum) ;Eigenvector storage array with number
                                    ;of rows equal to number of eigenvalues.
    for k = 0, enum - 1 do begin
      alud = a  ;Create a copy of the array for next eigenvalue computation.
      if imaginary(eval(k)) ne 0 then begin ;Complex eigenvalue.
        alud = dcomplex(alud)
        alud(diag) = alud(diag) - eval(k)
        ;re = double(alud)
        ;im = imaginary(alud)
        ;Avoid intermediate variables.
        comp = [[double(alud), -imaginary(alud)], $
                [imaginary(alud), double(alud)]]
        b = replicate(1.0d, 2*sa(1)) / sqrt(2.0d * sa(1)) ;Initial eigenvector.
        LUDC, comp, index, double = double
        it = 0
        while it lt itmax do begin ;Iteratively compute the eigenvector.
          x = LUSOL(comp, index, b, double = double)
          b = x / sqrt(total(x^2, 1, double = double)) ;Normalize eigenvector.
          it = it + 1
        endwhile
        evec(*, k) = dcomplex(b(0:sa(1)-1), b(sa(1):*)) ;Row vector storage.
      endif else begin ;Real eigenvalue
        alud(diag) = alud(diag) - double(eval(k))
        b = replicate(1.0d, sa(1)) / sqrt(sa(1)+0.0d)
        LUDC, alud, index, double = double
        it = 0
        while it lt itmax do begin
          x = LUSOL(alud, index, b, double = double)
          b = x / sqrt(total(x^2, 1, double = double)) ;Normalize eigenvector.
          it = it + 1
        endwhile
        evec(*, k) = dcomplex(b, 0.0d0) ;Row vector storage.
      endelse
    endfor
    if keyword_set(residual) then begin ;Compute eigenvalue/vector residuals.
      ;Because RESIDUAL is a keyword that envokes functionality and returns
      ;a named variable, it must be initialized before calling EIGENVEC().
      residual = dcomplexarr(sa(1), enum) ;Dimensioned the same as evec.
        for k = 0, enum - 1 do $
          residual(*,k) = (a##evec(*,k)) - (eval(k) * evec(*,k))
    endif
  endif else begin ;Single Precision.
    evec = complexarr(sa(1), enum) ;Eigenvector storage array.
    for k = 0, enum - 1 do begin
      alud = a  ;Create a copy of the array for next eigenvalue computation.
      if imaginary(eval(k)) ne 0 then begin ;Complex eigenvalue.
        alud = complex(alud)
        alud(diag) = alud(diag) - eval(k)
        ;re = float(alud)
        ;im = imaginary(alud)
        ;Avoid intermediate variables.
        comp = [[float(alud), -imaginary(alud)], $
                [imaginary(alud), float(alud)]]
        b = replicate(1.0, 2*sa(1)) / sqrt(2.0 * sa(1)) ;Initial eigenvector.
        LUDC, comp, index, double = double
        it = 0
        while it lt itmax do begin ;Iteratively compute the eigenvector. 
          x = LUSOL(comp, index, b, double = double)
          b = x / sqrt(total(x^2, 1)) ;Normalize eigenvector.
          it = it + 1
        endwhile
        evec(*, k) = complex(b(0:sa(1)-1), b(sa(1):*)) ;Row vector storage.
      endif else begin ;Real eigenvalue 
        alud(diag) = alud(diag) - float(eval(k))
        b = replicate(1.0, sa(1)) / sqrt(sa(1))
        LUDC, alud, index, double = double
        it = 0
        while it lt itmax do begin
          x = LUSOL(alud, index, b, double = double)
          b = x / sqrt(total(x^2, 1))  ;Normalize eigenvector.
          it = it + 1
        endwhile
        evec(*, k) = complex(b, 0.0) ;Row vector storage.
      endelse
    endfor
    if keyword_set(residual) then begin ;Compute eigenvalue/vector residuals.
      ;Because RESIDUAL is a keyword that envokes functionality and returns
      ;a named variable, it must be initialized before calling EIGENVEC().
      residual = complexarr(sa(1), enum) ;Dimensioned the same as evec.
        for k = 0, enum - 1 do $
          residual(*,k) = (a##evec(*,k)) - (eval(k) * evec(*,k))
    endif
  endelse
    
  return, evec
end

