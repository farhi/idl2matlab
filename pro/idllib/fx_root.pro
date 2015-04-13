;$Id: fx_root.pro,v 1.2 1994/11/29 18:37:26 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       FX_ROOT
;
; PURPOSE:
;       This function computes real and complex roots (zeros) of
;       a univariate nonlinear function.
;
; CATEGORY:
;       Nonlinear Equations/Root Finding
;
; CALLING SEQUENCE:
;       Result = FX_ROOT(X, Func)
;
; INPUTS:
;       X :      A 3-element initial guess vector of type real or complex.
;                Real initial guesses may result in real or complex roots.
;                Complex initial guesses will result in complex roots.
;
;       Func:    A scalar string specifying the name of a user-supplied IDL
;                function that defines the univariate nonlinear function.
;                This function must accept the vector argument X.
;
; KEYWORD PARAMETERS:
;       DOUBLE:  If set to a non-zero value, computations are done in
;                double precision arithmetic. 
;
;       ITMAX:   Set this keyword to specify the maximum number of iterations
;                The default is 100.
;       
;       STOP:    Set this keyword to specify the stopping criterion used to
;                judge the accuracy of a computed root, r(k). 
;                STOP = 0 implements an absolute error criterion between two
;                successively-computed roots, |r(k) - r(k+1)|.
;                STOP = 1 implements a functional error criterion at the 
;                current root, |Func(r(k))|. The default is 0.
;
;       TOL:     Set this keyword to specify the stopping error tolerance.
;                If the STOP keyword is set to 0, the algorithm stops when
;                |x(k) - x(k+1)| < TOL.
;                If the STOP keyword is set to 1, the algorithm stops when 
;                |Func(x(k))| < TOL. The default is 1.0e-4.
;
; EXAMPLE:
;       Define an IDL function named FUNC.
;         function FUNC, x
;           return, exp(sin(x)^2 + cos(x)^2 - 1) - 1 
;         end 
;
;       Define a real 3-element initial guess vector.
;         x = [0.0, -!pi/2, !pi]
;
;       Compute a root of the function using double-precision arithmetic.
;         root = FX_ROOT(x, 'FUNC', /double)
;
;       Check the accuracy of the computed root.
;         print, exp(sin(root)^2 + cos(root)^2 - 1) - 1
;
;       Define a complex 3-element initial guess vector.
;         x = [complex(-!pi/3, 0), complex(0, !pi), complex(0, -!pi/6)]
;
;       Compute a root of the function.
;         root = FX_ROOT(x, 'FUNC')
;
;       Check the accuracy of the computed root.
;         print, exp(sin(root)^2 + cos(root)^2 - 1) - 1
;
; PROCEDURE:
;       FX_ROOT implements an optimal Muller's method using complex 
;       arithmetic only when necessary.
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, March 1994
;       Modified:    GGS, RSI, September 1994
;                    Added support for double-precision complex inputs.
;-

function fx_root, xi, func, double = double, itmax = itmax, $
                            stop = stop, tol = tol  

  on_error, 2 ;Return to caller if error occurs.

  x = xi + 0.0 ;Create an internal floating-point variable, x.
  sx = size(x)
  if sx(1) ne 3 then $
    message, 'x must be a 3-element initial guess vector.'

  ;Initialize keyword parameters.
  if keyword_set(double) ne 0 then begin 
    if sx(2) eq 4 or sx(2) eq 5 then x = x + 0.0d $
    else x = dcomplex(x)
  endif
  if keyword_set(itmax)  eq 0 then itmax = 100
  if keyword_set(stop)   eq 0 then stop = 0
  if keyword_set(tol)    eq 0 then tol = 1.0e-4

  ;Initialize stopping criterion and iteration count.
  cond = 0  &  it = 0
 
  ;Begin to iteratively compute a root of the nonlinear function.
  while (it lt itmax and cond ne 1) do begin
    q = (x(2) - x(1))/(x(1) - x(0))
    pls = (1 + q)
    f = call_function(func, x)
    a = q*f(2) - q*pls*f(1) + q^2*f(0)
    b = (2*q+1)*f(2) - pls^2*f(1) + q^2*f(0)
    c = pls*f(2)
    disc = b^2 - 4*a*c
    roc = size(disc)  ;Real or complex discriminant?
    if roc(1) ne 6 and roc(1) ne 9 then begin  ;Proceed toward real root.   
      if disc lt 0 then begin  ;Switch to complex root.
        ;Single-precision complex.
        if keyword_set(double) eq 0 and sx(2) ne 9 then begin
          r0 = b + complex(0, sqrt(abs(disc)))
          r1 = b - complex(0, sqrt(abs(disc)))
        endif else begin ;Double-precision complex.
          r0 = b + dcomplex(0, sqrt(abs(disc)))
          r1 = b - dcomplex(0, sqrt(abs(disc)))
        endelse
        if abs(r0) gt abs(r1) then div = r0 $  ;Maximum modulus.
          else div = r1
       endif else $
        div = max([abs(b + sqrt(disc)), abs(b - sqrt(disc))]) ;Real root. 
    endif else begin  ;Proceed toward complex root.
      c0 = b + sqrt(disc)
      c1 = b - sqrt(disc)
      if abs(c0) gt abs(c1) then div = c0 $  ;Maximum modulus.
        else div = c1
    endelse
    root = x(2) - (x(2) - x(1)) * (2 * c/div) 
    ;Absolute error tolerance.
    if stop eq 0 and abs(root - x(2)) le tol then cond = 1 $ 
    else $  
    ;Functional error tolerance. 
    if stop ne 0 and abs(call_function(func, root)) le tol then cond = 1
    x = [x(1), x(2), root] 
    it = it + 1
  endwhile
  if it ge itmax and cond eq 0 then $
    message, 'Algorithm failed to converge within given parameters.'
  return, root

end

