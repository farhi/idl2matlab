;$Id: gs_iter.pro,v 1.4 1994/04/28 18:20:26 doug Exp $
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       GS_ITER
;
; PURPOSE:
;       This function solves an n by n linear system of equations
;       using Gauss-Seidel iteration.
;
; CATEGORY:
;       Linear Algebra.
;
; CALLING SEQUENCE:
;       Result = GS_ITER(A, B)
;
; INPUTS:
;       A:      An N by N array of type: int, float, or double.
;
;       B:      An N-element vector of type: int, float, or double.
;
; KEYWORD PARAMETERS:
;       CHECK:    An integer value of 0 or 1 that denies or requests
;                 checking A for a diagonally dominant form.
;                 CHECK = 0 (the default) results in no checking.
;                 CHECK = 1  Checks A and reports if it does not
;                            meet the required condition. This is
;                            just a warning. The algorithm will
;                            proceed on the chance it may converge.
;
;       LAMBDA:   A scalar value in the range: [0.0, 2.0]
;                 This value determines the amount of 'RELAXATION'.
;                 Relaxation is a weighting technique that is used
;                 to enhance convergence.
;                 1) LAMBDA = 1.0 (the default) no weighting.
;                 2) A value in the range  0.0 <= LAMBDA < 1.0  improves
;                    convergence in oscillatory and non-convergent systems.
;                 3) A value in the range  1.0 < LAMBDA <= 2.0  improves
;                    convergence in systems known to converge.
;
;       MAX_ITER: The maximum number of iterations allowable for the
;                 algorithm to converge to the solution. The default 
;                 is 30 iterations.
;         
;       X_0:      An N-element vector that provides the algorithm's 
;                 starting point. The default is [1.0, 1.0, ... , 1.0].       
;
;       TOL:      The relative error tolerance between current and past
;                 iterates calculated as:  ABS( (current-past)/current )
;                 The default is 1.0e-4.
;
; SIDE EFFECTS:
;       Upon output A and B are divided by the diagonal elements of A.
;       Integer inputs are converted to floats.
;
; RESTRICTIONS:
;       The equations must be entered in a DIAGONALLY DOMINANT form
;       to guarantee convergence.
;       A system is diagonally dominant if it satisfies the condition:
;                   abs(A(row,row)) > SUM(abs(A(row,col)))
;       where SUM runs col=1,N excluding col = row and A is in row major.
;       This restriction on A is known as a sufficient condition. That is,
;       it is sometimes possible for the algorithm to converge without the
;       condition being satisfied. But, convergence is guaranteed if the
;       condition is satisfied.
;
; EXAMPLE:
;       Define an array (a) in a non-diagonally dominant form.
;         a = [[ 1.0,  7.0, -4.0], $
;              [ 4.0, -4.0,  9.0], $
;              [12.0, -1.0,  3.0]]
;       And right-side vector (b).
;         b = [12.0, 2.0, -9.0]
;       Compute the solution of the system, ax = b.
;         result = gs_iter(a, b)
;       Note: This example fails to converge, because A is not in
;             diagonally dominant form.
;
;       Reorder the array given above into diagonally dominant form.
;         a = [[12.0, -1.0,  3.0], $
;              [ 1.0,  7.0, -4.0], $
;              [ 4.0, -4.0,  9.0]]
;       Make corresponding changes in the ordering of b.
;         b = [-9.0, 12.0, 2.0]
;       Compute the solution of the system, ax = b.
;         result = gs_iter(a, b)
;
; PROCEDURE:
;       GS_ITER.PRO implements the Gauss-Seidel iterative method with
;       over- and under- relaxation to enhance convergence.
;     
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, April 1993
;       Modified:    GGS, RSI, February 1994
;                    1) Format keyword is no longer supported. The matrix
;                       should be supplied in a row major format. 
;                    2) The input/output parameter X has been removed. The 
;                       algorithm's initial starting point is an n-element
;                       vector of 1s. The keyword X_0 has been added to 
;                       override the default.
;                    3) GS_ITER is now called as a function, x = gs_iter( ). 
;-

function GS_ITER, a, b, check = check, lambda = lambda, max_iter = max_iter, $
                        x_0 = x_0, tol = tol

  on_error, 2 ;Return to caller if error occurs.

  dim = size(a)
  if dim(1) ne dim(2) then $
    message, 'Input matrix is not square.'

  ;If inputs are not float or double convert to float.
  if dim(3) ne 4 and dim(3) ne 5 then $
    a = a + 0.0  &  b = b + 0.0 

  ; set default values for keyword parameters
  if keyword_set(lambda)   eq 0 then lambda = 1.0
  if keyword_set(max_iter) eq 0 then max_iter = 30
  if keyword_set(x_0)      eq 0 then x_0 = replicate(1.0, dim(1))
  if keyword_set(tol)      eq 0 then tol = 1.0e-4

  ;Diagonal elements of input matrix.
  diag = a(indgen(dim(1)) * (dim(1)+1))

  if keyword_set(check) ne 0 then begin
    sum = total(abs(a), 1) - abs(diag)
    caution = where(sum ge abs(diag), count)
    if count ne 0 then begin
      print, 'Input matrix is not in Diagonally Dominant form.' & $
      print, 'Algorithm may not converge.'
    endif
  endif

  ;Precondition inputs.
  ;Divide A and B by the diagonal elements of A.
  a = a / (replicate(1, dim(1)) # diag)
  b = b / diag

  cond = 0
  iter = 0

  ;Begin the computational loop and continue WHILE
  ;the number of iterations is less than max_iter
  ;AND the relative error between iterations is
  ;greater than tol.
  while(iter lt max_iter and cond eq 0) do begin
    cond = 1
    iter = iter + 1
  ;Formulate x_0 as the row vectors of A.
  for k = 0, dim(1)-1 do begin
    x_last = x_0(k)
    x_0(k) = lambda * (b(k) - (total(x_0*a(*,k),1)) + (a(k,k) * x_0(k))) + $
             (1.0 - lambda) * x_last
    if cond eq 1 and x_0(k) ne 0.0 then begin
      error = abs((x_0(k) - x_last) / x_0(k))
      if error gt tol then cond = 0
    endif
  endfor
  endwhile

  if iter ge max_iter and cond eq 0 then $
    message, 'Algorithm failed to converge within given parameters.'

  return, x_0

end



