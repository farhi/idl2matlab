; Copyright (c) 1995, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       INT_TABULATED
;
; PURPOSE:
;       This function integrates a tabulated set of data { x(i) , f(i) },
;       on the closed interval [min(X) , max(X)].
;
; CATEGORY:
;       Numerical Analysis.
;
; CALLING SEQUENCE:
;       Result = INT_TABULATED(X, F)
;
; INPUTS:
;       X:  The tabulated X-value data. This data may be irregularly
;           gridded and in random order. If the data is randomly ordered
;	    you must set the SORT keyword to a nonzero value.
;       F:  The tabulated F-value data. Upon input to the function
;           X(i) and F(i) must have corresponding indices for all
;	    values of i. If X is reordered, F is also reordered.
;
;       X and F must be of floating point or double precision type.
;
; KEYWORD PARAMETERS:
;       SORT:   A zero or non-zero scalar value.
;               SORT = 0 (the default) The tabulated x-value data is
;                        already in ascending order.
;               SORT = 1 The tabulated x-value data is in random order
;                        and requires sorting into ascending order. Both
;			 input parameters X and F are returned sorted.
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
; OUTPUTS:
;       This fuction returns the integral of F computed from the tabulated
;	data in the closed interval [min(X) , max(X)].
;
; RESTRICTIONS:
;       Data that is highly oscillatory requires a sufficient number
;       of samples for an accurate integral approximation.
;
; PROCEDURE:
;       INT_TABULATED.PRO constructs a regularly gridded x-axis with a
;	number of segments as an integer multiple of four. Segments
;	are processed in groups of four using a 5-point Newton-Cotes
;	integration formula.
;       For 'sufficiently sampled' data, this algorithm is highly accurate.
;
; EXAMPLES:
;       Example 1:
;       Define 11 x-values on the closed interval [0.0 , 0.8].
;         x = [0.0, .12, .22, .32, .36, .40, .44, .54, .64, .70, .80]
;
;       Define 11 f-values corresponding to x(i).
;         f = [0.200000, 1.30973, 1.30524, 1.74339, 2.07490, 2.45600, $
;              2.84299,  3.50730, 3.18194, 2.36302, 0.231964]
;
;       Compute the integral.
;         result = INT_TABULATED(x, f)
;
;       In this example, the f-values are generated from a known function,
;       (f = .2 + 25*x - 200*x^2 + 675*x^3 - 900*x^4 + 400*x^5)
;
;       The Multiple Application Trapazoid Method yields;  result = 1.5648
;       The Multiple Application Simpson's Method yields;  result = 1.6036
;				INT_TABULATED.PRO yields;  result = 1.6232
;         The Exact Solution (4 decimal accuracy) yields;  result = 1.6405
;
;	Example 2: 
;       Create 30 random points in the closed interval [-2 , 1].
;         x = randomu(seed, 30) * 3.0 - 2.0
;
;       Explicitly define the interval's endpoints.
;         x(0) = -2.0  &  x(29) = 1.0
;
;       Generate f(i) corresponding to x(i) from a given function.
;         f = sin(2*x) * exp(cos(2*x))
;
;       Call INT_TABULATED with the SORT keyword.
;         result = INT_TABULATED(x, f, /sort)
;
;       In this example, the f-values are generated from the function,
;       f = sin(2*x) * exp(cos(2*x))
;
;       The result of this example will vary because the x(i) are random.
;       Executing this example three times gave the following results:
;		               INT_TABULATED.PRO yields;  result = -0.0702
;		               INT_TABULATED.PRO yields;  result = -0.0731
;		               INT_TABULATED.PRO yields;  result = -0.0698
;        The Exact Solution (4 decimal accuracy) yields;  result = -0.0697
;
; MODIFICATION HISTORY:
;           Written by:  GGS, RSI, September 1993
;           Modified:    GGS, RSI, November  1993
;                        Use Numerical Recipes cubic spline interpolation 
;                        function NR_SPLINE/NR_SPLINT. Execution time is 
;                        greatly reduced. Added DOUBLE keyword. The 'sigma' 
;                        keyword is no longer supported.
;           Modified:    GGS, RSI, April  1995
;                        Changed cubic spline calls from NR_SPLINE/NR_SPLINT
;                        to SPL_INIT/SPL_INTERP. Improved double-precision
;                        accuracy. 
;-

function int_tabulated, x, f, double = double, sort = sort

  ;Return to caller if an error occurs.
  on_error, 2 

  x_elems = n_elements(x)
  x_sgmts = x_elems - 1L

  if x_elems ne n_elements(f) then $
    message, 'x and f must be vectors of equal length.'

  ;Sort vectors into ascending order.
  if keyword_set(sort) ne 0 then begin
    i = sort(x)
    x = x(i)
    f = f(i)
  endif

  while (x_sgmts mod 4L) ne 0L do $
    x_sgmts = x_sgmts + 1L

  xmin = min(x)
  xmax = max(x)

  ;Single-precision computations.
  if keyword_set(double) eq 0 then begin 
    ;Uniform step size.
    h = (xmax - xmin) / (x_sgmts + 0.)
    ;x values of interpolates.
    xgrid = h * findgen(x_sgmts + 1L) + xmin
    ;Compute the interpolates.
    z = spl_interp(x, f, spl_init(x, f), xgrid)
    i = 4L
    intgrl = 0.0
    z_sgmts = n_elements(z) - 1L
    ;Compute the integral using the 5-point Newton-Cotes formula.
    while i le z_sgmts do begin
      intgrl = intgrl + $
               2. * h * (7. * (z(i-4) + z(i)) + $
               32. * (z(i-3) + z(i-1)) + 12. * z(i-2)) / 45.
      i = i + 4L
    endwhile
  endif $
  ;Double-precision computations.
  else begin
    ;Uniform step size.
    h = (xmax - xmin) / (x_sgmts + 0.0d)
    ;x values of interpolates.
    xgrid = h * dindgen(x_sgmts + 1L) + xmin
    ;Compute the interpolates.
    z = spl_interp(x, f, spl_init(x, f, /double), xgrid, /double)
    i = 4L
    intgrl = 0.0d
    z_sgmts = n_elements(z) - 1L
    ;Compute the integral using the 5-point Newton-Cotes formula.
    while i le z_sgmts do begin
      intgrl = intgrl + $
               2d * h * (7d * (z(i-4) + z(i)) + $
               32d * (z(i-3) + z(i-1)) + 12d * z(i-2)) / 45d
      i = i + 4L
    endwhile
  endelse
 
  return, intgrl

end

