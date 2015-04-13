;$Id: m_correlate.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       M_CORRELATE
;
; PURPOSE:
;       This function computes the multiple correlation coefficient of a
;       dependent variable and two or more independent variables.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE: 
;       Result = M_correlate(X, Y)
;                           
; INPUTS:
;       X:    An array of m-columns and n-rows of type integer, float or double
;             that specifies the independent variable data. The columns of this
;             two dimensional array correspond to the n-element vectors of 
;             independent variable data.
;
;       Y:    An n-element vector of type integer, float or double that
;             specifies the dependent variable data.
;
; EXAMPLE:
;       Define the independent (X) and dependent (Y) data.
;         X = [[0.477121, 2.0, 13.0], $
;              [0.477121, 5.0,  6.0], $
;              [0.301030, 5.0,  9.0], $
;              [0.000000, 7.0,  5.5], $
;              [0.602060, 3.0,  7.0], $
;              [0.698970, 2.0,  9.5], $
;              [0.301030, 2.0, 17.0], $
;              [0.477121, 5.0, 12.5], $
;              [0.698970, 2.0, 13.5], $
;              [0.000000, 3.0, 12.5], $
;              [0.602060, 4.0, 13.0], $
;              [0.301030, 6.0,  7.5], $
;              [0.301030, 2.0,  7.5], $
;              [0.698970, 3.0, 12.0], $
;              [0.000000, 4.0, 14.0], $
;              [0.698970, 6.0, 11.5], $
;              [0.301030, 2.0, 15.0], $
;              [0.602060, 6.0,  8.5], $
;              [0.477121, 7.0, 14.5], $
;              [0.000000, 5.0, 9.5]]
;
;          Y = [97.682, 98.424, 101.435, 102.266,  97.067,  97.397, 99.481, $
;               99.613, 96.901, 100.152,  98.797, 100.796,  98.750, 97.991, $
;              100.007, 98.615, 100.225,  98.388,  98.937, 100.617]
;
;       Compute the multiple correlation of Y on the first column of X.
;       The result should be 0.798816
;         result = m_correlate(X(0,*), Y)
;
;       Compute the multiple correlation of Y on the first two columns of X.
;       The result should be 0.875872
;         result = m_correlate(X(0:1,*), Y)
;
;       Compute the multiple correlation of Y on all columns of X.
;       The result should be 0.877197
;         result = m_correlate(X, Y)
;         
; PROCEDURE:
;       M_CORRELATE uses relationships based upon partial correlation to
;       compute the multiple correlation coefficient of linear models with
;       two or more independent variables: y(x0, x1), y(x0, x1, ... , xn-1).   
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Written by:   GGS, RSI, July 1994
;-

function  m_correlate, x, y

  on_error, 2  ;Return to caller if an error occurs.

  sx = size(x)

  if sx(0) ne 2 then $
    message, 'x must be two-dimensional array.'
  if sx(2) ne n_elements(y) then $ 
    message, 'x and y have incompatible dimensions.'

  nvars = sx(1) ;Number of independent variables (columns of x).

  comd = (1 - correlate(x(0,*), y)^2)

  for k = 0, nvars-2 do $
    ;Compute the Coefficient of Multiple Determination using a product
    ;of partial correlations.
    comd = comd * (1 - p_correlate(transpose(x(k+1,*)), y, x(0:k,*))^2)
 
    ;For example:
    ;The Coefficient of Multiple Determination for a model with 5 
    ;independent parameters y(x0, x1, x2, x3, x4) is computed as:
    ;comd = (1 - correlate(x(0,*), y)^2) * $
    ;       (1 - p_correlate(transpose(x(1,*)), y, x(0,*))^2)   * $
    ;       (1 - p_correlate(transpose(x(2,*)), y, x(0:1,*))^2) * $
    ;       (1 - p_correlate(transpose(x(3,*)), y, x(0:2,*))^2) * $
    ;       (1 - p_correlate(transpose(x(4,*)), y, x(0:3,*))^2)  

  return, sqrt(1 - comd)

end
