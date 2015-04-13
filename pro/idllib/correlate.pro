;$Id: correlate.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CORRELATE
;
; PURPOSE:
;       This function computes the linear Pearson correlation coefficient
;       of two n-element vectors or the Correlation Matrix of an M x N 
;       array. Alternatively, this function computes the covariance of two
;       n-element vectors or the Covariance Matrix of an M x N array.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Correlate(X [,Y])
;
; INPUTS:
;       X:    An n-element vector or an M x N array of type integer, float
;             or double.
; 
;       Y:    An n-element vector of type integer, float or double. If X is 
;             an M x N array, this parameter should not be supplied in the
;             calling sequence.
;
; KEYWORD PARAMETERS:
;       COVARIANCE:    If set to a non-zero value, the sample covariance is 
;                      computed. 
;       
; RESTRICTIONS:
;       If X is an n-element vector, then Y must also be supplied as an 
;       n-element vector; Result = Correlate(X, Y)
;
;       If X is an M x N array, then Y should not be supplied; 
;       Result = Correlate(X)
;
; EXAMPLES:
;       Define the n-element data vectors.
;         x = [65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71]
;         y = [68, 66, 68, 65, 69, 66, 68, 65, 71, 67, 68, 70]
;
;       Compute the linear Pearson correlation coefficient of x and y.
;         result = correlate(x, y)
;       The result should be 0.702652
;
;       Compute the covariance of x and y.
;         result = correlate(x, y, /covariance)
;       The result should be 3.66667
;  
;       Define an array with x and y as its columns.
;         a = transpose([[x],[y]])
;       Compute the correlation matrix.
;         result = correlate(a)
;       The result should be [[1.000000,  0.702652]
;                             [0.702652,  1.000000]]
;
;       Compute the covariance matrix.
;         result = correlate(a, /covariance)
;       The result should be [[7.69697,  3.66667]
;                             [3.66667,  3.53788]]
;
; PROCEDURE:
;       CORRELATE computes the linear Pearson correlation coefficient of
;       two n-element vectors. If X is an M x N array, M-columns by N-rows,
;       the result will be an M x M array of linear Pearson correlation 
;       coefficients with the iTH column and jTH row element corresponding
;       to the correlation of the iTH and jTH columns of the M x N array.
;       The M x M array of linear Pearson correlation coefficients (also 
;       known as the Correlation Matrix) is always symmetric and contains 
;       1s on the main diagonal. The Covariance Matrix is also symmetric.  
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Written by:  DMS, RSI, Sept 1983
;       Modified:    GGS, RSI, July 1994
;                    Added COVARIANCE keyword.
;                    Included support for matrix input.  
;                    New documentation header.
;-

function cov_mtrx, x
  nx = size(x)
  varxi = x - (x # replicate(1.0/nx(2), nx(2))) # replicate(1.0, nx(2))
  varxi = (varxi # transpose(varxi)) / (nx(2)-1)
  return, varxi
end

function crr_mtrx, x
  nx = size(x)
  ;cm = replicate(1.0, nx(1), nx(1))
  varxi = x - (x # replicate(1.0/nx(2), nx(2))) # replicate(1.0, nx(2))
  ss = varxi^2 # replicate(1.0, nx(2))
  ss = sqrt(ss # ss)
  ;cm = (varxi # transpose(varxi))/ss
  return, (varxi # transpose(varxi))/ss
end

function correlate, x, y, covariance = covariance

  on_error,2  ;Return to caller if an error occurs.

  if n_params() eq 2 then begin  ;Vector inputs.
    nx = n_elements(x)
    if nx le 1 or n_elements(y) le 1 then $
      message, 'x and y must be n-element vectors.'  
    ;Means.
    xmean = total(x) / nx
    ymean = total(y) / nx
    ;Deviations.
    xx = x - xmean
    yy = y - ymean
    if keyword_set(covariance) eq 0 then return, $  ;Correlation.
      total(xx * yy)/sqrt(total(xx^2) * total(yy^2)) $
    else return, total(xx * yy) / (nx-1)  ;Covariance.
  endif else begin          
    if keyword_set(covariance) eq 0 then return, $
      crr_mtrx(x) $  ;Correlation Matrix.
    else return, cov_mtrx(x)  ;Covariance Matrix.
  endelse
end

