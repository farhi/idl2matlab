;$Id: linfit.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       LINFIT
;
; PURPOSE:
;       This function fits the paired data {X(i), Y(i)} to the linear model,
;       y = A + Bx, by minimizing the chi-square error statistic. The result
;       is a two-element vector containing the model parameters, A and B. 
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = LINFIT(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An n-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;  CHISQR:    Use this keyword to specify a named variable which returns the
;             chi-square error statistic as the sum of squared errors between
;             Y(i) and A + BX(i). If individual standard deviations are 
;             supplied, then the chi-square error statistic is computed as
;             the sum of squared errors divided by the standard deviations.
;
;    PROB:    Use this keyword to specify a named variable which returns the
;             probability that the computed fit would have a value of CHISQR 
;             or greater. If PROB is greater than 0.1, the model parameters 
;             are "believable". If PROB is less than 0.1, the accuracy of the
;             model parameters is questionable.
;
;    SDEV:    An n-element vector of type integer, float or double that 
;             specifies the individual standard deviations for {X(i), Y(i)}.
;
;  SIG_AB:    Use this keyword to specify a named variable which returns a 
;             two-element vector of probable uncertainties for the model par-
;             ameters A and B, respectively. 
;
; EXAMPLE:
;       Define two n-element vectors of paired data.
;         x = [-3.20, 4.49, -1.66, 0.64, -2.43, -0.89, -0.12, 1.41, $
;               2.95, 2.18,  3.72, 5.26]
;         y = [-7.14, -1.30, -4.26, -1.90, -6.19, -3.98, -2.87, -1.66, $
;              -0.78, -2.61,  0.31,  1.74]
;       Define a vector of standard deviations with a constant value of 0.85
;         sdev = replicate(0.85, n_elements(x))
;       Compute the model parameters, A and B.
;         result = linfit(x, y, chisqr = chisqr, prob = prob, sdev = sdev)
;       The result should be the two-element vector:
;         [-3.44596, 0.867329]
;       The keyword parameters should be returned as:
;         chisqr = 11.4998, prob = 0.319925
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1994
;                    LINFIT is based on the routines: fit.c, gammq.c, gser.c,
;                    and gcf.c described in section 15.2 of Numerical Recipes,
;                    The Art of Scientific Computing (Second Edition), and is
;                    used by permission.
;-

function linfit, x, y, chisqr = chisqr, prob = prob, sdev = sdev, $
                                                   sig_ab = sig_ab

  on_error, 2

  nx = n_elements(x)

  if nx ne n_elements(y) then $
    message, 'x and y must be vectors of equal length.'

  nsdev = n_elements(sdev)

  if nsdev eq nx then begin ;Standard deviations are supplied.
    wt = 1.0 / sdev^2
    ss = total(wt)
    sx = total(wt * x)
    sy = total(wt * y)
    t =  (x - sx/ss) / sdev
    st2 = total(t^2)
    b = total(t * y / sdev)
  endif else if nsdev eq 0 then begin
    ss = nx + 0.0
    sx = total(x)
    sy = total(y)
    t = x - sx/ss
    st2 = total(t^2)
    b = total(t * y)
  endif else $
    message, 'sdev and x must be vectors of equal length.'

  b = b / st2
  a = (sy - sx * b) / ss
  sdeva = sqrt((1.0 + sx * sx / (ss * st2)) / ss)
  sdevb = sqrt(1.0 / st2)

  if nsdev ne 0 then begin
    chisqr = total( ((y - a - b * x) / sdev)^2 )
    prob = 1 - igamma(0.5*(nx-2), 0.5*chisqr)
  endif else begin
    chisqr = total( (y - a - b * x)^2 )
    prob = 1.0
    sdevdat = sqrt(chisqr / (nx-2))
    sdeva = sdeva * sdevdat
    sdevb = sdevb * sdevdat
  endelse

  sig_ab = [sdeva, sdevb]
  return, [a, b]

end
