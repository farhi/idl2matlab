;$Id: t_pdf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       T_PDF
;
; PURPOSE:
;       This function computes the probabilty (p) such that:
;                   Probability(X <= v) = p
;       where X is a random variable from the Student's t distribution
;       with (df) degrees of freedom.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = T_pdf(V, DF)
;
; INPUTS:
;       V:    A scalar of type integer, float or double that specifies
;             the cutoff value.
;
;      DF:    A positive scalar of type integer, float or double that
;             specifies the degrees of freedom of the Student's t 
;             distribution.
;
; EXAMPLE:
;       Compute the probability that a random variable X, from the 
;       Student's t distribution with (df = 15) degrees of freedom,
;       is less than or equal to 0.691. The result should be 0.749940
;         result = t_pdf(0.691, 15)
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. New documentation header.
;-

function t_pdf , v, df

  on_error, 2  ;Return to caller if error occurs.

  if df le 0 then message, $
    'Degrees of freedom must be positive.'

  return, 1.0 - 0.5 * ibeta_pdf(df/(df + v^2), df/2.0, 0.5) 
end 

