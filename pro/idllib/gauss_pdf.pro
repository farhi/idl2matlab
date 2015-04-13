;$Id: gauss_pdf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       GAUSS_PDF
;
; PURPOSE:
;       This function computes the probabilty (p) such that:
;                   Probability(X <= v) = p
;       where X is a random variable from the standard Gaussian (Normal) 
;       distribution with a mean of 0.0 and a variance of 1.0
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Gauss_Pdf(V)
;
; INPUTS:
;       V:    A scalar of type integer, float or double that specifies
;             the cutoff value.
;
; EXAMPLES:
;       Compute the probability that a random variable X, from the
;       standard Gaussian (Normal) distribution, is less than or equal 
;       to 2.44. The result should be 0.992656
;         result = gauss_pdf(2.44)
;
;       Compute the probability that a random variable X, from the 
;       standard Gaussian (Normal) distribution, is less than or equal 
;       to 10.0 and greater than or equal to 2.0. The result should be
;       0.0227501 [i.e. Probability(2.0 <= X <= 10.0)]
;         result = gauss_pdf(10.0) - gauss_pdf(2.0)
;
;       Compute the probability that a random variable X, from the 
;       Gaussian (Normal) distribution with a mean of 0.8 and a variance 
;       of 4.0, is less than or equal to 2.44. The result should be 
;       0.793892
;         result = gauss_pdf( (2.44 - 0.80)/sqrt(4.0) )
;
; PROCEDURE:
;       GAUSS_PDF calls GAUSSINT() to evaluate the Gaussian integral.
;       This function was included to provide consistency with the 
;       other probability functions: CHISQR_PDF(), F_PDF(), and T_PDF().
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Written by:   GGS, RSI, July 1994
;-

function gauss_pdf, v

  on_error, 2  ;Return to caller if error occurs.
  return, gaussint(v)

end
