;$Id: chisqr_pdf.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CHISQR_PDF
;
; PURPOSE: 
;       This function computes the probabilty (p) such that:
;                   Probability(X <= v) = p
;       where X is a random variable from the Chi-square distribution
;       with (df) degrees of freedom.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = chisqr_pdf(V, DF)
;
; INPUTS:
;       V:    A scalar of type integer, float or double that specifies 
;             the cutoff value.
;
;      DF:    A positive scalar of type integer, float or double that
;             specifies the degrees of freedom of the Chi-square distribution.
;
; EXAMPLES:
;       Compute the probability that a random variable X, from the Chi-square 
;       distribution with (DF = 3) degrees of freedom, is less than or equal 
;       to 6.25. The result should be 0.899939 
;         result = chisqr_pdf(6.25, 3)
;
;       Compute the probability that a random variable X, from the Chi-square
;       distribution with (DF = 3) degrees of freedom, is greater than 6.25. 
;       The result should be 0.100061
;         result = 1 - chisqr_pdf(6.25, 3)
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. New documentation header.
;-

function chisqr_pdf, x, df

  on_error, 2  ;Return to caller if error occurs.

  if x le 0 then return, 0.0 else begin
    gres = igamma_pdf(df/2.0, x/2.0)
    if gres ne -1 then return, gres else $
      message, 'Computational error detected.'
  end

end

