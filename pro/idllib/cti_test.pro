;$Id: cti_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CTI_TEST
;
; PURPOSE:
;       This function constructs a "contingency table" from an array of
;       observed frequencies and tests the hypothesis that the rows and
;       columns are independent using an extension of the chi-squared 
;       goodness-of-fit test. The result is a two-element vector contain-
;       ing the chi-squared test statistic X2 and probability of obtaining 
;       a value of X2 or greater.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = CTI_TEST(OBFREQ)
;
; INPUTS:
;       OBFREQ:  An array of c-columns and r-rows of type integer, float 
;                or double containing observed frequencies.
;
; KEYWORD PARAMETERS:
;        COEFF:  Use this keyword to specify a named variable which returns
;                the Coefficient of Contingency. A non-negative scalar,
;                in the interval [0.0, 1.0], which measures the degree
;                of dependence within a contingency table. The larger the
;                value of COEFF, the greater the degree of dependence.
;
;    CORRECTED:  If set to a nonzero value, the "Yate's Correction for
;                Continuity" is used to compute the chi-squared test 
;                statistic, X2. The Yate's correction always decreases the
;                magnitude of the chi-squared test statistic, X2. In general,
;                this keyword should be set for small sample sizes.
;
;         CRAMV: Use this keyword to specify a named variable which returns
;                Cramer's V. A non-negative scalar, in the interval [0.0, 1.0],
;                which measures the degree of dependence within a contingency
;                table.
;
;           DF:  Use this keyword to specify a named variable which returns
;                the degrees of freedom used to compute the probability of 
;                obtaining the value of the chi-squared test statistic or
;                greater. DF = (r - 1) * (c - 1) where r and c are the
;                number of rows and columns of the contingency table, 
;                respectively.
;
;       EXFREQ:  Use this keyword to specify a named variable which returns
;                an array of c-columns and r-rows containing expected  
;                frequencies. The elements of this array are often refered
;                to as the "cells" of the expected frequencies. The expected
;                frequency of each cell is computed as the product of row
;                and column marginal frequencies divided by the overall 
;                total of observed frequencies.
; 
;     RESIDUAL:  Use this keyword to specify a named variable which returns
;                an array of c-columns and r-rows containing signed differences
;                between corresponding cells of observed frequencies and 
;                expected frequencies.
;
; EXAMPLE:
;       Define the 5-column and 4-row array of observed frequencies.
;         obfreq = [[748, 821, 786, 720, 672], $
;                   [ 74,  60,  51,  66,  50], $
;                   [ 31,  25,  22,  16,  15], $
;                   [  9,  10,   6,   5,   7]]
;       Test the hypothesis that the rows and columns of "obfreq" contain
;       independent data at the 0.05 significance level.
;         result = cti_test(obfreq, coeff = coeff)
;       The result should be the two-element vector [14.3953, 0.276181].
;       The computed value of 0.276181 indicates that there is no reason to
;       reject the proposed hypothesis at the 0.05 significance level.
;       The Coefficient of Contingency returned in the parameter "coeff" 
;       (coeff = 0.0584860) also indicates the lack of dependence between
;       the rows and columns of the observed frequencies. Setting the 
;       CORRECTED keyword returns the two-element vector [12.0032, 0.445420]
;       and (coeff = 0.0534213) resulting in the same conclusion of 
;       independence.
;
; PROCEDURE:
;       CTI_TEST constructs a "contingency table" from a 2-dimensional
;       input array of observed frequencies and applies the principles of
;       the chi-squared goodness-of-fit test to determine the independence
;       of the rows and columns. For small sample sizes, a correction for
;       absolute differences between observed and expected frequencies may
;       be applied by setting the CORRECTED keyword.
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, August 1994
;-

function cti_test, obfreq, corrected = corrected, coeff = coeff, $
                           cramv = cramv, df = df, $
                           exfreq = exfreq, residual = residual

  on_error, 2

  ;Size attributes for array of observed values.
  sobfreq = size(obfreq)

  if sobfreq(0) ne 2 then message, $
    'Observed frequencies must be two-dimensional array.'

  ineg = where(obfreq lt 0, nneg)
  if nneg ne 0 then message, $
    'Array of observed frequencies cannot contain negative data.'

  col = sobfreq(1) ;Column dimension.
  row = sobfreq(2) ;Row dimension.

  obtotal = total(obfreq) ;Overall total.

  ;Marginal frequencies.
  coltotal = total(obfreq, 2)
  rowtotal = total(obfreq, 1)

  ;Expected frequencies.
  exfreq = coltotal # rowtotal / obtotal  

  ;Degrees of freedom
  df = (row - 1) * (col - 1)

  if keyword_set(corrected) ne 0 then begin
    ;Apply Yate's Correction.
    residual = abs(obfreq - exfreq) 
    z = total((residual - 0.5)^2.0 / exfreq)
    prob = 1 - chisqr_pdf(z, df)
  endif else begin
    residual = (obfreq - exfreq)
    z = total(residual^2.0 / exfreq)
    prob = 1 - chisqr_pdf(z, df)
  endelse

  ;Coefficient of contingency.
  coeff = sqrt(z / (z + obtotal)) 

  ;Cramer's V.
  cramv = sqrt(z / (obtotal * min([row - 1, col - 1])))

  return, [z, prob]

end
