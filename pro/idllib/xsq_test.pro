;$Id: xsq_test.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       XSQ_TEST
;
; PURPOSE:
;       This function computes the chi-squared goodness-of-fit test
;       between observed frequencies and the expected frequencies of
;       a theoretical distribution. The result is a two-element vector
;       containing the chi-squared test statistic X2 and probability
;       of obtaining a value of X2 or greater.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = XSQ_TEST(OBFREQ, EXFREQ)
;
; INPUTS:
;       OBFREQ:  An n-element vector of type integer, float or double
;                containing observed frequencies.
;
;       EXFREQ:  An n-element vector of type integer, float or double
;                containing expected frequencies.
;
; KEYWORD PARAMETERS:
;       OBCELL:  Use this keyword to specify a named variable which returns
;                a vector of observed frequencies used to formulate the chi-
;                squared test statistic. The elements of this vector are 
;                often refered to as the "cells" of the observed frequencies.
;                The length of this vector is determined by the length of 
;                EXCELL described below.
;
;       EXCELL:  Use this keyword to specify a named variable which returns 
;                a vector of expected frequencies used to formulate the chi-
;                squared test statistic. If each of the expected frequencies 
;                contained in the n-element input vector, EXFREQ, has a 
;                magnitude of 5 or greater, then this vector is identical to 
;                EXFREQ. If EXFREQ contains elements of magnitude less than 5,
;                adjacent expected frequencies are combined. The identical
;                combinations are performed on the corresponding elements of 
;                OBFREQ.  
;
;     RESIDUAL:  Use this keyword to specify a named variable which returns
;                a vector of signed differences between corresponding cells
;                of observed frequencies and expected frequencies.
;                RESIDUAL(i) = OBCELL(i) - EXCELL(i). The length of this 
;                vector is determined by the length of EXCELL described
;                above. 
;
; EXAMPLE:
;       Define the vectors of observed and expected frequencies.
;         obfreq = [2, 1, 4, 15, 10, 5, 3]
;         exfreq = [0.5, 2.1, 5.9, 10.3, 10.7, 7.0, 3.5]
;       Test the hypothesis that the given observed frequencies are
;       an accurate approximation to the expected frequency distribution.
;         result = $
;           xsq_test(obfreq, exfreq, obcell = obcell, excell = excell)
;       The result should be the two-element vector [3.05040, 0.383920].
;       Since the vector of expected frequencies contains elements of
;       magnitude less than 5, adjacent expected frequencies are combined
;       resulting in fewer cells. The identical combinations are performed
;       on the corresponding elements of observed frequencies.
;       The cells used to formulate the chi-squared test statistic are 
;       contained in the keyword parameters, obcell and excell.
;       They should contain the values, [7, 15, 10, 8] and 
;       [8.5, 10.3, 10.7, 10.5], respectively.
;       The computed value of 0.383920 indicates that there is no reason to
;       reject the proposed hypothesis at the 0.05 significance level.
;
; PROCEDURE:
;       XSQ_TEST computes chi-squared goodness-of-fit test between observed 
;       frequencies and the expected frequencies of a theoretical distribution.
;       Expected frequencies of magnitude less than 5 are combined with 
;       adjacent elements resulting in a reduction of cells used to formulate
;       the chi-squared test statistic. If the observed frequencies differ 
;       significantly from the expected frequencies, the chi-squared test 
;       statistic will be large and the fit is poor. This situation requires 
;       the rejection of the hypothesis that the given observed frequencies 
;       are an accurate approximation to the expected frequency distribution. 
;
; REFERENCE:
;       PROBABILITY and STATISTICS for ENGINEERS and SCIENTISTS (3rd edition)
;       Ronald E. Walpole & Raymond H. Myers
;       ISBN 0-02-424170-9
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, August 1994
;-

pro freq_cell, obfreq, exfreq
  ;Combine elements of the expected frequency that are less than 5.
  ;Make corresponding changes to the vector of observed frequencies.
  ief = where(exfreq lt 5, nex)
  if nex ne 0 then begin
    while nex ne 0 do begin
      nfreq = n_elements(exfreq)
      if exfreq(ief(0)) eq exfreq(0) then begin
        ;First element less than 5.
        exfreq(1) = exfreq(0) + exfreq(1)
        exfreq = exfreq(1:*)
        obfreq(1) = obfreq(0) + obfreq(1)
        obfreq = obfreq(1:*)
      endif else if exfreq(ief(0)) eq exfreq(nfreq-1) then begin
        ;Last element less than 5.
        exfreq(nfreq-2) = exfreq(nfreq-2) + exfreq(nfreq-1)
        exfreq = exfreq(0:nfreq-2)
        obfreq(nfreq-2) = obfreq(nfreq-2) + obfreq(nfreq-1)
        obfreq = obfreq(0:nfreq-2)
      endif else begin
        ;Some middle element less than 5.
        exfreq(ief(0)) = exfreq(ief(0)) + exfreq(ief(0)+1)
        obfreq(ief(0)) = obfreq(ief(0)) + obfreq(ief(0)+1)
        if ief(0) ne nfreq-2 then begin ;Second to last element?
          exfreq = [ exfreq(0:ief(0)), exfreq(ief(0)+2:*) ]
          obfreq = [ obfreq(0:ief(0)), obfreq(ief(0)+2:*) ]
        endif else begin 
          exfreq = [ exfreq(0:ief(0)) ]
          obfreq = [ obfreq(0:ief(0)) ]
        endelse
      endelse
      ief = where(exfreq lt 5, nex)
    endwhile
  endif
end

function xsq_test, obfreq, exfreq, excell = excell, obcell = obcell, $
                                   residual = residual

  on_error, 2
  nex = n_elements(exfreq)
  if nex ne n_elements(obfreq) then $
    message, 'Observed and expected frequencies must be n-element vectors.

  ineg = where(obfreq lt 0, nneg)
  if nneg ne 0 then message, $
    'Vector of observed frequencies cannot contain negative data.'

  if total(exfreq) lt 5 then $
    message, 'Total of expected frequencies must be 5 or greater.'

  iex = where(exfreq lt 5, nex)
  if nex ne 0 then begin
  ;Combine adjacent elements of expected frequency vector that are 
  ;less than 5. Corresponding changes are made to adjacent elements 
  ;of the observed frequency vector.
    obcell = obfreq 
    excell = exfreq
    freq_cell, obcell, excell
    ;Adjust degrees of freedom
    df = n_elements(excell)-1
    ;Chi-square test statistic.
    residual = (obcell - excell)
    z = total(residual^2.0 / excell)
    ;Probability of obtaining a value of z or larger
    prob = 1 - chisqr_pdf(z, df)
    return, [z, prob]
  endif else begin
    obcell = obfreq
    excell = exfreq
    ;Degrees of freedom
    df = n_elements(excell)-1
    residual = (obcell - excell)
    z = total(residual^2.0 / excell)
    prob = 1 - chisqr_pdf(z, df)
    return, [z, prob] 
  endelse
end
