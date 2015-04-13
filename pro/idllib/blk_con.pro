;$Id: blk_con.pro,v 1.5 1994/11/29 18:30:12 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       BLK_CON
;
; PURPOSE:
;       This function computes a "fast convolution" of a digital signal 
;       and an impulse-response sequence.
;
; CATEGORY:
;       Digital Signal Processing
;
; CALLING SEQUENCE:
;       Result = BLK_CON(Filter, Signal, B_length = B_length)
;
; INPUTS:
;       Filter = A P-element floating-point vector containing the impulse-
;                response sequence of the digital filter.
;       Signal = An N-element floating-point vector containing the discrete 
;                signal samples.
;                
; KEYWORD PARAMETERS:
;       B_length = (Block Length) An integer specifying the length of
;                  the subdivided signal segments. If this paramter is
;                  not specified, a near-optimal value is chosen by the 
;                  algorithm based upon the length of the impulse-response 
;                  sequence, P. If P is a value less than 11 or greater 
;                  than 377, then B_length must be specified.
;
; RESTRICTIONS:
;       1) The block length must be greater than the filter length.
;          B_length > P
;       2) The block length must be less than the number of signal samples.
;          B_length < N_elements(Signal)
;
; EXAMPLE:
;       Create an impulse-response sequence of length P = 32. 
;         filter = replicate(1.0, 32) ;Set all points to 1.0
;         filter(2*indgen(16)) = 0.5  ;Set even points to 0.5
;
;       Create a sampled signal with random noise.
;         signal = sin((findgen(1000)/35.0)^2.5)
;         noise  = (randomu(SEED,1000)-0.5)/2
;         signal = signal + noise
;
;       Convolve the filter and signal using block convolution.
;         result = BLK_CON(filter, signal)
;
; PROCEDURE:
;       Implementation of the "overlap-save" method in the frequency domain.
;       The discrete signal samples are divided into overlapping segments of
;       a length specified by the parameter B_length. B_length may be supplied
;       by the user as an optional keyword parameter or determined by the
;       algorithm to a near-optimal value. Each input segment consists of P-1
;       samples from the previous input segment and (B_length-P+1) new signal
;       samples, where P is the length of the filter. Each of these segments
;       is processed in the frequency domain and then 'reassembled' in the
;       time domain. The first and last input segments are handled differently.
;       The result is an N-element floating-point vector containing the 
;       filtered signal.
;
; REFERENCE:
;       Oppenheim, A.V. and Schafer, R.W.
;       DIGITAL SIGNAL PROCESSING
;       Prentice-Hall, 1975
;
; MODIFICATION HISTORY:
;           Written by:  GGS, RSI, May 1993
;           Modified:    GGS, RSI, June 1994
;                        Added long indexing into vectors. Minor changes in 
;                        the use of intermediate variables reduces memory
;                        allocation in certain instances. Made slight changes
;                        to the documentation header.
;-

function BLK_CON, Filter, Signal, B_length = B_length

  ;Return to caller if an error occurs.
  on_error, 2 

  ;Block length is based upon filter length.
  p = n_elements(filter)
  if n_elements(b_length) eq 0 then begin
    if (p lt 11) then stop, $
      'Block length must be specified as a keyword parameter.' $
    else if (p LE 17)  then blen = 64   $
    else if (p LE 29)  then blen = 128  $
    else if (p LE 52)  then blen = 256  $
    else if (p LE 94)  then blen = 512  $
    else if (p LE 171) then blen = 1024 $
    else if (p LE 377) then blen = 2048 $
    else stop, 'Block length must be specified as a keyword parameter.'
  endif else blen = long(b_length) ;User specified block length.

  ;RESTRICTION:1
  if p ge blen then stop, $
    'Block length must be greater than the filter length.'

  ;Number of discrete signal samples.
  ns = n_elements(signal) 

  ;RESTRICTION:2
  if blen ge ns then stop, $
    'Block length must be less than the number of signal samples.'

  ;Number of signal segments of length, blen-p+1
  k = ns / (blen - P + 1L)

  ;Length of last signal segment.
  rem = ns mod (blen - p + 1L)

  ;Represent the filter in frequency domain.
  z_filter = fft([filter, fltarr(blen-p)], -1)

  ;The first segment is handled differently than the rest.
  ;Forward zero-pad signal segment up to a length of blen.
  lower = 0L
  upper = blen - p
  input = [fltarr(p-1), signal(lower:upper)]
  
  ;Allocate storage for the result.
  result = fltarr(ns, /nozero)

  ;Complex point-wise multiplication and inverse transform.
  output = float(fft((z_filter * fft(input, -1)), 1) *  blen)
  ;The 'blen' term is a scale factor unique to IDL's FFT algorithm.
  ;Discard the first p-1 points to obtain the first output segment.
  result(0) = output(p-1:blen-1)

  ;Next place to store.
  ip = blen - p + 1L

  ;Begin to process the subdivided signal segments.
  loop  = 1L
  lower = upper + 1L
  upper = lower + blen - p  ;upper = lower+(blen-p+1)-1
  while loop le k-1L do begin
    input  = [input(blen-p+1:blen-1), signal(lower:upper)]
    output = float(fft((z_filter * fft(input, -1) ),1) * blen)
    result(ip) = output(p-1:blen-1) ;Next output segment.
    ip = ip + blen - p + 1L
    lower = upper + 1L
    upper = lower + blen - p
    loop  = loop + 1L
  endwhile

  ;Last signal segment is of length, rem. If rem is not zero 
  ;there is a signal segment that is handled differently.
  if rem ne 0L then begin
    output = float(fft((fft([filter, fltarr(p+rem-2)] ,-1) $
             * fft([input(blen-p+1:blen-1), signal(lower:lower+rem-1), $
               fltarr(p-1)], -1)), 1) * (2*p+rem-2))
    ;(2*p+rem-2) is an FFT scale factor.
    result(ip) = output(p-1:p+rem-2)
  endif
  return, result
  ;Clean up intermediate variables.
  z_filter = 0
  input = 0
  output = 0
  result = 0
end
