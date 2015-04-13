;$Id: r_correlate.pro,v 1.2 1994/11/29 20:51:52 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       R_CORRELATE
;
; PURPOSE:
;       This function computes Spearman's (rho) or Kendalls's (tau) rank 
;       correlation of two n-element vectors. The result is a two-element 
;       vector containing the rank correlation coefficient and the two-sided 
;       significance level of its deviation from zero.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = R_correlate(X, Y)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An n-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
; KENDALL:    If set to a nonzero value, Kendalls's (tau) rank correlation
;             is computed. By default, Spearman's (rho) rank correlation is
;             computed.
;
;       D:    Use this keyword to specify a named variable which returns the 
;             sum-squared difference of ranks. If the KENDALL keyword is set
;             to a nonzero value, this parameter is returned as zero.
;
;      ZD:    Use this keyword to specify a named variable which returns the 
;             number of standard deviations by which D deviates from its null-
;             hypothesis expected value. If the KENDALL keyword is set to a
;             nonzero value, this parameter is returned as zero.
;
;   PROBD:    Use this keyword to specify a named variable which returns the 
;             two-sided significance level of ZD. If the KENDALL keyword is 
;             set to a nonzero value, this parameter is returned as zero.
;
; EXAMPLE
;       Define two n-element vectors of tabulated data.
;         x = [257, 208, 296, 324, 240, 246, 267, 311, 324, 323, 263, 305, $
;              270, 260, 251, 275, 288, 242, 304, 267]
;         y = [201, 56, 185, 221, 165, 161, 182, 239, 278, 243, 197, 271, $
;              214, 216, 175, 192, 208, 150, 281, 196]
;
;       Compute Spearman's (rho) rank correlation of x and y.
;         result = r_correlate(x, y, d = d, zd = zd, probd = probd)
;       The result should be the two-element vector:
;         [0.835967, 4.42899e-06]
;       The keyword parameters should be returned as:
;         d = 218.000, zd = -3.64390, probd = 0.000268542      
;
;       Compute Kendalls's (tau) rank correlation of x and y.
;         result = r_correlate(x, y, /kendall)
;       The result should be the two-element vector:
;         [0.624347  0.000118732]
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, Aug 1994
;                    R_CORRELATE is based on the routines spear.c and kendl1.c
;                    described in section 14.6 of Numerical Recipes, The Art 
;                    of Scientific Computing (Second Edition), and is used by 
;                    permission.
;-

function betacf, a, b, x
  on_error, 2
  eps   = 3.0e-7
  fpmin = 1.0e-30
  maxit = 100
  qab = a + b
  qap = a + 1.0
  qam = a - 1.0
    c = 1.0
    d = 1.0 - qab * x / qap
  if(abs(d) lt fpmin) then d = fpmin
  d = 1.0 / d
  h = d
  for m = 1, maxit do begin
    m2 = 2 * m
    aa = m * (b - m) * x / ((qam + m2) * (a + m2))
     d = 1.0 + aa*d
     if(abs(d) lt fpmin) then d = fpmin
     c = 1.0 + aa / c
     if(abs(c) lt fpmin) then c = fpmin
     d = 1.0 / d
     h = h * d * c
     aa = -(a + m) *(qab + m) * x/((a + m2) * (qap + m2))
     d = 1.0 + aa * d
     if(abs(d) lt fpmin) then d = fpmin
     c = 1.0 + aa / c
     if(abs(c) lt fpmin) then c = fpmin
     d = 1.0 / d
     del = d * c
     h = h * del
     if(abs(del - 1.0) lt eps) then return, h
  endfor
  message, 'Failed to converge within given parameters.'
end

function gammln, xx
  coff = [76.18009172947146d0,   -86.50532032941677d0,  $
          24.01409824083091d0,    -1.231739572450155d0, $
           0.1208650973866179d-2, -0.5395239384953d-5]
  stp = 2.5066282746310005d0
  x = xx
  y = x
  tmp = x + 5.5d0
  tmp = (x + 0.5d0) * alog(tmp) - tmp
  ser = 1.000000000190015d0
  for j = 0, n_elements(coff)-1 do begin
    y = y + 1.d0
    ser = ser + coff(j) / y
  endfor
  return, tmp + alog(stp * ser / x)
end

function ibeta, a, b, x
  on_error, 2
  if (x lt 0 or x gt 1) then message, $
    'x must be in the interval [0, 1].'
  if (x eq 0  or x eq 1) then bt = 0.0 $
  else $
    bt = exp(gammln(a + b) - gammln(a) - gammln(b) + $
             a * alog(x) + b * alog(1.0 - x))
  if(x lt (a + 1.0)/(a + b + 2.0)) then return, $
    bt * betacf(a, b, x) / a $
  else return, 1.0 - bt * betacf(b, a, 1.0-x) / b
end

pro crank, w, s
  n = n_elements(w)
  w = [0.0, w]  ;operate on elements w(1), ... , w(n) of the
                ;n+1 element float array, w.
  s = 0.0
  j = 1
  while(j lt n) do begin
    if(w(j+1) ne w(j)) then begin
      w(j) = j
      j = j+1
    endif else begin
      for jt = j+1, n do begin
        if (w(jt) ne w(j)) then goto, case2
      endfor
      jt = n + 1
      case2:
      rank = 0.5 * (j + jt - 1)
      for ji = j, jt-1 do $
        w(ji) = rank
      t = jt - j
      s = s + t^3 - t
      j = jt
    endelse
  endwhile
  if(j eq n) then w(n) = n
  w = w(1:*)
end

function erfcc, x
  z = abs(x)
  t = 1.0 / (1.0 + 0.5 * z)
  erfc = t*exp(-z*z-1.26551223+t*(1.00002368+t*(.37409196+t* $
         (.09678418+t*(-.18628806+t*(.27886807+t*(-1.13520398+t* $
         (1.48851587+t*(-.82215223+t*.17087277)))))))))
  if x lt 0 then return, 2.0 - erfc $
  else return, erfc
end

function r_correlate, x, y, kendall = kendall, d = d, zd = zd, probd = probd

  on_error, 2
  nx = n_elements(x)
  if nx le 1 or n_elements(y) le 1 then $
    message, 'x and y must be n-element vectors.'
  if nx ne n_elements(y) then $
    message, 'x and y must be vectors of equal length.'

  if keyword_set(kendall) eq 0 then begin ;Spearman's (rho)
    type = size(x)
    wrkx = x
    wrky = y
    ixy  = sort(wrkx) ;Indexes of "wrkx" in ascending order. 
    wrkx = wrkx(ixy)  ;Rearrangement of "wrkx" according to ixy.
    wrky = wrky(ixy)  ;Rearrangement of "wrky" according to ixy.
    crank, wrkx, sf   ;Replace elements of "wrkx" by their rank.
    ixy  = sort(wrky) ;Indexes of "wrky" in ascending order.
    wrkx = wrkx(ixy)  ;Rearrangement of "wrkx" according to ixy.
    wrky = wrky(ixy)  ;Rearrangement of "wrky" according to ixy.
    crank, wrky, sg   ;Replace elements of "wrky" by their rank.
    d = total((wrkx-wrky)^2)
    ;Free intermediate variables.
    wrkx = 0
    wrky = 0
    ixy = 0
    en = nx
    en3n = en^3 - en
    aved = en3n/6.0 - (sf + sg)/12.0
    fac = (1.0 - sf/en3n) * (1.0 - sg/en3n)
    vard = ((en - 1.0) * en^2 * (en + 1.0)^2 / 36.0) * fac
    zd = (d - aved) / sqrt(vard)
    probd = erfcc(abs(zd)/1.4142136)
    rs = (1.0 - (6.0/en3n) * (d+(sf+sg)/12.0))/sqrt(fac)
    fac = (1.0 + rs) * (1.0 - rs)
    if (fac gt 0.0) then begin
      t = rs * sqrt((en - 2.0)/fac)
      df = en - 2
      probrs = ibeta(0.5*df, 0.5, df/(df+t^2))
    endif else probrs = 0.0
    ;Return a vector of rank-correlation parameters.
    if type(2) eq 5 then begin 
      d = d+0d & zd = zd+0d & probd = probd+0d
      return, [rs, probrs]
    endif else begin
      d = float(d) & zd = float(zd) & probd = float(probd)
      return, float([rs, probrs])
    endelse
  endif else begin ;Kendall's (tau)
    nnx = 0.0
    nny = 0.0
    is  = 0.0
    ;There seems to be no efficient method of avoiding this nested 
    ;FOR loop structure. An alternate method is possible, but requires 
    ;about (1/2 * nx^2) storage and one FOR loop.
    for j = 0, nx-2 do begin
      for k = j+1, nx-1 do begin
        dx = x(j) - x(k)
        dy = y(j) - y(k)
        aa = dx * dy
        if aa ne 0 then begin
          nnx = nnx + 1
          nny = nny + 1
          if aa gt 0 then is = is + 1 $
            else is = is - 1
        endif else begin
          if dx ne 0 then nnx = nnx + 1 $
          else if dy ne 0 then nny = nny + 1
        endelse
      endfor
    endfor
    d = 0 & zd = 0 & probd = 0 ;Keyword parameters of Spearman's (rho).
    tau = is / sqrt(nnx * nny)
    var = (4.0 * nx + 10.0) / (9.0 * nx * (nx-1.0))
      z = tau / sqrt(var)
    prob = erfcc(abs(z) / 1.4142136)
    ;prob = 1 - errorf(abs(z) / 1.4142136) ;IDL's error function
    return, [tau, prob]
  endelse

end





