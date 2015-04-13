FUNCTION q2tt,ww,wl,zeroshift=z
 ; 2qtrue = 2qexp.-ZER

IF NOT KEYWORD_SET(z) THEN z=0.
take_datp,datp
q=datp.x
e=datp.e
range=where(q gt 0)
q=q(range)
ww=ww(range)
e=e(range)
if n_elements(wl) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl =datp.p(9) 
if n_elements(wl) eq 1 THEN BEGIN
  x=asin(wl/2.*q/2./!pi)*360./!pi
  idx=sort(x)
  mod_datp,datp,'x',x(idx)+z
  mod_datp, datp, 'E', e(idx)
  mod_datp,datp,'x_tit','2theta/deg'
  ww=ww(idx)
  give_datp,datp
  return,ww
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
