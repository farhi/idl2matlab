FUNCTION q2d,w,wl,lambda=wl
 ; 2qtrue = 2qexp.-ZER

take_datp,datp
q=datp.x
e=datp.e
range=where(q gt 0.1)
q=q(range)
ww=ww(range)
e=e(range)
if n_elements(wl) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl =datp.p(9) 
if n_elements(wl) eq 1 THEN BEGIN
  d=2.*!pi/q
  idx=sort(d)
  mod_datp,datp,'x',d(idx)
  mod_datp, datp, 'E', e(idx)
  mod_datp,datp,'x_tit','d/Ang'
  ww=ww(idx)
  give_datp,datp
  return,ww
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
