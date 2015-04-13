FUNCTION d2q,w
 ; 2qtrue = 2qexp.-ZER

take_datp,datp
d=datp.x(*,0)
e=datp.e
range=where(d gt 0)
d=d(range)
wout=w(range,*)
IF N_ELEMENTS(E) EQ N_ELEMENTS(W) THEN e=e(range,*)
if n_elements(wl) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl =datp.p(9) 
if n_elements(wl) eq 1 THEN BEGIN
  q=2*!pi/d
  idx=sort(q)
  mod_datp,datp,'x',q(idx)
  IF N_ELEMENTS(E) EQ N_ELEMENTS(wout) THEN BEGIN
    mod_datp, datp, 'E', e(idx,*)
  ENDIF ELSE mod_datp, datp, 'E',0
  mod_datp,datp,'x_tit','Q/Ang^-1'
  wout=wout(idx,*)
  give_datp,datp
  return,wout
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
