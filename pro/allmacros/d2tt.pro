FUNCTION d2tt,w,lambda=wl,zeroshift=z
; tt(true) = tt(obs)-ZER (FullProf)

take_datp,datp
d=datp.x(*,0)
e=datp.e
range=where(d gt 0)
d=d(range)
wout=w(range,*)
IF N_ELEMENTS(E) EQ N_ELEMENTS(W) THEN e=e(range,*)
IF NOT KEYWORD_SET(z) THEN z=0.0
IF NOT KEYWORD_SET(wl)THEN BEGIN
  IF n_elements(datp.p) gt 9 THEN BEGIN
    IF datp.p(9) NE 0.0 THEN wl =datp.p(9)
  ENDIF
ENDIF
if KEYWORD_SET(wl) THEN BEGIN
  tt=asin(wl/2./d)*360./!pi
  idx=sort(tt)
  mod_datp,datp,'x',tt(idx)+z
  IF N_ELEMENTS(E) EQ N_ELEMENTS(wout) THEN BEGIN
    mod_datp, datp, 'E', e(idx,*)
  ENDIF ELSE mod_datp, datp, 'E',0
  mod_datp,datp,'x_tit','2theta/deg'
  wout=wout(idx,*)
  give_datp,datp
  return,wout
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
