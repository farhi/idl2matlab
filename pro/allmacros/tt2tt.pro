FUNCTION tt2tt,w,wl2,l2=l2,z1=z1,l1=wl1,z2=z2
 ; 2qtrue = 2qexp.-ZER (FULLPROF)

IF NOT KEYWORD_SET(z1) THEN z1=0
IF NOT KEYWORD_SET(z2) THEN z2=0
take_datp,datp
IF NOT KEYWORD_SET(wl1) THEN wl1=datp.p(33)
IF KEYWORD_SET(l2) THEN wl2=l2
tt1=datp.x(*,0)-z1
e=datp.e
range=where(tt1 gt 1)
tt1=tt1(range)
wout=w(range,*)
IF N_ELEMENTS(E) EQ N_ELEMENTS(w) THEN e=e(range,*)
if n_elements(wl1) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl1 =datp.p(9) 
if n_elements(wl1) eq 1 THEN BEGIN
  d=wl1/(2.*sin(!pi/360.*tt1))
  range=WHERE(d gt 0 AND  wl2/2./d LT 1)
  d=d(range)
  IF N_ELEMENTS(E) EQ N_ELEMENTS(wout) THEN BEGIN
    mod_datp, datp, 'E', e(range,*)
  ENDIF ELSE mod_datp, datp, 'E',0
  wout=wout(range,*)
  tt2=asin(wl2/2./d)*360./!pi
  datp.p(9)=wl2
  mod_datp,datp,'x',tt2+z2
  give_datp,datp
  return,wout
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
