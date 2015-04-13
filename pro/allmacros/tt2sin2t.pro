FUNCTION tt2sin2t,w,lambda=wl,zeroshift=z
 ; 2qtrue = 2qexp.-ZER (FULLPROF)

IF NOT KEYWORD_SET(z) THEN z=0
take_datp,datp
IF NOT KEYWORD_SET(wl) THEN wl=datp.p(33)
tt=datp.x(*,0)-z
e=datp.e
range=where(tt gt 1)
tt=tt(range)
wout=w(range,*)
IF N_ELEMENTS(E) EQ N_ELEMENTS(W) THEN e=e(range,*)
if n_elements(wl) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl =datp.p(9) 
if n_elements(wl) eq 1 THEN BEGIN
  sin2t=sin(!pi/360.*tt)
  idx=sort(sin2t)
  mod_datp,datp,'x',sin2t(idx)
  IF N_ELEMENTS(E) EQ N_ELEMENTS(wout) THEN BEGIN
    mod_datp, datp, 'E', e(idx,*)
  ENDIF ELSE mod_datp, datp, 'E',0
  mod_datp,datp,'x_tit','sin(theta)**2'
  wout=wout(idx,*)
  give_datp,datp
  return,wout
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
