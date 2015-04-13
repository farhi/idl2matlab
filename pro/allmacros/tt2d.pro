FUNCTION tt2d,w,lambda=wl,zeroshift=z
 ; 2qtrue = 2qexp.-ZER (FULLPROF)

IF NOT KEYWORD_SET(z) THEN z=0
take_datp,datp
IF NOT KEYWORD_SET(wl) THEN wl=datp.p(33)
tt=datp.x(*,0)-z
e=datp.e
range=where(tt gt 1)
tt=tt(range)
wout=w(range,*)
IF N_ELEMENTS(E) EQ N_ELEMENTS(w) THEN e=e(range,*)
if n_elements(wl) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl =datp.p(9) 
if n_elements(wl) eq 1 THEN BEGIN
  d=wl/(2.*sin(!pi/360.*tt))
  idx=sort(d)
  mod_datp,datp,'x',d(idx)
  IF N_ELEMENTS(E) EQ N_ELEMENTS(wout) THEN BEGIN
    mod_datp, datp, 'E', e(idx,*)
  ENDIF ELSE mod_datp, datp, 'E',0
  mod_datp,datp,'x_tit','d/Angstrom'
  wout=wout(idx,*)
  give_datp,datp
  return,wout
ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
ENDELSE
END
