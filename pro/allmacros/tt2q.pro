FUNCTION tt2q,w,wavelength=wl,ZEROSHIFT=z,printer=printer
ww=w
take_datp,datp
IF NOT KEYWORD_SET(z) THEN z=0
IF NOT KEYWORD_SET(wl) THEN wl=datp.p(33)
x=datp.x-z
e=datp.e
range=where(x gt 0)
x=x(range)
ww=ww(range,*)
e=e(range,*)
IF max(x) GE 170 OR datp.x_tit NE '2*Theta' THEN BEGIN
  IF max(x) GE 170 THEN PRINT,'Attention, your 2Theta is too large!' ELSE PRINT,'Attention, your x_tit is not 2*Theta!'
ENDIF ELSE BEGIN
 if n_elements(wl) eq 0 then if n_elements(datp.p) gt 9 then if datp.p(9) NE 0.0 THEN wl =datp.p(9) 
 if n_elements(wl) eq 1 THEN BEGIN
  q=2.*!pi*(2.*sin(!pi/360.*x))/wl
  IF KEYWORD_SET(printer) THEN FOR i=0,N_ELEMENTS(range)-1,printer DO PRINT,x(i),q(i)
  idx=sort(q)
  mod_datp,datp,'x',q(idx)
  IF N_ELEMENTS(datp.e) EQ N_ELEMENTS(w) THEN mod_datp, datp, 'E', e(idx,*)
  mod_datp,datp,'x_tit','Q/Ang^-1'
  ww=ww(idx,*)
  give_datp,datp
  return,ww
 ENDIF ELSE BEGIN
  print,'Attention, no wavelength given for original workspace (P(9))!'
  return,0
 ENDELSE
ENDELSE
END
