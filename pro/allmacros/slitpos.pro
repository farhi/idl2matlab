FUNCTION slitpos, w, first, last, step, delta, datp=datpp

IF NOT KEYWORD_SET(datpp) THEN TAKE_DATP, datp ELSE datp=datpp
x = datp.x
resw=FLTARR(1+FLOOR((last-first)/step))
resx=FLTARR(1+FLOOR((last-first)/step))
;HELP,resw,resx
FOR i = first, last, step DO BEGIN
  trapw=w(WHERE(x LE i+delta AND x ge i-delta))
  trapx=x(WHERE(x LE i+delta AND x ge i-delta))
  fit=GAUSSFIT(trapx,trapw,a)
  PRINT,ROUND((i-first)/step),i,a(1),a(0)*a(2)
  resx(ROUND((i-first)/step))=a(1)
  resw(ROUND((i-first)/step))=a(0)*a(2)
ENDFOR
resw=resw/TOTAL(resw)*N_ELEMENTS(resw)
MOD_DATP,datp,'x',resx
IF NOT KEYWORD_SET(datpp) THEN GIVE_DATP,datp ELSE datpp=datp
RETURN,resw

END
