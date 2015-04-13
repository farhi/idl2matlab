FUNCTION variance,n1,n2,s=s,w=w

take_datp,datp
IF n2 LE n1 THEN RETURN,0
s = rdsum (n1, n2)  ;/ (n2 - n1 + 1)
calib,s
;PRINT, (n2 - n1 + 1)
v = s * 0.

FOR i = n1, n2 DO BEGIN
  w = rdrun (i)
  calib,w
  v = v + (w - s)^2
ENDFOR
mod_datp,datp,'x',INDGEN (N_ELEMENTS(s))
give_datp,datp
RETURN,v
END

