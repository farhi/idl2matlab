PRO par,w,var,var2
;
; Started by Thomas HANSEN
; Macro to show non-variable parameters of D20's numors
; Call: PAR,W
; Last modified 13-Jun-97
;
Take_datp,datp
IF n_elements(var) EQ 0 THEN BEGIN
  FOR i=0,39 DO PRINT,strmid(string(i),6,2),' ',strmid(datp.par_txt(i)+string(datp.p(i)),0,75)
ENDIF ELSE BEGIN
  IF n_elements(var2) EQ 0 THEN BEGIN 
    FOR i=0,30 DO PRINT,strmid(string(i),6,2),' ',strmid(datp.par_txt(i)+string(datp.pv(i,var)),0,75)
  ENDIF ELSE BEGIN
    FOR i=0,30 DO PRINT,strmid(string(i),6,2),' ',strmid(datp.par_txt(i)+string(datp.pv(i,var,var2)),0,75)
  ENDELSE
ENDELSE
END
