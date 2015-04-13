FUNCTION unic,w,STEP=step
;
; Started by Thomas HANSEN
; Macro to transform D20's 2Theta Scans into one one-dimensional Workspace
; that can be treated by Rietveld (after EXRIET,W) 
; PRETREAT,W, CALIB,W and NORM,W can be run before,
; EXRIET,W can be run afterwards
; Calling by W_new=UNIC(W_old[,STEP=0.1]) with W_old 2-dim., W_new 1-dim.
; Last modified 21-Nov-96 by Thomas HANSEN
;
take_datp,datp
x=datp.x
e=(datp.e)^2
tmpw=reform(w,n_elements(w))
x=reform(x,n_elements(w))
e=reform(e,n_elements(w))
IF NOT KEYWORD_SET(step) THEN BEGIN
  step=0.1
  IF datp.p(10) EQ 3.0 THEN step=(0.1+datp.p(25)) mod 0.1
ENDIF
datp.p(25)=step
x=round(x/step)*step
unix = UNIQ (x,sort(x))
neww = fltarr(n_elements(unix))
newx = fltarr(n_elements(unix))
newe = fltarr(n_elements(unix))
FOR i=0,n_elements(unix)-1 DO BEGIN
  index=WHERE(x EQ x(unix(i)), count)
  count=float(count)
  neww(i)=TOTAL(tmpw(index)/count)
  newx(i)=x(unix(i))
  newe(i)=SQRT(TOTAL(e(index)))/count
ENDFOR

mod_datp,datp,'e',newe
mod_datp,datp,'x',newx
give_datp,datp
RETURN,neww
END
