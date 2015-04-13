PRO y2p,w,p ;** The call is Y2P,Wn[,m]
if N_ELEMENTS(p) LT 1 THEN p=12 ; default: sample temperature for D20
take_datp,d
IF N_ELEMENTS(d.pv(0,*)) EQ N_ELEMENTS(W(0,*)) THEN BEGIN
  IF N_ELEMENTS(d.pv(*,0)) GT p THEN BEGIN
    d.y=d.pv(p,*)
    d.y_tit=STRMID(d.par_txt(p),0,STRPOS(d.par_txt(p),' ('))
  ENDIF ELSE print,'variable parameter no.',p,' does not exist!'
ENDIF ELSE print,'variable parameter array does not extend over whole y-range'
give_datp,d
end


