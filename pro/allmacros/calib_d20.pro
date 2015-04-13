pro calib_d20,w,filename,eff=cal_d20,ang=ang_d20,NOEFF=noeff,NOANG=noang
;
; Macro (started 28/10/96 by TH.HANSEN) in order to effectuate an efficiency calibration and/or 
; angle calibration
;
; Last modification 17-Feb-97 by Th.Hansen
;
take_datp,datp
x=datp.x
e=datp.e
nd = n_elements(w(*,0,0))

IF ( long(datp.p(38)) MOD 4) MOD 2 ne 0  THEN noeff=1
IF ( long(datp.p(38)) MOD 4)   / 2 ne 0  THEN noang=1
IF nd eq n_elements(w(*,0,0)) THEN cell=indgen(nd) ELSE cell=ROUND((datp.x(*,0)-datp.x(0,0))*10.0)
IF n_elements(ang_d20) eq 0 THEN ang_d20=findgen(nd)
IF n_elements(cal_d20) eq 0 THEN BEGIN
  line=' '
  ang_d20=findgen(nd)
  on_ioerror,misd20
  in=-1
  ok=0
  IF NOT KEYWORD_SET(filename) THEN filename=P_LAMBDA()+'/CALIBRATION/d20.cal' $
                               ELSE filename=P_LAMBDA()+'/CALIBRATION/'+filename
  PRINT,'Calibration by ',filename
  OPENR,in,filename,/get_lun
  cal_d20=fltarr(nd)
  READF,in,line
  print,line
  nd_cal=1600
  ang_d20=findgen(nd_cal)
  cal_d20=fltarr(nd_cal)
  READF,in,ang_d20,cal_d20
  ang_d20=ang_d20-ang_d20(0)
  ok=1
  misd20: IF in gt 0 THEN FREE_LUN,in
  IF ok eq 0 THEN cal_d20=0 ; no calibration
  IF ok eq 0 THEN PRINT,'no calibration effectuated!' ; ELSE plot,cal_d20
ENDIF
IF n_elements(cal_d20) ge n_elements(w(*,0)) THEN BEGIN 
  IF not keyword_set(noeff) THEN BEGIN
    datp.p(38) = datp.p(38)+1.
    w = float (w)
    FOR j=0,n_elements(w(*,0))-1 DO IF cal_d20(cell(j)) eq 0.0 THEN print,'Cell no.',cell(j),' okay? Probably parasite affected! Use PRETREAT,w,cell!'
  ENDIF
  IF not keyword_set(noang) THEN datp.p(38) = datp.p(38)+2.
  FOR i=0,n_elements(w(0,*))-1 DO FOR j=0,n_elements(w(*,0))-1 DO BEGIN
    IF not keyword_set(noeff) THEN IF cal_d20(cell(j)) ne 0.0 THEN BEGIN
      w(j,i)=w(j,i)*cal_d20(cell(j))
      e(j,i)=e(j,i)*cal_d20(cell(j))
    ENDIF
    IF not keyword_set(noang) AND i lt n_elements(x(0,*)) THEN x(j,i)=x(0,i)+ang_d20(cell(j))
  ENDFOR
ENDIF ELSE BEGIN
  noang=1
  noeff=1
ENDELSE
datp.x = x
datp.e = e
give_datp,datp
END
