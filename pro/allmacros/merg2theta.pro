Function merg2theta,wn,nosum=nosum

;+
; Merge twotheta scan of n steps
; W1 = merg2theta (2)
; Function
; Parameters
;   Number of workspace to be merged
; Output
;   Workspace containing merged diagram
;-

COMMON C_LAMP_INFO 

IF N_ELEMENTS(wn) GT 1 THEN BEGIN
  n=two
ENDIF
IF N_ELEMENTS(wn) EQ 1 THEN n=wn
IF N_ELEMENTS(n) EQ 1 THEN BEGIN
  take_w,w,w=n
  take_datp,d,w=n
  e=d.e
  n=d.n
  x=d.x
  IF N_ELEMENTS(w(0,*)) GT 2 THEN BEGIN
    ;############# Normalisation #############
    norm=TOTAL(d.n(0,0,*))/N_ELEMENTS(d.n(0,0,*))
    FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
      w(*,i)  =  w(*,i)*norm/d.n(0,0,i)
      e(*,i)  =  e(*,i)*norm/d.n(0,0,i)
      n(*,*,i)=n(*,*,i)*norm/d.n(0,0,i)
    ENDFOR
    ;############# 2Theta ####################
    delta=(10.*d.x(0,*)-FLOOR(10.*d.x(0,*)))/10.
    ;print,REFORM(delta)
    ;print,MEDIAN(delta),TOTAL(delta)/N_ELEMENTS(d.x(0,*))
    test=ABS(median(delta)-total(delta)/N_ELEMENTS(d.x(0,*)))
    delta=(10.*(d.x(0,*)-.05)-FLOOR(10.*(d.x(0,*)-.05)))/10.
    ;print,REFORM(delta+.05)
    ;print,MEDIAN(delta+.05),total(delta+.05)/N_ELEMENTS(d.x(0,*))
    IF test LT ABS(median(delta+.05)-total(delta+.05)/N_ELEMENTS(d.x(0,*))) THEN BEGIN
      delta=(10.*d.x(0,*)-FLOOR(10.*d.x(0,*)))/10.
      delta=MEDIAN(delta)
    ENDIF ELSE BEGIN
      delta=MEDIAN(delta+.05)
      delta=((10.*delta)-FLOOR(10.*delta))/10.
    ENDELSE
    PRINT,delta
    xrange=[FLOOR(min(d.x))+delta,CEIL(max(d.x))+delta]
    x=0
    e=0
    y=0
    IF NOT KEYWORD_SET(nosum)  THEN BEGIN
     FOR i=xrange(0),xrange(1),0.1 DO BEGIN
      index=WHERE(d.x GE i-.05 AND d.x LT i+.05,count)
      IF count GT 0 THEN BEGIN
        tmp_w=  w(index)
        tmp_m=MEDIAN(tmp_w)
        tmp_e=d.e(index)
        tmp_i=WHERE(ABS(tmp_w-tmp_m) LE 3.*tmp_e,count)
        IF count GT 0 THEN index=index(tmp_i)
      ENDIF
      IF count GT 0 THEN BEGIN
        x=[x,i]
        
        y=[y,TOTAL(w(index))/count]
        e=[e,y(N_ELEMENTS(y)-1)/SQRT(TOTAL(w(index)^2/d.e(index)^2))] 
      ENDIF
     ENDFOR
     x=x[1:N_ELEMENTS(x)-1]
     w=y[1:N_ELEMENTS(y)-1]
     e=e[1:N_ELEMENTS(e)-1]
    ENDIF
  ENDIF ELSE PRINT,'WorkSpace should be a 2theta scan of >2 steps'
ENDIF ELSE PRINT,'WorkSpace Numbers missing'
IF KEYWORD_SET(nosum) THEN BEGIN
    index=SORT(d.x)
    w=w(index)
    e=d.e(index)
    x=d.x(index)
ENDIF
mod_datp,d,'e',e
mod_datp,d,'x',x
give_datp,d
RETURN,w
END
