Function sumscan,wn1
;+
; Merge twotheta scan of two steps
; W1 = sumscan (2)
; Function
; Parameters
;   Number of workspace to be merged
; Output
;   Workspace containing merged diagram
;-
common C_LAMP_INFO 

;help,n1
IF N_ELEMENTS(wn1) GT 1 THEN BEGIN
  n1=two
ENDIF
IF N_ELEMENTS(wn1) EQ 1 THEN n1=wn1
;help,n1
IF N_ELEMENTS(n1) EQ 1 THEN BEGIN
 take_w,w1,w=n1
; help,w1
 print,N_ELEMENTS(w1(0,*))
 IF N_ELEMENTS(w1(0,*)) EQ 2 THEN BEGIN
  take_datp,d1,w=n1
  e1=d1.e(*,0)
  e2=d1.e(*,1)
  w2=w1(*,1)
  w1=w1(*,0)
  IF d1.n(0,0,0) NE d1.n(0,0,1) THEN BEGIN
    norm=(d1.n(0,0,0) + d1.n(0,0,1))/2.
    PRINT, 'Normalisation to ', norm, ' (before : ',d1.n(0,0,0), d1.n(0,0,1),')'
    w1=w1/d1.n(0,0,0)*norm
    e1=e1/d1.n(0,0,0)*norm
    w2=w2/d1.n(0,0,1)*norm
    e2=e2/d1.n(0,0,1)*norm
    d1.n(0,*,0)=d1.n(0,*,0)*norm/d1(0,0,0)
    d1.n(0,*,1)=d1.n(0,*,1)*norm/d1(0,0,1)
  ENDIF ELSE norm=d1.n(0)
  dx1=ROUND(d1.x(*,0)*10)/10.
  dx2=ROUND(d1.x(*,1<(N_ELEMENTS(d1.x(0,*))-1))*10)/10.
;  print,d1.x(0,0),d1.x(0,1)
;  print,dx1(0),dx2(0)
  delta=((d1.x(0,0)+d1.x(0,1<(N_ELEMENTS(d1.x(0,*))-1)))-(dx1(0)+dx2(0)))/2
; help,delta
  x1=delta+dx1
  x2=delta+dx2
;  print,x1(0),x2(0)
; help,d1.x(0,*)
; print,N_ELEMENTS(d1.x(0,*))-1
; print,1<(N_ELEMENTS(d1.x(0,*))-1)
; help,w1,d1.x(*,0),x1,e1,w2,d1.x(*,1<(N_ELEMENTS(d1.x(0,*))-1)),x2,e2
  i1=INTERPOL(w1,d1.x(*,0),x1)
  e1=INTERPOL(e1,d1.x(*,0),x1)
;plot,x1,i1
  i2=INTERPOL(w2,d1.x(*,1<(N_ELEMENTS(d1.x(0,*))-1)),x2)
  e2=INTERPOL(e2,d1.x(*,1<(N_ELEMENTS(d1.x(0,*))-1)),x2)
;oplot,x2,i2
  dx1=ROUND(10.*dx1)
  dx2=ROUND(10.*dx2)
  start=min(dx1)<min(dx2)
  stop =max(dx1)>max(dx2)
  array=[dx1,dx2]
  array=array(SORT(array))
  array=array(UNIQ(array))
  x=FLOAT(array/10.)+delta
  points=N_ELEMENTS(array)
  w=FLTARR(points)
  e=FLTARR(points)
  For i= 0,points-1 DO BEGIN
    f1=where(dx1 EQ array(i),c1)
    f2=where(dx2 EQ array(i),c2)
    f1=f1(0)
    f2=f2(0)
    IF c1 GT 0 THEN BEGIN
      IF c2 GT 0 THEN BEGIN
        w(i)=(i1(f1)+i2(f2))/2.
        e(i)=w(i)/SQRT(i1(f1)^2/e1(f1)^2+i2(f2)^2/e2(f2)^2)
      ;  IF (i1(f1)-3*e1(f1) GE i2(f2)+3*e2(f2)) THEN BEGIN
      ;    print,'Problem Cells : ',ROUND((x1(f1)-x1(0))*10),'>',ROUND((x2(f2)-x2(0))*10)
      ;  ENDIF ELSE IF (i1(f1)+3*e1(f1) LE i2(f2)-3*e2(f2)) THEN BEGIN
      ;    print,'Problem Cells : ',ROUND((x1(f1)-x1(0))*10),'<',ROUND((x2(f2)-x2(0))*10)
      ;  ENDIF
      ENDIF ELSE BEGIN
        w(i)=i1(f1)
        e(i)=e1(f1)
      ENDELSE
    ENDIF ELSE BEGIN
      w(i)=i2(f2)
      e(i)=e2(f2)
    ENDELSE  
  ENDFOR
  d=d1
  mod_datp,d,'x',x
  mod_datp,d,'e',e
  mod_datp,n,'n',[norm,(d1.n(0,1,0)+d1.n(0,1,1))/2.,(d1.n(0,2,0)+d1.n(0,2,1))/2.]
  d.w_tit=d.w_tit+' - sum of 2 pattern'
  give_datp,d
  RETURN,w
 ENDIF ELSE PRINT,'WorkSpace should be a 2theta scan of 2 steps'
ENDIF ELSE PRINT,'WorkSpace Numbers missing'
END
