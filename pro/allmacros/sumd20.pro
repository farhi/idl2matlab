Function sumd20,wn1,wn2

common C_LAMP_INFO 

IF N_ELEMENTS(wn2) GT 1 THEN BEGIN
  n2=three
ENDIF
IF N_ELEMENTS(wn1) GT 1 THEN BEGIN
  n1=two
ENDIF
IF N_ELEMENTS(wn1) EQ 1 THEN n1=wn1
IF N_ELEMENTS(wn2) EQ 1 THEN n2=wn2
IF N_ELEMENTS(n1) EQ 1 THEN BEGIN
IF N_ELEMENTS(n2) EQ 1 THEN BEGIN
take_w,w1,w=n1
take_w,w2,w=n2
IF N_ELEMENTS(w1(0,*)) LE 1 THEN BEGIN
 IF N_ELEMENTS(w2(0,*)) LE 1 THEN BEGIN
  take_datp,d1,w=n1
  take_datp,d2,w=n2
  e1=d1.e
  e2=d2.e
  IF d1.n(0) NE d2.n(0) THEN BEGIN
    norm=(d1.n(0) + d2.n(0))/2.
    PRINT, 'Normalisation to ', norm, ' (before : ',d1.n(0), d2.n(0),')'
    w1=w1/d1.n(0)*norm
    e1=e1/d1.n(0)*norm
    w2=w2/d2.n(0)*norm
    e2=e2/d2.n(0)*norm
  ENDIF ELSE norm=d1.n(0)
  dx1=FLOOR(d1.x*10)/10.
  dx2=FLOOR(d2.x*10)/10.
  delta=((d1.x(0)+d2.x(0))-(dx1(0)+dx2(0)))/2
  x1=delta+dx1
  x2=delta+dx2
  i1=INTERPOL(w1,d1.x,x1)
  e1=INTERPOL(e1,d1.x,x1)
  i2=INTERPOL(w2,d2.x,x2)
  e2=INTERPOL(e2,d2.x,x2)
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
  d.n(0)=norm
  d.w_tit=d.w_tit+' - Average W'+STRCOMPRESS(n1,/RE)+'+W'+STRCOMPRESS(n2,/RE)
  give_datp,d
  RETURN,w
 ENDIF ELSE PRINT,'2nd WorkSpace should not be pluri-dimensional'
ENDIF ELSE PRINT,'1st WorkSpace should not be pluri-dimensional'
ENDIF ELSE PRINT,'2nd WorkSpace Number missing'
ENDIF ELSE PRINT,'WorkSpace Numbers missing'
END
