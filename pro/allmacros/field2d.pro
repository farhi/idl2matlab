PRO field2d, charge, xr, yr, zr, u, v, w, arrows=arrows, DAMPING=damp,size=size,$
             VELOVECT=velovect,sx=sx,sy=sy,sz=sz ,gap=gap, scale=scale,$
             probe=probe,nframes=nframes,progress=progress,$
             xtimes=xtimes,ztimes=ztimes,reduce=reduce,noprint=noprint,noplot=noplot
;+
;field2d,charge,[-.5,1.5,.1],[-.5,1.5,.1],[-.5,1.5,.1],u,v,w,sx=sx,sy=sy,sz=sz
;field2d,electrodes,[50,120,10],[380,420,10],[-1.5,1.5,1.],u,v,w,sx=sx,sy=sy,sz=sz,/prog
;SCALE3, X=[-2,2],Y=[-2,2],Z=[-2,2],ax=40,az=0
;flow3,u,v,w,arrowsize=.02,sx=sx,sy=sy,sz=sz
;-


charge(3,WHERE(charge(3,*) LT 0,tmp))=-N_ELEMENTS(WHERE(charge(3,*) GT 0))
charge(3,WHERE(charge(3,*) GT 0,tmp))=tmp

IF NOT KEYWORD_SET(probe) THEN probe=-1.
IF NOT KEYWORD_SET(reduce) THEN reduce=1. ELSE reduce=FLOAT(reduce)
xsize=ABS(xr(1)-xr(0))
ysize=ABS(yr(1)-yr(0))
zsize=ABS(zr(1)-zr(0))
u= FLTARR(xsize/xr(2)+1,ysize/yr(2)+1,zsize/zr(2)+1)
v= u
w= u
IF NOT KEYWORD_SET(scale) THEN scale=[0,MAX([N_ELEMENTS(u(*,0,0)),N_ELEMENTS(u(0,*,0)),N_ELEMENTS(u(0,0,*))])]
IF NOT KEYWORD_SET(gap) THEN gap=1
gap=ROUND(gap)
i=0
IF NOT KEYWORD_SET(damp) THEN BEGIN
  IF KEYWORD_SET(velovect) THEN damp=20. ELSE damp=200.
ENDIF
IF NOT KEYWORD_SET(arrows) THEN arrows=200
ASPECT=FLOAT(xsize)/FLOAT(ysize)
IF N_ELEMENTS(u(0,*,0)) LE 1 THEN ASPECT=FLOAT(xsize)/FLOAT(zsize)
IF aspect LT 1 THEN window,0,xsize=1000*(aspect>0.5),ysize=1000 ELSE window,0,xsize=1000,ysize=1000/(aspect<2)
i=0L
j=0L
k=0L
l=0L
sx=FLTARR(N_ELEMENTS(u))
sy=sx
sz=sx
for z= zr(0),zr(1)+zr(2)/10.,zr(2) DO BEGIN
   for y= yr(0),yr(1)+yr(2)/10.,yr(2) DO BEGIN
     for x= xr(0),xr(1)+xr(2)/10.,xr(2) DO BEGIN
       E= field([x,y,z,probe], charge, min([xr(2),yr(2)])/10.)
       IF NOT KEYWORD_SET(noprint) THEN print,x,y,z,SQRT(TOTAL(E^2))
       u(i,j,k)=E(0)
       v(i,j,k)=E(1)
       w(i,j,k)=E(2)
       i=i+1
    ENDFOR
    IF KEYWORD_SET(noprint) THEN print,x,y,z,SQRT(TOTAL(E^2))
    j=j+1
    i=0
  ENDFOR
  field=SQRT(u(*,*,k)^2+v(*,*,k)^2)/damp
  IF NOT KEYWORD_SET(noplot) THEN IF N_ELEMENTS(u(0,*,0)) GT 1 THEN BEGIN
     WSET,0
     IF KEYWORD_SET(VeloVect) THEN BEGIN
      VELOVECT,u(*,*,k),v(*,*,k),INDGEN(xsize/xr(2)+1)*xr(2)+xr(0),INDGEN(ysize/yr(2)+1)*yr(2)+yr(0),TITLE='z'+STRCOMPRESS(z),LENGTH=max(field)/reduce
    ENDIF ELSE BEGIN
      PLOT_FIELD,u(*,*,k),v(*,*,k),TITLE='z'+STRCOMPRESS(z),n=arrows,ASPECT=aspect,LENGTH=max(field)/reduce
    ENDELSE
  ENDIF
  k=k+1
  j=0
ENDFOR
IF NOT KEYWORD_SET(noplot) THEN IF N_ELEMENTS(u(0,*,0)) LE 1 AND N_ELEMENTS(u(*,0,0)) GT 1 AND N_ELEMENTS(u(0,0,*)) GT 1 THEN BEGIN
     field=SQRT(u(*,*,*)^2+v(*,*,*)^2+w(*,*,*)^2)/damp
     WSET,0
     IF KEYWORD_SET(VeloVect) THEN BEGIN
      VELOVECT,u(*,0,*),v(*,0,*),INDGEN(xsize/xr(2)+1)*xr(2)+xr(0),INDGEN(zsize/zr(2)+1)*zr(2)+zr(0),TITLE='y'+STRCOMPRESS(y);,LENGTH=max(field)/reduce
    ENDIF ELSE BEGIN
      PLOT_FIELD,u(*,0,*),w(*,0,*),TITLE='y'+STRCOMPRESS(y),n=arrows,ASPECT=aspect;,LENGTH=max(field)/reduce
    ENDELSE
ENDIF
k=0
l=0
sx=0L
sy=0L
sz=0L
FOR i=0L,N_ELEMENTS(u(*,0,0)),gap do FOR j=0L,N_ELEMENTS(u(0,*,0)),gap do FOR k=0L,N_ELEMENTS(u(0,0,*)),gap do begin
  sx=[sx,i]
  sy=[sy,j]
  sz=[sz,k]
  l=l+1
endfor
sx=sx(1:l-1)
sy=sy(1:l-1)
sz=sz(1:l-1)
END
