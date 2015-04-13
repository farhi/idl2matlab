PRO slicefield, xr, yr, zr, u, v, w, arrows=arrows, DAMPING=damp,size=size,$
                VELOVECT=velovect,scale=scale,$
                reduce=reduce
;+
;field2d,charge,[-.5,1.5,.1],[-.5,1.5,.1],[-.5,1.5,.1],u,v,w,sx=sx,sy=sy,sz=sz
;field2d,electrodes,[50,120,10],[380,420,10],[-1.5,1.5,1.],u,v,w,sx=sx,sy=sy,sz=sz,/prog
;SCALE3, X=[-2,2],Y=[-2,2],Z=[-2,2],ax=40,az=0
;flow3,u,v,w,arrowsize=.02,sx=sx,sy=sy,sz=sz
;-
IF NOT KEYWORD_SET(reduce) THEN reduce=1. ELSE reduce=FLOAT(reduce)
xsize=ABS(xr[1]-xr[0])
ysize=ABS(yr[1]-yr[0])
zsize=ABS(zr[1]-zr[0])
IF NOT KEYWORD_SET(scale) THEN scale=[0,MAX([N_ELEMENTS(u(*,0,0)),N_ELEMENTS(u(0,*,0)),N_ELEMENTS(u(0,0,*))])]
IF NOT KEYWORD_SET(damp) THEN BEGIN
  IF KEYWORD_SET(velovect) THEN damp=20. ELSE damp=200.
ENDIF
IF NOT KEYWORD_SET(arrows) THEN arrows=200
ASPECT=FLOAT(xsize)/FLOAT(ysize)
window,0,xsize=1000*aspect,ysize=1000
for k= 0,N_ELEMENTS(u(0,0,*))-1 DO BEGIN
  WSET,0
  field=SQRT(u(*,*,k)^2+v(*,*,k)^2)/damp
  IF KEYWORD_SET(VeloVect) THEN BEGIN
    VELOVECT,u(*,*,k),v(*,*,k),INDGEN(xsize/xr[2]+1)*xr[2]+xr[0],INDGEN(ysize/yr[2]+1)*yr[2]+yr[0],TITLE='z'+STRCOMPRESS(z),LENGTH=max(field)/reduce
  ENDIF ELSE BEGIN
    PLOT_FIELD,u(*,*,k),v(*,*,k),TITLE='z'+STRCOMPRESS(zr(0)+k*zr(2)),n=arrows,ASPECT=aspect,LENGTH=max(field)/reduce
  ENDELSE
ENDFOR
END
