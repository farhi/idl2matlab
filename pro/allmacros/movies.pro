PRO animfield, xr, yr, zr, u, v, w, arrows=arrows,$
               nvecs=nvecs,size=size,$
               sx=sx,sy=sy,sz=sz, scale=scale,$
               probe=probe,nframes=nframes,$
               xtimes=xtimes,ytimes=ytimes,len=len
;+
;field2d,charge,(-.5,1.5,.1],(-.5,1.5,.1],(-.5,1.5,.1],u,v,w,sx=sx,sy=sy,sz=sz
;field2d,electrodes,(50,120,10],(380,420,10],(-1.5,1.5,1.],u,v,w,sx=sx,sy=sy,sz=sz,/prog
;SCALE3, X=(-2,2],Y=(-2,2],Z=(-2,2],ax=40,az=0
;flow3,u,v,w,arrowsize=.02,sx=sx,sy=sy,sz=sz
;-
IF NOT KEYWORD_SET(len) THEN len=2.
xsize=ABS(xr(1)-xr(0))
ysize=ABS(yr(1)-yr(0))
zsize=ABS(zr(1)-zr(0))
IF NOT KEYWORD_SET(xtimes) THEN xtimes=2
IF NOT KEYWORD_SET(ztimes) THEN ztimes=1
IF NOT KEYWORD_SET(sx) THEN sx=0
IF NOT KEYWORD_SET(sy) THEN sy=0
IF NOT KEYWORD_SET(sz) THEN sz=0
IF KEYWORD_SET(nvecs) THEN BEGIN
  sx=0
  sy=0
  sz=0
ENDIF ELSE nvecs=0
IF NOT KEYWORD_SET(scale) THEN scale=[0,MAX([N_ELEMENTS(u(*,0,0)),N_ELEMENTS(u(0,*,0)),N_ELEMENTS(u(0,0,*))])]
IF NOT KEYWORD_SET(arrows) THEN arrows=200
ASPECT=FLOAT(xsize)/FLOAT(ysize)
IF NOT KEYWORD_SEt(size) THEN sizx=400 ELSE sizx=size
sizy=sizx
WINDOW, /FREE, XSIZE=sizx, YSIZE=sizy, COLORS=-16
IF NOT KEYWORD_SET ( nframes ) THEN nframes = 160
XINTERANIMATE, SET=[sizx, sizy, nframes] 
FOR i = 0, nframes - 1 DO BEGIN 
    SCALE3, X=scale,Y=scale,Z=scale,AX=i*ROUND(xtimes)*360./nframes,AZ=i*ROUND(ztimes)*360./nframes 
    ERASE 
    PRINT,'Frame',i,', AX=',i*ROUND(xtimes)*360./nframes,', AZ=',i*ROUND(ztimes)*360./nframes
    IF KEYWORD_SET(nvecs) THEN BEGIN
      flow3,u,v,w,arrowsize=.02,nvecs=nvecs,len=len
    ENDIF ELSE BEGIN
      flow3,u,v,w,arrowsize=.02,sx=sx,sy=sy,sz=sz,len=len
    ENDELSE
    SCALE3,AX=i*720./nframes,AZ=i*360./nframes 
    XINTERANIMATE, FRAME=i, WINDOW=!D.WINDOW
ENDFOR 
WDELETE
XINTERANIMATE
END
