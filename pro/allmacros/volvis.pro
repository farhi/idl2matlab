PRO volvis,e,level,maxlevel,frames=frames,levels=levels,ax=ax,az=az,sceleton=wire,u=u,v=v,w=w,$
                   arrowsize=arrowsize,catvertex=catv, catpoly=catp,anvertex=anv, anpoly=anp,ps=ps,$
                   scale=scale,file=file,notv=notv,noclose=noclose

;+
;y=INDGEN((600-540)/10+1)*10+540
;x=INDGEN((129-44)/1+1)*1+44
;vol=fltarr(86,7,15)
;restore,'field09.sav'
;FOR i=0,6 do for j=0,85 do vol(j,i,0)=reduced(3,WHERE (reduced(0,*) EQ x(j) AND reduced(1,*) EQ y(i)))
;shade_volume,vol,-10,vcat,pcat
;shade_volume,vol,10,van,pan
;vcat(0,*)=vcat(0,*)/5.
;van(0,*)=van(0,*)/5.
;restore,'field10.sav'
;volvis,e,340,fra=1,/scel,u=u,v=v,w=w,ax=10,az=10,arr=0.01,catp=pcat,catv=vcat,anp=pan,anv=van,scale=[85.,60.,1.4],/notv,/ps,fil='test.ps'
;-

IF KEYWORD_SET(ps) THEN BEGIN
  SET_PLOT, 'PS'
  IF NOT KEYWORD_SET(file) THEN file='volvis.ps'
  DEVICE, FILE=file, /COLOR
  TVLCT, [0,255,0,0], [0,0,255,0], [0,0,0,255]
ENDIF
IF NOT KEYWORD_SET(arrowsize) THEN arrowsize=0.05
IF N_PARAMS() LT 2 THEN level=(MIN(e)+MAX(e))/2.
IF NOT KEYWORD_SET(frames) THEN frames=20
step=0
IF NOT KEYWORD_SET(ax) THEN ax=0
IF NOT KEYWORD_SET(az) THEN az=45
IF NOT KEYWORD_SET(levels) THEN levels=0 ELSE BEGIN
  level=MIN(e)
  step=(MAX(e)-level)/(levels+2)
  IF NOT KEYWORD_SET(frames) THEN frames=1
  IF N_PARAMS() GE 2 THEN level=level 
  IF N_PARAMS() GE 3 THEN step=(MAXlevel-level)/(levels+2)
ENDELSE
j=0
;sizx=600 & sizy=600
;XINTERANIMATE, SET=[sizx, sizy, frames]
WHILE j LE levels DO BEGIN
  level=level+step
  level=level<max(E)
  level=level>min(E)
  j=j+1
  PRINT,'Level = ',level
  SHADE_VOLUME,e,level,vox,pol
  color=pol*0+2
  IF KEYWORD_SET(anp) THEN BEGIN
      ap=anp
      av=anv
      k=0
      WHILE k LT N_ELEMENTS(ap) DO BEGIN
        ap(k+1:k+ap(k))=ap(k+1:k+ap(k))+N_ELEMENTS(vox(0,*))
        k=k+1+ap(k)
      ENDWHILE
      vox=[[vox],[av]]
      pol=[ pol , ap ]
      ;help,vox,pol
      color=[color,ap*0+1]
  ENDIF
  IF KEYWORD_SET(catp) THEN BEGIN
      cp=catp
      cv=catv
      k=0
      WHILE k LT N_ELEMENTS(cp) DO BEGIN
        cp(k+1:k+cp(k))=cp(k+1:k+cp(k))+N_ELEMENTS(vox(0,*))
        k=k+1+cp(k)
      ENDWHILE
      vox=[[vox],[cv]]
      pol=[ pol , cp ]
      color=[color,cp*0+3]
  ENDIF
  ;IF NOT KEYWORD_SET(ps) THEN color=color*0
  siz=SIZE(e)
  IF KEYWORD_SET(scale) THEN BEGIN
    siz=FLOAT(siz)
    scale=(1./scale)/MIN(1./FLOAT(scale))*siz(1:3)
    siz(1:3)=scale
  ENDIF
  SCALE3,XR=[0,siz(1)],YR=[0,siz(2)],ZR=[0,siz(3)],AX=ax,AZ=az
  FOR i=0,frames-1 DO BEGIN
    T3D,TR=[-.5,-.5,-.5],ROT=[0,360./frames,0]
    T3D,TR=[.5,.5,.5]
    IF NOT KEYWORD_SET(notv) THEN TV,POLYSHADE(vox,pol,/T3D)
    IF KEYWORD_SET(wire) THEN BEGIN
      k=0
      IF KEYWORD_SET(ps) THEN WHILE k LT N_ELEMENTS(pol) DO BEGIN
        n=pol(k)
        plots,vox(*,pol(k+1)),/T3D,COLOR=color(k)
        FOR l=1,n-1 DO plots,vox(*,pol(k+1+l)),/CONTINUE,/T3D,COLOR=color(k)
        plots,vox(*,pol(k+1)),/T3D,/CONTINUE,COLOR=color(k)
        k=k+1+n
      ENDWHILE
      IF NOT KEYWORD_SET(ps) THEN WHILE k LT N_ELEMENTS(pol) DO BEGIN
        n=pol(k)
        plots,vox(*,pol(k+1)),/T3D
        FOR l=1,n-1 DO plots,vox(*,pol(k+1+l)),/CONTINUE,/T3D
        plots,vox(*,pol(k+1)),/T3D,/CONTINUE
        k=k+1+n
      ENDWHILE
    ENDIF 
    ;XINTERANIMATE, FRAME=i, WINDOW=!D.WINDOW & $
    IF KEYWORD_SET(u) AND  KEYWORD_SET(v) AND  KEYWORD_SET(w) THEN BEGIN
      FLOW3,u,v,w,ARROWSIZE=arrowsize
    ENDIF
  ENDFOR
ENDWHILE
IF KEYWORD_SET(ps) AND NOT KEYWORD_SET(noclose) THEN DEVICE, /CLOSE
;XANIMATE
END
