PRO cathod_3d,x,y,a,f,ps=ps,shade=shade,portrait=portrait,landscape=landscape,noclet=noclet
;a0 factor,
;a1 t0,
;a2 sigma
;a3 capture_ratio,
;a4 gamma_ratio,
;a5 gamma_exp,
;a6 trace/cell_width,
;a7 NO_CLET=0/CLET=1

take_datp,datp
start_angle = 5.
stop_angle  = 85.
step_angle  = 10.
start = .05
stop  = .95
step  = .1
inside_counter=ABS((stop-start)/step)*ABS((stop_angle-start_angle)/step_angle)
IF KEYWORD_SET(noclet) THEN BEGIN
  start = start-a(6)
  stop  = stop +a(6)
ENDIF
x=x(*,0)
y=findgen(ABS((stop-start)/step))*step+start
counter=ABS((stop-start)/step)*ABS((stop_angle-start_angle)/step_angle)
IF KEYWORD_SET(noclet) THEN clet=' without CLET ' ELSE clet=' with CLET '
IF KEYWORD_SET(noclet) THEN name='3d_cathod_pure.ps' ELSE name='3d_cathod_clet.ps'
IF KEYWORD_SET(noclet) THEN a=[a,0.] ELSE a=[a,1.] 
f=fltarr(N_ELEMENTS(x(*,0)),N_ELEMENTS(y(*,0)))
w=x*0.0

FOR j = 0,ABS((stop-start)/step)-1 DO BEGIN
FOR   i = start_angle,stop_angle,step_angle DO BEGIN
    project  = SIN(i/180.*!PI)*a(6)
    tt       = a(1) * (min([y(j),project/2.]) + min([(1.-y(j)),project/2.])) / project
    abbruch,x,[a(0),tt,a(2:5)],w
    ;print,i,j,tt
    f(*,j) = f(*,j) + w/inside_counter
  ENDFOR
ENDFOR
IF KEYWORD_SET(ps) THEN BEGIN
  IF NOT KEYWORD_SET(portrait) THEN portrait=0 ELSE landscape=0
  IF NOT KEYWORD_SET(landscape) THEN landscape=0
  i=1
  ff=total(f,2)
  xdim = N_ELEMENTS(x)
  ydim = N_ELEMENTS(y)
  WHILE ff(i) LE ff(i-1) AND i LT N_ELEMENTS(ff) DO i = i+1
  IF NOT (i LT N_ELEMENTS(ff)) THEN i=0
  SET_PLOT,'ps'
  DEVICE,FILENAME=name,/COLOR,BITS_PER_PIXEL=256,portrait=portrait,landscape=landscape
  IF KEYWORD_SET(shade) THEN $
  SHADE_SURF,f(i-1:xdim-1,0:ydim-1),x(i-1:xdim-1),y,$
    TITLE='3D-Amplitude histogram for detector cathod'+clet+systime(),$
    xRANGE=[0,max(x)],$
    ZRANGE=[0,max(f(i:N_ELEMENTS(f(*,0))-1,*))],$
    ZTITLE='Number of counts',$
    SUBTITLE='hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,5)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5) $
  ELSE $
  SURFACE,f(i-1:xdim-1,0:ydim-1),x(i-1:xdim-1),y,$
    TITLE='3D-Amplitude histogram for detector cathod'+clet+systime(),$
    xRANGE=[0,max(x)],$
    ZRANGE=[0,max(f(i:N_ELEMENTS(f(*,0))-1,*))],$
    ZTITLE='Number of counts',$
    SUBTITLE='hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,5)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5) 

  DEVICE,/CLOSE
  SET_PLOT,'x'
ENDIF
mod_datp,datp,'w_tit','3D-Amplitude histogram for detector cathod'+clet+systime()
mod_datp,datp,'other_tit','hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,4)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5)
give_datp,datp
END
