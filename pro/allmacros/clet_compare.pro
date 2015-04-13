PRO clet_compare, a=a,cp=cathod_pure, cc=cathod_clet,$
                  cp3d=cathod_pure_3d, cc3d=cathod_clet_3d, $
                  sp=signal_pure,sc=signal_clet,x=x,yc=y_clet,yp=y_pure,$
                  ps=ps,portrait=portrait,landscape=landscape,$
                  left=left,right=right,counts=counts
;a0 factor,
;a1 t0,
;a2 sigma
;a3 capture_ratio,
;a4 gamma_ratio,
;a5 gamma_exp,
;a6 trace/cell_width,
;a7 NO_CLET=0/CLET=1

IF NOT KEYWORD_SET(x)         THEN x=(findgen(100)+1)/1.
IF NOT KEYWORD_SET(a)         THEN a=[3450,106,10.,.27,240,3.5,1.4]
IF NOT KEYWORD_SET(ps)        THEN ps=0
IF NOT KEYWORD_SET(portrait)  THEN portrait=0 ELSE landscape=0
IF NOT KEYWORD_SET(landscape) THEN landscape=0
IF NOT KEYWORD_SET(left)      THEN left=0
IF NOT KEYWORD_SET(right)     THEN right=0
IF NOT KEYWORD_SET(counts)    THEN counts=0

cathod,x,a,cathod_clet,ps=ps,landscape=landscape,portrait=portrait
cathod,x,a,cathod_pure,ps=ps,landscape=landscape,portrait=portrait,/noclet,left=left,right=right,counts=counts

y=cathod_pure
IF KEYWORD_SET(ps) THEN BEGIN
  i=1
  WHILE y(i) LE y(i-1) AND i LT N_ELEMENTS(y) DO i = i+1
  IF NOT (i LT N_ELEMENTS(y)) THEN i=0
  SET_PLOT,'ps'
  DEVICE,FILENAME='cathod.ps',portrait=portrait,landscape=landscape
  PLOT,x,y,$
    TITLE='Amplitude histogram for detector cathod with/without CLET '+systime(),$
    YRANGE=[0,max(y(i:N_ELEMENTS(y)-1))],$
    YTITLE='Number of counts',$
    XTITLE='Signal amplitude',$
    SUBTITLE='hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,5)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5)
  yy=cathod_clet
  PLOT,x,yy,/noerase,$
    TITLE='Amplitude histogram for detector cathod with/without CLET '+systime(),$
    YRANGE=[0,max(y(i:N_ELEMENTS(y)-1))],$
    YTITLE='Number of counts',$
    XTITLE='Signal amplitude',$
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

signal_clet=signal_cathod(cathod_clet,a,x=x,ps=ps,landscape=landscape,portrait=portrait)
signal_pure=signal_cathod(cathod_pure,a,x=x,ps=ps,landscape=landscape,portrait=portrait)

y=signal_pure
IF KEYWORD_SET(ps) THEN BEGIN
  SET_PLOT,'ps'
  DEVICE,FILENAME='signal.ps',portrait=portrait,landscape=landscape
  PLOT,x,y,$
    TITLE='Integrated counts for detector cathod with/without CLET'+systime(),$
    YRANGE=[0,max(y)],$ 
    YTITLE='Number of counts',$
    XTITLE='Threshold',$
    SUBTITLE='hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,5)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5)
  yy=signal_clet
  PLOT,x,yy,/NOERASE,$
    TITLE='Integrated counts for detector cathod with/without CLET'+systime(),$
    YRANGE=[0,max(y)],$ 
    YTITLE='Number of counts',$
    XTITLE='Threshold',$
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

cathod_3d,x,y_clet,a,cathod_clet3d, ps=ps,landscape=landscape,portrait=portrait
cathod_3d,x,y_pure,a,cathod_pure_3d,ps=ps,landscape=landscape,portrait=portrait,/noclet

END
