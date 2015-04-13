FUNCTION signal_cathod, w, a,x=x,$
                        ps=ps,portrait=portrait,landscape=landscape
;a0 factor,
;a1 t0,
;a2 sigma
;a3 capture_ratio,
;a4 gamma_ratio,
;a5 gamma_exp,
;a6 trace/cell_width,
;a7 NO_CLET=0/CLET=1

take_datp,datp
wnew=w
IF N_ELEMENTS(w(0,*)) GT 1 THEN wnew=total(w,2)
signals=wnew
canals=N_ELEMENTS(signals)
FOR i=0, canals-1 DO signals(i) = TOTAL (wnew (i:canals-1))
y=signals/canals
IF NOT KEYWORD_SET(x) THEN x=datp.x
IF N_ELEMENTS(a) EQ 7 THEN a=[a,1.]
IF a(7) EQ 0. THEN clet=' without CLET ' ELSE clet=' with CLET '
IF a(7) EQ 0. THEN name='signal_pure.ps' ELSE name='signal_clet.ps'
IF KEYWORD_SET(ps) THEN BEGIN
  IF NOT KEYWORD_SET(portrait) THEN portrait=0 ELSE landscape=0
  IF NOT KEYWORD_SET(landscape) THEN landscape=0
  SET_PLOT,'ps'
  DEVICE,FILENAME=name,portrait=portrait,landscape=landscape
  PLOT,x,y,$
    TITLE='Integrated counts for detector cathod'+clet+systime(),$
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
mod_datp,datp,'w_tit','Integrated counts for detector cathod'+clet+systime()
mod_datp,datp,'other_tit','hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,5)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5)
give_datp,datp
RETURN, y

END
