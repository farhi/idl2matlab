PRO cathod,x,a,y,ps=ps,portrait=portrait,landscape=landscape,noclet=noclet,$
           left=left,right=right,counts=counts
;a0 factor,
;a1 t0,
;a2 sigma
;a3 capture_ratio,
;a4 gamma_ratio,
;a5 gamma_exp,
;a6 trace/cell_width,
;a7 NO_CLET=0/CLET=1

take_datp,datp
w=x*0.0
y=x*0.0
start_angle =  5.0
stop_angle  = 85.0
step_angle  =  10.0
start = .05
stop  = .95
step  = .10
;inside_counter=ABS((stop-start)/step)*(ABS((stop_angle-start_angle)/step_angle))
inside_counter=ABS((stop-start)/step)*(ABS((stop_angle-start_angle)/step_angle))^2
print,'Steps: ',inside_counter
IF KEYWORD_SET(noclet) THEN BEGIN
  start = start-a(6)
  stop  = stop +a(6)
ENDIF
counter=ABS((stop-start)/step)*ABS((stop_angle-start_angle)/step_angle)
IF KEYWORD_SET(noclet) THEN clet =' without CLET ' ELSE clet =' with CLET '
IF KEYWORD_SET(noclet) THEN name ='cathod_pure.ps' ELSE name ='cathod_clet.ps'
IF KEYWORD_SET(noclet) THEN a    = [a(0:6), 0.]    ELSE a    = [a(0:6), 1.] 
counts=fltarr(N_ELEMENTS(x))
canals=N_ELEMENTS(w)
IF KEYWORD_SET(right) THEN k_right=where(x GE right)
IF KEYWORD_SET(left)  THEN k_left =where(x GE left)
FOR j = start,      stop,      step       DO BEGIN
  FOR   i = start_angle,stop_angle,step_angle DO FOR   ii= start_angle,stop_angle,step_angle DO BEGIN
  ;FOR   i = start_angle,stop_angle,step_angle DO BEGIN
    project  = SIN(i/180.*!PI)*a(6) ; *SIN(ii/180.*!PI)
    tt       = a(1) * (min([(j+project/2.),1.]) - max([(j-project/2.),0.])) / project
    IF tt GE 0. THEN BEGIN
      abbruch,x,[a(0),tt,a(2:5)],w
      y=y+w/inside_counter
      I_mid = fltarr(canals)
      FOR l=0,canals-1 DO I_mid(l)=total(w(l:canals-1)*x(l:canals-1))/x(l)
      I_left = 0.
      IF KEYWORD_SET(left) THEN BEGIN
        tt_left       = a(1) * (-min([j-project/2.,0.])) / project
        IF (tt_left) GT (0) THEN BEGIN 
          abbruch,x,[a(0),tt_left,a(2:5)],w_left
          ;FOR k=0,canals-1 DO IF x(k) GE left THEN I_left = I_left + w_left(k) * x(k) / left 
          I_left= total(w_left(k_left) * x(k_left))/left
        ENDIF ELSE I_left=0.
      ENDIF
      I_right = 0.
      IF KEYWORD_SET(right) THEN BEGIN
        tt_right       = a(1) * (max([j+project/2.-1.,0.])) / project
        IF (tt_right) GT (0) THEN BEGIN
          abbruch,x,[a(0),tt_right,a(2:5)],w_right
          ;FOR k=0,canals-1 DO IF x(k) GE right THEN I_right = I_right + w_right(k) * x(k) / right 
          I_right= total(w_right(k_right) * x(k_right))/right
        ENDIF ELSE I_right=0.
        IF KEYWORD_SET(left) THEN BEGIN
          PRINT,j,i,ii,project,tt
          ;PRINT,i,j,project,tt
          ;PRINT,' ',tt_left,I_left,tt_right,I_right
        ENDIF
      ENDIF
      signals=fltarr(canals)
      FOR k=0, canals-1 DO signals(k) = TOTAL (w (k:canals-1))/inside_counter/canals
      ;count_flag = fltarr(canals)
      ;count_flag(where((I_mid GT I_left) AND (I_mid GT I_right))) = 1.
      ;counts = counts + count_flag * signals
      counts = counts + I_mid/(I_mid+I_left+I_right) * signals
      ;IF KEYWORD_SET(right) THEN print,I_mid(k_right)/(I_mid(k_right)+I_left+I_right)
      ;IF KEYWORD_SET(right) THEN plot,x,I_mid(k_right)/(I_mid(k_right)+I_left+I_right),yrange=[0,1]
      plot,x,I_mid/(I_mid+I_left+I_right) * signals ;,yrange=[0,1]
    ENDIF
  ENDFOR
  ;plot,x,counts,yrange=[0,1000]
ENDFOR
IF KEYWORD_SET(ps) THEN BEGIN
  IF NOT KEYWORD_SET(portrait) THEN portrait=0 ELSE landscape=0
  IF NOT KEYWORD_SET(landscape) THEN landscape=0
  i=1
  WHILE y(i) LE y(i-1) AND i LT N_ELEMENTS(y) DO i = i+1
  IF NOT (i LT N_ELEMENTS(y)) THEN i=0
  SET_PLOT,'ps'
  DEVICE,FILENAME=name,portrait=portrait,landscape=landscape
  PLOT,x,y,$
    TITLE='Amplitude histogram for detector cathod'+clet+systime(),$
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
mod_datp,datp,'w_tit','Amplitude histogram for detector cathod'+clet+systime()
mod_datp,datp,'other_tit','hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,4)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,5)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,5)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,5)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,5)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,5)+$
             ', trc'+STRMID(STRCOMPRESS(STRING(a(6))),0,5)
give_datp,datp
END
