pro d20reg,w,filename,extension,comment=comment,par=par,bad=bad,datp=datp,nolamp=nolamp
;IF NOT KEYWORD_SET(nolamp) THEN BEGIN
;  XiCute,'@lamp.cbk'
;ENDIF
IF N_ELEMENTS(w(0,*)) GT 1 THEN PRINT,'Attention: This macro does not support yet export of 2-dimensional workspaces!'
IF not keyword_set(datp) THEN take_datp,datp
;help,datp,/struc
IF N_ELEMENTS(datp.e) LT N_ELEMENTS(w) THEN mod_datp,datp,'e',SQRT(W)
if n_elements(filename) eq 0 then BEGIN
  IF N_ELEMENTS (W_NUMOR) GT 0 AND N_ELEMENTS(alone) GT 0 THEN BEGIN
    filename=W_NUMOR(alone) 
  ENDIF ELSE BEGIN
    filename='d20reg'
  ENDELSE
ENDIF
if n_elements(extension) eq 0 then extension='xy'
datname=filename+'.'+extension
OPENW,dat,datname,/get_lun
if extension ne 'dat' then cross='#' ELSE cross='!'
PRINTF,dat,cross+' ',strmid(datp.other_tit ,0,76)   
IF STRLEN(datp.other_tit) GT 76 THEN PRINTF,dat,cross+' ...',strmid(datp.other_tit ,76,72)
PRINTF,dat,cross+' ',strmid(datp.w_tit  ,0,76)   
IF STRLEN(datp.w_tit) GT 76 THEN PRINTF,dat,cross+' ... ',strmid(datp.w_tit ,76,72)
;PRINTF,dat,cross+' ',filename,'.',extension
IF KEYWORD_SET(comment) THEN PRINTF,dat,cross+' ',comment
PRINTF,dat,cross+''
PRINTF,dat,cross+' telnet d20sgi.ill.fr | lamp -nw'
FOR i=1,20 DO BEGIN
    search='w'+strcompress(string(i),/remove)
    flag=0
    IF N_ELEMENTS(alone) EQ 1 THEN IF i NE alone THEN BEGIN
      FOR j=0,STRLEN(his(alone))-STRLEN(search)-1 DO BEGIN
        test=strmid(his(alone),j,STRLEN(search))
        if test EQ search and flag eq 0 THEN  BEGIN
          PRINTF,dat,cross+'   '+strmid(his(i),0,76)   
          IF STRLEN(his(i)) GT 76 THEN PRINTF,dat,cross+' ...'+strmid(his(i) ,76,72)
          flag=1
        ENDIF
      ENDFOR
    ENDIF
ENDFOR
IF N_ELEMENTS(alone) NE 1 THEN alone=0
IF N_ELEMENTS(his) GT alone THEN BEGIN
  PRINTF,dat,cross+'   '+strmid(his(alone),0,76)  
  IF STRLEN(his(alone)) GT 76 THEN PRINTF,dat,cross+' ...'+strmid(his(alone) ,76,72)
ENDIF
PRINTF,dat,cross+' d20reg,W',strcompress(string(alone),/r),',"',filename,'","',extension,'"'
PRINTF,dat,cross+''
PRINTF,dat,cross+' Created: ',systime()
IF N_ELEMENTS(lamp_host) EQ 1 THEN PRINTF,dat,cross+'      By: ',lamp_host
if extension ne 'dat' then PRINTF,dat,cross+' x-y-dy to be read by xmgr, gnuplot, igor etc.' ELSE PRINTF,dat,cross+' fullprof : INSTR=10'
IF KEYWORD_SET(par) THEN PRINTF,dat,cross+''
IF KEYWORD_SET(par) THEN par=par(WHERE(par LT N_ELEMENTS(datp.p) AND par GE 0))
IF KEYWORD_SET(par) THEN FOR i=0,N_ELEMENTS(par)-1 DO PRINTF,dat,cross+' ',datp.par_txt(par(i)),datp.p(par(i))
IF KEYWORD_SET(bad) THEN PRINTF,dat,cross+''
IF KEYWORD_SET(bad) THEN BEGIN
  IF N_ELEMENTS(datp.z) EQ 0 THEN PRINTF,dat,cross+' no bad cells' ELSE BEGIN
    IF N_ELEMENTS(w(*,0))+N_ELEMENTS(datp.z) EQ 1600 THEN BEGIN
      PRINTF,dat,cross+string(N_ELEMENTS(datp.z))+' bad cells excluded : cell-no.' 
      FOR i=0,(N_ELEMENTS(datp.z)-1)/15 DO PRINTF,dat,cross+strcompress(datp.z(i*15:i*15+14))
    ENDIF ELSE BEGIN
      PRINTF,dat,cross+string(N_ELEMENTS(datp.z))+' bad cells not excluded, evt. interpolated : cell-no. angle (2th.)' 
      FOR i=0,(N_ELEMENTS(datp.z)-1) DO PRINTF,dat,cross,datp.z(i),datp.x(datp.z(i))
      ENDELSE
  ENDELSE
ENDIF
PRINTF,dat,cross+''
PRINTF,dat,cross+' Monitor/counts   Counting time/sec (A2,F15.0,F9.1)'
PRINTF,dat,format='(A2,F15.0,F9.1)',cross+' ',datp.n(0,0:1<(N_ELEMENTS(datp.n(0,*,0))-1),0)
PRINTF,dat,cross+''
PRINTF,dat,cross+'  angle  counts   sigma (F9.3,F15.1,F9.2)'
PRINT,'Export of ',N_ELEMENTS(w(*,0,0)), ' cells to ',filename,'.',extension
FOR i=0,N_ELEMENTS(w(*,0,0))-1 DO BEGIN
  IF datp.x(i,0,0) GE 0 THEN BEGIN
    PRINTF,dat,FORMAT='(F9.3,F25.3,F15.4)',datp.x(i,0,0),w(i,0,0)<1E19,datp.e(i,0,0)<1E9
  ENDIF
ENDFOR
; PRINTF,dat,cross+' end of data file'
FREE_LUN,dat
END
