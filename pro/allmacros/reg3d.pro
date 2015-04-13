pro reg3d,w,filename,extension=extension,comment=comment,par=par,bad=bad,datp=datp
;+
; Variante of d20reg for 3D-workspaces ...
; Created 27 July 1998 by Th. Hansen after demand of X. Turrillas
;-
@lamp.cbk
IF N_ELEMENTS(w(0,*,0)) LE 1 THEN PRINT,'Attention: The macro is not usefull for simple 2D workspaces, better use d20reg!'
IF N_ELEMENTS(w(0,0,*)) GT 1 THEN PRINT,'Attention: This macro is not ready for 4D workspaces!'
IF NOT KEYWORD_SET(datp) THEN take_datp,datp
if n_elements(filename) eq 0 then filename=W_NUMOR(alone)
if n_elements(extension) eq 0 then extension='xyz'
datname=filename+'.'+extension
txtname=filename+'.'+'txt'
OPENW,txt,txtname,/get_lun
cross='#'
PRINTF,txt,'# ',strmid(datp.other_tit ,0,76)   
IF STRLEN(datp.other_tit) GT 76 THEN PRINTF,txt,cross+' ...',strmid(datp.other_tit ,76,72)
PRINTF,txt,cross+' ',strmid(datp.w_tit  ,0,76)   
IF STRLEN(datp.w_tit) GT 76 THEN PRINTF,txt,cross+' ... ',strmid(datp.w_tit ,76,72)
IF KEYWORD_SET(comment) THEN PRINTF,txt,cross+' ',comment
PRINTF,txt,cross+''
PRINTF,txt,cross+' telnet d20sgi.ill.fr | lamp -nw'
FOR i=1,20 DO BEGIN
    search='w'+strcompress(string(i),/remove)
    flag=0
    IF i NE alone THEN FOR j=0,STRLEN(his(alone))-STRLEN(search)-1 DO BEGIN
      test=strmid(his(alone),j,STRLEN(search))
      if test EQ search and flag eq 0 THEN  BEGIN
        PRINTF,txt,cross+'   '+strmid(his(i),0,76)   
        IF STRLEN(his(i)) GT 76 THEN PRINTF,txt,cross+' ...'+strmid(his(i) ,76,72)
        flag=1
      ENDIF
    ENDFOR
ENDFOR
PRINTF,txt,cross+'   '+strmid(his(alone),0,76)  
IF STRLEN(his(alone)) GT 76 THEN PRINTF,txt,cross+' ...'+strmid(his(alone) ,76,72)
PRINTF,txt,cross+' d20reg,W',strcompress(string(alone),/r),',"',filename,'","',extension,'"'
PRINTF,txt,cross+''
PRINTF,txt,cross+' Created: ',systime()
PRINTF,txt,cross+'      By: ',lamp_host
PRINTF,txt,cross+' x-y-z-dz to be read by xmgr, gnuplot, igor etc.' 
PRINTF,txt,cross+STRCOMPRESS(N_ELEMENTS(w(*,0,0)))+' *'+STRCOMPRESS(N_ELEMENTS(w(0,*,0)))+' points' 
IF KEYWORD_SET(par) THEN PRINTF,txt,cross+''
IF KEYWORD_SET(par) THEN par=par(WHERE(par LT N_ELEMENTS(datp.p) AND par GE 0))
IF KEYWORD_SET(par) THEN FOR i=0,N_ELEMENTS(par)-1 DO PRINTF,txt,cross+' ',datp.par_txt(par(i)),datp.p(par(i))
IF KEYWORD_SET(bad) THEN PRINTF,txt,cross+''
IF KEYWORD_SET(bad) THEN BEGIN
IF N_ELEMENTS(datp.z) EQ 0 THEN PRINTF,txt,cross+' no bad cells' ELSE BEGIN
    IF N_ELEMENTS(w(*,0))+N_ELEMENTS(datp.z) EQ 1600 THEN BEGIN
      PRINTF,txt,cross+string(N_ELEMENTS(datp.z))+' bad cells excluded : cell-no.' 
      FOR i=0,(N_ELEMENTS(datp.z)-1)/15 DO PRINTF,txt,cross+strcompress(datp.z(i*15:i*15+14))
    ENDIF ELSE BEGIN
      PRINTF,txt,cross+string(N_ELEMENTS(datp.z))+' bad cells not excluded, evt. interpolated : cell-no. angle (2th.)' 
      FOR i=0,(N_ELEMENTS(datp.z)-1) DO PRINTF,txt,cross,datp.z(i),datp.x(datp.z(i))
      ENDELSE
  ENDELSE
ENDIF
PRINTF,txt,cross+''
PRINTF,txt,cross+' Monitor/counts   Counting time/sec (A2,F15.0,F9.1)'
PRINTF,txt,format='(A2,F15.0,F9.1)',cross+' ',datp.n(0,0:1<(N_ELEMENTS(datp.n(0,*,0))-1),0)
PRINTF,txt,cross+''
PRINTF,txt,FORMAT="(2A1,A11,3A13)",cross,' ',datp.x_tit,datp.y_tit,datp.z_tit,'sigma'
FREE_LUN,txt
OPENW,dat,datname,/get_lun
PRINT,'Export of ',N_ELEMENTS(w(*,0,0)),' *',N_ELEMENTS(w(0,*,0)), ' cells to ',filename,'.',extension
IF N_ELEMENTS(datp.e(*,*,0)) NE N_ELEMENTS(w(*,*,0)) THEN e=SQRT(w) ELSE e=datp.e
j=(N_ELEMENTS(w(0,*,0)))
i=(N_ELEMENTS(w(*,0,0)))
k=INDGEN(N_ELEMENTS(w(*,*,0)))
out=FLTARR(4,N_ELEMENTS(w(*,*,0)))
IF N_ELEMENTS(datp.x(0,*)) LE 1 THEN  out(0,*) = datp.x(k mod i) ELSE BEGIN
  out(0,*) = datp.x(k mod i,(k / i)<(N_ELEMENTS(datp.x(0,*,0))-1),0)
ENDELSE
IF N_ELEMENTS(datp.y(0,*)) LE 1 THEN out(1,*) = datp.y(k / i) ELSE  BEGIN
  out(1,*) = datp.y((k mod i)<(N_ELEMENTS(datp.y(*,0,0))-1),(k / i)<(N_ELEMENTS(datp.y(0,*,0))-1),0)
ENDELSE
out(2,*) = w(k)
out(3,*) = e(k)
PRINTF, dat,out
PRINT, 'finished - use tempcat for list of numor - time - temperature - monitor ...'
FREE_LUN,dat
END
