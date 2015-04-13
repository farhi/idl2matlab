PRO rdmon,monitor,cyc=cyc

take_datp,datp
unit=1
unittxt='s '
lambda =P_LAMBDA()
IF KEYWORD_SET(cyc) THEN cyc=ROUND(cyc) ELSE BEGIN
  OPENR,in,lambda+'/CALIBRATION/last.cyc',/get_lun
  READF,in,cyc
  CLOSE,in
ENDELSE
filename=lambda+'/MONITOR/'+STRCOMPRESS(STRING(LONG(cyc)), /REMOVE_ALL)+'.mon'
  OPENR,in,filename,/get_lun
  READF,in,n0,n1,refday,elements,unit
		n0=LONG(n0)
		n1=LONG(n1)
		refday=LONG(refday)
		elements=LONG(elements)
		unit=LONG(unit)
  IF unit EQ 7*86400 THEN   unittxt='w '
  IF unit EQ 86400   THEN 	 unittxt='d '
  IF unit EQ 3600    THEN 	 unittxt='h '
		IF unit EQ 60      THEN			unittxt='mn'
  x=fltarr(elements)

help,x
  e=fltarr(elements)
	 monitor=lonarr(elements)
	 y=intarr(elements)
	 n=lonarr(elements)
  READF,in,x,monitor,e,n,y
  CLOSE,in
  CALDAT,refday,MON,DD,YY
  since=STRCOMPRESS(STRING(DD),/remove_all)+'/'+STRCOMPRESS(STRING(MON),/remove_all)+'/'+$
		      STRCOMPRESS(STRING(YY),/remove_all)
  PRINT,'First Day of Cycle',cyc,':',since
mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
mod_datp,datp,'p',[n0,n1]
mod_datp,datp,'e',e
mod_datp,datp,'n',n
mod_datp,datp,'y',y
mod_datp,datp,'x',x
help,x
help,datp
help,datp.x
mod_datp,datp,'w_tit','Monitor/time for Cycle'+STRING(cyc)
mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n1)
mod_datp,datp,'y_tit','cnt/sec'
mod_datp,datp,'x_tit','time/'+unittxt+' since '+since
give_datp,datp
end
