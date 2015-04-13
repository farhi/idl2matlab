pro exclusions,w
common calibration
common d20
@lamp.cbk
take_datp,datp
print,' '
print,'Workspace ',alone
print,'Please pay attention to bad (excluded or interpolated) detector cells!'
print,'Correction flag ',datp.p(38)
int_flag=0
if datp.p(38) ge 8 then BEGIN
  datp.p(38)=datp.p(38)-8
  print,'Bad cells have been interpolated'
  int_flag=1
ENDIF
bad_flag=0
if datp.p(38) ge 4 then BEGIN
  datp.p(38)=datp.p(38)-4
  print,'Bad cells have been treated'
  bad_flag=1
ENDIF
FOR i=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
  if bad_flag eq 1 then BEGIN
    IF int_flag eq 1 THEN print,i,bad_d20(i),datp.x(bad_d20(i),0,0) else print,i,bad_d20(i)
  ENDIF
ENDFOR
print,'List of files in ~lambda/BAD_CELLS :',psd_d20
end
