PRO pathcal
;******* ****
;**
common calibration
common c_rdid , dzap, pzap, pzip
;common d20

  if (sys_dep('MACHINE') eq 'mac') THEN BEGIN
    PATHCAL =P_LAMBDA()+'CALIBRATION:'
  ENDIF ELSE PATHCAL =P_LAMBDA()+'CALIBRATION/'
  if (sys_dep('MACHINE') eq 'vms') THEN PATHCAL =strmid(P_LAMBDA(),0,strlen(P_LAMBDA())-1)+ '.' +CALIBRATION +']'
  if (sys_dep('MACHINE') eq 'win') THEN PATHCAL =P_LAMBDA()+'CALIBRATION\' 
END
