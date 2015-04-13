PRO lerch,ww,ff,extension

@lamp.cbk
IF N_ELEMENTS(ff) LT 1 THEN extension='rtv'
IF N_ELEMENTS(ff) LT 1 THEN ff=W_NUMOR(alone)
IF STRPOS(ff,'.') GT 0 THEN ff=STRMID(ff,0,STRPOS(ff,'.'))
ff=ff+'.'+extension
TAKE_DATP,datp
xx=datp.x
openw,aa,ff,/get_lun            
printf,aa,xx(0),xx(1)-xx(0),max(xx)            
printf,aa,F='('+STRCOMPRESS(N_ELEMENTS(ww),/RE)+'I10)',ww
free_lun,aa            
PRINT,'Export of W'+STRCOMPRESS(alone,/RE),' with',STRCOMPRESS(N_ELEMENTS(ww)), ' counts to ',ff
END
