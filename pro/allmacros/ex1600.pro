PRO ex1600,w,fil

IF N_ELEMENTS(w) NE 1600 THEN BEGIN
  PRINT,'Array does not contain 1600 points!'
  fil=fil+'.'+STRCOMPRESS(N_ELEMENTS(w),/REMOVE_ALL)
  PRINT,'Stored as ', fil
ENDIF
openw,a,/get,fil
printf,a,w
free_lun,a

END
