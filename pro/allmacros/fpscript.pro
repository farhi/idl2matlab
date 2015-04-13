PRO fpscript ,name,first,last,hkl=hkl
;***
;**
;** The call is fpscript,'khp',1216,1387,/hkl

IF NOT KEYWORD_SET(hkl) THEN hkl=0
openw,unit,name+'.sh',/get_lun
for i=first+1,last do begin
  printf,unit,"cp "+name+STRCOMPRESS(i-1,/REMOVE)+".pcr "+$
                    name+STRCOMPRESS(i,/REMOVE)+".pcr "
  FOR j=1,hkl DO BEGIN
    PRINTF,unit,"cp "+name+STRCOMPRESS(i-1,/REMOVE)+STRCOMPRESS(j,/REMOVE)+$
                ".hkl "+name+STRCOMPRESS(i,/REMOVE)+STRCOMPRESS(j,/REMOVE)+$
                ".hkl "
  ENDFOR
  printf,unit,"fullprof "+name+STRCOMPRESS(i,/REMOVE)
endfor
free_lun,unit
end
