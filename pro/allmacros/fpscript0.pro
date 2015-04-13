PRO fpscript0 ,name,first,last,hkl
;***
;**
;** The call is fpscript,...

IF NOT KEYWORD_SET(hkl) THEN hkl=0
LF=string(10b)
k=0
l=0
openw,unit,name+'.sh',/get_lun
IF STRPOS(!VERSION.os,'MacOS') NE -1 THEN BEGIN
    printf,unit,'tell application "Finder"'
ENDIF
step=1
if last lt first then step=-1
for i=first+1,last,step do begin
  IF STRPOS(!VERSION.os,'MacOS') NE -1 THEN BEGIN
    printf,unit,'  duplicate file "'+name+STRCOMPRESS(i-1,/REMOVE)+'.pcr"'
    printf,unit,'  try'
    printf,unit,'    delete file "'+name+STRCOMPRESS(i,/REMOVE)+'.pcr"'
    printf,unit,'  end try'
    printf,unit,'  set name of file "'+name+STRCOMPRESS(i-1,/REMOVE)+'.pcr copy" to "'+name+STRCOMPRESS(i,/REMOVE)+'.pcr"'
    FOR j=1,hkl DO BEGIN
      printf,unit,'  duplicate file "'+name+STRCOMPRESS(i-1,/REMOVE)+STRCOMPRESS(j,/REMOVE)+'.hkl"'
      printf,unit,'  try'
      printf,unit,'    delete file "'+name+STRCOMPRESS(i,/REMOVE)+STRCOMPRESS(j,/REMOVE)+'.hkl"'
      printf,unit,'  end try'
      printf,unit,'  set name of file "'+name+STRCOMPRESS(i-1,/REMOVE)+STRCOMPRESS(j,/REMOVE)+'.hkl copy" to "'+name+STRCOMPRESS(i,/REMOVE)+STRCOMPRESS(j,/REMOVE)+'.hkl"'
    ENDFOR
    printf,unit,'  open file "'+name+STRCOMPRESS(i,/REMOVE)+'.pcr" using file "HansenG3:Applications (Mac OS 9):JFullProf:FullProf"'
    printf,unit,'tell application "Igor Pro"'
    printf,unit,'  Do Script "dowindow/k graph1;LoadWave/T/O \"'+name+STRCOMPRESS(i,/REMOVE)+'.prf\""'
    printf,unit,'  Do Script "Redimension/N=(-1,'+STRCOMPRESS(i-first+1,/RE)+') calc obs;calc[]['+STRCOMPRESS(i-first,/RE)+']=icalc[p];obs[]['+STRCOMPRESS(i-first,/RE)+']=iobs[p]"'
    printf,unit,'end tell'
    k=k+1
    IF k EQ 40 THEN BEGIN
      printf,unit,'end tell'
      free_lun,unit
      openw,unit,name+STRCOMPRESS(l)+'.sh',/get_lun
      l=l+1
      k=0
    ENDIF
  ENDIF ELSE BEGIN
    printf,unit,"cp "+name+STRCOMPRESS(i-step,/REMOVE)+".pcr "+name+STRCOMPRESS(i,/REMOVE)+".pcr "
    printf,unit,"fullprof "+name+STRCOMPRESS(i,/REMOVE)
  ENDELSE
endfor
IF STRPOS(!VERSION.os,'MacOS') NE -1 THEN BEGIN
    printf,unit,'end tell'
ENDIF
free_lun,unit
end
