FUNCTION fpread2 ,name,first,last,searchstring
;********
;**
;** The call is w6=fpread2('khp',1000,1122,'Background')
; 

take_datp,datp
Wout=0.0
x=0
E=0.0
value=0.0
error=0.0
length=STRLEN(searchstring)
for i=first,last do begin
  files=FINDFILE(name+STRCOMPRESS(i,/RE)+'.sum',COUNT=count)
  IF count GT 0 THEN BEGIN
    openr,file,name+STRCOMPRESS(i,/RE)+'.sum',/get_lun
    line=""
    WHILE STRPOS(line,searchstring) EQ -1 AND NOT EOF(file) DO BEGIN
      READF,file,line
    ENDWHILE
    IF STRPOS(line,searchstring) NE -1 THEN BEGIN
      position=STRPOS(line,searchstring)
      text=STRMID(line,position+length,strlen(line)-position-length)
      READF,file,text
      reads,text,value,error
      Wout=[Wout,value]
      E=[E,error]
      X=[X,i]
    ENDIF ELSE PRINT,'string not found in ',i
    free_lun,file
  ENDIF ELSE PRINT,'file not found for ',i
endfor
Wout=Wout(1:N_ELEMENTS(Wout)-1)
E=E(1:N_ELEMENTS(E)-1)
x=x(1:N_ELEMENTS(x)-1)
MOD_DATP,datp,'e',E
MOD_DATP,datp,'x',X
give_datp,datp
return, Wout
end
