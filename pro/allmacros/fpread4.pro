FUNCTION fpread4 ,name,first,last,searchstring,searchstring2,$
                  error=rd_error,nextline=nextline
;********
;**
;** The call is w12=fpread4('khp',1000,1102,'Phase No.  1','scale factor : ')

take_datp,datp
Wout=0.0
E=0.0
x=0.0
value=0.0
error=0.0
length=STRLEN(searchstring2)
for i=first,last do begin
  files=FINDFILE(name+STRCOMPRESS(i,/RE)+'.sum',COUNT=count)
  IF count GT 0 THEN BEGIN
    openr,file,name+STRCOMPRESS(i,/RE)+'.sum',/get_lun
    line=""
    WHILE STRPOS(line,searchstring) EQ -1 AND NOT EOF(file) DO BEGIN
      READF,file,line
    ENDWHILE
    IF STRPOS(line,searchstring) NE -1 THEN BEGIN
      WHILE STRPOS(line,searchstring2) EQ -1 AND NOT EOF(file) DO BEGIN
        READF,file,line
      ENDWHILE
      IF STRPOS(line,searchstring2) NE -1 THEN BEGIN
        position=STRPOS(line,searchstring2)
        text=STRMID(line,position+length,strlen(line)-position-length)
        IF KEYWORD_SET(nextline) THEN FOR j=1,nextline DO READF,file,text 
        IF KEYWORD_SET(rd_error) THEN READS,text,value,error ELSE BEGIN
	  READS,text,value
	ENDELSE
        Wout=[Wout,value]
        IF KEYWORD_SET(rd_error) THEN E=[E,error]
        x=[x,i]
      ENDIF ELSE PRINT,searchstring2,' not found in ',i
    ENDIF ELSE PRINT,searchstring,' not found in ',i
    free_lun,file
  ENDIF ELSE PRINT,'file not found for ',i
endfor
IF N_ELEMENTS(Wout) GT 1 THEN BEGIN
  Wout=Wout(1:N_ELEMENTS(Wout)-1)
  IF KEYWORD_SET(rd_error) THEN E=E(1:N_ELEMENTS(E)-1)
  x=x(1:N_ELEMENTS(x)-1)
  MOD_DATP,datp,'e',E
  MOD_DATP,datp,'x',x
  give_datp,datp
  return, Wout
ENDIF
END
