pro titles,maxi=maxi,list=final,high=high,low=low

common C_LAMP_ACCESS
common C_LAMP

IF NOT KEYWORD_SET(maxi) THEN maxi=100
IF NOT KEYWORD_SET(maxi) THEN maxi=100
inst=STRLOWCASE(INST_VALUE)
length=STRLEN(PATH_FOR_ONLINE+inst+'_'+LAMP_DVD)+1
PRINT,PATH_FOR_ONLINE
IF N_ELEMENTS(final) LT 1 THEN FOR i=0,9 DO BEGIN
 FOR j=100,199 DO BEGIN
  list=FINDFILE(PATH_FOR_ONLINE+inst+'_'+STRCOMPRESS(i,/REMOVE)+LAMP_DVD+'??'+STRMID(STRCOMPRESS(j,/REMOVE),1,2)+'??',count=count)
  IF count GT 0 THEN BEGIN
    list=STRCOMPRESS(STRMID(list,length,6),/remove)
    IF N_ELEMENTS(final) GE 1 THEN final=[final,list] ELSE final=list
  ENDIF
 ENDFOR
 PRINT,i,N_ELEMENTS(final)
ENDFOR
IF N_ELEMENTS(final) LE 0 THEN  FOR j=100,199 DO BEGIN
  final=FINDFILE(PATH_FOR_ONLINE+inst+'_'+STRCOMPRESS(i,/REMOVE)+LAMP_DVD+'??'+STRMID(STRCOMPRESS(j,/REMOVE),1,2)+'??',count=count)
ENDFOR
datp=1
list=final
IF KEYWORD_SET(high) THEN list=list(WHERE(LONG(list) LE high)) 
IF KEYWORD_SET(low)  THEN list=list(WHERE(LONG(list) GE low)) 
FOR i=N_ELEMENTS(list)-1,(N_ELEMENTS(list)-maxi)>0,-1 DO BEGIN
  w=rdrun(LONG(list(i)),datp=datp)
  PRINT,list(i),' ',datp.other_tit,datp.w_tit
ENDFOR
END
