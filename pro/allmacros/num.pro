PRO num, filename,start=start,help=help

IF KEYWORD_SET(help) THEN BEGIN
  PRINT,' '
  PRINT,'################# LAMP/IDL Macro PROcedure NUM.pro ##################'
  PRINT,'#                                                                   #'
  PRINT,'# *************** Thomas HANSEN, June 1997 D20/ILL **************** #'
  PRINT,'#                                                                   #'
  PRINT,'# Call:  NUM,'fil',START=10000  (example)                           #'
  PRINT,'# Creates a fil.num file out of a fil.f20 file                      #'
  PRINT,'# With increasing numor numbers from START if this keyword is given #'
  PRINT,'#                                                                   #'
  PRINT,'#####################################################################'
  PRINT,' '
ENDIF

Openr,in, strmid(filename,0,3)+'.f20',/get_lun
Openw,out,strmid(filename,0,3)+'.num',/get_lun
readf,in,bid
line=''
readf,in,line
WHILE strmid(line,0,10) NE '    -10000' DO BEGIN
  printf,out,strmid(strcompress(line)+'                         ',0,80)
  print,strmid(line,0,80)
  readf,in,line
  IF KEYWORD_SET (start) THEN BEGIN
    READS,line,bid,numor
    PRINTF,out,FORMAT='(I3,I7,"                                                                      ")',bid,start
    PRINT,     FORMAT='(I3,I7,"                                                                      ")',bid,start
    start=start+1
  ENDIF ELSE BEGIN
    printf,out,strmid(line,0,80)
    print,strmid(line,0,80)
  ENDELSE
  readf,in,line
  printf,out,strmid(line,0,80)
  print,strmid(line,0,80)
  readf,in,cells,line
  FOR i=0,long(cells)/10 DO readf,in,bid
  readf,in,line
ENDWHILE
printf,out,strmid(line,0,80)
print,strmid(line,0,80)
CLOSE,in
CLOSE,out
FREE_LUN,in
FREE_LUN,out
END
