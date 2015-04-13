
pro ascii2dat,parameter

list=findfile('*_LAMPascii')
print,'These files will be copied to *.dat:'
print,list
for i=0, N_ELEMENTS(list)-1 Do BEGIN
  SPAWN,'cp '+list(i) +' '+STRMID(list(i),0,STRPOS(list(i),'_LAMP'))+'.dat'
ENDFOR
END
