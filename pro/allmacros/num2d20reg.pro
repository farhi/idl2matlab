pro num2d20reg,start,stop,base=base,xoffset=xoffset,comment=extracomment,$
                          par=par,extension=extension,merg=merg,bin=bin,$
                          sdev=sdev,min_count=min_count

;w15=num2merg([112623,112625,112626],/sdev,min=3,bin=.2)

;@lamp.cbk
comment=''
IF not KEYWORD_SET(extracomment) THEN comment='' ELSE comment=string(extracomment)
IF not KEYWORD_SET(base) THEN base='' ELSE base=strcompress(base,/re)
partxt="["
FOR i=0,N_ELEMENTS(par)-1 DO BEGIN
  partxt=partxt+STRCOMPRESS(par(i))
  IF i NE (N_ELEMENTS(par)-1) THEN partxt=partxt+','
ENDFOR
partxt=partxt+"]"
    IF NOT KEYWORD_SET(sdev) THEN sdev=0
    IF NOT KEYWORD_SET(bin) THEN bin=0
    IF NOT KEYWORD_SET(min_count) THEN min_count=0

for i=start,stop do begin
  IF KEYWORD_SET (merg) THEN BEGIN
    print,'w1=num2merg('+string(i)+',sdev='+string(sdev)+',bin='+string(bin)+',min='+string(min_count)+',/nopr)'
    xicute,'w1=num2merg('+string(i)+',sdev='+string(sdev)+',bin='+string(bin)+',min='+string(min_count)+',/nopr)'
  ENDIF ELSE BEGIN
    print,'w1=rdrun('+string(i)+')'
    xicute,'w1=rdrun('+string(i)+')'
  ENDELSE
  IF KEYWORD_SET(xoffset) THEN print,'x1=x1+('+string(xoffset)+')'
  IF KEYWORD_SET(xoffset) THEN xicute,'x1=x1+('+string(xoffset)+')'
  IF KEYWORD_SET(xoffset) THEN comment=extracomment+' x1=x1+('+string(xoffset)+')'
  IF KEYWORD_SET(extension) THEN BEGIN
    IF KEYWORD_SET(par) THEN BEGIN
      print,"d20reg,w1,'"+base+strcompress(string(i),/re)+"','dat',comment='"+comment+"',par="+partxt
      xicute,"d20reg,w1,'"+base+strcompress(string(i),/re)+"','dat',comment='"+comment+"',par="+partxt
    ENDIF ELSE BEGIN
      print,"d20reg,w1,'"+base+strcompress(string(i),/re)+"','dat',comment='"+comment+"'"
      xicute,"d20reg,w1,'"+base+strcompress(string(i),/re)+"','dat',comment='"+comment+"'"
    ENDELSE
  ENDIF ELSE BEGIN
    IF KEYWORD_SET(par) THEN BEGIN
      print,"d20reg,w1,'"+base+strcompress(string(i),/re)+"',comment='"+comment+"',par="+partxt
      xicute,"d20reg,w1,'"+base+strcompress(string(i),/re)+"',comment='"+comment+"',par="+partxt
    ENDIF ELSE BEGIN
      print,"d20reg,w1,'"+base+strcompress(string(i),/re)+"',comment='"+comment+"'"
      xicute,"d20reg,w1,'"+base+strcompress(string(i),/re)+"',comment='"+comment+"'"
    ENDELSE
  ENDELSE
endfor
end
