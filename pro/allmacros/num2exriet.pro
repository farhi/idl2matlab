pro num2exriet,start,stop,xoffset=xoffset,nobeambg=nobeambg,illdat=illdat
; Modification 30 August 2001 by T.C.Hansen: Keyword illdat transmitted to exriet
@lamp.cbk

IF KEYWORD_SET(nobeambg) THEN BEGIN
  print,'w2=rdrun('+string(nobeambg)+')'
  xicute,'w2=rdrun('+string(nobeambg)+')'
ENDIF
IF NOT KEYWORD_SET(illdat) THEN illdat=0

for i=start,stop do begin
  print,'w1=rdrun('+string(i)+')'
  xicute,'w1=rdrun('+string(i)+')'
  IF KEYWORD_SET(nobeambg) THEN BEGIN
    print,'w2=w2*n1(0,1,0)/n2(0,1,0)'
    xicute,'w2=w2*n1(0,1,0)/n2(0,1,0)'
    print,'n2=n1'
    xicute,'n2=n1'
    print,'w1=w1-w2'
    xicute,'w1=w1-w2'
  ENDIF
  IF KEYWORD_SET(xoffset) THEN BEGIN
    print,'x1=x1+('+string(xoffset)+')'
    xicute,'x1=x1+('+string(xoffset)+')'
  ENDIF
  print,"exriet,w1,'"+strcompress(string(i),/r)+"',illdat="+strcompress(string(illdat),/r)
  xicute,"exriet,w1,'"+strcompress(string(i),/r)+"',illdat="+strcompress(string(illdat),/r)
endfor

end
