pro num2d20ff,start,stop,T0=T0,mT=mT

@lamp.cbk

for i=start,stop do begin
  xicute,'w1=rdrun('+string(i)+')'
  if keyword_set(T0) then xicute,'p1(12)='+string(T0+mT*(i-start))
  xicute,"d20ff,w1,'"+strcompress(string(i),/re)+"'"
endfor

end
