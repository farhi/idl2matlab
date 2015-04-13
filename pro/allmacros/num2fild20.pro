pro num2fild20,start,stop,xoffset=xoffset

@lamp.cbk

for i=start,stop do begin
  print,'w1=rdrun('+string(i)+')'
  xicute,'w1=rdrun('+string(i)+')'
  print,'x1=x1+('+string(xoffset)+')'
  xicute,'x1=x1+('+string(xoffset)+')'
  print,'fild20,w1,/nod20,/nof20,/nonum'
  xicute,'fild20,w1,/nod20,/nof20,/nonum'
  print,'$ mv fil.dat '+strcompress(string(i),/r)+'.dat'
  xicute,'$ mv fil.dat '+strcompress(string(i),/r)+'.dat'
endfor

end
