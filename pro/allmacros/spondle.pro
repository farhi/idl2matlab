PRO spondle,fil,a,b,c

y=fltarr(10000)
x=fltarr(10000)
e=fltarr(10000)
d=fltarr(10000)
xx='xxxxx'
print,fil
openr,unit,fil,/get_lun
i=0
while strmid(xx,1,3) ne 'lam' do begin
  readf,unit,xx
  print,xx
endwhile
readf,unit,xx
print,xx
while  not eof(unit) do begin

 readf,unit,k,l,m,n
 
 y(i)=k & x(i)=l & e(i)=m
 i=i+1
endwhile

a=alog10(x(0:i-1)) & b=y(0:i-1) & c=e(0:i-1)/(x(0:i-1)*alog(10))


free_lun,unit
return
print,!stime
end
