PRO bundle,fil,resf,a,b,c

y=fltarr(10000)
x=fltarr(10000)
e=fltarr(10000)
d=fltarr(10000)
dq=fltarr(10000)

ny=fltarr(10000)
nx=fltarr(10000)
nee=fltarr(10000)
ndq=fltarr(10000)
;resf=2.

xx='xxxxx'
print,fil
openr,unit,fil,/get_lun

while strmid(xx,1,3) ne 'wat' do begin
  readf,unit,xx
  print,xx
;  print,'looking for wat',strmid(xx,1,3)
endwhile

readf,unit,xx
print,xx

k=.00001 & l=.000001 & m=.000001 & n=.000001

i=0
while  not eof(unit) do begin
if i eq 0 then print,'start read data'

readf,unit,k,l,m,n
print,k,l,m,n
 
 x(i)=k & y(i)=l & e(i)=m & dq(i)=n
 i=i+1
endwhile

num=i
print,'total number inthe beginning: ',num


c=0
itt=4
tot=num

for k=0,itt do begin
 blim=0
 c=0
 num=tot
 new=0
 pairs=0
 print,'itteration: ',k+1

; weight data by errors

for i=0,num do begin

  if (x(c+1)-x(c) lt dq(c)/resf and x(c+1) ne 0) then begin
     ny(new)=(y(c+1)/e(c+1)^2.+y(c)/e(c)^2.)/((e(c+1)^(-2.)+e(c)^(-2.)))
     nee(new)=(sqrt(e(c)^2.+e(c+1)^2.))/2
     nx(new)=(x(c)+x(c+1))/2.
     ndq(new)=(dq(c)+dq(c+1))/2.
     pairs=pairs+1
     print,'found a pair:'
     c=c+1
     tot=tot-1
     blim=0
  endif else begin
     ny(new)=y(c)
     nx(new)=x(c)
     nee(new)=e(c)
     ndq(new)=dq(c)
  endelse
new=new+1 
c=c+1

endfor
 y=ny
 x=nx
 e=nee
 dq=ndq

 print,'tot num,new,pairs:',tot,num,new,pairs
 if (blim eq 0) then itt=1
endfor

a=alog10(ny(0:tot-1)) & b=nx(0:tot-1) & c=(nee(0:tot-1)/ny(0:tot-1))/alog(10)
;a=ny & b=nx & c=nee

free_lun,unit


while (((i=strpos(fil,'out'))) ne -1) do strput,fil,'aft',i


title='spanner'
nb=2.07
print,fil
openw,unit,fil,/get_lun
printf,unit,'"AFIT"'
printf,unit,'"'+title+'"'
printf,unit,format='(f4.2)',nb
printf,format='(i3)',unit,tot
for i=0,tot-2 do begin

 printf,unit,format='(g10.4,10x,g10.4,10x,g10.4)',nx(i),ny(i),nee(i)
endfor

close,unit
free_lun,unit

while (((i=strpos(fil,'aft'))) ne -1) do strput,fil,'dat',i
print,fil
openw,unit,fil,/get_lun
for i=0,tot-1 do begin
 printf,unit,nx(i),'	',ny(i),'	',nee(i)
endfor
close,unit
free_lun,unit

return
print,!stime
end
