pro vana,w7,steps,start,stop,zero=zero,twotheta=x10,result=w10,error=e10,eff=w12,cell=x12,gain=w14,box=x14

steps=float(steps)
w8=fltarr(steps)
e8=fltarr(steps)
w9=fltarr(steps)
w10=fltarr(1600)*0.
e10=fltarr(1600)*0.
w11=fltarr(1600)*0.
w12=fltarr(1600)*0.
w14=fltarr(1600)*0.
x10=findgen(1600)*0.1+zero
x12=indgen(1600)
for j=0,steps-1 do w9(j)=w7(start-1,j)
for i=start,stop do begin
  for j=0,steps-1 do w8(j)=w7(i,j)
  e8=w8/sqrt(1.+w8)
  w12(i)=float(total(w8(0:steps-2)/w9(1:steps-1)))/(steps-1.)
  ;print,i,w12(i)
  w8=w8/w12(i)
  e8=e8/w12(i)
  w9=w8
  w10(i-steps+1:i)=w10(i-steps+1:i)+w8(0:steps-1)
  e10(i-steps+1:i)=e10(i-steps+1:i)+e8(0:steps-1)
  w11(i-steps+1:i)=0.+1.
  ;plot,x10,w10,xrange=[start*.1,stop*.1]
endfor
w10=w10/w11
e10=e10/w11
w13=fltarr(50)
for i=start,stop do w13(i/32)= w13(i/32)+w12(i)
w13=float(w13)/32.
x14=fltarr(1600)
for i=start,stop do w14(i)=w13(i/32)
for i=start,stop do x14(i)=float(i)/32.
w14=w14(start:stop)
x14=x14(start:stop)
w10=w10(start:stop)
e10=e10(start:stop)
x10=x10(start:stop)
w12=w12(start:stop)
x12=x12(start:stop)
plot,x10,w10,xrange=[zero+start*.1,zero+stop*.1]
end
