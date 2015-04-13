pro vanarev,w7,x10,w10,e10,x12,w12,e12,x14,w14,e14,stop=stop ; 2theta scan, powder difractogram, efficiency, gain per box, box number
;result=w10,eff=w12,gain=w14,box=x14

take_datp,p7

;steps,start,stop,zero
steps=N_ELEMENTS(w7(0,*))
start=steps-1
IF NOT KEYWORD_SET(stop) THEN stop=N_ELEMENTS(w7(*,0))-1
zero=p7.x(0,0)

print,'start at ',zero, ' deg, cells ',start,' to ',stop

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
;**** start reference *******************
FOR j=0,steps-1 DO BEGIN
  w9(j)=w7(start-1,j)
  e9(j)=p7.e(start-1,j)
ENDFOR
FOR i=start,stop DO begin
  FOR j=0,steps-1 DO BEGIN
    w8(j)=w7(i,j)
    e8(j)=p7.e(i,j)
  ENDFOR
  ;**** relative efficiency *******************
  w12(i)=float(TOTAL(w8(0:steps-2)/w9(1:steps-1))) /(steps-1.)
  e12(i)=float(TOTAL((e8(0:steps-2)*w9(1:steps-1)+w8(0:steps-2)*w9(1:steps-1))/w9(1:steps-1)/w9(1:steps-1)))/(steps-1.)/((steps-1.)/sqrt(steps))
print,i,w12(i),e12(i)
  w8=w8/w12(i)
  e8=e8*w12(i)+w8*e12(i)/w12(i)/w12(i)
  w9=w8
  e9=e8
  w10(i-steps+1:i)=w10(i-steps+1:i)+w8(0:steps-1)
  e10(i-steps+1:i)=e10(i-steps+1:i)+e8(0:steps-1)
  w11(i-steps+1:i)=0.+1.
;plot,x10,w10,xrange=[start*.1,stop*.1]
endfor
w10=w10/w11
e10=e10/w11/(w11/(sqrt(w11)+1.))
w13=fltarr(50)
e13=fltarr(50)
for i=start,stop DO BEGIN
 w13(i/32)= w13(i/32)+w12(i)
 e13(i/32)= e13(i/32)+e12(i)
ENDFOR
w13=float(w13)/32.
e13=float(e13)/32./(32./sqrt(33.))
x14=fltarr(1600)
for i=start,stop DO BEGIN
  w14(i)=w13(i/32)
  x14(i)=float(i)/32.
  e14(i)=e13(i/32)
ENDFOR
w14=w14(start:stop-steps)
x14=x14(start:stop-steps)
e14=e14(start:stop-steps)
w10=w10(start:stop-steps)
e10=e10(start:stop-steps)
x10=x10(start:stop-steps)
w12=w12(start:stop-steps)
x12=x12(start:stop-steps)
e12=e12(start:stop-steps)
help,w10,e10
plot,x10,w10,xrange=[zero+start*.1,zero+stop*.1]

;twotheta=x10,error=e10,cell=x12
give_datp,p7
end
