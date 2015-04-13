PRO anf10scan,nu1,nu2,w10,x10,e10,w11,x11,e11,w12,x12,e12,w13,x13,e13

; f1-f2 is the foreground range and b1-b2 is background range
; back=0 means no background is taken

f1=175 & f2=215 & b1=160 & b2=165
lam=5.3

back=1

;normalize by time or monitor  for monitor set norm='monitor'
norm='time'

;for det=3400mm

; direct beam dan value is d0
d0=0.922
;direct beam hits pixel p0
p0=193.862


xpixels=286
mmpp=1.0357
dpr=180./!pi
pcen=138.98

points=(nu2-nu1+1)
hpoints=fix((float(nu2-nu1)+1.5)/2.)

print,points,hpoints,nu1,nu2

w10=fltarr(hpoints)
e10=fltarr(hpoints)
x10=fltarr(hpoints)
w11=fltarr(hpoints)
e11=fltarr(hpoints)
x11=fltarr(hpoints)

w12=fltarr(hpoints)
e12=fltarr(hpoints)
x12=fltarr(hpoints)





water=fltarr(xpixels)
close,3


;  open water correction file created by awater.pro
openr,3,'water_LAMPascii'

for  i=0,xpixels-1 do begin
  readf,3,a,b,c
  water(i)=b
endfor

k=0
for l=0,points-1,2 do begin
  num=l+nu1
  data_read,num,w1,w2,y,m,q,th,s,d,dett,nx,time
  if norm eq 'time' then norm_fac=time
  if norm eq 'monitor' then norm_fac=w2/1000.
  
if back eq 1 then begin

  w10(k)=total(w1(f1:f2)/(norm_fac*water(f1:f2)))-total(w1(b1:b2)*(f2-f1+1)/((b2-b1+1)*norm_fac*water(b1:b2)))
  e10(k)= sqrt(total(w1(f1:f2))/(norm_fac^2.)+ total(w1(b1:b2))*(f2-f1+1)^2/((b2-b1+1)^2*norm_fac^2.))

endif else begin

  w10(k)=total(w1(f1:f2)/(norm_fac*water(f1:f2)))
  e10(k)= sqrt(total(w1(f1:f2)))/norm_fac

endelse
;find peak
numersum=0.
denomsum=0.
for i=f1,f2 do begin
     numer=w1(i)*float(i)
     denom=w1(i)
     numersum=numer+numersum
     denomsum=denom+denomsum
endfor

pr=numersum/denomsum

th=(d+dpr*atan((pcen-pr)*mmpp/dett))/2-(d0+dpr*atan((pcen-p0)*mmpp/dett))/2
print,'calculated theta= ',th

;calculate q from the calculated theta
  x10(k)=4*!pi*sin(th/dpr)/lam
k=k+1
endfor

k=0
for l=1,points-1,2 do begin
  num=l+nu1
  data_read,num,w1,w2,y,m,q,th,s,d,dett,nx,time
  if norm eq 'time' then norm_fac=time
  if norm eq 'monitor' then norm_fac=w2/1000.
if back eq 1 then begin

  w11(k)=total(w1(f1:f2)/(norm_fac*water(f1:f2)))-total(w1(b1:b2)*(f2-f1+1)/((b2-b1+1)*norm_fac*water(b1:b2)))
  e11(k)= sqrt(total(w1(f1:f2))/(norm_fac^2.)+ total(w1(b1:b2))*(f2-f1+1)^2/((b2-b1+1)^2*norm_fac^2.))

endif else begin

  w11(k)=total(w1(f1:f2)/(norm_fac*water(f1:f2)))
  e11(k)= sqrt(total(w1(f1:f2)))/norm_fac

endelse




;find peak
numersum=0.
denomsum=0.
for i=f1,f2 do begin
     numer=w1(i)*float(i)
     denom=w1(i)
     numersum=numer+numersum
     denomsum=denom+denomsum
endfor

pr=numersum/denomsum

th=(d+dpr*atan((pcen-pr)*mmpp/dett))/2-(d0+dpr*atan((pcen-p0)*mmpp/dett))/2
print,'calculated theta= ',th

;calculate q from the calculated theta
  x11(k)=4*!pi*sin(th/dpr)/lam
k=k+1
endfor

;illumination normalisation

ef10=e10/w10
ef11=e11/w11
w10=w10*x10(0)/(w10(0)*x10)
e10=w10*ef10
w11=w11*x10(0)/(w11(0)*x11)
e11=w11*ef11

w12=(w10-w11)/(w11+w10)
x12=x10
e12=w12*sqrt((sqrt(e11^2+e10^2)/(w10-w11))^2+(sqrt(e11^2+e10^2)/(w11+w10))^2)

w13=w10/w11
e13=w13*sqrt((e10/w10)^2+(e11/w11)^2)
x13=x10
print,points,hpoints,nu1,nu2

end
