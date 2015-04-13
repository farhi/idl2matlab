PRO anth2th,nu1,nu2,w10,x10,e10


f1=210 & f2=230 & b1=190 & b2=209
lam=5.97
saml=10.
beam=0.7
p0=199.105
d0=0.9904


xpixels=286
mmpp=1.04
dpr=180./!pi
pcen=136.94
pp0=142.3
;d0=0
points=((nu2-nu1+1))
print,points,nu1,nu2
san=fltarr(points)
dan=fltarr(points)
th=fltarr(points)
w10=fltarr(points)
e10=fltarr(points)
x10=fltarr(points)
water=fltarr(xpixels)
close,3


;  open water correction file created by awater.pro
openr,3,'water_LAMPascii'

for  i=0,xpixels-1 do begin
  readf,3,a,b,c
  water(i)=b
endfor


for k=0,points-1 do begin
  num=k+nu1

   
  data_read,num,w1,w2,y,m,q,t,s,d,dett,nx,time
  san(k)=s
  dan(k)=d
  foot=saml*sin(s/dpr)
  w10(k)=total(w1(f1:f2)/(time*water(f1:f2)))-total(w1(b1:b2)*(f2-f1+1)/((b2-b1+1)*time*water(b1:b2)))
  e10(k)= sqrt(total(w1(f1:f2))/(time^2.)+ total(w1(b1:b2))*(f2-f1+1)^2/((b2-b1+1)^2*time^2.))

;find peak
  
numersum=0.
denomsum=0.
for i=f1,f2 do begin
	numer=w1(i)*float(i)
	denom=w1(i)
	numersum=numer+numersum
	denomsum=denom+denomsum
endfor
;print,'db stuff:', numersum,denomsum
pr=numersum/denomsum
;print,'fitted ref peak at: ',(pr)


th(k)=(d+dpr*atan((pcen-pr)*mmpp/dett))/2.-(d0+dpr*atan((pcen-p0)*mmpp/dett))/2.
;print,'calculated theta= ',th

;calculate q from the calculated theta
  x10(k)=4*!pi*sin(th(k)/dpr)/lam
endfor
print,th
help,th
print,'det dist=',dett
print,'time= ',time

end
