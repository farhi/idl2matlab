PRO florin,n1,n2,n3,w11,x11,y11,e11

saml=10.
beam=0.7

xpixels=286
mmpp=1.04
dpr=180./!pi
pcen=137.6
pp0=142.3
d0=0
points=n2-n1+1
san=fltarr(points)
dan=fltarr(points)
w10=fltarr(xpixels,points)
e10=fltarr(xpixels,points)
x10=fltarr(xpixels,points)
y10=fltarr(xpixels,points)
water=fltarr(xpixels)
close,3


;  open water correction file created by awater.pro
openr,3,'water_LAMPascii'

for  i=0,xpixels-1 do begin
  readf,3,a,b,c
  water(i)=b
endfor

; open background file read into wb
if n3 ne 0 then begin
  data_read,n3,wb,w2,y,m,q,th,s,d,dett,nx,timeb
endif else begin
  wb=0.
  timeb=1.
endelse


for j=0,points-1 do begin
  num=j+n1
  data_read,num,w1,w2,y,m,q,th,s,d,dett,nx,time
  san(j)=s
  dan(j)=d
  foot=saml*sin(s/dpr)
  w10(*,j)=(w1/(time*water))-wb/(timeb*water)
  e10(*,j)= sqrt(w1/(time*water)^2.+  wb/(timeb*water)^2.)


  x10(*,j)=(d+dpr*atan((pcen-indgen(xpixels))*mmpp/dett))-(d0+dpr*atan((pcen-pp0)*mmpp/dett))-s
endfor

print,'det dist=',dett
print,'time= ',time

for i=0,points-1 do begin
  for j=0,285 do begin
   y10(j,i)=san(i)
  endfor
endfor
close,3


sizex=300
sizey=100

gs=[(max(x10)-min(x10))/sizex,(max(y10)-min(y10))/sizey]


triangulate,x10,y10,triangles
x11=indgen(sizex+1)*(max(x10)-min(x10))/sizex+min(x10)
y11=indgen(sizey+1)*(max(y10)-min(y10))/sizey+min(y10)
w11=trigrid(x10,y10,w10,triangles,gs)
e11=trigrid(x10,y10,e10,triangles,gs)




end
