pro refract,n1,x0,xr,x1,x2,y1,y2,y,nb

tr=fltarr(500)
lam=fltarr(500)
th2=fltarr(500)
nn=fltarr(500)
nb=fltarr(500)



dpr=180./!pi


count=30
rdan=2.3

dbtrs=5
range=2.
points=15.
shif=3
nx=1.
pcen=135.79/nx
mmpp=1.04*nx 

data_read,n1,w1,w2,y,m,q,th,san,dan,dett,nx,time

refarr=total(w1(xr-shif:xr+shif),1)
th1=san

   k=0.
   kk=0.
   print,'ref pixel maximum:',xr
   for i=xr-shif,xr+shif do begin
    k=k+total(w1(i,*),2)
    kk=kk+total(w1(i,*),2)*float(i)
   endfor

   ppr=kk/k

   print,'ref beam pixel fit:',ppr


thx=(2.*tan(san/dpr))*dett/mmpp
th1=180.*(atan((x0-xr)/dett))/(2.*!pi)
x0c=ppr+thx
print,'san was: ',san,dett,'cal th= ',th1
print,'direct beam calculated at pixel: ',x0c
print,'x0: ',x0

print,x1,x2,y1,y2
for j=0,y2-y1 do begin
   ypix=j+y1
   mm=max(w1(x1:x2,ypix:ypix),b)
   b=b+x1
   k=0.
   kk=0.
   print,'lam pixmax:',y(ypix),b
   for i=b-shif,b+shif do begin
    k=k+w1(i,ypix:ypix)
    kk=kk+w1(i,ypix:ypix)*float(i)
   endfor
   
   tr(j)=(kk/k)
   lam(j)=y(ypix)
   th2(j)=th1-dpr*atan((x0-tr(j))/(dett*mmpp))
   nn(j)=cos(th1/dpr)/cos(th2(j)/dpr)
   nb(j)=(1-nn(j))*2.*!pi/y(ypix)^2
   endfor

   for m=0,y2-y1 do begin
    print,y(m),tr(m),th2(m),nn(m),nb(m)
   endfor
   result=moment(nb(0:y2-y1))
   print,'mean  nb: ',mean(nb(0:y2-y1)),' +/- ',sqrt(result(1))
end
