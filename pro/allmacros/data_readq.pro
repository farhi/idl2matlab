pro data_readq,num,nn,d0,p0,dr,pr,neww1,qx,qz,res
close,3

par1 = fltarr(128)
par2 = fltarr(256)
txt=sindgen(34)
txt(*)='RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR'
txt1=txt(1)
path='/users/data/'
name=path+'00'+string(strtrim(num,2))

print,' '
print,' '
print,'opening... ',name
openr,3,name
print,'opened: ',name
readf,3,txt
print,txt(4)
print,txt(25)
readf,3,par1
readf,3,txt1
readf,3,txt1
print,txt1
readf,3,par2

print,'no of chans= ',par1(94),' chan width= ',par1(95),' tof delay= ',par1(96)
print,'x1= ',par1(97),' x2= ',par1(98),' y1= ',par1(99),' y2= ',par1(100)
print,'nx= ',par1(101),' ny= ',par1(102)
print,'chop 1 speed req= ',par2(40),' chop 1 phase req= ',par2(41)
print,'chop 2 speed req= ',par2(42),' chop 2 phase req= ',par2(43)
print,'chop 1 speed act= ',par2(44),' chop 1 phase act= ',par2(45)
print,'chop 2 speed act= ',par2(46),' chop 2 phase act= ',par2(47)

; useful chopper variables
openr=45.-(par2(43)-par2(41))
opena=45.-(par2(47)-par2(45))
period=60./par2(44)
dela=(285.-opena)/2.
delt=(dela/360.)*period
chopsam=4
tofd=chopsam+(par2(15)/1000.)
chanpa=(tofd/3956.)/(par1(95)*1e-6)
chanpam=(chopsam/3956.)/(par1(95)*1e-6)
delchan=delt/(par1(95)*1e-6)
delechan=par1(96)/par1(95)
print,'TOF distance    = ',tofd, ' period chans    =',period/(par1(95)*1e-6)
print,'channels/Ang    = ',chanpa,' chop del chans  = ',delchan
print,'chopper period  = ',period*1000.,' elec del chans  = ',delechan
print,'opening      req= ',openr,' opening      act= ',opena
print,'chop delay angle= ',dela,' chop delay  time= ',delt*1000.,' ms'
print,'     san= ',par2(2),' deg.','     dan= ',par2(16),' deg.'
san=par2(2)
dan=par2(16)
print,' 27A TOF        = ',27.*chanpa,' 4.5A TOF        = ',4.5*chanpa
time=par1(2)/10.
print,'run time= ',time,' s',' det= ',par2(15)
det=par2(15)
print,det
readf,3,txt1
readf,3,txt1
readf,3,txt1



readf,3,tot
tsize=long(par1(94))
xsize=long(par1(98)-par1(97)+1)
ysize=long(par1(100)-par1(99)+1)
dsize=xsize*ysize
if (tot ne (dsize*tsize+tsize)) then print,' Error in data array dimensions'
print,'tsize= ',tsize,' detector size= ',dsize
print,'xsize= ',xsize,' ysize= ',ysize,' tot1= ',dsize*tsize,' tot2= ',tot

w1=lonarr(ysize,xsize,tsize)
if (ysize eq 1) then begin
  w1=lonarr(xsize,tsize)
endif

mon=lonarr(tsize)
xy=lonarr(xsize,ysize)
xt=lonarr(xsize,tsize)
yt=lonarr(ysize,tsize)


c=1
readf,3,w1,mon
help,w1
if (ysize gt 1) then begin
  w1=total(w1,1)
endif

;print,'mon= ',mon

;make lambda array
yy=indgen(tsize)
;larr=(yy+delechan-delchan)/chanpa
larr=(yy)/chanpa
m=(yy+delechan-delchan)/chanpam



lfirst=(delchan-delechan)
print,'channel shift = ',lfirst

; loose nn channels at the beginning to avoid infinities in reciprocal space
help,w1
;nn=5
aa=shift(w1,1,-round(lfirst))

neww1=aa(*,nn:tsize-1)
newlarr=larr(nn:tsize-1)

q=fltarr(xsize,tsize-nn)
th=fltarr(xsize,tsize-nn)
qx=fltarr(xsize,tsize-nn)
qz=fltarr(xsize,tsize-nn)

 
w2=mon


close,3
print,'total counts in detector = ',total(w1),' (',total(w1)/time,')'


print,'total counts in monitor = ',total(w2),' (',total(w2)/time,')'

;  conversion to reciprocal space

dpr=180./!pi
pmin=25.
pmax=253.
pcen=132.73
;mmpp=250./(pmax-pmin)
mmpp=1.04
print,'pcen= ',pcen,' mmpp= ',mmpp
print,'ref th= ',(dr+dpr*atan((pcen-pr)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
lmax=max(newlarr)
lmin=min(newlarr)

;expt information d0=dan for diect beam p0=pixel for direct beam
;dr=dan for reflection pr=pixel for reflection



;find limits in reciprocal space

for i=pmin,pmax do begin
  for j=0,tsize-nn-1 do begin
  
   l=newlarr(j)
   th(i,j)=(dr+dpr*atan((pcen-i)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
   q(i,j)=4*!pi*sin(th(i,j)/dpr)/newlarr(j)
   qx(i,j)=q(i,j)*sin((th(i,j)-san)/dpr)
   qz(i,j)=q(i,j)*cos((th(i,j)-san)/dpr)
   ; print,'i= ',i,' j= ',j,' q= ',q(i,j),' th= ',th(i,j),' lam= ',newlarr(j)
   ; print,'qx= ',qx(i,j),' qz= ',qz(i,j)
   
   endfor
 endfor

   
  
   print,'by array sums....'
   print,'thmax= ',max(th),'thmin= ',min(th),'qmin= ',min(q),'qmax= ',max(q)
   print,'qxmax= ',max(qx),'qxmin= ',min(qx),'qzmax= ',max(qz),'qzmin= ',min(qz)


  
triangulate,qx,qz,triangles
res=trigrid(qx,qz,neww1,triangles)
 

return

end
