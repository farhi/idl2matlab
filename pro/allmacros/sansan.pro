pro sansan

; program to set san=theta without using the dan coder

calsan

; that should get it to within 0.02 degrees

dbdan=1.
sanmax=0.7
sanstep=0.05
sanmin=0.1
count=10
mmpp=1.0213
dpr=180./!pi
pcen=139.98
range=5

mv,'dan',dbdan
mv,'dan',dbdan
mv,'san',0
mv,'trs',5

co,count,'t n'

   m=DIAL_MAD_READ('motors')
   d0=m(16)
   det=m(15)
   print,'dan error was dan-true=',dbdan-d0
   d17data,data
   s=size(data)
   if s(0) gt 1 then data=total(data,2)
   d=max(data,p0)

   print,'direct beam pixel maximum:',p0
   
   yy=gaussfit(findgen(286),data,a)
   print,'gausfit db: ',a(1)
   pp0=a(1)
  

; scan san 

mv,'trs',-0.5
mv,'trs',0.
points=floor(((sanmax-sanmin)/sanstep)+1)

sant=fltarr(points)
ppr=fltarr(points)

for i=0,points-1 do begin
  san=sanmax-float(i)*sanstep
  mv,'san',san
  m=DIAL_MAD_READ('motors')
  print,'moved san to ',m(2), (m(2)-san)
  prc=pp0-(2.*det*m(2))/(dpr*mmpp)
  ph=pp0-(det*m(2))/(dpr*mmpp)
  print,'expected reflection pixel: ',prc

  
  aa=total(fix(prc)-range)
  ab=total(fix(prc)+range)
  
  print,'will search between: ',aa,ab
  co,count,'t n'
   d17data,data
   s=size(data)
   if s(0) gt 1 then data=total(data,2)

   d=max(data(aa:ab),pr)

   pr=pr+aa
  
   print,'reflection pixel maximum:',pr

   yy=gaussfit((findgen(ab-aa+1)+float(aa)),data(aa:ab),a)
   print,'gaussfit centre: ',a(1)
   ppr(i)=a(1)
    
   print,'reflection pixel fit:',ppr(i)
   sant(i)=m(2)
endfor

ppr=pp0-ppr


z=poly_fit(ppr,sant,1,yout,yband,sigma,corr)

plot,ppr,sant,psym=2,xtitle='pixels',ytitle='san',$
yrange=[sanmin-0.1,sanmax+0.1]
xout=sant
oplot,ppr,yout


print,'calculated offset:  ',z(0),mean(yband)
print,'calculated gradient:',z(1),sigma/z(1)
mmppc=(z(1)*det/dpr)*2.0
print,'calculated mmpp based on this gradient:',mmppc
print,'present mmpp= ',mmpp
;print,'percentage difference: ', ((mmppc-mmpp)*100.)/mmpp

;print,'sigma',sigma

;print,'yerrors on fit',yband

mv,'san',z(0)
mv,'san',z(0)

d17mad,'par set san 0'

end






