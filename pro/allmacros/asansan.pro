pro asansan,w1,x1

; program to set san=theta without using the dan coder

;calsan

; that should get it to within 0.02 degrees

dbdan=1.
danmax=1.5
danstep=0.1
danmin=-1.5
count=10
mmpp=1.0213
dpr=180./!pi
pcen=139.98
range=5


mv,'san',0
mv,'trs',5

  

; scan dan


mv,'san',0.
points=floor(((danmax-danmin)/danstep)+1)

dant=fltarr(points)
ppr=fltarr(points)

for i=0,points-1 do begin
  dan=danmin+float(i)*danstep
  mv,'dan',dan
  m=DIAL_MAD_READ('motors')
  det=m(15)
  print,'moved dan to ',m(16), (m(16)-dan)
  prc=pcen+(det*m(16))/(dpr*mmpp)
  
  print,'expected reflection pixel: ',prc

 
  co,count,'t n'
   d17data,data
   s=size(data)
   if s(0) gt 1 then data=total(data,2)

   d=max(data,pr)
  
   print,'db pixel maximum:',pr

   yy=gaussfit(findgen(s(1)),data,a)
   print,'gaussfit centre: ',a(1)
   ppr(i)=a(1)
    
   print,'db pixel fit:',ppr(i)
   dant(i)=m(16)
endfor



z=poly_fit(tan(dant/dpr),ppr,1,yout,yband,sigma,corr)

plot,tan(dant/dpr),ppr,psym=2,xtitle='tan(dan)',ytitle='pixel fit',$
yrange=[min(ppr)-1,max(ppr)+1]
xout=dant
oplot,tan(dant/dpr),yout

w1=yout-ppr
x1=tan(dant/dpr)
print,'calculated pcen:  ',z(0),mean(yband)
print,'calculated gradient:',z(1)
mmppc=(det/z(1))
print,'calculated mmpp based on this gradient:',mmppc
print,'present mmpp= ',mmpp
;print,'percentage difference: ', ((mmppc-mmpp)*100.)/mmpp

;print,'sigma',sigma

;print,'yerrors on fit',yband

end






