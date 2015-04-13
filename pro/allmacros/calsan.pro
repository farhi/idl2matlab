pro calsan
print,'Starting calibration of san....'
prin=0
dpr=180./!pi


count=10
;rdan=2.3

dbtrs=5
range=2.
points=15.
shif=2
nx=1.
pcen=138.98/nx
mmpp=1.0357*nx 


;  use existing value of san for the calibration unless it is zero

   m=DIAL_MAD_READ('motors')
   if (m(2) lt 0.4) then begin
     san=0.4
   endif else begin
     san=m(2)
   endelse
   det=m(15)
   rdan=2.5*3400./det
   dbdan=rdan-2.*san   

   print,'Taking a run with the direct beam'

   mv,'dan',dbdan
   mv,'dan',dbdan
   mv,'trs',dbtrs
   mv,'san',0

   co,count,'t n'
   
   m=DIAL_MAD_READ('motors')
   d0=m(16)
   print,'dan error was dan-true=',dbdan-d0
   d17data,data
   s=size(data)
   if s(0) gt 1 then data=total(data,2)
   d=max(data,p0)


   yy=gaussfit(findgen(s(1)),data,a)
   pp0=a(1)
   print,'direct beam pixel fit:',pp0
   
   
;   take a run at the peak and calculate th

   print,'Taking a run with a reflection'

   mv,'trs',-.5
   mv,'trs',0.
   mv,'san ',0.0
   mv,'san',san
   mv,'dan',rdan
   mv,'dan',rdan
   co,count,'t n'
   m=DIAL_MAD_READ('motors')
   wait,.2
   dr=m(16)
   print,'dan error was dan-true=',rdan-dr
   d17data,data
   s=size(data)
   if s(0) gt 1 then data=total(data,2)
   d=max(data,pr)

   yy=gaussfit(findgen(s(1)),data,a)
   ppr=a(1)
   print,'reflection pixel maximum fit:',ppr
   print,'reflection pixel gauss fit:',a(1)


   ; now calculate th
   
   det=m(15)
   th=(dr+dpr*atan((pcen-ppr)*mmpp/det))/2-(d0+dpr*atan((pcen-pp0)*mmpp/det))/2

   print,'found theta is: ',th,' deg.'
   
   sanstr=strtrim(string(th),2)
   d17mad,'par set san '+sanstr
   

   print,'..and set san to that value'
   print,' your error (san-th) was :',san-th,' deg.'

;put motors back for reflection condition

   mv,'dan',rdan
   mv,'trs',-0.5
   mv,'trs',0
   mv,'san',san

end
