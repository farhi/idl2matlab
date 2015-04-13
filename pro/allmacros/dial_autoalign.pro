;pro dial_autoalign,len,san,scantrs,scandata

;pro err_event
;  if ev.select then widget_control, ev.tp,/DESTROY
;end

;pro message,tit
; base=widget_base()
; button=widget_button(base,value='DONE',title=tit)
; widget_control,base,/realize
; xmanager,'message',base
;end


;*************************************************************************


pro dial_autoalign,len,san,scantrs,scandata
print,'Starting autoalign....'
prin=0
dpr=180./!pi

;message,'Please make sure SAN~theta and sample is aligned with TRS'

foot=len*sin(san/dpr)
count='10'
rdan=2.3
dbdan=0.8
dbtrs=3.
range=2.5
points=25.
shif=2
nx=1.
pcen=138.98/nx
mmpp=1.0357*nx 
scandata=fltarr(points)
scantrs =fltarr(points)

step=range/points
print,'range: ',range,' points: ',points,'footprint: ',foot

print,'Defining SAN and TRS to be zero'
d17mad,'par set trs 0'
d17mad,'par set san 0'


mv,'s2w',0
mv,'s3w',0

mv,'dan',rdan

; set sample slit equal to the footprint s2w needs to be minimum 0.5

mv,'s2w',0.5
mv,'s3w',foot


; set san equal to requested value
mv,'san',san
wait,1
co,count,'t n'
co,count,'t n'
d=dial_mad_read('t_res')
size=d.par_tof(0)*(d.par_tof(4)-d.par_tof(3)+1)*(d.par_tof(2)-d.par_tof(1)+1)
print,'size= ',d.par_tof(0),(d.par_tof(4)-d.par_tof(3)+1),(d.par_tof(2)-d.par_tof(1)+1)

d17mad,'kill'
;  scan the translation trs
   for i=0,points-1 do scandata(i)=0.
   for i=0,points-1 do scantrs(i)=0.


for i=0,points-1 do begin

   trs=-range/2.+i*step
   mv,'trs',trs
   co,count,'t n'
   dat=dial_mad_read('data')
   scandata(i)=total(dat(0:size-1))
   wait,1
   m=DIAL_MAD_READ('motors')
   scantrs(i)=m(1)
endfor


for i=0,points-1 do print,i,scantrs(i),scandata(i)
dialwset
plot,scantrs,scandata
errplot,scantrs,scandata-sqrt(scandata),scandata+sqrt(scandata)

; find the maximum of the peak

  maxtot=max(scandata(1:points-1),a)
  print,'max is: ',maxtot
  maxtrs=scantrs(a+1)
  print,'at: ',maxtrs
;  find half footprint from the peak


  a1=(a+1)-shif
  a2=(a+1)+shif
  print,'finding centre between trs values: ',scantrs(a+1-shif),' and ',scantrs(a+1+shif) 

;  m1=maxtrs-foot/2.
;  m2=maxtrs+foot/2.
;  print,'finding centre between trs values: ',scantrs(a+1-shif),' and ',scantrs(a+1+shif) 
;  blap=where ((scantrs(1:points-1) gt m1 and scantrs(1:points-1) lt m2))

;  a1=min(blap)
;  a2=max(blap)



  sum1=0
  sum2=0
  for i=a1,a2 do begin
     sum1=sum1+scantrs(i)*scandata(i)
     sum2=sum2+scandata(i)
  endfor
  print,sum1,sum2
  centrs=sum1/sum2


  print,'centre of scan found at trs: ',centrs


;  move to centre and set value to zero

   mv,'trs',centrs-.5
   mv,'trs',centrs
  

   d17mad,'par set trs 0'
 
   mv,'san',san
   calsan

   mv,'dan',rdan
   mv,'trs',-0.5
   mv,'trs',0
   mv,'san',0


   m=DIAL_MAD_READ('motors')
   print,' '
   print,'SAN is now equal to theta and is at ',m(2),' deg.'
   print,'Detector angle DAN is now at ',m(16),' deg.'
   print,'Sample translation TRS is centred at zero and is at',m(1),' mm'
   print,'  '
   print,'You are now ready to start a measurement'
  
end
