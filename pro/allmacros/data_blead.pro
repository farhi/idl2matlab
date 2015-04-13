pro data_blead,num,w1,w2,y,m,q,th,san,dan,dett,nx,time
close,3

par1 = fltarr(128)
par2 = fltarr(256)
txt=sindgen(34)
txt(*)='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
txt1=txt(1)
path='/users/data/'

if strlen(string(strtrim(num,2))) eq 1 then name=path+'00000'+string(strtrim(num,2))
if strlen(string(strtrim(num,2))) eq 2 then name=path+'0000'+string(strtrim(num,2))
if strlen(string(strtrim(num,2))) eq 3 then name=path+'000'+string(strtrim(num,2))
if strlen(string(strtrim(num,2))) eq 4 then name=path+'00'+string(strtrim(num,2))
if strlen(string(strtrim(num,2))) eq 5 then name=path+'0'+string(strtrim(num,2))

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
readf,3,par2

;  

dpr=180./!pi
print,'no of chans= ',par1(94),' chan width= ',par1(95),' tof delay= ',par1(96)
print,'x1= ',par1(97),' x2= ',par1(98),' y1= ',par1(99),' y2= ',par1(100)
nx=par1(101)


print,'nx= ',par1(101),' ny= ',par1(102)
print,'chop 1 speed req= ',par2(40),' chop 1 phase req= ',par2(41)
print,'chop 2 speed req= ',par2(42),' chop 2 phase req= ',par2(43)
print,'chop 1 speed act= ',par2(44),' chop 1 phase act= ',par2(45)
print,'chop 2 speed act= ',par2(46),' chop 2 phase act= ',par2(47)

; useful chopper variables
openr=45.-(par2(43)-par2(41))
opena=45.-(par2(47)-par2(45))
period=60./par2(44)

opena=opena-1.02

dela=(283.754-opena)/2.
delt=(dela/360.)*period
chopsam=3.9475-(85.0e-3)/2
chopmon=.455
tofd=chopsam+(par2(15)/1000.)
chanpa=(tofd/3956.)/(par1(95)*1e-6)
chanpam=(chopmon/3956.)/(par1(95)*1e-6)
r=chopsam/tofd
delchan=delt/(par1(95)*1e-6)
delechan=par1(96)/par1(95)
print,'TOF distance    = ',tofd, ' period chans    =',period/(par1(95)*1e-6)
print,'channels/Ang    = ',chanpa,' chop del chans  = ',delchan
print,'chopper period  = ',period*1000.,' elec del chans  = ',delechan
print,'opening      req= ',openr,' opening      act= ',opena
print,'chop delay angle= ',dela,' chop delay  time= ',delt*1000.,' ms'
;print,'     san= ',par2(2),' deg.'
print,' 27A TOF        = ',27.*chanpa,' 4.5A TOF        = ',4.5*chanpa

time=par1(2)/10.
print,'run time= ',time,' s',' det= ',par2(15),'dan = ',par2(16)
dan=par2(16)
san=par2(2)
dett=par2(15)
print,par1(90:100)
tsize=long(par1(94))
xsize=long(par1(98)-par1(97)+1)
ysize=long(par1(100)-par1(99)+1)
readf,3,txt1
readf,3,txt1
readf,3,txt1
readf,3,tot
print,tot


; define the data array
;tsize=long(par1(100)-par1(99)+1)
dsize=xsize*ysize
if (tot ne (dsize*tsize+tsize)) then print,' Error in data array dimensions'
print,'tsize= ',tsize,' detector size= ',dsize
print,'xsize= ',xsize,' ysize= ',ysize,' tot1= ',dsize*tsize,' tot2= ',tot

det=lonarr(ysize,xsize,tsize)
if (ysize eq 1) then begin
  det=lonarr(xsize,tsize)
  
endif
if (tsize eq 1) then begin
 det=lonarr(ysize,xsize)
endif


c=1

;  read detector and monitor data
;readf,3,det,mon

;bum monitor option
readf,3,det
print,'setting this value to zero',det(ysize-1,xsize-1)
det(ysize-1,xsize-1)=0

;print,'mon= ',mon

;make lambda array (y)

 yy=indgen(tsize)
 y=(yy+delechan-delchan+0.5)/chanpa
 m=(yy+delechan-delchan+0.5)/chanpam

if tsize gt 1 then begin
  w1=det
endif else begin
  w1=transpose(det)
endelse

w2=par1(4)


dpr=180./!pi
p0=135.79/nx

mmpp=1.0357*nx
th=fltarr(xsize)

; calculate th array
for i=0,xsize-1 do th(i)=dan+dpr*atan(((p0-i)*mmpp)/dett)



; and Q
q=4*!pi*sin(par2(2)/dpr)/y

close,3
print,'total counts in detector = ',total(w1),' (',total(w1)/time,')'
;print,'total counts in monitor = ',total(w2),' (',total(w2)/time,')'
return

end
