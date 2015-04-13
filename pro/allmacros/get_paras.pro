pro get_paras,num,y,m,q,th,r
close,3

par1 = fltarr(128)
par2 = fltarr(256)
txt=sindgen(34)
txt(*)='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
txt1=txt(1)
path='/users/data/'
;name=path+'00'+string(strtrim(num,2))
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

; note offset for opening is 1.1 deg!!!!!!!


opena=opena-1.02

; inter chopper distance is cht
cht=0.085
period=60./par2(44)




dela=(283.754-opena)/2.
delt=(dela/360.)*period


chopsam=3.9475-cht/2.

tofd=chopsam+(par2(15)/1000.)
chanpa=(tofd/3956.)/(par1(95)*1e-6)
chanpam=(chopsam/3956.)/(par1(95)*1e-6)
r=chopsam/tofd
delchan=delt/(par1(95)*1e-6)
delechan=par1(96)/par1(95)
print,'channel offset: ',delchan-delechan
print,'TOF distance    = ',tofd, ' period chans    =',period/(par1(95)*1e-6)
print,'channels/Ang    = ',chanpa,' chop del chans  = ',delchan
print,'chopper period  = ',period*1000.,' elec del chans  = ',delechan
print,'opening      req= ',openr,' opening      act= ',opena
print,'chop delay angle= ',dela,' chop delay  time= ',delt*1000.,' ms'
;print,'     san= ',par2(2),' deg.'
print,' 27A TOF        = ',27.*chanpa,' 4.5A TOF        = ',4.5*chanpa
time=par1(2)/10.
print,'run time= ',time,' s',' det= ',par2(15),'dan = ',par2(16)
print,'san= ',par2(2)
dan=par2(16)
dett=par2(15)
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

;make lambda array
yy=indgen(tsize)
y=(yy+delechan+.5-delchan)/chanpa
m=(yy+delechan+.5-delchan)/chanpam


;make 2th array

dpr=180./!pi
p0=135.79/nx

mmpp=1.04*nx
th=fltarr(xsize)

for i=0,xsize-1 do th(i)=dan+dpr*atan(((p0-i)*mmpp)/dett)




q=4*!pi*sin(par2(2)/dpr)/y

return
end
