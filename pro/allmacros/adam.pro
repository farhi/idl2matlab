pro adam,n1,n2,cen,sbox,back,spin,fac

; n1-n2 are the scan numbers
; cen is the centre of the region to be summed in x-pixel
; sbox is the number of pixels to the left and right included in the sum region
; i.e if cen=119 and sbox=2 and back=3 we sum from 117 to 121 and the background
; is 122 to 124
; spin is either 'u' or 'd'
; fac is just a normalization constant

wav=4.4063

close,1
close,2
close,10
i=0
fsize=intarr(n2-n1+1)

for f=n1,n2 do begin
  text=strtrim(string(f),2)
  name2='lee_align_1D_'+text+'_'+spin+'.par'
  openr,2,name2
  print,'opening ',name2
  dum='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

   while not EOF(2) do begin
     readf,2,dum
     print,dum
     i=i+1
   endwhile

  print,'number of points for ',f,'= ',i
  fsize(f-n1)=i
  close,2
  i=0
endfor

print,'total number of all points = ',total(fsize)

data=fltarr(4,total(fsize))
all=fltarr(256,total(fsize))
count=0
for f=n1,n2 do begin

  text=strtrim(string(f),2)
  name2='lee_align_1D_'+text+'_'+spin+'.par'
  name1='lee_align_1D_'+text+'_'+spin

  openr,1,name1
  openr,2,name2

  dat=intarr(256,fsize(f-n1))
  dat2=intarr(256,fsize(f-n1))
  par=fltarr(7,fsize(f-n1))
  
  readf,1,dat
  readf,2,par
  left=cen-sbox
  right=cen+sbox
  leftb=right+1
  rightb=leftb+back-1
  sum=total(dat(left:right,*),1)
  sumerr=sqrt(sum)

  if (back gt 0) then begin
    sumb=total(dat(leftb:rightb,*),1)
    sumberr=sqrt(sumb)
  endif else begin
    sumb=0
    sumberr=0
  endelse

  sumberr=sqrt(sumb)
  ssum=sum-sumb
  ssumerr=sqrt(sum+sumb)
  ferr=ssumerr/ssum

  r=ssum/(par(3,*)*sin(par(0,*)*!pi/180.)*fac)

  for k=0,fsize(f-n1)-1 do begin
    for l=0,255 do begin
      dat2(l,k)=dat(l,k)/(par(2,k)*sin(par(0,k)*!pi/180.)*fac)
    endfor
  endfor

  rerr=r*ferr
  q=sin(par(0,*)*!pi/180.)*4*!pi/wav
  start=total(fsize(0:(f-n1)))-fsize(f-n1)
  finish=total(fsize(0:(f-n1)))-1
  data(0,start:finish)=q
  data(1,start:finish)=r
  data(2,start:finish)=rerr
  data(3,start:finish)=q*0.04
  all(0:255,start:finish)=dat2
  close,1
  close,2
endfor

window,0,retain=2,xsize=470,ysize=500
plot,/ylog,data(0,*),data(1,*),xtitle='q(A^-1)',ytitle='Reflectivity',psym=7
oploterr,data(0,*),data(1,*),data(2,*)

window,1,retain=2,xsize=470,ysize=500
plot,data(0,*),data(1,*)*data(0,*)^4,xtitle='q(A^-1)',ytitle='Reflectivity',psym=7
oploterr,data(0,*),data(1,*),data(2,*)




all=all(112:126,*)
window,2,retain=2,xsize=470,ysize=500
n=30
m1=max(all)
m2=min(all)
ll=fltarr(n+1)

for i=0,n do begin 
	ll(i)=m2+float(i)*(((m1-m2))/float(n+1))
endfor

print,m2,m1
print,ll
loadct,5
gamma_ct,0.302
contour,all,levels=ll,c_colors=indgen(10)*25
print,data
oname='scan'+strtrim(string(n1),2)+'-'+strtrim(string(n2),2)+'-'+spin+'.dat'
print,'saving data in file: ',oname

print,'opening file'
openw,10,oname
printf,10,'box = ',left,right,'   back = ',leftb,rightb,'   fac = ',fac
;writeu,10,'fart '
printf,10,data,format='(f10.8,2x,f10.8,2x,f10.8,2x,f10.8)'

close,10
end
