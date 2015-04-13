pro anal,db,ref,fac,water,q,r,bg,w5,x5,e5,chop
;help,db,ref,water


; db is the direct beam run
; ref is the reflection run
; water is the x effiency of the detector
; q is the array of q(A^-1)
; r is the range over which to sum the intensity
; bg is the range to sum the background either side of r
; w5 is the output reflectivity with the q (x5) and error (e4)
; fac is the normalisation factor for w2 reflection data ie
; divide the ref data by this to make it normalised to the direct beam




ref1=ref
ref2=ref
db1=db
db2=db
edb=db
ere=ref

z=size(db)
print,z(2)

a=max(total(db,2),b)
f1=b-(r-1)/2
f2=b+(r-1)/2
print,'db peak at: ',b
for i=0,z(2)-1 do db1(*,i)=db(*,i)/water
print,'done water correction'

; subtract background from direct beam
if (bg gt 0) then begin
 for i=0,z(2)-1 do db2(*,i)=db1(*,i)-(mean(db1(f1-1-bg:f1-1,i))+mean(db1(f2+1:f2+1+bg,i)))/2
endif else begin
 db2=db1
endelse

print,'done background correction',f1-1-bg,f1-1,f1,f2,f2+1,f2+1+bg


a=max(total(ref(*,40:z(2)-1),2),c)
ff1=c-(r-1)/2
ff2=c+(r-1)/2
for i=0,z(2)-1 do ref1(*,i)=ref(*,i)/(water*fac)


; subtract background from reflected beam
if (bg gt 0) then begin
  for i=0,z(2)-1 do ref2(*,i)=ref1(*,i)-(mean(ref1(ff1-1-bg:ff1-1,i))+mean(ref1(ff2+1:ff2+1+bg,i)))/2
endif else begin
 ref2=ref1
endelse

print,'reflection peak at ',c




r=total(ref2(ff1:ff2,*),1)
d=total(db2(f1:f2,*),1)
dbb=d
rb=r


;errors for direct beam background
if (bg gt 0) then begin
  dbb=(sqrt(total(db1(f1-1-bg:f1-1),1)+total(db1(f2+1:f2+1+bg),1)))/(2.*bg)
endif else begin
  dbb=0
endelse

ed=sqrt(d+dbb^2)

;errors for reflected beam background
if (bg gt 0) then begin
  rb=(sqrt(total(ref1(ff1-1-bg:ff1-1),1)+total(ref1(ff2+1:ff2+1+bg),1)))/(2.*bg)
endif else begin
  rb=0
endelse


er=sqrt(((sqrt(r*fac)/fac))^2+rb^2)

;help,r & help,d & help,er & help,ed

reff=r
ereff=r

for i=0,z(2)-1 do begin
;  print,'raw',i,r(i),d(i)
  if ((r(i) gt 0.) and (d(i) gt 0.)) then begin
    reff(i)=r(i)/d(i)
    
    ereff(i)=reff(i)*sqrt((er(i)/r(i))^2+(ed(i)/d(i))^2) 
;    print,'good point',i,r(i),d(i),reff(i),ereff(i),ereff(i)/(reff(i)*alog(10.))
  endif else begin
    print,'Youve made a bad point there'
    ereff(i)=0
    reff(i)=1e-9
;    print,' bum point',i,ereff(i),reff(i)
  endelse
endfor


; unlogged data for reflectivity is in reff, q is q and error is ereff

;help,reff & help,ereff
w5=alog10(reff(chop:z(2)-1)) & x5=q(chop:z(2)-1) & e5=ereff(chop:z(2)-1)/(reff(chop:z(2)-1)*alog(10.))

return
end


