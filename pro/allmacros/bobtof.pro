pro bobtof,name
loadct,3

XBIN=35
YBIN=34
TSLICE=500
det=intarr(XBIN,YBIN,TSLICE)

print,name
openr,3,name
readf,3,det
close,3

;window,0,retain=2
;plot,total(total(det,1),1)

;window,1,xsize=200,ysize=200,retain=2
;tvscl,congrid(total(det,3),200,200)

;window,2,retain=2,xsize=200,ysize=200
;for i=1,TSLICE-1 do tvscl,congrid(det(*,*,i),200,200)
;surface,total(det,1)
m1=max(total(det,3))
m2=min(total(det,3))
m3=total(det)
print,'total counts: ',m3
print,'min counts: ',m2
print,'max counts: ',m1


m=19
ll=m2+indgen(m)*((float(m1-m2))/float(m+1))
window,3,retain=2
;,xsize=500,ysize=1000

contour,(total(det,3)),levels=ll,c_colors=indgen(15)*37
;,xrange=[0,525],yrange=[0,1053]
xloadct




end ; test_tof
