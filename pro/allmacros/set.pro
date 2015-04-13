pro set

close,3

openr,3,'data.'
w1=intarr(1101,571)
readf,3,w1
w1=reverse(w1,1)
window,6
tv,w1

m1=max(w1)
m2=min(w1)
m3=total(w1)
print,'total counts: ',m3
print,'min counts: ',m2
print,'max counts: ',m1

m=9
ll=m2+indgen(m)*((float(m1-m2))/float(m+1))
window,3,retain=2
;,xsize=500,ysize=1000

contour,w1,levels=ll,c_colors=indgen(10)*25
;,xrange=[0,525],yrange=[0,1053]
end
