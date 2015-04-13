pro blap,pic2,picture
close,4
close,3
openr,3,'data.'
xdim=600
ydim=1200

picture=fltarr(xdim,ydim)

i=1
x=1
y=1



xmin=1000000
xmax=-1000000
ymin=1000000
ymax=-1000000

while (not eof(3) and i lt 50000) do begin

  i=i+1
  readf,3,y,x
  if (x lt xmin ) then xmin=x
  if (x gt xmax ) then xmax=x
  if (y lt ymin ) then ymin=y
  if (y gt ymax ) then ymax=y
  
  x=x
  y=y+880
 if ((x le xdim) and (x ge 1) and (y le ydim) and (y ge 1))then begin
  picture(x,y)=picture(x,y)+1 
 endif
endwhile

bfac=1.2
picture=reverse(picture)
pic2=congrid(picture,(xdim/bfac),(ydim/bfac))
pic2=pic2*bfac*bfac
pic2=smooth(pic2,5)
m1=max(pic2)
m2=min(pic2)
m3=total(pic2)
print,'total counts: ',m3
print,'min counts: ',m2
print,'max counts: ',m1
print,'xmax: ',xmax
print,'xmin: ',xmin
print,'ymin: ',ymin
print,'ymax: ',ymax

n=9
ll=fltarr(n+1)
for i=0,n do begin 
	ll(i)=m2+float(i)*(((m1-m2))/float(n+1))
endfor
print,ll


loadct,5
gamma_ct,0.302

;,c_colors=indgen(10)*25,levels=ll
;window,0,retain=2,xsize=1000,ysize=1000
;contour,pic2,levels=ll,c_colors=indgen(10)*25
window,1,retain=2,xsize=470,ysize=900
tv,fix((pic2*256/max(picture)))
window,2,retain=2,xsize=470,ysize=500
plot,total(pic2,1)
window,3,retain=2,xsize=470,ysize=500
plot,total(pic2,2)


openw,4,'picture'
printf,4,picture
close,4
close,3

end
