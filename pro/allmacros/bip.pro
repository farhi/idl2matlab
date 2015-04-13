pro bip

openr,6,'data_d17b.'

time1=intarr(200000)
time2=intarr(200000)
;tot1=intarr(200000)
time3=intarr(200000)
time4=intarr(200000)
;tot2=intarr(200000)
timea=intarr(200000)
;hit1=intarr(200000)
;hit2=intarr(200000)
;hit3=intarr(200000)
;hit4=intarr(200000)
;hita=intarr(200000)
ll=fltarr(10)
picture=intarr(1000,400)



d1=0
d2=0
d3=0
d4=0
d5=0
d6=0
d7=0
d8=0
d9=0
d10=0


rejected=0.

i=0.0
num=0.0
ix=0.0
while ( NOT eof(6) AND (i LT 200000)) do begin


readf,6,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10
ix=ix+1
;put the values into time and hit arrays inside the loop otherwise we make statistics on 
;un-rejected data



if( (d5 EQ 1) AND (d6 EQ 1) AND (d7 EQ 1) AND (d8 EQ 1) and (d9 EQ 1)) then begin

	
;	x=d1-d2+200
;	y=d3-d4+400
	y=d1-d2+200
	x=d4-d3+400
	if((x GE 0) AND (x LT 1000) AND (y GE 0) AND (y LT 1000)$
        and (d1+d2 GE 0) AND (d1+d2 LT 400) AND (d3+d4 GE 0) AND (d3+d4 LT 1000)) then begin
		picture(x,y)=picture(x,y) +1
		time1(i)=d1
		time2(i)=d2
		time3(i)=d3
		time4(i)=d4
		timea(i)=d10
;		hit1(i)=d5
;		hit2(i)=d6
;		hit3(i)=d7
;		hit4(i)=d8
;		hita(i)=d9
		i=i+1
	endif	         
	
        endif else begin
		rejected=rejected+1
	endelse

end
close,6
num=i
print,'number read in: ',ix
print,'good ones: ',num
print,'rejected: ',rejected,' = ', 100.*rejected/ix,' %'






tot1=intarr(num)
tot2=intarr(num)

for i=0.0,num-1 do begin
  tot1(i)=time1(i)+time2(i)
  tot2(i)=time3(i)+time4(i)
;  print,i,tot1(i),tot2(i),time1(i)-time2(i),time3(i)-time4(i)
endfor




loadct,5
window,0,retain=2,xsize=400,ysize=400
;plot,time3,time4,PSYM=3,/NOdata,color=256,xstyle=1,ystyle=1,xrange=[470,700],yrange=[400,610]

plot,time3,time4,PSYM=3,color=69,xrange=[0,600],yrange=[0,600]
window,1,retain=2,xsize=400,ysize=400
plot,time1,time2,PSYM=3,color=120,xrange=[0,200],yrange=[0,200]
window,2,retain=2,xsize=400,ysize=400
plot,(time1+time2),(time3+time4),PSYM=3,color=150

print,'TEST ',min(time1)
print,'TEST ',min(time2)
print,'TEST ',min(time3)
print,'TEST ',min(time4)

; probability distributions of delay sums

  result=moment(tot1)
  tit=' mean= '+string(result(0))+'  rms= '
  tit2=string(sqrt(result(1)))+' skewness='+string(result(2))
  tit=tit+tit2
  print,'mean time1+time2: ',result(0)
  print,'rms  time1+time2: ',sqrt(result(1))
  print,'skew time1+time2: ',result(2)
  print,'min  time1+time2: ',min(tot1)
  print,'max time1+time2: ',max(tot1)
  
  

  nn=(max(tot1)-min(tot1))
  b=1.0
  window,3,retain=2,xsize=550,ysize=400
  x=fltarr(nn)
  x=findgen(nn)*b+min(tot1)+b/2.
  plot,x,histogram(tot1,binsize=b), $
  color=120,title=tit,xtitle='time1+time2',$
  charsize=.6

  result=moment(tot2)
  tit=' mean= '+string(result(0))+'  rms= '
  tit2=string(sqrt(result(1)))+' skewness='+string(result(2))
  tit=tit+tit2
  print,'mean time3+time4: ',result(0)
  print,'rms  time3+time4: ',sqrt(result(1))
  print,'skew time3+time4: ',result(2)
  print,'min  time3+time4: ',min(tot2)
  print,'max  time3+time4: ',max(tot2)
  
  
  nn=(max(tot2)-min(tot2))
  b=1.0
  window,4,retain=2,xsize=550,ysize=400
  x=fltarr(nn)
  x=findgen(nn)*b+min(tot2)+b/2
  plot,x,histogram(tot2,binsize=b),$
  color=69,title=tit,xtitle='time3+time4',$
  charsize=.6



;window,1,xsize=400,ysize=400
;plot,timea,xrange=[0,2000],yrange=[110,125]


m1=max(picture)
m2=min(picture)
m3=total(picture)
print,'total counts: ',m3
print,'min counts: ',m2
print,'max counts: ',m1
print,'rejected neutron = ',rejected

n=9

for i=0,n do begin 
	ll(i)=m2+float(i)*((float(m1-m2))/float(n+1))
	
endfor


window,3,retain=2,xsize=2000,ysize=2000
contour,picture,levels=ll,c_colors=indgen(10)*25
;xrange=[0,610],yrange=[270,410]
end ;of procedure bip
