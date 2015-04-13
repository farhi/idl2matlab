pro fred

openr,6,'data_d17b.'
nb_neutron=40000






time1=intarr(nb_neutron)
time2=intarr(nb_neutron)
time3=intarr(nb_neutron)
time4=intarr(nb_neutron)
timea=intarr(nb_neutron)
time1r=intarr(nb_neutron)
time2r=intarr(nb_neutron)
time3r=intarr(nb_neutron)
time4r=intarr(nb_neutron)
timear=intarr(nb_neutron)


ll=fltarr(10)
projx1=fltarr(800)
projx2=fltarr(800)
proja=fltarr(4000)

hit12=fltarr(3,3)
hit34=fltarr(3,3)


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


rejected=0

i=0.0
j=0.0
num=0.0
ix=0
while ( NOT eof(6) AND (i LT nb_neutron-1)AND (j LT nb_neutron-1)) do begin


readf,6,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10
ix=ix+1
;put the values into time and hit arrays inside the loop otherwise we make statistics on 
;un-rejected data


;  single pulse neutrons
 if( (d5 EQ 1) AND (d6 EQ 1) AND (d7 EQ 1) AND (d8 EQ 1) AND (d9 EQ 1)  ) then begin


		


		time1(i)=d1
		time2(i)=d2
		time3(i)=d3
		time4(i)=d4
		timea(i)=d10
		
		i=i+1
	        if((d4-d3+400 GT 0) AND (d4-d3+400 LT 800)) then projx1(d4-d3+400)=projx1(d4-d3+400)+1 
	        if((d3 GT 0) AND (d3 LT 800)) then projx2(d3)=projx2(d3)+1
	        if(d10 LT 4000) then proja(d10)=proja(d10)+1
	
		
		
		if(d9 EQ 0) then begin


		time1r(j)=d1
		time2r(j)=d2
		time3r(j)=d3
		time4r(j)=d4
		timear(j)=d10
		
		j=j+1
	        
		end
		
	
	
	
        endif else begin
		rejected=rejected+1
	endelse

end
close,6
num=i
print,'number read in: ',ix
print,'good ones: ',num
print,'rejected: ',rejected



loadct,5
;window,0,retain=2,xsize=650,ysize=600

;plot,time3,time4,PSYM=3,color=69
;window,2,retain=2,xsize=650,ysize=600
;plot,time1,time2,PSYM=3,color=120



window,1,retain=2,xsize=800,ysize=400
plot,time3-time4,time1-time2,color=256,PSYM=3

;window,2,retain=2,xsize=800,ysize=400
;plot,time3r-time4r,time1r-time2r,color=256,PSYM=3

;window,3,retain=2
;plot,projx1,xrange=[600,700]

;window,4,retain=2
;plot,projx2


end ;of procedure fred
