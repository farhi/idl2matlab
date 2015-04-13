PRO th2th,sani,nsan,dsan,time

;sani = initial value
;nsan = no. data point
;dsan = step size
;time = counting time

co,1,' t n'
co,1,' t n'
d0=1.5
mv,'dan',d0-0.5

for i=0,nsan-1 do begin
	mv,'san',sani+i*dsan
        mv,'dan',(sani+i*dsan)*2.+d0
        mv,'dan',(sani+i*dsan)*2.+d0
	co,time,' t'
endfor
end
