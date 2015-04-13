PRO f10scan,sani,nsan,dsan,time

;sani = initial value
;nsan = no. data point
;dsan = step size
;time = counting time
co,1,' t n'
co,1,' t n'


dan0=1.7
mv,'san',sani


mv,'b1',0
mv,'b2',0


for i=0,nsan-1 do begin
        d17mad,'par sub flipper off'
	mv,'san',sani+float(i)*dsan
        mv,'dan',dan0+(sani+float(i)*dsan)*2.
        mv,'dan',dan0+(sani+float(i)*dsan)*2.
	co,time,' t'
	mv,'b1',1.8
	mv,'b2',.8
        d17mad,'par sub flipper on'
	co,time,' t'
	mv,'b1',0
	mv,'b2',0


endfor
mv,'b1',1.8
mv,'b2',.8

d17mad,'b1 off'
wait,2
d17mad,'b2 off'


end
